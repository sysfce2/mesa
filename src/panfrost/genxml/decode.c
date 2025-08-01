/*
 * Copyright (C) 2017-2019 Alyssa Rosenzweig
 * Copyright (C) 2017-2019 Connor Abbott
 * Copyright (C) 2019 Collabora, Ltd.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "decode.h"
#include <ctype.h>
#include <errno.h>
#include <memory.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <genxml/gen_macros.h>
#include <sys/mman.h>

#include "compiler/bifrost/disassemble.h"
#include "compiler/valhall/disassemble.h"
#include "midgard/disassemble.h"
#include "util/set.h"
#include "pan_format.h"

#if PAN_ARCH <= 5
/* Midgard's tiler descriptor is embedded within the
 * larger FBD */

static void
pandecode_midgard_tiler_descriptor(struct pandecode_context *ctx,
                                   const struct mali_tiler_context_packed *tp,
                                   const struct mali_tiler_weights_packed *wp)
{
   pan_unpack(tp, TILER_CONTEXT, t);
   DUMP_UNPACKED(ctx, TILER_CONTEXT, t, "Tiler:\n");

   /* We've never seen weights used in practice, but they exist */
   pan_unpack(wp, TILER_WEIGHTS, w);
   bool nonzero_weights = false;

   nonzero_weights |= w.weight0 != 0x0;
   nonzero_weights |= w.weight1 != 0x0;
   nonzero_weights |= w.weight2 != 0x0;
   nonzero_weights |= w.weight3 != 0x0;
   nonzero_weights |= w.weight4 != 0x0;
   nonzero_weights |= w.weight5 != 0x0;
   nonzero_weights |= w.weight6 != 0x0;
   nonzero_weights |= w.weight7 != 0x0;

   if (nonzero_weights)
      DUMP_UNPACKED(ctx, TILER_WEIGHTS, w, "Tiler Weights:\n");
}
#endif

#if PAN_ARCH >= 5
static void
pandecode_rt(struct pandecode_context *ctx, unsigned index, uint64_t gpu_va)
{
   uint64_t rt_va = gpu_va + index * pan_size(RENDER_TARGET);
   const struct mali_render_target_packed *PANDECODE_PTR_VAR(ctx, rtp, rt_va);

   pan_unpack(rtp, RENDER_TARGET, rt);
#if PAN_ARCH >= 10
   switch (rt.rgb.writeback_mode) {
   case MALI_WRITEBACK_MODE_COMPAT:
      break;
   case MALI_WRITEBACK_MODE_AFRC_RGB:
      DUMP_UNPACKED(ctx, AFRC_RGB_RENDER_TARGET, rt.afrc_rgb,
                    "AFRC RGB Color Render Target %d:\n", index);
      break;
   case MALI_WRITEBACK_MODE_AFRC_YUV:
      DUMP_UNPACKED(ctx, AFRC_YUV_RENDER_TARGET, rt.afrc_yuv,
                    "AFRC YUV Color Render Target %d:\n", index);
      break;
   default:
      UNREACHABLE("Invalid writeback mode");
   }
#endif

   switch (rt.rgb.writeback_block_format) {
#if PAN_ARCH >= 7
   case MALI_BLOCK_FORMAT_NO_WRITE:
#else
   case MALI_BLOCK_FORMAT_TILED_LINEAR:
#endif
   case MALI_BLOCK_FORMAT_TILED_U_INTERLEAVED:
   case MALI_BLOCK_FORMAT_LINEAR:
      if (rt.rgb.yuv_enable) {
         DUMP_UNPACKED(ctx, YUV_RENDER_TARGET, rt.yuv,
                       "%s YUV Color Render Target %d:\n",
                       rt.rgb.writeback_block_format == MALI_BLOCK_FORMAT_LINEAR
                          ? "Linear"
                          : "U-Tiled",
                       index);
      } else {
         DUMP_UNPACKED(ctx, RGB_RENDER_TARGET, rt.rgb,
                       "%s RGB Color Render Target %d:\n",
                       rt.rgb.writeback_block_format == MALI_BLOCK_FORMAT_LINEAR
                          ? "Linear"
                          : "U-Tiled",
                       index);
      }
      break;
#if PAN_ARCH >= 7
   case MALI_BLOCK_FORMAT_AFBC_TILED:
#endif
   case MALI_BLOCK_FORMAT_AFBC:
#if PAN_ARCH >= 6
      if (rt.rgb.yuv_enable) {
         DUMP_UNPACKED(ctx, AFBC_YUV_RENDER_TARGET, rt.afbc_yuv,
                       "AFBC YUV Color Render Target %d:\n", index);
         break;
      }
#else
      assert(!rt.rgb.yuv_enable);
#endif

      DUMP_UNPACKED(ctx, AFBC_RGB_RENDER_TARGET, rt.afbc_rgb,
                    "AFBC RGB Color Render Target %d:\n", index);
      break;
   }
}

static void
pandecode_rts(struct pandecode_context *ctx, uint64_t gpu_va, unsigned gpu_id,
              const struct MALI_FRAMEBUFFER_PARAMETERS *fb)
{
   pandecode_log(ctx, "Color Render Targets @%" PRIx64 ":\n", gpu_va);
   ctx->indent++;

   for (int i = 0; i < (fb->render_target_count); i++)
      pandecode_rt(ctx, i, gpu_va);

   ctx->indent--;
   pandecode_log(ctx, "\n");
}

static void
pandecode_zs_crc_ext(struct pandecode_context *ctx, uint64_t gpu_va)
{
   const struct mali_zs_crc_extension_packed *PANDECODE_PTR_VAR(
      ctx, zs_crc_packed, (uint64_t)gpu_va);

   pan_unpack(zs_crc_packed, ZS_CRC_EXTENSION, zs_crc);
   DUMP_UNPACKED(ctx, CRC, zs_crc.crc, "CRC:\n");

   switch (zs_crc.zs.block_format) {
#if PAN_ARCH >= 7
   case MALI_BLOCK_FORMAT_NO_WRITE:
#else
   case MALI_BLOCK_FORMAT_TILED_LINEAR:
#endif
   case MALI_BLOCK_FORMAT_TILED_U_INTERLEAVED:
   case MALI_BLOCK_FORMAT_LINEAR:
      DUMP_UNPACKED(ctx, ZS_TARGET, zs_crc.zs, "ZS:\n");
      break;
#if PAN_ARCH >= 7
   case MALI_BLOCK_FORMAT_AFBC_TILED:
#endif
   case MALI_BLOCK_FORMAT_AFBC:
      DUMP_UNPACKED(ctx, AFBC_ZS_TARGET, zs_crc.afbc_zs, "ZS:\n");
      break;

   default:
      UNREACHABLE("Invalid block format");
   }

   switch (zs_crc.s.block_format) {
#if PAN_ARCH >= 7
   case MALI_BLOCK_FORMAT_NO_WRITE:
#else
   case MALI_BLOCK_FORMAT_TILED_LINEAR:
#endif
   case MALI_BLOCK_FORMAT_TILED_U_INTERLEAVED:
   case MALI_BLOCK_FORMAT_LINEAR:
      DUMP_UNPACKED(ctx, S_TARGET, zs_crc.s, "S:\n");
      break;
#if PAN_ARCH >= 9
   case MALI_BLOCK_FORMAT_AFBC_TILED:
   case MALI_BLOCK_FORMAT_AFBC:
      DUMP_UNPACKED(ctx, AFBC_S_TARGET, zs_crc.afbc_s, "S:\n");
      break;
#endif

   default:
      UNREACHABLE("Invalid block format");
   }

   pandecode_log(ctx, "\n");
}
#endif


#if PAN_ARCH >= 6
static void
pandecode_sample_locations(struct pandecode_context *ctx, const void *fb)
{
   pan_section_unpack(fb, FRAMEBUFFER, PARAMETERS, params);

   const uint16_t *PANDECODE_PTR_VAR(ctx, samples, params.sample_locations);

   pandecode_log(ctx, "Sample locations @%" PRIx64 ":\n",
                 params.sample_locations);
   for (int i = 0; i < 33; i++) {
      pandecode_log(ctx, "  (%d, %d),\n", samples[2 * i] - 128,
                    samples[2 * i + 1] - 128);
   }
}
#endif

struct pandecode_fbd
GENX(pandecode_fbd)(struct pandecode_context *ctx, uint64_t gpu_va,
                    bool is_fragment, unsigned gpu_id)
{
   const void *PANDECODE_PTR_VAR(ctx, fb, (uint64_t)gpu_va);
   pan_section_unpack(fb, FRAMEBUFFER, PARAMETERS, params);
   DUMP_UNPACKED(ctx, FRAMEBUFFER_PARAMETERS, params, "Parameters:\n");

#if PAN_ARCH >= 6
   pandecode_sample_locations(ctx, fb);

   unsigned dcd_size = pan_size(DRAW);
   unsigned job_type_param = 0;

#if PAN_ARCH <= 9
   job_type_param = MALI_JOB_TYPE_FRAGMENT;
#endif

   if (params.pre_frame_0 != MALI_PRE_POST_FRAME_SHADER_MODE_NEVER) {
      const struct mali_draw_packed *PANDECODE_PTR_VAR(
         ctx, dcd, params.frame_shader_dcds + (0 * dcd_size));
      pan_unpack(dcd, DRAW, draw);
      pandecode_log(ctx, "Pre frame 0 @%" PRIx64 " (mode=%d):\n",
                    params.frame_shader_dcds, params.pre_frame_0);
      GENX(pandecode_dcd)(ctx, &draw, job_type_param, gpu_id);
   }

   if (params.pre_frame_1 != MALI_PRE_POST_FRAME_SHADER_MODE_NEVER) {
      const struct mali_draw_packed *PANDECODE_PTR_VAR(
         ctx, dcd, params.frame_shader_dcds + (1 * dcd_size));
      pan_unpack(dcd, DRAW, draw);
      pandecode_log(ctx, "Pre frame 1 @%" PRIx64 ":\n",
                    params.frame_shader_dcds + (1 * dcd_size));
      GENX(pandecode_dcd)(ctx, &draw, job_type_param, gpu_id);
   }

   if (params.post_frame != MALI_PRE_POST_FRAME_SHADER_MODE_NEVER) {
      const struct mali_draw_packed *PANDECODE_PTR_VAR(
         ctx, dcd, params.frame_shader_dcds + (2 * dcd_size));
      pan_unpack(dcd, DRAW, draw);
      pandecode_log(ctx, "Post frame:\n");
      GENX(pandecode_dcd)(ctx, &draw, job_type_param, gpu_id);
   }
#else
   DUMP_SECTION(ctx, FRAMEBUFFER, LOCAL_STORAGE, fb, "Local Storage:\n");

   const void *t = pan_section_ptr(fb, FRAMEBUFFER, TILER);
   const void *w = pan_section_ptr(fb, FRAMEBUFFER, TILER_WEIGHTS);
   pandecode_midgard_tiler_descriptor(ctx, t, w);
#endif

   pandecode_log(ctx, "Framebuffer @%" PRIx64 ":\n", gpu_va);
   ctx->indent++;

   DUMP_UNPACKED(ctx, FRAMEBUFFER_PARAMETERS, params, "Parameters:\n");
#if PAN_ARCH >= 6
   if (params.tiler)
      GENX(pandecode_tiler)(ctx, params.tiler, gpu_id);
#endif

   ctx->indent--;
   pandecode_log(ctx, "\n");

#if PAN_ARCH >= 5
   gpu_va += pan_size(FRAMEBUFFER);

   if (params.has_zs_crc_extension) {
      pandecode_zs_crc_ext(ctx, gpu_va);

      gpu_va += pan_size(ZS_CRC_EXTENSION);
   }

   if (is_fragment)
      pandecode_rts(ctx, gpu_va, gpu_id, &params);

   return (struct pandecode_fbd){
      .rt_count = params.render_target_count,
      .has_extra = params.has_zs_crc_extension,
   };
#else
   /* Dummy unpack of the padding section to make sure all words are 0.
    * No need to call print here since the section is supposed to be empty.
    */
   pan_section_unpack(fb, FRAMEBUFFER, PADDING_1, padding1);
   pan_section_unpack(fb, FRAMEBUFFER, PADDING_2, padding2);

   return (struct pandecode_fbd){
      .rt_count = 1,
   };
#endif
}

#if PAN_ARCH >= 5
uint64_t
GENX(pandecode_blend)(struct pandecode_context *ctx,
                      struct mali_blend_packed *descs, int rt_no,
                      uint64_t frag_shader)
{
   pan_unpack(&descs[rt_no], BLEND, b);
   DUMP_UNPACKED(ctx, BLEND, b, "Blend RT %d:\n", rt_no);
#if PAN_ARCH >= 6
   if (b.internal.mode != MALI_BLEND_MODE_SHADER)
      return 0;
   /* If we don't have a frag shader, we can't extract the LSB of the blend
    * shader so return NULL in that case. It doesn't matter, because the
    * blend shader won't be executed anyway, so disassembling is not
    * super useful. */
   if (!frag_shader)
      return 0;

   return (frag_shader & 0xFFFFFFFF00000000ULL) | b.internal.shader.pc;
#else
   return b.blend_shader ? (b.shader_pc & ~0xf) : 0;
#endif
}
#endif

#if PAN_ARCH < 9
static bool
pan_is_yuv_format(uint32_t packed)
{
#if PAN_ARCH == 7
   enum mali_format mali_fmt = packed >> 12;
   return mali_fmt >= MALI_YUV8 && mali_fmt <= MALI_CUSTOM_YUV_5;
#else
   /* Currently only supported by panfrost on v7 */
   assert(0);
   return false;
#endif
}

static void
pandecode_texture_payload(struct pandecode_context *ctx, uint64_t payload,
                          const struct MALI_TEXTURE *tex)
{
   unsigned nr_samples =
      tex->dimension == MALI_TEXTURE_DIMENSION_3D ? 1 : tex->sample_count;

   if (!payload)
      return;

   /* A bunch of bitmap pointers follow.
    * We work out the correct number,
    * based on the mipmap/cubemap
    * properties, but dump extra
    * possibilities to futureproof */

   int bitmap_count = tex->levels;

   /* Miptree for each face */
   if (tex->dimension == MALI_TEXTURE_DIMENSION_CUBE)
      bitmap_count *= 6;

   /* Array of layers */
   bitmap_count *= nr_samples;

   /* Array of textures */
   bitmap_count *= tex->array_size;

#define PANDECODE_EMIT_TEX_PAYLOAD_DESC(T, msg)                                \
   for (int i = 0; i < bitmap_count; ++i) {                                    \
      uint64_t addr = payload + pan_size(T) * i;                               \
      pan_unpack(PANDECODE_PTR(ctx, addr, MALI_##T##_PACKED_T), T, s);         \
      DUMP_UNPACKED(ctx, T, s, msg " @%" PRIx64 ":\n", addr)                   \
   }

#if PAN_ARCH <= 5
   switch (tex->surface_type) {
   case MALI_SURFACE_TYPE_32:
      PANDECODE_EMIT_TEX_PAYLOAD_DESC(SURFACE_32, "Surface 32");
      break;
   case MALI_SURFACE_TYPE_64:
      PANDECODE_EMIT_TEX_PAYLOAD_DESC(SURFACE, "Surface");
      break;
   case MALI_SURFACE_TYPE_32_WITH_ROW_STRIDE:
      PANDECODE_EMIT_TEX_PAYLOAD_DESC(SURFACE_32, "Surface 32 With Row Stride");
      break;
   case MALI_SURFACE_TYPE_64_WITH_STRIDES:
      PANDECODE_EMIT_TEX_PAYLOAD_DESC(SURFACE_WITH_STRIDE,
                                      "Surface With Stride");
      break;
   default:
      fprintf(ctx->dump_stream, "Unknown surface descriptor type %X\n",
              tex->surface_type);
      break;
   }
#elif PAN_ARCH == 6
   PANDECODE_EMIT_TEX_PAYLOAD_DESC(SURFACE_WITH_STRIDE, "Surface With Stride");
#else
   STATIC_ASSERT(PAN_ARCH == 7);
   if (pan_is_yuv_format(tex->format)) {
      PANDECODE_EMIT_TEX_PAYLOAD_DESC(MULTIPLANAR_SURFACE, "Surface YUV");
   } else {
      PANDECODE_EMIT_TEX_PAYLOAD_DESC(SURFACE_WITH_STRIDE,
                                      "Surface With Stride");
   }
#endif

#undef PANDECODE_EMIT_TEX_PAYLOAD_DESC
}
#endif

#if PAN_ARCH >= 9
static void
pandecode_tex_plane(struct pandecode_context *ctx, uint64_t u, unsigned idx)
{
   const struct mali_plane_header_packed *cl =
      pandecode_fetch_gpu_mem(ctx, u, pan_size(PLANE_HEADER));

   pan_unpack(cl, PLANE_HEADER, temp);

   switch (temp.plane_type) {
   case MALI_PLANE_TYPE_NULL:
      DUMP_ADDR(ctx, NULL_PLANE, u, "Plane %u:\n", idx);
      break;
   case MALI_PLANE_TYPE_GENERIC:
      DUMP_ADDR(ctx, GENERIC_PLANE, u, "Plane %u:\n", idx);
      break;
   case MALI_PLANE_TYPE_CHROMA_2P:
      DUMP_ADDR(ctx, CHROMA_2P_PLANE, u, "Plane %u:\n", idx);
      break;
   case MALI_PLANE_TYPE_ASTC_2D:
      DUMP_ADDR(ctx, ASTC_2D_PLANE, u, "Plane %u:\n", idx);
      break;
   case MALI_PLANE_TYPE_ASTC_3D:
      DUMP_ADDR(ctx, ASTC_3D_PLANE, u, "Plane %u:\n", idx);
      break;
   case MALI_PLANE_TYPE_AFBC:
      DUMP_ADDR(ctx, AFBC_PLANE, u, "Plane %u:\n", idx);
      break;
#if PAN_ARCH >= 10
   case MALI_PLANE_TYPE_AFRC:
      DUMP_ADDR(ctx, AFRC_PLANE, u, "Plane %u:\n", idx);
      break;
   case MALI_PLANE_TYPE_AFRC_CHROMA_2P:
      DUMP_ADDR(ctx, AFRC_CHROMA_2P_PLANE, u, "Plane %u:\n", idx);
      break;
#endif
   default:
      UNREACHABLE("Unknown plane type");
   }
}
#endif

#if PAN_ARCH <= 5
void
GENX(pandecode_texture)(struct pandecode_context *ctx, uint64_t u, unsigned tex)
{
   const struct mali_texture_packed *cl =
      pandecode_fetch_gpu_mem(ctx, u, pan_size(TEXTURE));

   pan_unpack(cl, TEXTURE, temp);
   DUMP_UNPACKED(ctx, TEXTURE, temp, "Texture:\n")

   ctx->indent++;
   pandecode_texture_payload(ctx, u + pan_size(TEXTURE), &temp);
   ctx->indent--;
}
#else
void
GENX(pandecode_texture)(struct pandecode_context *ctx,
                        const struct mali_texture_packed *cl, unsigned tex)
{
   pan_unpack(cl, TEXTURE, temp);
   DUMP_UNPACKED(ctx, TEXTURE, temp, "Texture:\n")

   ctx->indent++;

#if PAN_ARCH >= 9
   int plane_count = temp.levels * temp.array_size;

   /* Miptree for each face */
   if (temp.dimension == MALI_TEXTURE_DIMENSION_CUBE)
      plane_count *= 6;

   for (unsigned i = 0; i < plane_count; ++i)
      pandecode_tex_plane(ctx, temp.surfaces + i * pan_size(NULL_PLANE), i);
#else
   pandecode_texture_payload(ctx, temp.surfaces, &temp);
#endif
   ctx->indent--;
}
#endif

#if PAN_ARCH >= 6
void
GENX(pandecode_tiler)(struct pandecode_context *ctx, uint64_t gpu_va,
                      unsigned gpu_id)
{
   pan_unpack(PANDECODE_PTR(ctx, gpu_va, struct mali_tiler_context_packed),
              TILER_CONTEXT, t);

   if (t.heap) {
      pan_unpack(PANDECODE_PTR(ctx, t.heap, struct mali_tiler_heap_packed),
                 TILER_HEAP, h);
      DUMP_UNPACKED(ctx, TILER_HEAP, h, "Tiler Heap:\n");
   }

   DUMP_UNPACKED(ctx, TILER_CONTEXT, t, "Tiler Context @%" PRIx64 ":\n",
                 gpu_va);
}
#endif

#if PAN_ARCH >= 9
void
GENX(pandecode_fau)(struct pandecode_context *ctx, uint64_t addr,
                    unsigned count, const char *name)
{
   if (count == 0)
      return;

   const uint32_t *PANDECODE_PTR_VAR(ctx, raw, addr);

   pandecode_validate_buffer(ctx, addr, count * 8);

   fprintf(ctx->dump_stream, "%s @%" PRIx64 ":\n", name, addr);
   for (unsigned i = 0; i < count; ++i) {
      fprintf(ctx->dump_stream, "  %08X %08X\n", raw[2 * i], raw[2 * i + 1]);
   }
   fprintf(ctx->dump_stream, "\n");
}

uint64_t
GENX(pandecode_shader)(struct pandecode_context *ctx, uint64_t addr,
                       const char *label, unsigned gpu_id)
{
   MAP_ADDR(ctx, SHADER_PROGRAM, addr, cl);
   pan_unpack(cl, SHADER_PROGRAM, desc);

   assert(desc.type == 8);

   DUMP_UNPACKED(ctx, SHADER_PROGRAM, desc, "%s Shader @%" PRIx64 ":\n", label,
                 addr);
   pandecode_shader_disassemble(ctx, desc.binary, gpu_id);
   return desc.binary;
}

static unsigned
pandecode_buffer(struct pandecode_context *ctx,
                 const struct mali_buffer_packed *cl, uint64_t addr)
{
   pan_unpack(cl, BUFFER, buffer)
      ;
   DUMP_UNPACKED(ctx, BUFFER, buffer, "Buffer @%" PRIx64 ":\n", addr);

   /* If the address is the following descriptor, this descriptor is an IUB. */
   if (buffer.address == (addr + 0x20)) {
      assert((buffer.size % 0x20) == 0);

      const uint8_t *cl_bytes = (uint8_t *)cl;
      const uint32_t *words = (uint32_t *)(cl_bytes + 0x20);
      unsigned count = buffer.size / (2 * sizeof(uint32_t));

      ctx->indent++;
      pandecode_log(ctx, "IUB @%" PRIx64 ":\n", buffer.address);
      ctx->indent++;
      for (unsigned i = 0; i < count; ++i)
         pandecode_log(ctx, "%08X %08X\n", words[2 * i], words[2 * i + 1]);
      ctx->indent--;
      ctx->indent--;
      pandecode_log(ctx, "\n");

      return buffer.size;
   }

   return 0;
}

static void
pandecode_resources(struct pandecode_context *ctx, uint64_t addr, unsigned size)
{
   const uint8_t *cl = pandecode_fetch_gpu_mem(ctx, addr, size);
   assert((size % 0x20) == 0);

   for (unsigned i = 0; i < size; i += 0x20) {
      pan_unpack((const struct mali_descriptor_header_packed *)&cl[i],
                 DESCRIPTOR_HEADER, header)
         ;
      switch (header.type) {
      case MALI_DESCRIPTOR_TYPE_NULL:
         DUMP_CL(ctx, NULL_DESCRIPTOR, cl + i, "NullDescriptor @%" PRIx64 "\n",
                 addr + i);
         break;
      case MALI_DESCRIPTOR_TYPE_SAMPLER:
         DUMP_CL(ctx, SAMPLER, cl + i, "Sampler @%" PRIx64 ":\n", addr + i);
         break;
      case MALI_DESCRIPTOR_TYPE_TEXTURE:
         pandecode_log(ctx, "Texture @%" PRIx64 "\n", addr + i);
         GENX(pandecode_texture)(ctx, (struct mali_texture_packed *)&cl[i], i);
         break;
      case MALI_DESCRIPTOR_TYPE_ATTRIBUTE:
         DUMP_CL(ctx, ATTRIBUTE, cl + i, "Attribute @%" PRIx64 ":\n", addr + i);
         break;
      case MALI_DESCRIPTOR_TYPE_BUFFER:
         i += pandecode_buffer(ctx, (const struct mali_buffer_packed *)&cl[i],
                               addr + i);
         break;
      default:
         fprintf(ctx->dump_stream, "Unknown descriptor type %X\n", header.type);
         break;
      }
   }
}

void
GENX(pandecode_resource_tables)(struct pandecode_context *ctx, uint64_t addr,
                                const char *label)
{
   if (!addr)
      return;

   unsigned count = addr & 0x3F;
   addr = addr & ~0x3F;

   const struct mali_resource_packed *cl =
      pandecode_fetch_gpu_mem(ctx, addr, MALI_RESOURCE_LENGTH * count);

   pandecode_log(ctx, "%s resource table @%" PRIx64 "\n", label, addr);
   ctx->indent += 2;
   for (unsigned i = 0; i < count; ++i) {
      pan_unpack(&cl[i], RESOURCE, entry);
      DUMP_UNPACKED(ctx, RESOURCE, entry, "Entry %u @%" PRIx64 ":\n", i,
                    addr + i * MALI_RESOURCE_LENGTH);

      ctx->indent += 2;
      if (entry.address)
         pandecode_resources(ctx, entry.address, entry.size);
      ctx->indent -= 2;
   }
   ctx->indent -= 2;
}

void
GENX(pandecode_depth_stencil)(struct pandecode_context *ctx, uint64_t addr)
{
   MAP_ADDR(ctx, DEPTH_STENCIL, addr, cl);
   pan_unpack(cl, DEPTH_STENCIL, desc);
   DUMP_UNPACKED(ctx, DEPTH_STENCIL, desc, "Depth/stencil");
}

#if PAN_ARCH < 12
void
GENX(pandecode_shader_environment)(struct pandecode_context *ctx,
                                   const struct MALI_SHADER_ENVIRONMENT *p,
                                   unsigned gpu_id)
{
   if (p->shader)
      GENX(pandecode_shader)(ctx, p->shader, "Shader", gpu_id);

   GENX(pandecode_resource_tables)(ctx, p->resources, "Resources");

   if (p->thread_storage)
      DUMP_ADDR(ctx, LOCAL_STORAGE, p->thread_storage, "Local Storage:\n");

   if (p->fau)
      GENX(pandecode_fau)(ctx, p->fau, p->fau_count, "FAU");
}
#endif

void
GENX(pandecode_blend_descs)(struct pandecode_context *ctx, uint64_t blend,
                            unsigned count, uint64_t frag_shader,
                            unsigned gpu_id)
{
   for (unsigned i = 0; i < count; ++i) {
      struct mali_blend_packed *PANDECODE_PTR_VAR(ctx, blend_descs, blend);

      uint64_t blend_shader =
         GENX(pandecode_blend)(ctx, blend_descs, i, frag_shader);
      if (blend_shader) {
         fprintf(ctx->dump_stream, "Blend shader %u @%" PRIx64 "", i,
                 blend_shader);
         pandecode_shader_disassemble(ctx, blend_shader, gpu_id);
      }
   }
}

void
GENX(pandecode_dcd)(struct pandecode_context *ctx, const struct MALI_DRAW *p,
                    unsigned unused, unsigned gpu_id)
{
   uint64_t frag_shader = 0;

   GENX(pandecode_depth_stencil)(ctx, p->depth_stencil);
   GENX(pandecode_blend_descs)
   (ctx, p->blend, p->blend_count, frag_shader, gpu_id);
#if PAN_ARCH >= 12
   if (p->vertex_shader)
      GENX(pandecode_shader)(ctx, p->vertex_shader, "Vertex Shader", gpu_id);

   GENX(pandecode_resource_tables)(ctx, p->vertex_resources,
                                   "Vertex Resources");

   if (p->vertex_fau.pointer)
      GENX(pandecode_fau)(ctx, p->vertex_fau.pointer, p->vertex_fau.count,
                          "Vertex FAU");

   if (p->fragment_shader)
      GENX(pandecode_shader)(ctx, p->fragment_shader, "Fragment Shader",
                             gpu_id);

   GENX(pandecode_resource_tables)(ctx, p->fragment_resources,
                                   "Fragment Resources");

   if (p->fragment_fau.pointer)
      GENX(pandecode_fau)(ctx, p->fragment_fau.pointer, p->fragment_fau.count,
                          "Fragment FAU");

   if (p->thread_storage)
      DUMP_ADDR(ctx, LOCAL_STORAGE, p->thread_storage, "Local Storage:\n");
#else
   GENX(pandecode_shader_environment)(ctx, &p->shader, gpu_id);
#endif
   DUMP_UNPACKED(ctx, DRAW, *p, "Draw:\n");
}
#endif

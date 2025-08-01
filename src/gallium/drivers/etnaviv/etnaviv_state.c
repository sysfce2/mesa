/*
 * Copyright (c) 2012-2015 Etnaviv Project
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sub license,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Authors:
 *    Wladimir J. van der Laan <laanwj@gmail.com>
 *    Christian Gmeiner <christian.gmeiner@gmail.com>
 */

#include "etnaviv_state.h"

#include "etnaviv_blend.h"
#include "etnaviv_clear_blit.h"
#include "etnaviv_context.h"
#include "etnaviv_format.h"
#include "etnaviv_rasterizer.h"
#include "etnaviv_screen.h"
#include "etnaviv_shader.h"
#include "etnaviv_translate.h"
#include "etnaviv_util.h"
#include "etnaviv_zsa.h"
#include "util/u_framebuffer.h"
#include "util/u_helpers.h"
#include "util/u_inlines.h"
#include "util/u_math.h"
#include "util/u_memory.h"
#include "util/u_upload_mgr.h"

static void
etna_set_stencil_ref(struct pipe_context *pctx, const struct pipe_stencil_ref sr)
{
   struct etna_context *ctx = etna_context(pctx);
   struct compiled_stencil_ref *cs = &ctx->stencil_ref;

   ctx->stencil_ref_s = sr;

   for (unsigned i = 0; i < 2; i++) {
      cs->PE_STENCIL_CONFIG[i] =
         VIVS_PE_STENCIL_CONFIG_REF_FRONT(sr.ref_value[i]);
      cs->PE_STENCIL_CONFIG_EXT[i] =
         VIVS_PE_STENCIL_CONFIG_EXT_REF_BACK(sr.ref_value[!i]);
   }
   ctx->dirty |= ETNA_DIRTY_STENCIL_REF;
}

static void
etna_set_clip_state(struct pipe_context *pctx, const struct pipe_clip_state *pcs)
{
   /* NOOP */
}

static void
etna_set_sample_mask(struct pipe_context *pctx, unsigned sample_mask)
{
   struct etna_context *ctx = etna_context(pctx);

   ctx->sample_mask = sample_mask;
   ctx->dirty |= ETNA_DIRTY_SAMPLE_MASK;
}

static void
etna_set_constant_buffer(struct pipe_context *pctx,
      enum pipe_shader_type shader, uint index, bool take_ownership,
      const struct pipe_constant_buffer *cb)
{
   struct etna_context *ctx = etna_context(pctx);
   struct etna_constbuf_state *so = &ctx->constant_buffer[shader];

   assert(index < ETNA_MAX_CONST_BUF);

   util_copy_constant_buffer(&so->cb[index], cb, take_ownership);

   /* Note that the gallium frontends can unbind constant buffers by
    * passing NULL here. */
   if (unlikely(!cb || (!cb->buffer && !cb->user_buffer))) {
      so->enabled_mask &= ~(1 << index);
      return;
   }

   assert(index != 0 || cb->user_buffer != NULL);

   if (!cb->buffer) {
      struct pipe_constant_buffer *cb = &so->cb[index];
      u_upload_data(pctx->const_uploader, 0, cb->buffer_size, 16, cb->user_buffer, &cb->buffer_offset, &cb->buffer);
      ctx->dirty |= ETNA_DIRTY_SHADER_CACHES;
   }

   so->enabled_mask |= 1 << index;
   ctx->dirty |= ETNA_DIRTY_CONSTBUF;
}

static void
etna_update_render_surface(struct pipe_context *pctx,
                           const struct pipe_surface *surf)
{
   struct etna_resource *base = etna_resource(surf->texture);
   struct etna_resource *to = base, *from = base;
   unsigned level = surf->level;

   if (base->texture &&
       etna_resource_level_newer(&etna_resource(base->texture)->levels[level],
                                 &base->levels[level]))
      from = etna_resource(base->texture);

   if (base->render)
      to = etna_resource(base->render);

   if ((to != from) &&
       etna_resource_level_older(&to->levels[level], &from->levels[level]))
      etna_copy_resource(pctx, &to->base, &from->base, level, level);
}

static void
etna_set_framebuffer_state(struct pipe_context *pctx,
                           const struct pipe_framebuffer_state *fb)
{
   struct etna_context *ctx = etna_context(pctx);
   struct etna_screen *screen = ctx->screen;
   struct compiled_framebuffer_state *cs = &ctx->framebuffer;
   int nr_samples_color = -1;
   int nr_samples_depth = -1;
   bool target_16bpp = false;
   bool target_linear = false;

   memset(cs, 0, sizeof(struct compiled_framebuffer_state));

   /* Set up TS as well. Warning: this state is used by both the RS and PE */
   uint32_t ts_mem_config = 0;
   uint32_t pe_mem_config = 0;
   uint32_t pe_logic_op = 0;

   const bool use_ts = etna_use_ts_for_mrt(screen, fb);
   unsigned rt = 0;

   for (unsigned i = 0; i < fb->nr_cbufs; i++) {
      if (!fb->cbufs[i].texture)
         continue;

      const struct pipe_surface *surf = &fb->cbufs[i];
      struct etna_resource *res = etna_resource_get_render_compatible(pctx, surf->texture);
      struct etna_resource_level *level = &res->levels[surf->level];

      bool color_supertiled = (res->layout & ETNA_LAYOUT_BIT_SUPER) != 0;
      uint32_t fmt = translate_pe_format(surf->format);

      /* Resolve TS if needed */
      if (!use_ts) {
         etna_copy_resource(pctx, &res->base, &res->base, surf->level, surf->level);
         etna_resource_level_ts_mark_invalid(level);
      }

      assert((res->layout & ETNA_LAYOUT_BIT_TILE) ||
             VIV_FEATURE(screen, ETNA_FEATURE_LINEAR_PE));
      assert(!screen->specs.pe_multitiled ||
             (res->layout & ETNA_LAYOUT_BIT_MULTI));
      etna_update_render_surface(pctx, surf);

      if (res->layout == ETNA_LAYOUT_LINEAR)
         target_linear = true;

      if (util_format_get_blocksize(surf->format) <= 2)
         target_16bpp = true;

      for (int i = 0; i < screen->specs.pixel_pipes; i++) {
         cs->PE_RT_PIPE_COLOR_ADDR[rt][i].bo = res->bo;
         cs->PE_RT_PIPE_COLOR_ADDR[rt][i].offset = level->offset + surf->first_layer * level->layer_stride;
         if (!screen->specs.single_buffer)
            cs->PE_RT_PIPE_COLOR_ADDR[rt][i].offset += level->layer_stride / screen->specs.pixel_pipes * i;
         cs->PE_RT_PIPE_COLOR_ADDR[rt][i].flags = ETNA_RELOC_READ | ETNA_RELOC_WRITE;
      }

      if (rt == 0) {
         if (fmt >= PE_FORMAT_R16F)
            cs->PE_COLOR_FORMAT = VIVS_PE_COLOR_FORMAT_FORMAT_EXT(fmt) |
                                  VIVS_PE_COLOR_FORMAT_FORMAT_MASK;
         else
            cs->PE_COLOR_FORMAT = VIVS_PE_COLOR_FORMAT_FORMAT(fmt);

         cs->PE_COLOR_FORMAT |=
            VIVS_PE_COLOR_FORMAT_COMPONENTS__MASK |
            COND(color_supertiled, VIVS_PE_COLOR_FORMAT_SUPER_TILED);

         nr_samples_color = res->base.nr_samples;
         if (nr_samples_color <= 1)
            cs->PE_COLOR_FORMAT |= VIVS_PE_COLOR_FORMAT_OVERWRITE;

         if (VIV_FEATURE(screen, ETNA_FEATURE_CACHE128B256BPERLINE))
            cs->PE_COLOR_FORMAT |= COND(color_supertiled, VIVS_PE_COLOR_FORMAT_SUPER_TILED_NEW);
         /* VIVS_PE_COLOR_FORMAT_COMPONENTS() and
         * VIVS_PE_COLOR_FORMAT_OVERWRITE comes from blend_state
         * but only if we set the bits above. */
         /* merged with depth_stencil_alpha */

         cs->PE_COLOR_STRIDE = level->stride;

         if (level->ts_size) {
            cs->TS_COLOR_CLEAR_VALUE = level->clear_value;
            cs->TS_COLOR_CLEAR_VALUE_EXT = level->clear_value >> 32;

            cs->TS_COLOR_STATUS_BASE.bo = res->ts_bo;
            cs->TS_COLOR_STATUS_BASE.offset = level->ts_offset;
            cs->TS_COLOR_STATUS_BASE.flags = ETNA_RELOC_READ | ETNA_RELOC_WRITE;

            pe_mem_config |= VIVS_PE_MEM_CONFIG_COLOR_TS_MODE(level->ts_mode);

            if (level->ts_compress_fmt >= 0) {
               /* overwrite bit breaks v1/v2 compression */
               if (!screen->specs.v4_compression)
                  cs->PE_COLOR_FORMAT &= ~VIVS_PE_COLOR_FORMAT_OVERWRITE;

               ts_mem_config |=
                  VIVS_TS_MEM_CONFIG_COLOR_COMPRESSION |
                  VIVS_TS_MEM_CONFIG_COLOR_COMPRESSION_FORMAT(level->ts_compress_fmt);
            }
         }

         if (util_format_is_srgb(surf->format))
            pe_logic_op |= VIVS_PE_LOGIC_OP_SRGB;
      } else {
         cs->PE_RT_CONFIG[rt - 1] =
            RT_CONFIG_STRIDE(level->stride) |
            RT_CONFIG_FORMAT(fmt) |
            COND(color_supertiled, RT_CONFIG_SUPER_TILED);

         if (VIV_FEATURE(screen, ETNA_FEATURE_CACHE128B256BPERLINE))
            cs->PE_RT_CONFIG[rt - 1] |= COND(color_supertiled, RT_CONFIG_SUPER_TILED_NEW);

         if (level->ts_size) {
            cs->RT_TS_MEM_CONFIG[rt - 1] =
               COND(level->ts_compress_fmt >= 0, VIVS_TS_RT_CONFIG_COMPRESSION) |
               COND(level->ts_compress_fmt >= 0, VIVS_TS_RT_CONFIG_COMPRESSION_FORMAT(level->ts_compress_fmt));

            cs->RT_TS_COLOR_CLEAR_VALUE[rt - 1] = level->clear_value;
            cs->RT_TS_COLOR_CLEAR_VALUE_EXT[rt - 1] = level->clear_value >> 32;

            cs->RT_TS_COLOR_STATUS_BASE[rt - 1].bo = res->ts_bo;
            cs->RT_TS_COLOR_STATUS_BASE[rt - 1].offset = level->ts_offset;
            cs->RT_TS_COLOR_STATUS_BASE[rt - 1].flags = ETNA_RELOC_READ | ETNA_RELOC_WRITE;
         } else {
            if (VIV_FEATURE(screen, ETNA_FEATURE_CACHE128B256BPERLINE))
               cs->PE_RT_CONFIG[rt - 1] |= RT_CONFIG_UNK27;
         }
      }

      static_assert((VIVS_PS_CONTROL_SATURATE_RT0 << 0) == VIVS_PS_CONTROL_SATURATE_RT0, "VIVS_PS_CONTROL_SATURATE_RT0");
      static_assert((VIVS_PS_CONTROL_SATURATE_RT0 << 1) == VIVS_PS_CONTROL_SATURATE_RT1, "VIVS_PS_CONTROL_SATURATE_RT1");
      static_assert((VIVS_PS_CONTROL_SATURATE_RT0 << 2) == VIVS_PS_CONTROL_SATURATE_RT2, "VIVS_PS_CONTROL_SATURATE_RT2");
      static_assert((VIVS_PS_CONTROL_SATURATE_RT0 << 3) == VIVS_PS_CONTROL_SATURATE_RT3, "VIVS_PS_CONTROL_SATURATE_RT3");

      static_assert((VIVS_PS_OUTPUT_REG2_SATURATE_RT4 << 0) == VIVS_PS_OUTPUT_REG2_SATURATE_RT4, "VIVS_PS_OUTPUT_REG2_SATURATE_RT4");
      static_assert((VIVS_PS_OUTPUT_REG2_SATURATE_RT4 << 8) == VIVS_PS_OUTPUT_REG2_SATURATE_RT5, "VIVS_PS_OUTPUT_REG2_SATURATE_RT5");
      static_assert((VIVS_PS_OUTPUT_REG2_SATURATE_RT4 << 16) == VIVS_PS_OUTPUT_REG2_SATURATE_RT6, "VIVS_PS_OUTPUT_REG2_SATURATE_RT6");
      static_assert(((uint32_t)VIVS_PS_OUTPUT_REG2_SATURATE_RT4 << 24) == VIVS_PS_OUTPUT_REG2_SATURATE_RT7, "VIVS_PS_OUTPUT_REG2_SATURATE_RT7");

      if (rt < 4)
         cs->PS_CONTROL |= COND(util_format_is_unorm(surf->format), VIVS_PS_CONTROL_SATURATE_RT0 << rt);
      else
         cs->PS_OUTPUT_REG2 |= COND(util_format_is_unorm(surf->format), VIVS_PS_OUTPUT_REG2_SATURATE_RT4 << (8 * (rt - 4)));

      static_assert((VIVS_PS_CONTROL_EXT_OUTPUT_MODE0__MASK << 4) == VIVS_PS_CONTROL_EXT_OUTPUT_MODE1__MASK, "VIVS_PS_CONTROL_EXT_OUTPUT_MODE1__MASK");
      static_assert((VIVS_PS_CONTROL_EXT_OUTPUT_MODE0__MASK << 8) == VIVS_PS_CONTROL_EXT_OUTPUT_MODE2__MASK, "VIVS_PS_CONTROL_EXT_OUTPUT_MODE2__MASK");
      static_assert((VIVS_PS_CONTROL_EXT_OUTPUT_MODE0__MASK << 12) == VIVS_PS_CONTROL_EXT_OUTPUT_MODE3__MASK, "VIVS_PS_CONTROL_EXT_OUTPUT_MODE3__MASK");
      static_assert((VIVS_PS_CONTROL_EXT_OUTPUT_MODE0__MASK << 16) == VIVS_PS_CONTROL_EXT_OUTPUT_MODE4__MASK, "VIVS_PS_CONTROL_EXT_OUTPUT_MODE4__MASK");
      static_assert((VIVS_PS_CONTROL_EXT_OUTPUT_MODE0__MASK << 20) == VIVS_PS_CONTROL_EXT_OUTPUT_MODE5__MASK, "VIVS_PS_CONTROL_EXT_OUTPUT_MODE5__MASK");
      static_assert((VIVS_PS_CONTROL_EXT_OUTPUT_MODE0__MASK << 24) == VIVS_PS_CONTROL_EXT_OUTPUT_MODE6__MASK, "VIVS_PS_CONTROL_EXT_OUTPUT_MODE6__MASK");
      static_assert((VIVS_PS_CONTROL_EXT_OUTPUT_MODE0__MASK << 28) == VIVS_PS_CONTROL_EXT_OUTPUT_MODE7__MASK, "VIVS_PS_CONTROL_EXT_OUTPUT_MODE7__MASK");

      cs->PS_CONTROL_EXT |=
         translate_output_mode(surf->format, screen->info->halti >= 5) << (4 * rt);

      /* When there are null render targets we need to modify the fragment
       * shader output mapping.
       */
      assert(rt < ARRAY_SIZE(cs->ps_output_remap));
      cs->ps_output_remap[rt] = i;

      rt++;
   }

   cs->num_rt = rt;

   if (rt)
      cs->PS_CONTROL |= VIVS_PS_CONTROL_RT_COUNT(rt - 1);

   /*
    * Disable the main render target if needed. The extra render targets are implicitly
    * disabled by using VIVS_PS_CONTROL_RT_COUNT(0).
    */
   if (fb->nr_cbufs == 0) {
      /* Clearing VIVS_PE_COLOR_FORMAT_COMPONENTS__MASK and
       * VIVS_PE_COLOR_FORMAT_OVERWRITE prevents us from overwriting the
       * color target */
      cs->PE_COLOR_FORMAT = VIVS_PE_COLOR_FORMAT_OVERWRITE;
      cs->PE_COLOR_STRIDE = 0;
      cs->TS_COLOR_STATUS_BASE.bo = NULL;

      for (int i = 0; i < screen->specs.pixel_pipes; i++)
         cs->PE_RT_PIPE_COLOR_ADDR[0][i] = screen->dummy_rt_reloc;
   }

   if (fb->zsbuf.texture != NULL) {
      const struct pipe_surface *surf = &fb->zsbuf;
      struct etna_resource *res = etna_resource_get_render_compatible(pctx, surf->texture);
      struct etna_resource_level *level = &res->levels[surf->level];

      etna_update_render_surface(pctx, surf);

      assert(res->layout &ETNA_LAYOUT_BIT_TILE); /* Cannot render to linear surfaces */

      uint32_t depth_format = translate_depth_format(surf->format);
      unsigned depth_bits =
         depth_format == VIVS_PE_DEPTH_CONFIG_DEPTH_FORMAT_D16 ? 16 : 24;
      bool depth_supertiled = (res->layout & ETNA_LAYOUT_BIT_SUPER) != 0;

      if (depth_bits == 16)
         target_16bpp = true;

      cs->depth_mrd = util_get_depth_format_mrd(util_format_description(surf->format));

      cs->PE_DEPTH_CONFIG =
         depth_format |
         COND(depth_supertiled, VIVS_PE_DEPTH_CONFIG_SUPER_TILED) |
         VIVS_PE_DEPTH_CONFIG_DEPTH_MODE_Z |
         VIVS_PE_DEPTH_CONFIG_UNK18; /* something to do with clipping? */
      /* VIVS_PE_DEPTH_CONFIG_ONLY_DEPTH */
      /* merged with depth_stencil_alpha */

      for (int i = 0; i < screen->specs.pixel_pipes; i++) {
         cs->PE_PIPE_DEPTH_ADDR[i].bo = res->bo;
         cs->PE_PIPE_DEPTH_ADDR[i].offset = level->offset + surf->first_layer * level->layer_stride;
         if (!screen->specs.single_buffer)
            cs->PE_PIPE_DEPTH_ADDR[i].offset += level->layer_stride / screen->specs.pixel_pipes * i;
         cs->PE_PIPE_DEPTH_ADDR[i].flags = ETNA_RELOC_READ | ETNA_RELOC_WRITE;
      }

      cs->PE_DEPTH_STRIDE = level->stride;
      cs->PE_HDEPTH_CONTROL = VIVS_PE_HDEPTH_CONTROL_FORMAT_DISABLED;
      cs->PE_DEPTH_NORMALIZE = fui(exp2f(depth_bits) - 1.0f);

      if (level->ts_size) {
         cs->TS_DEPTH_CLEAR_VALUE = level->clear_value;

         cs->TS_DEPTH_STATUS_BASE.bo = res->ts_bo;
         cs->TS_DEPTH_STATUS_BASE.offset = level->ts_offset;
         cs->TS_DEPTH_STATUS_BASE.flags = ETNA_RELOC_READ | ETNA_RELOC_WRITE;

         pe_mem_config |= VIVS_PE_MEM_CONFIG_DEPTH_TS_MODE(level->ts_mode);

         if (level->ts_compress_fmt >= 0) {
            ts_mem_config |=
               VIVS_TS_MEM_CONFIG_DEPTH_COMPRESSION |
               COND(level->ts_compress_fmt == COMPRESSION_FORMAT_D24S8,
                    VIVS_TS_MEM_CONFIG_STENCIL_ENABLE);
         }
      }

      ts_mem_config |= COND(depth_bits == 16, VIVS_TS_MEM_CONFIG_DEPTH_16BPP);

      nr_samples_depth = res->base.nr_samples;
   } else {
      cs->depth_mrd = 0.0f;
      cs->PE_DEPTH_CONFIG = VIVS_PE_DEPTH_CONFIG_DEPTH_MODE_NONE;
      cs->PE_DEPTH_STRIDE = 0;
      cs->TS_DEPTH_STATUS_BASE.bo = NULL;

      for (int i = 0; i < ETNA_MAX_PIXELPIPES; i++)
         cs->PE_PIPE_DEPTH_ADDR[i].bo = NULL;
   }

   /* MSAA setup */
   if (nr_samples_depth != -1 && nr_samples_color != -1 &&
       nr_samples_depth != nr_samples_color) {
      BUG("Number of samples in color and depth texture must match (%i and %i respectively)",
          nr_samples_color, nr_samples_depth);
   }

   switch (MAX2(nr_samples_depth, nr_samples_color)) {
   case 0:
   case 1: /* Are 0 and 1 samples allowed? */
      cs->GL_MULTI_SAMPLE_CONFIG =
         VIVS_GL_MULTI_SAMPLE_CONFIG_MSAA_SAMPLES_NONE;
      cs->msaa_mode = false;
      break;
   case 2:
      cs->GL_MULTI_SAMPLE_CONFIG = VIVS_GL_MULTI_SAMPLE_CONFIG_MSAA_SAMPLES_2X;
      cs->msaa_mode = true; /* Add input to PS */
      cs->RA_MULTISAMPLE_UNK00E04 = 0x0;
      cs->RA_MULTISAMPLE_UNK00E10[0] = 0x0000aa22;
      cs->RA_CENTROID_TABLE[0] = 0x66aa2288;
      cs->RA_CENTROID_TABLE[1] = 0x88558800;
      cs->RA_CENTROID_TABLE[2] = 0x88881100;
      cs->RA_CENTROID_TABLE[3] = 0x33888800;
      break;
   case 4:
      cs->GL_MULTI_SAMPLE_CONFIG = VIVS_GL_MULTI_SAMPLE_CONFIG_MSAA_SAMPLES_4X;
      cs->msaa_mode = true; /* Add input to PS */
      cs->RA_MULTISAMPLE_UNK00E04 = 0x0;
      cs->RA_MULTISAMPLE_UNK00E10[0] = 0xeaa26e26;
      cs->RA_MULTISAMPLE_UNK00E10[1] = 0xe6ae622a;
      cs->RA_MULTISAMPLE_UNK00E10[2] = 0xaaa22a22;
      cs->RA_CENTROID_TABLE[0] = 0x4a6e2688;
      cs->RA_CENTROID_TABLE[1] = 0x888888a2;
      cs->RA_CENTROID_TABLE[2] = 0x888888ea;
      cs->RA_CENTROID_TABLE[3] = 0x888888c6;
      cs->RA_CENTROID_TABLE[4] = 0x46622a88;
      cs->RA_CENTROID_TABLE[5] = 0x888888ae;
      cs->RA_CENTROID_TABLE[6] = 0x888888e6;
      cs->RA_CENTROID_TABLE[7] = 0x888888ca;
      cs->RA_CENTROID_TABLE[8] = 0x262a2288;
      cs->RA_CENTROID_TABLE[9] = 0x886688a2;
      cs->RA_CENTROID_TABLE[10] = 0x888866aa;
      cs->RA_CENTROID_TABLE[11] = 0x668888a6;
      if (VIV_FEATURE(screen, ETNA_FEATURE_SMALL_MSAA))
         pe_logic_op |= VIVS_PE_LOGIC_OP_UNK24(0x5);
      break;
   }

   cs->TS_MEM_CONFIG = ts_mem_config;
   cs->PE_MEM_CONFIG = pe_mem_config;

   /* Single buffer setup. There is only one switch for this, not a separate
    * one per color buffer / depth buffer. To keep the logic simple always use
    * single buffer when this feature is available.
    */
   if (unlikely(target_linear))
      pe_logic_op |= VIVS_PE_LOGIC_OP_SINGLE_BUFFER(1);
   else if (screen->specs.single_buffer)
      pe_logic_op |= VIVS_PE_LOGIC_OP_SINGLE_BUFFER(target_16bpp ? 3 : 2);
   cs->PE_LOGIC_OP = pe_logic_op;

   /* keep copy of original structure */
   util_copy_framebuffer_state(&ctx->framebuffer_s, fb);
   ctx->dirty |= ETNA_DIRTY_FRAMEBUFFER | ETNA_DIRTY_DERIVE_TS;
}

static void
etna_set_polygon_stipple(struct pipe_context *pctx,
      const struct pipe_poly_stipple *stipple)
{
   /* NOP */
}

static void
etna_set_scissor_states(struct pipe_context *pctx, unsigned start_slot,
      unsigned num_scissors, const struct pipe_scissor_state *ss)
{
   struct etna_context *ctx = etna_context(pctx);
   assert(ss->minx <= ss->maxx);
   assert(ss->miny <= ss->maxy);

   ctx->scissor = *ss;
   ctx->dirty |= ETNA_DIRTY_SCISSOR;
}

static void
etna_set_viewport_states(struct pipe_context *pctx, unsigned start_slot,
      unsigned num_scissors, const struct pipe_viewport_state *vs)
{
   struct etna_context *ctx = etna_context(pctx);
   struct compiled_viewport_state *cs = &ctx->viewport;

   ctx->viewport_s = *vs;
   /**
    * For Vivante GPU, viewport z transformation is 0..1 to 0..1 instead of
    * -1..1 to 0..1.
    * scaling and translation to 0..1 already happened, so remove that
    *
    * z' = (z * 2 - 1) * scale + translate
    *    = z * (2 * scale) + (translate - scale)
    *
    * scale' = 2 * scale
    * translate' = translate - scale
    */

   /* must be fixp as v4 state deltas assume it is */
   cs->PA_VIEWPORT_SCALE_X = etna_f32_to_fixp16(vs->scale[0]);
   cs->PA_VIEWPORT_SCALE_Y = etna_f32_to_fixp16(vs->scale[1]);
   cs->PA_VIEWPORT_SCALE_Z = fui(vs->scale[2] * 2.0f);
   cs->PA_VIEWPORT_OFFSET_X = etna_f32_to_fixp16(vs->translate[0]);
   cs->PA_VIEWPORT_OFFSET_Y = etna_f32_to_fixp16(vs->translate[1]);
   cs->PA_VIEWPORT_OFFSET_Z = fui(vs->translate[2] - vs->scale[2]);

   /* Compute scissor rectangle (fixp) from viewport.
    * Make sure left is always < right and top always < bottom.
    */
   cs->SE_SCISSOR_LEFT = MAX2(vs->translate[0] - fabsf(vs->scale[0]), 0.0f);
   cs->SE_SCISSOR_TOP = MAX2(vs->translate[1] - fabsf(vs->scale[1]), 0.0f);
   cs->SE_SCISSOR_RIGHT = ceilf(MAX2(vs->translate[0] + fabsf(vs->scale[0]), 0.0f));
   cs->SE_SCISSOR_BOTTOM = ceilf(MAX2(vs->translate[1] + fabsf(vs->scale[1]), 0.0f));

   cs->PE_DEPTH_NEAR = fui(0.0); /* not affected if depth mode is Z (as in GL) */
   cs->PE_DEPTH_FAR = fui(1.0);
   ctx->dirty |= ETNA_DIRTY_VIEWPORT;
}

static void
etna_set_vertex_buffers(struct pipe_context *pctx, unsigned num_buffers,
                        const struct pipe_vertex_buffer *vb)
{
   struct etna_context *ctx = etna_context(pctx);
   struct etna_vertexbuf_state *so = &ctx->vertex_buffer;

   util_set_vertex_buffers_mask(so->vb, &so->enabled_mask, vb, num_buffers,
                                true);
   so->count = util_last_bit(so->enabled_mask);

   if (!num_buffers) {
      so->count = 1;
      so->cvb[0].FE_VERTEX_STREAM_BASE_ADDR.bo = ctx->screen->dummy_bo;
      so->cvb[0].FE_VERTEX_STREAM_BASE_ADDR.offset = 0;
      so->cvb[0].FE_VERTEX_STREAM_BASE_ADDR.flags = ETNA_RELOC_READ;
   }

   for (unsigned idx = 0; idx < num_buffers; ++idx) {
      struct compiled_set_vertex_buffer *cs = &so->cvb[idx];
      struct pipe_vertex_buffer *vbi = &so->vb[idx];

      assert(!vbi->is_user_buffer); /* XXX support user_buffer using
                                       etna_usermem_map */

      if (vbi->buffer.resource) { /* GPU buffer */
         cs->FE_VERTEX_STREAM_BASE_ADDR.bo = etna_buffer_resource(vbi->buffer.resource)->bo;
         cs->FE_VERTEX_STREAM_BASE_ADDR.offset = vbi->buffer_offset;
         cs->FE_VERTEX_STREAM_BASE_ADDR.flags = ETNA_RELOC_READ;
      } else {
         cs->FE_VERTEX_STREAM_BASE_ADDR.bo = NULL;
      }
   }

   ctx->dirty |= ETNA_DIRTY_VERTEX_BUFFERS;
}

static void
etna_blend_state_bind(struct pipe_context *pctx, void *bs)
{
   struct etna_context *ctx = etna_context(pctx);

   ctx->blend = bs;
   ctx->dirty |= ETNA_DIRTY_BLEND;
}

static void
etna_blend_state_delete(struct pipe_context *pctx, void *bs)
{
   FREE(bs);
}

static void
etna_rasterizer_state_bind(struct pipe_context *pctx, void *rs)
{
   struct etna_context *ctx = etna_context(pctx);

   ctx->rasterizer = rs;
   ctx->dirty |= ETNA_DIRTY_RASTERIZER;
}

static void
etna_rasterizer_state_delete(struct pipe_context *pctx, void *rs)
{
   FREE(rs);
}

static void
etna_zsa_state_bind(struct pipe_context *pctx, void *zs)
{
   struct etna_context *ctx = etna_context(pctx);

   ctx->zsa = zs;
   ctx->dirty |= ETNA_DIRTY_ZSA;
}

static void
etna_zsa_state_delete(struct pipe_context *pctx, void *zs)
{
   FREE(zs);
}

/** Create vertex element states, which define a layout for fetching
 * vertices for rendering.
 */
static void *
etna_vertex_elements_state_create(struct pipe_context *pctx,
      unsigned num_elements, const struct pipe_vertex_element *elements)
{
   struct etna_context *ctx = etna_context(pctx);
   struct etna_screen *screen = ctx->screen;
   struct compiled_vertex_elements_state *cs = CALLOC_STRUCT(compiled_vertex_elements_state);

   if (!cs)
      return NULL;

   if (num_elements > screen->specs.vertex_max_elements) {
      BUG("number of elements (%u) exceeds chip maximum (%u)", num_elements,
          screen->specs.vertex_max_elements);
      FREE(cs);
      return NULL;
   }

   /* XXX could minimize number of consecutive stretches here by sorting, and
    * permuting the inputs in shader or does Mesa do this already? */

   if (!num_elements) {
      /* There's no way to disable all elements on the hardware, so we need to
       * plug in a dummy element and vertex buffer (stride = 0, so only fetches
       * first location). */
      static const struct pipe_vertex_element dummy_element = {
         .src_format = PIPE_FORMAT_R8G8B8A8_UNORM,
      };

      elements = &dummy_element;
      num_elements = 1;
   }

   cs->num_elements = num_elements;

   unsigned start_offset = 0; /* start of current consecutive stretch */
   bool nonconsecutive = true; /* previous value of nonconsecutive */
   uint32_t buffer_mask = 0; /* mask of buffer_idx already seen */

   for (unsigned idx = 0; idx < num_elements; ++idx) {
      unsigned buffer_idx = elements[idx].vertex_buffer_index;
      unsigned element_size = util_format_get_blocksize(elements[idx].src_format);
      unsigned end_offset = elements[idx].src_offset + element_size;
      uint32_t format_type, normalize;

      if (nonconsecutive)
         start_offset = elements[idx].src_offset;

      /* guaranteed by pipe_caps.max_vertex_buffers */
      assert(buffer_idx < screen->info->gpu.stream_count);

      /* maximum vertex size is 256 bytes */
      assert(element_size != 0 && (end_offset - start_offset) < 256);

      /* check whether next element is consecutive to this one */
      nonconsecutive = (idx == (num_elements - 1)) ||
                       elements[idx + 1].vertex_buffer_index != buffer_idx ||
                       end_offset != elements[idx + 1].src_offset;

      format_type = translate_vertex_format_type(elements[idx].src_format);
      normalize = translate_vertex_format_normalize(elements[idx].src_format);

      assert(format_type != ETNA_NO_MATCH);
      assert(normalize != ETNA_NO_MATCH);

      if (screen->info->halti < 5) {
         cs->FE_VERTEX_ELEMENT_CONFIG[idx] =
            COND(nonconsecutive, VIVS_FE_VERTEX_ELEMENT_CONFIG_NONCONSECUTIVE) |
            format_type |
            VIVS_FE_VERTEX_ELEMENT_CONFIG_NUM(util_format_get_nr_components(elements[idx].src_format)) |
            normalize | VIVS_FE_VERTEX_ELEMENT_CONFIG_ENDIAN(ENDIAN_MODE_NO_SWAP) |
            VIVS_FE_VERTEX_ELEMENT_CONFIG_STREAM(buffer_idx) |
            VIVS_FE_VERTEX_ELEMENT_CONFIG_START(elements[idx].src_offset) |
            VIVS_FE_VERTEX_ELEMENT_CONFIG_END(end_offset - start_offset);
      } else { /* HALTI5 spread vertex attrib config over two registers */
         cs->NFE_GENERIC_ATTRIB_CONFIG0[idx] =
            format_type |
            VIVS_NFE_GENERIC_ATTRIB_CONFIG0_NUM(util_format_get_nr_components(elements[idx].src_format)) |
            normalize | VIVS_NFE_GENERIC_ATTRIB_CONFIG0_ENDIAN(ENDIAN_MODE_NO_SWAP) |
            VIVS_NFE_GENERIC_ATTRIB_CONFIG0_STREAM(buffer_idx) |
            VIVS_NFE_GENERIC_ATTRIB_CONFIG0_START(elements[idx].src_offset);
         cs->NFE_GENERIC_ATTRIB_CONFIG1[idx] =
            COND(nonconsecutive, VIVS_NFE_GENERIC_ATTRIB_CONFIG1_NONCONSECUTIVE) |
            VIVS_NFE_GENERIC_ATTRIB_CONFIG1_END(end_offset - start_offset);
      }
      cs->FE_VERTEX_STREAM_CONTROL[buffer_idx] =
            FE_VERTEX_STREAM_CONTROL_VERTEX_STRIDE(elements[idx].src_stride);

      if (util_format_is_pure_integer(elements[idx].src_format))
         cs->NFE_GENERIC_ATTRIB_SCALE[idx] = 1;
      else
         cs->NFE_GENERIC_ATTRIB_SCALE[idx] = fui(1.0f);

      /* instance_divisor is part of elements state but should be the same for all buffers */
      if (buffer_mask & 1 << buffer_idx)
         assert(cs->NFE_VERTEX_STREAMS_VERTEX_DIVISOR[buffer_idx] == elements[idx].instance_divisor);
      else
         cs->NFE_VERTEX_STREAMS_VERTEX_DIVISOR[buffer_idx] = elements[idx].instance_divisor;

      buffer_mask |= 1 << buffer_idx;
      cs->num_buffers = MAX2(cs->num_buffers, buffer_idx + 1);
   }

   return cs;
}

static void
etna_vertex_elements_state_delete(struct pipe_context *pctx, void *ve)
{
   FREE(ve);
}

static void
etna_vertex_elements_state_bind(struct pipe_context *pctx, void *ve)
{
   struct etna_context *ctx = etna_context(pctx);

   ctx->vertex_elements = ve;
   ctx->dirty |= ETNA_DIRTY_VERTEX_ELEMENTS;
}

static void
etna_set_stream_output_targets(struct pipe_context *pctx,
      unsigned num_targets, struct pipe_stream_output_target **targets,
      const unsigned *offsets,
      enum mesa_prim output_prim)
{
   /* stub */
}

static bool
etna_update_ts_config(struct etna_context *ctx)
{
   const struct pipe_framebuffer_state *fb = &ctx->framebuffer_s;
   bool dirty = ctx->dirty & ETNA_DIRTY_FRAMEBUFFER;
   unsigned rt = 0;

   for (unsigned i = 0; i < fb->nr_cbufs; i++) {
      struct etna_resource_level *level;
      struct etna_resource *res;
      uint32_t ts_config;

      if (!fb->cbufs[i].texture)
         continue;

      res = etna_resource_get_render_compatible(&ctx->base, fb->cbufs[i].texture);
      level = &res->levels[fb->cbufs[i].level];

      /* Read the current ts config value for the render target. */
      if (rt == 0)
         ts_config = ctx->framebuffer.TS_MEM_CONFIG;
      else
         ts_config = ctx->framebuffer.RT_TS_MEM_CONFIG[rt - 1];

      /* Update the ts config for color fast clear. */
      if (etna_resource_level_ts_valid(level)) {
         if (rt == 0)
            ts_config |= VIVS_TS_MEM_CONFIG_COLOR_FAST_CLEAR;
         else
            ts_config |= VIVS_TS_RT_CONFIG_ENABLE;
      } else {
         if (rt == 0)
            ts_config &= ~VIVS_TS_MEM_CONFIG_COLOR_FAST_CLEAR;
         else
            ts_config &= ~VIVS_TS_RT_CONFIG_ENABLE;
      }

      /* Update dirty state and if needed store the new ts config value. */
      if (rt == 0)
         dirty |= ctx->framebuffer.TS_MEM_CONFIG != ts_config;
      else
         dirty |= ctx->framebuffer.RT_TS_MEM_CONFIG[rt] != ts_config;

      if (dirty) {
         if (rt == 0)
            ctx->framebuffer.TS_MEM_CONFIG = ts_config;
         else
            ctx->framebuffer.RT_TS_MEM_CONFIG[rt - 1] = ts_config;
      }

      rt++;
   }

   /* Update the ts config for depth fast clear. */
   if (ctx->framebuffer_s.zsbuf.texture) {
      struct etna_resource *res = etna_resource_get_render_compatible(&ctx->base, fb->zsbuf.texture);
      struct etna_resource_level *level = &res->levels[fb->zsbuf.level];
      uint32_t ts_config = ctx->framebuffer.TS_MEM_CONFIG;

      if (etna_resource_level_ts_valid(level))
         ts_config |= VIVS_TS_MEM_CONFIG_DEPTH_FAST_CLEAR;
      else
         ts_config &= ~VIVS_TS_MEM_CONFIG_DEPTH_FAST_CLEAR;

      dirty |= ctx->framebuffer.TS_MEM_CONFIG != ts_config;

      if (dirty)
         ctx->framebuffer.TS_MEM_CONFIG = ts_config;
   }

   if (dirty)
      ctx->dirty |= ETNA_DIRTY_TS;

   ctx->dirty &= ~ETNA_DIRTY_DERIVE_TS;

   return true;
}

static bool
etna_update_clipping(struct etna_context *ctx)
{
   const struct etna_rasterizer_state *rasterizer = etna_rasterizer_state(ctx->rasterizer);
   const struct pipe_framebuffer_state *fb = &ctx->framebuffer_s;

   if (ctx->rasterizer->rasterizer_discard) {
      ctx->clipping.minx = 0;
      ctx->clipping.miny = 0;
      ctx->clipping.maxx = 0;
      ctx->clipping.maxy = 0;
   } else {
      /* clip framebuffer against viewport */
      uint32_t scissor_left = ctx->viewport.SE_SCISSOR_LEFT;
      uint32_t scissor_top = ctx->viewport.SE_SCISSOR_TOP;
      uint32_t scissor_right = MIN2(fb->width, ctx->viewport.SE_SCISSOR_RIGHT);
      uint32_t scissor_bottom = MIN2(fb->height, ctx->viewport.SE_SCISSOR_BOTTOM);

      /* clip against scissor */
      if (rasterizer->scissor) {
         scissor_left = MAX2(ctx->scissor.minx, scissor_left);
         scissor_top = MAX2(ctx->scissor.miny, scissor_top);
         scissor_right = MIN2(ctx->scissor.maxx, scissor_right);
         scissor_bottom = MIN2(ctx->scissor.maxy, scissor_bottom);
      }

      ctx->clipping.minx = scissor_left;
      ctx->clipping.miny = scissor_top;
      ctx->clipping.maxx = scissor_right;
      ctx->clipping.maxy = scissor_bottom;
   }

   ctx->dirty |= ETNA_DIRTY_SCISSOR_CLIP;

   return true;
}

static bool
etna_update_zsa(struct etna_context *ctx)
{
   struct pipe_framebuffer_state *fb = &ctx->framebuffer_s;
   struct compiled_shader_state *shader_state = &ctx->shader_state;
   struct pipe_depth_stencil_alpha_state *zsa_state = ctx->zsa;
   struct etna_zsa_state *zsa = etna_zsa_state(zsa_state);
   struct etna_screen *screen = ctx->screen;
   uint32_t new_pe_depth, new_ra_depth;
   bool early_z_allowed = !VIV_FEATURE(screen, ETNA_FEATURE_NO_EARLY_Z);
   bool late_zs = false, early_zs = false,
        late_z_test = false, early_z_test = false;

   /* Linear PE breaks the combination of early test with late write, as it
    * seems RA and PE disagree about the buffer layout in this mode. Fall back
    * to late Z always even though early Z write might be possible, as we don't
    * know if any other draws to the same surface require late Z write.
    */
   for (unsigned i = 0; i < fb->nr_cbufs; i++) {
      struct etna_resource *res;

      if (!fb->cbufs[i].texture)
         continue;

      res = etna_resource_get_render_compatible(&ctx->base, fb->cbufs[i].texture);

      if (res->layout == ETNA_LAYOUT_LINEAR)
         early_z_allowed = false;

      /* Stop after the first render target. */
      break;
   }

   if (zsa->z_write_enabled || zsa->stencil_enabled) {
      if (VIV_FEATURE(screen, ETNA_FEATURE_RA_WRITE_DEPTH) &&
          early_z_allowed &&
          !zsa_state->alpha_enabled &&
          !shader_state->writes_z &&
          !shader_state->uses_discard)
         early_zs = true;
      else
         late_zs = true;
   }

   if (zsa->z_test_enabled) {
      if (early_z_allowed &&
          (!zsa->stencil_modified || early_zs) &&
          !shader_state->writes_z)
         early_z_test = true;
      else
         late_z_test = true;
   }

   new_pe_depth = VIVS_PE_DEPTH_CONFIG_DEPTH_FUNC(zsa->z_test_enabled ?
                     /* compare funcs have 1 to 1 mapping */
                     zsa_state->depth_func : PIPE_FUNC_ALWAYS) |
                  COND(zsa->z_write_enabled, VIVS_PE_DEPTH_CONFIG_WRITE_ENABLE) |
                  COND(early_z_test, VIVS_PE_DEPTH_CONFIG_EARLY_Z) |
                  COND(!late_zs && !late_z_test,
                       VIVS_PE_DEPTH_CONFIG_DISABLE_ZS);

   /* blob sets this to 0x40000031 on GC7000, seems to make no difference,
    * but keep it in mind if depth behaves strangely. */
   new_ra_depth = 0x0000030 |
                  COND(early_z_test, VIVS_RA_EARLY_DEPTH_TEST_ENABLE);

   if (VIV_FEATURE(screen, ETNA_FEATURE_RA_WRITE_DEPTH)) {
      new_ra_depth |= VIVS_RA_EARLY_DEPTH_FORWARD_W |
                      VIVS_RA_EARLY_DEPTH_FORWARD_Z;

      if (!early_zs)
         new_ra_depth |= VIVS_RA_EARLY_DEPTH_WRITE_DISABLE;

      for (unsigned i = 0; i < fb->nr_cbufs; i++) {
         if (!fb->cbufs[i].texture)
            continue;

         struct pipe_resource *res = fb->cbufs[i].texture;

         if ((late_z_test || late_zs) && res->nr_samples > 1)
            new_ra_depth |= VIVS_RA_EARLY_DEPTH_LATE_DEPTH_MSAA;

         /* Stop after the first render target. */
         break;
      }
   }

   if (new_pe_depth != zsa->PE_DEPTH_CONFIG ||
       new_ra_depth != zsa->RA_DEPTH_CONFIG)
      ctx->dirty |= ETNA_DIRTY_ZSA;

   zsa->PE_DEPTH_CONFIG = new_pe_depth;
   zsa->RA_DEPTH_CONFIG = new_ra_depth;

   return true;
}

static bool
etna_record_flush_resources(struct etna_context *ctx)
{
   struct pipe_framebuffer_state *fb = &ctx->framebuffer_s;

   for (unsigned i = 0; i < fb->nr_cbufs; i++) {
      if (!fb->cbufs[i].texture)
         continue;

      struct etna_resource *rsc = etna_resource(fb->cbufs[i].texture);

      if (rsc->shared && !rsc->explicit_flush)
         etna_context_add_flush_resource(ctx, &rsc->base);
   }

   return true;
}

struct etna_state_updater {
   bool (*update)(struct etna_context *ctx);
   uint32_t dirty;
};

static const struct etna_state_updater etna_state_updates[] = {
   {
      etna_shader_update_vertex, ETNA_DIRTY_SHADER | ETNA_DIRTY_VERTEX_ELEMENTS,
   },
   {
      etna_shader_link, ETNA_DIRTY_SHADER | ETNA_DIRTY_FRAMEBUFFER,
   },
   {
      etna_update_blend, ETNA_DIRTY_BLEND | ETNA_DIRTY_FRAMEBUFFER
   },
   {
      etna_update_blend_color, ETNA_DIRTY_BLEND_COLOR | ETNA_DIRTY_FRAMEBUFFER,
   },
   {
      etna_update_ts_config, ETNA_DIRTY_DERIVE_TS,
   },
   {
      etna_update_clipping, ETNA_DIRTY_SCISSOR | ETNA_DIRTY_FRAMEBUFFER |
                            ETNA_DIRTY_RASTERIZER | ETNA_DIRTY_VIEWPORT,
   },
   {
      etna_update_zsa, ETNA_DIRTY_ZSA | ETNA_DIRTY_SHADER |
                       ETNA_DIRTY_FRAMEBUFFER,
   },
   {
      etna_record_flush_resources, ETNA_DIRTY_FRAMEBUFFER,
   }
};

bool
etna_state_update(struct etna_context *ctx)
{
   for (unsigned int i = 0; i < ARRAY_SIZE(etna_state_updates); i++)
      if (ctx->dirty & etna_state_updates[i].dirty)
         if (!etna_state_updates[i].update(ctx))
            return false;

   return true;
}

void
etna_state_init(struct pipe_context *pctx)
{
   pctx->set_blend_color = etna_set_blend_color;
   pctx->set_stencil_ref = etna_set_stencil_ref;
   pctx->set_clip_state = etna_set_clip_state;
   pctx->set_sample_mask = etna_set_sample_mask;
   pctx->set_constant_buffer = etna_set_constant_buffer;
   pctx->set_framebuffer_state = etna_set_framebuffer_state;
   pctx->set_polygon_stipple = etna_set_polygon_stipple;
   pctx->set_scissor_states = etna_set_scissor_states;
   pctx->set_viewport_states = etna_set_viewport_states;

   pctx->set_vertex_buffers = etna_set_vertex_buffers;

   pctx->bind_blend_state = etna_blend_state_bind;
   pctx->delete_blend_state = etna_blend_state_delete;

   pctx->bind_rasterizer_state = etna_rasterizer_state_bind;
   pctx->delete_rasterizer_state = etna_rasterizer_state_delete;

   pctx->bind_depth_stencil_alpha_state = etna_zsa_state_bind;
   pctx->delete_depth_stencil_alpha_state = etna_zsa_state_delete;

   pctx->create_vertex_elements_state = etna_vertex_elements_state_create;
   pctx->delete_vertex_elements_state = etna_vertex_elements_state_delete;
   pctx->bind_vertex_elements_state = etna_vertex_elements_state_bind;

   pctx->set_stream_output_targets = etna_set_stream_output_targets;
}

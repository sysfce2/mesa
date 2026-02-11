/*
 * Copyright (C) 2026 Collabora, Ltd.
 * SPDX-License-Identifier: MIT
 */

#include "pan_desc.h"
#include "pan_fb.h"

#include "compiler/nir/nir.h"
#include "compiler/nir/nir_builder.h"
#include "compiler/pan_nir.h"

static enum glsl_base_type
mali_to_glsl_dim(enum mali_texture_dimension dim)
{
   switch (dim) {
   case MALI_TEXTURE_DIMENSION_1D: return GLSL_SAMPLER_DIM_1D;
   case MALI_TEXTURE_DIMENSION_CUBE:
   case MALI_TEXTURE_DIMENSION_2D: return GLSL_SAMPLER_DIM_2D;
   case MALI_TEXTURE_DIMENSION_3D: return GLSL_SAMPLER_DIM_3D;
   default:
      UNREACHABLE("Unknown mali_texture_dimension");
   }
}

static enum pan_fb_shader_data_type
data_type_for_format(enum pipe_format format)
{
   if (util_format_is_pure_uint(format))
      return PAN_FB_SHADER_DATA_TYPE_U32;
   else if (util_format_is_pure_sint(format))
      return PAN_FB_SHADER_DATA_TYPE_I32;
   else
      return PAN_FB_SHADER_DATA_TYPE_F32;
}

static nir_alu_type
nir_alu_type_for_data_type(enum pan_fb_shader_data_type data_type)
{
   switch (data_type) {
   case PAN_FB_SHADER_DATA_TYPE_F32:   return nir_type_float32;
   case PAN_FB_SHADER_DATA_TYPE_I32:   return nir_type_int32;
   case PAN_FB_SHADER_DATA_TYPE_U32:   return nir_type_uint32;
   default: UNREACHABLE("Invalid pan_fb_shader_data_type");
   }
   UNREACHABLE("Invalid pan_fb_shader_data_type");
}

static enum pan_fb_shader_op
get_shader_op_for_load(enum pan_fb_load_op op)
{
   switch (op) {
   case PAN_FB_LOAD_NONE:     return PAN_FB_SHADER_DONT_CARE;
   case PAN_FB_LOAD_CLEAR:    return PAN_FB_SHADER_LOAD_CLEAR;
   case PAN_FB_LOAD_IMAGE:    return PAN_FB_SHADER_LOAD_IMAGE;
   case PAN_FB_LOAD_OP_COUNT: UNREACHABLE("Invalid load op");
   }
   UNREACHABLE("Invalid load op");
}

static enum pan_fb_msaa_copy_op
reduce_msaa_op(enum pan_fb_msaa_copy_op msaa,
               uint8_t dst_sample_count, uint8_t src_sample_count)
{
   if (msaa == PAN_FB_MSAA_COPY_ALL)
      assert(src_sample_count == dst_sample_count);

   if (src_sample_count <= 1)
      return PAN_FB_MSAA_COPY_SINGLE;

   return msaa;
}

static const struct pan_fb_shader_key_target key_target_dont_care = {
   .in_bounds_op = PAN_FB_SHADER_DONT_CARE,
   .border_op = PAN_FB_SHADER_DONT_CARE,
};

static const struct pan_fb_shader_key_target key_target_preserve = {
   .in_bounds_op = PAN_FB_SHADER_PRESERVE,
   .border_op = PAN_FB_SHADER_PRESERVE,
};

static struct pan_fb_shader_key_target
get_key_target(enum pipe_format format,
               uint8_t fb_sample_count,
               bool has_border,
               enum pan_fb_shader_op in_bounds_op,
               enum pan_fb_shader_op border_op,
               enum pan_fb_msaa_copy_op msaa,
               const struct pan_image_view *iview)
{
   if (format == PIPE_FORMAT_NONE)
      return key_target_dont_care;

   /* If we have a full framebuffer, there is no border.  Set the boarder load
    * equal to the in-bounds load so things in the shader fold nicely.
    */
   if (!has_border)
      border_op = in_bounds_op;

   if (in_bounds_op == PAN_FB_SHADER_DONT_CARE &&
       border_op == PAN_FB_SHADER_DONT_CARE)
      return key_target_dont_care;

   /* If both load ops can be implemented as a discard, set the target to
    * always PRESERVE and we just won't emit any code to do a load.
    *
    * Importantly, this is true for all load ops except PAN_FB_LOAD_OP_IMAGE.
    */
   if (pan_fb_shader_op_can_discard(in_bounds_op) &&
       pan_fb_shader_op_can_discard(border_op))
      return key_target_preserve;

   /* We don't have a bound on this enum so we need a live assert */
   enum mali_texture_dimension dim = MALI_TEXTURE_DIMENSION_2D;
   bool is_array = false;
   uint8_t image_sample_count = 0;
   if (in_bounds_op == PAN_FB_SHADER_LOAD_IMAGE ||
       border_op == PAN_FB_SHADER_LOAD_IMAGE) {
      if (util_format_has_depth(util_format_description(format)))
         assert(format == util_format_get_depth_only(iview->format));
      else if (util_format_has_stencil(util_format_description(format)))
         assert(util_format_has_stencil(util_format_description(iview->format)));
      else
         assert(format == iview->format);

      dim = iview->dim;
      is_array = iview->first_layer != iview->last_layer;
      image_sample_count = pan_image_view_get_nr_samples(iview);

      msaa = reduce_msaa_op(msaa, fb_sample_count, image_sample_count);
   } else {
      msaa = PAN_FB_MSAA_COPY_ALL;
   }

   assert(((unsigned)dim) < (1 << 2));

   return (struct pan_fb_shader_key_target) {
      .in_bounds_op = in_bounds_op,
      .border_op = border_op,
      .image_msaa = msaa,
      .image_dim = dim,
      .image_is_array = is_array,
      .data_type = data_type_for_format(format),
   };
}

static struct pan_fb_shader_key_target
get_load_key_target(enum pipe_format format,
                    uint8_t fb_sample_count,
                    bool has_border,
                    const struct pan_fb_load_target *load)
{
   return get_key_target(format, fb_sample_count, has_border,
                         get_shader_op_for_load(load->in_bounds_load),
                         get_shader_op_for_load(load->border_load),
                         load->msaa, load->iview);
}

bool
GENX(pan_fb_load_shader_key_fill)(struct pan_fb_shader_key *key,
                                  const struct pan_fb_layout *fb,
                                  const struct pan_fb_load *load,
                                  bool zs_prepass)
{
   *key = (struct pan_fb_shader_key) { };

   const bool has_border = pan_fb_has_partial_tiles(fb);

   /* Z/S can only be written from the prepass and color can only be written
    * from the non-prepass.  Setting everything to zero will cause the shader
    * to just not write anything in that case.
    */
   if (zs_prepass) {
      key->z = get_load_key_target(fb->z_format, fb->sample_count,
                                   has_border, &load->z);
      key->s = get_load_key_target(fb->s_format, fb->sample_count,
                                   has_border, &load->s);
      return pan_fb_shader_key_target_written(&key->z) ||
             pan_fb_shader_key_target_written(&key->s);
   } else {
      bool needs_shader = false;
      for (unsigned rt = 0; rt < fb->rt_count; rt++) {
         key->rts[rt] = get_load_key_target(fb->rt_formats[rt],
                                            fb->sample_count,
                                            has_border, &load->rts[rt]);
         if (pan_fb_shader_key_target_written(&key->rts[rt]))
            needs_shader = true;
      }
      return needs_shader;
   }
}

static nir_def *
build_sample_id(nir_builder *b)
{
   b->shader->info.fs.uses_sample_shading = true;
   return nir_load_sample_id(b);
}

static nir_def *
build_load(nir_builder *b, nir_def *pos,
           enum pan_fb_shader_op op,
           gl_frag_result location,
           const struct pan_fb_shader_key_target *target)
{
   const nir_alu_type nir_type = nir_alu_type_for_data_type(target->data_type);
   const bool is_zs = location == FRAG_RESULT_DEPTH ||
                      location == FRAG_RESULT_STENCIL;

   switch (op) {
   case PAN_FB_SHADER_DONT_CARE:
      return nir_imm_zero(b, is_zs ? 1 : 4, 32);

   case PAN_FB_SHADER_LOAD_CLEAR:
      return nir_load_clear_value_pan(b, is_zs ? 1 : 4, 32,
                                      .io_semantics.location = location,
                                      .dest_type = nir_type);

   case PAN_FB_SHADER_LOAD_IMAGE: {
      assert(pos->num_components == 3);
      switch (target->image_dim) {
      case MALI_TEXTURE_DIMENSION_1D:
         if (target->image_is_array)
            pos = nir_channels(b, pos, 0b101);
         else
            pos = nir_channel(b, pos, 0);
         break;

      case MALI_TEXTURE_DIMENSION_CUBE:
         assert(target->image_is_array);
         break;

      case MALI_TEXTURE_DIMENSION_2D:
         if (!target->image_is_array)
            pos = nir_channels(b, pos, 0b011);
         break;

      case MALI_TEXTURE_DIMENSION_3D:
         break;

      default:
         UNREACHABLE("Unsupported dim");
      }

      nir_def *val;
      if (target->image_msaa == PAN_FB_MSAA_COPY_SINGLE) {
         val = nir_txf(b, pos,
                       .texture_index = location,
                       .dim = mali_to_glsl_dim(target->image_dim),
                       .dest_type = nir_type,
                       .is_array = target->image_is_array);
      } else {
         assert(target->image_dim == MALI_TEXTURE_DIMENSION_2D);

         assert(target->image_msaa == PAN_FB_MSAA_COPY_ALL ||
                target->image_msaa == PAN_FB_MSAA_COPY_SAMPLE_0);
         nir_def *sample_id = target->image_msaa == PAN_FB_MSAA_COPY_ALL
                              ? build_sample_id(b) : nir_imm_int(b, 0);

         val = nir_txf_ms(b, pos, sample_id,
                          .texture_index = location,
                          .dim = GLSL_SAMPLER_DIM_MS,
                          .dest_type = nir_type,
                          .is_array = target->image_is_array);
      }

      if (is_zs)
         val = nir_channel(b, val, 0);

      return val;
   }

   default:
      UNREACHABLE("Unsupported load op");
   }
}

static void
build_op(nir_builder *b, nir_def *pos, nir_def *in_bounds,
         gl_frag_result location,
         const struct pan_fb_shader_key_target *target)
{
   nir_def *val;
   if (target->in_bounds_op == target->border_op) {
      val = build_load(b, pos, target->in_bounds_op, location, target);
   } else {
      nir_def *in_bounds_val, *border_val;
      nir_if *nif = nir_push_if(b, in_bounds);
      {
         in_bounds_val = build_load(b, pos, target->in_bounds_op,
                                    location, target);
      }
      nir_push_else(b, nif);
      {
         border_val = build_load(b, pos, target->border_op,
                                 location, target);
      }
      nir_pop_if(b, nif);
      val = nir_if_phi(b, in_bounds_val, border_val);
   }

   nir_store_output(b, val, nir_imm_int(b, 0),
                    .base = location, .range = 1,
                    .write_mask = nir_component_mask(val->num_components),
                    .src_type = nir_alu_type_for_data_type(target->data_type),
                    .io_semantics.location = location,
                    .io_semantics.num_slots = 1);
}

struct pan_fb_shader_info {
   bool discard_in_bounds;
   bool discard_border;
};

static void
gather_target_info(struct pan_fb_shader_info *info,
                   const struct pan_fb_shader_key_target *target)
{
   /* Ignore any targets we don't write */
   if (!pan_fb_shader_key_target_written(target))
      return;

   if (!pan_fb_shader_op_can_discard(target->in_bounds_op))
      info->discard_in_bounds = false;

   if (!pan_fb_shader_op_can_discard(target->border_op))
      info->discard_border = false;
}

static struct pan_fb_shader_info
gather_shader_info(const struct pan_fb_shader_key *key)
{
   struct pan_fb_shader_info info = {
      .discard_in_bounds = true,
      .discard_border = true,
   };

   for (unsigned rt = 0; rt < PAN_MAX_RTS; rt++)
      gather_target_info(&info, &key->rts[rt]);

   gather_target_info(&info, &key->z);
   gather_target_info(&info, &key->s);

   return info;
}

nir_shader *
GENX(pan_get_fb_shader)(const struct pan_fb_shader_key *key,
                        const struct nir_shader_compiler_options *nir_options)
{
   nir_builder builder = nir_builder_init_simple_shader(
      MESA_SHADER_FRAGMENT, nir_options, "pan-fb-load");
   nir_builder *b = &builder;

   const struct pan_fb_shader_info info = gather_shader_info(key);

   /* We shouldn't even be building a shader at all in this case but, on the
    * off chance that we do, build one that just discards.
    */
   if (info.discard_in_bounds && info.discard_border) {
      nir_terminate(b);
      return builder.shader;
   }

   nir_def *pos_xy = nir_load_pixel_coord(b);
   nir_def *pos_x = nir_u2u32(b, nir_channel(b, pos_xy, 0));
   nir_def *pos_y = nir_u2u32(b, nir_channel(b, pos_xy, 1));
   nir_def *pos = nir_vec3(b, pos_x, pos_y, nir_load_layer_id(b));

   nir_def *ra = nir_load_fb_render_area_pan(b);
   nir_def *ra_min_xy = nir_channels(b, ra, 0b0011);
   nir_def *ra_max_xy = nir_channels(b, ra, 0b1100);

   nir_def *in_bounds =
      nir_ball(b, nir_iand(b, nir_uge(b, pos_xy, ra_min_xy),
                              nir_uge(b, ra_max_xy, pos_xy)));

   if (PAN_ARCH >= 6 && !pan_fb_shader_key_target_written(&key->z) &&
                        !pan_fb_shader_key_target_written(&key->s)) {
      /* We assume that the framebuffer will clear if either load op is set to
       * clear.  For color-only on bifrost, we can do a bit better by emitting
       * BLEND instructions directly only writing the tile buffer if we're
       * actually loading the image.
       */
      for (unsigned rt = 0; rt < PAN_MAX_RTS; rt++) {
         const struct pan_fb_shader_key_target *target = &key->rts[rt];
         const gl_frag_result location = FRAG_RESULT_DATA0 + rt;

         if (!pan_fb_shader_key_target_written(target))
            continue;

         nir_def *coverage = nir_load_cumulative_coverage_pan(b);
         nir_def *z1 = nir_imm_int(b, 0);
         nir_def *u4 = nir_undef(b, 4, 32);

         nir_def *color;
         if (target->in_bounds_op == target->border_op) {
            color = build_load(b, pos, target->in_bounds_op,
                               location, target);
         } else {
            nir_def *ib_color, *bd_color, *ib_cov, *bd_cov;
            nir_if *nif = nir_push_if(b, in_bounds);
            {
               if (pan_fb_shader_op_can_discard(target->in_bounds_op)) {
                  ib_color = u4;
                  ib_cov = z1;
               } else {
                  ib_color = build_load(b, pos, target->in_bounds_op,
                                        location, target);
                  ib_cov = coverage;
               }
            }
            nir_push_else(b, nif);
            {
               if (pan_fb_shader_op_can_discard(target->border_op)) {
                  bd_color = u4;
                  bd_cov = z1;
               } else {
                  bd_color = build_load(b, pos, target->border_op,
                                        location, target);
                  bd_cov = coverage;
               }
            }
            nir_pop_if(b, nif);
            color = nir_if_phi(b, ib_color, bd_color);
            coverage = nir_if_phi(b, ib_cov, bd_cov);
         }

         const nir_alu_type nir_type =
            nir_alu_type_for_data_type(target->data_type);
         nir_def *blend = nir_load_blend_descriptor_pan(b, .base = rt);

         nir_blend_pan(b, coverage, blend, color,
                       .src_type = nir_type,
                       .io_semantics.location = location,
                       .io_semantics.num_slots = 1);
      }
   } else {
      if (info.discard_in_bounds) {
         nir_terminate_if(b, in_bounds);
         in_bounds = nir_imm_false(b);
      } else if (info.discard_border) {
         nir_terminate_if(b, nir_inot(b, in_bounds));
         in_bounds = nir_imm_true(b);
      }

      if (pan_fb_shader_key_target_written(&key->z))
         build_op(b, pos, in_bounds, FRAG_RESULT_DEPTH, &key->z);
      if (pan_fb_shader_key_target_written(&key->s))
         build_op(b, pos, in_bounds, FRAG_RESULT_STENCIL, &key->s);

      for (unsigned rt = 0; rt < PAN_MAX_RTS; rt++) {
         if (pan_fb_shader_key_target_written(&key->rts[rt]))
            build_op(b, pos, in_bounds, FRAG_RESULT_DATA0 + rt, &key->rts[rt]);
      }
   }

   return builder.shader;
}

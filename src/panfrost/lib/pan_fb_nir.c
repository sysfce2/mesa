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

static enum glsl_base_type
glsl_type_for_format(enum pipe_format format)
{
   if (util_format_is_pure_uint(format))
      return GLSL_TYPE_UINT;
   else if (util_format_is_pure_sint(format))
      return GLSL_TYPE_INT;
   else
      return GLSL_TYPE_FLOAT;
}

static struct pan_fb_shader_load_key_target
get_key_target(enum pipe_format format,
               uint8_t fb_sample_count,
               bool has_border,
               enum glsl_base_type glsl_type,
               const struct pan_fb_load_target *load)
{
   const struct pan_fb_shader_load_key_target load_none = {
      .in_bounds_load = PAN_FB_LOAD_NONE,
      .border_load = PAN_FB_LOAD_NONE,
   };

   if (format == PIPE_FORMAT_NONE)
      return load_none;

   enum pan_fb_load_op in_bounds_load = load->in_bounds_load;
   enum pan_fb_load_op border_load = load->border_load;

   /* If we have a full framebuffer, there is no border.  Set the boarder load
    * equal to the in-bounds load so things in the shader fold nicely.
    */
   if (!has_border)
      border_load = in_bounds_load;

   /* If both load ops are CLEAR or NONE, we assume that the clears are
    * handled by the framebuffer itself and we just don't load that
    * attachment.
    */
   if (in_bounds_load != PAN_FB_LOAD_IMAGE &&
       border_load != PAN_FB_LOAD_IMAGE)
      return load_none;

   enum pan_fb_msaa_copy_op msaa = load->msaa;

   /* For MSAA_COPY_ALL, the sample counts have to match */
   if (msaa == PAN_FB_MSAA_COPY_ALL)
      assert(pan_image_view_get_nr_samples(load->iview) == fb_sample_count);

   /* If the target only has one sample, use COPY_SINGLE */
   if (pan_image_view_get_nr_samples(load->iview) <= 1)
      msaa = PAN_FB_MSAA_COPY_SINGLE;

   /* We don't have a bound on this enum so we need a live assert */
   enum glsl_sampler_dim dim = load->iview->dim;
   assert(((unsigned)dim) < (1 << 4));

   bool is_array = load->iview->first_layer != load->iview->last_layer;

   if (util_format_has_depth(util_format_description(format)))
      assert(format == util_format_get_depth_only(load->iview->format));
   else if (util_format_has_stencil(util_format_description(format)))
      assert(util_format_has_stencil(util_format_description(load->iview->format)));
   else
      assert(format == load->iview->format);

   if (glsl_type == GLSL_TYPE_VOID)
      glsl_type = glsl_type_for_format(format);
   assert(((unsigned)glsl_type) < (1 << 5));

   return (struct pan_fb_shader_load_key_target) {
      .in_bounds_load = in_bounds_load,
      .border_load = border_load,
      .msaa = msaa,
      .dim = dim,
      .is_array = is_array,
      .glsl_type = glsl_type,
   };
}

static bool
key_target_has_load(struct pan_fb_shader_load_key_target target)
{
   return target.in_bounds_load != PAN_FB_LOAD_NONE ||
          target.border_load != PAN_FB_LOAD_NONE;
}

bool
GENX(pan_fb_load_shader_key_fill)(struct pan_fb_load_shader_key *key,
                                  const struct pan_fb_layout *fb,
                                  const struct pan_fb_load *load,
                                  bool zs_prepass)
{
   *key = (struct pan_fb_load_shader_key) { };

   const bool has_border = pan_fb_has_partial_tiles(fb);

   /* Z/S can only be written from the prepass and color can only be written
    * from the non-prepass.  Setting everything to zero will cause the shader
    * to just not write anything in that case.
    */
   if (zs_prepass) {
      key->z = get_key_target(fb->z_format, fb->sample_count, has_border,
                              GLSL_TYPE_FLOAT, &load->z);
      key->s = get_key_target(fb->s_format, fb->sample_count, has_border,
                              GLSL_TYPE_INT, &load->s);
      return key_target_has_load(key->z) || key_target_has_load(key->s);
   } else {
      bool has_load = false;
      for (unsigned rt = 0; rt < fb->rt_count; rt++) {
         key->rts[rt] = get_key_target(fb->rt_formats[rt], fb->sample_count,
                                       has_border, GLSL_TYPE_VOID,
                                       &load->rts[rt]);
         if (key_target_has_load(key->rts[rt]))
            has_load = true;
      }
      return has_load;
   }
}

static const struct pan_fb_shader_load_key_target *
key_get_target_for_location(const struct pan_fb_load_shader_key *key,
                            gl_frag_result location)
{
   if (location == FRAG_RESULT_DEPTH) {
      return &key->z;
   } else if (location == FRAG_RESULT_STENCIL) {
      return &key->s;
   } else {
      assert(location >= FRAG_RESULT_DATA0);
      assert(location < FRAG_RESULT_DATA0 + PAN_MAX_RTS);
      return &key->rts[location - FRAG_RESULT_DATA0];
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
           enum pan_fb_load_op load_op,
           gl_frag_result location,
           const struct pan_fb_load_shader_key *key)
{
   const struct pan_fb_shader_load_key_target *target =
      key_get_target_for_location(key, location);

   const bool is_zs = location == FRAG_RESULT_DEPTH ||
                      location == FRAG_RESULT_STENCIL;

   const nir_alu_type nir_type =
      nir_get_nir_type_for_glsl_base_type(target->glsl_type);

   switch (load_op) {
   case PAN_FB_LOAD_NONE:
      return nir_imm_zero(b, is_zs ? 1 : 4, 32);

   case PAN_FB_LOAD_CLEAR:
      return nir_load_clear_value_pan(b, is_zs ? 1 : 4, 32,
                                      .io_semantics.location = location,
                                      .dest_type = nir_type);

   case PAN_FB_LOAD_IMAGE: {
      assert(pos->num_components == 3);
      switch (target->dim) {
      case MALI_TEXTURE_DIMENSION_1D:
         if (target->is_array)
            pos = nir_channels(b, pos, 0b101);
         else
            pos = nir_channel(b, pos, 0);
         break;

      case MALI_TEXTURE_DIMENSION_CUBE:
         assert(target->is_array);
         break;

      case MALI_TEXTURE_DIMENSION_2D:
         if (!target->is_array)
            pos = nir_channels(b, pos, 0b011);
         break;

      case MALI_TEXTURE_DIMENSION_3D:
         break;

      default:
         UNREACHABLE("Unsupported dim");
      }

      nir_def *val;
      if (target->msaa == PAN_FB_MSAA_COPY_SINGLE) {
         val = nir_txf(b, pos,
                       .texture_index = location,
                       .dim = mali_to_glsl_dim(target->dim),
                       .dest_type = nir_type,
                       .is_array = target->is_array);
      } else {
         assert(target->dim == MALI_TEXTURE_DIMENSION_2D);

         assert(target->msaa == PAN_FB_MSAA_COPY_ALL ||
                target->msaa == PAN_FB_MSAA_COPY_SAMPLE_0);
         nir_def *sample_id = target->msaa == PAN_FB_MSAA_COPY_ALL
                              ? build_sample_id(b) : nir_imm_int(b, 0);

         val = nir_txf_ms(b, pos, sample_id,
                          .texture_index = location,
                          .dim = GLSL_SAMPLER_DIM_MS,
                          .dest_type = nir_type,
                          .is_array = target->is_array);
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
build_load_to_target(nir_builder *b, nir_def *pos, nir_def *in_bounds,
                     gl_frag_result location,
                     const struct pan_fb_load_shader_key *key)
{
   const struct pan_fb_shader_load_key_target *target =
      key_get_target_for_location(key, location);

   const bool is_zs = location == FRAG_RESULT_DEPTH ||
                      location == FRAG_RESULT_STENCIL;
   const unsigned num_components = is_zs ? 1 : 4;
   const nir_alu_type nir_type =
      nir_get_nir_type_for_glsl_base_type(target->glsl_type);

   nir_def *val;
   if (target->in_bounds_load == target->border_load) {
      val = build_load(b, pos, target->in_bounds_load, location, key);
   } else {
      nir_def *in_bounds_val, *border_val;
      nir_if *nif = nir_push_if(b, in_bounds);
      {
         in_bounds_val = build_load(b, pos, target->in_bounds_load,
                                    location, key);
      }
      nir_push_else(b, nif);
      {
         border_val = build_load(b, pos, target->border_load,
                                 location, key);
      }
      nir_pop_if(b, nif);
      val = nir_if_phi(b, in_bounds_val, border_val);
   }

   nir_store_output(b, val, nir_imm_int(b, 0),
                    .base = location, .range = 1,
                    .write_mask = nir_component_mask(num_components),
                    .src_type = nir_type,
                    .io_semantics.location = location,
                    .io_semantics.num_slots = 1);
}

nir_shader *
GENX(pan_get_fb_load_shader)(const struct pan_fb_load_shader_key *key,
                             const struct nir_shader_compiler_options *nir_options)
{
   nir_builder builder = nir_builder_init_simple_shader(
      MESA_SHADER_FRAGMENT, nir_options, "pan-fb-load");
   nir_builder *b = &builder;

   bool no_in_bounds_image = true, no_border_image = true;
   for (unsigned rt = 0; rt < PAN_MAX_RTS; rt++) {
      if (key->rts[rt].in_bounds_load == PAN_FB_LOAD_IMAGE)
         no_in_bounds_image = false;

      if (key->rts[rt].border_load == PAN_FB_LOAD_IMAGE)
         no_border_image = false;
   }

   if (key->z.in_bounds_load == PAN_FB_LOAD_IMAGE ||
       key->s.in_bounds_load == PAN_FB_LOAD_IMAGE)
      no_in_bounds_image = false;

   if (key->z.border_load == PAN_FB_LOAD_IMAGE ||
       key->s.border_load == PAN_FB_LOAD_IMAGE)
      no_border_image = false;

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

   if (PAN_ARCH >= 6 && !key_target_has_load(key->z) &&
                        !key_target_has_load(key->s)) {
      /* We assume that the framebuffer will clear if either load op is set to
       * clear.  For color-only on bifrost, we can do a bit better by emitting
       * BLEND instructions directly only writing the tile buffer if we're
       * actually loading the image.
       */
      for (unsigned rt = 0; rt < PAN_MAX_RTS; rt++) {
         if (key->rts[rt].in_bounds_load != PAN_FB_LOAD_IMAGE &&
             key->rts[rt].border_load != PAN_FB_LOAD_IMAGE)
            continue;

         nir_def *coverage = nir_load_cumulative_coverage_pan(b);
         nir_def *z1 = nir_imm_int(b, 0);
         nir_def *u4 = nir_undef(b, 4, 32);

         nir_if *nif = NULL;
         if (key->rts[rt].in_bounds_load != PAN_FB_LOAD_IMAGE)
            nif = nir_push_if(b, nir_inot(b, in_bounds));
         else if (key->rts[rt].border_load != PAN_FB_LOAD_IMAGE)
            nif = nir_push_if(b, in_bounds);

         gl_frag_result loc = FRAG_RESULT_DATA0 + rt;
         nir_def *color = build_load(b, pos, PAN_FB_LOAD_IMAGE, loc, key);

         if (nif) {
            nir_pop_if(b, nif);
            /* Setting the coverage to 0 should disable the write even with
             * the BLENDs happening at the end of the shader.
             */
            coverage = nir_if_phi(b, coverage, z1);
            color = nir_if_phi(b, color, u4);
         }

         const nir_alu_type nir_type =
            nir_get_nir_type_for_glsl_base_type(key->rts[rt].glsl_type);
         nir_blend_pan(b, coverage,
                       nir_load_blend_descriptor_pan(b, .base = rt),
                       color,
                       .src_type = nir_type,
                       .io_semantics.location = loc,
                       .io_semantics.num_slots = 1);
      }
   } else {
      /* We assume that the framebuffer will clear if either load op is set to
       * clear.  If we know that at least one of in-bounds or out-of-bounds does
       * not have any image loads, we can discard and then assume in-bounds for
       * the rest of the shader and let the ifs fold.
       */
      if (no_in_bounds_image && no_border_image) {
         nir_terminate(b);
         return builder.shader;
      }
      if (no_in_bounds_image) {
         nir_terminate_if(b, in_bounds);
         in_bounds = nir_imm_false(b);
      } else if (no_border_image) {
         nir_terminate_if(b, nir_inot(b, in_bounds));
         in_bounds = nir_imm_true(b);
      }

      if (key_target_has_load(key->z))
         build_load_to_target(b, pos, in_bounds, FRAG_RESULT_DEPTH, key);
      if (key_target_has_load(key->s))
         build_load_to_target(b, pos, in_bounds, FRAG_RESULT_STENCIL, key);

      for (unsigned rt = 0; rt < PAN_MAX_RTS; rt++) {
         if (key_target_has_load(key->rts[rt])) {
            build_load_to_target(b, pos, in_bounds,
                                 FRAG_RESULT_DATA0 + rt, key);
         }
      }
   }

   return builder.shader;
}

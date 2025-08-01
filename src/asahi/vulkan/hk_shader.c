/*
 * Copyright 2024 Valve Corporation
 * Copyright 2024 Alyssa Rosenzweig
 * Copyright 2022-2023 Collabora Ltd. and Red Hat Inc.
 * Copyright 2023 Advanced Micro Devices, Inc.
 * Copyright 2018 Intel Corporation
 * SPDX-License-Identifier: MIT
 */
#include "hk_shader.h"

#include "agx_debug.h"
#include "agx_device.h"
#include "agx_helpers.h"
#include "agx_nir_lower_gs.h"
#include "agx_nir_lower_vbo.h"
#include "glsl_types.h"
#include "libagx.h"
#include "nir.h"
#include "nir_builder.h"

#include "agx_bo.h"
#include "hk_cmd_buffer.h"
#include "hk_descriptor_set_layout.h"
#include "hk_device.h"
#include "hk_physical_device.h"
#include "hk_sampler.h"
#include "hk_shader.h"

#include "nir_builder_opcodes.h"
#include "nir_builtin_builder.h"
#include "nir_intrinsics.h"
#include "nir_intrinsics_indices.h"
#include "nir_xfb_info.h"
#include "shader_enums.h"
#include "vk_graphics_state.h"
#include "vk_nir_convert_ycbcr.h"
#include "vk_physical_device_features.h"
#include "vk_pipeline.h"
#include "vk_pipeline_layout.h"
#include "vk_shader.h"
#include "vk_shader_module.h"
#include "vk_ycbcr_conversion.h"

#include "asahi/compiler/agx_compile.h"
#include "asahi/compiler/agx_nir.h"
#include "asahi/compiler/agx_nir_texture.h"
#include "asahi/lib/agx_abi.h"
#include "asahi/lib/agx_linker.h"
#include "asahi/lib/agx_tilebuffer.h"
#include "asahi/lib/agx_uvs.h"
#include "compiler/spirv/nir_spirv.h"

#include "util/blob.h"
#include "util/hash_table.h"
#include "util/macros.h"
#include "util/mesa-sha1.h"
#include "util/simple_mtx.h"
#include "util/u_debug.h"
#include "vulkan/vulkan_core.h"

struct hk_fs_key {
   bool zs_self_dep;

   /** True if sample shading is forced on via an API knob such as
    * VkPipelineMultisampleStateCreateInfo::minSampleShading
    */
   bool force_sample_shading;

   uint8_t pad[2];
};
static_assert(sizeof(struct hk_fs_key) == 4, "packed");

struct hk_vs_key {
   struct agx_velem_key attribs[32];
   bool skip_prolog;
   bool static_strides;
   bool pad[2];
};
static_assert(sizeof(struct hk_vs_key) == 260, "packed");

union hk_key {
   struct hk_vs_key vs;
   struct hk_fs_key fs;
};

static void
shared_var_info(const struct glsl_type *type, unsigned *size, unsigned *align)
{
   assert(glsl_type_is_vector_or_scalar(type));

   uint32_t comp_size =
      glsl_type_is_boolean(type) ? 4 : glsl_get_bit_size(type) / 8;
   unsigned length = glsl_get_vector_elements(type);
   *size = comp_size * length, *align = comp_size;
}

uint64_t
hk_physical_device_compiler_flags(const struct hk_physical_device *pdev)
{
   /* This could be optimized but it doesn't matter */
   return pdev->dev.debug;
}

const nir_shader_compiler_options *
hk_get_nir_options(struct vk_physical_device *vk_pdev, gl_shader_stage stage,
                   UNUSED const struct vk_pipeline_robustness_state *rs)
{
   return &agx_nir_options;
}

static struct spirv_to_nir_options
hk_get_spirv_options(struct vk_physical_device *vk_pdev,
                     UNUSED gl_shader_stage stage,
                     const struct vk_pipeline_robustness_state *rs)
{
   return (struct spirv_to_nir_options){
      .ssbo_addr_format = hk_buffer_addr_format(rs->storage_buffers),
      .phys_ssbo_addr_format = nir_address_format_64bit_global,
      .ubo_addr_format = hk_buffer_addr_format(rs->uniform_buffers),
      .shared_addr_format = nir_address_format_32bit_offset,
      .min_ssbo_alignment = HK_MIN_SSBO_ALIGNMENT,
      .min_ubo_alignment = HK_MIN_UBO_ALIGNMENT,
   };
}

void
hk_preprocess_nir_internal(struct vk_physical_device *vk_pdev, nir_shader *nir)
{
   /* Must lower before io to temps */
   if (nir->info.stage == MESA_SHADER_FRAGMENT) {
      NIR_PASS(_, nir, nir_lower_terminate_to_demote);
      NIR_PASS(_, nir, nir_lower_halt_to_return);
      NIR_PASS(_, nir, nir_lower_returns);
   }

   /* Unroll loops before lowering indirects via
    * nir_lower_io_vars_to_temporaries */
   UNUSED bool progress = false;
   NIR_PASS(_, nir, nir_lower_global_vars_to_local);

   do {
      progress = false;
      NIR_PASS(progress, nir, nir_lower_vars_to_ssa);
      NIR_PASS(progress, nir, nir_copy_prop);
      NIR_PASS(progress, nir, nir_opt_dce);
      NIR_PASS(progress, nir, nir_opt_constant_folding);
      NIR_PASS(progress, nir, nir_opt_loop);
      NIR_PASS(progress, nir, nir_opt_loop_unroll);
   } while (progress);

   if (nir->info.stage == MESA_SHADER_FRAGMENT) {
      struct nir_lower_sysvals_to_varyings_options sysvals_opts = {
         .point_coord = true,
      };

      nir_lower_sysvals_to_varyings(nir, &sysvals_opts);
   }

   NIR_PASS(_, nir, nir_lower_system_values);

   /* Gather info before preprocess_nir but after some general lowering, so
    * inputs_read and system_values_read are accurately set.
    */
   nir_shader_gather_info(nir, nir_shader_get_entrypoint(nir));

   NIR_PASS(_, nir, nir_lower_io_vars_to_temporaries,
            nir_shader_get_entrypoint(nir), true, false);

   NIR_PASS(_, nir, nir_lower_global_vars_to_local);

   NIR_PASS(_, nir, nir_split_var_copies);
   NIR_PASS(_, nir, nir_split_struct_vars, nir_var_function_temp);

   /* Optimize but allow copies because we haven't lowered them yet */
   agx_preprocess_nir(nir);

   NIR_PASS(_, nir, nir_lower_load_const_to_scalar);
   NIR_PASS(_, nir, nir_lower_var_copies);
}

static void
hk_preprocess_nir(struct vk_physical_device *vk_pdev, nir_shader *nir,
                  UNUSED const struct vk_pipeline_robustness_state *rs)
{
   hk_preprocess_nir_internal(vk_pdev, nir);
   nir_lower_compute_system_values_options csv_options = {
      .has_base_workgroup_id = true,
   };
   NIR_PASS(_, nir, nir_lower_compute_system_values, &csv_options);
}

static void
hk_populate_vs_key(struct hk_vs_key *key,
                   const struct vk_graphics_pipeline_state *state)
{
   memset(key, 0, sizeof(*key));

   if (state == NULL || !state->vi ||
       BITSET_TEST(state->dynamic, MESA_VK_DYNAMIC_VI) ||
       BITSET_TEST(state->dynamic, MESA_VK_DYNAMIC_VI_BINDINGS_VALID))
      return;

   agx_fill_velem_keys(state->vi, ~0 /* compacted on use */, key->attribs);
   key->skip_prolog = true;
   key->static_strides =
      !BITSET_TEST(state->dynamic, MESA_VK_DYNAMIC_VI_BINDING_STRIDES);

   if (!key->static_strides) {
      for (unsigned i = 0; i < ARRAY_SIZE(key->attribs); ++i) {
         key->attribs[i].stride = 0;
      }
   }
}

static void
hk_populate_fs_key(struct hk_fs_key *key,
                   const struct vk_graphics_pipeline_state *state)
{
   memset(key, 0, sizeof(*key));

   if (state == NULL)
      return;

   if (state->pipeline_flags &
       VK_PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT)
      key->zs_self_dep = true;

   /* We force per-sample interpolation whenever sampleShadingEnable is set
    * regardless of minSampleShading or rasterizationSamples.
    *
    * When sampleShadingEnable is set, few guarantees are made about the
    * location of interpolation of the inputs.  The only real guarantees are
    * that the inputs are interpolated within the pixel and that you get at
    * least `rasterizationSamples * minSampleShading` unique positions.
    * Importantly, it does not require that when `rasterizationSamples *
    * minSampleShading <= 1.0` that those positions are at the fragment
    * center.  Therefore, it's valid to just always do per-sample all the time.
    *
    * The one caveat here is that we have to be careful about gl_SampleMaskIn.
    * When `hk_fs_key::force_sample_shading = true` we also turn any reads of
    * gl_SampleMaskIn into `1 << gl_SampleID` because the hardware sample mask
    * is actually per-fragment, not per-pass.  We handle this by smashing
    * minSampleShading to 1.0 whenever gl_SampleMaskIn is read.
    */
   const struct vk_multisample_state *ms = state->ms;
   if (ms != NULL && ms->sample_shading_enable)
      key->force_sample_shading = true;
}

enum hk_feature_key {
   HK_FEAT_MIN_LOD = BITFIELD_BIT(0),
   HK_FEAT_CUSTOM_BORDER = BITFIELD_BIT(1),
   HK_FEAT_LARGE_POINTS = BITFIELD_BIT(2),
};

static enum hk_feature_key
hk_make_feature_key(const struct vk_features *features)
{
   if (!features)
      return ~0U;

   return (features->minLod ? HK_FEAT_MIN_LOD : 0) |
          (features->customBorderColors ? HK_FEAT_CUSTOM_BORDER : 0) |
          (features->largePoints ? HK_FEAT_LARGE_POINTS : 0);
}

static void
hk_hash_graphics_state(struct vk_physical_device *device,
                       const struct vk_graphics_pipeline_state *state,
                       const struct vk_features *features,
                       VkShaderStageFlags stages, blake3_hash blake3_out)
{
   struct mesa_blake3 blake3_ctx;
   _mesa_blake3_init(&blake3_ctx);
   if (state && (stages & VK_SHADER_STAGE_VERTEX_BIT)) {
      struct hk_vs_key key;
      hk_populate_vs_key(&key, state);
      _mesa_blake3_update(&blake3_ctx, &key, sizeof(key));
   } else if (state && (stages & VK_SHADER_STAGE_FRAGMENT_BIT)) {
      struct hk_fs_key key;
      hk_populate_fs_key(&key, state);
      _mesa_blake3_update(&blake3_ctx, &key, sizeof(key));

      const bool is_multiview = state->rp->view_mask != 0;
      _mesa_blake3_update(&blake3_ctx, &is_multiview, sizeof(is_multiview));
   }

   enum hk_feature_key feature_key = hk_make_feature_key(features);
   _mesa_blake3_update(&blake3_ctx, &feature_key, sizeof(feature_key));

   _mesa_blake3_final(&blake3_ctx, blake3_out);
}

static nir_def *
bounds_check(nir_builder *b, nir_def *data, nir_def *offs, nir_def *bound)
{
   if (data->bit_size == 32 && data->num_components == 1) {
      return nir_bounds_agx(b, data, offs, bound);
   } else {
      /* TODO: Optimize */
      return nir_bcsel(b, nir_uge(b, bound, offs), data,
                       nir_imm_zero(b, data->num_components, data->bit_size));
   }
}

static bool
is_op(nir_scalar s, nir_op op)
{
   return nir_scalar_is_alu(s) && nir_scalar_alu_op(s) == op;
}

static nir_def *
check_in_bounds(nir_builder *b, nir_intrinsic_instr *intr)
{
   nir_def *offset = intr->src[1].ssa;
   nir_def *bound = intr->src[2].ssa;

   unsigned bit_size = intr->def.bit_size;
   assert(bit_size >= 8 && bit_size % 8 == 0);
   unsigned byte_size = bit_size / 8;
   unsigned load_size = byte_size * intr->num_components;

   /* Try to bounds check in terms of elements */
   nir_scalar offset_s = nir_scalar_resolved(offset, 0);
   if (is_op(offset_s, nir_op_amul)) {
      nir_scalar srcs[] = {nir_scalar_chase_alu_src(offset_s, 0),
                           nir_scalar_chase_alu_src(offset_s, 1)};
      unsigned i = nir_scalar_is_const(srcs[0]) ? 1 : 0;
      if (nir_scalar_is_const(srcs[1 - i]) &&
          nir_scalar_as_uint(srcs[1 - i]) == load_size) {

         nir_def *index = nir_mov_scalar(b, srcs[i]);
         return nir_ult(b, index, nir_udiv_imm(b, bound, load_size));
      }
   }

   /* TODO: handle also the iadd(amul) pattern, this is important */

   /* Otherwise bounds check in bytes */
   nir_def *sat_offset = nir_umin_imm(b, offset, UINT32_MAX - (load_size - 1));
   return nir_ult(b, nir_iadd_imm(b, sat_offset, load_size - 1), bound);
}

static nir_def *
bound_offset(nir_builder *b, nir_def *valid, nir_scalar offset)
{
   /* If we can, clamp the source of an amul instead of the result, to remain
    * compatible with the hardware address mode.
    */
   if (is_op(offset, nir_op_amul)) {
      nir_scalar srcs[] = {
         nir_scalar_chase_alu_src(offset, 0),
         nir_scalar_chase_alu_src(offset, 1),
      };
      unsigned i = nir_scalar_is_const(srcs[0]) ? 1 : 0;
      nir_def *x = nir_mov_scalar(b, srcs[i]);
      nir_def *y = nir_mov_scalar(b, srcs[1 - i]);

      return nir_amul(b, nir_bcsel(b, valid, x, nir_imm_int(b, 0)), y);
   }

   /* Similar case for when there's an addition (chain) in the way */
   if (is_op(offset, nir_op_iadd)) {
      nir_scalar x = nir_scalar_chase_alu_src(offset, 0);
      nir_scalar y = nir_scalar_chase_alu_src(offset, 1);

      if (is_op(x, nir_op_amul) || is_op(y, nir_op_amul)) {
         return nir_iadd(b, bound_offset(b, valid, x),
                         bound_offset(b, valid, y));
      }
   }

   nir_def *def = nir_mov_scalar(b, offset);

   /* If the offset fits within the zero page, clamping is pointless */
   if (nir_scalar_is_const(offset) &&
       (nir_scalar_as_uint(offset) + 16) < AGX_ZERO_PAGE_SIZE)
      return def;

   /* Otherwise, fallback on clamping the offset */
   return nir_bcsel(b, valid, def, nir_imm_int(b, 0));
}

static void
lower_load_global_bounded(nir_builder *b, nir_intrinsic_instr *intr)
{
   b->cursor = nir_before_instr(&intr->instr);
   nir_def *base = intr->src[0].ssa;
   nir_def *offset = intr->src[1].ssa;

   /* Bounds check compatibly with the hardware address mode */
   nir_def *valid = check_in_bounds(b, intr);
   base = nir_bcsel(b, valid, base, nir_imm_int64(b, AGX_ZERO_PAGE_ADDRESS));
   offset = bound_offset(b, valid, nir_scalar_resolved(offset, 0));

   enum gl_access_qualifier access = nir_intrinsic_access(intr);

   if (intr->intrinsic == nir_intrinsic_load_global_constant_bounded) {
      access |= ACCESS_NON_WRITEABLE | ACCESS_CAN_REORDER;
   }

   nir_def *val = nir_build_load_global(
      b, intr->def.num_components, intr->def.bit_size,
      nir_iadd(b, base, nir_u2u64(b, offset)),
      .align_mul = nir_intrinsic_align_mul(intr),
      .align_offset = nir_intrinsic_align_offset(intr), .access = access);

   nir_def_replace(&intr->def, val);
}

static bool
lower_load_global_constant_offset_instr(nir_builder *b,
                                        nir_intrinsic_instr *intrin, void *data)
{
   bool *has_soft_fault = data;

   if (intrin->intrinsic == nir_intrinsic_load_global_bounded ||
       (intrin->intrinsic == nir_intrinsic_load_global_constant_bounded &&
        !(*has_soft_fault))) {

      lower_load_global_bounded(b, intrin);
      return true;
   }

   if (intrin->intrinsic != nir_intrinsic_load_global_constant_offset &&
       intrin->intrinsic != nir_intrinsic_load_global_constant_bounded)
      return false;

   b->cursor = nir_before_instr(&intrin->instr);

   nir_def *base_addr = intrin->src[0].ssa;
   nir_def *offset = intrin->src[1].ssa;
   nir_def *bound = NULL;

   unsigned bit_size = intrin->def.bit_size;
   assert(bit_size >= 8 && bit_size % 8 == 0);
   unsigned byte_size = bit_size / 8;
   unsigned load_size = byte_size * intrin->num_components;

   if (intrin->intrinsic == nir_intrinsic_load_global_constant_bounded) {
      bound = intrin->src[2].ssa;
   }

   unsigned align_mul = nir_intrinsic_align_mul(intrin);
   unsigned align_offset = nir_intrinsic_align_offset(intrin);

   nir_def *val = nir_build_load_global_constant(
      b, intrin->def.num_components, intrin->def.bit_size,
      nir_iadd(b, base_addr, nir_u2u64(b, offset)), .align_mul = align_mul,
      .align_offset = align_offset, .access = nir_intrinsic_access(intrin));

   if (intrin->intrinsic == nir_intrinsic_load_global_constant_bounded) {
      nir_scalar offs = nir_scalar_resolved(offset, 0);
      if (nir_scalar_is_const(offs)) {
         /* Calculate last byte loaded */
         unsigned offs_imm = nir_scalar_as_uint(offs) + load_size;

         /* Simplify the bounds check. Uniform buffers are bounds checked at
          * 64B granularity, so `bound` is a multiple of K = 64. Then
          *
          * offs_imm < bound <==> round_down(offs_imm, K) < bound. Proof:
          *
          * "=>" round_down(offs_imm, K) <= offs_imm < bound.
          *
          * "<=" Let a, b be integer s.t. offs_imm = K a + b with b < K.
          *      Note round_down(offs_imm, K) = Ka.
          *
          *      Let c be integer s.t. bound = Kc.
          *      We have Ka < Kc => a < c.
          *      b < K => Ka + b < K(a + 1).
          *
          *      a < c with integers => a + 1 <= c.
          *      offs_imm < K(a + 1) <= Kc = bound.
          *      Hence offs_imm < bound.
          */
         assert(align_mul == 64);
         offs_imm &= ~(align_mul - 1);

         /* Bounds checks are `offset > bound ? 0 : val` so if offset = 0,
          * the bounds check is useless.
          */
         if (offs_imm) {
            val = bounds_check(b, val, nir_imm_int(b, offs_imm), bound);
         }
      } else {
         offset = nir_iadd_imm(b, offset, load_size);
         val = bounds_check(b, val, offset, bound);
      }
   }

   nir_def_replace(&intrin->def, val);
   return true;
}

struct lower_ycbcr_state {
   uint32_t set_layout_count;
   struct vk_descriptor_set_layout *const *set_layouts;
};

static const struct vk_ycbcr_conversion_state *
lookup_ycbcr_conversion(const void *_state, uint32_t set, uint32_t binding,
                        uint32_t array_index)
{
   const struct lower_ycbcr_state *state = _state;
   assert(set < state->set_layout_count);
   assert(state->set_layouts[set] != NULL);
   const struct hk_descriptor_set_layout *set_layout =
      vk_to_hk_descriptor_set_layout(state->set_layouts[set]);
   assert(binding < set_layout->binding_count);

   const struct hk_descriptor_set_binding_layout *bind_layout =
      &set_layout->binding[binding];

   if (bind_layout->immutable_samplers == NULL)
      return NULL;

   array_index = MIN2(array_index, bind_layout->array_size - 1);

   const struct hk_sampler *sampler =
      bind_layout->immutable_samplers[array_index];

   return sampler && sampler->vk.ycbcr_conversion
             ? &sampler->vk.ycbcr_conversion->state
             : NULL;
}

static int
glsl_type_size(const struct glsl_type *type, bool bindless)
{
   return glsl_count_attribute_slots(type, false);
}

/*
 * This is the world's worst multiview implementation. We simply duplicate each
 * draw on the CPU side, changing a uniform in between, and then plumb the view
 * index into the layer ID here. Whatever, it works.
 *
 * The "proper" implementation on AGX would use vertex amplification, but a
 * MacBook is not a VR headset.
 */
static void
hk_lower_multiview(nir_shader *nir)
{
   /* If there's an existing layer ID write, ignore it. This avoids validation
    * splat with vk_meta.
    */
   nir_variable *existing = nir_find_variable_with_location(
      nir, nir_var_shader_out, VARYING_SLOT_LAYER);

   if (existing) {
      existing->data.mode = nir_var_shader_temp;
      existing->data.location = 0;
      nir_fixup_deref_modes(nir);
   }

   /* Now write the view index as the layer */
   nir_builder b =
      nir_builder_at(nir_after_impl(nir_shader_get_entrypoint(nir)));

   nir_variable *layer =
      nir_variable_create(nir, nir_var_shader_out, glsl_uint_type(), NULL);

   layer->data.location = VARYING_SLOT_LAYER;

   nir_store_var(&b, layer, nir_load_view_index(&b), nir_component_mask(1));
   b.shader->info.outputs_written |= VARYING_BIT_LAYER;
}

static nir_def *
query_custom_border(nir_builder *b, nir_tex_instr *tex)
{
   return nir_build_texture_query(b, tex, nir_texop_custom_border_color_agx, 4,
                                  tex->dest_type, false, false);
}

static nir_def *
has_custom_border(nir_builder *b, nir_tex_instr *tex)
{
   return nir_build_texture_query(b, tex, nir_texop_has_custom_border_color_agx,
                                  1, nir_type_bool1, false, false);
}

static bool
lower(nir_builder *b, nir_tex_instr *tex, UNUSED void *_data)
{
   if (!nir_tex_instr_need_sampler(tex) || nir_tex_instr_is_query(tex))
      return false;

   /* XXX: this is a really weird edge case, is this even well-defined? */
   if (tex->is_shadow)
      return false;

   b->cursor = nir_after_instr(&tex->instr);
   nir_def *has_custom = has_custom_border(b, tex);

   nir_instr *orig = nir_instr_clone(b->shader, &tex->instr);
   nir_builder_instr_insert(b, orig);
   nir_def *clamp_to_1 = &nir_instr_as_tex(orig)->def;

   nir_push_if(b, has_custom);
   nir_def *replaced = NULL;
   {
      /* Sample again, this time with clamp-to-0 instead of clamp-to-1 */
      nir_instr *clone_instr = nir_instr_clone(b->shader, &tex->instr);
      nir_builder_instr_insert(b, clone_instr);

      nir_tex_instr *tex_0 = nir_instr_as_tex(clone_instr);
      nir_def *clamp_to_0 = &tex_0->def;

      tex_0->backend_flags |= AGX_TEXTURE_FLAG_CLAMP_TO_0;

      /* Grab the border colour */
      nir_def *border = query_custom_border(b, tex_0);

      if (tex->op == nir_texop_tg4) {
         border = nir_replicate(b, nir_channel(b, border, tex->component), 4);
      }

      /* Combine together with the border */
      if (nir_alu_type_get_base_type(tex->dest_type) == nir_type_float &&
          tex->op != nir_texop_tg4) {

         /* For floats, lerp together:
          *
          * For border texels:  (1 * border) + (0 * border      ) = border
          * For regular texels: (x * border) + (x * (1 - border)) = x.
          *
          * Linear filtering is linear (duh), so lerping is compatible.
          */
         replaced = nir_flrp(b, clamp_to_0, clamp_to_1, border);
      } else {
         /* For integers, just select componentwise since there is no linear
          * filtering. Gathers also use this path since they are unfiltered in
          * each component.
          */
         replaced = nir_bcsel(b, nir_ieq(b, clamp_to_0, clamp_to_1), clamp_to_0,
                              border);
      }
   }
   nir_pop_if(b, NULL);

   /* Put it together with a phi */
   nir_def *phi = nir_if_phi(b, replaced, clamp_to_1);
   nir_def_replace(&tex->def, phi);
   return true;
}

static bool
agx_nir_lower_custom_border(nir_shader *nir)
{
   return nir_shader_tex_pass(nir, lower, nir_metadata_none, NULL);
}

static nir_def *
query_min_lod(nir_builder *b, nir_tex_instr *tex, bool int_coords)
{
   nir_alu_type T = int_coords ? nir_type_uint16 : nir_type_float16;
   return nir_build_texture_query(b, tex, nir_texop_image_min_lod_agx, 1, T,
                                  false, false);
}

static bool
lower_min_lod(nir_builder *b, nir_tex_instr *tex, UNUSED void *_data)
{
   if (nir_tex_instr_is_query(tex))
      return false;

   /* Buffer textures don't have levels-of-detail */
   if (tex->sampler_dim == GLSL_SAMPLER_DIM_BUF)
      return false;

   if (tex->backend_flags & AGX_TEXTURE_FLAG_NO_CLAMP)
      return false;

   bool int_coords = tex->op == nir_texop_txf || tex->op == nir_texop_txf_ms ||
                     tex->op == nir_texop_tg4;

   b->cursor = nir_before_instr(&tex->instr);
   nir_def *min_lod = query_min_lod(b, tex, int_coords);
   nir_def *other_min_lod = nir_steal_tex_src(tex, nir_tex_src_min_lod);

   if (tex->op == nir_texop_tg4) {
      b->cursor = nir_after_instr(&tex->instr);

      /* The Vulkan spec section "Texel Gathering" says:
       *
       *    If levelbase < minLodIntegerimageView, then any values fetched are
       *    zero if the robustImageAccess2 feature is enabled.
       *
       * We currently always enable robustImageAccess2, so implement that
       * semantic here.
       *
       * We could probably optimize this with a special descriptor for this case
       * but tg4 is rare enough I'm not bothered.
       */
      nir_def *old = &tex->def;
      nir_def *oob = nir_ine_imm(b, min_lod, 0);
      nir_def *zero = nir_imm_zero(b, old->num_components, old->bit_size);
      nir_def *new_ = nir_bcsel(b, oob, zero, old);
      nir_def_rewrite_uses_after(old, new_);
   } else if (tex->op == nir_texop_txl) {
      assert(other_min_lod == NULL && "txl doesn't have an API min lod");

      nir_def *lod = nir_steal_tex_src(tex, nir_tex_src_lod);
      if (lod) {
         min_lod = nir_fmax(b, nir_f2fN(b, lod, min_lod->bit_size), min_lod);
      }

      nir_tex_instr_add_src(tex, nir_tex_src_lod, min_lod);
   } else {
      if (other_min_lod) {
         assert(!int_coords && "no API min lod");
         min_lod = nir_fmax(b, min_lod, nir_f2f16(b, other_min_lod));
      }

      nir_tex_instr_add_src(tex, nir_tex_src_min_lod, min_lod);
   }

   return true;
}

static bool
agx_nir_lower_image_view_min_lod(nir_shader *nir)
{
   return nir_shader_tex_pass(nir, lower_min_lod, nir_metadata_none, NULL);
}

/*
 * In Vulkan, the VIEWPORT should read 0 in the fragment shader if it is not
 * written by the vertex shader, but in our implementation, the varying would
 * otherwise be undefined. This small pass predicates VIEWPORT reads based on
 * whether the hardware vertex shader writes the VIEWPORT (nonzero UVS index).
 */
static bool
lower_viewport_fs(nir_builder *b, nir_intrinsic_instr *intr, UNUSED void *data)
{
   if (intr->intrinsic != nir_intrinsic_load_input)
      return false;

   nir_io_semantics sem = nir_intrinsic_io_semantics(intr);
   if (sem.location != VARYING_SLOT_VIEWPORT)
      return false;

   b->cursor = nir_after_instr(&intr->instr);
   nir_def *orig = &intr->def;

   nir_def *uvs = nir_load_uvs_index_agx(b, .io_semantics = sem);
   nir_def *def = nir_bcsel(b, nir_ine_imm(b, uvs, 0), orig, nir_imm_int(b, 0));

   nir_def_rewrite_uses_after(orig, def);
   return true;
}

static bool
lower_subpass_dim(nir_builder *b, nir_tex_instr *tex, UNUSED void *_data)
{
   if (tex->sampler_dim == GLSL_SAMPLER_DIM_SUBPASS)
      tex->sampler_dim = GLSL_SAMPLER_DIM_2D;
   else if (tex->sampler_dim == GLSL_SAMPLER_DIM_SUBPASS_MS)
      tex->sampler_dim = GLSL_SAMPLER_DIM_MS;
   else
      return false;

   return true;
}

static bool
should_lower_robust(const nir_intrinsic_instr *intr, const void *_)
{
   /* The hardware is robust, but our software image atomics are not. Unlike the
    * GL driver, we don't use the common buffer image lowering, using the
    * agx_nir_lower_texture lowering for robustImageAccess2 semantics.
    */
   return intr->intrinsic == nir_intrinsic_image_deref_atomic ||
          intr->intrinsic == nir_intrinsic_image_deref_atomic_swap;
}

static void
hk_lower_nir(struct hk_device *dev, nir_shader *nir,
             const struct vk_pipeline_robustness_state *rs, bool is_multiview,
             uint32_t set_layout_count,
             struct vk_descriptor_set_layout *const *set_layouts,
             enum hk_feature_key features)
{
   if (HK_PERF(dev, NOROBUST)) {
      rs = &vk_robustness_disabled;
   }

   const nir_opt_access_options access_options = {
      .is_vulkan = true,
   };
   NIR_PASS(_, nir, nir_opt_access, &access_options);

   if (nir->info.stage == MESA_SHADER_FRAGMENT) {
      NIR_PASS(_, nir, nir_lower_input_attachments,
               &(nir_input_attachment_options){
                  .use_fragcoord_sysval = true,
                  .use_layer_id_sysval = true,
                  .use_view_id_for_layer = is_multiview,
               });

      NIR_PASS(_, nir, nir_shader_tex_pass, lower_subpass_dim, nir_metadata_all,
               NULL);
      NIR_PASS(_, nir, nir_lower_wpos_center);
   }

   /* XXX: should be last geometry stage, how do I get to that? */
   if (nir->info.stage == MESA_SHADER_VERTEX) {
      if (is_multiview)
         hk_lower_multiview(nir);
   }

   if (nir->info.stage == MESA_SHADER_TESS_EVAL) {
      NIR_PASS(_, nir, nir_lower_patch_vertices,
               nir->info.tess.tcs_vertices_out, NULL);
   }

   const struct lower_ycbcr_state ycbcr_state = {
      .set_layout_count = set_layout_count,
      .set_layouts = set_layouts,
   };
   NIR_PASS(_, nir, nir_vk_lower_ycbcr_tex, lookup_ycbcr_conversion,
            &ycbcr_state);

   /* Lower push constants before lower_descriptors */
   NIR_PASS(_, nir, nir_lower_explicit_io, nir_var_mem_push_const,
            nir_address_format_32bit_offset);

   // NIR_PASS(_, nir, nir_opt_large_constants, NULL, 32);

   /* Turn cache flushes into image coherency bits while we still have derefs */
   NIR_PASS(_, nir, nir_lower_memory_model);

   NIR_PASS(_, nir, nir_lower_robust_access, should_lower_robust, NULL);

   /* We must do early lowering before hk_nir_lower_descriptors, since this will
    * create lod_bias instructions.
    */
   NIR_PASS(_, nir, agx_nir_lower_texture_early, true /* support_lod_bias */);

   if (features & HK_FEAT_MIN_LOD) {
      NIR_PASS(_, nir, agx_nir_lower_image_view_min_lod);
   }

   if ((features & HK_FEAT_CUSTOM_BORDER) && !HK_PERF(dev, NOBORDER)) {
      NIR_PASS(_, nir, agx_nir_lower_custom_border);
   }

   NIR_PASS(_, nir, hk_nir_lower_descriptors, rs, set_layout_count,
            set_layouts);
   NIR_PASS(_, nir, nir_lower_explicit_io, nir_var_mem_global,
            nir_address_format_64bit_global);
   NIR_PASS(_, nir, nir_lower_explicit_io, nir_var_mem_ssbo,
            hk_buffer_addr_format(rs->storage_buffers));
   NIR_PASS(_, nir, nir_lower_explicit_io, nir_var_mem_ubo,
            hk_buffer_addr_format(rs->uniform_buffers));

   /* Before inserting bounds checks, we want to do a fair bit of optimization.
    * lower_load_global_constant_offset_instr has special optimizations for
    * constant offsets, so we want as many offsets to be constant as possible.
    */
   bool progress;
   do {
      progress = false;
      NIR_PASS(progress, nir, nir_opt_constant_folding);
      NIR_PASS(progress, nir, nir_opt_algebraic);
      NIR_PASS(progress, nir, nir_copy_prop);
      NIR_PASS(progress, nir, nir_opt_dce);
   } while (progress);

   nir_move_options opts = nir_move_load_ssbo | nir_move_load_ubo;
   NIR_PASS(progress, nir, nir_opt_sink, opts);
   NIR_PASS(progress, nir, nir_opt_move, opts);

   bool soft_fault = agx_has_soft_fault(&dev->dev);
   NIR_PASS(_, nir, nir_shader_intrinsics_pass,
            lower_load_global_constant_offset_instr, nir_metadata_none,
            &soft_fault);

   assert(nir->info.shared_size == 0);

   NIR_PASS(_, nir, nir_lower_vars_to_explicit_types, nir_var_mem_shared,
            shared_var_info);
   NIR_PASS(_, nir, nir_lower_explicit_io, nir_var_mem_shared,
            nir_address_format_32bit_offset);

   if (nir->info.zero_initialize_shared_memory && nir->info.shared_size > 0) {
      /* Align everything up to 16B so we can write whole vec4s. */
      nir->info.shared_size = align(nir->info.shared_size, 16);
      NIR_PASS(_, nir, nir_zero_initialize_shared_memory, nir->info.shared_size,
               16);

      /* We need to call lower_compute_system_values again because
       * nir_zero_initialize_shared_memory generates load_invocation_id which
       * has to be lowered to load_invocation_index.
       */
      NIR_PASS(_, nir, nir_lower_compute_system_values, NULL);
   }

   /* TODO: we can do indirect VS output */
   nir_variable_mode lower_indirect_modes = 0;
   if (nir->info.stage == MESA_SHADER_FRAGMENT)
      lower_indirect_modes |= nir_var_shader_out;
   else if (nir->info.stage == MESA_SHADER_VERTEX)
      lower_indirect_modes |= nir_var_shader_in | nir_var_shader_out;

   NIR_PASS(_, nir, nir_lower_indirect_derefs, lower_indirect_modes,
            UINT32_MAX);

   NIR_PASS(_, nir, nir_lower_io, nir_var_shader_in | nir_var_shader_out,
            glsl_type_size,
            nir_lower_io_lower_64bit_to_32 |
               nir_lower_io_use_interpolated_input_intrinsics);

   if (nir->info.stage == MESA_SHADER_FRAGMENT) {
      NIR_PASS(_, nir, nir_shader_intrinsics_pass, lower_viewport_fs,
               nir_metadata_control_flow, NULL);
   }

   NIR_PASS(_, nir, agx_nir_lower_texture);
   NIR_PASS(_, nir, agx_nir_lower_multisampled_image_store);

   agx_preprocess_nir(nir);

   nir_opt_peephole_select_options peephole_select_options = {
      .limit = 0,
      .discard_ok = true,
   };
   NIR_PASS(_, nir, nir_opt_peephole_select, &peephole_select_options);
   NIR_PASS(_, nir, nir_opt_if,
            nir_opt_if_optimize_phi_true_false | nir_opt_if_avoid_64bit_phis);
}

static void
hk_upload_shader(struct hk_device *dev, struct hk_shader *shader)
{
   if (shader->b.info.has_preamble || shader->b.info.rodata.size_16) {
      /* TODO: Do we wnat to compact? Revisit when we rework prolog/epilogs. */
      size_t size = shader->b.info.binary_size;
      assert(size > 0);

      shader->bo = agx_bo_create(&dev->dev, size, 0,
                                 AGX_BO_EXEC | AGX_BO_LOW_VA, "Preamble");
      memcpy(agx_bo_map(shader->bo), shader->b.binary, size);
      shader->preamble_addr =
         shader->bo->va->addr + shader->b.info.preamble_offset;
   }

   if (!shader->linked.ht) {
      /* If we only have a single variant, link now. */
      shader->only_linked = hk_fast_link(dev, false, shader, NULL, NULL, 0);
   }

   if (shader->info.stage == MESA_SHADER_FRAGMENT) {
      agx_pack_fragment_face_2(&shader->frag_face, 0, &shader->b.info);
   }

   agx_pack(&shader->counts, COUNTS, cfg) {
      cfg.uniform_register_count = shader->b.info.push_count;
      cfg.preshader_register_count = shader->b.info.nr_preamble_gprs;
      cfg.sampler_state_register_count = agx_translate_sampler_state_count(
         shader->b.info.sampler_state_count, false);
      cfg.texture_state_register_count = shader->b.info.texture_state_count;
   }
}

DERIVE_HASH_TABLE(hk_fast_link_key_vs);
DERIVE_HASH_TABLE(hk_fast_link_key_fs);

static VkResult
hk_init_link_ht(struct hk_shader *shader, gl_shader_stage sw_stage)
{
   simple_mtx_init(&shader->linked.lock, mtx_plain);

   bool multiple_variants =
      sw_stage == MESA_SHADER_VERTEX || sw_stage == MESA_SHADER_FRAGMENT;

   if (!multiple_variants)
      return VK_SUCCESS;

   if (sw_stage == MESA_SHADER_VERTEX)
      shader->linked.ht = hk_fast_link_key_vs_table_create(NULL);
   else
      shader->linked.ht = hk_fast_link_key_fs_table_create(NULL);

   return (shader->linked.ht == NULL) ? VK_ERROR_OUT_OF_HOST_MEMORY
                                      : VK_SUCCESS;
}

static bool
lower_uniforms(nir_builder *b, nir_intrinsic_instr *intr, void *data)
{
   /* Root is first, descriptor sets follow immediately. */
   unsigned *root_ = data;
   unsigned root = *root_;
   unsigned sets = root + 4;

   if (intr->intrinsic == nir_intrinsic_bindless_image_agx ||
       intr->intrinsic == nir_intrinsic_bindless_sampler_agx) {
      /* Change of units from sets to uniforms */
      nir_intrinsic_set_desc_set(intr,
                                 sets + (nir_intrinsic_desc_set(intr) * 4));
      return true;
   }

   if (intr->intrinsic != nir_intrinsic_load_root_agx &&
       intr->intrinsic != nir_intrinsic_load_descriptor_set_agx)
      return false;

   b->cursor = nir_before_instr(&intr->instr);
   nir_def *rep;

   if (intr->intrinsic == nir_intrinsic_load_descriptor_set_agx) {
      unsigned s = nir_intrinsic_desc_set(intr);
      rep = nir_load_preamble(b, 1, 64, .base = sets + (4 * s));
   } else {
      rep = nir_load_preamble(b, 1, 64, .base = root);
   }

   nir_def_replace(&intr->def, rep);
   return true;
}

static bool
kill_psiz(nir_builder *b, nir_intrinsic_instr *intr, void *data)
{
   if (intr->intrinsic != nir_intrinsic_store_output ||
       nir_intrinsic_io_semantics(intr).location != VARYING_SLOT_PSIZ)
      return false;

   return nir_remove_sysval_output(intr, MESA_SHADER_FRAGMENT);
}

static void
hk_lower_hw_vs(nir_shader *nir, struct hk_shader *shader,
               enum hk_feature_key features)
{
   /* Point size must be clamped, excessively large points don't render
    * properly on G13.
    *
    * Must be synced with pointSizeRange.
    */
   NIR_PASS(_, nir, nir_lower_point_size, 1.0f, 511.95f);

   if (!(features & HK_FEAT_LARGE_POINTS)) {
      NIR_PASS(_, nir, nir_shader_intrinsics_pass, kill_psiz,
               nir_metadata_control_flow, NULL);
   }

   NIR_PASS(_, nir, nir_lower_io_to_scalar, nir_var_shader_out, NULL, NULL);
   NIR_PASS(_, nir, agx_nir_lower_cull_distance_vs);

   NIR_PASS(_, nir, agx_nir_lower_uvs, &shader->info.uvs);
}

static VkResult
hk_compile_nir(struct hk_device *dev, const VkAllocationCallbacks *pAllocator,
               nir_shader *nir, VkShaderCreateFlagsEXT shader_flags,
               const struct vk_pipeline_robustness_state *rs,
               const union hk_key *key, enum hk_feature_key features,
               struct hk_shader *shader, gl_shader_stage sw_stage, bool hw,
               nir_xfb_info *xfb_info, unsigned set_count)
{
   unsigned nr_vbos = 0;

   /* For now, only shader objects are supported */
   if (sw_stage == MESA_SHADER_VERTEX) {
      nr_vbos = DIV_ROUND_UP(
         BITSET_LAST_BIT(shader->info.vs.attrib_components_read), 4);
   } else if (sw_stage == MESA_SHADER_FRAGMENT) {
      shader->info.fs.interp = agx_gather_interp_info(nir);
      shader->info.fs.writes_memory = nir->info.writes_memory;

      /* Discards must be lowering before lowering MSAA to handle discards */
      NIR_PASS(_, nir, agx_nir_lower_discard_zs_emit);
      NIR_PASS(_, nir, agx_nir_lower_fs_output_to_epilog,
               &shader->info.fs.epilog_key);
      NIR_PASS(_, nir, agx_nir_lower_sample_mask);

      if (nir->info.fs.uses_sample_shading) {
         /* Ensure the sample mask is preserved in register */
         nir_builder b =
            nir_builder_at(nir_after_impl(nir_shader_get_entrypoint(nir)));

         nir_def *mask =
            nir_load_exported_agx(&b, 1, 16, .base = AGX_ABI_FIN_SAMPLE_MASK);

         nir_export_agx(&b, mask, .base = AGX_ABI_FOUT_SAMPLE_MASK);

         NIR_PASS(_, nir, agx_nir_lower_to_per_sample);
      }

      NIR_PASS(_, nir, agx_nir_lower_fs_active_samples_to_register);
      NIR_PASS(_, nir, agx_nir_lower_interpolation);
   } else if (sw_stage == MESA_SHADER_TESS_EVAL ||
              sw_stage == MESA_SHADER_TESS_CTRL) {

      shader->info.tess.info.ccw = nir->info.tess.ccw;
      shader->info.tess.info.points = nir->info.tess.point_mode;
      shader->info.tess.info.spacing = nir->info.tess.spacing;
      shader->info.tess.info.mode = nir->info.tess._primitive_mode;

      if (sw_stage == MESA_SHADER_TESS_CTRL) {
         shader->info.tess.tcs_output_patch_size =
            nir->info.tess.tcs_vertices_out;
         shader->info.tess.tcs_per_vertex_outputs =
            agx_tcs_per_vertex_outputs(nir);
         shader->info.tess.tcs_nr_patch_outputs =
            util_last_bit(nir->info.patch_outputs_written);
         shader->info.tess.tcs_output_stride = agx_tcs_output_stride(nir);
      } else {
         /* This destroys info so it needs to happen after the gather */
         NIR_PASS(_, nir, agx_nir_lower_tes, hw);
      }
   }

   /* Normally, vertex shaders need to write a default point size. However, if
    * we have a geometry/tessellation shader, the hardware vertex (software
    * GS/TES) will handle this itself instead.
    *
    * TODO: Optimize out for monolithic?
    */
   if (sw_stage == MESA_SHADER_VERTEX && hw &&
       (features & HK_FEAT_LARGE_POINTS)) {
      NIR_PASS(_, nir, nir_lower_default_point_size);
   }

   uint64_t outputs = nir->info.outputs_written;
   if (sw_stage == MESA_SHADER_VERTEX || sw_stage == MESA_SHADER_TESS_EVAL) {
      if (hw) {
         hk_lower_hw_vs(nir, shader, features);
      } else {
         NIR_PASS(_, nir, agx_nir_lower_vs_before_gs);
         nir->info.stage = MESA_SHADER_COMPUTE;
         memset(&nir->info.cs, 0, sizeof(nir->info.cs));
         nir->xfb_info = NULL;
      }
   }

   unsigned root = 0;
   if (sw_stage == MESA_SHADER_FRAGMENT)
      root = AGX_ABI_FUNI_ROOT;
   else if (sw_stage == MESA_SHADER_VERTEX)
      root = AGX_ABI_VUNI_COUNT_VK(nr_vbos);

   shader->info.set_count = set_count;

   /* XXX: rename */
   NIR_PASS(_, nir, hk_lower_uvs_index, nr_vbos);
   NIR_PASS(_, nir, nir_shader_intrinsics_pass, lower_uniforms,
            nir_metadata_control_flow, &root);

#if 0
   /* TODO */
   nir_variable_mode robust2_modes = 0;
   if (rs->uniform_buffers == VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT)
      robust2_modes |= nir_var_mem_ubo;
   if (rs->storage_buffers == VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT)
      robust2_modes |= nir_var_mem_ssbo;
#endif

   struct agx_shader_key backend_key = {
      .reserved_preamble = root + (4 * (1 + set_count)),
      .dev = agx_gather_device_key(&dev->dev),
      .no_stop = nir->info.stage == MESA_SHADER_FRAGMENT,
      .has_scratch = !nir->info.internal,
      .promote_constants = true,
      .promote_textures = true,
   };

   /* For now, sample shading is always dynamic. Indicate that. */
   if (nir->info.stage == MESA_SHADER_FRAGMENT &&
       nir->info.fs.uses_sample_shading)
      backend_key.fs.inside_sample_loop = true;

   simple_mtx_t *lock = NULL;
   if (agx_get_compiler_debug())
      lock = &hk_device_physical(dev)->debug_compile_lock;

   if (lock)
      simple_mtx_lock(lock);

   assert(nir->info.io_lowered);
   agx_compile_shader_nir(nir, &backend_key, &shader->b);

   if (lock)
      simple_mtx_unlock(lock);

   shader->code_ptr = shader->b.binary;
   shader->code_size = shader->b.info.binary_size;

   shader->info.stage = sw_stage;
   shader->info.clip_distance_array_size = nir->info.clip_distance_array_size;
   shader->info.cull_distance_array_size = nir->info.cull_distance_array_size;
   shader->b.info.outputs = outputs;

   if (xfb_info) {
      assert(xfb_info->output_count < ARRAY_SIZE(shader->info.xfb_outputs));

      memcpy(&shader->info.xfb_info, xfb_info,
             nir_xfb_info_size(xfb_info->output_count));

      typed_memcpy(shader->info.xfb_stride, nir->info.xfb_stride, 4);
   }

   if (nir->constant_data_size > 0) {
      uint32_t data_size = align(nir->constant_data_size, HK_MIN_UBO_ALIGNMENT);

      void *data = malloc(data_size);
      if (data == NULL) {
         ralloc_free(nir);
         return vk_error(dev, VK_ERROR_OUT_OF_HOST_MEMORY);
      }

      memcpy(data, nir->constant_data, nir->constant_data_size);

      assert(nir->constant_data_size <= data_size);
      memset(data + nir->constant_data_size, 0,
             data_size - nir->constant_data_size);

      shader->data_ptr = data;
      shader->data_size = data_size;
   }

   ralloc_free(nir);

   VkResult result = hk_init_link_ht(shader, sw_stage);
   if (result != VK_SUCCESS)
      return vk_error(dev, result);

   hk_upload_shader(dev, shader);
   return VK_SUCCESS;
}

static const struct vk_shader_ops hk_shader_ops;

static void
hk_destroy_linked_shader(struct hk_device *dev, struct hk_linked_shader *linked)
{
   agx_bo_unreference(&dev->dev, linked->b.bo);
   ralloc_free(linked);
}

static void
hk_shader_destroy(struct hk_device *dev, struct hk_shader *s)
{
   free((void *)s->code_ptr);
   free((void *)s->data_ptr);
   agx_bo_unreference(&dev->dev, s->bo);

   simple_mtx_destroy(&s->linked.lock);

   if (s->only_linked)
      hk_destroy_linked_shader(dev, s->only_linked);

   if (s->linked.ht) {
      hash_table_foreach(s->linked.ht, entry) {
         hk_destroy_linked_shader(dev, entry->data);
      }
      _mesa_hash_table_destroy(s->linked.ht, NULL);
   }
}

void
hk_api_shader_destroy(struct vk_device *vk_dev, struct vk_shader *vk_shader,
                      const VkAllocationCallbacks *pAllocator)
{
   struct hk_device *dev = container_of(vk_dev, struct hk_device, vk);
   struct hk_api_shader *obj =
      container_of(vk_shader, struct hk_api_shader, vk);

   hk_foreach_variant(obj, shader) {
      hk_shader_destroy(dev, shader);
   }

   vk_shader_free(&dev->vk, pAllocator, &obj->vk);
}

VkResult
hk_compile_shader(struct hk_device *dev, struct vk_shader_compile_info *info,
                  const struct vk_graphics_pipeline_state *state,
                  const struct vk_features *vk_features,
                  const VkAllocationCallbacks *pAllocator,
                  struct hk_api_shader **shader_out)
{
   VkResult result;
   enum hk_feature_key features = hk_make_feature_key(vk_features);

   /* We consume the NIR, regardless of success or failure */
   nir_shader *nir = info->nir;

   size_t size = sizeof(struct hk_api_shader) +
                 sizeof(struct hk_shader) * hk_num_variants(info->stage);
   struct hk_api_shader *obj =
      vk_shader_zalloc(&dev->vk, &hk_shader_ops, info->stage, pAllocator, size);

   if (obj == NULL) {
      ralloc_free(nir);
      return vk_error(dev, VK_ERROR_OUT_OF_HOST_MEMORY);
   }

   if (!nir->info.io_lowered) {
      hk_lower_nir(dev, nir, info->robustness, false, info->set_layout_count,
                   info->set_layouts, features);
   }

   gl_shader_stage sw_stage = nir->info.stage;

   union hk_key key_tmp, *key = NULL;
   if (sw_stage == MESA_SHADER_FRAGMENT) {
      hk_populate_fs_key(&key_tmp.fs, state);
      key = &key_tmp;

      nir->info.fs.uses_sample_shading |= key->fs.force_sample_shading;

      /* Force late-Z for Z/S self-deps. TODO: There's probably a less silly way
       * to do this.
       */
      if (key->fs.zs_self_dep) {
         nir_builder b =
            nir_builder_at(nir_before_impl(nir_shader_get_entrypoint(nir)));
         nir_discard_if(&b, nir_imm_false(&b));
         nir->info.fs.uses_discard = true;
      }

      NIR_PASS(_, nir, agx_nir_lower_sample_intrinsics, false);
   } else if (sw_stage == MESA_SHADER_VERTEX) {
      hk_populate_vs_key(&key_tmp.vs, state);
      key = &key_tmp;
   } else if (sw_stage == MESA_SHADER_TESS_CTRL) {
      NIR_PASS(_, nir, agx_nir_lower_tcs);
   }

   /* Compile all variants up front */
   if (sw_stage == MESA_SHADER_GEOMETRY) {
      struct hk_shader *main_variant = hk_main_gs_variant(obj);
      struct hk_shader *count_variant = hk_count_gs_variant(obj);

      nir_shader *count = NULL, *rast = NULL, *pre_gs = NULL;

      NIR_PASS(_, nir, agx_nir_lower_gs, &count, &rast, &pre_gs,
               &count_variant->info.gs);

      struct hk_shader *shader = &obj->variants[HK_GS_VARIANT_RAST];
      hk_lower_hw_vs(rast, shader, features);
      shader->info.gs = count_variant->info.gs;
      main_variant->info.gs = count_variant->info.gs;

      struct {
         nir_shader *in;
         struct hk_shader *out;
      } variants[] = {
         {nir, hk_main_gs_variant(obj)},
         {pre_gs, hk_pre_gs_variant(obj)},
         {count, count_variant},
         {rast, &obj->variants[HK_GS_VARIANT_RAST]},
      };

      for (unsigned v = 0; v < ARRAY_SIZE(variants); ++v) {
         if (variants[v].in) {
            result =
               hk_compile_nir(dev, pAllocator, variants[v].in, info->flags,
                              info->robustness, NULL, features, variants[v].out,
                              sw_stage, true, NULL, info->set_layout_count);

            if (result != VK_SUCCESS) {
               hk_api_shader_destroy(&dev->vk, &obj->vk, pAllocator);
               ralloc_free(nir);
               ralloc_free(pre_gs);
               ralloc_free(count);
               ralloc_free(rast);
               return result;
            }
         }
      }
   } else if (sw_stage == MESA_SHADER_VERTEX ||
              sw_stage == MESA_SHADER_TESS_EVAL) {

      VkShaderStageFlags next_stage = info->next_stage_mask;

      /* Transform feedback is layered on top of geometry shaders. If there is
       * not a geometry shader in the pipeline, we will compile a geometry
       * shader for the purpose. Update the next_stage mask accordingly.
       */
      if (nir->xfb_info != NULL) {
         next_stage |= VK_SHADER_STAGE_GEOMETRY_BIT;
      }

      if (sw_stage == MESA_SHADER_VERTEX) {
         assert(
            !(nir->info.inputs_read & BITFIELD64_MASK(VERT_ATTRIB_GENERIC0)) &&
            "Fixed-function attributes not used in Vulkan");

         NIR_PASS(_, nir, nir_recompute_io_bases, nir_var_shader_in);
      }

      /* the shader_out portion of this is load-bearing even for tess eval */
      NIR_PASS(_, nir, nir_io_add_const_offset_to_base,
               nir_var_shader_in | nir_var_shader_out);

      for (enum hk_vs_variant v = 0; v < HK_VS_VARIANTS; ++v) {
         /* Only compile the software variant if we might use this shader with
          * geometry/tessellation. We need to compile the hardware variant
          * unconditionally to handle the VS -> null FS case, which does not
          * require setting the FRAGMENT bit.
          */
         if (v == HK_VS_VARIANT_SW &&
             !(next_stage & (VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT |
                             VK_SHADER_STAGE_GEOMETRY_BIT)))
            continue;

         struct hk_shader *shader = &obj->variants[v];
         bool hw = v == HK_VS_VARIANT_HW;
         bool last = (v + 1) == HK_VS_VARIANTS;

         /* Each variant gets its own NIR. To save an extra clone, we use the
          * original NIR for the last stage.
          */
         nir_shader *clone = last ? nir : nir_shader_clone(NULL, nir);

         NIR_PASS(_, clone, agx_nir_gather_vs_inputs,
                  shader->info.vs.attrib_components_read);

         if (sw_stage == MESA_SHADER_VERTEX) {
            shader->info.vs.use_prolog = !(key && key->vs.skip_prolog);
            shader->info.vs.attribs_read =
               nir->info.inputs_read >> VERT_ATTRIB_GENERIC0;

            if (shader->info.vs.use_prolog) {
               NIR_PASS(_, clone, agx_nir_lower_vs_input_to_prolog);
            } else {
               struct agx_velem_key attribs[AGX_MAX_ATTRIBS];
               for (unsigned a = 0; a < AGX_MAX_ATTRIBS; ++a) {
                  if (key->vs.attribs[a].format) {
                     unsigned slot = util_bitcount64(
                        shader->info.vs.attribs_read & BITFIELD_MASK(a));

                     attribs[slot] = key->vs.attribs[a];
                  }
               }

               struct agx_robustness agx_rs = {
                  .soft_fault = agx_has_soft_fault(&dev->dev),

                  /* Correctly handling GPL + pipeline-robustness requires
                   * runtime changes, and I don't care enough to optimize this.
                   */
                  .level = AGX_ROBUSTNESS_D3D,
               };

               agx_nir_lower_vbo(clone, attribs, agx_rs,
                                 !key->vs.static_strides);

               unsigned nr = DIV_ROUND_UP(
                  BITSET_LAST_BIT(shader->info.vs.attrib_components_read), 4);
               agx_nir_lower_non_monolithic_uniforms(clone, nr);
            }
         }

         /* hk_compile_nir takes ownership of the clone */
         result =
            hk_compile_nir(dev, pAllocator, clone, info->flags,
                           info->robustness, key, features, shader, sw_stage,
                           hw, nir->xfb_info, info->set_layout_count);
         if (result != VK_SUCCESS) {
            hk_api_shader_destroy(&dev->vk, &obj->vk, pAllocator);
            ralloc_free(nir);
            return result;
         }
      }
   } else {
      struct hk_shader *shader = hk_only_variant(obj);

      /* hk_compile_nir takes ownership of nir */
      result = hk_compile_nir(dev, pAllocator, nir, info->flags,
                              info->robustness, key, features, shader, sw_stage,
                              true, NULL, info->set_layout_count);
      if (result != VK_SUCCESS) {
         hk_api_shader_destroy(&dev->vk, &obj->vk, pAllocator);
         return result;
      }
   }

   *shader_out = obj;
   return VK_SUCCESS;
}

static void
nir_opts(nir_shader *nir)
{
   bool progress;

   do {
      progress = false;

      NIR_PASS(progress, nir, nir_opt_loop);
      NIR_PASS(progress, nir, nir_copy_prop);
      NIR_PASS(progress, nir, nir_opt_remove_phis);
      NIR_PASS(progress, nir, nir_opt_dce);

      NIR_PASS(progress, nir, nir_opt_if, 0);
      NIR_PASS(progress, nir, nir_opt_dead_cf);
      NIR_PASS(progress, nir, nir_opt_cse);

      NIR_PASS(progress, nir, nir_opt_peephole_select,
               &(nir_opt_peephole_select_options){
                  .limit = 8,
                  .expensive_alu_ok = true,
                  .discard_ok = true,
               });

      NIR_PASS(progress, nir, nir_opt_phi_precision);
      NIR_PASS(progress, nir, nir_opt_algebraic);
      NIR_PASS(progress, nir, nir_opt_constant_folding);
      NIR_PASS(progress, nir, nir_io_add_const_offset_to_base,
               nir_var_shader_in | nir_var_shader_out);

      NIR_PASS(progress, nir, nir_opt_undef);
      NIR_PASS(progress, nir, nir_opt_loop_unroll);
   } while (progress);
}

static VkResult
hk_compile_shaders(struct vk_device *vk_dev, uint32_t shader_count,
                   struct vk_shader_compile_info *infos,
                   const struct vk_graphics_pipeline_state *state,
                   const struct vk_features *features,
                   const VkAllocationCallbacks *pAllocator,
                   struct vk_shader **shaders_out)
{
   struct hk_device *dev = container_of(vk_dev, struct hk_device, vk);
   nir_shader *shaders[shader_count];

   /* Lower shaders, notably lowering I/O. This is a prerequisite for
    * intershader optimization.
    */
   for (uint32_t i = 0; i < shader_count; i++) {
      const struct vk_shader_compile_info *info = &infos[i];
      /* TODO: Multiview with ESO */
      const bool is_multiview = state && state->rp->view_mask != 0;
      enum hk_feature_key hk_features = hk_make_feature_key(features);
      nir_shader *nir = info->nir;

      hk_lower_nir(dev, nir, info->robustness, is_multiview,
                   info->set_layout_count, info->set_layouts, hk_features);

      if (nir->xfb_info) {
         nir_io_add_const_offset_to_base(
            nir, nir_var_shader_in | nir_var_shader_out);

         nir_io_add_intrinsic_xfb_info(nir);
      }

      shaders[i] = nir;
   }

   nir_opt_varyings_bulk(shaders, shader_count, true, UINT32_MAX, UINT32_MAX,
                         nir_opts);

   for (uint32_t i = 0; i < shader_count; i++) {
      nir_shader_gather_info(infos[i].nir,
                             nir_shader_get_entrypoint(infos[i].nir));

      VkResult result =
         hk_compile_shader(dev, &infos[i], state, features, pAllocator,
                           (struct hk_api_shader **)&shaders_out[i]);
      if (result != VK_SUCCESS) {
         /* Clean up all the shaders before this point */
         for (uint32_t j = 0; j < i; j++)
            hk_api_shader_destroy(&dev->vk, shaders_out[j], pAllocator);

         /* Clean up all the NIR after this point */
         for (uint32_t j = i + 1; j < shader_count; j++)
            ralloc_free(infos[j].nir);

         /* Memset the output array */
         memset(shaders_out, 0, shader_count * sizeof(*shaders_out));

         return result;
      }
   }

   return VK_SUCCESS;
}

static VkResult
hk_deserialize_shader(struct hk_device *dev, struct blob_reader *blob,
                      struct hk_shader *shader)
{
   struct hk_shader_info info;
   blob_copy_bytes(blob, &info, sizeof(info));

   struct agx_shader_info b_info;
   blob_copy_bytes(blob, &b_info, sizeof(b_info));

   const uint32_t code_size = blob_read_uint32(blob);
   const uint32_t data_size = blob_read_uint32(blob);
   if (blob->overrun)
      return vk_error(dev, VK_ERROR_INCOMPATIBLE_SHADER_BINARY_EXT);

   VkResult result = hk_init_link_ht(shader, info.stage);
   if (result != VK_SUCCESS)
      return vk_error(dev, VK_ERROR_OUT_OF_HOST_MEMORY);

   simple_mtx_init(&shader->linked.lock, mtx_plain);

   shader->b.info = b_info;
   shader->info = info;
   shader->code_size = code_size;
   shader->data_size = data_size;
   shader->b.info.binary_size = code_size;

   shader->code_ptr = malloc(code_size);
   if (shader->code_ptr == NULL)
      return vk_error(dev, VK_ERROR_OUT_OF_HOST_MEMORY);

   shader->data_ptr = malloc(data_size);
   if (shader->data_ptr == NULL)
      return vk_error(dev, VK_ERROR_OUT_OF_HOST_MEMORY);

   blob_copy_bytes(blob, (void *)shader->code_ptr, shader->code_size);
   blob_copy_bytes(blob, (void *)shader->data_ptr, shader->data_size);
   if (blob->overrun)
      return vk_error(dev, VK_ERROR_INCOMPATIBLE_SHADER_BINARY_EXT);

   shader->b.binary = (void *)shader->code_ptr;
   hk_upload_shader(dev, shader);
   return VK_SUCCESS;
}

static VkResult
hk_deserialize_api_shader(struct vk_device *vk_dev, struct blob_reader *blob,
                          uint32_t binary_version,
                          const VkAllocationCallbacks *pAllocator,
                          struct vk_shader **shader_out)
{
   struct hk_device *dev = container_of(vk_dev, struct hk_device, vk);

   gl_shader_stage stage = blob_read_uint8(blob);
   if (blob->overrun)
      return vk_error(dev, VK_ERROR_INCOMPATIBLE_SHADER_BINARY_EXT);

   size_t size = sizeof(struct hk_api_shader) +
                 sizeof(struct hk_shader) * hk_num_variants(stage);

   struct hk_api_shader *obj =
      vk_shader_zalloc(&dev->vk, &hk_shader_ops, stage, pAllocator, size);

   if (obj == NULL)
      return vk_error(dev, VK_ERROR_OUT_OF_HOST_MEMORY);

   hk_foreach_variant(obj, shader) {
      VkResult result = hk_deserialize_shader(dev, blob, shader);

      if (result != VK_SUCCESS) {
         hk_api_shader_destroy(&dev->vk, &obj->vk, pAllocator);
         return result;
      }
   }

   *shader_out = &obj->vk;
   return VK_SUCCESS;
}

static void
hk_shader_serialize(struct vk_device *vk_dev, const struct hk_shader *shader,
                    struct blob *blob)
{
   blob_write_bytes(blob, &shader->info, sizeof(shader->info));
   blob_write_bytes(blob, &shader->b.info, sizeof(shader->b.info));

   blob_write_uint32(blob, shader->code_size);
   blob_write_uint32(blob, shader->data_size);
   blob_write_bytes(blob, shader->code_ptr, shader->code_size);
   blob_write_bytes(blob, shader->data_ptr, shader->data_size);
}

static bool
hk_api_shader_serialize(struct vk_device *vk_dev,
                        const struct vk_shader *vk_shader, struct blob *blob)
{
   struct hk_api_shader *obj =
      container_of(vk_shader, struct hk_api_shader, vk);

   blob_write_uint8(blob, vk_shader->stage);

   hk_foreach_variant(obj, shader) {
      hk_shader_serialize(vk_dev, shader, blob);
   }

   return !blob->out_of_memory;
}

static VkResult
hk_shader_get_executable_properties(
   UNUSED struct vk_device *device, const struct vk_shader *vk_shader,
   uint32_t *executable_count, VkPipelineExecutablePropertiesKHR *properties)
{
   struct hk_api_shader *obj =
      container_of(vk_shader, struct hk_api_shader, vk);

   VK_OUTARRAY_MAKE_TYPED(VkPipelineExecutablePropertiesKHR, out, properties,
                          executable_count);

   vk_outarray_append_typed(VkPipelineExecutablePropertiesKHR, &out, props)
   {
      props->stages = mesa_to_vk_shader_stage(obj->vk.stage);
      props->subgroupSize = 32;
      VK_COPY_STR(props->name, _mesa_shader_stage_to_string(obj->vk.stage));
      VK_PRINT_STR(props->description, "%s shader",
                   _mesa_shader_stage_to_string(obj->vk.stage));
   }

   return vk_outarray_status(&out);
}

static VkResult
hk_shader_get_executable_statistics(
   UNUSED struct vk_device *device, const struct vk_shader *vk_shader,
   uint32_t executable_index, uint32_t *statistic_count,
   VkPipelineExecutableStatisticKHR *statistics)
{
   struct hk_api_shader *obj =
      container_of(vk_shader, struct hk_api_shader, vk);

   VK_OUTARRAY_MAKE_TYPED(VkPipelineExecutableStatisticKHR, out, statistics,
                          statistic_count);

   assert(executable_index == 0);

   /* TODO: find a sane way to report multiple variants and have that play nice
    * with zink.
    */
   struct hk_shader *shader = hk_any_variant(obj);
   vk_add_agx2_stats(out, &shader->b.info.stats);
   return vk_outarray_status(&out);
}

static bool
write_ir_text(VkPipelineExecutableInternalRepresentationKHR *ir,
              const char *data)
{
   ir->isText = VK_TRUE;

   size_t data_len = strlen(data) + 1;

   if (ir->pData == NULL) {
      ir->dataSize = data_len;
      return true;
   }

   strncpy(ir->pData, data, ir->dataSize);
   if (ir->dataSize < data_len)
      return false;

   ir->dataSize = data_len;
   return true;
}

static VkResult
hk_shader_get_executable_internal_representations(
   UNUSED struct vk_device *device, const struct vk_shader *vk_shader,
   uint32_t executable_index, uint32_t *internal_representation_count,
   VkPipelineExecutableInternalRepresentationKHR *internal_representations)
{
   VK_OUTARRAY_MAKE_TYPED(VkPipelineExecutableInternalRepresentationKHR, out,
                          internal_representations,
                          internal_representation_count);
   bool incomplete_text = false;

   assert(executable_index == 0);

   /* TODO */
#if 0
   vk_outarray_append_typed(VkPipelineExecutableInternalRepresentationKHR, &out, ir) {
      VK_COPY_STR(ir->name, "AGX assembly");
      VK_COPY_STR(ir->description, "AGX assembly");
      if (!write_ir_text(ir, TODO))
         incomplete_text = true;
   }
#endif

   return incomplete_text ? VK_INCOMPLETE : vk_outarray_status(&out);
}

static const struct vk_shader_ops hk_shader_ops = {
   .destroy = hk_api_shader_destroy,
   .serialize = hk_api_shader_serialize,
   .get_executable_properties = hk_shader_get_executable_properties,
   .get_executable_statistics = hk_shader_get_executable_statistics,
   .get_executable_internal_representations =
      hk_shader_get_executable_internal_representations,
};

const struct vk_device_shader_ops hk_device_shader_ops = {
   .get_nir_options = hk_get_nir_options,
   .get_spirv_options = hk_get_spirv_options,
   .preprocess_nir = hk_preprocess_nir,
   .hash_state = hk_hash_graphics_state,
   .compile = hk_compile_shaders,
   .deserialize = hk_deserialize_api_shader,
   .cmd_set_dynamic_graphics_state = vk_cmd_set_dynamic_graphics_state,
   .cmd_bind_shaders = hk_cmd_bind_shaders,
};

struct hk_linked_shader *
hk_fast_link(struct hk_device *dev, bool fragment, struct hk_shader *main,
             struct agx_shader_part *prolog, struct agx_shader_part *epilog,
             unsigned nr_samples_shaded)
{
   struct hk_linked_shader *s = rzalloc(NULL, struct hk_linked_shader);
   agx_fast_link(&s->b, &dev->dev, fragment, &main->b, prolog, epilog,
                 nr_samples_shaded);

   if (fragment) {
      unsigned samplers = main->b.info.sampler_state_count;
      if (s->b.uses_txf)
         samplers = MAX2(samplers, 1);

      agx_pack(&s->fs_counts, FRAGMENT_SHADER_WORD_0, cfg) {
         cfg.cf_binding_count = s->b.cf.nr_bindings;
         cfg.uniform_register_count = main->b.info.push_count;
         cfg.preshader_register_count = main->b.info.nr_preamble_gprs;
         cfg.texture_state_register_count = main->b.info.texture_state_count;
         cfg.sampler_state_register_count =
            agx_translate_sampler_state_count(samplers, false);
      }
   }

   /* Now that we've linked, bake the USC words to bind this program */
   struct agx_usc_builder b = agx_usc_builder(s->usc.data, sizeof(s->usc.data));

   if (main && main->b.info.rodata.size_16) {
      agx_usc_immediates(&b, &main->b.info.rodata, main->bo->va->addr);
   }

   if (s->b.uses_txf)
      agx_usc_push_packed(&b, SAMPLER, dev->dev.txf_sampler);

   agx_usc_shared_non_fragment(&b, &main->b.info, 0);
   agx_usc_push_packed(&b, SHADER, s->b.shader);
   agx_usc_push_packed(&b, REGISTERS, s->b.regs);

   if (fragment)
      agx_usc_push_packed(&b, FRAGMENT_PROPERTIES, s->b.fragment_props);

   if (main && main->b.info.has_preamble) {
      agx_usc_pack(&b, PRESHADER, cfg) {
         cfg.code = agx_usc_addr(&dev->dev, main->preamble_addr);
      }
   } else {
      agx_usc_pack(&b, NO_PRESHADER, cfg)
         ;
   }

   s->usc.size = b.head - s->usc.data;
   return s;
}

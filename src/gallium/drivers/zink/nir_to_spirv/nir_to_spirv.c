/*
 * Copyright 2018 Collabora Ltd.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "nir_to_spirv.h"
#include "spirv_builder.h"

#include "nir.h"
#include "pipe/p_state.h"
#include "util/u_math.h"
#include "util/u_memory.h"
#include "util/hash_table.h"

#define SLOT_UNSET ((unsigned char) -1)

struct ntv_context {
   void *mem_ctx;

   /* SPIR-V 1.4 and later requires entrypoints to list all global
    * variables in the interface.
    */
   bool spirv_1_4_interfaces;
   bool have_spirv16;

   bool explicit_lod; //whether to set lod=0 for texture()

   struct spirv_builder builder;
   nir_shader *nir;

   struct hash_table *glsl_types[2]; //[implicit_stride]
   struct hash_table *bo_struct_types;
   struct hash_table *bo_array_types;

   SpvId GLSL_std_450;

   gl_shader_stage stage;
   const struct zink_shader_info *sinfo;

   SpvId ubos[PIPE_MAX_CONSTANT_BUFFERS][5]; //8, 16, 32, unused, 64
   nir_variable *ubo_vars[PIPE_MAX_CONSTANT_BUFFERS];

   SpvId ssbos[5]; //8, 16, 32, unused, 64
   nir_variable *ssbo_vars;

   SpvId images[PIPE_MAX_SHADER_IMAGES];
   struct hash_table image_types;
   SpvId samplers[PIPE_MAX_SHADER_SAMPLER_VIEWS];
   SpvId bindless_samplers[2];
   nir_variable *sampler_var[PIPE_MAX_SHADER_SAMPLER_VIEWS]; /* driver_location -> variable */
   nir_variable *bindless_sampler_var[2];
   unsigned last_sampler;
   unsigned bindless_set_idx;
   nir_variable *image_var[PIPE_MAX_SHADER_IMAGES]; /* driver_location -> variable */

   SpvId entry_ifaces[PIPE_MAX_SHADER_INPUTS * 4 + PIPE_MAX_SHADER_OUTPUTS * 4];
   size_t num_entry_ifaces;

   SpvId *defs;
   nir_alu_type *def_types;
   SpvId *resident_defs;
   size_t num_defs;

   struct hash_table *vars; /* nir_variable -> SpvId */

   const SpvId *block_ids;
   size_t num_blocks;
   bool block_started;
   SpvId loop_break, loop_cont;

   SpvId shared_block_var[5]; //8, 16, 32, unused, 64
   SpvId shared_block_arr_type[5]; //8, 16, 32, unused, 64
   SpvId scratch_block_var[5]; //8, 16, 32, unused, 64

   SpvId front_face_var, instance_id_var, vertex_id_var,
         primitive_id_var, invocation_id_var, // geometry
         sample_mask_type, sample_id_var, sample_pos_var, sample_mask_in_var,
         tess_patch_vertices_in, tess_coord_var, // tess
         push_const_var, point_coord_var,
         workgroup_id_var, num_workgroups_var,
         local_invocation_id_var, global_invocation_id_var,
         local_invocation_index_var, helper_invocation_var,
         local_group_size_var, view_index_var,
         base_vertex_var, base_instance_var, draw_id_var;

   SpvId shared_mem_size;

   SpvId subgroup_eq_mask_var,
         subgroup_ge_mask_var,
         subgroup_gt_mask_var,
         subgroup_id_var,
         subgroup_invocation_var,
         subgroup_le_mask_var,
         subgroup_lt_mask_var,
         subgroup_size_var,
         num_subgroups_var;

   SpvId discard_func;
   SpvId float_array_type[2];
};

static SpvId
get_fvec_constant(struct ntv_context *ctx, unsigned bit_size,
                  unsigned num_components, double value);

static SpvId
get_ivec_constant(struct ntv_context *ctx, unsigned bit_size,
                  unsigned num_components, int64_t value);

static SpvId
emit_unop(struct ntv_context *ctx, SpvOp op, SpvId type, SpvId src);

static SpvId
emit_binop(struct ntv_context *ctx, SpvOp op, SpvId type,
           SpvId src0, SpvId src1);

static SpvId
emit_triop(struct ntv_context *ctx, SpvOp op, SpvId type,
           SpvId src0, SpvId src1, SpvId src2);

static bool
alu_op_is_typeless(nir_op op)
{
   switch (op) {
   case nir_op_mov:
   case nir_op_vec16:
   case nir_op_vec2:
   case nir_op_vec3:
   case nir_op_vec4:
   case nir_op_vec5:
   case nir_op_vec8:
   case nir_op_bcsel:
      return true;
   default:
      break;
   }
   return false;
}

static nir_alu_type
get_nir_alu_type(const struct glsl_type *type)
{
   return nir_alu_type_get_base_type(nir_get_nir_type_for_glsl_base_type(glsl_get_base_type(glsl_without_array_or_matrix(type))));
}

static nir_alu_type
infer_nir_alu_type_from_uses_ssa(nir_def *ssa);

static nir_alu_type
infer_nir_alu_type_from_use(nir_src *src)
{
   nir_instr *instr = nir_src_parent_instr(src);
   nir_alu_type atype = nir_type_invalid;
   switch (instr->type) {
   case nir_instr_type_alu: {
      nir_alu_instr *alu = nir_instr_as_alu(instr);
      if (alu->op == nir_op_bcsel) {
         if (nir_srcs_equal(alu->src[0].src, *src)) {
            /* special case: the first src in bcsel is always bool */
            return nir_type_bool;
         }
      }
      /* ignore typeless ops */
      if (alu_op_is_typeless(alu->op)) {
         atype = infer_nir_alu_type_from_uses_ssa(&alu->def);
         break;
      }
      for (unsigned i = 0; i < nir_op_infos[alu->op].num_inputs; i++) {
         if (!nir_srcs_equal(alu->src[i].src, *src))
            continue;
         atype = nir_op_infos[alu->op].input_types[i];
         break;
      }
      break;
   }
   case nir_instr_type_tex: {
      nir_tex_instr *tex = nir_instr_as_tex(instr);
      for (unsigned i = 0; i < tex->num_srcs; i++) {
         if (!nir_srcs_equal(tex->src[i].src, *src))
            continue;
         switch (tex->src[i].src_type) {
         case nir_tex_src_coord:
         case nir_tex_src_lod:
            if (tex->op == nir_texop_txf ||
               tex->op == nir_texop_txf_ms ||
               tex->op == nir_texop_txs)
               atype = nir_type_int;
            else
               atype = nir_type_float;
            break;
         case nir_tex_src_projector:
         case nir_tex_src_bias:
         case nir_tex_src_min_lod:
         case nir_tex_src_comparator:
         case nir_tex_src_ddx:
         case nir_tex_src_ddy:
            atype = nir_type_float;
            break;
         case nir_tex_src_offset:
         case nir_tex_src_ms_index:
         case nir_tex_src_texture_offset:
         case nir_tex_src_sampler_offset:
         case nir_tex_src_sampler_handle:
         case nir_tex_src_texture_handle:
            atype = nir_type_int;
            break;
         default:
            break;
         }
         break;
      }
      break;
   }
   case nir_instr_type_intrinsic: {
      if (nir_instr_as_intrinsic(instr)->intrinsic == nir_intrinsic_load_deref) {
         atype = get_nir_alu_type(nir_instr_as_deref(instr)->type);
      } else if (nir_instr_as_intrinsic(instr)->intrinsic == nir_intrinsic_store_deref) {
         atype = get_nir_alu_type(nir_src_as_deref(nir_instr_as_intrinsic(instr)->src[0])->type);
      }
      break;
   }
   default:
      break;
   }
   return nir_alu_type_get_base_type(atype);
}

static nir_alu_type
infer_nir_alu_type_from_uses_ssa(nir_def *ssa)
{
   nir_alu_type atype = nir_type_invalid;
   /* try to infer a type: if it's wrong then whatever, but at least we tried */
   nir_foreach_use_including_if(src, ssa) {
      if (nir_src_is_if(src))
         return nir_type_bool;
      atype = infer_nir_alu_type_from_use(src);
      if (atype)
         break;
   }
   return atype ? atype : nir_type_uint;
}

static SpvId
get_bvec_type(struct ntv_context *ctx, int num_components)
{
   SpvId bool_type = spirv_builder_type_bool(&ctx->builder);
   if (num_components > 1)
      return spirv_builder_type_vector(&ctx->builder, bool_type,
                                       num_components);

   assert(num_components == 1);
   return bool_type;
}

static SpvId
find_image_type(struct ntv_context *ctx, nir_variable *var)
{
   struct hash_entry *he = _mesa_hash_table_search(&ctx->image_types, var);
   return he ? (intptr_t)he->data : 0;
}

static SpvScope
get_scope(mesa_scope scope)
{
   SpvScope conv[] = {
      [SCOPE_NONE] = 0,
      [SCOPE_INVOCATION] = SpvScopeInvocation,
      [SCOPE_SUBGROUP] = SpvScopeSubgroup,
      [SCOPE_SHADER_CALL] = SpvScopeShaderCallKHR,
      [SCOPE_WORKGROUP] = SpvScopeWorkgroup,
      [SCOPE_QUEUE_FAMILY] = SpvScopeQueueFamily,
      [SCOPE_DEVICE] = SpvScopeDevice,
   };
   return conv[scope];
}

static SpvId
block_label(struct ntv_context *ctx, nir_block *block)
{
   assert(block->index < ctx->num_blocks);
   return ctx->block_ids[block->index];
}

static void
emit_access_decorations(struct ntv_context *ctx, nir_variable *var, SpvId var_id)
{
    u_foreach_bit(bit, var->data.access) {
       switch (1 << bit) {
       case ACCESS_COHERENT:
          /* SpvDecorationCoherent can't be used with vulkan memory model */
          break;
       case ACCESS_RESTRICT:
          spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationRestrict);
          break;
       case ACCESS_VOLATILE:
          /* SpvDecorationVolatile can't be used with vulkan memory model */
          break;
       case ACCESS_NON_READABLE:
          spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationNonReadable);
          break;
       case ACCESS_NON_WRITEABLE:
          spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationNonWritable);
          break;
       case ACCESS_NON_UNIFORM:
          spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationNonUniform);
          break;
       case ACCESS_CAN_REORDER:
       case ACCESS_NON_TEMPORAL:
          /* no equivalent */
          break;
       default:
          UNREACHABLE("unknown access bit");
       }
    }
    /* The Simple, GLSL, and Vulkan memory models can assume that aliasing is generally
     * not present between the memory object declarations. Specifically, the consumer
     * is free to assume aliasing is not present between memory object declarations,
     * unless the memory object declarations explicitly indicate they alias.
     * ...
     * Applying Restrict is allowed, but has no effect.
     * ...
     * Only those memory object declarations decorated with Aliased or AliasedPointer may alias each other.
     *
     * - SPIRV 2.18.2 Aliasing
     *
     * thus if the variable isn't marked restrict, assume it may alias
     */
    if (!(var->data.access & ACCESS_RESTRICT))
       spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationAliased);
}

static SpvOp
get_atomic_op(struct ntv_context *ctx, unsigned bit_size, nir_atomic_op op)
{
   switch (op) {
#define ATOMIC_FCAP(NAME) \
   do {\
      if (bit_size == 16) \
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilityAtomicFloat16##NAME##EXT); \
      if (bit_size == 32) \
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilityAtomicFloat32##NAME##EXT); \
      if (bit_size == 64) \
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilityAtomicFloat64##NAME##EXT); \
   } while (0)

   case nir_atomic_op_fadd:
      ATOMIC_FCAP(Add);
      if (bit_size == 16)
         spirv_builder_emit_extension(&ctx->builder, "SPV_EXT_shader_atomic_float16_add");
      else
         spirv_builder_emit_extension(&ctx->builder, "SPV_EXT_shader_atomic_float_add");
      return SpvOpAtomicFAddEXT;
   case nir_atomic_op_fmax:
      ATOMIC_FCAP(MinMax);
      spirv_builder_emit_extension(&ctx->builder, "SPV_EXT_shader_atomic_float_min_max");
      return SpvOpAtomicFMaxEXT;
   case nir_atomic_op_fmin:
      ATOMIC_FCAP(MinMax);
      spirv_builder_emit_extension(&ctx->builder, "SPV_EXT_shader_atomic_float_min_max");
      return SpvOpAtomicFMinEXT;

   case nir_atomic_op_iadd:
      return SpvOpAtomicIAdd;
   case nir_atomic_op_umin:
      return SpvOpAtomicUMin;
   case nir_atomic_op_imin:
      return SpvOpAtomicSMin;
   case nir_atomic_op_umax:
      return SpvOpAtomicUMax;
   case nir_atomic_op_imax:
      return SpvOpAtomicSMax;
   case nir_atomic_op_iand:
      return SpvOpAtomicAnd;
   case nir_atomic_op_ior:
      return SpvOpAtomicOr;
   case nir_atomic_op_ixor:
      return SpvOpAtomicXor;
   case nir_atomic_op_xchg:
      return SpvOpAtomicExchange;
   case nir_atomic_op_cmpxchg:
      return SpvOpAtomicCompareExchange;
   default:
      debug_printf("%s - ", nir_intrinsic_infos[op].name);
      UNREACHABLE("unhandled atomic op");
   }
   return 0;
}

static SpvId
emit_float_const(struct ntv_context *ctx, int bit_size, double value)
{
   assert(bit_size == 16 || bit_size == 32 || bit_size == 64);
   return spirv_builder_const_float(&ctx->builder, bit_size, value);
}

static SpvId
emit_uint_const(struct ntv_context *ctx, int bit_size, uint64_t value)
{
   assert(bit_size == 8 || bit_size == 16 || bit_size == 32 || bit_size == 64);
   return spirv_builder_const_uint(&ctx->builder, bit_size, value);
}

static SpvId
emit_int_const(struct ntv_context *ctx, int bit_size, int64_t value)
{
   assert(bit_size == 8 || bit_size == 16 || bit_size == 32 || bit_size == 64);
   return spirv_builder_const_int(&ctx->builder, bit_size, value);
}

static SpvId
get_fvec_type(struct ntv_context *ctx, unsigned bit_size, unsigned num_components)
{
   assert(bit_size == 16 || bit_size == 32 || bit_size == 64);

   SpvId float_type = spirv_builder_type_float(&ctx->builder, bit_size);
   if (num_components > 1)
      return spirv_builder_type_vector(&ctx->builder, float_type,
                                       num_components);

   assert(num_components == 1);
   return float_type;
}

static SpvId
get_ivec_type(struct ntv_context *ctx, unsigned bit_size, unsigned num_components)
{
   assert(bit_size == 8 || bit_size == 16 || bit_size == 32 || bit_size == 64);

   SpvId int_type = spirv_builder_type_int(&ctx->builder, bit_size);
   if (num_components > 1)
      return spirv_builder_type_vector(&ctx->builder, int_type,
                                       num_components);

   assert(num_components == 1);
   return int_type;
}

static SpvId
get_uvec_type(struct ntv_context *ctx, unsigned bit_size, unsigned num_components)
{
   assert(bit_size == 8 || bit_size == 16 || bit_size == 32 || bit_size == 64);

   SpvId uint_type = spirv_builder_type_uint(&ctx->builder, bit_size);
   if (num_components > 1)
      return spirv_builder_type_vector(&ctx->builder, uint_type,
                                       num_components);

   assert(num_components == 1);
   return uint_type;
}

static SpvId
get_alu_type(struct ntv_context *ctx, nir_alu_type type, unsigned num_components, unsigned bit_size)
{
   if (bit_size == 1)
      return get_bvec_type(ctx, num_components);

   type = nir_alu_type_get_base_type(type);
   switch (nir_alu_type_get_base_type(type)) {
   case nir_type_bool:
      return get_bvec_type(ctx, num_components);

   case nir_type_int:
      return get_ivec_type(ctx, bit_size, num_components);

   case nir_type_uint:
      return get_uvec_type(ctx, bit_size, num_components);

   case nir_type_float:
      return get_fvec_type(ctx, bit_size, num_components);

   default:
      UNREACHABLE("unsupported nir_alu_type");
   }
}

static SpvStorageClass
get_storage_class(struct nir_variable *var)
{
   switch (var->data.mode) {
   case nir_var_function_temp:
      return SpvStorageClassFunction;
   case nir_var_mem_push_const:
      return SpvStorageClassPushConstant;
   case nir_var_shader_in:
      return SpvStorageClassInput;
   case nir_var_shader_out:
      return SpvStorageClassOutput;
   case nir_var_uniform:
   case nir_var_image:
      return SpvStorageClassUniformConstant;
   case nir_var_mem_ubo:
      return SpvStorageClassUniform;
   case nir_var_mem_ssbo:
      return SpvStorageClassStorageBuffer;
   default:
      UNREACHABLE("Unsupported nir_variable_mode");
   }
   return 0;
}

static SpvId
get_def_uvec_type(struct ntv_context *ctx, nir_def *def)
{
   unsigned bit_size = def->bit_size;
   return get_uvec_type(ctx, bit_size, def->num_components);
}

static SpvId
get_glsl_basetype(struct ntv_context *ctx, enum glsl_base_type type)
{
   switch (type) {
   case GLSL_TYPE_BOOL:
      return spirv_builder_type_bool(&ctx->builder);

   case GLSL_TYPE_FLOAT16:
      return spirv_builder_type_float(&ctx->builder, 16);

   case GLSL_TYPE_FLOAT:
      return spirv_builder_type_float(&ctx->builder, 32);

   case GLSL_TYPE_INT:
      return spirv_builder_type_int(&ctx->builder, 32);

   case GLSL_TYPE_UINT:
      return spirv_builder_type_uint(&ctx->builder, 32);

   case GLSL_TYPE_DOUBLE:
      return spirv_builder_type_float(&ctx->builder, 64);

   case GLSL_TYPE_INT64:
      return spirv_builder_type_int(&ctx->builder, 64);

   case GLSL_TYPE_UINT64:
      return spirv_builder_type_uint(&ctx->builder, 64);

   case GLSL_TYPE_UINT16:
      return spirv_builder_type_uint(&ctx->builder, 16);
   case GLSL_TYPE_INT16:
      return spirv_builder_type_int(&ctx->builder, 16);
   case GLSL_TYPE_INT8:
      return spirv_builder_type_int(&ctx->builder, 8);
   case GLSL_TYPE_UINT8:
      return spirv_builder_type_uint(&ctx->builder, 8);

   default:
      UNREACHABLE("unknown GLSL type");
   }
}

static SpvId
get_glsl_type(struct ntv_context *ctx, const struct glsl_type *type, bool implicit_stride)
{
   assert(type);
   if (glsl_type_is_scalar(type))
      return get_glsl_basetype(ctx, glsl_get_base_type(type));

   if (glsl_type_is_vector(type))
      return spirv_builder_type_vector(&ctx->builder,
         get_glsl_basetype(ctx, glsl_get_base_type(type)),
         glsl_get_vector_elements(type));

   if (glsl_type_is_matrix(type))
      return spirv_builder_type_matrix(&ctx->builder,
                                       spirv_builder_type_vector(&ctx->builder,
                                                                 get_glsl_basetype(ctx, glsl_get_base_type(type)),
                                                                 glsl_get_vector_elements(type)),
                                       glsl_get_matrix_columns(type));

   /* Aggregate types aren't cached in spirv_builder, so let's cache
    * them here instead.
    */

   struct hash_entry *entry =
      _mesa_hash_table_search(ctx->glsl_types[implicit_stride], type);
   if (entry)
      return (SpvId)(uintptr_t)entry->data;

   SpvId ret;
   if (glsl_type_is_array(type)) {
      SpvId element_type = get_glsl_type(ctx, glsl_get_array_element(type), implicit_stride);
      if (glsl_type_is_unsized_array(type))
         ret = spirv_builder_type_runtime_array(&ctx->builder, element_type);
      else
         ret = spirv_builder_type_array(&ctx->builder,
                                        element_type,
                                        emit_uint_const(ctx, 32, glsl_get_length(type)));
      if (!implicit_stride) {
         uint32_t stride = glsl_get_explicit_stride(type);
         if (!stride && glsl_type_is_scalar(glsl_get_array_element(type))) {
            stride = MAX2(glsl_get_bit_size(glsl_get_array_element(type)) / 8, 1);
         }
         if (stride)
            spirv_builder_emit_array_stride(&ctx->builder, ret, stride);
      }
   } else if (glsl_type_is_struct_or_ifc(type)) {
      const unsigned length = glsl_get_length(type);

      /* allocate some SpvId on the stack, falling back to the heap if the array is too long */
      SpvId *types, types_stack[16];

      if (length <= ARRAY_SIZE(types_stack)) {
         types = types_stack;
      } else {
         types = ralloc_array_size(ctx->mem_ctx, sizeof(SpvId), length);
         assert(types != NULL);
      }

      for (unsigned i = 0; i < glsl_get_length(type); i++)
         types[i] = get_glsl_type(ctx, glsl_get_struct_field(type, i), implicit_stride);
      ret = spirv_builder_type_struct(&ctx->builder, types,
                                      glsl_get_length(type));
      for (unsigned i = 0; i < glsl_get_length(type); i++) {
         int32_t offset = glsl_get_struct_field_offset(type, i);
         if (offset >= 0)
            spirv_builder_emit_member_offset(&ctx->builder, ret, i, offset);
      }
   } else
      UNREACHABLE("Unhandled GLSL type");

   _mesa_hash_table_insert(ctx->glsl_types[implicit_stride], type, (void *)(uintptr_t)ret);
   return ret;
}

static void
create_scratch_block(struct ntv_context *ctx, unsigned scratch_size, unsigned bit_size)
{
   unsigned idx = bit_size >> 4;
   SpvId type = spirv_builder_type_uint(&ctx->builder, bit_size);
   unsigned block_size = scratch_size / (bit_size / 8);
   assert(block_size);
   SpvId array = spirv_builder_type_array(&ctx->builder, type, emit_uint_const(ctx, 32, block_size));
   spirv_builder_emit_array_stride(&ctx->builder, array, bit_size / 8);
   SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                               SpvStorageClassPrivate,
                                               array);
   ctx->scratch_block_var[idx] = spirv_builder_emit_var(&ctx->builder, ptr_type, SpvStorageClassPrivate);
   if (ctx->spirv_1_4_interfaces) {
      assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
      ctx->entry_ifaces[ctx->num_entry_ifaces++] = ctx->scratch_block_var[idx];
   }
}

static SpvId
get_scratch_block(struct ntv_context *ctx, unsigned bit_size)
{
   unsigned idx = bit_size >> 4;
   if (!ctx->scratch_block_var[idx])
      create_scratch_block(ctx, ctx->nir->scratch_size, bit_size);
   return ctx->scratch_block_var[idx];
}

static void
create_shared_block(struct ntv_context *ctx, unsigned bit_size)
{
   unsigned idx = bit_size >> 4;
   SpvId type = spirv_builder_type_uint(&ctx->builder, bit_size);
   SpvId array;

   assert(gl_shader_stage_is_compute(ctx->nir->info.stage));
   if (ctx->nir->info.cs.has_variable_shared_mem) {
      assert(ctx->shared_mem_size);
      SpvId const_shared_size = emit_uint_const(ctx, 32, ctx->nir->info.shared_size);
      SpvId shared_mem_size = spirv_builder_emit_triop(&ctx->builder, SpvOpSpecConstantOp, spirv_builder_type_uint(&ctx->builder, 32), SpvOpIAdd, const_shared_size, ctx->shared_mem_size);
      shared_mem_size = spirv_builder_emit_triop(&ctx->builder, SpvOpSpecConstantOp, spirv_builder_type_uint(&ctx->builder, 32), SpvOpUDiv, shared_mem_size, emit_uint_const(ctx, 32, bit_size / 8));
      array = spirv_builder_type_array(&ctx->builder, type, shared_mem_size);
   } else {
      unsigned block_size = ctx->nir->info.shared_size / (bit_size / 8);
      assert(block_size);
      array = spirv_builder_type_array(&ctx->builder, type, emit_uint_const(ctx, 32, block_size));
   }

   ctx->shared_block_arr_type[idx] = array;
   if (ctx->sinfo->have_workgroup_memory_explicit_layout)
      spirv_builder_emit_array_stride(&ctx->builder, array, bit_size / 8);

   /* Create wrapper struct for Block, Offset and Aliased decorations. */
   SpvId block = spirv_builder_type_struct(&ctx->builder, &array, 1);

   SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                               SpvStorageClassWorkgroup,
                                               block);
   ctx->shared_block_var[idx] = spirv_builder_emit_var(&ctx->builder, ptr_type, SpvStorageClassWorkgroup);
   if (ctx->spirv_1_4_interfaces) {
      assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
      ctx->entry_ifaces[ctx->num_entry_ifaces++] = ctx->shared_block_var[idx];
   }
   /* Alias our shared memory blocks */
   if (ctx->sinfo->have_workgroup_memory_explicit_layout) {
      spirv_builder_emit_member_offset(&ctx->builder, block, 0, 0);
      spirv_builder_emit_decoration(&ctx->builder, block, SpvDecorationBlock);
      spirv_builder_emit_decoration(&ctx->builder, ctx->shared_block_var[idx], SpvDecorationAliased);
   }
}

static SpvId
get_shared_block(struct ntv_context *ctx, unsigned bit_size)
{
   unsigned idx = bit_size >> 4;
   if (!ctx->shared_block_var[idx])
      create_shared_block(ctx, bit_size);
   if (ctx->sinfo->have_workgroup_memory_explicit_layout) {
      spirv_builder_emit_extension(&ctx->builder, "SPV_KHR_workgroup_memory_explicit_layout");
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityWorkgroupMemoryExplicitLayoutKHR);
      if (ctx->shared_block_var[0])
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilityWorkgroupMemoryExplicitLayout8BitAccessKHR);
      if (ctx->shared_block_var[1])
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilityWorkgroupMemoryExplicitLayout16BitAccessKHR);
   }

   SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                               SpvStorageClassWorkgroup,
                                               ctx->shared_block_arr_type[idx]);
   SpvId zero = emit_uint_const(ctx, 32, 0);

   return spirv_builder_emit_access_chain(&ctx->builder, ptr_type,
                                          ctx->shared_block_var[idx], &zero, 1);
}

#define HANDLE_EMIT_BUILTIN(SLOT, BUILTIN) \
      case VARYING_SLOT_##SLOT: \
         spirv_builder_emit_builtin(&ctx->builder, var_id, SpvBuiltIn##BUILTIN); \
         break


static SpvId
input_var_init(struct ntv_context *ctx, struct nir_variable *var)
{
   SpvId var_type = get_glsl_type(ctx, var->type, false);
   SpvStorageClass sc = get_storage_class(var);
   if (sc == SpvStorageClassPushConstant)
      spirv_builder_emit_decoration(&ctx->builder, var_type, SpvDecorationBlock);
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   sc, var_type);
   SpvId var_id = spirv_builder_emit_var(&ctx->builder, pointer_type, sc);

   if (var->name)
      spirv_builder_emit_name(&ctx->builder, var_id, var->name);

   if (var->data.mode == nir_var_mem_push_const) {
      ctx->push_const_var = var_id;

      if (ctx->spirv_1_4_interfaces) {
         assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
         ctx->entry_ifaces[ctx->num_entry_ifaces++] = var_id;
      }
   }
   return var_id;
}

static void
emit_interpolation(struct ntv_context *ctx, SpvId var_id,
                   enum glsl_interp_mode mode)
{
   switch (mode) {
   case INTERP_MODE_NONE:
   case INTERP_MODE_SMOOTH:
      /* XXX spirv doesn't seem to have anything for this */
      break;
   case INTERP_MODE_FLAT:
      spirv_builder_emit_decoration(&ctx->builder, var_id,
                                    SpvDecorationFlat);
      break;
   case INTERP_MODE_EXPLICIT:
      spirv_builder_emit_decoration(&ctx->builder, var_id,
                                    SpvDecorationExplicitInterpAMD);
      break;
   case INTERP_MODE_NOPERSPECTIVE:
      spirv_builder_emit_decoration(&ctx->builder, var_id,
                                    SpvDecorationNoPerspective);
      break;
   default:
      UNREACHABLE("unknown interpolation value");
   }
}

static void
emit_input(struct ntv_context *ctx, struct nir_variable *var)
{
   SpvId var_id = input_var_init(ctx, var);
   if (ctx->stage == MESA_SHADER_VERTEX)
      spirv_builder_emit_location(&ctx->builder, var_id,
                                  var->data.driver_location);
   else if (ctx->stage == MESA_SHADER_FRAGMENT) {
      switch (var->data.location) {
      HANDLE_EMIT_BUILTIN(POS, FragCoord);
      HANDLE_EMIT_BUILTIN(LAYER, Layer);
      HANDLE_EMIT_BUILTIN(PRIMITIVE_ID, PrimitiveId);
      HANDLE_EMIT_BUILTIN(CLIP_DIST0, ClipDistance);
      HANDLE_EMIT_BUILTIN(CULL_DIST0, CullDistance);
      HANDLE_EMIT_BUILTIN(VIEWPORT, ViewportIndex);
      HANDLE_EMIT_BUILTIN(FACE, FrontFacing);

      default:
         spirv_builder_emit_location(&ctx->builder, var_id,
                                     var->data.driver_location);
      }
      if (var->data.centroid)
         spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationCentroid);
      else if (var->data.sample)
         spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationSample);
      emit_interpolation(ctx, var_id, var->data.interpolation);
   } else if (ctx->stage < MESA_SHADER_FRAGMENT) {
      switch (var->data.location) {
      HANDLE_EMIT_BUILTIN(POS, Position);
      HANDLE_EMIT_BUILTIN(PSIZ, PointSize);
      HANDLE_EMIT_BUILTIN(LAYER, Layer);
      HANDLE_EMIT_BUILTIN(PRIMITIVE_ID, PrimitiveId);
      HANDLE_EMIT_BUILTIN(CULL_DIST0, CullDistance);
      HANDLE_EMIT_BUILTIN(VIEWPORT, ViewportIndex);
      HANDLE_EMIT_BUILTIN(TESS_LEVEL_OUTER, TessLevelOuter);
      HANDLE_EMIT_BUILTIN(TESS_LEVEL_INNER, TessLevelInner);

      case VARYING_SLOT_CLIP_DIST0:
         assert(glsl_type_is_array(var->type));
         spirv_builder_emit_builtin(&ctx->builder, var_id, SpvBuiltInClipDistance);
         break;

      default:
         spirv_builder_emit_location(&ctx->builder, var_id,
                                     var->data.driver_location);
      }
   }

   if (var->data.location_frac)
      spirv_builder_emit_component(&ctx->builder, var_id,
                                   var->data.location_frac);

   if (var->data.patch)
      spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationPatch);

   _mesa_hash_table_insert(ctx->vars, var, (void *)(intptr_t)var_id);

   assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
   ctx->entry_ifaces[ctx->num_entry_ifaces++] = var_id;
}

static void
emit_output(struct ntv_context *ctx, struct nir_variable *var)
{
   SpvId var_type = get_glsl_type(ctx, var->type, false);

   /* SampleMask is always an array in spirv */
   if (ctx->stage == MESA_SHADER_FRAGMENT && var->data.location == FRAG_RESULT_SAMPLE_MASK)
      ctx->sample_mask_type = var_type = spirv_builder_type_array(&ctx->builder, var_type, emit_uint_const(ctx, 32, 1));
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassOutput,
                                                   var_type);
   SpvId var_id = spirv_builder_emit_var(&ctx->builder, pointer_type,
                                         SpvStorageClassOutput);
   if (var->name)
      spirv_builder_emit_name(&ctx->builder, var_id, var->name);

   if (var->data.precision == GLSL_PRECISION_MEDIUM || var->data.precision == GLSL_PRECISION_LOW) {
      spirv_builder_emit_decoration(&ctx->builder, var_id,
                                    SpvDecorationRelaxedPrecision);
   }

   if (ctx->stage != MESA_SHADER_FRAGMENT) {
      switch (var->data.location) {
      HANDLE_EMIT_BUILTIN(POS, Position);
      HANDLE_EMIT_BUILTIN(PSIZ, PointSize);
      HANDLE_EMIT_BUILTIN(LAYER, Layer);
      HANDLE_EMIT_BUILTIN(PRIMITIVE_ID, PrimitiveId);
      HANDLE_EMIT_BUILTIN(CLIP_DIST0, ClipDistance);
      HANDLE_EMIT_BUILTIN(CULL_DIST0, CullDistance);
      HANDLE_EMIT_BUILTIN(VIEWPORT, ViewportIndex);
      HANDLE_EMIT_BUILTIN(TESS_LEVEL_OUTER, TessLevelOuter);
      HANDLE_EMIT_BUILTIN(TESS_LEVEL_INNER, TessLevelInner);

      default:
         /* non-xfb psiz output will have location -1 */
         if (var->data.location >= 0)
            spirv_builder_emit_location(&ctx->builder, var_id,
                                        var->data.driver_location);
      }
      emit_interpolation(ctx, var_id, var->data.interpolation);
   } else {
      if (var->data.location >= FRAG_RESULT_DATA0) {
         spirv_builder_emit_location(&ctx->builder, var_id,
                                     var->data.location - FRAG_RESULT_DATA0);
         spirv_builder_emit_index(&ctx->builder, var_id, var->data.index);
      } else {
         switch (var->data.location) {
         case FRAG_RESULT_COLOR:
            UNREACHABLE("gl_FragColor should be lowered by now");

         case FRAG_RESULT_DEPTH:
            spirv_builder_emit_builtin(&ctx->builder, var_id, SpvBuiltInFragDepth);
            break;

         case FRAG_RESULT_SAMPLE_MASK:
            spirv_builder_emit_builtin(&ctx->builder, var_id, SpvBuiltInSampleMask);
            break;

         case FRAG_RESULT_STENCIL:
            spirv_builder_emit_builtin(&ctx->builder, var_id, SpvBuiltInFragStencilRefEXT);
            break;

         default:
            spirv_builder_emit_location(&ctx->builder, var_id,
                                        var->data.location);
            spirv_builder_emit_index(&ctx->builder, var_id, var->data.index);
         }
      }
      if (var->data.sample)
         spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationSample);
   }

   if (var->data.location_frac)
      spirv_builder_emit_component(&ctx->builder, var_id,
                                   var->data.location_frac);

   if (var->data.patch)
      spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationPatch);

   if (var->data.explicit_xfb_buffer && ctx->nir->xfb_info) {
      spirv_builder_emit_offset(&ctx->builder, var_id, var->data.offset);
      spirv_builder_emit_xfb_buffer(&ctx->builder, var_id, var->data.xfb.buffer);
      spirv_builder_emit_xfb_stride(&ctx->builder, var_id, var->data.xfb.stride);
      if (var->data.stream)
         spirv_builder_emit_stream(&ctx->builder, var_id, var->data.stream);
   }

   _mesa_hash_table_insert(ctx->vars, var, (void *)(intptr_t)var_id);

   assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
   ctx->entry_ifaces[ctx->num_entry_ifaces++] = var_id;
}

static void
emit_shader_temp(struct ntv_context *ctx, struct nir_variable *var)
{
   SpvId var_type = get_glsl_type(ctx, var->type, true);

   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassPrivate,
                                                   var_type);
   SpvId var_id = spirv_builder_emit_var(&ctx->builder, pointer_type,
                                         SpvStorageClassPrivate);
   if (var->name)
      spirv_builder_emit_name(&ctx->builder, var_id, var->name);

   _mesa_hash_table_insert(ctx->vars, var, (void *)(intptr_t)var_id);

   assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
   ctx->entry_ifaces[ctx->num_entry_ifaces++] = var_id;
}

static void
emit_temp(struct ntv_context *ctx, struct nir_variable *var)
{
   SpvId var_type = get_glsl_type(ctx, var->type, true);

   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassFunction,
                                                   var_type);
   SpvId var_id = spirv_builder_emit_var(&ctx->builder, pointer_type,
                                         SpvStorageClassFunction);
   if (var->name)
      spirv_builder_emit_name(&ctx->builder, var_id, var->name);

   _mesa_hash_table_insert(ctx->vars, var, (void *)(intptr_t)var_id);
}

static SpvDim
type_to_dim(enum glsl_sampler_dim gdim, bool *is_ms)
{
   *is_ms = false;
   switch (gdim) {
   case GLSL_SAMPLER_DIM_1D:
      return SpvDim1D;
   case GLSL_SAMPLER_DIM_2D:
      return SpvDim2D;
   case GLSL_SAMPLER_DIM_3D:
      return SpvDim3D;
   case GLSL_SAMPLER_DIM_CUBE:
      return SpvDimCube;
   case GLSL_SAMPLER_DIM_RECT:
      return SpvDim2D;
   case GLSL_SAMPLER_DIM_BUF:
      return SpvDimBuffer;
   case GLSL_SAMPLER_DIM_EXTERNAL:
      return SpvDim2D; /* seems dodgy... */
   case GLSL_SAMPLER_DIM_MS:
      *is_ms = true;
      return SpvDim2D;
   case GLSL_SAMPLER_DIM_SUBPASS_MS:
      *is_ms = true;
      return SpvDimSubpassData;
   case GLSL_SAMPLER_DIM_SUBPASS:
      return SpvDimSubpassData;
   default:
      fprintf(stderr, "unknown sampler type %d\n", gdim);
      break;
   }
   return SpvDim2D;
}

static inline SpvImageFormat
get_shader_image_format(enum pipe_format format)
{
   switch (format) {
   case PIPE_FORMAT_R32G32B32A32_FLOAT:
      return SpvImageFormatRgba32f;
   case PIPE_FORMAT_R16G16B16A16_FLOAT:
      return SpvImageFormatRgba16f;
   case PIPE_FORMAT_R32_FLOAT:
      return SpvImageFormatR32f;
   case PIPE_FORMAT_R8G8B8A8_UNORM:
      return SpvImageFormatRgba8;
   case PIPE_FORMAT_R8G8B8A8_SNORM:
      return SpvImageFormatRgba8Snorm;
   case PIPE_FORMAT_R32G32B32A32_SINT:
      return SpvImageFormatRgba32i;
   case PIPE_FORMAT_R16G16B16A16_SINT:
      return SpvImageFormatRgba16i;
   case PIPE_FORMAT_R8G8B8A8_SINT:
      return SpvImageFormatRgba8i;
   case PIPE_FORMAT_R32_SINT:
      return SpvImageFormatR32i;
   case PIPE_FORMAT_R32G32B32A32_UINT:
      return SpvImageFormatRgba32ui;
   case PIPE_FORMAT_R16G16B16A16_UINT:
      return SpvImageFormatRgba16ui;
   case PIPE_FORMAT_R8G8B8A8_UINT:
      return SpvImageFormatRgba8ui;
   case PIPE_FORMAT_R32_UINT:
      return SpvImageFormatR32ui;
   default:
      return SpvImageFormatUnknown;
   }
}

static inline SpvImageFormat
get_extended_image_format(enum pipe_format format)
{
   switch (format) {
   case PIPE_FORMAT_R32G32_FLOAT:
      return SpvImageFormatRg32f;
   case PIPE_FORMAT_R16G16_FLOAT:
      return SpvImageFormatRg16f;
   case PIPE_FORMAT_R11G11B10_FLOAT:
      return SpvImageFormatR11fG11fB10f;
   case PIPE_FORMAT_R16_FLOAT:
      return SpvImageFormatR16f;
   case PIPE_FORMAT_R16G16B16A16_UNORM:
      return SpvImageFormatRgba16;
   case PIPE_FORMAT_R10G10B10A2_UNORM:
      return SpvImageFormatRgb10A2;
   case PIPE_FORMAT_R16G16_UNORM:
      return SpvImageFormatRg16;
   case PIPE_FORMAT_R8G8_UNORM:
      return SpvImageFormatRg8;
   case PIPE_FORMAT_R16_UNORM:
      return SpvImageFormatR16;
   case PIPE_FORMAT_R8_UNORM:
      return SpvImageFormatR8;
   case PIPE_FORMAT_R16G16B16A16_SNORM:
      return SpvImageFormatRgba16Snorm;
   case PIPE_FORMAT_R16G16_SNORM:
      return SpvImageFormatRg16Snorm;
   case PIPE_FORMAT_R8G8_SNORM:
      return SpvImageFormatRg8Snorm;
   case PIPE_FORMAT_R16_SNORM:
      return SpvImageFormatR16Snorm;
   case PIPE_FORMAT_R8_SNORM:
      return SpvImageFormatR8Snorm;
   case PIPE_FORMAT_R32G32_SINT:
      return SpvImageFormatRg32i;
   case PIPE_FORMAT_R16G16_SINT:
      return SpvImageFormatRg16i;
   case PIPE_FORMAT_R8G8_SINT:
      return SpvImageFormatRg8i;
   case PIPE_FORMAT_R16_SINT:
      return SpvImageFormatR16i;
   case PIPE_FORMAT_R8_SINT:
      return SpvImageFormatR8i;
   case PIPE_FORMAT_R10G10B10A2_UINT:
      return SpvImageFormatRgb10a2ui;
   case PIPE_FORMAT_R32G32_UINT:
      return SpvImageFormatRg32ui;
   case PIPE_FORMAT_R16G16_UINT:
      return SpvImageFormatRg16ui;
   case PIPE_FORMAT_R8G8_UINT:
      return SpvImageFormatRg8ui;
   case PIPE_FORMAT_R16_UINT:
      return SpvImageFormatR16ui;
   case PIPE_FORMAT_R8_UINT:
      return SpvImageFormatR8ui;

   default:
      return SpvImageFormatUnknown;
   }
}

static inline SpvImageFormat
get_image_format(struct ntv_context *ctx, enum pipe_format format)
{
   /* always supported */
   if (format == PIPE_FORMAT_NONE)
      return SpvImageFormatUnknown;

   SpvImageFormat ret = get_shader_image_format(format);
   if (ret != SpvImageFormatUnknown) {
      /* requires the shader-cap, but we already emit that */
      return ret;
   }

   ret = get_extended_image_format(format);
   assert(ret != SpvImageFormatUnknown);
   spirv_builder_emit_cap(&ctx->builder,
                          SpvCapabilityStorageImageExtendedFormats);
   return ret;
}

static SpvId
get_bare_image_type(struct ntv_context *ctx, struct nir_variable *var, bool is_sampler)
{
   const struct glsl_type *type = glsl_without_array(var->type);

   bool is_ms;

   if (var->data.fb_fetch_output) {
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityInputAttachment);
   } else if (!is_sampler && !var->data.image.format) {
      if (!(var->data.access & ACCESS_NON_WRITEABLE))
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilityStorageImageWriteWithoutFormat);
      if (!(var->data.access & ACCESS_NON_READABLE))
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilityStorageImageReadWithoutFormat);
   }

   SpvDim dimension = type_to_dim(glsl_get_sampler_dim(type), &is_ms);
   if (dimension == SpvDim1D) {
      if (is_sampler)
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilitySampled1D);
      else
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilityImage1D);
   }
   if (dimension == SpvDimBuffer) {
      if (is_sampler)
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilitySampledBuffer);
      else
         spirv_builder_emit_cap(&ctx->builder, SpvCapabilityImageBuffer);
   }

   bool arrayed = glsl_sampler_type_is_array(type);
   if (dimension == SpvDimCube && arrayed)
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityImageCubeArray);
   if (arrayed && !is_sampler && is_ms)
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityImageMSArray);

   SpvId result_type = get_glsl_basetype(ctx, glsl_get_sampler_result_type(type));
   return spirv_builder_type_image(&ctx->builder, result_type,
                                               dimension, false,
                                               arrayed,
                                               is_ms, is_sampler ? 1 : 2,
                                               get_image_format(ctx, var->data.image.format));
}

static SpvId
get_image_type(struct ntv_context *ctx, struct nir_variable *var,
               bool is_sampler, bool is_buffer)
{
   SpvId image_type = get_bare_image_type(ctx, var, is_sampler);
   return is_sampler && ctx->stage != MESA_SHADER_KERNEL && !is_buffer ?
          spirv_builder_type_sampled_image(&ctx->builder, image_type) :
          image_type;
}

static SpvId
emit_image(struct ntv_context *ctx, struct nir_variable *var, SpvId image_type)
{
   if (var->data.bindless)
      return 0;
   const struct glsl_type *type = glsl_without_array(var->type);

   bool is_sampler = glsl_type_is_sampler(type);
   bool is_buffer = glsl_get_sampler_dim(type) == GLSL_SAMPLER_DIM_BUF;
   SpvId var_type = is_sampler && ctx->stage != MESA_SHADER_KERNEL && !is_buffer ?
      spirv_builder_type_sampled_image(&ctx->builder, image_type) : image_type;

   bool mediump = (var->data.precision == GLSL_PRECISION_MEDIUM || var->data.precision == GLSL_PRECISION_LOW);

   int index = var->data.driver_location;
   assert(!find_image_type(ctx, var));

   if (glsl_type_is_array(var->type)) {
      var_type = spirv_builder_type_array(&ctx->builder, var_type,
                                              emit_uint_const(ctx, 32, glsl_get_aoa_size(var->type)));
   }
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassUniformConstant,
                                                   var_type);

   SpvId var_id = spirv_builder_emit_var(&ctx->builder, pointer_type,
                                         SpvStorageClassUniformConstant);

   if (mediump) {
      spirv_builder_emit_decoration(&ctx->builder, var_id,
                                    SpvDecorationRelaxedPrecision);
   }

   if (var->name)
      spirv_builder_emit_name(&ctx->builder, var_id, var->name);

   if (var->data.fb_fetch_output)
      spirv_builder_emit_input_attachment_index(&ctx->builder, var_id, var->data.index);

   _mesa_hash_table_insert(ctx->vars, var, (void *)(intptr_t)var_id);
   if (is_sampler) {
      if (var->data.descriptor_set == ctx->bindless_set_idx) {
         assert(!ctx->bindless_samplers[index]);
         ctx->bindless_samplers[index] = var_id;
      } else {
         assert(!ctx->samplers[index]);
         ctx->samplers[index] = var_id;
      }
   } else {
      assert(!ctx->images[index]);
      ctx->images[index] = var_id;
      emit_access_decorations(ctx, var, var_id);
   }
   _mesa_hash_table_insert(&ctx->image_types, var, (void *)(intptr_t)image_type);
   if (ctx->spirv_1_4_interfaces) {
      assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
      ctx->entry_ifaces[ctx->num_entry_ifaces++] = var_id;
   }

   spirv_builder_emit_descriptor_set(&ctx->builder, var_id, var->data.descriptor_set);
   spirv_builder_emit_binding(&ctx->builder, var_id, var->data.binding);
   return var_id;
}

static void
emit_sampler(struct ntv_context *ctx, nir_variable *var)
{
   SpvId type = spirv_builder_type_sampler(&ctx->builder);
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassUniformConstant,
                                                   type);

   SpvId var_id = spirv_builder_emit_var(&ctx->builder, pointer_type,
                                         SpvStorageClassUniformConstant);
   char buf[128];
   snprintf(buf, sizeof(buf), "sampler_%u", var->data.driver_location);
   spirv_builder_emit_name(&ctx->builder, var_id, buf);
   spirv_builder_emit_descriptor_set(&ctx->builder, var_id, var->data.descriptor_set);
   spirv_builder_emit_binding(&ctx->builder, var_id, var->data.driver_location);
   _mesa_hash_table_insert(ctx->vars, var, (void *)(intptr_t)var_id);
   if (ctx->spirv_1_4_interfaces) {
      assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
      ctx->entry_ifaces[ctx->num_entry_ifaces++] = var_id;
   }
}

static SpvId
get_sized_uint_array_type(struct ntv_context *ctx, unsigned array_size, unsigned bitsize)
{
   SpvId array_length = emit_uint_const(ctx, 32, array_size);
   SpvId array_type = spirv_builder_type_array(&ctx->builder, get_uvec_type(ctx, bitsize, 1),
                                            array_length);
   spirv_builder_emit_array_stride(&ctx->builder, array_type, bitsize / 8);
   return array_type;
}

/* get array<struct(array_type <--this one)> */
static SpvId
get_bo_array_type(struct ntv_context *ctx, struct nir_variable *var)
{
   struct hash_entry *he = _mesa_hash_table_search(ctx->bo_array_types, var);
   if (he)
      return (SpvId)(uintptr_t)he->data;
   unsigned bitsize = glsl_get_bit_size(glsl_get_array_element(glsl_get_struct_field(glsl_without_array(var->type), 0)));
   assert(bitsize);
   SpvId array_type;
   const struct glsl_type *type = glsl_without_array(var->type);
   const struct glsl_type *first_type = glsl_get_struct_field(type, 0);
   if (!glsl_type_is_unsized_array(first_type)) {
      uint32_t array_size = glsl_get_length(first_type);
      assert(array_size);
      return get_sized_uint_array_type(ctx, array_size, bitsize);
   }
   SpvId uint_type = spirv_builder_type_uint(&ctx->builder, bitsize);
   array_type = spirv_builder_type_runtime_array(&ctx->builder, uint_type);
   spirv_builder_emit_array_stride(&ctx->builder, array_type, bitsize / 8);
   return array_type;
}

/* get array<struct(array_type) <--this one> */
static SpvId
get_bo_struct_type(struct ntv_context *ctx, struct nir_variable *var)
{
   struct hash_entry *he = _mesa_hash_table_search(ctx->bo_struct_types, var);
   if (he)
      return (SpvId)(uintptr_t)he->data;
   const struct glsl_type *bare_type = glsl_without_array(var->type);
   unsigned bitsize = glsl_get_bit_size(glsl_get_array_element(glsl_get_struct_field(bare_type, 0)));
   SpvId array_type = get_bo_array_type(ctx, var);
   _mesa_hash_table_insert(ctx->bo_array_types, var, (void *)(uintptr_t)array_type);
   bool ssbo = var->data.mode == nir_var_mem_ssbo;

   // wrap UBO-array in a struct
   SpvId runtime_array = 0;
   if (ssbo && glsl_get_length(bare_type) > 1) {
       const struct glsl_type *last_member = glsl_get_struct_field(bare_type, glsl_get_length(bare_type) - 1);
       if (glsl_type_is_unsized_array(last_member)) {
          runtime_array = spirv_builder_type_runtime_array(&ctx->builder, get_uvec_type(ctx, bitsize, 1));
          spirv_builder_emit_array_stride(&ctx->builder, runtime_array, glsl_get_explicit_stride(last_member));
       }
   }
   SpvId types[] = {array_type, runtime_array};
   SpvId struct_type = spirv_builder_type_struct(&ctx->builder, types, 1 + !!runtime_array);
   if (var->name) {
      char struct_name[100];
      snprintf(struct_name, sizeof(struct_name), "struct_%s", var->name);
      spirv_builder_emit_name(&ctx->builder, struct_type, struct_name);
   }

   spirv_builder_emit_decoration(&ctx->builder, struct_type,
                                 SpvDecorationBlock);
   spirv_builder_emit_member_offset(&ctx->builder, struct_type, 0, 0);
   if (runtime_array)
      spirv_builder_emit_member_offset(&ctx->builder, struct_type, 1, 0);

   return struct_type;
}

static void
emit_bo(struct ntv_context *ctx, struct nir_variable *var, bool aliased)
{
   unsigned bitsize = glsl_get_bit_size(glsl_get_array_element(glsl_get_struct_field(glsl_without_array(var->type), 0)));
   bool ssbo = var->data.mode == nir_var_mem_ssbo;
   SpvId struct_type = get_bo_struct_type(ctx, var);
   _mesa_hash_table_insert(ctx->bo_struct_types, var, (void *)(uintptr_t)struct_type);
   SpvId array_length = emit_uint_const(ctx, 32, glsl_get_length(var->type));
   SpvId array_type = spirv_builder_type_array(&ctx->builder, struct_type, array_length);
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   ssbo ? SpvStorageClassStorageBuffer : SpvStorageClassUniform,
                                                   array_type);
   SpvId var_id = spirv_builder_emit_var(&ctx->builder, pointer_type,
                                         ssbo ? SpvStorageClassStorageBuffer : SpvStorageClassUniform);
   if (var->name)
      spirv_builder_emit_name(&ctx->builder, var_id, var->name);

   if (aliased)
      spirv_builder_emit_decoration(&ctx->builder, var_id, SpvDecorationAliased);

   unsigned idx = bitsize >> 4;
   assert(idx < ARRAY_SIZE(ctx->ssbos));
   if (ssbo) {
      assert(!ctx->ssbos[idx]);
      ctx->ssbos[idx] = var_id;
      if (bitsize == 32)
         ctx->ssbo_vars = var;
   } else {
      assert(!ctx->ubos[var->data.driver_location][idx]);
      ctx->ubos[var->data.driver_location][idx] = var_id;
      ctx->ubo_vars[var->data.driver_location] = var;
   }
   if (ctx->spirv_1_4_interfaces) {
      assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
      ctx->entry_ifaces[ctx->num_entry_ifaces++] = var_id;
   }
   _mesa_hash_table_insert(ctx->vars, var, (void *)(intptr_t)var_id);

   spirv_builder_emit_descriptor_set(&ctx->builder, var_id, var->data.descriptor_set);
   spirv_builder_emit_binding(&ctx->builder, var_id, var->data.binding);
}

static SpvId
get_vec_from_bit_size(struct ntv_context *ctx, uint32_t bit_size, uint32_t num_components)
{
   if (bit_size == 1)
      return get_bvec_type(ctx, num_components);
   return get_uvec_type(ctx, bit_size, num_components);
}

static SpvId
get_src_ssa(struct ntv_context *ctx, const nir_def *ssa, nir_alu_type *atype)
{
   assert(ssa->index < ctx->num_defs);
   assert(ctx->defs[ssa->index] != 0);
   *atype = ctx->def_types[ssa->index];
   return ctx->defs[ssa->index];
}

static void
init_reg(struct ntv_context *ctx, nir_intrinsic_instr *decl, nir_alu_type atype)
{
   unsigned index = decl->def.index;
   unsigned num_components = nir_intrinsic_num_components(decl);
   unsigned bit_size = nir_intrinsic_bit_size(decl);

   if (ctx->defs[index])
      return;

   SpvId type = get_alu_type(ctx, atype, num_components, bit_size);
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassFunction,
                                                   type);
   SpvId var = spirv_builder_emit_var(&ctx->builder, pointer_type,
                                       SpvStorageClassFunction);

   ctx->defs[index] = var;
   ctx->def_types[index] = nir_alu_type_get_base_type(atype);
}

static SpvId
get_src(struct ntv_context *ctx, nir_src *src, nir_alu_type *atype)
{
   return get_src_ssa(ctx, src->ssa, atype);
}

static SpvId
get_alu_src_raw(struct ntv_context *ctx, nir_alu_instr *alu, unsigned src, nir_alu_type *atype)
{
   SpvId def = get_src(ctx, &alu->src[src].src, atype);

   unsigned used_channels = 0;
   bool need_swizzle = false;
   for (unsigned i = 0; i < NIR_MAX_VEC_COMPONENTS; i++) {
      if (!nir_alu_instr_channel_used(alu, src, i))
         continue;

      used_channels++;

      if (alu->src[src].swizzle[i] != i)
         need_swizzle = true;
   }
   assert(used_channels != 0);

   unsigned live_channels = nir_src_num_components(alu->src[src].src);
   if (used_channels != live_channels)
      need_swizzle = true;

   if (!need_swizzle)
      return def;

   int bit_size = nir_src_bit_size(alu->src[src].src);
   SpvId raw_type = get_alu_type(ctx, *atype, 1, bit_size);

   if (used_channels == 1) {
      uint32_t indices[] =  { alu->src[src].swizzle[0] };
      return spirv_builder_emit_composite_extract(&ctx->builder, raw_type,
                                                  def, indices,
                                                  ARRAY_SIZE(indices));
   } else if (live_channels == 1) {
      SpvId raw_vec_type = spirv_builder_type_vector(&ctx->builder,
                                                     raw_type,
                                                     used_channels);

      SpvId constituents[NIR_MAX_VEC_COMPONENTS] = {0};
      for (unsigned i = 0; i < used_channels; ++i)
        constituents[i] = def;

      return spirv_builder_emit_composite_construct(&ctx->builder,
                                                    raw_vec_type,
                                                    constituents,
                                                    used_channels);
   } else {
      SpvId raw_vec_type = spirv_builder_type_vector(&ctx->builder,
                                                     raw_type,
                                                     used_channels);

      uint32_t components[NIR_MAX_VEC_COMPONENTS] = {0};
      size_t num_components = 0;
      for (unsigned i = 0; i < NIR_MAX_VEC_COMPONENTS; i++) {
         if (!nir_alu_instr_channel_used(alu, src, i))
            continue;

         components[num_components++] = alu->src[src].swizzle[i];
      }

      return spirv_builder_emit_vector_shuffle(&ctx->builder, raw_vec_type,
                                               def, def, components,
                                               num_components);
   }
}

static SpvId
emit_select(struct ntv_context *ctx, SpvId type, SpvId cond,
            SpvId if_true, SpvId if_false)
{
   return emit_triop(ctx, SpvOpSelect, type, cond, if_true, if_false);
}

static SpvId
emit_bitcast(struct ntv_context *ctx, SpvId type, SpvId value)
{
   return emit_unop(ctx, SpvOpBitcast, type, value);
}

static SpvId
bitcast_to_uvec(struct ntv_context *ctx, SpvId value, unsigned bit_size,
                unsigned num_components)
{
   SpvId type = get_uvec_type(ctx, bit_size, num_components);
   return emit_bitcast(ctx, type, value);
}

static SpvId
bitcast_to_ivec(struct ntv_context *ctx, SpvId value, unsigned bit_size,
                unsigned num_components)
{
   SpvId type = get_ivec_type(ctx, bit_size, num_components);
   return emit_bitcast(ctx, type, value);
}

static SpvId
bitcast_to_fvec(struct ntv_context *ctx, SpvId value, unsigned bit_size,
               unsigned num_components)
{
   SpvId type = get_fvec_type(ctx, bit_size, num_components);
   return emit_bitcast(ctx, type, value);
}

static SpvId
cast_src_to_type(struct ntv_context *ctx, SpvId value, nir_src src, nir_alu_type atype)
{
   atype = nir_alu_type_get_base_type(atype);
   unsigned num_components = nir_src_num_components(src);
   unsigned bit_size = nir_src_bit_size(src);
   return emit_bitcast(ctx, get_alu_type(ctx, atype, num_components, bit_size), value);
}

static void
store_def(struct ntv_context *ctx, unsigned def_index, SpvId result, nir_alu_type type)
{
   assert(result != 0);
   assert(def_index < ctx->num_defs);
   ctx->def_types[def_index] = nir_alu_type_get_base_type(type);
   ctx->defs[def_index] = result;
}

static SpvId
emit_unop(struct ntv_context *ctx, SpvOp op, SpvId type, SpvId src)
{
   return spirv_builder_emit_unop(&ctx->builder, op, type, src);
}

static SpvId
emit_atomic(struct ntv_context *ctx, SpvId op, SpvId type, SpvId src0, SpvId src1, SpvId src2)
{
   if (op == SpvOpAtomicLoad)
      return spirv_builder_emit_triop(&ctx->builder, op, type, src0, emit_uint_const(ctx, 32, SpvScopeDevice),
                                       emit_uint_const(ctx, 32, 0));
   if (op == SpvOpAtomicCompareExchange)
      return spirv_builder_emit_hexop(&ctx->builder, op, type, src0, emit_uint_const(ctx, 32, SpvScopeDevice),
                                       emit_uint_const(ctx, 32, 0),
                                       emit_uint_const(ctx, 32, 0),
                                       /* these params are intentionally swapped */
                                       src2, src1);

   return spirv_builder_emit_quadop(&ctx->builder, op, type, src0, emit_uint_const(ctx, 32, SpvScopeDevice),
                                    emit_uint_const(ctx, 32, 0), src1);
}

static SpvId
emit_binop(struct ntv_context *ctx, SpvOp op, SpvId type,
           SpvId src0, SpvId src1)
{
   return spirv_builder_emit_binop(&ctx->builder, op, type, src0, src1);
}

static SpvId
emit_triop(struct ntv_context *ctx, SpvOp op, SpvId type,
           SpvId src0, SpvId src1, SpvId src2)
{
   return spirv_builder_emit_triop(&ctx->builder, op, type, src0, src1, src2);
}

static SpvId
emit_builtin_unop(struct ntv_context *ctx, enum GLSLstd450 op, SpvId type,
                  SpvId src)
{
   SpvId args[] = { src };
   return spirv_builder_emit_ext_inst(&ctx->builder, type, ctx->GLSL_std_450,
                                      op, args, ARRAY_SIZE(args));
}

static SpvId
emit_builtin_binop(struct ntv_context *ctx, enum GLSLstd450 op, SpvId type,
                   SpvId src0, SpvId src1)
{
   SpvId args[] = { src0, src1 };
   return spirv_builder_emit_ext_inst(&ctx->builder, type, ctx->GLSL_std_450,
                                      op, args, ARRAY_SIZE(args));
}

static SpvId
emit_builtin_triop(struct ntv_context *ctx, enum GLSLstd450 op, SpvId type,
                   SpvId src0, SpvId src1, SpvId src2)
{
   SpvId args[] = { src0, src1, src2 };
   return spirv_builder_emit_ext_inst(&ctx->builder, type, ctx->GLSL_std_450,
                                      op, args, ARRAY_SIZE(args));
}

static SpvId
get_fvec_constant(struct ntv_context *ctx, unsigned bit_size,
                  unsigned num_components, double value)
{
   assert(bit_size == 16 || bit_size == 32 || bit_size == 64);

   SpvId result = emit_float_const(ctx, bit_size, value);
   if (num_components == 1)
      return result;

   assert(num_components > 1);
   SpvId components[NIR_MAX_VEC_COMPONENTS];
   for (int i = 0; i < num_components; i++)
      components[i] = result;

   SpvId type = get_fvec_type(ctx, bit_size, num_components);
   return spirv_builder_const_composite(&ctx->builder, type, components,
                                        num_components);
}

static SpvId
get_ivec_constant(struct ntv_context *ctx, unsigned bit_size,
                  unsigned num_components, int64_t value)
{
   assert(bit_size == 8 || bit_size == 16 || bit_size == 32 || bit_size == 64);

   SpvId result = emit_int_const(ctx, bit_size, value);
   if (num_components == 1)
      return result;

   assert(num_components > 1);
   SpvId components[NIR_MAX_VEC_COMPONENTS];
   for (int i = 0; i < num_components; i++)
      components[i] = result;

   SpvId type = get_ivec_type(ctx, bit_size, num_components);
   return spirv_builder_const_composite(&ctx->builder, type, components,
                                        num_components);
}

static inline unsigned
alu_instr_src_components(const nir_alu_instr *instr, unsigned src)
{
   if (nir_op_infos[instr->op].input_sizes[src] > 0)
      return nir_op_infos[instr->op].input_sizes[src];

   return instr->def.num_components;
}

static SpvId
get_alu_src(struct ntv_context *ctx, nir_alu_instr *alu, unsigned src, SpvId *raw_value, nir_alu_type *atype)
{
   *raw_value = get_alu_src_raw(ctx, alu, src, atype);

   unsigned num_components = alu_instr_src_components(alu, src);
   unsigned bit_size = nir_src_bit_size(alu->src[src].src);
   nir_alu_type type = alu_op_is_typeless(alu->op) ? *atype : nir_op_infos[alu->op].input_types[src];
   type = nir_alu_type_get_base_type(type);
   if (type == *atype)
      return *raw_value;

   if (bit_size == 1)
      return *raw_value;
   else {
      switch (nir_alu_type_get_base_type(type)) {
      case nir_type_bool:
         UNREACHABLE("bool should have bit-size 1");

      case nir_type_int:
         return bitcast_to_ivec(ctx, *raw_value, bit_size, num_components);

      case nir_type_uint:
         return bitcast_to_uvec(ctx, *raw_value, bit_size, num_components);

      case nir_type_float:
         return bitcast_to_fvec(ctx, *raw_value, bit_size, num_components);

      default:
         UNREACHABLE("unknown nir_alu_type");
      }
   }
}

static void
store_alu_result(struct ntv_context *ctx, nir_alu_instr *alu, SpvId result, nir_alu_type atype)
{
   store_def(ctx, alu->def.index, result, atype);
}

static SpvId
get_def_type(struct ntv_context *ctx, nir_def *def, nir_alu_type type)
{
   return get_alu_type(ctx, type, def->num_components, def->bit_size);
}

static void
emit_alu(struct ntv_context *ctx, nir_alu_instr *alu)
{
   bool is_bcsel = alu->op == nir_op_bcsel;
   nir_alu_type stype[NIR_MAX_VEC_COMPONENTS] = {0};
   SpvId src[NIR_MAX_VEC_COMPONENTS];
   SpvId raw_src[NIR_MAX_VEC_COMPONENTS];
   for (unsigned i = 0; i < nir_op_infos[alu->op].num_inputs; i++)
      src[i] = get_alu_src(ctx, alu, i, &raw_src[i], &stype[i]);

   nir_alu_type typeless_type = stype[is_bcsel];
   if (nir_op_infos[alu->op].num_inputs > 1 &&
       alu_op_is_typeless(alu->op) &&
       nir_src_bit_size(alu->src[is_bcsel].src) != 1) {
      unsigned uint_count = 0;
      unsigned int_count = 0;
      unsigned float_count = 0;
      for (unsigned i = is_bcsel; i < nir_op_infos[alu->op].num_inputs; i++) {
         if (stype[i] == nir_type_bool)
            break;
         switch (stype[i]) {
         case nir_type_uint:
            uint_count++;
            break;
         case nir_type_int:
            int_count++;
            break;
         case nir_type_float:
            float_count++;
            break;
         default:
            UNREACHABLE("this shouldn't happen");
         }
      }
      if (uint_count > int_count && uint_count > float_count)
         typeless_type = nir_type_uint;
      else if (int_count > uint_count && int_count > float_count)
         typeless_type = nir_type_int;
      else if (float_count > uint_count && float_count > int_count)
         typeless_type = nir_type_float;
      else if (float_count == uint_count || uint_count == int_count)
         typeless_type = nir_type_uint;
      else if (float_count == int_count)
         typeless_type = nir_type_float;
      else
         typeless_type = nir_type_uint;
      assert(typeless_type != nir_type_bool);
      for (unsigned i = is_bcsel; i < nir_op_infos[alu->op].num_inputs; i++) {
         unsigned num_components = alu_instr_src_components(alu, i);
         unsigned bit_size = nir_src_bit_size(alu->src[i].src);
         SpvId type = get_alu_type(ctx, typeless_type, num_components, bit_size);
         if (stype[i] != typeless_type) {
            src[i] = emit_bitcast(ctx, type, src[i]);
         }
      }
   }

   unsigned bit_size = alu->def.bit_size;
   unsigned num_components = alu->def.num_components;
   nir_alu_type atype = bit_size == 1 ?
                        nir_type_bool :
                        (alu_op_is_typeless(alu->op) ? typeless_type : nir_op_infos[alu->op].output_type);
   SpvId dest_type = get_def_type(ctx, &alu->def, atype);

   SpvId result = 0;
   switch (alu->op) {
   case nir_op_mov:
      assert(nir_op_infos[alu->op].num_inputs == 1);
      result = src[0];
      break;

#define UNOP(nir_op, spirv_op) \
   case nir_op: \
      assert(nir_op_infos[alu->op].num_inputs == 1); \
      result = emit_unop(ctx, spirv_op, dest_type, src[0]); \
      break;

   UNOP(nir_op_ineg, SpvOpSNegate)
   UNOP(nir_op_fneg, SpvOpFNegate)
   UNOP(nir_op_f2i8, SpvOpConvertFToS)
   UNOP(nir_op_f2u8, SpvOpConvertFToU)
   UNOP(nir_op_f2i16, SpvOpConvertFToS)
   UNOP(nir_op_f2u16, SpvOpConvertFToU)
   UNOP(nir_op_f2i32, SpvOpConvertFToS)
   UNOP(nir_op_f2u32, SpvOpConvertFToU)
   UNOP(nir_op_i2f16, SpvOpConvertSToF)
   UNOP(nir_op_i2f32, SpvOpConvertSToF)
   UNOP(nir_op_u2f16, SpvOpConvertUToF)
   UNOP(nir_op_u2f32, SpvOpConvertUToF)
   UNOP(nir_op_i2i8, SpvOpSConvert)
   UNOP(nir_op_i2i16, SpvOpSConvert)
   UNOP(nir_op_i2i32, SpvOpSConvert)
   UNOP(nir_op_u2u8, SpvOpUConvert)
   UNOP(nir_op_u2u16, SpvOpUConvert)
   UNOP(nir_op_u2u32, SpvOpUConvert)
   UNOP(nir_op_f2f16, SpvOpFConvert)
   UNOP(nir_op_f2f32, SpvOpFConvert)
   UNOP(nir_op_f2i64, SpvOpConvertFToS)
   UNOP(nir_op_f2u64, SpvOpConvertFToU)
   UNOP(nir_op_u2f64, SpvOpConvertUToF)
   UNOP(nir_op_i2f64, SpvOpConvertSToF)
   UNOP(nir_op_i2i64, SpvOpSConvert)
   UNOP(nir_op_u2u64, SpvOpUConvert)
   UNOP(nir_op_f2f64, SpvOpFConvert)
   UNOP(nir_op_bitfield_reverse, SpvOpBitReverse)
   UNOP(nir_op_bit_count, SpvOpBitCount)
#undef UNOP

   case nir_op_f2f16_rtz:
      assert(nir_op_infos[alu->op].num_inputs == 1);
      result = emit_unop(ctx, SpvOpFConvert, dest_type, src[0]);
      spirv_builder_emit_rounding_mode(&ctx->builder, result, SpvFPRoundingModeRTZ);
      break;

   case nir_op_inot:
      if (bit_size == 1)
         result = emit_unop(ctx, SpvOpLogicalNot, dest_type, src[0]);
      else
         result = emit_unop(ctx, SpvOpNot, dest_type, src[0]);
      break;

   case nir_op_b2i8:
   case nir_op_b2i16:
   case nir_op_b2i32:
   case nir_op_b2i64:
      assert(nir_op_infos[alu->op].num_inputs == 1);
      result = emit_select(ctx, dest_type, src[0],
                           get_ivec_constant(ctx, bit_size, num_components, 1),
                           get_ivec_constant(ctx, bit_size, num_components, 0));
      break;

   case nir_op_b2f16:
   case nir_op_b2f32:
   case nir_op_b2f64:
      assert(nir_op_infos[alu->op].num_inputs == 1);
      result = emit_select(ctx, dest_type, src[0],
                           get_fvec_constant(ctx, bit_size, num_components, 1),
                           get_fvec_constant(ctx, bit_size, num_components, 0));
      break;

   case nir_op_uclz:
      assert(nir_op_infos[alu->op].num_inputs == 1);
      result = emit_unop(ctx, SpvOpUCountLeadingZerosINTEL, dest_type, src[0]);
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityIntegerFunctions2INTEL);
      spirv_builder_emit_extension(&ctx->builder, "SPV_INTEL_shader_integer_functions2");
      break;
#define BUILTIN_UNOP(nir_op, spirv_op) \
   case nir_op: \
      assert(nir_op_infos[alu->op].num_inputs == 1); \
      result = emit_builtin_unop(ctx, spirv_op, dest_type, src[0]); \
      break;

#define BUILTIN_UNOPF(nir_op, spirv_op) \
   case nir_op: \
      assert(nir_op_infos[alu->op].num_inputs == 1); \
      result = emit_builtin_unop(ctx, spirv_op, get_def_type(ctx, &alu->def, nir_type_float), src[0]); \
      atype = nir_type_float; \
      break;

   BUILTIN_UNOP(nir_op_iabs, GLSLstd450SAbs)
   BUILTIN_UNOP(nir_op_fabs, GLSLstd450FAbs)
   BUILTIN_UNOP(nir_op_fsqrt, GLSLstd450Sqrt)
   BUILTIN_UNOP(nir_op_frsq, GLSLstd450InverseSqrt)
   BUILTIN_UNOP(nir_op_flog2, GLSLstd450Log2)
   BUILTIN_UNOP(nir_op_fexp2, GLSLstd450Exp2)
   BUILTIN_UNOP(nir_op_ffract, GLSLstd450Fract)
   BUILTIN_UNOP(nir_op_ffloor, GLSLstd450Floor)
   BUILTIN_UNOP(nir_op_fceil, GLSLstd450Ceil)
   BUILTIN_UNOP(nir_op_ftrunc, GLSLstd450Trunc)
   BUILTIN_UNOP(nir_op_fround_even, GLSLstd450RoundEven)
   BUILTIN_UNOP(nir_op_fsign, GLSLstd450FSign)
   BUILTIN_UNOP(nir_op_isign, GLSLstd450SSign)
   BUILTIN_UNOP(nir_op_fsin, GLSLstd450Sin)
   BUILTIN_UNOP(nir_op_fcos, GLSLstd450Cos)
   BUILTIN_UNOP(nir_op_ufind_msb, GLSLstd450FindUMsb)
   BUILTIN_UNOP(nir_op_find_lsb, GLSLstd450FindILsb)
   BUILTIN_UNOP(nir_op_ifind_msb, GLSLstd450FindSMsb)

   case nir_op_pack_half_2x16:
      assert(nir_op_infos[alu->op].num_inputs == 1);
      result = emit_builtin_unop(ctx, GLSLstd450PackHalf2x16, get_def_type(ctx, &alu->def, nir_type_uint), src[0]);
      break;

   BUILTIN_UNOPF(nir_op_unpack_half_2x16, GLSLstd450UnpackHalf2x16)
#undef BUILTIN_UNOP
#undef BUILTIN_UNOPF

   case nir_op_frcp:
      assert(nir_op_infos[alu->op].num_inputs == 1);
      result = emit_binop(ctx, SpvOpFDiv, dest_type,
                          get_fvec_constant(ctx, bit_size, num_components, 1),
                          src[0]);
      break;


#define BINOP(nir_op, spirv_op) \
   case nir_op: \
      assert(nir_op_infos[alu->op].num_inputs == 2); \
      result = emit_binop(ctx, spirv_op, dest_type, src[0], src[1]); \
      break;

   BINOP(nir_op_iadd, SpvOpIAdd)
   BINOP(nir_op_isub, SpvOpISub)
   BINOP(nir_op_imul, SpvOpIMul)
   BINOP(nir_op_idiv, SpvOpSDiv)
   BINOP(nir_op_udiv, SpvOpUDiv)
   BINOP(nir_op_umod, SpvOpUMod)
   BINOP(nir_op_imod, SpvOpSMod)
   BINOP(nir_op_irem, SpvOpSRem)
   BINOP(nir_op_fadd, SpvOpFAdd)
   BINOP(nir_op_fsub, SpvOpFSub)
   BINOP(nir_op_fmul, SpvOpFMul)
   BINOP(nir_op_fdiv, SpvOpFDiv)
   BINOP(nir_op_fmod, SpvOpFMod)
   BINOP(nir_op_ilt, SpvOpSLessThan)
   BINOP(nir_op_ige, SpvOpSGreaterThanEqual)
   BINOP(nir_op_ult, SpvOpULessThan)
   BINOP(nir_op_uge, SpvOpUGreaterThanEqual)
   BINOP(nir_op_flt, SpvOpFOrdLessThan)
   BINOP(nir_op_fge, SpvOpFOrdGreaterThanEqual)
   BINOP(nir_op_frem, SpvOpFRem)
#undef BINOP

#define BINOP_LOG(nir_op, spv_op, spv_log_op) \
   case nir_op: \
      assert(nir_op_infos[alu->op].num_inputs == 2); \
      if (nir_src_bit_size(alu->src[0].src) == 1) \
         result = emit_binop(ctx, spv_log_op, dest_type, src[0], src[1]); \
      else \
         result = emit_binop(ctx, spv_op, dest_type, src[0], src[1]); \
      break;

   BINOP_LOG(nir_op_iand, SpvOpBitwiseAnd, SpvOpLogicalAnd)
   BINOP_LOG(nir_op_ior, SpvOpBitwiseOr, SpvOpLogicalOr)
   BINOP_LOG(nir_op_ieq, SpvOpIEqual, SpvOpLogicalEqual)
   BINOP_LOG(nir_op_ine, SpvOpINotEqual, SpvOpLogicalNotEqual)
   BINOP_LOG(nir_op_ixor, SpvOpBitwiseXor, SpvOpLogicalNotEqual)
#undef BINOP_LOG

#define BINOP_SHIFT(nir_op, spirv_op) \
   case nir_op: { \
      assert(nir_op_infos[alu->op].num_inputs == 2); \
      int shift_bit_size = nir_src_bit_size(alu->src[1].src); \
      nir_alu_type shift_nir_type = nir_alu_type_get_base_type(nir_op_infos[alu->op].input_types[1]); \
      SpvId shift_type = get_alu_type(ctx, shift_nir_type, num_components, shift_bit_size); \
      SpvId shift_mask = get_ivec_constant(ctx, shift_bit_size, num_components, bit_size - 1); \
      SpvId shift_count = emit_binop(ctx, SpvOpBitwiseAnd, shift_type, src[1], shift_mask); \
      result = emit_binop(ctx, spirv_op, dest_type, src[0], shift_count); \
      break; \
   }

   BINOP_SHIFT(nir_op_ishl, SpvOpShiftLeftLogical)
   BINOP_SHIFT(nir_op_ishr, SpvOpShiftRightArithmetic)
   BINOP_SHIFT(nir_op_ushr, SpvOpShiftRightLogical)
#undef BINOP_SHIFT

#define BUILTIN_BINOP(nir_op, spirv_op) \
   case nir_op: \
      assert(nir_op_infos[alu->op].num_inputs == 2); \
      result = emit_builtin_binop(ctx, spirv_op, dest_type, src[0], src[1]); \
      break;

   BUILTIN_BINOP(nir_op_fmin, GLSLstd450FMin)
   BUILTIN_BINOP(nir_op_fmax, GLSLstd450FMax)
   BUILTIN_BINOP(nir_op_imin, GLSLstd450SMin)
   BUILTIN_BINOP(nir_op_imax, GLSLstd450SMax)
   BUILTIN_BINOP(nir_op_umin, GLSLstd450UMin)
   BUILTIN_BINOP(nir_op_umax, GLSLstd450UMax)
   BUILTIN_BINOP(nir_op_ldexp, GLSLstd450Ldexp)
   BUILTIN_BINOP(nir_op_fpow, GLSLstd450Pow)
#undef BUILTIN_BINOP

#define INTEL_BINOP(nir_op, spirv_op) \
   case nir_op: \
      assert(nir_op_infos[alu->op].num_inputs == 2); \
      result = emit_binop(ctx, spirv_op, dest_type, src[0], src[1]); \
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityIntegerFunctions2INTEL); \
      spirv_builder_emit_extension(&ctx->builder, "SPV_INTEL_shader_integer_functions2"); \
      break;

   INTEL_BINOP(nir_op_uabs_isub, SpvOpAbsISubINTEL)
   INTEL_BINOP(nir_op_uabs_usub, SpvOpAbsUSubINTEL)
   INTEL_BINOP(nir_op_iadd_sat, SpvOpIAddSatINTEL)
   INTEL_BINOP(nir_op_uadd_sat, SpvOpUAddSatINTEL)
   INTEL_BINOP(nir_op_ihadd, SpvOpIAverageINTEL)
   INTEL_BINOP(nir_op_uhadd, SpvOpUAverageINTEL)
   INTEL_BINOP(nir_op_irhadd, SpvOpIAverageRoundedINTEL)
   INTEL_BINOP(nir_op_urhadd, SpvOpUAverageRoundedINTEL)
   INTEL_BINOP(nir_op_isub_sat, SpvOpISubSatINTEL)
   INTEL_BINOP(nir_op_usub_sat, SpvOpUSubSatINTEL)
   INTEL_BINOP(nir_op_imul_32x16, SpvOpIMul32x16INTEL)
   INTEL_BINOP(nir_op_umul_32x16, SpvOpUMul32x16INTEL)
#undef INTEL_BINOP

   case nir_op_fdot2:
   case nir_op_fdot3:
   case nir_op_fdot4:
      assert(nir_op_infos[alu->op].num_inputs == 2);
      result = emit_binop(ctx, SpvOpDot, dest_type, src[0], src[1]);
      break;

   case nir_op_fdph:
   case nir_op_seq:
   case nir_op_sne:
   case nir_op_slt:
   case nir_op_sge:
      UNREACHABLE("should already be lowered away");

   case nir_op_fneu:
      assert(nir_op_infos[alu->op].num_inputs == 2);
      if (raw_src[0] == raw_src[1])
         result =  emit_unop(ctx, SpvOpIsNan, dest_type, src[0]);
      else
         result = emit_binop(ctx, SpvOpFUnordNotEqual, dest_type, src[0], src[1]);
      break;

   case nir_op_feq:
      assert(nir_op_infos[alu->op].num_inputs == 2);
      if (raw_src[0] == raw_src[1])
         result =  emit_unop(ctx, SpvOpLogicalNot, dest_type,
                             emit_unop(ctx, SpvOpIsNan, dest_type, src[0]));
      else
         result = emit_binop(ctx, SpvOpFOrdEqual, dest_type, src[0], src[1]);
      break;

   case nir_op_flrp:
      assert(nir_op_infos[alu->op].num_inputs == 3);
      result = emit_builtin_triop(ctx, GLSLstd450FMix, dest_type,
                                  src[0], src[1], src[2]);
      break;

   case nir_op_bcsel:
      assert(nir_op_infos[alu->op].num_inputs == 3);
      result = emit_select(ctx, dest_type, src[0], src[1], src[2]);
      break;

   case nir_op_pack_half_2x16_split: {
      SpvId fvec = spirv_builder_emit_composite_construct(&ctx->builder, get_fvec_type(ctx, 32, 2),
                                                          src, 2);
      result = emit_builtin_unop(ctx, GLSLstd450PackHalf2x16, dest_type, fvec);
      break;
   }
   case nir_op_vec2:
   case nir_op_vec3:
   case nir_op_vec4: {
      int num_inputs = nir_op_infos[alu->op].num_inputs;
      assert(2 <= num_inputs && num_inputs <= 4);
      result = spirv_builder_emit_composite_construct(&ctx->builder, dest_type,
                                                      src, num_inputs);
   }
   break;

   case nir_op_ubitfield_extract:
      assert(nir_op_infos[alu->op].num_inputs == 3);
      result = emit_triop(ctx, SpvOpBitFieldUExtract, dest_type, src[0], src[1], src[2]);
      break;

   case nir_op_ibitfield_extract:
      assert(nir_op_infos[alu->op].num_inputs == 3);
      result = emit_triop(ctx, SpvOpBitFieldSExtract, dest_type, src[0], src[1], src[2]);
      break;

   case nir_op_bitfield_insert:
      assert(nir_op_infos[alu->op].num_inputs == 4);
      result = spirv_builder_emit_quadop(&ctx->builder, SpvOpBitFieldInsert, dest_type, src[0], src[1], src[2], src[3]);
      break;

   /* those are all simple bitcasts, we could do better, but it doesn't matter */
   case nir_op_pack_32_4x8:
   case nir_op_pack_32_2x16:
   case nir_op_pack_64_2x32:
   case nir_op_pack_64_4x16:
   case nir_op_unpack_32_4x8:
   case nir_op_unpack_32_2x16:
   case nir_op_unpack_64_2x32:
   case nir_op_unpack_64_4x16: {
      result = emit_bitcast(ctx, dest_type, src[0]);
      break;
   }

   case nir_op_pack_32_2x16_split:
   case nir_op_pack_64_2x32_split: {
      nir_alu_type type = nir_alu_type_get_base_type(nir_op_infos[alu->op].input_types[0]);
      if (num_components <= 2) {
         SpvId components[] = {src[0], src[1]};
         SpvId vec_type = get_alu_type(ctx, type, num_components * 2, nir_src_bit_size(alu->src[0].src));
         result = spirv_builder_emit_composite_construct(&ctx->builder, vec_type, components, 2);
         result = emit_bitcast(ctx, dest_type, result);
      } else {
         SpvId components[NIR_MAX_VEC_COMPONENTS];
         SpvId conv_type = get_alu_type(ctx, type, 1, nir_src_bit_size(alu->src[0].src));
         SpvId vec_type = get_alu_type(ctx, type, 2, nir_src_bit_size(alu->src[0].src));
         SpvId dest_scalar_type = get_alu_type(ctx, nir_op_infos[alu->op].output_type, 1, bit_size);
         for (unsigned i = 0; i < nir_src_num_components(alu->src[0].src); i++) {
            SpvId conv[2];
            conv[0] = spirv_builder_emit_composite_extract(&ctx->builder, conv_type, src[0], &i, 1);
            conv[1] = spirv_builder_emit_composite_extract(&ctx->builder, conv_type, src[1], &i, 1);
            SpvId vec = spirv_builder_emit_composite_construct(&ctx->builder, vec_type, conv, 2);
            components[i] = emit_bitcast(ctx, dest_scalar_type, vec);
         }
         result = spirv_builder_emit_composite_construct(&ctx->builder, dest_type, components, num_components);
      }
      break;
   }

   case nir_op_unpack_32_2x16_split_x:
   case nir_op_unpack_64_2x32_split_x: {
      nir_alu_type type = nir_alu_type_get_base_type(nir_op_infos[alu->op].input_types[0]);
      SpvId vec_type = get_alu_type(ctx, type, 2, bit_size);
      unsigned idx = 0;
      if (num_components == 1) {
         SpvId vec = emit_bitcast(ctx, vec_type, src[0]);
         result = spirv_builder_emit_composite_extract(&ctx->builder, dest_type, vec, &idx, 1);
      } else {
         SpvId components[NIR_MAX_VEC_COMPONENTS];
         for (unsigned i = 0; i < nir_src_num_components(alu->src[0].src); i++) {
            SpvId conv = spirv_builder_emit_composite_extract(&ctx->builder, get_alu_type(ctx, type, 1, nir_src_bit_size(alu->src[0].src)), src[0], &i, 1);
            conv = emit_bitcast(ctx, vec_type, conv);
            SpvId conv_type = get_alu_type(ctx, type, 1, bit_size);
            components[i] = spirv_builder_emit_composite_extract(&ctx->builder, conv_type, conv, &idx, 1);
         }
         result = spirv_builder_emit_composite_construct(&ctx->builder, dest_type, components, num_components);
      }
      break;
   }

   case nir_op_unpack_32_2x16_split_y:
   case nir_op_unpack_64_2x32_split_y: {
      nir_alu_type type = nir_alu_type_get_base_type(nir_op_infos[alu->op].input_types[0]);
      SpvId vec_type = get_alu_type(ctx, type, 2, bit_size);
      unsigned idx = 1;
      if (num_components == 1) {
         SpvId vec = emit_bitcast(ctx, vec_type, src[0]);
         result = spirv_builder_emit_composite_extract(&ctx->builder, dest_type, vec, &idx, 1);
      } else {
         SpvId components[NIR_MAX_VEC_COMPONENTS];
         for (unsigned i = 0; i < nir_src_num_components(alu->src[0].src); i++) {
            SpvId conv = spirv_builder_emit_composite_extract(&ctx->builder, get_alu_type(ctx, type, 1, nir_src_bit_size(alu->src[0].src)), src[0], &i, 1);
            conv = emit_bitcast(ctx, vec_type, conv);
            SpvId conv_type = get_alu_type(ctx, type, 1, bit_size);
            components[i] = spirv_builder_emit_composite_extract(&ctx->builder, conv_type, conv, &idx, 1);
         }
         result = spirv_builder_emit_composite_construct(&ctx->builder, dest_type, components, num_components);
      }
      break;
   }

   default:
      fprintf(stderr, "emit_alu: not implemented (%s)\n",
              nir_op_infos[alu->op].name);

      UNREACHABLE("unsupported opcode");
      return;
   }
   if (alu->exact)
      spirv_builder_emit_decoration(&ctx->builder, result, SpvDecorationNoContraction);

   store_alu_result(ctx, alu, result, atype);
}

static void
emit_load_const(struct ntv_context *ctx, nir_load_const_instr *load_const)
{
   unsigned bit_size = load_const->def.bit_size;
   unsigned num_components = load_const->def.num_components;

   SpvId components[NIR_MAX_VEC_COMPONENTS];
   nir_alu_type atype;
   if (bit_size == 1) {
      atype = nir_type_bool;
      for (int i = 0; i < num_components; i++)
         components[i] = spirv_builder_const_bool(&ctx->builder,
                                                  load_const->value[i].b);
   } else {
      if (ctx->sinfo->broken_arbitary_type_const)
         atype = nir_type_uint;
      else
         atype = infer_nir_alu_type_from_uses_ssa(&load_const->def);
      for (int i = 0; i < num_components; i++) {
         switch (atype) {
         case nir_type_uint: {
            uint64_t tmp = nir_const_value_as_uint(load_const->value[i], bit_size);
            components[i] = emit_uint_const(ctx, bit_size, tmp);
            break;
         }
         case nir_type_int: {
            int64_t tmp = nir_const_value_as_int(load_const->value[i], bit_size);
            components[i] = emit_int_const(ctx, bit_size, tmp);
            break;
         }
         case nir_type_float: {
            double tmp = nir_const_value_as_float(load_const->value[i], bit_size);
            components[i] = emit_float_const(ctx, bit_size, tmp);
            break;
         }
         default:
            UNREACHABLE("this shouldn't happen!");
         }
      }
   }

   if (num_components > 1) {
      SpvId type = get_alu_type(ctx, atype, num_components, bit_size);
      SpvId value = spirv_builder_const_composite(&ctx->builder,
                                                  type, components,
                                                  num_components);
      store_def(ctx, load_const->def.index, value, atype);
   } else {
      assert(num_components == 1);
      store_def(ctx, load_const->def.index, components[0], atype);
   }
}

static void
emit_discard(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   assert(ctx->discard_func);
   SpvId type_void = spirv_builder_type_void(&ctx->builder);
   spirv_builder_function_call(&ctx->builder, type_void,
                               ctx->discard_func, NULL, 0);
}

static void
emit_load_deref(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type atype;
   SpvId ptr = get_src(ctx, intr->src, &atype);

   nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
   SpvId type;
   if (glsl_type_is_image(deref->type)) {
      nir_variable *var = nir_deref_instr_get_variable(deref);
      const struct glsl_type *gtype = glsl_without_array(var->type);
      type = get_image_type(ctx, var,
                            glsl_type_is_sampler(gtype),
                            glsl_get_sampler_dim(gtype) == GLSL_SAMPLER_DIM_BUF);
      atype = nir_get_nir_type_for_glsl_base_type(glsl_get_sampler_result_type(gtype));
   } else {
      type = get_glsl_type(ctx, deref->type, deref->modes & (nir_var_shader_temp | nir_var_function_temp));
      atype = get_nir_alu_type(deref->type);
   }
   SpvId result;

   if (nir_intrinsic_access(intr) & ACCESS_COHERENT)
      result = emit_atomic(ctx, SpvOpAtomicLoad, type, ptr, 0, 0);
   else
      result = spirv_builder_emit_load(&ctx->builder, type, ptr);
   store_def(ctx, intr->def.index, result, atype);
}

static void
emit_store_deref(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type ptype, stype;
   SpvId ptr = get_src(ctx, &intr->src[0], &ptype);
   SpvId src = get_src(ctx, &intr->src[1], &stype);

   const struct glsl_type *gtype = nir_src_as_deref(intr->src[0])->type;
   nir_variable *var = nir_intrinsic_get_var(intr, 0);
   SpvId type = get_glsl_type(ctx, gtype, var->data.mode & (nir_var_shader_temp | nir_var_function_temp));
   unsigned wrmask = nir_intrinsic_write_mask(intr);
   if (!glsl_type_is_scalar(gtype) &&
       wrmask != BITFIELD_MASK(glsl_type_is_array(gtype) ? glsl_get_aoa_size(gtype) : glsl_get_vector_elements(gtype))) {
      /* no idea what we do if this fails */
      assert(glsl_type_is_array(gtype) || glsl_type_is_vector(gtype));

      /* this is a partial write, so we have to loop and do a per-component write */
      SpvId result_type;
      SpvId member_type;
      if (glsl_type_is_vector(gtype)) {
         result_type = get_glsl_basetype(ctx, glsl_get_base_type(gtype));
         member_type = get_alu_type(ctx, stype, 1, glsl_get_bit_size(gtype));
      } else
         member_type = result_type = get_glsl_type(ctx, glsl_get_array_element(gtype), var->data.mode & (nir_var_shader_temp | nir_var_function_temp));
      SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                                  get_storage_class(var),
                                                  result_type);
      for (unsigned i = 0; i < 4; i++)
         if (wrmask & BITFIELD_BIT(i)) {
            SpvId idx = emit_uint_const(ctx, 32, i);
            SpvId val = spirv_builder_emit_composite_extract(&ctx->builder, member_type, src, &i, 1);
            if (stype != ptype)
               val = emit_bitcast(ctx, result_type, val);
            SpvId member = spirv_builder_emit_access_chain(&ctx->builder, ptr_type,
                                                           ptr, &idx, 1);
            spirv_builder_emit_store(&ctx->builder, member, val);
         }
      return;

   }
   SpvId result;
   if (ctx->stage == MESA_SHADER_FRAGMENT &&
       var->data.mode == nir_var_shader_out &&
       var->data.location == FRAG_RESULT_SAMPLE_MASK) {
      src = emit_bitcast(ctx, type, src);
      /* SampleMask is always an array in spirv, so we need to construct it into one */
      result = spirv_builder_emit_composite_construct(&ctx->builder, ctx->sample_mask_type, &src, 1);
   } else {
      if (ptype == stype)
         result = src;
      else
         result = emit_bitcast(ctx, type, src);
   }
   if (nir_intrinsic_access(intr) & ACCESS_COHERENT)
      spirv_builder_emit_atomic_store(&ctx->builder, ptr, SpvScopeDevice, 0, result);
   else
      spirv_builder_emit_store(&ctx->builder, ptr, result);
}

static void
emit_load_shared(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvId dest_type = get_def_type(ctx, &intr->def, nir_type_uint);
   unsigned num_components = intr->def.num_components;
   unsigned bit_size = intr->def.bit_size;
   SpvId uint_type = get_uvec_type(ctx, bit_size, 1);
   SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                               SpvStorageClassWorkgroup,
                                               uint_type);
   nir_alu_type atype;
   SpvId offset = get_src(ctx, &intr->src[0], &atype);
   if (atype == nir_type_float)
      offset = bitcast_to_uvec(ctx, offset, nir_src_bit_size(intr->src[0]), 1);
   SpvId constituents[NIR_MAX_VEC_COMPONENTS];
   SpvId shared_block = get_shared_block(ctx, bit_size);
   /* need to convert array -> vec */
   for (unsigned i = 0; i < num_components; i++) {
      SpvId member = spirv_builder_emit_access_chain(&ctx->builder, ptr_type,
                                                     shared_block, &offset, 1);
      constituents[i] = spirv_builder_emit_load(&ctx->builder, uint_type, member);
      offset = emit_binop(ctx, SpvOpIAdd, spirv_builder_type_uint(&ctx->builder, 32), offset, emit_uint_const(ctx, 32, 1));
   }
   SpvId result;
   if (num_components > 1)
      result = spirv_builder_emit_composite_construct(&ctx->builder, dest_type, constituents, num_components);
   else
      result = constituents[0];
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_store_shared(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type atype;
   SpvId src = get_src(ctx, &intr->src[0], &atype);

   unsigned wrmask = nir_intrinsic_write_mask(intr);
   unsigned bit_size = nir_src_bit_size(intr->src[0]);
   SpvId uint_type = get_uvec_type(ctx, bit_size, 1);
   SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                               SpvStorageClassWorkgroup,
                                               uint_type);
   nir_alu_type otype;
   SpvId offset = get_src(ctx, &intr->src[1], &otype);
   if (otype == nir_type_float)
      offset = bitcast_to_uvec(ctx, offset, nir_src_bit_size(intr->src[0]), 1);
   SpvId shared_block = get_shared_block(ctx, bit_size);
   /* this is a partial write, so we have to loop and do a per-component write */
   u_foreach_bit(i, wrmask) {
      SpvId shared_offset = emit_binop(ctx, SpvOpIAdd, spirv_builder_type_uint(&ctx->builder, 32), offset, emit_uint_const(ctx, 32, i));
      SpvId val = src;
      if (nir_src_num_components(intr->src[0]) != 1)
         val = spirv_builder_emit_composite_extract(&ctx->builder, uint_type, src, &i, 1);
      if (atype != nir_type_uint)
         val = emit_bitcast(ctx, get_alu_type(ctx, nir_type_uint, 1, bit_size), val);
      SpvId member = spirv_builder_emit_access_chain(&ctx->builder, ptr_type,
                                                     shared_block, &shared_offset, 1);
      spirv_builder_emit_store(&ctx->builder, member, val);
   }
}

static void
emit_load_scratch(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvId dest_type = get_def_type(ctx, &intr->def, nir_type_uint);
   unsigned num_components = intr->def.num_components;
   unsigned bit_size = intr->def.bit_size;
   SpvId uint_type = get_uvec_type(ctx, bit_size, 1);
   SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                               SpvStorageClassPrivate,
                                               uint_type);
   nir_alu_type atype;
   SpvId offset = get_src(ctx, &intr->src[0], &atype);
   if (atype != nir_type_uint)
      offset = bitcast_to_uvec(ctx, offset, nir_src_bit_size(intr->src[0]), 1);
   SpvId constituents[NIR_MAX_VEC_COMPONENTS];
   SpvId scratch_block = get_scratch_block(ctx, bit_size);
   /* need to convert array -> vec */
   for (unsigned i = 0; i < num_components; i++) {
      SpvId member = spirv_builder_emit_access_chain(&ctx->builder, ptr_type,
                                                     scratch_block, &offset, 1);
      constituents[i] = spirv_builder_emit_load(&ctx->builder, uint_type, member);
      offset = emit_binop(ctx, SpvOpIAdd, spirv_builder_type_uint(&ctx->builder, 32), offset, emit_uint_const(ctx, 32, 1));
   }
   SpvId result;
   if (num_components > 1)
      result = spirv_builder_emit_composite_construct(&ctx->builder, dest_type, constituents, num_components);
   else
      result = constituents[0];
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_store_scratch(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type atype;
   SpvId src = get_src(ctx, &intr->src[0], &atype);

   unsigned wrmask = nir_intrinsic_write_mask(intr);
   unsigned bit_size = nir_src_bit_size(intr->src[0]);
   SpvId uint_type = get_uvec_type(ctx, bit_size, 1);
   SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                               SpvStorageClassPrivate,
                                               uint_type);
   nir_alu_type otype;
   SpvId offset = get_src(ctx, &intr->src[1], &otype);
   if (otype != nir_type_uint)
      offset = bitcast_to_uvec(ctx, offset, nir_src_bit_size(intr->src[1]), 1);
   SpvId scratch_block = get_scratch_block(ctx, bit_size);
   /* this is a partial write, so we have to loop and do a per-component write */
   u_foreach_bit(i, wrmask) {
      SpvId scratch_offset = emit_binop(ctx, SpvOpIAdd, spirv_builder_type_uint(&ctx->builder, 32), offset, emit_uint_const(ctx, 32, i));
      SpvId val = src;
      if (nir_src_num_components(intr->src[0]) != 1)
         val = spirv_builder_emit_composite_extract(&ctx->builder, uint_type, src, &i, 1);
      if (atype != nir_type_uint)
         val = emit_bitcast(ctx, get_alu_type(ctx, nir_type_uint, 1, bit_size), val);
      SpvId member = spirv_builder_emit_access_chain(&ctx->builder, ptr_type,
                                                     scratch_block, &scratch_offset, 1);
      spirv_builder_emit_store(&ctx->builder, member, val);
   }
}

static void
emit_load_push_const(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvId uint_type = get_uvec_type(ctx, 32, 1);
   SpvId load_type = get_uvec_type(ctx, 32, 1);

   /* number of components being loaded */
   unsigned num_components = intr->def.num_components;
   SpvId constituents[NIR_MAX_VEC_COMPONENTS * 2];
   SpvId result;

   /* destination type for the load */
   SpvId type = get_def_uvec_type(ctx, &intr->def);
   SpvId one = emit_uint_const(ctx, 32, 1);

   /* we grab a single array member at a time, so it's a pointer to a uint */
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassPushConstant,
                                                   load_type);

   nir_alu_type atype;
   SpvId member = get_src(ctx, &intr->src[0], &atype);
   if (atype == nir_type_float)
      member = bitcast_to_uvec(ctx, member, nir_src_bit_size(intr->src[0]), 1);
   /* reuse the offset from ZINK_PUSH_CONST_OFFSET */
   SpvId offset = emit_uint_const(ctx, 32, nir_intrinsic_component(intr));
   /* OpAccessChain takes an array of indices that drill into a hierarchy based on the type:
    * index 0 is accessing 'base'
    * index 1 is accessing 'base[index 1]'
    *
    */
   for (unsigned i = 0; i < num_components; i++) {
      SpvId indices[2] = { member, offset };
      SpvId ptr = spirv_builder_emit_access_chain(&ctx->builder, pointer_type,
                                                  ctx->push_const_var, indices,
                                                  ARRAY_SIZE(indices));
      /* load a single value into the constituents array */
      constituents[i] = spirv_builder_emit_load(&ctx->builder, load_type, ptr);
      /* increment to the next vec4 member index for the next load */
      offset = emit_binop(ctx, SpvOpIAdd, uint_type, offset, one);
   }

   /* if loading more than 1 value, reassemble the results into the desired type,
    * otherwise just use the loaded result
    */
   if (num_components > 1) {
      result = spirv_builder_emit_composite_construct(&ctx->builder,
                                                      type,
                                                      constituents,
                                                      num_components);
   } else
      result = constituents[0];

   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_load_global(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   bool coherent = ctx->sinfo->have_vulkan_memory_model && nir_intrinsic_access(intr) & ACCESS_COHERENT;
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityPhysicalStorageBufferAddresses);
   SpvId dest_type = get_def_type(ctx, &intr->def, nir_type_uint);
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassPhysicalStorageBuffer,
                                                   dest_type);
   nir_alu_type atype;
   SpvId ptr = emit_bitcast(ctx, pointer_type, get_src(ctx, &intr->src[0], &atype));
   SpvId result = spirv_builder_emit_load_aligned(&ctx->builder, dest_type, ptr, intr->def.bit_size / 8, coherent);
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_store_global(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   bool coherent = ctx->sinfo->have_vulkan_memory_model && nir_intrinsic_access(intr) & ACCESS_COHERENT;
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityPhysicalStorageBufferAddresses);
   unsigned bit_size = nir_src_bit_size(intr->src[0]);
   SpvId dest_type = get_uvec_type(ctx, bit_size, 1);
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassPhysicalStorageBuffer,
                                                   dest_type);
   nir_alu_type atype;
   SpvId param = get_src(ctx, &intr->src[0], &atype);
   if (atype != nir_type_uint)
      param = emit_bitcast(ctx, dest_type, param);
   SpvId ptr = emit_bitcast(ctx, pointer_type, get_src(ctx, &intr->src[1], &atype));
   spirv_builder_emit_store_aligned(&ctx->builder, ptr, param, bit_size / 8, coherent);
}

static void
emit_load_reg(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   assert(nir_intrinsic_base(intr) == 0 && "no array registers");

   nir_intrinsic_instr *decl = nir_reg_get_decl(intr->src[0].ssa);
   unsigned num_components = nir_intrinsic_num_components(decl);
   unsigned bit_size = nir_intrinsic_bit_size(decl);
   unsigned index = decl->def.index;
   assert(index < ctx->num_defs);

   init_reg(ctx, decl, nir_type_uint);
   assert(ctx->defs[index] != 0);

   nir_alu_type atype = ctx->def_types[index];
   SpvId var = ctx->defs[index];
   SpvId type = get_alu_type(ctx, atype, num_components, bit_size);
   SpvId result = spirv_builder_emit_load(&ctx->builder, type, var);
   store_def(ctx, intr->def.index, result, atype);
}

static void
emit_store_reg(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type atype;
   SpvId param = get_src(ctx, &intr->src[0], &atype);

   nir_intrinsic_instr *decl = nir_reg_get_decl(intr->src[1].ssa);
   unsigned index = decl->def.index;
   unsigned num_components = nir_intrinsic_num_components(decl);
   unsigned bit_size = nir_intrinsic_bit_size(decl);

   atype = nir_alu_type_get_base_type(atype);
   init_reg(ctx, decl, atype);
   SpvId var = ctx->defs[index];
   nir_alu_type vtype = ctx->def_types[index];
   if (atype != vtype) {
      assert(vtype != nir_type_bool);
      param = emit_bitcast(ctx, get_alu_type(ctx, vtype, num_components, bit_size), param);
   }
   assert(var);
   spirv_builder_emit_store(&ctx->builder, var, param);
}

static SpvId
create_builtin_var(struct ntv_context *ctx, SpvId var_type,
                   SpvStorageClass storage_class,
                   const char *name, SpvBuiltIn builtin)
{
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   storage_class,
                                                   var_type);
   SpvId var = spirv_builder_emit_var(&ctx->builder, pointer_type,
                                      storage_class);
   spirv_builder_emit_name(&ctx->builder, var, name);
   spirv_builder_emit_builtin(&ctx->builder, var, builtin);

   if (ctx->stage == MESA_SHADER_FRAGMENT) {
      switch (builtin) {
      case SpvBuiltInSampleId:
      case SpvBuiltInSubgroupLocalInvocationId:
         spirv_builder_emit_decoration(&ctx->builder, var, SpvDecorationFlat);
         break;
      default:
         break;
      }
   }

   assert(ctx->num_entry_ifaces < ARRAY_SIZE(ctx->entry_ifaces));
   ctx->entry_ifaces[ctx->num_entry_ifaces++] = var;
   return var;
}

static void
emit_load_front_face(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvId var_type = spirv_builder_type_bool(&ctx->builder);
   if (!ctx->front_face_var)
      ctx->front_face_var = create_builtin_var(ctx, var_type,
                                               SpvStorageClassInput,
                                               "gl_FrontFacing",
                                               SpvBuiltInFrontFacing);

   SpvId result = spirv_builder_emit_load(&ctx->builder, var_type,
                                          ctx->front_face_var);
   assert(1 == intr->def.num_components);
   store_def(ctx, intr->def.index, result, nir_type_bool);
}

static void
emit_load_view_index(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvId var_type = spirv_builder_type_uint(&ctx->builder, 32);
   spirv_builder_emit_extension(&ctx->builder, "SPV_KHR_multiview");
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityMultiView);
   if (!ctx->view_index_var)
      ctx->view_index_var = create_builtin_var(ctx, var_type,
                                               SpvStorageClassInput,
                                               "gl_ViewIndex",
                                               SpvBuiltInViewIndex);

   SpvId result = spirv_builder_emit_load(&ctx->builder, var_type,
                                          ctx->view_index_var);
   assert(1 == intr->def.num_components);
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_load_uint_input(struct ntv_context *ctx, nir_intrinsic_instr *intr, SpvId *var_id, const char *var_name, SpvBuiltIn builtin)
{
   SpvId var_type = spirv_builder_type_uint(&ctx->builder, 32);
   if (!*var_id) {
      if (builtin == SpvBuiltInSampleMask) {
         /* gl_SampleMaskIn is an array[1] in spirv... */
         var_type = spirv_builder_type_array(&ctx->builder, var_type, emit_uint_const(ctx, 32, 1));
         spirv_builder_emit_array_stride(&ctx->builder, var_type, sizeof(uint32_t));
      }
      *var_id = create_builtin_var(ctx, var_type,
                                   SpvStorageClassInput,
                                   var_name,
                                   builtin);
   }

   SpvId load_var = *var_id;
   if (builtin == SpvBuiltInSampleMask) {
      SpvId zero = emit_uint_const(ctx, 32, 0);
      var_type = spirv_builder_type_uint(&ctx->builder, 32);
      SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                      SpvStorageClassInput,
                                                      var_type);
      load_var = spirv_builder_emit_access_chain(&ctx->builder, pointer_type, load_var, &zero, 1);
   }

   SpvId result = spirv_builder_emit_load(&ctx->builder, var_type, load_var);
   assert(1 == intr->def.num_components);
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_load_vec_input(struct ntv_context *ctx, nir_intrinsic_instr *intr, SpvId *var_id, const char *var_name, SpvBuiltIn builtin, nir_alu_type type)
{
   SpvId var_type;

   switch (type) {
   case nir_type_bool:
      var_type = get_bvec_type(ctx, intr->def.num_components);
      break;
   case nir_type_int:
      var_type = get_ivec_type(ctx, intr->def.bit_size,
                               intr->def.num_components);
      break;
   case nir_type_uint:
      var_type = get_uvec_type(ctx, intr->def.bit_size,
                               intr->def.num_components);
      break;
   case nir_type_float:
      var_type = get_fvec_type(ctx, intr->def.bit_size,
                               intr->def.num_components);
      break;
   default:
      UNREACHABLE("unknown type passed");
   }
   if (!*var_id)
      *var_id = create_builtin_var(ctx, var_type,
                                   SpvStorageClassInput,
                                   var_name,
                                   builtin);

   SpvId result = spirv_builder_emit_load(&ctx->builder, var_type, *var_id);
   store_def(ctx, intr->def.index, result, type);
}

static void
emit_interpolate(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvId op;
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityInterpolationFunction);
   SpvId src1 = 0;
   nir_alu_type atype;
   switch (intr->intrinsic) {
   case nir_intrinsic_interp_deref_at_centroid:
      op = GLSLstd450InterpolateAtCentroid;
      break;
   case nir_intrinsic_interp_deref_at_sample:
      op = GLSLstd450InterpolateAtSample;
      src1 = get_src(ctx, &intr->src[1], &atype);
      if (atype != nir_type_int)
         src1 = emit_bitcast(ctx, get_ivec_type(ctx, 32, 1), src1);
      break;
   case nir_intrinsic_interp_deref_at_offset:
      op = GLSLstd450InterpolateAtOffset;
      src1 = get_src(ctx, &intr->src[1], &atype);
      /*
         The offset operand must be a vector of 2 components of 32-bit floating-point type.
         - InterpolateAtOffset spec
       */
      if (atype != nir_type_float)
         src1 = emit_bitcast(ctx, get_fvec_type(ctx, 32, 2), src1);
      break;
   default:
      UNREACHABLE("unknown interp op");
   }
   nir_alu_type ptype;
   SpvId ptr = get_src(ctx, &intr->src[0], &ptype);
   SpvId result;
   const struct glsl_type *gtype = nir_src_as_deref(intr->src[0])->type;
   assert(glsl_get_vector_elements(gtype) == intr->num_components);
   assert(ptype == get_nir_alu_type(gtype));
   if (intr->intrinsic == nir_intrinsic_interp_deref_at_centroid)
      result = emit_builtin_unop(ctx, op, get_glsl_type(ctx, gtype, false), ptr);
   else
      result = emit_builtin_binop(ctx, op, get_glsl_type(ctx, gtype, false), ptr, src1);
   store_def(ctx, intr->def.index, result, ptype);
}

static void
handle_atomic_op(struct ntv_context *ctx, nir_intrinsic_instr *intr, SpvId ptr, SpvId param, SpvId param2, nir_alu_type type)
{
   SpvId dest_type = get_def_type(ctx, &intr->def, type);
   SpvId result = emit_atomic(ctx,
                              get_atomic_op(ctx, intr->def.bit_size, nir_intrinsic_atomic_op(intr)),
                              dest_type, ptr, param, param2);
   assert(result);
   store_def(ctx, intr->def.index, result, type);
}

static void
emit_deref_atomic_intrinsic(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type atype;
   nir_alu_type ret_type = nir_atomic_op_type(nir_intrinsic_atomic_op(intr)) == nir_type_float ? nir_type_float : nir_type_uint;
   SpvId ptr = get_src(ctx, &intr->src[0], &atype);
   if (atype != ret_type && ret_type == nir_type_float) {
      unsigned bit_size = nir_src_bit_size(intr->src[0]);
      SpvId *float_array_type = &ctx->float_array_type[bit_size == 32 ? 0 : 1];
      if (!*float_array_type) {
         *float_array_type = spirv_builder_type_pointer(&ctx->builder, SpvStorageClassStorageBuffer,
                                                        spirv_builder_type_float(&ctx->builder, bit_size));
      }
      ptr = emit_unop(ctx, SpvOpBitcast, *float_array_type, ptr);
   }

   SpvId param = get_src(ctx, &intr->src[1], &atype);
   if (atype != ret_type)
      param = cast_src_to_type(ctx, param, intr->src[1], ret_type);

   SpvId param2 = 0;

   if (nir_src_bit_size(intr->src[1]) == 64)
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityInt64Atomics);

   if (intr->intrinsic == nir_intrinsic_deref_atomic_swap) {
      param2 = get_src(ctx, &intr->src[2], &atype);
      if (atype != ret_type)
         param2 = cast_src_to_type(ctx, param2, intr->src[2], ret_type);
   }

   handle_atomic_op(ctx, intr, ptr, param, param2, ret_type);
}

static void
emit_shared_atomic_intrinsic(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   unsigned bit_size = nir_src_bit_size(intr->src[1]);
   SpvId dest_type = get_def_type(ctx, &intr->def, nir_type_uint);
   nir_alu_type atype;
   nir_alu_type ret_type = nir_atomic_op_type(nir_intrinsic_atomic_op(intr)) == nir_type_float ? nir_type_float : nir_type_uint;
   SpvId param = get_src(ctx, &intr->src[1], &atype);
   if (atype != ret_type)
      param = cast_src_to_type(ctx, param, intr->src[1], ret_type);

   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassWorkgroup,
                                                   dest_type);
   SpvId offset = get_src(ctx, &intr->src[0], &atype);
   if (atype != nir_type_uint)
      offset = cast_src_to_type(ctx, offset, intr->src[0], nir_type_uint);
   offset = emit_binop(ctx, SpvOpUDiv, get_uvec_type(ctx, 32, 1), offset, emit_uint_const(ctx, 32, bit_size / 8));
   SpvId shared_block = get_shared_block(ctx, bit_size);
   SpvId ptr = spirv_builder_emit_access_chain(&ctx->builder, pointer_type,
                                               shared_block, &offset, 1);
   if (nir_src_bit_size(intr->src[1]) == 64)
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityInt64Atomics);
   SpvId param2 = 0;

   if (intr->intrinsic == nir_intrinsic_shared_atomic_swap) {
      param2 = get_src(ctx, &intr->src[2], &atype);
      if (atype != ret_type)
         param2 = cast_src_to_type(ctx, param2, intr->src[2], ret_type);
   }

   handle_atomic_op(ctx, intr, ptr, param, param2, ret_type);
}

static void
emit_global_atomic_intrinsic(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   unsigned bit_size = nir_src_bit_size(intr->src[1]);
   SpvId dest_type = get_def_type(ctx, &intr->def, nir_type_uint);
   nir_alu_type atype;
   nir_alu_type ret_type = nir_atomic_op_type(nir_intrinsic_atomic_op(intr)) == nir_type_float ? nir_type_float : nir_type_uint;
   SpvId param = get_src(ctx, &intr->src[1], &atype);

   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityPhysicalStorageBufferAddresses);
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassPhysicalStorageBuffer,
                                                   dest_type);
   SpvId ptr = emit_bitcast(ctx, pointer_type, get_src(ctx, &intr->src[0], &atype));

   if (bit_size == 64)
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityInt64Atomics);
   SpvId param2 = 0;

   if (intr->intrinsic == nir_intrinsic_global_atomic_swap)
      param2 = get_src(ctx, &intr->src[2], &atype);

   handle_atomic_op(ctx, intr, ptr, param, param2, ret_type);
}

static void
emit_get_ssbo_size(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvId uint_type = get_uvec_type(ctx, 32, 1);
   nir_variable *var = ctx->ssbo_vars;
   const struct glsl_type *bare_type = glsl_without_array(var->type);
   unsigned last_member_idx = glsl_get_length(bare_type) - 1;
   SpvId pointer_type = spirv_builder_type_pointer(&ctx->builder,
                                                   SpvStorageClassStorageBuffer,
                                                   get_bo_struct_type(ctx, var));
   nir_alu_type atype;
   SpvId bo = get_src(ctx, &intr->src[0], &atype);
   if (atype == nir_type_float)
      bo = bitcast_to_uvec(ctx, bo, nir_src_bit_size(intr->src[0]), 1);
   SpvId indices[] = { bo };
   SpvId ptr = spirv_builder_emit_access_chain(&ctx->builder, pointer_type,
                                               ctx->ssbos[2], indices,
                                               ARRAY_SIZE(indices));
   SpvId result = spirv_builder_emit_binop(&ctx->builder, SpvOpArrayLength, uint_type,
                                           ptr, last_member_idx);
   /* this is going to be converted by nir to:

      length = (buffer_size - offset) / stride

      * so we need to un-convert it to avoid having the calculation performed twice
      */
   const struct glsl_type *last_member = glsl_get_struct_field(bare_type, last_member_idx);
   /* multiply by stride */
   result = emit_binop(ctx, SpvOpIMul, uint_type, result, emit_uint_const(ctx, 32, glsl_get_explicit_stride(last_member)));
   /* get total ssbo size by adding offset */
   result = emit_binop(ctx, SpvOpIAdd, uint_type, result,
                        emit_uint_const(ctx, 32,
                                       glsl_get_struct_field_offset(bare_type, last_member_idx)));
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static SpvId
get_image_coords(struct ntv_context *ctx, const struct glsl_type *type, nir_src *src)
{
   uint32_t num_coords = glsl_get_sampler_coordinate_components(type);
   uint32_t src_components = nir_src_num_components(*src);

   nir_alu_type atype;
   SpvId spv = get_src(ctx, src, &atype);
   if (num_coords == src_components)
      return spv;

   /* need to extract the coord dimensions that the image can use */
   SpvId vec_type = get_alu_type(ctx, atype, num_coords, 32);
   if (num_coords == 1)
      return spirv_builder_emit_vector_extract(&ctx->builder, vec_type, spv, 0);
   uint32_t constituents[4];
   SpvId zero = atype == nir_type_uint ? emit_uint_const(ctx, nir_src_bit_size(*src), 0) : emit_float_const(ctx, nir_src_bit_size(*src), 0);
   assert(num_coords < ARRAY_SIZE(constituents));
   for (unsigned i = 0; i < num_coords; i++)
      constituents[i] = i < src_components ? i : zero;
   return spirv_builder_emit_vector_shuffle(&ctx->builder, vec_type, spv, spv, constituents, num_coords);
}

static void
emit_image_deref_store(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type atype;
   SpvId img_var = get_src(ctx, &intr->src[0], &atype);
   nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
   nir_variable *var = nir_deref_instr_get_variable(deref);
   SpvId img_type = find_image_type(ctx, var);
   const struct glsl_type *type = glsl_without_array(var->type);
   SpvId base_type = get_glsl_basetype(ctx, glsl_get_sampler_result_type(type));
   SpvId img = spirv_builder_emit_load(&ctx->builder, img_type, img_var);
   SpvId coord = get_image_coords(ctx, type, &intr->src[1]);
   SpvId texel = get_src(ctx, &intr->src[3], &atype);
   /* texel type must match image type */
   if (atype != nir_get_nir_type_for_glsl_base_type(glsl_get_sampler_result_type(type)))
      texel = emit_bitcast(ctx,
                           spirv_builder_type_vector(&ctx->builder, base_type, 4),
                           texel);
   bool use_sample = glsl_get_sampler_dim(type) == GLSL_SAMPLER_DIM_MS ||
                     glsl_get_sampler_dim(type) == GLSL_SAMPLER_DIM_SUBPASS_MS;
   SpvId sample = use_sample ? get_src(ctx, &intr->src[2], &atype) : 0;
   assert(nir_src_bit_size(intr->src[3]) == glsl_base_type_bit_size(glsl_get_sampler_result_type(type)));
   spirv_builder_emit_image_write(&ctx->builder, img, coord, texel, 0, sample, 0);
}

static SpvId
extract_sparse_load(struct ntv_context *ctx, SpvId result, SpvId dest_type, nir_def *def)
{
   /* Result Type must be an OpTypeStruct with two members.
    * The first member’s type must be an integer type scalar.
    * It holds a Residency Code that can be passed to OpImageSparseTexelsResident
    * - OpImageSparseRead spec
    */
   uint32_t idx = 0;
   SpvId resident = spirv_builder_emit_composite_extract(&ctx->builder, spirv_builder_type_uint(&ctx->builder, 32), result, &idx, 1);
   idx = 1;
   /* normal vec4 return */
   if (def->num_components == 4)
      result = spirv_builder_emit_composite_extract(&ctx->builder, dest_type, result, &idx, 1);
   else {
      /* shadow */
      assert(def->num_components == 1);
      SpvId type = spirv_builder_type_float(&ctx->builder, def->bit_size);
      SpvId val[2];
      /* pad to 2 components: the upcoming is_sparse_texels_resident instr will always use the
       * separate residency value, but the shader still expects this return to be a vec2,
       * so give it a vec2
       */
      val[0] = spirv_builder_emit_composite_extract(&ctx->builder, type, result, &idx, 1);
      val[1] = emit_float_const(ctx, def->bit_size, 0);
      result = spirv_builder_emit_composite_construct(&ctx->builder, get_fvec_type(ctx, def->bit_size, 2), val, 2);
   }
   assert(resident != 0);
   assert(def->index < ctx->num_defs);
   ctx->resident_defs[def->index] = resident;
   return result;
}

static void
emit_image_deref_load(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   bool sparse = intr->intrinsic == nir_intrinsic_image_deref_sparse_load;
   nir_alu_type atype;
   SpvId img_var = get_src(ctx, &intr->src[0], &atype);
   nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
   nir_variable *var = nir_deref_instr_get_variable(deref);
   bool mediump = (var->data.precision == GLSL_PRECISION_MEDIUM || var->data.precision == GLSL_PRECISION_LOW);
   SpvId img_type = find_image_type(ctx, var);
   const struct glsl_type *type = glsl_without_array(var->type);
   SpvId base_type = get_glsl_basetype(ctx, glsl_get_sampler_result_type(type));
   SpvId img = spirv_builder_emit_load(&ctx->builder, img_type, img_var);
   SpvId coord = get_image_coords(ctx, type, &intr->src[1]);
   bool use_sample = glsl_get_sampler_dim(type) == GLSL_SAMPLER_DIM_MS ||
                     glsl_get_sampler_dim(type) == GLSL_SAMPLER_DIM_SUBPASS_MS;
   SpvId sample = use_sample ? get_src(ctx, &intr->src[2], &atype) : 0;
   SpvId dest_type = spirv_builder_type_vector(&ctx->builder, base_type,
                                               intr->def.num_components);
   SpvId result = spirv_builder_emit_image_read(&ctx->builder,
                                 dest_type,
                                 img, coord, 0, sample, 0, sparse);
   if (sparse)
      result = extract_sparse_load(ctx, result, dest_type, &intr->def);

   if (!sparse && mediump) {
      spirv_builder_emit_decoration(&ctx->builder, result,
                                    SpvDecorationRelaxedPrecision);
   }

   store_def(ctx, intr->def.index, result, nir_get_nir_type_for_glsl_base_type(glsl_get_sampler_result_type(type)));
}

static void
emit_image_deref_size(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type atype;
   SpvId img_var = get_src(ctx, &intr->src[0], &atype);
   nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
   nir_variable *var = nir_deref_instr_get_variable(deref);
   SpvId img_type = find_image_type(ctx, var);
   const struct glsl_type *type = glsl_without_array(var->type);
   SpvId img = spirv_builder_emit_load(&ctx->builder, img_type, img_var);
   unsigned num_components = glsl_get_sampler_coordinate_components(type);
   /* SPIRV requires 2 components for non-array cube size */
   if (glsl_get_sampler_dim(type) == GLSL_SAMPLER_DIM_CUBE && !glsl_sampler_type_is_array(type))
      num_components = 2;

   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityImageQuery);
   SpvId result = spirv_builder_emit_image_query_size(&ctx->builder, get_uvec_type(ctx, 32, num_components), img, 0);
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_image_deref_samples(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type atype;
   SpvId img_var = get_src(ctx, &intr->src[0], &atype);
   nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
   nir_variable *var = nir_deref_instr_get_variable(deref);
   SpvId img_type = find_image_type(ctx, var);
   SpvId img = spirv_builder_emit_load(&ctx->builder, img_type, img_var);

   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityImageQuery);
   SpvId result = spirv_builder_emit_unop(&ctx->builder, SpvOpImageQuerySamples, get_def_type(ctx, &intr->def, nir_type_uint), img);
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_image_intrinsic(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   nir_alu_type atype, ptype;
   SpvId param = get_src(ctx, &intr->src[3], &ptype);
   SpvId img_var = get_src(ctx, &intr->src[0], &atype);
   nir_deref_instr *deref = nir_src_as_deref(intr->src[0]);
   nir_variable *var = nir_deref_instr_get_variable(deref);
   const struct glsl_type *type = glsl_without_array(var->type);
   bool is_ms;
   type_to_dim(glsl_get_sampler_dim(type), &is_ms);
   SpvId sample = is_ms ? get_src(ctx, &intr->src[2], &atype) : emit_uint_const(ctx, 32, 0);
   SpvId coord = get_image_coords(ctx, type, &intr->src[1]);
   enum glsl_base_type glsl_result_type = glsl_get_sampler_result_type(type);
   SpvId base_type = get_glsl_basetype(ctx, glsl_result_type);
   SpvId texel = spirv_builder_emit_image_texel_pointer(&ctx->builder, base_type, img_var, coord, sample);
   SpvId param2 = 0;

   /* The type of Value must be the same as Result Type.
    * The type of the value pointed to by Pointer must be the same as Result Type.
    */
   nir_alu_type ntype = nir_get_nir_type_for_glsl_base_type(glsl_result_type);
   if (ptype != ntype) {
      SpvId cast_type = get_def_type(ctx, &intr->def, ntype);
      param = emit_bitcast(ctx, cast_type, param);
   }

   if (intr->intrinsic == nir_intrinsic_image_deref_atomic_swap) {
      param2 = get_src(ctx, &intr->src[4], &ptype);
      if (ptype != ntype) {
         SpvId cast_type = get_def_type(ctx, &intr->def, ntype);
         param2 = emit_bitcast(ctx, cast_type, param2);
      }
   }

   handle_atomic_op(ctx, intr, texel, param, param2, ntype);
}

static void
emit_ballot(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilitySubgroupBallotKHR);
   spirv_builder_emit_extension(&ctx->builder, "SPV_KHR_shader_ballot");
   SpvId type = get_def_uvec_type(ctx, &intr->def);
   nir_alu_type atype;
   SpvId result = emit_unop(ctx, SpvOpSubgroupBallotKHR, type, get_src(ctx, &intr->src[0], &atype));
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_read_first_invocation(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilitySubgroupBallotKHR);
   spirv_builder_emit_extension(&ctx->builder, "SPV_KHR_shader_ballot");
   nir_alu_type atype;
   SpvId src = get_src(ctx, &intr->src[0], &atype);
   SpvId type = get_def_type(ctx, &intr->def, atype);
   SpvId result = emit_unop(ctx, SpvOpSubgroupFirstInvocationKHR, type, src);
   store_def(ctx, intr->def.index, result, atype);
}

static void
emit_read_invocation(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilitySubgroupBallotKHR);
   spirv_builder_emit_extension(&ctx->builder, "SPV_KHR_shader_ballot");
   nir_alu_type atype, itype;
   SpvId src = get_src(ctx, &intr->src[0], &atype);
   SpvId type = get_def_type(ctx, &intr->def, atype);
   SpvId result = emit_binop(ctx, SpvOpSubgroupReadInvocationKHR, type,
                              src,
                              get_src(ctx, &intr->src[1], &itype));
   store_def(ctx, intr->def.index, result, atype);
}

static void
emit_shader_clock(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityShaderClockKHR);
   spirv_builder_emit_extension(&ctx->builder, "SPV_KHR_shader_clock");

   SpvScope scope = get_scope(nir_intrinsic_memory_scope(intr));
   SpvId type = get_def_type(ctx, &intr->def, nir_type_uint);
   SpvId result = spirv_builder_emit_unop_const(&ctx->builder, SpvOpReadClockKHR, type, scope);
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_is_sparse_texels_resident(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilitySparseResidency);

   SpvId type = get_def_type(ctx, &intr->def, nir_type_uint);

   unsigned index = intr->src[0].ssa->index;
   assert(index < ctx->num_defs);
   assert(ctx->resident_defs[index] != 0);
   SpvId resident = ctx->resident_defs[index];

   SpvId result = spirv_builder_emit_unop(&ctx->builder, SpvOpImageSparseTexelsResident, type, resident);
   store_def(ctx, intr->def.index, result, nir_type_uint);
}

static void
emit_vote(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvOp op;

   switch (intr->intrinsic) {
   case nir_intrinsic_vote_all:
      op = SpvOpGroupNonUniformAll;
      break;
   case nir_intrinsic_vote_any:
      op = SpvOpGroupNonUniformAny;
      break;
   case nir_intrinsic_vote_ieq:
   case nir_intrinsic_vote_feq:
      op = SpvOpGroupNonUniformAllEqual;
      break;
   default:
      UNREACHABLE("unknown vote intrinsic");
   }
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityGroupNonUniformVote);
   nir_alu_type atype;
   SpvId result = spirv_builder_emit_vote(&ctx->builder, op, get_src(ctx, &intr->src[0], &atype));
   store_def(ctx, intr->def.index, result, nir_type_bool);
}

static void
emit_is_helper_invocation(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvId result;
   if (!ctx->have_spirv16 && ctx->nir->options->discard_is_demote) {
      result = spirv_is_helper_invocation(&ctx->builder);
   } else {
      SpvId var_type = spirv_builder_type_bool(&ctx->builder);
      if (!ctx->helper_invocation_var)
         ctx->helper_invocation_var = create_builtin_var(ctx, var_type,
                                                         SpvStorageClassInput,
                                                         "gl_HelperInvocation",
                                                         SpvBuiltInHelperInvocation);
      result = spirv_builder_emit_load_volatile(&ctx->builder, var_type,
                                                ctx->helper_invocation_var);
   }
   store_def(ctx, intr->def.index, result, nir_type_bool);
}

static void
emit_barrier(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvScope scope = get_scope(nir_intrinsic_execution_scope(intr));
   SpvScope mem_scope = get_scope(nir_intrinsic_memory_scope(intr));
   SpvMemorySemanticsMask semantics = 0;

   if (nir_intrinsic_memory_scope(intr) != SCOPE_NONE) {
      nir_variable_mode modes = nir_intrinsic_memory_modes(intr);

      if (modes & nir_var_image)
         semantics |= SpvMemorySemanticsImageMemoryMask;

      if (modes & nir_var_mem_shared)
         semantics |= SpvMemorySemanticsWorkgroupMemoryMask;

      if (modes & (nir_var_mem_ssbo | nir_var_mem_global))
         semantics |= SpvMemorySemanticsUniformMemoryMask;

      if (modes & nir_var_mem_global)
         semantics |= SpvMemorySemanticsCrossWorkgroupMemoryMask;

      if (modes & (nir_var_shader_out | nir_var_mem_task_payload))
         semantics |= SpvMemorySemanticsOutputMemoryMask;

      if (!modes)
         semantics = SpvMemorySemanticsWorkgroupMemoryMask |
                     SpvMemorySemanticsUniformMemoryMask |
                     SpvMemorySemanticsImageMemoryMask |
                     SpvMemorySemanticsCrossWorkgroupMemoryMask;
      semantics |= SpvMemorySemanticsAcquireReleaseMask;
   }

   if (nir_intrinsic_execution_scope(intr) != SCOPE_NONE)
      spirv_builder_emit_control_barrier(&ctx->builder, scope, mem_scope, semantics);
   else
      spirv_builder_emit_memory_barrier(&ctx->builder, mem_scope, semantics);
}

static void
emit_derivative(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvOp op;
   switch (intr->intrinsic) {
   case nir_intrinsic_ddx:
      op = SpvOpDPdx;
      break;
   case nir_intrinsic_ddy:
      op = SpvOpDPdy;
      break;
   case nir_intrinsic_ddx_fine:
      op = SpvOpDPdxFine;
      break;
   case nir_intrinsic_ddy_fine:
      op = SpvOpDPdyFine;
      break;
   case nir_intrinsic_ddx_coarse:
      op = SpvOpDPdxCoarse;
      break;
   case nir_intrinsic_ddy_coarse:
      op = SpvOpDPdyCoarse;
      break;
   default:
      UNREACHABLE("invalid ddx/ddy");
   }

   if (op != SpvOpDPdx && op != SpvOpDPdy)
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityDerivativeControl);

   SpvId type = get_fvec_type(ctx, intr->def.bit_size, intr->def.num_components);

   nir_alu_type atype;
   SpvId value = get_src(ctx, &intr->src[0], &atype);
   if (atype != nir_type_float)
      value = emit_bitcast(ctx, type, value);

   SpvId result = emit_unop(ctx, op, type, value);
   store_def(ctx, intr->def.index, result, nir_type_float);
}

static void
emit_subgroup(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvOp op;

   switch (nir_intrinsic_reduction_op(intr)) {
#define SUBGROUP_CASE(nir, spirv) \
   case nir_op_##nir: \
      op = SpvOpGroupNonUniform##spirv; \
      break
   SUBGROUP_CASE(iadd, IAdd);
   SUBGROUP_CASE(fadd, FAdd);
   SUBGROUP_CASE(imul, IMul);
   SUBGROUP_CASE(fmul, FMul);
   SUBGROUP_CASE(imin, SMin);
   SUBGROUP_CASE(umin, UMin);
   SUBGROUP_CASE(fmin, FMin);
   SUBGROUP_CASE(imax, SMax);
   SUBGROUP_CASE(umax, UMax);
   SUBGROUP_CASE(fmax, FMax);
#undef SUBGROUP_CASE

#define SUBGROUP_CASE_LOGICAL(nir, spirv) \
   case nir_op_##nir: \
      op = intr->src[0].ssa->bit_size != 1 ? SpvOpGroupNonUniformBitwise##spirv : SpvOpGroupNonUniformLogical##spirv; \
      break
   SUBGROUP_CASE_LOGICAL(iand, And);
   SUBGROUP_CASE_LOGICAL(ior, Or);
   SUBGROUP_CASE_LOGICAL(ixor, Xor);
#undef SUBGROUP_CASE_LOGICAL
   default:
      fprintf(stderr, "emit_subgroup: reduction op not implemented (%s)\n",
              nir_intrinsic_infos[nir_intrinsic_reduction_op(intr)].name);
      UNREACHABLE("unhandled intrinsic");
   }

   SpvGroupOperation groupop;
   unsigned cluster_size = 0;
   switch (intr->intrinsic) {
   case nir_intrinsic_reduce:
      cluster_size = nir_intrinsic_cluster_size(intr);
      groupop = cluster_size ? SpvGroupOperationClusteredReduce : SpvGroupOperationReduce;
      break;
   case nir_intrinsic_inclusive_scan:
      groupop = SpvGroupOperationInclusiveScan;
      break;
   case nir_intrinsic_exclusive_scan:
      groupop = SpvGroupOperationExclusiveScan;
      break;
   default:
      fprintf(stderr, "emit_subgroup: not implemented (%s)\n",
              nir_intrinsic_infos[intr->intrinsic].name);
      UNREACHABLE("unhandled intrinsic");
   }
   spirv_builder_emit_cap(&ctx->builder, cluster_size ? SpvCapabilityGroupNonUniformClustered : SpvCapabilityGroupNonUniformArithmetic);

   nir_alu_type atype;
   SpvId src0 = get_src(ctx, &intr->src[0], &atype);
   switch (op) {
   case SpvOpGroupNonUniformFAdd:
   case SpvOpGroupNonUniformFMul:
   case SpvOpGroupNonUniformFMin:
   case SpvOpGroupNonUniformFMax:
      atype = nir_type_float;
      src0 = emit_bitcast(ctx, get_def_type(ctx, intr->src[0].ssa, atype), src0);
      break;
   default: break;
   }
   SpvId type = get_def_type(ctx, intr->src[0].ssa, atype);
   SpvId result = 0;
   if (cluster_size)
      result = spirv_builder_emit_triop_subgroup(&ctx->builder, op, type, groupop, src0, spirv_builder_const_uint(&ctx->builder, 32, cluster_size));
   else
      result = spirv_builder_emit_binop_subgroup(&ctx->builder, op, type, groupop, src0);
   store_def(ctx, intr->def.index, result, atype);
}

static void
emit_subgroup_quad(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvOp op;
   nir_alu_type atype, itype;
   SpvId src0 = get_src(ctx, &intr->src[0], &atype);
   SpvId src1 = 0;
   enum {
      QUAD_SWAP_HORIZONTAL,
      QUAD_SWAP_VERTICAL,
      QUAD_SWAP_DIAGONAL,
   };

   switch (intr->intrinsic) {
   case nir_intrinsic_quad_broadcast:
      op = SpvOpGroupNonUniformQuadBroadcast;
      src1 = get_src(ctx, &intr->src[1], &itype);
      if (itype != nir_type_uint)
         src1 = emit_bitcast(ctx, get_def_type(ctx, intr->src[1].ssa, nir_type_uint), src1);
      break;
   case nir_intrinsic_quad_swap_horizontal:
      op = SpvOpGroupNonUniformQuadSwap;
      src1 = spirv_builder_const_uint(&ctx->builder, 32, QUAD_SWAP_HORIZONTAL);
      break;
   case nir_intrinsic_quad_swap_vertical:
      op = SpvOpGroupNonUniformQuadSwap;
      src1 = spirv_builder_const_uint(&ctx->builder, 32, QUAD_SWAP_VERTICAL);
      break;
   case nir_intrinsic_quad_swap_diagonal:
      op = SpvOpGroupNonUniformQuadSwap;
      src1 = spirv_builder_const_uint(&ctx->builder, 32, QUAD_SWAP_DIAGONAL);
      break;
   default:
      fprintf(stderr, "emit_subgroup_quad: not implemented (%s)\n",
              nir_intrinsic_infos[intr->intrinsic].name);
      UNREACHABLE("unhandled intrinsic");
   }
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityGroupNonUniformQuad);

   SpvId result = spirv_builder_emit_binop_subgroup(&ctx->builder, op, get_def_type(ctx, intr->src[0].ssa, atype), src0, src1);
   store_def(ctx, intr->def.index, result, atype);
}

static void
emit_shuffle(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   SpvOp op;

   switch (intr->intrinsic) {
   case nir_intrinsic_shuffle:
      op = SpvOpGroupNonUniformShuffle;
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityGroupNonUniformShuffle);
      break;
   case nir_intrinsic_shuffle_xor:
      op = SpvOpGroupNonUniformShuffleXor;
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityGroupNonUniformShuffle);
      break;
   case nir_intrinsic_shuffle_up:
      op = SpvOpGroupNonUniformShuffleUp;
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityGroupNonUniformShuffleRelative);
      break;
   case nir_intrinsic_shuffle_down:
      op = SpvOpGroupNonUniformShuffleDown;
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityGroupNonUniformShuffleRelative);
      break;
   default:
      fprintf(stderr, "emit_shuffle: not implemented (%s)\n",
              nir_intrinsic_infos[intr->intrinsic].name);
      UNREACHABLE("unhandled intrinsic");
   }
   nir_alu_type atype, unused;
   SpvId src0 = get_src(ctx, &intr->src[0], &atype);
   SpvId src1 = get_src(ctx, &intr->src[1], &unused);

   SpvId result = spirv_builder_emit_binop_subgroup(&ctx->builder, op, get_def_type(ctx, intr->src[0].ssa, atype), src0, src1);
   store_def(ctx, intr->def.index, result, atype);
}

static void
emit_elect(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   spirv_builder_emit_cap(&ctx->builder, SpvCapabilityGroupNonUniform);
   SpvId result = spirv_builder_emit_unop_const(&ctx->builder, SpvOpGroupNonUniformElect, spirv_builder_type_bool(&ctx->builder), SpvScopeSubgroup);
   store_def(ctx, intr->def.index, result, nir_type_bool);
}

static void
emit_intrinsic(struct ntv_context *ctx, nir_intrinsic_instr *intr)
{
   switch (intr->intrinsic) {
   case nir_intrinsic_decl_reg:
      /* Nothing to do */
      break;

   case nir_intrinsic_load_reg:
      emit_load_reg(ctx, intr);
      break;

   case nir_intrinsic_store_reg:
      emit_store_reg(ctx, intr);
      break;

   case nir_intrinsic_terminate:
      emit_discard(ctx, intr);
      break;

   case nir_intrinsic_demote:
      spirv_builder_emit_demote(&ctx->builder);
      break;

   case nir_intrinsic_load_deref:
      emit_load_deref(ctx, intr);
      break;

   case nir_intrinsic_store_deref:
      emit_store_deref(ctx, intr);
      break;

   case nir_intrinsic_load_push_constant_zink:
      emit_load_push_const(ctx, intr);
      break;

   case nir_intrinsic_load_global:
   case nir_intrinsic_load_global_constant:
      emit_load_global(ctx, intr);
      break;

   case nir_intrinsic_store_global:
      emit_store_global(ctx, intr);
      break;

   case nir_intrinsic_load_front_face:
      emit_load_front_face(ctx, intr);
      break;

   case nir_intrinsic_load_view_index:
      emit_load_view_index(ctx, intr);
      break;

   case nir_intrinsic_load_base_instance:
      emit_load_uint_input(ctx, intr, &ctx->base_instance_var, "gl_BaseInstance", SpvBuiltInBaseInstance);
      break;

   case nir_intrinsic_load_instance_id:
      emit_load_uint_input(ctx, intr, &ctx->instance_id_var, "gl_InstanceId", SpvBuiltInInstanceIndex);
      break;

   case nir_intrinsic_load_base_vertex:
      emit_load_uint_input(ctx, intr, &ctx->base_vertex_var, "gl_BaseVertex", SpvBuiltInBaseVertex);
      break;

   case nir_intrinsic_load_draw_id:
      emit_load_uint_input(ctx, intr, &ctx->draw_id_var, "gl_DrawID", SpvBuiltInDrawIndex);
      break;

   case nir_intrinsic_load_vertex_id:
      emit_load_uint_input(ctx, intr, &ctx->vertex_id_var, "gl_VertexId", SpvBuiltInVertexIndex);
      break;

   case nir_intrinsic_load_primitive_id:
      emit_load_uint_input(ctx, intr, &ctx->primitive_id_var, "gl_PrimitiveIdIn", SpvBuiltInPrimitiveId);
      break;

   case nir_intrinsic_load_invocation_id:
      emit_load_uint_input(ctx, intr, &ctx->invocation_id_var, "gl_InvocationId", SpvBuiltInInvocationId);
      break;

   case nir_intrinsic_load_sample_id:
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilitySampleRateShading);
      emit_load_uint_input(ctx, intr, &ctx->sample_id_var, "gl_SampleId", SpvBuiltInSampleId);
      break;

   case nir_intrinsic_load_point_coord_maybe_flipped:
   case nir_intrinsic_load_point_coord:
      emit_load_vec_input(ctx, intr, &ctx->point_coord_var, "gl_PointCoord", SpvBuiltInPointCoord, nir_type_float);
      break;

   case nir_intrinsic_load_sample_pos:
      emit_load_vec_input(ctx, intr, &ctx->sample_pos_var, "gl_SamplePosition", SpvBuiltInSamplePosition, nir_type_float);
      break;

   case nir_intrinsic_load_sample_mask_in:
      emit_load_uint_input(ctx, intr, &ctx->sample_mask_in_var, "gl_SampleMaskIn", SpvBuiltInSampleMask);
      break;

   case nir_intrinsic_emit_vertex:
      if (ctx->nir->info.gs.vertices_out) //skip vertex emission if !vertices_out
         spirv_builder_emit_vertex(&ctx->builder, nir_intrinsic_stream_id(intr),
                                   ctx->nir->info.stage == MESA_SHADER_GEOMETRY && util_bitcount(ctx->nir->info.gs.active_stream_mask) > 1);
      break;

   case nir_intrinsic_end_primitive:
      spirv_builder_end_primitive(&ctx->builder, nir_intrinsic_stream_id(intr),
                                  ctx->nir->info.stage == MESA_SHADER_GEOMETRY && util_bitcount(ctx->nir->info.gs.active_stream_mask) > 1);
      break;

   case nir_intrinsic_is_helper_invocation:
   case nir_intrinsic_load_helper_invocation:
      emit_is_helper_invocation(ctx, intr);
      break;

   case nir_intrinsic_load_patch_vertices_in:
      emit_load_vec_input(ctx, intr, &ctx->tess_patch_vertices_in, "gl_PatchVerticesIn",
                          SpvBuiltInPatchVertices, nir_type_int);
      break;

   case nir_intrinsic_load_tess_coord:
      emit_load_vec_input(ctx, intr, &ctx->tess_coord_var, "gl_TessCoord",
                          SpvBuiltInTessCoord, nir_type_float);
      break;

   case nir_intrinsic_barrier:
      emit_barrier(ctx, intr);
      break;

   case nir_intrinsic_interp_deref_at_centroid:
   case nir_intrinsic_interp_deref_at_sample:
   case nir_intrinsic_interp_deref_at_offset:
      emit_interpolate(ctx, intr);
      break;

   case nir_intrinsic_deref_atomic:
   case nir_intrinsic_deref_atomic_swap:
      emit_deref_atomic_intrinsic(ctx, intr);
      break;

   case nir_intrinsic_shared_atomic:
   case nir_intrinsic_shared_atomic_swap:
      emit_shared_atomic_intrinsic(ctx, intr);
      break;

   case nir_intrinsic_global_atomic:
   case nir_intrinsic_global_atomic_swap:
      emit_global_atomic_intrinsic(ctx, intr);
      break;

   case nir_intrinsic_begin_invocation_interlock:
   case nir_intrinsic_end_invocation_interlock:
      spirv_builder_emit_interlock(&ctx->builder, intr->intrinsic == nir_intrinsic_end_invocation_interlock);
      break;

   case nir_intrinsic_get_ssbo_size:
      emit_get_ssbo_size(ctx, intr);
      break;

   case nir_intrinsic_image_deref_store:
      emit_image_deref_store(ctx, intr);
      break;

   case nir_intrinsic_image_deref_sparse_load:
   case nir_intrinsic_image_deref_load:
      emit_image_deref_load(ctx, intr);
      break;

   case nir_intrinsic_image_deref_size:
      emit_image_deref_size(ctx, intr);
      break;

   case nir_intrinsic_image_deref_samples:
      emit_image_deref_samples(ctx, intr);
      break;

   case nir_intrinsic_image_deref_atomic:
   case nir_intrinsic_image_deref_atomic_swap:
      emit_image_intrinsic(ctx, intr);
      break;

   case nir_intrinsic_load_workgroup_id:
      emit_load_vec_input(ctx, intr, &ctx->workgroup_id_var, "gl_WorkGroupID", SpvBuiltInWorkgroupId, nir_type_uint);
      break;

   case nir_intrinsic_load_num_workgroups:
      emit_load_vec_input(ctx, intr, &ctx->num_workgroups_var, "gl_NumWorkGroups", SpvBuiltInNumWorkgroups, nir_type_uint);
      break;

   case nir_intrinsic_load_local_invocation_id:
      emit_load_vec_input(ctx, intr, &ctx->local_invocation_id_var, "gl_LocalInvocationID", SpvBuiltInLocalInvocationId, nir_type_uint);
      break;

   case nir_intrinsic_load_global_invocation_id:
      emit_load_vec_input(ctx, intr, &ctx->global_invocation_id_var, "gl_GlobalInvocationID", SpvBuiltInGlobalInvocationId, nir_type_uint);
      break;

   case nir_intrinsic_load_local_invocation_index:
      emit_load_uint_input(ctx, intr, &ctx->local_invocation_index_var, "gl_LocalInvocationIndex", SpvBuiltInLocalInvocationIndex);
      break;

#define LOAD_SHADER_BALLOT(lowercase, camelcase) \
   case nir_intrinsic_load_##lowercase: \
      emit_load_uint_input(ctx, intr, &ctx->lowercase##_var, "gl_"#camelcase, SpvBuiltIn##camelcase); \
      break

   LOAD_SHADER_BALLOT(subgroup_id, SubgroupId);
   LOAD_SHADER_BALLOT(subgroup_eq_mask, SubgroupEqMask);
   LOAD_SHADER_BALLOT(subgroup_ge_mask, SubgroupGeMask);
   LOAD_SHADER_BALLOT(subgroup_invocation, SubgroupLocalInvocationId);
   LOAD_SHADER_BALLOT(subgroup_le_mask, SubgroupLeMask);
   LOAD_SHADER_BALLOT(subgroup_lt_mask, SubgroupLtMask);
   LOAD_SHADER_BALLOT(subgroup_size, SubgroupSize);
   LOAD_SHADER_BALLOT(num_subgroups, NumSubgroups);

   case nir_intrinsic_ballot:
      emit_ballot(ctx, intr);
      break;

   case nir_intrinsic_read_first_invocation:
      emit_read_first_invocation(ctx, intr);
      break;

   case nir_intrinsic_read_invocation:
      emit_read_invocation(ctx, intr);
      break;

   case nir_intrinsic_load_workgroup_size:
      assert(ctx->local_group_size_var);
      store_def(ctx, intr->def.index, ctx->local_group_size_var, nir_type_uint);
      break;

   case nir_intrinsic_load_shared:
      emit_load_shared(ctx, intr);
      break;

   case nir_intrinsic_store_shared:
      emit_store_shared(ctx, intr);
      break;

   case nir_intrinsic_load_scratch:
      emit_load_scratch(ctx, intr);
      break;

   case nir_intrinsic_store_scratch:
      emit_store_scratch(ctx, intr);
      break;

   case nir_intrinsic_shader_clock:
      emit_shader_clock(ctx, intr);
      break;

   case nir_intrinsic_vote_all:
   case nir_intrinsic_vote_any:
   case nir_intrinsic_vote_ieq:
   case nir_intrinsic_vote_feq:
      emit_vote(ctx, intr);
      break;

   case nir_intrinsic_is_sparse_resident_zink:
      emit_is_sparse_texels_resident(ctx, intr);
      break;

   case nir_intrinsic_ddx:
   case nir_intrinsic_ddy:
   case nir_intrinsic_ddx_fine:
   case nir_intrinsic_ddy_fine:
   case nir_intrinsic_ddx_coarse:
   case nir_intrinsic_ddy_coarse:
      emit_derivative(ctx, intr);
      break;

   case nir_intrinsic_reduce:
   case nir_intrinsic_inclusive_scan:
   case nir_intrinsic_exclusive_scan:
      emit_subgroup(ctx, intr);
      break;

   case nir_intrinsic_quad_broadcast:
   case nir_intrinsic_quad_swap_horizontal:
   case nir_intrinsic_quad_swap_vertical:
   case nir_intrinsic_quad_swap_diagonal:
      emit_subgroup_quad(ctx, intr);
      break;

   case nir_intrinsic_shuffle:
   case nir_intrinsic_shuffle_xor:
   case nir_intrinsic_shuffle_up:
   case nir_intrinsic_shuffle_down:
      emit_shuffle(ctx, intr);
      break;

   case nir_intrinsic_elect:
      emit_elect(ctx, intr);
      break;

   default:
      fprintf(stderr, "emit_intrinsic: not implemented (%s)\n",
              nir_intrinsic_infos[intr->intrinsic].name);
      UNREACHABLE("unsupported intrinsic");
   }
}

static void
emit_undef(struct ntv_context *ctx, nir_undef_instr *undef)
{
   SpvId type = undef->def.bit_size == 1 ? get_bvec_type(ctx, undef->def.num_components) :
                                           get_uvec_type(ctx, undef->def.bit_size,
                                                         undef->def.num_components);

   store_def(ctx, undef->def.index,
                 spirv_builder_emit_undef(&ctx->builder, type),
                 undef->def.bit_size == 1 ? nir_type_bool : nir_type_uint);
}

static SpvId
get_src_float(struct ntv_context *ctx, nir_src *src)
{
   nir_alu_type atype;
   SpvId def = get_src(ctx, src, &atype);
   if (atype == nir_type_float)
      return def;
   unsigned num_components = nir_src_num_components(*src);
   unsigned bit_size = nir_src_bit_size(*src);
   return bitcast_to_fvec(ctx, def, bit_size, num_components);
}

static SpvId
get_src_int(struct ntv_context *ctx, nir_src *src)
{
   nir_alu_type atype;
   SpvId def = get_src(ctx, src, &atype);
   if (atype == nir_type_int)
      return def;
   unsigned num_components = nir_src_num_components(*src);
   unsigned bit_size = nir_src_bit_size(*src);
   return bitcast_to_ivec(ctx, def, bit_size, num_components);
}

static inline bool
tex_instr_is_lod_allowed(nir_tex_instr *tex)
{
   /* This can only be used with an OpTypeImage that has a Dim operand of 1D, 2D, 3D, or Cube
    * - SPIR-V: 3.14. Image Operands
    */

   return (tex->sampler_dim == GLSL_SAMPLER_DIM_1D ||
           tex->sampler_dim == GLSL_SAMPLER_DIM_2D ||
           tex->sampler_dim == GLSL_SAMPLER_DIM_3D ||
           tex->sampler_dim == GLSL_SAMPLER_DIM_CUBE ||
           /* External images are interpreted as 2D in type_to_dim,
            * so LOD is allowed */
           tex->sampler_dim == GLSL_SAMPLER_DIM_EXTERNAL ||
           /* RECT will always become 2D, so this is fine */
           tex->sampler_dim == GLSL_SAMPLER_DIM_RECT);
}

static nir_variable *
get_tex_srcs(struct ntv_context *ctx, nir_tex_instr *tex,
             nir_variable **bindless_var, unsigned *coord_components,
             struct spriv_tex_src *tex_src)
{
   nir_variable *var = NULL;
   nir_alu_type atype;
   tex_src->sparse = tex->is_sparse;
   for (unsigned i = 0; i < tex->num_srcs; i++) {
      nir_const_value *cv;
      switch (tex->src[i].src_type) {
      case nir_tex_src_texture_deref:
         var = nir_deref_instr_get_variable(nir_def_as_deref(tex->src[i].src.ssa));
         tex_src->src = get_src(ctx, &tex->src[i].src, &atype);
         break;
      case nir_tex_src_sampler_deref:
         tex_src->cl_sampler = get_src(ctx, &tex->src[i].src, &atype);
         break;

      case nir_tex_src_coord:
         if (tex->op == nir_texop_txf ||
             tex->op == nir_texop_txf_ms)
            tex_src->coord = get_src_int(ctx, &tex->src[i].src);
         else
            tex_src->coord = get_src_float(ctx, &tex->src[i].src);
         *coord_components = nir_src_num_components(tex->src[i].src);
         break;

      case nir_tex_src_projector:
         assert(nir_src_num_components(tex->src[i].src) == 1);
         tex_src->proj = get_src_float(ctx, &tex->src[i].src);
         assert(tex_src->proj != 0);
         break;

      case nir_tex_src_offset:
         cv = nir_src_as_const_value(tex->src[i].src);
         if (cv) {
            unsigned bit_size = nir_src_bit_size(tex->src[i].src);
            unsigned num_components = nir_src_num_components(tex->src[i].src);

            SpvId components[NIR_MAX_VEC_COMPONENTS];
            for (int j = 0; j < num_components; ++j) {
               int64_t tmp = nir_const_value_as_int(cv[j], bit_size);
               components[j] = emit_int_const(ctx, bit_size, tmp);
            }

            if (num_components > 1) {
               SpvId type = get_ivec_type(ctx, bit_size, num_components);
               tex_src->const_offset = spirv_builder_const_composite(&ctx->builder,
                                                                    type,
                                                                    components,
                                                                    num_components);
            } else
               tex_src->const_offset = components[0];
         } else
            tex_src->offset = get_src_int(ctx, &tex->src[i].src);
         break;

      case nir_tex_src_bias:
         assert(tex->op == nir_texop_txb);
         tex_src->bias = get_src_float(ctx, &tex->src[i].src);
         assert(tex_src->bias != 0);
         break;

      case nir_tex_src_min_lod:
         assert(nir_src_num_components(tex->src[i].src) == 1);
         tex_src->min_lod = get_src_float(ctx, &tex->src[i].src);
         assert(tex_src->min_lod != 0);
         break;

      case nir_tex_src_lod:
         assert(nir_src_num_components(tex->src[i].src) == 1);
         if (tex->op == nir_texop_txf ||
             tex->op == nir_texop_txf_ms ||
             tex->op == nir_texop_txs)
            tex_src->lod = get_src_int(ctx, &tex->src[i].src);
         else
            tex_src->lod = get_src_float(ctx, &tex->src[i].src);
         assert(tex_src->lod != 0);
         break;

      case nir_tex_src_ms_index:
         assert(nir_src_num_components(tex->src[i].src) == 1);
         tex_src->sample = get_src_int(ctx, &tex->src[i].src);
         break;

      case nir_tex_src_comparator:
         assert(nir_src_num_components(tex->src[i].src) == 1);
         tex_src->dref = get_src_float(ctx, &tex->src[i].src);
         assert(tex_src->dref != 0);
         break;

      case nir_tex_src_ddx:
         tex_src->dx = get_src_float(ctx, &tex->src[i].src);
         assert(tex_src->dx != 0);
         break;

      case nir_tex_src_ddy:
         tex_src->dy = get_src_float(ctx, &tex->src[i].src);
         assert(tex_src->dy != 0);
         break;

      case nir_tex_src_texture_offset:
         tex_src->tex_offset = get_src_int(ctx, &tex->src[i].src);
         break;

      case nir_tex_src_sampler_offset:
      case nir_tex_src_sampler_handle:
         /* don't care */
         break;

      case nir_tex_src_texture_handle:
         tex_src->src = get_src(ctx, &tex->src[i].src, &atype);
         var = *bindless_var = nir_deref_instr_get_variable(nir_src_as_deref(tex->src[i].src));
         break;

      default:
         fprintf(stderr, "texture source: %d\n", tex->src[i].src_type);
         UNREACHABLE("unknown texture source");
      }
   }
   return var;
}

static SpvId
get_texture_load(struct ntv_context *ctx, SpvId sampler_id, nir_tex_instr *tex,
                 SpvId cl_sampler, SpvId image_type, SpvId sampled_type)
{
   if (ctx->stage == MESA_SHADER_KERNEL) {
      SpvId image_load = spirv_builder_emit_load(&ctx->builder, image_type, sampler_id);
      if (nir_tex_instr_need_sampler(tex)) {
         SpvId sampler_load = spirv_builder_emit_load(&ctx->builder, spirv_builder_type_sampler(&ctx->builder),
                                                      cl_sampler);
         return spirv_builder_emit_sampled_image(&ctx->builder, sampled_type, image_load, sampler_load);
      } else {
         return image_load;
      }
   } else {
      return spirv_builder_emit_load(&ctx->builder, sampled_type, sampler_id);
   }
}

static SpvId
get_texop_dest_type(struct ntv_context *ctx, const nir_tex_instr *tex)
{
   SpvId actual_dest_type;
   unsigned num_components = tex->def.num_components;
   switch (nir_alu_type_get_base_type(tex->dest_type)) {
   case nir_type_int:
      actual_dest_type = get_ivec_type(ctx, 32, num_components);
      break;

   case nir_type_uint:
      actual_dest_type = get_uvec_type(ctx, 32, num_components);
      break;

   case nir_type_float:
      actual_dest_type = get_fvec_type(ctx, 32, num_components);
      break;

   default:
      UNREACHABLE("unexpected nir_alu_type");
   }

   return actual_dest_type;
}

static void
move_tex_proj_to_coord(struct ntv_context *ctx, unsigned coord_components, struct spriv_tex_src *tex_src)
{
   SpvId constituents[NIR_MAX_VEC_COMPONENTS + 1];
   if (coord_components == 1)
      constituents[0] = tex_src->coord;
   else {
      assert(coord_components > 1);
      SpvId float_type = spirv_builder_type_float(&ctx->builder, 32);
      for (uint32_t i = 0; i < coord_components; ++i)
         constituents[i] = spirv_builder_emit_composite_extract(&ctx->builder,
                                                                float_type,
                                                                tex_src->coord,
                                                                &i, 1);
   }

   constituents[coord_components++] = tex_src->proj;

   SpvId vec_type = get_fvec_type(ctx, 32, coord_components);
   tex_src->coord = spirv_builder_emit_composite_construct(&ctx->builder,
                                                           vec_type,
                                                           constituents,
                                                           coord_components);
}

static SpvId
get_tex_image_to_load( struct ntv_context *ctx, SpvId image_type, bool is_buffer, SpvId load)
{
   return is_buffer || ctx->stage == MESA_SHADER_KERNEL ?
              load :
              spirv_builder_emit_image(&ctx->builder, image_type, load);
}

static SpvId
emit_tex_readop(struct ntv_context *ctx, nir_variable *bindless_var, SpvId load,
                struct spriv_tex_src *tex_src, SpvId dest_type, bool is_buffer,
                nir_variable *var, SpvId image_type, nir_tex_instr *tex)
{
   SpvId actual_dest_type = get_texop_dest_type(ctx, tex);

   SpvId result;
   if (tex_src->offset)
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityImageGatherExtended);
   if (tex_src->min_lod)
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityMinLod);
   if (tex->op == nir_texop_txf ||
       tex->op == nir_texop_txf_ms ||
       tex->op == nir_texop_tg4) {
      SpvId image = get_tex_image_to_load(ctx, image_type, is_buffer, load);

      if (tex->op == nir_texop_tg4) {
         if (tex_src->const_offset)
            spirv_builder_emit_cap(&ctx->builder, SpvCapabilityImageGatherExtended);
         result = spirv_builder_emit_image_gather(&ctx->builder, actual_dest_type,
                                                  load, tex_src, emit_uint_const(ctx, 32, tex->component));
         actual_dest_type = dest_type;
      } else {
         assert(tex->op == nir_texop_txf_ms || !tex_src->sample);
         bool is_ms;
         type_to_dim(glsl_get_sampler_dim(glsl_without_array(var->type)), &is_ms);
         assert(is_ms || !tex_src->sample);
         result = spirv_builder_emit_image_fetch(&ctx->builder, actual_dest_type,
                                                 image, tex_src);
      }
   } else {
      if (tex->op == nir_texop_txl)
         tex_src->min_lod = 0;
      result = spirv_builder_emit_image_sample(&ctx->builder,
                                               actual_dest_type, load,
                                               tex_src);
   }

   if (!bindless_var && (var->data.precision == GLSL_PRECISION_MEDIUM || var->data.precision == GLSL_PRECISION_LOW)) {
      spirv_builder_emit_decoration(&ctx->builder, result,
                                    SpvDecorationRelaxedPrecision);
   }

   if (tex->is_sparse)
      result = extract_sparse_load(ctx, result, actual_dest_type, &tex->def);

   if (tex->def.bit_size != 32) {
      /* convert FP32 to FP16 */
      result = emit_unop(ctx, SpvOpFConvert, dest_type, result);
   }

   return result;
}

static void
emit_tex(struct ntv_context *ctx, nir_tex_instr *tex)
{
   assert(tex->op == nir_texop_tex ||
          tex->op == nir_texop_txb ||
          tex->op == nir_texop_txl ||
          tex->op == nir_texop_txd ||
          tex->op == nir_texop_txf ||
          tex->op == nir_texop_txf_ms ||
          tex->op == nir_texop_txs ||
          tex->op == nir_texop_lod ||
          tex->op == nir_texop_tg4 ||
          tex->op == nir_texop_texture_samples ||
          tex->op == nir_texop_query_levels);

   struct spriv_tex_src tex_src = {0};
   unsigned coord_components = 0;
   nir_variable *bindless_var = NULL;
   nir_variable *var = get_tex_srcs(ctx, tex, &bindless_var, &coord_components, &tex_src);

   assert(var);
   SpvId image_type = find_image_type(ctx, var);
   assert(image_type);

   bool is_buffer = glsl_get_sampler_dim(glsl_without_array(var->type)) ==
                    GLSL_SAMPLER_DIM_BUF;
   SpvId sampled_type = is_buffer ? image_type :
                            spirv_builder_type_sampled_image(&ctx->builder, image_type);
   assert(sampled_type);

   SpvId sampler_id = tex_src.src;
   if (tex_src.tex_offset) {
      SpvId ptr = spirv_builder_type_pointer(&ctx->builder, SpvStorageClassUniformConstant, sampled_type);
      sampler_id = spirv_builder_emit_access_chain(&ctx->builder, ptr, sampler_id, &tex_src.tex_offset, 1);
   }

   SpvId load = get_texture_load(ctx, sampler_id, tex, tex_src.cl_sampler, image_type, sampled_type);

   if (tex->is_sparse)
      tex->def.num_components--;
   SpvId dest_type = get_def_type(ctx, &tex->def, tex->dest_type);

   if (nir_tex_instr_is_query(tex))
      spirv_builder_emit_cap(&ctx->builder, SpvCapabilityImageQuery);

   if (!tex_instr_is_lod_allowed(tex))
      tex_src.lod = 0;
   else if (ctx->stage != MESA_SHADER_FRAGMENT &&
            tex->op == nir_texop_tex && ctx->explicit_lod && !tex_src.lod)
      tex_src.lod = emit_float_const(ctx, 32, 0.0);

   if (tex_src.proj && coord_components > 0)
      move_tex_proj_to_coord(ctx, coord_components, &tex_src);

   SpvId result = 0;

   switch (tex->op) {
   case nir_texop_txs: {
      SpvId image = get_tex_image_to_load(ctx, image_type, is_buffer, load);
      /* Its Dim operand must be one of 1D, 2D, 3D, or Cube
       * - OpImageQuerySizeLod specification
       *
       * Additionally, if its Dim is 1D, 2D, 3D, or Cube,
       * it must also have either an MS of 1 or a Sampled of 0 or 2.
       * - OpImageQuerySize specification
       *
       * all spirv samplers use these types
       */
      if (!tex_src.lod && tex_instr_is_lod_allowed(tex))
         tex_src.lod = emit_uint_const(ctx, 32, 0);
      result = spirv_builder_emit_image_query_size(&ctx->builder,
                                                   dest_type, image,
                                                   tex_src.lod);
      break;
   }
   case nir_texop_query_levels: {
      SpvId image = get_tex_image_to_load(ctx, image_type, is_buffer, load);
      result = spirv_builder_emit_image_query_levels(&ctx->builder,
                                                     dest_type, image);
      break;
   }
   case nir_texop_texture_samples: {
      SpvId image = get_tex_image_to_load(ctx, image_type, is_buffer, load);
      result = spirv_builder_emit_unop(&ctx->builder, SpvOpImageQuerySamples,
                                       dest_type, image);
      break;
   }
   case nir_texop_lod: {
      result = spirv_builder_emit_image_query_lod(&ctx->builder,
                                                  dest_type, load,
                                                  tex_src.coord);
      break;
   }
   default:
      result = emit_tex_readop(ctx, bindless_var, load, &tex_src,
                               dest_type, is_buffer, var, image_type, tex);
      break;
   }

   store_def(ctx, tex->def.index, result, tex->dest_type);

   if (tex->is_sparse)
      tex->def.num_components++;
}

static void
start_block(struct ntv_context *ctx, SpvId label)
{
   /* terminate previous block if needed */
   if (ctx->block_started)
      spirv_builder_emit_branch(&ctx->builder, label);

   /* start new block */
   spirv_builder_label(&ctx->builder, label);
   ctx->block_started = true;
}

static void
branch(struct ntv_context *ctx, SpvId label)
{
   assert(ctx->block_started);
   spirv_builder_emit_branch(&ctx->builder, label);
   ctx->block_started = false;
}

static void
branch_conditional(struct ntv_context *ctx, SpvId condition, SpvId then_id,
                   SpvId else_id)
{
   assert(ctx->block_started);
   spirv_builder_emit_branch_conditional(&ctx->builder, condition,
                                         then_id, else_id);
   ctx->block_started = false;
}

static void
emit_jump(struct ntv_context *ctx, nir_jump_instr *jump)
{
   switch (jump->type) {
   case nir_jump_break:
      assert(ctx->loop_break);
      branch(ctx, ctx->loop_break);
      break;

   case nir_jump_continue:
      assert(ctx->loop_cont);
      branch(ctx, ctx->loop_cont);
      break;

   default:
      UNREACHABLE("Unsupported jump type\n");
   }
}

static void
emit_deref_var(struct ntv_context *ctx, nir_deref_instr *deref)
{
   assert(deref->deref_type == nir_deref_type_var);

   struct hash_entry *he = _mesa_hash_table_search(ctx->vars, deref->var);
   assert(he);
   SpvId result = (SpvId)(intptr_t)he->data;
   store_def(ctx, deref->def.index, result, get_nir_alu_type(deref->type));
}

static void
emit_deref_array(struct ntv_context *ctx, nir_deref_instr *deref)
{
   assert(deref->deref_type == nir_deref_type_array);
   nir_variable *var = nir_deref_instr_get_variable(deref);

   if (!nir_src_is_always_uniform(deref->arr.index)) {
      if (deref->modes & nir_var_mem_ubo)
         spirv_builder_emit_cap(&ctx->builder,
                                SpvCapabilityUniformBufferArrayDynamicIndexing);

      if (deref->modes & nir_var_mem_ssbo)
         spirv_builder_emit_cap(&ctx->builder,
                                SpvCapabilityStorageBufferArrayDynamicIndexing);

      if (deref->modes & (nir_var_uniform | nir_var_image)) {
         const struct glsl_type *type = glsl_without_array(var->type);
         assert(glsl_type_is_sampler(type) || glsl_type_is_image(type));

         if (glsl_type_is_sampler(type))
            spirv_builder_emit_cap(&ctx->builder,
                                   SpvCapabilitySampledImageArrayDynamicIndexing);
         else
            spirv_builder_emit_cap(&ctx->builder,
                                   SpvCapabilityStorageImageArrayDynamicIndexing);
      }
   }

   SpvStorageClass storage_class = get_storage_class(var);
   SpvId type;
   nir_alu_type atype = nir_type_uint;

   SpvId base = get_src(ctx, &deref->parent, &atype);

   switch (var->data.mode) {

   case nir_var_mem_ubo:
   case nir_var_mem_ssbo:
      base = get_src(ctx, &deref->parent, &atype);
      /* this is either the array<buffers> deref or the array<uint> deref */
      if (glsl_type_is_struct_or_ifc(deref->type)) {
         /* array<buffers> */
         type = get_bo_struct_type(ctx, var);
         break;
      }
      /* array<uint> */
      FALLTHROUGH;
   case nir_var_function_temp:
   case nir_var_shader_in:
   case nir_var_shader_out:
      base = get_src(ctx, &deref->parent, &atype);
      type = get_glsl_type(ctx, deref->type, var->data.mode & (nir_var_shader_temp | nir_var_function_temp));
      break;

   case nir_var_uniform:
   case nir_var_image: {
      base = get_src(ctx, &deref->parent, &atype);
      const struct glsl_type *gtype = glsl_without_array(deref->type);
      type = get_image_type(ctx, var,
                            glsl_type_is_sampler(gtype),
                            glsl_get_sampler_dim(gtype) == GLSL_SAMPLER_DIM_BUF);
      break;
   }

   default:
      UNREACHABLE("Unsupported nir_variable_mode\n");
   }

   nir_alu_type itype;
   SpvId index = get_src(ctx, &deref->arr.index, &itype);
   if (itype == nir_type_float)
      index = emit_bitcast(ctx, get_uvec_type(ctx, 32, 1), index);

   SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                               storage_class,
                                               type);

   SpvId result = spirv_builder_emit_access_chain(&ctx->builder,
                                                  ptr_type,
                                                  base,
                                                  &index, 1);
   /* uint is a bit of a lie here, it's really just an opaque type */
   store_def(ctx, deref->def.index, result, get_nir_alu_type(deref->type));
}

static void
emit_deref_struct(struct ntv_context *ctx, nir_deref_instr *deref)
{
   assert(deref->deref_type == nir_deref_type_struct);
   nir_variable *var = nir_deref_instr_get_variable(deref);

   SpvStorageClass storage_class = get_storage_class(var);

   SpvId index = emit_uint_const(ctx, 32, deref->strct.index);
   SpvId type = (var->data.mode & (nir_var_mem_ubo | nir_var_mem_ssbo)) ?
                get_bo_array_type(ctx, var) :
                get_glsl_type(ctx, deref->type, var->data.mode & (nir_var_shader_temp | nir_var_function_temp));

   SpvId ptr_type = spirv_builder_type_pointer(&ctx->builder,
                                               storage_class,
                                               type);

   nir_alu_type atype;
   SpvId result = spirv_builder_emit_access_chain(&ctx->builder,
                                                  ptr_type,
                                                  get_src(ctx, &deref->parent, &atype),
                                                  &index, 1);
   /* uint is a bit of a lie here, it's really just an opaque type */
   store_def(ctx, deref->def.index, result, get_nir_alu_type(deref->type));
}

static void
emit_deref(struct ntv_context *ctx, nir_deref_instr *deref)
{
   switch (deref->deref_type) {
   case nir_deref_type_var:
      emit_deref_var(ctx, deref);
      break;

   case nir_deref_type_array:
      emit_deref_array(ctx, deref);
      break;

   case nir_deref_type_struct:
      emit_deref_struct(ctx, deref);
      break;

   default:
      UNREACHABLE("unexpected deref_type");
   }
}

static void
emit_block(struct ntv_context *ctx, struct nir_block *block)
{
   start_block(ctx, block_label(ctx, block));
   nir_foreach_instr(instr, block) {
      switch (instr->type) {
      case nir_instr_type_alu:
         emit_alu(ctx, nir_instr_as_alu(instr));
         break;
      case nir_instr_type_intrinsic:
         emit_intrinsic(ctx, nir_instr_as_intrinsic(instr));
         break;
      case nir_instr_type_load_const:
         emit_load_const(ctx, nir_instr_as_load_const(instr));
         break;
      case nir_instr_type_undef:
         emit_undef(ctx, nir_instr_as_undef(instr));
         break;
      case nir_instr_type_tex:
         emit_tex(ctx, nir_instr_as_tex(instr));
         break;
      case nir_instr_type_phi:
         UNREACHABLE("nir_instr_type_phi not supported");
         break;
      case nir_instr_type_jump:
         emit_jump(ctx, nir_instr_as_jump(instr));
         break;
      case nir_instr_type_call:
         UNREACHABLE("nir_instr_type_call not supported");
         break;
      case nir_instr_type_parallel_copy:
         UNREACHABLE("nir_instr_type_parallel_copy not supported");
         break;
      case nir_instr_type_deref:
         emit_deref(ctx, nir_instr_as_deref(instr));
         break;
      }
   }
}

static void
emit_cf_list(struct ntv_context *ctx, struct exec_list *list);

static SpvId
get_src_bool(struct ntv_context *ctx, nir_src *src)
{
   assert(nir_src_bit_size(*src) == 1);
   nir_alu_type atype;
   return get_src(ctx, src, &atype);
}

static void
emit_if(struct ntv_context *ctx, nir_if *if_stmt)
{
   SpvId condition = get_src_bool(ctx, &if_stmt->condition);

   SpvId header_id = spirv_builder_new_id(&ctx->builder);
   SpvId then_id = block_label(ctx, nir_if_first_then_block(if_stmt));
   SpvId endif_id = spirv_builder_new_id(&ctx->builder);
   SpvId else_id = endif_id;

   bool has_else = !exec_list_is_empty(&if_stmt->else_list);
   if (has_else) {
      assert(nir_if_first_else_block(if_stmt)->index < ctx->num_blocks);
      else_id = block_label(ctx, nir_if_first_else_block(if_stmt));
   }

   /* create a header-block */
   start_block(ctx, header_id);
   spirv_builder_emit_selection_merge(&ctx->builder, endif_id,
                                      SpvSelectionControlMaskNone);
   branch_conditional(ctx, condition, then_id, else_id);

   emit_cf_list(ctx, &if_stmt->then_list);

   if (has_else) {
      if (ctx->block_started)
         branch(ctx, endif_id);

      emit_cf_list(ctx, &if_stmt->else_list);
   }

   start_block(ctx, endif_id);
}

static void
emit_loop(struct ntv_context *ctx, nir_loop *loop)
{
   assert(!nir_loop_has_continue_construct(loop));
   SpvId header_id = spirv_builder_new_id(&ctx->builder);
   SpvId begin_id = block_label(ctx, nir_loop_first_block(loop));
   SpvId break_id = spirv_builder_new_id(&ctx->builder);
   SpvId cont_id = spirv_builder_new_id(&ctx->builder);

   /* create a header-block */
   start_block(ctx, header_id);
   spirv_builder_loop_merge(&ctx->builder, break_id, cont_id, SpvLoopControlMaskNone);
   branch(ctx, begin_id);

   SpvId save_break = ctx->loop_break;
   SpvId save_cont = ctx->loop_cont;
   ctx->loop_break = break_id;
   ctx->loop_cont = cont_id;

   emit_cf_list(ctx, &loop->body);

   ctx->loop_break = save_break;
   ctx->loop_cont = save_cont;

   /* loop->body may have already ended our block */
   if (ctx->block_started)
      branch(ctx, cont_id);
   start_block(ctx, cont_id);
   branch(ctx, header_id);

   start_block(ctx, break_id);
}

static void
emit_cf_list(struct ntv_context *ctx, struct exec_list *list)
{
   foreach_list_typed(nir_cf_node, node, node, list) {
      switch (node->type) {
      case nir_cf_node_block:
         emit_block(ctx, nir_cf_node_as_block(node));
         break;

      case nir_cf_node_if:
         emit_if(ctx, nir_cf_node_as_if(node));
         break;

      case nir_cf_node_loop:
         emit_loop(ctx, nir_cf_node_as_loop(node));
         break;

      case nir_cf_node_function:
         UNREACHABLE("nir_cf_node_function not supported");
         break;
      }
   }
}

static SpvExecutionMode
get_input_prim_type_mode(enum mesa_prim type)
{
   switch (type) {
   case MESA_PRIM_POINTS:
      return SpvExecutionModeInputPoints;
   case MESA_PRIM_LINES:
   case MESA_PRIM_LINE_LOOP:
   case MESA_PRIM_LINE_STRIP:
      return SpvExecutionModeInputLines;
   case MESA_PRIM_TRIANGLE_STRIP:
   case MESA_PRIM_TRIANGLES:
   case MESA_PRIM_TRIANGLE_FAN:
      return SpvExecutionModeTriangles;
   case MESA_PRIM_QUADS:
   case MESA_PRIM_QUAD_STRIP:
      return SpvExecutionModeQuads;
      break;
   case MESA_PRIM_POLYGON:
      UNREACHABLE("handle polygons in gs");
      break;
   case MESA_PRIM_LINES_ADJACENCY:
   case MESA_PRIM_LINE_STRIP_ADJACENCY:
      return SpvExecutionModeInputLinesAdjacency;
   case MESA_PRIM_TRIANGLES_ADJACENCY:
   case MESA_PRIM_TRIANGLE_STRIP_ADJACENCY:
      return SpvExecutionModeInputTrianglesAdjacency;
      break;
   default:
      debug_printf("unknown geometry shader input mode %u\n", type);
      UNREACHABLE("error!");
      break;
   }

   return 0;
}
static SpvExecutionMode
get_output_prim_type_mode(enum mesa_prim type)
{
   switch (type) {
   case MESA_PRIM_POINTS:
      return SpvExecutionModeOutputPoints;
   case MESA_PRIM_LINES:
   case MESA_PRIM_LINE_LOOP:
      UNREACHABLE("MESA_PRIM_LINES/LINE_LOOP passed as gs output");
      break;
   case MESA_PRIM_LINE_STRIP:
      return SpvExecutionModeOutputLineStrip;
   case MESA_PRIM_TRIANGLE_STRIP:
      return SpvExecutionModeOutputTriangleStrip;
   case MESA_PRIM_TRIANGLES:
   case MESA_PRIM_TRIANGLE_FAN: //FIXME: not sure if right for output
      return SpvExecutionModeTriangles;
   case MESA_PRIM_QUADS:
   case MESA_PRIM_QUAD_STRIP:
      return SpvExecutionModeQuads;
   case MESA_PRIM_POLYGON:
      UNREACHABLE("handle polygons in gs");
      break;
   case MESA_PRIM_LINES_ADJACENCY:
   case MESA_PRIM_LINE_STRIP_ADJACENCY:
      UNREACHABLE("handle line adjacency in gs");
      break;
   case MESA_PRIM_TRIANGLES_ADJACENCY:
   case MESA_PRIM_TRIANGLE_STRIP_ADJACENCY:
      UNREACHABLE("handle triangle adjacency in gs");
      break;
   default:
      debug_printf("unknown geometry shader output mode %u\n", type);
      UNREACHABLE("error!");
      break;
   }

   return 0;
}

static SpvExecutionMode
get_depth_layout_mode(enum gl_frag_depth_layout depth_layout)
{
   switch (depth_layout) {
   case FRAG_DEPTH_LAYOUT_NONE:
   case FRAG_DEPTH_LAYOUT_ANY:
      return SpvExecutionModeDepthReplacing;
   case FRAG_DEPTH_LAYOUT_GREATER:
      return SpvExecutionModeDepthGreater;
   case FRAG_DEPTH_LAYOUT_LESS:
      return SpvExecutionModeDepthLess;
   case FRAG_DEPTH_LAYOUT_UNCHANGED:
      return SpvExecutionModeDepthUnchanged;
   default:
      UNREACHABLE("unexpected depth layout");
   }
}

static SpvExecutionMode
get_primitive_mode(enum tess_primitive_mode primitive_mode)
{
   switch (primitive_mode) {
   case TESS_PRIMITIVE_TRIANGLES: return SpvExecutionModeTriangles;
   case TESS_PRIMITIVE_QUADS: return SpvExecutionModeQuads;
   case TESS_PRIMITIVE_ISOLINES: return SpvExecutionModeIsolines;
   default:
      UNREACHABLE("unknown tess prim type!");
   }
}

static SpvExecutionMode
get_spacing(enum gl_tess_spacing spacing)
{
   switch (spacing) {
   case TESS_SPACING_EQUAL:
      return SpvExecutionModeSpacingEqual;
   case TESS_SPACING_FRACTIONAL_ODD:
      return SpvExecutionModeSpacingFractionalOdd;
   case TESS_SPACING_FRACTIONAL_EVEN:
      return SpvExecutionModeSpacingFractionalEven;
   default:
      UNREACHABLE("unknown tess spacing!");
   }
}

struct spirv_shader *
nir_to_spirv(struct nir_shader *s, const struct zink_shader_info *sinfo, const struct zink_screen *screen)
{
   const uint32_t spirv_version = screen->spirv_version;
   struct spirv_shader *ret = NULL;

   struct ntv_context ctx = {0};
   ctx.mem_ctx = ralloc_context(NULL);
   ctx.nir = s;
   ctx.builder.mem_ctx = ctx.mem_ctx;
   assert(spirv_version >= SPIRV_VERSION(1, 0));
   ctx.spirv_1_4_interfaces = spirv_version >= SPIRV_VERSION(1, 4);
   ctx.have_spirv16 = spirv_version >= SPIRV_VERSION(1, 6);

   ctx.bindless_set_idx = sinfo->bindless_set_idx;
   ctx.glsl_types[0] = _mesa_pointer_hash_table_create(ctx.mem_ctx);
   ctx.glsl_types[1] = _mesa_pointer_hash_table_create(ctx.mem_ctx);
   ctx.bo_array_types = _mesa_pointer_hash_table_create(ctx.mem_ctx);
   ctx.bo_struct_types = _mesa_pointer_hash_table_create(ctx.mem_ctx);
   if (!ctx.glsl_types[0] || !ctx.glsl_types[1] || !ctx.bo_array_types || !ctx.bo_struct_types ||
       !_mesa_hash_table_init(&ctx.image_types, ctx.mem_ctx, _mesa_hash_pointer, _mesa_key_pointer_equal))
      goto fail;

   spirv_builder_emit_cap(&ctx.builder, SpvCapabilityShader);

   switch (s->info.stage) {
   case MESA_SHADER_FRAGMENT:
      if (s->info.fs.uses_sample_shading)
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilitySampleRateShading);

      if (s->info.fs.uses_discard && screen->info.have_EXT_shader_demote_to_helper_invocation) {
         if (!ctx.have_spirv16)
            spirv_builder_emit_extension(&ctx.builder, "SPV_EXT_demote_to_helper_invocation");
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityDemoteToHelperInvocation);
      }

      if (BITSET_TEST(s->info.system_values_read, SYSTEM_VALUE_HELPER_INVOCATION) &&
          screen->info.have_EXT_shader_demote_to_helper_invocation && !ctx.have_spirv16) {
         spirv_builder_emit_extension(&ctx.builder, "SPV_EXT_demote_to_helper_invocation");
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityDemoteToHelperInvocation);
      }

      break;

   case MESA_SHADER_VERTEX:
      if (BITSET_TEST(s->info.system_values_read, SYSTEM_VALUE_INSTANCE_ID) ||
          BITSET_TEST(s->info.system_values_read, SYSTEM_VALUE_DRAW_ID) ||
          BITSET_TEST(s->info.system_values_read, SYSTEM_VALUE_BASE_INSTANCE) ||
          BITSET_TEST(s->info.system_values_read, SYSTEM_VALUE_BASE_VERTEX)) {
         if (spirv_version < SPIRV_VERSION(1, 3))
            spirv_builder_emit_extension(&ctx.builder, "SPV_KHR_shader_draw_parameters");
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityDrawParameters);
      }
      break;

   case MESA_SHADER_TESS_CTRL:
   case MESA_SHADER_TESS_EVAL:
      spirv_builder_emit_cap(&ctx.builder, SpvCapabilityTessellation);
      /* TODO: check features for this */
      if (s->info.outputs_written & VARYING_BIT_PSIZ)
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityTessellationPointSize);
      break;

   case MESA_SHADER_GEOMETRY:
      spirv_builder_emit_cap(&ctx.builder, SpvCapabilityGeometry);
      if (s->info.outputs_written & VARYING_BIT_PSIZ)
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityGeometryPointSize);
      break;

   default: ;
   }

   if (s->info.stage < MESA_SHADER_GEOMETRY) {
      if (s->info.outputs_written & VARYING_BIT_LAYER ||
          s->info.inputs_read & VARYING_BIT_LAYER) {
         if (spirv_version >= SPIRV_VERSION(1, 5))
            spirv_builder_emit_cap(&ctx.builder, SpvCapabilityShaderLayer);
         else {
            spirv_builder_emit_extension(&ctx.builder, "SPV_EXT_shader_viewport_index_layer");
            spirv_builder_emit_cap(&ctx.builder, SpvCapabilityShaderViewportIndexLayerEXT);
         }
      }
   } else if (s->info.stage == MESA_SHADER_FRAGMENT) {
      /* incredibly, this is legal and intended.
       * https://github.com/KhronosGroup/SPIRV-Registry/issues/95
       */
      if (s->info.inputs_read & (VARYING_BIT_LAYER |
                                 VARYING_BIT_PRIMITIVE_ID))
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityGeometry);
   }

   if (s->info.num_ssbos && spirv_version < SPIRV_VERSION(1, 1))
      spirv_builder_emit_extension(&ctx.builder, "SPV_KHR_storage_buffer_storage_class");

   if (s->info.stage < MESA_SHADER_FRAGMENT &&
       s->info.outputs_written & VARYING_BIT_VIEWPORT) {
      if (s->info.stage < MESA_SHADER_GEOMETRY)
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityShaderViewportIndex);
      else
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityMultiViewport);
   }

   if (s->info.stage > MESA_SHADER_VERTEX &&
       s->info.inputs_read & VARYING_BIT_VIEWPORT) {
      if (s->info.stage < MESA_SHADER_GEOMETRY)
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityShaderViewportIndex);
      else
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityMultiViewport);
   }

   ctx.stage = s->info.stage;
   ctx.sinfo = sinfo;
   ctx.GLSL_std_450 = spirv_builder_import(&ctx.builder, "GLSL.std.450");
   ctx.explicit_lod = true;
   spirv_builder_emit_source(&ctx.builder, SpvSourceLanguageUnknown, 0);

   SpvAddressingModel model = SpvAddressingModelLogical;
   if (gl_shader_stage_is_compute(s->info.stage)) {
      if (s->info.cs.ptr_size == 32)
         model = SpvAddressingModelPhysical32;
      else if (s->info.cs.ptr_size == 64) {
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityPhysicalStorageBufferAddresses);
         model = SpvAddressingModelPhysicalStorageBuffer64;
      } else
         model = SpvAddressingModelLogical;
   }

   if (ctx.sinfo->have_vulkan_memory_model) {
      spirv_builder_emit_cap(&ctx.builder, SpvCapabilityVulkanMemoryModel);
      spirv_builder_emit_cap(&ctx.builder, SpvCapabilityVulkanMemoryModelDeviceScope);
      spirv_builder_emit_mem_model(&ctx.builder, model,
                                   SpvMemoryModelVulkan);
   } else {
      spirv_builder_emit_mem_model(&ctx.builder, model,
                                   SpvMemoryModelGLSL450);
   }

   if (s->info.stage == MESA_SHADER_FRAGMENT &&
       s->info.outputs_written & BITFIELD64_BIT(FRAG_RESULT_STENCIL)) {
      spirv_builder_emit_extension(&ctx.builder, "SPV_EXT_shader_stencil_export");
      spirv_builder_emit_cap(&ctx.builder, SpvCapabilityStencilExportEXT);
   }

   SpvExecutionModel exec_model;
   switch (s->info.stage) {
   case MESA_SHADER_VERTEX:
      exec_model = SpvExecutionModelVertex;
      break;
   case MESA_SHADER_TESS_CTRL:
      exec_model = SpvExecutionModelTessellationControl;
      break;
   case MESA_SHADER_TESS_EVAL:
      exec_model = SpvExecutionModelTessellationEvaluation;
      break;
   case MESA_SHADER_GEOMETRY:
      exec_model = SpvExecutionModelGeometry;
      break;
   case MESA_SHADER_FRAGMENT:
      exec_model = SpvExecutionModelFragment;
      break;
   case MESA_SHADER_COMPUTE:
   case MESA_SHADER_KERNEL:
      exec_model = SpvExecutionModelGLCompute;
      break;
   default:
      UNREACHABLE("invalid stage");
   }

   SpvId type_void = spirv_builder_type_void(&ctx.builder);
   SpvId type_void_func = spirv_builder_type_function(&ctx.builder, type_void,
                                                      NULL, 0);
   SpvId entry_point = spirv_builder_new_id(&ctx.builder);
   spirv_builder_emit_name(&ctx.builder, entry_point, "main");

   ctx.vars = _mesa_hash_table_create(ctx.mem_ctx, _mesa_hash_pointer,
                                      _mesa_key_pointer_equal);

   nir_foreach_variable_with_modes(var, s, nir_var_mem_push_const)
      input_var_init(&ctx, var);

   nir_foreach_shader_in_variable(var, s)
      emit_input(&ctx, var);

   int max_output = 0;
   nir_foreach_shader_out_variable(var, s) {
      /* ignore SPIR-V built-ins, tagged with a sentinel value */
      if (var->data.driver_location != UINT_MAX) {
         assert(var->data.driver_location < INT_MAX);
         unsigned extent = glsl_count_attribute_slots(var->type, false);
         max_output = MAX2(max_output, (int)var->data.driver_location + extent);
      }
      emit_output(&ctx, var);
   }

   uint32_t tcs_vertices_out_word = 0;

   unsigned ubo_counter[2] = {0};
   nir_foreach_variable_with_modes(var, s, nir_var_mem_ubo)
      ubo_counter[var->data.driver_location != 0]++;
   nir_foreach_variable_with_modes(var, s, nir_var_mem_ubo)
      emit_bo(&ctx, var, ubo_counter[var->data.driver_location != 0] > 1);

   unsigned ssbo_counter = 0;
   nir_foreach_variable_with_modes(var, s, nir_var_mem_ssbo)
      ssbo_counter++;
   nir_foreach_variable_with_modes(var, s, nir_var_mem_ssbo)
      emit_bo(&ctx, var, ssbo_counter > 1);

   nir_foreach_variable_with_modes(var, s, nir_var_image)
      ctx.image_var[var->data.driver_location] = var;
   nir_foreach_variable_with_modes(var, s, nir_var_uniform) {
      if (glsl_type_is_sampler(glsl_without_array(var->type))) {
         if (var->data.descriptor_set == ctx.bindless_set_idx)
            ctx.bindless_sampler_var[var->data.driver_location] = var;
         else
            ctx.sampler_var[var->data.driver_location] = var;
         ctx.last_sampler = MAX2(ctx.last_sampler, var->data.driver_location);
      }
   }
   nir_foreach_variable_with_modes(var, s, nir_var_image | nir_var_uniform) {
      const struct glsl_type *type = glsl_without_array(var->type);
      if (glsl_type_is_bare_sampler(type))
         emit_sampler(&ctx, var);
      else if (glsl_type_is_sampler(type))
         emit_image(&ctx, var, get_bare_image_type(&ctx, var, true));
      else if (glsl_type_is_image(type))
         emit_image(&ctx, var, get_bare_image_type(&ctx, var, false));
   }

   if (sinfo->float_controls.flush_denorms) {
      unsigned execution_mode = s->info.float_controls_execution_mode;
      bool flush_16_bit = nir_is_denorm_flush_to_zero(execution_mode, 16);
      bool flush_32_bit = nir_is_denorm_flush_to_zero(execution_mode, 32);
      bool flush_64_bit = nir_is_denorm_flush_to_zero(execution_mode, 64);
      bool preserve_16_bit = nir_is_denorm_preserve(execution_mode, 16);
      bool preserve_32_bit = nir_is_denorm_preserve(execution_mode, 32);
      bool preserve_64_bit = nir_is_denorm_preserve(execution_mode, 64);
      bool emit_cap_flush = false;
      bool emit_cap_preserve = false;

      if (!sinfo->float_controls.denorms_all_independence) {
         bool flush = flush_16_bit && flush_64_bit;
         bool preserve = preserve_16_bit && preserve_64_bit;

         if (!sinfo->float_controls.denorms_32_bit_independence) {
            flush = flush && flush_32_bit;
            preserve = preserve && preserve_32_bit;

            flush_32_bit = flush;
            preserve_32_bit = preserve;
         }

         flush_16_bit = flush;
         flush_64_bit = flush;
         preserve_16_bit = preserve;
         preserve_64_bit = preserve;
      }

      if (flush_16_bit && sinfo->float_controls.flush_denorms & BITFIELD_BIT(0)) {
         emit_cap_flush = true;
         spirv_builder_emit_exec_mode_literal(&ctx.builder, entry_point,
                                              SpvExecutionModeDenormFlushToZero, 16);
      }
      if (flush_32_bit && sinfo->float_controls.flush_denorms & BITFIELD_BIT(1)) {
         emit_cap_flush = true;
         spirv_builder_emit_exec_mode_literal(&ctx.builder, entry_point,
                                              SpvExecutionModeDenormFlushToZero, 32);
      }
      if (flush_64_bit && sinfo->float_controls.flush_denorms & BITFIELD_BIT(2)) {
         emit_cap_flush = true;
         spirv_builder_emit_exec_mode_literal(&ctx.builder, entry_point,
                                              SpvExecutionModeDenormFlushToZero, 64);
      }

      if (preserve_16_bit && sinfo->float_controls.preserve_denorms & BITFIELD_BIT(0)) {
         emit_cap_preserve = true;
         spirv_builder_emit_exec_mode_literal(&ctx.builder, entry_point,
                                              SpvExecutionModeDenormPreserve, 16);
      }
      if (preserve_32_bit && sinfo->float_controls.preserve_denorms & BITFIELD_BIT(1)) {
         emit_cap_preserve = true;
         spirv_builder_emit_exec_mode_literal(&ctx.builder, entry_point,
                                              SpvExecutionModeDenormPreserve, 32);
      }
      if (preserve_64_bit && sinfo->float_controls.preserve_denorms & BITFIELD_BIT(2)) {
         emit_cap_preserve = true;
         spirv_builder_emit_exec_mode_literal(&ctx.builder, entry_point,
                                              SpvExecutionModeDenormPreserve, 64);
      }

      if (emit_cap_flush)
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityDenormFlushToZero);
      if (emit_cap_preserve)
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityDenormPreserve);
   }

   switch (s->info.stage) {
   case MESA_SHADER_FRAGMENT:
      spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                   SpvExecutionModeOriginUpperLeft);
      if (s->info.outputs_written & BITFIELD64_BIT(FRAG_RESULT_DEPTH))
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                      get_depth_layout_mode(s->info.fs.depth_layout));
      if (s->info.outputs_written & BITFIELD64_BIT(FRAG_RESULT_STENCIL))
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                      SpvExecutionModeStencilRefReplacingEXT);
      if (s->info.fs.early_fragment_tests)
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                      SpvExecutionModeEarlyFragmentTests);
      if (s->info.fs.post_depth_coverage) {
         spirv_builder_emit_extension(&ctx.builder, "SPV_KHR_post_depth_coverage");
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilitySampleMaskPostDepthCoverage);
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                      SpvExecutionModePostDepthCoverage);
      }

      if (s->info.fs.pixel_interlock_ordered || s->info.fs.pixel_interlock_unordered ||
          s->info.fs.sample_interlock_ordered || s->info.fs.sample_interlock_unordered)
         spirv_builder_emit_extension(&ctx.builder, "SPV_EXT_fragment_shader_interlock");
      if (s->info.fs.pixel_interlock_ordered || s->info.fs.pixel_interlock_unordered)
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityFragmentShaderPixelInterlockEXT);
      if (s->info.fs.sample_interlock_ordered || s->info.fs.sample_interlock_unordered)
         spirv_builder_emit_cap(&ctx.builder, SpvCapabilityFragmentShaderSampleInterlockEXT);
      if (s->info.fs.pixel_interlock_ordered)
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point, SpvExecutionModePixelInterlockOrderedEXT);
      if (s->info.fs.pixel_interlock_unordered)
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point, SpvExecutionModePixelInterlockUnorderedEXT);
      if (s->info.fs.sample_interlock_ordered)
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point, SpvExecutionModeSampleInterlockOrderedEXT);
      if (s->info.fs.sample_interlock_unordered)
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point, SpvExecutionModeSampleInterlockUnorderedEXT);
      break;
   case MESA_SHADER_TESS_CTRL:
      tcs_vertices_out_word = spirv_builder_emit_exec_mode_literal(&ctx.builder, entry_point,
                                                                   SpvExecutionModeOutputVertices,
                                                                   s->info.tess.tcs_vertices_out);
      break;
   case MESA_SHADER_TESS_EVAL:
      spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                   get_primitive_mode(s->info.tess._primitive_mode));
      spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                   s->info.tess.ccw ? SpvExecutionModeVertexOrderCcw
                                                    : SpvExecutionModeVertexOrderCw);
      spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                   get_spacing(s->info.tess.spacing));
      if (s->info.tess.point_mode)
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point, SpvExecutionModePointMode);
      break;
   case MESA_SHADER_GEOMETRY:
      spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                   get_input_prim_type_mode(s->info.gs.input_primitive));
      spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                   get_output_prim_type_mode(s->info.gs.output_primitive));
      spirv_builder_emit_exec_mode_literal(&ctx.builder, entry_point,
                                           SpvExecutionModeInvocations,
                                           s->info.gs.invocations);
      spirv_builder_emit_exec_mode_literal(&ctx.builder, entry_point,
                                           SpvExecutionModeOutputVertices,
                                           MAX2(s->info.gs.vertices_out, 1));
      break;
   case MESA_SHADER_KERNEL:
   case MESA_SHADER_COMPUTE:
      if (s->info.workgroup_size[0] || s->info.workgroup_size[1] || s->info.workgroup_size[2])
         spirv_builder_emit_exec_mode_literal3(&ctx.builder, entry_point, SpvExecutionModeLocalSize,
                                               (uint32_t[3]){(uint32_t)s->info.workgroup_size[0], (uint32_t)s->info.workgroup_size[1],
                                               (uint32_t)s->info.workgroup_size[2]});
      else {
         SpvId sizes[3];
         uint32_t ids[] = {ZINK_WORKGROUP_SIZE_X, ZINK_WORKGROUP_SIZE_Y, ZINK_WORKGROUP_SIZE_Z};
         const char *names[] = {"x", "y", "z"};
         for (int i = 0; i < 3; i ++) {
            sizes[i] = spirv_builder_spec_const_uint(&ctx.builder, 32);
            spirv_builder_emit_specid(&ctx.builder, sizes[i], ids[i]);
            spirv_builder_emit_name(&ctx.builder, sizes[i], names[i]);
         }
         SpvId var_type = get_uvec_type(&ctx, 32, 3);
         // Even when using LocalSizeId this need to be initialized for nir_intrinsic_load_workgroup_size
         ctx.local_group_size_var = spirv_builder_spec_const_composite(&ctx.builder, var_type, sizes, 3);
         spirv_builder_emit_name(&ctx.builder, ctx.local_group_size_var, "gl_LocalGroupSizeARB");

         /* WorkgroupSize is deprecated in SPIR-V 1.6 */
         if (spirv_version >= SPIRV_VERSION(1, 6)) {
            spirv_builder_emit_exec_mode_id3(&ctx.builder, entry_point,
                                                  SpvExecutionModeLocalSizeId,
                                                  sizes);
         } else {
            spirv_builder_emit_builtin(&ctx.builder, ctx.local_group_size_var, SpvBuiltInWorkgroupSize);
         }
      }
      if (s->info.cs.has_variable_shared_mem) {
         ctx.shared_mem_size = spirv_builder_spec_const_uint(&ctx.builder, 32);
         spirv_builder_emit_specid(&ctx.builder, ctx.shared_mem_size, ZINK_VARIABLE_SHARED_MEM);
         spirv_builder_emit_name(&ctx.builder, ctx.shared_mem_size, "variable_shared_mem");
      }
      if (s->info.derivative_group) {
         SpvCapability caps[] = { 0, SpvCapabilityComputeDerivativeGroupQuadsNV, SpvCapabilityComputeDerivativeGroupLinearNV };
         SpvExecutionMode modes[] = { 0, SpvExecutionModeDerivativeGroupQuadsNV, SpvExecutionModeDerivativeGroupLinearNV };
         spirv_builder_emit_extension(&ctx.builder, "SPV_NV_compute_shader_derivatives");
         spirv_builder_emit_cap(&ctx.builder, caps[s->info.derivative_group]);
         spirv_builder_emit_exec_mode(&ctx.builder, entry_point, modes[s->info.derivative_group]);
         ctx.explicit_lod = false;
      }
      break;
   default:
      break;
   }
   if (BITSET_TEST_RANGE(s->info.system_values_read, SYSTEM_VALUE_SUBGROUP_SIZE, SYSTEM_VALUE_SUBGROUP_LT_MASK)) {
      spirv_builder_emit_cap(&ctx.builder, SpvCapabilitySubgroupBallotKHR);
      spirv_builder_emit_extension(&ctx.builder, "SPV_KHR_shader_ballot");
   }
   if (s->info.has_transform_feedback_varyings && s->info.stage != MESA_SHADER_FRAGMENT) {
      spirv_builder_emit_cap(&ctx.builder, SpvCapabilityTransformFeedback);
      spirv_builder_emit_exec_mode(&ctx.builder, entry_point,
                                   SpvExecutionModeXfb);
   }

   if (s->info.stage == MESA_SHADER_FRAGMENT && s->info.fs.uses_discard) {
      ctx.discard_func = spirv_builder_new_id(&ctx.builder);
      spirv_builder_emit_name(&ctx.builder, ctx.discard_func, "discard");
      spirv_builder_function(&ctx.builder, ctx.discard_func, type_void,
                             SpvFunctionControlMaskNone,
                             type_void_func);
      SpvId label = spirv_builder_new_id(&ctx.builder);
      spirv_builder_label(&ctx.builder, label);

      /* kill is deprecated in SPIR-V 1.6, use terminate instead */
      if (spirv_version >= SPIRV_VERSION(1, 6))
         spirv_builder_emit_terminate(&ctx.builder);
      else
         spirv_builder_emit_kill(&ctx.builder);

      spirv_builder_function_end(&ctx.builder);
   }

   spirv_builder_function(&ctx.builder, entry_point, type_void,
                          SpvFunctionControlMaskNone,
                          type_void_func);

   nir_function_impl *entry = nir_shader_get_entrypoint(s);
   nir_metadata_require(entry, nir_metadata_block_index);

   ctx.defs = rzalloc_array_size(ctx.mem_ctx,
                                 sizeof(SpvId), entry->ssa_alloc);
   ctx.def_types = ralloc_array_size(ctx.mem_ctx,
                                     sizeof(nir_alu_type), entry->ssa_alloc);
   if (!ctx.defs || !ctx.def_types)
      goto fail;
   if (sinfo->have_sparse) {
      spirv_builder_emit_cap(&ctx.builder, SpvCapabilitySparseResidency);
      /* this could be huge, so only alloc if needed since it's extremely unlikely to
       * ever be used by anything except cts
       */
      ctx.resident_defs = rzalloc_array_size(ctx.mem_ctx,
                                            sizeof(SpvId), entry->ssa_alloc);
      if (!ctx.resident_defs)
         goto fail;
   }
   ctx.num_defs = entry->ssa_alloc;

   SpvId *block_ids = ralloc_array_size(ctx.mem_ctx,
                                        sizeof(SpvId), entry->num_blocks);
   if (!block_ids)
      goto fail;

   for (int i = 0; i < entry->num_blocks; ++i)
      block_ids[i] = spirv_builder_new_id(&ctx.builder);

   ctx.block_ids = block_ids;
   ctx.num_blocks = entry->num_blocks;

   /* emit a block only for the variable declarations */
   start_block(&ctx, spirv_builder_new_id(&ctx.builder));
   spirv_builder_begin_local_vars(&ctx.builder);

   nir_foreach_reg_decl(reg, entry) {
      if (nir_intrinsic_bit_size(reg) == 1)
         init_reg(&ctx, reg, nir_type_bool);
   }

   nir_foreach_variable_with_modes(var, s, nir_var_shader_temp)
      emit_shader_temp(&ctx, var);

   nir_foreach_function_temp_variable(var, entry)
      emit_temp(&ctx, var);


   emit_cf_list(&ctx, &entry->body);

   spirv_builder_return(&ctx.builder); // doesn't belong here, but whatevz
   spirv_builder_function_end(&ctx.builder);

   spirv_builder_emit_entry_point(&ctx.builder, exec_model, entry_point,
                                  "main", ctx.entry_ifaces,
                                  ctx.num_entry_ifaces);

   if (ctx.num_subgroups_var)
      spirv_builder_emit_cap(&ctx.builder, SpvCapabilityGroupNonUniform);

   size_t num_words = spirv_builder_get_num_words(&ctx.builder);

   ret = ralloc(NULL, struct spirv_shader);
   if (!ret)
      goto fail;

   ret->words = ralloc_size(ret, sizeof(uint32_t) * num_words);
   if (!ret->words)
      goto fail;

   ret->num_words = spirv_builder_get_words(&ctx.builder, ret->words, num_words, spirv_version, &tcs_vertices_out_word);
   ret->tcs_vertices_out_word = tcs_vertices_out_word;
   assert(ret->num_words == num_words);

   ralloc_free(ctx.mem_ctx);

   return ret;

fail:
   ralloc_free(ctx.mem_ctx);

   if (ret)
      spirv_shader_delete(ret);

   return NULL;
}

void
spirv_shader_delete(struct spirv_shader *s)
{
   ralloc_free(s);
}

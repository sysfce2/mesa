/*
 * Copyright © 2023 Valve Corporation
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
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "lp_context.h"
#include "lp_texture_handle.h"
#include "lp_screen.h"

#include "gallivm/lp_bld_const.h"
#include "gallivm/lp_bld_debug.h"
#include "gallivm/lp_bld_nir.h"

#include "nir.h"
#include "nir_builder.h"

#include "pipe/p_context.h"
#include "pipe/p_screen.h"
#include "util/mesa-sha1.h"

static const char *image_function_base_hash = "8ca89d7a4ab5830be6a1ba1140844081235b01164a8fce8316ca6a2f81f1a899";
static const char *sample_function_base_hash = "0789b032c4a1ddba086e07496fe2a992b1ee08f78c0884a2923564b1ed52b9cc";
static const char *size_function_base_hash = "6d249ab9c1106c68b87ec9fdb5ade28368171d27f221c687f32ae1544231d2fe";
static const char *jit_sample_function_base_hash = "21de75bb5dbcfea1f90d03b8b688f19bdb0d96f95681cbe8b26853e1723846e4";
static const char *jit_fetch_function_base_hash = "8cc6d433304c6e2f24581f47121678bbe90d170aff4a206ce122d677639254d3";
static const char *jit_size_function_base_hash = "ecf7edd7cc56cad4a6f0a4622bce3794b7ea2883273a5482727ab62549400155";

static void
llvmpipe_register_texture(struct llvmpipe_context *ctx, struct lp_texture_handle_state *state, bool sampled);

static void
llvmpipe_register_sampler(struct llvmpipe_context *ctx, struct lp_static_sampler_state *state);

static uint64_t
llvmpipe_create_texture_handle(struct pipe_context *pctx, struct pipe_sampler_view *view, const struct pipe_sampler_state *sampler)
{
   struct llvmpipe_context *ctx = llvmpipe_context(pctx);
   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;

   struct lp_texture_handle *handle = calloc(1, sizeof(struct lp_texture_handle));

   if (view) {
      struct lp_texture_handle_state state;
      memset(&state, 0, sizeof(state));
      lp_sampler_static_texture_state(&state.static_state, view);
      if (view->texture)
         lp_jit_texture_from_pipe(&state.dynamic_state, view);
      else
         assert(view->format == PIPE_FORMAT_NONE);

      state.dynamic_state.base = NULL;
      if (state.static_state.tiled)
         state.dynamic_state.residency = NULL;

      llvmpipe_register_texture(ctx, &state, true);

      bool found = false;
      for (uint32_t i = 0; i < matrix->texture_count; i++) {
         if (!memcmp(&matrix->textures[i]->state, &state, sizeof(struct lp_texture_handle_state))) {
            handle->functions = matrix->textures[i];
            found = true;
            break;
         }
      }
      assert(found);
   }

   if (sampler) {
      struct lp_static_sampler_state state;
      lp_sampler_static_sampler_state(&state, sampler);

      llvmpipe_register_sampler(ctx, &state);

      bool found = false;
      for (uint32_t i = 0; i < matrix->sampler_count; i++) {
         if (!memcmp(matrix->samplers + i, &state, sizeof(struct lp_static_sampler_state))) {
            handle->sampler_index = i;
            found = true;
            break;
         }
      }
      assert(found);
   }

   return (uint64_t)(uintptr_t)handle;
}

static void
llvmpipe_delete_texture_handle(struct pipe_context *pctx, uint64_t handle)
{
   free((void *)(uintptr_t)handle);
}

static uint64_t
llvmpipe_create_image_handle(struct pipe_context *pctx, const struct pipe_image_view *view)
{
   struct llvmpipe_context *ctx = llvmpipe_context(pctx);
   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;

   struct lp_texture_handle *handle = calloc(1, sizeof(struct lp_texture_handle));

   struct lp_texture_handle_state state;
   memset(&state, 0, sizeof(state));
   lp_sampler_static_texture_state_image(&state.static_state, view);

   /* Trade a bit of performance for potentially less sampler/texture combinations. */
   state.static_state.pot_width = false;
   state.static_state.pot_height = false;
   state.static_state.pot_depth = false;

   /*
    * Rely on single_layer_view to distinguish array and non-array targets,
    * since there's no target field in pipe_image_view, and there can
    * be mismatch between resource and view.
    * We cannot unconditionally demote array to non-array targets if there's
    * only one layer, since we'd then ignore the layer coord completely and
    * OOB behavior would be wrong.
    *
    * XXX shouldn't we do this in lp_sampler_static_texture_state_image()
    * in the first place (there's more callers)?
    */
   if (view->resource && llvmpipe_resource_is_texture(view->resource) &&
       view->u.tex.single_layer_view &&
       view->u.tex.first_layer == view->u.tex.last_layer) {
      if (state.static_state.target == PIPE_TEXTURE_1D_ARRAY)
         state.static_state.target = PIPE_TEXTURE_1D;
      else if (state.static_state.target == PIPE_TEXTURE_2D_ARRAY ||
               (state.static_state.target == PIPE_TEXTURE_3D && !state.static_state.tiled))
         state.static_state.target = PIPE_TEXTURE_2D;
      else if (state.static_state.target == PIPE_TEXTURE_CUBE_ARRAY)
         state.static_state.target = PIPE_TEXTURE_CUBE;
   }

   llvmpipe_register_texture(ctx, &state, false);

   bool found = false;
   for (uint32_t i = 0; i < matrix->texture_count; i++) {
      if (!memcmp(&matrix->textures[i]->state, &state, sizeof(struct lp_texture_handle_state))) {
         handle->functions = matrix->textures[i];
         found = true;
         break;
      }
   }
   assert(found);

   return (uint64_t)(uintptr_t)handle;
}

static void
llvmpipe_delete_image_handle(struct pipe_context *pctx, uint64_t handle)
{
   free((void *)(uintptr_t)handle);
}

static uint64_t
get_sample_function(uint64_t _matrix, uint64_t _texture_functions, uint64_t _sampler_desc, uint32_t sample_key);

static uint64_t
get_fetch_function(uint64_t _matrix, uint64_t _texture_functions, uint32_t sample_key);

static uint64_t
get_size_function(uint64_t _matrix, uint64_t _texture_functions, uint32_t samples);

static void *
compile_jit_size_function(struct llvmpipe_context *ctx, bool samples);

struct sample_function_cache_key {
   struct lp_texture_functions *texture_functions;
   uint32_t sampler_index;
   uint32_t sample_key;
};

DERIVE_HASH_TABLE(sample_function_cache_key)

struct size_function_cache_key {
   struct lp_texture_functions *texture_functions;
   uint32_t samples;
   uint32_t padding;
};

DERIVE_HASH_TABLE(size_function_cache_key)

static struct hash_table *
acquire_latest_function_cache(struct lp_function_cache *cache)
{
   uint64_t value = p_atomic_read(&cache->latest_cache.value);
   return (struct hash_table *)(uintptr_t)value;
}

static void
replace_function_cache_locked(struct lp_function_cache *cache, struct hash_table *new_cache)
{
   uint64_t old_value = p_atomic_xchg(&cache->latest_cache.value, (uint64_t)(uintptr_t)new_cache);
   /* Like RCU pointers, defer cleanup of old values until we know no readers are left. */
   struct hash_table *old_cache = (struct hash_table *)(uintptr_t)old_value;
   util_dynarray_append(&cache->trash_caches, struct hash_table *, old_cache);
}

static void
lp_function_cache_init(struct lp_function_cache *cache, struct hash_table *initial_cache)
{
   p_atomic_set(&cache->latest_cache.value, (uint64_t)(uintptr_t)initial_cache);
   util_dynarray_init(&cache->trash_caches, NULL);
}

void
llvmpipe_init_sampler_matrix(struct llvmpipe_context *ctx)
{
   ctx->pipe.create_texture_handle = llvmpipe_create_texture_handle;
   ctx->pipe.delete_texture_handle = llvmpipe_delete_texture_handle;
   ctx->pipe.create_image_handle = llvmpipe_create_image_handle;
   ctx->pipe.delete_image_handle = llvmpipe_delete_image_handle;

   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;

   util_dynarray_init(&matrix->gallivms, NULL);

   matrix->ctx = ctx;

   matrix->compile_sample_function = get_sample_function;
   matrix->compile_fetch_function = get_fetch_function;
   matrix->compile_size_function = get_size_function;

   lp_function_cache_init(&matrix->caches[LP_FUNCTION_CACHE_SAMPLE], sample_function_cache_key_table_create(NULL));
   lp_function_cache_init(&matrix->caches[LP_FUNCTION_CACHE_FETCH], sample_function_cache_key_table_create(NULL));
   lp_function_cache_init(&matrix->caches[LP_FUNCTION_CACHE_SIZE], size_function_cache_key_table_create(NULL));

   simple_mtx_init(&matrix->lock, mtx_plain);

   matrix->jit_size_functions[0] = compile_jit_size_function(ctx, false);
   matrix->jit_size_functions[1] = compile_jit_size_function(ctx, true);
}

void
llvmpipe_sampler_matrix_destroy(struct llvmpipe_context *ctx)
{
   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;

   simple_mtx_destroy(&matrix->lock);

   for (uint32_t i = 0; i < ARRAY_SIZE(matrix->caches); i++) {
      _mesa_hash_table_destroy(acquire_latest_function_cache(&matrix->caches[i]), NULL);
      util_dynarray_foreach (&matrix->caches[i].trash_caches, struct hash_table *, trash)
         _mesa_hash_table_destroy(*trash, NULL);
      util_dynarray_fini(&matrix->caches[i].trash_caches);
   }

   free(matrix->samplers);

   for (uint32_t texture_index = 0; texture_index < matrix->texture_count; texture_index++) {
      struct lp_texture_functions *texture = matrix->textures[texture_index];

      uint32_t sampler_count = texture->sampler_count;
      if (texture->state.static_state.format == PIPE_FORMAT_NONE)
         sampler_count = MIN2(sampler_count, 1);

      for (uint32_t sampler_index = 0; sampler_index < sampler_count; sampler_index++) {
         if (texture->sample_functions[sampler_index] != matrix->jit_sample_functions)
            free(texture->sample_functions[sampler_index]);
      }
      free(texture->sample_functions);

      if (texture->fetch_functions != matrix->jit_fetch_functions)
         free(texture->fetch_functions);

      free(texture->image_functions);
      free(texture);
   }
   free(matrix->textures);

   util_dynarray_foreach (&matrix->gallivms, struct gallivm_state *, gallivm)
      gallivm_destroy(*gallivm);

   util_dynarray_fini(&matrix->gallivms);

   if (matrix->context.ref)
      lp_context_destroy(&matrix->context);
}

static lp_context_ref *
get_llvm_context(struct llvmpipe_context *ctx)
{
   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;

   if (!matrix->context.ref)
      lp_context_create(&matrix->context);

   return &matrix->context;
}

static void *
compile_function(struct llvmpipe_context *ctx, struct gallivm_state *gallivm, LLVMValueRef function,
                 const char *func_name,
                 bool needs_caching,
                 uint8_t cache_key[SHA1_DIGEST_LENGTH])
{
   gallivm_verify_function(gallivm, function);
   gallivm_compile_module(gallivm);

   void *function_ptr = func_to_pointer(gallivm_jit_function(gallivm, function, func_name));

   if (needs_caching)
      lp_disk_cache_insert_shader(llvmpipe_screen(ctx->pipe.screen), gallivm->cache, cache_key);

   gallivm_free_ir(gallivm);

   util_dynarray_append(&ctx->sampler_matrix.gallivms, struct gallivm_state *, gallivm);

   return function_ptr;
}

static void *
compile_image_function(struct llvmpipe_context *ctx, struct lp_static_texture_state *texture, uint32_t op)
{
   const struct util_format_description *desc = util_format_description(texture->format);
   if (desc->colorspace != UTIL_FORMAT_COLORSPACE_ZS && !lp_storage_render_image_format_supported(texture->format))
      return NULL;

   uint32_t flags = op / LP_IMAGE_OP_COUNT;
   bool ms = flags & LP_IMAGE_OP_MS;
   bool is64 = flags & LP_IMAGE_OP_64;

   struct lp_img_params params = { 0 };

   params.instr_has_layer_coord = flags & LP_IMAGE_OP_HAS_LAYER;

   params.img_op = op % LP_IMAGE_OP_COUNT;
   if (params.img_op >= LP_IMG_OP_COUNT - 1) {
      params.op = params.img_op - (LP_IMG_OP_COUNT - 1);
      params.img_op = LP_IMG_ATOMIC;
   } else if (params.img_op != LP_IMG_LOAD && params.img_op != LP_IMG_LOAD_SPARSE && params.img_op != LP_IMG_STORE) {
      params.img_op = LP_IMG_ATOMIC_CAS;
   }

   struct lp_static_texture_state local_texture = *texture;
   /* Rewrite rg32 stype formats as r64 for 64-bit atomics. */
   if (params.img_op == LP_IMG_ATOMIC && is64) {
      switch (local_texture.res_format) {
      case PIPE_FORMAT_R32G32_SINT:
         local_texture.format = PIPE_FORMAT_R64_SINT;
         local_texture.res_format = PIPE_FORMAT_R64_SINT;
         break;
      case PIPE_FORMAT_R32G32_UINT:
         local_texture.format = PIPE_FORMAT_R64_UINT;
         local_texture.res_format = PIPE_FORMAT_R64_UINT;
         break;
      default:
         /* Nothing to do. */
         break;
      }
   }

   /* Loads need to support a wider range of formats for input attachments. */
   if (params.img_op != LP_IMG_LOAD)
      if (local_texture.format != PIPE_FORMAT_NONE && !lp_storage_image_format_supported(local_texture.format))
         return NULL;

   uint8_t cache_key[SHA1_DIGEST_LENGTH];
   struct mesa_sha1 hash_ctx;
   _mesa_sha1_init(&hash_ctx);
   _mesa_sha1_update(&hash_ctx, image_function_base_hash, strlen(image_function_base_hash));
   _mesa_sha1_update(&hash_ctx, &local_texture, sizeof(local_texture));
   _mesa_sha1_update(&hash_ctx, &op, sizeof(op));
   _mesa_sha1_update(&hash_ctx, &ms, sizeof(ms));
   _mesa_sha1_final(&hash_ctx, cache_key);

   struct lp_cached_code cached = { 0 };
   lp_disk_cache_find_shader(llvmpipe_screen(ctx->pipe.screen), &cached, cache_key);
   bool needs_caching = !cached.data_size;

   struct gallivm_state *gallivm = gallivm_create("image_function", get_llvm_context(ctx), &cached);

   struct lp_image_static_state state = {
      .image_state = local_texture,
   };
   struct lp_build_image_soa *image_soa = lp_bld_llvm_image_soa_create(&state, 1);

   struct lp_type type;
   memset(&type, 0, sizeof type);
   type.floating = true;      /* floating point values */
   type.sign = true;          /* values are signed */
   type.norm = false;         /* values are not limited to [0,1] or [-1,1] */
   type.width = 32;           /* 32-bit float */
   type.length = MIN2(lp_native_vector_width / 32, 16); /* n*4 elements per vector */

   struct lp_compute_shader_variant cs = { .gallivm = gallivm };
   lp_jit_init_cs_types(&cs);

   params.type = type;
   params.target = local_texture.target;
   params.resources_type = cs.jit_resources_type;
   params.format = local_texture.format;

   LLVMTypeRef function_type = lp_build_image_function_type(gallivm, &params, ms, is64);
   if (!function_type) {
      free(image_soa);
      gallivm_destroy(gallivm);
      return NULL;
   }

   LLVMValueRef function = LLVMAddFunction(gallivm->module, "image", function_type);

   lp_function_add_debug_info(gallivm, function, function_type);

   uint32_t arg_index = 0;

   gallivm->texture_descriptor = LLVMGetParam(function, arg_index++);

   if (params.img_op != LP_IMG_LOAD && params.img_op != LP_IMG_LOAD_SPARSE)
      params.exec_mask = LLVMGetParam(function, arg_index++);

   LLVMValueRef coords[3];
   params.coords = coords;
   for (uint32_t i = 0; i < 3; i++)
      coords[i] = LLVMGetParam(function, arg_index++);

   if (ms)
      params.ms_index = LLVMGetParam(function, arg_index++);

   if (params.img_op != LP_IMG_LOAD && params.img_op != LP_IMG_LOAD_SPARSE)
      for (uint32_t i = 0; i < 4; i++)
         params.indata[i] = LLVMGetParam(function, arg_index++);

   if (params.img_op == LP_IMG_ATOMIC_CAS)
      for (uint32_t i = 0; i < 4; i++)
         params.indata2[i] = LLVMGetParam(function, arg_index++);

   LLVMBuilderRef old_builder = gallivm->builder;
   LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(gallivm->context, function, "entry");
   gallivm->builder = LLVMCreateBuilderInContext(gallivm->context);
   LLVMPositionBuilderAtEnd(gallivm->builder, block);

   LLVMValueRef outdata[5] = { 0 };
   lp_build_img_op_soa(&local_texture, lp_build_image_soa_dynamic_state(image_soa), gallivm, &params, outdata);

   for (uint32_t i = 1; i < 4; i++)
      if (!outdata[i])
         outdata[i] = outdata[0];

   if (outdata[4])
      outdata[4] = LLVMBuildZExt(gallivm->builder, outdata[4], lp_build_int_vec_type(gallivm, lp_int_type(type)), "");
   else
      outdata[4] = lp_build_one(gallivm, lp_int_type(type));

   if (params.img_op != LP_IMG_STORE)
      LLVMBuildAggregateRet(gallivm->builder, outdata, params.img_op == LP_IMG_LOAD_SPARSE ? 5 : 4);
   else
      LLVMBuildRetVoid(gallivm->builder);

   LLVMDisposeBuilder(gallivm->builder);
   gallivm->builder = old_builder;

   free(image_soa);

   return compile_function(ctx, gallivm, function, "image", needs_caching, cache_key);
}

static void *
compile_sample_function(struct llvmpipe_context *ctx, struct lp_texture_handle_state *texture,
                        struct lp_static_sampler_state *sampler, uint32_t sample_key)
{
   enum lp_sampler_lod_control lod_control = (sample_key & LP_SAMPLER_LOD_CONTROL_MASK) >> LP_SAMPLER_LOD_CONTROL_SHIFT;

   bool supported = true;
   if (texture->static_state.format != PIPE_FORMAT_NONE) {
      enum lp_sampler_op_type op_type = (sample_key & LP_SAMPLER_OP_TYPE_MASK) >> LP_SAMPLER_OP_TYPE_SHIFT;
      if (op_type != LP_SAMPLER_OP_LODQ)
         if ((sampler->compare_mode == PIPE_TEX_COMPARE_NONE) == !!(sample_key & LP_SAMPLER_SHADOW))
            supported = false;

      /* Skip integer formats which would cause a type mismatch in the compare function. */
      const struct util_format_description *desc = util_format_description(texture->static_state.format);
      struct lp_type texel_type = {
         .floating = true,
         .width = 32,
         .length = 1,
      };
      texel_type = lp_build_texel_type(texel_type, desc);
      if ((sample_key & LP_SAMPLER_SHADOW) && !texel_type.floating)
         supported = false;

      if (texture_dims(texture->static_state.target) != 2 && op_type == LP_SAMPLER_OP_GATHER)
         supported = false;

      if (op_type != LP_SAMPLER_OP_FETCH) {
         if (!sampler->normalized_coords) {
            if (texture->static_state.target != PIPE_TEXTURE_1D && texture->static_state.target != PIPE_TEXTURE_2D &&
                texture->static_state.target != PIPE_TEXTURE_1D_ARRAY && texture->static_state.target != PIPE_TEXTURE_2D_ARRAY)
               supported = false;

            if (!texture->static_state.level_zero_only)
               supported = false;
         }
      }

      if (util_format_is_pure_integer(texture->static_state.format) &&
          (sampler->min_img_filter == PIPE_TEX_FILTER_LINEAR ||
           sampler->min_mip_filter == PIPE_TEX_MIPFILTER_LINEAR ||
           sampler->mag_img_filter == PIPE_TEX_FILTER_LINEAR))
         supported = false;

      if (sampler->aniso) {
         if (util_format_is_pure_integer(texture->static_state.format))
            supported = false;
      }

      if (util_format_get_num_planes(texture->static_state.format) > 1)
         return NULL;

      uint32_t bind = op_type == LP_SAMPLER_OP_FETCH ? PIPE_BIND_CONSTANT_BUFFER : PIPE_BIND_SAMPLER_VIEW;
      if (!ctx->pipe.screen->is_format_supported(ctx->pipe.screen, texture->static_state.format, texture->static_state.target, 0, 0, bind))
         supported = false;
   }

   uint8_t cache_key[SHA1_DIGEST_LENGTH];
   struct mesa_sha1 hash_ctx;
   _mesa_sha1_init(&hash_ctx);
   _mesa_sha1_update(&hash_ctx, sample_function_base_hash, strlen(sample_function_base_hash));
   _mesa_sha1_update(&hash_ctx, texture, sizeof(*texture));
   _mesa_sha1_update(&hash_ctx, sampler, sizeof(*sampler));
   _mesa_sha1_update(&hash_ctx, &sample_key, sizeof(sample_key));
   _mesa_sha1_final(&hash_ctx, cache_key);

   struct lp_cached_code cached = { 0 };
   lp_disk_cache_find_shader(llvmpipe_screen(ctx->pipe.screen), &cached, cache_key);
   bool needs_caching = !cached.data_size;

   struct gallivm_state *gallivm = gallivm_create("sample_function", get_llvm_context(ctx), &cached);

   struct lp_sampler_static_state state = {
      .texture_state = texture->static_state,
      .sampler_state = *sampler,
   };
   struct lp_build_sampler_soa *sampler_soa = lp_llvm_sampler_soa_create(&state, 1);

   struct lp_type type;
   memset(&type, 0, sizeof type);
   type.floating = true;      /* floating point values */
   type.sign = true;          /* values are signed */
   type.norm = false;         /* values are not limited to [0,1] or [-1,1] */
   type.width = 32;           /* 32-bit float */
   type.length = MIN2(lp_native_vector_width / 32, 16); /* n*4 elements per vector */

   struct lp_compute_shader_variant cs = { .gallivm = gallivm };
   lp_jit_init_cs_types(&cs);

   LLVMTypeRef function_type = lp_build_sample_function_type(gallivm, sample_key);
   LLVMValueRef function = LLVMAddFunction(gallivm->module, "sample", function_type);

   lp_function_add_debug_info(gallivm, function, function_type);

   uint32_t arg_index = 0;

   gallivm->texture_descriptor = LLVMGetParam(function, arg_index++);
   gallivm->sampler_descriptor = LLVMGetParam(function, arg_index++);

   LLVMValueRef coords[5];
   for (unsigned i = 0; i < 4; i++)
      coords[i] = LLVMGetParam(function, arg_index++);

   if (sample_key & LP_SAMPLER_SHADOW)
      coords[4] = LLVMGetParam(function, arg_index++);
   else
      coords[4] = lp_build_undef(gallivm, type);

   LLVMValueRef ms_index = NULL;
   if (sample_key & LP_SAMPLER_FETCH_MS)
      ms_index = LLVMGetParam(function, arg_index++);

   LLVMValueRef offsets[3] = { 0 };
   if (sample_key & LP_SAMPLER_OFFSETS)
      for (unsigned i = 0; i < 3; i++)
         offsets[i] = LLVMGetParam(function, arg_index++);

   LLVMValueRef lod = NULL;
   if (lod_control == LP_SAMPLER_LOD_BIAS || lod_control == LP_SAMPLER_LOD_EXPLICIT)
      lod = LLVMGetParam(function, arg_index++);

   LLVMValueRef min_lod = NULL;
   if (sample_key & LP_SAMPLER_MIN_LOD)
      min_lod = LLVMGetParam(function, arg_index++);

   LLVMBuilderRef old_builder = gallivm->builder;
   LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(gallivm->context, function, "entry");
   gallivm->builder = LLVMCreateBuilderInContext(gallivm->context);
   LLVMPositionBuilderAtEnd(gallivm->builder, block);

   gallivm->texture_dynamic_state = &texture->dynamic_state;

   LLVMValueRef texel_out[5] = { 0 };
   if (supported) {
      lp_build_sample_soa_code(gallivm, &texture->static_state, sampler, lp_build_sampler_soa_dynamic_state(sampler_soa),
                               type, sample_key, 0, 0, cs.jit_resources_type, NULL, cs.jit_cs_thread_data_type,
                               NULL, coords, offsets, NULL, lod, min_lod, ms_index, texel_out);
   } else {
      lp_build_sample_nop(gallivm, lp_build_texel_type(type, util_format_description(texture->static_state.format)), coords, texel_out);
   }

   if (texel_out[4])
      texel_out[4] = LLVMBuildZExt(gallivm->builder, texel_out[4], lp_build_int_vec_type(gallivm, lp_int_type(type)), "");
   else
      texel_out[4] = lp_build_one(gallivm, lp_int_type(type));

   LLVMBuildAggregateRet(gallivm->builder, texel_out, 5);

   LLVMDisposeBuilder(gallivm->builder);
   gallivm->builder = old_builder;

   free(sampler_soa);

   return compile_function(ctx, gallivm, function, "sample", needs_caching, cache_key);
}

static void *
compile_size_function(struct llvmpipe_context *ctx, struct lp_texture_handle_state *texture, bool samples)
{
   uint8_t cache_key[SHA1_DIGEST_LENGTH];
   struct mesa_sha1 hash_ctx;
   _mesa_sha1_init(&hash_ctx);
   _mesa_sha1_update(&hash_ctx, size_function_base_hash, strlen(size_function_base_hash));
   _mesa_sha1_update(&hash_ctx, texture, sizeof(*texture));
   _mesa_sha1_update(&hash_ctx, &samples, sizeof(samples));
   _mesa_sha1_final(&hash_ctx, cache_key);

   struct lp_cached_code cached = { 0 };
   lp_disk_cache_find_shader(llvmpipe_screen(ctx->pipe.screen), &cached, cache_key);
   bool needs_caching = !cached.data_size;

   struct gallivm_state *gallivm = gallivm_create("size_function", get_llvm_context(ctx), &cached);

   struct lp_sampler_static_state state = {
      .texture_state = texture->static_state,
   };
   struct lp_build_sampler_soa *sampler_soa = lp_llvm_sampler_soa_create(&state, 1);

   struct lp_type type;
   memset(&type, 0, sizeof type);
   type.floating = true;      /* floating point values */
   type.sign = true;          /* values are signed */
   type.norm = false;         /* values are not limited to [0,1] or [-1,1] */
   type.width = 32;           /* 32-bit float */
   type.length = MIN2(lp_native_vector_width / 32, 16); /* n*4 elements per vector */

   struct lp_compute_shader_variant cs = { .gallivm = gallivm };
   lp_jit_init_cs_types(&cs);

   struct lp_sampler_size_query_params params = {
      .int_type = lp_int_type(type),
      .target = texture->static_state.target,
      .resources_type = cs.jit_resources_type,
      .is_sviewinfo = true,
      .samples_only = samples,
      .ms = samples,
   };

   if (params.target == PIPE_TEXTURE_1D)
      params.target = PIPE_TEXTURE_1D_ARRAY;
   else if (params.target == PIPE_TEXTURE_2D)
      params.target = PIPE_TEXTURE_2D_ARRAY;
   else if (params.target == PIPE_TEXTURE_CUBE)
      params.target = PIPE_TEXTURE_CUBE_ARRAY;

   LLVMTypeRef function_type = lp_build_size_function_type(gallivm, &params);
   LLVMValueRef function = LLVMAddFunction(gallivm->module, "size", function_type);

   lp_function_add_debug_info(gallivm, function, function_type);

   uint32_t arg_index = 0;

   gallivm->texture_descriptor = LLVMGetParam(function, arg_index++);

   if (!samples)
      params.explicit_lod = LLVMGetParam(function, arg_index++);

   LLVMBuilderRef old_builder = gallivm->builder;
   LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(gallivm->context, function, "entry");
   gallivm->builder = LLVMCreateBuilderInContext(gallivm->context);
   LLVMPositionBuilderAtEnd(gallivm->builder, block);

   gallivm->texture_dynamic_state = &texture->dynamic_state;

   LLVMValueRef out_sizes[4] = { 0 };
   params.sizes_out = out_sizes;
   lp_build_size_query_soa(gallivm, &texture->static_state, lp_build_sampler_soa_dynamic_state(sampler_soa), &params);

   for (uint32_t i = 0; i < 4; i++)
      if (!out_sizes[i])
         out_sizes[i] = lp_build_const_int_vec(gallivm, params.int_type, 0);

   LLVMBuildAggregateRet(gallivm->builder, out_sizes, 4);

   LLVMDisposeBuilder(gallivm->builder);
   gallivm->builder = old_builder;

   free(sampler_soa);

   return compile_function(ctx, gallivm, function, "size", needs_caching, cache_key);
}

static uint64_t
get_sample_function(uint64_t _matrix, uint64_t _texture_functions, uint64_t _sampler_desc, uint32_t sample_key)
{
   struct lp_sampler_matrix *matrix = (void *)(uintptr_t)_matrix;
   struct lp_descriptor *sampler_desc = (void *)(uintptr_t)_sampler_desc;
   uint32_t sampler_index = sampler_desc->texture.sampler_index;

   struct lp_texture_functions *texture_functions = (void *)(uintptr_t)_texture_functions;
   struct sample_function_cache_key key = {
      .texture_functions = texture_functions,
      .sampler_index = sampler_index,
      .sample_key = sample_key,
   };

   struct lp_function_cache *cache = &matrix->caches[LP_FUNCTION_CACHE_SAMPLE];

   void *result;
   {
      struct hash_entry *entry = _mesa_hash_table_search(acquire_latest_function_cache(cache), &key);
      result = entry ? entry->data : NULL;
   }

   if (!result) {
      simple_mtx_lock(&matrix->lock);
      /* Check once more in case the cache got modified between the first check and acquiring the lock. */
      struct hash_table *current_cache = acquire_latest_function_cache(cache);
      struct hash_entry *entry = _mesa_hash_table_search(current_cache, &key);
      result = entry ? entry->data : NULL;
      if (!result) {
         result = compile_sample_function(matrix->ctx, &texture_functions->state, matrix->samplers + sampler_index, sample_key);
         struct sample_function_cache_key *allocated_key = malloc(sizeof(struct sample_function_cache_key));
         *allocated_key = key;
         /* RCU style update: swap in an updated copy of the cache.
         /  Old caches are kept as trash to be safely deleted later. */
         struct hash_table *new_cache = _mesa_hash_table_clone(current_cache, NULL);
         _mesa_hash_table_insert(new_cache, allocated_key, result);
         replace_function_cache_locked(cache, new_cache);
      }
      simple_mtx_unlock(&matrix->lock);
   }

   return (uint64_t)(uintptr_t)result;
}

static uint64_t
get_fetch_function(uint64_t _matrix, uint64_t _texture_functions, uint32_t sample_key)
{
   struct lp_sampler_matrix *matrix = (void *)(uintptr_t)_matrix;

   struct lp_texture_functions *texture_functions = (void *)(uintptr_t)_texture_functions;
   struct sample_function_cache_key key = {
      .texture_functions = texture_functions,
      .sample_key = sample_key,
   };

   struct lp_function_cache *cache = &matrix->caches[LP_FUNCTION_CACHE_FETCH];

   void *result;
   {
      struct hash_entry *entry = _mesa_hash_table_search(acquire_latest_function_cache(cache), &key);
      result = entry ? entry->data : NULL;
   }

   if (!result) {
      simple_mtx_lock(&matrix->lock);
      /* Check once more in case the cache got modified between the first check and acquiring the lock. */
      struct hash_table *current_cache = acquire_latest_function_cache(cache);
      struct hash_entry *entry = _mesa_hash_table_search(current_cache, &key);
      result = entry ? entry->data : NULL;
      if (!result) {
         struct lp_static_sampler_state dummy_sampler = { 0 };
         result = compile_sample_function(matrix->ctx, &texture_functions->state, &dummy_sampler, sample_key);
         struct sample_function_cache_key *allocated_key = malloc(sizeof(struct sample_function_cache_key));
         *allocated_key = key;
         /* RCU style update: swap in an updated copy of the cache.
         /  Old caches are kept as trash to be safely deleted later. */
         struct hash_table *new_cache = _mesa_hash_table_clone(current_cache, NULL);
         _mesa_hash_table_insert(new_cache, allocated_key, result);
         replace_function_cache_locked(cache, new_cache);
      }
      simple_mtx_unlock(&matrix->lock);
   }

   return (uint64_t)(uintptr_t)result;
}

static uint64_t
get_size_function(uint64_t _matrix, uint64_t _texture_functions, uint32_t samples)
{
   struct lp_sampler_matrix *matrix = (void *)(uintptr_t)_matrix;

   struct lp_texture_functions *texture_functions = (void *)(uintptr_t)_texture_functions;
   struct size_function_cache_key key = {
      .texture_functions = texture_functions,
      .samples = samples,
   };

   struct lp_function_cache *cache = &matrix->caches[LP_FUNCTION_CACHE_SIZE];

   void *result;
   {
      struct hash_entry *entry = _mesa_hash_table_search(acquire_latest_function_cache(cache), &key);
      result = entry ? entry->data : NULL;
   }

   if (!result) {
      simple_mtx_lock(&matrix->lock);
      /* Check once more in case the cache got modified between the first check and acquiring the lock. */
      struct hash_table *current_cache = acquire_latest_function_cache(cache);
      struct hash_entry *entry = _mesa_hash_table_search(current_cache, &key);
      result = entry ? entry->data : NULL;
      if (!result) {
         result = compile_size_function(matrix->ctx, &texture_functions->state, samples);
         struct size_function_cache_key *allocated_key = malloc(sizeof(struct size_function_cache_key));
         *allocated_key = key;
         /* RCU style update: swap in an updated copy of the cache.
         /  Old caches are kept as trash to be safely deleted later. */
         struct hash_table *new_cache = _mesa_hash_table_clone(current_cache, NULL);
         _mesa_hash_table_insert(new_cache, allocated_key, result);
         replace_function_cache_locked(cache, new_cache);
      }
      simple_mtx_unlock(&matrix->lock);
   }

   return (uint64_t)(uintptr_t)result;
}

static LLVMTypeRef
lp_build_compile_sample_function_type(struct gallivm_state *gallivm)
{
   LLVMTypeRef param_types[4] = {
      LLVMInt64TypeInContext(gallivm->context),
      LLVMInt64TypeInContext(gallivm->context),
      LLVMInt64TypeInContext(gallivm->context),
      LLVMInt32TypeInContext(gallivm->context),
   };
   LLVMTypeRef ret_type = LLVMInt64TypeInContext(gallivm->context);

   return LLVMFunctionType(ret_type, param_types, ARRAY_SIZE(param_types), false);
}

static void *
compile_jit_sample_function(struct llvmpipe_context *ctx, uint32_t sample_key)
{
   uint8_t cache_key[SHA1_DIGEST_LENGTH];
   struct mesa_sha1 hash_ctx;
   _mesa_sha1_init(&hash_ctx);
   _mesa_sha1_update(&hash_ctx, jit_sample_function_base_hash, strlen(jit_sample_function_base_hash));
   _mesa_sha1_update(&hash_ctx, &sample_key, sizeof(sample_key));
   _mesa_sha1_final(&hash_ctx, cache_key);

   struct lp_cached_code cached = { 0 };
   lp_disk_cache_find_shader(llvmpipe_screen(ctx->pipe.screen), &cached, cache_key);
   bool needs_caching = !cached.data_size;

   struct gallivm_state *gallivm = gallivm_create("jit_sample_function", get_llvm_context(ctx), &cached);

   struct lp_type type;
   memset(&type, 0, sizeof type);
   type.floating = true;      /* floating point values */
   type.sign = true;          /* values are signed */
   type.norm = false;         /* values are not limited to [0,1] or [-1,1] */
   type.width = 32;           /* 32-bit float */
   type.length = MIN2(lp_native_vector_width / 32, 16); /* n*4 elements per vector */

   struct lp_compute_shader_variant cs = { .gallivm = gallivm };
   lp_jit_init_cs_types(&cs);

   LLVMTypeRef function_type = lp_build_sample_function_type(gallivm, sample_key);
   LLVMValueRef function = LLVMAddFunction(gallivm->module, "sample", function_type);

   lp_function_add_debug_info(gallivm, function, function_type);

   uint32_t arg_index = 0;
   LLVMValueRef texture_descriptor = LLVMGetParam(function, arg_index++);
   LLVMValueRef sampler_descriptor = LLVMGetParam(function, arg_index++);

   LLVMBuilderRef old_builder = gallivm->builder;
   LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(gallivm->context, function, "entry");
   gallivm->builder = LLVMCreateBuilderInContext(gallivm->context);
   LLVMBuilderRef builder = gallivm->builder;
   LLVMPositionBuilderAtEnd(gallivm->builder, block);

   LLVMValueRef functions_offset =
      lp_build_const_int64(gallivm, offsetof(struct lp_descriptor, functions));
   LLVMValueRef functions_ptr =
      LLVMBuildAdd(builder, texture_descriptor, functions_offset, "");

   LLVMTypeRef functions_ptr_type = LLVMInt64TypeInContext(gallivm->context);
   LLVMTypeRef functions_ptr_ptr_type = LLVMPointerType(functions_ptr_type, 0);

   functions_ptr = LLVMBuildIntToPtr(builder, functions_ptr, functions_ptr_ptr_type, "");
   /* struct lp_texture_functions * */
   functions_ptr = LLVMBuildLoad2(builder, functions_ptr_type, functions_ptr, "");

   LLVMValueRef matrix_offset =
      lp_build_const_int64(gallivm, offsetof(struct lp_texture_functions, matrix));
   LLVMValueRef matrix_ptr = LLVMBuildAdd(builder, functions_ptr, matrix_offset, "");

   matrix_ptr = LLVMBuildIntToPtr(builder, matrix_ptr, functions_ptr_ptr_type, "");
   /* struct lp_sampler_matrix * */
   matrix_ptr = LLVMBuildLoad2(builder, functions_ptr_type, matrix_ptr, "");

   LLVMTypeRef compile_function_type = lp_build_compile_sample_function_type(gallivm);
   LLVMTypeRef compile_function_ptr_type = LLVMPointerType(compile_function_type, 0);
   LLVMTypeRef compile_function_ptr_ptr_type = LLVMPointerType(compile_function_ptr_type, 0);

   LLVMValueRef compile_function_offset =
      lp_build_const_int64(gallivm, offsetof(struct lp_sampler_matrix, compile_sample_function));
   LLVMValueRef compile_function_ptr =
      LLVMBuildAdd(builder, matrix_ptr, compile_function_offset, "");

   compile_function_ptr =
      LLVMBuildIntToPtr(builder, compile_function_ptr, compile_function_ptr_ptr_type, "");
   /* struct lp_texture_functions * */
   compile_function_ptr =
      LLVMBuildLoad2(builder, compile_function_ptr_type, compile_function_ptr, "");

   LLVMValueRef compile_args[4] = {
      matrix_ptr, functions_ptr, sampler_descriptor, lp_build_const_int32(gallivm, sample_key)
   };

   LLVMValueRef sample_function =
      LLVMBuildCall2(builder, compile_function_type, compile_function_ptr,
                     compile_args, ARRAY_SIZE(compile_args), "");

   sample_function = LLVMBuildIntToPtr(builder, sample_function, LLVMPointerType(function_type, 0), "");

   LLVMValueRef args[LP_MAX_TEX_FUNC_ARGS];
   uint32_t num_args = 0;

   LLVMValueRef arg = LLVMGetFirstParam(function);
   while (true) {
      args[num_args++] = arg;
      if (arg == LLVMGetLastParam(function))
         break;

      arg = LLVMGetNextParam(arg);
   }

   LLVMValueRef result = LLVMBuildCall2(builder, function_type, sample_function, args, num_args, "");
   LLVMBuildRet(gallivm->builder, result);

   LLVMDisposeBuilder(gallivm->builder);
   gallivm->builder = old_builder;

   return compile_function(ctx, gallivm, function, "sample", needs_caching, cache_key);
}

static LLVMTypeRef
lp_build_compile_fetch_function_type(struct gallivm_state *gallivm)
{
   LLVMTypeRef param_types[3] = {
      LLVMInt64TypeInContext(gallivm->context),
      LLVMInt64TypeInContext(gallivm->context),
      LLVMInt32TypeInContext(gallivm->context),
   };
   LLVMTypeRef ret_type = LLVMInt64TypeInContext(gallivm->context);

   return LLVMFunctionType(ret_type, param_types, ARRAY_SIZE(param_types), false);
}

static void *
compile_jit_fetch_function(struct llvmpipe_context *ctx, uint32_t sample_key)
{
   uint8_t cache_key[SHA1_DIGEST_LENGTH];
   struct mesa_sha1 hash_ctx;
   _mesa_sha1_init(&hash_ctx);
   _mesa_sha1_update(&hash_ctx, jit_fetch_function_base_hash, strlen(jit_fetch_function_base_hash));
   _mesa_sha1_update(&hash_ctx, &sample_key, sizeof(sample_key));
   _mesa_sha1_final(&hash_ctx, cache_key);

   struct lp_cached_code cached = { 0 };
   lp_disk_cache_find_shader(llvmpipe_screen(ctx->pipe.screen), &cached, cache_key);
   bool needs_caching = !cached.data_size;

   struct gallivm_state *gallivm = gallivm_create("jit_fetch_function", get_llvm_context(ctx), &cached);

   struct lp_type type;
   memset(&type, 0, sizeof type);
   type.floating = true;      /* floating point values */
   type.sign = true;          /* values are signed */
   type.norm = false;         /* values are not limited to [0,1] or [-1,1] */
   type.width = 32;           /* 32-bit float */
   type.length = MIN2(lp_native_vector_width / 32, 16); /* n*4 elements per vector */

   struct lp_compute_shader_variant cs = { .gallivm = gallivm };
   lp_jit_init_cs_types(&cs);

   LLVMTypeRef function_type = lp_build_sample_function_type(gallivm, sample_key);
   LLVMValueRef function = LLVMAddFunction(gallivm->module, "fetch", function_type);

   lp_function_add_debug_info(gallivm, function, function_type);

   LLVMValueRef texture_descriptor = LLVMGetParam(function, 0);

   LLVMBuilderRef old_builder = gallivm->builder;
   LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(gallivm->context, function, "entry");
   gallivm->builder = LLVMCreateBuilderInContext(gallivm->context);
   LLVMBuilderRef builder = gallivm->builder;
   LLVMPositionBuilderAtEnd(gallivm->builder, block);

   LLVMValueRef functions_offset =
      lp_build_const_int64(gallivm, offsetof(struct lp_descriptor, functions));
   LLVMValueRef functions_ptr =
      LLVMBuildAdd(builder, texture_descriptor, functions_offset, "");

   LLVMTypeRef functions_ptr_type = LLVMInt64TypeInContext(gallivm->context);
   LLVMTypeRef functions_ptr_ptr_type = LLVMPointerType(functions_ptr_type, 0);

   functions_ptr = LLVMBuildIntToPtr(builder, functions_ptr, functions_ptr_ptr_type, "");
   /* struct lp_texture_functions * */
   functions_ptr = LLVMBuildLoad2(builder, functions_ptr_type, functions_ptr, "");

   LLVMValueRef matrix_offset =
      lp_build_const_int64(gallivm, offsetof(struct lp_texture_functions, matrix));
   LLVMValueRef matrix_ptr = LLVMBuildAdd(builder, functions_ptr, matrix_offset, "");

   matrix_ptr = LLVMBuildIntToPtr(builder, matrix_ptr, functions_ptr_ptr_type, "");
   /* struct lp_sampler_matrix * */
   matrix_ptr = LLVMBuildLoad2(builder, functions_ptr_type, matrix_ptr, "");

   LLVMTypeRef compile_function_type = lp_build_compile_fetch_function_type(gallivm);
   LLVMTypeRef compile_function_ptr_type = LLVMPointerType(compile_function_type, 0);
   LLVMTypeRef compile_function_ptr_ptr_type = LLVMPointerType(compile_function_ptr_type, 0);

   LLVMValueRef compile_function_offset =
      lp_build_const_int64(gallivm, offsetof(struct lp_sampler_matrix, compile_fetch_function));
   LLVMValueRef compile_function_ptr =
      LLVMBuildAdd(builder, matrix_ptr, compile_function_offset, "");

   compile_function_ptr =
      LLVMBuildIntToPtr(builder, compile_function_ptr, compile_function_ptr_ptr_type, "");
   /* struct lp_texture_functions * */
   compile_function_ptr =
      LLVMBuildLoad2(builder, compile_function_ptr_type, compile_function_ptr, "");

   LLVMValueRef compile_args[3] = {
      matrix_ptr, functions_ptr, lp_build_const_int32(gallivm, sample_key)
   };

   LLVMValueRef fetch_function =
      LLVMBuildCall2(builder, compile_function_type, compile_function_ptr,
                     compile_args, ARRAY_SIZE(compile_args), "");

   fetch_function = LLVMBuildIntToPtr(builder, fetch_function, LLVMPointerType(function_type, 0), "");

   LLVMValueRef args[LP_MAX_TEX_FUNC_ARGS];
   uint32_t num_args = 0;

   LLVMValueRef arg = LLVMGetFirstParam(function);
   while (true) {
      args[num_args++] = arg;
      if (arg == LLVMGetLastParam(function))
         break;

      arg = LLVMGetNextParam(arg);
   }

   LLVMValueRef result = LLVMBuildCall2(builder, function_type, fetch_function, args, num_args, "");
   LLVMBuildRet(gallivm->builder, result);

   LLVMDisposeBuilder(gallivm->builder);
   gallivm->builder = old_builder;

   return compile_function(ctx, gallivm, function, "fetch", needs_caching, cache_key);
}

static LLVMTypeRef
lp_build_compile_size_function_type(struct gallivm_state *gallivm)
{
   LLVMTypeRef param_types[3] = {
      LLVMInt64TypeInContext(gallivm->context),
      LLVMInt64TypeInContext(gallivm->context),
      LLVMInt32TypeInContext(gallivm->context),
   };
   LLVMTypeRef ret_type = LLVMInt64TypeInContext(gallivm->context);

   return LLVMFunctionType(ret_type, param_types, ARRAY_SIZE(param_types), false);
}

static void *
compile_jit_size_function(struct llvmpipe_context *ctx, bool samples)
{
   uint8_t cache_key[SHA1_DIGEST_LENGTH];
   struct mesa_sha1 hash_ctx;
   _mesa_sha1_init(&hash_ctx);
   _mesa_sha1_update(&hash_ctx, jit_size_function_base_hash, strlen(jit_size_function_base_hash));
   _mesa_sha1_update(&hash_ctx, &samples, sizeof(samples));
   _mesa_sha1_final(&hash_ctx, cache_key);

   struct lp_cached_code cached = { 0 };
   lp_disk_cache_find_shader(llvmpipe_screen(ctx->pipe.screen), &cached, cache_key);
   bool needs_caching = !cached.data_size;

   struct gallivm_state *gallivm = gallivm_create("jit_size_function", get_llvm_context(ctx), &cached);

   struct lp_type type;
   memset(&type, 0, sizeof type);
   type.floating = true;      /* floating point values */
   type.sign = true;          /* values are signed */
   type.norm = false;         /* values are not limited to [0,1] or [-1,1] */
   type.width = 32;           /* 32-bit float */
   type.length = MIN2(lp_native_vector_width / 32, 16); /* n*4 elements per vector */

   struct lp_compute_shader_variant cs = { .gallivm = gallivm };
   lp_jit_init_cs_types(&cs);

   struct lp_sampler_size_query_params params = {
      .samples_only = samples,
      .ms = samples,
   };

   LLVMTypeRef function_type = lp_build_size_function_type(gallivm, &params);
   LLVMValueRef function = LLVMAddFunction(gallivm->module, "size", function_type);

   lp_function_add_debug_info(gallivm, function, function_type);

   LLVMValueRef texture_descriptor = LLVMGetParam(function, 0);

   LLVMBuilderRef old_builder = gallivm->builder;
   LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(gallivm->context, function, "entry");
   gallivm->builder = LLVMCreateBuilderInContext(gallivm->context);
   LLVMBuilderRef builder = gallivm->builder;
   LLVMPositionBuilderAtEnd(gallivm->builder, block);

   LLVMValueRef functions_offset =
      lp_build_const_int64(gallivm, offsetof(struct lp_descriptor, functions));
   LLVMValueRef functions_ptr =
      LLVMBuildAdd(builder, texture_descriptor, functions_offset, "");

   LLVMTypeRef functions_ptr_type = LLVMInt64TypeInContext(gallivm->context);
   LLVMTypeRef functions_ptr_ptr_type = LLVMPointerType(functions_ptr_type, 0);

   functions_ptr = LLVMBuildIntToPtr(builder, functions_ptr, functions_ptr_ptr_type, "");
   /* struct lp_texture_functions * */
   functions_ptr = LLVMBuildLoad2(builder, functions_ptr_type, functions_ptr, "");

   LLVMValueRef matrix_offset =
      lp_build_const_int64(gallivm, offsetof(struct lp_texture_functions, matrix));
   LLVMValueRef matrix_ptr = LLVMBuildAdd(builder, functions_ptr, matrix_offset, "");

   matrix_ptr = LLVMBuildIntToPtr(builder, matrix_ptr, functions_ptr_ptr_type, "");
   /* struct lp_sampler_matrix * */
   matrix_ptr = LLVMBuildLoad2(builder, functions_ptr_type, matrix_ptr, "");

   LLVMTypeRef compile_function_type = lp_build_compile_size_function_type(gallivm);
   LLVMTypeRef compile_function_ptr_type = LLVMPointerType(compile_function_type, 0);
   LLVMTypeRef compile_function_ptr_ptr_type = LLVMPointerType(compile_function_ptr_type, 0);

   LLVMValueRef compile_function_offset =
      lp_build_const_int64(gallivm, offsetof(struct lp_sampler_matrix, compile_size_function));
   LLVMValueRef compile_function_ptr =
      LLVMBuildAdd(builder, matrix_ptr, compile_function_offset, "");

   compile_function_ptr =
      LLVMBuildIntToPtr(builder, compile_function_ptr, compile_function_ptr_ptr_type, "");
   /* struct lp_texture_functions * */
   compile_function_ptr =
      LLVMBuildLoad2(builder, compile_function_ptr_type, compile_function_ptr, "");

   LLVMValueRef compile_args[3] = {
      matrix_ptr, functions_ptr, lp_build_const_int32(gallivm, samples)
   };

   LLVMValueRef size_function =
      LLVMBuildCall2(builder, compile_function_type, compile_function_ptr,
                     compile_args, ARRAY_SIZE(compile_args), "");

   size_function = LLVMBuildIntToPtr(builder, size_function, LLVMPointerType(function_type, 0), "");

   LLVMValueRef args[LP_MAX_TEX_FUNC_ARGS];
   uint32_t num_args = 0;

   LLVMValueRef arg = LLVMGetFirstParam(function);
   while (true) {
      args[num_args++] = arg;
      if (arg == LLVMGetLastParam(function))
         break;

      arg = LLVMGetNextParam(arg);
   }

   LLVMValueRef result = LLVMBuildCall2(builder, function_type, size_function, args, num_args, "");
   LLVMBuildRet(gallivm->builder, result);

   LLVMDisposeBuilder(gallivm->builder);
   gallivm->builder = old_builder;

   return compile_function(ctx, gallivm, function, "size", needs_caching, cache_key);
}

static void
compile_sample_functions(struct llvmpipe_context *ctx, struct lp_texture_handle_state *texture,
                        struct lp_static_sampler_state *sampler, void **functions)
{
   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;

   /* There is nothing to do if this texture+sampler combination uses the "default" functions. */
   if (functions == matrix->jit_sample_functions || functions == matrix->jit_fetch_functions)
      return;

   bool has_sampler = !!sampler;

   struct lp_static_sampler_state dummy_sampler = { 0 };
   if (!sampler)
      sampler = &dummy_sampler;

   for (uint32_t sample_key = 0; sample_key < LP_SAMPLE_KEY_COUNT; sample_key++) {
      if (!BITSET_TEST(matrix->sample_keys, sample_key))
         continue;

      enum lp_sampler_op_type op_type = (sample_key & LP_SAMPLER_OP_TYPE_MASK) >> LP_SAMPLER_OP_TYPE_SHIFT;
      if (has_sampler && op_type == LP_SAMPLER_OP_FETCH)
         continue;

      if (!functions[sample_key]) {
         if (has_sampler)
            functions[sample_key] = matrix->jit_sample_functions[sample_key];
         else if (op_type == LP_SAMPLER_OP_FETCH)
            functions[sample_key] = matrix->jit_fetch_functions[sample_key];
         else if (texture->static_state.format == PIPE_FORMAT_NONE)
            functions[sample_key] = compile_sample_function(ctx, texture, sampler, sample_key);
      }
   }
}

static void
llvmpipe_register_texture(struct llvmpipe_context *ctx, struct lp_texture_handle_state *state, bool sampled)
{
   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;

   bool packed = true;
   uint32_t dst_index = matrix->texture_count;
   for (uint32_t i = 0; i < matrix->texture_count; i++) {
      if (memcmp(&matrix->textures[i]->state, state, sizeof(struct lp_texture_handle_state)))
         continue;

      bool has_functions = sampled ? matrix->textures[i]->sampled : matrix->textures[i]->storage;
      if (has_functions)
         return;

      packed = false;
      dst_index = i;
      break;
   }

   struct lp_texture_functions *entry;
   if (packed) {
      matrix->texture_count++;
      matrix->textures = realloc(matrix->textures, matrix->texture_count * sizeof(struct lp_texture_functions *));

      entry = calloc(1, sizeof(struct lp_texture_functions));
      matrix->textures[dst_index] = entry;

      entry->state = *state;
      entry->image_functions = calloc(LP_TOTAL_IMAGE_OP_COUNT, sizeof(void **));
      entry->matrix = matrix;
   } else {
      entry = matrix->textures[dst_index];
   }

   if (sampled)
      entry->sampled = true;
   else
      entry->storage = true;

   simple_mtx_lock(&matrix->lock);

   if (entry->sampled) {
      if (matrix->sampler_count > 0) {
         if (entry->sample_functions) {
            entry->sample_functions = realloc(entry->sample_functions, matrix->sampler_count * sizeof(void **));
            memset(entry->sample_functions + entry->sampler_count, 0,
                   (matrix->sampler_count - entry->sampler_count) * sizeof(void **));
         } else {
            entry->sample_functions = calloc(matrix->sampler_count, sizeof(void **));
         }
      }
      entry->sampler_count = matrix->sampler_count;

      if (state->static_state.format == PIPE_FORMAT_NONE) {
         if (matrix->sampler_count) {
            entry->sample_functions[0] = calloc(LP_SAMPLE_KEY_COUNT, sizeof(void *));
            compile_sample_functions(ctx, state, NULL, entry->sample_functions[0]);
            for (uint32_t i = 1; i < matrix->sampler_count; i++)
               entry->sample_functions[i] = entry->sample_functions[0];
         }
      } else {
         for (uint32_t i = 0; i < matrix->sampler_count; i++)
            entry->sample_functions[i] = matrix->jit_sample_functions;
      }

      entry->fetch_functions = matrix->jit_fetch_functions;

      if (!entry->size_function)
         entry->size_function = matrix->jit_size_functions[0];

      if (!entry->samples_function)
         entry->samples_function = matrix->jit_size_functions[1];
   }

   if (entry->storage) {
      uint32_t image_op;
      BITSET_FOREACH_SET (image_op, matrix->image_ops, LP_TOTAL_IMAGE_OP_COUNT)
         if (!entry->image_functions[image_op])
            entry->image_functions[image_op] = compile_image_function(ctx, &state->static_state, image_op);
   }

   simple_mtx_unlock(&matrix->lock);
}

static void
llvmpipe_register_sampler(struct llvmpipe_context *ctx, struct lp_static_sampler_state *state)
{
   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;
   for (uint32_t i = 0; i < matrix->sampler_count; i++)
      if (!memcmp(matrix->samplers + i, state, sizeof(struct lp_static_sampler_state)))
         return;

   matrix->sampler_count++;
   matrix->samplers = realloc(matrix->samplers, matrix->sampler_count * sizeof(struct lp_static_sampler_state));

   matrix->samplers[matrix->sampler_count - 1] = *state;

   simple_mtx_lock(&matrix->lock);

   for (uint32_t i = 0; i < matrix->texture_count; i++) {
      struct lp_texture_functions *texture = matrix->textures[i];
      if (!texture->sampled)
         continue;

      texture->sampler_count = matrix->sampler_count;
      texture->sample_functions = realloc(texture->sample_functions, matrix->sampler_count * sizeof(void **));

      if (texture->state.static_state.format == PIPE_FORMAT_NONE)  {
         if (matrix->sampler_count == 1) {
            texture->sample_functions[0] = calloc(LP_SAMPLE_KEY_COUNT, sizeof(void *));
            compile_sample_functions(ctx, &texture->state, NULL, texture->sample_functions[0]);
         } else {
            texture->sample_functions[matrix->sampler_count - 1] = texture->sample_functions[0];
         }
         continue;
      }

      texture->sample_functions[matrix->sampler_count - 1] = matrix->jit_sample_functions;
   }

   simple_mtx_unlock(&matrix->lock);
}

static void
register_sample_key(struct llvmpipe_context *ctx, uint32_t sample_key)
{
   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;
   if (BITSET_TEST(matrix->sample_keys, sample_key))
      return;

   BITSET_SET(matrix->sample_keys, sample_key);

   simple_mtx_lock(&matrix->lock);

   enum lp_sampler_op_type op_type = (sample_key & LP_SAMPLER_OP_TYPE_MASK) >> LP_SAMPLER_OP_TYPE_SHIFT;
   if (op_type == LP_SAMPLER_OP_FETCH)
      matrix->jit_fetch_functions[sample_key] = compile_jit_fetch_function(ctx, sample_key);
   else
      matrix->jit_sample_functions[sample_key] = compile_jit_sample_function(ctx, sample_key);

   for (uint32_t texture_index = 0; texture_index < matrix->texture_count; texture_index++) {
      struct lp_texture_functions *texture = matrix->textures[texture_index];
      if (!texture->sampled)
         continue;

      enum lp_sampler_op_type op_type = (sample_key & LP_SAMPLER_OP_TYPE_MASK) >> LP_SAMPLER_OP_TYPE_SHIFT;
      if (op_type == LP_SAMPLER_OP_FETCH) {
         if (texture->fetch_functions != matrix->jit_fetch_functions)
            texture->fetch_functions[sample_key] = matrix->jit_fetch_functions[sample_key];
         continue;
      }

      if (texture->state.static_state.format == PIPE_FORMAT_NONE) {
         if (matrix->sampler_count) {
            struct lp_static_sampler_state dummy_sampler = { 0 };
            texture->sample_functions[0][sample_key] = compile_sample_function(ctx, &texture->state, &dummy_sampler, sample_key);
         }
         continue;
      }

      for (uint32_t sampler_index = 0; sampler_index < matrix->sampler_count; sampler_index++)
         texture->sample_functions[sampler_index][sample_key] = matrix->jit_sample_functions[sample_key];
   }

   simple_mtx_unlock(&matrix->lock);
}

static void
register_image_op(struct llvmpipe_context *ctx, uint32_t op)
{
   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;
   if (BITSET_TEST(matrix->image_ops, op))
      return;

   BITSET_SET(matrix->image_ops, op);

   simple_mtx_lock(&matrix->lock);

   for (uint32_t texture_index = 0; texture_index < matrix->texture_count; texture_index++) {
      struct lp_texture_functions *texture = matrix->textures[texture_index];
      if (texture->storage)
         texture->image_functions[op] = compile_image_function(ctx, &texture->state.static_state, op);
   }

   simple_mtx_unlock(&matrix->lock);
}

static bool
register_instr(nir_builder *b, nir_instr *instr, void *data)
{
   struct llvmpipe_context *ctx = data;

   if (instr->type == nir_instr_type_tex) {
      nir_tex_instr *tex = nir_instr_as_tex(instr);
      uint32_t sample_key = lp_build_nir_sample_key(b->shader->info.stage, tex);

      register_sample_key(ctx, sample_key);
   } else if (instr->type == nir_instr_type_intrinsic) {
      nir_intrinsic_instr *intrin = nir_instr_as_intrinsic(instr);

      uint32_t op = lp_packed_img_op_from_intrinsic(intrin);
      if (op != -1)
         register_image_op(ctx, op);
   }

   return false;
}

void
llvmpipe_register_shader(struct pipe_context *ctx, const struct pipe_shader_state *shader)
{
   if (shader->type == PIPE_SHADER_IR_NIR)
      nir_shader_instructions_pass(shader->ir.nir, register_instr, nir_metadata_all, ctx);
}

void
llvmpipe_clear_sample_functions_cache(struct llvmpipe_context *ctx, struct pipe_fence_handle **fence)
{
   if (!fence)
      return;

   struct lp_sampler_matrix *matrix = &ctx->sampler_matrix;

   /* If the cache is empty, there is nothing to do. */
   bool has_cache_entry = false;
   for (uint32_t i = 0; i < ARRAY_SIZE(matrix->caches); i++) {
      if (_mesa_hash_table_num_entries(acquire_latest_function_cache(&matrix->caches[i]))) {
         has_cache_entry = true;
         break;
      }
   }
   if (!has_cache_entry)
      return;

   ctx->pipe.screen->fence_finish(ctx->pipe.screen, NULL, *fence, OS_TIMEOUT_INFINITE);

   /* All work is finished, it's safe to move cache entries into the table. */
   hash_table_foreach_remove(acquire_latest_function_cache(&matrix->caches[LP_FUNCTION_CACHE_SAMPLE]), entry) {
      struct sample_function_cache_key *key = (void *)entry->key;

      if (key->texture_functions->sample_functions[key->sampler_index] == matrix->jit_sample_functions) {
         key->texture_functions->sample_functions[key->sampler_index] = malloc(LP_SAMPLE_KEY_COUNT * sizeof(void *));
         memcpy(key->texture_functions->sample_functions[key->sampler_index], matrix->jit_sample_functions,
                LP_SAMPLE_KEY_COUNT * sizeof(void *));
      }

      key->texture_functions->sample_functions[key->sampler_index][key->sample_key] = entry->data;
      free(key);
   }

   hash_table_foreach_remove(acquire_latest_function_cache(&matrix->caches[LP_FUNCTION_CACHE_FETCH]), entry) {
      struct sample_function_cache_key *key = (void *)entry->key;

      if (key->texture_functions->fetch_functions == matrix->jit_fetch_functions) {
         key->texture_functions->fetch_functions = malloc(LP_SAMPLE_KEY_COUNT * sizeof(void *));
         memcpy(key->texture_functions->fetch_functions, matrix->jit_fetch_functions, LP_SAMPLE_KEY_COUNT * sizeof(void *));
      }

      key->texture_functions->fetch_functions[key->sample_key] = entry->data;
      free(key);
   }

   hash_table_foreach_remove(acquire_latest_function_cache(&matrix->caches[LP_FUNCTION_CACHE_SIZE]), entry) {
      struct size_function_cache_key *key = (void *)entry->key;
      if (key->samples)
         key->texture_functions->samples_function = entry->data;
      else
         key->texture_functions->size_function = entry->data;
      free(key);
   }

   for (uint32_t i = 0; i < ARRAY_SIZE(matrix->caches); i++) {
      util_dynarray_foreach (&matrix->caches[i].trash_caches, struct hash_table *, trash)
         _mesa_hash_table_destroy(*trash, NULL);
      util_dynarray_clear(&matrix->caches[i].trash_caches);
   }
}

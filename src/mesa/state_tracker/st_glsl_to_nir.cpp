/*
 * Copyright © 2015 Red Hat
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

#include "st_nir.h"

#include "pipe/p_defines.h"
#include "pipe/p_screen.h"
#include "pipe/p_context.h"

#include "program/program.h"
#include "program/prog_statevars.h"
#include "program/prog_parameter.h"
#include "main/context.h"
#include "main/mtypes.h"
#include "main/errors.h"
#include "main/glspirv.h"
#include "main/shaderapi.h"
#include "main/uniforms.h"

#include "main/shaderobj.h"
#include "st_context.h"
#include "st_program.h"
#include "st_shader_cache.h"

#include "compiler/nir/nir.h"
#include "compiler/nir/nir_builder.h"
#include "compiler/glsl_types.h"
#include "compiler/glsl/glsl_to_nir.h"
#include "compiler/glsl/gl_nir.h"
#include "compiler/glsl/gl_nir_linker.h"
#include "compiler/glsl/ir.h"
#include "compiler/glsl/ir_optimization.h"
#include "compiler/glsl/linker_util.h"
#include "compiler/glsl/shader_cache.h"
#include "compiler/glsl/string_to_uint_map.h"

#include "util/log.h"

static int
type_size(const struct glsl_type *type)
{
   return glsl_count_attribute_slots(type, false);
}

static int
st_nir_lookup_parameter_index(struct gl_program *prog, nir_variable *var)
{
   struct gl_program_parameter_list *params = prog->Parameters;

   /* Lookup the first parameter that the uniform storage that match the
    * variable location.
    */
   for (unsigned i = 0; i < params->NumParameters; i++) {
      int index = params->Parameters[i].MainUniformStorageIndex;
      if (index == var->data.location)
         return i;
   }

   /* TODO: Handle this fallback for SPIR-V.  We need this for GLSL e.g. in
    * dEQP-GLES2.functional.uniform_api.random.3
    */

   /* is there a better way to do this?  If we have something like:
    *
    *    struct S {
    *           float f;
    *           vec4 v;
    *    };
    *    uniform S color;
    *
    * Then what we get in prog->Parameters looks like:
    *
    *    0: Name=color.f, Type=6, DataType=1406, Size=1
    *    1: Name=color.v, Type=6, DataType=8b52, Size=4
    *
    * So the name doesn't match up and _mesa_lookup_parameter_index()
    * fails.  In this case just find the first matching "color.*"..
    *
    * Note for arrays you could end up w/ color[n].f, for example.
    */
   if (!prog->sh.data->spirv) {
      int namelen = strlen(var->name);
      for (unsigned i = 0; i < params->NumParameters; i++) {
         struct gl_program_parameter *p = &params->Parameters[i];
         if ((strncmp(p->Name, var->name, namelen) == 0) &&
             ((p->Name[namelen] == '.') || (p->Name[namelen] == '['))) {
            return i;
         }
      }
   }

   return -1;
}

static void
st_nir_assign_uniform_locations(struct gl_context *ctx,
                                struct gl_program *prog,
                                nir_shader *nir)
{
   int shaderidx = 0;
   int imageidx = 0;

   nir_foreach_variable_with_modes(uniform, nir, nir_var_uniform |
                                                 nir_var_image) {
      int loc;

      const struct glsl_type *type = glsl_without_array(uniform->type);
      if (!uniform->data.bindless && (glsl_type_is_sampler(type) || glsl_type_is_image(type))) {
         if (glsl_type_is_sampler(type)) {
            loc = shaderidx;
            shaderidx += type_size(uniform->type);
         } else {
            loc = imageidx;
            imageidx += type_size(uniform->type);
         }
      } else if (uniform->state_slots) {
         const gl_state_index16 *const stateTokens = uniform->state_slots[0].tokens;

         unsigned comps;
         if (glsl_type_is_struct_or_ifc(type)) {
            comps = 4;
         } else {
            comps = glsl_get_vector_elements(type);
         }

         if (ctx->Const.PackedDriverUniformStorage) {
            loc = _mesa_add_sized_state_reference(prog->Parameters,
                                                  stateTokens, comps, false);
            loc = prog->Parameters->Parameters[loc].ValueOffset;
         } else {
            loc = _mesa_add_state_reference(prog->Parameters, stateTokens);
         }
      } else {
         loc = st_nir_lookup_parameter_index(prog, uniform);

         /* We need to check that loc is not -1 here before accessing the
          * array. It can be negative for example when we have a struct that
          * only contains opaque types.
          */
         if (loc >= 0 && ctx->Const.PackedDriverUniformStorage) {
            loc = prog->Parameters->Parameters[loc].ValueOffset;
         }
      }

      uniform->data.driver_location = loc;
   }
}

static bool
def_is_64bit(nir_def *def, void *state)
{
   bool *lower = (bool *)state;
   if (def && (def->bit_size == 64)) {
      *lower = true;
      return false;
   }
   return true;
}

static bool
src_is_64bit(nir_src *src, void *state)
{
   bool *lower = (bool *)state;
   if (src && (nir_src_bit_size(*src) == 64)) {
      *lower = true;
      return false;
   }
   return true;
}

static bool
filter_64_bit_instr(const nir_instr *const_instr, UNUSED const void *data)
{
   bool lower = false;
   /* lower_alu_to_scalar required nir_instr to be const, but nir_foreach_*
    * doesn't have const variants, so do the ugly const_cast here. */
   nir_instr *instr = const_cast<nir_instr *>(const_instr);

   nir_foreach_def(instr, def_is_64bit, &lower);
   if (lower)
      return true;
   nir_foreach_src(instr, src_is_64bit, &lower);
   return lower;
}

/* Second third of converting glsl_to_nir. This creates uniforms, gathers
 * info on varyings, etc after NIR link time opts have been applied.
 */
static void
st_glsl_to_nir_post_opts(struct st_context *st, struct gl_program *prog,
                         struct gl_shader_program *shader_program)
{
   nir_shader *nir = prog->nir;
   struct pipe_screen *screen = st->screen;

   /* Make a pass over the IR to add state references for any built-in
    * uniforms that are used.  This has to be done now (during linking).
    * Code generation doesn't happen until the first time this shader is
    * used for rendering.  Waiting until then to generate the parameters is
    * too late.  At that point, the values for the built-in uniforms won't
    * get sent to the shader.
    */
   nir_foreach_uniform_variable(var, nir) {
      const nir_state_slot *const slots = var->state_slots;
      if (slots != NULL) {
         const struct glsl_type *type = glsl_without_array(var->type);
         for (unsigned int i = 0; i < var->num_state_slots; i++) {
            unsigned comps;
            if (glsl_type_is_struct_or_ifc(type)) {
               comps = _mesa_program_state_value_size(slots[i].tokens);
            } else {
               comps = glsl_get_vector_elements(type);
            }

            if (st->ctx->Const.PackedDriverUniformStorage) {
               _mesa_add_sized_state_reference(prog->Parameters,
                                               slots[i].tokens,
                                               comps, false);
            } else {
               _mesa_add_state_reference(prog->Parameters,
                                         slots[i].tokens);
            }
         }
      }
   }

   /* Avoid reallocation of the program parameter list, because the uniform
    * storage is only associated with the original parameter list.
    * This should be enough for Bitmap and DrawPixels constants.
    */
   _mesa_ensure_and_associate_uniform_storage(st->ctx, shader_program, prog, 28);

   /* None of the builtins being lowered here can be produced by SPIR-V.  See
    * _mesa_builtin_uniform_desc. Also drivers that support packed uniform
    * storage don't need to lower builtins.
    */
   if (!shader_program->data->spirv &&
       !st->ctx->Const.PackedDriverUniformStorage)
      NIR_PASS(_, nir, st_nir_lower_builtin);

   if (!screen->caps.nir_atomics_as_deref)
      NIR_PASS(_, nir, gl_nir_lower_atomics, shader_program, true);

   NIR_PASS(_, nir, nir_opt_intrinsics);

   /* Lower 64-bit ops. */
   if (nir->options->lower_int64_options ||
       nir->options->lower_doubles_options) {
      bool lowered_64bit_ops = false;
      bool revectorize = false;

      if (nir->options->lower_doubles_options) {
         /* nir_lower_doubles is not prepared for vector ops, so if the backend doesn't
          * request lower_alu_to_scalar until now, lower all 64 bit ops, and try to
          * vectorize them afterwards again */
         if (!nir->options->lower_to_scalar) {
            NIR_PASS(revectorize, nir, nir_lower_alu_to_scalar, filter_64_bit_instr, nullptr);
            NIR_PASS(revectorize, nir, nir_lower_phis_to_scalar, NULL, NULL);
         }
         /* doubles lowering requires frexp to be lowered first if it will be,
          * since the pass generates other 64-bit ops.  Most backends lower
          * frexp, and using doubles is rare, and using frexp is even more rare
          * (no instances in shader-db), so we're not too worried about
          * accidentally lowering a 32-bit frexp here.
          */
         NIR_PASS(lowered_64bit_ops, nir, nir_lower_frexp);

         NIR_PASS(lowered_64bit_ops, nir, nir_lower_doubles,
                  st->ctx->SoftFP64, nir->options->lower_doubles_options);
      }
      if (nir->options->lower_int64_options)
         NIR_PASS(lowered_64bit_ops, nir, nir_lower_int64);

      if (revectorize && !nir->options->vectorize_vec2_16bit)
         NIR_PASS(_, nir, nir_opt_vectorize, nullptr, nullptr);

      if (revectorize || lowered_64bit_ops)
         gl_nir_opts(nir);
   }

   nir_remove_dead_variables(nir, nir_var_function_temp, NULL);

   if (!st->has_hw_atomics && !screen->caps.nir_atomics_as_deref) {
      unsigned align_offset_state = 0;
      if (st->ctx->Const.ShaderStorageBufferOffsetAlignment > 4) {
         struct gl_program_parameter_list *params = prog->Parameters;
         for (unsigned i = 0; i < shader_program->data->NumAtomicBuffers; i++) {
            gl_state_index16 state[STATE_LENGTH] = { STATE_ATOMIC_COUNTER_OFFSET, (short)shader_program->data->AtomicBuffers[i].Binding };
            _mesa_add_state_reference(params, state);
         }
         align_offset_state = STATE_ATOMIC_COUNTER_OFFSET;
      }
      NIR_PASS(_, nir, nir_lower_atomics_to_ssbo, align_offset_state);
   }

   st_set_prog_affected_state_flags(prog);
   nir_shader_gather_info(nir, nir_shader_get_entrypoint(nir));

   if (st->allow_st_finalize_nir_twice) {
      st_serialize_base_nir(prog, nir);
      st_finalize_nir(st, prog, shader_program, nir, true, false);

      if (screen->finalize_nir)
         screen->finalize_nir(screen, nir);
   }

   if (st->ctx->_Shader->Flags & GLSL_DUMP) {
      _mesa_log("\n");
      _mesa_log("NIR IR for linked %s program %d:\n",
             _mesa_shader_stage_to_string(prog->info.stage),
             shader_program->Name);
      nir_print_shader(nir, mesa_log_get_file());
      _mesa_log("\n\n");
   }
}

extern "C" {

bool
st_nir_lower_wpos_ytransform(struct nir_shader *nir,
                             struct gl_program *prog,
                             struct pipe_screen *pscreen)
{
   bool progress = false;

   if (nir->info.stage != MESA_SHADER_FRAGMENT) {
      nir_shader_preserve_all_metadata(nir);
      return progress;
   }

   static const gl_state_index16 wposTransformState[STATE_LENGTH] = {
      STATE_FB_WPOS_Y_TRANSFORM
   };
   nir_lower_wpos_ytransform_options wpos_options = { { 0 } };

   memcpy(wpos_options.state_tokens, wposTransformState,
          sizeof(wpos_options.state_tokens));
   wpos_options.fs_coord_origin_upper_left =
      pscreen->caps.fs_coord_origin_upper_left;
   wpos_options.fs_coord_origin_lower_left =
      pscreen->caps.fs_coord_origin_lower_left;
   wpos_options.fs_coord_pixel_center_integer =
      pscreen->caps.fs_coord_pixel_center_integer;
   wpos_options.fs_coord_pixel_center_half_integer =
      pscreen->caps.fs_coord_pixel_center_half_integer;

   if (nir_lower_wpos_ytransform(nir, &wpos_options)) {
      _mesa_add_state_reference(prog->Parameters, wposTransformState);
      progress = true;
   }

   static const gl_state_index16 pntcTransformState[STATE_LENGTH] = {
      STATE_FB_PNTC_Y_TRANSFORM
   };

   if (nir_lower_pntc_ytransform(nir, &pntcTransformState)) {
      _mesa_add_state_reference(prog->Parameters, pntcTransformState);
      progress = true;
   }

   return progress;
}

static bool
st_link_glsl_to_nir(struct gl_context *ctx,
                    struct gl_shader_program *shader_program)
{
   struct st_context *st = st_context(ctx);
   struct gl_linked_shader *linked_shader[MESA_SHADER_STAGES];
   unsigned num_shaders = 0;

   /* Return early if we are loading the shader from on-disk cache */
   if (st_load_nir_from_disk_cache(ctx, shader_program)) {
      return GL_TRUE;
   }

   MESA_TRACE_FUNC();

   assert(shader_program->data->LinkStatus);

   if (!shader_program->data->spirv) {
      if (!gl_nir_link_glsl(ctx, shader_program))
         return GL_FALSE;
   }

   for (unsigned i = 0; i < MESA_SHADER_STAGES; i++) {
      if (shader_program->_LinkedShaders[i])
         linked_shader[num_shaders++] = shader_program->_LinkedShaders[i];
   }

   for (unsigned i = 0; i < num_shaders; i++) {
      struct gl_linked_shader *shader = linked_shader[i];
      const nir_shader_compiler_options *options =
         ctx->screen->nir_options[shader->Stage];
      struct gl_program *prog = shader->Program;

      shader->Program->info.separate_shader = shader_program->SeparateShader;
      prog->state.type = PIPE_SHADER_IR_NIR;

      if (shader_program->data->spirv) {
         /* Parameters will be filled during NIR linking. */
         prog->Parameters = _mesa_new_parameter_list();
         prog->shader_program = shader_program;

         assert(!prog->nir);
         prog->nir = _mesa_spirv_to_nir(ctx, shader_program, shader->Stage, options);
      } else {
         assert(prog->nir);
         prog->nir->info.name =
            ralloc_asprintf(shader, "GLSL%d", shader_program->Name);
         if (shader_program->Label)
            prog->nir->info.label = ralloc_strdup(shader, shader_program->Label);
      }

      nir_shader_gather_info(prog->nir, nir_shader_get_entrypoint(prog->nir));
      if (!st->ctx->SoftFP64 && ((prog->nir->info.bit_sizes_int | prog->nir->info.bit_sizes_float) & 64) &&
          (options->lower_doubles_options & nir_lower_fp64_full_software) != 0) {

         /* It's not possible to use float64 on GLSL ES, so don't bother trying to
          * build the support code.  The support code depends on higher versions of
          * desktop GLSL, so it will fail to compile (below) anyway.
          */
         if (_mesa_is_desktop_gl(st->ctx) && st->ctx->Const.GLSLVersion >= 400)
            st->ctx->SoftFP64 = glsl_float64_funcs_to_nir(st->ctx, options);
      }
   }

   if (shader_program->data->spirv) {
      static const gl_nir_linker_options opts = {
         true /*fill_parameters */
      };
      if (!gl_nir_link_spirv(ctx->screen, &ctx->Const, &ctx->Extensions,
                             shader_program, &opts))
         return GL_FALSE;
   }

   for (unsigned i = 0; i < num_shaders; i++) {
      struct gl_program *prog = linked_shader[i]->Program;
      prog->ExternalSamplersUsed = gl_external_samplers(prog);
      _mesa_update_shader_textures_used(shader_program, prog);
   }

   nir_build_program_resource_list(&ctx->Const, shader_program,
                                   shader_program->data->spirv);

   for (unsigned i = 0; i < num_shaders; i++) {
      struct gl_linked_shader *shader = linked_shader[i];
      nir_shader *nir = shader->Program->nir;
      gl_shader_stage stage = shader->Stage;
      const struct gl_shader_compiler_options *options =
            &ctx->Const.ShaderCompilerOptions[stage];

      /* Since IO is lowered, we won't need the IO variables from now on.
       * nir_build_program_resource_list was the last pass that needed them.
       */
      NIR_PASS(_, nir, nir_remove_dead_variables,
               nir_var_shader_in | nir_var_shader_out, NULL);

      /* If there are forms of indirect addressing that the driver
       * cannot handle, perform the lowering pass.
       */
      if (options->EmitNoIndirectTemp || options->EmitNoIndirectUniform) {
         nir_variable_mode mode = (nir_variable_mode)0;

         mode |= options->EmitNoIndirectTemp ?
            nir_var_function_temp : (nir_variable_mode)0;
         mode |= options->EmitNoIndirectUniform ?
            nir_var_uniform | nir_var_mem_ubo | nir_var_mem_ssbo :
            (nir_variable_mode)0;

         if (mode)
            nir_lower_indirect_derefs(nir, mode, UINT32_MAX);
      }

      /* This needs to run after the initial pass of nir_lower_vars_to_ssa, so
       * that the buffer indices are constants in nir where they where
       * constants in GLSL. */
      NIR_PASS(_, nir, gl_nir_lower_buffers, shader_program);

      NIR_PASS(_, nir, st_nir_lower_wpos_ytransform, shader->Program,
               st->screen);

      /* needed to lower base_workgroup_id and base_global_invocation_id */
      struct nir_lower_compute_system_values_options cs_options = {};
      NIR_PASS(_, nir, nir_lower_system_values);
      NIR_PASS(_, nir, nir_lower_compute_system_values, &cs_options);
   }

   struct shader_info *prev_info = NULL;

   for (unsigned i = 0; i < num_shaders; i++) {
      struct gl_linked_shader *shader = linked_shader[i];
      struct shader_info *info = &shader->Program->nir->info;

      st_glsl_to_nir_post_opts(st, shader->Program, shader_program);

      if (prev_info &&
          ctx->screen->nir_options[shader->Stage]->unify_interfaces) {
         prev_info->outputs_written |= info->inputs_read &
            ~(VARYING_BIT_TESS_LEVEL_INNER | VARYING_BIT_TESS_LEVEL_OUTER);
         info->inputs_read |= prev_info->outputs_written &
            ~(VARYING_BIT_TESS_LEVEL_INNER | VARYING_BIT_TESS_LEVEL_OUTER);

         prev_info->patch_outputs_written |= info->patch_inputs_read;
         info->patch_inputs_read |= prev_info->patch_outputs_written;
      }
      prev_info = info;
   }

   /* Get TCS and TES shader info. */
   struct shader_info *tcs_info = NULL, *tes_info = NULL;

   for (unsigned i = 0; i < num_shaders; i++) {
      struct gl_linked_shader *shader = linked_shader[i];
      struct shader_info *info = &shader->Program->nir->info;

      if (info->stage == MESA_SHADER_TESS_CTRL)
         tcs_info = info;
      else if (info->stage == MESA_SHADER_TESS_EVAL)
         tes_info = info;
   }

   /* Copy some fields from TES to TCS shader info because some drivers
    * (radeonsi) need them in TCS.
    */
   if (tcs_info && tes_info) {
      tcs_info->tess._primitive_mode = tes_info->tess._primitive_mode;
      tcs_info->tess.spacing = tes_info->tess.spacing;
   }

   for (unsigned i = 0; i < num_shaders; i++) {
      struct gl_linked_shader *shader = linked_shader[i];
      struct gl_program *prog = shader->Program;

      /* Make sure that prog->info is in sync with nir->info, but st/mesa
       * expects some of the values to be from before lowering.
       */
      shader_info old_info = prog->info;
      prog->info = prog->nir->info;
      prog->info.name = old_info.name;
      prog->info.label = old_info.label;
      prog->info.num_ssbos = old_info.num_ssbos;
      prog->info.num_ubos = old_info.num_ubos;
      prog->info.num_abos = old_info.num_abos;

      if (prog->info.stage == MESA_SHADER_VERTEX) {
         prog->info.inputs_read = prog->nir->info.inputs_read;
         prog->DualSlotInputs = prog->nir->info.dual_slot_inputs;

         /* Initialize st_vertex_program members. */
         st_prepare_vertex_program(prog);
      }

      /* Get pipe_stream_output_info. */
      if (shader->Stage == MESA_SHADER_VERTEX ||
          shader->Stage == MESA_SHADER_TESS_EVAL ||
          shader->Stage == MESA_SHADER_GEOMETRY)
         st_translate_stream_output_info(prog);

      st_store_nir_in_disk_cache(st, prog);

      st_release_variants(st, prog);
      char *error = st_finalize_program(st, prog, true);

      if (error) {
         linker_error(shader_program, error);
         free(error);
         return false;
      }
   }

   struct pipe_context *pctx = st_context(ctx)->pipe;
   if (pctx->link_shader) {
      void *driver_handles[PIPE_SHADER_TYPES];
      memset(driver_handles, 0, sizeof(driver_handles));

      for (uint32_t i = 0; i < MESA_SHADER_STAGES; ++i) {
         struct gl_linked_shader *shader = shader_program->_LinkedShaders[i];
         if (shader) {
            struct gl_program *p = shader->Program;
            if (p && p->variants) {
               enum pipe_shader_type type = pipe_shader_type_from_mesa(shader->Stage);
               driver_handles[type] = p->variants->driver_shader;
            }
         }
      }

      pctx->link_shader(pctx, driver_handles);
   }

   return true;
}

void
st_nir_lower_samplers(struct pipe_screen *screen, nir_shader *nir,
                      struct gl_shader_program *shader_program,
                      struct gl_program *prog)
{
   if (screen->caps.nir_samplers_as_deref)
      NIR_PASS(_, nir, gl_nir_lower_samplers_as_deref, shader_program);
   else
      NIR_PASS(_, nir, gl_nir_lower_samplers, shader_program);

   if (prog) {
      BITSET_COPY(prog->info.textures_used, nir->info.textures_used);
      BITSET_COPY(prog->info.textures_used_by_txf, nir->info.textures_used_by_txf);
      BITSET_COPY(prog->info.samplers_used, nir->info.samplers_used);
      BITSET_COPY(prog->info.images_used, nir->info.images_used);
      BITSET_COPY(prog->info.image_buffers, nir->info.image_buffers);
      BITSET_COPY(prog->info.msaa_images, nir->info.msaa_images);
   }
}

static int
st_packed_uniforms_type_size(const struct glsl_type *type, bool bindless)
{
   return glsl_count_dword_slots(type, bindless);
}

static int
st_unpacked_uniforms_type_size(const struct glsl_type *type, bool bindless)
{
   return glsl_count_vec4_slots(type, false, bindless);
}

void
st_nir_lower_uniforms(struct st_context *st, nir_shader *nir)
{
   if (st->ctx->Const.PackedDriverUniformStorage) {
      NIR_PASS(_, nir, nir_lower_io, nir_var_uniform,
                 st_packed_uniforms_type_size,
                 (nir_lower_io_options)0);
   } else {
      NIR_PASS(_, nir, nir_lower_io, nir_var_uniform,
                 st_unpacked_uniforms_type_size,
                 (nir_lower_io_options)0);
   }

   if (nir->options->lower_uniforms_to_ubo)
      NIR_PASS(_, nir, nir_lower_uniforms_to_ubo,
                 st->ctx->Const.PackedDriverUniformStorage,
                 !st->ctx->Const.NativeIntegers);
}

/* Last third of preparing nir from glsl, which happens after shader
 * variant lowering.
 */
void
st_finalize_nir(struct st_context *st, struct gl_program *prog,
                struct gl_shader_program *shader_program, nir_shader *nir,
                bool is_before_variants, bool is_draw_shader)
{
   struct pipe_screen *screen = st->screen;

   MESA_TRACE_FUNC();

   const bool lower_tg4_offsets =
      !is_draw_shader && !st->screen->caps.texture_gather_offsets;

   if (!is_draw_shader && (st->lower_rect_tex || lower_tg4_offsets)) {
      struct nir_lower_tex_options opts = {0};
      opts.lower_rect = !!st->lower_rect_tex;
      opts.lower_tg4_offsets = lower_tg4_offsets;

      NIR_PASS(_, nir, nir_lower_tex, &opts);
   }

   st_nir_assign_uniform_locations(st->ctx, prog, nir);

   /* Set num_uniforms in number of attribute slots (vec4s) */
   nir->num_uniforms = DIV_ROUND_UP(prog->Parameters->NumParameterValues, 4);

   st_nir_lower_uniforms(st, nir);

   if (!is_draw_shader && is_before_variants && nir->options->lower_uniforms_to_ubo) {
      /* This must be done after uniforms are lowered to UBO and all
       * nir_var_uniform variables are removed from NIR to prevent conflicts
       * between state parameter merging and shader variant generation.
       */
      _mesa_optimize_state_parameters(&st->ctx->Const, prog->Parameters);
   }

   st_nir_lower_samplers(screen, nir, shader_program, prog);
   if (!is_draw_shader && !screen->caps.nir_images_as_deref)
      NIR_PASS(_, nir, gl_nir_lower_images, false);
}

/**
 * Link a GLSL shader program.  Called via glLinkProgram().
 */
void
st_link_shader(struct gl_context *ctx, struct gl_shader_program *prog)
{
   unsigned int i;
   bool spirv = false;

   MESA_TRACE_FUNC();

   _mesa_clear_shader_program_data(ctx, prog);

   prog->data = _mesa_create_shader_program_data();

   prog->data->LinkStatus = LINKING_SUCCESS;

   for (i = 0; i < prog->NumShaders; i++) {
      if (!prog->Shaders[i]->CompileStatus) {
	 linker_error(prog, "linking with uncompiled/unspecialized shader");
      }

      if (!i) {
         spirv = (prog->Shaders[i]->spirv_data != NULL);
      } else if (spirv && !prog->Shaders[i]->spirv_data) {
         /* The GL_ARB_gl_spirv spec adds a new bullet point to the list of
          * reasons LinkProgram can fail:
          *
          *    "All the shader objects attached to <program> do not have the
          *     same value for the SPIR_V_BINARY_ARB state."
          */
         linker_error(prog,
                      "not all attached shaders have the same "
                      "SPIR_V_BINARY_ARB state");
      }
   }
   prog->data->spirv = spirv;

   if (prog->data->LinkStatus) {
      if (!spirv) {
         link_shaders_init(ctx, prog);

#ifdef ENABLE_SHADER_CACHE
         shader_cache_read_program_metadata(ctx, prog);
#endif
      } else
         _mesa_spirv_link_shaders(ctx, prog);
   }

   /* If LinkStatus is LINKING_SUCCESS, then reset sampler validated to true.
    * Validation happens via the LinkShader call below. If LinkStatus is
    * LINKING_SKIPPED, then SamplersValidated will have been restored from the
    * shader cache.
    */
   if (prog->data->LinkStatus == LINKING_SUCCESS) {
      prog->SamplersValidated = GL_TRUE;
   }

   if (prog->data->LinkStatus && !st_link_glsl_to_nir(ctx, prog)) {
      prog->data->LinkStatus = LINKING_FAILURE;
   }

   if (prog->data->LinkStatus != LINKING_FAILURE)
      _mesa_create_program_resource_hash(prog);

   /* Return early if we are loading the shader from on-disk cache */
   if (prog->data->LinkStatus == LINKING_SKIPPED)
      return;

   if (ctx->_Shader->Flags & GLSL_DUMP) {
      if (!prog->data->LinkStatus) {
	 fprintf(stderr, "GLSL shader program %d failed to link\n", prog->Name);
      }

      if (prog->data->InfoLog && prog->data->InfoLog[0] != 0) {
	 fprintf(stderr, "GLSL shader program %d info log:\n", prog->Name);
         fprintf(stderr, "%s\n", prog->data->InfoLog);
      }
   }

#ifdef ENABLE_SHADER_CACHE
   if (prog->data->LinkStatus)
      shader_cache_write_program_metadata(ctx, prog);
#endif
}

} /* extern "C" */

/*
 * Copyright © 2013 Intel Corporation
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
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

/**
 * \file builtin_functions.cpp
 *
 * Support for GLSL built-in functions.
 *
 * This file is split into several main components:
 *
 * 1. Availability predicates
 *
 *    A series of small functions that check whether the current shader
 *    supports the version/extensions required to expose a built-in.
 *
 * 2. Core builtin_builder class functionality
 *
 * 3. Lists of built-in functions
 *
 *    The builtin_builder::create_builtins() function contains lists of all
 *    built-in function signatures, where they're available, what types they
 *    take, and so on.
 *
 * 4. Implementations of built-in function signatures
 *
 *    A series of functions which create ir_function_signatures and emit IR
 *    via ir_builder to implement them.
 *
 * 5. External API
 *
 *    A few functions the rest of the compiler can use to interact with the
 *    built-in function module.  For example, searching for a built-in by
 *    name and parameters.
 */


/**
 * Unfortunately, some versions of MinGW produce bad code if this file
 * is compiled with -O2 or -O3.  The resulting driver will crash in random
 * places if the app uses GLSL.
 * The work-around is to disable optimizations for just this file.  Luckily,
 * this code is basically just executed once.
 *
 * MinGW 4.6.3 (in Ubuntu 13.10) does not have this bug.
 * MinGW 5.3.1 (in Ubuntu 16.04) definitely has this bug.
 * MinGW 6.2.0 (in Ubuntu 16.10) definitely has this bug.
 * MinGW 7.3.0 (in Ubuntu 18.04) does not have this bug.  Assume versions before 7.3.x are buggy
 */

#include "util/detect_cc.h"

#if defined(__MINGW32__) && (DETECT_CC_GCC_VERSION < 703)
#warning "disabling optimizations for this file to work around compiler bug"
#pragma GCC optimize("O1")
#endif


#include <stdarg.h>
#include <stdio.h>
#include "util/simple_mtx.h"
#include "main/consts_exts.h"
#include "main/shader_types.h"
#include "main/shaderobj.h"
#include "ir_builder.h"
#include "glsl_parser_extras.h"
#include "program/prog_instruction.h"
#include <math.h>
#include "builtin_functions.h"
#include "util/hash_table.h"

#ifndef M_PIf
#define M_PIf   ((float) M_PI)
#endif
#ifndef M_PI_2f
#define M_PI_2f ((float) M_PI_2)
#endif
#ifndef M_PI_4f
#define M_PI_4f ((float) M_PI_4)
#endif

using namespace ir_builder;

static simple_mtx_t builtins_lock = SIMPLE_MTX_INITIALIZER;

/**
 * Availability predicates:
 *  @{
 */
static bool
always_available(const _mesa_glsl_parse_state *)
{
   return true;
}

static bool
compatibility_vs_only(const _mesa_glsl_parse_state *state)
{
   return state->stage == MESA_SHADER_VERTEX &&
          (state->compat_shader || state->ARB_compatibility_enable) &&
          !state->es_shader;
}

static bool
derivatives_only(const _mesa_glsl_parse_state *state)
{
   return state->stage == MESA_SHADER_FRAGMENT ||
          (state->stage == MESA_SHADER_COMPUTE &&
           state->NV_compute_shader_derivatives_enable);
}

static bool
gs_only(const _mesa_glsl_parse_state *state)
{
   return state->stage == MESA_SHADER_GEOMETRY;
}

/* For texture functions moved to compat profile in GLSL 4.20 */
static bool
deprecated_texture(const _mesa_glsl_parse_state *state)
{
   return state->compat_shader || !state->is_version(420, 0);
}

static bool
deprecated_texture_derivatives_only(const _mesa_glsl_parse_state *state)
{
   return (deprecated_texture(state) && derivatives_only(state)) || state->allow_vertex_texture_bias;
}

static bool
v110(const _mesa_glsl_parse_state *state)
{
   return !state->es_shader;
}

static bool
v110_deprecated_texture(const _mesa_glsl_parse_state *state)
{
   return !state->es_shader && deprecated_texture(state);
}

static bool
v110_derivatives_only_deprecated_texture(const _mesa_glsl_parse_state *state)
{
   return v110_deprecated_texture(state) &&
          derivatives_only(state);
}

static bool
v120(const _mesa_glsl_parse_state *state)
{
   return state->is_version(120, 300);
}

static bool
v130(const _mesa_glsl_parse_state *state)
{
   return state->is_version(130, 300);
}

static bool
v130_desktop(const _mesa_glsl_parse_state *state)
{
   return state->is_version(130, 0);
}

static bool
v460_desktop(const _mesa_glsl_parse_state *state)
{
   return state->is_version(460, 0);
}

static bool
v130_derivatives_only(const _mesa_glsl_parse_state *state)
{
   return state->is_version(130, 300) &&
          derivatives_only(state);
}

static bool
v140_or_es3(const _mesa_glsl_parse_state *state)
{
   return state->is_version(140, 300);
}

static bool
v400_derivatives_only(const _mesa_glsl_parse_state *state)
{
   return state->is_version(400, 0) &&
          derivatives_only(state);
}

static bool
texture_rectangle(const _mesa_glsl_parse_state *state)
{
   return state->ARB_texture_rectangle_enable;
}

static bool
texture_external(const _mesa_glsl_parse_state *state)
{
   return state->OES_EGL_image_external_enable;
}

static bool
texture_external_es3(const _mesa_glsl_parse_state *state)
{
   return state->OES_EGL_image_external_essl3_enable &&
      state->es_shader &&
      state->is_version(0, 300);
}

static bool
texture_shadow2Dext(const _mesa_glsl_parse_state *state)
{
   return state->es_shader &&
      state->EXT_shadow_samplers_enable;
}

/** True if texturing functions with explicit LOD are allowed. */
static bool
lod_exists_in_stage(const _mesa_glsl_parse_state *state)
{
   /* Texturing functions with "Lod" in their name exist:
    * - In the vertex shader stage (for all languages)
    * - In any stage for GLSL 1.30+ or GLSL ES 3.00
    * - In any stage for desktop GLSL with ARB_shader_texture_lod enabled.
    *
    * Since ARB_shader_texture_lod can only be enabled on desktop GLSL, we
    * don't need to explicitly check state->es_shader.
    */
   return state->stage == MESA_SHADER_VERTEX ||
          state->is_version(130, 300) ||
          state->ARB_shader_texture_lod_enable ||
          state->EXT_gpu_shader4_enable;
}

static bool
lod_deprecated_texture(const _mesa_glsl_parse_state *state)
{
   return deprecated_texture(state) && lod_exists_in_stage(state);
}

static bool
v110_lod_deprecated_texture(const _mesa_glsl_parse_state *state)
{
   return !state->es_shader && lod_deprecated_texture(state);
}

static bool
texture_buffer(const _mesa_glsl_parse_state *state)
{
   return state->is_version(140, 320) ||
      state->EXT_texture_buffer_enable ||
      state->OES_texture_buffer_enable;
}

static bool
shader_texture_lod(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shader_texture_lod_enable;
}

static bool
shader_texture_lod_and_rect(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shader_texture_lod_enable &&
          state->ARB_texture_rectangle_enable;
}

static bool
shader_bit_encoding(const _mesa_glsl_parse_state *state)
{
   return state->is_version(330, 300) ||
          state->ARB_shader_bit_encoding_enable ||
          state->ARB_gpu_shader5_enable;
}

static bool
shader_integer_mix(const _mesa_glsl_parse_state *state)
{
   return state->is_version(450, 310) ||
          state->ARB_ES3_1_compatibility_enable ||
          (v130(state) && state->EXT_shader_integer_mix_enable);
}

static bool
shader_packing_or_es3(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shading_language_packing_enable ||
          state->is_version(420, 300);
}

static bool
shader_packing_or_es3_or_gpu_shader5(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shading_language_packing_enable ||
          state->ARB_gpu_shader5_enable ||
          state->is_version(400, 300);
}

static bool
gpu_shader4(const _mesa_glsl_parse_state *state)
{
   return state->EXT_gpu_shader4_enable;
}

static bool
gpu_shader4_integer(const _mesa_glsl_parse_state *state)
{
   return state->EXT_gpu_shader4_enable &&
          state->exts->EXT_texture_integer;
}

static bool
gpu_shader4_array(const _mesa_glsl_parse_state *state)
{
   return state->EXT_gpu_shader4_enable &&
          state->exts->EXT_texture_array;
}

static bool
gpu_shader4_array_integer(const _mesa_glsl_parse_state *state)
{
   return gpu_shader4_array(state) &&
          state->exts->EXT_texture_integer;
}

static bool
gpu_shader4_rect(const _mesa_glsl_parse_state *state)
{
   return state->EXT_gpu_shader4_enable &&
          state->exts->NV_texture_rectangle;
}

static bool
gpu_shader4_rect_integer(const _mesa_glsl_parse_state *state)
{
   return gpu_shader4_rect(state) &&
          state->exts->EXT_texture_integer;
}

static bool
gpu_shader4_tbo(const _mesa_glsl_parse_state *state)
{
   return state->EXT_gpu_shader4_enable &&
          state->exts->EXT_texture_buffer_object;
}

static bool
gpu_shader4_tbo_integer(const _mesa_glsl_parse_state *state)
{
   return gpu_shader4_tbo(state) &&
          state->exts->EXT_texture_integer;
}

static bool
gpu_shader4_derivs_only(const _mesa_glsl_parse_state *state)
{
   return state->EXT_gpu_shader4_enable &&
          derivatives_only(state);
}

static bool
gpu_shader4_integer_derivs_only(const _mesa_glsl_parse_state *state)
{
   return gpu_shader4_derivs_only(state) &&
          state->exts->EXT_texture_integer;
}

static bool
gpu_shader4_array_derivs_only(const _mesa_glsl_parse_state *state)
{
   return gpu_shader4_derivs_only(state) &&
          state->exts->EXT_texture_array;
}

static bool
gpu_shader4_array_integer_derivs_only(const _mesa_glsl_parse_state *state)
{
   return gpu_shader4_array_derivs_only(state) &&
          state->exts->EXT_texture_integer;
}

static bool
v130_or_gpu_shader4(const _mesa_glsl_parse_state *state)
{
   return state->is_version(130, 300) || state->EXT_gpu_shader4_enable;
}

static bool
v130_or_gpu_shader4_and_tex_shadow_lod(const _mesa_glsl_parse_state *state)
{
   return v130_or_gpu_shader4(state) &&
          state->EXT_texture_shadow_lod_enable;
}

static bool
gpu_shader5(const _mesa_glsl_parse_state *state)
{
   return state->is_version(400, 0) || state->ARB_gpu_shader5_enable;
}

static bool
gpu_shader5_es(const _mesa_glsl_parse_state *state)
{
   return state->is_version(400, 320) ||
          state->ARB_gpu_shader5_enable ||
          state->EXT_gpu_shader5_enable ||
          state->OES_gpu_shader5_enable;
}

static bool
gpu_shader5_or_OES_texture_cube_map_array(const _mesa_glsl_parse_state *state)
{
   return state->is_version(400, 320) ||
          state->ARB_gpu_shader5_enable ||
          state->EXT_texture_cube_map_array_enable ||
          state->OES_texture_cube_map_array_enable;
}

static bool
es31_not_gs5(const _mesa_glsl_parse_state *state)
{
   return state->is_version(0, 310) && !gpu_shader5_es(state);
}

static bool
gpu_shader5_or_es31(const _mesa_glsl_parse_state *state)
{
   return state->is_version(400, 310) || state->ARB_gpu_shader5_enable;
}

static bool
shader_packing_or_es31_or_gpu_shader5(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shading_language_packing_enable ||
          state->ARB_gpu_shader5_enable ||
          state->is_version(400, 310);
}

static bool
gpu_shader5_or_es31_or_integer_functions(const _mesa_glsl_parse_state *state)
{
   return gpu_shader5_or_es31(state) ||
          state->MESA_shader_integer_functions_enable;
}

static bool
gpu_shader_half_float(const _mesa_glsl_parse_state *state)
{
   return state->AMD_gpu_shader_half_float_enable;
}

static bool
fs_interpolate_at(const _mesa_glsl_parse_state *state)
{
   return state->stage == MESA_SHADER_FRAGMENT &&
          (state->is_version(400, 320) ||
           state->ARB_gpu_shader5_enable ||
           state->OES_shader_multisample_interpolation_enable);
}

static bool
fs_half_float_interpolate_at(const _mesa_glsl_parse_state *state)
{
   return fs_interpolate_at(state) && gpu_shader_half_float(state);
}

static bool
texture_array_lod(const _mesa_glsl_parse_state *state)
{
   return lod_exists_in_stage(state) &&
          (state->EXT_texture_array_enable ||
           (state->EXT_gpu_shader4_enable &&
            state->exts->EXT_texture_array));
}

static bool
texture_array(const _mesa_glsl_parse_state *state)
{
   return state->EXT_texture_array_enable ||
          (state->EXT_gpu_shader4_enable &&
           state->exts->EXT_texture_array);
}

static bool
texture_array_derivs_only(const _mesa_glsl_parse_state *state)
{
   return derivatives_only(state) &&
          texture_array(state);
}

static bool
texture_multisample(const _mesa_glsl_parse_state *state)
{
   return state->is_version(150, 310) ||
          state->ARB_texture_multisample_enable;
}

static bool
texture_multisample_array(const _mesa_glsl_parse_state *state)
{
   return state->is_version(150, 320) ||
          state->ARB_texture_multisample_enable ||
          state->OES_texture_storage_multisample_2d_array_enable;
}

static bool
texture_samples_identical(const _mesa_glsl_parse_state *state)
{
   return texture_multisample(state) &&
          state->EXT_shader_samples_identical_enable;
}

static bool
texture_samples_identical_array(const _mesa_glsl_parse_state *state)
{
   return texture_multisample_array(state) &&
          state->EXT_shader_samples_identical_enable;
}

static bool
derivatives_texture_cube_map_array(const _mesa_glsl_parse_state *state)
{
   return state->has_texture_cube_map_array() &&
          derivatives_only(state);
}

static bool
texture_cube_map_array(const _mesa_glsl_parse_state *state)
{
   return state->has_texture_cube_map_array();
}

static bool
v130_or_gpu_shader4_and_tex_cube_map_array(const _mesa_glsl_parse_state *state)
{
   return texture_cube_map_array(state) &&
          v130_or_gpu_shader4(state) &&
          state->EXT_texture_shadow_lod_enable;
}

static bool
texture_query_levels(const _mesa_glsl_parse_state *state)
{
   return state->is_version(430, 0) ||
          state->ARB_texture_query_levels_enable;
}

static bool
texture_query_lod(const _mesa_glsl_parse_state *state)
{
   return derivatives_only(state) &&
          (state->ARB_texture_query_lod_enable ||
           state->EXT_texture_query_lod_enable);
}

static bool
texture_gather_cube_map_array(const _mesa_glsl_parse_state *state)
{
   return state->is_version(400, 320) ||
          state->ARB_texture_gather_enable ||
          state->ARB_gpu_shader5_enable ||
          state->EXT_texture_cube_map_array_enable ||
          state->OES_texture_cube_map_array_enable;
}

static bool
texture_texture4(const _mesa_glsl_parse_state *state)
{
   return state->AMD_texture_texture4_enable;
}

static bool
texture_gather_or_es31(const _mesa_glsl_parse_state *state)
{
   return state->is_version(400, 310) ||
          state->ARB_texture_gather_enable ||
          state->ARB_gpu_shader5_enable;
}

/* Only ARB_texture_gather but not GLSL 4.0 or ARB_gpu_shader5.
 * used for relaxation of const offset requirements.
 */
static bool
texture_gather_only_or_es31(const _mesa_glsl_parse_state *state)
{
   return !state->is_version(400, 320) &&
          !state->ARB_gpu_shader5_enable &&
          !state->EXT_gpu_shader5_enable &&
          !state->OES_gpu_shader5_enable &&
          (state->ARB_texture_gather_enable ||
           state->is_version(0, 310));
}

/* Desktop GL or OES_standard_derivatives */
static bool
derivatives(const _mesa_glsl_parse_state *state)
{
   return derivatives_only(state) &&
          (state->is_version(110, 300) ||
           state->OES_standard_derivatives_enable ||
           state->consts->AllowGLSLRelaxedES);
}

static bool
derivative_control(const _mesa_glsl_parse_state *state)
{
   return derivatives_only(state) &&
          (state->is_version(450, 0) ||
           state->ARB_derivative_control_enable);
}

static bool
half_float_derivatives(const _mesa_glsl_parse_state *state)
{
   return derivatives(state) && gpu_shader_half_float(state);
}

static bool
half_float_derivative_control(const _mesa_glsl_parse_state *state)
{
   return derivative_control(state) && gpu_shader_half_float(state);
}

/** True if sampler3D exists */
static bool
tex3d(const _mesa_glsl_parse_state *state)
{
   /* sampler3D exists in all desktop GLSL versions, GLSL ES 1.00 with the
    * OES_texture_3D extension, and in GLSL ES 3.00.
    */
   return (!state->es_shader ||
           state->OES_texture_3D_enable ||
           state->language_version >= 300) && deprecated_texture(state);
}

static bool
derivatives_tex3d(const _mesa_glsl_parse_state *state)
{
   return (!state->es_shader || state->OES_texture_3D_enable) &&
          derivatives_only(state) && deprecated_texture(state);
}

static bool
tex3d_lod(const _mesa_glsl_parse_state *state)
{
   return tex3d(state) && lod_exists_in_stage(state);
}

static bool
shader_atomic_counters(const _mesa_glsl_parse_state *state)
{
   return state->has_atomic_counters();
}

static bool
shader_atomic_counter_ops(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shader_atomic_counter_ops_enable;
}

static bool
shader_atomic_counter_ops_or_v460_desktop(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shader_atomic_counter_ops_enable || v460_desktop(state);
}

static bool
shader_ballot(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shader_ballot_enable || state->KHR_shader_subgroup_ballot_enable;
}

static bool
ballot_arb(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shader_ballot_enable;
}

static bool
ballot_khr(const _mesa_glsl_parse_state *state)
{
   return state->KHR_shader_subgroup_ballot_enable;
}

static bool
supports_arb_fragment_shader_interlock(const _mesa_glsl_parse_state *state)
{
   return state->ARB_fragment_shader_interlock_enable;
}

static bool
supports_nv_fragment_shader_interlock(const _mesa_glsl_parse_state *state)
{
   return state->NV_fragment_shader_interlock_enable;
}

static bool
shader_clock(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shader_clock_enable ||
          state->EXT_shader_clock_enable;
}

static bool
shader_clock_int64(const _mesa_glsl_parse_state *state)
{
   return (state->ARB_shader_clock_enable ||
           state->EXT_shader_clock_enable) &&
          (state->ARB_gpu_shader_int64_enable ||
           state->AMD_gpu_shader_int64_enable);
}

static bool
shader_clock_realtime(const _mesa_glsl_parse_state *state)
{
   return state->EXT_shader_realtime_clock_enable;
}

static bool
shader_clock_realtime_int64(const _mesa_glsl_parse_state *state)
{
   return state->EXT_shader_realtime_clock_enable &&
          state->ARB_gpu_shader_int64_enable;
}

static bool
shader_storage_buffer_object(const _mesa_glsl_parse_state *state)
{
   return state->has_shader_storage_buffer_objects();
}

static bool
shader_trinary_minmax(const _mesa_glsl_parse_state *state)
{
   return state->AMD_shader_trinary_minmax_enable;
}

static bool
shader_trinary_minmax_half_float(const _mesa_glsl_parse_state *state)
{
   return shader_trinary_minmax(state) && gpu_shader_half_float(state);
}

static bool
shader_image_load_store(const _mesa_glsl_parse_state *state)
{
   return (state->is_version(420, 310) ||
           state->ARB_shader_image_load_store_enable ||
           state->EXT_shader_image_load_store_enable);
}

static bool
shader_image_load_store_ext(const _mesa_glsl_parse_state *state)
{
   return state->EXT_shader_image_load_store_enable;
}

static bool
shader_image_atomic(const _mesa_glsl_parse_state *state)
{
   return (state->is_version(420, 320) ||
           state->ARB_shader_image_load_store_enable ||
           state->EXT_shader_image_load_store_enable ||
           state->OES_shader_image_atomic_enable);
}

static bool
shader_image_atomic_exchange_float(const _mesa_glsl_parse_state *state)
{
   return (state->is_version(450, 320) ||
           state->ARB_ES3_1_compatibility_enable ||
           state->OES_shader_image_atomic_enable ||
           state->NV_shader_atomic_float_enable);
}

static bool
shader_image_atomic_add_float(const _mesa_glsl_parse_state *state)
{
   return state->NV_shader_atomic_float_enable;
}

static bool
shader_image_size(const _mesa_glsl_parse_state *state)
{
   return state->is_version(430, 310) ||
           state->ARB_shader_image_size_enable;
}

static bool
shader_samples(const _mesa_glsl_parse_state *state)
{
   return state->is_version(450, 0) ||
          state->ARB_shader_texture_image_samples_enable;
}

static bool
gs_streams(const _mesa_glsl_parse_state *state)
{
   return gpu_shader5(state) && gs_only(state);
}

static bool
fp64(const _mesa_glsl_parse_state *state)
{
   return state->has_double();
}

static bool
int64_avail(const _mesa_glsl_parse_state *state)
{
   return state->has_int64();
}

static bool
int64_fp64(const _mesa_glsl_parse_state *state)
{
   return state->has_int64() && state->has_double();
}

static bool
shader_ballot_and_fp64(const _mesa_glsl_parse_state *state)
{
   return shader_ballot(state) && fp64(state);
}

static bool
ballot_khr_and_fp64(const _mesa_glsl_parse_state *state)
{
   return ballot_khr(state) && fp64(state);
}

static bool
compute_shader(const _mesa_glsl_parse_state *state)
{
   return state->stage == MESA_SHADER_COMPUTE;
}

static bool
compute_shader_supported(const _mesa_glsl_parse_state *state)
{
   return state->has_compute_shader();
}

static bool
buffer_atomics_supported(const _mesa_glsl_parse_state *state)
{
   return compute_shader(state) || shader_storage_buffer_object(state);
}

static bool
buffer_int64_atomics_supported(const _mesa_glsl_parse_state *state)
{
   return state->NV_shader_atomic_int64_enable &&
      buffer_atomics_supported(state);
}

static bool
barrier_supported(const _mesa_glsl_parse_state *state)
{
   return compute_shader(state) ||
          state->stage == MESA_SHADER_TESS_CTRL;
}

static bool
vote(const _mesa_glsl_parse_state *state)
{
   return state->ARB_shader_group_vote_enable;
}

static bool
vote_ext(const _mesa_glsl_parse_state *state)
{
   return state->EXT_shader_group_vote_enable;
}

static bool
vote_khr(const _mesa_glsl_parse_state *state)
{
   return state->KHR_shader_subgroup_vote_enable;
}

static bool
vote_khr_and_fp64(const _mesa_glsl_parse_state *state)
{
   return vote_khr(state) && fp64(state);
}

static bool
vote_or_v460_desktop(const _mesa_glsl_parse_state *state)
{
   return state->KHR_shader_subgroup_vote_enable ||
          state->EXT_shader_group_vote_enable ||
          state->ARB_shader_group_vote_enable ||
          v460_desktop(state);
}

static bool
vote_or_v460_desktop_and_fp64(const _mesa_glsl_parse_state *state)
{
   return vote_or_v460_desktop(state) && fp64(state);
}

static bool
NV_shader_atomic_float_supported(const _mesa_glsl_parse_state *state)
{
   return state->extensions->NV_shader_atomic_float;
}

static bool
shader_atomic_float_add(const _mesa_glsl_parse_state *state)
{
   return state->NV_shader_atomic_float_enable;
}

static bool
shader_atomic_float_exchange(const _mesa_glsl_parse_state *state)
{
   return state->NV_shader_atomic_float_enable ||
          state->INTEL_shader_atomic_float_minmax_enable;
}

static bool
INTEL_shader_atomic_float_minmax_supported(const _mesa_glsl_parse_state *state)
{
   return state->extensions->INTEL_shader_atomic_float_minmax;
}

static bool
shader_atomic_float_minmax(const _mesa_glsl_parse_state *state)
{
   return state->INTEL_shader_atomic_float_minmax_enable;
}

static bool
demote_to_helper_invocation(const _mesa_glsl_parse_state *state)
{
   return state->EXT_demote_to_helper_invocation_enable;
}

static bool
shader_integer_functions2(const _mesa_glsl_parse_state *state)
{
   return state->INTEL_shader_integer_functions2_enable;
}

static bool
shader_integer_functions2_int64(const _mesa_glsl_parse_state *state)
{
   return state->INTEL_shader_integer_functions2_enable && state->has_int64();
}

static bool
sparse_enabled(const _mesa_glsl_parse_state *state)
{
   return state->ARB_sparse_texture2_enable;
}

static bool
v130_desktop_and_sparse(const _mesa_glsl_parse_state *state)
{
   return v130_desktop(state) && state->ARB_sparse_texture2_enable;
}

static bool
texture_cube_map_array_and_sparse(const _mesa_glsl_parse_state *state)
{
   return texture_cube_map_array(state) && state->ARB_sparse_texture2_enable;
}

static bool
v130_derivatives_only_and_sparse(const _mesa_glsl_parse_state *state)
{
   return v130_derivatives_only(state) && state->ARB_sparse_texture2_enable;
}

static bool
derivatives_texture_cube_map_array_and_sparse(const _mesa_glsl_parse_state *state)
{
   return derivatives_texture_cube_map_array(state) && state->ARB_sparse_texture2_enable;
}

static bool
texture_gather_and_sparse(const _mesa_glsl_parse_state *state)
{
   return (gpu_shader5(state) || state->ARB_texture_gather_enable) &&
      state->ARB_sparse_texture2_enable;
}

static bool
gpu_shader5_and_sparse(const _mesa_glsl_parse_state *state)
{
   return gpu_shader5(state) && state->ARB_sparse_texture2_enable;
}

static bool
texture_multisample_and_sparse(const _mesa_glsl_parse_state *state)
{
   return texture_multisample(state) &&
      state->ARB_sparse_texture2_enable;
}

static bool
texture_multisample_array_and_sparse(const _mesa_glsl_parse_state *state)
{
   return texture_multisample_array(state) &&
      state->ARB_sparse_texture2_enable;
}

static bool
shader_image_load_store_and_sparse(const _mesa_glsl_parse_state *state)
{
   return shader_image_load_store(state) &&
      state->ARB_sparse_texture2_enable;
}

static bool
v130_desktop_and_clamp(const _mesa_glsl_parse_state *state)
{
   return v130_desktop(state) && state->ARB_sparse_texture_clamp_enable;
}

static bool
texture_cube_map_array_and_clamp(const _mesa_glsl_parse_state *state)
{
   return texture_cube_map_array(state) && state->ARB_sparse_texture_clamp_enable;
}

static bool
v130_derivatives_only_and_clamp(const _mesa_glsl_parse_state *state)
{
   return v130_derivatives_only(state) && state->ARB_sparse_texture_clamp_enable;
}

static bool
derivatives_texture_cube_map_array_and_clamp(const _mesa_glsl_parse_state *state)
{
   return derivatives_texture_cube_map_array(state) && state->ARB_sparse_texture_clamp_enable;
}

static bool
subgroup_basic(const _mesa_glsl_parse_state *state)
{
   return state->KHR_shader_subgroup_basic_enable;
}

static bool
compute_shader_and_subgroup_basic(const _mesa_glsl_parse_state *state)
{
   return state->stage == MESA_SHADER_COMPUTE && state->KHR_shader_subgroup_basic_enable;
}

static bool
subgroup_shuffle(const _mesa_glsl_parse_state *state)
{
   return state->KHR_shader_subgroup_shuffle_enable;
}

static bool
subgroup_shuffle_and_fp64(const _mesa_glsl_parse_state *state)
{
   return subgroup_shuffle(state) && fp64(state);
}

static bool
subgroup_shuffle_relative(const _mesa_glsl_parse_state *state)
{
   return state->KHR_shader_subgroup_shuffle_relative_enable;
}

static bool
subgroup_shuffle_relative_and_fp64(const _mesa_glsl_parse_state *state)
{
   return subgroup_shuffle_relative(state) && fp64(state);
}

static bool
subgroup_arithmetic(const _mesa_glsl_parse_state *state)
{
   return state->KHR_shader_subgroup_arithmetic_enable;
}

static bool
subgroup_arithmetic_and_fp64(const _mesa_glsl_parse_state *state)
{
   return subgroup_arithmetic(state) && fp64(state);
}

static bool
subgroup_clustered(const _mesa_glsl_parse_state *state)
{
   return state->KHR_shader_subgroup_clustered_enable;
}

static bool
subgroup_clustered_and_fp64(const _mesa_glsl_parse_state *state)
{
   return subgroup_clustered(state) && fp64(state);
}

static bool
subgroup_quad(const _mesa_glsl_parse_state *state)
{
   return state->KHR_shader_subgroup_quad_enable;
}

static bool
subgroup_quad_and_fp64(const _mesa_glsl_parse_state *state)
{
   return subgroup_quad(state) && fp64(state);
}

/** @} */

/******************************************************************************/

namespace {

/**
 * builtin_builder: A singleton object representing the core of the built-in
 * function module.
 *
 * It generates IR for every built-in function signature, and organizes them
 * into functions.
 */
class builtin_builder {
public:
   builtin_builder();
   builtin_builder(const builtin_builder &) = delete;
   ~builtin_builder();
   builtin_builder & operator=(const builtin_builder &) = delete;

   void initialize();
   void release();
   ir_function_signature *find(_mesa_glsl_parse_state *state,
                               const char *name, ir_exec_list *actual_parameters);

   /**
    * A symbol table to hold all the built-in signatures; created by this
    * module.
    *
    * This includes signatures for every built-in, regardless of version or
    * enabled extensions.  The availability predicate associated with each
    * signature allows matching_signature() to filter out the irrelevant ones.
    */
   struct glsl_symbol_table *symbols;

private:
   void *mem_ctx;

   void create_shader();
   void create_intrinsics();
   void create_builtins();

   /**
    * IR builder helpers:
    *
    * These convenience functions assist in emitting IR, but don't necessarily
    * fit in ir_builder itself.  Many of them rely on having a mem_ctx class
    * member available.
    */
   ir_variable *in_var(const glsl_type *type, const char *name);
   ir_variable *in_mediump_var(const glsl_type *type, const char *name);
   ir_variable *in_highp_var(const glsl_type *type, const char *name);
   ir_variable *out_var(const glsl_type *type, const char *name);
   ir_variable *out_lowp_var(const glsl_type *type, const char *name);
   ir_variable *out_highp_var(const glsl_type *type, const char *name);
   ir_variable *as_highp(ir_factory &body, ir_variable *var);
   ir_constant *imm(float16_t f16, unsigned vector_elements=1);
   ir_constant *imm(float f, unsigned vector_elements=1);
   ir_constant *imm(bool b, unsigned vector_elements=1);
   ir_constant *imm(int i, unsigned vector_elements=1);
   ir_constant *imm(unsigned u, unsigned vector_elements=1);
   ir_constant *imm(double d, unsigned vector_elements=1);
   ir_constant *imm(const glsl_type *type, const ir_constant_data &);
   ir_dereference_variable *var_ref(ir_variable *var);
   ir_dereference_array *array_ref(ir_variable *var, int i);
   ir_swizzle *matrix_elt(ir_variable *var, int col, int row);
   ir_dereference_record *record_ref(ir_variable *var, const char *field);

   ir_expression *asin_expr(ir_variable *x, float p0, float p1);

   /**
    * Call function \param f with parameters specified as the linked
    * list \param params of \c ir_variable objects.  \param ret should
    * point to the ir_variable that will hold the function return
    * value, or be \c NULL if the function has void return type.
    */
   ir_call *call(ir_function *f, ir_variable *ret, ir_exec_list params);

   /** Create a new function and add the given signatures. */
   void add_function(const char *name, ...);

   typedef ir_function_signature *(builtin_builder::*image_prototype_ctr)(const glsl_type *image_type,
                                                                          unsigned num_arguments,
                                                                          unsigned flags);

   /**
    * Create a new image built-in function for all known image types.
    * \p flags is a bitfield of \c image_function_flags flags.
    */
   void add_image_function(const char *name,
                           const char *intrinsic_name,
                           image_prototype_ctr prototype,
                           unsigned num_arguments,
                           unsigned flags,
                           enum ir_intrinsic_id id);

   /**
    * Create new functions for all known image built-ins and types.
    * If \p glsl is \c true, use the GLSL built-in names and emit code
    * to call into the actual compiler intrinsic.  If \p glsl is
    * false, emit a function prototype with no body for each image
    * intrinsic name.
    */
   void add_image_functions(bool glsl);

   ir_function_signature *new_sig(const glsl_type *return_type,
                                  builtin_available_predicate avail,
                                  int num_params, ...);

   /**
    * Function signature generators:
    *  @{
    */
   ir_function_signature *unop(builtin_available_predicate avail,
                               ir_expression_operation opcode,
                               const glsl_type *return_type,
                               const glsl_type *param_type);
   ir_function_signature *unop_precision(builtin_available_predicate avail,
                                         ir_expression_operation opcode,
                                         const glsl_type *return_type,
                                         const glsl_type *param_type, uint32_t precision);
   ir_function_signature *binop(builtin_available_predicate avail,
                                ir_expression_operation opcode,
                                const glsl_type *return_type,
                                const glsl_type *param0_type,
                                const glsl_type *param1_type,
                                bool swap_operands = false);

#define B0(X) ir_function_signature *_##X();
#define B1(X) ir_function_signature *_##X(const glsl_type *);
#define B2(X) ir_function_signature *_##X(const glsl_type *, const glsl_type *);
#define B3(X) ir_function_signature *_##X(const glsl_type *, const glsl_type *, const glsl_type *);
#define BA1(X) ir_function_signature *_##X(builtin_available_predicate, const glsl_type *);
#define BA2(X) ir_function_signature *_##X(builtin_available_predicate, const glsl_type *, const glsl_type *);
   BA1(radians)
   BA1(degrees)
   BA1(sin)
   BA1(cos)
   BA1(tan)
   BA1(asin)
   BA1(acos)
   BA1(atan2)
   BA1(atan)
   BA1(sinh)
   BA1(cosh)
   BA1(tanh)
   BA1(asinh)
   BA1(acosh)
   BA1(atanh)
   BA1(pow)
   BA1(exp)
   BA1(log)
   BA1(exp2)
   BA1(log2)
   BA1(sqrt)
   BA1(inversesqrt)
   BA1(abs)
   BA1(sign)
   BA1(floor)
   BA1(truncate)
   BA1(trunc)
   BA1(round)
   BA1(roundEven)
   BA1(ceil)
   BA1(fract)
   BA2(mod)
   BA1(modf)
   BA2(min)
   BA2(max)
   BA2(clamp)
   BA2(mix_lrp)
   ir_function_signature *_mix_sel(builtin_available_predicate avail,
                                   const glsl_type *val_type,
                                   const glsl_type *blend_type);
   BA2(step)
   BA2(smoothstep)
   BA1(isnan)
   BA1(isinf)
   B1(floatBitsToInt)
   B1(floatBitsToUint)
   B1(intBitsToFloat)
   B1(uintBitsToFloat)

   BA1(doubleBitsToInt64)
   BA1(doubleBitsToUint64)
   BA1(int64BitsToDouble)
   BA1(uint64BitsToDouble)

   ir_function_signature *_packUnorm2x16(builtin_available_predicate avail);
   ir_function_signature *_packSnorm2x16(builtin_available_predicate avail);
   ir_function_signature *_packUnorm4x8(builtin_available_predicate avail);
   ir_function_signature *_packSnorm4x8(builtin_available_predicate avail);
   ir_function_signature *_unpackUnorm2x16(builtin_available_predicate avail);
   ir_function_signature *_unpackSnorm2x16(builtin_available_predicate avail);
   ir_function_signature *_unpackUnorm4x8(builtin_available_predicate avail);
   ir_function_signature *_unpackSnorm4x8(builtin_available_predicate avail);
   ir_function_signature *_packHalf2x16(builtin_available_predicate avail);
   ir_function_signature *_unpackHalf2x16(builtin_available_predicate avail);
   ir_function_signature *_packFloat2x16(builtin_available_predicate avail);
   ir_function_signature *_unpackFloat2x16(builtin_available_predicate avail);
   ir_function_signature *_packDouble2x32(builtin_available_predicate avail);
   ir_function_signature *_unpackDouble2x32(builtin_available_predicate avail);
   ir_function_signature *_packInt2x32(builtin_available_predicate avail);
   ir_function_signature *_unpackInt2x32(builtin_available_predicate avail);
   ir_function_signature *_packUint2x32(builtin_available_predicate avail);
   ir_function_signature *_unpackUint2x32(builtin_available_predicate avail);

   BA1(length)
   BA1(distance);
   BA1(dot);
   BA1(cross);
   BA1(normalize);
   B0(ftransform);
   BA1(faceforward);
   BA1(reflect);
   BA1(refract);
   BA1(matrixCompMult);
   BA1(outerProduct);
   BA1(determinant_mat2);
   BA1(determinant_mat3);
   BA1(determinant_mat4);
   BA1(inverse_mat2);
   BA1(inverse_mat3);
   BA1(inverse_mat4);
   BA1(transpose);
   BA1(lessThan);
   BA1(lessThanEqual);
   BA1(greaterThan);
   BA1(greaterThanEqual);
   BA1(equal);
   BA1(notEqual);
   B1(any);
   B1(all);
   B1(not);
   BA2(textureSize);
   BA1(textureSamples);

   B0(is_sparse_texels_resident);
   B0(is_sparse_texels_resident_intrinsic);

/** Flags to _texture() */
#define TEX_PROJECT 1
#define TEX_OFFSET  2
#define TEX_COMPONENT 4
#define TEX_OFFSET_NONCONST 8
#define TEX_OFFSET_ARRAY 16
#define TEX_SPARSE 32
#define TEX_CLAMP 64

   ir_function_signature *_texture(ir_texture_opcode opcode,
                                   builtin_available_predicate avail,
                                   const glsl_type *return_type,
                                   const glsl_type *sampler_type,
                                   const glsl_type *coord_type,
                                   int flags = 0);
   ir_function_signature *_textureCubeArrayShadow(ir_texture_opcode opcode,
                                                  builtin_available_predicate avail,
                                                  const glsl_type *x,
                                                  int flags = 0);
   ir_function_signature *_texelFetch(builtin_available_predicate avail,
                                      const glsl_type *return_type,
                                      const glsl_type *sampler_type,
                                      const glsl_type *coord_type,
                                      const glsl_type *offset_type = NULL,
                                      bool sparse = false);

   B0(EmitVertex)
   B0(EndPrimitive)
   ir_function_signature *_EmitStreamVertex(builtin_available_predicate avail,
                                            const glsl_type *stream_type);
   ir_function_signature *_EndStreamPrimitive(builtin_available_predicate avail,
                                              const glsl_type *stream_type);
   B0(barrier)

   BA2(textureQueryLod);
   BA1(textureQueryLevels);
   BA2(textureSamplesIdentical);
   BA1(dFdx);
   BA1(dFdy);
   BA1(fwidth);
   BA1(dFdxCoarse);
   BA1(dFdyCoarse);
   BA1(fwidthCoarse);
   BA1(dFdxFine);
   BA1(dFdyFine);
   BA1(fwidthFine);
   B1(noise1);
   B1(noise2);
   B1(noise3);
   B1(noise4);

   B1(bitfieldExtract)
   B1(bitfieldInsert)
   B1(bitfieldReverse)
   B1(bitCount)
   B1(findLSB)
   B1(findMSB)
   BA1(countLeadingZeros)
   BA1(countTrailingZeros)
   BA1(fma)
   B2(ldexp)
   B2(frexp)
   B2(dfrexp)
   B1(uaddCarry)
   B1(usubBorrow)
   BA1(addSaturate)
   BA1(subtractSaturate)
   BA1(absoluteDifference)
   BA1(average)
   BA1(averageRounded)
   B1(mulExtended)
   BA1(multiply32x16)
   BA1(interpolateAtCentroid)
   BA1(interpolateAtOffset)
   BA1(interpolateAtSample)

   ir_function_signature *_atomic_counter_intrinsic(builtin_available_predicate avail,
                                                    enum ir_intrinsic_id id);
   ir_function_signature *_atomic_counter_intrinsic1(builtin_available_predicate avail,
                                                     enum ir_intrinsic_id id);
   ir_function_signature *_atomic_counter_intrinsic2(builtin_available_predicate avail,
                                                     enum ir_intrinsic_id id);
   ir_function_signature *_atomic_counter_op(const char *intrinsic,
                                             builtin_available_predicate avail);
   ir_function_signature *_atomic_counter_op1(const char *intrinsic,
                                              builtin_available_predicate avail);
   ir_function_signature *_atomic_counter_op2(const char *intrinsic,
                                              builtin_available_predicate avail);

   ir_function_signature *_atomic_intrinsic2(builtin_available_predicate avail,
                                             const glsl_type *type,
                                             enum ir_intrinsic_id id);
   ir_function_signature *_atomic_op2(const char *intrinsic,
                                      builtin_available_predicate avail,
                                      const glsl_type *type);
   ir_function_signature *_atomic_intrinsic3(builtin_available_predicate avail,
                                             const glsl_type *type,
                                             enum ir_intrinsic_id id);
   ir_function_signature *_atomic_op3(const char *intrinsic,
                                      builtin_available_predicate avail,
                                      const glsl_type *type);

   BA1(min3)
   BA1(max3)
   BA1(mid3)

   ir_function_signature *_image_prototype(const glsl_type *image_type,
                                           unsigned num_arguments,
                                           unsigned flags);
   ir_function_signature *_image_size_prototype(const glsl_type *image_type,
                                                unsigned num_arguments,
                                                unsigned flags);
   ir_function_signature *_image_samples_prototype(const glsl_type *image_type,
                                                   unsigned num_arguments,
                                                   unsigned flags);
   ir_function_signature *_image(image_prototype_ctr prototype,
                                 const glsl_type *image_type,
                                 const char *intrinsic_name,
                                 unsigned num_arguments,
                                 unsigned flags,
                                 enum ir_intrinsic_id id);

   ir_function_signature *_memory_barrier_intrinsic(
      builtin_available_predicate avail,
      enum ir_intrinsic_id id);
   ir_function_signature *_memory_barrier(const char *intrinsic_name,
                                          builtin_available_predicate avail);

   ir_function_signature *_ballot_intrinsic(const glsl_type *type);
   ir_function_signature *_ballot(const glsl_type *type, builtin_available_predicate avail);

   ir_function_signature *_inverse_ballot_intrinsic();
   ir_function_signature *_inverse_ballot();

   ir_function_signature *_ballot_bit_extract_intrinsic();
   ir_function_signature *_ballot_bit_extract();

   ir_function_signature *_ballot_bit_intrinsic(enum ir_intrinsic_id id);
   ir_function_signature *_ballot_bit(const char *intrinsic_name);

   ir_function_signature *_read_first_invocation_intrinsic(const glsl_type *type);
   ir_function_signature *_read_first_invocation(const glsl_type *type,
                                                 builtin_available_predicate avail);

   ir_function_signature *_read_invocation_intrinsic(const glsl_type *type);
   ir_function_signature *_read_invocation(const glsl_type *type,
                                           builtin_available_predicate avail);


   ir_function_signature *_invocation_interlock_intrinsic(
      builtin_available_predicate avail,
      enum ir_intrinsic_id id);
   ir_function_signature *_invocation_interlock(
      const char *intrinsic_name,
      builtin_available_predicate avail);

   ir_function_signature *_shader_clock_intrinsic(builtin_available_predicate avail,
                                                  const glsl_type *type);
   ir_function_signature *_shader_clock(builtin_available_predicate avail,
                                        const glsl_type *type);

   ir_function_signature *_shader_clock_realtime_intrinsic(builtin_available_predicate avail,
                                                           const glsl_type *type);
   ir_function_signature *_shader_clock_realtime(builtin_available_predicate avail,
                                                 const glsl_type *type);

   ir_function_signature *_vote_intrinsic(const glsl_type *type,
                                          builtin_available_predicate avail,
                                          enum ir_intrinsic_id id);
   ir_function_signature *_vote(const glsl_type *type,
                                builtin_available_predicate avail,
                                const char *intrinsic_name);

   ir_function_signature *_helper_invocation_intrinsic();
   ir_function_signature *_helper_invocation();

   ir_function_signature *_subgroup_barrier_intrinsic(enum ir_intrinsic_id id,
                                                      builtin_available_predicate avail);
   ir_function_signature *_subgroup_barrier(const char *intrinsic_name,
                                            builtin_available_predicate avail);

   ir_function_signature *_elect_intrinsic();
   ir_function_signature *_elect();

   ir_function_signature *_shuffle_intrinsic(const glsl_type *type);
   ir_function_signature *_shuffle(const glsl_type *type);

   ir_function_signature *_shuffle_xor_intrinsic(const glsl_type *type);
   ir_function_signature *_shuffle_xor(const glsl_type *type);

   ir_function_signature *_shuffle_up_intrinsic(const glsl_type *type);
   ir_function_signature *_shuffle_up(const glsl_type *type);

   ir_function_signature *_shuffle_down_intrinsic(const glsl_type *type);
   ir_function_signature *_shuffle_down(const glsl_type *type);

   ir_function_signature *_subgroup_arithmetic_intrinsic(const glsl_type *type,
                                                         enum ir_intrinsic_id id);
   ir_function_signature *_subgroup_arithmetic(const glsl_type *type,
                                               const char *intrinsic_name);

   ir_function_signature *_subgroup_clustered_intrinsic(const glsl_type *type,
                                                        enum ir_intrinsic_id id);
   ir_function_signature *_subgroup_clustered(const glsl_type *type,
                                              const char *intrinsic_name);

   ir_function_signature *_quad_broadcast_intrinsic(const glsl_type *type);
   ir_function_signature *_quad_broadcast(const glsl_type *type);

   ir_function_signature *_quad_swap_intrinsic(const glsl_type *type, enum ir_intrinsic_id id);
   ir_function_signature *_quad_swap(const glsl_type *type, const char *intrinsic_name);

#undef B0
#undef B1
#undef B2
#undef B3
#undef BA1
#undef BA2
   /** @} */
};

enum image_function_flags {
   IMAGE_FUNCTION_EMIT_STUB = (1 << 0),
   IMAGE_FUNCTION_RETURNS_VOID = (1 << 1),
   IMAGE_FUNCTION_HAS_VECTOR_DATA_TYPE = (1 << 2),
   IMAGE_FUNCTION_SUPPORTS_FLOAT_DATA_TYPE = (1 << 3),
   IMAGE_FUNCTION_READ_ONLY = (1 << 4),
   IMAGE_FUNCTION_WRITE_ONLY = (1 << 5),
   IMAGE_FUNCTION_AVAIL_ATOMIC = (1 << 6),
   IMAGE_FUNCTION_MS_ONLY = (1 << 7),
   IMAGE_FUNCTION_AVAIL_ATOMIC_EXCHANGE = (1 << 8),
   IMAGE_FUNCTION_AVAIL_ATOMIC_ADD = (1 << 9),
   IMAGE_FUNCTION_EXT_ONLY = (1 << 10),
   IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE = (1 << 11),
   IMAGE_FUNCTION_SPARSE = (1 << 12),
};

} /* anonymous namespace */

/**
 * Core builtin_builder functionality:
 *  @{
 */
builtin_builder::builtin_builder()
   : symbols(NULL)
{
   mem_ctx = NULL;
}

builtin_builder::~builtin_builder()
{
   simple_mtx_lock(&builtins_lock);

   ralloc_free(mem_ctx);
   mem_ctx = NULL;
   symbols = NULL;

   simple_mtx_unlock(&builtins_lock);
}

ir_function_signature *
builtin_builder::find(_mesa_glsl_parse_state *state,
                      const char *name, ir_exec_list *actual_parameters)
{
   /* The shader currently being compiled requested a built-in function;
    * it needs to link against builtin_builder::shader in order to get them.
    *
    * Even if we don't find a matching signature, we still need to do this so
    * that the "no matching signature" error will list potential candidates
    * from the available built-ins.
    */
   state->uses_builtin_functions = true;

   ir_function *f = symbols->get_function(name);
   if (f == NULL)
      return NULL;

   ir_function_signature *sig =
      f->matching_signature(state, actual_parameters,
                            state->has_implicit_conversions(),
                            state->has_implicit_int_to_uint_conversion(),
                            true);
   if (sig == NULL)
      return NULL;

   return sig;
}

void
builtin_builder::initialize()
{
   /* If already initialized, don't do it again. */
   if (mem_ctx != NULL)
      return;

   glsl_type_singleton_init_or_ref();

   mem_ctx = ralloc_context(NULL);
   create_shader();
   create_intrinsics();
   create_builtins();
}

void
builtin_builder::release()
{
   ralloc_free(mem_ctx);
   mem_ctx = NULL;
   symbols = NULL;

   glsl_type_singleton_decref();
}

void
builtin_builder::create_shader()
{
   symbols = new(mem_ctx) glsl_symbol_table;
}

/** @} */

#define FIU(func, ...) \
   func(&glsl_type_builtin_float, ##__VA_ARGS__), \
   func(&glsl_type_builtin_vec2, ##__VA_ARGS__), \
   func(&glsl_type_builtin_vec3, ##__VA_ARGS__), \
   func(&glsl_type_builtin_vec4, ##__VA_ARGS__), \
   func(&glsl_type_builtin_int, ##__VA_ARGS__), \
   func(&glsl_type_builtin_ivec2, ##__VA_ARGS__), \
   func(&glsl_type_builtin_ivec3, ##__VA_ARGS__), \
   func(&glsl_type_builtin_ivec4, ##__VA_ARGS__), \
   func(&glsl_type_builtin_uint, ##__VA_ARGS__), \
   func(&glsl_type_builtin_uvec2, ##__VA_ARGS__), \
   func(&glsl_type_builtin_uvec3, ##__VA_ARGS__), \
   func(&glsl_type_builtin_uvec4, ##__VA_ARGS__)

#define FIUB(func, ...) \
   FIU(func, ##__VA_ARGS__), \
   func(&glsl_type_builtin_bool, ##__VA_ARGS__), \
   func(&glsl_type_builtin_bvec2, ##__VA_ARGS__), \
   func(&glsl_type_builtin_bvec3, ##__VA_ARGS__), \
   func(&glsl_type_builtin_bvec4, ##__VA_ARGS__)

#define FIUD(func, ...) \
   FIU(func, ##__VA_ARGS__), \
   func(&glsl_type_builtin_double, ##__VA_ARGS__), \
   func(&glsl_type_builtin_dvec2, ##__VA_ARGS__), \
   func(&glsl_type_builtin_dvec3, ##__VA_ARGS__), \
   func(&glsl_type_builtin_dvec4, ##__VA_ARGS__)

#define FIUBD(func, ...) \
   FIUB(func, ##__VA_ARGS__), \
   func(&glsl_type_builtin_double, ##__VA_ARGS__), \
   func(&glsl_type_builtin_dvec2, ##__VA_ARGS__), \
   func(&glsl_type_builtin_dvec3, ##__VA_ARGS__), \
   func(&glsl_type_builtin_dvec4, ##__VA_ARGS__)

#define FIUBD_AVAIL(func, avail, ...) \
   FIUB(func, avail, ##__VA_ARGS__), \
   func(&glsl_type_builtin_double, avail##_and_fp64, ##__VA_ARGS__), \
   func(&glsl_type_builtin_dvec2, avail##_and_fp64, ##__VA_ARGS__), \
   func(&glsl_type_builtin_dvec3, avail##_and_fp64, ##__VA_ARGS__), \
   func(&glsl_type_builtin_dvec4, avail##_and_fp64, ##__VA_ARGS__)

#define IUB(func, ...) \
   func(&glsl_type_builtin_int, ##__VA_ARGS__), \
   func(&glsl_type_builtin_ivec2, ##__VA_ARGS__), \
   func(&glsl_type_builtin_ivec3, ##__VA_ARGS__), \
   func(&glsl_type_builtin_ivec4, ##__VA_ARGS__), \
   func(&glsl_type_builtin_uint, ##__VA_ARGS__), \
   func(&glsl_type_builtin_uvec2, ##__VA_ARGS__), \
   func(&glsl_type_builtin_uvec3, ##__VA_ARGS__), \
   func(&glsl_type_builtin_uvec4, ##__VA_ARGS__), \
   func(&glsl_type_builtin_bool, ##__VA_ARGS__), \
   func(&glsl_type_builtin_bvec2, ##__VA_ARGS__), \
   func(&glsl_type_builtin_bvec3, ##__VA_ARGS__), \
   func(&glsl_type_builtin_bvec4, ##__VA_ARGS__)

/**
 * Create ir_function and ir_function_signature objects for each
 * intrinsic.
 */
void
builtin_builder::create_intrinsics()
{
   add_function("__intrinsic_atomic_read",
                _atomic_counter_intrinsic(shader_atomic_counters,
                                          ir_intrinsic_atomic_counter_read),
                NULL);
   add_function("__intrinsic_atomic_increment",
                _atomic_counter_intrinsic(shader_atomic_counters,
                                          ir_intrinsic_atomic_counter_increment),
                NULL);
   add_function("__intrinsic_atomic_predecrement",
                _atomic_counter_intrinsic(shader_atomic_counters,
                                          ir_intrinsic_atomic_counter_predecrement),
                NULL);

   add_function("__intrinsic_atomic_add",
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_uint,
                                   ir_intrinsic_generic_atomic_add),
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_int,
                                   ir_intrinsic_generic_atomic_add),
                _atomic_intrinsic2(NV_shader_atomic_float_supported,
                                   &glsl_type_builtin_float,
                                   ir_intrinsic_generic_atomic_add),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_int64_t,
                                   ir_intrinsic_generic_atomic_add),
                _atomic_counter_intrinsic1(shader_atomic_counter_ops_or_v460_desktop,
                                           ir_intrinsic_atomic_counter_add),
                NULL);
   add_function("__intrinsic_atomic_min",
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_uint,
                                   ir_intrinsic_generic_atomic_min),
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_int,
                                   ir_intrinsic_generic_atomic_min),
                _atomic_intrinsic2(INTEL_shader_atomic_float_minmax_supported,
                                   &glsl_type_builtin_float,
                                   ir_intrinsic_generic_atomic_min),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_uint64_t,
                                   ir_intrinsic_generic_atomic_min),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_int64_t,
                                   ir_intrinsic_generic_atomic_min),
                _atomic_counter_intrinsic1(shader_atomic_counter_ops_or_v460_desktop,
                                           ir_intrinsic_atomic_counter_min),
                NULL);
   add_function("__intrinsic_atomic_max",
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_uint,
                                   ir_intrinsic_generic_atomic_max),
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_int,
                                   ir_intrinsic_generic_atomic_max),
                _atomic_intrinsic2(INTEL_shader_atomic_float_minmax_supported,
                                   &glsl_type_builtin_float,
                                   ir_intrinsic_generic_atomic_max),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_uint64_t,
                                   ir_intrinsic_generic_atomic_max),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_int64_t,
                                   ir_intrinsic_generic_atomic_max),
                _atomic_counter_intrinsic1(shader_atomic_counter_ops_or_v460_desktop,
                                           ir_intrinsic_atomic_counter_max),
                NULL);
   add_function("__intrinsic_atomic_and",
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_uint,
                                   ir_intrinsic_generic_atomic_and),
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_int,
                                   ir_intrinsic_generic_atomic_and),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_uint64_t,
                                   ir_intrinsic_generic_atomic_and),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_int64_t,
                                   ir_intrinsic_generic_atomic_and),
                _atomic_counter_intrinsic1(shader_atomic_counter_ops_or_v460_desktop,
                                           ir_intrinsic_atomic_counter_and),
                NULL);
   add_function("__intrinsic_atomic_or",
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_uint,
                                   ir_intrinsic_generic_atomic_or),
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_int,
                                   ir_intrinsic_generic_atomic_or),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_uint64_t,
                                   ir_intrinsic_generic_atomic_or),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_int64_t,
                                   ir_intrinsic_generic_atomic_or),
                _atomic_counter_intrinsic1(shader_atomic_counter_ops_or_v460_desktop,
                                           ir_intrinsic_atomic_counter_or),
                NULL);
   add_function("__intrinsic_atomic_xor",
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_uint,
                                   ir_intrinsic_generic_atomic_xor),
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_int,
                                   ir_intrinsic_generic_atomic_xor),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_uint64_t,
                                   ir_intrinsic_generic_atomic_xor),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_int64_t,
                                   ir_intrinsic_generic_atomic_xor),
                _atomic_counter_intrinsic1(shader_atomic_counter_ops_or_v460_desktop,
                                           ir_intrinsic_atomic_counter_xor),
                NULL);
   add_function("__intrinsic_atomic_exchange",
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_uint,
                                   ir_intrinsic_generic_atomic_exchange),
                _atomic_intrinsic2(buffer_atomics_supported,
                                   &glsl_type_builtin_int,
                                   ir_intrinsic_generic_atomic_exchange),
                _atomic_intrinsic2(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_int64_t,
                                   ir_intrinsic_generic_atomic_exchange),
                _atomic_intrinsic2(NV_shader_atomic_float_supported,
                                   &glsl_type_builtin_float,
                                   ir_intrinsic_generic_atomic_exchange),
                _atomic_counter_intrinsic1(shader_atomic_counter_ops_or_v460_desktop,
                                           ir_intrinsic_atomic_counter_exchange),
                NULL);
   add_function("__intrinsic_atomic_comp_swap",
                _atomic_intrinsic3(buffer_atomics_supported,
                                   &glsl_type_builtin_uint,
                                   ir_intrinsic_generic_atomic_comp_swap),
                _atomic_intrinsic3(buffer_atomics_supported,
                                   &glsl_type_builtin_int,
                                   ir_intrinsic_generic_atomic_comp_swap),
                _atomic_intrinsic3(buffer_int64_atomics_supported,
                                   &glsl_type_builtin_int64_t,
                                   ir_intrinsic_generic_atomic_comp_swap),
                _atomic_intrinsic3(INTEL_shader_atomic_float_minmax_supported,
                                   &glsl_type_builtin_float,
                                   ir_intrinsic_generic_atomic_comp_swap),
                _atomic_counter_intrinsic2(shader_atomic_counter_ops_or_v460_desktop,
                                           ir_intrinsic_atomic_counter_comp_swap),
                NULL);

   add_image_functions(false);

   add_function("__intrinsic_memory_barrier",
                _memory_barrier_intrinsic(shader_image_load_store,
                                          ir_intrinsic_memory_barrier),
                NULL);
   add_function("__intrinsic_group_memory_barrier",
                _memory_barrier_intrinsic(compute_shader,
                                          ir_intrinsic_group_memory_barrier),
                NULL);
   add_function("__intrinsic_memory_barrier_atomic_counter",
                _memory_barrier_intrinsic(compute_shader_supported,
                                          ir_intrinsic_memory_barrier_atomic_counter),
                NULL);
   add_function("__intrinsic_memory_barrier_buffer",
                _memory_barrier_intrinsic(compute_shader_supported,
                                          ir_intrinsic_memory_barrier_buffer),
                NULL);
   add_function("__intrinsic_memory_barrier_image",
                _memory_barrier_intrinsic(compute_shader_supported,
                                          ir_intrinsic_memory_barrier_image),
                NULL);
   add_function("__intrinsic_memory_barrier_shared",
                _memory_barrier_intrinsic(compute_shader,
                                          ir_intrinsic_memory_barrier_shared),
                NULL);

   add_function("__intrinsic_begin_invocation_interlock",
                _invocation_interlock_intrinsic(
                   supports_arb_fragment_shader_interlock,
                   ir_intrinsic_begin_invocation_interlock), NULL);

   add_function("__intrinsic_end_invocation_interlock",
                _invocation_interlock_intrinsic(
                   supports_arb_fragment_shader_interlock,
                   ir_intrinsic_end_invocation_interlock), NULL);

   add_function("__intrinsic_shader_clock",
                _shader_clock_intrinsic(shader_clock,
                                        &glsl_type_builtin_uvec2),
                NULL);

   add_function("__intrinsic_shader_clock_realtime",
                _shader_clock_realtime_intrinsic(shader_clock,
                                                 &glsl_type_builtin_uvec2),
                NULL);

   add_function("__intrinsic_vote_all",
                _vote_intrinsic(&glsl_type_builtin_bool, vote_or_v460_desktop,
                                ir_intrinsic_vote_all),
                NULL);
   add_function("__intrinsic_vote_any",
                _vote_intrinsic(&glsl_type_builtin_bool, vote_or_v460_desktop,
                                ir_intrinsic_vote_any),
                NULL);
   add_function("__intrinsic_vote_eq",
                FIUBD_AVAIL(_vote_intrinsic, vote_or_v460_desktop, ir_intrinsic_vote_eq),
                NULL);

   add_function("__intrinsic_ballot_uint64",
                _ballot_intrinsic(&glsl_type_builtin_uint64_t),
                NULL);

   add_function("__intrinsic_ballot_uvec4",
                _ballot_intrinsic(&glsl_type_builtin_uvec4),
                NULL);

   add_function("__intrinsic_inverse_ballot", _inverse_ballot_intrinsic(), NULL);

   add_function("__intrinsic_ballot_bit_extract", _ballot_bit_extract_intrinsic(), NULL);

   add_function("__intrinsic_ballot_bit_count",
                _ballot_bit_intrinsic(ir_intrinsic_ballot_bit_count), NULL);
   add_function("__intrinsic_ballot_inclusive_bit_count",
                _ballot_bit_intrinsic(ir_intrinsic_ballot_inclusive_bit_count), NULL);
   add_function("__intrinsic_ballot_exclusive_bit_count",
                _ballot_bit_intrinsic(ir_intrinsic_ballot_exclusive_bit_count), NULL);
   add_function("__intrinsic_ballot_find_lsb",
                _ballot_bit_intrinsic(ir_intrinsic_ballot_find_lsb), NULL);
   add_function("__intrinsic_ballot_find_msb",
                _ballot_bit_intrinsic(ir_intrinsic_ballot_find_msb), NULL);

   add_function("__intrinsic_read_invocation", FIUBD(_read_invocation_intrinsic), NULL);

   add_function("__intrinsic_read_first_invocation",
                FIUBD(_read_first_invocation_intrinsic), NULL);

   add_function("__intrinsic_helper_invocation",
                _helper_invocation_intrinsic(), NULL);

   add_function("__intrinsic_is_sparse_texels_resident",
                _is_sparse_texels_resident_intrinsic(), NULL);

   add_function("__intrinsic_subgroup_barrier",
                _subgroup_barrier_intrinsic(ir_intrinsic_subgroup_barrier, subgroup_basic),
                NULL);
   add_function("__intrinsic_subgroup_memory_barrier",
                _subgroup_barrier_intrinsic(ir_intrinsic_subgroup_memory_barrier,
                                            subgroup_basic),
                NULL);
   add_function("__intrinsic_subgroup_memory_barrier_buffer",
                _subgroup_barrier_intrinsic(ir_intrinsic_subgroup_memory_barrier_buffer,
                                            subgroup_basic),
                NULL);
   add_function("__intrinsic_subgroup_memory_barrier_shared",
                _subgroup_barrier_intrinsic(ir_intrinsic_subgroup_memory_barrier_shared,
                                            compute_shader_and_subgroup_basic),
                NULL);
   add_function("__intrinsic_subgroup_memory_barrier_image",
                _subgroup_barrier_intrinsic(ir_intrinsic_subgroup_memory_barrier_image,
                                            subgroup_basic),
                NULL);

   add_function("__intrinsic_elect", _elect_intrinsic(), NULL);

   add_function("__intrinsic_shuffle", FIUBD(_shuffle_intrinsic), NULL);

   add_function("__intrinsic_shuffle_xor", FIUBD(_shuffle_xor_intrinsic), NULL);

   add_function("__intrinsic_shuffle_up", FIUBD(_shuffle_up_intrinsic), NULL);

   add_function("__intrinsic_shuffle_down", FIUBD(_shuffle_down_intrinsic), NULL);

#define SUBGROUP_ARITH_INTRINSICS(ext, group) \
   add_function("__intrinsic_" #group "_add", \
                FIUD(_subgroup_##ext##_intrinsic, ir_intrinsic_##group##_add), NULL); \
   add_function("__intrinsic_" #group "_mul", \
                FIUD(_subgroup_##ext##_intrinsic, ir_intrinsic_##group##_mul), NULL); \
   add_function("__intrinsic_" #group "_min", \
                FIUD(_subgroup_##ext##_intrinsic, ir_intrinsic_##group##_min), NULL); \
   add_function("__intrinsic_" #group "_max", \
                FIUD(_subgroup_##ext##_intrinsic, ir_intrinsic_##group##_max), NULL); \
   add_function("__intrinsic_" #group "_and", \
                IUB(_subgroup_##ext##_intrinsic, ir_intrinsic_##group##_and), NULL); \
   add_function("__intrinsic_" #group "_or", \
                IUB(_subgroup_##ext##_intrinsic, ir_intrinsic_##group##_or), NULL); \
   add_function("__intrinsic_" #group "_xor", \
                IUB(_subgroup_##ext##_intrinsic, ir_intrinsic_##group##_xor), NULL)

   SUBGROUP_ARITH_INTRINSICS(arithmetic, reduce);
   SUBGROUP_ARITH_INTRINSICS(arithmetic, inclusive);
   SUBGROUP_ARITH_INTRINSICS(arithmetic, exclusive);

   SUBGROUP_ARITH_INTRINSICS(clustered, clustered);

   add_function("__intrinsic_quad_broadcast", FIUBD(_quad_broadcast_intrinsic), NULL);

   add_function("__intrinsic_quad_swap_horizontal",
                FIUBD(_quad_swap_intrinsic, ir_intrinsic_quad_swap_horizontal), NULL);
   add_function("__intrinsic_quad_swap_vertical",
                FIUBD(_quad_swap_intrinsic, ir_intrinsic_quad_swap_vertical), NULL);
   add_function("__intrinsic_quad_swap_diagonal",
                FIUBD(_quad_swap_intrinsic, ir_intrinsic_quad_swap_diagonal), NULL);
}

/**
 * Create ir_function and ir_function_signature objects for each built-in.
 *
 * Contains a list of every available built-in.
 */
void
builtin_builder::create_builtins()
{
#define F(NAME)                                 \
   add_function(#NAME,                          \
                _##NAME(&glsl_type_builtin_float), \
                _##NAME(&glsl_type_builtin_vec2),  \
                _##NAME(&glsl_type_builtin_vec3),  \
                _##NAME(&glsl_type_builtin_vec4),  \
                NULL);

#define FHF(NAME)                                 \
   add_function(#NAME,                          \
                _##NAME(always_available, &glsl_type_builtin_float), \
                _##NAME(always_available, &glsl_type_builtin_vec2),  \
                _##NAME(always_available, &glsl_type_builtin_vec3),  \
                _##NAME(always_available, &glsl_type_builtin_vec4),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_float16_t), \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4),  \
                NULL);

#define FHF_DERIVATIVES(NAME)                                               \
   add_function(#NAME,                                                      \
                _##NAME(derivatives, &glsl_type_builtin_float),                \
                _##NAME(derivatives, &glsl_type_builtin_vec2),                 \
                _##NAME(derivatives, &glsl_type_builtin_vec3),                 \
                _##NAME(derivatives, &glsl_type_builtin_vec4),                 \
                _##NAME(half_float_derivatives, &glsl_type_builtin_float16_t), \
                _##NAME(half_float_derivatives, &glsl_type_builtin_f16vec2),   \
                _##NAME(half_float_derivatives, &glsl_type_builtin_f16vec3),   \
                _##NAME(half_float_derivatives, &glsl_type_builtin_f16vec4),   \
                NULL);

#define FHF_DERIVATIVE_CONTROL(NAME)                                               \
   add_function(#NAME,                                                             \
                _##NAME(derivative_control, &glsl_type_builtin_float),                \
                _##NAME(derivative_control, &glsl_type_builtin_vec2),                 \
                _##NAME(derivative_control, &glsl_type_builtin_vec3),                 \
                _##NAME(derivative_control, &glsl_type_builtin_vec4),                 \
                _##NAME(half_float_derivative_control, &glsl_type_builtin_float16_t), \
                _##NAME(half_float_derivative_control, &glsl_type_builtin_f16vec2),   \
                _##NAME(half_float_derivative_control, &glsl_type_builtin_f16vec3),   \
                _##NAME(half_float_derivative_control, &glsl_type_builtin_f16vec4),   \
                NULL);

#define FHF130(NAME)                                 \
   add_function(#NAME,                          \
                _##NAME(v130, &glsl_type_builtin_float), \
                _##NAME(v130, &glsl_type_builtin_vec2),  \
                _##NAME(v130, &glsl_type_builtin_vec3),  \
                _##NAME(v130, &glsl_type_builtin_vec4),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_float16_t), \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4),  \
                NULL);

#define FDHF(NAME)                                 \
   add_function(#NAME,                          \
                _##NAME(always_available, &glsl_type_builtin_float), \
                _##NAME(always_available, &glsl_type_builtin_vec2),  \
                _##NAME(always_available, &glsl_type_builtin_vec3),  \
                _##NAME(always_available, &glsl_type_builtin_vec4),  \
                _##NAME(fp64, &glsl_type_builtin_double),  \
                _##NAME(fp64, &glsl_type_builtin_dvec2),    \
                _##NAME(fp64, &glsl_type_builtin_dvec3),     \
                _##NAME(fp64, &glsl_type_builtin_dvec4),      \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_float16_t), \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4),  \
                NULL);

#define FDHF130(NAME)                                                      \
   add_function(#NAME,                                                     \
                _##NAME(v130, &glsl_type_builtin_float),                      \
                _##NAME(v130, &glsl_type_builtin_vec2),                       \
                _##NAME(v130, &glsl_type_builtin_vec3),                       \
                _##NAME(v130, &glsl_type_builtin_vec4),                       \
                _##NAME(fp64, &glsl_type_builtin_double),                     \
                _##NAME(fp64, &glsl_type_builtin_dvec2),                      \
                _##NAME(fp64, &glsl_type_builtin_dvec3),                      \
                _##NAME(fp64, &glsl_type_builtin_dvec4),                      \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_float16_t), \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2),   \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3),   \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4),   \
                NULL);

#define FDHF130GS4(NAME)                                                   \
   add_function(#NAME,                                                     \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_float),    \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_vec2),     \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_vec3),     \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_vec4),     \
                _##NAME(fp64, &glsl_type_builtin_double),                  \
                _##NAME(fp64, &glsl_type_builtin_dvec2),                      \
                _##NAME(fp64, &glsl_type_builtin_dvec3),                      \
                _##NAME(fp64, &glsl_type_builtin_dvec4),                      \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_float16_t), \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2),   \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3),   \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4),   \
                NULL);

#define FDHFGS5(NAME)                                                      \
   add_function(#NAME,                                                     \
                _##NAME(gpu_shader5_es, &glsl_type_builtin_float),            \
                _##NAME(gpu_shader5_es, &glsl_type_builtin_vec2),             \
                _##NAME(gpu_shader5_es, &glsl_type_builtin_vec3),             \
                _##NAME(gpu_shader5_es, &glsl_type_builtin_vec4),             \
                _##NAME(fp64, &glsl_type_builtin_double),                     \
                _##NAME(fp64, &glsl_type_builtin_dvec2),                      \
                _##NAME(fp64, &glsl_type_builtin_dvec3),                      \
                _##NAME(fp64, &glsl_type_builtin_dvec4),                      \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_float16_t), \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2),   \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3),   \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4),   \
                NULL);

#define FI64HF(NAME)                                \
   add_function(#NAME,                          \
                _##NAME(always_available, &glsl_type_builtin_float), \
                _##NAME(always_available, &glsl_type_builtin_vec2),  \
                _##NAME(always_available, &glsl_type_builtin_vec3),  \
                _##NAME(always_available, &glsl_type_builtin_vec4),  \
                _##NAME(always_available, &glsl_type_builtin_int),   \
                _##NAME(always_available, &glsl_type_builtin_ivec2), \
                _##NAME(always_available, &glsl_type_builtin_ivec3), \
                _##NAME(always_available, &glsl_type_builtin_ivec4), \
                _##NAME(fp64, &glsl_type_builtin_double), \
                _##NAME(fp64, &glsl_type_builtin_dvec2),  \
                _##NAME(fp64, &glsl_type_builtin_dvec3),  \
                _##NAME(fp64, &glsl_type_builtin_dvec4),  \
                _##NAME(int64_avail, &glsl_type_builtin_int64_t), \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec2),  \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec3),  \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec4),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_float16_t), \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4),  \
                NULL);

#define FIUDHF_VEC(NAME)                                          \
   add_function(#NAME,                                            \
                _##NAME(always_available, &glsl_type_builtin_vec2),  \
                _##NAME(always_available, &glsl_type_builtin_vec3),  \
                _##NAME(always_available, &glsl_type_builtin_vec4),  \
                                                                  \
                _##NAME(always_available, &glsl_type_builtin_ivec2), \
                _##NAME(always_available, &glsl_type_builtin_ivec3), \
                _##NAME(always_available, &glsl_type_builtin_ivec4), \
                                                                  \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec2), \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec3), \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec4), \
                _##NAME(fp64, &glsl_type_builtin_dvec2),  \
                _##NAME(fp64, &glsl_type_builtin_dvec3),  \
                _##NAME(fp64, &glsl_type_builtin_dvec4),  \
                _##NAME(int64_avail, &glsl_type_builtin_int64_t), \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec2),  \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec3),  \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec4),  \
                _##NAME(int64_avail, &glsl_type_builtin_uint64_t), \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec2),  \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec3),  \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec4),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4),  \
                NULL);

#define IU(NAME)                                \
   add_function(#NAME,                          \
                _##NAME(&glsl_type_builtin_int),   \
                _##NAME(&glsl_type_builtin_ivec2), \
                _##NAME(&glsl_type_builtin_ivec3), \
                _##NAME(&glsl_type_builtin_ivec4), \
                                                \
                _##NAME(&glsl_type_builtin_uint),  \
                _##NAME(&glsl_type_builtin_uvec2), \
                _##NAME(&glsl_type_builtin_uvec3), \
                _##NAME(&glsl_type_builtin_uvec4), \
                NULL);

#define FIUBDHF_VEC(NAME)                                           \
   add_function(#NAME,                                            \
                _##NAME(always_available, &glsl_type_builtin_vec2),  \
                _##NAME(always_available, &glsl_type_builtin_vec3),  \
                _##NAME(always_available, &glsl_type_builtin_vec4),  \
                                                                  \
                _##NAME(always_available, &glsl_type_builtin_ivec2), \
                _##NAME(always_available, &glsl_type_builtin_ivec3), \
                _##NAME(always_available, &glsl_type_builtin_ivec4), \
                                                                  \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec2), \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec3), \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec4), \
                                                                  \
                _##NAME(always_available, &glsl_type_builtin_bvec2), \
                _##NAME(always_available, &glsl_type_builtin_bvec3), \
                _##NAME(always_available, &glsl_type_builtin_bvec4), \
                                                                  \
                _##NAME(fp64, &glsl_type_builtin_dvec2), \
                _##NAME(fp64, &glsl_type_builtin_dvec3), \
                _##NAME(fp64, &glsl_type_builtin_dvec4), \
                _##NAME(int64_avail, &glsl_type_builtin_int64_t), \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec2),  \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec3),  \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec4),  \
                _##NAME(int64_avail, &glsl_type_builtin_uint64_t), \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec2),  \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec3),  \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec4),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3),  \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4),  \
                NULL);

#define FIUDHF2_MIXED(NAME)                                                                           \
   add_function(#NAME,                                                                                \
                _##NAME(always_available, &glsl_type_builtin_float, &glsl_type_builtin_float),        \
                _##NAME(always_available, &glsl_type_builtin_vec2,  &glsl_type_builtin_float),        \
                _##NAME(always_available, &glsl_type_builtin_vec3,  &glsl_type_builtin_float),        \
                _##NAME(always_available, &glsl_type_builtin_vec4,  &glsl_type_builtin_float),        \
                                                                                                      \
                _##NAME(always_available, &glsl_type_builtin_vec2,  &glsl_type_builtin_vec2),         \
                _##NAME(always_available, &glsl_type_builtin_vec3,  &glsl_type_builtin_vec3),         \
                _##NAME(always_available, &glsl_type_builtin_vec4,  &glsl_type_builtin_vec4),         \
                                                                                                      \
                _##NAME(always_available, &glsl_type_builtin_int,   &glsl_type_builtin_int),          \
                _##NAME(always_available, &glsl_type_builtin_ivec2, &glsl_type_builtin_int),          \
                _##NAME(always_available, &glsl_type_builtin_ivec3, &glsl_type_builtin_int),          \
                _##NAME(always_available, &glsl_type_builtin_ivec4, &glsl_type_builtin_int),          \
                                                                                                      \
                _##NAME(always_available, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),        \
                _##NAME(always_available, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3),        \
                _##NAME(always_available, &glsl_type_builtin_ivec4, &glsl_type_builtin_ivec4),        \
                                                                                                      \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uint,  &glsl_type_builtin_uint),      \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec2, &glsl_type_builtin_uint),      \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec3, &glsl_type_builtin_uint),      \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec4, &glsl_type_builtin_uint),      \
                                                                                                      \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec2, &glsl_type_builtin_uvec2),     \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec3, &glsl_type_builtin_uvec3),     \
                _##NAME(v130_or_gpu_shader4, &glsl_type_builtin_uvec4, &glsl_type_builtin_uvec4),     \
                                                                                                      \
                _##NAME(fp64, &glsl_type_builtin_double, &glsl_type_builtin_double),                  \
                _##NAME(fp64, &glsl_type_builtin_dvec2, &glsl_type_builtin_double),                   \
                _##NAME(fp64, &glsl_type_builtin_dvec3, &glsl_type_builtin_double),                   \
                _##NAME(fp64, &glsl_type_builtin_dvec4, &glsl_type_builtin_double),                   \
                _##NAME(fp64, &glsl_type_builtin_dvec2, &glsl_type_builtin_dvec2),                    \
                _##NAME(fp64, &glsl_type_builtin_dvec3, &glsl_type_builtin_dvec3),                    \
                _##NAME(fp64, &glsl_type_builtin_dvec4, &glsl_type_builtin_dvec4),                    \
                                                                                                      \
                _##NAME(int64_avail, &glsl_type_builtin_int64_t, &glsl_type_builtin_int64_t),         \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec2, &glsl_type_builtin_int64_t),         \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec3, &glsl_type_builtin_int64_t),         \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec4, &glsl_type_builtin_int64_t),         \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec2, &glsl_type_builtin_i64vec2),         \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec3, &glsl_type_builtin_i64vec3),         \
                _##NAME(int64_avail, &glsl_type_builtin_i64vec4, &glsl_type_builtin_i64vec4),         \
                _##NAME(int64_avail, &glsl_type_builtin_uint64_t, &glsl_type_builtin_uint64_t),       \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec2, &glsl_type_builtin_uint64_t),        \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec3, &glsl_type_builtin_uint64_t),        \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec4, &glsl_type_builtin_uint64_t),        \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec2, &glsl_type_builtin_u64vec2),         \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec3, &glsl_type_builtin_u64vec3),         \
                _##NAME(int64_avail, &glsl_type_builtin_u64vec4, &glsl_type_builtin_u64vec4),         \
                                                                                                      \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_float16_t), \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2, &glsl_type_builtin_float16_t),   \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3, &glsl_type_builtin_float16_t),   \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4, &glsl_type_builtin_float16_t),   \
                                                                                                            \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec2, &glsl_type_builtin_f16vec2),     \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec3, &glsl_type_builtin_f16vec3),     \
                _##NAME(gpu_shader_half_float, &glsl_type_builtin_f16vec4, &glsl_type_builtin_f16vec4),     \
                NULL);

   FHF(radians)
   FHF(degrees)
   FHF(sin)
   FHF(cos)
   FHF(tan)
   FHF(asin)
   FHF(acos)

   add_function("atan",
                _atan(always_available, &glsl_type_builtin_float),
                _atan(always_available, &glsl_type_builtin_vec2),
                _atan(always_available, &glsl_type_builtin_vec3),
                _atan(always_available, &glsl_type_builtin_vec4),
                _atan2(always_available, &glsl_type_builtin_float),
                _atan2(always_available, &glsl_type_builtin_vec2),
                _atan2(always_available, &glsl_type_builtin_vec3),
                _atan2(always_available, &glsl_type_builtin_vec4),
                _atan(gpu_shader_half_float, &glsl_type_builtin_float16_t),
                _atan(gpu_shader_half_float, &glsl_type_builtin_f16vec2),
                _atan(gpu_shader_half_float, &glsl_type_builtin_f16vec3),
                _atan(gpu_shader_half_float, &glsl_type_builtin_f16vec4),
                _atan2(gpu_shader_half_float, &glsl_type_builtin_float16_t),
                _atan2(gpu_shader_half_float, &glsl_type_builtin_f16vec2),
                _atan2(gpu_shader_half_float, &glsl_type_builtin_f16vec3),
                _atan2(gpu_shader_half_float, &glsl_type_builtin_f16vec4),
                NULL);

   FHF130(sinh)
   FHF130(cosh)
   FHF130(tanh)
   FHF130(asinh)
   FHF130(acosh)
   FHF130(atanh)
   FHF(pow)
   FHF(exp)
   FHF(log)
   FHF(exp2)
   FHF(log2)
   FDHF(sqrt)
   FDHF(inversesqrt)
   FI64HF(abs)
   FI64HF(sign)
   FDHF(floor)
   FDHF130(trunc)
   FDHF130GS4(round)
   FDHF130(roundEven)
   FDHF(ceil)
   FDHF(fract)

   add_function("truncate",
                _truncate(gpu_shader4, &glsl_type_builtin_float),
                _truncate(gpu_shader4, &glsl_type_builtin_vec2),
                _truncate(gpu_shader4, &glsl_type_builtin_vec3),
                _truncate(gpu_shader4, &glsl_type_builtin_vec4),
                NULL);


   add_function("mod",
                _mod(always_available, &glsl_type_builtin_float, &glsl_type_builtin_float),
                _mod(always_available, &glsl_type_builtin_vec2,  &glsl_type_builtin_float),
                _mod(always_available, &glsl_type_builtin_vec3,  &glsl_type_builtin_float),
                _mod(always_available, &glsl_type_builtin_vec4,  &glsl_type_builtin_float),

                _mod(always_available, &glsl_type_builtin_vec2,  &glsl_type_builtin_vec2),
                _mod(always_available, &glsl_type_builtin_vec3,  &glsl_type_builtin_vec3),
                _mod(always_available, &glsl_type_builtin_vec4,  &glsl_type_builtin_vec4),

                _mod(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_float16_t),
                _mod(gpu_shader_half_float, &glsl_type_builtin_f16vec2,  &glsl_type_builtin_float16_t),
                _mod(gpu_shader_half_float, &glsl_type_builtin_f16vec3,  &glsl_type_builtin_float16_t),
                _mod(gpu_shader_half_float, &glsl_type_builtin_f16vec4,  &glsl_type_builtin_float16_t),

                _mod(gpu_shader_half_float, &glsl_type_builtin_f16vec2,  &glsl_type_builtin_f16vec2),
                _mod(gpu_shader_half_float, &glsl_type_builtin_f16vec3,  &glsl_type_builtin_f16vec3),
                _mod(gpu_shader_half_float, &glsl_type_builtin_f16vec4,  &glsl_type_builtin_f16vec4),

                _mod(fp64, &glsl_type_builtin_double, &glsl_type_builtin_double),
                _mod(fp64, &glsl_type_builtin_dvec2,  &glsl_type_builtin_double),
                _mod(fp64, &glsl_type_builtin_dvec3,  &glsl_type_builtin_double),
                _mod(fp64, &glsl_type_builtin_dvec4,  &glsl_type_builtin_double),

                _mod(fp64, &glsl_type_builtin_dvec2,  &glsl_type_builtin_dvec2),
                _mod(fp64, &glsl_type_builtin_dvec3,  &glsl_type_builtin_dvec3),
                _mod(fp64, &glsl_type_builtin_dvec4,  &glsl_type_builtin_dvec4),
                NULL);

   FDHF130(modf)

   FIUDHF2_MIXED(min)
   FIUDHF2_MIXED(max)
   FIUDHF2_MIXED(clamp)

   add_function("mix",
                _mix_lrp(always_available, &glsl_type_builtin_float, &glsl_type_builtin_float),
                _mix_lrp(always_available, &glsl_type_builtin_vec2,  &glsl_type_builtin_float),
                _mix_lrp(always_available, &glsl_type_builtin_vec3,  &glsl_type_builtin_float),
                _mix_lrp(always_available, &glsl_type_builtin_vec4,  &glsl_type_builtin_float),

                _mix_lrp(always_available, &glsl_type_builtin_vec2,  &glsl_type_builtin_vec2),
                _mix_lrp(always_available, &glsl_type_builtin_vec3,  &glsl_type_builtin_vec3),
                _mix_lrp(always_available, &glsl_type_builtin_vec4,  &glsl_type_builtin_vec4),

                _mix_lrp(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_float16_t),
                _mix_lrp(gpu_shader_half_float, &glsl_type_builtin_f16vec2,  &glsl_type_builtin_float16_t),
                _mix_lrp(gpu_shader_half_float, &glsl_type_builtin_f16vec3,  &glsl_type_builtin_float16_t),
                _mix_lrp(gpu_shader_half_float, &glsl_type_builtin_f16vec4,  &glsl_type_builtin_float16_t),

                _mix_lrp(gpu_shader_half_float, &glsl_type_builtin_f16vec2,  &glsl_type_builtin_f16vec2),
                _mix_lrp(gpu_shader_half_float, &glsl_type_builtin_f16vec3,  &glsl_type_builtin_f16vec3),
                _mix_lrp(gpu_shader_half_float, &glsl_type_builtin_f16vec4,  &glsl_type_builtin_f16vec4),

                _mix_lrp(fp64, &glsl_type_builtin_double, &glsl_type_builtin_double),
                _mix_lrp(fp64, &glsl_type_builtin_dvec2,  &glsl_type_builtin_double),
                _mix_lrp(fp64, &glsl_type_builtin_dvec3,  &glsl_type_builtin_double),
                _mix_lrp(fp64, &glsl_type_builtin_dvec4,  &glsl_type_builtin_double),

                _mix_lrp(fp64, &glsl_type_builtin_dvec2,  &glsl_type_builtin_dvec2),
                _mix_lrp(fp64, &glsl_type_builtin_dvec3,  &glsl_type_builtin_dvec3),
                _mix_lrp(fp64, &glsl_type_builtin_dvec4,  &glsl_type_builtin_dvec4),

                _mix_sel(v130, &glsl_type_builtin_float, &glsl_type_builtin_bool),
                _mix_sel(v130, &glsl_type_builtin_vec2,  &glsl_type_builtin_bvec2),
                _mix_sel(v130, &glsl_type_builtin_vec3,  &glsl_type_builtin_bvec3),
                _mix_sel(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_bvec4),

                _mix_sel(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_bool),
                _mix_sel(gpu_shader_half_float, &glsl_type_builtin_f16vec2,  &glsl_type_builtin_bvec2),
                _mix_sel(gpu_shader_half_float, &glsl_type_builtin_f16vec3,  &glsl_type_builtin_bvec3),
                _mix_sel(gpu_shader_half_float, &glsl_type_builtin_f16vec4,  &glsl_type_builtin_bvec4),

                _mix_sel(fp64, &glsl_type_builtin_double, &glsl_type_builtin_bool),
                _mix_sel(fp64, &glsl_type_builtin_dvec2,  &glsl_type_builtin_bvec2),
                _mix_sel(fp64, &glsl_type_builtin_dvec3,  &glsl_type_builtin_bvec3),
                _mix_sel(fp64, &glsl_type_builtin_dvec4,  &glsl_type_builtin_bvec4),

                _mix_sel(shader_integer_mix, &glsl_type_builtin_int,   &glsl_type_builtin_bool),
                _mix_sel(shader_integer_mix, &glsl_type_builtin_ivec2, &glsl_type_builtin_bvec2),
                _mix_sel(shader_integer_mix, &glsl_type_builtin_ivec3, &glsl_type_builtin_bvec3),
                _mix_sel(shader_integer_mix, &glsl_type_builtin_ivec4, &glsl_type_builtin_bvec4),

                _mix_sel(shader_integer_mix, &glsl_type_builtin_uint,  &glsl_type_builtin_bool),
                _mix_sel(shader_integer_mix, &glsl_type_builtin_uvec2, &glsl_type_builtin_bvec2),
                _mix_sel(shader_integer_mix, &glsl_type_builtin_uvec3, &glsl_type_builtin_bvec3),
                _mix_sel(shader_integer_mix, &glsl_type_builtin_uvec4, &glsl_type_builtin_bvec4),

                _mix_sel(shader_integer_mix, &glsl_type_builtin_bool,  &glsl_type_builtin_bool),
                _mix_sel(shader_integer_mix, &glsl_type_builtin_bvec2, &glsl_type_builtin_bvec2),
                _mix_sel(shader_integer_mix, &glsl_type_builtin_bvec3, &glsl_type_builtin_bvec3),
                _mix_sel(shader_integer_mix, &glsl_type_builtin_bvec4, &glsl_type_builtin_bvec4),

                _mix_sel(int64_avail, &glsl_type_builtin_int64_t, &glsl_type_builtin_bool),
                _mix_sel(int64_avail, &glsl_type_builtin_i64vec2, &glsl_type_builtin_bvec2),
                _mix_sel(int64_avail, &glsl_type_builtin_i64vec3, &glsl_type_builtin_bvec3),
                _mix_sel(int64_avail, &glsl_type_builtin_i64vec4, &glsl_type_builtin_bvec4),

                _mix_sel(int64_avail, &glsl_type_builtin_uint64_t,  &glsl_type_builtin_bool),
                _mix_sel(int64_avail, &glsl_type_builtin_u64vec2, &glsl_type_builtin_bvec2),
                _mix_sel(int64_avail, &glsl_type_builtin_u64vec3, &glsl_type_builtin_bvec3),
                _mix_sel(int64_avail, &glsl_type_builtin_u64vec4, &glsl_type_builtin_bvec4),
                NULL);

   add_function("step",
                _step(always_available, &glsl_type_builtin_float, &glsl_type_builtin_float),
                _step(always_available, &glsl_type_builtin_float, &glsl_type_builtin_vec2),
                _step(always_available, &glsl_type_builtin_float, &glsl_type_builtin_vec3),
                _step(always_available, &glsl_type_builtin_float, &glsl_type_builtin_vec4),

                _step(always_available, &glsl_type_builtin_vec2,  &glsl_type_builtin_vec2),
                _step(always_available, &glsl_type_builtin_vec3,  &glsl_type_builtin_vec3),
                _step(always_available, &glsl_type_builtin_vec4,  &glsl_type_builtin_vec4),
                _step(fp64, &glsl_type_builtin_double, &glsl_type_builtin_double),
                _step(fp64, &glsl_type_builtin_double, &glsl_type_builtin_dvec2),
                _step(fp64, &glsl_type_builtin_double, &glsl_type_builtin_dvec3),
                _step(fp64, &glsl_type_builtin_double, &glsl_type_builtin_dvec4),

                _step(fp64, &glsl_type_builtin_dvec2,  &glsl_type_builtin_dvec2),
                _step(fp64, &glsl_type_builtin_dvec3,  &glsl_type_builtin_dvec3),
                _step(fp64, &glsl_type_builtin_dvec4,  &glsl_type_builtin_dvec4),

                _step(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_float16_t),
                _step(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_f16vec2),
                _step(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_f16vec3),
                _step(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_f16vec4),

                _step(gpu_shader_half_float, &glsl_type_builtin_f16vec2,  &glsl_type_builtin_f16vec2),
                _step(gpu_shader_half_float, &glsl_type_builtin_f16vec3,  &glsl_type_builtin_f16vec3),
                _step(gpu_shader_half_float, &glsl_type_builtin_f16vec4,  &glsl_type_builtin_f16vec4),
                NULL);

   add_function("smoothstep",
                _smoothstep(always_available, &glsl_type_builtin_float, &glsl_type_builtin_float),
                _smoothstep(always_available, &glsl_type_builtin_float, &glsl_type_builtin_vec2),
                _smoothstep(always_available, &glsl_type_builtin_float, &glsl_type_builtin_vec3),
                _smoothstep(always_available, &glsl_type_builtin_float, &glsl_type_builtin_vec4),

                _smoothstep(always_available, &glsl_type_builtin_vec2,  &glsl_type_builtin_vec2),
                _smoothstep(always_available, &glsl_type_builtin_vec3,  &glsl_type_builtin_vec3),
                _smoothstep(always_available, &glsl_type_builtin_vec4,  &glsl_type_builtin_vec4),
                _smoothstep(fp64, &glsl_type_builtin_double, &glsl_type_builtin_double),
                _smoothstep(fp64, &glsl_type_builtin_double, &glsl_type_builtin_dvec2),
                _smoothstep(fp64, &glsl_type_builtin_double, &glsl_type_builtin_dvec3),
                _smoothstep(fp64, &glsl_type_builtin_double, &glsl_type_builtin_dvec4),

                _smoothstep(fp64, &glsl_type_builtin_dvec2,  &glsl_type_builtin_dvec2),
                _smoothstep(fp64, &glsl_type_builtin_dvec3,  &glsl_type_builtin_dvec3),
                _smoothstep(fp64, &glsl_type_builtin_dvec4,  &glsl_type_builtin_dvec4),

                _smoothstep(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_float16_t),
                _smoothstep(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_f16vec2),
                _smoothstep(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_f16vec3),
                _smoothstep(gpu_shader_half_float, &glsl_type_builtin_float16_t, &glsl_type_builtin_f16vec4),

                _smoothstep(gpu_shader_half_float, &glsl_type_builtin_f16vec2,  &glsl_type_builtin_f16vec2),
                _smoothstep(gpu_shader_half_float, &glsl_type_builtin_f16vec3,  &glsl_type_builtin_f16vec3),
                _smoothstep(gpu_shader_half_float, &glsl_type_builtin_f16vec4,  &glsl_type_builtin_f16vec4),
                NULL);

   FDHF130(isnan)
   FDHF130(isinf)

   F(floatBitsToInt)
   F(floatBitsToUint)
   add_function("intBitsToFloat",
                _intBitsToFloat(&glsl_type_builtin_int),
                _intBitsToFloat(&glsl_type_builtin_ivec2),
                _intBitsToFloat(&glsl_type_builtin_ivec3),
                _intBitsToFloat(&glsl_type_builtin_ivec4),
                NULL);
   add_function("uintBitsToFloat",
                _uintBitsToFloat(&glsl_type_builtin_uint),
                _uintBitsToFloat(&glsl_type_builtin_uvec2),
                _uintBitsToFloat(&glsl_type_builtin_uvec3),
                _uintBitsToFloat(&glsl_type_builtin_uvec4),
                NULL);

   add_function("doubleBitsToInt64",
                _doubleBitsToInt64(int64_fp64, &glsl_type_builtin_double),
                _doubleBitsToInt64(int64_fp64, &glsl_type_builtin_dvec2),
                _doubleBitsToInt64(int64_fp64, &glsl_type_builtin_dvec3),
                _doubleBitsToInt64(int64_fp64, &glsl_type_builtin_dvec4),
                NULL);

   add_function("doubleBitsToUint64",
                _doubleBitsToUint64(int64_fp64, &glsl_type_builtin_double),
                _doubleBitsToUint64(int64_fp64, &glsl_type_builtin_dvec2),
                _doubleBitsToUint64(int64_fp64, &glsl_type_builtin_dvec3),
                _doubleBitsToUint64(int64_fp64, &glsl_type_builtin_dvec4),
                NULL);

   add_function("int64BitsToDouble",
                _int64BitsToDouble(int64_fp64, &glsl_type_builtin_int64_t),
                _int64BitsToDouble(int64_fp64, &glsl_type_builtin_i64vec2),
                _int64BitsToDouble(int64_fp64, &glsl_type_builtin_i64vec3),
                _int64BitsToDouble(int64_fp64, &glsl_type_builtin_i64vec4),
                NULL);

   add_function("uint64BitsToDouble",
                _uint64BitsToDouble(int64_fp64, &glsl_type_builtin_uint64_t),
                _uint64BitsToDouble(int64_fp64, &glsl_type_builtin_u64vec2),
                _uint64BitsToDouble(int64_fp64, &glsl_type_builtin_u64vec3),
                _uint64BitsToDouble(int64_fp64, &glsl_type_builtin_u64vec4),
                NULL);

   add_function("packUnorm2x16",   _packUnorm2x16(shader_packing_or_es3_or_gpu_shader5),   NULL);
   add_function("packSnorm2x16",   _packSnorm2x16(shader_packing_or_es3),                  NULL);
   add_function("packUnorm4x8",    _packUnorm4x8(shader_packing_or_es31_or_gpu_shader5),   NULL);
   add_function("packSnorm4x8",    _packSnorm4x8(shader_packing_or_es31_or_gpu_shader5),   NULL);
   add_function("unpackUnorm2x16", _unpackUnorm2x16(shader_packing_or_es3_or_gpu_shader5), NULL);
   add_function("unpackSnorm2x16", _unpackSnorm2x16(shader_packing_or_es3),                NULL);
   add_function("unpackUnorm4x8",  _unpackUnorm4x8(shader_packing_or_es31_or_gpu_shader5), NULL);
   add_function("unpackSnorm4x8",  _unpackSnorm4x8(shader_packing_or_es31_or_gpu_shader5), NULL);
   add_function("packHalf2x16",    _packHalf2x16(shader_packing_or_es3),                   NULL);
   add_function("unpackHalf2x16",  _unpackHalf2x16(shader_packing_or_es3),                 NULL);
   add_function("packFloat2x16",    _packFloat2x16(gpu_shader_half_float),                 NULL);
   add_function("unpackFloat2x16",  _unpackFloat2x16(gpu_shader_half_float),               NULL);
   add_function("packDouble2x32",    _packDouble2x32(fp64),                   NULL);
   add_function("unpackDouble2x32",  _unpackDouble2x32(fp64),                 NULL);

   add_function("packInt2x32",     _packInt2x32(int64_avail),                    NULL);
   add_function("unpackInt2x32",   _unpackInt2x32(int64_avail),                  NULL);
   add_function("packUint2x32",    _packUint2x32(int64_avail),                   NULL);
   add_function("unpackUint2x32",  _unpackUint2x32(int64_avail),                 NULL);

   FDHF(length)
   FDHF(distance)
   FDHF(dot)

   add_function("cross", _cross(always_available, &glsl_type_builtin_vec3),
                _cross(fp64, &glsl_type_builtin_dvec3),
                _cross(gpu_shader_half_float, &glsl_type_builtin_f16vec3),
                NULL);

   FDHF(normalize)
   add_function("ftransform", _ftransform(), NULL);
   FDHF(faceforward)
   FDHF(reflect)
   FDHF(refract)
   // ...
   add_function("matrixCompMult",
                _matrixCompMult(always_available, &glsl_type_builtin_mat2),
                _matrixCompMult(always_available, &glsl_type_builtin_mat3),
                _matrixCompMult(always_available, &glsl_type_builtin_mat4),
                _matrixCompMult(always_available, &glsl_type_builtin_mat2x3),
                _matrixCompMult(always_available, &glsl_type_builtin_mat2x4),
                _matrixCompMult(always_available, &glsl_type_builtin_mat3x2),
                _matrixCompMult(always_available, &glsl_type_builtin_mat3x4),
                _matrixCompMult(always_available, &glsl_type_builtin_mat4x2),
                _matrixCompMult(always_available, &glsl_type_builtin_mat4x3),
                _matrixCompMult(fp64, &glsl_type_builtin_dmat2),
                _matrixCompMult(fp64, &glsl_type_builtin_dmat3),
                _matrixCompMult(fp64, &glsl_type_builtin_dmat4),
                _matrixCompMult(fp64, &glsl_type_builtin_dmat2x3),
                _matrixCompMult(fp64, &glsl_type_builtin_dmat2x4),
                _matrixCompMult(fp64, &glsl_type_builtin_dmat3x2),
                _matrixCompMult(fp64, &glsl_type_builtin_dmat3x4),
                _matrixCompMult(fp64, &glsl_type_builtin_dmat4x2),
                _matrixCompMult(fp64, &glsl_type_builtin_dmat4x3),
                _matrixCompMult(gpu_shader_half_float, &glsl_type_builtin_f16mat2),
                _matrixCompMult(gpu_shader_half_float, &glsl_type_builtin_f16mat3),
                _matrixCompMult(gpu_shader_half_float, &glsl_type_builtin_f16mat4),
                _matrixCompMult(gpu_shader_half_float, &glsl_type_builtin_f16mat2x3),
                _matrixCompMult(gpu_shader_half_float, &glsl_type_builtin_f16mat2x4),
                _matrixCompMult(gpu_shader_half_float, &glsl_type_builtin_f16mat3x2),
                _matrixCompMult(gpu_shader_half_float, &glsl_type_builtin_f16mat3x4),
                _matrixCompMult(gpu_shader_half_float, &glsl_type_builtin_f16mat4x2),
                _matrixCompMult(gpu_shader_half_float, &glsl_type_builtin_f16mat4x3),
                NULL);
   add_function("outerProduct",
                _outerProduct(v120, &glsl_type_builtin_mat2),
                _outerProduct(v120, &glsl_type_builtin_mat3),
                _outerProduct(v120, &glsl_type_builtin_mat4),
                _outerProduct(v120, &glsl_type_builtin_mat2x3),
                _outerProduct(v120, &glsl_type_builtin_mat2x4),
                _outerProduct(v120, &glsl_type_builtin_mat3x2),
                _outerProduct(v120, &glsl_type_builtin_mat3x4),
                _outerProduct(v120, &glsl_type_builtin_mat4x2),
                _outerProduct(v120, &glsl_type_builtin_mat4x3),
                _outerProduct(fp64, &glsl_type_builtin_dmat2),
                _outerProduct(fp64, &glsl_type_builtin_dmat3),
                _outerProduct(fp64, &glsl_type_builtin_dmat4),
                _outerProduct(fp64, &glsl_type_builtin_dmat2x3),
                _outerProduct(fp64, &glsl_type_builtin_dmat2x4),
                _outerProduct(fp64, &glsl_type_builtin_dmat3x2),
                _outerProduct(fp64, &glsl_type_builtin_dmat3x4),
                _outerProduct(fp64, &glsl_type_builtin_dmat4x2),
                _outerProduct(fp64, &glsl_type_builtin_dmat4x3),
                _outerProduct(gpu_shader_half_float, &glsl_type_builtin_f16mat2),
                _outerProduct(gpu_shader_half_float, &glsl_type_builtin_f16mat3),
                _outerProduct(gpu_shader_half_float, &glsl_type_builtin_f16mat4),
                _outerProduct(gpu_shader_half_float, &glsl_type_builtin_f16mat2x3),
                _outerProduct(gpu_shader_half_float, &glsl_type_builtin_f16mat2x4),
                _outerProduct(gpu_shader_half_float, &glsl_type_builtin_f16mat3x2),
                _outerProduct(gpu_shader_half_float, &glsl_type_builtin_f16mat3x4),
                _outerProduct(gpu_shader_half_float, &glsl_type_builtin_f16mat4x2),
                _outerProduct(gpu_shader_half_float, &glsl_type_builtin_f16mat4x3),
                NULL);
   add_function("determinant",
                _determinant_mat2(v120, &glsl_type_builtin_mat2),
                _determinant_mat3(v120, &glsl_type_builtin_mat3),
                _determinant_mat4(v120, &glsl_type_builtin_mat4),
                _determinant_mat2(fp64, &glsl_type_builtin_dmat2),
                _determinant_mat3(fp64, &glsl_type_builtin_dmat3),
                _determinant_mat4(fp64, &glsl_type_builtin_dmat4),
                _determinant_mat2(gpu_shader_half_float, &glsl_type_builtin_f16mat2),
                _determinant_mat3(gpu_shader_half_float, &glsl_type_builtin_f16mat3),
                _determinant_mat4(gpu_shader_half_float, &glsl_type_builtin_f16mat4),
                NULL);
   add_function("inverse",
                _inverse_mat2(v140_or_es3, &glsl_type_builtin_mat2),
                _inverse_mat3(v140_or_es3, &glsl_type_builtin_mat3),
                _inverse_mat4(v140_or_es3, &glsl_type_builtin_mat4),
                _inverse_mat2(fp64, &glsl_type_builtin_dmat2),
                _inverse_mat3(fp64, &glsl_type_builtin_dmat3),
                _inverse_mat4(fp64, &glsl_type_builtin_dmat4),
                _inverse_mat2(gpu_shader_half_float, &glsl_type_builtin_f16mat2),
                _inverse_mat3(gpu_shader_half_float, &glsl_type_builtin_f16mat3),
                _inverse_mat4(gpu_shader_half_float, &glsl_type_builtin_f16mat4),
                NULL);
   add_function("transpose",
                _transpose(v120, &glsl_type_builtin_mat2),
                _transpose(v120, &glsl_type_builtin_mat3),
                _transpose(v120, &glsl_type_builtin_mat4),
                _transpose(v120, &glsl_type_builtin_mat2x3),
                _transpose(v120, &glsl_type_builtin_mat2x4),
                _transpose(v120, &glsl_type_builtin_mat3x2),
                _transpose(v120, &glsl_type_builtin_mat3x4),
                _transpose(v120, &glsl_type_builtin_mat4x2),
                _transpose(v120, &glsl_type_builtin_mat4x3),
                _transpose(fp64, &glsl_type_builtin_dmat2),
                _transpose(fp64, &glsl_type_builtin_dmat3),
                _transpose(fp64, &glsl_type_builtin_dmat4),
                _transpose(fp64, &glsl_type_builtin_dmat2x3),
                _transpose(fp64, &glsl_type_builtin_dmat2x4),
                _transpose(fp64, &glsl_type_builtin_dmat3x2),
                _transpose(fp64, &glsl_type_builtin_dmat3x4),
                _transpose(fp64, &glsl_type_builtin_dmat4x2),
                _transpose(fp64, &glsl_type_builtin_dmat4x3),
                _transpose(gpu_shader_half_float, &glsl_type_builtin_f16mat2),
                _transpose(gpu_shader_half_float, &glsl_type_builtin_f16mat3),
                _transpose(gpu_shader_half_float, &glsl_type_builtin_f16mat4),
                _transpose(gpu_shader_half_float, &glsl_type_builtin_f16mat2x3),
                _transpose(gpu_shader_half_float, &glsl_type_builtin_f16mat2x4),
                _transpose(gpu_shader_half_float, &glsl_type_builtin_f16mat3x2),
                _transpose(gpu_shader_half_float, &glsl_type_builtin_f16mat3x4),
                _transpose(gpu_shader_half_float, &glsl_type_builtin_f16mat4x2),
                _transpose(gpu_shader_half_float, &glsl_type_builtin_f16mat4x3),
                NULL);
   FIUDHF_VEC(lessThan)
   FIUDHF_VEC(lessThanEqual)
   FIUDHF_VEC(greaterThan)
   FIUDHF_VEC(greaterThanEqual)
   FIUBDHF_VEC(notEqual)
   FIUBDHF_VEC(equal)

   add_function("any",
                _any(&glsl_type_builtin_bvec2),
                _any(&glsl_type_builtin_bvec3),
                _any(&glsl_type_builtin_bvec4),
                NULL);

   add_function("all",
                _all(&glsl_type_builtin_bvec2),
                _all(&glsl_type_builtin_bvec3),
                _all(&glsl_type_builtin_bvec4),
                NULL);

   add_function("not",
                _not(&glsl_type_builtin_bvec2),
                _not(&glsl_type_builtin_bvec3),
                _not(&glsl_type_builtin_bvec4),
                NULL);

   add_function("textureSize",
                _textureSize(v130, &glsl_type_builtin_int,   &glsl_type_builtin_sampler1D),
                _textureSize(v130, &glsl_type_builtin_int,   &glsl_type_builtin_isampler1D),
                _textureSize(v130, &glsl_type_builtin_int,   &glsl_type_builtin_usampler1D),

                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler2D),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_isampler2D),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_usampler2D),

                _textureSize(v130, &glsl_type_builtin_ivec3, &glsl_type_builtin_sampler3D),
                _textureSize(v130, &glsl_type_builtin_ivec3, &glsl_type_builtin_isampler3D),
                _textureSize(v130, &glsl_type_builtin_ivec3, &glsl_type_builtin_usampler3D),

                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_samplerCube),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_isamplerCube),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_usamplerCube),

                _textureSize(v130, &glsl_type_builtin_int,   &glsl_type_builtin_sampler1DShadow),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler2DShadow),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_samplerCubeShadow),

                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler1DArray),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_isampler1DArray),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_usampler1DArray),
                _textureSize(v130, &glsl_type_builtin_ivec3, &glsl_type_builtin_sampler2DArray),
                _textureSize(v130, &glsl_type_builtin_ivec3, &glsl_type_builtin_isampler2DArray),
                _textureSize(v130, &glsl_type_builtin_ivec3, &glsl_type_builtin_usampler2DArray),

                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler1DArrayShadow),
                _textureSize(v130, &glsl_type_builtin_ivec3, &glsl_type_builtin_sampler2DArrayShadow),

                _textureSize(texture_cube_map_array, &glsl_type_builtin_ivec3, &glsl_type_builtin_samplerCubeArray),
                _textureSize(texture_cube_map_array, &glsl_type_builtin_ivec3, &glsl_type_builtin_isamplerCubeArray),
                _textureSize(texture_cube_map_array, &glsl_type_builtin_ivec3, &glsl_type_builtin_usamplerCubeArray),
                _textureSize(texture_cube_map_array, &glsl_type_builtin_ivec3, &glsl_type_builtin_samplerCubeArrayShadow),

                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler2DRect),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_isampler2DRect),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_usampler2DRect),
                _textureSize(v130, &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler2DRectShadow),

                _textureSize(texture_buffer, &glsl_type_builtin_int,   &glsl_type_builtin_samplerBuffer),
                _textureSize(texture_buffer, &glsl_type_builtin_int,   &glsl_type_builtin_isamplerBuffer),
                _textureSize(texture_buffer, &glsl_type_builtin_int,   &glsl_type_builtin_usamplerBuffer),
                _textureSize(texture_multisample, &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler2DMS),
                _textureSize(texture_multisample, &glsl_type_builtin_ivec2, &glsl_type_builtin_isampler2DMS),
                _textureSize(texture_multisample, &glsl_type_builtin_ivec2, &glsl_type_builtin_usampler2DMS),

                _textureSize(texture_multisample_array, &glsl_type_builtin_ivec3, &glsl_type_builtin_sampler2DMSArray),
                _textureSize(texture_multisample_array, &glsl_type_builtin_ivec3, &glsl_type_builtin_isampler2DMSArray),
                _textureSize(texture_multisample_array, &glsl_type_builtin_ivec3, &glsl_type_builtin_usampler2DMSArray),

                _textureSize(texture_external_es3, &glsl_type_builtin_ivec2, &glsl_type_builtin_samplerExternalOES),
                NULL);

   add_function("textureSize1D",
                _textureSize(gpu_shader4, &glsl_type_builtin_int,   &glsl_type_builtin_sampler1D),
                _textureSize(gpu_shader4_integer, &glsl_type_builtin_int,   &glsl_type_builtin_isampler1D),
                _textureSize(gpu_shader4_integer, &glsl_type_builtin_int,   &glsl_type_builtin_usampler1D),
                NULL);

   add_function("textureSize2D",
                _textureSize(gpu_shader4, &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler2D),
                _textureSize(gpu_shader4_integer, &glsl_type_builtin_ivec2, &glsl_type_builtin_isampler2D),
                _textureSize(gpu_shader4_integer, &glsl_type_builtin_ivec2, &glsl_type_builtin_usampler2D),
                NULL);

   add_function("textureSize3D",
                _textureSize(gpu_shader4, &glsl_type_builtin_ivec3, &glsl_type_builtin_sampler3D),
                _textureSize(gpu_shader4_integer, &glsl_type_builtin_ivec3, &glsl_type_builtin_isampler3D),
                _textureSize(gpu_shader4_integer, &glsl_type_builtin_ivec3, &glsl_type_builtin_usampler3D),
                NULL);

   add_function("textureSizeCube",
                _textureSize(gpu_shader4, &glsl_type_builtin_ivec2, &glsl_type_builtin_samplerCube),
                _textureSize(gpu_shader4_integer, &glsl_type_builtin_ivec2, &glsl_type_builtin_isamplerCube),
                _textureSize(gpu_shader4_integer, &glsl_type_builtin_ivec2, &glsl_type_builtin_usamplerCube),
                NULL);

   add_function("textureSize1DArray",
                _textureSize(gpu_shader4_array,         &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler1DArray),
                _textureSize(gpu_shader4_array_integer, &glsl_type_builtin_ivec2, &glsl_type_builtin_isampler1DArray),
                _textureSize(gpu_shader4_array_integer, &glsl_type_builtin_ivec2, &glsl_type_builtin_usampler1DArray),
                NULL);

   add_function("textureSize2DArray",
                _textureSize(gpu_shader4_array,         &glsl_type_builtin_ivec3, &glsl_type_builtin_sampler2DArray),
                _textureSize(gpu_shader4_array_integer, &glsl_type_builtin_ivec3, &glsl_type_builtin_isampler2DArray),
                _textureSize(gpu_shader4_array_integer, &glsl_type_builtin_ivec3, &glsl_type_builtin_usampler2DArray),
                NULL);

   add_function("textureSize2DRect",
                _textureSize(gpu_shader4_rect,         &glsl_type_builtin_ivec2, &glsl_type_builtin_sampler2DRect),
                _textureSize(gpu_shader4_rect_integer, &glsl_type_builtin_ivec2, &glsl_type_builtin_isampler2DRect),
                _textureSize(gpu_shader4_rect_integer, &glsl_type_builtin_ivec2, &glsl_type_builtin_usampler2DRect),
                NULL);

   add_function("textureSizeBuffer",
                _textureSize(gpu_shader4_tbo,         &glsl_type_builtin_int,   &glsl_type_builtin_samplerBuffer),
                _textureSize(gpu_shader4_tbo_integer, &glsl_type_builtin_int,   &glsl_type_builtin_isamplerBuffer),
                _textureSize(gpu_shader4_tbo_integer, &glsl_type_builtin_int,   &glsl_type_builtin_usamplerBuffer),
                NULL);

   add_function("textureSamples",
                _textureSamples(shader_samples, &glsl_type_builtin_sampler2DMS),
                _textureSamples(shader_samples, &glsl_type_builtin_isampler2DMS),
                _textureSamples(shader_samples, &glsl_type_builtin_usampler2DMS),

                _textureSamples(shader_samples, &glsl_type_builtin_sampler2DMSArray),
                _textureSamples(shader_samples, &glsl_type_builtin_isampler2DMSArray),
                _textureSamples(shader_samples, &glsl_type_builtin_usampler2DMSArray),
                NULL);

   add_function("texture",
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow,   &glsl_type_builtin_vec3),
                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3),
                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3),

                _texture(ir_tex, texture_cube_map_array, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4),
                _texture(ir_tex, texture_cube_map_array, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4),
                _texture(ir_tex, texture_cube_map_array, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3),
                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),
                /* samplerCubeArrayShadow is special; it has an extra parameter
                 * for the shadow comparator since there is no vec5 type.
                 */
                _textureCubeArrayShadow(ir_tex, texture_cube_map_array, &glsl_type_builtin_samplerCubeArrayShadow),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3),

                _texture(ir_tex, texture_external_es3, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerExternalOES, &glsl_type_builtin_vec2),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow,   &glsl_type_builtin_vec3),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3),

                _texture(ir_txb, derivatives_texture_cube_map_array, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4),
                _texture(ir_txb, derivatives_texture_cube_map_array, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4),
                _texture(ir_txb, derivatives_texture_cube_map_array, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3),
                _texture(ir_tex, v130_or_gpu_shader4_and_tex_shadow_lod, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),
                _texture(ir_txb, v130_or_gpu_shader4_and_tex_shadow_lod, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),

                _textureCubeArrayShadow(ir_tex, v130_or_gpu_shader4_and_tex_cube_map_array, &glsl_type_builtin_samplerCubeArrayShadow),
                _textureCubeArrayShadow(ir_txb, v130_or_gpu_shader4_and_tex_cube_map_array, &glsl_type_builtin_samplerCubeArrayShadow),
                NULL);

   add_function("textureLod",
                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),

                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3),
                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3),

                _texture(ir_txl, texture_cube_map_array, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4),
                _texture(ir_txl, texture_cube_map_array, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4),
                _texture(ir_txl, texture_cube_map_array, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4),

                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3),
                _texture(ir_txl, v130_or_gpu_shader4_and_tex_shadow_lod, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),
                _texture(ir_txl, v130_or_gpu_shader4_and_tex_shadow_lod, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4),
                _textureCubeArrayShadow(ir_txl, v130_or_gpu_shader4_and_tex_cube_map_array, &glsl_type_builtin_samplerCubeArrayShadow),
                NULL);

   add_function("textureOffset",
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                /* The next one was forgotten in GLSL 1.30 spec. It's from
                 * EXT_gpu_shader4 originally. It was added in 4.30 with the
                 * wrong syntax. This was corrected in 4.40. 4.30 indicates
                 * that it was intended to be included previously, so allow it
                 * in 1.30.
                 */
                _texture(ir_tex, v130_desktop, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, v130_or_gpu_shader4_and_tex_shadow_lod, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET),
                _texture(ir_txb, v130_or_gpu_shader4_and_tex_shadow_lod, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET),
                NULL);

   add_function("texture1DOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                NULL);

   add_function("texture2DOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                NULL);

   add_function("texture3DOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("texture2DRectOffset",
                _texture(ir_tex, gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET),
                NULL);

   add_function("shadow2DRectOffset",
                _texture(ir_tex, gpu_shader4_rect, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow1DOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow2DOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("texture1DArrayOffset",
                _texture(ir_tex, gpu_shader4_array,                     &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_array_integer,             &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_array_integer,             &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_array_derivs_only,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_array_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_array_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                NULL);

   add_function("texture2DArrayOffset",
                _texture(ir_tex, gpu_shader4_array,                     &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_array_integer,             &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_array_integer,             &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_array_derivs_only,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_array_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_array_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow1DArrayOffset",
                _texture(ir_tex, gpu_shader4_array,             &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_array_derivs_only, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow2DArrayOffset",
                _texture(ir_tex, gpu_shader4_array, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET),
                NULL);

   add_function("textureProj",
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, texture_external_es3, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerExternalOES, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, texture_external_es3, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerExternalOES, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texelFetch",
                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_int),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_int),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_int),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_ivec2),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_ivec3),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_ivec3),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_ivec3),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_ivec2),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_ivec2),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_ivec3),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_ivec3),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_ivec3),

                _texelFetch(texture_buffer, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerBuffer,  &glsl_type_builtin_int),
                _texelFetch(texture_buffer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerBuffer, &glsl_type_builtin_int),
                _texelFetch(texture_buffer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerBuffer, &glsl_type_builtin_int),

                _texelFetch(texture_multisample, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DMS,  &glsl_type_builtin_ivec2),
                _texelFetch(texture_multisample, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DMS, &glsl_type_builtin_ivec2),
                _texelFetch(texture_multisample, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DMS, &glsl_type_builtin_ivec2),

                _texelFetch(texture_multisample_array, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DMSArray,  &glsl_type_builtin_ivec3),
                _texelFetch(texture_multisample_array, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DMSArray, &glsl_type_builtin_ivec3),
                _texelFetch(texture_multisample_array, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DMSArray, &glsl_type_builtin_ivec3),

                _texelFetch(texture_external_es3, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerExternalOES, &glsl_type_builtin_ivec2),

                NULL);

   add_function("texelFetch1D",
                _texelFetch(gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_int),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_int),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_int),
                NULL);

   add_function("texelFetch2D",
                _texelFetch(gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_ivec2),
                NULL);

   add_function("texelFetch3D",
                _texelFetch(gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_ivec3),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_ivec3),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_ivec3),
                NULL);

   add_function("texelFetch2DRect",
                _texelFetch(gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_rect_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_rect_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_ivec2),
                NULL);

   add_function("texelFetch1DArray",
                _texelFetch(gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_ivec2),
                NULL);

   add_function("texelFetch2DArray",
                _texelFetch(gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_ivec3),
                _texelFetch(gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_ivec3),
                _texelFetch(gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_ivec3),
                NULL);

   add_function("texelFetchBuffer",
                _texelFetch(gpu_shader4_tbo,         &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerBuffer,  &glsl_type_builtin_int),
                _texelFetch(gpu_shader4_tbo_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerBuffer, &glsl_type_builtin_int),
                _texelFetch(gpu_shader4_tbo_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerBuffer, &glsl_type_builtin_int),
                NULL);

   add_function("texelFetchOffset",
                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_int, &glsl_type_builtin_int),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_int, &glsl_type_builtin_int),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_int, &glsl_type_builtin_int),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_ivec2, &glsl_type_builtin_int),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_ivec2, &glsl_type_builtin_int),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_ivec2, &glsl_type_builtin_int),

                _texelFetch(v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec2),
                _texelFetch(v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec2),

                NULL);

   add_function("texelFetch1DOffset",
                _texelFetch(gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_int, &glsl_type_builtin_int),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_int, &glsl_type_builtin_int),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_int, &glsl_type_builtin_int),
                NULL);

   add_function("texelFetch2DOffset",
                _texelFetch(gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                NULL);

   add_function("texelFetch3DOffset",
                _texelFetch(gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3),
                _texelFetch(gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3),
                NULL);

   add_function("texelFetch2DRectOffset",
                _texelFetch(gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_rect_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_rect_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2),
                NULL);

   add_function("texelFetch1DArrayOffset",
                _texelFetch(gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_ivec2, &glsl_type_builtin_int),
                _texelFetch(gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_ivec2, &glsl_type_builtin_int),
                _texelFetch(gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_ivec2, &glsl_type_builtin_int),
                NULL);

   add_function("texelFetch2DArrayOffset",
                _texelFetch(gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec2),
                _texelFetch(gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec2),
                NULL);

   add_function("textureProjOffset",
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_tex, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, v130_derivatives_only, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture1DProjOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture2DProjOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture3DProjOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_integer,     &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("shadow1DProjOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("shadow2DProjOffset",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture2DRectProjOffset",
                _texture(ir_tex, gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("shadow2DRectProjOffset",
                _texture(ir_tex, gpu_shader4_rect, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("textureLodOffset",
                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, v130_or_gpu_shader4_and_tex_shadow_lod, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET),
                NULL);

   add_function("texture1DLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                NULL);

   add_function("texture2DLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                NULL);

   add_function("texture3DLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow1DLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow2DLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("texture1DArrayLodOffset",
                _texture(ir_txl, gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                NULL);

   add_function("texture2DArrayLodOffset",
                _texture(ir_txl, gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow1DArrayLodOffset",
                _texture(ir_txl, gpu_shader4_array, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("textureProjLod",
                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("textureProjLodOffset",
                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txl, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture1DProjLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture2DProjLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture3DProjLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("shadow1DProjLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("shadow2DProjLodOffset",
                _texture(ir_txl, gpu_shader4, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("textureGrad",
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow,   &glsl_type_builtin_vec3),
                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3),
                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3),

                _texture(ir_txd, texture_cube_map_array, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4),
                _texture(ir_txd, texture_cube_map_array, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4),
                _texture(ir_txd, texture_cube_map_array, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3),
                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),
                NULL);

   add_function("textureGradOffset",
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET),
                NULL);

   add_function("texture1DGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET),
                NULL);

   add_function("texture2DGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                NULL);

   add_function("texture3DGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("texture2DRectGradOffset",
                _texture(ir_txd, gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET),
                NULL);

   add_function("shadow2DRectGradOffset",
                _texture(ir_txd, gpu_shader4_rect, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow1DGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow2DGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("texture1DArrayGradOffset",
                _texture(ir_txd, gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET),
                NULL);

   add_function("texture2DArrayGradOffset",
                _texture(ir_txd, gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow1DArrayGradOffset",
                _texture(ir_txd, gpu_shader4_array, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("shadow2DArrayGradOffset",
                _texture(ir_txd, gpu_shader4_array, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET),
                NULL);

   add_function("textureProjGrad",
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec4, TEX_PROJECT),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("textureProjGradOffset",
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),

                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, v130, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture1DProjGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture2DProjGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture3DProjGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("texture2DRectProjGradOffset",
                _texture(ir_txd, gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("shadow2DRectProjGradOffset",
                _texture(ir_txd, gpu_shader4_rect, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("shadow1DProjGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("shadow2DProjGradOffset",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT | TEX_OFFSET),
                NULL);

   add_function("EmitVertex",   _EmitVertex(),   NULL);
   add_function("EndPrimitive", _EndPrimitive(), NULL);
   add_function("EmitStreamVertex",
                _EmitStreamVertex(gs_streams, &glsl_type_builtin_uint),
                _EmitStreamVertex(gs_streams, &glsl_type_builtin_int),
                NULL);
   add_function("EndStreamPrimitive",
                _EndStreamPrimitive(gs_streams, &glsl_type_builtin_uint),
                _EndStreamPrimitive(gs_streams, &glsl_type_builtin_int),
                NULL);
   add_function("barrier", _barrier(), NULL);

   add_function("textureQueryLOD",
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),

                _textureQueryLod(texture_query_lod, &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),

                _textureQueryLod(texture_query_lod, &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),

                _textureQueryLod(texture_query_lod, &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),

                _textureQueryLod(texture_query_lod, &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_float),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_float),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_float),

                _textureQueryLod(texture_query_lod, &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec2),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec2),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec2),

                _textureQueryLod(texture_query_lod, &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec3),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec3),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec3),

                _textureQueryLod(texture_query_lod, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_float),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec2),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec3),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_float),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec2),
                _textureQueryLod(texture_query_lod, &glsl_type_builtin_samplerCubeArrayShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("textureQueryLod",
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),

                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),

                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),

                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),

                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_float),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_float),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_float),

                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec2),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec2),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec2),

                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec3),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec3),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec3),

                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_float),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec2),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec3),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_float),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec2),
                _textureQueryLod(v400_derivatives_only, &glsl_type_builtin_samplerCubeArrayShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("textureQueryLevels",
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_sampler1D),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_sampler2D),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_sampler3D),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_samplerCube),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_sampler1DArray),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_sampler2DArray),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_samplerCubeArray),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_sampler1DShadow),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_sampler2DShadow),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_samplerCubeShadow),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_sampler1DArrayShadow),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_sampler2DArrayShadow),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_samplerCubeArrayShadow),

                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_isampler1D),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_isampler2D),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_isampler3D),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_isamplerCube),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_isampler1DArray),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_isampler2DArray),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_isamplerCubeArray),

                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_usampler1D),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_usampler2D),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_usampler3D),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_usamplerCube),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_usampler1DArray),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_usampler2DArray),
                _textureQueryLevels(texture_query_levels, &glsl_type_builtin_usamplerCubeArray),

                NULL);

   add_function("textureSamplesIdenticalEXT",
                _textureSamplesIdentical(texture_samples_identical, &glsl_type_builtin_sampler2DMS,  &glsl_type_builtin_ivec2),
                _textureSamplesIdentical(texture_samples_identical, &glsl_type_builtin_isampler2DMS, &glsl_type_builtin_ivec2),
                _textureSamplesIdentical(texture_samples_identical, &glsl_type_builtin_usampler2DMS, &glsl_type_builtin_ivec2),

                _textureSamplesIdentical(texture_samples_identical_array, &glsl_type_builtin_sampler2DMSArray,  &glsl_type_builtin_ivec3),
                _textureSamplesIdentical(texture_samples_identical_array, &glsl_type_builtin_isampler2DMSArray, &glsl_type_builtin_ivec3),
                _textureSamplesIdentical(texture_samples_identical_array, &glsl_type_builtin_usampler2DMSArray, &glsl_type_builtin_ivec3),
                NULL);

   add_function("texture1D",
                _texture(ir_tex, v110_deprecated_texture,                      &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_float),
                _texture(ir_txb, v110_derivatives_only_deprecated_texture,     &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_float),
                _texture(ir_tex, gpu_shader4_integer,               &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _texture(ir_txb, gpu_shader4_integer_derivs_only,   &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _texture(ir_tex, gpu_shader4_integer,               &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),
                _texture(ir_txb, gpu_shader4_integer_derivs_only,   &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),
                NULL);

   add_function("texture1DArray",
                _texture(ir_tex, texture_array,           &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txb, texture_array_derivs_only,&glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_tex, gpu_shader4_array_integer,             &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txb, gpu_shader4_array_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_tex, gpu_shader4_array_integer,             &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txb, gpu_shader4_array_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture1DProj",
                _texture(ir_tex, v110_deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_tex, v110_deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v110_derivatives_only_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txb, v110_derivatives_only_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture1DLod",
                _texture(ir_txl, v110_lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_float),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),
                NULL);

   add_function("texture1DArrayLod",
                _texture(ir_txl, texture_array_lod, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txl, gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txl, gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture1DProjLod",
                _texture(ir_txl, v110_lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txl, v110_lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture2D",
                _texture(ir_tex, deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txb, deprecated_texture_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),
                _texture(ir_tex, texture_external,        &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerExternalOES, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture2DArray",
                _texture(ir_tex, texture_array,           &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txb, texture_array_derivs_only, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_tex, gpu_shader4_array_integer,             &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txb, gpu_shader4_array_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_tex, gpu_shader4_array_integer,             &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txb, gpu_shader4_array_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3),
                NULL);

   add_function("texture2DProj",
                _texture(ir_tex, deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, deprecated_texture_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txb, deprecated_texture_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, texture_external,        &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerExternalOES, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, texture_external,        &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerExternalOES, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture2DLod",
                _texture(ir_txl, lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture2DArrayLod",
                _texture(ir_txl, texture_array_lod, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txl, gpu_shader4_array_integer, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txl, gpu_shader4_array_integer, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3),
                NULL);

   add_function("texture2DProjLod",
                _texture(ir_txl, lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txl, lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture3D",
                _texture(ir_tex, tex3d,                   &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txb, derivatives_tex3d,       &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec3),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),
                NULL);

   add_function("texture3DProj",
                _texture(ir_tex, tex3d,                   &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, derivatives_tex3d,       &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture3DLod",
                _texture(ir_txl, tex3d_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),
                NULL);

   add_function("texture3DProjLod",
                _texture(ir_txl, tex3d_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("textureCube",
                _texture(ir_tex, deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txb, deprecated_texture_derivatives_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube, &glsl_type_builtin_vec3),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_ivec4,  &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_tex, gpu_shader4_integer,             &glsl_type_builtin_uvec4,  &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txb, gpu_shader4_integer_derivs_only, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),
                NULL);

   add_function("textureCubeLod",
                _texture(ir_txl, lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txl, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),
                NULL);

   add_function("texture2DRect",
                _texture(ir_tex, texture_rectangle, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec2),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture2DRectProj",
                _texture(ir_tex, texture_rectangle, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, texture_rectangle, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_tex, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow1D",
                _texture(ir_tex, v110_deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3),
                _texture(ir_txb, v110_derivatives_only_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow1DArray",
                _texture(ir_tex, texture_array,    &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3),
                _texture(ir_txb, texture_array_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2D",
                _texture(ir_tex, v110_deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3),
                _texture(ir_txb, v110_derivatives_only_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2DEXT",
                _texture(ir_tex, texture_shadow2Dext,                  &glsl_type_builtin_float,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3),
                _texture(ir_txb, texture_shadow2Dext, &glsl_type_builtin_float,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2DArray",
                _texture(ir_tex, texture_array,    &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),
                _texture(ir_txb, texture_array_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),
                NULL);

   add_function("shadow1DProj",
                _texture(ir_tex, v110_deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v110_derivatives_only_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow2DArray",
                _texture(ir_tex, texture_array,    &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),
                _texture(ir_txb, texture_array_derivs_only, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),
                NULL);

   add_function("shadowCube",
                _texture(ir_tex, gpu_shader4,             &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4),
                _texture(ir_txb, gpu_shader4_derivs_only, &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4),
                NULL);

   add_function("shadow2DProj",
                _texture(ir_tex, v110_deprecated_texture,                  &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, v110_derivatives_only_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow2DProjEXT",
                _texture(ir_tex, texture_shadow2Dext,                  &glsl_type_builtin_float,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txb, texture_shadow2Dext, &glsl_type_builtin_float,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow1DLod",
                _texture(ir_txl, v110_lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2DLod",
                _texture(ir_txl, v110_lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow1DArrayLod",
                _texture(ir_txl, texture_array_lod, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow1DProjLod",
                _texture(ir_txl, v110_lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow2DProjLod",
                _texture(ir_txl, v110_lod_deprecated_texture, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow2DRect",
                _texture(ir_tex, texture_rectangle, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2DRectProj",
                _texture(ir_tex, texture_rectangle, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture1DGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_float),
                NULL);

   add_function("texture1DProjGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture2DGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture2DProjGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture3DGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec3),
                NULL);

   add_function("texture3DProjGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("textureCubeGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow1DGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow1DProjGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow2DGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2DProjGradARB",
                _texture(ir_txd, shader_texture_lod, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture2DRectGradARB",
                _texture(ir_txd, shader_texture_lod_and_rect, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture2DRectProjGradARB",
                _texture(ir_txd, shader_texture_lod_and_rect, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, shader_texture_lod_and_rect, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow2DRectGradARB",
                _texture(ir_txd, shader_texture_lod_and_rect, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2DRectProjGradARB",
                _texture(ir_txd, shader_texture_lod_and_rect, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture4",
                _texture(ir_tg4, texture_texture4, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture1DGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_float),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_float),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_float),
                NULL);

   add_function("texture1DProjGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec2, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture1DArrayGrad",
                _texture(ir_txd, gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txd, gpu_shader4_array_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2),
                _texture(ir_txd, gpu_shader4_array_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture2DGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture2DProjGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("texture2DArrayGrad",
                _texture(ir_txd, gpu_shader4_array,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txd, gpu_shader4_array_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_txd, gpu_shader4_array_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3),
                NULL);

   add_function("texture3DGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3),
                NULL);

   add_function("texture3DProjGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("textureCubeGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_txd, gpu_shader4_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow1DGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow1DProjGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow1DArrayGrad",
                _texture(ir_txd, gpu_shader4_array, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2DGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2DProjGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow2DArrayGrad",
                _texture(ir_txd, gpu_shader4_array, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4),
                NULL);

   add_function("texture2DRectGrad",
                _texture(ir_txd, gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec2),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2),
                NULL);

   add_function("texture2DRectProjGrad",
                _texture(ir_txd, gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_rect,         &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_ivec4,  &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec3, TEX_PROJECT),
                _texture(ir_txd, gpu_shader4_rect_integer, &glsl_type_builtin_uvec4,  &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadow2DRectGrad",
                _texture(ir_txd, gpu_shader4_rect, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3),
                NULL);

   add_function("shadow2DRectProjGrad",
                _texture(ir_txd, gpu_shader4_rect, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec4, TEX_PROJECT),
                NULL);

   add_function("shadowCubeGrad",
                _texture(ir_txd, gpu_shader4, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4),
                NULL);

   add_function("textureGather",
                _texture(ir_tg4, texture_gather_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2),
                _texture(ir_tg4, texture_gather_or_es31, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2),
                _texture(ir_tg4, texture_gather_or_es31, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2),

                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec2),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2),

                _texture(ir_tg4, texture_gather_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_tg4, texture_gather_or_es31, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3),
                _texture(ir_tg4, texture_gather_or_es31, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3),

                _texture(ir_tg4, texture_gather_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCube, &glsl_type_builtin_vec3),
                _texture(ir_tg4, texture_gather_or_es31, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3),
                _texture(ir_tg4, texture_gather_or_es31, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3),

                _texture(ir_tg4, texture_gather_cube_map_array, &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCubeArray, &glsl_type_builtin_vec4),
                _texture(ir_tg4, texture_gather_cube_map_array, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4),
                _texture(ir_tg4, texture_gather_cube_map_array, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4),

                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec2, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCube, &glsl_type_builtin_vec3, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5_or_OES_texture_cube_map_array, &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCubeArray, &glsl_type_builtin_vec4, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_or_OES_texture_cube_map_array, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_or_OES_texture_cube_map_array, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec2),
                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec3),
                _texture(ir_tg4, gpu_shader5_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec3),
                _texture(ir_tg4, gpu_shader5_or_OES_texture_cube_map_array, &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCubeArrayShadow, &glsl_type_builtin_vec4),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec2),
                NULL);

   add_function("textureGatherOffset",
                _texture(ir_tg4, texture_gather_only_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tg4, texture_gather_only_or_es31, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tg4, texture_gather_only_or_es31, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET),

                _texture(ir_tg4, texture_gather_only_or_es31, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tg4, texture_gather_only_or_es31, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),
                _texture(ir_tg4, texture_gather_only_or_es31, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET),

                _texture(ir_tg4, es31_not_gs5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2, TEX_OFFSET | TEX_COMPONENT),
                _texture(ir_tg4, es31_not_gs5, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET | TEX_COMPONENT),
                _texture(ir_tg4, es31_not_gs5, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET | TEX_COMPONENT),

                _texture(ir_tg4, es31_not_gs5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET | TEX_COMPONENT),
                _texture(ir_tg4, es31_not_gs5, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET | TEX_COMPONENT),
                _texture(ir_tg4, es31_not_gs5, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET | TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST),

                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST),

                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST),

                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST | TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST | TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST | TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST),

                _texture(ir_tg4, es31_not_gs5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec2, TEX_OFFSET),
                _texture(ir_tg4, es31_not_gs5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET),
                NULL);

   add_function("textureGatherOffsets",
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY),

                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY | TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY),

                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY | TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY),

                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY | TEX_COMPONENT),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY | TEX_COMPONENT),

                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY),
                _texture(ir_tg4, gpu_shader5_es, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY),
                _texture(ir_tg4, gpu_shader5, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY),
                NULL);

   add_function("sparseTextureARB",
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4, TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_tex, texture_cube_map_array_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_tex, texture_cube_map_array_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_tex, texture_cube_map_array_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_SPARSE),

                _textureCubeArrayShadow(ir_tex, texture_cube_map_array_and_sparse, &glsl_type_builtin_samplerCubeArrayShadow, TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),

                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4, TEX_SPARSE),

                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txb, derivatives_texture_cube_map_array_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_txb, derivatives_texture_cube_map_array_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_txb, derivatives_texture_cube_map_array_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),
                NULL);

   add_function("sparseTextureLodARB",
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),

                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txl, texture_cube_map_array_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_txl, texture_cube_map_array_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_txl, texture_cube_map_array_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),
                NULL);

   add_function("sparseTextureOffsetARB",
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_tex, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txb, v130_derivatives_only_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                NULL);

   add_function("sparseTexelFetchARB",
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_ivec2, NULL, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_ivec2, NULL, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_ivec2, NULL, true),

                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_ivec3, NULL, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_ivec3, NULL, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_ivec3, NULL, true),

                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_ivec2, NULL, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_ivec2, NULL, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_ivec2, NULL, true),

                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_ivec3, NULL, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_ivec3, NULL, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_ivec3, NULL, true),

                _texelFetch(texture_multisample_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DMS,  &glsl_type_builtin_ivec2, NULL, true),
                _texelFetch(texture_multisample_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DMS, &glsl_type_builtin_ivec2, NULL, true),
                _texelFetch(texture_multisample_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DMS, &glsl_type_builtin_ivec2, NULL, true),

                _texelFetch(texture_multisample_array_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DMSArray,  &glsl_type_builtin_ivec3, NULL, true),
                _texelFetch(texture_multisample_array_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DMSArray, &glsl_type_builtin_ivec3, NULL, true),
                _texelFetch(texture_multisample_array_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DMSArray, &glsl_type_builtin_ivec3, NULL, true),
                NULL);

   add_function("sparseTexelFetchOffsetARB",
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2, true),

                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec3, true),

                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_ivec2, &glsl_type_builtin_ivec2, true),

                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec2, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec2, true),
                _texelFetch(v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_ivec3, &glsl_type_builtin_ivec2, true),
                NULL);

   add_function("sparseTextureLodOffsetARB",
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txl, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                NULL);

   add_function("sparseTextureGradARB",
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4, TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_txd, texture_cube_map_array_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_txd, texture_cube_map_array_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_txd, texture_cube_map_array_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_SPARSE),
                NULL);

   add_function("sparseTextureGradOffsetARB",
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DRectShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),
                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE),

                _texture(ir_txd, v130_desktop_and_sparse, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET|TEX_SPARSE),
                NULL);

   add_function("sparseTextureGatherARB",
                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_SPARSE),

                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE),

                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_tg4, texture_gather_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow,        &glsl_type_builtin_vec2, TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArrayShadow,   &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCubeShadow,      &glsl_type_builtin_vec3, TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_samplerCubeArrayShadow, &glsl_type_builtin_vec4, TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow,    &glsl_type_builtin_vec2, TEX_SPARSE),
                NULL);

   add_function("sparseTextureGatherOffsetARB",
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST|TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow,      &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET_NONCONST|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow,  &glsl_type_builtin_vec2, TEX_OFFSET_NONCONST|TEX_SPARSE),
                NULL);

   add_function("sparseTextureGatherOffsetsARB",
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY|TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DRect,  &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_COMPONENT|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DRect, &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_COMPONENT|TEX_SPARSE),

                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DShadow,      &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET_ARRAY|TEX_SPARSE),
                _texture(ir_tg4, gpu_shader5_and_sparse, &glsl_type_builtin_vec4, &glsl_type_builtin_sampler2DRectShadow,  &glsl_type_builtin_vec2, TEX_OFFSET_ARRAY|TEX_SPARSE),
                NULL);

   add_function("sparseTexelsResidentARB", _is_sparse_texels_resident(), NULL);

   add_function("sparseTextureClampARB",
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, texture_cube_map_array_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, texture_cube_map_array_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, texture_cube_map_array_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),

                _textureCubeArrayShadow(ir_tex, texture_cube_map_array_and_clamp, &glsl_type_builtin_samplerCubeArrayShadow, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, derivatives_texture_cube_map_array_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, derivatives_texture_cube_map_array_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, derivatives_texture_cube_map_array_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),
                NULL);

   add_function("textureClampARB",
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow,   &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4, TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_CLAMP),

                _texture(ir_tex, texture_cube_map_array_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_CLAMP),
                _texture(ir_tex, texture_cube_map_array_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_CLAMP),
                _texture(ir_tex, texture_cube_map_array_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_CLAMP),

                _textureCubeArrayShadow(ir_tex, texture_cube_map_array_and_clamp, &glsl_type_builtin_samplerCubeArrayShadow, TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow,   &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4, TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_CLAMP),

                _texture(ir_txb, derivatives_texture_cube_map_array_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_CLAMP),
                _texture(ir_txb, derivatives_texture_cube_map_array_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_CLAMP),
                _texture(ir_txb, derivatives_texture_cube_map_array_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_CLAMP),

                _texture(ir_txb, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_CLAMP),
                NULL);

   add_function("sparseTextureOffsetClampARB",
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                NULL);

   add_function("textureOffsetClampARB",
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_tex, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txb, v130_derivatives_only_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txb, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                NULL);

   add_function("sparseTextureGradClampARB",
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, texture_cube_map_array_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, texture_cube_map_array_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, texture_cube_map_array_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_SPARSE|TEX_CLAMP),
                NULL);

   add_function("textureGradClampARB",
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCube,  &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCube, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCube, &glsl_type_builtin_vec3, TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow,   &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow,   &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_samplerCubeShadow, &glsl_type_builtin_vec4, TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_CLAMP),

                _texture(ir_txd, texture_cube_map_array_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_samplerCubeArray,  &glsl_type_builtin_vec4, TEX_CLAMP),
                _texture(ir_txd, texture_cube_map_array_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isamplerCubeArray, &glsl_type_builtin_vec4, TEX_CLAMP),
                _texture(ir_txd, texture_cube_map_array_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usamplerCubeArray, &glsl_type_builtin_vec4, TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_CLAMP),
                NULL);

   add_function("sparseTextureGradOffsetClampARB",
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET|TEX_SPARSE|TEX_CLAMP),
                NULL);

   add_function("textureGradOffsetClampARB",
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1D,  &glsl_type_builtin_float, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1D, &glsl_type_builtin_float, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1D, &glsl_type_builtin_float, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2D,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2D, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler3D,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler3D, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler1DArray,  &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler1DArray, &glsl_type_builtin_vec2, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_vec4,  &glsl_type_builtin_sampler2DArray,  &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_ivec4, &glsl_type_builtin_isampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_uvec4, &glsl_type_builtin_usampler2DArray, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),

                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler1DArrayShadow, &glsl_type_builtin_vec3, TEX_OFFSET|TEX_CLAMP),
                _texture(ir_txd, v130_desktop_and_clamp, &glsl_type_builtin_float, &glsl_type_builtin_sampler2DArrayShadow, &glsl_type_builtin_vec4, TEX_OFFSET|TEX_CLAMP),
                NULL);

   FHF_DERIVATIVES(dFdx)
   FHF_DERIVATIVES(dFdy)
   FHF_DERIVATIVES(fwidth)
   FHF_DERIVATIVE_CONTROL(dFdxCoarse)
   FHF_DERIVATIVE_CONTROL(dFdyCoarse)
   FHF_DERIVATIVE_CONTROL(fwidthCoarse)
   FHF_DERIVATIVE_CONTROL(dFdxFine)
   FHF_DERIVATIVE_CONTROL(dFdyFine)
   FHF_DERIVATIVE_CONTROL(fwidthFine)
   F(noise1)
   F(noise2)
   F(noise3)
   F(noise4)

   IU(bitfieldExtract)
   IU(bitfieldInsert)
   IU(bitfieldReverse)
   IU(bitCount)
   IU(findLSB)
   IU(findMSB)
   FDHFGS5(fma)

   add_function("ldexp",
                _ldexp(&glsl_type_builtin_float, &glsl_type_builtin_int),
                _ldexp(&glsl_type_builtin_vec2,  &glsl_type_builtin_ivec2),
                _ldexp(&glsl_type_builtin_vec3,  &glsl_type_builtin_ivec3),
                _ldexp(&glsl_type_builtin_vec4,  &glsl_type_builtin_ivec4),
                _ldexp(&glsl_type_builtin_double, &glsl_type_builtin_int),
                _ldexp(&glsl_type_builtin_dvec2,  &glsl_type_builtin_ivec2),
                _ldexp(&glsl_type_builtin_dvec3,  &glsl_type_builtin_ivec3),
                _ldexp(&glsl_type_builtin_dvec4,  &glsl_type_builtin_ivec4),
                _ldexp(&glsl_type_builtin_float16_t, &glsl_type_builtin_int),
                _ldexp(&glsl_type_builtin_f16vec2,  &glsl_type_builtin_ivec2),
                _ldexp(&glsl_type_builtin_f16vec3,  &glsl_type_builtin_ivec3),
                _ldexp(&glsl_type_builtin_f16vec4,  &glsl_type_builtin_ivec4),
                NULL);

   add_function("frexp",
                _frexp(&glsl_type_builtin_float, &glsl_type_builtin_int),
                _frexp(&glsl_type_builtin_vec2,  &glsl_type_builtin_ivec2),
                _frexp(&glsl_type_builtin_vec3,  &glsl_type_builtin_ivec3),
                _frexp(&glsl_type_builtin_vec4,  &glsl_type_builtin_ivec4),
                _frexp(&glsl_type_builtin_double, &glsl_type_builtin_int),
                _frexp(&glsl_type_builtin_dvec2,  &glsl_type_builtin_ivec2),
                _frexp(&glsl_type_builtin_dvec3,  &glsl_type_builtin_ivec3),
                _frexp(&glsl_type_builtin_dvec4,  &glsl_type_builtin_ivec4),
                _frexp(&glsl_type_builtin_float16_t, &glsl_type_builtin_int),
                _frexp(&glsl_type_builtin_f16vec2,  &glsl_type_builtin_ivec2),
                _frexp(&glsl_type_builtin_f16vec3,  &glsl_type_builtin_ivec3),
                _frexp(&glsl_type_builtin_f16vec4,  &glsl_type_builtin_ivec4),
                NULL);
   add_function("uaddCarry",
                _uaddCarry(&glsl_type_builtin_uint),
                _uaddCarry(&glsl_type_builtin_uvec2),
                _uaddCarry(&glsl_type_builtin_uvec3),
                _uaddCarry(&glsl_type_builtin_uvec4),
                NULL);
   add_function("usubBorrow",
                _usubBorrow(&glsl_type_builtin_uint),
                _usubBorrow(&glsl_type_builtin_uvec2),
                _usubBorrow(&glsl_type_builtin_uvec3),
                _usubBorrow(&glsl_type_builtin_uvec4),
                NULL);
   add_function("imulExtended",
                _mulExtended(&glsl_type_builtin_int),
                _mulExtended(&glsl_type_builtin_ivec2),
                _mulExtended(&glsl_type_builtin_ivec3),
                _mulExtended(&glsl_type_builtin_ivec4),
                NULL);
   add_function("umulExtended",
                _mulExtended(&glsl_type_builtin_uint),
                _mulExtended(&glsl_type_builtin_uvec2),
                _mulExtended(&glsl_type_builtin_uvec3),
                _mulExtended(&glsl_type_builtin_uvec4),
                NULL);
   add_function("interpolateAtCentroid",
                _interpolateAtCentroid(fs_interpolate_at, &glsl_type_builtin_float),
                _interpolateAtCentroid(fs_interpolate_at, &glsl_type_builtin_vec2),
                _interpolateAtCentroid(fs_interpolate_at, &glsl_type_builtin_vec3),
                _interpolateAtCentroid(fs_interpolate_at, &glsl_type_builtin_vec4),
                _interpolateAtCentroid(fs_half_float_interpolate_at, &glsl_type_builtin_float16_t),
                _interpolateAtCentroid(fs_half_float_interpolate_at, &glsl_type_builtin_f16vec2),
                _interpolateAtCentroid(fs_half_float_interpolate_at, &glsl_type_builtin_f16vec3),
                _interpolateAtCentroid(fs_half_float_interpolate_at, &glsl_type_builtin_f16vec4),
                NULL);
   add_function("interpolateAtOffset",
                _interpolateAtOffset(fs_interpolate_at, &glsl_type_builtin_float),
                _interpolateAtOffset(fs_interpolate_at, &glsl_type_builtin_vec2),
                _interpolateAtOffset(fs_interpolate_at, &glsl_type_builtin_vec3),
                _interpolateAtOffset(fs_interpolate_at, &glsl_type_builtin_vec4),
                _interpolateAtOffset(fs_half_float_interpolate_at, &glsl_type_builtin_float16_t),
                _interpolateAtOffset(fs_half_float_interpolate_at, &glsl_type_builtin_f16vec2),
                _interpolateAtOffset(fs_half_float_interpolate_at, &glsl_type_builtin_f16vec3),
                _interpolateAtOffset(fs_half_float_interpolate_at, &glsl_type_builtin_f16vec4),
                NULL);
   add_function("interpolateAtSample",
                _interpolateAtSample(fs_interpolate_at, &glsl_type_builtin_float),
                _interpolateAtSample(fs_interpolate_at, &glsl_type_builtin_vec2),
                _interpolateAtSample(fs_interpolate_at, &glsl_type_builtin_vec3),
                _interpolateAtSample(fs_interpolate_at, &glsl_type_builtin_vec4),
                _interpolateAtSample(fs_half_float_interpolate_at, &glsl_type_builtin_float16_t),
                _interpolateAtSample(fs_half_float_interpolate_at, &glsl_type_builtin_f16vec2),
                _interpolateAtSample(fs_half_float_interpolate_at, &glsl_type_builtin_f16vec3),
                _interpolateAtSample(fs_half_float_interpolate_at, &glsl_type_builtin_f16vec4),
                NULL);

   add_function("atomicCounter",
                _atomic_counter_op("__intrinsic_atomic_read",
                                   shader_atomic_counters),
                NULL);
   add_function("atomicCounterIncrement",
                _atomic_counter_op("__intrinsic_atomic_increment",
                                   shader_atomic_counters),
                NULL);
   add_function("atomicCounterDecrement",
                _atomic_counter_op("__intrinsic_atomic_predecrement",
                                   shader_atomic_counters),
                NULL);

   add_function("atomicCounterAddARB",
                _atomic_counter_op1("__intrinsic_atomic_add",
                                    shader_atomic_counter_ops),
                NULL);
   add_function("atomicCounterSubtractARB",
                _atomic_counter_op1("__intrinsic_atomic_sub",
                                    shader_atomic_counter_ops),
                NULL);
   add_function("atomicCounterMinARB",
                _atomic_counter_op1("__intrinsic_atomic_min",
                                    shader_atomic_counter_ops),
                NULL);
   add_function("atomicCounterMaxARB",
                _atomic_counter_op1("__intrinsic_atomic_max",
                                    shader_atomic_counter_ops),
                NULL);
   add_function("atomicCounterAndARB",
                _atomic_counter_op1("__intrinsic_atomic_and",
                                    shader_atomic_counter_ops),
                NULL);
   add_function("atomicCounterOrARB",
                _atomic_counter_op1("__intrinsic_atomic_or",
                                    shader_atomic_counter_ops),
                NULL);
   add_function("atomicCounterXorARB",
                _atomic_counter_op1("__intrinsic_atomic_xor",
                                    shader_atomic_counter_ops),
                NULL);
   add_function("atomicCounterExchangeARB",
                _atomic_counter_op1("__intrinsic_atomic_exchange",
                                    shader_atomic_counter_ops),
                NULL);
   add_function("atomicCounterCompSwapARB",
                _atomic_counter_op2("__intrinsic_atomic_comp_swap",
                                    shader_atomic_counter_ops),
                NULL);

   add_function("atomicCounterAdd",
                _atomic_counter_op1("__intrinsic_atomic_add",
                                    v460_desktop),
                NULL);
   add_function("atomicCounterSubtract",
                _atomic_counter_op1("__intrinsic_atomic_sub",
                                    v460_desktop),
                NULL);
   add_function("atomicCounterMin",
                _atomic_counter_op1("__intrinsic_atomic_min",
                                    v460_desktop),
                NULL);
   add_function("atomicCounterMax",
                _atomic_counter_op1("__intrinsic_atomic_max",
                                    v460_desktop),
                NULL);
   add_function("atomicCounterAnd",
                _atomic_counter_op1("__intrinsic_atomic_and",
                                    v460_desktop),
                NULL);
   add_function("atomicCounterOr",
                _atomic_counter_op1("__intrinsic_atomic_or",
                                    v460_desktop),
                NULL);
   add_function("atomicCounterXor",
                _atomic_counter_op1("__intrinsic_atomic_xor",
                                    v460_desktop),
                NULL);
   add_function("atomicCounterExchange",
                _atomic_counter_op1("__intrinsic_atomic_exchange",
                                    v460_desktop),
                NULL);
   add_function("atomicCounterCompSwap",
                _atomic_counter_op2("__intrinsic_atomic_comp_swap",
                                    v460_desktop),
                NULL);

   add_function("atomicAdd",
                _atomic_op2("__intrinsic_atomic_add",
                            buffer_atomics_supported,
                            &glsl_type_builtin_uint),
                _atomic_op2("__intrinsic_atomic_add",
                            buffer_atomics_supported,
                            &glsl_type_builtin_int),
                _atomic_op2("__intrinsic_atomic_add",
                            shader_atomic_float_add,
                            &glsl_type_builtin_float),
                _atomic_op2("__intrinsic_atomic_add",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_int64_t),
                NULL);
   add_function("atomicMin",
                _atomic_op2("__intrinsic_atomic_min",
                            buffer_atomics_supported,
                            &glsl_type_builtin_uint),
                _atomic_op2("__intrinsic_atomic_min",
                            buffer_atomics_supported,
                            &glsl_type_builtin_int),
                _atomic_op2("__intrinsic_atomic_min",
                            shader_atomic_float_minmax,
                            &glsl_type_builtin_float),
                _atomic_op2("__intrinsic_atomic_min",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_uint64_t),
                _atomic_op2("__intrinsic_atomic_min",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_int64_t),
                NULL);
   add_function("atomicMax",
                _atomic_op2("__intrinsic_atomic_max",
                            buffer_atomics_supported,
                            &glsl_type_builtin_uint),
                _atomic_op2("__intrinsic_atomic_max",
                            buffer_atomics_supported,
                            &glsl_type_builtin_int),
                _atomic_op2("__intrinsic_atomic_max",
                            shader_atomic_float_minmax,
                            &glsl_type_builtin_float),
                _atomic_op2("__intrinsic_atomic_max",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_uint64_t),
                _atomic_op2("__intrinsic_atomic_max",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_int64_t),
                NULL);
   add_function("atomicAnd",
                _atomic_op2("__intrinsic_atomic_and",
                            buffer_atomics_supported,
                            &glsl_type_builtin_uint),
                _atomic_op2("__intrinsic_atomic_and",
                            buffer_atomics_supported,
                            &glsl_type_builtin_int),
                _atomic_op2("__intrinsic_atomic_and",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_uint64_t),
                _atomic_op2("__intrinsic_atomic_and",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_int64_t),
                NULL);
   add_function("atomicOr",
                _atomic_op2("__intrinsic_atomic_or",
                            buffer_atomics_supported,
                            &glsl_type_builtin_uint),
                _atomic_op2("__intrinsic_atomic_or",
                            buffer_atomics_supported,
                            &glsl_type_builtin_int),
                _atomic_op2("__intrinsic_atomic_or",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_uint64_t),
                _atomic_op2("__intrinsic_atomic_or",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_int64_t),
                NULL);
   add_function("atomicXor",
                _atomic_op2("__intrinsic_atomic_xor",
                            buffer_atomics_supported,
                            &glsl_type_builtin_uint),
                _atomic_op2("__intrinsic_atomic_xor",
                            buffer_atomics_supported,
                            &glsl_type_builtin_int),
                _atomic_op2("__intrinsic_atomic_xor",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_uint64_t),
                _atomic_op2("__intrinsic_atomic_xor",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_int64_t),
                NULL);
   add_function("atomicExchange",
                _atomic_op2("__intrinsic_atomic_exchange",
                            buffer_atomics_supported,
                            &glsl_type_builtin_uint),
                _atomic_op2("__intrinsic_atomic_exchange",
                            buffer_atomics_supported,
                            &glsl_type_builtin_int),
                _atomic_op2("__intrinsic_atomic_exchange",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_int64_t),
                _atomic_op2("__intrinsic_atomic_exchange",
                            shader_atomic_float_exchange,
                            &glsl_type_builtin_float),
                NULL);
   add_function("atomicCompSwap",
                _atomic_op3("__intrinsic_atomic_comp_swap",
                            buffer_atomics_supported,
                            &glsl_type_builtin_uint),
                _atomic_op3("__intrinsic_atomic_comp_swap",
                            buffer_atomics_supported,
                            &glsl_type_builtin_int),
                _atomic_op3("__intrinsic_atomic_comp_swap",
                            buffer_int64_atomics_supported,
                            &glsl_type_builtin_int64_t),
                _atomic_op3("__intrinsic_atomic_comp_swap",
                            shader_atomic_float_minmax,
                            &glsl_type_builtin_float),
                NULL);

   add_function("min3",
                _min3(shader_trinary_minmax, &glsl_type_builtin_float),
                _min3(shader_trinary_minmax, &glsl_type_builtin_vec2),
                _min3(shader_trinary_minmax, &glsl_type_builtin_vec3),
                _min3(shader_trinary_minmax, &glsl_type_builtin_vec4),

                _min3(shader_trinary_minmax_half_float, &glsl_type_builtin_float16_t),
                _min3(shader_trinary_minmax_half_float, &glsl_type_builtin_f16vec2),
                _min3(shader_trinary_minmax_half_float, &glsl_type_builtin_f16vec3),
                _min3(shader_trinary_minmax_half_float, &glsl_type_builtin_f16vec4),

                _min3(shader_trinary_minmax, &glsl_type_builtin_int),
                _min3(shader_trinary_minmax, &glsl_type_builtin_ivec2),
                _min3(shader_trinary_minmax, &glsl_type_builtin_ivec3),
                _min3(shader_trinary_minmax, &glsl_type_builtin_ivec4),

                _min3(shader_trinary_minmax, &glsl_type_builtin_uint),
                _min3(shader_trinary_minmax, &glsl_type_builtin_uvec2),
                _min3(shader_trinary_minmax, &glsl_type_builtin_uvec3),
                _min3(shader_trinary_minmax, &glsl_type_builtin_uvec4),
                NULL);

   add_function("max3",
                _max3(shader_trinary_minmax, &glsl_type_builtin_float),
                _max3(shader_trinary_minmax, &glsl_type_builtin_vec2),
                _max3(shader_trinary_minmax, &glsl_type_builtin_vec3),
                _max3(shader_trinary_minmax, &glsl_type_builtin_vec4),

                _max3(shader_trinary_minmax_half_float, &glsl_type_builtin_float16_t),
                _max3(shader_trinary_minmax_half_float, &glsl_type_builtin_f16vec2),
                _max3(shader_trinary_minmax_half_float, &glsl_type_builtin_f16vec3),
                _max3(shader_trinary_minmax_half_float, &glsl_type_builtin_f16vec4),

                _max3(shader_trinary_minmax, &glsl_type_builtin_int),
                _max3(shader_trinary_minmax, &glsl_type_builtin_ivec2),
                _max3(shader_trinary_minmax, &glsl_type_builtin_ivec3),
                _max3(shader_trinary_minmax, &glsl_type_builtin_ivec4),

                _max3(shader_trinary_minmax, &glsl_type_builtin_uint),
                _max3(shader_trinary_minmax, &glsl_type_builtin_uvec2),
                _max3(shader_trinary_minmax, &glsl_type_builtin_uvec3),
                _max3(shader_trinary_minmax, &glsl_type_builtin_uvec4),
                NULL);

   add_function("mid3",
                _mid3(shader_trinary_minmax, &glsl_type_builtin_float),
                _mid3(shader_trinary_minmax, &glsl_type_builtin_vec2),
                _mid3(shader_trinary_minmax, &glsl_type_builtin_vec3),
                _mid3(shader_trinary_minmax, &glsl_type_builtin_vec4),

                _mid3(shader_trinary_minmax_half_float, &glsl_type_builtin_float16_t),
                _mid3(shader_trinary_minmax_half_float, &glsl_type_builtin_f16vec2),
                _mid3(shader_trinary_minmax_half_float, &glsl_type_builtin_f16vec3),
                _mid3(shader_trinary_minmax_half_float, &glsl_type_builtin_f16vec4),

                _mid3(shader_trinary_minmax, &glsl_type_builtin_int),
                _mid3(shader_trinary_minmax, &glsl_type_builtin_ivec2),
                _mid3(shader_trinary_minmax, &glsl_type_builtin_ivec3),
                _mid3(shader_trinary_minmax, &glsl_type_builtin_ivec4),

                _mid3(shader_trinary_minmax, &glsl_type_builtin_uint),
                _mid3(shader_trinary_minmax, &glsl_type_builtin_uvec2),
                _mid3(shader_trinary_minmax, &glsl_type_builtin_uvec3),
                _mid3(shader_trinary_minmax, &glsl_type_builtin_uvec4),
                NULL);

   add_image_functions(true);

   add_function("memoryBarrier",
                _memory_barrier("__intrinsic_memory_barrier",
                                shader_image_load_store),
                NULL);
   add_function("groupMemoryBarrier",
                _memory_barrier("__intrinsic_group_memory_barrier",
                                compute_shader),
                NULL);
   add_function("memoryBarrierAtomicCounter",
                _memory_barrier("__intrinsic_memory_barrier_atomic_counter",
                                compute_shader_supported),
                NULL);
   add_function("memoryBarrierBuffer",
                _memory_barrier("__intrinsic_memory_barrier_buffer",
                                compute_shader_supported),
                NULL);
   add_function("memoryBarrierImage",
                _memory_barrier("__intrinsic_memory_barrier_image",
                                compute_shader_supported),
                NULL);
   add_function("memoryBarrierShared",
                _memory_barrier("__intrinsic_memory_barrier_shared",
                                compute_shader),
                NULL);

   add_function("ballotARB", _ballot(&glsl_type_builtin_uint64_t, ballot_arb), NULL);

   add_function("readInvocationARB",
                FIU(_read_invocation, ballot_arb),
                NULL);

   add_function("readFirstInvocationARB",
                FIU(_read_first_invocation, ballot_arb),
                NULL);

   add_function("clock2x32ARB",
                _shader_clock(shader_clock,
                              &glsl_type_builtin_uvec2),
                NULL);

   add_function("clockARB",
                _shader_clock(shader_clock_int64,
                              &glsl_type_builtin_uint64_t),
                NULL);

   add_function("clockRealtime2x32EXT",
                _shader_clock_realtime(shader_clock_realtime,
                                       &glsl_type_builtin_uvec2),
                NULL);

   add_function("clockRealtimeEXT",
                  _shader_clock_realtime(shader_clock_realtime_int64,
                                         &glsl_type_builtin_uint64_t),
                  NULL);
   add_function("beginInvocationInterlockARB",
                _invocation_interlock(
                   "__intrinsic_begin_invocation_interlock",
                   supports_arb_fragment_shader_interlock),
                NULL);

   add_function("endInvocationInterlockARB",
                _invocation_interlock(
                   "__intrinsic_end_invocation_interlock",
                   supports_arb_fragment_shader_interlock),
                NULL);

   add_function("beginInvocationInterlockNV",
                _invocation_interlock(
                   "__intrinsic_begin_invocation_interlock",
                   supports_nv_fragment_shader_interlock),
                NULL);

   add_function("endInvocationInterlockNV",
                _invocation_interlock(
                   "__intrinsic_end_invocation_interlock",
                   supports_nv_fragment_shader_interlock),
                NULL);

   add_function("anyInvocationARB",
                _vote(&glsl_type_builtin_bool, vote, "__intrinsic_vote_any"),
                NULL);

   add_function("allInvocationsARB",
                _vote(&glsl_type_builtin_bool, vote, "__intrinsic_vote_all"),
                NULL);

   add_function("allInvocationsEqualARB",
                _vote(&glsl_type_builtin_bool, vote, "__intrinsic_vote_eq"),
                NULL);

   add_function("anyInvocationEXT",
                _vote(&glsl_type_builtin_bool, vote_ext, "__intrinsic_vote_any"),
                NULL);

   add_function("allInvocationsEXT",
                _vote(&glsl_type_builtin_bool, vote_ext, "__intrinsic_vote_all"),
                NULL);

   add_function("allInvocationsEqualEXT",
                _vote(&glsl_type_builtin_bool, vote_ext, "__intrinsic_vote_eq"),
                NULL);

   add_function("anyInvocation",
                _vote(&glsl_type_builtin_bool, v460_desktop, "__intrinsic_vote_any"),
                NULL);

   add_function("allInvocations",
                _vote(&glsl_type_builtin_bool, v460_desktop, "__intrinsic_vote_all"),
                NULL);

   add_function("allInvocationsEqual",
                _vote(&glsl_type_builtin_bool, v460_desktop, "__intrinsic_vote_eq"),
                NULL);

   add_function("helperInvocationEXT", _helper_invocation(), NULL);

   add_function("countLeadingZeros",
                _countLeadingZeros(shader_integer_functions2,
                                   &glsl_type_builtin_uint),
                _countLeadingZeros(shader_integer_functions2,
                                   &glsl_type_builtin_uvec2),
                _countLeadingZeros(shader_integer_functions2,
                                   &glsl_type_builtin_uvec3),
                _countLeadingZeros(shader_integer_functions2,
                                   &glsl_type_builtin_uvec4),
                NULL);

   add_function("countTrailingZeros",
                _countTrailingZeros(shader_integer_functions2,
                                    &glsl_type_builtin_uint),
                _countTrailingZeros(shader_integer_functions2,
                                    &glsl_type_builtin_uvec2),
                _countTrailingZeros(shader_integer_functions2,
                                    &glsl_type_builtin_uvec3),
                _countTrailingZeros(shader_integer_functions2,
                                    &glsl_type_builtin_uvec4),
                NULL);

   add_function("absoluteDifference",
                _absoluteDifference(shader_integer_functions2,
                                    &glsl_type_builtin_int),
                _absoluteDifference(shader_integer_functions2,
                                    &glsl_type_builtin_ivec2),
                _absoluteDifference(shader_integer_functions2,
                                    &glsl_type_builtin_ivec3),
                _absoluteDifference(shader_integer_functions2,
                                    &glsl_type_builtin_ivec4),
                _absoluteDifference(shader_integer_functions2,
                                    &glsl_type_builtin_uint),
                _absoluteDifference(shader_integer_functions2,
                                    &glsl_type_builtin_uvec2),
                _absoluteDifference(shader_integer_functions2,
                                    &glsl_type_builtin_uvec3),
                _absoluteDifference(shader_integer_functions2,
                                    &glsl_type_builtin_uvec4),

                _absoluteDifference(shader_integer_functions2_int64,
                                    &glsl_type_builtin_int64_t),
                _absoluteDifference(shader_integer_functions2_int64,
                                    &glsl_type_builtin_i64vec2),
                _absoluteDifference(shader_integer_functions2_int64,
                                    &glsl_type_builtin_i64vec3),
                _absoluteDifference(shader_integer_functions2_int64,
                                    &glsl_type_builtin_i64vec4),
                _absoluteDifference(shader_integer_functions2_int64,
                                    &glsl_type_builtin_uint64_t),
                _absoluteDifference(shader_integer_functions2_int64,
                                    &glsl_type_builtin_u64vec2),
                _absoluteDifference(shader_integer_functions2_int64,
                                    &glsl_type_builtin_u64vec3),
                _absoluteDifference(shader_integer_functions2_int64,
                                    &glsl_type_builtin_u64vec4),
                NULL);

   add_function("addSaturate",
                _addSaturate(shader_integer_functions2,
                             &glsl_type_builtin_int),
                _addSaturate(shader_integer_functions2,
                             &glsl_type_builtin_ivec2),
                _addSaturate(shader_integer_functions2,
                             &glsl_type_builtin_ivec3),
                _addSaturate(shader_integer_functions2,
                             &glsl_type_builtin_ivec4),
                _addSaturate(shader_integer_functions2,
                             &glsl_type_builtin_uint),
                _addSaturate(shader_integer_functions2,
                             &glsl_type_builtin_uvec2),
                _addSaturate(shader_integer_functions2,
                             &glsl_type_builtin_uvec3),
                _addSaturate(shader_integer_functions2,
                             &glsl_type_builtin_uvec4),

                _addSaturate(shader_integer_functions2_int64,
                             &glsl_type_builtin_int64_t),
                _addSaturate(shader_integer_functions2_int64,
                             &glsl_type_builtin_i64vec2),
                _addSaturate(shader_integer_functions2_int64,
                             &glsl_type_builtin_i64vec3),
                _addSaturate(shader_integer_functions2_int64,
                             &glsl_type_builtin_i64vec4),
                _addSaturate(shader_integer_functions2_int64,
                             &glsl_type_builtin_uint64_t),
                _addSaturate(shader_integer_functions2_int64,
                             &glsl_type_builtin_u64vec2),
                _addSaturate(shader_integer_functions2_int64,
                             &glsl_type_builtin_u64vec3),
                _addSaturate(shader_integer_functions2_int64,
                             &glsl_type_builtin_u64vec4),
                NULL);

   add_function("average",
                _average(shader_integer_functions2,
                         &glsl_type_builtin_int),
                _average(shader_integer_functions2,
                         &glsl_type_builtin_ivec2),
                _average(shader_integer_functions2,
                         &glsl_type_builtin_ivec3),
                _average(shader_integer_functions2,
                         &glsl_type_builtin_ivec4),
                _average(shader_integer_functions2,
                         &glsl_type_builtin_uint),
                _average(shader_integer_functions2,
                         &glsl_type_builtin_uvec2),
                _average(shader_integer_functions2,
                         &glsl_type_builtin_uvec3),
                _average(shader_integer_functions2,
                         &glsl_type_builtin_uvec4),

                _average(shader_integer_functions2_int64,
                         &glsl_type_builtin_int64_t),
                _average(shader_integer_functions2_int64,
                         &glsl_type_builtin_i64vec2),
                _average(shader_integer_functions2_int64,
                         &glsl_type_builtin_i64vec3),
                _average(shader_integer_functions2_int64,
                         &glsl_type_builtin_i64vec4),
                _average(shader_integer_functions2_int64,
                         &glsl_type_builtin_uint64_t),
                _average(shader_integer_functions2_int64,
                         &glsl_type_builtin_u64vec2),
                _average(shader_integer_functions2_int64,
                         &glsl_type_builtin_u64vec3),
                _average(shader_integer_functions2_int64,
                         &glsl_type_builtin_u64vec4),
                NULL);

   add_function("averageRounded",
                _averageRounded(shader_integer_functions2,
                                &glsl_type_builtin_int),
                _averageRounded(shader_integer_functions2,
                                &glsl_type_builtin_ivec2),
                _averageRounded(shader_integer_functions2,
                                &glsl_type_builtin_ivec3),
                _averageRounded(shader_integer_functions2,
                                &glsl_type_builtin_ivec4),
                _averageRounded(shader_integer_functions2,
                                &glsl_type_builtin_uint),
                _averageRounded(shader_integer_functions2,
                                &glsl_type_builtin_uvec2),
                _averageRounded(shader_integer_functions2,
                                &glsl_type_builtin_uvec3),
                _averageRounded(shader_integer_functions2,
                                &glsl_type_builtin_uvec4),

                _averageRounded(shader_integer_functions2_int64,
                                &glsl_type_builtin_int64_t),
                _averageRounded(shader_integer_functions2_int64,
                                &glsl_type_builtin_i64vec2),
                _averageRounded(shader_integer_functions2_int64,
                                &glsl_type_builtin_i64vec3),
                _averageRounded(shader_integer_functions2_int64,
                                &glsl_type_builtin_i64vec4),
                _averageRounded(shader_integer_functions2_int64,
                                &glsl_type_builtin_uint64_t),
                _averageRounded(shader_integer_functions2_int64,
                                &glsl_type_builtin_u64vec2),
                _averageRounded(shader_integer_functions2_int64,
                                &glsl_type_builtin_u64vec3),
                _averageRounded(shader_integer_functions2_int64,
                                &glsl_type_builtin_u64vec4),
                NULL);

   add_function("subtractSaturate",
                _subtractSaturate(shader_integer_functions2,
                                  &glsl_type_builtin_int),
                _subtractSaturate(shader_integer_functions2,
                                  &glsl_type_builtin_ivec2),
                _subtractSaturate(shader_integer_functions2,
                                  &glsl_type_builtin_ivec3),
                _subtractSaturate(shader_integer_functions2,
                                  &glsl_type_builtin_ivec4),
                _subtractSaturate(shader_integer_functions2,
                                  &glsl_type_builtin_uint),
                _subtractSaturate(shader_integer_functions2,
                                  &glsl_type_builtin_uvec2),
                _subtractSaturate(shader_integer_functions2,
                                  &glsl_type_builtin_uvec3),
                _subtractSaturate(shader_integer_functions2,
                                  &glsl_type_builtin_uvec4),

                _subtractSaturate(shader_integer_functions2_int64,
                                  &glsl_type_builtin_int64_t),
                _subtractSaturate(shader_integer_functions2_int64,
                                  &glsl_type_builtin_i64vec2),
                _subtractSaturate(shader_integer_functions2_int64,
                                  &glsl_type_builtin_i64vec3),
                _subtractSaturate(shader_integer_functions2_int64,
                                  &glsl_type_builtin_i64vec4),
                _subtractSaturate(shader_integer_functions2_int64,
                                  &glsl_type_builtin_uint64_t),
                _subtractSaturate(shader_integer_functions2_int64,
                                  &glsl_type_builtin_u64vec2),
                _subtractSaturate(shader_integer_functions2_int64,
                                  &glsl_type_builtin_u64vec3),
                _subtractSaturate(shader_integer_functions2_int64,
                                  &glsl_type_builtin_u64vec4),
                NULL);

   add_function("multiply32x16",
                _multiply32x16(shader_integer_functions2,
                               &glsl_type_builtin_int),
                _multiply32x16(shader_integer_functions2,
                               &glsl_type_builtin_ivec2),
                _multiply32x16(shader_integer_functions2,
                               &glsl_type_builtin_ivec3),
                _multiply32x16(shader_integer_functions2,
                               &glsl_type_builtin_ivec4),
                _multiply32x16(shader_integer_functions2,
                               &glsl_type_builtin_uint),
                _multiply32x16(shader_integer_functions2,
                               &glsl_type_builtin_uvec2),
                _multiply32x16(shader_integer_functions2,
                               &glsl_type_builtin_uvec3),
                _multiply32x16(shader_integer_functions2,
                               &glsl_type_builtin_uvec4),
                NULL);

   add_function("subgroupBarrier",
                _subgroup_barrier("__intrinsic_subgroup_barrier", subgroup_basic), NULL);
   add_function("subgroupMemoryBarrier",
                _subgroup_barrier("__intrinsic_subgroup_memory_barrier", subgroup_basic), NULL);
   add_function("subgroupMemoryBarrierBuffer",
                _subgroup_barrier("__intrinsic_subgroup_memory_barrier_buffer", subgroup_basic),
                NULL);
   add_function("subgroupMemoryBarrierShared",
                _subgroup_barrier("__intrinsic_subgroup_memory_barrier_shared",
                                  compute_shader_and_subgroup_basic),
                NULL);
   add_function("subgroupMemoryBarrierImage",
                _subgroup_barrier("__intrinsic_subgroup_memory_barrier_image", subgroup_basic),
                NULL);

   add_function("subgroupElect", _elect(), NULL);

   add_function("subgroupAll",
                _vote(&glsl_type_builtin_bool, vote_khr, "__intrinsic_vote_all"), NULL);
   add_function("subgroupAny",
                _vote(&glsl_type_builtin_bool, vote_khr, "__intrinsic_vote_any"), NULL);
   add_function("subgroupAllEqual",
                FIUBD_AVAIL(_vote, vote_khr, "__intrinsic_vote_eq"), NULL);

   add_function("subgroupBroadcast", FIUBD_AVAIL(_read_invocation, ballot_khr), NULL);

   add_function("subgroupBroadcastFirst", FIUBD_AVAIL(_read_first_invocation, ballot_khr), NULL);

   add_function("subgroupBallot", _ballot(&glsl_type_builtin_uvec4, ballot_khr), NULL);

   add_function("subgroupInverseBallot", _inverse_ballot(), NULL);

   add_function("subgroupBallotBitExtract", _ballot_bit_extract(), NULL);

   add_function("subgroupBallotBitCount", _ballot_bit("__intrinsic_ballot_bit_count"), NULL);

   add_function("subgroupBallotInclusiveBitCount",
                _ballot_bit("__intrinsic_ballot_inclusive_bit_count"), NULL);
   add_function("subgroupBallotExclusiveBitCount",
                _ballot_bit("__intrinsic_ballot_exclusive_bit_count"), NULL);

   add_function("subgroupBallotFindLSB", _ballot_bit("__intrinsic_ballot_find_lsb"), NULL);
   add_function("subgroupBallotFindMSB", _ballot_bit("__intrinsic_ballot_find_msb"), NULL);

   add_function("subgroupShuffle", FIUBD(_shuffle), NULL);

   add_function("subgroupShuffleXor", FIUBD(_shuffle_xor), NULL);

   add_function("subgroupShuffleUp", FIUBD(_shuffle_up), NULL);

   add_function("subgroupShuffleDown", FIUBD(_shuffle_down), NULL);

#define SUBGROUP_ARITH(ext, group1, group2) \
   add_function("subgroup" #group1 "Add", \
                FIUD(_subgroup_##ext, "__intrinsic_" #group2 "_add"), NULL); \
   add_function("subgroup" #group1 "Mul", \
                FIUD(_subgroup_##ext, "__intrinsic_" #group2 "_mul"), NULL); \
   add_function("subgroup" #group1 "Min", \
                FIUD(_subgroup_##ext, "__intrinsic_" #group2 "_min"), NULL); \
   add_function("subgroup" #group1 "Max", \
                FIUD(_subgroup_##ext, "__intrinsic_" #group2 "_max"), NULL); \
   add_function("subgroup" #group1 "And", \
                IUB(_subgroup_##ext, "__intrinsic_" #group2 "_and"), NULL); \
   add_function("subgroup" #group1 "Or", \
                IUB(_subgroup_##ext, "__intrinsic_" #group2 "_or"), NULL); \
   add_function("subgroup" #group1 "Xor", \
                IUB(_subgroup_##ext, "__intrinsic_" #group2 "_xor"), NULL)

   SUBGROUP_ARITH(arithmetic, /* empty */, reduce);
   SUBGROUP_ARITH(arithmetic, Inclusive, inclusive);
   SUBGROUP_ARITH(arithmetic, Exclusive, exclusive);

   SUBGROUP_ARITH(clustered, Clustered, clustered);

   add_function("subgroupQuadBroadcast", FIUBD(_quad_broadcast), NULL);

   add_function("subgroupQuadSwapHorizontal",
                FIUBD(_quad_swap, "__intrinsic_quad_swap_horizontal"), NULL);
   add_function("subgroupQuadSwapVertical",
                FIUBD(_quad_swap, "__intrinsic_quad_swap_vertical"), NULL);
   add_function("subgroupQuadSwapDiagonal",
                FIUBD(_quad_swap, "__intrinsic_quad_swap_diagonal"), NULL);

#undef F
#undef FI
#undef FIUDHF_VEC
#undef FIUBDHF_VEC
#undef FIU2_MIXED
}

void
builtin_builder::add_function(const char *name, ...)
{
   va_list ap;

   ir_function *f = new(mem_ctx) ir_function(name);

   va_start(ap, name);
   while (true) {
      ir_function_signature *sig = va_arg(ap, ir_function_signature *);
      if (sig == NULL)
         break;

      if (false) {
         ir_exec_list stuff;
         stuff.push_tail(sig);
         validate_ir_tree(&stuff);
      }

      f->add_signature(sig);
   }
   va_end(ap);

   symbols->add_function(f);
}

void
builtin_builder::add_image_function(const char *name,
                                    const char *intrinsic_name,
                                    image_prototype_ctr prototype,
                                    unsigned num_arguments,
                                    unsigned flags,
                                    enum ir_intrinsic_id intrinsic_id)
{
   static const glsl_type *const types[] = {
      &glsl_type_builtin_image1D,
      &glsl_type_builtin_image2D,
      &glsl_type_builtin_image3D,
      &glsl_type_builtin_image2DRect,
      &glsl_type_builtin_imageCube,
      &glsl_type_builtin_imageBuffer,
      &glsl_type_builtin_image1DArray,
      &glsl_type_builtin_image2DArray,
      &glsl_type_builtin_imageCubeArray,
      &glsl_type_builtin_image2DMS,
      &glsl_type_builtin_image2DMSArray,
      &glsl_type_builtin_iimage1D,
      &glsl_type_builtin_iimage2D,
      &glsl_type_builtin_iimage3D,
      &glsl_type_builtin_iimage2DRect,
      &glsl_type_builtin_iimageCube,
      &glsl_type_builtin_iimageBuffer,
      &glsl_type_builtin_iimage1DArray,
      &glsl_type_builtin_iimage2DArray,
      &glsl_type_builtin_iimageCubeArray,
      &glsl_type_builtin_iimage2DMS,
      &glsl_type_builtin_iimage2DMSArray,
      &glsl_type_builtin_uimage1D,
      &glsl_type_builtin_uimage2D,
      &glsl_type_builtin_uimage3D,
      &glsl_type_builtin_uimage2DRect,
      &glsl_type_builtin_uimageCube,
      &glsl_type_builtin_uimageBuffer,
      &glsl_type_builtin_uimage1DArray,
      &glsl_type_builtin_uimage2DArray,
      &glsl_type_builtin_uimageCubeArray,
      &glsl_type_builtin_uimage2DMS,
      &glsl_type_builtin_uimage2DMSArray
   };

   ir_function *f = new(mem_ctx) ir_function(name);

   for (unsigned i = 0; i < ARRAY_SIZE(types); ++i) {
      if (types[i]->sampled_type == GLSL_TYPE_FLOAT && !(flags & IMAGE_FUNCTION_SUPPORTS_FLOAT_DATA_TYPE))
         continue;
      if (types[i]->sampled_type == GLSL_TYPE_INT && !(flags & IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE))
         continue;
      if ((types[i]->sampler_dimensionality != GLSL_SAMPLER_DIM_MS) && (flags & IMAGE_FUNCTION_MS_ONLY))
         continue;
      if (flags & IMAGE_FUNCTION_SPARSE) {
         switch (types[i]->sampler_dimensionality) {
         case GLSL_SAMPLER_DIM_2D:
         case GLSL_SAMPLER_DIM_3D:
         case GLSL_SAMPLER_DIM_CUBE:
         case GLSL_SAMPLER_DIM_RECT:
         case GLSL_SAMPLER_DIM_MS:
            break;
         default:
            continue;
         }
      }
      f->add_signature(_image(prototype, types[i], intrinsic_name,
                              num_arguments, flags, intrinsic_id));
   }
   symbols->add_function(f);
}

void
builtin_builder::add_image_functions(bool glsl)
{
   const unsigned flags = (glsl ? IMAGE_FUNCTION_EMIT_STUB : 0);

   add_image_function(glsl ? "imageLoad" : "__intrinsic_image_load",
                       "__intrinsic_image_load",
                       &builtin_builder::_image_prototype, 0,
                       (flags | IMAGE_FUNCTION_HAS_VECTOR_DATA_TYPE |
                       IMAGE_FUNCTION_SUPPORTS_FLOAT_DATA_TYPE |
                       IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE |
                       IMAGE_FUNCTION_READ_ONLY),
                      ir_intrinsic_image_load);

   add_image_function(glsl ? "imageStore" : "__intrinsic_image_store",
                      "__intrinsic_image_store",
                      &builtin_builder::_image_prototype, 1,
                      (flags | IMAGE_FUNCTION_RETURNS_VOID |
                       IMAGE_FUNCTION_HAS_VECTOR_DATA_TYPE |
                       IMAGE_FUNCTION_SUPPORTS_FLOAT_DATA_TYPE |
                       IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE |
                       IMAGE_FUNCTION_WRITE_ONLY),
                      ir_intrinsic_image_store);

   const unsigned atom_flags = flags | IMAGE_FUNCTION_AVAIL_ATOMIC;

   add_image_function(glsl ? "imageAtomicAdd" : "__intrinsic_image_atomic_add",
                      "__intrinsic_image_atomic_add",
                      &builtin_builder::_image_prototype, 1,
                      (flags | IMAGE_FUNCTION_AVAIL_ATOMIC_ADD |
                       IMAGE_FUNCTION_SUPPORTS_FLOAT_DATA_TYPE |
                       IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE),
                      ir_intrinsic_image_atomic_add);

   add_image_function(glsl ? "imageAtomicMin" : "__intrinsic_image_atomic_min",
                      "__intrinsic_image_atomic_min",
                      &builtin_builder::_image_prototype, 1,
                      atom_flags | IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE,
                      ir_intrinsic_image_atomic_min);

   add_image_function(glsl ? "imageAtomicMax" : "__intrinsic_image_atomic_max",
                      "__intrinsic_image_atomic_max",
                      &builtin_builder::_image_prototype, 1,
                      atom_flags | IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE,
                      ir_intrinsic_image_atomic_max);

   add_image_function(glsl ? "imageAtomicAnd" : "__intrinsic_image_atomic_and",
                      "__intrinsic_image_atomic_and",
                      &builtin_builder::_image_prototype, 1,
                      atom_flags | IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE,
                      ir_intrinsic_image_atomic_and);

   add_image_function(glsl ? "imageAtomicOr" : "__intrinsic_image_atomic_or",
                      "__intrinsic_image_atomic_or",
                      &builtin_builder::_image_prototype, 1,
                      atom_flags | IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE,
                      ir_intrinsic_image_atomic_or);

   add_image_function(glsl ? "imageAtomicXor" : "__intrinsic_image_atomic_xor",
                      "__intrinsic_image_atomic_xor",
                      &builtin_builder::_image_prototype, 1,
                      atom_flags | IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE,
                      ir_intrinsic_image_atomic_xor);

   add_image_function((glsl ? "imageAtomicExchange" :
                       "__intrinsic_image_atomic_exchange"),
                      "__intrinsic_image_atomic_exchange",
                      &builtin_builder::_image_prototype, 1,
                      (flags | IMAGE_FUNCTION_AVAIL_ATOMIC_EXCHANGE |
                       IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE |
                       IMAGE_FUNCTION_SUPPORTS_FLOAT_DATA_TYPE),
                      ir_intrinsic_image_atomic_exchange);

   add_image_function((glsl ? "imageAtomicCompSwap" :
                       "__intrinsic_image_atomic_comp_swap"),
                      "__intrinsic_image_atomic_comp_swap",
                      &builtin_builder::_image_prototype, 2,
                      atom_flags | IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE,
                      ir_intrinsic_image_atomic_comp_swap);

   add_image_function(glsl ? "imageSize" : "__intrinsic_image_size",
                      "__intrinsic_image_size",
                      &builtin_builder::_image_size_prototype, 1,
                      flags | IMAGE_FUNCTION_SUPPORTS_FLOAT_DATA_TYPE |
                      IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE,
                      ir_intrinsic_image_size);

   add_image_function(glsl ? "imageSamples" : "__intrinsic_image_samples",
                      "__intrinsic_image_samples",
                      &builtin_builder::_image_samples_prototype, 1,
                      flags | IMAGE_FUNCTION_SUPPORTS_FLOAT_DATA_TYPE |
                      IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE |
                      IMAGE_FUNCTION_MS_ONLY,
                      ir_intrinsic_image_samples);

   /* EXT_shader_image_load_store */
   add_image_function(glsl ? "imageAtomicIncWrap" : "__intrinsic_image_atomic_inc_wrap",
                      "__intrinsic_image_atomic_inc_wrap",
                      &builtin_builder::_image_prototype, 1,
                      (atom_flags | IMAGE_FUNCTION_EXT_ONLY),
                      ir_intrinsic_image_atomic_inc_wrap);
   add_image_function(glsl ? "imageAtomicDecWrap" : "__intrinsic_image_atomic_dec_wrap",
                      "__intrinsic_image_atomic_dec_wrap",
                      &builtin_builder::_image_prototype, 1,
                      (atom_flags | IMAGE_FUNCTION_EXT_ONLY),
                      ir_intrinsic_image_atomic_dec_wrap);

   /* ARB_sparse_texture2 */
   add_image_function(glsl ? "sparseImageLoadARB" : "__intrinsic_image_sparse_load",
                      "__intrinsic_image_sparse_load",
                      &builtin_builder::_image_prototype, 0,
                      (flags | IMAGE_FUNCTION_HAS_VECTOR_DATA_TYPE |
                       IMAGE_FUNCTION_SUPPORTS_FLOAT_DATA_TYPE |
                       IMAGE_FUNCTION_SUPPORTS_SIGNED_DATA_TYPE |
                       IMAGE_FUNCTION_READ_ONLY |
                       IMAGE_FUNCTION_SPARSE),
                      ir_intrinsic_image_sparse_load);
}

ir_variable *
builtin_builder::in_var(const glsl_type *type, const char *name)
{
   return new(mem_ctx) ir_variable(type, name, ir_var_function_in);
}

ir_variable *
builtin_builder::in_highp_var(const glsl_type *type, const char *name)
{
   ir_variable *var = in_var(type, name);
   var->data.precision = GLSL_PRECISION_HIGH;
   return var;
}

ir_variable *
builtin_builder::in_mediump_var(const glsl_type *type, const char *name)
{
   ir_variable *var = in_var(type, name);
   var->data.precision = GLSL_PRECISION_MEDIUM;
   return var;
}

ir_variable *
builtin_builder::out_var(const glsl_type *type, const char *name)
{
   return new(mem_ctx) ir_variable(type, name, ir_var_function_out);
}

ir_variable *
builtin_builder::out_lowp_var(const glsl_type *type, const char *name)
{
   ir_variable *var = out_var(type, name);
   var->data.precision = GLSL_PRECISION_LOW;
   return var;
}

ir_variable *
builtin_builder::out_highp_var(const glsl_type *type, const char *name)
{
   ir_variable *var = out_var(type, name);
   var->data.precision = GLSL_PRECISION_HIGH;
   return var;
}

ir_variable *
builtin_builder::as_highp(ir_factory &body, ir_variable *var)
{
   ir_variable *t = body.make_temp(var->type, "highp_tmp");
   body.emit(assign(t, var));
   return t;
}

ir_constant *
builtin_builder::imm(float16_t f16, unsigned vector_elements)
{
   return new(mem_ctx) ir_constant(f16, vector_elements);
}

ir_constant *
builtin_builder::imm(bool b, unsigned vector_elements)
{
   return new(mem_ctx) ir_constant(b, vector_elements);
}

ir_constant *
builtin_builder::imm(float f, unsigned vector_elements)
{
   return new(mem_ctx) ir_constant(f, vector_elements);
}

ir_constant *
builtin_builder::imm(int i, unsigned vector_elements)
{
   return new(mem_ctx) ir_constant(i, vector_elements);
}

ir_constant *
builtin_builder::imm(unsigned u, unsigned vector_elements)
{
   return new(mem_ctx) ir_constant(u, vector_elements);
}

ir_constant *
builtin_builder::imm(double d, unsigned vector_elements)
{
   return new(mem_ctx) ir_constant(d, vector_elements);
}

ir_constant *
builtin_builder::imm(const glsl_type *type, const ir_constant_data &data)
{
   return new(mem_ctx) ir_constant(type, &data);
}

#define IMM_FP(type, val) (glsl_type_is_double(type)) ? imm(val) : \
   (glsl_type_is_float_16(type)  ? imm((float16_t)val) : imm((float)val))

ir_dereference_variable *
builtin_builder::var_ref(ir_variable *var)
{
   return new(mem_ctx) ir_dereference_variable(var);
}

ir_dereference_array *
builtin_builder::array_ref(ir_variable *var, int idx)
{
   return new(mem_ctx) ir_dereference_array(var, imm(idx));
}

/** Return an element of a matrix */
ir_swizzle *
builtin_builder::matrix_elt(ir_variable *var, int column, int row)
{
   return swizzle(array_ref(var, column), row, 1);
}

ir_dereference_record *
builtin_builder::record_ref(ir_variable *var, const char *field)
{
   return new(mem_ctx) ir_dereference_record(var, field);
}

/**
 * Implementations of built-in functions:
 *  @{
 */
ir_function_signature *
builtin_builder::new_sig(const glsl_type *return_type,
                         builtin_available_predicate avail,
                         int num_params,
                         ...)
{
   va_list ap;

   ir_function_signature *sig =
      new(mem_ctx) ir_function_signature(return_type, avail);

   ir_exec_list plist;
   va_start(ap, num_params);
   for (int i = 0; i < num_params; i++) {
      plist.push_tail(va_arg(ap, ir_variable *));
   }
   va_end(ap);

   sig->replace_parameters(&plist);
   return sig;
}

#define MAKE_SIG(return_type, avail, ...)  \
   ir_function_signature *sig =               \
      new_sig(return_type, avail, __VA_ARGS__);      \
   ir_factory body(&sig->body, mem_ctx);             \
   sig->is_defined = true;

#define MAKE_INTRINSIC(return_type, id, avail, ...)  \
   ir_function_signature *sig =                      \
      new_sig(return_type, avail, __VA_ARGS__);      \
   sig->intrinsic_id = id;

ir_function_signature *
builtin_builder::unop(builtin_available_predicate avail,
                      ir_expression_operation opcode,
                      const glsl_type *return_type,
                      const glsl_type *param_type)
{
   ir_variable *x = in_var(param_type, "x");
   MAKE_SIG(return_type, avail, 1, x);
   body.emit(ret(expr(opcode, x)));
   return sig;
}

#define UNOP(NAME, OPCODE, AVAIL)               \
ir_function_signature *                         \
builtin_builder::_##NAME(const glsl_type *type) \
{                                               \
   return unop(&AVAIL, OPCODE, type, type);     \
}

#define UNOPA(NAME, OPCODE)               \
ir_function_signature *                         \
builtin_builder::_##NAME(builtin_available_predicate avail, const glsl_type *type) \
{                                               \
   return unop(avail, OPCODE, type, type);     \
}

ir_function_signature *
builtin_builder::binop(builtin_available_predicate avail,
                       ir_expression_operation opcode,
                       const glsl_type *return_type,
                       const glsl_type *param0_type,
                       const glsl_type *param1_type,
                       bool swap_operands)
{
   ir_variable *x = in_var(param0_type, "x");
   ir_variable *y = in_var(param1_type, "y");
   MAKE_SIG(return_type, avail, 2, x, y);

   if (swap_operands)
      body.emit(ret(expr(opcode, y, x)));
   else
      body.emit(ret(expr(opcode, x, y)));

   return sig;
}

#define BINOP(NAME, OPCODE, AVAIL)                                      \
ir_function_signature *                                                 \
builtin_builder::_##NAME(const glsl_type *return_type,                  \
                         const glsl_type *param0_type,                  \
                         const glsl_type *param1_type)                  \
{                                                                       \
   return binop(&AVAIL, OPCODE, return_type, param0_type, param1_type); \
}

/**
 * Angle and Trigonometry Functions @{
 */

ir_function_signature *
builtin_builder::_radians(builtin_available_predicate avail,
                          const glsl_type *type)
{
   ir_variable *degrees = in_var(type, "degrees");
   MAKE_SIG(type, avail, 1, degrees);
   body.emit(ret(mul(degrees, IMM_FP(type, 0.0174532925f))));
   return sig;
}

ir_function_signature *
builtin_builder::_degrees(builtin_available_predicate avail,
                          const glsl_type *type)
{
   ir_variable *radians = in_var(type, "radians");
   MAKE_SIG(type, avail, 1, radians);
   body.emit(ret(mul(radians, IMM_FP(type, 57.29578f))));
   return sig;
}

UNOPA(sin, ir_unop_sin)
UNOPA(cos, ir_unop_cos)

ir_function_signature *
builtin_builder::_tan(builtin_available_predicate avail,
                      const glsl_type *type)
{
   ir_variable *theta = in_var(type, "theta");
   MAKE_SIG(type, avail, 1, theta);
   body.emit(ret(div(sin(theta), cos(theta))));
   return sig;
}

ir_expression *
builtin_builder::asin_expr(ir_variable *x, float p0, float p1)
{
   return mul(sign(x),
              sub(IMM_FP(x->type, M_PI_2f),
                  mul(sqrt(sub(IMM_FP(x->type, 1.0f), abs(x))),
                      add(IMM_FP(x->type, M_PI_2f),
                          mul(abs(x),
                              add(IMM_FP(x->type, (M_PI_4f - 1.0f)),
                                  mul(abs(x),
                                      add(IMM_FP(x->type, p0),
                                          mul(abs(x), IMM_FP(x->type, p1))))))))));
}

/**
 * Generate a ir_call to a function with a set of parameters
 *
 * The input \c params can either be a list of \c ir_variable or a list of
 * \c ir_dereference_variable.  In the latter case, all nodes will be removed
 * from \c params and used directly as the parameters to the generated
 * \c ir_call.
 */
ir_call *
builtin_builder::call(ir_function *f, ir_variable *ret, ir_exec_list params)
{
   ir_exec_list actual_params;

   ir_foreach_in_list_safe(ir_instruction, ir, &params) {
      ir_dereference_variable *d = ir->as_dereference_variable();
      if (d != NULL) {
         d->remove();
         actual_params.push_tail(d);
      } else {
         ir_variable *var = ir->as_variable();
         assert(var != NULL);
         actual_params.push_tail(var_ref(var));
      }
   }

   ir_function_signature *sig =
      f->exact_matching_signature(NULL, &actual_params);
   if (!sig)
      return NULL;

   ir_dereference_variable *deref =
      (glsl_type_is_void(sig->return_type) ? NULL : var_ref(ret));

   return new(mem_ctx) ir_call(sig, deref, &actual_params);
}

ir_function_signature *
builtin_builder::_asin(builtin_available_predicate avail,
                       const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(type, avail, 1, x);

   body.emit(ret(asin_expr(x, 0.086566724f, -0.03102955f)));

   return sig;
}

ir_function_signature *
builtin_builder::_acos(builtin_available_predicate avail,
                       const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(type, avail, 1, x);

   body.emit(ret(sub(IMM_FP(type, M_PI_2f), asin_expr(x, 0.08132463f, -0.02363318f))));

   return sig;
}

ir_function_signature *
builtin_builder::_sinh(builtin_available_predicate avail,
                       const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(type, avail, 1, x);

   /* 0.5 * (e^x - e^(-x)) */
   body.emit(ret(mul(IMM_FP(type, 0.5f), sub(exp(x), exp(neg(x))))));

   return sig;
}

ir_function_signature *
builtin_builder::_cosh(builtin_available_predicate avail,
                       const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(type, avail, 1, x);

   /* 0.5 * (e^x + e^(-x)) */
   body.emit(ret(mul(IMM_FP(type, 0.5f), add(exp(x), exp(neg(x))))));

   return sig;
}

ir_function_signature *
builtin_builder::_tanh(builtin_available_predicate avail,
                       const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(type, avail, 1, x);

   /* Clamp x to [-10, +10] to avoid precision problems.
    * When x > 10, e^(-x) is so small relative to e^x that it gets flushed to
    * zero in the computation e^x + e^(-x). The same happens in the other
    * direction when x < -10.
    */
   ir_variable *t = body.make_temp(type, "tmp");
   body.emit(assign(t, min2(max2(x, IMM_FP(type, -10.0f)), IMM_FP(type, 10.0f))));

   /* (e^x - e^(-x)) / (e^x + e^(-x)) */
   body.emit(ret(div(sub(exp(t), exp(neg(t))),
                     add(exp(t), exp(neg(t))))));

   return sig;
}

ir_function_signature *
builtin_builder::_asinh(builtin_available_predicate avail,
                        const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(type, avail, 1, x);

   body.emit(ret(mul(sign(x), log(add(abs(x), sqrt(add(mul(x, x),
                                                       IMM_FP(type, 1.0f))))))));
   return sig;
}

ir_function_signature *
builtin_builder::_acosh(builtin_available_predicate avail,
                        const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(type, avail, 1, x);

   body.emit(ret(log(add(x, sqrt(sub(mul(x, x), IMM_FP(type, 1.0f)))))));
   return sig;
}

ir_function_signature *
builtin_builder::_atanh(builtin_available_predicate avail,
                        const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(type, avail, 1, x);

   body.emit(ret(mul(IMM_FP(type, 0.5f), log(div(add(IMM_FP(type, 1.0f), x),
                                                 sub(IMM_FP(type, 1.0f), x))))));
   return sig;
}
/** @} */

/**
 * Exponential Functions @{
 */

ir_function_signature *
builtin_builder::_pow(builtin_available_predicate avail,
                      const glsl_type *type)
{
   return binop(avail, ir_binop_pow, type, type, type);
}

UNOPA(exp,         ir_unop_exp)
UNOPA(log,         ir_unop_log)
UNOPA(exp2,        ir_unop_exp2)
UNOPA(log2,        ir_unop_log2)
UNOPA(atan,        ir_unop_atan)
UNOPA(sqrt,        ir_unop_sqrt)
UNOPA(inversesqrt, ir_unop_rsq)

/** @} */

UNOPA(abs,       ir_unop_abs)
UNOPA(sign,      ir_unop_sign)
UNOPA(floor,     ir_unop_floor)
UNOPA(truncate,  ir_unop_trunc)
UNOPA(trunc,     ir_unop_trunc)
UNOPA(round,     ir_unop_round_even)
UNOPA(roundEven, ir_unop_round_even)
UNOPA(ceil,      ir_unop_ceil)
UNOPA(fract,     ir_unop_fract)

ir_function_signature *
builtin_builder::_mod(builtin_available_predicate avail,
                      const glsl_type *x_type, const glsl_type *y_type)
{
   return binop(avail, ir_binop_mod, x_type, x_type, y_type);
}

ir_function_signature *
builtin_builder::_modf(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   ir_variable *i = out_var(type, "i");
   MAKE_SIG(type, avail, 2, x, i);

   ir_variable *t = body.make_temp(type, "t");
   body.emit(assign(t, expr(ir_unop_trunc, x)));
   body.emit(assign(i, t));
   body.emit(ret(sub(x, t)));

   return sig;
}

ir_function_signature *
builtin_builder::_min(builtin_available_predicate avail,
                      const glsl_type *x_type, const glsl_type *y_type)
{
   return binop(avail, ir_binop_min, x_type, x_type, y_type);
}

ir_function_signature *
builtin_builder::_max(builtin_available_predicate avail,
                      const glsl_type *x_type, const glsl_type *y_type)
{
   return binop(avail, ir_binop_max, x_type, x_type, y_type);
}

ir_function_signature *
builtin_builder::_clamp(builtin_available_predicate avail,
                        const glsl_type *val_type, const glsl_type *bound_type)
{
   ir_variable *x = in_var(val_type, "x");
   ir_variable *minVal = in_var(bound_type, "minVal");
   ir_variable *maxVal = in_var(bound_type, "maxVal");
   MAKE_SIG(val_type, avail, 3, x, minVal, maxVal);

   body.emit(ret(clamp(x, minVal, maxVal)));

   return sig;
}

ir_function_signature *
builtin_builder::_mix_lrp(builtin_available_predicate avail, const glsl_type *val_type, const glsl_type *blend_type)
{
   ir_variable *x = in_var(val_type, "x");
   ir_variable *y = in_var(val_type, "y");
   ir_variable *a = in_var(blend_type, "a");
   MAKE_SIG(val_type, avail, 3, x, y, a);

   body.emit(ret(lrp(x, y, a)));

   return sig;
}

ir_function_signature *
builtin_builder::_mix_sel(builtin_available_predicate avail,
                          const glsl_type *val_type,
                          const glsl_type *blend_type)
{
   ir_variable *x = in_var(val_type, "x");
   ir_variable *y = in_var(val_type, "y");
   ir_variable *a = in_var(blend_type, "a");
   MAKE_SIG(val_type, avail, 3, x, y, a);

   /* csel matches the ternary operator in that a selector of true choses the
    * first argument. This differs from mix(x, y, false) which choses the
    * second argument (to remain consistent with the interpolating version of
    * mix() which takes a blend factor from 0.0 to 1.0 where 0.0 is only x.
    *
    * To handle the behavior mismatch, reverse the x and y arguments.
    */
   body.emit(ret(csel(a, y, x)));

   return sig;
}

ir_function_signature *
builtin_builder::_step(builtin_available_predicate avail, const glsl_type *edge_type, const glsl_type *x_type)
{
   ir_variable *edge = in_var(edge_type, "edge");
   ir_variable *x = in_var(x_type, "x");
   MAKE_SIG(x_type, avail, 2, edge, x);

   ir_variable *t = body.make_temp(x_type, "t");
   if (x_type->vector_elements == 1) {
      /* Both are floats */
      if (glsl_type_is_double(edge_type))
         body.emit(assign(t, f2d(b2f(gequal(x, edge)))));
      else if (glsl_type_is_float_16(edge_type))
         body.emit(assign(t, f2f16(b2f(gequal(x, edge)))));
      else
         body.emit(assign(t, b2f(gequal(x, edge))));
   } else if (edge_type->vector_elements == 1) {
      /* x is a vector but edge is a float */
      for (int i = 0; i < x_type->vector_elements; i++) {
         if (glsl_type_is_double(edge_type))
            body.emit(assign(t, f2d(b2f(gequal(swizzle(x, i, 1), edge))), 1 << i));
         else if (glsl_type_is_float_16(edge_type))
            body.emit(assign(t, f2f16(b2f(gequal(swizzle(x, i, 1), edge))), 1 << i));
         else
            body.emit(assign(t, b2f(gequal(swizzle(x, i, 1), edge)), 1 << i));
      }
   } else {
      /* Both are vectors */
      for (int i = 0; i < x_type->vector_elements; i++) {
         if (glsl_type_is_double(edge_type))
            body.emit(assign(t, f2d(b2f(gequal(swizzle(x, i, 1), swizzle(edge, i, 1)))),
                             1 << i));
         else if (glsl_type_is_float_16(edge_type))
            body.emit(assign(t, f2f16(b2f(gequal(swizzle(x, i, 1), swizzle(edge, i, 1)))),
                             1 << i));
         else
            body.emit(assign(t, b2f(gequal(swizzle(x, i, 1), swizzle(edge, i, 1))),
                             1 << i));

      }
   }
   body.emit(ret(t));

   return sig;
}

ir_function_signature *
builtin_builder::_smoothstep(builtin_available_predicate avail, const glsl_type *edge_type, const glsl_type *x_type)
{
   ir_variable *edge0 = in_var(edge_type, "edge0");
   ir_variable *edge1 = in_var(edge_type, "edge1");
   ir_variable *x = in_var(x_type, "x");
   MAKE_SIG(x_type, avail, 3, edge0, edge1, x);

   /* From the GLSL 1.10 specification:
    *
    *    genType t;
    *    t = clamp((x - edge0) / (edge1 - edge0), 0, 1);
    *    return t * t * (3 - 2 * t);
    */

   ir_variable *t = body.make_temp(x_type, "t");
   body.emit(assign(t, clamp(div(sub(x, edge0), sub(edge1, edge0)),
                             IMM_FP(x_type, 0.0), IMM_FP(x_type, 1.0))));

   body.emit(ret(mul(t, mul(t, sub(IMM_FP(x_type, 3.0), mul(IMM_FP(x_type, 2.0), t))))));

   return sig;
}

ir_function_signature *
builtin_builder::_isnan(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_bvec_type(type->vector_elements), avail, 1, x);

   body.emit(ret(nequal(x, x)));

   return sig;
}

ir_function_signature *
builtin_builder::_isinf(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_bvec_type(type->vector_elements), avail, 1, x);

   ir_constant_data infinities;
   for (int i = 0; i < type->vector_elements; i++) {
      switch (type->base_type) {
      case GLSL_TYPE_FLOAT16:
         infinities.f16[i] = _mesa_float_to_half(INFINITY);
         break;
      case GLSL_TYPE_FLOAT:
         infinities.f[i] = INFINITY;
         break;
      case GLSL_TYPE_DOUBLE:
         infinities.d[i] = INFINITY;
         break;
      default:
         UNREACHABLE("unknown type");
      }
   }

   body.emit(ret(equal(abs(x), imm(type, infinities))));

   return sig;
}

ir_function_signature *
builtin_builder::_atan2(builtin_available_predicate avail,
                        const glsl_type *x_type)
{
   return binop(avail, ir_binop_atan2, x_type, x_type, x_type);
}

ir_function_signature *
builtin_builder::_floatBitsToInt(const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_ivec_type(type->vector_elements), shader_bit_encoding, 1, x);
   body.emit(ret(bitcast_f2i(as_highp(body, x))));
   return sig;
}

ir_function_signature *
builtin_builder::_floatBitsToUint(const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_uvec_type(type->vector_elements), shader_bit_encoding, 1, x);
   body.emit(ret(bitcast_f2u(as_highp(body, x))));
   return sig;
}

ir_function_signature *
builtin_builder::_intBitsToFloat(const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_vec_type(type->vector_elements), shader_bit_encoding, 1, x);
   body.emit(ret(bitcast_i2f(as_highp(body, x))));
   return sig;
}

ir_function_signature *
builtin_builder::_uintBitsToFloat(const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_vec_type(type->vector_elements), shader_bit_encoding, 1, x);
   body.emit(ret(bitcast_u2f(as_highp(body, x))));
   return sig;
}

ir_function_signature *
builtin_builder::_doubleBitsToInt64(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_i64vec_type(type->vector_elements), avail, 1, x);
   body.emit(ret(bitcast_d2i64(x)));
   return sig;
}

ir_function_signature *
builtin_builder::_doubleBitsToUint64(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_u64vec_type(type->vector_elements), avail, 1, x);
   body.emit(ret(bitcast_d2u64(x)));
   return sig;
}

ir_function_signature *
builtin_builder::_int64BitsToDouble(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_dvec_type(type->vector_elements), avail, 1, x);
   body.emit(ret(bitcast_i642d(x)));
   return sig;
}

ir_function_signature *
builtin_builder::_uint64BitsToDouble(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_dvec_type(type->vector_elements), avail, 1, x);
   body.emit(ret(bitcast_u642d(x)));
   return sig;
}

ir_function_signature *
builtin_builder::_packUnorm2x16(builtin_available_predicate avail)
{
   ir_variable *v = in_highp_var(&glsl_type_builtin_vec2, "v");
   MAKE_SIG(&glsl_type_builtin_uint, avail, 1, v);
   sig->return_precision = GLSL_PRECISION_HIGH;
   body.emit(ret(expr(ir_unop_pack_unorm_2x16, v)));
   return sig;
}

ir_function_signature *
builtin_builder::_packSnorm2x16(builtin_available_predicate avail)
{
   ir_variable *v = in_var(&glsl_type_builtin_vec2, "v");
   MAKE_SIG(&glsl_type_builtin_uint, avail, 1, v);
   sig->return_precision = GLSL_PRECISION_HIGH;
   body.emit(ret(expr(ir_unop_pack_snorm_2x16, v)));
   return sig;
}

ir_function_signature *
builtin_builder::_packUnorm4x8(builtin_available_predicate avail)
{
   ir_variable *v = in_mediump_var(&glsl_type_builtin_vec4, "v");
   MAKE_SIG(&glsl_type_builtin_uint, avail, 1, v);
   sig->return_precision = GLSL_PRECISION_HIGH;
   body.emit(ret(expr(ir_unop_pack_unorm_4x8, v)));
   return sig;
}

ir_function_signature *
builtin_builder::_packSnorm4x8(builtin_available_predicate avail)
{
   ir_variable *v = in_mediump_var(&glsl_type_builtin_vec4, "v");
   MAKE_SIG(&glsl_type_builtin_uint, avail, 1, v);
   sig->return_precision = GLSL_PRECISION_HIGH;
   body.emit(ret(expr(ir_unop_pack_snorm_4x8, v)));
   return sig;
}

ir_function_signature *
builtin_builder::_unpackUnorm2x16(builtin_available_predicate avail)
{
   ir_variable *p = in_highp_var(&glsl_type_builtin_uint, "p");
   MAKE_SIG(&glsl_type_builtin_vec2, avail, 1, p);
   sig->return_precision = GLSL_PRECISION_HIGH;
   body.emit(ret(expr(ir_unop_unpack_unorm_2x16, p)));
   return sig;
}

ir_function_signature *
builtin_builder::_unpackSnorm2x16(builtin_available_predicate avail)
{
   ir_variable *p = in_highp_var(&glsl_type_builtin_uint, "p");
   MAKE_SIG(&glsl_type_builtin_vec2, avail, 1, p);
   sig->return_precision = GLSL_PRECISION_HIGH;
   body.emit(ret(expr(ir_unop_unpack_snorm_2x16, p)));
   return sig;
}


ir_function_signature *
builtin_builder::_unpackUnorm4x8(builtin_available_predicate avail)
{
   ir_variable *p = in_highp_var(&glsl_type_builtin_uint, "p");
   MAKE_SIG(&glsl_type_builtin_vec4, avail, 1, p);
   sig->return_precision = GLSL_PRECISION_MEDIUM;
   body.emit(ret(expr(ir_unop_unpack_unorm_4x8, p)));
   return sig;
}

ir_function_signature *
builtin_builder::_unpackSnorm4x8(builtin_available_predicate avail)
{
   ir_variable *p = in_highp_var(&glsl_type_builtin_uint, "p");
   MAKE_SIG(&glsl_type_builtin_vec4, avail, 1, p);
   sig->return_precision = GLSL_PRECISION_MEDIUM;
   body.emit(ret(expr(ir_unop_unpack_snorm_4x8, p)));
   return sig;
}

ir_function_signature *
builtin_builder::_packHalf2x16(builtin_available_predicate avail)
{
   ir_variable *v = in_mediump_var(&glsl_type_builtin_vec2, "v");
   MAKE_SIG(&glsl_type_builtin_uint, avail, 1, v);
   sig->return_precision = GLSL_PRECISION_HIGH;
   body.emit(ret(expr(ir_unop_pack_half_2x16, v)));
   return sig;
}

ir_function_signature *
builtin_builder::_unpackHalf2x16(builtin_available_predicate avail)
{
   ir_variable *p = in_highp_var(&glsl_type_builtin_uint, "p");
   MAKE_SIG(&glsl_type_builtin_vec2, avail, 1, p);
   sig->return_precision = GLSL_PRECISION_MEDIUM;
   body.emit(ret(expr(ir_unop_unpack_half_2x16, p)));
   return sig;
}

ir_function_signature *
builtin_builder::_packFloat2x16(builtin_available_predicate avail)
{
   ir_variable *v = in_var(&glsl_type_builtin_f16vec2, "v");
   MAKE_SIG(&glsl_type_builtin_uint, avail, 1, v);

   ir_rvalue *value = new(mem_ctx)ir_dereference_variable(v);
   body.emit(ret(expr(ir_unop_pack_half_2x16, new(mem_ctx) ir_expression(ir_unop_f162f, &glsl_type_builtin_vec2, value, NULL))));
   return sig;
}

ir_function_signature *
builtin_builder::_unpackFloat2x16(builtin_available_predicate avail)
{
   ir_variable *p = in_var(&glsl_type_builtin_uint, "p");
   MAKE_SIG(&glsl_type_builtin_f16vec2, avail, 1, p);
   body.emit(ret(f2f16(expr(ir_unop_unpack_half_2x16, p))));
   return sig;
}

ir_function_signature *
builtin_builder::_packDouble2x32(builtin_available_predicate avail)
{
   ir_variable *v = in_var(&glsl_type_builtin_uvec2, "v");
   MAKE_SIG(&glsl_type_builtin_double, avail, 1, v);
   body.emit(ret(expr(ir_unop_pack_double_2x32, v)));
   return sig;
}

ir_function_signature *
builtin_builder::_unpackDouble2x32(builtin_available_predicate avail)
{
   ir_variable *p = in_var(&glsl_type_builtin_double, "p");
   MAKE_SIG(&glsl_type_builtin_uvec2, avail, 1, p);
   body.emit(ret(expr(ir_unop_unpack_double_2x32, p)));
   return sig;
}

ir_function_signature *
builtin_builder::_packInt2x32(builtin_available_predicate avail)
{
   ir_variable *v = in_var(&glsl_type_builtin_ivec2, "v");
   MAKE_SIG(&glsl_type_builtin_int64_t, avail, 1, v);
   body.emit(ret(expr(ir_unop_pack_int_2x32, v)));
   return sig;
}

ir_function_signature *
builtin_builder::_unpackInt2x32(builtin_available_predicate avail)
{
   ir_variable *p = in_var(&glsl_type_builtin_int64_t, "p");
   MAKE_SIG(&glsl_type_builtin_ivec2, avail, 1, p);
   body.emit(ret(expr(ir_unop_unpack_int_2x32, p)));
   return sig;
}

ir_function_signature *
builtin_builder::_packUint2x32(builtin_available_predicate avail)
{
   ir_variable *v = in_var(&glsl_type_builtin_uvec2, "v");
   MAKE_SIG(&glsl_type_builtin_uint64_t, avail, 1, v);
   body.emit(ret(expr(ir_unop_pack_uint_2x32, v)));
   return sig;
}

ir_function_signature *
builtin_builder::_unpackUint2x32(builtin_available_predicate avail)
{
   ir_variable *p = in_var(&glsl_type_builtin_uint64_t, "p");
   MAKE_SIG(&glsl_type_builtin_uvec2, avail, 1, p);
   body.emit(ret(expr(ir_unop_unpack_uint_2x32, p)));
   return sig;
}

ir_function_signature *
builtin_builder::_length(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_get_base_glsl_type(type), avail, 1, x);

   body.emit(ret(sqrt(dot(x, x))));

   return sig;
}

ir_function_signature *
builtin_builder::_distance(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *p0 = in_var(type, "p0");
   ir_variable *p1 = in_var(type, "p1");
   MAKE_SIG(glsl_get_base_glsl_type(type), avail, 2, p0, p1);

   if (type->vector_elements == 1) {
      body.emit(ret(abs(sub(p0, p1))));
   } else {
      ir_variable *p = body.make_temp(type, "p");
      body.emit(assign(p, sub(p0, p1)));
      body.emit(ret(sqrt(dot(p, p))));
   }

   return sig;
}

ir_function_signature *
builtin_builder::_dot(builtin_available_predicate avail, const glsl_type *type)
{
   if (type->vector_elements == 1)
      return binop(avail, ir_binop_mul, type, type, type);

   return binop(avail, ir_binop_dot,
                glsl_get_base_glsl_type(type), type, type);
}

ir_function_signature *
builtin_builder::_cross(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *a = in_var(type, "a");
   ir_variable *b = in_var(type, "b");
   MAKE_SIG(type, avail, 2, a, b);

   int yzx = MAKE_SWIZZLE4(SWIZZLE_Y, SWIZZLE_Z, SWIZZLE_X, 0);
   int zxy = MAKE_SWIZZLE4(SWIZZLE_Z, SWIZZLE_X, SWIZZLE_Y, 0);

   body.emit(ret(sub(mul(swizzle(a, yzx, 3), swizzle(b, zxy, 3)),
                     mul(swizzle(a, zxy, 3), swizzle(b, yzx, 3)))));

   return sig;
}

ir_function_signature *
builtin_builder::_normalize(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(type, avail, 1, x);

   if (type->vector_elements == 1) {
      body.emit(ret(sign(x)));
   } else {
      body.emit(ret(mul(x, rsq(dot(x, x)))));
   }

   return sig;
}

ir_function_signature *
builtin_builder::_ftransform()
{
   MAKE_SIG(&glsl_type_builtin_vec4, compatibility_vs_only, 0);

   /* ftransform() refers to global variables, and is always emitted
    * directly by ast_function.cpp.  Just emit a prototype here so we
    * can recognize calls to it.
    */
   return sig;
}

ir_function_signature *
builtin_builder::_faceforward(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *N = in_var(type, "N");
   ir_variable *I = in_var(type, "I");
   ir_variable *Nref = in_var(type, "Nref");
   MAKE_SIG(type, avail, 3, N, I, Nref);

   body.emit(if_tree(less(dot(Nref, I), IMM_FP(type, 0.0)),
                     ret(N), ret(neg(N))));

   return sig;
}

ir_function_signature *
builtin_builder::_reflect(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *I = in_var(type, "I");
   ir_variable *N = in_var(type, "N");
   MAKE_SIG(type, avail, 2, I, N);

   /* I - 2 * dot(N, I) * N */
   body.emit(ret(sub(I, mul(IMM_FP(type, 2.0), mul(dot(N, I), N)))));

   return sig;
}

ir_function_signature *
builtin_builder::_refract(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *I = in_var(type, "I");
   ir_variable *N = in_var(type, "N");
   ir_variable *eta = in_var(glsl_get_base_glsl_type(type), "eta");
   MAKE_SIG(type, avail, 3, I, N, eta);

   ir_variable *n_dot_i = body.make_temp(glsl_get_base_glsl_type(type), "n_dot_i");
   body.emit(assign(n_dot_i, dot(N, I)));

   /* From the GLSL 1.10 specification:
    * k = 1.0 - eta * eta * (1.0 - dot(N, I) * dot(N, I))
    * if (k < 0.0)
    *    return genType(0.0)
    * else
    *    return eta * I - (eta * dot(N, I) + sqrt(k)) * N
    */
   ir_variable *k = body.make_temp(glsl_get_base_glsl_type(type), "k");
   body.emit(assign(k, sub(IMM_FP(type, 1.0),
                           mul(eta, mul(eta, sub(IMM_FP(type, 1.0),
                                                 mul(n_dot_i, n_dot_i)))))));
   body.emit(if_tree(less(k, IMM_FP(type, 0.0)),
                     ret(ir_constant::zero(mem_ctx, type)),
                     ret(sub(mul(eta, I),
                             mul(add(mul(eta, n_dot_i), sqrt(k)), N)))));

   return sig;
}

ir_function_signature *
builtin_builder::_matrixCompMult(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   ir_variable *y = in_var(type, "y");
   MAKE_SIG(type, avail, 2, x, y);

   ir_variable *z = body.make_temp(type, "z");
   for (int i = 0; i < type->matrix_columns; i++) {
      body.emit(assign(array_ref(z, i), mul(array_ref(x, i), array_ref(y, i))));
   }
   body.emit(ret(z));

   return sig;
}

ir_function_signature *
builtin_builder::_outerProduct(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *c;
   ir_variable *r;

   if (glsl_type_is_double(type)) {
      r = in_var(glsl_dvec_type(type->matrix_columns), "r");
      c = in_var(glsl_dvec_type(type->vector_elements), "c");
   } else if (glsl_type_is_float_16(type)) {
      r = in_var(glsl_f16vec_type(type->matrix_columns), "r");
      c = in_var(glsl_f16vec_type(type->vector_elements), "c");
   } else {
      r = in_var(glsl_vec_type(type->matrix_columns), "r");
      c = in_var(glsl_vec_type(type->vector_elements), "c");
   }
   MAKE_SIG(type, avail, 2, c, r);

   ir_variable *m = body.make_temp(type, "m");
   for (int i = 0; i < type->matrix_columns; i++) {
      body.emit(assign(array_ref(m, i), mul(c, swizzle(r, i, 1))));
   }
   body.emit(ret(m));

   return sig;
}

ir_function_signature *
builtin_builder::_transpose(builtin_available_predicate avail, const glsl_type *orig_type)
{
   const glsl_type *transpose_type =
      glsl_simple_type(orig_type->base_type,
                       orig_type->matrix_columns,
                       orig_type->vector_elements);

   ir_variable *m = in_var(orig_type, "m");
   MAKE_SIG(transpose_type, avail, 1, m);

   ir_variable *t = body.make_temp(transpose_type, "t");
   for (int i = 0; i < orig_type->matrix_columns; i++) {
      for (int j = 0; j < orig_type->vector_elements; j++) {
         body.emit(assign(array_ref(t, j),
                          matrix_elt(m, i, j),
                          1 << i));
      }
   }
   body.emit(ret(t));

   return sig;
}

ir_function_signature *
builtin_builder::_determinant_mat2(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *m = in_var(type, "m");
   MAKE_SIG(glsl_get_base_glsl_type(type), avail, 1, m);

   body.emit(ret(sub(mul(matrix_elt(m, 0, 0), matrix_elt(m, 1, 1)),
                     mul(matrix_elt(m, 1, 0), matrix_elt(m, 0, 1)))));

   return sig;
}

ir_function_signature *
builtin_builder::_determinant_mat3(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *m = in_var(type, "m");
   MAKE_SIG(glsl_get_base_glsl_type(type), avail, 1, m);

   ir_expression *f1 =
      sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 2, 2)),
          mul(matrix_elt(m, 1, 2), matrix_elt(m, 2, 1)));

   ir_expression *f2 =
      sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 2)),
          mul(matrix_elt(m, 1, 2), matrix_elt(m, 2, 0)));

   ir_expression *f3 =
      sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 1)),
          mul(matrix_elt(m, 1, 1), matrix_elt(m, 2, 0)));

   body.emit(ret(add(sub(mul(matrix_elt(m, 0, 0), f1),
                         mul(matrix_elt(m, 0, 1), f2)),
                     mul(matrix_elt(m, 0, 2), f3))));

   return sig;
}

ir_function_signature *
builtin_builder::_determinant_mat4(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *m = in_var(type, "m");
   const glsl_type *btype = glsl_get_base_glsl_type(type);
   MAKE_SIG(btype, avail, 1, m);

   ir_variable *SubFactor00 = body.make_temp(btype, "SubFactor00");
   ir_variable *SubFactor01 = body.make_temp(btype, "SubFactor01");
   ir_variable *SubFactor02 = body.make_temp(btype, "SubFactor02");
   ir_variable *SubFactor03 = body.make_temp(btype, "SubFactor03");
   ir_variable *SubFactor04 = body.make_temp(btype, "SubFactor04");
   ir_variable *SubFactor05 = body.make_temp(btype, "SubFactor05");
   ir_variable *SubFactor06 = body.make_temp(btype, "SubFactor06");
   ir_variable *SubFactor07 = body.make_temp(btype, "SubFactor07");
   ir_variable *SubFactor08 = body.make_temp(btype, "SubFactor08");
   ir_variable *SubFactor09 = body.make_temp(btype, "SubFactor09");
   ir_variable *SubFactor10 = body.make_temp(btype, "SubFactor10");
   ir_variable *SubFactor11 = body.make_temp(btype, "SubFactor11");
   ir_variable *SubFactor12 = body.make_temp(btype, "SubFactor12");
   ir_variable *SubFactor13 = body.make_temp(btype, "SubFactor13");
   ir_variable *SubFactor14 = body.make_temp(btype, "SubFactor14");
   ir_variable *SubFactor15 = body.make_temp(btype, "SubFactor15");
   ir_variable *SubFactor16 = body.make_temp(btype, "SubFactor16");
   ir_variable *SubFactor17 = body.make_temp(btype, "SubFactor17");
   ir_variable *SubFactor18 = body.make_temp(btype, "SubFactor18");

   body.emit(assign(SubFactor00, sub(mul(matrix_elt(m, 2, 2), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 2), matrix_elt(m, 2, 3)))));
   body.emit(assign(SubFactor01, sub(mul(matrix_elt(m, 2, 1), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 2, 3)))));
   body.emit(assign(SubFactor02, sub(mul(matrix_elt(m, 2, 1), matrix_elt(m, 3, 2)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 2, 2)))));
   body.emit(assign(SubFactor03, sub(mul(matrix_elt(m, 2, 0), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 2, 3)))));
   body.emit(assign(SubFactor04, sub(mul(matrix_elt(m, 2, 0), matrix_elt(m, 3, 2)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 2, 2)))));
   body.emit(assign(SubFactor05, sub(mul(matrix_elt(m, 2, 0), matrix_elt(m, 3, 1)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 2, 1)))));
   body.emit(assign(SubFactor06, sub(mul(matrix_elt(m, 1, 2), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 2), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor07, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor08, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 3, 2)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 1, 2)))));
   body.emit(assign(SubFactor09, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor10, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 3, 2)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 1, 2)))));
   body.emit(assign(SubFactor11, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor12, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 3, 1)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 1, 1)))));
   body.emit(assign(SubFactor13, sub(mul(matrix_elt(m, 1, 2), matrix_elt(m, 2, 3)), mul(matrix_elt(m, 2, 2), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor14, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 2, 3)), mul(matrix_elt(m, 2, 1), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor15, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 2, 2)), mul(matrix_elt(m, 2, 1), matrix_elt(m, 1, 2)))));
   body.emit(assign(SubFactor16, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 3)), mul(matrix_elt(m, 2, 0), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor17, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 2)), mul(matrix_elt(m, 2, 0), matrix_elt(m, 1, 2)))));
   body.emit(assign(SubFactor18, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 1)), mul(matrix_elt(m, 2, 0), matrix_elt(m, 1, 1)))));

   ir_variable *adj_0 = body.make_temp(btype == &glsl_type_builtin_float ? &glsl_type_builtin_vec4 : btype == &glsl_type_builtin_float16_t ? &glsl_type_builtin_f16vec4 : &glsl_type_builtin_dvec4, "adj_0");

   body.emit(assign(adj_0,
                    add(sub(mul(matrix_elt(m, 1, 1), SubFactor00),
                            mul(matrix_elt(m, 1, 2), SubFactor01)),
                        mul(matrix_elt(m, 1, 3), SubFactor02)),
                    WRITEMASK_X));
   body.emit(assign(adj_0, neg(
                    add(sub(mul(matrix_elt(m, 1, 0), SubFactor00),
                            mul(matrix_elt(m, 1, 2), SubFactor03)),
                        mul(matrix_elt(m, 1, 3), SubFactor04))),
                    WRITEMASK_Y));
   body.emit(assign(adj_0,
                    add(sub(mul(matrix_elt(m, 1, 0), SubFactor01),
                            mul(matrix_elt(m, 1, 1), SubFactor03)),
                        mul(matrix_elt(m, 1, 3), SubFactor05)),
                    WRITEMASK_Z));
   body.emit(assign(adj_0, neg(
                    add(sub(mul(matrix_elt(m, 1, 0), SubFactor02),
                            mul(matrix_elt(m, 1, 1), SubFactor04)),
                        mul(matrix_elt(m, 1, 2), SubFactor05))),
                    WRITEMASK_W));

   body.emit(ret(dot(array_ref(m, 0), adj_0)));

   return sig;
}

ir_function_signature *
builtin_builder::_inverse_mat2(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *m = in_var(type, "m");
   MAKE_SIG(type, avail, 1, m);

   ir_variable *adj = body.make_temp(type, "adj");
   body.emit(assign(array_ref(adj, 0), matrix_elt(m, 1, 1), 1 << 0));
   body.emit(assign(array_ref(adj, 0), neg(matrix_elt(m, 0, 1)), 1 << 1));
   body.emit(assign(array_ref(adj, 1), neg(matrix_elt(m, 1, 0)), 1 << 0));
   body.emit(assign(array_ref(adj, 1), matrix_elt(m, 0, 0), 1 << 1));

   ir_expression *det =
      sub(mul(matrix_elt(m, 0, 0), matrix_elt(m, 1, 1)),
          mul(matrix_elt(m, 1, 0), matrix_elt(m, 0, 1)));

   body.emit(ret(div(adj, det)));
   return sig;
}

ir_function_signature *
builtin_builder::_inverse_mat3(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *m = in_var(type, "m");
   const glsl_type *btype = glsl_get_base_glsl_type(type);
   MAKE_SIG(type, avail, 1, m);

   ir_variable *f11_22_21_12 = body.make_temp(btype, "f11_22_21_12");
   ir_variable *f10_22_20_12 = body.make_temp(btype, "f10_22_20_12");
   ir_variable *f10_21_20_11 = body.make_temp(btype, "f10_21_20_11");

   body.emit(assign(f11_22_21_12,
                    sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 2, 2)),
                        mul(matrix_elt(m, 2, 1), matrix_elt(m, 1, 2)))));
   body.emit(assign(f10_22_20_12,
                    sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 2)),
                        mul(matrix_elt(m, 2, 0), matrix_elt(m, 1, 2)))));
   body.emit(assign(f10_21_20_11,
                    sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 1)),
                        mul(matrix_elt(m, 2, 0), matrix_elt(m, 1, 1)))));

   ir_variable *adj = body.make_temp(type, "adj");
   body.emit(assign(array_ref(adj, 0), f11_22_21_12, WRITEMASK_X));
   body.emit(assign(array_ref(adj, 1), neg(f10_22_20_12), WRITEMASK_X));
   body.emit(assign(array_ref(adj, 2), f10_21_20_11, WRITEMASK_X));

   body.emit(assign(array_ref(adj, 0), neg(
                    sub(mul(matrix_elt(m, 0, 1), matrix_elt(m, 2, 2)),
                        mul(matrix_elt(m, 2, 1), matrix_elt(m, 0, 2)))),
                    WRITEMASK_Y));
   body.emit(assign(array_ref(adj, 1),
                    sub(mul(matrix_elt(m, 0, 0), matrix_elt(m, 2, 2)),
                        mul(matrix_elt(m, 2, 0), matrix_elt(m, 0, 2))),
                    WRITEMASK_Y));
   body.emit(assign(array_ref(adj, 2), neg(
                    sub(mul(matrix_elt(m, 0, 0), matrix_elt(m, 2, 1)),
                        mul(matrix_elt(m, 2, 0), matrix_elt(m, 0, 1)))),
                    WRITEMASK_Y));

   body.emit(assign(array_ref(adj, 0),
                    sub(mul(matrix_elt(m, 0, 1), matrix_elt(m, 1, 2)),
                        mul(matrix_elt(m, 1, 1), matrix_elt(m, 0, 2))),
                    WRITEMASK_Z));
   body.emit(assign(array_ref(adj, 1), neg(
                    sub(mul(matrix_elt(m, 0, 0), matrix_elt(m, 1, 2)),
                        mul(matrix_elt(m, 1, 0), matrix_elt(m, 0, 2)))),
                    WRITEMASK_Z));
   body.emit(assign(array_ref(adj, 2),
                    sub(mul(matrix_elt(m, 0, 0), matrix_elt(m, 1, 1)),
                        mul(matrix_elt(m, 1, 0), matrix_elt(m, 0, 1))),
                    WRITEMASK_Z));

   ir_expression *det =
      add(sub(mul(matrix_elt(m, 0, 0), f11_22_21_12),
              mul(matrix_elt(m, 0, 1), f10_22_20_12)),
          mul(matrix_elt(m, 0, 2), f10_21_20_11));

   body.emit(ret(div(adj, det)));

   return sig;
}

ir_function_signature *
builtin_builder::_inverse_mat4(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *m = in_var(type, "m");
   const glsl_type *btype = glsl_get_base_glsl_type(type);
   MAKE_SIG(type, avail, 1, m);

   ir_variable *SubFactor00 = body.make_temp(btype, "SubFactor00");
   ir_variable *SubFactor01 = body.make_temp(btype, "SubFactor01");
   ir_variable *SubFactor02 = body.make_temp(btype, "SubFactor02");
   ir_variable *SubFactor03 = body.make_temp(btype, "SubFactor03");
   ir_variable *SubFactor04 = body.make_temp(btype, "SubFactor04");
   ir_variable *SubFactor05 = body.make_temp(btype, "SubFactor05");
   ir_variable *SubFactor06 = body.make_temp(btype, "SubFactor06");
   ir_variable *SubFactor07 = body.make_temp(btype, "SubFactor07");
   ir_variable *SubFactor08 = body.make_temp(btype, "SubFactor08");
   ir_variable *SubFactor09 = body.make_temp(btype, "SubFactor09");
   ir_variable *SubFactor10 = body.make_temp(btype, "SubFactor10");
   ir_variable *SubFactor11 = body.make_temp(btype, "SubFactor11");
   ir_variable *SubFactor12 = body.make_temp(btype, "SubFactor12");
   ir_variable *SubFactor13 = body.make_temp(btype, "SubFactor13");
   ir_variable *SubFactor14 = body.make_temp(btype, "SubFactor14");
   ir_variable *SubFactor15 = body.make_temp(btype, "SubFactor15");
   ir_variable *SubFactor16 = body.make_temp(btype, "SubFactor16");
   ir_variable *SubFactor17 = body.make_temp(btype, "SubFactor17");
   ir_variable *SubFactor18 = body.make_temp(btype, "SubFactor18");

   body.emit(assign(SubFactor00, sub(mul(matrix_elt(m, 2, 2), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 2), matrix_elt(m, 2, 3)))));
   body.emit(assign(SubFactor01, sub(mul(matrix_elt(m, 2, 1), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 2, 3)))));
   body.emit(assign(SubFactor02, sub(mul(matrix_elt(m, 2, 1), matrix_elt(m, 3, 2)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 2, 2)))));
   body.emit(assign(SubFactor03, sub(mul(matrix_elt(m, 2, 0), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 2, 3)))));
   body.emit(assign(SubFactor04, sub(mul(matrix_elt(m, 2, 0), matrix_elt(m, 3, 2)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 2, 2)))));
   body.emit(assign(SubFactor05, sub(mul(matrix_elt(m, 2, 0), matrix_elt(m, 3, 1)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 2, 1)))));
   body.emit(assign(SubFactor06, sub(mul(matrix_elt(m, 1, 2), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 2), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor07, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor08, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 3, 2)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 1, 2)))));
   body.emit(assign(SubFactor09, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor10, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 3, 2)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 1, 2)))));
   body.emit(assign(SubFactor11, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 3, 3)), mul(matrix_elt(m, 3, 1), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor12, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 3, 1)), mul(matrix_elt(m, 3, 0), matrix_elt(m, 1, 1)))));
   body.emit(assign(SubFactor13, sub(mul(matrix_elt(m, 1, 2), matrix_elt(m, 2, 3)), mul(matrix_elt(m, 2, 2), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor14, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 2, 3)), mul(matrix_elt(m, 2, 1), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor15, sub(mul(matrix_elt(m, 1, 1), matrix_elt(m, 2, 2)), mul(matrix_elt(m, 2, 1), matrix_elt(m, 1, 2)))));
   body.emit(assign(SubFactor16, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 3)), mul(matrix_elt(m, 2, 0), matrix_elt(m, 1, 3)))));
   body.emit(assign(SubFactor17, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 2)), mul(matrix_elt(m, 2, 0), matrix_elt(m, 1, 2)))));
   body.emit(assign(SubFactor18, sub(mul(matrix_elt(m, 1, 0), matrix_elt(m, 2, 1)), mul(matrix_elt(m, 2, 0), matrix_elt(m, 1, 1)))));

   ir_variable *adj = body.make_temp(btype == &glsl_type_builtin_float ? &glsl_type_builtin_mat4 : (btype == &glsl_type_builtin_double ? &glsl_type_builtin_dmat4 : &glsl_type_builtin_f16mat4), "adj");
   body.emit(assign(array_ref(adj, 0),
                    add(sub(mul(matrix_elt(m, 1, 1), SubFactor00),
                            mul(matrix_elt(m, 1, 2), SubFactor01)),
                        mul(matrix_elt(m, 1, 3), SubFactor02)),
                    WRITEMASK_X));
   body.emit(assign(array_ref(adj, 1), neg(
                    add(sub(mul(matrix_elt(m, 1, 0), SubFactor00),
                            mul(matrix_elt(m, 1, 2), SubFactor03)),
                        mul(matrix_elt(m, 1, 3), SubFactor04))),
                    WRITEMASK_X));
   body.emit(assign(array_ref(adj, 2),
                    add(sub(mul(matrix_elt(m, 1, 0), SubFactor01),
                            mul(matrix_elt(m, 1, 1), SubFactor03)),
                        mul(matrix_elt(m, 1, 3), SubFactor05)),
                    WRITEMASK_X));
   body.emit(assign(array_ref(adj, 3), neg(
                    add(sub(mul(matrix_elt(m, 1, 0), SubFactor02),
                            mul(matrix_elt(m, 1, 1), SubFactor04)),
                        mul(matrix_elt(m, 1, 2), SubFactor05))),
                    WRITEMASK_X));

   body.emit(assign(array_ref(adj, 0), neg(
                    add(sub(mul(matrix_elt(m, 0, 1), SubFactor00),
                            mul(matrix_elt(m, 0, 2), SubFactor01)),
                        mul(matrix_elt(m, 0, 3), SubFactor02))),
                    WRITEMASK_Y));
   body.emit(assign(array_ref(adj, 1),
                    add(sub(mul(matrix_elt(m, 0, 0), SubFactor00),
                            mul(matrix_elt(m, 0, 2), SubFactor03)),
                        mul(matrix_elt(m, 0, 3), SubFactor04)),
                    WRITEMASK_Y));
   body.emit(assign(array_ref(adj, 2), neg(
                    add(sub(mul(matrix_elt(m, 0, 0), SubFactor01),
                            mul(matrix_elt(m, 0, 1), SubFactor03)),
                        mul(matrix_elt(m, 0, 3), SubFactor05))),
                    WRITEMASK_Y));
   body.emit(assign(array_ref(adj, 3),
                    add(sub(mul(matrix_elt(m, 0, 0), SubFactor02),
                            mul(matrix_elt(m, 0, 1), SubFactor04)),
                        mul(matrix_elt(m, 0, 2), SubFactor05)),
                    WRITEMASK_Y));

   body.emit(assign(array_ref(adj, 0),
                    add(sub(mul(matrix_elt(m, 0, 1), SubFactor06),
                            mul(matrix_elt(m, 0, 2), SubFactor07)),
                        mul(matrix_elt(m, 0, 3), SubFactor08)),
                    WRITEMASK_Z));
   body.emit(assign(array_ref(adj, 1), neg(
                    add(sub(mul(matrix_elt(m, 0, 0), SubFactor06),
                            mul(matrix_elt(m, 0, 2), SubFactor09)),
                        mul(matrix_elt(m, 0, 3), SubFactor10))),
                    WRITEMASK_Z));
   body.emit(assign(array_ref(adj, 2),
                    add(sub(mul(matrix_elt(m, 0, 0), SubFactor11),
                            mul(matrix_elt(m, 0, 1), SubFactor09)),
                        mul(matrix_elt(m, 0, 3), SubFactor12)),
                    WRITEMASK_Z));
   body.emit(assign(array_ref(adj, 3), neg(
                    add(sub(mul(matrix_elt(m, 0, 0), SubFactor08),
                            mul(matrix_elt(m, 0, 1), SubFactor10)),
                        mul(matrix_elt(m, 0, 2), SubFactor12))),
                    WRITEMASK_Z));

   body.emit(assign(array_ref(adj, 0), neg(
                    add(sub(mul(matrix_elt(m, 0, 1), SubFactor13),
                            mul(matrix_elt(m, 0, 2), SubFactor14)),
                        mul(matrix_elt(m, 0, 3), SubFactor15))),
                    WRITEMASK_W));
   body.emit(assign(array_ref(adj, 1),
                    add(sub(mul(matrix_elt(m, 0, 0), SubFactor13),
                            mul(matrix_elt(m, 0, 2), SubFactor16)),
                        mul(matrix_elt(m, 0, 3), SubFactor17)),
                    WRITEMASK_W));
   body.emit(assign(array_ref(adj, 2), neg(
                    add(sub(mul(matrix_elt(m, 0, 0), SubFactor14),
                            mul(matrix_elt(m, 0, 1), SubFactor16)),
                        mul(matrix_elt(m, 0, 3), SubFactor18))),
                    WRITEMASK_W));
   body.emit(assign(array_ref(adj, 3),
                    add(sub(mul(matrix_elt(m, 0, 0), SubFactor15),
                            mul(matrix_elt(m, 0, 1), SubFactor17)),
                        mul(matrix_elt(m, 0, 2), SubFactor18)),
                    WRITEMASK_W));

   ir_expression *det =
      add(mul(matrix_elt(m, 0, 0), matrix_elt(adj, 0, 0)),
          add(mul(matrix_elt(m, 0, 1), matrix_elt(adj, 1, 0)),
              add(mul(matrix_elt(m, 0, 2), matrix_elt(adj, 2, 0)),
                  mul(matrix_elt(m, 0, 3), matrix_elt(adj, 3, 0)))));

   body.emit(ret(div(adj, det)));

   return sig;
}


ir_function_signature *
builtin_builder::_lessThan(builtin_available_predicate avail,
                           const glsl_type *type)
{
   return binop(avail, ir_binop_less,
                glsl_bvec_type(type->vector_elements), type, type);
}

ir_function_signature *
builtin_builder::_lessThanEqual(builtin_available_predicate avail,
                                const glsl_type *type)
{
   return binop(avail, ir_binop_gequal,
                glsl_bvec_type(type->vector_elements), type, type,
                true);
}

ir_function_signature *
builtin_builder::_greaterThan(builtin_available_predicate avail,
                              const glsl_type *type)
{
   return binop(avail, ir_binop_less,
                glsl_bvec_type(type->vector_elements), type, type,
                true);
}

ir_function_signature *
builtin_builder::_greaterThanEqual(builtin_available_predicate avail,
                                   const glsl_type *type)
{
   return binop(avail, ir_binop_gequal,
                glsl_bvec_type(type->vector_elements), type, type);
}

ir_function_signature *
builtin_builder::_equal(builtin_available_predicate avail,
                        const glsl_type *type)
{
   return binop(avail, ir_binop_equal,
                glsl_bvec_type(type->vector_elements), type, type);
}

ir_function_signature *
builtin_builder::_notEqual(builtin_available_predicate avail,
                           const glsl_type *type)
{
   return binop(avail, ir_binop_nequal,
                glsl_bvec_type(type->vector_elements), type, type);
}

ir_function_signature *
builtin_builder::_any(const glsl_type *type)
{
   ir_variable *v = in_var(type, "v");
   MAKE_SIG(&glsl_type_builtin_bool, always_available, 1, v);

   const unsigned vec_elem = v->type->vector_elements;
   body.emit(ret(expr(ir_binop_any_nequal, v, imm(false, vec_elem))));

   return sig;
}

ir_function_signature *
builtin_builder::_all(const glsl_type *type)
{
   ir_variable *v = in_var(type, "v");
   MAKE_SIG(&glsl_type_builtin_bool, always_available, 1, v);

   const unsigned vec_elem = v->type->vector_elements;
   body.emit(ret(expr(ir_binop_all_equal, v, imm(true, vec_elem))));

   return sig;
}

UNOP(not, ir_unop_logic_not, always_available)

static bool
has_lod(const glsl_type *sampler_type)
{
   assert(glsl_type_is_sampler(sampler_type));

   switch (sampler_type->sampler_dimensionality) {
   case GLSL_SAMPLER_DIM_RECT:
   case GLSL_SAMPLER_DIM_BUF:
   case GLSL_SAMPLER_DIM_MS:
      return false;
   default:
      return true;
   }
}

ir_function_signature *
builtin_builder::_textureSize(builtin_available_predicate avail,
                              const glsl_type *return_type,
                              const glsl_type *sampler_type)
{
   ir_variable *s = in_var(sampler_type, "sampler");
   /* The sampler always exists; add optional lod later. */
   MAKE_SIG(return_type, avail, 1, s);
   sig->return_precision = GLSL_PRECISION_HIGH;

   ir_texture *tex = new(mem_ctx) ir_texture(ir_txs);
   tex->set_sampler(new(mem_ctx) ir_dereference_variable(s), return_type);

   if (has_lod(sampler_type)) {
      ir_variable *lod = in_var(&glsl_type_builtin_int, "lod");
      sig->parameters.push_tail(lod);
      tex->lod_info.lod = var_ref(lod);
   } else {
      tex->lod_info.lod = imm(0u);
   }

   body.emit(ret(tex));

   return sig;
}

ir_function_signature *
builtin_builder::_textureSamples(builtin_available_predicate avail,
                                 const glsl_type *sampler_type)
{
   ir_variable *s = in_var(sampler_type, "sampler");
   MAKE_SIG(&glsl_type_builtin_int, avail, 1, s);

   ir_texture *tex = new(mem_ctx) ir_texture(ir_texture_samples);
   tex->set_sampler(new(mem_ctx) ir_dereference_variable(s), &glsl_type_builtin_int);
   body.emit(ret(tex));

   return sig;
}

ir_function_signature *
builtin_builder::_is_sparse_texels_resident(void)
{
   ir_variable *code = in_var(&glsl_type_builtin_int, "code");
   MAKE_SIG(&glsl_type_builtin_bool, sparse_enabled, 1, code);

   ir_variable *retval = body.make_temp(&glsl_type_builtin_bool, "retval");
   ir_function *f =
      symbols->get_function("__intrinsic_is_sparse_texels_resident");

   body.emit(call(f, retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_is_sparse_texels_resident_intrinsic(void)
{
   ir_variable *code = in_var(&glsl_type_builtin_int, "code");
   MAKE_INTRINSIC(&glsl_type_builtin_bool, ir_intrinsic_is_sparse_texels_resident,
                  sparse_enabled, 1, code);
   return sig;
}

ir_function_signature *
builtin_builder::_texture(ir_texture_opcode opcode,
                          builtin_available_predicate avail,
                          const glsl_type *return_type,
                          const glsl_type *sampler_type,
                          const glsl_type *coord_type,
                          int flags)
{
   ir_variable *s = in_var(sampler_type, "sampler");
   ir_variable *P = in_var(coord_type, "P");
   /* Sparse texture return residency info. */
   const glsl_type *type = flags & TEX_SPARSE ? &glsl_type_builtin_int : return_type;
   /* The sampler and coordinate always exist; add optional parameters later. */
   MAKE_SIG(type, avail, 2, s, P);

   ir_texture *tex = new(mem_ctx) ir_texture(opcode, flags & TEX_SPARSE);
   tex->set_sampler(var_ref(s), return_type);

   const int coord_size = glsl_get_sampler_coordinate_components(sampler_type);

   if (coord_size == coord_type->vector_elements) {
      tex->coordinate = var_ref(P);
   } else {
      /* The incoming coordinate also has the projector or shadow comparator,
       * so we need to swizzle those away.
       */
      tex->coordinate = swizzle_for_size(P, coord_size);
   }

   /* The projector is always in the last component. */
   if (flags & TEX_PROJECT)
      tex->projector = swizzle(P, coord_type->vector_elements - 1, 1);

   if (sampler_type->sampler_shadow) {
      if (opcode == ir_tg4) {
         /* gather has refz as a separate parameter, immediately after the
          * coordinate
          */
         ir_variable *refz = in_var(&glsl_type_builtin_float, "refz");
         sig->parameters.push_tail(refz);
         tex->shadow_comparator = var_ref(refz);
      } else {
         /* The shadow comparator is normally in the Z component, but a few types
          * have sufficiently large coordinates that it's in W.
          */
         tex->shadow_comparator = swizzle(P, MAX2(coord_size, SWIZZLE_Z), 1);
      }
   }

   if (opcode == ir_txl) {
      ir_variable *lod = in_var(&glsl_type_builtin_float, "lod");
      sig->parameters.push_tail(lod);
      tex->lod_info.lod = var_ref(lod);
   } else if (opcode == ir_txd) {
      int grad_size = coord_size - (sampler_type->sampler_array ? 1 : 0);
      ir_variable *dPdx = in_var(glsl_vec_type(grad_size), "dPdx");
      ir_variable *dPdy = in_var(glsl_vec_type(grad_size), "dPdy");
      sig->parameters.push_tail(dPdx);
      sig->parameters.push_tail(dPdy);
      tex->lod_info.grad.dPdx = var_ref(dPdx);
      tex->lod_info.grad.dPdy = var_ref(dPdy);
   }

   if (flags & (TEX_OFFSET | TEX_OFFSET_NONCONST)) {
      int offset_size = coord_size - (sampler_type->sampler_array ? 1 : 0);
      ir_variable *offset =
         new(mem_ctx) ir_variable(glsl_ivec_type(offset_size), "offset",
                                  (flags & TEX_OFFSET) ? ir_var_const_in : ir_var_function_in);
      sig->parameters.push_tail(offset);
      tex->offset = var_ref(offset);
   }

   if (flags & TEX_OFFSET_ARRAY) {
      ir_variable *offsets =
         new(mem_ctx) ir_variable(glsl_array_type(&glsl_type_builtin_ivec2, 4, 0),
                                  "offsets", ir_var_const_in);
      sig->parameters.push_tail(offsets);
      tex->offset = var_ref(offsets);
   }

   if (flags & TEX_CLAMP) {
      ir_variable *clamp = in_var(&glsl_type_builtin_float, "lodClamp");
      sig->parameters.push_tail(clamp);
      tex->clamp = var_ref(clamp);
   }

   ir_variable *texel = NULL;
   if (flags & TEX_SPARSE) {
      texel = out_var(return_type, "texel");
      sig->parameters.push_tail(texel);
   }

   if (opcode == ir_tg4) {
      if (flags & TEX_COMPONENT) {
         ir_variable *component =
            new(mem_ctx) ir_variable(&glsl_type_builtin_int, "comp", ir_var_const_in);
         sig->parameters.push_tail(component);
         tex->lod_info.component = var_ref(component);
      }
      else {
         tex->lod_info.component = imm(0);
      }
   }

   /* The "bias" parameter comes /after/ the "offset" parameter, which is
    * inconsistent with both textureLodOffset and textureGradOffset.
    */
   if (opcode == ir_txb) {
      ir_variable *bias = in_var(&glsl_type_builtin_float, "bias");
      sig->parameters.push_tail(bias);
      tex->lod_info.bias = var_ref(bias);
   }

   if (flags & TEX_SPARSE) {
      ir_variable *r = body.make_temp(tex->type, "result");
      body.emit(assign(r, tex));
      body.emit(assign(texel, record_ref(r, "texel")));
      body.emit(ret(record_ref(r, "code")));
   } else
      body.emit(ret(tex));

   return sig;
}

ir_function_signature *
builtin_builder::_textureCubeArrayShadow(ir_texture_opcode opcode,
                                         builtin_available_predicate avail,
                                         const glsl_type *sampler_type,
                                         int flags)
{
   ir_variable *s = in_var(sampler_type, "sampler");
   ir_variable *P = in_var(&glsl_type_builtin_vec4, "P");
   ir_variable *compare = in_var(&glsl_type_builtin_float, "compare");
   const glsl_type *return_type = &glsl_type_builtin_float;
   bool sparse = flags & TEX_SPARSE;
   bool clamp = flags & TEX_CLAMP;
   /* Sparse texture return residency info. */
   const glsl_type *type = sparse ? &glsl_type_builtin_int : return_type;
   MAKE_SIG(type, avail, 3, s, P, compare);

   ir_texture *tex = new(mem_ctx) ir_texture(opcode, sparse);
   tex->set_sampler(var_ref(s), return_type);

   tex->coordinate = var_ref(P);
   tex->shadow_comparator = var_ref(compare);

   if (opcode == ir_txl) {
      ir_variable *lod = in_var(&glsl_type_builtin_float, "lod");
      sig->parameters.push_tail(lod);
      tex->lod_info.lod = var_ref(lod);
   }

   if (clamp) {
      ir_variable *lod_clamp = in_var(&glsl_type_builtin_float, "lodClamp");
      sig->parameters.push_tail(lod_clamp);
      tex->clamp = var_ref(lod_clamp);
   }

   ir_variable *texel = NULL;
   if (sparse) {
      texel = out_var(return_type, "texel");
      sig->parameters.push_tail(texel);
   }

   if (opcode == ir_txb) {
      ir_variable *bias = in_var(&glsl_type_builtin_float, "bias");
      sig->parameters.push_tail(bias);
      tex->lod_info.bias = var_ref(bias);
   }

   if (sparse) {
      ir_variable *r = body.make_temp(tex->type, "result");
      body.emit(assign(r, tex));
      body.emit(assign(texel, record_ref(r, "texel")));
      body.emit(ret(record_ref(r, "code")));
   } else
      body.emit(ret(tex));

   return sig;
}

ir_function_signature *
builtin_builder::_texelFetch(builtin_available_predicate avail,
                             const glsl_type *return_type,
                             const glsl_type *sampler_type,
                             const glsl_type *coord_type,
                             const glsl_type *offset_type,
                             bool sparse)
{
   ir_variable *s = in_var(sampler_type, "sampler");
   ir_variable *P = in_var(coord_type, "P");
   /* Sparse texture return residency info. */
   const glsl_type *type = sparse ? &glsl_type_builtin_int : return_type;
   /* The sampler and coordinate always exist; add optional parameters later. */
   MAKE_SIG(type, avail, 2, s, P);

   ir_texture *tex = new(mem_ctx) ir_texture(ir_txf, sparse);
   tex->coordinate = var_ref(P);
   tex->set_sampler(var_ref(s), return_type);

   if (sampler_type->sampler_dimensionality == GLSL_SAMPLER_DIM_MS) {
      ir_variable *sample = in_var(&glsl_type_builtin_int, "sample");
      sig->parameters.push_tail(sample);
      tex->lod_info.sample_index = var_ref(sample);
      tex->op = ir_txf_ms;
   } else if (has_lod(sampler_type)) {
      ir_variable *lod = in_var(&glsl_type_builtin_int, "lod");
      sig->parameters.push_tail(lod);
      tex->lod_info.lod = var_ref(lod);
   } else {
      tex->lod_info.lod = imm(0u);
   }

   if (offset_type != NULL) {
      ir_variable *offset =
         new(mem_ctx) ir_variable(offset_type, "offset", ir_var_const_in);
      sig->parameters.push_tail(offset);
      tex->offset = var_ref(offset);
   }

   if (sparse) {
      ir_variable *texel = out_var(return_type, "texel");
      sig->parameters.push_tail(texel);

      ir_variable *r = body.make_temp(tex->type, "result");
      body.emit(assign(r, tex));
      body.emit(assign(texel, record_ref(r, "texel")));
      body.emit(ret(record_ref(r, "code")));
   } else
      body.emit(ret(tex));

   return sig;
}

ir_function_signature *
builtin_builder::_EmitVertex()
{
   MAKE_SIG(&glsl_type_builtin_void, gs_only, 0);

   ir_rvalue *stream = new(mem_ctx) ir_constant(0, 1);
   body.emit(new(mem_ctx) ir_emit_vertex(stream));

   return sig;
}

ir_function_signature *
builtin_builder::_EmitStreamVertex(builtin_available_predicate avail,
                                   const glsl_type *stream_type)
{
   /* Section 8.12 (Geometry Shader Functions) of the GLSL 4.0 spec says:
    *
    *     "Emit the current values of output variables to the current output
    *     primitive on stream stream. The argument to stream must be a constant
    *     integral expression."
    */
   ir_variable *stream =
      new(mem_ctx) ir_variable(stream_type, "stream", ir_var_const_in);

   MAKE_SIG(&glsl_type_builtin_void, avail, 1, stream);

   body.emit(new(mem_ctx) ir_emit_vertex(var_ref(stream)));

   return sig;
}

ir_function_signature *
builtin_builder::_EndPrimitive()
{
   MAKE_SIG(&glsl_type_builtin_void, gs_only, 0);

   ir_rvalue *stream = new(mem_ctx) ir_constant(0, 1);
   body.emit(new(mem_ctx) ir_end_primitive(stream));

   return sig;
}

ir_function_signature *
builtin_builder::_EndStreamPrimitive(builtin_available_predicate avail,
                                     const glsl_type *stream_type)
{
   /* Section 8.12 (Geometry Shader Functions) of the GLSL 4.0 spec says:
    *
    *     "Completes the current output primitive on stream stream and starts
    *     a new one. The argument to stream must be a constant integral
    *     expression."
    */
   ir_variable *stream =
      new(mem_ctx) ir_variable(stream_type, "stream", ir_var_const_in);

   MAKE_SIG(&glsl_type_builtin_void, avail, 1, stream);

   body.emit(new(mem_ctx) ir_end_primitive(var_ref(stream)));

   return sig;
}

ir_function_signature *
builtin_builder::_barrier()
{
   MAKE_SIG(&glsl_type_builtin_void, barrier_supported, 0);

   body.emit(new(mem_ctx) ir_barrier());
   return sig;
}

ir_function_signature *
builtin_builder::_textureQueryLod(builtin_available_predicate avail,
                                  const glsl_type *sampler_type,
                                  const glsl_type *coord_type)
{
   ir_variable *s = in_var(sampler_type, "sampler");
   ir_variable *coord = in_var(coord_type, "coord");
   /* The sampler and coordinate always exist; add optional parameters later. */
   MAKE_SIG(&glsl_type_builtin_vec2, avail, 2, s, coord);

   ir_texture *tex = new(mem_ctx) ir_texture(ir_lod);
   tex->coordinate = var_ref(coord);
   tex->set_sampler(var_ref(s), &glsl_type_builtin_vec2);

   body.emit(ret(tex));

   return sig;
}

ir_function_signature *
builtin_builder::_textureQueryLevels(builtin_available_predicate avail,
                                     const glsl_type *sampler_type)
{
   ir_variable *s = in_var(sampler_type, "sampler");
   const glsl_type *return_type = &glsl_type_builtin_int;
   MAKE_SIG(return_type, avail, 1, s);

   ir_texture *tex = new(mem_ctx) ir_texture(ir_query_levels);
   tex->set_sampler(var_ref(s), return_type);

   body.emit(ret(tex));

   return sig;
}

ir_function_signature *
builtin_builder::_textureSamplesIdentical(builtin_available_predicate avail,
                                          const glsl_type *sampler_type,
                                          const glsl_type *coord_type)
{
   ir_variable *s = in_var(sampler_type, "sampler");
   ir_variable *P = in_var(coord_type, "P");
   const glsl_type *return_type = &glsl_type_builtin_bool;
   MAKE_SIG(return_type, avail, 2, s, P);

   ir_texture *tex = new(mem_ctx) ir_texture(ir_samples_identical);
   tex->coordinate = var_ref(P);
   tex->set_sampler(var_ref(s), return_type);

   body.emit(ret(tex));

   return sig;
}

UNOPA(dFdx, ir_unop_dFdx)
UNOPA(dFdxCoarse, ir_unop_dFdx_coarse)
UNOPA(dFdxFine, ir_unop_dFdx_fine)
UNOPA(dFdy, ir_unop_dFdy)
UNOPA(dFdyCoarse, ir_unop_dFdy_coarse)
UNOPA(dFdyFine, ir_unop_dFdy_fine)

ir_function_signature *
builtin_builder::_fwidth(builtin_available_predicate avail,
                         const glsl_type *type)
{
   ir_variable *p = in_var(type, "p");
   MAKE_SIG(type, avail, 1, p);

   body.emit(ret(add(abs(expr(ir_unop_dFdx, p)), abs(expr(ir_unop_dFdy, p)))));

   return sig;
}

ir_function_signature *
builtin_builder::_fwidthCoarse(builtin_available_predicate avail,
                               const glsl_type *type)
{
   ir_variable *p = in_var(type, "p");
   MAKE_SIG(type, avail, 1, p);

   body.emit(ret(add(abs(expr(ir_unop_dFdx_coarse, p)),
                     abs(expr(ir_unop_dFdy_coarse, p)))));

   return sig;
}

ir_function_signature *
builtin_builder::_fwidthFine(builtin_available_predicate avail,
                             const glsl_type *type)
{
   ir_variable *p = in_var(type, "p");
   MAKE_SIG(type, avail, 1, p);

   body.emit(ret(add(abs(expr(ir_unop_dFdx_fine, p)),
                     abs(expr(ir_unop_dFdy_fine, p)))));

   return sig;
}

ir_function_signature *
builtin_builder::_noise1(const glsl_type *type)
{
   /* From the GLSL 4.60 specification:
    *
    *    "The noise functions noise1, noise2, noise3, and noise4 have been
    *    deprecated starting with version 4.4 of GLSL. When not generating
    *    SPIR-V they are defined to return the value 0.0 or a vector whose
    *    components are all 0.0. When generating SPIR-V the noise functions
    *    are not declared and may not be used."
    *
    * In earlier versions of the GLSL specification attempt to define some
    * sort of statistical noise function.  However, the function's
    * characteristics have always been such that always returning 0 is
    * valid and Mesa has always returned 0 for noise on most drivers.
    */
   ir_variable *p = in_var(type, "p");
   MAKE_SIG(&glsl_type_builtin_float, v110, 1, p);
   body.emit(ret(imm(&glsl_type_builtin_float, ir_constant_data())));
   return sig;
}

ir_function_signature *
builtin_builder::_noise2(const glsl_type *type)
{
   /* See builtin_builder::_noise1 */
   ir_variable *p = in_var(type, "p");
   MAKE_SIG(&glsl_type_builtin_vec2, v110, 1, p);
   body.emit(ret(imm(&glsl_type_builtin_vec2, ir_constant_data())));
   return sig;
}

ir_function_signature *
builtin_builder::_noise3(const glsl_type *type)
{
   /* See builtin_builder::_noise1 */
   ir_variable *p = in_var(type, "p");
   MAKE_SIG(&glsl_type_builtin_vec3, v110, 1, p);
   body.emit(ret(imm(&glsl_type_builtin_vec3, ir_constant_data())));
   return sig;
}

ir_function_signature *
builtin_builder::_noise4(const glsl_type *type)
{
   /* See builtin_builder::_noise1 */
   ir_variable *p = in_var(type, "p");
   MAKE_SIG(&glsl_type_builtin_vec4, v110, 1, p);
   body.emit(ret(imm(&glsl_type_builtin_vec4, ir_constant_data())));
   return sig;
}

ir_function_signature *
builtin_builder::_bitfieldExtract(const glsl_type *type)
{
   bool is_uint = type->base_type == GLSL_TYPE_UINT;
   ir_variable *value  = in_var(type, "value");
   ir_variable *offset = in_var(&glsl_type_builtin_int, "offset");
   ir_variable *bits   = in_var(&glsl_type_builtin_int, "bits");
   MAKE_SIG(type, gpu_shader5_or_es31_or_integer_functions, 3, value, offset,
            bits);

   operand cast_offset = is_uint ? i2u(offset) : operand(offset);
   operand cast_bits = is_uint ? i2u(bits) : operand(bits);

   body.emit(ret(expr(ir_triop_bitfield_extract, value,
      swizzle(cast_offset, SWIZZLE_XXXX, type->vector_elements),
      swizzle(cast_bits, SWIZZLE_XXXX, type->vector_elements))));

   return sig;
}

ir_function_signature *
builtin_builder::_bitfieldInsert(const glsl_type *type)
{
   bool is_uint = type->base_type == GLSL_TYPE_UINT;
   ir_variable *base   = in_var(type, "base");
   ir_variable *insert = in_var(type, "insert");
   ir_variable *offset = in_var(&glsl_type_builtin_int, "offset");
   ir_variable *bits   = in_var(&glsl_type_builtin_int, "bits");
   MAKE_SIG(type, gpu_shader5_or_es31_or_integer_functions, 4, base, insert,
            offset, bits);

   operand cast_offset = is_uint ? i2u(offset) : operand(offset);
   operand cast_bits = is_uint ? i2u(bits) : operand(bits);

   body.emit(ret(bitfield_insert(base, insert,
      swizzle(cast_offset, SWIZZLE_XXXX, type->vector_elements),
      swizzle(cast_bits, SWIZZLE_XXXX, type->vector_elements))));

   return sig;
}

ir_function_signature *
builtin_builder::_bitfieldReverse(const glsl_type *type)
{
   ir_variable *x = in_highp_var(type, "x");
   MAKE_SIG(type, gpu_shader5_or_es31_or_integer_functions, 1, x);
   sig->return_precision = GLSL_PRECISION_HIGH;
   body.emit(ret(expr(ir_unop_bitfield_reverse, x)));
   return sig;
}

ir_function_signature *
builtin_builder::_bitCount(const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   MAKE_SIG(glsl_ivec_type(type->vector_elements), gpu_shader5_or_es31_or_integer_functions, 1, x);
   sig->return_precision = GLSL_PRECISION_LOW;
   body.emit(ret(expr(ir_unop_bit_count, x)));
   return sig;
}

ir_function_signature *
builtin_builder::_findLSB(const glsl_type *type)
{
   ir_variable *x = in_highp_var(type, "x");
   MAKE_SIG(glsl_ivec_type(type->vector_elements), gpu_shader5_or_es31_or_integer_functions, 1, x);
   sig->return_precision = GLSL_PRECISION_LOW;
   body.emit(ret(expr(ir_unop_find_lsb, x)));
   return sig;
}

ir_function_signature *
builtin_builder::_findMSB(const glsl_type *type)
{
   ir_variable *x = in_highp_var(type, "x");
   MAKE_SIG(glsl_ivec_type(type->vector_elements), gpu_shader5_or_es31_or_integer_functions, 1, x);
   sig->return_precision = GLSL_PRECISION_LOW;
   body.emit(ret(expr(ir_unop_find_msb, x)));
   return sig;
}

ir_function_signature *
builtin_builder::_countLeadingZeros(builtin_available_predicate avail,
                                    const glsl_type *type)
{
   return unop(avail, ir_unop_clz,
               glsl_uvec_type(type->vector_elements), type);
}

ir_function_signature *
builtin_builder::_countTrailingZeros(builtin_available_predicate avail,
                                     const glsl_type *type)
{
   ir_variable *a = in_var(type, "a");
   MAKE_SIG(glsl_uvec_type(type->vector_elements), avail, 1, a);

   body.emit(ret(ir_builder::min2(
                    ir_builder::i2u(ir_builder::expr(ir_unop_find_lsb, a)),
                    imm(32u))));

   return sig;
}

ir_function_signature *
builtin_builder::_fma(builtin_available_predicate avail, const glsl_type *type)
{
   ir_variable *a = in_var(type, "a");
   ir_variable *b = in_var(type, "b");
   ir_variable *c = in_var(type, "c");
   MAKE_SIG(type, avail, 3, a, b, c);

   body.emit(ret(ir_builder::fma(a, b, c)));

   return sig;
}

ir_function_signature *
builtin_builder::_ldexp(const glsl_type *x_type, const glsl_type *exp_type)
{
   ir_variable *x = in_highp_var(x_type, "x");
   ir_variable *y = in_highp_var(exp_type, "y");
   builtin_available_predicate avail = glsl_type_is_double(x_type) ? fp64 :
      (glsl_type_is_float_16(x_type) ?
          gpu_shader_half_float : gpu_shader5_or_es31_or_integer_functions);

   MAKE_SIG(x_type, avail, 2, x, y);
   sig->return_precision = GLSL_PRECISION_HIGH;
   body.emit(ret(expr(ir_binop_ldexp, x, y)));
   return sig;
}

ir_function_signature *
builtin_builder::_frexp(const glsl_type *x_type, const glsl_type *exp_type)
{
   ir_variable *x = in_highp_var(x_type, "x");
   ir_variable *exponent = out_var(exp_type, "exp");
   builtin_available_predicate avail = glsl_type_is_double(x_type) ? fp64 :
      (glsl_type_is_float_16(x_type) ?
          gpu_shader_half_float : gpu_shader5_or_es31_or_integer_functions);
   MAKE_SIG(x_type, avail, 2, x, exponent);
   sig->return_precision = GLSL_PRECISION_HIGH;

   body.emit(assign(exponent, expr(ir_unop_frexp_exp, x)));

   body.emit(ret(expr(ir_unop_frexp_sig, x)));
   return sig;
}

ir_function_signature *
builtin_builder::_uaddCarry(const glsl_type *type)
{
   ir_variable *x = in_highp_var(type, "x");
   ir_variable *y = in_highp_var(type, "y");
   ir_variable *carry = out_lowp_var(type, "carry");
   MAKE_SIG(type, gpu_shader5_or_es31_or_integer_functions, 3, x, y, carry);
   sig->return_precision = GLSL_PRECISION_HIGH;

   body.emit(assign(carry, ir_builder::carry(x, y)));
   body.emit(ret(add(x, y)));

   return sig;
}

ir_function_signature *
builtin_builder::_addSaturate(builtin_available_predicate avail,
                              const glsl_type *type)
{
   return binop(avail, ir_binop_add_sat, type, type, type);
}

ir_function_signature *
builtin_builder::_usubBorrow(const glsl_type *type)
{
   ir_variable *x = in_highp_var(type, "x");
   ir_variable *y = in_highp_var(type, "y");
   ir_variable *borrow = out_lowp_var(type, "borrow");
   MAKE_SIG(type, gpu_shader5_or_es31_or_integer_functions, 3, x, y, borrow);
   sig->return_precision = GLSL_PRECISION_HIGH;

   body.emit(assign(borrow, ir_builder::borrow(x, y)));
   body.emit(ret(sub(x, y)));

   return sig;
}

ir_function_signature *
builtin_builder::_subtractSaturate(builtin_available_predicate avail,
                                   const glsl_type *type)
{
   return binop(avail, ir_binop_sub_sat, type, type, type);
}

ir_function_signature *
builtin_builder::_absoluteDifference(builtin_available_predicate avail,
                                     const glsl_type *type)
{
   /* absoluteDifference returns an unsigned type that has the same number of
    * bits and number of vector elements as the type of the operands.
    */
   return binop(avail, ir_binop_abs_sub,
                glsl_simple_type(glsl_unsigned_base_type_of(type->base_type),
                                 type->vector_elements, 1),
                type, type);
}

ir_function_signature *
builtin_builder::_average(builtin_available_predicate avail,
                          const glsl_type *type)
{
   return binop(avail, ir_binop_avg, type, type, type);
}

ir_function_signature *
builtin_builder::_averageRounded(builtin_available_predicate avail,
                                 const glsl_type *type)
{
   return binop(avail, ir_binop_avg_round, type, type, type);
}

/**
 * For both imulExtended() and umulExtended() built-ins.
 */
ir_function_signature *
builtin_builder::_mulExtended(const glsl_type *type)
{
   const glsl_type *mul_type, *unpack_type;
   ir_expression_operation unpack_op;

   if (type->base_type == GLSL_TYPE_INT) {
      unpack_op = ir_unop_unpack_int_2x32;
      mul_type = glsl_simple_type(GLSL_TYPE_INT64, type->vector_elements, 1);
      unpack_type = &glsl_type_builtin_ivec2;
   } else {
      unpack_op = ir_unop_unpack_uint_2x32;
      mul_type = glsl_simple_type(GLSL_TYPE_UINT64, type->vector_elements, 1);
      unpack_type = &glsl_type_builtin_uvec2;
   }

   ir_variable *x = in_highp_var(type, "x");
   ir_variable *y = in_highp_var(type, "y");
   ir_variable *msb = out_highp_var(type, "msb");
   ir_variable *lsb = out_highp_var(type, "lsb");
   MAKE_SIG(&glsl_type_builtin_void, gpu_shader5_or_es31_or_integer_functions, 4, x, y, msb, lsb);

   ir_variable *unpack_val = body.make_temp(unpack_type, "_unpack_val");

   ir_expression *mul_res = new(mem_ctx) ir_expression(ir_binop_mul, mul_type,
                                                       new(mem_ctx)ir_dereference_variable(x),
                                                       new(mem_ctx)ir_dereference_variable(y));

   if (type->vector_elements == 1) {
      body.emit(assign(unpack_val, expr(unpack_op, mul_res)));
      body.emit(assign(msb, swizzle_y(unpack_val)));
      body.emit(assign(lsb, swizzle_x(unpack_val)));
   } else {
      for (int i = 0; i < type->vector_elements; i++) {
         body.emit(assign(unpack_val, expr(unpack_op, swizzle(mul_res, i, 1))));
         body.emit(assign(array_ref(msb, i), swizzle_y(unpack_val)));
         body.emit(assign(array_ref(lsb, i), swizzle_x(unpack_val)));
      }
   }

   return sig;
}

ir_function_signature *
builtin_builder::_multiply32x16(builtin_available_predicate avail,
                                const glsl_type *type)
{
   return binop(avail, ir_binop_mul_32x16, type, type, type);
}

ir_function_signature *
builtin_builder::_interpolateAtCentroid(builtin_available_predicate avail,
                                        const glsl_type *type)
{
   ir_variable *interpolant = in_var(type, "interpolant");
   interpolant->data.must_be_shader_input = 1;
   MAKE_SIG(type, fs_interpolate_at, 1, interpolant);

   body.emit(ret(interpolate_at_centroid(interpolant)));

   return sig;
}

ir_function_signature *
builtin_builder::_interpolateAtOffset(builtin_available_predicate avail,
                                      const glsl_type *type)
{
   ir_variable *interpolant = in_var(type, "interpolant");
   interpolant->data.must_be_shader_input = 1;
   ir_variable *offset = in_var(glsl_type_is_float_16(type) ? &glsl_type_builtin_f16vec2 : &glsl_type_builtin_vec2, "offset");
   MAKE_SIG(type, fs_interpolate_at, 2, interpolant, offset);

   body.emit(ret(interpolate_at_offset(interpolant, offset)));

   return sig;
}

ir_function_signature *
builtin_builder::_interpolateAtSample(builtin_available_predicate avail,
                                      const glsl_type *type)
{
   ir_variable *interpolant = in_var(type, "interpolant");
   interpolant->data.must_be_shader_input = 1;
   ir_variable *sample_num = in_var(&glsl_type_builtin_int, "sample_num");
   MAKE_SIG(type, fs_interpolate_at, 2, interpolant, sample_num);

   body.emit(ret(interpolate_at_sample(interpolant, sample_num)));

   return sig;
}

/* The highp isn't specified in the built-in function descriptions, but in the
 * atomic counter description: "The default precision of all atomic types is
 * highp. It is an error to declare an atomic type with a different precision or
 * to specify the default precision for an atomic type to be lowp or mediump."
 */
ir_function_signature *
builtin_builder::_atomic_counter_intrinsic(builtin_available_predicate avail,
                                           enum ir_intrinsic_id id)
{
   ir_variable *counter = in_highp_var(&glsl_type_builtin_atomic_uint, "counter");
   MAKE_INTRINSIC(&glsl_type_builtin_uint, id, avail, 1, counter);
   return sig;
}

ir_function_signature *
builtin_builder::_atomic_counter_intrinsic1(builtin_available_predicate avail,
                                            enum ir_intrinsic_id id)
{
   ir_variable *counter = in_highp_var(&glsl_type_builtin_atomic_uint, "counter");
   ir_variable *data = in_var(&glsl_type_builtin_uint, "data");
   MAKE_INTRINSIC(&glsl_type_builtin_uint, id, avail, 2, counter, data);
   return sig;
}

ir_function_signature *
builtin_builder::_atomic_counter_intrinsic2(builtin_available_predicate avail,
                                            enum ir_intrinsic_id id)
{
   ir_variable *counter = in_highp_var(&glsl_type_builtin_atomic_uint, "counter");
   ir_variable *compare = in_var(&glsl_type_builtin_uint, "compare");
   ir_variable *data = in_var(&glsl_type_builtin_uint, "data");
   MAKE_INTRINSIC(&glsl_type_builtin_uint, id, avail, 3, counter, compare, data);
   return sig;
}

ir_function_signature *
builtin_builder::_atomic_intrinsic2(builtin_available_predicate avail,
                                    const glsl_type *type,
                                    enum ir_intrinsic_id id)
{
   ir_variable *atomic = in_var(type, "atomic");
   ir_variable *data = in_var(type, "data");
   MAKE_INTRINSIC(type, id, avail, 2, atomic, data);
   return sig;
}

ir_function_signature *
builtin_builder::_atomic_intrinsic3(builtin_available_predicate avail,
                                    const glsl_type *type,
                                    enum ir_intrinsic_id id)
{
   ir_variable *atomic = in_var(type, "atomic");
   ir_variable *data1 = in_var(type, "data1");
   ir_variable *data2 = in_var(type, "data2");
   MAKE_INTRINSIC(type, id, avail, 3, atomic, data1, data2);
   return sig;
}

ir_function_signature *
builtin_builder::_atomic_counter_op(const char *intrinsic,
                                    builtin_available_predicate avail)
{
   ir_variable *counter = in_highp_var(&glsl_type_builtin_atomic_uint, "atomic_counter");
   MAKE_SIG(&glsl_type_builtin_uint, avail, 1, counter);

   ir_variable *retval = body.make_temp(&glsl_type_builtin_uint, "atomic_retval");
   body.emit(call(symbols->get_function(intrinsic), retval,
                  sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_atomic_counter_op1(const char *intrinsic,
                                     builtin_available_predicate avail)
{
   ir_variable *counter = in_highp_var(&glsl_type_builtin_atomic_uint, "atomic_counter");
   ir_variable *data = in_var(&glsl_type_builtin_uint, "data");
   MAKE_SIG(&glsl_type_builtin_uint, avail, 2, counter, data);

   ir_variable *retval = body.make_temp(&glsl_type_builtin_uint, "atomic_retval");

   /* Instead of generating an __intrinsic_atomic_sub, generate an
    * __intrinsic_atomic_add with the data parameter negated.
    */
   if (strcmp("__intrinsic_atomic_sub", intrinsic) == 0) {
      ir_variable *const neg_data =
         body.make_temp(&glsl_type_builtin_uint, "neg_data");

      body.emit(assign(neg_data, neg(data)));

      ir_exec_list parameters;

      parameters.push_tail(new(mem_ctx) ir_dereference_variable(counter));
      parameters.push_tail(new(mem_ctx) ir_dereference_variable(neg_data));

      ir_function *const func =
         symbols->get_function("__intrinsic_atomic_add");
      ir_instruction *const c = call(func, retval, parameters);

      assert(c != NULL);
      assert(parameters.is_empty());

      body.emit(c);
   } else {
      body.emit(call(symbols->get_function(intrinsic), retval,
                     sig->parameters));
   }

   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_atomic_counter_op2(const char *intrinsic,
                                    builtin_available_predicate avail)
{
   ir_variable *counter = in_highp_var(&glsl_type_builtin_atomic_uint, "atomic_counter");
   ir_variable *compare = in_var(&glsl_type_builtin_uint, "compare");
   ir_variable *data = in_var(&glsl_type_builtin_uint, "data");
   MAKE_SIG(&glsl_type_builtin_uint, avail, 3, counter, compare, data);

   ir_variable *retval = body.make_temp(&glsl_type_builtin_uint, "atomic_retval");
   body.emit(call(symbols->get_function(intrinsic), retval,
                  sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_atomic_op2(const char *intrinsic,
                             builtin_available_predicate avail,
                             const glsl_type *type)
{
   ir_variable *atomic = in_var(type, "atomic_var");
   ir_variable *data = in_var(type, "atomic_data");
   MAKE_SIG(type, avail, 2, atomic, data);

   atomic->data.implicit_conversion_prohibited = true;

   ir_variable *retval = body.make_temp(type, "atomic_retval");
   body.emit(call(symbols->get_function(intrinsic), retval,
                  sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_atomic_op3(const char *intrinsic,
                             builtin_available_predicate avail,
                             const glsl_type *type)
{
   ir_variable *atomic = in_var(type, "atomic_var");
   ir_variable *data1 = in_var(type, "atomic_data1");
   ir_variable *data2 = in_var(type, "atomic_data2");
   MAKE_SIG(type, avail, 3, atomic, data1, data2);

   atomic->data.implicit_conversion_prohibited = true;

   ir_variable *retval = body.make_temp(type, "atomic_retval");
   body.emit(call(symbols->get_function(intrinsic), retval,
                  sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_min3(builtin_available_predicate avail,
                       const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   ir_variable *y = in_var(type, "y");
   ir_variable *z = in_var(type, "z");
   MAKE_SIG(type, avail, 3, x, y, z);

   ir_expression *min3 = min2(x, min2(y,z));
   body.emit(ret(min3));

   return sig;
}

ir_function_signature *
builtin_builder::_max3(builtin_available_predicate avail,
                       const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   ir_variable *y = in_var(type, "y");
   ir_variable *z = in_var(type, "z");
   MAKE_SIG(type, avail, 3, x, y, z);

   ir_expression *max3 = max2(x, max2(y,z));
   body.emit(ret(max3));

   return sig;
}

ir_function_signature *
builtin_builder::_mid3(builtin_available_predicate avail,
                       const glsl_type *type)
{
   ir_variable *x = in_var(type, "x");
   ir_variable *y = in_var(type, "y");
   ir_variable *z = in_var(type, "z");
   MAKE_SIG(type, avail, 3, x, y, z);

   ir_expression *mid3 = max2(min2(x, y), max2(min2(x, z), min2(y, z)));
   body.emit(ret(mid3));

   return sig;
}

static builtin_available_predicate
get_image_available_predicate(const glsl_type *type, unsigned flags)
{
   if ((flags & IMAGE_FUNCTION_AVAIL_ATOMIC_EXCHANGE) &&
       type->sampled_type == GLSL_TYPE_FLOAT)
      return shader_image_atomic_exchange_float;

   if ((flags & IMAGE_FUNCTION_AVAIL_ATOMIC_ADD) &&
       type->sampled_type == GLSL_TYPE_FLOAT)
      return shader_image_atomic_add_float;

   else if (flags & (IMAGE_FUNCTION_AVAIL_ATOMIC_EXCHANGE |
                     IMAGE_FUNCTION_AVAIL_ATOMIC_ADD |
                     IMAGE_FUNCTION_AVAIL_ATOMIC))
      return shader_image_atomic;

   else if (flags & IMAGE_FUNCTION_EXT_ONLY)
      return shader_image_load_store_ext;

   else if (flags & IMAGE_FUNCTION_SPARSE)
      return shader_image_load_store_and_sparse;

   else
      return shader_image_load_store;
}

ir_function_signature *
builtin_builder::_image_prototype(const glsl_type *image_type,
                                  unsigned num_arguments,
                                  unsigned flags)
{
   const glsl_type *data_type = glsl_simple_type(
      image_type->sampled_type,
      (flags & IMAGE_FUNCTION_HAS_VECTOR_DATA_TYPE ? 4 : 1),
      1);

   const glsl_type *ret_type;
   if (flags & IMAGE_FUNCTION_RETURNS_VOID)
      ret_type = &glsl_type_builtin_void;
   else if (flags & IMAGE_FUNCTION_SPARSE) {
      if (flags & IMAGE_FUNCTION_EMIT_STUB)
         ret_type = &glsl_type_builtin_int;
      else {
         /* code holds residency info */
         glsl_struct_field fields[2] = {
            glsl_struct_field(&glsl_type_builtin_int, "code"),
            glsl_struct_field(data_type, "texel"),
         };
         ret_type = glsl_struct_type(fields, 2, "struct", false /* packed */);
      }
   } else
      ret_type = data_type;

   /* Addressing arguments that are always present. */
   ir_variable *image = in_var(image_type, "image");
   ir_variable *coord = in_var(
      glsl_ivec_type(glsl_get_sampler_coordinate_components(image_type)), "coord");

   ir_function_signature *sig = new_sig(
      ret_type, get_image_available_predicate(image_type, flags),
      2, image, coord);

   /* Sample index for multisample images. */
   if (image_type->sampler_dimensionality == GLSL_SAMPLER_DIM_MS)
      sig->parameters.push_tail(in_var(&glsl_type_builtin_int, "sample"));

   /* Data arguments. */
   for (unsigned i = 0; i < num_arguments; ++i) {
      char *arg_name = ralloc_asprintf(NULL, "arg%d", i);
      sig->parameters.push_tail(in_var(data_type, arg_name));
      ralloc_free(arg_name);
   }

   /* Set the maximal set of qualifiers allowed for this image
    * built-in.  Function calls with arguments having fewer
    * qualifiers than present in the prototype are allowed by the
    * spec, but not with more, i.e. this will make the compiler
    * accept everything that needs to be accepted, and reject cases
    * like loads from write-only or stores to read-only images.
    */
   image->data.memory_read_only = (flags & IMAGE_FUNCTION_READ_ONLY) != 0;
   image->data.memory_write_only = (flags & IMAGE_FUNCTION_WRITE_ONLY) != 0;
   image->data.memory_coherent = true;
   image->data.memory_volatile = true;
   image->data.memory_restrict = true;

   return sig;
}

ir_function_signature *
builtin_builder::_image_size_prototype(const glsl_type *image_type,
                                       unsigned /* num_arguments */,
                                       unsigned /* flags */)
{
   const glsl_type *ret_type;
   unsigned num_components = glsl_get_sampler_coordinate_components(image_type);

   /* From the ARB_shader_image_size extension:
    * "Cube images return the dimensions of one face."
    */
   if (image_type->sampler_dimensionality == GLSL_SAMPLER_DIM_CUBE &&
       !image_type->sampler_array) {
      num_components = 2;
   }

   /* FIXME: Add the highp precision qualifier for GLES 3.10 when it is
    * supported by mesa.
    */
   ret_type = glsl_simple_type(GLSL_TYPE_INT, num_components, 1);

   ir_variable *image = in_var(image_type, "image");
   ir_function_signature *sig = new_sig(ret_type, shader_image_size, 1, image);

   /* Set the maximal set of qualifiers allowed for this image
    * built-in.  Function calls with arguments having fewer
    * qualifiers than present in the prototype are allowed by the
    * spec, but not with more, i.e. this will make the compiler
    * accept everything that needs to be accepted, and reject cases
    * like loads from write-only or stores to read-only images.
    */
   image->data.memory_read_only = true;
   image->data.memory_write_only = true;
   image->data.memory_coherent = true;
   image->data.memory_volatile = true;
   image->data.memory_restrict = true;

   return sig;
}

ir_function_signature *
builtin_builder::_image_samples_prototype(const glsl_type *image_type,
                                          unsigned /* num_arguments */,
                                          unsigned /* flags */)
{
   ir_variable *image = in_var(image_type, "image");
   ir_function_signature *sig =
      new_sig(&glsl_type_builtin_int, shader_samples, 1, image);

   /* Set the maximal set of qualifiers allowed for this image
    * built-in.  Function calls with arguments having fewer
    * qualifiers than present in the prototype are allowed by the
    * spec, but not with more, i.e. this will make the compiler
    * accept everything that needs to be accepted, and reject cases
    * like loads from write-only or stores to read-only images.
    */
   image->data.memory_read_only = true;
   image->data.memory_write_only = true;
   image->data.memory_coherent = true;
   image->data.memory_volatile = true;
   image->data.memory_restrict = true;

   return sig;
}

ir_function_signature *
builtin_builder::_image(image_prototype_ctr prototype,
                        const glsl_type *image_type,
                        const char *intrinsic_name,
                        unsigned num_arguments,
                        unsigned flags,
                        enum ir_intrinsic_id id)
{
   ir_function_signature *sig = (this->*prototype)(image_type,
                                                   num_arguments, flags);

   if (flags & IMAGE_FUNCTION_EMIT_STUB) {
      ir_factory body(&sig->body, mem_ctx);
      ir_function *f = symbols->get_function(intrinsic_name);

      if (flags & IMAGE_FUNCTION_RETURNS_VOID) {
         body.emit(call(f, NULL, sig->parameters));
      } else if (flags & IMAGE_FUNCTION_SPARSE) {
         ir_function_signature *intr_sig =
            f->exact_matching_signature(NULL, &sig->parameters);
         assert(intr_sig);

         ir_variable *ret_val = body.make_temp(intr_sig->return_type, "_ret_val");
         ir_dereference_record *texel_field = record_ref(ret_val, "texel");
         ir_variable *texel = out_var(texel_field->type, "texel");

         /* Add texel param to builtin function after call intrinsic function
          * because they have different prototype:
          *   struct {int code; gvec4 texel;} __intrinsic_image_sparse_load(in)
          *   int sparseImageLoad(in, out texel)
          */
         body.emit(call(f, ret_val, sig->parameters));
         sig->parameters.push_tail(texel);

         body.emit(assign(texel, texel_field));
         body.emit(ret(record_ref(ret_val, "code")));
      } else {
         ir_variable *ret_val =
            body.make_temp(sig->return_type, "_ret_val");
         /* all non-void image functions return highp, so make our temporary and return
          * value in the signature highp.
          */
         ret_val->data.precision = GLSL_PRECISION_HIGH;
         body.emit(call(f, ret_val, sig->parameters));
         body.emit(ret(ret_val));
      }

      sig->is_defined = true;

   } else {
      sig->intrinsic_id = id;
   }
   sig->return_precision = GLSL_PRECISION_HIGH;

   return sig;
}

ir_function_signature *
builtin_builder::_memory_barrier_intrinsic(builtin_available_predicate avail,
                                           enum ir_intrinsic_id id)
{
   MAKE_INTRINSIC(&glsl_type_builtin_void, id, avail, 0);
   return sig;
}

ir_function_signature *
builtin_builder::_memory_barrier(const char *intrinsic_name,
                                 builtin_available_predicate avail)
{
   MAKE_SIG(&glsl_type_builtin_void, avail, 0);
   body.emit(call(symbols->get_function(intrinsic_name),
                  NULL, sig->parameters));
   return sig;
}

ir_function_signature *
builtin_builder::_ballot_intrinsic(const glsl_type *type)
{
   ir_variable *value = in_var(&glsl_type_builtin_bool, "value");
   MAKE_INTRINSIC(type, ir_intrinsic_ballot, shader_ballot, 1, value);
   return sig;
}

ir_function_signature *
builtin_builder::_ballot(const glsl_type *type, builtin_available_predicate avail)
{
   ir_variable *value = in_var(&glsl_type_builtin_bool, "value");

   MAKE_SIG(type, avail, 1, value);
   ir_variable *retval = body.make_temp(type, "retval");

   if (type == &glsl_type_builtin_uint64_t) {
      body.emit(call(symbols->get_function("__intrinsic_ballot_uint64"),
                     retval, sig->parameters));
   } else {
      assert(type == &glsl_type_builtin_uvec4);
      body.emit(call(symbols->get_function("__intrinsic_ballot_uvec4"),
                     retval, sig->parameters));
   }
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_inverse_ballot_intrinsic()
{
   ir_variable *value = in_var(&glsl_type_builtin_uvec4, "value");
   MAKE_INTRINSIC(&glsl_type_builtin_bool, ir_intrinsic_inverse_ballot, ballot_khr, 1, value);
   return sig;
}

ir_function_signature *
builtin_builder::_inverse_ballot()
{
   ir_variable *value = in_var(&glsl_type_builtin_uvec4, "value");

   MAKE_SIG(&glsl_type_builtin_bool, ballot_khr, 1, value);
   ir_variable *retval = body.make_temp(&glsl_type_builtin_bool, "retval");

   body.emit(call(symbols->get_function("__intrinsic_inverse_ballot"),
                  retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_ballot_bit_extract_intrinsic()
{
   ir_variable *value = in_var(&glsl_type_builtin_uvec4, "value");
   ir_variable *index = in_var(&glsl_type_builtin_uint, "index");

   MAKE_INTRINSIC(&glsl_type_builtin_bool, ir_intrinsic_ballot_bit_extract, ballot_khr,
                  2, value, index);
   return sig;
}

ir_function_signature *
builtin_builder::_ballot_bit_extract()
{
   ir_variable *value = in_var(&glsl_type_builtin_uvec4, "value");
   ir_variable *index = in_var(&glsl_type_builtin_uint, "index");

   MAKE_SIG(&glsl_type_builtin_bool, ballot_khr, 2, value, index);
   ir_variable *retval = body.make_temp(&glsl_type_builtin_bool, "retval");

   body.emit(call(symbols->get_function("__intrinsic_ballot_bit_extract"),
                  retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_ballot_bit_intrinsic(enum ir_intrinsic_id id)
{
   ir_variable *value = in_var(&glsl_type_builtin_uvec4, "value");
   MAKE_INTRINSIC(&glsl_type_builtin_uint, id, ballot_khr, 1, value);
   return sig;
}

ir_function_signature *
builtin_builder::_ballot_bit(const char *intrinsic_name)
{
   ir_variable *value = in_var(&glsl_type_builtin_uvec4, "value");

   MAKE_SIG(&glsl_type_builtin_uint, ballot_khr, 1, value);
   ir_variable *retval = body.make_temp(&glsl_type_builtin_uint, "retval");

   body.emit(call(symbols->get_function(intrinsic_name), retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_read_first_invocation_intrinsic(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   MAKE_INTRINSIC(type, ir_intrinsic_read_first_invocation,
                  glsl_type_is_double(type) ? shader_ballot_and_fp64 : shader_ballot,
                  1, value);
   return sig;
}

ir_function_signature *
builtin_builder::_read_first_invocation(const glsl_type *type, builtin_available_predicate avail)
{
   ir_variable *value = in_var(type, "value");

   MAKE_SIG(type, avail, 1, value);
   ir_variable *retval = body.make_temp(type, "retval");

   body.emit(call(symbols->get_function("__intrinsic_read_first_invocation"),
                  retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_read_invocation_intrinsic(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *invocation = in_var(&glsl_type_builtin_uint, "invocation");
   MAKE_INTRINSIC(type, ir_intrinsic_read_invocation,
                  glsl_type_is_double(type) ? shader_ballot_and_fp64 : shader_ballot,
                  2, value, invocation);
   return sig;
}

ir_function_signature *
builtin_builder::_read_invocation(const glsl_type *type, builtin_available_predicate avail)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *invocation = in_var(&glsl_type_builtin_uint, "invocation");

   MAKE_SIG(type, avail, 2, value, invocation);
   ir_variable *retval = body.make_temp(type, "retval");

   body.emit(call(symbols->get_function("__intrinsic_read_invocation"),
                  retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_invocation_interlock_intrinsic(builtin_available_predicate avail,
                                                 enum ir_intrinsic_id id)
{
   MAKE_INTRINSIC(&glsl_type_builtin_void, id, avail, 0);
   return sig;
}

ir_function_signature *
builtin_builder::_invocation_interlock(const char *intrinsic_name,
                                       builtin_available_predicate avail)
{
   MAKE_SIG(&glsl_type_builtin_void, avail, 0);
   body.emit(call(symbols->get_function(intrinsic_name),
                  NULL, sig->parameters));
   return sig;
}

ir_function_signature *
builtin_builder::_shader_clock_intrinsic(builtin_available_predicate avail,
                                         const glsl_type *type)
{
   MAKE_INTRINSIC(type, ir_intrinsic_shader_clock, avail, 0);
   return sig;
}

ir_function_signature *
builtin_builder::_shader_clock(builtin_available_predicate avail,
                               const glsl_type *type)
{
   MAKE_SIG(type, avail, 0);

   ir_variable *retval = body.make_temp(&glsl_type_builtin_uvec2, "clock_retval");

   body.emit(call(symbols->get_function("__intrinsic_shader_clock"),
                  retval, sig->parameters));

   if (type == &glsl_type_builtin_uint64_t) {
      body.emit(ret(expr(ir_unop_pack_uint_2x32, retval)));
   } else {
      body.emit(ret(retval));
   }

   return sig;
}

ir_function_signature *
builtin_builder::_shader_clock_realtime_intrinsic(builtin_available_predicate avail,
                                                  const glsl_type *type)
{
   MAKE_INTRINSIC(type, ir_intrinsic_shader_clock_realtime, avail, 0);
   return sig;
}

ir_function_signature *
builtin_builder::_shader_clock_realtime(builtin_available_predicate avail,
                                        const glsl_type *type)
{
   MAKE_SIG(type, avail, 0);

   ir_variable *retval = body.make_temp(&glsl_type_builtin_uvec2, "clock_retval");

   body.emit(call(symbols->get_function("__intrinsic_shader_clock_realtime"),
                  retval, sig->parameters));

   if (type == &glsl_type_builtin_uint64_t) {
      body.emit(ret(expr(ir_unop_pack_uint_2x32, retval)));
   } else {
      body.emit(ret(retval));
   }

   return sig;
}

ir_function_signature *
builtin_builder::_vote_intrinsic(const glsl_type *type,
                                 builtin_available_predicate avail,
                                 enum ir_intrinsic_id id)
{
   ir_variable *value = in_var(type, "value");
   MAKE_INTRINSIC(&glsl_type_builtin_bool, id, avail, 1, value);
   return sig;
}

ir_function_signature *
builtin_builder::_vote(const glsl_type *type,
                       builtin_available_predicate avail,
                       const char *intrinsic_name)
{
   ir_variable *value = in_var(type, "value");

   MAKE_SIG(&glsl_type_builtin_bool, avail, 1, value);

   ir_variable *retval = body.make_temp(&glsl_type_builtin_bool, "retval");

   body.emit(call(symbols->get_function(intrinsic_name),
                  retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_helper_invocation_intrinsic()
{
   MAKE_INTRINSIC(&glsl_type_builtin_bool, ir_intrinsic_helper_invocation,
                  demote_to_helper_invocation, 0);
   return sig;
}

ir_function_signature *
builtin_builder::_helper_invocation()
{
   MAKE_SIG(&glsl_type_builtin_bool, demote_to_helper_invocation, 0);

   ir_variable *retval = body.make_temp(&glsl_type_builtin_bool, "retval");

   body.emit(call(symbols->get_function("__intrinsic_helper_invocation"),
                  retval, sig->parameters));
   body.emit(ret(retval));

   return sig;
}

ir_function_signature *
builtin_builder::_subgroup_barrier_intrinsic(enum ir_intrinsic_id id,
                                             builtin_available_predicate avail)
{
   MAKE_INTRINSIC(&glsl_type_builtin_void, id, avail, 0);
   return sig;
}

ir_function_signature *
builtin_builder::_subgroup_barrier(const char *intrinsic_name,
                                   builtin_available_predicate avail)
{
   MAKE_SIG(&glsl_type_builtin_void, avail, 0);
   body.emit(call(symbols->get_function(intrinsic_name), NULL, sig->parameters));
   return sig;
}

ir_function_signature *
builtin_builder::_elect_intrinsic()
{
   MAKE_INTRINSIC(&glsl_type_builtin_bool, ir_intrinsic_elect, subgroup_basic, 0);
   return sig;
}

ir_function_signature *
builtin_builder::_elect()
{
   MAKE_SIG(&glsl_type_builtin_bool, subgroup_basic, 0);

   ir_variable *retval = body.make_temp(&glsl_type_builtin_bool, "retval");

   body.emit(call(symbols->get_function("__intrinsic_elect"), retval, sig->parameters));
   body.emit(ret(retval));

   return sig;
}

ir_function_signature *
builtin_builder::_shuffle_intrinsic(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *id = in_var(&glsl_type_builtin_uint, "id");
   MAKE_INTRINSIC(type, ir_intrinsic_shuffle,
                  glsl_type_is_double(type) ? subgroup_shuffle_and_fp64 : subgroup_shuffle,
                  2, value, id);
   return sig;
}

ir_function_signature *
builtin_builder::_shuffle(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *id = in_var(&glsl_type_builtin_uint, "id");

   MAKE_SIG(type, glsl_type_is_double(type) ? subgroup_shuffle_and_fp64 : subgroup_shuffle,
            2, value, id);

   ir_variable *retval = body.make_temp(type, "retval");

   body.emit(call(symbols->get_function("__intrinsic_shuffle"), retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_shuffle_xor_intrinsic(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *mask = in_var(&glsl_type_builtin_uint, "mask");
   MAKE_INTRINSIC(type, ir_intrinsic_shuffle_xor,
                  glsl_type_is_double(type) ? subgroup_shuffle_and_fp64 : subgroup_shuffle,
                  2, value, mask);
   return sig;
}

ir_function_signature *
builtin_builder::_shuffle_xor(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *mask = in_var(&glsl_type_builtin_uint, "mask");

   MAKE_SIG(type, glsl_type_is_double(type) ? subgroup_shuffle_and_fp64 : subgroup_shuffle,
            2, value, mask);

   ir_variable *retval = body.make_temp(type, "retval");

   body.emit(call(symbols->get_function("__intrinsic_shuffle_xor"),
                  retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_shuffle_up_intrinsic(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *delta = in_var(&glsl_type_builtin_uint, "delta");
   MAKE_INTRINSIC(type, ir_intrinsic_shuffle_up,
                  glsl_type_is_double(type) ? subgroup_shuffle_relative_and_fp64 : subgroup_shuffle_relative,
                  2, value, delta);
   return sig;
}

ir_function_signature *
builtin_builder::_shuffle_up(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *delta = in_var(&glsl_type_builtin_uint, "delta");

   MAKE_SIG(type, glsl_type_is_double(type) ? subgroup_shuffle_relative_and_fp64 : subgroup_shuffle_relative,
            2, value, delta);
   ir_variable *retval = body.make_temp(type, "retval");

   body.emit(call(symbols->get_function("__intrinsic_shuffle_up"),
                  retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_shuffle_down_intrinsic(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *delta = in_var(&glsl_type_builtin_uint, "delta");
   MAKE_INTRINSIC(type, ir_intrinsic_shuffle_down,
                  glsl_type_is_double(type) ? subgroup_shuffle_relative_and_fp64 : subgroup_shuffle_relative,
                  2, value, delta);
   return sig;
}

ir_function_signature *
builtin_builder::_shuffle_down(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *delta = in_var(&glsl_type_builtin_uint, "delta");

   MAKE_SIG(type, glsl_type_is_double(type) ? subgroup_shuffle_relative_and_fp64 : subgroup_shuffle_relative,
            2, value, delta);
   ir_variable *retval = body.make_temp(type, "retval");

   body.emit(call(symbols->get_function("__intrinsic_shuffle_down"),
                  retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_subgroup_arithmetic_intrinsic(const glsl_type *type, enum ir_intrinsic_id id)
{
   ir_variable *value = in_var(type, "value");
   MAKE_INTRINSIC(type, id,
                  glsl_type_is_double(type) ? subgroup_arithmetic_and_fp64 : subgroup_arithmetic,
                  1, value);
   return sig;
}

ir_function_signature *
builtin_builder::_subgroup_arithmetic(const glsl_type *type, const char *intrinsic_name)
{
   ir_variable *value = in_var(type, "value");
   MAKE_SIG(type, glsl_type_is_double(type) ? subgroup_arithmetic_and_fp64 : subgroup_arithmetic,
            1, value);

   ir_variable *retval = body.make_temp(type, "retval");
   body.emit(call(symbols->get_function(intrinsic_name), retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_subgroup_clustered_intrinsic(const glsl_type *type, enum ir_intrinsic_id id)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *size =
      new(mem_ctx) ir_variable(&glsl_type_builtin_uint, "clusterSize", ir_var_const_in);

   MAKE_INTRINSIC(type, id,
                  glsl_type_is_double(type) ? subgroup_clustered_and_fp64 : subgroup_clustered,
                  2, value, size);
   return sig;
}

ir_function_signature *
builtin_builder::_subgroup_clustered(const glsl_type *type, const char *intrinsic_name)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *size =
      new(mem_ctx) ir_variable(&glsl_type_builtin_uint, "clusterSize", ir_var_const_in);

   MAKE_SIG(type, glsl_type_is_double(type) ? subgroup_clustered_and_fp64 : subgroup_clustered,
            2, value, size);

   ir_variable *retval = body.make_temp(type, "retval");
   body.emit(call(symbols->get_function(intrinsic_name), retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_quad_broadcast_intrinsic(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *id = in_var(&glsl_type_builtin_uint, "id");
   MAKE_INTRINSIC(type, ir_intrinsic_quad_broadcast,
                  glsl_type_is_double(type) ? subgroup_quad_and_fp64 : subgroup_quad,
                  2, value, id);
   return sig;
}

ir_function_signature *
builtin_builder::_quad_broadcast(const glsl_type *type)
{
   ir_variable *value = in_var(type, "value");
   ir_variable *id = in_var(&glsl_type_builtin_uint, "id");

   MAKE_SIG(type, glsl_type_is_double(type) ? subgroup_quad_and_fp64 : subgroup_quad,
            2, value, id);
   ir_variable *retval = body.make_temp(type, "retval");

   body.emit(call(symbols->get_function("__intrinsic_quad_broadcast"),
                  retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

ir_function_signature *
builtin_builder::_quad_swap_intrinsic(const glsl_type *type, enum ir_intrinsic_id id)
{
   ir_variable *value = in_var(type, "value");
   MAKE_INTRINSIC(type, id,
                  glsl_type_is_double(type) ? subgroup_quad_and_fp64 : subgroup_quad,
                  1, value);
   return sig;
}

ir_function_signature *
builtin_builder::_quad_swap(const glsl_type *type, const char *intrinsic_name)
{
   ir_variable *value = in_var(type, "value");
   MAKE_SIG(type, glsl_type_is_double(type) ? subgroup_quad_and_fp64 : subgroup_quad,
            1, value);

   ir_variable *retval = body.make_temp(type, "retval");
   body.emit(call(symbols->get_function(intrinsic_name), retval, sig->parameters));
   body.emit(ret(retval));
   return sig;
}

/** @} */

/******************************************************************************/

/* The singleton instance of builtin_builder. */
static builtin_builder builtins;
static uint32_t builtin_users = 0;

/**
 * External API (exposing the built-in module to the rest of the compiler):
 *  @{
 */
extern "C" void
_mesa_glsl_builtin_functions_init_or_ref()
{
   simple_mtx_lock(&builtins_lock);
   if (builtin_users++ == 0)
      builtins.initialize();
   simple_mtx_unlock(&builtins_lock);
}

extern "C" void
_mesa_glsl_builtin_functions_decref()
{
   simple_mtx_lock(&builtins_lock);
   assert(builtin_users != 0);
   if (--builtin_users == 0)
      builtins.release();
   simple_mtx_unlock(&builtins_lock);
}

ir_function_signature *
_mesa_glsl_find_builtin_function(_mesa_glsl_parse_state *state,
                                 const char *name, ir_exec_list *actual_parameters)
{
   ir_function_signature *s;
   simple_mtx_lock(&builtins_lock);
   s = builtins.find(state, name, actual_parameters);
   simple_mtx_unlock(&builtins_lock);

   return s;
}

bool
_mesa_glsl_has_builtin_function(_mesa_glsl_parse_state *state, const char *name)
{
   ir_function *f;
   bool ret = false;
   simple_mtx_lock(&builtins_lock);
   f = builtins.symbols->get_function(name);
   if (f != NULL) {
      ir_foreach_in_list(ir_function_signature, sig, &f->signatures) {
         if (sig->is_builtin_available(state)) {
            ret = true;
            break;
         }
      }
   }
   simple_mtx_unlock(&builtins_lock);

   return ret;
}

struct glsl_symbol_table *
_mesa_glsl_get_builtin_function_symbols()
{
   return builtins.symbols;
}


/**
 * Get the function signature for main from a shader
 */
ir_function_signature *
_mesa_get_main_function_signature(glsl_symbol_table *symbol_table)
{
   ir_function *const f = symbol_table->get_function("main");
   if (f != NULL) {
      ir_exec_list void_parameters;

      /* Look for the 'void main()' signature and ensure that it's defined.
       * This keeps the linker from accidentally pick a shader that just
       * contains a prototype for main.
       *
       * We don't have to check for multiple definitions of main (in multiple
       * shaders) because that would have already been caught above.
       */
      ir_function_signature *sig =
         f->matching_signature(NULL, &void_parameters, false, false, false);
      if ((sig != NULL) && sig->is_defined) {
         return sig;
      }
   }

   return NULL;
}

/** @} */

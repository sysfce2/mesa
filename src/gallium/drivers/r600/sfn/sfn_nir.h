/* -*- mesa-c++  -*-
 * Copyright 2019 Collabora LTD
 * Author: Gert Wollny <gert.wollny@collabora.com>
 * SPDX-License-Identifier: MIT
 */

#ifndef SFN_NIR_H
#define SFN_NIR_H

#include "gallium/include/pipe/p_state.h"

#include "amd_family.h"
#include "nir.h"
#include "nir_builder.h"

#ifdef __cplusplus
#include "sfn_shader.h"

#include <vector>

namespace r600 {

class NirLowerInstruction {
public:
   NirLowerInstruction();

   bool run(nir_shader *shader);

private:
   static bool filter_instr(const nir_instr *instr, const void *data);
   static nir_def *lower_instr(nir_builder *b, nir_instr *instr, void *data);

   void set_builder(nir_builder *_b) { b = _b; }

   virtual bool filter(const nir_instr *instr) const = 0;
   virtual nir_def *lower(nir_instr *instr) = 0;

protected:
   nir_builder *b;
};

bool
r600_lower_ubo_to_align16(nir_shader *shader);

bool
r600_nir_split_64bit_io(nir_shader *sh);

bool
r600_nir_64_to_vec2(nir_shader *sh);

bool
r600_merge_vec2_stores(nir_shader *shader);

bool
r600_split_64bit_uniforms_and_ubo(nir_shader *sh);
bool
r600_lower_64bit_to_vec2(nir_shader *sh);
bool
r600_split_64bit_alu_and_phi(nir_shader *sh);
bool
r600_lower_clipvertex_to_clipdist(nir_shader *sh);

class AssemblyFromShader {
public:
   virtual ~AssemblyFromShader();
   bool lower(const Shader& s);

private:
   virtual bool do_lower(const Shader& s) = 0;
};

} // namespace r600

static inline nir_def *
r600_imm_ivec3(nir_builder *build, int x, int y, int z)
{
   nir_const_value v[3] = {
      nir_const_value_for_int(x, 32),
      nir_const_value_for_int(y, 32),
      nir_const_value_for_int(z, 32),
   };

   return nir_build_imm(build, 3, 32, v);
}

bool
r600_lower_tess_io(nir_shader *shader, enum mesa_prim prim_type);
bool
r600_append_tcs_TF_emission(nir_shader *shader, enum mesa_prim prim_type);

bool
r600_legalize_image_load_store(nir_shader *shader);

void
r600_finalize_and_optimize_shader(r600::Shader *shader);
r600::Shader *
r600_schedule_shader(r600::Shader *shader);

#else
#include "gallium/drivers/r600/r600_shader.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

bool
r600_vectorize_vs_inputs(nir_shader *shader);

bool
r600_lower_to_scalar_instr_filter(const nir_instr *instr, const void *);

void
r600_lower_and_optimize_nir(nir_shader *sh,
                            const union r600_shader_key *key,
                            enum amd_gfx_level gfx_level,
                            struct pipe_stream_output_info *so_info);

void
r600_finalize_nir_common(nir_shader *nir, enum amd_gfx_level gfx_level);

#ifdef __cplusplus
}
#endif

#endif // SFN_NIR_H

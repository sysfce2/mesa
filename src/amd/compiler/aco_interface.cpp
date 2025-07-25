/*
 * Copyright © 2018 Google
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_interface.h"

#include "aco_ir.h"

#include "util/memstream.h"

#include "ac_gpu_info.h"
#include "nir.h"
#include <array>
#include <iostream>
#include <vector>

using namespace aco;

namespace {

static void
validate(Program* program)
{
   if (!(debug_flags & DEBUG_VALIDATE_IR))
      return;

   ASSERTED bool is_valid = validate_ir(program);
   assert(is_valid);
}

static std::string
get_disasm_string(Program* program, std::vector<uint32_t>& code, unsigned exec_size)
{
   std::string disasm;

   char* data = NULL;
   size_t disasm_size = 0;
   struct u_memstream mem;
   if (u_memstream_open(&mem, &data, &disasm_size)) {
      FILE* const memf = u_memstream_get(&mem);
      if (check_print_asm_support(program)) {
         print_asm(program, code, exec_size / 4u, memf);
      } else {
         fprintf(memf, "Shader disassembly is not supported in the current configuration"
#if !AMD_LLVM_AVAILABLE
                       " (LLVM not available)"
#endif
                       ", falling back to print_program.\n\n");
         aco_print_program(program, memf);
      }
      fputc(0, memf);
      u_memstream_close(&mem);
      disasm = std::string(data, data + disasm_size);
      free(data);
   }

   return disasm;
}

static std::string
aco_postprocess_shader(const struct aco_compiler_options* options,
                       std::unique_ptr<Program>& program)
{
   std::string llvm_ir;

   if (options->dump_preoptir)
      aco_print_program(program.get(), stderr);

   ASSERTED bool is_valid = validate_cfg(program.get());
   assert(is_valid);

   dominator_tree(program.get());
   if (program->should_repair_ssa)
      repair_ssa(program.get());
   lower_phis(program.get());

   if (program->gfx_level <= GFX7)
      lower_subdword(program.get());

   validate(program.get());

   /* Optimization */
   if (!options->optimisations_disabled) {
      if (!(debug_flags & DEBUG_NO_VN))
         value_numbering(program.get());
      if (!(debug_flags & DEBUG_NO_OPT))
         optimize(program.get());

      /* Optimization may move SGPR uses down, requiring further SSA repair. */
      if (program->should_repair_ssa && repair_ssa(program.get()))
         lower_phis(program.get());
   }

   /* cleanup and exec mask handling */
   setup_reduce_temp(program.get());
   insert_exec_mask(program.get());
   validate(program.get());

   /* spilling and scheduling */
   live_var_analysis(program.get());
   if (program->collect_statistics)
      collect_presched_stats(program.get());
   spill(program.get());

   if (options->record_ir) {
      char* data = NULL;
      size_t size = 0;
      u_memstream mem;
      if (u_memstream_open(&mem, &data, &size)) {
         FILE* const memf = u_memstream_get(&mem);
         aco_print_program(program.get(), memf);
         fputc(0, memf);
         u_memstream_close(&mem);
      }

      llvm_ir = std::string(data, data + size);
      free(data);
   }

   if ((debug_flags & DEBUG_LIVE_INFO) && options->dump_ir)
      aco_print_program(program.get(), stderr, print_live_vars | print_kill);

   if (!options->optimisations_disabled && !(debug_flags & DEBUG_NO_SCHED))
      schedule_program(program.get());
   validate(program.get());

   /* Register Allocation */
   register_allocation(program.get());

   if (validate_ra(program.get())) {
      aco_print_program(program.get(), stderr);
      abort();
   } else if (options->dump_ir) {
      aco_print_program(program.get(), stderr);
   }

   validate(program.get());

   /* Optimization */
   if (!options->optimisations_disabled && !(debug_flags & DEBUG_NO_OPT)) {
      optimize_postRA(program.get());
      validate(program.get());
   }

   /* Lower to HW Instructions */
   ssa_elimination(program.get());
   lower_to_hw_instr(program.get());
   lower_branches(program.get());
   validate(program.get());

   if (!options->optimisations_disabled && !(debug_flags & DEBUG_NO_SCHED_VOPD))
      schedule_vopd(program.get());

   /* Schedule hardware instructions for ILP */
   if (!options->optimisations_disabled && !(debug_flags & DEBUG_NO_SCHED_ILP))
      schedule_ilp(program.get());

   if (program->needs_fp_mode_insertion)
      insert_fp_mode(program.get());

   insert_waitcnt(program.get());
   insert_NOPs(program.get());
   if (program->gfx_level >= GFX11)
      insert_delay_alu(program.get());

   if (program->gfx_level >= GFX10)
      form_hard_clauses(program.get());

   if (program->gfx_level >= GFX11)
      combine_delay_alu(program.get());

   if (program->collect_statistics || (debug_flags & DEBUG_PERF_INFO))
      collect_preasm_stats(program.get());

   return llvm_ir;
}

typedef void(select_shader_part_callback)(Program* program, void* pinfo, ac_shader_config* config,
                                          const struct aco_compiler_options* options,
                                          const struct aco_shader_info* info,
                                          const struct ac_shader_args* args);

static void
aco_compile_shader_part(const struct aco_compiler_options* options,
                        const struct aco_shader_info* info, const struct ac_shader_args* args,
                        select_shader_part_callback select_shader_part, void* pinfo,
                        aco_shader_part_callback* build_binary, void** binary,
                        bool is_prolog = false)
{
   init();

   ac_shader_config config = {0};
   std::unique_ptr<Program> program{new Program};

   program->collect_statistics = options->record_stats;
   memset(&program->statistics, 0, sizeof(program->statistics));

   program->debug.func = options->debug.func;
   program->debug.private_data = options->debug.private_data;

   program->is_prolog = is_prolog;
   program->is_epilog = !is_prolog;

   /* Instruction selection */
   select_shader_part(program.get(), pinfo, &config, options, info, args);

   aco_postprocess_shader(options, program);

   /* assembly */
   std::vector<uint32_t> code;
   bool append_endpgm = !(options->is_opengl && is_prolog);
   unsigned exec_size = emit_program(program.get(), code, NULL, append_endpgm);

   std::string disasm;
   if (options->record_asm)
      disasm = get_disasm_string(program.get(), code, exec_size);

   (*build_binary)(binary, config.num_sgprs, config.num_vgprs, code.data(), code.size(),
                   disasm.data(), disasm.size());
}

} /* end namespace */

void
aco_compile_shader(const struct aco_compiler_options* options, const struct aco_shader_info* info,
                   unsigned shader_count, struct nir_shader* const* shaders,
                   const struct ac_shader_args* args, aco_callback* build_binary, void** binary)
{
   init();

   ac_shader_config config = {0};
   std::unique_ptr<Program> program{new Program};

   program->collect_statistics = options->record_stats;
   memset(&program->statistics, 0, sizeof(program->statistics));

   program->debug.func = options->debug.func;
   program->debug.private_data = options->debug.private_data;

   /* Instruction Selection */
   select_program(program.get(), shader_count, shaders, &config, options, info, args);

   std::string llvm_ir = aco_postprocess_shader(options, program);

   /* assembly */
   std::vector<uint32_t> code;
   std::vector<struct aco_symbol> symbols;
   /* OpenGL combine multi shader parts into one continous code block,
    * so only last part need the s_endpgm instruction.
    */
   bool append_endpgm = !(options->is_opengl && info->ps.has_epilog);
   unsigned exec_size = emit_program(program.get(), code, &symbols, append_endpgm);

   if (program->collect_statistics)
      collect_postasm_stats(program.get(), code);

   std::string disasm;
   if (options->record_asm)
      disasm = get_disasm_string(program.get(), code, exec_size);

   (*build_binary)(binary, &config, llvm_ir.c_str(), llvm_ir.size(), disasm.c_str(), disasm.size(),
                   &program->statistics, exec_size, code.data(), code.size(), symbols.data(),
                   symbols.size(), program->debug_info.data(), program->debug_info.size());
}

void
aco_compile_rt_prolog(const struct aco_compiler_options* options,
                      const struct aco_shader_info* info, const struct ac_shader_args* in_args,
                      const struct ac_shader_args* out_args, aco_callback* build_prolog,
                      void** binary)
{
   init();

   /* create program */
   ac_shader_config config = {0};
   std::unique_ptr<Program> program{new Program};
   program->collect_statistics = false;
   program->debug.func = NULL;
   program->debug.private_data = NULL;

   select_rt_prolog(program.get(), &config, options, info, in_args, out_args);
   validate(program.get());
   insert_waitcnt(program.get());
   insert_NOPs(program.get());
   if (program->gfx_level >= GFX11)
      insert_delay_alu(program.get());
   if (program->gfx_level >= GFX10)
      form_hard_clauses(program.get());
   if (program->gfx_level >= GFX11)
      combine_delay_alu(program.get());

   if (options->dump_ir)
      aco_print_program(program.get(), stderr);

   /* assembly */
   std::vector<uint32_t> code;
   code.reserve(align(program->blocks[0].instructions.size() * 2, 16));
   unsigned exec_size = emit_program(program.get(), code);

   std::string disasm;
   if (options->record_asm)
      disasm = get_disasm_string(program.get(), code, exec_size);

   (*build_prolog)(binary, &config, NULL, 0, disasm.c_str(), disasm.size(), NULL, exec_size,
                   code.data(), code.size(), NULL, 0, NULL, 0);
}

void
aco_compile_vs_prolog(const struct aco_compiler_options* options,
                      const struct aco_shader_info* info, const struct aco_vs_prolog_info* pinfo,
                      const struct ac_shader_args* args, aco_shader_part_callback* build_prolog,
                      void** binary)
{
   init();

   /* create program */
   ac_shader_config config = {0};
   std::unique_ptr<Program> program{new Program};
   program->collect_statistics = false;
   program->debug.func = NULL;
   program->debug.private_data = NULL;

   /* create IR */
   select_vs_prolog(program.get(), pinfo, &config, options, info, args);
   validate(program.get());
   insert_NOPs(program.get());
   if (program->gfx_level >= GFX10)
      form_hard_clauses(program.get());

   if (options->dump_ir)
      aco_print_program(program.get(), stderr);

   /* assembly */
   std::vector<uint32_t> code;
   code.reserve(align(program->blocks[0].instructions.size() * 2, 16));
   unsigned exec_size = emit_program(program.get(), code);

   std::string disasm;
   if (options->record_asm)
      disasm = get_disasm_string(program.get(), code, exec_size);

   (*build_prolog)(binary, config.num_sgprs, config.num_vgprs, code.data(), code.size(),
                   disasm.data(), disasm.size());
}

void
aco_compile_ps_epilog(const struct aco_compiler_options* options,
                      const struct aco_shader_info* info, const struct aco_ps_epilog_info* pinfo,
                      const struct ac_shader_args* args, aco_shader_part_callback* build_epilog,
                      void** binary)
{
   aco_compile_shader_part(options, info, args, select_ps_epilog, (void*)pinfo, build_epilog,
                           binary);
}

void
aco_compile_ps_prolog(const struct aco_compiler_options* options,
                      const struct aco_shader_info* info, const struct aco_ps_prolog_info* pinfo,
                      const struct ac_shader_args* args, aco_shader_part_callback* build_prolog,
                      void** binary)
{
   aco_compile_shader_part(options, info, args, select_ps_prolog, (void*)pinfo, build_prolog,
                           binary, true);
}

void
aco_compile_trap_handler(const struct aco_compiler_options* options,
                         const struct aco_shader_info* info, const struct ac_shader_args* args,
                         aco_callback* build_binary, void** binary)
{
   init();

   ac_shader_config config = {0};
   std::unique_ptr<Program> program{new Program};
   program->collect_statistics = false;
   program->debug.func = NULL;
   program->debug.private_data = NULL;

   select_trap_handler_shader(program.get(), &config, options, info, args);

   if (options->dump_preoptir)
      aco_print_program(program.get(), stderr);
   validate(program.get());

   insert_exec_mask(program.get());
   validate(program.get());

   lower_to_hw_instr(program.get());
   lower_branches(program.get());
   validate(program.get());

   insert_waitcnt(program.get());
   insert_NOPs(program.get());

   /* assembly */
   std::vector<uint32_t> code;
   code.reserve(align(program->blocks[0].instructions.size() * 2, 16));
   unsigned exec_size = emit_program(program.get(), code);

   std::string disasm;
   if (options->record_asm)
      disasm = get_disasm_string(program.get(), code, exec_size);

   (*build_binary)(binary, &config, NULL, 0, disasm.c_str(), disasm.size(), NULL, exec_size,
                   code.data(), code.size(), NULL, 0, NULL, 0);
}

uint64_t
aco_get_codegen_flags()
{
   init();
   /* Exclude flags which don't affect code generation. */
   uint64_t exclude = DEBUG_VALIDATE_IR | DEBUG_VALIDATE_RA | DEBUG_PERF_INFO | DEBUG_LIVE_INFO |
                      DEBUG_NO_VALIDATE | DEBUG_VALIDATE_LIVE_VARS | DEBUG_VALIDATE_OPT;
   return debug_flags & ~exclude;
}

bool
aco_is_gpu_supported(const struct radeon_info* info)
{
   switch (info->gfx_level) {
   case GFX6:
   case GFX7:
   case GFX8:
      return true;
   case GFX9:
      return info->has_graphics; /* no CDNA support */
   case GFX10:
   case GFX10_3:
   case GFX11:
   case GFX11_5:
   case GFX12:
      return true;
   default:
      return false;
   }
}

bool
aco_nir_op_supports_packed_math_16bit(const nir_alu_instr* alu)
{
   switch (alu->op) {
   case nir_op_f2f16: {
      nir_shader* shader = nir_cf_node_get_function(&alu->instr.block->cf_node)->function->shader;
      unsigned execution_mode = shader->info.float_controls_execution_mode;
      return (shader->options->force_f2f16_rtz && !nir_is_rounding_mode_rtne(execution_mode, 16)) ||
             nir_is_rounding_mode_rtz(execution_mode, 16);
   }
   case nir_op_fadd:
   case nir_op_fsub:
   case nir_op_fmul:
   case nir_op_ffma:
   case nir_op_fdiv:
   case nir_op_flrp:
   case nir_op_fabs:
   case nir_op_fneg:
   case nir_op_fsat:
   case nir_op_fmin:
   case nir_op_fmax:
   case nir_op_f2f16_rtz:
   case nir_op_iabs:
   case nir_op_iadd:
   case nir_op_iadd_sat:
   case nir_op_uadd_sat:
   case nir_op_isub:
   case nir_op_isub_sat:
   case nir_op_usub_sat:
   case nir_op_ineg:
   case nir_op_imul:
   case nir_op_imin:
   case nir_op_imax:
   case nir_op_umin:
   case nir_op_umax:
   case nir_op_extract_u8:
   case nir_op_extract_i8:
   case nir_op_ishl:
   case nir_op_ishr:
   case nir_op_ushr: return true;
   case nir_op_u2u16:
   case nir_op_i2i16: return alu->src[0].src.ssa->bit_size == 8;
   default: return false;
   }
}

void
aco_print_asm(const struct radeon_info *info, unsigned wave_size,
              uint32_t *binary, unsigned num_dw)
{
   std::vector<uint32_t> binarray(binary, binary + num_dw);
   aco::Program prog;

   prog.gfx_level = info->gfx_level;
   prog.family = info->family;
   prog.wave_size = wave_size;
   prog.blocks.push_back(aco::Block());

   aco::print_asm(&prog, binarray, num_dw, stderr);
}

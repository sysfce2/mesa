/*
 * Copyright © 2015 Intel Corporation
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

/** @file
 *
 * Implements a pass that validates various invariants of the IR.  The current
 * pass only validates that GRF's uses are sane.  More can be added later.
 */

#include "brw_shader.h"
#include "brw_cfg.h"
#include "brw_eu.h"

#define fsv_assert(assertion)                                           \
   {                                                                    \
      if (!(assertion)) {                                               \
         fprintf(stderr, "ASSERT: Scalar %s validation failed!\n",      \
                 _mesa_shader_stage_to_abbrev(s.stage));                \
         brw_print_instruction(s, inst, stderr);                        \
         fprintf(stderr, "%s:%d: '%s' failed\n", __FILE__, __LINE__, #assertion);  \
         abort();                                                       \
      }                                                                 \
   }

#define fsv_assert_eq(A, B)                                             \
   {                                                                    \
      uintptr_t a = uintptr_t(A);                                       \
      uintptr_t b = uintptr_t(B);                                       \
      if (a != b) {                                                     \
         fprintf(stderr, "ASSERT: Scalar %s validation failed!\n",      \
                 _mesa_shader_stage_to_abbrev(s.stage));                \
         brw_print_instruction(s, inst, stderr);                        \
         fprintf(stderr, "%s:%d: A == B failed\n", __FILE__, __LINE__); \
         fprintf(stderr, "  A = %s = %" PRIuPTR "\n", #A, a);           \
         fprintf(stderr, "  B = %s = %" PRIuPTR "\n", #B, b);           \
         abort();                                                       \
      }                                                                 \
   }

#define fsv_assert_ne(A, B)                                             \
   {                                                                    \
      unsigned a = (A);                                                 \
      unsigned b = (B);                                                 \
      if (a == b) {                                                     \
         fprintf(stderr, "ASSERT: Scalar %s validation failed!\n",      \
                 _mesa_shader_stage_to_abbrev(s.stage));                \
         brw_print_instruction(s, inst, stderr);                        \
         fprintf(stderr, "%s:%d: A != B failed\n", __FILE__, __LINE__); \
         fprintf(stderr, "  A = %s = %u\n", #A, a);                     \
         fprintf(stderr, "  B = %s = %u\n", #B, b);                     \
         abort();                                                       \
      }                                                                 \
   }

#define fsv_assert_lte(A, B)                                            \
   {                                                                    \
      unsigned a = (A);                                                 \
      unsigned b = (B);                                                 \
      if (a > b) {                                                      \
         fprintf(stderr, "ASSERT: Scalar %s validation failed!\n",      \
                 _mesa_shader_stage_to_abbrev(s.stage));                \
         brw_print_instruction(s, inst, stderr);                        \
         fprintf(stderr, "%s:%d: A <= B failed\n", __FILE__, __LINE__); \
         fprintf(stderr, "  A = %s = %u\n", #A, a);                     \
         fprintf(stderr, "  B = %s = %u\n", #B, b);                     \
         abort();                                                       \
      }                                                                 \
   }

#ifndef NDEBUG
static inline bool
is_ud_imm(const brw_reg &reg)
{
   return reg.file == IMM && reg.type == BRW_TYPE_UD;
}

static inline bool
is_d_imm(const brw_reg &reg)
{
   return reg.file == IMM && reg.type == BRW_TYPE_D;
}

static void
validate_memory_logical(const brw_shader &s, const brw_inst *inst)
{
   const intel_device_info *devinfo = s.devinfo;

   fsv_assert(is_ud_imm(inst->src[MEMORY_LOGICAL_OPCODE]));
   fsv_assert(is_ud_imm(inst->src[MEMORY_LOGICAL_MODE]));
   fsv_assert(is_ud_imm(inst->src[MEMORY_LOGICAL_BINDING_TYPE]));
   fsv_assert(is_ud_imm(inst->src[MEMORY_LOGICAL_COORD_COMPONENTS]));
   fsv_assert(is_ud_imm(inst->src[MEMORY_LOGICAL_ALIGNMENT]));
   fsv_assert(is_ud_imm(inst->src[MEMORY_LOGICAL_DATA_SIZE]));
   fsv_assert(is_ud_imm(inst->src[MEMORY_LOGICAL_COMPONENTS]));
   fsv_assert(is_ud_imm(inst->src[MEMORY_LOGICAL_FLAGS]));
   fsv_assert(is_d_imm(inst->src[MEMORY_LOGICAL_ADDRESS_OFFSET]));

   enum lsc_opcode op = (enum lsc_opcode) inst->src[MEMORY_LOGICAL_OPCODE].ud;
   enum memory_flags flags = (memory_flags)inst->src[MEMORY_LOGICAL_FLAGS].ud;
   bool transpose = flags & MEMORY_FLAG_TRANSPOSE;
   bool include_helpers = flags & MEMORY_FLAG_INCLUDE_HELPERS;
   enum memory_logical_mode mode =
      (memory_logical_mode)inst->src[MEMORY_LOGICAL_MODE].ud;

   enum lsc_data_size data_size =
      (enum lsc_data_size) inst->src[MEMORY_LOGICAL_DATA_SIZE].ud;
   unsigned data_size_B = lsc_data_size_bytes(data_size);

   if (!devinfo->has_lsc) {
      fsv_assert(data_size == LSC_DATA_SIZE_D8U32 ||
                 data_size == LSC_DATA_SIZE_D16U32 ||
                 data_size == LSC_DATA_SIZE_D32 ||
                 data_size == LSC_DATA_SIZE_D64);

      if (transpose) {
         const unsigned min_alignment =
            mode == MEMORY_MODE_SHARED_LOCAL ? 16 : 4;
         fsv_assert(inst->src[MEMORY_LOGICAL_ALIGNMENT].ud >= min_alignment);
      }
   }

   fsv_assert(!transpose || !include_helpers);
   fsv_assert(!transpose || lsc_opcode_has_transpose(op));

   if (inst->src[MEMORY_LOGICAL_BINDING_TYPE].ud == LSC_ADDR_SURFTYPE_FLAT)
      fsv_assert(inst->src[MEMORY_LOGICAL_BINDING].file == BAD_FILE);

   if (inst->src[MEMORY_LOGICAL_DATA1].file != BAD_FILE) {
      fsv_assert(inst->src[MEMORY_LOGICAL_COMPONENTS].ud ==
                 inst->components_read(MEMORY_LOGICAL_DATA1));

      fsv_assert(inst->src[MEMORY_LOGICAL_DATA0].type ==
                 inst->src[MEMORY_LOGICAL_DATA1].type);
   }

   if (inst->src[MEMORY_LOGICAL_DATA0].file != BAD_FILE) {
      fsv_assert(inst->src[MEMORY_LOGICAL_COMPONENTS].ud ==
                 inst->components_read(MEMORY_LOGICAL_DATA0));

      fsv_assert(brw_type_size_bytes(inst->src[MEMORY_LOGICAL_DATA0].type) ==
                 data_size_B);
   }

   if (inst->dst.file != BAD_FILE)
      fsv_assert(brw_type_size_bytes(inst->dst.type) == data_size_B);

   /** TGM messages cannot have a base offset */
   if (mode == MEMORY_MODE_TYPED)
      fsv_assert(inst->src[MEMORY_LOGICAL_ADDRESS_OFFSET].d == 0);

   /* Offset must be DWord aligned */
   fsv_assert((inst->src[MEMORY_LOGICAL_ADDRESS_OFFSET].d % 4) == 0);

   switch (inst->opcode) {
   case SHADER_OPCODE_MEMORY_LOAD_LOGICAL:
      fsv_assert(op == LSC_OP_LOAD || op == LSC_OP_LOAD_CMASK ||
                 op == LSC_OP_LOAD_CMASK_MSRT);
      fsv_assert(inst->src[MEMORY_LOGICAL_DATA0].file == BAD_FILE);
      fsv_assert(inst->src[MEMORY_LOGICAL_DATA1].file == BAD_FILE);
      break;
   case SHADER_OPCODE_MEMORY_STORE_LOGICAL:
      fsv_assert(lsc_opcode_is_store(op));
      fsv_assert(inst->src[MEMORY_LOGICAL_DATA0].file != BAD_FILE);
      fsv_assert(inst->src[MEMORY_LOGICAL_DATA1].file == BAD_FILE);
      break;
   case SHADER_OPCODE_MEMORY_ATOMIC_LOGICAL:
      fsv_assert(lsc_opcode_is_atomic(op));
      fsv_assert((inst->src[MEMORY_LOGICAL_DATA0].file == BAD_FILE)
                  == (lsc_op_num_data_values(op) < 1));
      fsv_assert((inst->src[MEMORY_LOGICAL_DATA1].file == BAD_FILE)
                  == (lsc_op_num_data_values(op) < 2));
      fsv_assert(inst->src[MEMORY_LOGICAL_COMPONENTS].ud == 1);
      fsv_assert(!include_helpers);
      break;
   default:
      UNREACHABLE("invalid opcode");
   }
}

static const char *
brw_shader_phase_to_string(enum brw_shader_phase phase)
{
   switch (phase) {
   case BRW_SHADER_PHASE_INITIAL:               return "INITIAL";
   case BRW_SHADER_PHASE_AFTER_NIR:             return "AFTER_NIR";
   case BRW_SHADER_PHASE_AFTER_OPT_LOOP:        return "AFTER_OPT_LOOP";
   case BRW_SHADER_PHASE_AFTER_EARLY_LOWERING:  return "AFTER_EARLY_LOWERING";
   case BRW_SHADER_PHASE_AFTER_MIDDLE_LOWERING: return "AFTER_MIDDLE_LOWERING";
   case BRW_SHADER_PHASE_AFTER_LATE_LOWERING:   return "AFTER_LATE_LOWERING";
   case BRW_SHADER_PHASE_AFTER_REGALLOC:        return "AFTER_REGALLOC";
   case BRW_SHADER_PHASE_INVALID:               break;
   }
   UNREACHABLE("invalid_phase");
   return NULL;
}

static void
brw_validate_instruction_phase(const brw_shader &s, brw_inst *inst)
{
   enum brw_shader_phase invalid_from = BRW_SHADER_PHASE_INVALID;

   switch (inst->opcode) {
   case FS_OPCODE_FB_WRITE_LOGICAL:
   case FS_OPCODE_FB_READ_LOGICAL:
   case SHADER_OPCODE_TEX_LOGICAL:
   case SHADER_OPCODE_TXD_LOGICAL:
   case SHADER_OPCODE_TXF_LOGICAL:
   case SHADER_OPCODE_TXL_LOGICAL:
   case SHADER_OPCODE_TXS_LOGICAL:
   case SHADER_OPCODE_IMAGE_SIZE_LOGICAL:
   case FS_OPCODE_TXB_LOGICAL:
   case SHADER_OPCODE_TXF_CMS_W_LOGICAL:
   case SHADER_OPCODE_TXF_CMS_W_GFX12_LOGICAL:
   case SHADER_OPCODE_TXF_MCS_LOGICAL:
   case SHADER_OPCODE_LOD_LOGICAL:
   case SHADER_OPCODE_TG4_LOGICAL:
   case SHADER_OPCODE_TG4_BIAS_LOGICAL:
   case SHADER_OPCODE_TG4_EXPLICIT_LOD_LOGICAL:
   case SHADER_OPCODE_TG4_IMPLICIT_LOD_LOGICAL:
   case SHADER_OPCODE_TG4_OFFSET_LOGICAL:
   case SHADER_OPCODE_TG4_OFFSET_LOD_LOGICAL:
   case SHADER_OPCODE_TG4_OFFSET_BIAS_LOGICAL:
   case SHADER_OPCODE_SAMPLEINFO_LOGICAL:
   case SHADER_OPCODE_GET_BUFFER_SIZE:
   case SHADER_OPCODE_MEMORY_LOAD_LOGICAL:
   case SHADER_OPCODE_MEMORY_STORE_LOGICAL:
   case SHADER_OPCODE_MEMORY_ATOMIC_LOGICAL:
   case FS_OPCODE_VARYING_PULL_CONSTANT_LOAD_LOGICAL:
   case FS_OPCODE_INTERPOLATE_AT_SAMPLE:
   case FS_OPCODE_INTERPOLATE_AT_SHARED_OFFSET:
   case FS_OPCODE_INTERPOLATE_AT_PER_SLOT_OFFSET:
   case SHADER_OPCODE_BTD_SPAWN_LOGICAL:
   case SHADER_OPCODE_BTD_RETIRE_LOGICAL:
   case RT_OPCODE_TRACE_RAY_LOGICAL:
   case SHADER_OPCODE_URB_READ_LOGICAL:
   case SHADER_OPCODE_URB_WRITE_LOGICAL:
   case SHADER_OPCODE_REDUCE:
   case SHADER_OPCODE_INCLUSIVE_SCAN:
   case SHADER_OPCODE_EXCLUSIVE_SCAN:
   case SHADER_OPCODE_VOTE_ANY:
   case SHADER_OPCODE_VOTE_ALL:
   case SHADER_OPCODE_VOTE_EQUAL:
   case SHADER_OPCODE_BALLOT:
   case SHADER_OPCODE_QUAD_SWAP:
   case SHADER_OPCODE_READ_FROM_LIVE_CHANNEL:
   case SHADER_OPCODE_READ_FROM_CHANNEL:
   case SHADER_OPCODE_LOAD_REG:
      invalid_from = BRW_SHADER_PHASE_AFTER_EARLY_LOWERING;
      break;

   case SHADER_OPCODE_LOAD_PAYLOAD:
      invalid_from = BRW_SHADER_PHASE_AFTER_MIDDLE_LOWERING;
      break;

   default:
      /* Nothing to do. */
      break;
   }

   assert(s.phase < BRW_SHADER_PHASE_INVALID);
   if (s.phase >= invalid_from) {
      fprintf(stderr, "INVALID INSTRUCTION IN PHASE: %s\n",
              brw_shader_phase_to_string(s.phase));
      brw_print_instruction(s, inst, stderr);
      abort();
   }
}

void
brw_validate(const brw_shader &s)
{
   const intel_device_info *devinfo = s.devinfo;

   if (s.cfg)
      s.cfg->validate(_mesa_shader_stage_to_abbrev(s.stage));

   if (s.phase <= BRW_SHADER_PHASE_AFTER_NIR)
      return;

   foreach_block(block, s.cfg) {
      /* Track the last used address register. Usage of the address register
       * in the IR should be limited to within a block, otherwise we would
       * unable to schedule some instructions without spilling the address
       * register to a VGRF.
       *
       * Another pattern we stick to when using the address register in the IR
       * is that we write and read the register in pairs of instruction.
       */
      uint32_t last_used_address_register[16] = {};

      foreach_inst_in_block (brw_inst, inst, block) {
         brw_validate_instruction_phase(s, inst);

         switch (inst->opcode) {
         case SHADER_OPCODE_SEND:
            fsv_assert(is_uniform(inst->src[0]) && is_uniform(inst->src[1]));
            break;

         case SHADER_OPCODE_SEND_GATHER:
            fsv_assert(is_uniform(inst->src[0]) && is_uniform(inst->src[1]));
            fsv_assert(devinfo->ver >= 30);
            break;

         case BRW_OPCODE_MOV:
            fsv_assert(inst->sources == 1);
            break;

         case SHADER_OPCODE_MEMORY_LOAD_LOGICAL:
         case SHADER_OPCODE_MEMORY_STORE_LOGICAL:
         case SHADER_OPCODE_MEMORY_ATOMIC_LOGICAL:
            validate_memory_logical(s, inst);
            break;

         case SHADER_OPCODE_MEMORY_FENCE:
         case SHADER_OPCODE_INTERLOCK:
            fsv_assert(inst->exec_size == 1);
            fsv_assert(inst->force_writemask_all);
            fsv_assert(inst->sources == 2);
            fsv_assert(is_ud_imm(inst->src[1])); /* commit enable */
            break;

         case SHADER_OPCODE_LOAD_REG: {
            fsv_assert_eq(inst->sources, 1);
            fsv_assert_eq(s.alloc.sizes[inst->dst.nr] * REG_SIZE, inst->size_written);
            fsv_assert(!inst->is_partial_write());
            fsv_assert_lte(inst->src[0].stride, 1);

            /* For example, if file == UNIFORM, stride will be zero and offset
             * may be non-zero.
             */
            if (inst->src[0].stride != 0)
               fsv_assert_eq(inst->src[0].offset, 0);

            const brw_def_analysis &defs = s.def_analysis.require();
            fsv_assert_eq(inst, defs.get(inst->dst));

            break;
         }

         default:
            break;
         }

         /* On Xe2, the "write the accumulator in addition to the explicit
          * destination" bit no longer exists. Try to catch uses of this
          * feature earlier in the process.
          */
         if (devinfo->ver >= 20 && inst->writes_accumulator) {
            fsv_assert(inst->dst.is_accumulator() ||
                       inst->opcode == BRW_OPCODE_ADDC ||
                       inst->opcode == BRW_OPCODE_MACH ||
                       inst->opcode == BRW_OPCODE_SUBB);
         }

         if (inst->is_3src(s.compiler)) {
            const unsigned integer_sources =
               brw_type_is_int(inst->src[0].type) +
               brw_type_is_int(inst->src[1].type) +
               brw_type_is_int(inst->src[2].type);
            const unsigned float_sources =
               brw_type_is_float_or_bfloat(inst->src[0].type) +
               brw_type_is_float_or_bfloat(inst->src[1].type) +
               brw_type_is_float_or_bfloat(inst->src[2].type);

            fsv_assert((integer_sources == 3 && float_sources == 0) ||
                       (integer_sources == 0 && float_sources == 3));

            if (devinfo->ver >= 10) {
               for (unsigned i = 0; i < 3; i++) {
                  if (inst->src[i].file == IMM)
                     continue;

                  switch (inst->src[i].vstride) {
                  case BRW_VERTICAL_STRIDE_0:
                  case BRW_VERTICAL_STRIDE_4:
                  case BRW_VERTICAL_STRIDE_8:
                  case BRW_VERTICAL_STRIDE_16:
                     break;

                  case BRW_VERTICAL_STRIDE_1:
                     fsv_assert_lte(12, devinfo->ver);
                     break;

               case BRW_VERTICAL_STRIDE_2:
                  fsv_assert_lte(devinfo->ver, 11);
                  break;

                  default:
                     fsv_assert(!"invalid vstride");
                     break;
                  }
               }
            } else if (s.grf_used != 0) {
               /* Only perform the pre-Gfx10 checks after register allocation
                * has occured.
                *
                * Many passes (e.g., constant copy propagation) will
                * genenerate invalid 3-source instructions with the
                * expectation that later passes (e.g., combine constants) will
                * fix them.
                */
               for (unsigned i = 0; i < 3; i++) {
                  fsv_assert_ne(inst->src[i].file, IMM);

                  /* A stride of 1 (the usual case) or 0, with a special
                   * "repctrl" bit, is allowed. The repctrl bit doesn't work
                   * for 64-bit datatypes, so if the source type is 64-bit
                   * then only a stride of 1 is allowed. From the Broadwell
                   * PRM, Volume 7 "3D Media GPGPU", page 944:
                   *
                   *    This is applicable to 32b datatypes and 16b datatype.
                   *    64b datatypes cannot use the replicate control.
                   */
                  const unsigned stride_in_bytes = byte_stride(inst->src[i]);
                  const unsigned size_in_bytes = brw_type_size_bytes(inst->src[i].type);
                  if (stride_in_bytes == 0) {
                     /* If the source is_scalar, then the stride will be
                      * converted to <4;4,1> in brw_lower_scalar_fp64_MAD
                      * after SIMD splitting.
                      */
                     if (!inst->src[i].is_scalar)
                        fsv_assert_lte(size_in_bytes, 4);
                  } else {
                     fsv_assert_eq(stride_in_bytes, size_in_bytes);
                  }
               }
            }
         }

         if (inst->dst.file == VGRF) {
            fsv_assert_lte(inst->dst.offset / REG_SIZE + regs_written(inst),
                           s.alloc.sizes[inst->dst.nr]);
            if (inst->exec_size > 1)
               fsv_assert_ne(inst->dst.stride, 0);
         } else if (inst->dst.is_address()) {
            fsv_assert(inst->dst.nr != 0);
         }

         bool read_address_reg = false;
         for (unsigned i = 0; i < inst->sources; i++) {
            if (inst->src[i].file == VGRF) {
               fsv_assert_lte(inst->src[i].offset / REG_SIZE + regs_read(devinfo, inst, i),
                              s.alloc.sizes[inst->src[i].nr]);
            } else if (inst->src[i].is_address()) {
               fsv_assert(inst->src[i].nr != 0);
               for (unsigned hw = 0; hw < inst->size_read(devinfo, i); hw += 2) {
                  fsv_assert_eq(inst->src[i].nr,
                                last_used_address_register[inst->src[i].address_slot(hw)]);
               }
               read_address_reg = true;
            }
         }

         /* Accumulator Registers, bspec 47251:
          *
          * "When destination is accumulator with offset 0, destination
          * horizontal stride must be 1."
          */
         if (intel_needs_workaround(devinfo, 14014617373) &&
             inst->dst.is_accumulator() &&
             phys_subnr(devinfo, inst->dst) == 0) {
            fsv_assert_eq(inst->dst.hstride, 1);
         }

         if (inst->is_math() && intel_needs_workaround(devinfo, 22016140776)) {
            /* Wa_22016140776:
             *
             *    Scalar broadcast on HF math (packed or unpacked) must not be
             *    used. Compiler must use a mov instruction to expand the
             *    scalar value to a vector before using in a HF (packed or
             *    unpacked) math operation.
             *
             * Since copy propagation knows about this restriction, nothing
             * should be able to generate these invalid source strides. Detect
             * potential problems sooner rather than later.
             */
            if (devinfo->ver >= 20 && inst->writes_accumulator) {
               fsv_assert(inst->dst.is_accumulator() ||
                          inst->opcode == BRW_OPCODE_ADDC ||
                          inst->opcode == BRW_OPCODE_MACH ||
                          inst->opcode == BRW_OPCODE_SUBB);
            }

            if (inst->is_3src(s.compiler)) {
               const unsigned integer_sources =
                  brw_type_is_int(inst->src[0].type) +
                  brw_type_is_int(inst->src[1].type) +
                  brw_type_is_int(inst->src[2].type);
               const unsigned float_sources =
                  brw_type_is_float(inst->src[0].type) +
                  brw_type_is_float(inst->src[1].type) +
                  brw_type_is_float(inst->src[2].type);

               fsv_assert((integer_sources == 3 && float_sources == 0) ||
                          (integer_sources == 0 && float_sources == 3));

               if (devinfo->ver >= 10) {
                  for (unsigned i = 0; i < 3; i++) {
                     if (inst->src[i].file == IMM)
                        continue;

                     switch (inst->src[i].vstride) {
                     case BRW_VERTICAL_STRIDE_0:
                     case BRW_VERTICAL_STRIDE_4:
                     case BRW_VERTICAL_STRIDE_8:
                     case BRW_VERTICAL_STRIDE_16:
                        break;

                     case BRW_VERTICAL_STRIDE_1:
                        fsv_assert_lte(12, devinfo->ver);
                        break;

                     case BRW_VERTICAL_STRIDE_2:
                        fsv_assert_lte(devinfo->ver, 11);
                        break;

                     default:
                        fsv_assert(!"invalid vstride");
                        break;
                     }
                  }
               } else if (s.grf_used != 0) {
                  /* Only perform the pre-Gfx10 checks after register
                   * allocation has occured.
                   *
                   * Many passes (e.g., constant copy propagation) will
                   * genenerate invalid 3-source instructions with the
                   * expectation that later passes (e.g., combine constants)
                   * will fix them.
                   */
                  for (unsigned i = 0; i < 3; i++) {
                     fsv_assert_ne(inst->src[i].file, IMM);

                     /* A stride of 1 (the usual case) or 0, with a special
                      * "repctrl" bit, is allowed. The repctrl bit doesn't
                      * work for 64-bit datatypes, so if the source type is
                      * 64-bit then only a stride of 1 is allowed. From the
                      * Broadwell PRM, Volume 7 "3D Media GPGPU", page 944:
                      *
                      *    This is applicable to 32b datatypes and 16b
                      *    datatype. 64b datatypes cannot use the replicate
                      *    control.
                      */
                     const unsigned stride_in_bytes = byte_stride(inst->src[i]);
                     const unsigned size_in_bytes = brw_type_size_bytes(inst->src[i].type);
                     if (stride_in_bytes == 0) {
                        fsv_assert_lte(size_in_bytes, 4);
                     } else {
                        fsv_assert_eq(stride_in_bytes, size_in_bytes);
                     }
                  }
               }
            }

            if (inst->dst.file == VGRF) {
               fsv_assert_lte(inst->dst.offset / REG_SIZE + regs_written(inst),
                              s.alloc.sizes[inst->dst.nr]);
            }

            for (unsigned i = 0; i < inst->sources; i++) {
               fsv_assert(inst->src[i].is_scalar ||
                          !is_uniform(inst->src[i]) ||
                          inst->src[i].type != BRW_TYPE_HF);
            }
         }

         /* Update the last used address register. */
         if (read_address_reg) {
            /* When an instruction only reads the address register, we assume
             * the read parts are never going to be used again.
             */
            for (unsigned i = 0; i < inst->sources; i++) {
               if (!inst->src[i].is_address())
                  continue;
               for (unsigned hw = 0; hw < inst->size_read(devinfo, i); hw += 2)
                  last_used_address_register[inst->src[i].address_slot(hw)] = 0;
            }
         }
         if (inst->dst.is_address()) {
            /* For the written part of the address register */
            for (unsigned hw = 0; hw < inst->size_written; hw += 2)
               last_used_address_register[inst->dst.address_slot(hw)] = inst->dst.nr;
         } else if (inst->uses_address_register_implicitly()) {
            /* If the instruction is making use of the address register,
             * discard the entire thing.
             */
            memset(last_used_address_register, 0,
                   sizeof(last_used_address_register));
         }
      }
   }
}
#endif

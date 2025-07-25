/*
 * Copyright © 2010 Intel Corporation
 * SPDX-License-Identifier: MIT
 */

#include "brw_shader.h"
#include "brw_builder.h"

/* Wa_14015360517
 *
 * The first instruction of any kernel should have non-zero emask.
 * Make sure this happens by introducing a dummy mov instruction.
 */
bool
brw_workaround_emit_dummy_mov_instruction(brw_shader &s)
{
   if (!intel_needs_workaround(s.devinfo, 14015360517))
      return false;

   brw_inst *first_inst =
      s.cfg->first_block()->start();

   /* We can skip the WA if first instruction is marked with
    * force_writemask_all or exec_size equals dispatch_width.
    */
   if (first_inst->force_writemask_all ||
       first_inst->exec_size == s.dispatch_width)
      return false;

   /* Insert dummy mov as first instruction. */
   const brw_builder ubld = brw_builder(first_inst).exec_all().group(8, 0);
   ubld.MOV(ubld.null_reg_ud(), brw_imm_ud(0u));

   s.invalidate_analysis(BRW_DEPENDENCY_INSTRUCTIONS |
                         BRW_DEPENDENCY_VARIABLES);
   return true;
}

static bool
needs_dummy_fence(const intel_device_info *devinfo, brw_inst *inst)
{
   /* This workaround is about making sure that any instruction writing
    * through UGM has completed before we hit EOT.
    */
   if (inst->sfid != BRW_SFID_UGM)
      return false;

   /* Any UGM, non-Scratch-surface Stores (not including Atomic) messages,
    * where the L1-cache override is NOT among {WB, WS, WT}
    */
   enum lsc_opcode opcode = lsc_msg_desc_opcode(devinfo, inst->desc);
   if (lsc_opcode_is_store(opcode)) {
      switch (lsc_msg_desc_cache_ctrl(devinfo, inst->desc)) {
      case LSC_CACHE_STORE_L1STATE_L3MOCS:
      case LSC_CACHE_STORE_L1WB_L3WB:
      case LSC_CACHE_STORE_L1S_L3UC:
      case LSC_CACHE_STORE_L1S_L3WB:
      case LSC_CACHE_STORE_L1WT_L3UC:
      case LSC_CACHE_STORE_L1WT_L3WB:
         return false;

      default:
         return true;
      }
   }

   /* Any UGM Atomic message WITHOUT return value */
   if (lsc_opcode_is_atomic(opcode) && inst->dst.is_null())
      return true;

   return false;
}

/* Wa_22013689345
 *
 * We need to emit UGM fence message before EOT, if shader has any UGM write
 * or atomic message.
 *
 * TODO/FINISHME: According to Curro we could avoid the fence in some cases.
 *                We probably need a better criteria in needs_dummy_fence().
 */
bool
brw_workaround_memory_fence_before_eot(brw_shader &s)
{
   bool progress = false;
   bool has_ugm_write_or_atomic = false;

   if (!intel_needs_workaround(s.devinfo, 22013689345))
      return false;

   /* Needs to happen after brw_lower_logical_sends & before
    * brw_lower_send_descriptors.
    */
   assert(s.phase == BRW_SHADER_PHASE_AFTER_MIDDLE_LOWERING);

   foreach_block_and_inst_safe (block, brw_inst, inst, s.cfg) {
      if (!inst->eot) {
         if (needs_dummy_fence(s.devinfo, inst))
            has_ugm_write_or_atomic = true;
         continue;
      }

      if (!has_ugm_write_or_atomic)
         break;

      const brw_builder ubld = brw_builder(inst).uniform();

      brw_reg dst = ubld.vgrf(BRW_TYPE_UD);
      brw_inst *dummy_fence = ubld.emit(SHADER_OPCODE_SEND, dst);

      dummy_fence->resize_sources(4);
      dummy_fence->src[0] = brw_imm_ud(0);
      dummy_fence->src[1] = brw_imm_ud(0);
      dummy_fence->src[2] = brw_vec8_grf(0, 0);
      dummy_fence->src[3] = brw_reg();
      dummy_fence->mlen = reg_unit(s.devinfo);
      dummy_fence->ex_mlen = 0;
      dummy_fence->sfid = BRW_SFID_UGM;
      dummy_fence->desc = lsc_fence_msg_desc(s.devinfo, LSC_FENCE_TILE,
                                             LSC_FLUSH_TYPE_NONE_6, false);
      dummy_fence->size_written = REG_SIZE * reg_unit(s.devinfo);
      ubld.emit(FS_OPCODE_SCHEDULING_FENCE, ubld.null_reg_ud(), dst);
      progress = true;
      /* TODO: remove this break if we ever have shader with multiple EOT. */
      break;
   }

   if (progress) {
      s.invalidate_analysis(BRW_DEPENDENCY_INSTRUCTIONS |
                            BRW_DEPENDENCY_VARIABLES);
   }

   return progress;
}

/**
 * Find the first instruction in the program that might start a region of
 * divergent control flow due to a HALT jump.  There is no
 * find_halt_control_flow_region_end(), the region of divergence extends until
 * the only SHADER_OPCODE_HALT_TARGET in the program.
 */
static const brw_inst *
find_halt_control_flow_region_start(const brw_shader *v)
{
   foreach_block_and_inst(block, brw_inst, inst, v->cfg) {
      if (inst->opcode == BRW_OPCODE_HALT ||
          inst->opcode == SHADER_OPCODE_HALT_TARGET)
         return inst;
   }

   return NULL;
}

/**
 * Work around the Gfx12 hardware bug filed as Wa_1407528679.  EU fusion
 * can cause a BB to be executed with all channels disabled, which will lead
 * to the execution of any NoMask instructions in it, even though any
 * execution-masked instructions will be correctly shot down.  This may break
 * assumptions of some NoMask SEND messages whose descriptor depends on data
 * generated by live invocations of the shader.
 *
 * This avoids the problem by predicating certain instructions on an ANY
 * horizontal predicate that makes sure that their execution is omitted when
 * all channels of the program are disabled.
 */
bool
brw_workaround_nomask_control_flow(brw_shader &s)
{
   if (s.devinfo->ver != 12)
      return false;

   const brw_predicate pred = s.dispatch_width > 16 ? BRW_PREDICATE_ALIGN1_ANY32H :
                              s.dispatch_width > 8 ? BRW_PREDICATE_ALIGN1_ANY16H :
                              BRW_PREDICATE_ALIGN1_ANY8H;
   const brw_inst *halt_start = find_halt_control_flow_region_start(&s);
   unsigned depth = 0;
   bool progress = false;

   const brw_live_variables &live_vars = s.live_analysis.require();

   /* Scan the program backwards in order to be able to easily determine
    * whether the flag register is live at any point.
    */
   foreach_block_reverse_safe(block, s.cfg) {
      BITSET_WORD flag_liveout = live_vars.block_data[block->num]
                                               .flag_liveout[0];
      STATIC_ASSERT(ARRAY_SIZE(live_vars.block_data[0].flag_liveout) == 1);

      foreach_inst_in_block_reverse_safe(brw_inst, inst, block) {
         if (!inst->predicate && inst->exec_size >= 8)
            flag_liveout &= ~inst->flags_written(s.devinfo);

         switch (inst->opcode) {
         case BRW_OPCODE_DO:
         case BRW_OPCODE_IF:
            /* Note that this doesn't handle BRW_OPCODE_HALT since only
             * the first one in the program closes the region of divergent
             * control flow due to any HALT instructions -- Instead this is
             * handled with the halt_start check below.
             */
            depth--;
            break;

         case BRW_OPCODE_WHILE:
         case BRW_OPCODE_ENDIF:
         case SHADER_OPCODE_HALT_TARGET:
            depth++;
            break;

         default:
            /* Note that the vast majority of NoMask SEND instructions in the
             * program are harmless while executed in a block with all
             * channels disabled, since any instructions with side effects we
             * could hit here should be execution-masked.
             *
             * The main concern is NoMask SEND instructions where the message
             * descriptor or header depends on data generated by live
             * invocations of the shader (RESINFO and
             * FS_OPCODE_UNIFORM_PULL_CONSTANT_LOAD with a dynamically
             * computed surface index seem to be the only examples right now
             * where this could easily lead to GPU hangs).  Unfortunately we
             * have no straightforward way to detect that currently, so just
             * predicate any NoMask SEND instructions we find under control
             * flow.
             *
             * If this proves to have a measurable performance impact it can
             * be easily extended with a whitelist of messages we know we can
             * safely omit the predication for.
             */
            if (depth && inst->force_writemask_all &&
                is_send(inst) && !inst->predicate &&
                !inst->has_no_mask_send_params) {
               /* We need to load the execution mask into the flag register by
                * using a builder with channel group matching the whole shader
                * (rather than the default which is derived from the original
                * instruction), in order to avoid getting a right-shifted
                * value.
                */
               const brw_builder ubld = brw_builder(inst)
                                       .exec_all().group(s.dispatch_width, 0);
               const brw_reg flag = retype(brw_flag_reg(0, 0),
                                          BRW_TYPE_UD);

               /* Due to the lack of flag register allocation we need to save
                * and restore the flag register if it's live.
                */
               const bool save_flag = flag_liveout &
                                      brw_flag_mask(flag, s.dispatch_width / 8);
               const brw_reg tmp = ubld.group(8, 0).vgrf(flag.type);

               if (save_flag) {
                  ubld.group(8, 0).UNDEF(tmp);
                  ubld.group(1, 0).MOV(tmp, flag);
               }

               ubld.emit(FS_OPCODE_LOAD_LIVE_CHANNELS);

               set_predicate(pred, inst);
               inst->flag_subreg = 0;
               inst->predicate_trivial = true;

               if (save_flag)
                  ubld.group(1, 0).after(inst).MOV(flag, tmp);

               progress = true;
            }
            break;
         }

         if (inst == halt_start)
            depth--;

         flag_liveout |= inst->flags_read(s.devinfo);
      }
   }

   if (progress)
      s.invalidate_analysis(BRW_DEPENDENCY_INSTRUCTIONS |
                            BRW_DEPENDENCY_VARIABLES);

   return progress;
}

/**
 * flags_read() and flags_written() return flag access with byte granularity,
 * but for Flag Register PRM lists "Access Granularity: Word", so we can assume
 * accessing any part of a word will clear its register dependency.
 */
static unsigned
bytes_bitmask_to_words(unsigned b)
{
   unsigned first_byte_mask = b & 0x55555555;
   unsigned second_byte_mask = b & 0xaaaaaaaa;
   return first_byte_mask |
          (first_byte_mask << 1) |
          second_byte_mask |
          (second_byte_mask >> 1);
}

/**
 * WaClearArfDependenciesBeforeEot
 *
 * Flag register dependency not cleared after EOT, so we have to source them
 * before EOT. We can do this with simple `mov(1) nullUD, f{0,1}UD`
 *
 * To avoid emitting MOVs when it's not needed, check if each block  reads all
 * the flags it sets. We might falsely determine register as unread if it'll be
 * accessed inside the next blocks, but this still should be good enough.
 */
bool
brw_workaround_source_arf_before_eot(brw_shader &s)
{
   bool progress = false;

   if (s.devinfo->ver != 9)
      return false;

   unsigned flags_unread = 0;

   foreach_block(block, s.cfg) {
      unsigned flags_unread_in_block = 0;

      foreach_inst_in_block(brw_inst, inst, block) {
         /* Instruction can read and write to the same flag, so the order is important */
         flags_unread_in_block &= ~bytes_bitmask_to_words(inst->flags_read(s.devinfo));
         flags_unread_in_block |= bytes_bitmask_to_words(inst->flags_written(s.devinfo));

         /* HALT does not start its block even though it can leave a dependency */
         if (inst->opcode == BRW_OPCODE_HALT ||
             inst->opcode == SHADER_OPCODE_HALT_TARGET) {
            flags_unread |= flags_unread_in_block;
            flags_unread_in_block = 0;
         }
      }

      flags_unread |= flags_unread_in_block;

      if ((flags_unread & 0x0f) && (flags_unread & 0xf0))
         break;
   }

   if (flags_unread) {
      int eot_count = 0;

      foreach_block_and_inst_safe(block, brw_inst, inst, s.cfg)
      {
         if (!inst->eot)
            continue;

         /* Currently, we always emit only one EOT per program,
          * this WA should be updated if it ever changes.
          */
         assert(++eot_count == 1);

         const brw_builder ubld = brw_builder(inst).uniform();

         if (flags_unread & 0x0f)
            ubld.MOV(ubld.null_reg_ud(), retype(brw_flag_reg(0, 0), BRW_TYPE_UD));

         if (flags_unread & 0xf0)
            ubld.MOV(ubld.null_reg_ud(), retype(brw_flag_reg(1, 0), BRW_TYPE_UD));
      }

      progress = true;
      s.invalidate_analysis(BRW_DEPENDENCY_INSTRUCTIONS);
   }

   return progress;
}

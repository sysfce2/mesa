/*
 * Copyright © 2012 Intel Corporation
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

#include "elk_fs.h"
#include "elk_fs_builder.h"
#include "elk_cfg.h"

/** @file elk_fs_cse.cpp
 *
 * Support for local common subexpression elimination.
 *
 * See Muchnick's Advanced Compiler Design and Implementation, section
 * 13.1 (p378).
 */

using namespace elk;

namespace {
struct aeb_entry : public brw_exec_node {
   /** The instruction that generates the expression value. */
   elk_fs_inst *generator;

   /** The temporary where the value is stored. */
   elk_fs_reg tmp;
};
}

static bool
is_expression(const elk_fs_visitor *v, const elk_fs_inst *const inst)
{
   switch (inst->opcode) {
   case ELK_OPCODE_MOV:
   case ELK_OPCODE_SEL:
   case ELK_OPCODE_NOT:
   case ELK_OPCODE_AND:
   case ELK_OPCODE_OR:
   case ELK_OPCODE_XOR:
   case ELK_OPCODE_SHR:
   case ELK_OPCODE_SHL:
   case ELK_OPCODE_ASR:
   case ELK_OPCODE_CMP:
   case ELK_OPCODE_CMPN:
   case ELK_OPCODE_ADD:
   case ELK_OPCODE_MUL:
   case ELK_SHADER_OPCODE_MULH:
   case ELK_OPCODE_FRC:
   case ELK_OPCODE_RNDU:
   case ELK_OPCODE_RNDD:
   case ELK_OPCODE_RNDE:
   case ELK_OPCODE_RNDZ:
   case ELK_OPCODE_LINE:
   case ELK_OPCODE_PLN:
   case ELK_OPCODE_MAD:
   case ELK_OPCODE_LRP:
   case ELK_FS_OPCODE_UNIFORM_PULL_CONSTANT_LOAD:
   case ELK_FS_OPCODE_VARYING_PULL_CONSTANT_LOAD_LOGICAL:
   case ELK_FS_OPCODE_LINTERP:
   case ELK_SHADER_OPCODE_FIND_LIVE_CHANNEL:
   case ELK_SHADER_OPCODE_FIND_LAST_LIVE_CHANNEL:
   case ELK_FS_OPCODE_LOAD_LIVE_CHANNELS:
   case ELK_SHADER_OPCODE_BROADCAST:
   case ELK_SHADER_OPCODE_MOV_INDIRECT:
   case ELK_SHADER_OPCODE_TEX_LOGICAL:
   case ELK_SHADER_OPCODE_TXD_LOGICAL:
   case ELK_SHADER_OPCODE_TXF_LOGICAL:
   case ELK_SHADER_OPCODE_TXL_LOGICAL:
   case ELK_SHADER_OPCODE_TXS_LOGICAL:
   case ELK_FS_OPCODE_TXB_LOGICAL:
   case ELK_SHADER_OPCODE_TXF_CMS_LOGICAL:
   case ELK_SHADER_OPCODE_TXF_CMS_W_LOGICAL:
   case ELK_SHADER_OPCODE_TXF_UMS_LOGICAL:
   case ELK_SHADER_OPCODE_TXF_MCS_LOGICAL:
   case ELK_SHADER_OPCODE_LOD_LOGICAL:
   case ELK_SHADER_OPCODE_TG4_LOGICAL:
   case ELK_SHADER_OPCODE_TG4_OFFSET_LOGICAL:
   case ELK_FS_OPCODE_PACK:
      return true;
   case ELK_SHADER_OPCODE_RCP:
   case ELK_SHADER_OPCODE_RSQ:
   case ELK_SHADER_OPCODE_SQRT:
   case ELK_SHADER_OPCODE_EXP2:
   case ELK_SHADER_OPCODE_LOG2:
   case ELK_SHADER_OPCODE_POW:
   case ELK_SHADER_OPCODE_INT_QUOTIENT:
   case ELK_SHADER_OPCODE_INT_REMAINDER:
   case ELK_SHADER_OPCODE_SIN:
   case ELK_SHADER_OPCODE_COS:
      return inst->mlen < 2;
   case ELK_SHADER_OPCODE_LOAD_PAYLOAD:
      return !is_coalescing_payload(v->alloc, inst);
   default:
      return inst->is_send_from_grf() && !inst->has_side_effects() &&
         !inst->is_volatile();
   }
}

static bool
operands_match(const elk_fs_inst *a, const elk_fs_inst *b, bool *negate)
{
   elk_fs_reg *xs = a->src;
   elk_fs_reg *ys = b->src;

   if (a->opcode == ELK_OPCODE_MAD) {
      return xs[0].equals(ys[0]) &&
             ((xs[1].equals(ys[1]) && xs[2].equals(ys[2])) ||
              (xs[2].equals(ys[1]) && xs[1].equals(ys[2])));
   } else if (a->opcode == ELK_OPCODE_MUL && a->dst.type == ELK_REGISTER_TYPE_F) {
      bool xs0_negate = xs[0].negate;
      bool xs1_negate = xs[1].file == IMM ? xs[1].f < 0.0f
                                          : xs[1].negate;
      bool ys0_negate = ys[0].negate;
      bool ys1_negate = ys[1].file == IMM ? ys[1].f < 0.0f
                                          : ys[1].negate;
      float xs1_imm = xs[1].f;
      float ys1_imm = ys[1].f;

      xs[0].negate = false;
      xs[1].negate = false;
      ys[0].negate = false;
      ys[1].negate = false;
      xs[1].f = fabsf(xs[1].f);
      ys[1].f = fabsf(ys[1].f);

      bool ret = (xs[0].equals(ys[0]) && xs[1].equals(ys[1])) ||
                 (xs[1].equals(ys[0]) && xs[0].equals(ys[1]));

      xs[0].negate = xs0_negate;
      xs[1].negate = xs[1].file == IMM ? false : xs1_negate;
      ys[0].negate = ys0_negate;
      ys[1].negate = ys[1].file == IMM ? false : ys1_negate;
      xs[1].f = xs1_imm;
      ys[1].f = ys1_imm;

      *negate = (xs0_negate != xs1_negate) != (ys0_negate != ys1_negate);
      if (*negate && (a->saturate || b->saturate))
         return false;
      return ret;
   } else if (!a->is_commutative()) {
      bool match = true;
      for (int i = 0; i < a->sources; i++) {
         if (!xs[i].equals(ys[i])) {
            match = false;
            break;
         }
      }
      return match;
   } else {
      return (xs[0].equals(ys[0]) && xs[1].equals(ys[1])) ||
             (xs[1].equals(ys[0]) && xs[0].equals(ys[1]));
   }
}

static bool
instructions_match(elk_fs_inst *a, elk_fs_inst *b, bool *negate)
{
   return a->opcode == b->opcode &&
          a->force_writemask_all == b->force_writemask_all &&
          a->exec_size == b->exec_size &&
          a->group == b->group &&
          a->saturate == b->saturate &&
          a->predicate == b->predicate &&
          a->predicate_inverse == b->predicate_inverse &&
          a->conditional_mod == b->conditional_mod &&
          a->flag_subreg == b->flag_subreg &&
          a->dst.type == b->dst.type &&
          a->offset == b->offset &&
          a->mlen == b->mlen &&
          a->sfid == b->sfid &&
          a->desc == b->desc &&
          a->size_written == b->size_written &&
          a->base_mrf == b->base_mrf &&
          a->check_tdr == b->check_tdr &&
          a->send_has_side_effects == b->send_has_side_effects &&
          a->eot == b->eot &&
          a->header_size == b->header_size &&
          a->shadow_compare == b->shadow_compare &&
          a->pi_noperspective == b->pi_noperspective &&
          a->target == b->target &&
          a->sources == b->sources &&
          operands_match(a, b, negate);
}

static void
create_copy_instr(const fs_builder &bld, elk_fs_inst *inst, elk_fs_reg src, bool negate)
{
   unsigned written = regs_written(inst);
   unsigned dst_width =
      DIV_ROUND_UP(inst->dst.component_size(inst->exec_size), REG_SIZE);
   elk_fs_inst *copy;

   if (inst->opcode == ELK_SHADER_OPCODE_LOAD_PAYLOAD) {
      assert(src.file == VGRF);
      elk_fs_reg *payload = ralloc_array(bld.shader->mem_ctx, elk_fs_reg,
                                     inst->sources);
      for (int i = 0; i < inst->header_size; i++) {
         payload[i] = src;
         src.offset += REG_SIZE;
      }
      for (int i = inst->header_size; i < inst->sources; i++) {
         src.type = inst->src[i].type;
         payload[i] = src;
         src = offset(src, bld, 1);
      }
      copy = bld.LOAD_PAYLOAD(inst->dst, payload, inst->sources,
                              inst->header_size);
   } else if (written != dst_width) {
      assert(src.file == VGRF);
      assert(written % dst_width == 0);
      const int sources = written / dst_width;
      elk_fs_reg *payload = ralloc_array(bld.shader->mem_ctx, elk_fs_reg, sources);
      for (int i = 0; i < sources; i++) {
         payload[i] = src;
         src = offset(src, bld, 1);
      }
      copy = bld.LOAD_PAYLOAD(inst->dst, payload, sources, 0);
   } else {
      copy = bld.MOV(inst->dst, src);
      copy->group = inst->group;
      copy->force_writemask_all = inst->force_writemask_all;
      copy->src[0].negate = negate;
   }
   assert(regs_written(copy) == written);
}

bool
elk_fs_visitor::opt_cse_local(const fs_live_variables &live, elk_bblock_t *block, int &ip)
{
   bool progress = false;
   brw_exec_list aeb;

   void *cse_ctx = ralloc_context(NULL);

   foreach_inst_in_block(elk_fs_inst, inst, block) {
      /* Skip some cases. */
      if (is_expression(this, inst) && !inst->is_partial_write() &&
          ((inst->dst.file != ARF && inst->dst.file != FIXED_GRF) ||
           inst->dst.is_null()))
      {
         bool found = false;
         bool negate = false;

         brw_foreach_in_list_use_after(aeb_entry, entry, &aeb) {
            /* Match current instruction's expression against those in AEB. */
            if (!(entry->generator->dst.is_null() && !inst->dst.is_null()) &&
                instructions_match(inst, entry->generator, &negate)) {
               found = true;
               progress = true;
               break;
            }
         }

         if (!found) {
            if (inst->opcode != ELK_OPCODE_MOV ||
                (inst->opcode == ELK_OPCODE_MOV &&
                 inst->src[0].file == IMM &&
                 inst->src[0].type == ELK_REGISTER_TYPE_VF)) {
               /* Our first sighting of this expression.  Create an entry. */
               aeb_entry *entry = ralloc(cse_ctx, aeb_entry);
               entry->tmp = reg_undef;
               entry->generator = inst;
               aeb.push_tail(entry);
            }
         } else {
            /* This is at least our second sighting of this expression.
             * If we don't have a temporary already, make one.
             */
            bool no_existing_temp = entry->tmp.file == BAD_FILE;
            if (no_existing_temp && !entry->generator->dst.is_null()) {
               const fs_builder ibld = fs_builder(this, block, entry->generator)
                                       .at(block, entry->generator->next);
               int written = regs_written(entry->generator);

               entry->tmp = elk_fs_reg(VGRF, alloc.allocate(written),
                                   entry->generator->dst.type);

               create_copy_instr(ibld, entry->generator, entry->tmp, false);

               entry->generator->dst = entry->tmp;
            }

            /* dest <- temp */
            if (!inst->dst.is_null()) {
               assert(inst->size_written == entry->generator->size_written);
               assert(inst->dst.type == entry->tmp.type);
               const fs_builder ibld(this, block, inst);

               create_copy_instr(ibld, inst, entry->tmp, negate);
            }

            /* Set our iterator so that next time through the loop inst->next
             * will get the instruction in the basic block after the one we've
             * removed.
             */
            elk_fs_inst *prev = (elk_fs_inst *)inst->prev;

            inst->remove(block);
            inst = prev;
         }
      }

      /* Discard jumps aren't represented in the CFG unfortunately, so we need
       * to make sure that they behave as a CSE barrier, since we lack global
       * dataflow information.  This is particularly likely to cause problems
       * with instructions dependent on the current execution mask like
       * ELK_SHADER_OPCODE_FIND_LIVE_CHANNEL.
       */
      if (inst->opcode == ELK_OPCODE_HALT ||
          inst->opcode == ELK_SHADER_OPCODE_HALT_TARGET)
         aeb.make_empty();

      brw_foreach_in_list_safe(aeb_entry, entry, &aeb) {
         /* Kill all AEB entries that write a different value to or read from
          * the flag register if we just wrote it.
          */
         if (inst->flags_written(devinfo)) {
            bool negate; /* dummy */
            if (entry->generator->flags_read(devinfo) ||
                (entry->generator->flags_written(devinfo) &&
                 !instructions_match(inst, entry->generator, &negate))) {
               entry->remove();
               ralloc_free(entry);
               continue;
            }
         }

         for (int i = 0; i < entry->generator->sources; i++) {
            elk_fs_reg *src_reg = &entry->generator->src[i];

            /* Kill all AEB entries that use the destination we just
             * overwrote.
             */
            if (regions_overlap(inst->dst, inst->size_written,
                                entry->generator->src[i],
                                entry->generator->size_read(i))) {
               entry->remove();
               ralloc_free(entry);
               break;
            }

            /* Kill any AEB entries using registers that don't get reused any
             * more -- a sure sign they'll fail operands_match().
             */
            if (src_reg->file == VGRF && live.vgrf_end[src_reg->nr] < ip) {
               entry->remove();
               ralloc_free(entry);
               break;
            }
         }
      }

      ip++;
   }

   ralloc_free(cse_ctx);

   return progress;
}

bool
elk_fs_visitor::opt_cse()
{
   const fs_live_variables &live = live_analysis.require();
   bool progress = false;
   int ip = 0;

   foreach_block (block, cfg) {
      progress = opt_cse_local(live, block, ip) || progress;
   }

   if (progress)
      invalidate_analysis(DEPENDENCY_INSTRUCTIONS | DEPENDENCY_VARIABLES);

   return progress;
}

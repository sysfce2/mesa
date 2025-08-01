/*
 * Copyright © 2012 Rob Clark <robdclark@gmail.com>
 * SPDX-License-Identifier: MIT
 */

#include "ir3.h"

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "util/bitscan.h"
#include "util/half_float.h"
#include "util/ralloc.h"
#include "util/u_math.h"

#include "instr-a3xx.h"
#include "ir3_shader.h"

/* simple allocator to carve allocations out of an up-front allocated heap,
 * so that we can free everything easily in one shot.
 */
void *
ir3_alloc(struct ir3 *shader, int sz)
{
   return rzalloc_size(shader, sz); /* TODO: don't use rzalloc */
}

struct ir3 *
ir3_create(struct ir3_compiler *compiler, struct ir3_shader_variant *v)
{
   struct ir3 *shader = rzalloc(v, struct ir3);

   shader->compiler = compiler;
   shader->type = v->type;

   list_inithead(&shader->block_list);
   list_inithead(&shader->array_list);

   return shader;
}

void
ir3_destroy(struct ir3 *shader)
{
   ralloc_free(shader);
}

static bool
is_shared_consts(struct ir3_compiler *compiler,
                 const struct ir3_const_state *const_state,
                 struct ir3_register *reg)
{
   if (const_state->push_consts_type == IR3_PUSH_CONSTS_SHARED &&
       reg->flags & IR3_REG_CONST) {
      uint32_t min_const_reg = regid(compiler->shared_consts_base_offset, 0);
      uint32_t max_const_reg =
         regid(compiler->shared_consts_base_offset +
               compiler->shared_consts_size, 0);
      return reg->num >= min_const_reg && min_const_reg < max_const_reg;
   }

   return false;
}

static void
collect_reg_info(struct ir3_shader_variant *v,
                 struct ir3_instruction *instr, struct ir3_register *reg,
                 struct ir3_info *info)
{
   if (reg->flags & IR3_REG_IMMED) {
      /* nothing to do */
      return;
   }

   /* Shared consts don't need to be included into constlen. */
   if (is_shared_consts(v->compiler, ir3_const_state(v), reg))
      return;

   unsigned components;
   int16_t max;

   if (reg->flags & IR3_REG_RELATIV) {
      components = reg->size;
      max = (reg->array.base + components - 1);
   } else {
      components = util_last_bit(reg->wrmask);
      max = (reg->num + components - 1);
   }

   if (reg->flags & IR3_REG_CONST) {
      info->max_const = MAX2(info->max_const, max >> 2);
   } else if (max < regid(48, 0)) {
      if (reg->flags & IR3_REG_HALF) {
         if (v->mergedregs) {
            /* starting w/ a6xx, half regs conflict with full regs: */
            info->max_reg = MAX2(info->max_reg, max >> 3);
         } else {
            info->max_half_reg = MAX2(info->max_half_reg, max >> 2);
         }
      } else {
         info->max_reg = MAX2(info->max_reg, max >> 2);
      }
   }
}

/* Returns whether the shader uses a significant amount of 16-bit ALU ops, for
 * the purposes of double_threadsize heuristics.
 */
static bool
uses_significant_16bit_alu(struct ir3_shader_variant *v)
{
   uint32_t full = 0, half = 0;

   foreach_block (block, &v->ir->block_list) {
      foreach_instr (instr, &block->instr_list) {
         /* Consider only ALU and EFU opcodes, not tex/buffer or most importantly meta ,*/
         if (opc_cat(instr->opc) <= 4) {
            bool is_half = false;

            /* The size of the operation is determined by the src sizes (which
             * must match) and there's an implicit conversion after the
             * operation to the dest size. But cwabbott thinks a 32-bit
             * upconversion does limit the throughput to being single-wide,
             * since the HW can't write a 32-bit register in a single cycle in
             * double-wide mode.
             */
            foreach_dst(dst, instr) {
               if (dst->flags & IR3_REG_HALF) {
                  foreach_src(src, instr) {
                     if (src->flags & IR3_REG_HALF) {
                        is_half = true;
                        break;
                     }
                  }
               }
            }

            if (is_half)
               half++;
            else
               full++;
         }
      }
   }

   /* We don't just check for nonzero 16-bit, because comparisons produce HALF
    * results, so mostly-fp32 shaders will have a nonzero amount.  So check if
    * we have 1/5 half ALUs as a compromise.  The number didn't matter much,
    * because shaders in testing tended to be mostly fp16 for GLES2 and mostly
    * fp32 otherwise.
    */
   return full < (half * 4);
}

/**
 * Use compiled shader parameters to determine if the shader should be run with
 * double_threadsize, where the instructions and footprint in the register file
 * expand to operate on (e.g. for a6xx+) 128 instead of 64 instances of the
 * shader at a time.  Sometimes doubling threadsize is impossible (when we run
 * up against HW limits), and sometimes it is required to hit API requirements
 * (large CS workgroup sizes), but most of the time we can choose.
 *
 * Doubling threadsize doesn't change the compiled shader instructions, but it
 * has complicated effects on performance:
 *
 * - Non-16bit ALU instructions go from 1 cycle per ALU op to 2, while 16-bit
 *   instructions run at double rate and still take 1 cycle.
 *
 * - Increasing threadsize increases the latency of requests to the EFU or
 *   memory accesses.
 *
 * - Increasing threadsize can reduce the number of waves executing in parallel
 *   when we exceed the register file size, reducing the ability to hide latency
 *   by switching waves.
 *
 * - Increasing threadsize can increase the cost of dynamic branching because it
 *   increases the likelihood that the wave executes both sides of a branch.
 *   (this effect is expected to be tiny, see
 *   https://gfxstrand.net/faith/blog/2020/10/does-subgroup-wave-size-matter/)
 *
 * - Increasing threadsize reduces icache missees for large shaders by executing
 *   half the waves.
 *
 * - Increasing threadsize reduces the overhead of scalar operations, uniform
 *   branching, and uniform memory accesses that may exist in a shader by
 *   running those instructions half as often.
 *
 * - Increasing threadsize increases the overhead of small waves when the second
 *   half of the wave is unused -- we can detect this in compute shaders
 *   sometimes (small workgroup size set), but it is workload-dependent in pixel
 *   shaders.
 *
 * Thus, the performance impact of doubling threadsize is quite complicated, and
 * we use heuristics when the choice is available to us.
 */
bool
ir3_should_double_threadsize(struct ir3_shader_variant *v, unsigned regs_count)
{
   const struct ir3_compiler *compiler = v->compiler;

   /* If the user forced a particular wavesize respect that. */
   if (v->shader_options.real_wavesize == IR3_SINGLE_ONLY)
      return false;
   if (v->shader_options.real_wavesize == IR3_DOUBLE_ONLY)
      return true;

   /* We can't support more than compiler->branchstack_size diverging threads
    * in a wave. Thus, doubling the threadsize is only possible if we don't
    * exceed the branchstack size limit.
    */
   if (MIN2(v->branchstack, compiler->threadsize_base * 2) >
       compiler->branchstack_size) {
      return false;
   }

   switch (v->type) {
   case MESA_SHADER_KERNEL:
   case MESA_SHADER_COMPUTE: {
      unsigned threads_per_wg =
         v->local_size[0] * v->local_size[1] * v->local_size[2];

      /* If the workgroups fit in the base threadsize, then doubling would just
       * leave us with an unused second half of each wave for no gain (The HW
       * can't pack multiple workgroups into a wave, because the workgroups
       * might make different barrier choices).
       */
      if (!v->local_size_variable) {
         if (threads_per_wg <= compiler->threadsize_base)
            return false;
      }

      /* For a5xx, if the workgroup size is greater than the maximum number
       * of threads per core with 32 threads per wave (512) then we have to
       * use the doubled threadsize because otherwise the workgroup wouldn't
       * fit. For smaller workgroup sizes, we follow the blob and use the
       * smaller threadsize.
       *
       * For a6xx, because threadsize_base is bumped to 64, we don't have to
       * worry about the workgroup fitting.
       */
      if (compiler->gen < 6) {
         return v->local_size_variable ||
                threads_per_wg >
                   compiler->threadsize_base * compiler->max_waves;
      }

   }
      FALLTHROUGH;
   case MESA_SHADER_FRAGMENT: {
      /* One of the limits on maximum waves of the shader running in parallel is
       * the register count used in the shader compared to the hardware's
       * register file size.  The absolute limit is if doubling the threadsize
       * would exceed regfile size (regs*2 <= reg_size_vec4, producing just
       * wave_granularity max_waves).  However, testing on X1-85 found that the
       * sweet spot for non-fp16 apps was when max_waves would still be >= 8
       * (4*wave_granularity) -- presumably reduced waves meant less ability to
       * hide latency through switching to another wave (and the increased
       * shader complexity that comes with low max_waves probably also
       * correlated with dynamic branching).  For fp16 apps, the increased ALU
       * rate made it worth it regardless.
       */
      if (uses_significant_16bit_alu(v)) {
         /* Check that doubling the threadsize wouldn't exceed the regfile size */
         return regs_count * 2 <= compiler->reg_size_vec4;
      } else {
         return regs_count * 2 <= compiler->reg_size_vec4 / 4;
      }
   }

   default:
      /* On a6xx+, it's impossible to use a doubled wavesize in the geometry
       * stages - the bit doesn't exist. The blob never used it for the VS
       * on earlier gen's anyway.
       */
      return false;
   }
}

/* Get the maximum number of waves that could be used even if this shader
 * didn't use any registers.
 */
unsigned
ir3_get_reg_independent_max_waves(struct ir3_shader_variant *v,
                                  bool double_threadsize)
{
   const struct ir3_compiler *compiler = v->compiler;
   unsigned max_waves = compiler->max_waves;

   /* Compute the limit based on branchstack */
   if (v->branchstack > 0) {
      unsigned branchstack_max_waves = compiler->branchstack_size /
                                       v->branchstack *
                                       compiler->wave_granularity;
      max_waves = MIN2(max_waves, branchstack_max_waves);
   }

   /* If this is a compute shader, compute the limit based on shared size */
   if ((v->type == MESA_SHADER_COMPUTE) ||
       (v->type == MESA_SHADER_KERNEL)) {
      unsigned threads_per_wg =
         v->local_size[0] * v->local_size[1] * v->local_size[2];
      unsigned waves_per_wg =
         DIV_ROUND_UP(threads_per_wg, compiler->threadsize_base *
                                         (double_threadsize ? 2 : 1) *
                                         compiler->wave_granularity);

      /* Shared is allocated in chunks of 1k */
      unsigned shared_per_wg = ALIGN_POT(v->shared_size, 1024);
      if (shared_per_wg > 0 && !v->local_size_variable) {
         unsigned wgs_per_core = compiler->local_mem_size / shared_per_wg;

         max_waves = MIN2(max_waves, waves_per_wg * wgs_per_core *
                                        compiler->wave_granularity);
      }

      /* If we have a compute shader that has a big workgroup, a barrier, and
       * a branchstack which limits max_waves - this may result in a situation
       * when we cannot run concurrently all waves of the workgroup, which
       * would lead to a hang.
       *
       * TODO: Could we spill branchstack or is there other way around?
       * Blob just explodes in such case.
       */
      if (v->has_barrier && (max_waves < waves_per_wg)) {
         mesa_loge(
            "Compute shader (%s) which has workgroup barrier cannot be used "
            "because it's impossible to have enough concurrent waves.",
            v->name);
         exit(1);
      }
   }

   return max_waves;
}

/* Get the maximum number of waves that could be launched limited by reg size.
 */
unsigned
ir3_get_reg_dependent_max_waves(const struct ir3_compiler *compiler,
                                unsigned reg_count, bool double_threadsize)
{
   return reg_count ? (compiler->reg_size_vec4 /
                       (reg_count * (double_threadsize ? 2 : 1)) *
                       compiler->wave_granularity)
                    : compiler->max_waves;
}

void
ir3_collect_info(struct ir3_shader_variant *v)
{
   struct ir3_info *info = &v->info;
   struct ir3 *shader = v->ir;
   const struct ir3_compiler *compiler = v->compiler;

   memset(info, 0, sizeof(*info));
   info->max_reg = -1;
   info->max_half_reg = -1;
   info->max_const = -1;
   info->multi_dword_ldp_stp = false;

   uint32_t instr_count = 0;
   foreach_block (block, &shader->block_list) {
      foreach_instr (instr, &block->instr_list) {
         instr_count++;
      }
   }

   v->instrlen = DIV_ROUND_UP(instr_count, compiler->instr_align);

   /* Pad out with NOPs to instrlen, including at least 4 so that cffdump
    * doesn't try to decode the following data as instructions (such as the
    * next stage's shader in turnip)
    */
   info->size = MAX2(v->instrlen * compiler->instr_align, instr_count + 4) * 8;
   info->sizedwords = info->size / 4;

   info->early_preamble = v->early_preamble;

   bool in_preamble = false;
   bool has_eq = false;

   /* Track which registers are currently aliases because they shouldn't be
    * included in the GPR footprint.
    */
   regmask_t aliases;

   /* Full and half aliases do not overlap so treat them as !mergedregs. */
   regmask_init(&aliases, false);

   struct block_data {
      unsigned sfu_delay;
      unsigned mem_delay;
   };

   void *mem_ctx = ralloc_context(NULL);

   foreach_block (block, &shader->block_list) {
      block->data = rzalloc(mem_ctx, struct block_data);
   }

   foreach_block (block, &shader->block_list) {
      struct block_data *bd = block->data;

      for (unsigned i = 0; i < block->predecessors_count; i++) {
         struct block_data *pbd = block->predecessors[i]->data;
         bd->sfu_delay = MAX2(bd->sfu_delay, pbd->sfu_delay);
         bd->mem_delay = MAX2(bd->mem_delay, pbd->mem_delay);
      }

      foreach_instr (instr, &block->instr_list) {

         foreach_src (reg, instr) {
            if (!is_reg_gpr(reg) || !regmask_get(&aliases, reg)) {
               collect_reg_info(v, instr, reg, info);
            }
         }

         foreach_dst (reg, instr) {
            if (instr->opc == OPC_ALIAS &&
                instr->cat7.alias_scope == ALIAS_TEX) {
               regmask_set(&aliases, instr->dsts[0]);
            } else if (is_dest_gpr(reg)) {
               collect_reg_info(v, instr, reg, info);
            }
         }

         if (is_tex(instr)) {
            /* All aliases are cleared after they are used. */
            regmask_init(&aliases, false);
         }

         if ((instr->opc == OPC_STP || instr->opc == OPC_LDP)) {
            unsigned components = instr->srcs[2]->uim_val;

            /* This covers any multi-component access that could straddle
             * across multiple double-words.
             */
            if (components > 1)
               info->multi_dword_ldp_stp = true;

            if (instr->opc == OPC_STP)
               info->stp_count += components;
            else
               info->ldp_count += components;
         }

         if ((instr->opc == OPC_BARY_F || instr->opc == OPC_FLAT_B) &&
             (instr->dsts[0]->flags & IR3_REG_EI))
            info->last_baryf = info->instrs_count;

         if ((instr->opc == OPC_NOP) && (instr->flags & IR3_INSTR_EQ)) {
            info->last_helper = info->instrs_count;
            has_eq = true;
         }

         if (v->type == MESA_SHADER_FRAGMENT && v->need_pixlod &&
             instr->opc == OPC_END && !v->prefetch_end_of_quad && !has_eq)
            info->last_helper = info->instrs_count;

         if (instr->opc == OPC_SHPS)
            in_preamble = true;

         /* Don't count instructions in the preamble for instruction-count type
          * stats, because their effect should be much smaller.
          * TODO: we should probably have separate stats for preamble
          * instructions, but that would blow up the amount of stats...
          */
         if (!in_preamble) {
            unsigned instrs_count = 1 + instr->repeat + instr->nop;
            unsigned nops_count = instr->nop;

            if (instr->opc == OPC_NOP) {
               nops_count = 1 + instr->repeat;
               info->instrs_per_cat[0] += nops_count;
            } else if (!is_meta(instr)) {
               info->instrs_per_cat[opc_cat(instr->opc)] += 1 + instr->repeat;
               info->instrs_per_cat[0] += nops_count;
            }

            if (instr->opc == OPC_MOV) {
               if (instr->cat1.src_type == instr->cat1.dst_type) {
                  info->mov_count += 1 + instr->repeat;
               } else {
                  info->cov_count += 1 + instr->repeat;
               }
            }

            info->instrs_count += instrs_count;
            info->nops_count += nops_count;

            if (instr->flags & IR3_INSTR_SS) {
               info->ss++;
               info->sstall += bd->sfu_delay;
               bd->sfu_delay = 0;
            }

            if (instr->flags & IR3_INSTR_SY) {
               info->sy++;
               info->systall += bd->mem_delay;
               bd->mem_delay = 0;
            }

            if (is_ss_producer(instr)) {
               bd->sfu_delay = soft_ss_delay(instr);
            } else {
               int n = MIN2(bd->sfu_delay, 1 + instr->repeat + instr->nop);
               bd->sfu_delay -= n;
            }

            if (is_sy_producer(instr)) {
               bd->mem_delay = soft_sy_delay(instr, shader);
            } else {
               int n = MIN2(bd->mem_delay, 1 + instr->repeat + instr->nop);
               bd->mem_delay -= n;
            }
         } else {
            unsigned instrs_count = 1 + instr->repeat + instr->nop;
            info->preamble_instrs_count += instrs_count;
         }

         if (instr->opc == OPC_SHPE)
            in_preamble = false;
      }
   }

   /* for vertex shader, the inputs are loaded into registers before the shader
    * is executed, so max_regs from the shader instructions might not properly
    * reflect the # of registers actually used, especially in case passthrough
    * varyings.
    *
    * Likewise, for fragment shader, we can have some regs which are passed
    * input values but never touched by the resulting shader (ie. as result
    * of dead code elimination or simply because we don't know how to turn
    * the reg off.
    */
   for (unsigned i = 0; i < v->inputs_count; i++) {
      /* skip frag inputs fetch via bary.f since their reg's are
       * not written by gpu before shader starts (and in fact the
       * regid's might not even be valid)
       */
      if (v->inputs[i].bary)
         continue;

      /* ignore high regs that are global to all threads in a warp
       * (they exist by default) (a5xx+)
       */
      if (v->inputs[i].regid >= regid(48, 0))
         continue;

      if (v->inputs[i].compmask) {
         unsigned n = util_last_bit(v->inputs[i].compmask) - 1;
         int32_t regid = v->inputs[i].regid + n;
         if (v->inputs[i].half) {
            if (!v->mergedregs) {
               v->info.max_half_reg = MAX2(v->info.max_half_reg, regid >> 2);
            } else {
               v->info.max_reg = MAX2(v->info.max_reg, regid >> 3);
            }
         } else {
            v->info.max_reg = MAX2(v->info.max_reg, regid >> 2);
         }
      }
   }

   for (unsigned i = 0; i < v->num_sampler_prefetch; i++) {
      unsigned n = util_last_bit(v->sampler_prefetch[i].wrmask) - 1;
      int32_t regid = v->sampler_prefetch[i].dst + n;
      if (v->sampler_prefetch[i].half_precision) {
         if (!v->mergedregs) {
            v->info.max_half_reg = MAX2(v->info.max_half_reg, regid >> 2);
         } else {
            v->info.max_reg = MAX2(v->info.max_reg, regid >> 3);
         }
      } else {
         v->info.max_reg = MAX2(v->info.max_reg, regid >> 2);
      }
   }

   /* TODO: for a5xx and below, is there a separate regfile for
    * half-registers?
    */
   unsigned regs_count =
      info->max_reg + 1 +
      (compiler->gen >= 6 ? ((info->max_half_reg + 2) / 2) : 0);

   info->double_threadsize = ir3_should_double_threadsize(v, regs_count);

   /* TODO this is different for earlier gens, but earlier gens don't use this */
   info->subgroup_size = v->info.double_threadsize ? 128 : 64;

   unsigned reg_independent_max_waves =
      ir3_get_reg_independent_max_waves(v, info->double_threadsize);
   unsigned reg_dependent_max_waves = ir3_get_reg_dependent_max_waves(
      compiler, regs_count, info->double_threadsize);
   info->max_waves = MIN2(reg_independent_max_waves, reg_dependent_max_waves);
   assert(info->max_waves <= v->compiler->max_waves);

   ralloc_free(mem_ctx);
}

static struct ir3_register *
reg_create(struct ir3 *shader, int num, int flags)
{
   struct ir3_register *reg = ir3_alloc(shader, sizeof(struct ir3_register));
   reg->wrmask = 1;
   reg->flags = flags;
   reg->num = num;
   return reg;
}

static void
insert_instr(struct ir3_cursor cursor, struct ir3_instruction *instr)
{
   struct ir3 *shader = instr->block->shader;

   instr->serialno = ++shader->instr_count;

   switch (cursor.option) {
   case IR3_CURSOR_BEFORE_BLOCK:
      list_add(&instr->node, &cursor.block->instr_list);
      break;
   case IR3_CURSOR_AFTER_BLOCK:
      list_addtail(&instr->node, &cursor.block->instr_list);
      break;
   case IR3_CURSOR_BEFORE_INSTR:
      list_addtail(&instr->node, &cursor.instr->node);
      break;
   case IR3_CURSOR_AFTER_INSTR:
      list_add(&instr->node, &cursor.instr->node);
      break;
   }

   if (is_input(instr))
      array_insert(shader, shader->baryfs, instr);
}

struct ir3_block *
ir3_block_create(struct ir3 *shader)
{
   struct ir3_block *block = ir3_alloc(shader, sizeof(*block));
#if MESA_DEBUG
   block->serialno = ++shader->block_count;
#endif
   block->shader = shader;
   list_inithead(&block->node);
   list_inithead(&block->instr_list);
   return block;
}

struct ir3_instruction *
ir3_find_end(struct ir3 *ir)
{
   foreach_block_rev (block, &ir->block_list) {
      foreach_instr_rev (instr, &block->instr_list) {
         if (instr->opc == OPC_END || instr->opc == OPC_CHMASK)
            return instr;
      }
   }
   UNREACHABLE("couldn't find end instruction");
}

static struct ir3_instruction *
block_get_last_instruction(struct ir3_block *block)
{
   if (list_is_empty(&block->instr_list))
      return NULL;
   return list_last_entry(&block->instr_list, struct ir3_instruction, node);
}

struct ir3_instruction *
ir3_block_get_terminator(struct ir3_block *block)
{
   struct ir3_instruction *last = block_get_last_instruction(block);

   if (last && is_terminator(last))
      return last;

   return NULL;
}

struct ir3_instruction *
ir3_block_take_terminator(struct ir3_block *block)
{
   struct ir3_instruction *terminator = ir3_block_get_terminator(block);

   if (terminator)
      list_delinit(&terminator->node);

   return terminator;
}

struct ir3_instruction *
ir3_block_get_last_non_terminator(struct ir3_block *block)
{
   struct ir3_instruction *last = block_get_last_instruction(block);

   if (!last)
      return NULL;

   if (!is_terminator(last))
      return last;

   if (last->node.prev != &block->instr_list)
      return list_entry(last->node.prev, struct ir3_instruction, node);

   return NULL;
}

struct ir3_instruction *
ir3_block_get_last_phi(struct ir3_block *block)
{
   struct ir3_instruction *last_phi = NULL;

   foreach_instr (instr, &block->instr_list) {
      if (instr->opc != OPC_META_PHI)
         break;

      last_phi = instr;
   }

   return last_phi;
}

struct ir3_instruction *
ir3_block_get_first_instr(struct ir3_block *block)
{
   if (list_is_empty(&block->instr_list)) {
      return NULL;
   }

   return list_first_entry(&block->instr_list, struct ir3_instruction, node);
}

struct ir3_instruction *
ir3_find_shpe(struct ir3 *ir)
{
   if (!ir3_has_preamble(ir)) {
      return NULL;
   }

   foreach_block (block, &ir->block_list) {
      struct ir3_instruction *last = ir3_block_get_terminator(block);

      if (last && last->opc == OPC_SHPE) {
         return last;
      }
   }

   UNREACHABLE("preamble without shpe");
}

struct ir3_instruction *
ir3_create_empty_preamble(struct ir3 *ir)
{
   assert(!ir3_has_preamble(ir));

   struct ir3_block *main_start_block = ir3_start_block(ir);

   /* Create a preamble CFG similar to what the frontend would generate. Note
    * that the empty else_block is important for ir3_after_preamble to work.
    *
    * shps_block:
    * if (shps) {
    *    getone_block:
    *    if (getone) {
    *       body_block:
    *       shpe
    *    }
    * } else {
    *    else_block:
    * }
    * main_start_block:
    */
   struct ir3_block *shps_block = ir3_block_create(ir);
   struct ir3_block *getone_block = ir3_block_create(ir);
   struct ir3_block *body_block = ir3_block_create(ir);
   struct ir3_block *else_block = ir3_block_create(ir);
   list_add(&else_block->node, &ir->block_list);
   list_add(&body_block->node, &ir->block_list);
   list_add(&getone_block->node, &ir->block_list);
   list_add(&shps_block->node, &ir->block_list);

   struct ir3_builder b = ir3_builder_at(ir3_after_block(shps_block));
   ir3_SHPS(&b);
   shps_block->successors[0] = getone_block;
   ir3_block_add_predecessor(getone_block, shps_block);
   ir3_block_link_physical(shps_block, getone_block);
   shps_block->successors[1] = else_block;
   ir3_block_add_predecessor(else_block, shps_block);
   ir3_block_link_physical(shps_block, else_block);

   b.cursor = ir3_after_block(getone_block);
   ir3_GETONE(&b);
   getone_block->divergent_condition = true;
   getone_block->successors[0] = body_block;
   ir3_block_add_predecessor(body_block, getone_block);
   ir3_block_link_physical(getone_block, body_block);
   getone_block->successors[1] = main_start_block;
   ir3_block_add_predecessor(main_start_block, getone_block);
   ir3_block_link_physical(getone_block, main_start_block);

   b.cursor = ir3_after_block(body_block);
   struct ir3_instruction *shpe = ir3_SHPE(&b);
   body_block->successors[0] = main_start_block;
   ir3_block_add_predecessor(main_start_block, body_block);
   ir3_block_link_physical(body_block, main_start_block);

   b.cursor = ir3_after_block(else_block);
   ir3_JUMP(&b);
   else_block->successors[0] = main_start_block;
   ir3_block_add_predecessor(main_start_block, else_block);
   ir3_block_link_physical(else_block, main_start_block);

   main_start_block->reconvergence_point = true;

   /* Inputs are always expected to be in the first block so move them there. */
   struct ir3_cursor inputs_cursor = ir3_before_terminator(shps_block);

   foreach_instr_safe (instr, &main_start_block->instr_list) {
      if (instr->opc == OPC_META_INPUT || instr->opc == OPC_META_TEX_PREFETCH) {
         list_del(&instr->node);
         insert_instr(inputs_cursor, instr);
         instr->block = shps_block;
      }
   }

   return shpe;
}

void
ir3_block_add_predecessor(struct ir3_block *block, struct ir3_block *pred)
{
   array_insert(block, block->predecessors, pred);
}

void
ir3_block_link_physical(struct ir3_block *pred,
                        struct ir3_block *succ)
{
   array_insert(pred, pred->physical_successors, succ);
   array_insert(succ, succ->physical_predecessors, pred);
}

void
ir3_block_remove_predecessor(struct ir3_block *block, struct ir3_block *pred)
{
   for (unsigned i = 0; i < block->predecessors_count; i++) {
      if (block->predecessors[i] == pred) {
         if (i < block->predecessors_count - 1) {
            block->predecessors[i] =
               block->predecessors[block->predecessors_count - 1];
         }

         block->predecessors_count--;
         return;
      }
   }
}

unsigned
ir3_block_get_pred_index(struct ir3_block *block, struct ir3_block *pred)
{
   for (unsigned i = 0; i < block->predecessors_count; i++) {
      if (block->predecessors[i] == pred) {
         return i;
      }
   }

   UNREACHABLE("ir3_block_get_pred_index() invalid predecessor");
}

static struct ir3_instruction *
instr_create(struct ir3_block *block, opc_t opc, int ndst, int nsrc)
{
   /* Add extra sources for array destinations and the address reg */
   if (1 <= opc_cat(opc))
      nsrc += 2;
   struct ir3_instruction *instr;
   unsigned sz = sizeof(*instr) + (ndst * sizeof(instr->dsts[0])) +
                 (nsrc * sizeof(instr->srcs[0]));
   char *ptr = ir3_alloc(block->shader, sz);

   instr = (struct ir3_instruction *)ptr;
   ptr += sizeof(*instr);
   instr->dsts = (struct ir3_register **)ptr;
   instr->srcs = instr->dsts + ndst;

#if MESA_DEBUG
   instr->dsts_max = ndst;
   instr->srcs_max = nsrc;
#endif

   list_inithead(&instr->rpt_node);
   return instr;
}

static void
add_to_address_users(struct ir3_instruction *instr)
{
   assert(instr->address != NULL);

   struct ir3 *ir = instr->block->shader;
   struct ir3_register *addr_reg = instr->address->def;
   assert(reg_num(addr_reg) == REG_A0);
   unsigned comp = reg_comp(addr_reg);
   if (comp == 0) {
      array_insert(ir, ir->a0_users, instr);
   } else {
      assert(comp == 1);
      array_insert(ir, ir->a1_users, instr);
   }
}

struct ir3_instruction *
ir3_instr_create_at(struct ir3_cursor cursor, opc_t opc, int ndst, int nsrc)
{
   struct ir3_block *block = ir3_cursor_current_block(cursor);
   struct ir3_instruction *instr = instr_create(block, opc, ndst, nsrc);
   instr->block = block;
   instr->opc = opc;
   insert_instr(cursor, instr);
   return instr;
}

struct ir3_instruction *
ir3_build_instr(struct ir3_builder *builder, opc_t opc, int ndst, int nsrc)
{
   struct ir3_instruction *instr =
      ir3_instr_create_at(builder->cursor, opc, ndst, nsrc);

   /* During instruction selection, instructions are sometimes emitted to blocks
    * other than the current one. For example, to predecessor blocks for phi
    * sources or to the first block for inputs. For those cases, a new builder
    * is created to emit at the end of the target block. However, if the target
    * block happens to be the same as the current block, the main builder would
    * not be updated to point past the new instructions. Therefore, don't update
    * the cursor when it points to the end of a block to ensure that new
    * instructions will always be added at the end.
    */
   if (builder->cursor.option != IR3_CURSOR_AFTER_BLOCK) {
      builder->cursor = ir3_after_instr(instr);
   }

   return instr;
}

struct ir3_instruction *
ir3_instr_create(struct ir3_block *block, opc_t opc, int ndst, int nsrc)
{
   return ir3_instr_create_at(ir3_before_terminator(block), opc, ndst, nsrc);
}

struct ir3_instruction *
ir3_instr_create_at_end(struct ir3_block *block, opc_t opc, int ndst, int nsrc)
{
   return ir3_instr_create_at(ir3_after_block(block), opc, ndst, nsrc);
}

struct ir3_instruction *
ir3_instr_clone(struct ir3_instruction *instr)
{
   struct ir3_instruction *new_instr = instr_create(
      instr->block, instr->opc, instr->dsts_count, instr->srcs_count);
   struct ir3_register **dsts, **srcs;

   dsts = new_instr->dsts;
   srcs = new_instr->srcs;
   *new_instr = *instr;
   new_instr->dsts = dsts;
   new_instr->srcs = srcs;
   new_instr->uses = NULL;
   list_inithead(&new_instr->rpt_node);

   insert_instr(ir3_before_terminator(instr->block), new_instr);

   /* clone registers: */
   new_instr->dsts_count = 0;
   new_instr->srcs_count = 0;
   foreach_dst (reg, instr) {
      struct ir3_register *new_reg =
         ir3_dst_create(new_instr, reg->num, reg->flags);
      *new_reg = *reg;
      if (new_reg->instr)
         new_reg->instr = new_instr;
   }
   foreach_src (reg, instr) {
      struct ir3_register *new_reg =
         ir3_src_create(new_instr, reg->num, reg->flags);
      *new_reg = *reg;
   }

   if (instr->address) {
      assert(instr->srcs_count > 0);
      new_instr->address = new_instr->srcs[instr->srcs_count - 1];
      add_to_address_users(new_instr);
   }

   return new_instr;
}

/* Add a false dependency to instruction, to ensure it is scheduled first: */
void
ir3_instr_add_dep(struct ir3_instruction *instr, struct ir3_instruction *dep)
{
   for (unsigned i = 0; i < instr->deps_count; i++) {
      if (instr->deps[i] == dep)
         return;
   }

   array_insert(instr, instr->deps, dep);
}

void
ir3_instr_remove(struct ir3_instruction *instr)
{
   list_delinit(&instr->node);
   list_delinit(&instr->rpt_node);
}

void
ir3_instr_create_rpt(struct ir3_instruction **instrs, unsigned n)
{
   assert(n > 0 && !ir3_instr_is_rpt(instrs[0]));

   for (unsigned i = 1; i < n; ++i) {
      assert(!ir3_instr_is_rpt(instrs[i]));
      assert(instrs[i]->serialno > instrs[i - 1]->serialno);

      list_addtail(&instrs[i]->rpt_node, &instrs[0]->rpt_node);
   }
}

bool
ir3_instr_is_rpt(const struct ir3_instruction *instr)
{
   return !list_is_empty(&instr->rpt_node);
}

bool
ir3_instr_is_first_rpt(const struct ir3_instruction *instr)
{
   if (!ir3_instr_is_rpt(instr))
      return false;

   struct ir3_instruction *prev_rpt =
      list_entry(instr->rpt_node.prev, struct ir3_instruction, rpt_node);
   return prev_rpt->serialno > instr->serialno;
}

struct ir3_instruction *
ir3_instr_prev_rpt(const struct ir3_instruction *instr)
{
   assert(ir3_instr_is_rpt(instr));

   if (ir3_instr_is_first_rpt(instr))
      return NULL;
   return list_entry(instr->rpt_node.prev, struct ir3_instruction, rpt_node);
}

struct ir3_instruction *
ir3_instr_first_rpt(struct ir3_instruction *instr)
{
   assert(ir3_instr_is_rpt(instr));

   while (!ir3_instr_is_first_rpt(instr)) {
      instr = ir3_instr_prev_rpt(instr);
      assert(instr);
   }

   return instr;
}

unsigned
ir3_instr_rpt_length(const struct ir3_instruction *instr)
{
   assert(ir3_instr_is_first_rpt(instr));

   return list_length(&instr->rpt_node) + 1;
}

struct ir3_register *
ir3_src_create(struct ir3_instruction *instr, int num, int flags)
{
   struct ir3 *shader = instr->block->shader;
#if MESA_DEBUG
   assert(instr->srcs_count < instr->srcs_max);
#endif
   struct ir3_register *reg = reg_create(shader, num, flags);
   instr->srcs[instr->srcs_count++] = reg;
   return reg;
}

struct ir3_register *
ir3_dst_create(struct ir3_instruction *instr, int num, int flags)
{
   struct ir3 *shader = instr->block->shader;
#if MESA_DEBUG
   assert(instr->dsts_count < instr->dsts_max);
#endif
   struct ir3_register *reg = reg_create(shader, num, flags);
   instr->dsts[instr->dsts_count++] = reg;
   return reg;
}

struct ir3_register *
ir3_reg_clone(struct ir3 *shader, struct ir3_register *reg)
{
   struct ir3_register *new_reg = reg_create(shader, 0, 0);
   *new_reg = *reg;
   return new_reg;
}

void
ir3_reg_set_last_array(struct ir3_instruction *instr, struct ir3_register *reg,
                       struct ir3_register *last_write)
{
   assert(reg->flags & IR3_REG_ARRAY);
   struct ir3_register *new_reg = ir3_src_create(instr, 0, 0);
   *new_reg = *reg;
   new_reg->def = last_write;
   ir3_reg_tie(reg, new_reg);
}

void
ir3_instr_set_address(struct ir3_instruction *instr,
                      struct ir3_instruction *addr)
{
   if (!instr->address) {
      assert(instr->block == addr->block);

      instr->address =
         ir3_src_create(instr, addr->dsts[0]->num, addr->dsts[0]->flags);
      instr->address->def = addr->dsts[0];
      add_to_address_users(instr);
   } else {
      assert(instr->address->def->instr == addr);
   }
}

struct ir3_instruction *
ir3_create_addr1(struct ir3_builder *build, unsigned const_val)
{
   struct ir3_instruction *immed =
      create_immed_typed(build, const_val, TYPE_U16);
   struct ir3_instruction *instr = ir3_MOV(build, immed, TYPE_U16);
   instr->dsts[0]->num = regid(REG_A0, 1);
   return instr;
}

static unsigned
dest_flags(struct ir3_instruction *instr)
{
   return instr->dsts[0]->flags & (IR3_REG_HALF | IR3_REG_SHARED);
}

struct ir3_instruction *
ir3_create_collect(struct ir3_builder *build,
                   struct ir3_instruction *const *arr, unsigned arrsz)
{
   struct ir3_instruction *collect;

   if (arrsz == 0)
      return NULL;

   if (arrsz == 1)
      return arr[0];

   int non_undef_src = -1;
   for (unsigned i = 0; i < arrsz; i++) {
      if (arr[i]) {
         non_undef_src = i;
         break;
      }
   }

   /* There should be at least one non-undef source to determine the type of the
    * destination.
    */
   assert(non_undef_src != -1);
   unsigned flags = dest_flags(arr[non_undef_src]);

   /* If any of the sources are themselves collects, flatten their sources into
    * the new collect. This is mainly useful for collects used for 64b values,
    * as we can treat them just like non-64b values when collecting them.
    */
   unsigned srcs_count = 0;

   for (unsigned i = 0; i < arrsz; i++) {
      if (arr[i] && arr[i]->opc == OPC_META_COLLECT) {
         srcs_count += arr[i]->srcs_count;
      } else {
         srcs_count++;
      }
   }

   struct ir3_instruction *srcs[srcs_count];

   for (unsigned i = 0, s = 0; i < arrsz; i++) {
      if (arr[i] && arr[i]->opc == OPC_META_COLLECT) {
         foreach_src (collect_src, arr[i]) {
            srcs[s++] = collect_src->def->instr;
         }
      } else {
         srcs[s++] = arr[i];
      }
   }

   collect = ir3_build_instr(build, OPC_META_COLLECT, 1, srcs_count);
   __ssa_dst(collect)->flags |= flags;
   for (unsigned i = 0; i < srcs_count; i++) {
      struct ir3_instruction *elem = srcs[i];

      /* Since arrays are pre-colored in RA, we can't assume that
       * things will end up in the right place.  (Ie. if a collect
       * joins elements from two different arrays.)  So insert an
       * extra mov.
       *
       * We could possibly skip this if all the collected elements
       * are contiguous elements in a single array.. not sure how
       * likely that is to happen.
       *
       * Fixes a problem with glamor shaders, that in effect do
       * something like:
       *
       *   if (foo)
       *     texcoord = ..
       *   else
       *     texcoord = ..
       *   color = texture2D(tex, texcoord);
       *
       * In this case, texcoord will end up as nir registers (which
       * translate to ir3 array's of length 1.  And we can't assume
       * the two (or more) arrays will get allocated in consecutive
       * scalar registers.
       *
       */
      if (elem && elem->dsts[0]->flags & IR3_REG_ARRAY) {
         type_t type = (flags & IR3_REG_HALF) ? TYPE_U16 : TYPE_U32;
         elem = ir3_MOV(build, elem, type);
      }

      if (elem) {
         assert(dest_flags(elem) == flags);
         __ssa_src(collect, elem, flags);
      } else {
         ir3_src_create(collect, INVALID_REG, flags | IR3_REG_SSA);
      }
   }

   collect->dsts[0]->wrmask = MASK(srcs_count);

   return collect;
}

/* helper for instructions that produce multiple consecutive scalar
 * outputs which need to have a split meta instruction inserted
 */
void
ir3_split_dest(struct ir3_builder *build, struct ir3_instruction **dst,
               struct ir3_instruction *src, unsigned base, unsigned n)
{
   if ((n == 1) && (src->dsts[0]->wrmask == 0x1) &&
       /* setup_input needs ir3_split_dest to generate a SPLIT instruction */
       src->opc != OPC_META_INPUT) {
      dst[0] = src;
      return;
   }

   if (src->opc == OPC_META_COLLECT) {
      assert((base + n) <= src->srcs_count);

      for (int i = 0; i < n; i++) {
         dst[i] = ssa(src->srcs[i + base]);
      }

      return;
   }

   unsigned flags = dest_flags(src);

   for (int i = 0, j = 0; i < n; i++) {
      struct ir3_instruction *split =
         ir3_build_instr(build, OPC_META_SPLIT, 1, 1);
      __ssa_dst(split)->flags |= flags;
      __ssa_src(split, src, flags);
      split->split.off = i + base;

      if (src->dsts[0]->wrmask & (1 << (i + base)))
         dst[j++] = split;
   }
}

/* Split off the first 1 (bit_size < 64) or 2 (bit_size == 64) components from
 * src and create a new 32b or 64b value.
 */
struct ir3_instruction *
ir3_split_off_scalar(struct ir3_builder *build, struct ir3_instruction *src,
                     unsigned bit_size)
{
   unsigned num_comps = bit_size == 64 ? 2 : 1;
   assert((src->dsts[0]->wrmask & MASK(num_comps)) == MASK(num_comps));

   if (num_comps == 1 && src->dsts[0]->wrmask == 0x1) {
      return src;
   }

   struct ir3_instruction *comps[num_comps];
   ir3_split_dest(build, comps, src, 0, num_comps);
   return bit_size == 64 ? ir3_64b(build, comps[0], comps[1]) : comps[0];
}

struct ir3_instruction *
ir3_store_const(struct ir3_shader_variant *so, struct ir3_builder *build,
                struct ir3_instruction *src, unsigned dst)
{
   unsigned dst_lo = dst & 0xff;
   unsigned dst_hi = dst >> 8;
   unsigned components = util_last_bit(src->dsts[0]->wrmask);

   struct ir3_instruction *a1 = NULL;
   if (dst_hi) {
      /* Encode only the high part of the destination in a1.x to increase the
       * chance that we can reuse the a1.x value in subsequent stc
       * instructions.
       */
      a1 = ir3_create_addr1(build, dst_hi << 8);
   }

   struct ir3_instruction *stc =
      ir3_STC(build, create_immed(build, dst_lo), 0, src, 0);
   stc->cat6.iim_val = components;
   stc->cat6.type = TYPE_U32;
   stc->barrier_conflict = IR3_BARRIER_CONST_W;

   /* This isn't necessary for instruction encoding but is used by ir3_sched to
    * set up dependencies between stc and const reads.
    */
   stc->cat6.dst_offset = dst;

   if (a1) {
      ir3_instr_set_address(stc, a1);
      stc->flags |= IR3_INSTR_A1EN;
   }

   /* The assembler isn't aware of what value a1.x has, so make sure that
    * constlen includes the stc here.
    */
   so->constlen = MAX2(so->constlen, DIV_ROUND_UP(dst + components, 4));
   struct ir3_block *block = ir3_cursor_current_block(build->cursor);
   array_insert(block, block->keeps, stc);
   return stc;
}

/* Does this instruction use the scalar ALU?
 */
bool
is_scalar_alu(struct ir3_instruction *instr,
              const struct ir3_compiler *compiler)
{
   /* MOVMSK seems to always need (ss) even with other scalar ALU instructions
    */
   return instr->opc != OPC_MOVMSK &&
      instr->opc != OPC_SCAN_CLUSTERS_MACRO &&
      instr->opc != OPC_SCAN_MACRO &&
      instr->opc != OPC_MOVS &&
      is_alu(instr) && (instr->dsts[0]->flags & IR3_REG_SHARED) &&
      /* scalar->scalar mov instructions (but NOT cov) were supported before the
       * scalar ALU was supported, but they still required (ss) whereas on GPUs
       * that have a scalar ALU they are executed on it and do not require (ss).
       * We have to be careful to return false for these if scalar ALU isn't
       * supported, so that we treat them like vector->scalar mov instructions
       * (such as requiring (ss)).
       */
      compiler->has_scalar_alu &&
      /* moves from normal to shared seem to use a separate ALU as before and
       * require a (ss) on dependent instructions.
       */
      ((instr->opc != OPC_MOV && !is_subgroup_cond_mov_macro(instr)) ||
       (instr->srcs[0]->flags & (IR3_REG_SHARED | IR3_REG_IMMED | IR3_REG_CONST)));
}

void
ir3_block_clear_mark(struct ir3_block *block)
{
   foreach_instr (instr, &block->instr_list)
      instr->flags &= ~IR3_INSTR_MARK;
}

void
ir3_clear_mark(struct ir3 *ir)
{
   foreach_block (block, &ir->block_list) {
      ir3_block_clear_mark(block);
   }
}

unsigned
ir3_count_instructions(struct ir3 *ir)
{
   unsigned cnt = 1;
   foreach_block (block, &ir->block_list) {
      block->start_ip = cnt;
      foreach_instr (instr, &block->instr_list) {
         instr->ip = cnt++;
      }
      block->end_ip = cnt;
   }
   return cnt;
}

unsigned
ir3_count_instructions_sched(struct ir3 *ir)
{
   unsigned cnt = 1;
   foreach_block (block, &ir->block_list) {
      block->start_ip = cnt;
      foreach_instr (instr, &block->instr_list) {
         if (!is_terminator(instr))
            instr->ip = cnt++;
      }
      block->end_ip = cnt;
   }
   return cnt;
}

/* When counting instructions for RA, we insert extra fake instructions at the
 * beginning of each block, where values become live, and at the end where
 * values die. This prevents problems where values live-in at the beginning or
 * live-out at the end of a block from being treated as if they were
 * live-in/live-out at the first/last instruction, which would be incorrect.
 * In ir3_legalize these ip's are assumed to be actual ip's of the final
 * program, so it would be incorrect to use this everywhere.
 */

unsigned
ir3_count_instructions_ra(struct ir3 *ir)
{
   unsigned cnt = 1;
   foreach_block (block, &ir->block_list) {
      block->start_ip = cnt++;
      foreach_instr (instr, &block->instr_list) {
         instr->ip = cnt++;
      }
      block->end_ip = cnt++;
   }
   return cnt;
}

struct ir3_array *
ir3_lookup_array(struct ir3 *ir, unsigned id)
{
   foreach_array (arr, &ir->array_list)
      if (arr->id == id)
         return arr;
   return NULL;
}

void ir3_find_ssa_uses_for(struct ir3 *ir, void *mem_ctx, use_filter_cb filter)
{
   /* We could do this in a single pass if we can assume instructions
    * are always sorted.  Which currently might not always be true.
    * (In particular after ir3_group pass, but maybe other places.)
    */
   foreach_block (block, &ir->block_list)
      foreach_instr (instr, &block->instr_list)
         instr->uses = NULL;

   foreach_block (block, &ir->block_list) {
      foreach_instr (instr, &block->instr_list) {
         foreach_ssa_src_n (src, n, instr) {
            if (!filter(instr, n))
               continue;
            if (!src->uses)
               src->uses = _mesa_pointer_set_create(mem_ctx);
            _mesa_set_add(src->uses, instr);
         }
      }
   }
}

static bool
no_false_deps(struct ir3_instruction *instr, unsigned src_n)
{
   return !__is_false_dep(instr, src_n);
}

static bool
any_src(struct ir3_instruction *instr, unsigned src_n)
{
   return true;
}

void
ir3_find_ssa_uses(struct ir3 *ir, void *mem_ctx, bool falsedeps)
{
   if (falsedeps)
      return ir3_find_ssa_uses_for(ir, mem_ctx, any_src);
   return ir3_find_ssa_uses_for(ir, mem_ctx, no_false_deps);
}

/**
 * Set the destination type of an instruction, for example if a
 * conversion is folded in, handling the special cases where the
 * instruction's dest type or opcode needs to be fixed up.
 */
void
ir3_set_dst_type(struct ir3_instruction *instr, bool half)
{
   if (half) {
      instr->dsts[0]->flags |= IR3_REG_HALF;
   } else {
      instr->dsts[0]->flags &= ~IR3_REG_HALF;
   }

   switch (opc_cat(instr->opc)) {
   case 1: /* move instructions */
      if (half) {
         instr->cat1.dst_type = half_type(instr->cat1.dst_type);
      } else {
         instr->cat1.dst_type = full_type(instr->cat1.dst_type);
      }
      break;
   case 4:
      if (half) {
         instr->opc = cat4_half_opc(instr->opc);
      } else {
         instr->opc = cat4_full_opc(instr->opc);
      }
      break;
   case 5:
      if (half) {
         instr->cat5.type = half_type(instr->cat5.type);
      } else {
         instr->cat5.type = full_type(instr->cat5.type);
      }
      break;
   }
}

/**
 * One-time fixup for instruction src-types.  Other than cov's that
 * are folded, an instruction's src type does not change.
 */
void
ir3_fixup_src_type(struct ir3_instruction *instr)
{
   if (instr->srcs_count == 0)
      return;

   switch (opc_cat(instr->opc)) {
   case 1: /* move instructions */
      if (instr->srcs[0]->flags & IR3_REG_HALF) {
         instr->cat1.src_type = half_type(instr->cat1.src_type);
      } else {
         instr->cat1.src_type = full_type(instr->cat1.src_type);
      }
      break;
   case 3:
      if (instr->srcs[0]->flags & IR3_REG_HALF) {
         instr->opc = cat3_half_opc(instr->opc);
      } else {
         instr->opc = cat3_full_opc(instr->opc);
      }
      break;
   }
}

/**
 * Map a floating point immed to FLUT (float lookup table) value,
 * returns negative for immediates that cannot be mapped.
 */
int
ir3_flut(struct ir3_register *src_reg)
{
   static const struct {
      uint32_t f32;
      uint16_t f16;
   } flut[] = {
         { .f32 = 0x00000000, .f16 = 0x0000 },    /* 0.0 */
         { .f32 = 0x3f000000, .f16 = 0x3800 },    /* 0.5 */
         { .f32 = 0x3f800000, .f16 = 0x3c00 },    /* 1.0 */
         { .f32 = 0x40000000, .f16 = 0x4000 },    /* 2.0 */
         { .f32 = 0x402df854, .f16 = 0x4170 },    /* e */
         { .f32 = 0x40490fdb, .f16 = 0x4248 },    /* pi */
         { .f32 = 0x3ea2f983, .f16 = 0x3518 },    /* 1/pi */
         { .f32 = 0x3f317218, .f16 = 0x398c },    /* 1/log2(e) */
         { .f32 = 0x3fb8aa3b, .f16 = 0x3dc5 },    /* log2(e) */
         { .f32 = 0x3e9a209b, .f16 = 0x34d1 },    /* 1/log2(10) */
         { .f32 = 0x40549a78, .f16 = 0x42a5 },    /* log2(10) */
         { .f32 = 0x40800000, .f16 = 0x4400 },    /* 4.0 */
   };

   if (src_reg->flags & IR3_REG_HALF) {
      /* Note that half-float immeds are already lowered to 16b in nir: */
      uint32_t imm = src_reg->uim_val;
      for (unsigned i = 0; i < ARRAY_SIZE(flut); i++) {
         if (flut[i].f16 == imm) {
            return i;
         }
      }
   } else {
      uint32_t imm = src_reg->uim_val;
      for (unsigned i = 0; i < ARRAY_SIZE(flut); i++) {
         if (flut[i].f32 == imm) {
            return i;
         }
      }
   }

   return -1;
}

static unsigned
cp_flags(unsigned flags)
{
   /* only considering these flags (at least for now): */
   flags &= (IR3_REG_CONST | IR3_REG_IMMED | IR3_REG_FNEG | IR3_REG_FABS |
             IR3_REG_SNEG | IR3_REG_SABS | IR3_REG_BNOT | IR3_REG_RELATIV |
             IR3_REG_SHARED);
   return flags;
}

bool
ir3_valid_flags(struct ir3_instruction *instr, unsigned n, unsigned flags)
{
   struct ir3_compiler *compiler = instr->block->shader->compiler;
   unsigned valid_flags;

   flags = cp_flags(flags);

   /* If destination is indirect, then source cannot be.. at least
    * I don't think so..
    */
   if (instr->dsts_count > 0 && (instr->dsts[0]->flags & IR3_REG_RELATIV) &&
       (flags & IR3_REG_RELATIV))
      return false;

   if (flags & IR3_REG_RELATIV) {
      /* TODO need to test on earlier gens.. pretty sure the earlier
       * problem was just that we didn't check that the src was from
       * same block (since we can't propagate address register values
       * across blocks currently)
       */
      if (compiler->gen < 6)
         return false;

      /* NOTE in the special try_swap_mad_two_srcs() case we can be
       * called on a src that has already had an indirect load folded
       * in, in which case ssa() returns NULL
       */
      if (instr->srcs[n]->flags & IR3_REG_SSA) {
         struct ir3_instruction *src = ssa(instr->srcs[n]);
         if (src->address->def->instr->block != instr->block)
            return false;
      }
   }

   if (is_meta(instr)) {
      /* collect and phi nodes support const/immed sources, which will be
       * turned into move instructions, but not anything else.
       */
      if (flags & ~(IR3_REG_IMMED | IR3_REG_CONST | IR3_REG_SHARED))
         return false;

      /* Except for immed/const sources, source and dest shared-ness must match.
       */
      if (!(flags & (IR3_REG_IMMED | IR3_REG_CONST)) &&
          (flags & IR3_REG_SHARED) != (instr->dsts[0]->flags & IR3_REG_SHARED))
         return false;

      return true;
   }

   switch (opc_cat(instr->opc)) {
   case 0: /* end, chmask */
      return flags == 0;
   case 1:
      switch (instr->opc) {
      case OPC_MOVMSK:
      case OPC_SWZ:
      case OPC_SCT:
      case OPC_GAT:
         valid_flags = IR3_REG_SHARED;
         break;
      case OPC_SCAN_MACRO:
         if (n == 0)
            return flags == 0;
         else
            return flags == IR3_REG_SHARED;
         break;
      case OPC_SCAN_CLUSTERS_MACRO:
         if (n == 0)
            return flags == IR3_REG_SHARED;
         else
            return flags == 0;
         break;
      case OPC_MOVS:
         if (n == 0) {
            valid_flags = IR3_REG_SHARED;
         } else {
            valid_flags = IR3_REG_IMMED;
         }
         break;
      default: {
         valid_flags =
            IR3_REG_IMMED | IR3_REG_CONST | IR3_REG_RELATIV | IR3_REG_SHARED;

         /* floating-point conversions when moving from non-shared to shared
          * seem not to work. We only use floating-point types in ir3 for
          * conversions, so don't bother specially handling the case where the
          * types are equal. Same goes for 8-bit sign extension.
          */
         if ((instr->dsts[0]->flags & IR3_REG_SHARED) &&
             !(flags & (IR3_REG_SHARED | IR3_REG_IMMED | IR3_REG_CONST)) &&
             ((full_type(instr->cat1.src_type) == TYPE_F32 ||
               full_type(instr->cat1.dst_type) == TYPE_F32) ||
              (instr->cat1.src_type == TYPE_U8 &&
               full_type(instr->cat1.dst_type) == TYPE_S32)))
            return false;

         /* Conversions seem not to work in shared->shared copies before scalar
          * ALU is supported.
          */
         if (!compiler->has_scalar_alu &&
             (flags & IR3_REG_SHARED) &&
             (instr->dsts[0]->flags & IR3_REG_SHARED) &&
             instr->cat1.src_type != instr->cat1.dst_type)
            return false;
      }
      }
      if (flags & ~valid_flags)
         return false;
      break;
   case 2:
      valid_flags = ir3_cat2_absneg(instr->opc) | IR3_REG_CONST |
                    IR3_REG_RELATIV | IR3_REG_IMMED | IR3_REG_SHARED;

      if (flags & ~valid_flags)
         return false;

      /* Allow an immediate src1 for flat.b, since it's ignored */
      if (instr->opc == OPC_FLAT_B &&
          n == 1 && flags == IR3_REG_IMMED)
         return true;

      /* cat2/cat3 scalar ALU instructions must not have regular sources. */
      if (instr->dsts[0]->flags & IR3_REG_SHARED) {
         if (!(flags & (IR3_REG_SHARED | IR3_REG_IMMED | IR3_REG_CONST)))
            return false;
      }

      if (flags & (IR3_REG_CONST | IR3_REG_IMMED | IR3_REG_SHARED)) {
         unsigned m = n ^ 1;
         /* cannot deal w/ const or shared in both srcs:
          * (note that some cat2 actually only have a single src)
          */
         if (m < instr->srcs_count) {
            struct ir3_register *reg = instr->srcs[m];
            if (instr->dsts[0]->flags & IR3_REG_SHARED) {
               if ((flags & IR3_REG_CONST) && (reg->flags & IR3_REG_CONST))
                  return false;
            } else {
               if ((flags & (IR3_REG_CONST | IR3_REG_SHARED)) &&
                   (reg->flags & (IR3_REG_CONST | IR3_REG_SHARED)))
                  return false;
            }
            if ((flags & IR3_REG_IMMED) && reg->flags & (IR3_REG_IMMED))
               return false;
         }
      }
      break;
   case 3:
      valid_flags =
         ir3_cat3_absneg(instr->opc, n) | IR3_REG_RELATIV | IR3_REG_SHARED;

      switch (instr->opc) {
      case OPC_SHRM:
      case OPC_SHLM:
      case OPC_SHRG:
      case OPC_SHLG:
      case OPC_ANDG: {
         if (n != 1) {
            valid_flags |= IR3_REG_IMMED;
         }

         /* Can be RELATIV+CONST but not CONST: */
         if (flags & IR3_REG_RELATIV)
            valid_flags |= IR3_REG_CONST;

         if (!(instr->dsts[0]->flags & IR3_REG_SHARED) && n < 2) {
            /* Of the first two sources, only one can be shared. */
            unsigned m = n ^ 1;

            if ((flags & IR3_REG_SHARED) &&
                (instr->srcs[m]->flags & IR3_REG_SHARED)) {
               return false;
            }
         }
         break;
      }
      case OPC_WMM:
      case OPC_WMM_ACCU: {
         valid_flags = IR3_REG_SHARED;
         if (n == 2)
            valid_flags = IR3_REG_CONST;
         break;
      }
      case OPC_DP2ACC:
      case OPC_DP4ACC:
         break;
      default:
         valid_flags |= IR3_REG_CONST;
      }

      if (flags & ~valid_flags)
         return false;

      if (flags & (IR3_REG_CONST | IR3_REG_RELATIV) ||
          (!(instr->dsts[0]->flags & IR3_REG_SHARED) &&
           (flags & IR3_REG_SHARED))) {
         /* cannot deal w/ const/shared/relativ in 2nd src: */
         if (n == 1)
            return false;
      }

      if (instr->dsts[0]->flags & IR3_REG_SHARED) {
         if (!(flags & (IR3_REG_SHARED | IR3_REG_IMMED | IR3_REG_CONST)))
            return false;
      }

      break;
   case 4:
      if ((instr->dsts[0]->flags & IR3_REG_SHARED) != (flags & IR3_REG_SHARED))
         return false;
      /* seems like blob compiler avoids const as src.. */
      /* TODO double check if this is still the case on a4xx */
      if (flags & (IR3_REG_CONST | IR3_REG_IMMED))
         return false;
      if (flags & (IR3_REG_SABS | IR3_REG_SNEG))
         return false;
      break;
   case 5:
      if (instr->opc == OPC_ISAM && (instr->flags & IR3_INSTR_V)) {
         if (((instr->flags & IR3_INSTR_S2EN) && n == 2) ||
             (!(instr->flags & IR3_INSTR_S2EN) && n == 1)) {
            return flags == IR3_REG_IMMED;
         }
      }
      /* no flags allowed */
      if (flags)
         return false;
      break;
   case 6:
      valid_flags = IR3_REG_IMMED;

      if (instr->opc == OPC_STC && n == 1)
         valid_flags |= IR3_REG_SHARED;
      if (instr->opc == OPC_SHFL) {
         if (n == 0)
            valid_flags &= ~IR3_REG_IMMED;
         else if (n == 1)
            valid_flags |= IR3_REG_SHARED;
      }

      if (flags & ~valid_flags)
         return false;

      if (flags & IR3_REG_IMMED) {
         /* doesn't seem like we can have immediate src for store
          * instructions:
          *
          * TODO this restriction could also apply to load instructions,
          * but for load instructions this arg is the address (and not
          * really sure any good way to test a hard-coded immed addr src)
          */
         if (is_store(instr) && (instr->opc != OPC_STG) && (n == 1))
            return false;

         if ((instr->opc == OPC_LDL) && (n == 0))
            return false;

         if ((instr->opc == OPC_STL) && (n != 2))
            return false;

         if ((instr->opc == OPC_LDP) && (n == 0))
            return false;

         if ((instr->opc == OPC_STP) && (n != 2))
            return false;

         if (instr->opc == OPC_STLW && n == 0)
            return false;

         if (instr->opc == OPC_LDLW && n == 0)
            return false;

         /* disallow immediates in anything but the SSBO slot argument for
          * cat6 instructions:
          */
         if (is_global_a3xx_atomic(instr->opc) && (n != 0))
            return false;

         if (is_local_atomic(instr->opc) || is_global_a6xx_atomic(instr->opc) ||
             is_bindless_atomic(instr->opc))
            return false;

         if (instr->opc == OPC_STG && (n == 2))
            return false;

         if (instr->opc == OPC_STG_A && (n == 4))
            return false;

         if (instr->opc == OPC_LDG && (n == 0))
            return false;

         if (instr->opc == OPC_LDG_A && (n < 2))
            return false;

         if (instr->opc == OPC_STC && n != 0)
            return false;

         /* as with atomics, these cat6 instrs can only have an immediate
          * for SSBO/UAV slot argument
          */
         switch (instr->opc) {
         case OPC_LDIB:
         case OPC_STIB:
            if (n != 0 && n != 2)
               return false;
            break;
         case OPC_RESINFO:
            if (n != 0)
               return false;
            break;
         default:
            break;
         }
      }

      break;
   }

   return true;
}

bool
ir3_valid_immediate(struct ir3_instruction *instr, int32_t immed)
{
   if (instr->opc == OPC_MOV || is_meta(instr) || instr->opc == OPC_ALIAS)
      return true;

   if (is_mem(instr)) {
      switch (instr->opc) {
      /* Some load/store instructions have a 13-bit offset and size which must
       * always be an immediate and the rest of the sources cannot be
       * immediates, so the frontend is responsible for checking the size:
       */
      case OPC_LDL:
      case OPC_STL:
      case OPC_LDP:
      case OPC_STP:
      case OPC_LDG:
      case OPC_STG:
      case OPC_SPILL_MACRO:
      case OPC_RELOAD_MACRO:
      case OPC_LDG_A:
      case OPC_STG_A:
      case OPC_LDLW:
      case OPC_STLW:
      case OPC_LDLV:
         return true;
      default:
         /* most cat6 src immediates can only encode 8 bits: */
         return !(immed & ~0xff);
      }
   }

   /* The alternative cat3 encoding used for sh[lr][gm]/andg uses 12 bit
    * immediates that won't be sign-extended.
    */
   if (is_cat3_alt(instr->opc)) {
      return !(immed & ~0xfff);
   }

   /* Other than cat1 (mov) we can only encode up to 10 bits, sign-extended: */
   return !(immed & ~0x1ff) || !(-immed & ~0x1ff);
}

struct ir3_instruction *
ir3_get_cond_for_nonzero_compare(struct ir3_instruction *instr)
{
   /* If instr is a negation (likely as a result of an nir_b2n), we can ignore
    * that and use its source, since the nonzero-ness stays the same.
    */
   if (instr->opc == OPC_ABSNEG_S && instr->flags == 0 &&
       (instr->srcs[0]->flags & (IR3_REG_SNEG | IR3_REG_SABS)) ==
          IR3_REG_SNEG) {
      return instr->srcs[0]->def->instr;
   }

   return instr;
}

bool
ir3_supports_rpt(struct ir3_compiler *compiler, unsigned opc)
{
   switch (opc_cat(opc)) {
   case 0:
      return opc == OPC_NOP;
   case 1:
      return opc == OPC_MOV || opc == OPC_SWZ || opc == OPC_MOVMSK;
   case 2:
      if (opc == OPC_BARY_F && !compiler->has_rpt_bary_f)
         return false;
      return true;
   case 3:
      return opc != OPC_DP2ACC && opc != OPC_DP4ACC;
   case 4:
      return opc != OPC_RCP;
   default:
      return false;
   }
}

static bool
is_unmodified_full_gpr(struct ir3_register *src)
{
   return !(src->flags & (IR3_REG_HALF | IR3_REG_CONST | IR3_REG_IMMED |
                          IR3_REG_RELATIV | IR3_REG_FNEG | IR3_REG_FABS |
                          IR3_REG_SNEG | IR3_REG_SABS | IR3_REG_BNOT));
}

/* Does `instr` move half of its full GPR src to its half dst? If this is the
 * case, and RA assigns overlapping registers to src and dst, the instruction
 * can be removed in mergedregs mode.
 */
enum ir3_subreg_move
ir3_is_subreg_move(struct ir3_instruction *instr)
{
   if (instr->opc == OPC_MOV) {
      /* `cov.u32u16 hdst, src`: moves lower half of src to hdst. */
      struct ir3_register *src = instr->srcs[0];
      struct ir3_register *dst = instr->dsts[0];

      if (instr->cat1.src_type == TYPE_U32 &&
          instr->cat1.dst_type == TYPE_U16 && is_unmodified_full_gpr(src) &&
          (src->flags & IR3_REG_SHARED) == (dst->flags & IR3_REG_SHARED)) {
         return IR3_SUBREG_MOVE_LOWER;
      }
   } else if (instr->opc == OPC_SHR_B || instr->opc == OPC_ASHR_B) {
      /* `[a]shr.b hdst, src, 16`: moves upper half of src to hdst. */
      struct ir3_register *src = instr->srcs[0];
      struct ir3_register *shamt = instr->srcs[1];
      struct ir3_register *dst = instr->dsts[0];

      if ((dst->flags & IR3_REG_HALF) && is_unmodified_full_gpr(src) &&
          ((src->flags & IR3_REG_SHARED) == (dst->flags & IR3_REG_SHARED)) &&
          (shamt->flags & IR3_REG_IMMED) && shamt->uim_val == 16) {
         return IR3_SUBREG_MOVE_UPPER;
      }
   }

   return IR3_SUBREG_MOVE_NONE;
}

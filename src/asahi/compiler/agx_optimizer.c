/*
 * Copyright 2021 Alyssa Rosenzweig
 * SPDX-License-Identifier: MIT
 */

#include "util/macros.h"
#include "agx_builder.h"
#include "agx_compiler.h"
#include "agx_opcodes.h"

/* AGX peephole optimizer responsible for instruction combining. It operates in
 * a forward direction and a backward direction, in each case traversing in
 * source order. SSA means the forward pass satisfies the invariant:
 *
 *    Every def is visited before any of its uses.
 *
 * Dually, the backend pass satisfies the invariant:
 *
 *    Every use of a def is visited before the def.
 *
 * This means the forward pass can propagate modifiers forward, whereas the
 * backwards pass propagates modifiers backward. Consider an example:
 *
 *    1 = fabs 0
 *    2 = fround 1
 *    3 = fsat 1
 *
 * The forwards pass would propagate the fabs to the fround (since we can
 * lookup the fabs from the fround source and do the replacement). By contrast
 * the backwards pass would propagate the fsat back to the fround (since when
 * we see the fround we know it has only a single user, fsat).  Propagatable
 * instruction have natural directions (like pushforwards and pullbacks).
 *
 * We are careful to update the tracked state whenever we modify an instruction
 * to ensure the passes are linear-time and converge in a single iteration.
 *
 * Size conversions are worth special discussion. Consider the snippet:
 *
 *    2 = fadd 0, 1
 *    3 = f2f16 2
 *    4 = fround 3
 *
 * A priori, we can move the f2f16 in either direction. But it's not equal --
 * if we move it up to the fadd, we get FP16 for two instructions, whereas if
 * we push it into the fround, we effectively get FP32 for two instructions. So
 * f2f16 is backwards. Likewise, consider
 *
 *    2 = fadd 0, 1
 *    3 = f2f32 1
 *    4 = fround 3
 *
 * This time if we move f2f32 up to the fadd, we get FP32 for two, but if we
 * move it down to the fround, we get FP16 to too. So f2f32 is backwards.
 */

static bool
agx_is_fmov(agx_instr *def)
{
   return (def->op == AGX_OPCODE_FADD || def->op == AGX_OPCODE_HADD) &&
          agx_is_equiv(def->src[1], agx_negzero());
}

/* Compose floating-point modifiers with floating-point sources */

static agx_index
agx_compose_float_src(agx_index to, agx_index from)
{
   if (to.abs) {
      from.neg = false;
      from.abs = true;
   }

   from.neg ^= to.neg;

   return from;
}

static void
agx_optimizer_fmov(agx_instr **defs, agx_instr *ins)
{
   agx_foreach_ssa_src(ins, s) {
      agx_index src = ins->src[s];
      agx_instr *def = defs[src.value];

      if (def == NULL)
         continue; /* happens for phis in loops */
      if (!agx_is_fmov(def))
         continue;
      if (def->saturate)
         continue;
      if (ins->op == AGX_OPCODE_FCMPSEL && s >= 2)
         continue;

      /* We can fold f2f32 into 32-bit instructions, but we can't fold f2f16
       * into 16-bit instructions, since the latter would implicitly promote to
       * a 32-bit instruction which is not exact.
       */
      assert(def->src[0].size == AGX_SIZE_32 ||
             def->src[0].size == AGX_SIZE_16);
      assert(src.size == AGX_SIZE_32 || src.size == AGX_SIZE_16);

      if (src.size == AGX_SIZE_16 && def->src[0].size == AGX_SIZE_32)
         continue;

      ins->src[s] = agx_compose_float_src(src, def->src[0]);
   }
}

static bool
image_write_source_can_be_immediate(agx_instr *I, unsigned s)
{
   bool block = I->op == AGX_OPCODE_BLOCK_IMAGE_STORE;
   assert(I->op == AGX_OPCODE_IMAGE_WRITE || block);

   /* LOD can always be immediate. Actually, it's just zero so far, we don't
    * support nonzero LOD for images yet.
    */
   if (s == 2 && !block)
      return true;

   /* If the "bindless" source (source 3) is an immediate, it means we don't
    * have a bindless image, instead we have a texture state index. We're
    * allowed to have immediate texture state registers (source 4). However,
    * we're not allowed to have immediate bindless offsets (also source 4).
    */
   unsigned base = block ? 0 : 3;
   bool is_texture_state = (I->src[base].type == AGX_INDEX_IMMEDIATE);
   if (s == (base + 1) && is_texture_state)
      return true;

   /* Otherwise, must be from a register */
   return false;
}

static void
agx_optimizer_inline_imm(agx_instr **defs, agx_instr *I)
{
   agx_foreach_ssa_src(I, s) {
      agx_index src = I->src[s];
      if (src.neg)
         continue;

      agx_instr *def = defs[src.value];
      if (!def || def->op != AGX_OPCODE_MOV_IMM)
         continue;

      uint8_t value = def->imm;
      uint16_t value_u16 = def->imm;

      bool float_src = agx_is_float_src(I, s);

      if (I->op == AGX_OPCODE_ST_TILE && s == 0)
         continue;
      if (I->op == AGX_OPCODE_ZS_EMIT && s != 0)
         continue;
      if (I->op == AGX_OPCODE_TEXTURE_SAMPLE && s != 4)
         continue;
      if ((I->op == AGX_OPCODE_DEVICE_STORE ||
           I->op == AGX_OPCODE_LOCAL_STORE || I->op == AGX_OPCODE_ATOMIC ||
           I->op == AGX_OPCODE_LOCAL_ATOMIC) &&
          s != 2)
         continue;
      if (I->op == AGX_OPCODE_ST_VARY && s != 0)
         continue;
      if ((I->op == AGX_OPCODE_LOCAL_LOAD || I->op == AGX_OPCODE_DEVICE_LOAD ||
           I->op == AGX_OPCODE_STACK_STORE) &&
          s != 1)
         continue;
      if (I->op == AGX_OPCODE_SPLIT)
         continue;

      if ((I->op == AGX_OPCODE_IMAGE_WRITE ||
           I->op == AGX_OPCODE_BLOCK_IMAGE_STORE) &&
          !image_write_source_can_be_immediate(I, s))
         continue;

      if (float_src) {
         bool fp16 = (def->dest[0].size == AGX_SIZE_16);
         assert(fp16 || (def->dest[0].size == AGX_SIZE_32));

         float f = fp16 ? _mesa_half_to_float(def->imm) : uif(def->imm);
         if (!agx_minifloat_exact(f))
            continue;

         I->src[s] = agx_immediate_f(f);
      } else if (value == def->imm) {
         I->src[s] = agx_immediate(value);
      } else if (value_u16 == def->imm && agx_allows_16bit_immediate(I)) {
         I->src[s] = agx_abs(agx_immediate(value_u16));
      } else if ((I->op == AGX_OPCODE_IADD || I->op == AGX_OPCODE_IMAD) &&
                 s == agx_negate_src_index(I)) {
         unsigned bits = agx_size_align_16(def->dest[0].size) * 16;
         uint64_t mask = BITFIELD64_MASK(bits);
         uint64_t negated = (-def->imm) & mask;
         value = negated;

         /* Try to negate the immediate */
         if (value == negated) {
            I->src[s] = agx_neg(agx_immediate(value));
         }
      }
   }
}

/*
 * Fuse not into and/or/xor. Specifically, acts on not and fuses:
 *
 *    not(and(x, y) -> nand(x, y)
 *    not(or(x, y) -> nor(x, y)
 *    not(xor(x, y) -> xnor(x, y)
 */
static bool
agx_optimizer_not(agx_instr *I, agx_instr *use)
{
   /* Check for bit op and use of not op */
   if (I->op != AGX_OPCODE_BITOP || use->op != AGX_OPCODE_NOT)
      return false;

   /* Remap operation to the appropriate one */
   I->truth_table ^= 0xF;
   I->dest[0] = use->dest[0];

   return true;
}

static bool
agx_optimizer_fmov_rev(agx_instr *I, agx_instr *use)
{
   if (!agx_is_fmov(use))
      return false;
   if (use->src[0].neg || use->src[0].abs)
      return false;

   /* We can fold f2f16 into 32-bit instructions, but we can't fold f2f32 into
    * 16-bit instructions, since the latter would implicitly promote to a 32-bit
    * instruction which is not exact.
    */
   assert(use->dest[0].size == AGX_SIZE_32 || use->dest[0].size == AGX_SIZE_16);
   assert(I->dest[0].size == AGX_SIZE_32 || I->dest[0].size == AGX_SIZE_16);

   if (I->dest[0].size == AGX_SIZE_16 && use->dest[0].size == AGX_SIZE_32)
      return false;

   /* saturate(saturate(x)) = saturate(x) */
   I->saturate |= use->saturate;
   I->dest[0] = use->dest[0];
   return true;
}

static bool
agx_icond_is_unsigned(enum agx_icond cond)
{
   switch (cond) {
   case AGX_ICOND_UEQ:
   case AGX_ICOND_ULT:
   case AGX_ICOND_UGT:
      return true;

   case AGX_ICOND_SEQ:
   case AGX_ICOND_SLT:
   case AGX_ICOND_SGT:
      return false;
   }

   UNREACHABLE("invalid condition");
}

static bool
agx_supports_zext(agx_instr *I, unsigned s)
{
   switch (I->op) {
   case AGX_OPCODE_IADD:
   case AGX_OPCODE_IMAD:
   case AGX_OPCODE_INTL:
   case AGX_OPCODE_FFS:
   case AGX_OPCODE_BITREV:
   case AGX_OPCODE_BFI:
   case AGX_OPCODE_BFEIL:
   case AGX_OPCODE_EXTR:
   case AGX_OPCODE_BITOP:
   case AGX_OPCODE_WHILE_ICMP:
   case AGX_OPCODE_IF_ICMP:
   case AGX_OPCODE_ELSE_ICMP:
   case AGX_OPCODE_BREAK_IF_ICMP:
   case AGX_OPCODE_ICMP_BALLOT:
   case AGX_OPCODE_ICMP_QUAD_BALLOT:
      return true;

   case AGX_OPCODE_ICMP:
   case AGX_OPCODE_ICMPSEL:
      /* Only the comparisons can be extended, not the selection. And we can
       * only zero-extend with unsigned comparison. Presumably the hardware
       * sign-extends with signed comparisons but we don't handle that yet.
       */
      return (s < 2) && agx_icond_is_unsigned(I->icond);

   default:
      return false;
   }
}

static void
agx_optimizer_copyprop(agx_context *ctx, agx_instr **defs, agx_instr *I)
{
   agx_foreach_ssa_src(I, s) {
      agx_index src = I->src[s];
      agx_instr *def = defs[src.value];

      if (def == NULL)
         continue; /* happens for phis in loops */
      if (def->op != AGX_OPCODE_MOV)
         continue;

      /* At the moment, not all instructions support size conversions. Notably
       * RA pseudo instructions don't handle size conversions. This should be
       * refined in the future.
       */
      if (def->src[0].size != src.size &&
          !(def->src[0].size < src.size && agx_supports_zext(I, s)))
         continue;

      /* Optimize split(64-bit uniform) so we can get better copyprop of the
       * 32-bit uniform parts. This helps reduce moves with 64-bit uniforms.
       */
      if (I->op == AGX_OPCODE_SPLIT && def->src[0].type == AGX_INDEX_UNIFORM &&
          src.size == AGX_SIZE_64 && I->dest[0].size == AGX_SIZE_32) {

         assert(I->nr_dests == 2 && "decomposing a 64-bit scalar");
         agx_builder b = agx_init_builder(ctx, agx_before_instr(I));

         agx_index lo = def->src[0];
         lo.size = AGX_SIZE_32;

         agx_index hi = lo;
         hi.value += 2 /* half of 64-bits = 32-bits = 2 x 16-bits */;

         defs[I->dest[0].value] = agx_mov_to(&b, I->dest[0], lo);
         defs[I->dest[1].value] = agx_mov_to(&b, I->dest[1], hi);

         agx_remove_instruction(I);
         continue;
      }

      /* Immediate inlining happens elsewhere */
      if (def->src[0].type == AGX_INDEX_IMMEDIATE)
         continue;

      /* ALU instructions cannot take 64-bit */
      if (def->src[0].size == AGX_SIZE_64 &&
          !(I->op == AGX_OPCODE_DEVICE_LOAD && s == 0) &&
          !(I->op == AGX_OPCODE_DEVICE_STORE && s == 1) &&
          !(I->op == AGX_OPCODE_ATOMIC && s == 1))
         continue;

      agx_replace_src(I, s, def->src[0]);

      /* If we are zero-extending into an instruction that distinguishes sign
       * and zero extend, make sure we pick zero-extend.
       */
      if (def->src[0].size < src.size &&
          (I->op == AGX_OPCODE_IMAD || I->op == AGX_OPCODE_IADD)) {

         assert(agx_supports_zext(I, s));
         I->src[s].abs = true;
      }
   }
}

/*
 * Fuse conditions into if. Specifically, acts on if_icmp and fuses:
 *
 *    if_icmp(cmp(x, y, *), 0, ne/eq) -> if_cmp(x, y, *)
 */
static void
agx_optimizer_if_cmp(agx_instr **defs, agx_instr *I)
{
   /* Check for unfused if */
   if (!agx_is_equiv(I->src[1], agx_zero()) || I->icond != AGX_ICOND_UEQ ||
       I->src[0].type != AGX_INDEX_NORMAL)
      return;

   /* Check for condition */
   agx_instr *def = defs[I->src[0].value];
   if (def->op != AGX_OPCODE_ICMP && def->op != AGX_OPCODE_FCMP)
      return;

   /* Fuse */
   I->src[0] = def->src[0];
   I->src[1] = def->src[1];
   I->invert_cond = def->invert_cond ^ !I->invert_cond;

   if (def->op == AGX_OPCODE_ICMP) {
      I->op = AGX_OPCODE_IF_ICMP;
      I->icond = def->icond;
   } else {
      I->op = AGX_OPCODE_IF_FCMP;
      I->fcond = def->fcond;
   }
}

/*
 * Fuse invert into if. Acts on if_icmp and fuses:
 *
 *    if_icmp(xor(x, 1), 0, ne) -> if_cmp(x, 0, eq)
 */
static void
agx_optimizer_if_not(agx_instr **defs, agx_instr *I)
{
   /* Check for unfused if */
   if (!agx_is_equiv(I->src[1], agx_zero()) || I->icond != AGX_ICOND_UEQ ||
       I->src[0].type != AGX_INDEX_NORMAL)
      return;

   /* Check for invert */
   agx_instr *def = defs[I->src[0].value];
   if (def->op != AGX_OPCODE_BITOP ||
       !agx_is_equiv(def->src[1], agx_immediate(1)) ||
       def->truth_table != AGX_BITOP_XOR)
      return;

   /* Fuse */
   I->src[0] = def->src[0];
   I->invert_cond = !I->invert_cond;
}

/*
 * Fuse conditions into select. Specifically, acts on icmpsel and fuses:
 *
 *    icmpsel(cmp(x, y, *), 0, z, w, eq) -> cmpsel(x, y, w, z, *)
 *
 * Care must be taken to invert the condition by swapping cmpsel arguments.
 */
static void
agx_optimizer_cmpsel(agx_instr **defs, agx_instr *I)
{
   /* Check for unfused select */
   if (!agx_is_equiv(I->src[1], agx_zero()) || I->icond != AGX_ICOND_UEQ ||
       I->src[0].type != AGX_INDEX_NORMAL)
      return;

   /* Check for condition */
   agx_instr *def = defs[I->src[0].value];
   if (def->op != AGX_OPCODE_ICMP && def->op != AGX_OPCODE_FCMP)
      return;

   /* Fuse */
   I->src[0] = def->src[0];
   I->src[1] = def->src[1];

   /* In the unfused select, the condition is inverted due to the form:
    *
    *    (cond == 0) ? x : y
    *
    * So we need to swap the arguments when fusing to become cond ? y : x. If
    * the condition was supposed to be inverted, we don't swap since it's
    * already inverted. cmpsel does not have an invert_cond bit to use.
    */
   if (!def->invert_cond) {
      agx_index temp = I->src[2];
      I->src[2] = I->src[3];
      I->src[3] = temp;
   }

   if (def->op == AGX_OPCODE_ICMP) {
      I->op = AGX_OPCODE_ICMPSEL;
      I->icond = def->icond;
   } else {
      I->op = AGX_OPCODE_FCMPSEL;
      I->fcond = def->fcond;
   }
}

/*
 * Fuse conditions into ballots:
 *
 *    ballot(cmp(x, y)) -> ballot_cmp(x, y)
 */
static void
agx_optimizer_ballot(agx_context *ctx, agx_instr **defs, agx_instr *I)
{
   if (I->src[0].type != AGX_INDEX_NORMAL)
      return;

   agx_instr *def = defs[I->src[0].value];
   if (!def || (def->op != AGX_OPCODE_ICMP && def->op != AGX_OPCODE_FCMP))
      return;

   bool quad = I->op == AGX_OPCODE_QUAD_BALLOT;
   assert(quad || I->op == AGX_OPCODE_BALLOT);

   /* Replace with a fused instruction since the # of sources changes */
   agx_builder b = agx_init_builder(ctx, agx_before_instr(I));

   agx_instr *fused = agx_icmp_ballot_to(
      &b, I->dest[0], def->src[0], def->src[1], def->icond, def->invert_cond);

   if (def->op == AGX_OPCODE_ICMP) {
      fused->op = quad ? AGX_OPCODE_ICMP_QUAD_BALLOT : AGX_OPCODE_ICMP_BALLOT;
   } else {
      fused->op = quad ? AGX_OPCODE_FCMP_QUAD_BALLOT : AGX_OPCODE_FCMP_BALLOT;
   }

   agx_remove_instruction(I);
}

/*
 * Fuse not srcs into bitop.
 */
static void
agx_optimizer_bitop(agx_instr **defs, agx_instr *I)
{
   agx_foreach_ssa_src(I, s) {
      agx_index src = I->src[s];
      agx_instr *def = defs[src.value];

      /* Check for not src */
      if (def->op != AGX_OPCODE_NOT)
         continue;

      /* Select new operation */
      if (s == 0) {
         I->truth_table =
            ((I->truth_table & 0x5) << 1) | ((I->truth_table & 0xa) >> 1);
      } else if (s == 1) {
         I->truth_table = ((I->truth_table & 0x3) << 2) | (I->truth_table >> 2);
      }

      /* Fuse */
      I->src[s] = def->src[0];
   }
}

/*
 * Fuse sign-extends into addition-like instructions:
 */
static void
agx_optimizer_signext(agx_instr **defs, agx_instr *I)
{
   agx_foreach_ssa_src(I, s) {
      agx_index src = I->src[s];
      agx_instr *def = defs[src.value];

      if (def == NULL || def->op != AGX_OPCODE_SIGNEXT)
         continue;

      agx_replace_src(I, s, def->src[0]);
      assert(!I->src[s].abs && "sign-extended");
   }
}

void
agx_optimizer_forward(agx_context *ctx)
{
   agx_instr **defs = calloc(ctx->alloc, sizeof(*defs));

   agx_foreach_instr_global_safe(ctx, I) {
      struct agx_opcode_info info = agx_opcodes_info[I->op];

      agx_foreach_ssa_dest(I, d) {
         defs[I->dest[d].value] = I;
      }

      /* Optimize moves */
      agx_optimizer_copyprop(ctx, defs, I);

      /* Propagate fmov down */
      if (info.is_float || I->op == AGX_OPCODE_FCMPSEL ||
          I->op == AGX_OPCODE_FCMP)
         agx_optimizer_fmov(defs, I);

      /* Inline immediates if we can. TODO: systematic */
      if (I->op != AGX_OPCODE_COLLECT && I->op != AGX_OPCODE_IMAGE_LOAD &&
          I->op != AGX_OPCODE_TEXTURE_LOAD &&
          I->op != AGX_OPCODE_UNIFORM_STORE && I->op != AGX_OPCODE_EXPORT)
         agx_optimizer_inline_imm(defs, I);

      if (I->op == AGX_OPCODE_IF_ICMP) {
         agx_optimizer_if_not(defs, I);
         agx_optimizer_if_cmp(defs, I);
      } else if (I->op == AGX_OPCODE_ICMPSEL) {
         agx_optimizer_cmpsel(defs, I);
      } else if (I->op == AGX_OPCODE_BALLOT ||
                 I->op == AGX_OPCODE_QUAD_BALLOT) {
         agx_optimizer_ballot(ctx, defs, I);
      } else if (I->op == AGX_OPCODE_BITOP) {
         agx_optimizer_bitop(defs, I);
      } else if (I->op == AGX_OPCODE_IADD || I->op == AGX_OPCODE_IMAD) {
         agx_optimizer_signext(defs, I);
      }
   }

   free(defs);
}

static void
record_use(agx_instr **uses, BITSET_WORD *multiple, agx_instr *I, unsigned src)
{
   unsigned v = I->src[src].value;

   if (uses[v])
      BITSET_SET(multiple, v);
   else
      uses[v] = I;
}

void
agx_optimizer_backward(agx_context *ctx)
{
   agx_instr **uses = calloc(ctx->alloc, sizeof(*uses));
   BITSET_WORD *multiple = calloc(BITSET_WORDS(ctx->alloc), sizeof(*multiple));

   agx_foreach_block_rev(ctx, block) {
      /* Phi sources are logically read at the end of predecessor, so process
       * our source in our successors' phis firsts. This ensures we set
       * `multiple` correctly with phi sources.
       */
      agx_foreach_successor(block, succ) {
         unsigned s = agx_predecessor_index(succ, block);

         agx_foreach_phi_in_block(succ, phi) {
            record_use(uses, multiple, phi, s);
         }
      }

      agx_foreach_instr_in_block_rev(block, I) {
         struct agx_opcode_info info = agx_opcodes_info[I->op];

         /* Skip phis, they're handled specially */
         if (I->op == AGX_OPCODE_PHI) {
            continue;
         }

         agx_foreach_ssa_src(I, s) {
            record_use(uses, multiple, I, s);
         }

         if (info.nr_dests != 1)
            continue;

         if (I->dest[0].type != AGX_INDEX_NORMAL)
            continue;

         agx_instr *use = uses[I->dest[0].value];

         if (!use || BITSET_TEST(multiple, I->dest[0].value))
            continue;

         if (agx_optimizer_not(I, use)) {
            agx_remove_instruction(use);
            continue;
         }

         /* Destination has a single use, try to propagate */
         if (info.is_float && agx_optimizer_fmov_rev(I, use)) {
            agx_remove_instruction(use);
            continue;
         }
      }
   }

   free(uses);
   free(multiple);
}

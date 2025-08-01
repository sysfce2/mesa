/*
 * Copyright © 2019 Valve Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "aco_builder.h"
#include "aco_ir.h"

#include "util/enum_operators.h"

#include <algorithm>
#include <map>
#include <vector>

namespace aco {

namespace {

enum class pred_defined : uint8_t {
   undef = 0,
   const_1 = 1,
   const_0 = 2,
   temp = 3,
   zero = 4, /* all disabled lanes are zero'd out */
};
MESA_DEFINE_CPP_ENUM_BITFIELD_OPERATORS(pred_defined);

struct ssa_state {
   unsigned loop_nest_depth;
   RegClass rc;

   std::vector<pred_defined> any_pred_defined;
   std::vector<bool> visited;
   std::vector<Operand> outputs; /* the output per block */
};

Operand get_output(Program* program, unsigned block_idx, ssa_state* state);

void
init_outputs(Program* program, ssa_state* state, unsigned start, unsigned end)
{
   for (unsigned i = start; i <= end; ++i) {
      if (state->visited[i])
         continue;
      state->outputs[i] = get_output(program, i, state);
      state->visited[i] = true;
   }
}

Operand
get_output(Program* program, unsigned block_idx, ssa_state* state)
{
   Block& block = program->blocks[block_idx];

   if (state->any_pred_defined[block_idx] == pred_defined::undef)
      return Operand(state->rc);

   if (block.loop_nest_depth < state->loop_nest_depth)
      /* loop-carried value for loop exit phis */
      return Operand::zero(state->rc.bytes());

   size_t num_preds = block.linear_preds.size();

   if (block.loop_nest_depth > state->loop_nest_depth || num_preds == 1 ||
       block.kind & block_kind_loop_exit)
      return state->outputs[block.linear_preds[0]];

   Operand output;

   /* Loop headers can contain back edges, in which case the predecessor
    * outputs aren't yet determined because the predecessor is after the block.
    * The predecessor outputs also depend on the output of the loop header,
    * so allocate a temporary that will store this block's output and use that
    * to calculate the predecessor block output. In this case, we always emit a phi
    * to ensure the allocated temporary is defined. */
   if (block.kind & block_kind_loop_header) {
      unsigned start_idx = block_idx + 1;
      unsigned end_idx = block.linear_preds.back();

      state->outputs[block_idx] = Operand(Temp(program->allocateTmp(state->rc)));
      init_outputs(program, state, start_idx, end_idx);
      output = state->outputs[block_idx];
   } else if (std::all_of(block.linear_preds.begin() + 1, block.linear_preds.end(),
                          [&](unsigned pred) {
                             return state->outputs[pred] == state->outputs[block.linear_preds[0]];
                          })) {
      return state->outputs[block.linear_preds[0]];
   } else {
      output = Operand(Temp(program->allocateTmp(state->rc)));
   }

   /* create phi */
   aco_ptr<Instruction> phi{
      create_instruction(aco_opcode::p_linear_phi, Format::PSEUDO, num_preds, 1)};
   for (unsigned i = 0; i < num_preds; i++)
      phi->operands[i] = state->outputs[block.linear_preds[i]];
   phi->definitions[0] = Definition(output.getTemp());
   block.instructions.emplace(block.instructions.begin(), std::move(phi));

   assert(output.size() == state->rc.size());

   return output;
}

Builder
bld_before_logical_end(Program* program, Block* block)
{
   auto IsLogicalEnd = [](const aco_ptr<Instruction>& inst) -> bool
   { return inst->opcode == aco_opcode::p_logical_end; };
   auto it = std::find_if(block->instructions.rbegin(), block->instructions.rend(), IsLogicalEnd);

   Builder bld(program);
   if (it == block->instructions.rend()) {
      assert(block->instructions.back()->isBranch());
      bld.reset(&block->instructions, std::prev(block->instructions.end()));
   } else {
      bld.reset(&block->instructions, std::prev(it.base()));
   }
   return bld;
}

void
insert_before_logical_end(Block* block, aco_ptr<Instruction> instr)
{
   bld_before_logical_end(NULL, block).insert(std::move(instr));
}

void
build_merge_code(Program* program, ssa_state* state, Block* block, Operand cur)
{
   unsigned block_idx = block->index;
   Definition dst = Definition(state->outputs[block_idx].getTemp());
   Operand prev = get_output(program, block_idx, state);
   if (cur.isUndefined())
      return;

   Builder bld = bld_before_logical_end(program, block);

   pred_defined defined = state->any_pred_defined[block_idx];
   if (defined == pred_defined::undef) {
      return;
   } else if (defined == pred_defined::const_0) {
      bld.sop2(Builder::s_and, dst, bld.def(s1, scc), cur, Operand(exec, bld.lm));
      return;
   } else if (defined == pred_defined::const_1) {
      bld.sop2(Builder::s_orn2, dst, bld.def(s1, scc), cur, Operand(exec, bld.lm));
      return;
   }

   assert(prev.isTemp());
   /* simpler sequence in case prev has only zeros in disabled lanes */
   if ((defined & pred_defined::zero) == pred_defined::zero) {
      if (cur.isConstant()) {
         if (!cur.constantValue()) {
            bld.copy(dst, prev);
            return;
         }
         cur = Operand(exec, bld.lm);
      } else {
         cur =
            bld.sop2(Builder::s_and, bld.def(bld.lm), bld.def(s1, scc), cur, Operand(exec, bld.lm));
      }
      bld.sop2(Builder::s_or, dst, bld.def(s1, scc), prev, cur);
      return;
   }

   if (cur.isConstant()) {
      if (cur.constantValue())
         bld.sop2(Builder::s_or, dst, bld.def(s1, scc), prev, Operand(exec, bld.lm));
      else
         bld.sop2(Builder::s_andn2, dst, bld.def(s1, scc), prev, Operand(exec, bld.lm));
      return;
   }
   prev =
      bld.sop2(Builder::s_andn2, bld.def(bld.lm), bld.def(s1, scc), prev, Operand(exec, bld.lm));
   cur = bld.sop2(Builder::s_and, bld.def(bld.lm), bld.def(s1, scc), cur, Operand(exec, bld.lm));
   bld.sop2(Builder::s_or, dst, bld.def(s1, scc), prev, cur);
   return;
}

void
build_const_else_merge_code(Program* program, Block& invert_block, aco_ptr<Instruction>& phi)
{
   /* When the else-side operand of a binary merge phi is constant,
    * we can use a simpler way to lower the phi by emitting some
    * instructions to the invert block instead.
    * This allows us to actually delete the else block when it's empty.
    */
   Builder bld(program);
   Operand then = phi->operands[0];
   const Operand els = phi->operands[1];

   /* Only -1 (all lanes true) and 0 (all lanes false) constants are supported here. */
   assert(!then.isConstant() || then.constantEquals(0) || then.constantEquals(-1));
   assert(els.constantEquals(0) || els.constantEquals(-1));

   if (!then.isConstant()) {
      /* Left-hand operand is not constant, so we need to emit a phi to access it. */
      bld.reset(&invert_block.instructions, invert_block.instructions.begin());
      then = bld.pseudo(aco_opcode::p_linear_phi, bld.def(bld.lm), then, Operand(bld.lm));
   }

   auto after_phis =
      std::find_if(invert_block.instructions.begin(), invert_block.instructions.end(),
                   [](const aco_ptr<Instruction>& instr) -> bool { return !is_phi(instr.get()); });
   bld.reset(&invert_block.instructions, after_phis);

   Temp tmp;
   if (then.constantEquals(-1) && els.constantEquals(0)) {
      tmp = bld.copy(bld.def(bld.lm), Operand(exec, bld.lm));
   } else {
      Builder::WaveSpecificOpcode opc = els.constantEquals(0) ? Builder::s_and : Builder::s_orn2;
      tmp = bld.sop2(opc, bld.def(bld.lm), bld.def(s1, scc), then, Operand(exec, bld.lm));
   }

   /* We can't delete the original phi because that'd invalidate the iterator in lower_phis,
    * so just make it a trivial phi instead.
    */
   phi->opcode = aco_opcode::p_linear_phi;
   phi->operands[0] = Operand(tmp);
   phi->operands[1] = Operand(tmp);
}

bool
block_is_empty(Block& block)
{
   for (auto& instr : block.instructions) {
      if (instr->opcode != aco_opcode::p_logical_start &&
          instr->opcode != aco_opcode::p_logical_end && instr->opcode != aco_opcode::p_branch)
         return false;
   }
   return true;
}

void
build_empty_else_merge_code(Program* program, Block& merge_block, Block& invert_block,
                            aco_ptr<Instruction>& phi)
{
   /* If the else block is empty, we know that the else phi operand dominates the
    * then block, so we can handle the phi only in the then block.
    */
   Block& then_block = program->blocks[merge_block.logical_preds[0]];
   Builder bld = bld_before_logical_end(program, &then_block);
   Operand then_op = phi->operands[0];
   Operand else_op = phi->operands[1];

   Operand new_op;

   if (then_op.constantEquals(-1)) {
      new_op =
         bld.sop2(Builder::s_or, bld.def(bld.lm), bld.def(s1, scc), else_op, Operand(exec, bld.lm));
   } else if (then_op.constantEquals(0)) {
      new_op = bld.sop2(Builder::s_andn2, bld.def(bld.lm), bld.def(s1, scc), else_op,
                        Operand(exec, bld.lm));
   } else {
      new_op = bld.sop2(Builder::s_andn2, bld.def(bld.lm), bld.def(s1, scc), else_op,
                        Operand(exec, bld.lm));
      then_op = bld.sop2(Builder::s_and, bld.def(bld.lm), bld.def(s1, scc), then_op,
                         Operand(exec, bld.lm));
      new_op = bld.sop2(Builder::s_or, bld.def(bld.lm), bld.def(s1, scc), then_op, new_op);
   }

   /* Insert new linear phi in the invert block, make merge block phi trivial to not invalidate
    * iterators. */
   bld.reset(&invert_block.instructions, invert_block.instructions.begin());
   Temp tmp = bld.pseudo(aco_opcode::p_linear_phi, bld.def(bld.lm), new_op, else_op);

   phi->opcode = aco_opcode::p_linear_phi;
   phi->operands[0] = Operand(tmp);
   phi->operands[1] = Operand(tmp);
}

void
init_state(Program* program, Block* block, ssa_state* state, aco_ptr<Instruction>& phi)
{
   Builder bld(program);

   /* do this here to avoid resizing in case of no boolean phis */
   state->rc = phi->definitions[0].regClass();
   state->visited.resize(program->blocks.size());
   state->outputs.resize(program->blocks.size());
   state->any_pred_defined.resize(program->blocks.size());
   state->loop_nest_depth = block->loop_nest_depth;
   if (block->kind & block_kind_loop_exit)
      state->loop_nest_depth += 1;
   std::fill(state->visited.begin(), state->visited.end(), false);
   std::fill(state->any_pred_defined.begin(), state->any_pred_defined.end(), pred_defined::undef);

   for (unsigned i = 0; i < block->logical_preds.size(); i++) {
      if (phi->operands[i].isUndefined())
         continue;
      pred_defined defined = pred_defined::temp;
      if (phi->operands[i].isConstant() && phi->opcode == aco_opcode::p_boolean_phi)
         defined = phi->operands[i].constantValue() ? pred_defined::const_1 : pred_defined::const_0;
      for (unsigned succ : program->blocks[block->logical_preds[i]].linear_succs)
         state->any_pred_defined[succ] |= defined;
   }

   unsigned start = block->logical_preds[0];
   unsigned end = block->linear_preds.back();

   /* The value might not be loop-invariant if the loop has a divergent break and
    *  - this is a boolean phi, which must be combined with logical exits from previous iterations
    *  - or the loop also has an additional linear exit (continue_or_break), which might be taken in
    *    a different iteration than the logical exit
    */
   bool continue_or_break = block->linear_preds.size() > block->logical_preds.size();
   bool has_divergent_break = std::any_of(
      block->logical_preds.begin(), block->logical_preds.end(),
      [&](unsigned pred) { return !(program->blocks[pred].kind & block_kind_uniform); });
   if (block->kind & block_kind_loop_exit && has_divergent_break &&
       (phi->opcode == aco_opcode::p_boolean_phi || continue_or_break)) {
      /* Start at the loop pre-header as we need the value from previous iterations. */
      while (program->blocks[start].loop_nest_depth >= state->loop_nest_depth)
         start--;
      end = block->index - 1;
      /* If the loop-header has a back-edge, we need to insert a phi.
       * This will contain a defined value */
      if (program->blocks[start + 1].linear_preds.size() > 1) {
         if (phi->opcode == aco_opcode::p_boolean_phi) {
            state->any_pred_defined[start + 1] = pred_defined::temp | pred_defined::zero;
            /* add dominating zero: this allows to emit simpler merge sequences
             * if we can ensure that all disabled lanes are always zero on incoming values
             */
            state->any_pred_defined[start] = pred_defined::const_0;
         } else {
            state->any_pred_defined[start + 1] = pred_defined::temp;
         }
      }
   }

   /* For loop header phis, don't propagate the incoming value */
   if (block->kind & block_kind_loop_header) {
      state->any_pred_defined[block->index] = pred_defined::undef;
   }

   for (unsigned j = start; j <= end; j++) {
      if (state->any_pred_defined[j] == pred_defined::undef)
         continue;
      for (unsigned succ : program->blocks[j].linear_succs)
         state->any_pred_defined[succ] |= state->any_pred_defined[j];
   }

   state->any_pred_defined[block->index] = pred_defined::undef;

   for (unsigned i = 0; i < phi->operands.size(); i++) {
      /* If the Operand is undefined, just propagate the previous value. */
      if (phi->operands[i].isUndefined())
         continue;

      unsigned pred = block->logical_preds[i];
      if (phi->opcode == aco_opcode::p_boolean_phi &&
          state->any_pred_defined[pred] != pred_defined::undef) {
         /* Needs merge code sequence. */
         state->outputs[pred] = Operand(bld.tmp(state->rc));
      } else {
         state->outputs[pred] = phi->operands[i];
      }
      assert(state->outputs[pred].size() == state->rc.size());
      state->visited[pred] = true;
   }

   init_outputs(program, state, start, end);
}

void
lower_phi_to_linear(Program* program, ssa_state* state, Block* block, aco_ptr<Instruction>& phi)
{
   if (phi->opcode == aco_opcode::p_phi) {
      /* Insert p_as_uniform for VGPR->SGPR phis. */
      Builder bld(program);
      for (unsigned i = 0; i < phi->operands.size(); i++) {
         if (phi->operands[i].isOfType(RegType::vgpr)) {
            Block* pred = &program->blocks[block->logical_preds[i]];
            Temp new_phi_src = bld.tmp(phi->definitions[0].regClass());
            insert_before_logical_end(
               pred, bld.pseudo(aco_opcode::p_as_uniform, Definition(new_phi_src), phi->operands[i])
                        .get_ptr());
            phi->operands[i].setTemp(new_phi_src);
         }
      }
   }

   if (block->linear_preds == block->logical_preds) {
      phi->opcode = aco_opcode::p_linear_phi;
      return;
   }

   if ((block->kind & block_kind_merge) && phi->opcode == aco_opcode::p_boolean_phi &&
       phi->operands.size() == 2) {
      Block& invert_block = program->blocks[block->linear_idom];
      Block& els_block = program->blocks[block->logical_preds[1]];
      assert(invert_block.kind & block_kind_invert);
      if (phi->operands[1].isConstant()) {
         build_const_else_merge_code(program, invert_block, phi);
         return;
      } else if (phi->operands[1].isTemp() && block_is_empty(els_block) &&
                 els_block.linear_preds[0] == invert_block.index) {
         build_empty_else_merge_code(program, *block, invert_block, phi);
         return;
      }
   }

   init_state(program, block, state, phi);

   if (phi->opcode == aco_opcode::p_boolean_phi) {
      /* Divergent boolean phis are lowered to logical arithmetic and linear phis. */
      for (unsigned i = 0; i < phi->operands.size(); i++)
         build_merge_code(program, state, &program->blocks[block->logical_preds[i]],
                          phi->operands[i]);
   }

   unsigned num_preds = block->linear_preds.size();
   if (phi->operands.size() != num_preds) {
      Instruction* new_phi{
         create_instruction(aco_opcode::p_linear_phi, Format::PSEUDO, num_preds, 1)};
      new_phi->definitions[0] = phi->definitions[0];
      phi.reset(new_phi);
   } else {
      phi->opcode = aco_opcode::p_linear_phi;
   }
   assert(phi->operands.size() == num_preds);

   for (unsigned i = 0; i < num_preds; i++)
      phi->operands[i] = state->outputs[block->linear_preds[i]];

   return;
}

void
lower_subdword_phis(Program* program, Block* block, aco_ptr<Instruction>& phi)
{
   Builder bld(program);
   for (unsigned i = 0; i < phi->operands.size(); i++) {
      if (!phi->operands[i].isTemp())
         continue;
      if (phi->operands[i].regClass() == phi->definitions[0].regClass())
         continue;

      assert(phi->operands[i].isTemp());
      Block* pred = &program->blocks[block->logical_preds[i]];
      Temp phi_src = phi->operands[i].getTemp();

      assert(phi_src.regClass().type() == RegType::sgpr);
      Temp tmp = bld.tmp(RegClass(RegType::vgpr, phi_src.size()));
      insert_before_logical_end(pred, bld.copy(Definition(tmp), phi_src).get_ptr());
      Temp new_phi_src = bld.tmp(phi->definitions[0].regClass());
      insert_before_logical_end(pred, bld.pseudo(aco_opcode::p_extract_vector,
                                                 Definition(new_phi_src), tmp, Operand::zero())
                                         .get_ptr());

      phi->operands[i].setTemp(new_phi_src);
   }
   return;
}

} /* end namespace */

void
lower_phis(Program* program)
{
   ssa_state state;

   for (Block& block : program->blocks) {
      for (aco_ptr<Instruction>& phi : block.instructions) {
         if (phi->opcode == aco_opcode::p_boolean_phi) {
            assert(program->wave_size == 64 ? phi->definitions[0].regClass() == s2
                                            : phi->definitions[0].regClass() == s1);
            lower_phi_to_linear(program, &state, &block, phi);
         } else if (phi->opcode == aco_opcode::p_phi) {
            if (phi->definitions[0].regClass().type() == RegType::sgpr)
               lower_phi_to_linear(program, &state, &block, phi);
            else if (phi->definitions[0].regClass().is_subdword())
               lower_subdword_phis(program, &block, phi);
         } else if (!is_phi(phi)) {
            break;
         }
      }
   }
}

} // namespace aco

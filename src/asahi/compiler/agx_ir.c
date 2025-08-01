/*
 * Copyright 2022 Alyssa Rosenzweig
 * SPDX-License-Identifier: MIT
 */

#include "agx_compiler.h"
#include "agx_opcodes.h"

bool
agx_allows_16bit_immediate(agx_instr *I)
{
   return (I->op == AGX_OPCODE_DEVICE_LOAD) ||
          (I->op == AGX_OPCODE_DEVICE_STORE) ||
          (I->op == AGX_OPCODE_STACK_LOAD) ||
          (I->op == AGX_OPCODE_STACK_STORE) ||
          (I->op == AGX_OPCODE_UNIFORM_STORE) || (I->op == AGX_OPCODE_ATOMIC) ||
          (I->op == AGX_OPCODE_PHI) || (I->op == AGX_OPCODE_TEX_STATE_STORE) ||
          (I->op == AGX_OPCODE_SAMPLER_STATE_STORE);
}

unsigned
agx_negate_src_index(agx_instr *I)
{
   switch (I->op) {
   case AGX_OPCODE_IMAD:
      return 2;
   case AGX_OPCODE_IADD:
      return 1;
   default:
      UNREACHABLE("not allowed");
   }
}

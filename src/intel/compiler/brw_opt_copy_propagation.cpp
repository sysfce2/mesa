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

/** @file
 *
 * Support for global copy propagation in two passes: A local pass that does
 * intra-block copy (and constant) propagation, and a global pass that uses
 * dataflow analysis on the copies available at the end of each block to re-do
 * local copy propagation with more copies available.
 *
 * See Muchnick's Advanced Compiler Design and Implementation, section
 * 12.5 (p356).
 */

#include "util/bitset.h"
#include "util/u_math.h"
#include "util/rb_tree.h"
#include "brw_shader.h"
#include "brw_analysis.h"
#include "brw_cfg.h"
#include "brw_eu.h"

namespace { /* avoid conflict with opt_copy_propagation_elements */
struct acp_entry {
   struct rb_node by_dst;
   struct rb_node by_src;
   brw_reg dst;
   brw_reg src;
   unsigned global_idx;
   unsigned size_written;
   unsigned size_read;
   enum opcode opcode;
   bool is_partial_write;
   bool force_writemask_all;
};

/**
 * Compare two acp_entry::src.nr
 *
 * This is intended to be used as the comparison function for rb_tree.
 */
static int
cmp_entry_dst_entry_dst(const struct rb_node *a_node, const struct rb_node *b_node)
{
   const struct acp_entry *a_entry =
      rb_node_data(struct acp_entry, a_node, by_dst);

   const struct acp_entry *b_entry =
      rb_node_data(struct acp_entry, b_node, by_dst);

   return a_entry->dst.nr - b_entry->dst.nr;
}

static int
cmp_entry_dst_nr(const struct rb_node *a_node, const void *b_key)
{
   const struct acp_entry *a_entry =
      rb_node_data(struct acp_entry, a_node, by_dst);

   return a_entry->dst.nr - (uintptr_t) b_key;
}

static int
cmp_entry_src_entry_src(const struct rb_node *a_node, const struct rb_node *b_node)
{
   const struct acp_entry *a_entry =
      rb_node_data(struct acp_entry, a_node, by_src);

   const struct acp_entry *b_entry =
      rb_node_data(struct acp_entry, b_node, by_src);

   return a_entry->src.nr - b_entry->src.nr;
}

/**
 * Compare an acp_entry::src.nr with a raw nr.
 *
 * This is intended to be used as the comparison function for rb_tree.
 */
static int
cmp_entry_src_nr(const struct rb_node *a_node, const void *b_key)
{
   const struct acp_entry *a_entry =
      rb_node_data(struct acp_entry, a_node, by_src);

   return a_entry->src.nr - (uintptr_t) b_key;
}

class acp_forward_iterator {
public:
   acp_forward_iterator(struct rb_node *n, unsigned offset)
      : curr(n), next(nullptr), offset(offset)
   {
      next = rb_node_next_or_null(curr);
   }

   acp_forward_iterator &operator++()
   {
      curr = next;
      next = rb_node_next_or_null(curr);

      return *this;
   }

   bool operator!=(const acp_forward_iterator &other) const
   {
      return curr != other.curr;
   }

   struct acp_entry *operator*() const
   {
      /* This open-codes part of rb_node_data. */
      return curr != NULL ? (struct acp_entry *)(((char *)curr) - offset)
                          : NULL;
   }

private:
   struct rb_node *curr;
   struct rb_node *next;
   unsigned offset;
};

struct acp {
   DECLARE_LINEAR_ALLOC_CXX_OPERATORS(acp);

   struct rb_tree by_dst;
   struct rb_tree by_src;

   acp()
   {
      rb_tree_init(&by_dst);
      rb_tree_init(&by_src);
   }

   acp_forward_iterator begin()
   {
      return acp_forward_iterator(rb_tree_first(&by_src),
                                  rb_tree_offsetof(struct acp_entry, by_src, 0));
   }

   const acp_forward_iterator end() const
   {
      return acp_forward_iterator(nullptr, 0);
   }

   unsigned length()
   {
      unsigned l = 0;

      for (rb_node *iter = rb_tree_first(&by_src);
           iter != NULL; iter = rb_node_next(iter))
         l++;

      return l;
   }

   void add(acp_entry *entry)
   {
      rb_tree_insert(&by_dst, &entry->by_dst, cmp_entry_dst_entry_dst);
      rb_tree_insert(&by_src, &entry->by_src, cmp_entry_src_entry_src);
   }

   void remove(acp_entry *entry)
   {
      rb_tree_remove(&by_dst, &entry->by_dst);
      rb_tree_remove(&by_src, &entry->by_src);
   }

   acp_forward_iterator find_by_src(unsigned nr)
   {
      struct rb_node *rbn = rb_tree_search(&by_src,
                                           (void *)(uintptr_t) nr,
                                           cmp_entry_src_nr);

      return acp_forward_iterator(rbn, rb_tree_offsetof(struct acp_entry,
                                                        by_src, rbn));
   }

   acp_forward_iterator find_by_dst(unsigned nr)
   {
      struct rb_node *rbn = rb_tree_search(&by_dst,
                                           (void *)(uintptr_t) nr,
                                           cmp_entry_dst_nr);

      return acp_forward_iterator(rbn, rb_tree_offsetof(struct acp_entry,
                                                        by_dst, rbn));
   }
};

struct block_data {
   /**
    * Which entries in the brw_copy_prop_dataflow acp table are live at the
    * start of this block.  This is the useful output of the analysis, since
    * it lets us plug those into the local copy propagation on the second
    * pass.
    */
   BITSET_WORD *livein;

   /**
    * Which entries in the brw_copy_prop_dataflow acp table are live at the end
    * of this block.  This is done in initial setup from the per-block acps
    * returned by the first local copy prop pass.
    */
   BITSET_WORD *liveout;

   /**
    * Which entries in the brw_copy_prop_dataflow acp table are generated by
    * instructions in this block which reach the end of the block without
    * being killed.
    */
   BITSET_WORD *copy;

   /**
    * Which entries in the brw_copy_prop_dataflow acp table are killed over the
    * course of this block.
    */
   BITSET_WORD *kill;

   /**
    * Which entries in the brw_copy_prop_dataflow acp table are guaranteed to
    * have a fully uninitialized destination at the end of this block.
    */
   BITSET_WORD *undef;

   /**
    * Which entries in the brw_copy_prop_dataflow acp table can the
    * start of this block be reached from.  Note that this is a weaker
    * condition than livein.
    */
   BITSET_WORD *reachin;

   /**
    * Which entries in the brw_copy_prop_dataflow acp table are
    * overwritten by an instruction with channel masks inconsistent
    * with the copy instruction (e.g. due to force_writemask_all).
    * Such an overwrite can cause the copy entry to become invalid
    * even if the copy instruction is subsequently re-executed for any
    * given channel i, since the execution of the overwrite for
    * channel i may corrupt other channels j!=i inactive for the
    * subsequent copy.
    */
   BITSET_WORD *exec_mismatch;
};

class brw_copy_prop_dataflow
{
public:
   brw_copy_prop_dataflow(linear_ctx *lin_ctx, cfg_t *cfg,
                          const brw_live_variables &live,
                          const brw_ip_ranges &ips,
                          struct acp *out_acp);

   void setup_initial_values();
   void run();

   void dump_block_data() const UNUSED;

   cfg_t *cfg;
   const brw_live_variables &live;
   const brw_ip_ranges &ips;

   acp_entry **acp;
   int num_acp;
   int bitset_words;

  struct block_data *bd;
};
} /* anonymous namespace */

brw_copy_prop_dataflow::brw_copy_prop_dataflow(linear_ctx *lin_ctx, cfg_t *cfg,
                                              const brw_live_variables &live,
                                              const brw_ip_ranges &ips,
                                              struct acp *out_acp)
   : cfg(cfg), live(live), ips(ips)
{
   bd = linear_zalloc_array(lin_ctx, struct block_data, cfg->num_blocks);

   num_acp = 0;
   foreach_block (block, cfg)
      num_acp += out_acp[block->num].length();

   bitset_words = BITSET_WORDS(num_acp);

   foreach_block (block, cfg) {
      bd[block->num].livein = linear_zalloc_array(lin_ctx, BITSET_WORD, bitset_words);
      bd[block->num].liveout = linear_zalloc_array(lin_ctx, BITSET_WORD, bitset_words);
      bd[block->num].copy = linear_zalloc_array(lin_ctx, BITSET_WORD, bitset_words);
      bd[block->num].kill = linear_zalloc_array(lin_ctx, BITSET_WORD, bitset_words);
      bd[block->num].undef = linear_zalloc_array(lin_ctx, BITSET_WORD, bitset_words);
      bd[block->num].reachin = linear_zalloc_array(lin_ctx, BITSET_WORD, bitset_words);
      bd[block->num].exec_mismatch = linear_zalloc_array(lin_ctx, BITSET_WORD, bitset_words);
   }

   acp = linear_zalloc_array(lin_ctx, struct acp_entry *, num_acp);

   int next_acp = 0;
   foreach_block (block, cfg) {
      for (auto iter = out_acp[block->num].begin();
           iter != out_acp[block->num].end(); ++iter) {
         acp[next_acp] = *iter;

         (*iter)->global_idx = next_acp;

         /* opt_copy_propagation_local populates out_acp with copies created
          * in a block which are still live at the end of the block.  This
          * is exactly what we want in the COPY set.
          */
         BITSET_SET(bd[block->num].copy, next_acp);

         next_acp++;
      }
   }

   assert(next_acp == num_acp);

   setup_initial_values();
   run();
}

/**
 * Like reg_offset, but register must be VGRF or FIXED_GRF.
 */
static inline unsigned
grf_reg_offset(const brw_reg &r)
{
   return (r.file == VGRF ? 0 : r.nr) * REG_SIZE +
          r.offset +
          (r.file == FIXED_GRF ? r.subnr : 0);
}

/**
 * Like regions_overlap, but register must be VGRF or FIXED_GRF.
 */
static inline bool
grf_regions_overlap(const brw_reg &r, unsigned dr, const brw_reg &s, unsigned ds)
{
   return reg_space(r) == reg_space(s) &&
          !(grf_reg_offset(r) + dr <= grf_reg_offset(s) ||
            grf_reg_offset(s) + ds <= grf_reg_offset(r));
}

/**
 * Set up initial values for each of the data flow sets, prior to running
 * the fixed-point algorithm.
 */
void
brw_copy_prop_dataflow::setup_initial_values()
{
   /* Initialize the COPY and KILL sets. */
   {
      struct acp acp_table;

      /* First, get all the KILLs for instructions which overwrite ACP
       * destinations.
       */
      for (int i = 0; i < num_acp; i++)
         acp_table.add(acp[i]);

      foreach_block (block, cfg) {
         foreach_inst_in_block(brw_inst, inst, block) {
            if (inst->dst.file != VGRF &&
                inst->dst.file != FIXED_GRF)
               continue;

            for (auto iter = acp_table.find_by_src(inst->dst.nr);
              iter != acp_table.end() && (*iter)->src.nr == inst->dst.nr;
              ++iter) {
               if (grf_regions_overlap(inst->dst, inst->size_written,
                                       (*iter)->src, (*iter)->size_read)) {
                  BITSET_SET(bd[block->num].kill, (*iter)->global_idx);
                  if (inst->force_writemask_all && !(*iter)->force_writemask_all)
                     BITSET_SET(bd[block->num].exec_mismatch, (*iter)->global_idx);
               }
            }

            if (inst->dst.file != VGRF)
               continue;

            for (auto iter = acp_table.find_by_dst(inst->dst.nr);
              iter != acp_table.end() && (*iter)->dst.nr == inst->dst.nr;
              ++iter) {
               if (grf_regions_overlap(inst->dst, inst->size_written,
                                       (*iter)->dst, (*iter)->size_written)) {
                  BITSET_SET(bd[block->num].kill, (*iter)->global_idx);
                  if (inst->force_writemask_all && !(*iter)->force_writemask_all)
                     BITSET_SET(bd[block->num].exec_mismatch, (*iter)->global_idx);
               }
            }
         }
      }
   }

   /* Populate the initial values for the livein and liveout sets.  For the
    * block at the start of the program, livein = 0 and liveout = copy.
    * For the others, set liveout and livein to ~0 (the universal set).
    */
   foreach_block (block, cfg) {
      if (block->parents.is_empty()) {
         for (int i = 0; i < bitset_words; i++) {
            bd[block->num].livein[i] = 0u;
            bd[block->num].liveout[i] = bd[block->num].copy[i];
         }
      } else {
         for (int i = 0; i < bitset_words; i++) {
            bd[block->num].liveout[i] = ~0u;
            bd[block->num].livein[i] = ~0u;
         }
      }
   }

   /* Initialize the undef set. */
   foreach_block (block, cfg) {
      for (int i = 0; i < num_acp; i++) {
         BITSET_SET(bd[block->num].undef, i);
         for (unsigned off = 0; off < acp[i]->size_written; off += REG_SIZE) {
            if (BITSET_TEST(live.block_data[block->num].defout,
                            live.var_from_reg(byte_offset(acp[i]->dst, off))))
               BITSET_CLEAR(bd[block->num].undef, i);
         }
      }
   }
}

/**
 * Walk the set of instructions in the block, marking which entries in the acp
 * are killed by the block.
 */
void
brw_copy_prop_dataflow::run()
{
   bool progress;

   do {
      progress = false;

      foreach_block (block, cfg) {
         if (block->parents.is_empty())
            continue;

         for (int i = 0; i < bitset_words; i++) {
            const BITSET_WORD old_liveout = bd[block->num].liveout[i];
            const BITSET_WORD old_reachin = bd[block->num].reachin[i];
            BITSET_WORD livein_from_any_block = 0;

            /* Update livein for this block.  If a copy is live out of all
             * parent blocks, it's live coming in to this block.
             */
            bd[block->num].livein[i] = ~0u;
            brw_foreach_list_typed(bblock_link, parent_link, link, &block->parents) {
               bblock_t *parent = parent_link->block;
               /* Consider ACP entries with a known-undefined destination to
                * be available from the parent.  This is valid because we're
                * free to set the undefined variable equal to the source of
                * the ACP entry without breaking the application's
                * expectations, since the variable is undefined.
                */
               bd[block->num].livein[i] &= (bd[parent->num].liveout[i] |
                                            bd[parent->num].undef[i]);
               livein_from_any_block |= bd[parent->num].liveout[i];

               /* Update reachin for this block.  If the end of any
                * parent block is reachable from the copy, the start
                * of this block is reachable from it as well.
                */
               bd[block->num].reachin[i] |= (bd[parent->num].reachin[i] |
                                             bd[parent->num].copy[i]);
            }

            /* Limit to the set of ACP entries that can possibly be available
             * at the start of the block, since propagating from a variable
             * which is guaranteed to be undefined (rather than potentially
             * undefined for some dynamic control-flow paths) doesn't seem
             * particularly useful.
             */
            bd[block->num].livein[i] &= livein_from_any_block;

            /* Update liveout for this block. */
            bd[block->num].liveout[i] =
               bd[block->num].copy[i] | (bd[block->num].livein[i] &
                                         ~bd[block->num].kill[i]);

            if (old_liveout != bd[block->num].liveout[i] ||
                old_reachin != bd[block->num].reachin[i])
               progress = true;
         }
      }
   } while (progress);

   /* Perform a second fixed-point pass in order to propagate the
    * exec_mismatch bitsets.  Note that this requires an accurate
    * value of the reachin bitsets as input, which isn't available
    * until the end of the first propagation pass, so this loop cannot
    * be folded into the previous one.
    */
   do {
      progress = false;

      foreach_block (block, cfg) {
         for (int i = 0; i < bitset_words; i++) {
            const BITSET_WORD old_exec_mismatch = bd[block->num].exec_mismatch[i];

            /* Update exec_mismatch for this block.  If the end of a
             * parent block is reachable by an overwrite with
             * inconsistent execution masking, the start of this block
             * is reachable by such an overwrite as well.
             */
            brw_foreach_list_typed(bblock_link, parent_link, link, &block->parents) {
               bblock_t *parent = parent_link->block;
               bd[block->num].exec_mismatch[i] |= (bd[parent->num].exec_mismatch[i] &
                                                   bd[parent->num].reachin[i]);
            }

            /* Only consider overwrites with inconsistent execution
             * masking if they are reachable from the copy, since
             * overwrites unreachable from a copy are harmless to that
             * copy.
             */
            bd[block->num].exec_mismatch[i] &= bd[block->num].reachin[i];
            if (old_exec_mismatch != bd[block->num].exec_mismatch[i])
               progress = true;
         }
      }
   } while (progress);
}

void
brw_copy_prop_dataflow::dump_block_data() const
{
   foreach_block (block, cfg) {
      brw_range range = ips.range(block);
      fprintf(stderr, "Block %d [%d, %d] (parents ", block->num,
              range.start, range.end);
      brw_foreach_list_typed(bblock_link, link, link, &block->parents) {
         bblock_t *parent = link->block;
         fprintf(stderr, "%d ", parent->num);
      }
      fprintf(stderr, "):\n");
      fprintf(stderr, "       livein = 0x");
      for (int i = 0; i < bitset_words; i++)
         fprintf(stderr, "%08x", bd[block->num].livein[i]);
      fprintf(stderr, ", liveout = 0x");
      for (int i = 0; i < bitset_words; i++)
         fprintf(stderr, "%08x", bd[block->num].liveout[i]);
      fprintf(stderr, ",\n       copy   = 0x");
      for (int i = 0; i < bitset_words; i++)
         fprintf(stderr, "%08x", bd[block->num].copy[i]);
      fprintf(stderr, ", kill    = 0x");
      for (int i = 0; i < bitset_words; i++)
         fprintf(stderr, "%08x", bd[block->num].kill[i]);
      fprintf(stderr, "\n");
   }
}

static bool
is_logic_op(enum opcode opcode)
{
   return (opcode == BRW_OPCODE_AND ||
           opcode == BRW_OPCODE_OR  ||
           opcode == BRW_OPCODE_XOR ||
           opcode == BRW_OPCODE_NOT);
}

static bool
can_take_stride(brw_inst *inst, brw_reg_type dst_type,
                unsigned arg, unsigned stride,
                const struct brw_compiler *compiler)
{
   const struct intel_device_info *devinfo = compiler->devinfo;

   if (stride > 4)
      return false;

   /* Bail if the channels of the source need to be aligned to the byte offset
    * of the corresponding channel of the destination, and the provided stride
    * would break this restriction.
    */
   if (has_dst_aligned_region_restriction(devinfo, inst, dst_type) &&
       !(brw_type_size_bytes(inst->src[arg].type) * stride ==
           brw_type_size_bytes(dst_type) * inst->dst.stride ||
         stride == 0))
      return false;

   /* 3-source instructions can only be Align16, which restricts what strides
    * they can take. They can only take a stride of 1 (the usual case), or 0
    * with a special "repctrl" bit. But the repctrl bit doesn't work for
    * 64-bit datatypes, so if the source type is 64-bit then only a stride of
    * 1 is allowed. From the Broadwell PRM, Volume 7 "3D Media GPGPU", page
    * 944:
    *
    *    This is applicable to 32b datatypes and 16b datatype. 64b datatypes
    *    cannot use the replicate control.
    */
   if (inst->is_3src(compiler)) {
      if (brw_type_size_bytes(inst->src[arg].type) > 4)
         return stride == 1;
      else
         return stride == 1 || stride == 0;
   }

   if (inst->is_math()) {
      /* Wa_22016140776:
       *
       *    Scalar broadcast on HF math (packed or unpacked) must not be used.
       *    Compiler must use a mov instruction to expand the scalar value to
       *    a vector before using in a HF (packed or unpacked) math operation.
       *
       * Prevent copy propagating a scalar value into a math instruction.
       */
      if (intel_needs_workaround(devinfo, 22016140776) &&
          stride == 0 && inst->src[arg].type == BRW_TYPE_HF) {
         return false;
      }

      /* From the Broadwell PRM, Volume 2a "Command Reference - Instructions",
       * page 391 ("Extended Math Function"):
       *
       *     The following restrictions apply for align1 mode: Scalar source
       *     is supported. Source and destination horizontal stride must be
       *     the same.
       */
      return stride == inst->dst.stride || stride == 0;
   }

   return true;
}

static bool
instruction_requires_packed_data(brw_inst *inst)
{
   switch (inst->opcode) {
   case FS_OPCODE_DDX_FINE:
   case FS_OPCODE_DDX_COARSE:
   case FS_OPCODE_DDY_FINE:
   case FS_OPCODE_DDY_COARSE:
   case SHADER_OPCODE_QUAD_SWIZZLE:
   case SHADER_OPCODE_QUAD_SWAP:
      return true;
   default:
      return false;
   }
}

static bool
try_copy_propagate(brw_shader &s, brw_inst *inst,
                   acp_entry *entry, int arg,
                   uint8_t max_polygons)
{
   if (inst->src[arg].file != VGRF)
      return false;

   const struct intel_device_info *devinfo = s.devinfo;

   assert(entry->src.file == VGRF || entry->src.file == UNIFORM ||
          entry->src.file == ATTR || entry->src.file == FIXED_GRF);

   /* Avoid propagating a LOAD_PAYLOAD instruction into another if there is a
    * good chance that we'll be able to eliminate the latter through register
    * coalescing.  If only part of the sources of the second LOAD_PAYLOAD can
    * be simplified through copy propagation we would be making register
    * coalescing impossible, ending up with unnecessary copies in the program.
    * This is also the case for is_multi_copy_payload() copies that can only
    * be coalesced when the instruction is lowered into a sequence of MOVs.
    *
    * Worse -- In cases where the ACP entry was the result of CSE combining
    * multiple LOAD_PAYLOAD subexpressions, propagating the first LOAD_PAYLOAD
    * into the second would undo the work of CSE, leading to an infinite
    * optimization loop.  Avoid this by detecting LOAD_PAYLOAD copies from CSE
    * temporaries which should match is_coalescing_payload().
    */
   if (entry->opcode == SHADER_OPCODE_LOAD_PAYLOAD &&
       (is_coalescing_payload(s, inst) ||
        is_multi_copy_payload(devinfo, inst)))
      return false;

   assert(entry->dst.file == VGRF);
   if (inst->src[arg].nr != entry->dst.nr)
      return false;

   /* Bail if inst is reading a range that isn't contained in the range
    * that entry is writing.
    */
   if (!region_contained_in(inst->src[arg], inst->size_read(devinfo, arg),
                            entry->dst, entry->size_written))
      return false;

   /* Send messages with EOT set are restricted to use g112-g127 (and we
    * sometimes need g127 for other purposes), so avoid copy propagating
    * anything that would make it impossible to satisfy that restriction.
    */
   if (inst->eot) {
      /* Don't propagate things that are already pinned. */
      if (entry->src.file != VGRF)
         return false;

      /* We might be propagating from a large register, while the SEND only
       * is reading a portion of it (say the .A channel in an RGBA value).
       * We need to pin both split SEND sources in g112-g126/127, so only
       * allow this if the registers aren't too large.
       */
      if (inst->opcode == SHADER_OPCODE_SEND && inst->sources >= 4 &&
          entry->src.file == VGRF) {
         int other_src = arg == 2 ? 3 : 2;
         unsigned other_size = inst->src[other_src].file == VGRF ?
                               s.alloc.sizes[inst->src[other_src].nr] :
                               inst->size_read(devinfo, other_src);
         unsigned prop_src_size = s.alloc.sizes[entry->src.nr];
         if (other_size + prop_src_size > 15)
            return false;
      }
   }

   /* we can't generally copy-propagate UD negations because we
    * can end up accessing the resulting values as signed integers
    * instead. See also resolve_ud_negate() and comment in
    * brw_generator::generate_code.
    */
   if (entry->src.type == BRW_TYPE_UD &&
       entry->src.negate)
      return false;

   bool has_source_modifiers = entry->src.abs || entry->src.negate;

   if (has_source_modifiers && !inst->can_do_source_mods(devinfo))
      return false;

   /* Reject cases that would violate register regioning restrictions. */
   if ((entry->src.file == UNIFORM || !entry->src.is_contiguous()) &&
       (inst->is_send_from_grf() ||
        inst->uses_indirect_addressing())) {
      return false;
   }

   /* Some instructions implemented in the generator backend, such as
    * derivatives, assume that their operands are packed so we can't
    * generally propagate strided regions to them.
    */
   const unsigned entry_stride = (entry->src.file == FIXED_GRF ? 1 :
                                  entry->src.stride);
   if (instruction_requires_packed_data(inst) && entry_stride != 1)
      return false;

   const brw_reg_type dst_type = (has_source_modifiers &&
                                  entry->dst.type != inst->src[arg].type) ?
      entry->dst.type : inst->dst.type;

   /* Bail if the result of composing both strides would exceed the
    * hardware limit.
    */
   if (!can_take_stride(inst, dst_type, arg,
                        entry_stride * inst->src[arg].stride,
                        s.compiler))
      return false;

   /* From the Cherry Trail/Braswell PRMs, Volume 7: 3D Media GPGPU:
    *    EU Overview
    *       Register Region Restrictions
    *          Special Requirements for Handling Double Precision Data Types :
    *
    *   "When source or destination datatype is 64b or operation is integer
    *    DWord multiply, regioning in Align1 must follow these rules:
    *
    *      1. Source and Destination horizontal stride must be aligned to the
    *         same qword.
    *      2. Regioning must ensure Src.Vstride = Src.Width * Src.Hstride.
    *      3. Source and Destination offset must be the same, except the case
    *         of scalar source."
    *
    * Most of this is already checked in can_take_stride(), we're only left
    * with checking 3.
    */
   if (has_dst_aligned_region_restriction(devinfo, inst, dst_type) &&
       entry_stride != 0 &&
       (reg_offset(inst->dst) % (REG_SIZE * reg_unit(devinfo))) != (reg_offset(entry->src) % (REG_SIZE * reg_unit(devinfo))))
      return false;

   /* BFloat16 sources always must be packed and not scalars,
    * so don't propagate those cases.
    */
   if (brw_type_is_bfloat(inst->src[arg].type) && entry_stride != 1)
      return false;

   /*
    * Bail if the composition of both regions would be affected by the Xe2+
    * regioning restrictions that apply to integer types smaller than a dword.
    * See BSpec #56640 for details.
    */
   const brw_reg tmp = horiz_stride(entry->src, inst->src[arg].stride);
   if (has_subdword_integer_region_restriction(devinfo, inst, &tmp, 1))
      return false;

   /* The <8;8,0> regions used for FS attributes in multipolygon
    * dispatch mode could violate regioning restrictions, don't copy
    * propagate them in such cases.
    */
   if (entry->src.file == ATTR && max_polygons > 1 &&
       (has_dst_aligned_region_restriction(devinfo, inst, dst_type) ||
	instruction_requires_packed_data(inst) ||
	(inst->is_3src(s.compiler) && arg == 2) ||
	entry->dst.type != inst->src[arg].type))
      return false;

   /* Bail if the source FIXED_GRF region of the copy cannot be trivially
    * composed with the source region of the instruction -- E.g. because the
    * copy uses some extended stride greater than 4 not supported natively by
    * the hardware as a horizontal stride, or because instruction compression
    * could require us to use a vertical stride shorter than a GRF.
    */
   if (entry->src.file == FIXED_GRF &&
       (inst->src[arg].stride > 4 ||
        inst->dst.component_size(inst->exec_size) >
        inst->src[arg].component_size(inst->exec_size)))
      return false;

   /* Bail if the instruction type is larger than the execution type of the
    * copy, what implies that each channel is reading multiple channels of the
    * destination of the copy, and simply replacing the sources would give a
    * program with different semantics.
    */
   if (brw_type_size_bits(entry->dst.type) < brw_type_size_bits(inst->src[arg].type) ||
       (entry->is_partial_write && inst->opcode != BRW_OPCODE_MOV)) {
      return false;
   }

   /* Bail if the result of composing both strides cannot be expressed
    * as another stride. This avoids, for example, trying to transform
    * this:
    *
    *     MOV (8) rX<1>UD rY<0;1,0>UD
    *     FOO (8) ...     rX<8;8,1>UW
    *
    * into this:
    *
    *     FOO (8) ...     rY<0;1,0>UW
    *
    * Which would have different semantics.
    */
   if (entry_stride != 1 &&
       (inst->src[arg].stride *
        brw_type_size_bytes(inst->src[arg].type)) % brw_type_size_bytes(entry->src.type) != 0)
      return false;

   if (has_source_modifiers) {
      /* If the sizes of the types are different the new instruction will read
       * a different amount of data than the original and the semantics will
       * always be different.
       */
      if (brw_type_size_bits(entry->dst.type) !=
          brw_type_size_bits(inst->src[arg].type))
         return false;

      if (is_logic_op(inst->opcode)) {
         /* For any value of X, X & 1 = -X & 1. In this case, source modifiers
          * from entry will not be applied to inst (far below).
          */
         if (inst->opcode != BRW_OPCODE_AND || !inst->src[1 - arg].is_one())
            return false;
      } else if (entry->dst.type != inst->src[arg].type &&
                 !inst->can_change_types()) {
         /* Since semantics of source modifiers are type-dependent we need to
          * ensure that the meaning of the instruction remains the same if we
          * change the type.
          */
         return false;
      }
   }

   /* Save the offset of inst->src[arg] relative to entry->dst for it to be
    * applied later.
    */
   const unsigned rel_offset = inst->src[arg].offset - entry->dst.offset;

   /* Fold the copy into the instruction consuming it. */
   inst->src[arg].file = entry->src.file;
   inst->src[arg].nr = entry->src.nr;
   inst->src[arg].subnr = entry->src.subnr;
   inst->src[arg].offset = entry->src.offset;

   /* Compose the strides of both regions. */
   if (entry->src.file == FIXED_GRF) {
      if (inst->src[arg].stride) {
         const unsigned orig_width = 1 << entry->src.width;
         const unsigned reg_width =
            REG_SIZE / (brw_type_size_bytes(inst->src[arg].type) *
                        inst->src[arg].stride);
         inst->src[arg].width = cvt(MIN2(orig_width, reg_width)) - 1;
         inst->src[arg].hstride = cvt(inst->src[arg].stride);
         inst->src[arg].vstride = inst->src[arg].hstride + inst->src[arg].width;
      } else {
         inst->src[arg].vstride = inst->src[arg].hstride =
            inst->src[arg].width = 0;
      }

      inst->src[arg].stride = 1;

      /* Hopefully no Align16 around here... */
      assert(entry->src.swizzle == BRW_SWIZZLE_XYZW);
      inst->src[arg].swizzle = entry->src.swizzle;
   } else {
      inst->src[arg].stride *= entry->src.stride;
   }

   /* Compute the first component of the copy that the instruction is
    * reading, and the base byte offset within that component.
    */
   assert(entry->dst.stride == 1);
   const unsigned component = rel_offset / brw_type_size_bytes(entry->dst.type);
   const unsigned suboffset = rel_offset % brw_type_size_bytes(entry->dst.type);

   /* Calculate the byte offset at the origin of the copy of the given
    * component and suboffset.
    */
   inst->src[arg] = byte_offset(inst->src[arg],
      component * entry_stride * brw_type_size_bytes(entry->src.type) + suboffset);

   if (has_source_modifiers && !is_logic_op(inst->opcode)) {
      if (entry->dst.type != inst->src[arg].type) {
         /* We are propagating source modifiers from a MOV with a different
          * type.  If we got here, then we can just change the source and
          * destination types of the instruction and keep going.
          */
         for (int i = 0; i < inst->sources; i++) {
            inst->src[i].type = entry->dst.type;
         }
         inst->dst.type = entry->dst.type;
      }

      if (!inst->src[arg].abs) {
         inst->src[arg].abs = entry->src.abs;
         inst->src[arg].negate ^= entry->src.negate;
      }
   }

   return true;
}

/**
 * Handle cases like UW subreads of a UD immediate, with an offset.
 */
static brw_reg
extract_imm(brw_reg val, brw_reg_type type, unsigned offset)
{
   assert(val.file == IMM);

   const unsigned bitsize = brw_type_size_bits(type);

   if (offset == 0 || bitsize == brw_type_size_bits(val.type))
      return val;

   /* The whole extracted value must come from bits that acutally exist in the
    * original immediate value.
    */
   assert((8 * offset) + bitsize <= brw_type_size_bits(val.type));

   val.u64 = (val.u64 >> (8 * offset)) & ((1ull << bitsize) - 1);

   return val;
}

static bool
try_constant_propagate_value(const intel_device_info *devinfo,
                             brw_reg val, brw_reg_type dst_type,
                             brw_inst *inst, int arg)
{
   bool progress = false;

   if (brw_type_size_bytes(val.type) > 4) {
      if (devinfo->ver < 20)
         return false;

      if (inst->src[arg].type != BRW_TYPE_Q &&
          inst->src[arg].type != BRW_TYPE_UQ) {
         return false;
      }

      if (brw_type_size_bits(val.type) !=
          brw_type_size_bits(dst_type)) {
         return false;
      }
   }

   /* If the size of the use type is smaller than the size of the entry,
    * clamp the value to the range of the use type.  This enables constant
    * copy propagation in cases like
    *
    *
    *    mov(8)          g12<1>UD        0x0000000cUD
    *    ...
    *    mul(8)          g47<1>D         g86<8,8,1>D     g12<16,8,2>W
    */
   if (brw_type_size_bits(inst->src[arg].type) <
       brw_type_size_bits(dst_type)) {
      if (brw_type_size_bytes(inst->src[arg].type) != 2 ||
          brw_type_size_bytes(dst_type) != 4)
         return false;

      val = extract_imm(val, inst->src[arg].type, inst->src[arg].subnr);
      val = brw_imm_uw(val.ud);
   }

   val.type = inst->src[arg].type;

   if (inst->src[arg].abs) {
      if (is_logic_op(inst->opcode) ||
          !brw_reg_abs_immediate(&val)) {
         return false;
      }
   }

   if (inst->src[arg].negate) {
      if (is_logic_op(inst->opcode) ||
          !brw_reg_negate_immediate(&val)) {
         return false;
      }
   }

   /* Some instructions can use a D or UD immediate value in a Q or UQ
    * instruction.
    */
   if (inst->src[arg].type == BRW_TYPE_Q ||
       inst->src[arg].type == BRW_TYPE_UQ) {
      if (inst->src[arg].type == BRW_TYPE_Q) {
         if (val.d64 < INT32_MIN || val.d64 > INT32_MAX)
            return false;

         val.type = BRW_TYPE_D;
      } else {
         if (val.u64 > UINT32_MAX)
            return false;

         val.type = BRW_TYPE_UD;
      }

      switch (inst->opcode) {
      case BRW_OPCODE_ADD:
      case BRW_OPCODE_ASR:
      case BRW_OPCODE_SHR:
         break;

      case BRW_OPCODE_SHL:
         /* SHL performs the shift, then expands to 64-bit. */
         if (arg == 0)
            return false;

         break;

      default:
         return false;
      }
   }

   switch (inst->opcode) {
   case BRW_OPCODE_MOV:
   case SHADER_OPCODE_LOAD_PAYLOAD:
   case SHADER_OPCODE_POW:
   case FS_OPCODE_PACK:
      inst->src[arg] = val;
      progress = true;
      break;

   case BRW_OPCODE_SUBB:
      if (arg == 1) {
         inst->src[arg] = val;
         progress = true;
      }
      break;

   case BRW_OPCODE_MACH:
   case BRW_OPCODE_MUL:
   case SHADER_OPCODE_MULH:
   case BRW_OPCODE_ADD:
   case BRW_OPCODE_XOR:
   case BRW_OPCODE_ADDC:
      if (arg == 1) {
         inst->src[arg] = val;
         progress = true;
      } else if (arg == 0 && inst->src[1].file != IMM) {
         /* We used to not copy propagate the constant in situations like
          *
          *    mov(8)          g8<1>D          0x7fffffffD
          *    mul(8)          g16<1>D         g8<8,8,1>D      g15<16,8,2>W
          *
          * On platforms that only have a 32x16 multiplier, this would
          * result in lowering the multiply to
          *
          *    mul(8)          g15<1>D         g14<8,8,1>D     0xffffUW
          *    mul(8)          g16<1>D         g14<8,8,1>D     0x7fffUW
          *    add(8)          g15.1<2>UW      g15.1<16,8,2>UW g16<16,8,2>UW
          *
          * On Gfx8 and Gfx9, which have the full 32x32 multiplier, it
          * would results in
          *
          *    mul(8)          g16<1>D         g15<16,8,2>W    0x7fffffffD
          *
          * Volume 2a of the Skylake PRM says:
          *
          *    When multiplying a DW and any lower precision integer, the
          *    DW operand must on src0.
          *
          * So it would have been invalid. However, brw_opt_combine_constants
          * will now "fix" the constant.
          */
         if (inst->opcode == BRW_OPCODE_MUL &&
             brw_type_size_bytes(inst->src[1].type) < 4 &&
             (inst->src[0].type == BRW_TYPE_D ||
              inst->src[0].type == BRW_TYPE_UD)) {
            inst->src[0] = val;
            inst->src[0].type = BRW_TYPE_D;
            progress = true;
            break;
         }

         /* Fit this constant in by commuting the operands.
          * Exception: we can't do this for 32-bit integer MUL/MACH
          * because it's asymmetric.
          *
          * The BSpec says for Broadwell that
          *
          *    "When multiplying DW x DW, the dst cannot be accumulator."
          *
          * Integer MUL with a non-accumulator destination will be lowered
          * by lower_integer_multiplication(), so don't restrict it.
          */
         if (((inst->opcode == BRW_OPCODE_MUL &&
               inst->dst.is_accumulator()) ||
              inst->opcode == BRW_OPCODE_MACH) &&
             (inst->src[1].type == BRW_TYPE_D ||
              inst->src[1].type == BRW_TYPE_UD))
            break;
         inst->src[0] = inst->src[1];
         inst->src[1] = val;
         progress = true;
      }
      break;

   case BRW_OPCODE_CMP:
      if (arg == 1) {
         inst->src[arg] = val;
         progress = true;
      } else if (arg == 0 && inst->src[1].file != IMM) {
         enum brw_conditional_mod new_cmod;

         new_cmod = brw_swap_cmod(inst->conditional_mod);
         if (new_cmod != BRW_CONDITIONAL_NONE) {
            /* Fit this constant in by swapping the operands and
             * flipping the test
             */
            inst->src[0] = inst->src[1];
            inst->src[1] = val;
            inst->conditional_mod = new_cmod;
            progress = true;
         }
      }
      break;

   case BRW_OPCODE_SEL:
      if (arg == 1) {
         inst->src[arg] = val;
         progress = true;
      } else if (arg == 0) {
         if (inst->src[1].file != IMM &&
             (inst->conditional_mod == BRW_CONDITIONAL_NONE ||
              /* Only GE and L are commutative. */
              inst->conditional_mod == BRW_CONDITIONAL_GE ||
              inst->conditional_mod == BRW_CONDITIONAL_L)) {
            inst->src[0] = inst->src[1];
            inst->src[1] = val;

            /* If this was predicated, flipping operands means
             * we also need to flip the predicate.
             */
            if (inst->conditional_mod == BRW_CONDITIONAL_NONE) {
               inst->predicate_inverse =
                  !inst->predicate_inverse;
            }
         } else {
            inst->src[0] = val;
         }

         progress = true;
      }
      break;

   case BRW_OPCODE_CSEL:
      assert(inst->conditional_mod != BRW_CONDITIONAL_NONE);

      if (arg == 0 &&
          inst->src[1].file != IMM &&
          (!brw_type_is_float(inst->src[1].type) ||
           inst->conditional_mod == BRW_CONDITIONAL_NZ ||
           inst->conditional_mod == BRW_CONDITIONAL_Z)) {
         /* Only EQ and NE are commutative due to NaN issues. */
         inst->src[0] = inst->src[1];
         inst->src[1] = val;
         inst->conditional_mod = brw_negate_cmod(inst->conditional_mod);
      } else {
         /* While CSEL is a 3-source instruction, the last source should never
          * be a constant.  We'll support that, but should it ever happen, we
          * should add support to the constant folding pass.
          */
         inst->src[arg] = val;
      }

      progress = true;
      break;

   case FS_OPCODE_FB_WRITE_LOGICAL:
      /* The stencil and omask sources of FS_OPCODE_FB_WRITE_LOGICAL are
       * bit-cast using a strided region so they cannot be immediates.
       */
      if (arg != FB_WRITE_LOGICAL_SRC_SRC_STENCIL &&
          arg != FB_WRITE_LOGICAL_SRC_OMASK) {
         inst->src[arg] = val;
         progress = true;
      }
      break;

   case SHADER_OPCODE_INT_QUOTIENT:
   case SHADER_OPCODE_INT_REMAINDER:
   case BRW_OPCODE_ADD3:
   case BRW_OPCODE_AND:
   case BRW_OPCODE_ASR:
   case BRW_OPCODE_BFE:
   case BRW_OPCODE_BFI1:
   case BRW_OPCODE_BFI2:
   case BRW_OPCODE_ROL:
   case BRW_OPCODE_ROR:
   case BRW_OPCODE_SHL:
   case BRW_OPCODE_SHR:
   case BRW_OPCODE_OR:
   case SHADER_OPCODE_TEX_LOGICAL:
   case SHADER_OPCODE_TXD_LOGICAL:
   case SHADER_OPCODE_TXF_LOGICAL:
   case SHADER_OPCODE_TXL_LOGICAL:
   case SHADER_OPCODE_TXS_LOGICAL:
   case FS_OPCODE_TXB_LOGICAL:
   case SHADER_OPCODE_TXF_CMS_W_LOGICAL:
   case SHADER_OPCODE_TXF_CMS_W_GFX12_LOGICAL:
   case SHADER_OPCODE_TXF_MCS_LOGICAL:
   case SHADER_OPCODE_LOD_LOGICAL:
   case SHADER_OPCODE_TG4_BIAS_LOGICAL:
   case SHADER_OPCODE_TG4_EXPLICIT_LOD_LOGICAL:
   case SHADER_OPCODE_TG4_IMPLICIT_LOD_LOGICAL:
   case SHADER_OPCODE_TG4_LOGICAL:
   case SHADER_OPCODE_TG4_OFFSET_LOGICAL:
   case SHADER_OPCODE_TG4_OFFSET_LOD_LOGICAL:
   case SHADER_OPCODE_TG4_OFFSET_BIAS_LOGICAL:
   case SHADER_OPCODE_SAMPLEINFO_LOGICAL:
   case SHADER_OPCODE_IMAGE_SIZE_LOGICAL:
   case SHADER_OPCODE_MEMORY_LOAD_LOGICAL:
   case SHADER_OPCODE_MEMORY_STORE_LOGICAL:
   case SHADER_OPCODE_MEMORY_ATOMIC_LOGICAL:
   case FS_OPCODE_UNIFORM_PULL_CONSTANT_LOAD:
   case FS_OPCODE_VARYING_PULL_CONSTANT_LOAD_LOGICAL:
   case SHADER_OPCODE_BROADCAST:
   case BRW_OPCODE_MAD:
   case BRW_OPCODE_LRP:
   case FS_OPCODE_PACK_HALF_2x16_SPLIT:
   case SHADER_OPCODE_SHUFFLE:
   case SHADER_OPCODE_BALLOT:
      inst->src[arg] = val;
      progress = true;
      break;

   default:
      break;
   }

   return progress;
}


static bool
try_constant_propagate(const struct intel_device_info *devinfo,
                       brw_inst *inst, acp_entry *entry, int arg)
{
   if (inst->src[arg].file != VGRF)
      return false;

   assert(entry->dst.file == VGRF);
   if (inst->src[arg].nr != entry->dst.nr)
      return false;

   /* Bail if inst is reading a range that isn't contained in the range
    * that entry is writing.
    */
   if (!region_contained_in(inst->src[arg], inst->size_read(devinfo, arg),
                            entry->dst, entry->size_written))
      return false;

   /* If the size of the use type is larger than the size of the entry
    * type, the entry doesn't contain all of the data that the user is
    * trying to use.
    */
   if (brw_type_size_bits(inst->src[arg].type) >
       brw_type_size_bits(entry->dst.type))
      return false;

   return try_constant_propagate_value(devinfo, entry->src, entry->dst.type,
                                       inst, arg);
}

static bool
can_propagate_from(const struct intel_device_info *devinfo, brw_inst *inst)
{
   return (inst->opcode == BRW_OPCODE_MOV &&
           inst->dst.file == VGRF &&
           ((inst->src[0].file == VGRF &&
             !grf_regions_overlap(inst->dst, inst->size_written,
                                  inst->src[0], inst->size_read(devinfo, 0))) ||
            inst->src[0].file == ATTR ||
            inst->src[0].file == UNIFORM ||
            inst->src[0].file == IMM ||
            (inst->src[0].file == FIXED_GRF &&
             inst->src[0].is_contiguous())) &&
           /* is_raw_move also rejects source modifiers, but copy propagation
            * can handle that if the types are the same.
            */
           ((inst->src[0].type == inst->dst.type &&
             !inst->saturate) ||
            inst->is_raw_move()) &&
           /* Subset of !is_partial_write() conditions. */
           !inst->predicate && inst->dst.is_contiguous()) ||
          is_identity_payload(devinfo, FIXED_GRF, inst);
}

static void
swap_srcs(brw_inst *inst, unsigned a, unsigned b)
{
   const auto tmp = inst->src[a];
   inst->src[a] = inst->src[b];
   inst->src[b] = tmp;
}

static void
commute_immediates(brw_inst *inst)
{
   /* ADD3 can have the immediate as src0 or src2. Using one or the other
    * consistently makes assembly dumps more readable, so we arbitrarily
    * prefer src0.
    */
   if (inst->opcode == BRW_OPCODE_ADD3) {
      if (inst->src[1].file == IMM) {
         if (inst->src[0].file != IMM)
            swap_srcs(inst, 0, 1);
         else if (inst->src[2].file != IMM)
            swap_srcs(inst, 1, 2);
      }
   }

   /* MAD can only have mutliplicand immediate in src2. */
   if (inst->opcode == BRW_OPCODE_MAD) {
      if (inst->src[1].file == IMM && inst->src[2].file != IMM)
         swap_srcs(inst, 1, 2);
   }

   /* If only one of the sources of a 2-source, commutative instruction (e.g.,
    * AND) is immediate, it must be src1. If both are immediate, opt_algebraic
    * should fold it away.
    */
   if (inst->sources == 2 && inst->is_commutative() &&
       inst->src[0].file == IMM && inst->src[1].file != IMM) {
      const auto src1 = inst->src[1];
      inst->src[1] = inst->src[0];
      inst->src[0] = src1;
   }
}

/* Walks a basic block and does copy propagation on it using the acp
 * list.
 */
static bool
opt_copy_propagation_local(brw_shader &s, linear_ctx *lin_ctx,
                           bblock_t *block, struct acp &acp,
                           uint8_t max_polygons)
{
   const struct intel_device_info *devinfo = s.devinfo;
   bool progress = false;

   foreach_inst_in_block(brw_inst, inst, block) {
      /* The non-defs copy propagation passes should not be called while
       * LOAD_REG instructions still exist.
       */
      assert(inst->opcode != SHADER_OPCODE_LOAD_REG);

      /* Try propagating into this instruction. */
      bool constant_progress = false;
      for (int i = inst->sources - 1; i >= 0; i--) {
         if (inst->src[i].file != VGRF)
            continue;

         for (auto iter = acp.find_by_dst(inst->src[i].nr);
              iter != acp.end() && (*iter)->dst.nr == inst->src[i].nr;
              ++iter) {
            if ((*iter)->src.file == IMM) {
               if (try_constant_propagate(devinfo, inst, *iter, i)) {
                  constant_progress = true;
                  break;
               }
            } else {
               if (try_copy_propagate(s, inst, *iter, i, max_polygons)) {
                  progress = true;
                  break;
               }
            }
         }
      }

      if (constant_progress) {
         commute_immediates(inst);
         brw_opt_constant_fold_instruction(devinfo, inst);
         progress = true;
      }

      /* kill the destination from the ACP */
      if (inst->dst.file == VGRF || inst->dst.file == FIXED_GRF) {
         for (auto iter = acp.find_by_dst(inst->dst.nr);
              iter != acp.end() && (*iter)->dst.nr == inst->dst.nr;
              ++iter) {
            if (grf_regions_overlap((*iter)->dst, (*iter)->size_written,
                                    inst->dst, inst->size_written))
               acp.remove(*iter);
         }

         for (auto iter = acp.find_by_src(inst->dst.nr);
              iter != acp.end() && (*iter)->src.nr == inst->dst.nr;
              ++iter) {
            /* Make sure we kill the entry if this instruction overwrites
             * _any_ of the registers that it reads
             */
            if (grf_regions_overlap((*iter)->src, (*iter)->size_read,
                                    inst->dst, inst->size_written))
               acp.remove(*iter);
         }
      }

      /* If this instruction's source could potentially be folded into the
       * operand of another instruction, add it to the ACP.
       */
      if (can_propagate_from(devinfo, inst)) {
         acp_entry *entry = linear_zalloc(lin_ctx, acp_entry);
         entry->dst = inst->dst;
         entry->src = inst->src[0];
         entry->size_written = inst->size_written;
         for (unsigned i = 0; i < inst->sources; i++)
            entry->size_read += inst->size_read(devinfo, i);
         entry->opcode = inst->opcode;
         entry->is_partial_write = inst->is_partial_write();
         entry->force_writemask_all = inst->force_writemask_all;
         acp.add(entry);
      } else if (inst->opcode == SHADER_OPCODE_LOAD_PAYLOAD &&
                 inst->dst.file == VGRF) {
         int offset = 0;
         for (int i = 0; i < inst->sources; i++) {
            int effective_width = i < inst->header_size ? 8 : inst->exec_size;
            const unsigned size_written =
               effective_width * brw_type_size_bytes(inst->src[i].type);
            if (inst->src[i].file == VGRF ||
                (inst->src[i].file == FIXED_GRF &&
                 inst->src[i].is_contiguous())) {
               const brw_reg_type t = i < inst->header_size ?
                  BRW_TYPE_UD : inst->src[i].type;
               brw_reg dst = byte_offset(retype(inst->dst, t), offset);
               if (!dst.equals(inst->src[i])) {
                  acp_entry *entry = linear_zalloc(lin_ctx, acp_entry);
                  entry->dst = dst;
                  entry->src = retype(inst->src[i], t);
                  entry->size_written = size_written;
                  entry->size_read = inst->size_read(devinfo, i);
                  entry->opcode = inst->opcode;
                  entry->force_writemask_all = inst->force_writemask_all;
                  acp.add(entry);
               }
            }
            offset += size_written;
         }
      }
   }

   return progress;
}

bool
brw_opt_copy_propagation(brw_shader &s)
{
   bool progress = false;
   void *copy_prop_ctx = ralloc_context(NULL);
   linear_ctx *lin_ctx = linear_context(copy_prop_ctx);
   struct acp *out_acp = new (lin_ctx) acp[s.cfg->num_blocks];

   const brw_live_variables &live = s.live_analysis.require();
   const brw_ip_ranges &ips = s.ip_ranges_analysis.require();

   /* First, walk through each block doing local copy propagation and getting
    * the set of copies available at the end of the block.
    */
   foreach_block (block, s.cfg) {
      progress = opt_copy_propagation_local(s, lin_ctx, block,
                                            out_acp[block->num],
                                            s.max_polygons) || progress;

      /* If the destination of an ACP entry exists only within this block,
       * then there's no need to keep it for dataflow analysis.  We can delete
       * it from the out_acp table and avoid growing the bitsets any bigger
       * than we absolutely have to.
       *
       * Because nothing in opt_copy_propagation_local touches the block
       * start/end IPs and opt_copy_propagation_local is incapable of
       * extending the live range of an ACP destination beyond the block,
       * it's safe to use the liveness information in this way.
       */
      for (auto iter = out_acp[block->num].begin();
           iter != out_acp[block->num].end(); ++iter) {
         assert((*iter)->dst.file == VGRF);

         brw_range block_range = ips.range(block);
         brw_range vgrf_range  = live.vgrf_range[(*iter)->dst.nr];

         if (block_range.contains(vgrf_range))
            out_acp[block->num].remove(*iter);
      }
   }

   /* Do dataflow analysis for those available copies. */
   brw_copy_prop_dataflow dataflow(lin_ctx, s.cfg, live, ips, out_acp);

   /* Next, re-run local copy propagation, this time with the set of copies
    * provided by the dataflow analysis available at the start of a block.
    */
   foreach_block (block, s.cfg) {
      struct acp in_acp;

      for (int i = 0; i < dataflow.num_acp; i++) {
         if (BITSET_TEST(dataflow.bd[block->num].livein, i) &&
             !BITSET_TEST(dataflow.bd[block->num].exec_mismatch, i)) {
            struct acp_entry *entry = dataflow.acp[i];
            in_acp.add(entry);
         }
      }

      progress = opt_copy_propagation_local(s, lin_ctx, block,
                                            in_acp, s.max_polygons) ||
                 progress;
   }

   ralloc_free(copy_prop_ctx);

   if (progress)
      s.invalidate_analysis(BRW_DEPENDENCY_INSTRUCTION_DATA_FLOW |
                            BRW_DEPENDENCY_INSTRUCTION_DETAIL);

   return progress;
}

static bool
try_copy_propagate_def(brw_shader &s,
                       brw_inst *def, const brw_reg &val,
                       brw_inst *inst, int arg,
                       uint8_t max_polygons)
{
   const struct intel_device_info *devinfo = s.devinfo;

   assert(val.file != BAD_FILE);

   /* We can't generally copy-propagate UD negations because we can end up
    * accessing the resulting values as signed integers instead.
    */
   if (val.negate && val.type == BRW_TYPE_UD)
      return false;

   /* Bail if the instruction type is larger than the execution type of the
    * copy, what implies that each channel is reading multiple channels of the
    * destination of the copy, and simply replacing the sources would give a
    * program with different semantics.
    */
   if (brw_type_size_bits(def->dst.type) <
       brw_type_size_bits(inst->src[arg].type))
      return false;

   const bool has_source_modifiers = val.abs || val.negate;

   if (has_source_modifiers) {
      if (!inst->can_do_source_mods(devinfo))
         return false;

      /* If the sizes of the types are different the new instruction will read
       * a different amount of data than the original and the semantics will
       * always be different.
       */
      if (brw_type_size_bits(def->dst.type) !=
          brw_type_size_bits(inst->src[arg].type)) {
         return false;
      }

      if (is_logic_op(inst->opcode)) {
         /* For any value of X, X & 1 = -X & 1. In this case, source modifiers
          * from entry will not be applied to inst (far below).
          */
         if (inst->opcode != BRW_OPCODE_AND || !inst->src[1 - arg].is_one())
            return false;
      } else if (def->dst.type != inst->src[arg].type &&
                 !inst->can_change_types()) {
         /* Since semantics of source modifiers are type-dependent we need to
          * ensure that the meaning of the instruction remains the same if we
          * change the type.
          */
         return false;
      }
   }

   /* Send messages with EOT set are restricted to use g112-g127 (and we
    * sometimes need g127 for other purposes), so avoid copy propagating
    * anything that would make it impossible to satisfy that restriction.
    */
   if (inst->eot) {
      /* Don't propagate things that are already pinned. */
      if (val.file != VGRF)
         return false;

      /* We might be propagating from a large register, while the SEND only
       * is reading a portion of it (say the .A channel in an RGBA value).
       * We need to pin both split SEND sources in g112-g126/127, so only
       * allow this if the registers aren't too large.
       */
      if (inst->opcode == SHADER_OPCODE_SEND && inst->sources >= 4 &&
          val.file == VGRF) {
         int other_src = arg == 2 ? 3 : 2;
         unsigned other_size = inst->src[other_src].file == VGRF ?
                               s.alloc.sizes[inst->src[other_src].nr] :
                               inst->size_read(devinfo, other_src);
         unsigned prop_src_size = s.alloc.sizes[val.nr];
         if (other_size + prop_src_size > 15)
            return false;
      }
   }

   /* Reject cases that would violate register regioning restrictions. */
   if (inst->opcode == SHADER_OPCODE_BROADCAST) {
      if (arg == 0)
         return false;

      assert(inst->src[arg].stride == 0);
   } else if ((val.file == UNIFORM || !val.is_contiguous()) &&
       (inst->is_send_from_grf() || inst->uses_indirect_addressing())) {
      return false;
   }

   /* Some instructions implemented in the generator backend, such as
    * derivatives, assume that their operands are packed so we can't
    * generally propagate strided regions to them.
    */
   const unsigned entry_stride = val.file == FIXED_GRF ? 1 : val.stride;
   if (instruction_requires_packed_data(inst) && entry_stride != 1)
      return false;

   /* load_reg loads a whole VGRF into a def. It is not allowed for the source
    * to have a stride or a non-zero offset (unless stride == 0). It is
    * allowed for the source to to be uniform.
    */
   if (inst->opcode == SHADER_OPCODE_LOAD_REG &&
       !is_uniform(val) &&
       (val.offset != 0 || entry_stride > 1)) {
      return false;
   }

   const brw_reg_type dst_type = (has_source_modifiers &&
                                  def->dst.type != inst->src[arg].type) ?
      def->dst.type : inst->dst.type;

   /* Bail if the result of composing both strides would exceed the
    * hardware limit.
    */
   if (!can_take_stride(inst, dst_type, arg,
                        entry_stride * inst->src[arg].stride,
                        s.compiler))
      return false;

   /* Bail if the source FIXED_GRF region of the copy cannot be trivially
    * composed with the source region of the instruction -- E.g. because the
    * copy uses some extended stride greater than 4 not supported natively by
    * the hardware as a horizontal stride, or because instruction compression
    * could require us to use a vertical stride shorter than a GRF.
    */
   if (val.file == FIXED_GRF &&
       (inst->src[arg].stride > 4 ||
        inst->dst.component_size(inst->exec_size) >
        inst->src[arg].component_size(inst->exec_size)))
      return false;

   /* Bail if the result of composing both strides cannot be expressed
    * as another stride. This avoids, for example, trying to transform
    * this:
    *
    *     MOV (8) rX<1>UD rY<0;1,0>UD
    *     FOO (8) ...     rX<8;8,1>UW
    *
    * into this:
    *
    *     FOO (8) ...     rY<0;1,0>UW
    *
    * Which would have different semantics.
    */
   if (entry_stride != 1 &&
       (inst->src[arg].stride *
        brw_type_size_bytes(inst->src[arg].type)) % brw_type_size_bytes(val.type) != 0)
      return false;

   /* From the Cherry Trail/Braswell PRMs, Volume 7: 3D Media GPGPU:
    *    EU Overview
    *       Register Region Restrictions
    *          Special Requirements for Handling Double Precision Data Types :
    *
    *   "When source or destination datatype is 64b or operation is integer
    *    DWord multiply, regioning in Align1 must follow these rules:
    *
    *      1. Source and Destination horizontal stride must be aligned to the
    *         same qword.
    *      2. Regioning must ensure Src.Vstride = Src.Width * Src.Hstride.
    *      3. Source and Destination offset must be the same, except the case
    *         of scalar source."
    *
    * Most of this is already checked in can_take_stride(), we're only left
    * with checking 3.
    */
   if (has_dst_aligned_region_restriction(devinfo, inst, dst_type) &&
       entry_stride != 0 &&
       (reg_offset(inst->dst) % (REG_SIZE * reg_unit(devinfo))) != (reg_offset(val) % (REG_SIZE * reg_unit(devinfo))))
      return false;

   /* BFloat16 sources always must be packed and not scalars,
    * so don't propagate those cases.
    */
   if (brw_type_is_bfloat(inst->src[arg].type) && entry_stride != 1)
      return false;

   /* The <8;8,0> regions used for FS attributes in multipolygon
    * dispatch mode could violate regioning restrictions, don't copy
    * propagate them in such cases.
    */
   if (max_polygons > 1 && val.file == ATTR &&
       (has_dst_aligned_region_restriction(devinfo, inst, dst_type) ||
        instruction_requires_packed_data(inst) ||
        (inst->is_3src(s.compiler) && arg == 2) ||
        def->dst.type != inst->src[arg].type))
      return false;

   /* Fold the copy into the instruction consuming it. */
   inst->src[arg].file = val.file;
   inst->src[arg].nr = val.nr;
   inst->src[arg].subnr = val.subnr;
   inst->src[arg].offset = val.offset;

   /* Compose the strides of both regions. */
   if (val.file == FIXED_GRF) {
      if (inst->src[arg].stride) {
         const unsigned orig_width = 1 << val.width;
         const unsigned reg_width =
            REG_SIZE / (brw_type_size_bytes(inst->src[arg].type) *
                        inst->src[arg].stride);
         inst->src[arg].width = cvt(MIN2(orig_width, reg_width)) - 1;
         inst->src[arg].hstride = cvt(inst->src[arg].stride);
         inst->src[arg].vstride = inst->src[arg].hstride + inst->src[arg].width;
      } else {
         inst->src[arg].vstride = inst->src[arg].hstride =
            inst->src[arg].width = 0;
      }

      inst->src[arg].stride = 1;

      /* Hopefully no Align16 around here... */
      assert(val.swizzle == BRW_SWIZZLE_XYZW);
      inst->src[arg].swizzle = val.swizzle;
   } else {
      inst->src[arg].stride *= val.stride;
   }

   /* Handle NoMask cases where the def replicates a small scalar to a number
    * of channels, but the use is a lower SIMD width but larger type, so each
    * invocation reads multiple channels worth of data, e.g.
    *
    *    mov(16) vgrf1:UW, u0<0>:UW NoMask
    *    mov(8)  vgrf2:UD, vgrf1:UD NoMask group0
    *
    * In this case, we should just use the scalar's type.
    */
   if (val.stride == 0 &&
       inst->opcode == BRW_OPCODE_MOV &&
       inst->force_writemask_all && def->force_writemask_all &&
       inst->exec_size < def->exec_size &&
       (inst->exec_size * brw_type_size_bytes(inst->src[arg].type) ==
        def->exec_size * brw_type_size_bytes(val.type))) {
      inst->src[arg].type = val.type;
      inst->dst.type = val.type;
      inst->exec_size = def->exec_size;
   }

   if (has_source_modifiers && !is_logic_op(inst->opcode)) {
      if (def->dst.type != inst->src[arg].type) {
         /* We are propagating source modifiers from a MOV with a different
          * type.  If we got here, then we can just change the source and
          * destination types of the instruction and keep going.
          */
         for (int i = 0; i < inst->sources; i++) {
            inst->src[i].type = def->dst.type;
         }
         inst->dst.type = def->dst.type;
      }

      if (!inst->src[arg].abs) {
         inst->src[arg].abs = val.abs;
         inst->src[arg].negate ^= val.negate;
      }
   }

   return true;
}

static bool
try_constant_propagate_def(const struct intel_device_info *devinfo,
                           brw_inst *def, brw_reg val, brw_inst *inst, int arg)
{
   /* Bail if inst is reading more than a single vector component of entry */
   if (inst->size_read(devinfo, arg) > def->dst.component_size(inst->exec_size))
      return false;

   return try_constant_propagate_value(devinfo, val, def->dst.type, inst, arg);
}

static brw_reg
find_value_for_offset(brw_inst *def, const brw_reg &src, unsigned src_size)
{
   brw_reg val;

   switch (def->opcode) {
   case BRW_OPCODE_MOV:
      /* is_raw_move also rejects source modifiers, but copy propagation
       * can handle that if the tyeps are the same.
       */
      if ((def->dst.type == def->src[0].type || def->is_raw_move()) &&
          def->src[0].stride <= 1) {
         val = def->src[0];

         unsigned rel_offset = src.offset - def->dst.offset;

         if (val.stride == 0)
            rel_offset %= brw_type_size_bytes(def->dst.type);

         if (val.file == IMM)
            val = extract_imm(val, src.type, rel_offset);
         else
            val = byte_offset(def->src[0], rel_offset);
      }
      break;
   case SHADER_OPCODE_LOAD_PAYLOAD: {
      unsigned offset = 0;
      for (int i = def->header_size; i < def->sources; i++) {
         /* Ignore the source splat if the source is a scalar. In that case
          * always use just the first component.
          */
         const unsigned splat =
            (def->src[i].stride == 0 && !src.is_scalar) || def->src[i].file == IMM ? def->exec_size : 1;
         const unsigned component_size =
            def->src[i].component_size(def->exec_size);

         if (offset == src.offset) {
            if (def->dst.type == def->src[i].type &&
                def->src[i].stride <= 1 &&
                (component_size * splat == src_size ||
                 (def->src[i].file == IMM && component_size == src_size)))
               val = def->src[i];

            break;
         }

         offset += def->exec_size * brw_type_size_bytes(def->src[i].type);
      }
      break;
   }
   case SHADER_OPCODE_LOAD_REG: {
      val = def->src[0];

      unsigned rel_offset = src.offset - def->dst.offset;

      if (val.stride == 0)
         rel_offset %= brw_type_size_bytes(def->dst.type);

      if (val.file == IMM)
         val = extract_imm(val, src.type, rel_offset);
      else
         val = byte_offset(def->src[0], rel_offset);

      break;
   }
   default:
      break;
   }

   return val;
}

bool
brw_opt_copy_propagation_defs(brw_shader &s)
{
   const brw_def_analysis &defs = s.def_analysis.require();
   unsigned *uses_deleted = new unsigned[defs.count()]();
   bool progress = false;

   foreach_block_and_inst_safe(block, brw_inst, inst, s.cfg) {
      /* Try propagating into this instruction. */
      bool constant_progress = false;

      for (int i = inst->sources - 1; i >= 0; i--) {
         brw_inst *def = defs.get(inst->src[i]);

         if (!def || def->saturate)
            continue;

         bool source_progress = false;

         if (def->opcode == SHADER_OPCODE_LOAD_PAYLOAD) {
            if (inst->size_read(s.devinfo, i) == def->size_written &&
                def->src[0].file != BAD_FILE && def->src[0].file != IMM &&
                is_identity_payload(s.devinfo, def->src[0].file, def)) {
               source_progress =
                  try_copy_propagate_def(s, def, def->src[0],
                                         inst, i, s.max_polygons);

               if (source_progress) {
                  progress = true;
                  ++uses_deleted[def->dst.nr];
                  if (defs.get_use_count(def->dst) == uses_deleted[def->dst.nr])
                     def->remove();
               }

               continue;
            }
         }

         /* Only propagate through a load_reg if the source is a def or a
          * UNIFORM (since these are also always invariant). The destination
          * of a load_reg is always a def (by definition).
          */
         if (def->opcode == SHADER_OPCODE_LOAD_REG &&
             (defs.get(def->src[0]) == NULL && def->src[0].file != UNIFORM)) {
            continue;
         }

         brw_reg val =
            find_value_for_offset(def, inst->src[i], inst->size_read(s.devinfo, i));

         if (val.file == IMM) {
               if (try_constant_propagate_def(s.devinfo, def, val, inst, i)) {
               source_progress = true;
               constant_progress = true;
            }
         } else if (val.file == VGRF ||
                    val.file == ATTR || val.file == UNIFORM ||
                    (val.file == FIXED_GRF && val.is_contiguous())) {
            source_progress =
               try_copy_propagate_def(s, def, val, inst, i, s.max_polygons);
         }

         if (source_progress) {
            progress = true;
            ++uses_deleted[def->dst.nr];

            /* We can copy propagate through an instruction like
             *
             *    mov.nz.f0.0(8) %2:D, -%78:D
             *
             * but deleting the instruction may alter the program.
             */
            if (def->conditional_mod == BRW_CONDITIONAL_NONE &&
                defs.get_use_count(def->dst) == uses_deleted[def->dst.nr]) {
               def->remove();
            }
         }
      }

      if (constant_progress) {
         commute_immediates(inst);
         brw_opt_constant_fold_instruction(s.compiler->devinfo, inst);
      }
   }

   if (progress) {
      s.invalidate_analysis(BRW_DEPENDENCY_INSTRUCTIONS);
   }

   delete [] uses_deleted;

   return progress;
}

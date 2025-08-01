/*
 * Copyright © 2016 Rob Clark <robclark@freedesktop.org>
 * Copyright © 2018 Google, Inc.
 * SPDX-License-Identifier: MIT
 *
 * Authors:
 *    Rob Clark <robclark@freedesktop.org>
 */

#define FD_BO_NO_HARDPIN 1

#include <initializer_list>

#include "pipe/p_state.h"
#include "util/bitset.h"
#include "util/format/u_format.h"
#include "util/u_inlines.h"
#include "util/u_memory.h"
#include "util/u_string.h"

#include "freedreno_program.h"

#include "fd6_const.h"
#include "fd6_emit.h"
#include "fd6_pack.h"
#include "fd6_program.h"
#include "fd6_texture.h"

/**
 * Temporary program building state.
 */
struct program_builder {
   struct fd6_program_state *state;
   struct fd_context *ctx;
   const struct ir3_cache_key *key;
   const struct ir3_shader_variant *vs;
   const struct ir3_shader_variant *hs;
   const struct ir3_shader_variant *ds;
   const struct ir3_shader_variant *gs;
   const struct ir3_shader_variant *fs;
   const struct ir3_shader_variant *last_shader;
   bool binning_pass;
};

template <chip CHIP>
struct xs_config {
   uint16_t reg_sp_xs_instrlen;
   uint16_t reg_hlsq_xs_ctrl;
   uint16_t reg_sp_xs_first_exec_offset;
   uint16_t reg_sp_xs_pvt_mem_hw_stack_offset;
   uint16_t reg_sp_xs_vgpr_config;
};

template <chip CHIP>
static const struct xs_config<CHIP> xs_configs[] = {
   [MESA_SHADER_VERTEX] = {
      REG_A6XX_SP_VS_INSTR_SIZE,
      CHIP == A6XX ? REG_A6XX_SP_VS_CONST_CONFIG : REG_A7XX_SP_VS_CONST_CONFIG,
      REG_A6XX_SP_VS_PROGRAM_COUNTER_OFFSET,
      REG_A6XX_SP_VS_PVT_MEM_STACK_OFFSET,
      REG_A7XX_SP_VS_VGS_CNTL,
   },
   [MESA_SHADER_TESS_CTRL] = {
      REG_A6XX_SP_HS_INSTR_SIZE,
      CHIP == A6XX ? REG_A6XX_SP_HS_CONST_CONFIG : REG_A7XX_SP_HS_CONST_CONFIG,
      REG_A6XX_SP_HS_PROGRAM_COUNTER_OFFSET,
      REG_A6XX_SP_HS_PVT_MEM_STACK_OFFSET,
      REG_A7XX_SP_HS_VGS_CNTL,
   },
   [MESA_SHADER_TESS_EVAL] = {
      REG_A6XX_SP_DS_INSTR_SIZE,
      CHIP == A6XX ? REG_A6XX_SP_DS_CONST_CONFIG : REG_A7XX_SP_DS_CONST_CONFIG,
      REG_A6XX_SP_DS_PROGRAM_COUNTER_OFFSET,
      REG_A6XX_SP_DS_PVT_MEM_STACK_OFFSET,
      REG_A7XX_SP_DS_VGS_CNTL,
   },
   [MESA_SHADER_GEOMETRY] = {
      REG_A6XX_SP_GS_INSTR_SIZE,
      CHIP == A6XX ? REG_A6XX_SP_GS_CONST_CONFIG : REG_A7XX_SP_GS_CONST_CONFIG,
      REG_A6XX_SP_GS_PROGRAM_COUNTER_OFFSET,
      REG_A6XX_SP_GS_PVT_MEM_STACK_OFFSET,
      REG_A7XX_SP_GS_VGS_CNTL,
   },
   [MESA_SHADER_FRAGMENT] = {
      REG_A6XX_SP_PS_INSTR_SIZE,
      CHIP == A6XX ? REG_A6XX_SP_PS_CONST_CONFIG : REG_A7XX_SP_PS_CONST_CONFIG,
      REG_A6XX_SP_PS_PROGRAM_COUNTER_OFFSET,
      REG_A6XX_SP_PS_PVT_MEM_STACK_OFFSET,
      REG_A7XX_SP_PS_VGS_CNTL,
   },
   [MESA_SHADER_COMPUTE] = {
      REG_A6XX_SP_CS_INSTR_SIZE,
      CHIP == A6XX ? REG_A6XX_SP_CS_CONST_CONFIG : REG_A7XX_SP_CS_CONST_CONFIG,
      REG_A6XX_SP_CS_PROGRAM_COUNTER_OFFSET,
      REG_A6XX_SP_CS_PVT_MEM_STACK_OFFSET,
      REG_A7XX_SP_CS_VGS_CNTL,
   },
};

template <chip CHIP>
void
fd6_emit_shader(struct fd_context *ctx, struct fd_ringbuffer *ring,
                const struct ir3_shader_variant *so)
{
   if (!so) {
      /* shader stage disabled */
      return;
   }

#if MESA_DEBUG
   /* Name should generally match what you get with MESA_SHADER_CAPTURE_PATH: */
   const char *name = so->name;
   if (name)
      fd_emit_string5(ring, name, strlen(name));
#endif

   gl_shader_stage type = so->type;
   if (type == MESA_SHADER_KERNEL)
      type = MESA_SHADER_COMPUTE;

   enum a6xx_threadsize thrsz =
      so->info.double_threadsize ? THREAD128 : THREAD64;

   switch (type) {
   case MESA_SHADER_VERTEX:
      OUT_REG(ring, A6XX_SP_VS_CNTL_0(
               .halfregfootprint = so->info.max_half_reg + 1,
               .fullregfootprint = so->info.max_reg + 1,
               .branchstack = ir3_shader_branchstack_hw(so),
               .mergedregs = so->mergedregs,
               .earlypreamble = so->early_preamble,
      ));
      break;
   case MESA_SHADER_TESS_CTRL:
      OUT_REG(ring, A6XX_SP_HS_CNTL_0(
               .halfregfootprint = so->info.max_half_reg + 1,
               .fullregfootprint = so->info.max_reg + 1,
               .branchstack = ir3_shader_branchstack_hw(so),
               .earlypreamble = so->early_preamble,
      ));
      break;
   case MESA_SHADER_TESS_EVAL:
      OUT_REG(ring, A6XX_SP_DS_CNTL_0(
               .halfregfootprint = so->info.max_half_reg + 1,
               .fullregfootprint = so->info.max_reg + 1,
               .branchstack = ir3_shader_branchstack_hw(so),
               .earlypreamble = so->early_preamble,
      ));
      break;
   case MESA_SHADER_GEOMETRY:
      OUT_REG(ring, A6XX_SP_GS_CNTL_0(
               .halfregfootprint = so->info.max_half_reg + 1,
               .fullregfootprint = so->info.max_reg + 1,
               .branchstack = ir3_shader_branchstack_hw(so),
               .earlypreamble = so->early_preamble,
      ));
      break;
   case MESA_SHADER_FRAGMENT:
      OUT_REG(ring, A6XX_SP_PS_CNTL_0(
               .halfregfootprint = so->info.max_half_reg + 1,
               .fullregfootprint = so->info.max_reg + 1,
               .branchstack = ir3_shader_branchstack_hw(so),
               .threadsize = thrsz,
               .varying = so->total_in != 0,
               .lodpixmask = so->need_full_quad,
               .inoutregoverlap = true,
               .pixlodenable = so->need_pixlod,
               .earlypreamble = so->early_preamble,
               .mergedregs = so->mergedregs,
      ));
      break;
   case MESA_SHADER_COMPUTE:
      thrsz = ctx->screen->info->a6xx.supports_double_threadsize ? thrsz : THREAD128;
      OUT_REG(ring, A6XX_SP_CS_CNTL_0(
               .halfregfootprint = so->info.max_half_reg + 1,
               .fullregfootprint = so->info.max_reg + 1,
               .branchstack = ir3_shader_branchstack_hw(so),
               .threadsize = thrsz,
               .earlypreamble = so->early_preamble,
               .mergedregs = so->mergedregs,
      ));
      break;
   default:
      UNREACHABLE("bad shader stage");
   }

   const struct xs_config<CHIP> *cfg = &xs_configs<CHIP>[type];

   OUT_PKT4(ring, cfg->reg_sp_xs_instrlen, 1);
   OUT_RING(ring, so->instrlen);

   /* emit program binary & private memory layout
    */

   ir3_get_private_mem(ctx, so);

   uint32_t per_sp_size = ctx->pvtmem[so->pvtmem_per_wave].per_sp_size;

   fd_ringbuffer_attach_bo(ring, so->bo);

   OUT_PKT4(ring, cfg->reg_sp_xs_first_exec_offset, 7);
   OUT_RING(ring, 0);                /* SP_xS_OBJ_FIRST_EXEC_OFFSET */
   OUT_RELOC(ring, so->bo, 0, 0, 0); /* SP_xS_OBJ_START_LO */
   OUT_RING(ring, A6XX_SP_VS_PVT_MEM_PARAM_MEMSIZEPERITEM(ctx->pvtmem[so->pvtmem_per_wave].per_fiber_size));
   if (so->pvtmem_size > 0) { /* SP_xS_PVT_MEM_ADDR */
      fd_ringbuffer_attach_bo(ring, ctx->pvtmem[so->pvtmem_per_wave].bo);
      OUT_RELOC(ring, ctx->pvtmem[so->pvtmem_per_wave].bo, 0, 0, 0);
   } else {
      OUT_RING(ring, 0);
      OUT_RING(ring, 0);
   }
   OUT_RING(ring, A6XX_SP_VS_PVT_MEM_SIZE_TOTALPVTMEMSIZE(per_sp_size) |
                     COND(so->pvtmem_per_wave,
                          A6XX_SP_VS_PVT_MEM_SIZE_PERWAVEMEMLAYOUT));

   OUT_PKT4(ring, cfg->reg_sp_xs_pvt_mem_hw_stack_offset, 1);
   OUT_RING(ring, A6XX_SP_VS_PVT_MEM_STACK_OFFSET_OFFSET(per_sp_size));

   if (CHIP >= A7XX) {
      OUT_PKT4(ring, cfg->reg_sp_xs_vgpr_config, 1);
      OUT_RING(ring, 0);
   }

   if (CHIP == A6XX) {
      uint32_t shader_preload_size =
         MIN2(so->instrlen, ctx->screen->info->a6xx.instr_cache_size);

      enum a6xx_state_block sb = fd6_stage2shadersb(so->type);
      OUT_PKT7(ring, fd6_stage2opcode(so->type), 3);
      OUT_RING(ring, CP_LOAD_STATE6_0_DST_OFF(0) |
                        CP_LOAD_STATE6_0_STATE_TYPE(ST6_SHADER) |
                        CP_LOAD_STATE6_0_STATE_SRC(SS6_INDIRECT) |
                        CP_LOAD_STATE6_0_STATE_BLOCK(sb) |
                        CP_LOAD_STATE6_0_NUM_UNIT(shader_preload_size));
      OUT_RELOC(ring, so->bo, 0, 0, 0);
   }

   fd6_emit_immediates<CHIP>(so, ring);
}
FD_GENX(fd6_emit_shader);

/**
 * Build a pre-baked state-obj to disable SO, so that we aren't dynamically
 * building this at draw time whenever we transition from SO enabled->disabled
 */
static void
setup_stream_out_disable(struct fd_context *ctx)
{
   unsigned sizedw = 4;

   if (ctx->screen->info->a6xx.tess_use_shared)
      sizedw += 2;

   struct fd_ringbuffer *ring =
      fd_ringbuffer_new_object(ctx->pipe, (1 + sizedw) * 4);

   OUT_PKT7(ring, CP_CONTEXT_REG_BUNCH, sizedw);
   OUT_RING(ring, REG_A6XX_VPC_SO_MAPPING_WPTR);
   OUT_RING(ring, 0);
   OUT_RING(ring, REG_A6XX_VPC_SO_CNTL);
   OUT_RING(ring, 0);

   if (ctx->screen->info->a6xx.tess_use_shared) {
      OUT_RING(ring, REG_A6XX_PC_DGEN_SO_CNTL);
      OUT_RING(ring, 0);
   }

   fd6_context(ctx)->streamout_disable_stateobj = ring;
}

static void
setup_stream_out(struct fd_context *ctx, struct fd6_program_state *state,
                 const struct ir3_shader_variant *v,
                 struct ir3_shader_linkage *l)
{
   const struct ir3_stream_output_info *strmout = &v->stream_output;

   /* Note: 64 here comes from the HW layout of the program RAM. The program
    * for stream N is at DWORD 64 * N.
    */
#define A6XX_SO_PROG_DWORDS 64
   uint32_t prog[A6XX_SO_PROG_DWORDS * IR3_MAX_SO_STREAMS] = {};
   BITSET_DECLARE(valid_dwords, A6XX_SO_PROG_DWORDS * IR3_MAX_SO_STREAMS) = {0};

   memset(prog, 0, sizeof(prog));

   for (unsigned i = 0; i < strmout->num_outputs; i++) {
      const struct ir3_stream_output *out = &strmout->output[i];
      unsigned k = out->register_index;
      unsigned idx;

      /* linkage map sorted by order frag shader wants things, so
       * a bit less ideal here..
       */
      for (idx = 0; idx < l->cnt; idx++)
         if (l->var[idx].slot == v->outputs[k].slot)
            break;

      assert(idx < l->cnt);

      for (unsigned j = 0; j < out->num_components; j++) {
         unsigned c = j + out->start_component;
         unsigned loc = l->var[idx].loc + c;
         unsigned off = j + out->dst_offset; /* in dwords */

         unsigned dword = out->stream * A6XX_SO_PROG_DWORDS + loc/2;
         if (loc & 1) {
            prog[dword] |= A6XX_VPC_SO_MAPPING_PORT_B_EN |
                           A6XX_VPC_SO_MAPPING_PORT_B_BUF(out->output_buffer) |
                           A6XX_VPC_SO_MAPPING_PORT_B_OFF(off * 4);
         } else {
            prog[dword] |= A6XX_VPC_SO_MAPPING_PORT_A_EN |
                           A6XX_VPC_SO_MAPPING_PORT_A_BUF(out->output_buffer) |
                           A6XX_VPC_SO_MAPPING_PORT_A_OFF(off * 4);
         }
         BITSET_SET(valid_dwords, dword);
      }
   }

   unsigned prog_count = 0;
   unsigned start, end;
   BITSET_FOREACH_RANGE (start, end, valid_dwords,
                         A6XX_SO_PROG_DWORDS * IR3_MAX_SO_STREAMS) {
      prog_count += end - start + 1;
   }

   const bool emit_pc_so_stream_cntl =
         ctx->screen->info->a6xx.tess_use_shared &&
         v->type == MESA_SHADER_TESS_EVAL;

   unsigned sizedw = 10 + (2 * prog_count);
   if (emit_pc_so_stream_cntl)
      sizedw += 2;

   struct fd_ringbuffer *ring =
      fd_ringbuffer_new_object(ctx->pipe, (1 + sizedw) * 4);

   OUT_PKT7(ring, CP_CONTEXT_REG_BUNCH, sizedw);
   OUT_RING(ring, REG_A6XX_VPC_SO_CNTL);
   OUT_RING(ring,
            A6XX_VPC_SO_CNTL_STREAM_ENABLE(strmout->streams_written) |
            COND(strmout->stride[0] > 0,
                 A6XX_VPC_SO_CNTL_BUF0_STREAM(1 + strmout->output[0].stream)) |
            COND(strmout->stride[1] > 0,
                 A6XX_VPC_SO_CNTL_BUF1_STREAM(1 + strmout->output[1].stream)) |
            COND(strmout->stride[2] > 0,
                 A6XX_VPC_SO_CNTL_BUF2_STREAM(1 + strmout->output[2].stream)) |
            COND(strmout->stride[3] > 0,
                 A6XX_VPC_SO_CNTL_BUF3_STREAM(1 + strmout->output[3].stream)));
   OUT_RING(ring, REG_A6XX_VPC_SO_BUFFER_STRIDE(0));
   OUT_RING(ring, strmout->stride[0]);
   OUT_RING(ring, REG_A6XX_VPC_SO_BUFFER_STRIDE(1));
   OUT_RING(ring, strmout->stride[1]);
   OUT_RING(ring, REG_A6XX_VPC_SO_BUFFER_STRIDE(2));
   OUT_RING(ring, strmout->stride[2]);
   OUT_RING(ring, REG_A6XX_VPC_SO_BUFFER_STRIDE(3));
   OUT_RING(ring, strmout->stride[3]);

   bool first = true;
   BITSET_FOREACH_RANGE (start, end, valid_dwords,
                         A6XX_SO_PROG_DWORDS * IR3_MAX_SO_STREAMS) {
      OUT_RING(ring, REG_A6XX_VPC_SO_MAPPING_WPTR);
      OUT_RING(ring, COND(first, A6XX_VPC_SO_MAPPING_WPTR_RESET) |
                     A6XX_VPC_SO_MAPPING_WPTR_ADDR(start));
      for (unsigned i = start; i < end; i++) {
         OUT_RING(ring, REG_A6XX_VPC_SO_MAPPING_PORT);
         OUT_RING(ring, prog[i]);
      }
      first = false;
   }

   if (emit_pc_so_stream_cntl) {
      /* Possibly not tess_use_shared related, but the combination of
       * tess + xfb fails some tests if we don't emit this.
       */
      OUT_RING(ring, REG_A6XX_PC_DGEN_SO_CNTL);
      OUT_RING(ring, A6XX_PC_DGEN_SO_CNTL_STREAM_ENABLE(0x1));
   }

   state->streamout_stateobj = ring;
}

static uint32_t
sp_xs_config(const struct ir3_shader_variant *v)
{
   if (!v)
      return 0;

   return A6XX_SP_VS_CONFIG_ENABLED |
         COND(v->bindless_tex, A6XX_SP_VS_CONFIG_BINDLESS_TEX) |
         COND(v->bindless_samp, A6XX_SP_VS_CONFIG_BINDLESS_SAMP) |
         COND(v->bindless_ibo, A6XX_SP_VS_CONFIG_BINDLESS_UAV) |
         COND(v->bindless_ubo, A6XX_SP_VS_CONFIG_BINDLESS_UBO) |
         A6XX_SP_VS_CONFIG_NUAV(ir3_shader_num_uavs(v)) |
         A6XX_SP_VS_CONFIG_NTEX(v->num_samp) |
         A6XX_SP_VS_CONFIG_NSAMP(v->num_samp);
}

template <chip CHIP>
static void
setup_config_stateobj(struct fd_context *ctx, struct fd6_program_state *state)
{
   struct fd_ringbuffer *ring = fd_ringbuffer_new_object(ctx->pipe, 100 * 4);

   OUT_REG(ring, SP_UPDATE_CNTL(CHIP, .vs_state = true, .hs_state = true,
                                          .ds_state = true, .gs_state = true,
                                          .fs_state = true, .cs_state = true,
                                          .cs_uav = true, .gfx_uav = true, ));

   assert(state->vs->constlen >= state->bs->constlen);

   OUT_REG(ring, SP_VS_CONST_CONFIG(
         CHIP,
         .constlen = state->vs->constlen,
         .enabled = true,
   ));
   OUT_REG(ring, SP_HS_CONST_CONFIG(
         CHIP,
         .constlen = COND(state->hs, state->hs->constlen),
         .enabled = COND(state->hs, true),
   ));
   OUT_REG(ring, SP_DS_CONST_CONFIG(
         CHIP,
         .constlen = COND(state->ds, state->ds->constlen),
         .enabled = COND(state->ds, true),
   ));
   OUT_REG(ring, SP_GS_CONST_CONFIG(
         CHIP,
         .constlen = COND(state->gs, state->gs->constlen),
         .enabled = COND(state->gs, true),
   ));
   OUT_REG(ring, SP_PS_CONST_CONFIG(
         CHIP,
         .constlen = state->fs->constlen,
         .enabled = true,
   ));

   OUT_PKT4(ring, REG_A6XX_SP_VS_CONFIG, 1);
   OUT_RING(ring, sp_xs_config(state->vs));

   OUT_PKT4(ring, REG_A6XX_SP_HS_CONFIG, 1);
   OUT_RING(ring, sp_xs_config(state->hs));

   OUT_PKT4(ring, REG_A6XX_SP_DS_CONFIG, 1);
   OUT_RING(ring, sp_xs_config(state->ds));

   OUT_PKT4(ring, REG_A6XX_SP_GS_CONFIG, 1);
   OUT_RING(ring, sp_xs_config(state->gs));

   OUT_PKT4(ring, REG_A6XX_SP_PS_CONFIG, 1);
   OUT_RING(ring, sp_xs_config(state->fs));

   OUT_PKT4(ring, REG_A6XX_SP_GFX_USIZE, 1);
   OUT_RING(ring, ir3_shader_num_uavs(state->fs));

   state->config_stateobj = ring;
}

static inline uint32_t
next_regid(uint32_t reg, uint32_t increment)
{
   if (VALIDREG(reg))
      return reg + increment;
   else
      return INVALID_REG;
}

static enum a6xx_tess_output
primitive_to_tess(enum mesa_prim primitive)
{
   switch (primitive) {
   case MESA_PRIM_POINTS:
      return TESS_POINTS;
   case MESA_PRIM_LINE_STRIP:
      return TESS_LINES;
   case MESA_PRIM_TRIANGLE_STRIP:
      return TESS_CW_TRIS;
   default:
      UNREACHABLE("");
   }
}

#define MAX_VERTEX_ATTRIBS 32

static void
emit_vfd_dest(struct fd_ringbuffer *ring, const struct ir3_shader_variant *vs)
{
   uint32_t attr_count = 0;

   for (uint32_t i = 0; i < vs->inputs_count; i++)
      if (!vs->inputs[i].sysval)
         attr_count++;

   OUT_REG(ring, A6XX_VFD_CNTL_0(
                     .fetch_cnt = attr_count, /* decode_cnt for binning pass ? */
                     .decode_cnt = attr_count));

   if (attr_count)
      OUT_PKT4(ring, REG_A6XX_VFD_DEST_CNTL_INSTR(0), attr_count);

   for (uint32_t i = 0; i < attr_count; i++) {
      assert(!vs->inputs[i].sysval);
      OUT_RING(ring,
               A6XX_VFD_DEST_CNTL_INSTR_WRITEMASK(vs->inputs[i].compmask) |
                  A6XX_VFD_DEST_CNTL_INSTR_REGID(vs->inputs[i].regid));
   }
}

static void
emit_vs_system_values(struct fd_ringbuffer *ring,
                      const struct program_builder *b)
{
   const uint32_t vertexid_regid =
         ir3_find_sysval_regid(b->vs, SYSTEM_VALUE_VERTEX_ID);
   const uint32_t instanceid_regid =
         ir3_find_sysval_regid(b->vs, SYSTEM_VALUE_INSTANCE_ID);
   const uint32_t tess_coord_x_regid =
         ir3_find_sysval_regid(b->ds, SYSTEM_VALUE_TESS_COORD);
   const uint32_t tess_coord_y_regid = next_regid(tess_coord_x_regid, 1);
   const uint32_t hs_rel_patch_regid =
         ir3_find_sysval_regid(b->hs, SYSTEM_VALUE_REL_PATCH_ID_IR3);
   const uint32_t ds_rel_patch_regid =
         ir3_find_sysval_regid(b->ds, SYSTEM_VALUE_REL_PATCH_ID_IR3);
   const uint32_t hs_invocation_regid =
         ir3_find_sysval_regid(b->hs, SYSTEM_VALUE_TCS_HEADER_IR3);
   const uint32_t gs_primitiveid_regid =
         ir3_find_sysval_regid(b->gs, SYSTEM_VALUE_PRIMITIVE_ID);
   const uint32_t vs_primitiveid_regid = b->hs ?
         ir3_find_sysval_regid(b->hs, SYSTEM_VALUE_PRIMITIVE_ID) :
         gs_primitiveid_regid;
   const uint32_t ds_primitiveid_regid =
         ir3_find_sysval_regid(b->ds, SYSTEM_VALUE_PRIMITIVE_ID);
   const uint32_t gsheader_regid =
         ir3_find_sysval_regid(b->gs, SYSTEM_VALUE_GS_HEADER_IR3);

   /* Note: we currently don't support multiview.
    */
   const uint32_t viewid_regid = INVALID_REG;

   OUT_PKT4(ring, REG_A6XX_VFD_CNTL_1, 6);
   OUT_RING(ring, A6XX_VFD_CNTL_1_REGID4VTX(vertexid_regid) |
                  A6XX_VFD_CNTL_1_REGID4INST(instanceid_regid) |
                  A6XX_VFD_CNTL_1_REGID4PRIMID(vs_primitiveid_regid) |
                  A6XX_VFD_CNTL_1_REGID4VIEWID(viewid_regid));
   OUT_RING(ring, A6XX_VFD_CNTL_2_REGID_HSRELPATCHID(hs_rel_patch_regid) |
                  A6XX_VFD_CNTL_2_REGID_INVOCATIONID(hs_invocation_regid));
   OUT_RING(ring, A6XX_VFD_CNTL_3_REGID_DSRELPATCHID(ds_rel_patch_regid) |
                  A6XX_VFD_CNTL_3_REGID_TESSX(tess_coord_x_regid) |
                  A6XX_VFD_CNTL_3_REGID_TESSY(tess_coord_y_regid) |
                  A6XX_VFD_CNTL_3_REGID_DSPRIMID(ds_primitiveid_regid));
   OUT_RING(ring, 0x000000fc); /* VFD_CNTL_4 */
   OUT_RING(ring, A6XX_VFD_CNTL_5_REGID_GSHEADER(gsheader_regid) |
                  0xfc00); /* VFD_CNTL_5 */
   OUT_RING(ring, COND(b->fs->reads_primid, A6XX_VFD_CNTL_6_PRIMID4PSEN)); /* VFD_CNTL_6 */
}

template <chip CHIP>
static void
emit_vpc(struct fd_ringbuffer *ring, const struct program_builder *b)
{
   const struct ir3_shader_variant *last_shader = b->last_shader;

   /* note: doesn't compile as static because of the array regs.. */
   const struct reg_config {
      uint16_t reg_sp_xs_out_reg;
      uint16_t reg_sp_xs_vpc_dst_reg;
      uint16_t reg_vpc_xs_pack;
      uint16_t reg_vpc_xs_clip_cntl;
      uint16_t reg_vpc_xs_clip_cntl_v2;
      uint16_t reg_gras_xs_cl_cntl;
      uint16_t reg_pc_xs_out_cntl;
      uint16_t reg_sp_xs_primitive_cntl;
      uint16_t reg_vpc_xs_layer_cntl;
      uint16_t reg_vpc_xs_layer_cntl_v2;
      uint16_t reg_gras_xs_layer_cntl;
   } reg_config[] = {
      [MESA_SHADER_VERTEX] = {
         REG_A6XX_SP_VS_OUTPUT_REG(0),
         REG_A6XX_SP_VS_VPC_DEST_REG(0),
         REG_A6XX_VPC_VS_CNTL,
         REG_A6XX_VPC_VS_CLIP_CULL_CNTL,
         REG_A6XX_VPC_VS_CLIP_CULL_CNTL_V2,
         REG_A6XX_GRAS_CL_VS_CLIP_CULL_DISTANCE,
         REG_A6XX_PC_VS_CNTL,
         REG_A6XX_SP_VS_OUTPUT_CNTL,
         REG_A6XX_VPC_VS_SIV_CNTL,
         REG_A6XX_VPC_VS_SIV_CNTL_V2,
         REG_A6XX_GRAS_SU_VS_SIV_CNTL,
      },
      [MESA_SHADER_TESS_CTRL] = {
         0,
         0,
         0,
         0,
         0,
         0,
         REG_A6XX_PC_HS_CNTL,
         0,
         0,
         0,
         0
      },
      [MESA_SHADER_TESS_EVAL] = {
         REG_A6XX_SP_DS_OUTPUT_REG(0),
         REG_A6XX_SP_DS_VPC_DEST_REG(0),
         REG_A6XX_VPC_DS_CNTL,
         REG_A6XX_VPC_DS_CLIP_CULL_CNTL,
         REG_A6XX_VPC_DS_CLIP_CULL_CNTL_V2,
         REG_A6XX_GRAS_CL_DS_CLIP_CULL_DISTANCE,
         REG_A6XX_PC_DS_CNTL,
         REG_A6XX_SP_DS_OUTPUT_CNTL,
         REG_A6XX_VPC_DS_SIV_CNTL,
         REG_A6XX_VPC_DS_SIV_CNTL_V2,
         REG_A6XX_GRAS_SU_DS_SIV_CNTL,
      },
      [MESA_SHADER_GEOMETRY] = {
         REG_A6XX_SP_GS_OUTPUT_REG(0),
         REG_A6XX_SP_GS_VPC_DEST_REG(0),
         REG_A6XX_VPC_GS_CNTL,
         REG_A6XX_VPC_GS_CLIP_CULL_CNTL,
         REG_A6XX_VPC_GS_CLIP_CULL_CNTL_V2,
         REG_A6XX_GRAS_CL_GS_CLIP_CULL_DISTANCE,
         REG_A6XX_PC_GS_CNTL,
         REG_A6XX_SP_GS_OUTPUT_CNTL,
         REG_A6XX_VPC_GS_SIV_CNTL,
         REG_A6XX_VPC_GS_SIV_CNTL_V2,
         REG_A6XX_GRAS_SU_GS_SIV_CNTL,
      },
   };
   const struct reg_config *cfg = &reg_config[b->last_shader->type];

   struct ir3_shader_linkage linkage = {
      .primid_loc = 0xff,
      .clip0_loc = 0xff,
      .clip1_loc = 0xff,
   };

   /* If we have streamout, link against the real FS, rather than the
    * dummy FS used for binning pass state, to ensure the OUTLOC's
    * match.  Depending on whether we end up doing sysmem or gmem,
    * the actual streamout could happen with either the binning pass
    * or draw pass program, but the same streamout stateobj is used
    * in either case:
    */
   bool do_streamout = (b->last_shader->stream_output.num_outputs > 0);
   ir3_link_shaders(&linkage, b->last_shader,
                    do_streamout ? b->state->fs : b->fs,
                    true);

   if (do_streamout)
      ir3_link_stream_out(&linkage, b->last_shader);

   emit_vs_system_values(ring, b);

   OUT_PKT4(ring, REG_A6XX_VPC_VARYING_LM_TRANSFER_CNTL_0_DISABLE(0), 4);
   OUT_RING(ring, ~linkage.varmask[0]);
   OUT_RING(ring, ~linkage.varmask[1]);
   OUT_RING(ring, ~linkage.varmask[2]);
   OUT_RING(ring, ~linkage.varmask[3]);

   /* a6xx finds position/pointsize at the end */
   const uint32_t position_regid =
      ir3_find_output_regid(last_shader, VARYING_SLOT_POS);
   const uint32_t pointsize_regid =
      ir3_find_output_regid(last_shader, VARYING_SLOT_PSIZ);
   const uint32_t layer_regid =
      ir3_find_output_regid(last_shader, VARYING_SLOT_LAYER);
   const uint32_t view_regid =
      ir3_find_output_regid(last_shader, VARYING_SLOT_VIEWPORT);
   const uint32_t clip0_regid =
      ir3_find_output_regid(last_shader, VARYING_SLOT_CLIP_DIST0);
   const uint32_t clip1_regid =
      ir3_find_output_regid(last_shader, VARYING_SLOT_CLIP_DIST1);
   uint32_t flags_regid = b->gs ?
      ir3_find_output_regid(b->gs, VARYING_SLOT_GS_VERTEX_FLAGS_IR3) : 0;

   uint32_t pointsize_loc = 0xff, position_loc = 0xff, layer_loc = 0xff, view_loc = 0xff;

// XXX replace regid(63,0) with INVALID_REG
   if (layer_regid != INVALID_REG) {
      layer_loc = linkage.max_loc;
      ir3_link_add(&linkage, VARYING_SLOT_LAYER, layer_regid, 0x1, linkage.max_loc);
   }

   if (view_regid != INVALID_REG) {
      view_loc = linkage.max_loc;
      ir3_link_add(&linkage, VARYING_SLOT_VIEWPORT, view_regid, 0x1, linkage.max_loc);
   }

   if (position_regid != INVALID_REG) {
      position_loc = linkage.max_loc;
      ir3_link_add(&linkage, VARYING_SLOT_POS, position_regid, 0xf, linkage.max_loc);
   }

   if (pointsize_regid != INVALID_REG) {
      pointsize_loc = linkage.max_loc;
      ir3_link_add(&linkage, VARYING_SLOT_PSIZ, pointsize_regid, 0x1, linkage.max_loc);
   }

   uint8_t clip_mask = last_shader->clip_mask,
           cull_mask = last_shader->cull_mask;
   uint8_t clip_cull_mask = clip_mask | cull_mask;

   clip_mask &= b->key->clip_plane_enable;

   /* Handle the case where clip/cull distances aren't read by the FS */
   uint32_t clip0_loc = linkage.clip0_loc, clip1_loc = linkage.clip1_loc;
   if (clip0_loc == 0xff && clip0_regid != INVALID_REG) {
      clip0_loc = linkage.max_loc;
      ir3_link_add(&linkage, VARYING_SLOT_CLIP_DIST0, clip0_regid,
                   clip_cull_mask & 0xf, linkage.max_loc);
   }
   if (clip1_loc == 0xff && clip1_regid != INVALID_REG) {
      clip1_loc = linkage.max_loc;
      ir3_link_add(&linkage, VARYING_SLOT_CLIP_DIST1, clip1_regid,
                   clip_cull_mask >> 4, linkage.max_loc);
   }

   /* If we have stream-out, we use the full shader for binning
    * pass, rather than the optimized binning pass one, so that we
    * have all the varying outputs available for xfb.  So streamout
    * state should always be derived from the non-binning pass
    * program:
    */
   if (do_streamout && !b->binning_pass) {
      setup_stream_out(b->ctx, b->state, b->last_shader, &linkage);

      if (!fd6_context(b->ctx)->streamout_disable_stateobj)
         setup_stream_out_disable(b->ctx);
   }

   /* There is a hardware bug on a750 where STRIDE_IN_VPC of 5 to 8 in GS with
    * an input primitive type with adjacency, an output primitive type of
    * points, and a high enough vertex count causes a hang.
    */
   if (b->ctx->screen->info->a7xx.gs_vpc_adjacency_quirk &&
       b->gs && b->gs->gs.output_primitive == MESA_PRIM_POINTS &&
       linkage.max_loc > 4) {
      linkage.max_loc = MAX2(linkage.max_loc, 9);
   }

   /* The GPU hangs on some models when there are no outputs (xs_pack::CNT),
    * at least when a DS is the last stage, so add a dummy output to keep it
    * happy if there aren't any. We do this late in order to avoid emitting
    * any unused code and make sure that optimizations don't remove it.
    */
   if (linkage.cnt == 0)
      ir3_link_add(&linkage, 0, 0, 0x1, linkage.max_loc);

   /* map outputs of the last shader to VPC */
   assert(linkage.cnt <= 32);
   const uint32_t sp_out_count = DIV_ROUND_UP(linkage.cnt, 2);
   const uint32_t sp_vpc_dst_count = DIV_ROUND_UP(linkage.cnt, 4);
   uint16_t sp_out[32] = {0};
   uint8_t sp_vpc_dst[32] = {0};
   for (uint32_t i = 0; i < linkage.cnt; i++) {
      sp_out[i] =
         A6XX_SP_VS_OUTPUT_REG_A_REGID(linkage.var[i].regid) |
         A6XX_SP_VS_OUTPUT_REG_A_COMPMASK(linkage.var[i].compmask);
      sp_vpc_dst[i] =
         A6XX_SP_VS_VPC_DEST_REG_OUTLOC0(linkage.var[i].loc);
   }

   OUT_PKT4(ring, cfg->reg_sp_xs_out_reg, sp_out_count);
   OUT_BUF(ring, sp_out, sp_out_count);

   OUT_PKT4(ring, cfg->reg_sp_xs_vpc_dst_reg, sp_vpc_dst_count);
   OUT_BUF(ring, sp_vpc_dst, sp_vpc_dst_count);

   OUT_PKT4(ring, cfg->reg_vpc_xs_pack, 1);
   OUT_RING(ring, A6XX_VPC_VS_CNTL_POSITIONLOC(position_loc) |
                  A6XX_VPC_VS_CNTL_PSIZELOC(pointsize_loc) |
                  A6XX_VPC_VS_CNTL_STRIDE_IN_VPC(linkage.max_loc));

   OUT_PKT4(ring, cfg->reg_vpc_xs_clip_cntl, 1);
   OUT_RING(ring, A6XX_VPC_VS_CLIP_CULL_CNTL_CLIP_MASK(clip_cull_mask) |
                  A6XX_VPC_VS_CLIP_CULL_CNTL_CLIP_DIST_03_LOC(clip0_loc) |
                  A6XX_VPC_VS_CLIP_CULL_CNTL_CLIP_DIST_47_LOC(clip1_loc));

   OUT_PKT4(ring, cfg->reg_vpc_xs_clip_cntl_v2, 1);
   OUT_RING(ring, A6XX_VPC_VS_CLIP_CULL_CNTL_CLIP_MASK(clip_cull_mask) |
                  A6XX_VPC_VS_CLIP_CULL_CNTL_CLIP_DIST_03_LOC(clip0_loc) |
                  A6XX_VPC_VS_CLIP_CULL_CNTL_CLIP_DIST_47_LOC(clip1_loc));

   OUT_PKT4(ring, cfg->reg_gras_xs_cl_cntl, 1);
   OUT_RING(ring, A6XX_GRAS_CL_VS_CLIP_CULL_DISTANCE_CLIP_MASK(clip_mask) |
                  A6XX_GRAS_CL_VS_CLIP_CULL_DISTANCE_CULL_MASK(cull_mask));

   const struct ir3_shader_variant *geom_stages[] = { b->vs, b->hs, b->ds, b->gs };

   for (unsigned i = 0; i < ARRAY_SIZE(geom_stages); i++) {
      const struct ir3_shader_variant *shader = geom_stages[i];
      if (!shader)
         continue;

      bool primid = shader->type != MESA_SHADER_VERTEX &&
         VALIDREG(ir3_find_sysval_regid(shader, SYSTEM_VALUE_PRIMITIVE_ID));

      OUT_PKT4(ring, reg_config[shader->type].reg_pc_xs_out_cntl, 1);
      if (shader == last_shader) {
         OUT_RING(ring, A6XX_PC_VS_CNTL_STRIDE_IN_VPC(linkage.max_loc) |
                        CONDREG(pointsize_regid, A6XX_PC_VS_CNTL_PSIZE) |
                        CONDREG(layer_regid, A6XX_PC_VS_CNTL_LAYER) |
                        CONDREG(view_regid, A6XX_PC_VS_CNTL_VIEW) |
                        COND(primid, A6XX_PC_VS_CNTL_PRIMITIVE_ID) |
                        COND(primid, A6XX_PC_GS_CNTL_PRIMITIVE_ID) |
                        A6XX_PC_VS_CNTL_CLIP_MASK(clip_cull_mask));
      } else {
         OUT_RING(ring, COND(primid, A6XX_PC_VS_CNTL_PRIMITIVE_ID));
      }
   }

   /* if vertex_flags somehow gets optimized out, your gonna have a bad time: */
   assert(flags_regid != INVALID_REG);

   OUT_PKT4(ring, cfg->reg_sp_xs_primitive_cntl, 1);
   OUT_RING(ring, A6XX_SP_VS_OUTPUT_CNTL_OUT(linkage.cnt) |
                  A6XX_SP_GS_OUTPUT_CNTL_FLAGS_REGID(flags_regid));

   OUT_PKT4(ring, cfg->reg_vpc_xs_layer_cntl, 1);
   OUT_RING(ring, A6XX_VPC_VS_SIV_CNTL_LAYERLOC(layer_loc) |
                  A6XX_VPC_VS_SIV_CNTL_VIEWLOC(view_loc) |
                  A6XX_VPC_VS_SIV_CNTL_SHADINGRATELOC(0xff));

   OUT_PKT4(ring, cfg->reg_vpc_xs_layer_cntl_v2, 1);
   OUT_RING(ring, A6XX_VPC_VS_SIV_CNTL_LAYERLOC(layer_loc) |
                  A6XX_VPC_VS_SIV_CNTL_VIEWLOC(view_loc) |
                  A6XX_VPC_VS_SIV_CNTL_SHADINGRATELOC(0xff));

   OUT_PKT4(ring, cfg->reg_gras_xs_layer_cntl, 1);
   OUT_RING(ring, CONDREG(layer_regid, A6XX_GRAS_SU_VS_SIV_CNTL_WRITES_LAYER) |
                  CONDREG(view_regid, A6XX_GRAS_SU_VS_SIV_CNTL_WRITES_VIEW));

   OUT_REG(ring, A6XX_PC_PS_CNTL(b->fs->reads_primid));

   if (CHIP >= A7XX) {
      OUT_REG(ring, A6XX_GRAS_UNKNOWN_8110(0x2));
      OUT_REG(ring, A7XX_SP_RENDER_CNTL(.fs_disable = false));
   }

   OUT_PKT4(ring, REG_A6XX_VPC_PS_CNTL, 1);
   OUT_RING(ring, A6XX_VPC_PS_CNTL_NUMNONPOSVAR(b->fs->total_in) |
                  COND(b->fs->total_in, A6XX_VPC_PS_CNTL_VARYING) |
                  A6XX_VPC_PS_CNTL_PRIMIDLOC(linkage.primid_loc) |
                  A6XX_VPC_PS_CNTL_VIEWIDLOC(linkage.viewid_loc));

   if (b->hs) {
      OUT_PKT4(ring, REG_A6XX_PC_HS_PARAM_0, 1);
      OUT_RING(ring, b->hs->tess.tcs_vertices_out);

      fd6_emit_link_map<CHIP>(b->ctx, b->vs, b->hs, ring);
      fd6_emit_link_map<CHIP>(b->ctx, b->hs, b->ds, ring);
   }

   if (b->gs) {
      uint32_t vertices_out, invocations, vec4_size;
      uint32_t prev_stage_output_size =
         b->ds ? b->ds->output_size : b->vs->output_size;

      if (b->hs) {
         fd6_emit_link_map<CHIP>(b->ctx, b->ds, b->gs, ring);
      } else {
         fd6_emit_link_map<CHIP>(b->ctx, b->vs, b->gs, ring);
      }

      vertices_out = MAX2(1, b->gs->gs.vertices_out) - 1;
      enum a6xx_tess_output output =
         primitive_to_tess((enum mesa_prim)b->gs->gs.output_primitive);
      invocations = b->gs->gs.invocations - 1;
      /* Size of per-primitive alloction in ldlw memory in vec4s. */
      vec4_size = b->gs->gs.vertices_in *
                  DIV_ROUND_UP(prev_stage_output_size, 4);

      OUT_PKT4(ring, REG_A6XX_PC_GS_PARAM_0, 1);
      OUT_RING(ring,
            A6XX_PC_GS_PARAM_0_GS_VERTICES_OUT(vertices_out) |
            A6XX_PC_GS_PARAM_0_GS_OUTPUT(output) |
            A6XX_PC_GS_PARAM_0_GS_INVOCATIONS(invocations));

      if (CHIP >= A7XX) {
         OUT_REG(ring,
            A7XX_VPC_GS_PARAM_0(
               .gs_vertices_out = vertices_out,
               .gs_invocations = invocations,
               .gs_output = output,
            )
         );
      } else {
         OUT_PKT4(ring, REG_A6XX_VPC_GS_PARAM, 1);
         OUT_RING(ring, 0xff);
      }

      if (CHIP == A6XX) {
         OUT_PKT4(ring, REG_A6XX_PC_PRIMITIVE_CNTL_6, 1);
         OUT_RING(ring, A6XX_PC_PRIMITIVE_CNTL_6_STRIDE_IN_VPC(vec4_size));
      }

      uint32_t prim_size = prev_stage_output_size;
      if (prim_size > 64)
         prim_size = 64;
      else if (prim_size == 64)
         prim_size = 63;

      OUT_PKT4(ring, REG_A6XX_SP_GS_CNTL_1, 1);
      OUT_RING(ring, prim_size);
   }
}

static enum a6xx_tex_prefetch_cmd
tex_opc_to_prefetch_cmd(opc_t tex_opc)
{
   switch (tex_opc) {
   case OPC_SAM:
      return TEX_PREFETCH_SAM;
   default:
      UNREACHABLE("Unknown tex opc for prefeth cmd");
   }
}

template <chip CHIP>
static void
emit_fs_inputs(struct fd_ringbuffer *ring, const struct program_builder *b)
{
   const struct ir3_shader_variant *fs = b->fs;
   uint32_t face_regid, coord_regid, zwcoord_regid, samp_id_regid;
   uint32_t ij_regid[IJ_COUNT];
   uint32_t smask_in_regid;

   bool sample_shading = fs->sample_shading;
   bool enable_varyings = fs->total_in > 0;

   samp_id_regid   = ir3_find_sysval_regid(fs, SYSTEM_VALUE_SAMPLE_ID);
   smask_in_regid  = ir3_find_sysval_regid(fs, SYSTEM_VALUE_SAMPLE_MASK_IN);
   face_regid      = ir3_find_sysval_regid(fs, SYSTEM_VALUE_FRONT_FACE);
   coord_regid     = ir3_find_sysval_regid(fs, SYSTEM_VALUE_FRAG_COORD);
   zwcoord_regid   = VALIDREG(coord_regid) ? coord_regid + 2 : INVALID_REG;
   for (unsigned i = 0; i < ARRAY_SIZE(ij_regid); i++)
      ij_regid[i] = ir3_find_sysval_regid(fs, SYSTEM_VALUE_BARYCENTRIC_PERSP_PIXEL + i);

   if (fs->num_sampler_prefetch > 0) {
      /* FS prefetch reads coordinates from r0.x */
      assert(!VALIDREG(ij_regid[fs->prefetch_bary_type]) ||
             ij_regid[fs->prefetch_bary_type] == regid(0, 0));
   }

   OUT_PKT4(ring, REG_A6XX_SP_PS_INITIAL_TEX_LOAD_CNTL, 1 + fs->num_sampler_prefetch);
   OUT_RING(ring, A6XX_SP_PS_INITIAL_TEX_LOAD_CNTL_COUNT(fs->num_sampler_prefetch) |
                     COND(CHIP >= A7XX, A6XX_SP_PS_INITIAL_TEX_LOAD_CNTL_CONSTSLOTID(0x1ff)) |
                     COND(CHIP >= A7XX, A6XX_SP_PS_INITIAL_TEX_LOAD_CNTL_CONSTSLOTID4COORD(0x1ff)) |
                     COND(!VALIDREG(ij_regid[IJ_PERSP_PIXEL]),
                          A6XX_SP_PS_INITIAL_TEX_LOAD_CNTL_IJ_WRITE_DISABLE) |
                     COND(fs->prefetch_end_of_quad,
                          A6XX_SP_PS_INITIAL_TEX_LOAD_CNTL_ENDOFQUAD));
   for (int i = 0; i < fs->num_sampler_prefetch; i++) {
      const struct ir3_sampler_prefetch *prefetch = &fs->sampler_prefetch[i];
      OUT_RING(ring, SP_PS_INITIAL_TEX_LOAD_CMD(
            CHIP, i,
            .src = prefetch->src,
            /* For a7xx, samp_id/tex_id is always in SP_PS_INITIAL_TEX_INDEX_CMD[n]
             * even in the non-bindless case (which probably makes the reg name
             * wrong)
             */
            .samp_id = (CHIP == A6XX) ? prefetch->samp_id : 0,
            .tex_id = (CHIP == A6XX) ? prefetch->tex_id : 0,
            .dst = prefetch->dst,
            .wrmask = prefetch->wrmask,
            .half = prefetch->half_precision,
            .bindless = prefetch->bindless,
            .cmd = tex_opc_to_prefetch_cmd(prefetch->tex_opc),
         ).value
      );
   }

   if (CHIP == A7XX) {
      for (int i = 0; i < fs->num_sampler_prefetch; i++) {
         const struct ir3_sampler_prefetch *prefetch = &fs->sampler_prefetch[i];
         OUT_REG(ring,
            A6XX_SP_PS_INITIAL_TEX_INDEX_CMD(i,
               .samp_id = prefetch->samp_id,
               .tex_id = prefetch->tex_id,
            )
         );
      }
   }

   OUT_REG(ring,
           SP_LB_PARAM_LIMIT(CHIP,
            b->ctx->screen->info->a6xx.prim_alloc_threshold),
           SP_REG_PROG_ID_0(
                 CHIP,
                 .faceregid = face_regid,
                 .sampleid = samp_id_regid,
                 .samplemask = smask_in_regid,
                 .centerrhw = ij_regid[IJ_PERSP_CENTER_RHW],
           ),
           SP_REG_PROG_ID_1(
                 CHIP,
                 .ij_persp_pixel = ij_regid[IJ_PERSP_PIXEL],
                 .ij_linear_pixel = ij_regid[IJ_LINEAR_PIXEL],
                 .ij_persp_centroid = ij_regid[IJ_PERSP_CENTROID],
                 .ij_linear_centroid = ij_regid[IJ_LINEAR_CENTROID],
           ),
           SP_REG_PROG_ID_2(
                 CHIP,
                 .ij_persp_sample = ij_regid[IJ_PERSP_SAMPLE],
                 .ij_linear_sample = ij_regid[IJ_LINEAR_SAMPLE],
                 .xycoordregid = coord_regid,
                 .zwcoordregid = zwcoord_regid,
           ),
           SP_REG_PROG_ID_3(
                 CHIP,
                 .linelengthregid = INVALID_REG,
                 .foveationqualityregid = INVALID_REG,
           ),
   );

   if (CHIP >= A7XX) {
      uint32_t sysval_regs = 0;
      for (unsigned i = 0; i < ARRAY_SIZE(ij_regid); i++) {
         if (VALIDREG(ij_regid[i])) {
            if (i == IJ_PERSP_CENTER_RHW)
               sysval_regs += 1;
            else
               sysval_regs += 2;
         }
      }

      for (uint32_t sysval : { face_regid, samp_id_regid, smask_in_regid }) {
         if (VALIDREG(sysval))
            sysval_regs += 1;
      }

      for (uint32_t sysval : { coord_regid, zwcoord_regid }) {
         if (VALIDREG(sysval))
            sysval_regs += 2;
      }

      OUT_REG(ring,
         A7XX_SP_PS_CNTL_1(
            .sysval_regs_count = sysval_regs,
            .unk8 = 1,
            .unk9 = 1,
         )
      );
   }

   enum a6xx_threadsize thrsz = fs->info.double_threadsize ? THREAD128 : THREAD64;
   OUT_REG(ring,
           SP_PS_WAVE_CNTL(
                 CHIP,
                 .threadsize = thrsz,
                 .varyings = enable_varyings,
           ),
   );

   bool need_size = fs->frag_face || fs->fragcoord_compmask != 0;
   bool need_size_persamp = false;
   if (VALIDREG(ij_regid[IJ_PERSP_CENTER_RHW])) {
      if (sample_shading)
         need_size_persamp = true;
      else
         need_size = true;
   }

   OUT_PKT4(ring, REG_A6XX_GRAS_CL_INTERP_CNTL, 1);
   OUT_RING(ring,
         CONDREG(ij_regid[IJ_PERSP_PIXEL], A6XX_GRAS_CL_INTERP_CNTL_IJ_PERSP_PIXEL) |
         CONDREG(ij_regid[IJ_PERSP_CENTROID], A6XX_GRAS_CL_INTERP_CNTL_IJ_PERSP_CENTROID) |
         CONDREG(ij_regid[IJ_PERSP_SAMPLE], A6XX_GRAS_CL_INTERP_CNTL_IJ_PERSP_SAMPLE) |
         CONDREG(ij_regid[IJ_LINEAR_PIXEL], A6XX_GRAS_CL_INTERP_CNTL_IJ_LINEAR_PIXEL) |
         CONDREG(ij_regid[IJ_LINEAR_CENTROID], A6XX_GRAS_CL_INTERP_CNTL_IJ_LINEAR_CENTROID) |
         CONDREG(ij_regid[IJ_LINEAR_SAMPLE], A6XX_GRAS_CL_INTERP_CNTL_IJ_LINEAR_SAMPLE) |
         COND(need_size, A6XX_GRAS_CL_INTERP_CNTL_IJ_LINEAR_PIXEL) |
         COND(need_size_persamp, A6XX_GRAS_CL_INTERP_CNTL_IJ_LINEAR_SAMPLE) |
         COND(fs->fragcoord_compmask != 0,
              A6XX_GRAS_CL_INTERP_CNTL_COORD_MASK(fs->fragcoord_compmask)));

   OUT_PKT4(ring, REG_A6XX_RB_INTERP_CNTL, 2);
   OUT_RING(ring,
         CONDREG(ij_regid[IJ_PERSP_PIXEL], A6XX_RB_INTERP_CNTL_IJ_PERSP_PIXEL) |
         CONDREG(ij_regid[IJ_PERSP_CENTROID], A6XX_RB_INTERP_CNTL_IJ_PERSP_CENTROID) |
         CONDREG(ij_regid[IJ_PERSP_SAMPLE], A6XX_RB_INTERP_CNTL_IJ_PERSP_SAMPLE) |
         CONDREG(ij_regid[IJ_LINEAR_PIXEL], A6XX_RB_INTERP_CNTL_IJ_LINEAR_PIXEL) |
         CONDREG(ij_regid[IJ_LINEAR_CENTROID], A6XX_RB_INTERP_CNTL_IJ_LINEAR_CENTROID) |
         CONDREG(ij_regid[IJ_LINEAR_SAMPLE], A6XX_RB_INTERP_CNTL_IJ_LINEAR_SAMPLE) |
         COND(need_size, A6XX_RB_INTERP_CNTL_IJ_LINEAR_PIXEL) |
         COND(enable_varyings, A6XX_RB_INTERP_CNTL_UNK10) |
         COND(need_size_persamp, A6XX_RB_INTERP_CNTL_IJ_LINEAR_SAMPLE) |
         COND(fs->fragcoord_compmask != 0,
              A6XX_RB_INTERP_CNTL_COORD_MASK(fs->fragcoord_compmask)));
   OUT_RING(ring,
         A6XX_RB_PS_INPUT_CNTL_FRAGCOORDSAMPLEMODE(
            sample_shading ? FRAGCOORD_SAMPLE : FRAGCOORD_CENTER) |
         CONDREG(smask_in_regid, A6XX_RB_PS_INPUT_CNTL_SAMPLEMASK) |
         CONDREG(samp_id_regid, A6XX_RB_PS_INPUT_CNTL_SAMPLEID) |
         CONDREG(ij_regid[IJ_PERSP_CENTER_RHW], A6XX_RB_PS_INPUT_CNTL_CENTERRHW) |
         COND(fs->post_depth_coverage, A6XX_RB_PS_INPUT_CNTL_POSTDEPTHCOVERAGE) |
         COND(fs->frag_face, A6XX_RB_PS_INPUT_CNTL_FACENESS));

   OUT_PKT4(ring, REG_A6XX_RB_PS_SAMPLEFREQ_CNTL, 1);
   OUT_RING(ring, COND(sample_shading, A6XX_RB_PS_SAMPLEFREQ_CNTL_PER_SAMP_MODE));

   OUT_PKT4(ring, REG_A6XX_GRAS_LRZ_PS_INPUT_CNTL, 1);
   OUT_RING(ring,
         CONDREG(samp_id_regid, A6XX_GRAS_LRZ_PS_INPUT_CNTL_SAMPLEID) |
         A6XX_GRAS_LRZ_PS_INPUT_CNTL_FRAGCOORDSAMPLEMODE(
            sample_shading ? FRAGCOORD_SAMPLE : FRAGCOORD_CENTER));

   OUT_PKT4(ring, REG_A6XX_GRAS_LRZ_PS_SAMPLEFREQ_CNTL, 1);
   OUT_RING(ring, COND(sample_shading, A6XX_GRAS_LRZ_PS_SAMPLEFREQ_CNTL_PER_SAMP_MODE));
}

template<chip CHIP>
static void
emit_fs_outputs(struct fd_ringbuffer *ring, const struct program_builder *b)
{
   const struct ir3_shader_variant *fs = b->fs;
   uint32_t smask_regid, posz_regid, stencilref_regid;

   posz_regid      = ir3_find_output_regid(fs, FRAG_RESULT_DEPTH);
   smask_regid     = ir3_find_output_regid(fs, FRAG_RESULT_SAMPLE_MASK);
   stencilref_regid = ir3_find_output_regid(fs, FRAG_RESULT_STENCIL);

   /* we can't write gl_SampleMask for !msaa..  if b0 is zero then we
    * end up masking the single sample!!
    */
   if (!b->key->key.msaa)
      smask_regid = INVALID_REG;

   int output_reg_count = 0;
   uint32_t fragdata_regid[8];
   uint32_t fragdata_aliased_components = 0;

   for (uint32_t i = 0; i < ARRAY_SIZE(fragdata_regid); i++) {
      unsigned slot = fs->color0_mrt ? FRAG_RESULT_COLOR : FRAG_RESULT_DATA0 + i;
      int output_idx = ir3_find_output(fs, (gl_varying_slot)slot);

      if (output_idx < 0) {
         fragdata_regid[i] = INVALID_REG;
         continue;
      }

      const struct ir3_shader_output *fragdata = &fs->outputs[output_idx];
      fragdata_regid[i] = ir3_get_output_regid(fragdata);

      if (VALIDREG(fragdata_regid[i]) || fragdata->aliased_components) {
         /* An invalid reg is only allowed if all components are aliased. */
         assert(
            VALIDREG(fragdata_regid[i] || fragdata->aliased_components == 0xf));

         output_reg_count = i + 1;
         fragdata_aliased_components |= fragdata->aliased_components << (i * 4);
      }
   }

   OUT_PKT4(ring, REG_A6XX_SP_PS_OUTPUT_CNTL, 1);
   OUT_RING(ring, A6XX_SP_PS_OUTPUT_CNTL_DEPTH_REGID(posz_regid) |
                  A6XX_SP_PS_OUTPUT_CNTL_SAMPMASK_REGID(smask_regid) |
                  A6XX_SP_PS_OUTPUT_CNTL_STENCILREF_REGID(stencilref_regid) |
                  COND(fs->dual_src_blend, A6XX_SP_PS_OUTPUT_CNTL_DUAL_COLOR_IN_ENABLE));

   OUT_PKT4(ring, REG_A6XX_SP_PS_OUTPUT_REG(0), output_reg_count);
   for (uint32_t i = 0; i < output_reg_count; i++) {
      OUT_RING(ring, A6XX_SP_PS_OUTPUT_REG_REGID(fragdata_regid[i]) |
                     COND(fragdata_regid[i] & HALF_REG_ID,
                             A6XX_SP_PS_OUTPUT_REG_HALF_PRECISION));

      if (VALIDREG(fragdata_regid[i]) ||
          (fragdata_aliased_components & (0xf << (i * 4)))) {
         b->state->mrt_components |= 0xf << (i * 4);
      }
   }

   if (CHIP >= A7XX) {
      OUT_REG(
         ring,
         A7XX_SP_PS_OUTPUT_CONST_CNTL(
               .enabled = fragdata_aliased_components != 0),
         A7XX_SP_PS_OUTPUT_CONST_MASK(.dword = fragdata_aliased_components));
   } else {
      assert(fragdata_aliased_components == 0);
   }
}

template <chip CHIP>
static void
setup_stateobj(struct fd_ringbuffer *ring, const struct program_builder *b)
   assert_dt
{
   fd6_emit_shader<CHIP>(b->ctx, ring, b->vs);
   fd6_emit_shader<CHIP>(b->ctx, ring, b->hs);
   fd6_emit_shader<CHIP>(b->ctx, ring, b->ds);
   fd6_emit_shader<CHIP>(b->ctx, ring, b->gs);
   if (!b->binning_pass)
      fd6_emit_shader<CHIP>(b->ctx, ring, b->fs);

   OUT_PKT4(ring, REG_A6XX_PC_STEREO_RENDERING_CNTL, 1);
   OUT_RING(ring, 0);

   emit_vfd_dest(ring, b->vs);

   emit_vpc<CHIP>(ring, b);

   emit_fs_inputs<CHIP>(ring, b);
   emit_fs_outputs<CHIP>(ring, b);

   if (b->hs) {
      uint32_t patch_control_points = b->key->patch_vertices;

      uint32_t patch_local_mem_size_16b =
         patch_control_points * b->vs->output_size / 4;

      /* Total attribute slots in HS incoming patch. */
      OUT_PKT4(ring, REG_A6XX_PC_HS_PARAM_1, 1);
      OUT_RING(ring, patch_local_mem_size_16b);

      const uint32_t wavesize = 64;
      const uint32_t vs_hs_local_mem_size = 16384;

      uint32_t max_patches_per_wave;
      if (b->ctx->screen->info->a6xx.tess_use_shared) {
         /* HS invocations for a patch are always within the same wave,
         * making barriers less expensive. VS can't have barriers so we
         * don't care about VS invocations being in the same wave.
         */
         max_patches_per_wave = wavesize / b->hs->tess.tcs_vertices_out;
      } else {
      /* VS is also in the same wave */
         max_patches_per_wave =
            wavesize / MAX2(patch_control_points,
                            b->hs->tess.tcs_vertices_out);
      }


      uint32_t patches_per_wave =
         MIN2(vs_hs_local_mem_size / (patch_local_mem_size_16b * 16),
              max_patches_per_wave);

      uint32_t wave_input_size = DIV_ROUND_UP(
         patches_per_wave * patch_local_mem_size_16b * 16, 256);

      OUT_PKT4(ring, REG_A6XX_SP_HS_CNTL_1, 1);
      OUT_RING(ring, wave_input_size);

      enum a6xx_tess_output output;
      if (b->ds->tess.point_mode)
         output = TESS_POINTS;
      else if (b->ds->tess.primitive_mode == TESS_PRIMITIVE_ISOLINES)
         output = TESS_LINES;
      else if (b->ds->tess.ccw)
         output = TESS_CCW_TRIS;
      else
         output = TESS_CW_TRIS;

      OUT_PKT4(ring, REG_A6XX_PC_DS_PARAM, 1);
      OUT_RING(ring, A6XX_PC_DS_PARAM_SPACING(
                        fd6_gl2spacing(b->ds->tess.spacing)) |
                        A6XX_PC_DS_PARAM_OUTPUT(output));
   }
}

static void emit_interp_state(struct fd_ringbuffer *ring,
                              const struct fd6_program_state *state,
                              bool rasterflat,
                              bool sprite_coord_mode,
                              uint32_t sprite_coord_enable);

static struct fd_ringbuffer *
create_interp_stateobj(struct fd_context *ctx, struct fd6_program_state *state)
{
   struct fd_ringbuffer *ring = fd_ringbuffer_new_object(ctx->pipe, 18 * 4);

   emit_interp_state(ring, state, false, false, 0);

   return ring;
}

/* build the program streaming state which is not part of the pre-
 * baked stateobj because of dependency on other gl state (rasterflat
 * or sprite-coord-replacement)
 */
struct fd_ringbuffer *
fd6_program_interp_state(struct fd6_emit *emit)
{
   const struct fd6_program_state *state = fd6_emit_get_prog(emit);

   if (!unlikely(emit->rasterflat || emit->sprite_coord_enable)) {
      /* fastpath: */
      return fd_ringbuffer_ref(state->interp_stateobj);
   } else {
      struct fd_ringbuffer *ring = fd_submit_new_ringbuffer(
         emit->ctx->batch->submit, 18 * 4, FD_RINGBUFFER_STREAMING);

      emit_interp_state(ring, state, emit->rasterflat,
                        emit->sprite_coord_mode, emit->sprite_coord_enable);

      return ring;
   }
}

static void
emit_interp_state(struct fd_ringbuffer *ring, const struct fd6_program_state *state,
                  bool rasterflat, bool sprite_coord_mode,
                  uint32_t sprite_coord_enable)
{
   const struct ir3_shader_variant *fs = state->fs;
   uint32_t vinterp[8], vpsrepl[8];

   memset(vinterp, 0, sizeof(vinterp));
   memset(vpsrepl, 0, sizeof(vpsrepl));

   for (int j = -1; (j = ir3_next_varying(fs, j)) < (int)fs->inputs_count;) {

      /* NOTE: varyings are packed, so if compmask is 0xb
       * then first, third, and fourth component occupy
       * three consecutive varying slots:
       */
      unsigned compmask = fs->inputs[j].compmask;

      uint32_t inloc = fs->inputs[j].inloc;

      bool coord_mode = sprite_coord_mode;
      if (ir3_point_sprite(fs, j, sprite_coord_enable, &coord_mode)) {
         /* mask is two 2-bit fields, where:
          *   '01' -> S
          *   '10' -> T
          *   '11' -> 1 - T  (flip mode)
          */
         unsigned mask = coord_mode ? 0b1101 : 0b1001;
         uint32_t loc = inloc;
         if (compmask & 0x1) {
            vpsrepl[loc / 16] |= ((mask >> 0) & 0x3) << ((loc % 16) * 2);
            loc++;
         }
         if (compmask & 0x2) {
            vpsrepl[loc / 16] |= ((mask >> 2) & 0x3) << ((loc % 16) * 2);
            loc++;
         }
         if (compmask & 0x4) {
            /* .z <- 0.0f */
            vinterp[loc / 16] |= INTERP_ZERO << ((loc % 16) * 2);
            loc++;
         }
         if (compmask & 0x8) {
            /* .w <- 1.0f */
            vinterp[loc / 16] |= INTERP_ONE << ((loc % 16) * 2);
            loc++;
         }
      } else if (fs->inputs[j].slot == VARYING_SLOT_LAYER ||
                 fs->inputs[j].slot == VARYING_SLOT_VIEWPORT) {
         const struct ir3_shader_variant *last_shader = fd6_last_shader(state);
         uint32_t loc = inloc;

         /* If the last geometry shader doesn't statically write these, they're
          * implicitly zero and the FS is supposed to read zero.
          */
         if (ir3_find_output(last_shader, (gl_varying_slot)fs->inputs[j].slot) < 0 &&
             (compmask & 0x1)) {
            vinterp[loc / 16] |= INTERP_ZERO << ((loc % 16) * 2);
         } else {
            vinterp[loc / 16] |= INTERP_FLAT << ((loc % 16) * 2);
         }
      } else if (fs->inputs[j].flat || (fs->inputs[j].rasterflat && rasterflat)) {
         uint32_t loc = inloc;

         for (int i = 0; i < 4; i++) {
            if (compmask & (1 << i)) {
               vinterp[loc / 16] |= INTERP_FLAT << ((loc % 16) * 2);
               loc++;
            }
         }
      }
   }

   OUT_PKT4(ring, REG_A6XX_VPC_VARYING_INTERP_MODE_MODE(0), 8);
   for (int i = 0; i < 8; i++)
      OUT_RING(ring, vinterp[i]); /* VPC_VARYING_INTERP_MODE[i].MODE */

   OUT_PKT4(ring, REG_A6XX_VPC_VARYING_REPLACE_MODE_0_MODE(0), 8);
   for (int i = 0; i < 8; i++)
      OUT_RING(ring, vpsrepl[i]); /* VPC_VARYING_REPLACE_MODE_0[i] */
}

template <chip CHIP>
static struct ir3_program_state *
fd6_program_create(void *data, const struct ir3_shader_variant *bs,
                   const struct ir3_shader_variant *vs,
                   const struct ir3_shader_variant *hs,
                   const struct ir3_shader_variant *ds,
                   const struct ir3_shader_variant *gs,
                   const struct ir3_shader_variant *fs,
                   const struct ir3_cache_key *key) in_dt
{
   struct fd_context *ctx = fd_context((struct pipe_context *)data);
   struct fd_screen *screen = ctx->screen;
   struct fd6_program_state *state = CALLOC_STRUCT(fd6_program_state);

   tc_assert_driver_thread(ctx->tc);

   /* if we have streamout, use full VS in binning pass, as the
    * binning pass VS will have outputs on other than position/psize
    * stripped out:
    */
   state->bs = vs->stream_output.num_outputs ? vs : bs;
   state->vs = vs;
   state->hs = hs;
   state->ds = ds;
   state->gs = gs;
   state->fs = fs;
   state->binning_stateobj = fd_ringbuffer_new_object(ctx->pipe, 0x1000);
   state->stateobj = fd_ringbuffer_new_object(ctx->pipe, 0x1000);

   if (hs) {
      /* Allocate the fixed-size tess factor BO globally on the screen.  This
       * lets the program (which ideally we would have shared across contexts,
       * though the current ir3_cache impl doesn't do that) bake in the
       * addresses.
       */
      fd_screen_lock(screen);
      if (!screen->tess_bo)
         screen->tess_bo =
            fd_bo_new(screen->dev, FD6_TESS_BO_SIZE, FD_BO_NOMAP, "tessfactor");
      fd_screen_unlock(screen);
   }

   /* Dummy frag shader used for binning pass: */
   static const struct ir3_shader_variant dummy_fs = {
         .info = {
               .max_reg = -1,
               .max_half_reg = -1,
               .max_const = -1,
         },
   };
   /* The last geometry stage in use: */
   const struct ir3_shader_variant *last_shader = fd6_last_shader(state);

   setup_config_stateobj<CHIP>(ctx, state);

   struct program_builder b = {
      .state = state,
      .ctx = ctx,
      .key = key,
      .hs  = state->hs,
      .ds  = state->ds,
      .gs  = state->gs,
   };

   /*
    * Setup binning pass program state:
    */

   /* binning VS is wrong when GS is present, so use nonbinning VS
    * TODO: compile both binning VS/GS variants correctly
    *
    * If we have stream-out, we use the full shader for binning
    * pass, rather than the optimized binning pass one, so that we
    * have all the varying outputs available for xfb.  So streamout
    * state should always be derived from the non-binning pass
    * program.
    */
   b.vs  = state->gs || last_shader->stream_output.num_outputs ?
           state->vs : state->bs;
   b.fs  = &dummy_fs;
   b.last_shader  = last_shader->type != MESA_SHADER_VERTEX ?
                    last_shader : state->bs;
   b.binning_pass = true;

   setup_stateobj<CHIP>(state->binning_stateobj, &b);

   /*
    * Setup draw pass program state:
    */
   b.vs = state->vs;
   b.fs = state->fs;
   b.last_shader = last_shader;
   b.binning_pass = false;

   setup_stateobj<CHIP>(state->stateobj, &b);

   state->interp_stateobj = create_interp_stateobj(ctx, state);

   const struct ir3_stream_output_info *stream_output = &last_shader->stream_output;
   if (stream_output->num_outputs > 0)
      state->stream_output = stream_output;

   bool has_viewport =
      VALIDREG(ir3_find_output_regid(last_shader, VARYING_SLOT_VIEWPORT));
   state->num_viewports = has_viewport ? PIPE_MAX_VIEWPORTS : 1;

   /* Note that binning pass uses same const state as draw pass: */
   state->user_consts_cmdstream_size =
         fd6_user_consts_cmdstream_size<CHIP>(state->vs) +
         fd6_user_consts_cmdstream_size<CHIP>(state->hs) +
         fd6_user_consts_cmdstream_size<CHIP>(state->ds) +
         fd6_user_consts_cmdstream_size<CHIP>(state->gs) +
         fd6_user_consts_cmdstream_size<CHIP>(state->fs);

   unsigned num_dp = 0;
   unsigned num_ubo_dp = 0;

   if (vs->need_driver_params)
      num_dp++;

   if (gs && gs->need_driver_params)
      num_ubo_dp++;
   if (hs && hs->need_driver_params)
      num_ubo_dp++;
   if (ds && ds->need_driver_params)
      num_ubo_dp++;

   if (!(CHIP == A7XX && vs->compiler->load_inline_uniforms_via_preamble_ldgk)) {
      /* On a6xx all shader stages use driver params pushed in cmdstream: */
      num_dp += num_ubo_dp;
      num_ubo_dp = 0;
   }

   state->num_driver_params = num_dp;
   state->num_ubo_driver_params = num_ubo_dp;

   /* dual source blending has an extra fs output in the 2nd slot */
   if (fs->fs.color_is_dual_source) {
      state->mrt_components |= 0xf << 4;
   }

   state->lrz_mask.val = ~0;

   if (fs->has_kill) {
      state->lrz_mask.write = false;
   }

   if (fs->no_earlyz || fs->writes_pos) {
      state->lrz_mask.enable = false;
      state->lrz_mask.write = false;
      state->lrz_mask.test = false;
   }

   if (fs->fs.early_fragment_tests) {
      state->lrz_mask.z_mode = A6XX_EARLY_Z;
   } else if (fs->no_earlyz || fs->writes_pos || fs->writes_stencilref) {
      state->lrz_mask.z_mode = A6XX_LATE_Z;
   } else {
      /* Wildcard indicates that we need to figure out at draw time: */
      state->lrz_mask.z_mode = A6XX_INVALID_ZTEST;
   }

   return &state->base;
}

static void
fd6_program_destroy(void *data, struct ir3_program_state *state)
{
   struct fd6_program_state *so = fd6_program_state(state);
   fd_ringbuffer_del(so->stateobj);
   fd_ringbuffer_del(so->binning_stateobj);
   fd_ringbuffer_del(so->config_stateobj);
   fd_ringbuffer_del(so->interp_stateobj);
   if (so->streamout_stateobj)
      fd_ringbuffer_del(so->streamout_stateobj);
   free(so);
}

template <chip CHIP>
static const struct ir3_cache_funcs cache_funcs = {
   .create_state = fd6_program_create<CHIP>,
   .destroy_state = fd6_program_destroy,
};

template <chip CHIP>
void
fd6_prog_init(struct pipe_context *pctx)
{
   struct fd_context *ctx = fd_context(pctx);

   ctx->shader_cache = ir3_cache_create(&cache_funcs<CHIP>, ctx);

   ir3_prog_init(pctx);

   fd_prog_init(pctx);
}
FD_GENX(fd6_prog_init);

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

#include <assert.h>
#include <stdbool.h>

#include "anv_private.h"
#include "anv_measure.h"
#include "vk_render_pass.h"
#include "vk_util.h"

#include "util/format_srgb.h"

#include "genxml/gen_macros.h"
#include "genxml/genX_pack.h"

#include "ds/intel_tracepoints.h"

#include "genX_mi_builder.h"
#include "genX_cmd_draw_generated_flush.h"

static void genX(flush_pipeline_select)(struct anv_cmd_buffer *cmd_buffer,
                                        uint32_t pipeline);

static enum anv_pipe_bits
convert_pc_to_bits(struct GENX(PIPE_CONTROL) *pc) {
   enum anv_pipe_bits bits = 0;
   bits |= (pc->DepthCacheFlushEnable) ?  ANV_PIPE_DEPTH_CACHE_FLUSH_BIT : 0;
   bits |= (pc->DCFlushEnable) ?  ANV_PIPE_DATA_CACHE_FLUSH_BIT : 0;
#if GFX_VERx10 >= 125
   bits |= (pc->PSSStallSyncEnable) ?  ANV_PIPE_PSS_STALL_SYNC_BIT : 0;
#endif
#if GFX_VER == 12
   bits |= (pc->TileCacheFlushEnable) ?  ANV_PIPE_TILE_CACHE_FLUSH_BIT : 0;
   bits |= (pc->L3FabricFlush) ?  ANV_PIPE_L3_FABRIC_FLUSH_BIT : 0;
#endif
#if GFX_VER >= 12
   bits |= (pc->HDCPipelineFlushEnable) ?  ANV_PIPE_HDC_PIPELINE_FLUSH_BIT : 0;
#endif
   bits |= (pc->RenderTargetCacheFlushEnable) ?  ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT : 0;
   bits |= (pc->VFCacheInvalidationEnable) ?  ANV_PIPE_VF_CACHE_INVALIDATE_BIT : 0;
   bits |= (pc->StateCacheInvalidationEnable) ?  ANV_PIPE_STATE_CACHE_INVALIDATE_BIT : 0;
   bits |= (pc->ConstantCacheInvalidationEnable) ?  ANV_PIPE_CONSTANT_CACHE_INVALIDATE_BIT : 0;
   bits |= (pc->TextureCacheInvalidationEnable) ?  ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT : 0;
   bits |= (pc->InstructionCacheInvalidateEnable) ?  ANV_PIPE_INSTRUCTION_CACHE_INVALIDATE_BIT : 0;
   bits |= (pc->StallAtPixelScoreboard) ?  ANV_PIPE_STALL_AT_SCOREBOARD_BIT : 0;
   bits |= (pc->DepthStallEnable) ?  ANV_PIPE_DEPTH_STALL_BIT : 0;
   bits |= (pc->CommandStreamerStallEnable) ?  ANV_PIPE_CS_STALL_BIT : 0;
#if GFX_VERx10 >= 125
   bits |= (pc->UntypedDataPortCacheFlushEnable) ? ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT : 0;
   bits |= (pc->CCSFlushEnable) ? ANV_PIPE_CCS_CACHE_FLUSH_BIT : 0;
#endif
   return bits;
}

#define anv_debug_dump_pc(pc, reason) \
   if (INTEL_DEBUG(DEBUG_PIPE_CONTROL)) { \
      fputs("pc : emit PC=( ", stdout); \
      anv_dump_pipe_bits(convert_pc_to_bits(&(pc)), stdout);   \
      fprintf(stdout, ") reason: %s\n", reason); \
   }

static inline void
fill_state_base_addr(struct anv_cmd_buffer *cmd_buffer,
                     struct GENX(STATE_BASE_ADDRESS) *sba)
{
   struct anv_device *device = cmd_buffer->device;
   const uint32_t mocs = isl_mocs(&device->isl_dev, 0, false);

   /* If no API entry point selected the current mode (this can happen if the
    * first operation in the command buffer is a , select BUFFER if
    * EXT_descriptor_buffer is enabled, otherwise LEGACY.
    */
   if (cmd_buffer->state.pending_db_mode ==
       ANV_CMD_DESCRIPTOR_BUFFER_MODE_UNKNOWN) {
      cmd_buffer->state.pending_db_mode =
         cmd_buffer->device->vk.enabled_extensions.EXT_descriptor_buffer ?
         ANV_CMD_DESCRIPTOR_BUFFER_MODE_BUFFER :
         ANV_CMD_DESCRIPTOR_BUFFER_MODE_LEGACY;
   }

   *sba = (struct GENX(STATE_BASE_ADDRESS)) { GENX(STATE_BASE_ADDRESS_header), };

   sba->GeneralStateBaseAddress = (struct anv_address) { NULL, 0 };
   sba->GeneralStateMOCS = mocs;
   sba->GeneralStateBufferSize = DIV_ROUND_UP(
      device->physical->va.first_2mb.size +
      device->physical->va.general_state_pool.size +
      device->physical->va.low_heap.size, 4096);
   sba->GeneralStateBaseAddressModifyEnable = true;
   sba->GeneralStateBufferSizeModifyEnable = true;

#if GFX_VERx10 == 120
   /* Since DG2, scratch surfaces have their own surface state with its own
    * MOCS setting, but prior to that, the MOCS for scratch accesses are
    * governed by SBA.StatelessDataPortAccessMOCS.
    */
   const isl_surf_usage_flags_t protected_usage =
      cmd_buffer->vk.pool->flags & VK_COMMAND_POOL_CREATE_PROTECTED_BIT ?
      ISL_SURF_USAGE_PROTECTED_BIT : 0;
   const uint32_t stateless_mocs = isl_mocs(&device->isl_dev, protected_usage, false);
#else
   const uint32_t stateless_mocs = mocs;
#endif

   sba->StatelessDataPortAccessMOCS = stateless_mocs;

#if GFX_VERx10 >= 125
   sba->SurfaceStateBaseAddress =
      (struct anv_address) { .offset =
                             device->physical->va.internal_surface_state_pool.addr,
   };
#else
   sba->SurfaceStateBaseAddress =
      anv_cmd_buffer_surface_base_address(cmd_buffer);
#endif
   sba->SurfaceStateMOCS = mocs;
   sba->SurfaceStateBaseAddressModifyEnable = true;

   sba->IndirectObjectBaseAddress = (struct anv_address) { NULL, 0 };
   sba->IndirectObjectMOCS = mocs;
   sba->IndirectObjectBufferSize = 0xfffff;
   sba->IndirectObjectBaseAddressModifyEnable = true;
   sba->IndirectObjectBufferSizeModifyEnable  = true;

   sba->InstructionBaseAddress =
      (struct anv_address) { device->instruction_state_pool.block_pool.bo, 0 };
   sba->InstructionMOCS = mocs;
   sba->InstructionBufferSize =
      device->physical->va.instruction_state_pool.size / 4096;
   sba->InstructionBaseAddressModifyEnable = true;
   sba->InstructionBuffersizeModifyEnable = true;

#if GFX_VER >= 11
   sba->BindlessSamplerStateBaseAddress = ANV_NULL_ADDRESS;
   sba->BindlessSamplerStateBufferSize = 0;
   sba->BindlessSamplerStateMOCS = mocs;
   sba->BindlessSamplerStateBaseAddressModifyEnable = true;
#endif

   sba->DynamicStateBaseAddress = (struct anv_address) {
      .offset = device->physical->va.dynamic_state_pool.addr,
   };
   sba->DynamicStateBufferSize =
      (device->physical->va.dynamic_state_pool.size +
       device->physical->va.dynamic_visible_pool.size +
       device->physical->va.push_descriptor_buffer_pool.size) / 4096;
   sba->DynamicStateMOCS = mocs;
   sba->DynamicStateBaseAddressModifyEnable = true;
   sba->DynamicStateBufferSizeModifyEnable = true;

   if (cmd_buffer->state.pending_db_mode == ANV_CMD_DESCRIPTOR_BUFFER_MODE_BUFFER) {
#if GFX_VERx10 >= 125
      sba->BindlessSurfaceStateBaseAddress = (struct anv_address) {
         .offset = device->physical->va.dynamic_visible_pool.addr,
      };
      sba->BindlessSurfaceStateSize =
         (device->physical->va.dynamic_visible_pool.size +
          device->physical->va.push_descriptor_buffer_pool.size) - 1;
      sba->BindlessSurfaceStateMOCS = mocs;
      sba->BindlessSurfaceStateBaseAddressModifyEnable = true;
#else
      /* We report descriptorBufferOffsetAlignment = 64, but
       * STATE_BASE_ADDRESS::BindlessSurfaceStateBaseAddress needs to be
       * aligned to 4096. Round down to 4096 here and add the remaining to the
       * push constants descriptor set offsets in
       * compute_descriptor_set_surface_offset().
       */
      const uint64_t surfaces_addr =
         ROUND_DOWN_TO(
            cmd_buffer->state.descriptor_buffers.surfaces_address != 0 ?
            cmd_buffer->state.descriptor_buffers.surfaces_address :
            anv_address_physical(device->workaround_address),
            4096);
      const uint64_t surfaces_size =
         cmd_buffer->state.descriptor_buffers.surfaces_address != 0 ?
         MIN2(device->physical->va.dynamic_visible_pool.size -
              (surfaces_addr - device->physical->va.dynamic_visible_pool.addr),
              anv_physical_device_bindless_heap_size(device->physical, true)) :
         (device->workaround_bo->size - device->workaround_address.offset);
      sba->BindlessSurfaceStateBaseAddress = (struct anv_address) {
         .offset = surfaces_addr,
      };
      sba->BindlessSurfaceStateSize = surfaces_size / ANV_SURFACE_STATE_SIZE - 1;
      sba->BindlessSurfaceStateMOCS = mocs;
      sba->BindlessSurfaceStateBaseAddressModifyEnable = true;
#endif /* GFX_VERx10 < 125 */
   } else if (!device->physical->indirect_descriptors) {
#if GFX_VERx10 >= 125
      sba->BindlessSurfaceStateBaseAddress = (struct anv_address) {
         .offset = device->physical->va.internal_surface_state_pool.addr,
      };
      sba->BindlessSurfaceStateSize =
         (device->physical->va.internal_surface_state_pool.size +
          device->physical->va.bindless_surface_state_pool.size) - 1;
      sba->BindlessSurfaceStateMOCS = mocs;
      sba->BindlessSurfaceStateBaseAddressModifyEnable = true;
#else
      UNREACHABLE("Direct descriptor not supported");
#endif
   } else {
      sba->BindlessSurfaceStateBaseAddress =
         (struct anv_address) { .offset =
                                device->physical->va.bindless_surface_state_pool.addr,
      };
      sba->BindlessSurfaceStateSize =
         anv_physical_device_bindless_heap_size(device->physical, false) /
         ANV_SURFACE_STATE_SIZE - 1;
      sba->BindlessSurfaceStateMOCS = mocs;
      sba->BindlessSurfaceStateBaseAddressModifyEnable = true;
   }

#if GFX_VERx10 >= 125
   sba->L1CacheControl = L1CC_WB;
#endif
}

void
genX(cmd_buffer_emit_state_base_address)(struct anv_cmd_buffer *cmd_buffer)
{
   if (anv_cmd_buffer_is_blitter_queue(cmd_buffer) ||
       anv_cmd_buffer_is_video_queue(cmd_buffer))
      return;

   struct anv_device *device = cmd_buffer->device;

   struct GENX(STATE_BASE_ADDRESS) sba = {};
   fill_state_base_addr(cmd_buffer, &sba);

#if GFX_VERx10 >= 125
   struct mi_builder b;
   mi_builder_init(&b, device->info, &cmd_buffer->batch);
   mi_builder_set_mocs(&b, isl_mocs(&device->isl_dev, 0, false));
   struct mi_goto_target t = MI_GOTO_TARGET_INIT;
   mi_goto_if(&b,
              mi_ieq(&b, mi_reg64(ANV_BINDLESS_SURFACE_BASE_ADDR_REG),
                         mi_imm(sba.BindlessSurfaceStateBaseAddress.offset)),
              &t);
#endif

   /* Emit a render target cache flush.
    *
    * This isn't documented anywhere in the PRM.  However, it seems to be
    * necessary prior to changing the surface state base address.  Without
    * this, we get GPU hangs when using multi-level command buffers which
    * clear depth, reset state base address, and then go render stuff.
    *
    * Render target cache flush before SBA is required by Wa_18039438632.
    */
   genx_batch_emit_pipe_control(&cmd_buffer->batch, device->info,
                                cmd_buffer->state.current_pipeline,
#if GFX_VER >= 12
                                ANV_PIPE_HDC_PIPELINE_FLUSH_BIT |
#else
                                ANV_PIPE_DATA_CACHE_FLUSH_BIT |
#endif
                                ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
                                ANV_PIPE_CS_STALL_BIT);

#if INTEL_NEEDS_WA_1607854226
   /* Wa_1607854226:
    *
    *  Workaround the non pipelined state not applying in MEDIA/GPGPU pipeline
    *  mode by putting the pipeline temporarily in 3D mode.
    */
   uint32_t gfx12_wa_pipeline = cmd_buffer->state.current_pipeline;
   genX(flush_pipeline_select_3d)(cmd_buffer);
#endif

   anv_batch_emit(&cmd_buffer->batch, GENX(STATE_BASE_ADDRESS), _sba) {
      _sba = sba;
   }

   if (cmd_buffer->state.current_db_mode != cmd_buffer->state.pending_db_mode)
      cmd_buffer->state.current_db_mode = cmd_buffer->state.pending_db_mode;

#if INTEL_NEEDS_WA_1607854226
   /* Wa_1607854226:
    *
    *  Put the pipeline back into its current mode.
    */
   if (gfx12_wa_pipeline != UINT32_MAX)
      genX(flush_pipeline_select)(cmd_buffer, gfx12_wa_pipeline);
#endif

   /* After re-setting the surface state base address, we have to do some
    * cache flushing so that the sampler engine will pick up the new
    * SURFACE_STATE objects and binding tables. From the Broadwell PRM,
    * Shared Function > 3D Sampler > State > State Caching (page 96):
    *
    *    Coherency with system memory in the state cache, like the texture
    *    cache is handled partially by software. It is expected that the
    *    command stream or shader will issue Cache Flush operation or
    *    Cache_Flush sampler message to ensure that the L1 cache remains
    *    coherent with system memory.
    *
    *    [...]
    *
    *    Whenever the value of the Dynamic_State_Base_Addr,
    *    Surface_State_Base_Addr are altered, the L1 state cache must be
    *    invalidated to ensure the new surface or sampler state is fetched
    *    from system memory.
    *
    * The PIPE_CONTROL command has a "State Cache Invalidation Enable" bit
    * which, according the PIPE_CONTROL instruction documentation in the
    * Broadwell PRM:
    *
    *    Setting this bit is independent of any other bit in this packet.
    *    This bit controls the invalidation of the L1 and L2 state caches
    *    at the top of the pipe i.e. at the parsing time.
    *
    * Unfortunately, experimentation seems to indicate that state cache
    * invalidation through a PIPE_CONTROL does nothing whatsoever in
    * regards to surface state and binding tables.  In stead, it seems that
    * invalidating the texture cache is what is actually needed.
    *
    * XXX:  As far as we have been able to determine through
    * experimentation, shows that flush the texture cache appears to be
    * sufficient.  The theory here is that all of the sampling/rendering
    * units cache the binding table in the texture cache.  However, we have
    * yet to be able to actually confirm this.
    *
    * Wa_14013910100:
    *
    *  "DG2 128/256/512-A/B: S/W must program STATE_BASE_ADDRESS command twice
    *   or program pipe control with Instruction cache invalidate post
    *   STATE_BASE_ADDRESS command"
    */
   enum anv_pipe_bits bits =
      ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT |
      ANV_PIPE_CONSTANT_CACHE_INVALIDATE_BIT |
      ANV_PIPE_STATE_CACHE_INVALIDATE_BIT |
      (intel_needs_workaround(device->info, 16013000631) ?
       ANV_PIPE_INSTRUCTION_CACHE_INVALIDATE_BIT : 0);

   genx_batch_emit_pipe_control(&cmd_buffer->batch, device->info,
                                cmd_buffer->state.current_pipeline,
                                bits);

   assert(cmd_buffer->state.current_db_mode !=
          ANV_CMD_DESCRIPTOR_BUFFER_MODE_UNKNOWN);

#if GFX_VERx10 >= 125
   assert(sba.BindlessSurfaceStateBaseAddress.offset != 0);
   mi_store(&b, mi_reg64(ANV_BINDLESS_SURFACE_BASE_ADDR_REG),
                mi_imm(sba.BindlessSurfaceStateBaseAddress.offset));

   mi_goto_target(&b, &t);
#endif

#if GFX_VERx10 >= 125
   genX(cmd_buffer_emit_bt_pool_base_address)(cmd_buffer);
#endif

   /* If we have emitted a new state base address we probably need to re-emit
    * binding tables.
    */
   cmd_buffer->state.descriptors_dirty |= ~0;
}

void
genX(cmd_buffer_emit_bt_pool_base_address)(struct anv_cmd_buffer *cmd_buffer)
{
   if (!anv_cmd_buffer_is_render_or_compute_queue(cmd_buffer))
      return;

   /* If we are emitting a new state base address we probably need to re-emit
    * binding tables.
    */
   cmd_buffer->state.descriptors_dirty |= ~0;

#if GFX_VERx10 >= 125
   struct anv_device *device = cmd_buffer->device;
   const uint32_t mocs = isl_mocs(&device->isl_dev, 0, false);

   /* We're changing base location of binding tables which affects the state
    * cache. We're adding texture cache invalidation following a
    * recommendation from the ICL PRMs, Volume 9: Render Engine, Coherency
    * Mechanisms:
    *
    *    "It is strongly recommended that a Texture cache invalidation be done
    *     whenever a State cache invalidation is done."
    *
    * Prior to do the invalidation, we need a CS_STALL to ensure that all work
    * using surface states has completed.
    */
   genx_batch_emit_pipe_control(&cmd_buffer->batch,
                                cmd_buffer->device->info,
                                cmd_buffer->state.current_pipeline,
                                ANV_PIPE_CS_STALL_BIT);
   anv_batch_emit(
      &cmd_buffer->batch, GENX(3DSTATE_BINDING_TABLE_POOL_ALLOC), btpa) {
      btpa.BindingTablePoolBaseAddress =
         anv_cmd_buffer_surface_base_address(cmd_buffer);
      btpa.BindingTablePoolBufferSize = device->physical->va.binding_table_pool.size / 4096;
      btpa.MOCS = mocs;
   }
   genx_batch_emit_pipe_control(&cmd_buffer->batch,
                                cmd_buffer->device->info,
                                cmd_buffer->state.current_pipeline,
                                ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT |
                                ANV_PIPE_STATE_CACHE_INVALIDATE_BIT);

#else /* GFX_VERx10 < 125 */
   genX(cmd_buffer_emit_state_base_address)(cmd_buffer);
#endif
}

static void
add_surface_reloc(struct anv_cmd_buffer *cmd_buffer,
                  struct anv_address addr)
{
   VkResult result = anv_reloc_list_add_bo(&cmd_buffer->surface_relocs,
                                           addr.bo);

   if (unlikely(result != VK_SUCCESS))
      anv_batch_set_error(&cmd_buffer->batch, result);
}

static void
add_surface_state_relocs(struct anv_cmd_buffer *cmd_buffer,
                         const struct anv_surface_state *state)
{
   assert(!anv_address_is_null(state->address));
   add_surface_reloc(cmd_buffer, state->address);

   if (!anv_address_is_null(state->aux_address)) {
      VkResult result =
         anv_reloc_list_add_bo(&cmd_buffer->surface_relocs,
                               state->aux_address.bo);
      if (result != VK_SUCCESS)
         anv_batch_set_error(&cmd_buffer->batch, result);
   }

   if (!anv_address_is_null(state->clear_address)) {
      VkResult result =
         anv_reloc_list_add_bo(&cmd_buffer->surface_relocs,
                               state->clear_address.bo);
      if (result != VK_SUCCESS)
         anv_batch_set_error(&cmd_buffer->batch, result);
   }
}

/* Transitions a HiZ-enabled depth buffer from one layout to another. Unless
 * the initial layout is undefined, the HiZ buffer and depth buffer will
 * represent the same data at the end of this operation.
 */
static void
transition_depth_buffer(struct anv_cmd_buffer *cmd_buffer,
                        const struct anv_image *image,
                        uint32_t base_level, uint32_t level_count,
                        uint32_t base_layer, uint32_t layer_count,
                        VkImageLayout initial_layout,
                        VkImageLayout final_layout,
                        bool will_full_fast_clear)
{
   const uint32_t depth_plane =
      anv_image_aspect_to_plane(image, VK_IMAGE_ASPECT_DEPTH_BIT);
   if (image->planes[depth_plane].aux_usage == ISL_AUX_USAGE_NONE)
      return;

   /* Initialize the indirect clear color prior to first use. */
   if (image->planes[depth_plane].fast_clear_memory_range.size > 0 &&
       (initial_layout == VK_IMAGE_LAYOUT_UNDEFINED ||
        initial_layout == VK_IMAGE_LAYOUT_PREINITIALIZED)) {
      const enum isl_format depth_format =
         image->planes[depth_plane].primary_surface.isl.format;
      genX(set_fast_clear_state)(cmd_buffer, image, depth_format,
                                 ISL_SWIZZLE_IDENTITY,
                                 anv_image_hiz_clear_value(image));
   }

   /* If will_full_fast_clear is set, the caller promises to fast-clear the
    * largest portion of the specified range as it can.
    */
   if (will_full_fast_clear)
      return;

   const enum isl_aux_state initial_state =
      anv_layout_to_aux_state(cmd_buffer->device->info, image,
                              VK_IMAGE_ASPECT_DEPTH_BIT,
                              initial_layout,
                              cmd_buffer->queue_family->queueFlags);
   const enum isl_aux_state final_state =
      anv_layout_to_aux_state(cmd_buffer->device->info, image,
                              VK_IMAGE_ASPECT_DEPTH_BIT,
                              final_layout,
                              cmd_buffer->queue_family->queueFlags);

   const bool initial_depth_valid =
      isl_aux_state_has_valid_primary(initial_state);
   const bool initial_hiz_valid =
      isl_aux_state_has_valid_aux(initial_state);
   const bool final_needs_depth =
      isl_aux_state_has_valid_primary(final_state);
   const bool final_needs_hiz =
      isl_aux_state_has_valid_aux(final_state);

   /* Getting into the pass-through state for Depth is tricky and involves
    * both a resolve and an ambiguate.  We don't handle that state right now
    * as anv_layout_to_aux_state never returns it.
    */
   assert(final_state != ISL_AUX_STATE_PASS_THROUGH);

   enum isl_aux_op hiz_op = ISL_AUX_OP_NONE;
   if (final_needs_depth && !initial_depth_valid) {
      assert(initial_hiz_valid);
      hiz_op = ISL_AUX_OP_FULL_RESOLVE;
   } else if (final_needs_hiz && !initial_hiz_valid) {
      assert(initial_depth_valid);
      hiz_op = ISL_AUX_OP_AMBIGUATE;
   }

   if (hiz_op != ISL_AUX_OP_NONE) {
      for (uint32_t l = 0; l < level_count; l++) {
         const uint32_t level = base_level + l;

         uint32_t aux_layers =
            anv_image_aux_layers(image, VK_IMAGE_ASPECT_DEPTH_BIT, level);
         if (base_layer >= aux_layers)
            break; /* We will only get fewer layers as level increases */
         uint32_t level_layer_count =
            MIN2(layer_count, aux_layers - base_layer);

         anv_image_hiz_op(cmd_buffer, image, VK_IMAGE_ASPECT_DEPTH_BIT,
                          level, base_layer, level_layer_count, hiz_op);
      }
   }

   /* Additional tile cache flush for MTL:
    *
    * https://gitlab.freedesktop.org/mesa/mesa/-/issues/10420
    * https://gitlab.freedesktop.org/mesa/mesa/-/issues/10530
    */
   if (intel_device_info_is_mtl(cmd_buffer->device->info) &&
       image->planes[depth_plane].aux_usage == ISL_AUX_USAGE_HIZ_CCS &&
       final_needs_depth && !initial_depth_valid) {
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_TILE_CACHE_FLUSH_BIT,
                                "HIZ-CCS flush");
   }
}

/* Transitions a HiZ-enabled depth buffer from one layout to another. Unless
 * the initial layout is undefined, the HiZ buffer and depth buffer will
 * represent the same data at the end of this operation.
 */
static void
transition_stencil_buffer(struct anv_cmd_buffer *cmd_buffer,
                          const struct anv_image *image,
                          uint32_t base_level, uint32_t level_count,
                          uint32_t base_layer, uint32_t layer_count,
                          VkImageLayout initial_layout,
                          VkImageLayout final_layout,
                          bool will_full_fast_clear)
{
#if GFX_VER == 12
   const uint32_t plane =
      anv_image_aspect_to_plane(image, VK_IMAGE_ASPECT_STENCIL_BIT);
   if (image->planes[plane].aux_usage == ISL_AUX_USAGE_NONE)
      return;

   if ((initial_layout == VK_IMAGE_LAYOUT_UNDEFINED ||
        initial_layout == VK_IMAGE_LAYOUT_PREINITIALIZED) &&
       cmd_buffer->device->info->has_aux_map) {
      /* If will_full_fast_clear is set, the caller promises to fast-clear the
       * largest portion of the specified range as it can.
       */
      if (will_full_fast_clear)
         return;

      for (uint32_t l = 0; l < level_count; l++) {
         const uint32_t level = base_level + l;
         const VkRect2D clear_rect = {
            .offset.x = 0,
            .offset.y = 0,
            .extent.width = u_minify(image->vk.extent.width, level),
            .extent.height = u_minify(image->vk.extent.height, level),
         };

         uint32_t aux_layers =
            anv_image_aux_layers(image, VK_IMAGE_ASPECT_STENCIL_BIT, level);

         if (base_layer >= aux_layers)
            break; /* We will only get fewer layers as level increases */

         uint32_t level_layer_count =
            MIN2(layer_count, aux_layers - base_layer);

         /* From Bspec's 3DSTATE_STENCIL_BUFFER_BODY > Stencil Compression
          * Enable:
          *
          *    "When enabled, Stencil Buffer needs to be initialized via
          *    stencil clear (HZ_OP) before any renderpass."
          */
         const VkClearDepthStencilValue clear_value = {};
         anv_image_hiz_clear(cmd_buffer, image, VK_IMAGE_ASPECT_STENCIL_BIT,
                             level, base_layer, level_layer_count,
                             clear_rect, &clear_value);
      }
   }

   /* Additional tile cache flush for MTL:
    *
    * https://gitlab.freedesktop.org/mesa/mesa/-/issues/10420
    * https://gitlab.freedesktop.org/mesa/mesa/-/issues/10530
    */
   if (intel_device_info_is_mtl(cmd_buffer->device->info)) {
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_TILE_CACHE_FLUSH_BIT,
                                "HIZ-CCS flush");
   }
#endif
}

#define MI_PREDICATE_SRC0    0x2400
#define MI_PREDICATE_SRC1    0x2408
#define MI_PREDICATE_RESULT  0x2418

static void
set_image_compressed_bit(struct anv_cmd_buffer *cmd_buffer,
                         const struct anv_image *image,
                         VkImageAspectFlagBits aspect,
                         uint32_t level,
                         uint32_t base_layer, uint32_t layer_count,
                         bool compressed)
{
   const uint32_t plane = anv_image_aspect_to_plane(image, aspect);

   /* We only have compression tracking for CCS_E */
   if (!isl_aux_usage_has_ccs_e(image->planes[plane].aux_usage))
      return;

   struct anv_device *device = cmd_buffer->device;
   struct mi_builder b;
   mi_builder_init(&b, device->info, &cmd_buffer->batch);
   mi_builder_set_mocs(&b, isl_mocs(&device->isl_dev, 0, false));

   for (uint32_t a = 0; a < layer_count; a++) {
      uint32_t layer = base_layer + a;
      struct anv_address comp_state_addr =
         anv_image_get_compression_state_addr(device,
                                              image, aspect,
                                              level, layer);
      mi_store(&b, mi_mem32(comp_state_addr),
                   mi_imm(compressed ? UINT32_MAX : 0));
   }

   /* FCV_CCS_E images are automatically fast cleared to default value at
    * render time. In order to account for this, anv should set the the
    * appropriate fast clear state for level0/layer0.
    *
    * At the moment, tracking the fast clear state for higher levels/layers is
    * neither supported, nor do we enter a situation where it is a concern.
    */
   if (image->planes[plane].aux_usage == ISL_AUX_USAGE_FCV_CCS_E &&
       base_layer == 0 && level == 0) {
      struct anv_address fc_type_addr =
         anv_image_get_fast_clear_type_addr(device, image, aspect);
      mi_store(&b, mi_mem32(fc_type_addr),
                   mi_imm(ANV_FAST_CLEAR_DEFAULT_VALUE));
   }
}

static void
set_image_fast_clear_state(struct anv_cmd_buffer *cmd_buffer,
                           const struct anv_image *image,
                           VkImageAspectFlagBits aspect,
                           enum anv_fast_clear_type fast_clear)
{
   struct anv_device *device = cmd_buffer->device;
   struct mi_builder b;
   mi_builder_init(&b, device->info, &cmd_buffer->batch);
   mi_builder_set_mocs(&b, isl_mocs(&device->isl_dev, 0, false));

   struct anv_address fc_type_addr =
      anv_image_get_fast_clear_type_addr(device, image, aspect);
   mi_store(&b, mi_mem32(fc_type_addr), mi_imm(fast_clear));

   /* Whenever we have fast-clear, we consider that slice to be compressed.
    * This makes building predicates much easier.
    */
   if (fast_clear != ANV_FAST_CLEAR_NONE)
      set_image_compressed_bit(cmd_buffer, image, aspect, 0, 0, 1, true);
}

/* This is only really practical on haswell and above because it requires
 * MI math in order to get it correct.
 */
static void
anv_cmd_compute_resolve_predicate(struct anv_cmd_buffer *cmd_buffer,
                                  const struct anv_image *image,
                                  VkImageAspectFlagBits aspect,
                                  uint32_t level, uint32_t array_layer,
                                  enum isl_aux_op resolve_op,
                                  enum anv_fast_clear_type fast_clear_supported)
{
   struct anv_device *device = cmd_buffer->device;
   struct anv_address addr =
      anv_image_get_fast_clear_type_addr(device, image, aspect);
   struct mi_builder b;
   mi_builder_init(&b, device->info, &cmd_buffer->batch);
   mi_builder_set_mocs(&b, isl_mocs(&device->isl_dev, 0, false));

   const struct mi_value fast_clear_type = mi_mem32(addr);

   if (resolve_op == ISL_AUX_OP_FULL_RESOLVE) {
      /* In this case, we're doing a full resolve which means we want the
       * resolve to happen if any compression (including fast-clears) is
       * present.
       *
       * In order to simplify the logic a bit, we make the assumption that,
       * if the first slice has been fast-cleared, it is also marked as
       * compressed.  See also set_image_fast_clear_state.
       */
      const struct mi_value compression_state =
         mi_mem32(anv_image_get_compression_state_addr(device,
                                                       image, aspect,
                                                       level, array_layer));
      mi_store(&b, mi_reg64(MI_PREDICATE_SRC0), compression_state);
      mi_store(&b, compression_state, mi_imm(0));

      if (level == 0 && array_layer == 0) {
         /* If the predicate is true, we want to write 0 to the fast clear type
          * and, if it's false, leave it alone.  We can do this by writing
          *
          * clear_type = clear_type & ~predicate;
          */
         struct mi_value new_fast_clear_type =
            mi_iand(&b, fast_clear_type,
                        mi_inot(&b, mi_reg64(MI_PREDICATE_SRC0)));
         mi_store(&b, fast_clear_type, new_fast_clear_type);
      }
   } else if (level == 0 && array_layer == 0) {
      /* In this case, we are doing a partial resolve to get rid of fast-clear
       * colors.  We don't care about the compression state but we do care
       * about how much fast clear is allowed by the final layout.
       */
      assert(resolve_op == ISL_AUX_OP_PARTIAL_RESOLVE);
      assert(fast_clear_supported < ANV_FAST_CLEAR_ANY);

      /* We need to compute (fast_clear_supported < image->fast_clear) */
      struct mi_value pred =
         mi_ult(&b, mi_imm(fast_clear_supported), fast_clear_type);
      mi_store(&b, mi_reg64(MI_PREDICATE_SRC0), mi_value_ref(&b, pred));

      /* If the predicate is true, we want to write 0 to the fast clear type
       * and, if it's false, leave it alone.  We can do this by writing
       *
       * clear_type = clear_type & ~predicate;
       */
      struct mi_value new_fast_clear_type =
         mi_iand(&b, fast_clear_type, mi_inot(&b, pred));
      mi_store(&b, fast_clear_type, new_fast_clear_type);
   } else {
      /* In this case, we're trying to do a partial resolve on a slice that
       * doesn't have clear color.  There's nothing to do.
       */
      assert(resolve_op == ISL_AUX_OP_PARTIAL_RESOLVE);
      return;
   }

   /* Set src1 to 0 and use a != condition */
   mi_store(&b, mi_reg64(MI_PREDICATE_SRC1), mi_imm(0));

   anv_batch_emit(&cmd_buffer->batch, GENX(MI_PREDICATE), mip) {
      mip.LoadOperation    = LOAD_LOADINV;
      mip.CombineOperation = COMBINE_SET;
      mip.CompareOperation = COMPARE_SRCS_EQUAL;
   }
}

static void
anv_cmd_predicated_ccs_resolve(struct anv_cmd_buffer *cmd_buffer,
                               const struct anv_image *image,
                               enum isl_format format,
                               struct isl_swizzle swizzle,
                               VkImageAspectFlagBits aspect,
                               uint32_t level, uint32_t array_layer,
                               enum isl_aux_op resolve_op,
                               enum anv_fast_clear_type fast_clear_supported)
{
   const uint32_t plane = anv_image_aspect_to_plane(image, aspect);

   anv_cmd_compute_resolve_predicate(cmd_buffer, image,
                                     aspect, level, array_layer,
                                     resolve_op, fast_clear_supported);

   /* CCS_D only supports full resolves and BLORP will assert on us if we try
    * to do a partial resolve on a CCS_D surface.
    */
   if (resolve_op == ISL_AUX_OP_PARTIAL_RESOLVE &&
       image->planes[plane].aux_usage == ISL_AUX_USAGE_CCS_D)
      resolve_op = ISL_AUX_OP_FULL_RESOLVE;

   anv_image_ccs_op(cmd_buffer, image, format, swizzle, aspect,
                    level, array_layer, 1, resolve_op, NULL, true);
}

static void
anv_cmd_predicated_mcs_resolve(struct anv_cmd_buffer *cmd_buffer,
                               const struct anv_image *image,
                               enum isl_format format,
                               struct isl_swizzle swizzle,
                               VkImageAspectFlagBits aspect,
                               uint32_t array_layer,
                               enum isl_aux_op resolve_op,
                               enum anv_fast_clear_type fast_clear_supported)
{
   assert(aspect == VK_IMAGE_ASPECT_COLOR_BIT);
   assert(resolve_op == ISL_AUX_OP_PARTIAL_RESOLVE);

   anv_cmd_compute_resolve_predicate(cmd_buffer, image,
                                     aspect, 0, array_layer,
                                     resolve_op, fast_clear_supported);

   anv_image_mcs_op(cmd_buffer, image, format, swizzle, aspect,
                    array_layer, 1, resolve_op, NULL, true);
}

void
genX(cmd_buffer_mark_image_written)(struct anv_cmd_buffer *cmd_buffer,
                                    const struct anv_image *image,
                                    VkImageAspectFlagBits aspect,
                                    enum isl_aux_usage aux_usage,
                                    uint32_t level,
                                    uint32_t base_layer,
                                    uint32_t layer_count)
{
#if GFX_VER < 20
   /* The aspect must be exactly one of the image aspects. */
   assert(util_bitcount(aspect) == 1 && (aspect & image->vk.aspects));

   /* Filter out aux usages that don't have any compression tracking.
    * Note: We only have compression tracking for CCS_E images, but it's
    * possible for a CCS_E enabled image to have a subresource with a different
    * aux usage.
    */
   if (!isl_aux_usage_has_compression(aux_usage))
      return;

   set_image_compressed_bit(cmd_buffer, image, aspect,
                            level, base_layer, layer_count, true);
#endif
}

/* Copy the fast-clear value dword(s) between a surface state object and an
 * image's fast clear state buffer.
 */
void
genX(cmd_buffer_load_clear_color)(struct anv_cmd_buffer *cmd_buffer,
                                  struct anv_state surface_state,
                                  const struct anv_image_view *iview)
{
#if GFX_VER < 10
   struct anv_address ss_clear_addr =
      anv_state_pool_state_address(
         &cmd_buffer->device->internal_surface_state_pool,
         (struct anv_state) {
            .offset = surface_state.offset +
                      cmd_buffer->device->isl_dev.ss.clear_value_offset
         });
   const struct anv_address entry_addr =
      anv_image_get_clear_color_addr(cmd_buffer->device, iview->image,
                                     iview->planes[0].isl.format,
                                     VK_IMAGE_ASPECT_COLOR_BIT, false);

   unsigned copy_size = cmd_buffer->device->isl_dev.ss.clear_value_size;

   struct mi_builder b;
   mi_builder_init(&b, cmd_buffer->device->info, &cmd_buffer->batch);
   mi_builder_set_write_check(&b, true);

   mi_memcpy(&b, ss_clear_addr, entry_addr, copy_size);

   /* Updating a surface state object may require that the state cache be
    * invalidated. From the SKL PRM, Shared Functions -> State -> State
    * Caching:
    *
    *    Whenever the RENDER_SURFACE_STATE object in memory pointed to by
    *    the Binding Table Pointer (BTP) and Binding Table Index (BTI) is
    *    modified [...], the L1 state cache must be invalidated to ensure
    *    the new surface or sampler state is fetched from system memory.
    *
    * In testing, SKL doesn't actually seem to need this, but HSW does.
    */
   anv_add_pending_pipe_bits(cmd_buffer,
                             ANV_PIPE_STATE_CACHE_INVALIDATE_BIT,
                             "after load_clear_color surface state update");
#endif
}

static void
set_image_clear_color(struct anv_cmd_buffer *cmd_buffer,
                      const struct anv_image *image,
                      const VkImageAspectFlags aspect,
                      const uint32_t *pixel)
{
   for (int i = 0; i < image->num_view_formats; i++) {
      union isl_color_value clear_color;
      if (image->view_formats[i] == ISL_FORMAT_RAW) {
         /* Only used for cross aspect copies (color <-> depth/stencil) */
         assert(vk_format_is_color_depth_stencil_capable(image->vk.format));
         assert(vk_format_get_blocksizebits(image->vk.format) <= 32);
         memcpy(clear_color.u32, pixel, sizeof(clear_color.u32));
      } else {
         isl_color_value_unpack(&clear_color, image->view_formats[i], pixel);
      }

      const struct anv_address addr =
         anv_image_get_clear_color_addr(cmd_buffer->device, image,
                                        image->view_formats[i], aspect,
                                        false);
      assert(!anv_address_is_null(addr));

#if GFX_VER >= 11
      assert(cmd_buffer->device->isl_dev.ss.clear_color_state_size == 32);
      uint32_t *dw = anv_batch_emitn(&cmd_buffer->batch, 3 + 6,
                                     GENX(MI_STORE_DATA_IMM),
                                     .StoreQword = true, .Address = addr);

      bool is_depth = aspect & VK_IMAGE_ASPECT_DEPTH_BIT;
      uint64_t sampler_offset =
         isl_get_sampler_clear_field_offset(cmd_buffer->device->info,
                                            image->view_formats[i], is_depth);
      dw[3] = clear_color.u32[0];
      dw[4] = clear_color.u32[1];
      dw[5] = clear_color.u32[2];
      dw[6] = clear_color.u32[3];
      dw[7] = 0;
      dw[8] = 0;
      dw[3 + sampler_offset / 4] = pixel[0];
      dw[4 + sampler_offset / 4] = pixel[1];
#else
      assert(cmd_buffer->device->isl_dev.ss.clear_color_state_size == 0);
      assert(cmd_buffer->device->isl_dev.ss.clear_value_size == 16);
      uint32_t *dw = anv_batch_emitn(&cmd_buffer->batch, 3 + 8,
                                     GENX(MI_STORE_DATA_IMM),
                                     .StoreQword = true, .Address = addr);
      dw[3] = clear_color.u32[0];
      dw[4] = clear_color.u32[1];
      dw[5] = clear_color.u32[2];
      dw[6] = clear_color.u32[3];
      dw[7] = fui(util_format_linear_to_srgb_float(clear_color.f32[0]));
      dw[8] = fui(util_format_linear_to_srgb_float(clear_color.f32[1]));
      dw[9] = fui(util_format_linear_to_srgb_float(clear_color.f32[2]));
      dw[10] = clear_color.u32[3];
#endif
   }
}

void
genX(set_fast_clear_state)(struct anv_cmd_buffer *cmd_buffer,
                           const struct anv_image *image,
                           const enum isl_format format,
                           const struct isl_swizzle swizzle,
                           union isl_color_value clear_color)
{
   VkImageAspectFlagBits aspect = image->vk.aspects &
      (VK_IMAGE_ASPECT_COLOR_BIT | VK_IMAGE_ASPECT_DEPTH_BIT);

   uint32_t pixel[4] = {};
   union isl_color_value swiz_color =
      isl_color_value_swizzle_inv(clear_color, swizzle);
   isl_color_value_pack(&swiz_color, format, pixel);
   set_image_clear_color(cmd_buffer, image, aspect, pixel);

   if (isl_color_value_is_zero(clear_color, format)) {
      /* This image has the auxiliary buffer enabled. We can mark the
       * subresource as not needing a resolve because the clear color
       * will match what's in every RENDER_SURFACE_STATE object when
       * it's being used for sampling.
       */
      set_image_fast_clear_state(cmd_buffer, image, aspect,
                                 ANV_FAST_CLEAR_DEFAULT_VALUE);
   } else {
      set_image_fast_clear_state(cmd_buffer, image, aspect,
                                 ANV_FAST_CLEAR_ANY);
   }
}

static bool ATTRIBUTE_CONST
queue_family_is_external(uint32_t index)
{
      return index == VK_QUEUE_FAMILY_FOREIGN_EXT ||
             index == VK_QUEUE_FAMILY_EXTERNAL;
}

/**
 * @brief Transitions a color buffer from one layout to another.
 *
 * See section 6.1.1. Image Layout Transitions of the Vulkan 1.0.50 spec for
 * more information.
 *
 * @param level_count VK_REMAINING_MIP_LEVELS isn't supported.
 * @param layer_count VK_REMAINING_ARRAY_LAYERS isn't supported. For 3D images,
 *                    this represents the maximum layers to transition at each
 *                    specified miplevel.
 * @param acquire_unmodified True if
 *    VkExternalMemoryAcquireUnmodifiedEXT::acquireUnmodifiedMemory is set and
 *    relevant.
 */
static void
transition_color_buffer(struct anv_cmd_buffer *cmd_buffer,
                        const struct anv_image *image,
                        VkImageAspectFlagBits aspect,
                        const uint32_t base_level, uint32_t level_count,
                        uint32_t base_layer, uint32_t layer_count,
                        VkImageLayout initial_layout,
                        VkImageLayout final_layout,
                        uint32_t src_queue_family,
                        uint32_t dst_queue_family,
                        bool will_full_fast_clear,
                        bool acquire_unmodified)
{
   struct anv_device *device = cmd_buffer->device;
   const struct intel_device_info *devinfo = device->info;
   /* Validate the inputs. */
   assert(cmd_buffer);
   assert(image && image->vk.aspects & VK_IMAGE_ASPECT_ANY_COLOR_BIT_ANV);
   /* These values aren't supported for simplicity's sake. */
   assert(level_count != VK_REMAINING_MIP_LEVELS &&
          layer_count != VK_REMAINING_ARRAY_LAYERS);
   /* Ensure the subresource range is valid. */
   UNUSED uint64_t last_level_num = base_level + level_count;
   const uint32_t max_depth = u_minify(image->vk.extent.depth, base_level);
   UNUSED const uint32_t image_layers = MAX2(image->vk.array_layers, max_depth);
   assert((uint64_t)base_layer + layer_count  <= image_layers);
   assert(last_level_num <= image->vk.mip_levels);
   /* If there is a layout transfer, the final layout cannot be undefined or
    * preinitialized (VUID-VkImageMemoryBarrier-newLayout-01198).
    */
   assert(initial_layout == final_layout ||
          (final_layout != VK_IMAGE_LAYOUT_UNDEFINED &&
           final_layout != VK_IMAGE_LAYOUT_PREINITIALIZED));
   const struct isl_drm_modifier_info *isl_mod_info =
      image->vk.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
      ? isl_drm_modifier_get_info(image->vk.drm_format_mod)
      : NULL;

   /**
    * Vulkan 1.4.313, 7.7.4. Queue Family Ownership Transfer:
    *
    *    If the values of srcQueueFamilyIndex and dstQueueFamilyIndex are equal,
    *    no ownership transfer is performed, and the barrier operates as if they
    *    were both set to VK_QUEUE_FAMILY_IGNORED.
    */
   if (src_queue_family == dst_queue_family)
      src_queue_family = dst_queue_family = VK_QUEUE_FAMILY_IGNORED;

   const bool src_queue_external = queue_family_is_external(src_queue_family);
   const bool dst_queue_external = queue_family_is_external(dst_queue_family);

   /* If the queues are external, consider the first queue family flags
    * (should be the most capable)
    */
   const VkQueueFlagBits src_queue_flags =
      device->physical->queue.families[
         (src_queue_external || src_queue_family == VK_QUEUE_FAMILY_IGNORED) ?
         0 : src_queue_family].queueFlags;
   const VkQueueFlagBits dst_queue_flags =
      device->physical->queue.families[
         (dst_queue_external || dst_queue_family == VK_QUEUE_FAMILY_IGNORED) ?
         0 : dst_queue_family].queueFlags;

   /* Simultaneous acquire and release on external queues is illegal. */
   assert(!src_queue_external || !dst_queue_external);

   /* Ownership transition on an external queue requires special action if we
    * store any image data in a driver-private bo that is inaccessible to the
    * external queue.
    */
   const bool private_binding_acquire =
      src_queue_external &&
      anv_image_has_private_binding(image);

   const bool private_binding_release =
      dst_queue_external &&
      anv_image_has_private_binding(image);

   if (initial_layout == final_layout &&
       !private_binding_acquire && !private_binding_release) {
      /* No work is needed. */
       return;
   }

   /**
    * Section 7.7.4 of the Vulkan 1.3.260 spec says:
    *
    *    If the transfer is via an image memory barrier, and an image layout
    *    transition is desired, then the values of oldLayout and newLayout in the
    *    release operation's memory barrier must be equal to values of oldLayout
    *    and newLayout in the acquire operation's memory barrier. Although the
    *    image layout transition is submitted twice, it will only be executed
    *    once. A layout transition specified in this way happens-after the
    *    release operation and happens-before the acquire operation.
    *
    * Because we know that we get match transition on each queue, we choose to
    * only do the work on one queue type : RENDER. In the cases where we do
    * transitions between COMPUTE & TRANSFER, we should have matching
    * aux/fast_clear value which would trigger no work in the code below.
    */
   if (!(src_queue_external || dst_queue_external) &&
       src_queue_family != VK_QUEUE_FAMILY_IGNORED &&
       dst_queue_family != VK_QUEUE_FAMILY_IGNORED &&
       src_queue_family != dst_queue_family) {
      enum intel_engine_class src_engine =
         cmd_buffer->queue_family->engine_class;
      if (src_engine != INTEL_ENGINE_CLASS_RENDER)
         return;
   }

   const uint32_t plane = anv_image_aspect_to_plane(image, aspect);

   /* Early return for CPB surfaces, nothing to do */
   if (isl_surf_usage_is_cpb(image->planes[plane].primary_surface.isl.usage))
      return;

   if (base_layer >= anv_image_aux_layers(image, aspect, base_level))
      return;

   enum isl_aux_usage initial_aux_usage =
      anv_layout_to_aux_usage(devinfo, image, aspect, 0,
                              initial_layout, src_queue_flags);
   enum isl_aux_usage final_aux_usage =
      anv_layout_to_aux_usage(devinfo, image, aspect, 0,
                              final_layout, dst_queue_flags);
   enum anv_fast_clear_type initial_fast_clear =
      anv_layout_to_fast_clear_type(devinfo, image, aspect, initial_layout,
                                    src_queue_flags);
   enum anv_fast_clear_type final_fast_clear =
      anv_layout_to_fast_clear_type(devinfo, image, aspect, final_layout,
                                    dst_queue_flags);

   /* We make this guarantee with optimalImageTransferToQueueFamilies */
   if (devinfo->ver >= 20 && (src_queue_external || dst_queue_external))
      assert(initial_aux_usage == final_aux_usage);

   /* We must override the anv_layout_to_* functions because they are unaware
    * of acquire/release direction.
    */
   if (private_binding_acquire) {
      initial_aux_usage = isl_drm_modifier_has_aux(isl_mod_info->modifier) ?
         image->planes[plane].aux_usage : ISL_AUX_USAGE_NONE;
      initial_fast_clear = isl_mod_info->supports_clear_color ?
         initial_fast_clear : ANV_FAST_CLEAR_NONE;
   } else if (private_binding_release) {
      final_aux_usage = isl_drm_modifier_has_aux(isl_mod_info->modifier) ?
         image->planes[plane].aux_usage : ISL_AUX_USAGE_NONE;
      final_fast_clear = isl_mod_info->supports_clear_color ?
         final_fast_clear : ANV_FAST_CLEAR_NONE;
   }

   /* The following layouts are equivalent for non-linear images and
    * for images not bound to host-visible memory.
    */
   const bool initial_layout_undefined =
      initial_layout == VK_IMAGE_LAYOUT_UNDEFINED ||
      initial_layout == VK_IMAGE_LAYOUT_PREINITIALIZED;

   bool must_init_fast_clear_state = false;
   bool must_init_aux_surface = false;

   if (initial_layout_undefined) {
      /* The subresource may have been aliased and populated with arbitrary
       * data, so we should initialize fast-clear state on platforms prior to
       * Xe2. Xe2+ platforms don't need it thanks to the new design of fast-
       * clear.
       */
      must_init_fast_clear_state = devinfo->ver < 20;

      if (isl_aux_usage_has_mcs(image->planes[plane].aux_usage) ||
          devinfo->has_illegal_ccs_values) {

         must_init_aux_surface = true;

      } else {
         assert(isl_aux_usage_has_ccs_e(image->planes[plane].aux_usage));

         /* We can start using the CCS immediately without ambiguating. The
          * two conditions that enable this are:
          *
          * 1) The device treats all possible CCS values as legal. In other
          *    words, we can't confuse the hardware with random bits in the
          *    CCS.
          *
          * 2) We enable compression on all writable image layouts. The CCS
          *    will receive all writes and will therefore always be in sync
          *    with the main surface.
          *
          *    If we were to disable compression on some writable layouts, the
          *    CCS could get out of sync with the main surface and the app
          *    could lose the data it wrote previously. For example, this
          *    could happen if an app: transitions from UNDEFINED w/o
          *    ambiguating -> renders with AUX_NONE -> samples with AUX_CCS.
          *
          * The second condition is asserted below, but could be moved
          * elsewhere for more coverage (we're only checking transitions from
          * an undefined layout).
          */
         assert(vk_image_layout_is_read_only(final_layout, aspect) ||
                (final_aux_usage != ISL_AUX_USAGE_NONE));

         must_init_aux_surface = false;
      }
   } else if (private_binding_acquire && !acquire_unmodified) {
      /* The fast clear state lives in a driver-private bo, and therefore the
       * external/foreign queue is unaware of it.
       *
       * If this is the first time we are accessing the image, then the fast
       * clear state is uninitialized.
       *
       * If this is NOT the first time we are accessing the image, then the fast
       * clear state may still be valid and correct due to the resolve during
       * our most recent ownership release.  However, we do not track the aux
       * state with MI stores, and therefore must assume the worst-case: that
       * this is the first time we are accessing the image.
       */
      assert(image->planes[plane].fast_clear_memory_range.binding ==
              ANV_IMAGE_MEMORY_BINDING_PRIVATE);
      must_init_fast_clear_state = true;

      if (anv_image_get_aux_memory_range(image, plane)->binding ==
          ANV_IMAGE_MEMORY_BINDING_PRIVATE) {
         /* The aux surface, like the fast clear state, lives in
          * a driver-private bo.  We must initialize the aux surface for the
          * same reasons we must initialize the fast clear state.
          */
         must_init_aux_surface = true;
      } else {
         /* The aux surface, unlike the fast clear state, lives in
          * application-visible VkDeviceMemory and is shared with the
          * external/foreign queue. Therefore, when we acquire ownership of the
          * image with a defined VkImageLayout, the aux surface is valid and has
          * the aux state required by the modifier.
          */
         must_init_aux_surface = false;
      }
   } else if (private_binding_acquire && acquire_unmodified) {
      /* The Vulkan 1.3.302 spec ensures we have previously initialized the
       * image memory, and therefore initialized the fast clear state and aux
       * surface, because initial_layout_undefined is false and
       * acquireUnmodifiedMemory is true.
       *
       * Since the time of our most recent image ownership release and up
       * until the current ownership re-acquisition, the externally-shared
       * image memory has remained unmodified. Therefore the fast clear state
       * and aux surface are valid and consistent with the image content.
       */
      must_init_fast_clear_state = false;
      must_init_aux_surface = false;
   }

   if (must_init_fast_clear_state) {
      if (image->planes[plane].aux_usage == ISL_AUX_USAGE_FCV_CCS_E) {
         /* Ensure the raw and converted clear colors are in sync. */
         const uint32_t zero_pixel[4] = {};
         set_image_clear_color(cmd_buffer, image, aspect, zero_pixel);
      }
      if (base_level == 0 && base_layer == 0) {
         set_image_fast_clear_state(cmd_buffer, image, aspect,
                                    ANV_FAST_CLEAR_NONE);
      }
   }

   if (must_init_aux_surface) {
      assert(devinfo->ver >= 20 || must_init_fast_clear_state);

      /* Initialize the aux buffers to enable correct rendering.  In order to
       * ensure that things such as storage images work correctly, aux buffers
       * need to be initialized to valid data.
       *
       * Having an aux buffer with invalid data is a problem for two reasons:
       *
       *  1) Having an invalid value in the buffer can confuse the hardware.
       *     For instance, with CCS_E on SKL, a two-bit CCS value of 2 is
       *     invalid and leads to the hardware doing strange things.  It
       *     doesn't hang as far as we can tell but rendering corruption can
       *     occur.
       *
       *  2) If this transition is into the GENERAL layout and we then use the
       *     image as a storage image, then we must have the aux buffer in the
       *     pass-through state so that, if we then go to texture from the
       *     image, we get the results of our storage image writes and not the
       *     fast clear color or other random data.
       *
       * For CCS both of the problems above are real demonstrable issues.  In
       * that case, the only thing we can do is to perform an ambiguate to
       * transition the aux surface into the pass-through state.
       *
       * For MCS, (2) is never an issue because we don't support multisampled
       * storage images.  In theory, issue (1) is a problem with MCS but we've
       * never seen it in the wild.  For 4x and 16x, all bit patterns could,
       * in theory, be interpreted as something but we don't know that all bit
       * patterns are actually valid.  For 2x and 8x, you could easily end up
       * with the MCS referring to an invalid plane because not all bits of
       * the MCS value are actually used.  Even though we've never seen issues
       * in the wild, it's best to play it safe and initialize the MCS.  We
       * could use a fast-clear for MCS because we only ever touch from render
       * and texture (no image load store). However, due to WA 14013111325,
       * we choose to ambiguate MCS as well.
       */
      if (image->vk.samples == 1) {
         for (uint32_t l = 0; l < level_count; l++) {
            const uint32_t level = base_level + l;

            uint32_t aux_layers = anv_image_aux_layers(image, aspect, level);
            if (base_layer >= aux_layers)
               break; /* We will only get fewer layers as level increases */
            uint32_t level_layer_count =
               MIN2(layer_count, aux_layers - base_layer);

            /* If will_full_fast_clear is set, the caller promises to
             * fast-clear the largest portion of the specified range as it can.
             * For color images, that means only the first LOD and array slice.
             */
            if (level == 0 && base_layer == 0 && will_full_fast_clear) {
               base_layer++;
               level_layer_count--;
               if (level_layer_count == 0)
                  continue;
            }

            anv_image_ccs_op(cmd_buffer, image,
                             image->planes[plane].primary_surface.isl.format,
                             ISL_SWIZZLE_IDENTITY,
                             aspect, level, base_layer, level_layer_count,
                             ISL_AUX_OP_AMBIGUATE, NULL, false);

            set_image_compressed_bit(cmd_buffer, image, aspect, level,
                                     base_layer, level_layer_count, false);
         }
      } else {
         /* If will_full_fast_clear is set, the caller promises to fast-clear
          * the largest portion of the specified range as it can.
          */
         if (will_full_fast_clear)
            return;

         assert(base_level == 0 && level_count == 1);
         anv_image_mcs_op(cmd_buffer, image,
                          image->planes[plane].primary_surface.isl.format,
                          ISL_SWIZZLE_IDENTITY,
                          aspect, base_layer, layer_count,
                          ISL_AUX_OP_AMBIGUATE, NULL, false);
      }
      return;
   }

   /* The current code assumes that there is no mixing of CCS_E and CCS_D.
    * We can handle transitions between CCS_D/E to and from NONE.  What we
    * don't yet handle is switching between CCS_E and CCS_D within a given
    * image.  Doing so in a performant way requires more detailed aux state
    * tracking such as what is done in i965.  For now, just assume that we
    * only have one type of compression.
    */
   assert(initial_aux_usage == ISL_AUX_USAGE_NONE ||
          final_aux_usage == ISL_AUX_USAGE_NONE ||
          initial_aux_usage == final_aux_usage);

   /* If initial aux usage is NONE, there is nothing to resolve */
   if (initial_aux_usage == ISL_AUX_USAGE_NONE)
      return;

   enum isl_aux_op resolve_op = ISL_AUX_OP_NONE;

   /* If the initial layout supports more fast clear than the final layout
    * then we need at least a partial resolve.
    */
   if (final_fast_clear < initial_fast_clear) {
      /* Partial resolves will actually only occur on layer 0/level 0. This
       * is generally okay because anv only allows explicit fast clears to
       * the first subresource.
       *
       * The situation is a bit different with FCV_CCS_E. With that aux
       * usage, implicit fast clears can occur on any layer and level.
       * anv doesn't track fast clear states for more than the first
       * subresource, so we need to assert that a layout transition doesn't
       * attempt to partial resolve the other subresources.
       *
       * At the moment, we don't enter such a situation, and partial resolves
       * for higher level/layer resources shouldn't be a concern.
       */
      if (image->planes[plane].aux_usage == ISL_AUX_USAGE_FCV_CCS_E) {
         assert(base_level == 0 && level_count == 1 &&
                base_layer == 0 && layer_count == 1);
      }
      resolve_op = ISL_AUX_OP_PARTIAL_RESOLVE;
   }

   if (isl_aux_usage_has_ccs_e(initial_aux_usage) &&
       !isl_aux_usage_has_ccs_e(final_aux_usage))
      resolve_op = ISL_AUX_OP_FULL_RESOLVE;

   if (resolve_op == ISL_AUX_OP_NONE)
      return;

   for (uint32_t l = 0; l < level_count; l++) {
      uint32_t level = base_level + l;

      uint32_t aux_layers = anv_image_aux_layers(image, aspect, level);
      if (base_layer >= aux_layers)
         break; /* We will only get fewer layers as level increases */
      uint32_t level_layer_count =
         MIN2(layer_count, aux_layers - base_layer);

      for (uint32_t a = 0; a < level_layer_count; a++) {
         uint32_t array_layer = base_layer + a;

         /* If will_full_fast_clear is set, the caller promises to fast-clear
          * the largest portion of the specified range as it can.  For color
          * images, that means only the first LOD and array slice.
          */
         if (level == 0 && array_layer == 0 && will_full_fast_clear)
            continue;

         if (image->vk.samples == 1) {
            anv_cmd_predicated_ccs_resolve(cmd_buffer, image,
                                           image->planes[plane].primary_surface.isl.format,
                                           ISL_SWIZZLE_IDENTITY,
                                           aspect, level, array_layer, resolve_op,
                                           final_fast_clear);
         } else {
            /* We only support fast-clear on the first layer so partial
             * resolves should not be used on other layers as they will use
             * the clear color stored in memory that is only valid for layer0.
             */
            if (resolve_op == ISL_AUX_OP_PARTIAL_RESOLVE &&
                array_layer != 0)
               continue;

            anv_cmd_predicated_mcs_resolve(cmd_buffer, image,
                                           image->planes[plane].primary_surface.isl.format,
                                           ISL_SWIZZLE_IDENTITY,
                                           aspect, array_layer, resolve_op,
                                           final_fast_clear);
         }
      }
   }
}

static MUST_CHECK VkResult
anv_cmd_buffer_init_attachments(struct anv_cmd_buffer *cmd_buffer,
                                uint32_t color_att_count)
{
   struct anv_cmd_graphics_state *gfx = &cmd_buffer->state.gfx;

   /* Reserve one for the NULL state. */
   unsigned num_states = 1 + color_att_count;
   const struct isl_device *isl_dev = &cmd_buffer->device->isl_dev;
   const uint32_t ss_stride = align(isl_dev->ss.size, isl_dev->ss.align);
   gfx->att_states =
      anv_cmd_buffer_alloc_surface_states(cmd_buffer, num_states);
   if (gfx->att_states.map == NULL)
      return VK_ERROR_OUT_OF_DEVICE_MEMORY;

   struct anv_state next_state = gfx->att_states;
   next_state.alloc_size = isl_dev->ss.size;

   gfx->null_surface_state = next_state;
   next_state.offset += ss_stride;
   next_state.map += ss_stride;

   gfx->color_att_count = color_att_count;
   for (uint32_t i = 0; i < color_att_count; i++) {
      gfx->color_att[i] = (struct anv_attachment) {
         .surface_state.state = next_state,
      };
      next_state.offset += ss_stride;
      next_state.map += ss_stride;
   }
   gfx->depth_att = (struct anv_attachment) { };
   gfx->stencil_att = (struct anv_attachment) { };

   return VK_SUCCESS;
}

static void
anv_cmd_buffer_reset_rendering(struct anv_cmd_buffer *cmd_buffer)
{
   struct anv_cmd_graphics_state *gfx = &cmd_buffer->state.gfx;

   gfx->render_area = (VkRect2D) { };
   gfx->layer_count = 0;
   gfx->samples = 0;

   gfx->color_att_count = 0;
   gfx->depth_att = (struct anv_attachment) { };
   gfx->stencil_att = (struct anv_attachment) { };
   gfx->null_surface_state = ANV_STATE_NULL;
}

/**
 * Program the hardware to use the specified L3 configuration.
 */
void
genX(cmd_buffer_config_l3)(struct anv_cmd_buffer *cmd_buffer,
                           const struct intel_l3_config *cfg)
{
   assert(cfg || GFX_VER >= 12);
   if (cfg == cmd_buffer->state.current_l3_config)
      return;

#if GFX_VER >= 11
   /* On Gfx11+ we use only one config, so verify it remains the same and skip
    * the stalling programming entirely.
    */
   assert(cfg == cmd_buffer->device->l3_config);
#else
   if (INTEL_DEBUG(DEBUG_L3)) {
      mesa_logd("L3 config transition: ");
      intel_dump_l3_config(cfg, stderr);
   }

   /* According to the hardware docs, the L3 partitioning can only be changed
    * while the pipeline is completely drained and the caches are flushed,
    * which involves a first PIPE_CONTROL flush which stalls the pipeline...
    */
   genx_batch_emit_pipe_control(&cmd_buffer->batch, cmd_buffer->device->info,
                                cmd_buffer->state.current_pipeline,
                                ANV_PIPE_DATA_CACHE_FLUSH_BIT |
                                ANV_PIPE_CS_STALL_BIT);

   /* ...followed by a second pipelined PIPE_CONTROL that initiates
    * invalidation of the relevant caches.  Note that because RO invalidation
    * happens at the top of the pipeline (i.e. right away as the PIPE_CONTROL
    * command is processed by the CS) we cannot combine it with the previous
    * stalling flush as the hardware documentation suggests, because that
    * would cause the CS to stall on previous rendering *after* RO
    * invalidation and wouldn't prevent the RO caches from being polluted by
    * concurrent rendering before the stall completes.  This intentionally
    * doesn't implement the SKL+ hardware workaround suggesting to enable CS
    * stall on PIPE_CONTROLs with the texture cache invalidation bit set for
    * GPGPU workloads because the previous and subsequent PIPE_CONTROLs
    * already guarantee that there is no concurrent GPGPU kernel execution
    * (see SKL HSD 2132585).
    */
   genx_batch_emit_pipe_control(&cmd_buffer->batch, cmd_buffer->device->info,
                                cmd_buffer->state.current_pipeline,
                                ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT |
                                ANV_PIPE_CONSTANT_CACHE_INVALIDATE_BIT |
                                ANV_PIPE_INSTRUCTION_CACHE_INVALIDATE_BIT |
                                ANV_PIPE_STATE_CACHE_INVALIDATE_BIT);

   /* Now send a third stalling flush to make sure that invalidation is
    * complete when the L3 configuration registers are modified.
    */
   genx_batch_emit_pipe_control(&cmd_buffer->batch, cmd_buffer->device->info,
                                cmd_buffer->state.current_pipeline,
                                ANV_PIPE_DATA_CACHE_FLUSH_BIT |
                                ANV_PIPE_CS_STALL_BIT);

   genX(emit_l3_config)(&cmd_buffer->batch, cmd_buffer->device, cfg);
#endif /* GFX_VER >= 11 */
   cmd_buffer->state.current_l3_config = cfg;
}

ALWAYS_INLINE void
genX(invalidate_aux_map)(struct anv_batch *batch,
                         struct anv_device *device,
                         enum intel_engine_class engine_class,
                         enum anv_pipe_bits bits)
{
#if GFX_VER == 12
   if ((bits & ANV_PIPE_AUX_TABLE_INVALIDATE_BIT) && device->info->has_aux_map) {
      uint32_t register_addr = 0;
      switch (engine_class) {
      case INTEL_ENGINE_CLASS_COMPUTE:
         register_addr = GENX(COMPCS0_CCS_AUX_INV_num);
         break;
      case INTEL_ENGINE_CLASS_COPY:
#if GFX_VERx10 >= 125
         register_addr = GENX(BCS_CCS_AUX_INV_num);
#endif
         break;
      case INTEL_ENGINE_CLASS_VIDEO:
         register_addr = GENX(VD0_CCS_AUX_INV_num);
         break;
      case INTEL_ENGINE_CLASS_RENDER:
      default:
         register_addr = GENX(GFX_CCS_AUX_INV_num);
         break;
      }

      anv_batch_emit(batch, GENX(MI_LOAD_REGISTER_IMM), lri) {
         lri.RegisterOffset = register_addr;
         lri.DataDWord = 1;
      }

      /* HSD 22012751911: SW Programming sequence when issuing aux invalidation:
       *
       *    "Poll Aux Invalidation bit once the invalidation is set
       *     (Register 4208 bit 0)"
       */
      anv_batch_emit(batch, GENX(MI_SEMAPHORE_WAIT), sem) {
         sem.CompareOperation = COMPARE_SAD_EQUAL_SDD;
         sem.WaitMode = PollingMode;
         sem.RegisterPollMode = true;
         sem.SemaphoreDataDword = 0x0;
         sem.SemaphoreAddress =
            anv_address_from_u64(register_addr);
      }
   }
#else
   assert(!device->info->has_aux_map);
#endif
}

ALWAYS_INLINE enum anv_pipe_bits
genX(emit_apply_pipe_flushes)(struct anv_batch *batch,
                              struct anv_device *device,
                              uint32_t current_pipeline,
                              enum anv_pipe_bits bits,
                              enum anv_pipe_bits *emitted_flush_bits)
{
#if GFX_VER >= 12
   /* From the TGL PRM, Volume 2a, "PIPE_CONTROL":
    *
    *     "SW must follow below programming restrictions when programming
    *      PIPE_CONTROL command [for ComputeCS]:
    *      ...
    *      Following bits must not be set when programmed for ComputeCS:
    *      - "Render Target Cache Flush Enable", "Depth Cache Flush Enable"
    *         and "Tile Cache Flush Enable"
    *      - "Depth Stall Enable", Stall at Pixel Scoreboard and
    *         "PSD Sync Enable".
    *      - "OVR Tile 0 Flush", "TBIMR Force Batch Closure",
    *         "AMFS Flush Enable", "VF Cache Invalidation Enable" and
    *         "Global Snapshot Count Reset"."
    *
    * XXX: According to spec this should not be a concern for a regular
    * RCS in GPGPU mode, but during testing it was found that at least
    * "VF Cache Invalidation Enable" bit is ignored in such case.
    * This can cause us to miss some important invalidations
    * (e.g. from CmdPipelineBarriers) and have incoherent data.
    *
    * There is also a Wa_1606932921 "RCS is not waking up fixed function clock
    * when specific 3d related bits are programmed in pipecontrol in
    * compute mode" that suggests us not to use "RT Cache Flush" in GPGPU mode.
    *
    * The other bits are not confirmed to cause problems, but included here
    * just to be safe, as they're also not really relevant in the GPGPU mode,
    * and having them doesn't seem to cause any regressions.
    *
    * So if we're currently in GPGPU mode, we hide some bits from
    * this flush, and will flush them only when we'll be able to.
    * Similar thing with GPGPU-only bits.
    */
   enum anv_pipe_bits defer_bits = bits &
      (current_pipeline == GPGPU ? ANV_PIPE_GFX_BITS: ANV_PIPE_GPGPU_BITS);

   bits &= ~defer_bits;
#endif

   /*
    * From Sandybridge PRM, volume 2, "1.7.2 End-of-Pipe Synchronization":
    *
    *    Write synchronization is a special case of end-of-pipe
    *    synchronization that requires that the render cache and/or depth
    *    related caches are flushed to memory, where the data will become
    *    globally visible. This type of synchronization is required prior to
    *    SW (CPU) actually reading the result data from memory, or initiating
    *    an operation that will use as a read surface (such as a texture
    *    surface) a previous render target and/or depth/stencil buffer
    *
    *
    * From Haswell PRM, volume 2, part 1, "End-of-Pipe Synchronization":
    *
    *    Exercising the write cache flush bits (Render Target Cache Flush
    *    Enable, Depth Cache Flush Enable, DC Flush) in PIPE_CONTROL only
    *    ensures the write caches are flushed and doesn't guarantee the data
    *    is globally visible.
    *
    *    SW can track the completion of the end-of-pipe-synchronization by
    *    using "Notify Enable" and "PostSync Operation - Write Immediate
    *    Data" in the PIPE_CONTROL command.
    *
    * In other words, flushes are pipelined while invalidations are handled
    * immediately.  Therefore, if we're flushing anything then we need to
    * schedule an end-of-pipe sync before any invalidations can happen.
    */
   if (bits & ANV_PIPE_FLUSH_BITS)
      bits |= ANV_PIPE_NEEDS_END_OF_PIPE_SYNC_BIT;

   /* From Bspec 43904 (Register_CCSAuxiliaryTableInvalidate):
    * RCS engine idle sequence:
    *
    *    Gfx12+:
    *       PIPE_CONTROL:- DC Flush + L3 Fabric Flush + CS Stall + Render
    *                      Target Cache Flush + Depth Cache
    *
    *    Gfx125+:
    *       PIPE_CONTROL:- DC Flush + L3 Fabric Flush + CS Stall + Render
    *                      Target Cache Flush + Depth Cache + CCS flush
    *
    * Compute engine idle sequence:
    *
    *    Gfx12+:
    *       PIPE_CONTROL:- DC Flush + L3 Fabric Flush + CS Stall
    *
    *    Gfx125+:
    *       PIPE_CONTROL:- DC Flush + L3 Fabric Flush + CS Stall + CCS flush
    */
   if (GFX_VER == 12 && (bits & ANV_PIPE_AUX_TABLE_INVALIDATE_BIT)) {
      if (current_pipeline == GPGPU) {
         bits |=  (ANV_PIPE_DATA_CACHE_FLUSH_BIT |
                   ANV_PIPE_L3_FABRIC_FLUSH_BIT |
                   ANV_PIPE_CS_STALL_BIT |
                   (GFX_VERx10 == 125 ? ANV_PIPE_CCS_CACHE_FLUSH_BIT: 0));
      } else if (current_pipeline == _3D) {
         bits |= (ANV_PIPE_DATA_CACHE_FLUSH_BIT |
                  ANV_PIPE_L3_FABRIC_FLUSH_BIT |
                  ANV_PIPE_CS_STALL_BIT |
                  ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
                  ANV_PIPE_DEPTH_CACHE_FLUSH_BIT |
                  (GFX_VERx10 == 125 ? ANV_PIPE_CCS_CACHE_FLUSH_BIT: 0));
      }
   }

   /* If we're going to do an invalidate and we have a pending end-of-pipe
    * sync that has yet to be resolved, we do the end-of-pipe sync now.
    */
   if ((bits & ANV_PIPE_INVALIDATE_BITS) &&
       (bits & ANV_PIPE_NEEDS_END_OF_PIPE_SYNC_BIT)) {
      bits |= ANV_PIPE_END_OF_PIPE_SYNC_BIT;
      bits &= ~ANV_PIPE_NEEDS_END_OF_PIPE_SYNC_BIT;

      if (INTEL_DEBUG(DEBUG_PIPE_CONTROL) && bits) {
         fputs("acc: add ", stdout);
         anv_dump_pipe_bits(ANV_PIPE_END_OF_PIPE_SYNC_BIT, stdout);
         fprintf(stdout, "reason: Ensure flushes done before invalidate\n");
      }
   }

   /* Project: SKL / Argument: LRI Post Sync Operation [23]
    *
    * "PIPECONTROL command with “Command Streamer Stall Enable” must be
    *  programmed prior to programming a PIPECONTROL command with "LRI
    *  Post Sync Operation" in GPGPU mode of operation (i.e when
    *  PIPELINE_SELECT command is set to GPGPU mode of operation)."
    *
    * The same text exists a few rows below for Post Sync Op.
    */
   if (bits & ANV_PIPE_POST_SYNC_BIT) {
      if (GFX_VER == 9 && current_pipeline == GPGPU)
         bits |= ANV_PIPE_CS_STALL_BIT;
      bits &= ~ANV_PIPE_POST_SYNC_BIT;
   }

   if (bits & (ANV_PIPE_FLUSH_BITS | ANV_PIPE_STALL_BITS |
               ANV_PIPE_END_OF_PIPE_SYNC_BIT)) {
      enum anv_pipe_bits flush_bits =
         bits & (ANV_PIPE_FLUSH_BITS | ANV_PIPE_STALL_BITS |
                 ANV_PIPE_END_OF_PIPE_SYNC_BIT);

      uint32_t sync_op = NoWrite;
      struct anv_address addr = ANV_NULL_ADDRESS;

      /* From Sandybridge PRM, volume 2, "1.7.3.1 Writing a Value to Memory":
       *
       *    "The most common action to perform upon reaching a
       *    synchronization point is to write a value out to memory. An
       *    immediate value (included with the synchronization command) may
       *    be written."
       *
       *
       * From Broadwell PRM, volume 7, "End-of-Pipe Synchronization":
       *
       *    "In case the data flushed out by the render engine is to be
       *    read back in to the render engine in coherent manner, then the
       *    render engine has to wait for the fence completion before
       *    accessing the flushed data. This can be achieved by following
       *    means on various products: PIPE_CONTROL command with CS Stall
       *    and the required write caches flushed with Post-Sync-Operation
       *    as Write Immediate Data.
       *
       *    Example:
       *       - Workload-1 (3D/GPGPU/MEDIA)
       *       - PIPE_CONTROL (CS Stall, Post-Sync-Operation Write
       *         Immediate Data, Required Write Cache Flush bits set)
       *       - Workload-2 (Can use the data produce or output by
       *         Workload-1)
       */
      if (flush_bits & ANV_PIPE_END_OF_PIPE_SYNC_BIT) {
         flush_bits |= ANV_PIPE_CS_STALL_BIT;
         sync_op = WriteImmediateData;
         addr = device->workaround_address;
      }

      /* Flush PC. */
      genx_batch_emit_pipe_control_write(batch, device->info, current_pipeline,
                                         sync_op, addr, 0, flush_bits);

      /* If the caller wants to know what flushes have been emitted,
       * provide the bits based off the PIPE_CONTROL programmed bits.
       */
      if (emitted_flush_bits != NULL)
         *emitted_flush_bits = flush_bits;

      bits &= ~(ANV_PIPE_FLUSH_BITS | ANV_PIPE_STALL_BITS |
                ANV_PIPE_END_OF_PIPE_SYNC_BIT);
   }

   if (bits & ANV_PIPE_INVALIDATE_BITS) {
      uint32_t sync_op = NoWrite;
      struct anv_address addr = ANV_NULL_ADDRESS;

      /* From the SKL PRM, Vol. 2a, "PIPE_CONTROL",
       *
       *    "When VF Cache Invalidate is set “Post Sync Operation” must be
       *    enabled to “Write Immediate Data” or “Write PS Depth Count” or
       *    “Write Timestamp”.
       */
      if (GFX_VER == 9 && (bits & ANV_PIPE_VF_CACHE_INVALIDATE_BIT)) {
         sync_op = WriteImmediateData;
         addr = device->workaround_address;
      }

      /* Invalidate PC. */
      genx_batch_emit_pipe_control_write(batch, device->info, current_pipeline,
                                         sync_op, addr, 0, bits);

      genX(invalidate_aux_map)(batch, device, batch->engine_class, bits);

      bits &= ~ANV_PIPE_INVALIDATE_BITS;
   }

#if GFX_VER >= 12
   bits |= defer_bits;
#endif

   return bits;
}

ALWAYS_INLINE void
genX(cmd_buffer_apply_pipe_flushes)(struct anv_cmd_buffer *cmd_buffer)
{
#if INTEL_NEEDS_WA_1508744258
   /* If we're changing the state of the RHWO optimization, we need to have
    * sb_stall+cs_stall.
    */
   const bool rhwo_opt_change =
      cmd_buffer->state.rhwo_optimization_enabled !=
      cmd_buffer->state.pending_rhwo_optimization_enabled;
   if (rhwo_opt_change) {
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_STALL_AT_SCOREBOARD_BIT |
                                ANV_PIPE_END_OF_PIPE_SYNC_BIT,
                                "change RHWO optimization");
   }
#endif

   enum anv_pipe_bits bits = cmd_buffer->state.pending_pipe_bits;

   if (unlikely(cmd_buffer->device->physical->always_flush_cache))
      bits |= ANV_PIPE_FLUSH_BITS | ANV_PIPE_INVALIDATE_BITS;
   else if (bits == 0)
      return;

   if (anv_cmd_buffer_is_blitter_queue(cmd_buffer) ||
       anv_cmd_buffer_is_video_queue(cmd_buffer)) {
      if (bits & ANV_PIPE_INVALIDATE_BITS) {
         if (bits & ANV_PIPE_AUX_TABLE_INVALIDATE_BIT) {
            if (anv_cmd_buffer_is_video_queue(cmd_buffer) || GFX_VERx10 == 125) {
               /* Wa_16018063123 - emit fast color dummy blit before MI_FLUSH_DW. */
               if (INTEL_WA_16018063123_GFX_VER &&
                   anv_cmd_buffer_is_blitter_queue(cmd_buffer))
                  genX(batch_emit_fast_color_dummy_blit)(&cmd_buffer->batch, cmd_buffer->device);

               anv_batch_emit(&cmd_buffer->batch, GENX(MI_FLUSH_DW), mi_flush);
            }

            genX(invalidate_aux_map)(&cmd_buffer->batch, cmd_buffer->device,
                                     cmd_buffer->queue_family->engine_class, bits);
         }
         bits &= ~ANV_PIPE_INVALIDATE_BITS;
      }
      cmd_buffer->state.pending_pipe_bits = bits;
      return;
   }

   if (GFX_VER == 9 &&
       (bits & ANV_PIPE_CS_STALL_BIT) &&
       (bits & ANV_PIPE_VF_CACHE_INVALIDATE_BIT)) {
      /* If we are doing a VF cache invalidate AND a CS stall (it must be
       * both) then we can reset our vertex cache tracking.
       */
      memset(cmd_buffer->state.gfx.vb_dirty_ranges, 0,
             sizeof(cmd_buffer->state.gfx.vb_dirty_ranges));
      memset(&cmd_buffer->state.gfx.ib_dirty_range, 0,
             sizeof(cmd_buffer->state.gfx.ib_dirty_range));
   }

   enum anv_pipe_bits emitted_bits = 0;
   cmd_buffer->state.pending_pipe_bits =
      genX(emit_apply_pipe_flushes)(&cmd_buffer->batch,
                                    cmd_buffer->device,
                                    cmd_buffer->state.current_pipeline,
                                    bits,
                                    &emitted_bits);
   anv_cmd_buffer_update_pending_query_bits(cmd_buffer, emitted_bits);

#if INTEL_NEEDS_WA_1508744258
   if (rhwo_opt_change) {
      anv_batch_write_reg(&cmd_buffer->batch, GENX(COMMON_SLICE_CHICKEN1), c1) {
         c1.RCCRHWOOptimizationDisable =
            !cmd_buffer->state.pending_rhwo_optimization_enabled;
         c1.RCCRHWOOptimizationDisableMask = true;
      }
      cmd_buffer->state.rhwo_optimization_enabled =
         cmd_buffer->state.pending_rhwo_optimization_enabled;
   }
#endif

}

static inline struct anv_state
emit_dynamic_buffer_binding_table_entry(struct anv_cmd_buffer *cmd_buffer,
                                        struct anv_cmd_pipeline_state *pipe_state,
                                        const struct anv_pipeline_binding *binding,
                                        const struct anv_descriptor *desc)
{
   if (!desc->buffer)
      return anv_null_surface_state_for_binding_table(cmd_buffer->device);

   /* Compute the offset within the buffer */
   uint32_t dynamic_offset =
      pipe_state->dynamic_offsets[
         binding->set].offsets[binding->dynamic_offset_index];
   uint64_t offset = desc->offset + dynamic_offset;
   /* Clamp to the buffer size */
   offset = MIN2(offset, desc->buffer->vk.size);
   /* Clamp the range to the buffer size */
   uint32_t range = MIN2(desc->range, desc->buffer->vk.size - offset);

   /* Align the range to the reported bounds checking alignment
    * VkPhysicalDeviceRobustness2PropertiesEXT::robustUniformBufferAccessSizeAlignment
    */
   if (desc->type == VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC)
      range = align(range, ANV_UBO_ALIGNMENT);

   struct anv_address address =
      anv_address_add(desc->buffer->address, offset);

   struct anv_state surface_state =
      anv_cmd_buffer_alloc_surface_states(cmd_buffer, 1);
   if (surface_state.map == NULL)
      return ANV_STATE_NULL;

   enum isl_format format =
      anv_isl_format_for_descriptor_type(cmd_buffer->device,
                                         desc->type);

   isl_surf_usage_flags_t usage =
      anv_isl_usage_for_descriptor_type(desc->type);

   anv_fill_buffer_surface_state(cmd_buffer->device,
                                 surface_state.map,
                                 format, ISL_SWIZZLE_IDENTITY,
                                 usage, address, range, 1);

   return surface_state;
}

static uint32_t
emit_indirect_descriptor_binding_table_entry(struct anv_cmd_buffer *cmd_buffer,
                                             struct anv_cmd_pipeline_state *pipe_state,
                                             const struct anv_pipeline_binding *binding,
                                             const struct anv_descriptor *desc)
{
   struct anv_device *device = cmd_buffer->device;
   struct anv_state surface_state;

   /* Relative offset in the STATE_BASE_ADDRESS::SurfaceStateBaseAddress heap.
    * Depending on where the descriptor surface state is allocated, they can
    * either come from device->internal_surface_state_pool or
    * device->bindless_surface_state_pool.
    */
   switch (desc->type) {
   case VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER:
   case VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE:
   case VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT: {
      if (desc->image_view) {
         const struct anv_surface_state *sstate =
            anv_image_view_texture_surface_state(desc->image_view,
                                                 binding->plane,
                                                 desc->layout);
         surface_state = desc->image_view->use_surface_state_stream ?
            sstate->state :
            anv_bindless_state_for_binding_table(device, sstate->state);
         assert(surface_state.alloc_size);
      } else {
         surface_state = anv_null_surface_state_for_binding_table(device);
      }
      break;
   }

   case VK_DESCRIPTOR_TYPE_STORAGE_IMAGE: {
      if (desc->image_view) {
         const struct anv_surface_state *sstate =
            anv_image_view_storage_surface_state(desc->image_view);
         surface_state = desc->image_view->use_surface_state_stream ?
            sstate->state :
            anv_bindless_state_for_binding_table(device, sstate->state);
         assert(surface_state.alloc_size);
      } else {
         surface_state =
            anv_null_surface_state_for_binding_table(device);
      }
      break;
   }

   case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER:
   case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER:
      if (desc->set_buffer_view) {
         surface_state = desc->set_buffer_view->general.state;
         assert(surface_state.alloc_size);
      } else {
         surface_state = anv_null_surface_state_for_binding_table(device);
      }
      break;

   case VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER:
      if (desc->buffer_view) {
         surface_state = anv_bindless_state_for_binding_table(
            device,
            desc->buffer_view->general.state);
         assert(surface_state.alloc_size);
      } else {
         surface_state = anv_null_surface_state_for_binding_table(device);
      }
      break;

   case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC:
   case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC:
      surface_state =
         emit_dynamic_buffer_binding_table_entry(cmd_buffer, pipe_state,
                                                 binding, desc);
      break;

   case VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER:
      if (desc->buffer_view) {
         surface_state = anv_bindless_state_for_binding_table(
            device, desc->buffer_view->storage.state);
         assert(surface_state.alloc_size);
      } else {
         surface_state = anv_null_surface_state_for_binding_table(device);
      }
      break;

   default:
      UNREACHABLE("Invalid descriptor type");
   }

   return surface_state.offset;
}

static uint32_t
emit_direct_descriptor_binding_table_entry(struct anv_cmd_buffer *cmd_buffer,
                                           struct anv_cmd_pipeline_state *pipe_state,
                                           const struct anv_descriptor_set *set,
                                           const struct anv_pipeline_binding *binding,
                                           const struct anv_descriptor *desc)
{
   uint32_t desc_offset;

   /* Relative offset in the STATE_BASE_ADDRESS::SurfaceStateBaseAddress heap.
    * Depending on where the descriptor surface state is allocated, they can
    * either come from device->internal_surface_state_pool or
    * device->bindless_surface_state_pool.
    */
   switch (desc->type) {
   case VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER:
   case VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE:
   case VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT:
   case VK_DESCRIPTOR_TYPE_STORAGE_IMAGE:
   case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER:
   case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER:
   case VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER:
   case VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER:
      desc_offset = set->desc_offset + binding->set_offset;
      break;

   case VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC:
   case VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC: {
      struct anv_state state =
         emit_dynamic_buffer_binding_table_entry(cmd_buffer, pipe_state,
                                                 binding, desc);
      desc_offset = state.offset;
      break;
   }

   default:
      UNREACHABLE("Invalid descriptor type");
   }

   return desc_offset;
}

static VkResult
emit_binding_table(struct anv_cmd_buffer *cmd_buffer,
                   struct anv_cmd_pipeline_state *pipe_state,
                   const struct anv_shader_bin *shader,
                   struct anv_state *bt_state)
{
   uint32_t state_offset;

   const struct anv_pipeline_bind_map *map = &shader->bind_map;
   if (map->surface_count == 0) {
      *bt_state = (struct anv_state) { 0, };
      return VK_SUCCESS;
   }

   *bt_state = anv_cmd_buffer_alloc_binding_table(cmd_buffer,
                                                  map->surface_count,
                                                  &state_offset);
   uint32_t *bt_map = bt_state->map;

   if (bt_state->map == NULL)
      return VK_ERROR_OUT_OF_DEVICE_MEMORY;

   for (uint32_t s = 0; s < map->surface_count; s++) {
      const struct anv_pipeline_binding *binding =
         &map->surface_to_descriptor[s];

      struct anv_state surface_state;

      switch (binding->set) {
      case ANV_DESCRIPTOR_SET_NULL:
         bt_map[s] = 0;
         break;

      case ANV_DESCRIPTOR_SET_COLOR_ATTACHMENTS:
         /* Color attachment binding */
         assert(shader->stage == MESA_SHADER_FRAGMENT);
         uint32_t index = binding->index < MAX_RTS ?
            cmd_buffer->state.gfx.color_output_mapping[binding->index] :
            binding->index;
         if (index < cmd_buffer->state.gfx.color_att_count) {
            assert(index < MAX_RTS);
            const struct anv_attachment *att =
               &cmd_buffer->state.gfx.color_att[index];
            surface_state = att->surface_state.state;
         } else {
            surface_state = cmd_buffer->state.gfx.null_surface_state;
         }
         assert(surface_state.map);
         bt_map[s] = surface_state.offset + state_offset;
         break;

      case ANV_DESCRIPTOR_SET_DESCRIPTORS: {
         struct anv_descriptor_set *set =
            pipe_state->descriptors[binding->index];

         /* If the shader doesn't access the set buffer, just put the null
          * surface.
          */
         if (set->is_push && shader->push_desc_info.push_set_buffer == 0) {
            bt_map[s] = 0;
            break;
         }

         /* This is a descriptor set buffer so the set index is actually
          * given by binding->binding.  (Yes, that's confusing.)
          */
         assert(set->desc_surface_mem.alloc_size);
         assert(set->desc_surface_state.alloc_size);
         bt_map[s] = set->desc_surface_state.offset + state_offset;
         add_surface_reloc(cmd_buffer, anv_descriptor_set_address(set));
         break;
      }

      case ANV_DESCRIPTOR_SET_DESCRIPTORS_BUFFER: {
         assert(pipe_state->descriptor_buffers[binding->index].state.alloc_size);
         bt_map[s] = pipe_state->descriptor_buffers[binding->index].state.offset +
                     state_offset;
         break;
      }

      default: {
         assert(binding->set < MAX_SETS);
         struct anv_descriptor_set *set =
            pipe_state->descriptors[binding->set];

         if (binding->index >= set->descriptor_count) {
            /* From the Vulkan spec section entitled "DescriptorSet and
             * Binding Assignment":
             *
             *    "If the array is runtime-sized, then array elements greater
             *    than or equal to the size of that binding in the bound
             *    descriptor set must not be used."
             *
             * Unfortunately, the compiler isn't smart enough to figure out
             * when a dynamic binding isn't used so it may grab the whole
             * array and stick it in the binding table.  In this case, it's
             * safe to just skip those bindings that are OOB.
             */
            assert(binding->index < set->layout->descriptor_count);
            continue;
         }

         /* For push descriptor, if the binding is fully promoted to push
          * constants, just reference the null surface in the binding table.
          * It's unused and we didn't allocate/pack a surface state for it .
          */
         if (set->is_push) {
            uint32_t desc_idx = set->layout->binding[binding->binding].descriptor_index;
            assert(desc_idx < MAX_PUSH_DESCRIPTORS);

            if (shader->push_desc_info.fully_promoted_ubo_descriptors & BITFIELD_BIT(desc_idx)) {
               surface_state =
                  anv_null_surface_state_for_binding_table(cmd_buffer->device);
               break;
            }
         }

         const struct anv_descriptor *desc = &set->descriptors[binding->index];
         if (desc->type == VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR ||
             desc->type == VK_DESCRIPTOR_TYPE_SAMPLER) {
            /* Nothing for us to do here */
            continue;
         }

         uint32_t surface_state_offset;
         if (map->layout_type == ANV_PIPELINE_DESCRIPTOR_SET_LAYOUT_TYPE_INDIRECT) {
            surface_state_offset =
               emit_indirect_descriptor_binding_table_entry(cmd_buffer,
                                                            pipe_state,
                                                            binding, desc);
         } else {
            assert(map->layout_type == ANV_PIPELINE_DESCRIPTOR_SET_LAYOUT_TYPE_DIRECT ||
                   map->layout_type == ANV_PIPELINE_DESCRIPTOR_SET_LAYOUT_TYPE_BUFFER);
            surface_state_offset =
               emit_direct_descriptor_binding_table_entry(cmd_buffer, pipe_state,
                                                          set, binding, desc);
         }

         bt_map[s] = surface_state_offset + state_offset;
         break;
      }
      }
   }

   return VK_SUCCESS;
}

static VkResult
emit_samplers(struct anv_cmd_buffer *cmd_buffer,
              struct anv_cmd_pipeline_state *pipe_state,
              const struct anv_shader_bin *shader,
              struct anv_state *state)
{
   const struct anv_pipeline_bind_map *map = &shader->bind_map;
   if (map->sampler_count == 0) {
      *state = (struct anv_state) { 0, };
      return VK_SUCCESS;
   }

   uint32_t size = map->sampler_count * 16;
   *state = anv_cmd_buffer_alloc_dynamic_state(cmd_buffer, size, 32);

   if (state->map == NULL)
      return VK_ERROR_OUT_OF_DEVICE_MEMORY;

   for (uint32_t s = 0; s < map->sampler_count; s++) {
      const struct anv_pipeline_binding *binding =
         &map->sampler_to_descriptor[s];
      const struct anv_descriptor *desc =
         &pipe_state->descriptors[binding->set]->descriptors[binding->index];

      if (desc->type != VK_DESCRIPTOR_TYPE_SAMPLER &&
          desc->type != VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
         continue;

      struct anv_sampler *sampler = desc->sampler;

      /* This can happen if we have an unfilled slot since TYPE_SAMPLER
       * happens to be zero.
       */
      if (sampler == NULL)
         continue;

      memcpy(state->map + (s * 16), sampler->state[binding->plane],
             sizeof(sampler->state[0]));
   }

   return VK_SUCCESS;
}

uint32_t
genX(cmd_buffer_flush_descriptor_sets)(struct anv_cmd_buffer *cmd_buffer,
                                       struct anv_cmd_pipeline_state *pipe_state,
                                       const VkShaderStageFlags dirty,
                                       const struct anv_shader_bin **shaders,
                                       uint32_t num_shaders)
{
   VkShaderStageFlags flushed = 0;

   VkResult result = VK_SUCCESS;
   for (uint32_t i = 0; i < num_shaders; i++) {
      if (!shaders[i])
         continue;

      gl_shader_stage stage = shaders[i]->stage;
      VkShaderStageFlags vk_stage = mesa_to_vk_shader_stage(stage);
      if ((vk_stage & dirty) == 0)
         continue;

      assert(stage < ARRAY_SIZE(cmd_buffer->state.samplers));
      result = emit_samplers(cmd_buffer, pipe_state, shaders[i],
                             &cmd_buffer->state.samplers[stage]);
      if (result != VK_SUCCESS)
         break;

      assert(stage < ARRAY_SIZE(cmd_buffer->state.binding_tables));
      result = emit_binding_table(cmd_buffer, pipe_state, shaders[i],
                                  &cmd_buffer->state.binding_tables[stage]);
      if (result != VK_SUCCESS)
         break;

      flushed |= vk_stage;
   }

   if (result != VK_SUCCESS) {
      assert(result == VK_ERROR_OUT_OF_DEVICE_MEMORY);

      result = anv_cmd_buffer_new_binding_table_block(cmd_buffer);
      if (result != VK_SUCCESS)
         return 0;

      /* Re-emit the BT base address so we get the new surface state base
       * address before we start emitting binding tables etc.
       */
      genX(cmd_buffer_emit_bt_pool_base_address)(cmd_buffer);

      /* Re-emit all active binding tables */
      flushed = 0;

      for (uint32_t i = 0; i < num_shaders; i++) {
         if (!shaders[i])
            continue;

         gl_shader_stage stage = shaders[i]->stage;

         result = emit_samplers(cmd_buffer, pipe_state, shaders[i],
                                &cmd_buffer->state.samplers[stage]);
         if (result != VK_SUCCESS) {
            anv_batch_set_error(&cmd_buffer->batch, result);
            return 0;
         }
         result = emit_binding_table(cmd_buffer, pipe_state, shaders[i],
                                     &cmd_buffer->state.binding_tables[stage]);
         if (result != VK_SUCCESS) {
            anv_batch_set_error(&cmd_buffer->batch, result);
            return 0;
         }

         flushed |= mesa_to_vk_shader_stage(stage);
      }
   }

   return flushed;
}

/* This function generates the surface state used to read the content of the
 * descriptor buffer.
 */
void
genX(cmd_buffer_emit_push_descriptor_buffer_surface)(struct anv_cmd_buffer *cmd_buffer,
                                                     struct anv_descriptor_set *set)
{
   assert(set->desc_surface_state.map == NULL);

   struct anv_descriptor_set_layout *layout = set->layout;
   enum isl_format format =
      anv_isl_format_for_descriptor_type(cmd_buffer->device,
                                         VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER);

   set->desc_surface_state =
      anv_cmd_buffer_alloc_surface_states(cmd_buffer, 1);
   if (set->desc_surface_state.map == NULL)
      return;
   anv_fill_buffer_surface_state(cmd_buffer->device,
                                 set->desc_surface_state.map,
                                 format, ISL_SWIZZLE_IDENTITY,
                                 ISL_SURF_USAGE_CONSTANT_BUFFER_BIT,
                                 set->desc_surface_addr,
                                 layout->descriptor_buffer_surface_size, 1);
}

/* This functions generates surface states used by a pipeline for push
 * descriptors. This is delayed to the draw/dispatch time to avoid allocation
 * and surface state generation when a pipeline is not going to use the
 * binding table to access any push descriptor data.
 */
void
genX(cmd_buffer_emit_push_descriptor_surfaces)(struct anv_cmd_buffer *cmd_buffer,
                                               struct anv_descriptor_set *set)
{
   while (set->generate_surface_states) {
      int desc_idx = u_bit_scan(&set->generate_surface_states);
      struct anv_descriptor *desc = &set->descriptors[desc_idx];
      struct anv_buffer_view *bview = desc->set_buffer_view;

      if (bview != NULL && bview->general.state.map == NULL) {
         bview->general.state =
            anv_cmd_buffer_alloc_surface_states(cmd_buffer, 1);
         if (bview->general.state.map == NULL)
            return;
         anv_descriptor_write_surface_state(cmd_buffer->device, desc,
                                            bview->general.state);
      }
   }
}

static void
emit_pipe_control(struct anv_batch *batch,
                  const struct intel_device_info *devinfo,
                  uint32_t current_pipeline,
                  uint32_t post_sync_op,
                  struct anv_address address,
                  uint32_t imm_data,
                  enum anv_pipe_bits bits,
                  const char *reason)
{
   if ((batch->engine_class == INTEL_ENGINE_CLASS_COPY) ||
       (batch->engine_class == INTEL_ENGINE_CLASS_VIDEO))
      UNREACHABLE("Trying to emit unsupported PIPE_CONTROL command.");

   const bool trace_flush =
      (bits & (ANV_PIPE_FLUSH_BITS |
               ANV_PIPE_STALL_BITS |
               ANV_PIPE_INVALIDATE_BITS |
               ANV_PIPE_END_OF_PIPE_SYNC_BIT)) != 0;
   if (trace_flush && batch->trace != NULL) {
      // Store pipe control reasons if there is enough space
      if (batch->pc_reasons_count < ARRAY_SIZE(batch->pc_reasons)) {
         batch->pc_reasons[batch->pc_reasons_count++] = reason;
      }
      trace_intel_begin_stall(batch->trace);
   }


   /* XXX - insert all workarounds and GFX specific things below. */

#if INTEL_WA_1607156449_GFX_VER || INTEL_NEEDS_WA_18040903259
   /* Wa_1607156449: For COMPUTE Workload - Any PIPE_CONTROL command with
    * POST_SYNC Operation Enabled MUST be preceded by a PIPE_CONTROL with
    * CS_STALL Bit set (with No POST_SYNC ENABLED)
    *
    * Wa_18040903259 says that timestamp are incorrect (not doing the CS Stall
    * prior to writing the timestamp) with a command like this:
    *
    *   PIPE_CONTROL(CS Stall, Post Sync = Timestamp)
    *
    * should be turned into :
    *
    *   PIPE_CONTROL(CS Stall)
    *   PIPE_CONTROL(CS Stall, Post Sync = Timestamp)
    *
    * Also : "This WA needs to be applied only when we have done a Compute
    *         Walker and there is a request for a Timestamp."
    *
    * At the moment it's unclear whether all other parameters should go in the
    * first or second PIPE_CONTROL. It seems logical that it should go to the
    * first so that the timestamp accounts for all the associated flushes.
    */
   if ((intel_needs_workaround(devinfo, 1607156449) ||
        intel_needs_workaround(devinfo, 18040903259)) &&
       current_pipeline == GPGPU &&
       post_sync_op != NoWrite) {
      emit_pipe_control(batch, devinfo, current_pipeline,
                        NoWrite, ANV_NULL_ADDRESS, 0,
                        bits, "Wa_18040903259/Wa_18040903259");
      bits = ANV_PIPE_CS_STALL_BIT;
   }
#endif

   /* SKL PRMs, Volume 7: 3D-Media-GPGPU, Programming Restrictions for
    * PIPE_CONTROL, Flush Types:
    *   "Requires stall bit ([20] of DW) set for all GPGPU Workloads."
    * For newer platforms this is documented in the PIPE_CONTROL instruction
    * page.
    */
   if (current_pipeline == GPGPU &&
       (bits & ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT))
      bits |= ANV_PIPE_CS_STALL_BIT;

#if INTEL_NEEDS_WA_1409600907
   /* Wa_1409600907: "PIPE_CONTROL with Depth Stall Enable bit must
    * be set with any PIPE_CONTROL with Depth Flush Enable bit set.
    */
   if (bits & ANV_PIPE_DEPTH_CACHE_FLUSH_BIT)
      bits |= ANV_PIPE_DEPTH_STALL_BIT;
#endif

#if GFX_VERx10 >= 125
   if (current_pipeline != GPGPU) {
      if (bits & ANV_PIPE_HDC_PIPELINE_FLUSH_BIT)
         bits |= ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
   } else {
      if (bits & (ANV_PIPE_HDC_PIPELINE_FLUSH_BIT |
                  ANV_PIPE_DATA_CACHE_FLUSH_BIT))
         bits |= ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
   }

   /* BSpec 47112: PIPE_CONTROL::Untyped Data-Port Cache Flush:
    *
    *    "'HDC Pipeline Flush' bit must be set for this bit to take
    *     effect."
    */
   if (bits & ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT)
      bits |= ANV_PIPE_HDC_PIPELINE_FLUSH_BIT;
#endif

#if GFX_VER < 12
   if (bits & ANV_PIPE_HDC_PIPELINE_FLUSH_BIT)
      bits |= ANV_PIPE_DATA_CACHE_FLUSH_BIT;
#endif

   /* From the SKL PRM, Vol. 2a, "PIPE_CONTROL",
    *
    *    "If the VF Cache Invalidation Enable is set to a 1 in a
    *    PIPE_CONTROL, a separate Null PIPE_CONTROL, all bitfields sets to
    *    0, with the VF Cache Invalidation Enable set to 0 needs to be sent
    *    prior to the PIPE_CONTROL with VF Cache Invalidation Enable set to
    *    a 1."
    *
    * This appears to hang Broadwell, so we restrict it to just gfx9.
    */
   if (GFX_VER == 9 && (bits & ANV_PIPE_VF_CACHE_INVALIDATE_BIT))
      anv_batch_emit(batch, GENX(PIPE_CONTROL), pipe);

   anv_batch_emit(batch, GENX(PIPE_CONTROL), pipe) {
#if GFX_VERx10 >= 125
      pipe.UntypedDataPortCacheFlushEnable =
         bits & ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
      pipe.CCSFlushEnable = bits & ANV_PIPE_CCS_CACHE_FLUSH_BIT;
#endif
#if GFX_VER == 12
      pipe.TileCacheFlushEnable = bits & ANV_PIPE_TILE_CACHE_FLUSH_BIT;
      pipe.L3FabricFlush = bits & ANV_PIPE_L3_FABRIC_FLUSH_BIT;
#endif
#if GFX_VER > 11
      pipe.HDCPipelineFlushEnable = bits & ANV_PIPE_HDC_PIPELINE_FLUSH_BIT;
#endif
      pipe.DepthCacheFlushEnable = bits & ANV_PIPE_DEPTH_CACHE_FLUSH_BIT;
      pipe.DCFlushEnable = bits & ANV_PIPE_DATA_CACHE_FLUSH_BIT;
      pipe.RenderTargetCacheFlushEnable =
         bits & ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT;

      pipe.DepthStallEnable = bits & ANV_PIPE_DEPTH_STALL_BIT;

      pipe.TLBInvalidate = bits & ANV_PIPE_TLB_INVALIDATE_BIT;

#if GFX_VERx10 >= 125
      pipe.PSSStallSyncEnable = bits & ANV_PIPE_PSS_STALL_SYNC_BIT;
#endif
      pipe.CommandStreamerStallEnable = bits & ANV_PIPE_CS_STALL_BIT;
      pipe.StallAtPixelScoreboard = bits & ANV_PIPE_STALL_AT_SCOREBOARD_BIT;

      pipe.StateCacheInvalidationEnable =
         bits & ANV_PIPE_STATE_CACHE_INVALIDATE_BIT;
      pipe.ConstantCacheInvalidationEnable =
         bits & ANV_PIPE_CONSTANT_CACHE_INVALIDATE_BIT;
#if GFX_VER >= 12
      /* Invalidates the L3 cache part in which index & vertex data is loaded
       * when VERTEX_BUFFER_STATE::L3BypassDisable is set.
       */
      pipe.L3ReadOnlyCacheInvalidationEnable =
         bits & ANV_PIPE_VF_CACHE_INVALIDATE_BIT;
#endif
      pipe.VFCacheInvalidationEnable =
         bits & ANV_PIPE_VF_CACHE_INVALIDATE_BIT;
      pipe.TextureCacheInvalidationEnable =
         bits & ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT;
      pipe.InstructionCacheInvalidateEnable =
         bits & ANV_PIPE_INSTRUCTION_CACHE_INVALIDATE_BIT;

      pipe.PostSyncOperation = post_sync_op;
      pipe.Address = address;
      pipe.DestinationAddressType = DAT_PPGTT;
      pipe.ImmediateData = imm_data;

      anv_debug_dump_pc(pipe, reason);
   }

   if (trace_flush && batch->trace != NULL) {
      trace_intel_end_stall(batch->trace, bits,
                            anv_pipe_flush_bit_to_ds_stall_flag,
                            batch->pc_reasons[0],
                            batch->pc_reasons[1],
                            batch->pc_reasons[2],
                            batch->pc_reasons[3]);
      batch->pc_reasons[0] = NULL;
      batch->pc_reasons[1] = NULL;
      batch->pc_reasons[2] = NULL;
      batch->pc_reasons[3] = NULL;
      batch->pc_reasons_count = 0;
   }
}

void
genX(batch_emit_pipe_control)(struct anv_batch *batch,
                              const struct intel_device_info *devinfo,
                              uint32_t current_pipeline,
                              enum anv_pipe_bits bits,
                              const char *reason)
{
   emit_pipe_control(batch, devinfo, current_pipeline,
                     NoWrite, ANV_NULL_ADDRESS, 0, bits, reason);
}

void
genX(batch_emit_pipe_control_write)(struct anv_batch *batch,
                                    const struct intel_device_info *devinfo,
                                    uint32_t current_pipeline,
                                    uint32_t post_sync_op,
                                    struct anv_address address,
                                    uint32_t imm_data,
                                    enum anv_pipe_bits bits,
                                    const char *reason)
{
   emit_pipe_control(batch, devinfo, current_pipeline,
                     post_sync_op, address, imm_data, bits, reason);
}

/* Set preemption on/off. */
void
genX(batch_set_preemption)(struct anv_batch *batch,
                           struct anv_device *device,
                           uint32_t current_pipeline,
                           bool value)
{
#if INTEL_WA_16013994831_GFX_VER
   if (!intel_needs_workaround(device->info, 16013994831))
      return;

   anv_batch_write_reg(batch, GENX(CS_CHICKEN1), cc1) {
      cc1.DisablePreemptionandHighPriorityPausingdueto3DPRIMITIVECommand = !value;
      cc1.DisablePreemptionandHighPriorityPausingdueto3DPRIMITIVECommandMask = true;
   }

   /* Wa_16013994831 - we need to insert CS_STALL and 250 noops. */
   genx_batch_emit_pipe_control(batch, device->info, current_pipeline,
                                ANV_PIPE_CS_STALL_BIT);

   for (unsigned i = 0; i < 250; i++)
      anv_batch_emit(batch, GENX(MI_NOOP), noop);
#endif
}

void
genX(cmd_buffer_set_preemption)(struct anv_cmd_buffer *cmd_buffer, bool value)
{
#if GFX_VERx10 >= 120
   if (cmd_buffer->state.gfx.object_preemption == value)
      return;

   genX(batch_set_preemption)(&cmd_buffer->batch, cmd_buffer->device,
                              cmd_buffer->state.current_pipeline,
                              value);
   cmd_buffer->state.gfx.object_preemption = value;
#endif
}

ALWAYS_INLINE static void
update_descriptor_set_surface_state(struct anv_cmd_buffer *cmd_buffer,
                                    struct anv_cmd_pipeline_state *pipe_state,
                                    uint32_t set_idx)
{
   if (!pipe_state->descriptor_buffers[set_idx].bound)
      return;

   const struct anv_physical_device *device = cmd_buffer->device->physical;
   const int32_t buffer_index =
      pipe_state->descriptor_buffers[set_idx].buffer_index;
   const struct anv_va_range *push_va_range =
      GFX_VERx10 >= 125 ?
      &device->va.push_descriptor_buffer_pool :
      &device->va.internal_surface_state_pool;
   const struct anv_va_range *va_range =
      buffer_index == -1 ? push_va_range : &device->va.dynamic_visible_pool;
   const uint64_t descriptor_set_addr =
      (buffer_index == -1 ? va_range->addr :
       cmd_buffer->state.descriptor_buffers.address[buffer_index]) +
      pipe_state->descriptor_buffers[set_idx].buffer_offset;
   const uint64_t set_size =
      MIN2(va_range->size - (descriptor_set_addr - va_range->addr),
           anv_physical_device_bindless_heap_size(device, true));

   if (descriptor_set_addr != pipe_state->descriptor_buffers[set_idx].address) {
      pipe_state->descriptor_buffers[set_idx].address = descriptor_set_addr;

      struct anv_state surface_state =
         anv_cmd_buffer_alloc_surface_states(cmd_buffer, 1);
      const enum isl_format format =
         anv_isl_format_for_descriptor_type(cmd_buffer->device,
                                            VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER);
      anv_fill_buffer_surface_state(
         cmd_buffer->device, surface_state.map,
         format, ISL_SWIZZLE_IDENTITY,
         ISL_SURF_USAGE_CONSTANT_BUFFER_BIT,
         anv_address_from_u64(pipe_state->descriptor_buffers[set_idx].address),
         set_size, 1);

      pipe_state->descriptor_buffers[set_idx].state = surface_state;
   }
}

ALWAYS_INLINE static uint32_t
compute_descriptor_set_surface_offset(const struct anv_cmd_buffer *cmd_buffer,
                                      const struct anv_cmd_pipeline_state *pipe_state,
                                      const uint32_t set_idx)
{
   const struct anv_physical_device *device = cmd_buffer->device->physical;

   if (device->uses_ex_bso) {
      int32_t buffer_index =
         pipe_state->descriptor_buffers[set_idx].buffer_index;
      uint64_t buffer_address =
         buffer_index == -1 ?
         device->va.push_descriptor_buffer_pool.addr :
         cmd_buffer->state.descriptor_buffers.address[buffer_index];

      return (buffer_address - device->va.dynamic_visible_pool.addr) +
              pipe_state->descriptor_buffers[set_idx].buffer_offset;
   }

   const uint32_t descriptor_buffer_align =
      cmd_buffer->state.descriptor_buffers.surfaces_address % 4096;

   return (descriptor_buffer_align +
           pipe_state->descriptor_buffers[set_idx].buffer_offset) << 6;
}

ALWAYS_INLINE static uint32_t
compute_descriptor_set_sampler_offset(const struct anv_cmd_buffer *cmd_buffer,
                                      const struct anv_cmd_pipeline_state *pipe_state,
                                      const uint32_t set_idx)
{
   const struct anv_physical_device *device = cmd_buffer->device->physical;
   int32_t buffer_index =
      pipe_state->descriptor_buffers[set_idx].buffer_index;
   uint64_t buffer_address =
      buffer_index == -1 ?
      device->va.push_descriptor_buffer_pool.addr :
      cmd_buffer->state.descriptor_buffers.address[buffer_index];

   return (buffer_address - device->va.dynamic_state_pool.addr) +
      pipe_state->descriptor_buffers[set_idx].buffer_offset;
}

void
genX(flush_descriptor_buffers)(struct anv_cmd_buffer *cmd_buffer,
                               struct anv_cmd_pipeline_state *pipe_state)
{
   /* On Gfx12.5+ the STATE_BASE_ADDRESS BindlessSurfaceStateBaseAddress &
    * DynamicStateBaseAddress are fixed. So as long as we stay in one
    * descriptor buffer mode, there is no need to switch.
    */
#if GFX_VERx10 >= 125
   if (cmd_buffer->state.current_db_mode !=
       cmd_buffer->state.pending_db_mode)
      genX(cmd_buffer_emit_state_base_address)(cmd_buffer);
#else
   if (cmd_buffer->state.descriptor_buffers.dirty)
      genX(cmd_buffer_emit_state_base_address)(cmd_buffer);
#endif

   assert(cmd_buffer->state.current_db_mode !=
          ANV_CMD_DESCRIPTOR_BUFFER_MODE_UNKNOWN);
   if (cmd_buffer->state.current_db_mode == ANV_CMD_DESCRIPTOR_BUFFER_MODE_BUFFER &&
       (cmd_buffer->state.descriptor_buffers.dirty ||
        (pipe_state->pipeline->active_stages &
         cmd_buffer->state.descriptor_buffers.offsets_dirty) != 0)) {
      struct anv_push_constants *push_constants =
         &pipe_state->push_constants;
      for (uint32_t i = 0; i < ARRAY_SIZE(push_constants->desc_surface_offsets); i++) {
         update_descriptor_set_surface_state(cmd_buffer, pipe_state, i);

         push_constants->desc_surface_offsets[i] =
            compute_descriptor_set_surface_offset(cmd_buffer, pipe_state, i);
         push_constants->desc_sampler_offsets[i] =
            compute_descriptor_set_sampler_offset(cmd_buffer, pipe_state, i);
      }

#if GFX_VERx10 < 125
      struct anv_device *device = cmd_buffer->device;
      push_constants->surfaces_base_offset =
         ROUND_DOWN_TO(
            cmd_buffer->state.descriptor_buffers.surfaces_address,
            4096) -
         device->physical->va.dynamic_visible_pool.addr;
#endif

      cmd_buffer->state.push_constants_dirty |=
         (cmd_buffer->state.descriptor_buffers.offsets_dirty &
          pipe_state->pipeline->active_stages);
      pipe_state->push_constants_data_dirty = true;
      cmd_buffer->state.descriptor_buffers.offsets_dirty &=
         ~pipe_state->pipeline->active_stages;
   }

   cmd_buffer->state.descriptor_buffers.dirty = false;
}

void
genX(cmd_buffer_begin_companion)(struct anv_cmd_buffer *cmd_buffer,
                                 VkCommandBufferLevel level)
{
   cmd_buffer->vk.level = level;
   cmd_buffer->is_companion_rcs_cmd_buffer = true;

   trace_intel_begin_cmd_buffer(&cmd_buffer->trace);

#if GFX_VER >= 12
   /* Reenable prefetching at the beginning of secondary command buffers. We
    * do this so that the return instruction edition is not prefetched before
    * completion.
    */
   if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_SECONDARY) {
      anv_batch_emit(&cmd_buffer->batch, GENX(MI_ARB_CHECK), arb) {
         arb.PreParserDisableMask = true;
         arb.PreParserDisable = false;
      }
   }
#endif

   /* A companion command buffer is only used for blorp commands atm, so
    * default to the legacy mode.
    */
   cmd_buffer->state.current_db_mode = ANV_CMD_DESCRIPTOR_BUFFER_MODE_LEGACY;
   genX(cmd_buffer_emit_bt_pool_base_address)(cmd_buffer);

   /* Invalidate the aux table in every primary command buffer. This ensures
    * the command buffer see the last updates made by the host.
    */
   if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_PRIMARY &&
       cmd_buffer->device->info->has_aux_map) {
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_AUX_TABLE_INVALIDATE_BIT,
                                "new cmd buffer with aux-tt");
   }
}

static bool
aux_op_resolves(enum isl_aux_op aux_op)
{
   return aux_op == ISL_AUX_OP_FULL_RESOLVE ||
          aux_op == ISL_AUX_OP_PARTIAL_RESOLVE;
}

static bool
aux_op_clears(enum isl_aux_op aux_op)
{
   return aux_op == ISL_AUX_OP_FAST_CLEAR ||
          aux_op == ISL_AUX_OP_AMBIGUATE;
}

static bool
aux_op_renders(enum isl_aux_op aux_op)
{
   return aux_op == ISL_AUX_OP_NONE;
}

static void
add_pending_pipe_bits_for_color_aux_op(struct anv_cmd_buffer *cmd_buffer,
                                       enum isl_aux_op next_aux_op,
                                       enum anv_pipe_bits pipe_bits)
{
   const enum isl_aux_op last_aux_op = cmd_buffer->state.color_aux_op;
   assert(next_aux_op != last_aux_op);

   char flush_reason[64] = {};
   if (INTEL_DEBUG(DEBUG_PIPE_CONTROL) ||
       u_trace_enabled(&cmd_buffer->device->ds.trace_context)) {
      int ret = snprintf(flush_reason, sizeof(flush_reason),
                         "color aux-op: %s -> %s",
                         isl_aux_op_to_name(last_aux_op),
                         isl_aux_op_to_name(next_aux_op));
      assert(ret < sizeof(flush_reason));
   }

   anv_add_pending_pipe_bits(cmd_buffer, pipe_bits, flush_reason);
}

void
genX(cmd_buffer_update_color_aux_op)(struct anv_cmd_buffer *cmd_buffer,
                                     enum isl_aux_op next_aux_op)
{
   const enum isl_aux_op last_aux_op = cmd_buffer->state.color_aux_op;

   if (!aux_op_clears(last_aux_op) && aux_op_clears(next_aux_op)) {
#if GFX_VER >= 20
      /* From the Xe2 Bspec 57340 (r59562),
       * "MCS/CCS Buffers, Fast Clear for Render Target(s)":
       *
       *    Synchronization:
       *    Due to interaction of scaled clearing rectangle with pixel
       *    scoreboard, we require one of the following commands to be
       *    issued. [...]
       *
       *    PIPE_CONTROL
       *    PSS Stall Sync Enable            [...] 1b (Enable)
       *       Machine-wide Stall at Pixel Stage, wait for all Prior Pixel
       *       Work to Reach End of Pipe
       *    Render Target Cache Flush Enable [...] 1b (Enable)
       *       Post-Sync Op Flushes Render Cache before Unblocking Stall
       *
       *    This synchronization step is required before and after the fast
       *    clear pass, to ensure correct ordering between pixels.
       */
      add_pending_pipe_bits_for_color_aux_op(
            cmd_buffer, next_aux_op,
            ANV_PIPE_PSS_STALL_SYNC_BIT |
            ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT);

#elif GFX_VERx10 == 125
      /* From the ACM Bspec 47704 (r52663), "Render Target Fast Clear":
       *
       *    Preamble pre fast clear synchronization
       *
       *    PIPE_CONTROL:
       *    PS sync stall = 1
       *    Tile Cache Flush = 1
       *    RT Write Flush = 1
       *    HDC Flush = 1
       *    DC Flush = 1
       *    Texture Invalidate = 1
       *
       *    [...]
       *
       *    Objective of the preamble flushes is to ensure all data is
       *    evicted from L1 caches prior to fast clear.
       */
      add_pending_pipe_bits_for_color_aux_op(
            cmd_buffer, next_aux_op,
            ANV_PIPE_PSS_STALL_SYNC_BIT |
            ANV_PIPE_TILE_CACHE_FLUSH_BIT |
            ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
            ANV_PIPE_HDC_PIPELINE_FLUSH_BIT |
            ANV_PIPE_DATA_CACHE_FLUSH_BIT |
            ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT);

#elif GFX_VERx10 == 120
      /* From the TGL Bspec 47704 (r52663), "Render Target Fast Clear":
       *
       *    Preamble pre fast clear synchronization
       *
       *    PIPE_CONTROL:
       *    Depth Stall = 1
       *    Tile Cache Flush = 1
       *    RT Write Flush = 1
       *    Texture Invalidate = 1
       *
       *    [...]
       *
       *    Objective of the preamble flushes is to ensure all data is
       *    evicted from L1 caches prior to fast clear.
       */
      add_pending_pipe_bits_for_color_aux_op(
            cmd_buffer, next_aux_op,
            ANV_PIPE_DEPTH_STALL_BIT  |
            ANV_PIPE_TILE_CACHE_FLUSH_BIT |
            ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
            ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT);

#else
      /* From the Sky Lake PRM Vol. 7, "MCS Buffer for Render Target(s)":
       *
       *    Any transition from any value in {Clear, Render, Resolve} to a
       *    different value in {Clear, Render, Resolve} requires end of pipe
       *    synchronization.
       *
       * From the Sky Lake PRM Vol. 7, "Render Target Fast Clear":
       *
       *    After Render target fast clear, pipe-control with color cache
       *    write-flush must be issued before sending any DRAW commands on
       *    that render target.
       *
       * The last comment is a bit cryptic and doesn't really tell you what's
       * going or what's really needed.  It appears that fast clear ops are
       * not properly synchronized with other drawing.  This means that we
       * cannot have a fast clear operation in the pipe at the same time as
       * other regular drawing operations.  We need to use a PIPE_CONTROL
       * to ensure that the contents of the previous draw hit the render
       * target before we resolve and then use a second PIPE_CONTROL after
       * the resolve to ensure that it is completed before any additional
       * drawing occurs.
       */
      add_pending_pipe_bits_for_color_aux_op(
            cmd_buffer, next_aux_op,
            ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
            ANV_PIPE_END_OF_PIPE_SYNC_BIT);
#endif

   } else if (aux_op_clears(last_aux_op) && !aux_op_clears(next_aux_op)) {
#if GFX_VERx10 >= 125
      /* From the ACM PRM Vol. 9, "Color Fast Clear Synchronization":
       *
       *    Postamble post fast clear synchronization
       *
       *    PIPE_CONTROL:
       *    PS sync stall = 1
       *    RT flush = 1
       */
      add_pending_pipe_bits_for_color_aux_op(
            cmd_buffer, next_aux_op,
            ANV_PIPE_PSS_STALL_SYNC_BIT |
            ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT);

#elif GFX_VERx10 == 120
      /* From the TGL PRM Vol. 9, "Color Fast Clear Synchronization":
       *
       *    Postamble post fast clear synchronization
       *
       *    PIPE_CONTROL:
       *    Depth Stall = 1
       *    Tile Cache Flush = 1
       *    RT Write Flush = 1
       *
       * From the TGL PRM Vol. 2a, "PIPE_CONTROL::L3 Fabric Flush":
       *
       *    For a sequence of color fast clears. A single PIPE_CONTROL
       *    command with Render Target Cache Flush, L3 Fabric Flush and Depth
       *    Stall set at the end of the sequence suffices.
       *
       * Replace the Tile Cache flush with an L3 fabric flush.
       */
      add_pending_pipe_bits_for_color_aux_op(
            cmd_buffer, next_aux_op,
            ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
            ANV_PIPE_L3_FABRIC_FLUSH_BIT |
            ANV_PIPE_DEPTH_STALL_BIT);

#else
      /* From the Sky Lake PRM Vol. 7, "Render Target Fast Clear":
       *
       *    After Render target fast clear, pipe-control with color cache
       *    write-flush must be issued before sending any DRAW commands on
       *    that render target.
       *
       * From the Sky Lake PRM Vol. 7, "MCS Buffer for Render Target(s)":
       *
       *    Any transition from any value in {Clear, Render, Resolve} to a
       *    different value in {Clear, Render, Resolve} requires end of pipe
       *    synchronization.
       */
      add_pending_pipe_bits_for_color_aux_op(
            cmd_buffer, next_aux_op,
            ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
            ANV_PIPE_END_OF_PIPE_SYNC_BIT);
#endif

   } else if (aux_op_renders(last_aux_op) != aux_op_renders(next_aux_op)) {
      assert(aux_op_resolves(last_aux_op) != aux_op_resolves(next_aux_op));
      /* From the Sky Lake PRM Vol. 7, "MCS Buffer for Render Target(s)":
       *
       *    Any transition from any value in {Clear, Render, Resolve} to a
       *    different value in {Clear, Render, Resolve} requires end of pipe
       *    synchronization.
       *
       * We perform a flush of the write cache before and after the clear and
       * resolve operations to meet this requirement.
       *
       * Unlike other drawing, fast clear operations are not properly
       * synchronized. The first PIPE_CONTROL here likely ensures that the
       * contents of the previous render or clear hit the render target before
       * we resolve and the second likely ensures that the resolve is complete
       * before we do any more rendering or clearing.
       */
      add_pending_pipe_bits_for_color_aux_op(
            cmd_buffer, next_aux_op,
            ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
            ANV_PIPE_END_OF_PIPE_SYNC_BIT);
   }

   if (last_aux_op != ISL_AUX_OP_FAST_CLEAR &&
       next_aux_op == ISL_AUX_OP_FAST_CLEAR &&
       cmd_buffer->device->isl_dev.ss.clear_color_state_size > 0) {
      /* From the ICL PRM Vol. 9, "State Caching":
       *
       *    Any values referenced by pointers within the RENDER_SURFACE_STATE
       *    [...] (e.g. Clear Color Pointer, [...]) are considered to be part
       *    of that state and any changes to these referenced values requires
       *    an invalidation of the L1 state cache to ensure the new values are
       *    being used as part of the state. [...]
       *
       * We could alternatively perform this invalidation when we stop
       * fast-clearing. A benefit to doing it now, when transitioning to a
       * fast clear, is that we save a pipe control by combining the state
       * cache invalidation with the texture cache invalidation done on gfx12.
       */
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_STATE_CACHE_INVALIDATE_BIT,
                                "Invalidate for new clear color");
   }

   /* Update the auxiliary surface operation, but with one exception. */
   if (last_aux_op == ISL_AUX_OP_FAST_CLEAR &&
       next_aux_op == ISL_AUX_OP_AMBIGUATE) {
      assert(aux_op_clears(last_aux_op) && aux_op_clears(next_aux_op));
      /* Fast clears and ambiguates are in the same class of operation, but
       * fast clears have more stringent synchronization requirements. For
       * better performance, don't replace the current fast clear operation
       * state with ambiguate. This allows us to perform one state cache
       * invalidation when leaving a sequence which alternates between
       * ambiguates and clears, instead of multiple such invalidations.
       */
   } else {
      cmd_buffer->state.color_aux_op = next_aux_op;
   }
}

static void
genX(cmd_buffer_set_protected_memory)(struct anv_cmd_buffer *cmd_buffer,
                                      bool enabled)
{
#if GFX_VER >= 12
   if (enabled) {
      anv_batch_emit(&cmd_buffer->batch, GENX(MI_SET_APPID), appid) {
         /* Default value for single session. */
         appid.ProtectedMemoryApplicationID = cmd_buffer->device->protected_session_id;
         appid.ProtectedMemoryApplicationIDType = DISPLAY_APP;
      }
   }
   anv_batch_emit(&cmd_buffer->batch, GENX(PIPE_CONTROL), pc) {
      pc.PipeControlFlushEnable = true;
      pc.DCFlushEnable = true;
      pc.RenderTargetCacheFlushEnable = true;
      pc.CommandStreamerStallEnable = true;
      if (enabled)
         pc.ProtectedMemoryEnable = true;
      else
         pc.ProtectedMemoryDisable = true;
   }
#else
   UNREACHABLE("Protected content not supported");
#endif
}

VkResult
genX(BeginCommandBuffer)(
    VkCommandBuffer                             commandBuffer,
    const VkCommandBufferBeginInfo*             pBeginInfo)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);
   VkResult result;

   /* If this is the first vkBeginCommandBuffer, we must *initialize* the
    * command buffer's state. Otherwise, we must *reset* its state. In both
    * cases we reset it.
    *
    * From the Vulkan 1.0 spec:
    *
    *    If a command buffer is in the executable state and the command buffer
    *    was allocated from a command pool with the
    *    VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT flag set, then
    *    vkBeginCommandBuffer implicitly resets the command buffer, behaving
    *    as if vkResetCommandBuffer had been called with
    *    VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT not set. It then puts
    *    the command buffer in the recording state.
    */
   anv_cmd_buffer_reset(&cmd_buffer->vk, 0);
   anv_cmd_buffer_reset_rendering(cmd_buffer);

   cmd_buffer->usage_flags = pBeginInfo->flags;

   /* VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT must be ignored for
    * primary level command buffers.
    *
    * From the Vulkan 1.0 spec:
    *
    *    VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT specifies that a
    *    secondary command buffer is considered to be entirely inside a render
    *    pass. If this is a primary command buffer, then this bit is ignored.
    */
   if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_PRIMARY)
      cmd_buffer->usage_flags &= ~VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT;

#if GFX_VER >= 12
   /* Reenable prefetching at the beginning of secondary command buffers. We
    * do this so that the return instruction edition is not prefetched before
    * completion.
    */
   if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_SECONDARY) {
      anv_batch_emit(&cmd_buffer->batch, GENX(MI_ARB_CHECK), arb) {
         arb.PreParserDisableMask = true;
         arb.PreParserDisable = false;
      }
   }
#endif

   /* Assume the viewport has already been set in primary command buffers. */
   if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_SECONDARY)
      cmd_buffer->state.gfx.viewport_set = true;

   trace_intel_begin_cmd_buffer(&cmd_buffer->trace);

   if (anv_cmd_buffer_is_video_queue(cmd_buffer) ||
       anv_cmd_buffer_is_blitter_queue(cmd_buffer)) {
      /* Invalidate the aux table in every primary command buffer. This
       * ensures the command buffer see the last updates made by the host.
       */
      if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_PRIMARY &&
          cmd_buffer->device->info->has_aux_map) {
         anv_add_pending_pipe_bits(cmd_buffer,
                                   ANV_PIPE_AUX_TABLE_INVALIDATE_BIT,
                                   "new cmd buffer with aux-tt");
      }
      return VK_SUCCESS;
   }

#if GFX_VER >= 12
   if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_PRIMARY &&
       cmd_buffer->vk.pool->flags & VK_COMMAND_POOL_CREATE_PROTECTED_BIT)
      genX(cmd_buffer_set_protected_memory)(cmd_buffer, true);
#endif

   if (cmd_buffer->device->vk.enabled_extensions.EXT_descriptor_buffer) {
      genX(cmd_buffer_emit_state_base_address)(cmd_buffer);
   } else {
      cmd_buffer->state.current_db_mode = ANV_CMD_DESCRIPTOR_BUFFER_MODE_LEGACY;
      genX(cmd_buffer_emit_bt_pool_base_address)(cmd_buffer);
   }

   /* We sometimes store vertex data in the dynamic state buffer for blorp
    * operations and our dynamic state stream may re-use data from previous
    * command buffers.  In order to prevent stale cache data, we flush the VF
    * cache.  We could do this on every blorp call but that's not really
    * needed as all of the data will get written by the CPU prior to the GPU
    * executing anything.  The chances are fairly high that they will use
    * blorp at least once per primary command buffer so it shouldn't be
    * wasted.
    *
    * There is also a workaround on gfx8 which requires us to invalidate the
    * VF cache occasionally.  It's easier if we can assume we start with a
    * fresh cache (See also genX(cmd_buffer_set_binding_for_gfx8_vb_flush).)
    */
   anv_add_pending_pipe_bits(cmd_buffer,
                             ANV_PIPE_VF_CACHE_INVALIDATE_BIT,
                             "new cmd buffer");

   /* Invalidate the aux table in every primary command buffer. This ensures
    * the command buffer see the last updates made by the host.
    */
   if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_PRIMARY &&
       cmd_buffer->device->info->has_aux_map) {
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_AUX_TABLE_INVALIDATE_BIT,
                                "new cmd buffer with aux-tt");
   }

   /* We send an "Indirect State Pointers Disable" packet at
    * EndCommandBuffer, so all push constant packets are ignored during a
    * context restore. Documentation says after that command, we need to
    * emit push constants again before any rendering operation. So we
    * flag them dirty here to make sure they get emitted.
    */
   cmd_buffer->state.push_constants_dirty |= VK_SHADER_STAGE_ALL_GRAPHICS;
   cmd_buffer->state.gfx.base.push_constants_data_dirty = true;

   if (cmd_buffer->usage_flags &
       VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT) {
      struct anv_cmd_graphics_state *gfx = &cmd_buffer->state.gfx;

      char gcbiar_data[VK_GCBIARR_DATA_SIZE(MAX_RTS)];
      const VkRenderingInfo *resume_info =
         vk_get_command_buffer_inheritance_as_rendering_resume(cmd_buffer->vk.level,
                                                               pBeginInfo,
                                                               gcbiar_data);
      if (resume_info != NULL) {
         genX(CmdBeginRendering)(commandBuffer, resume_info);
      } else {
         const VkCommandBufferInheritanceRenderingInfo *inheritance_info =
            vk_get_command_buffer_inheritance_rendering_info(cmd_buffer->vk.level,
                                                             pBeginInfo);
         assert(inheritance_info);

         gfx->rendering_flags = inheritance_info->flags;
         gfx->render_area = (VkRect2D) { };
         gfx->layer_count = 0;
         gfx->samples = inheritance_info->rasterizationSamples;
         gfx->view_mask = inheritance_info->viewMask;

         uint32_t color_att_count = inheritance_info->colorAttachmentCount;
         result = anv_cmd_buffer_init_attachments(cmd_buffer, color_att_count);
         if (result != VK_SUCCESS)
            return result;

         for (uint32_t i = 0; i < color_att_count; i++) {
            gfx->color_att[i].vk_format =
               inheritance_info->pColorAttachmentFormats[i];
         }
         gfx->depth_att.vk_format =
            inheritance_info->depthAttachmentFormat;
         gfx->stencil_att.vk_format =
            inheritance_info->stencilAttachmentFormat;

         anv_cmd_graphic_state_update_has_uint_rt(gfx);

         cmd_buffer->state.gfx.dirty |= ANV_CMD_DIRTY_RENDER_AREA |
                                        ANV_CMD_DIRTY_RENDER_TARGETS;
      }
   }

   /* Emit the sample pattern at the beginning of the batch because the
    * default locations emitted at the device initialization might have been
    * changed by a previous command buffer.
    *
    * Do not change that when we're continuing a previous renderpass.
    */
   if (cmd_buffer->device->vk.enabled_extensions.EXT_sample_locations &&
       !(cmd_buffer->usage_flags & VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT))
      genX(emit_sample_pattern)(&cmd_buffer->batch, NULL);

   if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_SECONDARY) {
      const VkCommandBufferInheritanceConditionalRenderingInfoEXT *conditional_rendering_info =
         vk_find_struct_const(pBeginInfo->pInheritanceInfo->pNext, COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT);

      /* If secondary buffer supports conditional rendering
       * we should emit commands as if conditional rendering is enabled.
       */
      cmd_buffer->state.conditional_render_enabled =
         conditional_rendering_info && conditional_rendering_info->conditionalRenderingEnable;

      if (pBeginInfo->pInheritanceInfo->occlusionQueryEnable) {
         cmd_buffer->state.gfx.n_occlusion_queries = 1;
         cmd_buffer->state.gfx.dirty |= ANV_CMD_DIRTY_OCCLUSION_QUERY_ACTIVE;
      }
   }

   return VK_SUCCESS;
}

/* From the PRM, Volume 2a:
 *
 *    "Indirect State Pointers Disable
 *
 *    At the completion of the post-sync operation associated with this pipe
 *    control packet, the indirect state pointers in the hardware are
 *    considered invalid; the indirect pointers are not saved in the context.
 *    If any new indirect state commands are executed in the command stream
 *    while the pipe control is pending, the new indirect state commands are
 *    preserved.
 *
 *    [DevIVB+]: Using Invalidate State Pointer (ISP) only inhibits context
 *    restoring of Push Constant (3DSTATE_CONSTANT_*) commands. Push Constant
 *    commands are only considered as Indirect State Pointers. Once ISP is
 *    issued in a context, SW must initialize by programming push constant
 *    commands for all the shaders (at least to zero length) before attempting
 *    any rendering operation for the same context."
 *
 * 3DSTATE_CONSTANT_* packets are restored during a context restore,
 * even though they point to a BO that has been already unreferenced at
 * the end of the previous batch buffer. This has been fine so far since
 * we are protected by these scratch page (every address not covered by
 * a BO should be pointing to the scratch page). But on CNL, it is
 * causing a GPU hang during context restore at the 3DSTATE_CONSTANT_*
 * instruction.
 *
 * The flag "Indirect State Pointers Disable" in PIPE_CONTROL tells the
 * hardware to ignore previous 3DSTATE_CONSTANT_* packets during a
 * context restore, so the mentioned hang doesn't happen. However,
 * software must program push constant commands for all stages prior to
 * rendering anything. So we flag them dirty in BeginCommandBuffer.
 *
 * Finally, we also make sure to stall at pixel scoreboard to make sure the
 * constants have been loaded into the EUs prior to disable the push constants
 * so that it doesn't hang a previous 3DPRIMITIVE.
 */
static void
emit_isp_disable(struct anv_cmd_buffer *cmd_buffer)
{
   genx_batch_emit_pipe_control(&cmd_buffer->batch,
                                cmd_buffer->device->info,
                                cmd_buffer->state.current_pipeline,
                                ANV_PIPE_CS_STALL_BIT |
                                ANV_PIPE_STALL_AT_SCOREBOARD_BIT);
   anv_batch_emit(&cmd_buffer->batch, GENX(PIPE_CONTROL), pc) {
         pc.IndirectStatePointersDisable = true;
         pc.CommandStreamerStallEnable = true;
         anv_debug_dump_pc(pc, __func__);
   }
}

static VkResult
end_command_buffer(struct anv_cmd_buffer *cmd_buffer)
{
   if (anv_batch_has_error(&cmd_buffer->batch))
      return cmd_buffer->batch.status;

   anv_measure_endcommandbuffer(cmd_buffer);

   if (anv_cmd_buffer_is_video_queue(cmd_buffer) ||
       anv_cmd_buffer_is_blitter_queue(cmd_buffer)) {
      trace_intel_end_cmd_buffer(&cmd_buffer->trace,
                                 (uintptr_t)(vk_command_buffer_to_handle(&cmd_buffer->vk)),
                                 cmd_buffer->vk.level);
      genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);
      anv_cmd_buffer_end_batch_buffer(cmd_buffer);
      return VK_SUCCESS;
   }

   /* Flush query clears using blorp so that secondary query writes do not
    * race with the clear.
    */
   if (cmd_buffer->state.queries.clear_bits) {
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_QUERY_BITS(cmd_buffer->state.queries.clear_bits),
                                "query clear flush prior command buffer end");
   }

   /* Flush any in-progress CCS/MCS operations in preparation for chaining. */
   genX(cmd_buffer_update_color_aux_op(cmd_buffer, ISL_AUX_OP_NONE));

   genX(cmd_buffer_flush_generated_draws)(cmd_buffer);

   /* Turn on object level preemption if it is disabled to have it in known
    * state at the beginning of new command buffer.
    */
   if (!cmd_buffer->state.gfx.object_preemption)
      genX(cmd_buffer_set_preemption)(cmd_buffer, true);

   /* We want every command buffer to start with the PMA fix in a known state,
    * so we disable it at the end of the command buffer.
    */
   genX(cmd_buffer_enable_pma_fix)(cmd_buffer, false);

   /* Wa_14015814527
    *
    * Apply task URB workaround in the end of primary or secondary cmd_buffer.
    */
   genX(apply_task_urb_workaround)(cmd_buffer);

   genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);

   emit_isp_disable(cmd_buffer);

#if GFX_VER >= 12
   if (cmd_buffer->vk.level == VK_COMMAND_BUFFER_LEVEL_PRIMARY &&
       cmd_buffer->vk.pool->flags & VK_COMMAND_POOL_CREATE_PROTECTED_BIT)
      genX(cmd_buffer_set_protected_memory)(cmd_buffer, false);
#endif

   trace_intel_end_cmd_buffer(&cmd_buffer->trace,
                              (uintptr_t)(vk_command_buffer_to_handle(&cmd_buffer->vk)),
                              cmd_buffer->vk.level);

   anv_cmd_buffer_end_batch_buffer(cmd_buffer);

   return VK_SUCCESS;
}

VkResult
genX(EndCommandBuffer)(
    VkCommandBuffer                             commandBuffer)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);

   VkResult status = end_command_buffer(cmd_buffer);
   if (status != VK_SUCCESS)
      return status;

   /* If there is MSAA access over the compute/transfer queue, we can use the
    * companion RCS command buffer and end it properly.
    */
   if (cmd_buffer->companion_rcs_cmd_buffer) {
       assert(anv_cmd_buffer_is_compute_queue(cmd_buffer) ||
              anv_cmd_buffer_is_blitter_queue(cmd_buffer));
       status = end_command_buffer(cmd_buffer->companion_rcs_cmd_buffer);
   }

   ANV_RMV(cmd_buffer_create, cmd_buffer->device, cmd_buffer);

   return status;
}

void
genX(CmdExecuteCommands)(
    VkCommandBuffer                             commandBuffer,
    uint32_t                                    commandBufferCount,
    const VkCommandBuffer*                      pCmdBuffers)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, container, commandBuffer);

   struct anv_device *device = container->device;

   if (anv_batch_has_error(&container->batch))
      return;

   /* The secondary command buffers will assume that the PMA fix is disabled
    * when they begin executing.  Make sure this is true.
    */
   genX(cmd_buffer_enable_pma_fix)(container, false);

   /* Turn on preemption in case it was toggled off. */
   if (!container->state.gfx.object_preemption)
      genX(cmd_buffer_set_preemption)(container, true);

   /* Wa_14015814527
    *
    * Apply task URB workaround before secondary cmd buffers.
    */
   genX(apply_task_urb_workaround)(container);

   /* Flush query clears using blorp so that secondary query writes do not
    * race with the clear.
    */
   if (container->state.queries.clear_bits) {
      anv_add_pending_pipe_bits(container,
                                ANV_PIPE_QUERY_BITS(container->state.queries.clear_bits),
                                "query clear flush prior to secondary buffer");
   }

   /* Ensure we're in a regular drawing cache mode (assumption for all
    * secondary).
    */
   genX(cmd_buffer_update_color_aux_op(container, ISL_AUX_OP_NONE));

   /* The secondary command buffer doesn't know which textures etc. have been
    * flushed prior to their execution.  Apply those flushes now.
    */
   genX(cmd_buffer_apply_pipe_flushes)(container);

   genX(cmd_buffer_flush_generated_draws)(container);

   UNUSED enum anv_cmd_descriptor_buffer_mode db_mode =
      container->state.current_db_mode;

   /* Do a first pass to copy the surface state content of the render targets
    * if needed.
    */
   bool need_surface_state_copy = false;
   for (uint32_t i = 0; i < commandBufferCount; i++) {
      ANV_FROM_HANDLE(anv_cmd_buffer, secondary, pCmdBuffers[i]);

      if (secondary->usage_flags &
          VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT) {
         need_surface_state_copy = true;
         break;
      }
   }

   if (need_surface_state_copy) {
      if (container->vk.pool->flags & VK_COMMAND_POOL_CREATE_PROTECTED_BIT)
         genX(cmd_buffer_set_protected_memory)(container, false);

      /* The memcpy will take care of the 3D preemption requirements. */
      struct anv_memcpy_state memcpy_state;
      genX(emit_so_memcpy_init)(&memcpy_state, device,
                                container, &container->batch);

      for (uint32_t i = 0; i < commandBufferCount; i++) {
         ANV_FROM_HANDLE(anv_cmd_buffer, secondary, pCmdBuffers[i]);

         assert(secondary->vk.level == VK_COMMAND_BUFFER_LEVEL_SECONDARY);
         assert(!anv_batch_has_error(&secondary->batch));

         if (secondary->usage_flags &
             VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT) {
            /* If we're continuing a render pass from the container, we need
             * to copy the surface states for the current subpass into the
             * storage we allocated for them in BeginCommandBuffer.
             */
            struct anv_state src_state = container->state.gfx.att_states;
            struct anv_state dst_state = secondary->state.gfx.att_states;
            assert(src_state.alloc_size == dst_state.alloc_size);

            genX(emit_so_memcpy)(
               &memcpy_state,
               anv_state_pool_state_address(&device->internal_surface_state_pool,
                                            dst_state),
               anv_state_pool_state_address(&device->internal_surface_state_pool,
                                            src_state),
               src_state.alloc_size);
         }
      }
      genX(emit_so_memcpy_fini)(&memcpy_state);

      anv_add_pending_pipe_bits(container,
                                ANV_PIPE_CS_STALL_BIT | ANV_PIPE_STALL_AT_SCOREBOARD_BIT,
                                "Wait for primary->secondary RP surface state copies");
      genX(cmd_buffer_apply_pipe_flushes)(container);

      if (container->vk.pool->flags & VK_COMMAND_POOL_CREATE_PROTECTED_BIT)
         genX(cmd_buffer_set_protected_memory)(container, true);
   }

   /* Ensure preemption is enabled (assumption for all secondary) */
   genX(cmd_buffer_set_preemption)(container, true);

   for (uint32_t i = 0; i < commandBufferCount; i++) {
      ANV_FROM_HANDLE(anv_cmd_buffer, secondary, pCmdBuffers[i]);

      assert(secondary->vk.level == VK_COMMAND_BUFFER_LEVEL_SECONDARY);
      assert(!anv_batch_has_error(&secondary->batch));

      if (secondary->state.conditional_render_enabled) {
         if (!container->state.conditional_render_enabled) {
            /* Secondary buffer is constructed as if it will be executed
             * with conditional rendering, we should satisfy this dependency
             * regardless of conditional rendering being enabled in container.
             */
            struct mi_builder b;
            mi_builder_init(&b, device->info, &container->batch);
            mi_store(&b, mi_reg64(ANV_PREDICATE_RESULT_REG),
                         mi_imm(UINT64_MAX));
         }
      }

      anv_cmd_buffer_add_secondary(container, secondary);

      /* Add secondary buffer's RCS command buffer to container buffer's RCS
       * command buffer for execution if secondary RCS is valid.
       */
      if (secondary->companion_rcs_cmd_buffer != NULL) {
         VkResult result = anv_cmd_buffer_ensure_rcs_companion(container);
         if (result != VK_SUCCESS) {
            anv_batch_set_error(&container->batch, result);
            return;
         }

         anv_cmd_buffer_add_secondary(container->companion_rcs_cmd_buffer,
                                      secondary->companion_rcs_cmd_buffer);
      }

      assert(secondary->perf_query_pool == NULL || container->perf_query_pool == NULL ||
             secondary->perf_query_pool == container->perf_query_pool);
      if (secondary->perf_query_pool)
         container->perf_query_pool = secondary->perf_query_pool;

#if INTEL_NEEDS_WA_1808121037
      if (secondary->state.gfx.depth_reg_mode != ANV_DEPTH_REG_MODE_UNKNOWN)
         container->state.gfx.depth_reg_mode = secondary->state.gfx.depth_reg_mode;
#endif

      container->state.gfx.viewport_set |= secondary->state.gfx.viewport_set;

      db_mode = secondary->state.current_db_mode;
   }

   /* The secondary isn't counted in our VF cache tracking so we need to
    * invalidate the whole thing.
    */
   if (GFX_VER == 9) {
      anv_add_pending_pipe_bits(container,
                                ANV_PIPE_CS_STALL_BIT | ANV_PIPE_VF_CACHE_INVALIDATE_BIT,
                                "Secondary cmd buffer not tracked in VF cache");
   }

#if INTEL_WA_16014538804_GFX_VER
   if (anv_cmd_buffer_is_render_queue(container) &&
       intel_needs_workaround(device->info, 16014538804))
      anv_batch_emit(&container->batch, GENX(PIPE_CONTROL), pc);
#endif

   /* The secondary may have selected a different pipeline (3D or compute) and
    * may have changed the current L3$ configuration.  Reset our tracking
    * variables to invalid values to ensure that we re-emit these in the case
    * where we do any draws or compute dispatches from the container after the
    * secondary has returned.
    */
   container->state.current_pipeline = UINT32_MAX;
   container->state.current_l3_config = NULL;
   container->state.current_hash_scale = 0;
   container->state.gfx.push_constant_stages = 0;

   memset(&container->state.gfx.urb_cfg, 0, sizeof(struct intel_urb_config));

   /* Reemit all GFX instructions in container */
   memcpy(container->state.gfx.dyn_state.dirty,
          device->gfx_dirty_state,
          sizeof(container->state.gfx.dyn_state.dirty));
   if (container->device->vk.enabled_extensions.KHR_fragment_shading_rate) {
      /* Also recompute the CPS_STATE offset */
      struct vk_dynamic_graphics_state *dyn =
         &container->vk.dynamic_graphics_state;
      BITSET_SET(dyn->dirty, MESA_VK_DYNAMIC_FSR);
   }

   /* Each of the secondary command buffers will use its own state base
    * address.  We need to re-emit state base address for the container after
    * all of the secondaries are done.
    */
   if (container->device->vk.enabled_extensions.EXT_descriptor_buffer) {
#if GFX_VERx10 >= 125
      /* If the last secondary had a different mode, reemit the last pending
       * mode. Otherwise, we can do a lighter binding table pool update.
       */
      if (db_mode != container->state.current_db_mode) {
         container->state.current_db_mode = db_mode;
         genX(cmd_buffer_emit_state_base_address)(container);
      } else {
         genX(cmd_buffer_emit_bt_pool_base_address)(container);
      }
#else
      genX(cmd_buffer_emit_state_base_address)(container);
#endif
   } else {
      genX(cmd_buffer_emit_bt_pool_base_address)(container);
   }

   /* Copy of utrace timestamp buffers from secondary into container */
   if (u_trace_enabled(&device->ds.trace_context)) {
      trace_intel_begin_trace_copy(&container->trace);

      struct anv_memcpy_state memcpy_state;
      genX(emit_so_memcpy_init)(&memcpy_state, device,
                                container, &container->batch);
      uint32_t num_traces = 0;
      for (uint32_t i = 0; i < commandBufferCount; i++) {
         ANV_FROM_HANDLE(anv_cmd_buffer, secondary, pCmdBuffers[i]);

         num_traces += secondary->trace.num_traces;
         u_trace_clone_append(u_trace_begin_iterator(&secondary->trace),
                              u_trace_end_iterator(&secondary->trace),
                              &container->trace,
                              &memcpy_state,
                              anv_device_utrace_emit_gfx_copy_buffer);
      }
      genX(emit_so_memcpy_fini)(&memcpy_state);

      trace_intel_end_trace_copy(&container->trace, num_traces);

      /* Memcpy is done using the 3D pipeline. */
      container->state.current_pipeline = _3D;
   }
}

static inline enum anv_pipe_bits
anv_pipe_flush_bits_for_access_flags(struct anv_cmd_buffer *cmd_buffer,
                                     VkAccessFlags2 flags,
                                     VkAccessFlagBits3KHR flags3)
{
   enum anv_pipe_bits pipe_bits = 0;

   u_foreach_bit64(b, flags) {
      switch ((VkAccessFlags2)BITFIELD64_BIT(b)) {
      case VK_ACCESS_2_SHADER_WRITE_BIT:
      case VK_ACCESS_2_SHADER_STORAGE_WRITE_BIT:
      case VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR:
         /* We're transitioning a buffer that was previously used as write
          * destination through the data port. To make its content available
          * to future operations, flush the hdc pipeline.
          */
         pipe_bits |= ANV_PIPE_HDC_PIPELINE_FLUSH_BIT;
         pipe_bits |= ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
         break;
      case VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT:
         /* We're transitioning a buffer that was previously used as render
          * target. To make its content available to future operations, flush
          * the render target cache.
          */
         pipe_bits |= ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT;
         break;
      case VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT:
         /* We're transitioning a buffer that was previously used as depth
          * buffer. To make its content available to future operations, flush
          * the depth cache.
          */
         pipe_bits |= ANV_PIPE_DEPTH_CACHE_FLUSH_BIT;
         break;
      case VK_ACCESS_2_TRANSFER_WRITE_BIT:
         /* We're transitioning a buffer that was previously used as a
          * transfer write destination. Generic write operations include color
          * & depth operations as well as buffer operations like :
          *     - vkCmdClearColorImage()
          *     - vkCmdClearDepthStencilImage()
          *     - vkCmdBlitImage()
          *     - vkCmdCopy*(), vkCmdUpdate*(), vkCmdFill*()
          *
          * Most of these operations are implemented using Blorp which writes
          * through the render target cache or the depth cache on the graphics
          * queue. On the compute queue, the writes are done through the data
          * port.
          */
         if (anv_cmd_buffer_is_compute_queue(cmd_buffer)) {
            pipe_bits |= ANV_PIPE_HDC_PIPELINE_FLUSH_BIT;
            pipe_bits |= ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
         } else {
            /* We can use the data port when trying to stay in compute mode on
             * the RCS.
             */
            pipe_bits |= ANV_PIPE_HDC_PIPELINE_FLUSH_BIT;
            pipe_bits |= ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
            /* Most operations are done through RT/detph writes */
            pipe_bits |= ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT;
            pipe_bits |= ANV_PIPE_DEPTH_CACHE_FLUSH_BIT;
         }
         break;
      case VK_ACCESS_2_MEMORY_WRITE_BIT:
         /* We're transitioning a buffer for generic write operations. Flush
          * all the caches.
          */
         pipe_bits |= ANV_PIPE_BARRIER_FLUSH_BITS;
         break;
      case VK_ACCESS_2_HOST_WRITE_BIT:
         /* We're transitioning a buffer for access by CPU. Invalidate
          * all the caches. Since data and tile caches don't have invalidate,
          * we are forced to flush those as well.
          */
         pipe_bits |= ANV_PIPE_BARRIER_FLUSH_BITS;
         pipe_bits |= ANV_PIPE_INVALIDATE_BITS;
         break;
      case VK_ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT:
      case VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT:
         /* We're transitioning a buffer written either from VS stage or from
          * the command streamer (see CmdEndTransformFeedbackEXT), we just
          * need to stall the CS.
          *
          * Streamout writes apparently bypassing L3, in order to make them
          * visible to the destination, we need to invalidate the other
          * caches.
          */
         pipe_bits |= ANV_PIPE_CS_STALL_BIT | ANV_PIPE_INVALIDATE_BITS;
         break;
      default:
         break; /* Nothing to do */
      }
   }

   return pipe_bits;
}

static inline enum anv_pipe_bits
anv_pipe_invalidate_bits_for_access_flags(struct anv_cmd_buffer *cmd_buffer,
                                          VkAccessFlags2 flags,
                                          VkAccessFlagBits3KHR flags3)
{
   struct anv_device *device = cmd_buffer->device;
   enum anv_pipe_bits pipe_bits = 0;

   u_foreach_bit64(b, flags) {
      switch ((VkAccessFlags2)BITFIELD64_BIT(b)) {
      case VK_ACCESS_2_INDIRECT_COMMAND_READ_BIT:
         /* Indirect draw commands take a buffer as input that we're going to
          * read from the command streamer to load some of the HW registers
          * (see genX_cmd_buffer.c:load_indirect_parameters). This requires a
          * command streamer stall so that all the cache flushes have
          * completed before the command streamer loads from memory.
          */
         pipe_bits |=  ANV_PIPE_CS_STALL_BIT;
         if (device->info->ver == 9) {
            /* Indirect draw commands on Gfx9 also set gl_BaseVertex &
             * gl_BaseIndex through a vertex buffer, so invalidate that cache.
             */
            pipe_bits |= ANV_PIPE_VF_CACHE_INVALIDATE_BIT;
         }
         /* For CmdDipatchIndirect, we load indirect gl_NumWorkGroups through
          * an A64 message, so we need to invalidate constant cache.
          */
         pipe_bits |= ANV_PIPE_CONSTANT_CACHE_INVALIDATE_BIT;
         /* Tile & Data cache flush needed For Cmd*Indirect* commands since
          * command streamer is not L3 coherent.
          */
         pipe_bits |= ANV_PIPE_TILE_CACHE_FLUSH_BIT |
                      ANV_PIPE_DATA_CACHE_FLUSH_BIT;
         break;
      case VK_ACCESS_2_INDEX_READ_BIT:
      case VK_ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT:
         /* We transitioning a buffer to be used for as input for vkCmdDraw*
          * commands, so we invalidate the VF cache to make sure there is no
          * stale data when we start rendering.
          */
         pipe_bits |= ANV_PIPE_VF_CACHE_INVALIDATE_BIT;
         break;
      case VK_ACCESS_2_UNIFORM_READ_BIT:
      case VK_ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR:
         /* We transitioning a buffer to be used as uniform data. Because
          * uniform is accessed through the data port & sampler, we need to
          * invalidate the texture cache (sampler) & constant cache (data
          * port) to avoid stale data.
          */
         pipe_bits |= ANV_PIPE_CONSTANT_CACHE_INVALIDATE_BIT;
         if (device->physical->compiler->indirect_ubos_use_sampler) {
            pipe_bits |= ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT;
         } else {
            pipe_bits |= ANV_PIPE_HDC_PIPELINE_FLUSH_BIT;
            pipe_bits |= ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
         }
         break;
      case VK_ACCESS_2_INPUT_ATTACHMENT_READ_BIT:
      case VK_ACCESS_2_TRANSFER_READ_BIT:
      case VK_ACCESS_2_SHADER_SAMPLED_READ_BIT:
         /* Transitioning a buffer to be read through the sampler, so
          * invalidate the texture cache, we don't want any stale data.
          */
         pipe_bits |= ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT;
         break;
      case VK_ACCESS_2_SHADER_READ_BIT:
         /* Same as VK_ACCESS_2_UNIFORM_READ_BIT and
          * VK_ACCESS_2_SHADER_SAMPLED_READ_BIT cases above
          */
         pipe_bits |= ANV_PIPE_CONSTANT_CACHE_INVALIDATE_BIT |
                      ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT;
         if (!device->physical->compiler->indirect_ubos_use_sampler) {
            pipe_bits |= ANV_PIPE_HDC_PIPELINE_FLUSH_BIT;
            pipe_bits |= ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
         }
         break;
      case VK_ACCESS_2_MEMORY_READ_BIT:
         /* Transitioning a buffer for generic read, invalidate all the
          * caches.
          */
         pipe_bits |= ANV_PIPE_INVALIDATE_BITS;
         break;
      case VK_ACCESS_2_MEMORY_WRITE_BIT:
         /* Generic write, make sure all previously written things land in
          * memory.
          */
         pipe_bits |= ANV_PIPE_BARRIER_FLUSH_BITS;
         break;
      case VK_ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT:
      case VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT:
         /* Transitioning a buffer for conditional rendering or transform
          * feedback. We'll load the content of this buffer into HW registers
          * using the command streamer, so we need to stall the command
          * streamer , so we need to stall the command streamer to make sure
          * any in-flight flush operations have completed.
          */
         pipe_bits |= ANV_PIPE_CS_STALL_BIT;
         pipe_bits |= ANV_PIPE_TILE_CACHE_FLUSH_BIT;
         pipe_bits |= ANV_PIPE_DATA_CACHE_FLUSH_BIT;
         break;
      case VK_ACCESS_2_HOST_READ_BIT:
         /* We're transitioning a buffer that was written by CPU.  Flush
          * all the caches.
          */
         pipe_bits |= ANV_PIPE_BARRIER_FLUSH_BITS;
         break;
      case VK_ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT:
         /* We're transitioning a buffer to be written by the streamout fixed
          * function. This one is apparently not L3 coherent, so we need a
          * tile cache flush to make sure any previous write is not going to
          * create WaW hazards.
          */
         pipe_bits |= ANV_PIPE_DATA_CACHE_FLUSH_BIT;
         pipe_bits |= ANV_PIPE_TILE_CACHE_FLUSH_BIT;
         break;
      case VK_ACCESS_2_SHADER_STORAGE_READ_BIT:
         /* VK_ACCESS_2_SHADER_STORAGE_READ_BIT specifies read access to a
          * storage buffer, physical storage buffer, storage texel buffer, or
          * storage image in any shader pipeline stage.
          *
          * Any storage buffers or images written to must be invalidated and
          * flushed before the shader can access them.
          *
          * Both HDC & Untyped flushes also do invalidation. This is why we
          * use this here on Gfx12+.
          *
          * Gfx11 and prior don't have HDC. Only Data cache flush is available
          * and it only operates on the written cache lines.
          */
         if (device->info->ver >= 12) {
            pipe_bits |= ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
            pipe_bits |= ANV_PIPE_HDC_PIPELINE_FLUSH_BIT;
         }
         break;
      case VK_ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT:
         pipe_bits |= ANV_PIPE_STATE_CACHE_INVALIDATE_BIT;
         break;
      default:
         break; /* Nothing to do */
      }
   }

   return pipe_bits;
}

static inline bool
stage_is_shader(const VkPipelineStageFlags2 stage)
{
   return (stage & (VK_PIPELINE_STAGE_2_VERTEX_SHADER_BIT |
                    VK_PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT |
                    VK_PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT |
                    VK_PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT |
                    VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT |
                    VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT |
                    VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT |
                    VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT |
                    VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR |
                    VK_PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT |
                    VK_PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT));
}

static inline bool
stage_is_transfer(const VkPipelineStageFlags2 stage)
{
   return (stage & (VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT |
                    VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT));
}

static inline bool
stage_is_video(const VkPipelineStageFlags2 stage)
{
   return (stage & (VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT |
#ifdef VK_ENABLE_BETA_EXTENSIONS
                    VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR |
#endif
                    VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR));
}

static inline bool
mask_is_shader_write(const VkAccessFlags2 access,
                     const VkAccessFlagBits3KHR access3)
{
   return (access & (VK_ACCESS_2_SHADER_WRITE_BIT |
                     VK_ACCESS_2_MEMORY_WRITE_BIT |
                     VK_ACCESS_2_SHADER_STORAGE_WRITE_BIT));
}

static inline bool
mask_is_write(const VkAccessFlags2 access,
              const VkAccessFlagBits3KHR access3)
{
   return access & (VK_ACCESS_2_SHADER_WRITE_BIT |
                    VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT |
                    VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT |
                    VK_ACCESS_2_TRANSFER_WRITE_BIT |
                    VK_ACCESS_2_HOST_WRITE_BIT |
                    VK_ACCESS_2_MEMORY_WRITE_BIT |
                    VK_ACCESS_2_SHADER_STORAGE_WRITE_BIT |
                    VK_ACCESS_2_VIDEO_DECODE_WRITE_BIT_KHR |
#ifdef VK_ENABLE_BETA_EXTENSIONS
                    VK_ACCESS_2_VIDEO_ENCODE_WRITE_BIT_KHR |
#endif
                    VK_ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT |
                    VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT |
                    VK_ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV |
                    VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR |
                    VK_ACCESS_2_MICROMAP_WRITE_BIT_EXT |
                    VK_ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV);
}

static inline bool
mask_is_transfer_write(const VkAccessFlags2 access)
{
   return access & (VK_ACCESS_2_TRANSFER_WRITE_BIT |
                    VK_ACCESS_2_MEMORY_WRITE_BIT);
}

static void
cmd_buffer_barrier_video(struct anv_cmd_buffer *cmd_buffer,
                         uint32_t n_dep_infos,
                         const VkDependencyInfo *dep_infos)
{
   assert(anv_cmd_buffer_is_video_queue(cmd_buffer));

   bool flush_llc = false;
   bool flush_ccs = false;

   for (uint32_t d = 0; d < n_dep_infos; d++) {
      const VkDependencyInfo *dep_info = &dep_infos[d];

      for (uint32_t i = 0; i < dep_info->imageMemoryBarrierCount; i++) {
         const VkImageMemoryBarrier2 *img_barrier =
            &dep_info->pImageMemoryBarriers[i];

         ANV_FROM_HANDLE(anv_image, image, img_barrier->image);
         const VkImageSubresourceRange *range = &img_barrier->subresourceRange;

         /* If srcQueueFamilyIndex is not equal to dstQueueFamilyIndex, this
          * memory barrier defines a queue family ownership transfer.
          */
         if (img_barrier->srcQueueFamilyIndex != img_barrier->dstQueueFamilyIndex)
            flush_llc = true;

         VkImageAspectFlags img_aspects =
            vk_image_expand_aspect_mask(&image->vk, range->aspectMask);
         anv_foreach_image_aspect_bit(aspect_bit, image, img_aspects) {
            const uint32_t plane =
               anv_image_aspect_to_plane(image, 1UL << aspect_bit);
            if (isl_aux_usage_has_ccs(image->planes[plane].aux_usage)) {
               flush_ccs = true;
            }
         }
      }

      for (uint32_t i = 0; i < dep_info->bufferMemoryBarrierCount; i++) {
         const VkBufferMemoryBarrier2 *buf_barrier =
            &dep_info->pBufferMemoryBarriers[i];
         const VkMemoryBarrierAccessFlags3KHR *barrier3 =
            vk_find_struct_const(buf_barrier->pNext,
                                 MEMORY_BARRIER_ACCESS_FLAGS_3_KHR);

         /* Flush the cache if something is written by the video operations and
          * used by any other stages except video encode/decode stages or if
          * srcQueueFamilyIndex is not equal to dstQueueFamilyIndex, this memory
          * barrier defines a queue family ownership transfer.
          */
         if ((stage_is_video(buf_barrier->srcStageMask) &&
              mask_is_write(buf_barrier->srcAccessMask,
                            barrier3 ? barrier3->srcAccessMask3 : 0) &&
              !stage_is_video(buf_barrier->dstStageMask)) ||
             (buf_barrier->srcQueueFamilyIndex !=
              buf_barrier->dstQueueFamilyIndex)) {
            flush_llc = true;
            break;
         }
      }

      for (uint32_t i = 0; i < dep_info->memoryBarrierCount; i++) {
         const VkMemoryBarrier2 *mem_barrier = &dep_info->pMemoryBarriers[i];
         const VkMemoryBarrierAccessFlags3KHR *barrier3 =
            vk_find_struct_const(mem_barrier->pNext,
                                 MEMORY_BARRIER_ACCESS_FLAGS_3_KHR);

         /* Flush the cache if something is written by the video operations and
          * used by any other stages except video encode/decode stage.
          */
         if (stage_is_video(mem_barrier->srcStageMask) &&
             mask_is_write(mem_barrier->srcAccessMask,
                           barrier3 ? barrier3->srcAccessMask3 : 0) &&
             !stage_is_video(mem_barrier->dstStageMask)) {
            flush_llc = true;
            break;
         }
      }

      /* We cannot gather more information than that. */
      if (flush_ccs && flush_llc)
         break;
   }

   if (flush_ccs || flush_llc) {
      anv_batch_emit(&cmd_buffer->batch, GENX(MI_FLUSH_DW), fd) {
#if GFX_VERx10 >= 125
         fd.FlushCCS = flush_ccs;
#endif
#if GFX_VER >= 12
         /* Using this bit on Gfx9 triggers a GPU hang.
          * This is undocumented behavior. Gfx12 seems fine.
          * TODO: check Gfx11
          */
         fd.FlushLLC = flush_llc;
#endif
      }
   }
}

static void
cmd_buffer_barrier_blitter(struct anv_cmd_buffer *cmd_buffer,
                           uint32_t n_dep_infos,
                           const VkDependencyInfo *dep_infos)
{
#if GFX_VERx10 >= 125
   assert(anv_cmd_buffer_is_blitter_queue(cmd_buffer));

   /* The blitter requires an MI_FLUSH_DW command when a buffer transitions
    * from being a destination to a source.
    */
   bool flush_llc = false;
   bool flush_ccs = false;

   for (uint32_t d = 0; d < n_dep_infos; d++) {
      const VkDependencyInfo *dep_info = &dep_infos[d];

      for (uint32_t i = 0; i < dep_info->imageMemoryBarrierCount; i++) {
         const VkImageMemoryBarrier2 *img_barrier =
            &dep_info->pImageMemoryBarriers[i];

         ANV_FROM_HANDLE(anv_image, image, img_barrier->image);
         const VkImageSubresourceRange *range = &img_barrier->subresourceRange;

         /* If srcQueueFamilyIndex is not equal to dstQueueFamilyIndex, this
          * memory barrier defines a queue family transfer operation.
          */
         if (img_barrier->srcQueueFamilyIndex != img_barrier->dstQueueFamilyIndex)
            flush_llc = true;

         /* Flush cache if transfer command reads the output of the previous
          * transfer command, ideally we should just wait for the completion
          * but for now just flush the cache to make the data visible.
          */
         if ((img_barrier->oldLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL ||
              img_barrier->oldLayout == VK_IMAGE_LAYOUT_GENERAL) &&
             (img_barrier->newLayout == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL ||
              img_barrier->newLayout == VK_IMAGE_LAYOUT_GENERAL)) {
            flush_llc = true;
         }

         VkImageAspectFlags img_aspects =
            vk_image_expand_aspect_mask(&image->vk, range->aspectMask);
         anv_foreach_image_aspect_bit(aspect_bit, image, img_aspects) {
            const uint32_t plane =
               anv_image_aspect_to_plane(image, 1UL << aspect_bit);
            if (isl_aux_usage_has_ccs(image->planes[plane].aux_usage)) {
               flush_ccs = true;
            }
         }
      }

      for (uint32_t i = 0; i < dep_info->bufferMemoryBarrierCount; i++) {
         const VkBufferMemoryBarrier2 *buf_barrier =
            &dep_info->pBufferMemoryBarriers[i];
         const VkMemoryBarrierAccessFlags3KHR *barrier3 =
            vk_find_struct_const(buf_barrier->pNext,
                                 MEMORY_BARRIER_ACCESS_FLAGS_3_KHR);

         /* Flush the cache if something is written by the transfer command
          * and used by any other stages except transfer stage or if
          * srcQueueFamilyIndex is not equal to dstQueueFamilyIndex, this
          * memory barrier defines a queue family transfer operation.
          */
         if ((stage_is_transfer(buf_barrier->srcStageMask) &&
              mask_is_write(buf_barrier->srcAccessMask,
                            barrier3 ? barrier3->srcAccessMask3 : 0)) ||
             (buf_barrier->srcQueueFamilyIndex !=
              buf_barrier->dstQueueFamilyIndex)) {
            flush_llc = true;
            break;
         }
      }

      for (uint32_t i = 0; i < dep_info->memoryBarrierCount; i++) {
         const VkMemoryBarrier2 *mem_barrier = &dep_info->pMemoryBarriers[i];
         const VkMemoryBarrierAccessFlags3KHR *barrier3 =
            vk_find_struct_const(mem_barrier->pNext,
                                 MEMORY_BARRIER_ACCESS_FLAGS_3_KHR);

         /* Flush the cache if something is written by the transfer command
          * and used by any other stages except transfer stage.
          */
         if (stage_is_transfer(mem_barrier->srcStageMask) &&
             mask_is_write(mem_barrier->srcAccessMask,
                barrier3 ? barrier3->srcAccessMask3 : 0)) {
            flush_llc = true;
            break;
         }
      }

      /* We cannot gather more information than that. */
      if (flush_ccs && flush_llc)
         break;
   }

   if (flush_ccs || flush_llc) {
      /* Wa_16018063123 - emit fast color dummy blit before MI_FLUSH_DW. */
      if (INTEL_WA_16018063123_GFX_VER) {
         genX(batch_emit_fast_color_dummy_blit)(&cmd_buffer->batch,
                                                cmd_buffer->device);
      }
      anv_batch_emit(&cmd_buffer->batch, GENX(MI_FLUSH_DW), fd) {
         fd.FlushCCS = flush_ccs;
         fd.FlushLLC = flush_llc;
      }
   }
#endif
}

static inline bool
cmd_buffer_has_pending_copy_query(struct anv_cmd_buffer *cmd_buffer)
{
   /* Query copies are only written with dataport, so we only need to check
    * that flag.
    */
   return (cmd_buffer->state.queries.buffer_write_bits &
           ANV_QUERY_WRITES_DATA_FLUSH) != 0;
}

static bool
img_barrier_has_acquire_unmodified(const VkImageMemoryBarrier2 *img_barrier)
{
   /* The Vulkan 1.3.302 spec says:
    *
    *    This struct [VkExternalMemoryAcquireUnmodifiedEXT] is ignored if the
    *    memory barrier's srcQueueFamilyIndex is not a special queue family
    *    reserved for external memory ownership transfers.
    */
   if (!queue_family_is_external(img_barrier->srcQueueFamilyIndex))
      return false;

   const VkExternalMemoryAcquireUnmodifiedEXT *unmodified_info =
      vk_find_struct_const(img_barrier->pNext,
                           EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT);

   return unmodified_info && unmodified_info->acquireUnmodifiedMemory;
}

static void
cmd_buffer_accumulate_barrier_bits(struct anv_cmd_buffer *cmd_buffer,
                                   uint32_t n_dep_infos,
                                   const VkDependencyInfo *dep_infos,
                                   VkPipelineStageFlags2 *out_src_stages,
                                   VkPipelineStageFlags2 *out_dst_stages,
                                   enum anv_pipe_bits *out_bits)
{
   /* XXX: Right now, we're really dumb and just flush whatever categories
    * the app asks for. One of these days we may make this a bit better but
    * right now that's all the hardware allows for in most areas.
    */
   VkAccessFlags2 src_flags = 0;
   VkAccessFlags2 dst_flags = 0;

   VkAccessFlags3KHR src_flags3 = 0;
   VkAccessFlags3KHR dst_flags3 = 0;

   VkPipelineStageFlags2 src_stages = 0;
   VkPipelineStageFlags2 dst_stages = 0;

   struct anv_device *device = cmd_buffer->device;
#if GFX_VER < 20
   bool apply_sparse_flushes = false;
#endif
   bool flush_query_copies = false;

   for (uint32_t d = 0; d < n_dep_infos; d++) {
      const VkDependencyInfo *dep_info = &dep_infos[d];

      for (uint32_t i = 0; i < dep_info->memoryBarrierCount; i++) {
         const VkMemoryBarrier2 *mem_barrier = &dep_info->pMemoryBarriers[i];
         const VkMemoryBarrierAccessFlags3KHR *barrier3 =
            vk_find_struct_const(mem_barrier->pNext,
                                 MEMORY_BARRIER_ACCESS_FLAGS_3_KHR);

         if (barrier3) {
            src_flags3 |= barrier3->srcAccessMask3;
            dst_flags3 |= barrier3->dstAccessMask3;
         }

         src_flags |= mem_barrier->srcAccessMask;
         dst_flags |= mem_barrier->dstAccessMask;

         src_stages |= mem_barrier->srcStageMask;
         dst_stages |= mem_barrier->dstStageMask;

         /* Shader writes to buffers that could then be written by a transfer
          * command (including queries).
          */
         if (stage_is_shader(mem_barrier->srcStageMask) &&
             mask_is_shader_write(mem_barrier->srcAccessMask,
                                  barrier3 ? barrier3->srcAccessMask3 : 0) &&
             stage_is_transfer(mem_barrier->dstStageMask)) {
            cmd_buffer->state.queries.buffer_write_bits |=
               ANV_QUERY_COMPUTE_WRITES_PENDING_BITS;
         }

         if (stage_is_transfer(mem_barrier->srcStageMask) &&
             mask_is_transfer_write(mem_barrier->srcAccessMask) &&
             cmd_buffer_has_pending_copy_query(cmd_buffer))
            flush_query_copies = true;

#if GFX_VER < 20
         /* There's no way of knowing if this memory barrier is related to
          * sparse buffers! This is pretty horrible.
          */
         if (mask_is_write(src_flags,
                           barrier3 ? barrier3->srcAccessMask3 : 0) &&
             p_atomic_read(&device->num_sparse_resources) > 0)
            apply_sparse_flushes = true;
#endif
      }

      for (uint32_t i = 0; i < dep_info->bufferMemoryBarrierCount; i++) {
         const VkBufferMemoryBarrier2 *buf_barrier =
            &dep_info->pBufferMemoryBarriers[i];

         const VkMemoryBarrierAccessFlags3KHR *barrier3 =
            vk_find_struct_const(buf_barrier->pNext,
                                 MEMORY_BARRIER_ACCESS_FLAGS_3_KHR);
         if (barrier3) {
            src_flags3 |= barrier3->srcAccessMask3;
            dst_flags3 |= barrier3->dstAccessMask3;
         }

         src_flags |= buf_barrier->srcAccessMask;
         dst_flags |= buf_barrier->dstAccessMask;

         src_stages |= buf_barrier->srcStageMask;
         dst_stages |= buf_barrier->dstStageMask;

         /* Shader writes to buffers that could then be written by a transfer
          * command (including queries).
          */
         if (stage_is_shader(buf_barrier->srcStageMask) &&
             mask_is_shader_write(buf_barrier->srcAccessMask,
                                  barrier3 ? barrier3->srcAccessMask3 : 0) &&
             stage_is_transfer(buf_barrier->dstStageMask)) {
            cmd_buffer->state.queries.buffer_write_bits |=
               ANV_QUERY_COMPUTE_WRITES_PENDING_BITS;
         }

         if (stage_is_transfer(buf_barrier->srcStageMask) &&
             mask_is_transfer_write(buf_barrier->srcAccessMask) &&
             cmd_buffer_has_pending_copy_query(cmd_buffer))
            flush_query_copies = true;

#if GFX_VER < 20
         ANV_FROM_HANDLE(anv_buffer, buffer, buf_barrier->buffer);

         if (anv_buffer_is_sparse(buffer) &&
             mask_is_write(src_flags, barrier3 ? barrier3->srcAccessMask3 : 0))
            apply_sparse_flushes = true;
#endif
      }

      for (uint32_t i = 0; i < dep_info->imageMemoryBarrierCount; i++) {
         const VkImageMemoryBarrier2 *img_barrier =
            &dep_info->pImageMemoryBarriers[i];

         const VkMemoryBarrierAccessFlags3KHR *barrier3 =
            vk_find_struct_const(img_barrier->pNext,
                                 MEMORY_BARRIER_ACCESS_FLAGS_3_KHR);
         if (barrier3) {
            src_flags3 |= barrier3->srcAccessMask3;
            dst_flags3 |= barrier3->dstAccessMask3;
         }

         src_flags |= img_barrier->srcAccessMask;
         dst_flags |= img_barrier->dstAccessMask;

         src_stages |= img_barrier->srcStageMask;
         dst_stages |= img_barrier->dstStageMask;

         ANV_FROM_HANDLE(anv_image, image, img_barrier->image);
         const VkImageSubresourceRange *range = &img_barrier->subresourceRange;

         uint32_t base_layer, layer_count;
         if (image->vk.image_type == VK_IMAGE_TYPE_3D) {
            const uint32_t depth_at_base_mip =
               u_minify(image->vk.extent.depth, range->baseMipLevel);
            /* VK_KHR_maintenance9:
             *
             *    "The effects of image memory barriers and image layout
             *    transitions on 3D images created with
             *    VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT are scoped to the
             *    slices specified by the user-provided
             *    VkImageSubresourceRange."
             */
            if ((image->vk.create_flags & VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT) &&
                device->vk.enabled_features.maintenance9) {
               base_layer = range->baseArrayLayer;
               layer_count = range->layerCount == VK_REMAINING_ARRAY_LAYERS ?
                             (depth_at_base_mip - range->baseArrayLayer) :
                             range->layerCount;
            } else {
               base_layer = 0;
               layer_count = depth_at_base_mip;
            }
         } else {
            base_layer = range->baseArrayLayer;
            layer_count = vk_image_subresource_layer_count(&image->vk, range);
         }
         const uint32_t level_count =
            vk_image_subresource_level_count(&image->vk, range);

         VkImageLayout old_layout = img_barrier->oldLayout;
         VkImageLayout new_layout = img_barrier->newLayout;

         /* If we're inside a render pass, the runtime might have converted
          * some layouts from GENERAL to FEEDBACK_LOOP. Check if that's the
          * case and reconvert back to the original layout so that application
          * barriers within renderpass are operating with consistent layouts.
          */
         if (!cmd_buffer->vk.runtime_rp_barrier &&
             cmd_buffer->vk.render_pass != NULL &&
             old_layout == VK_IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT) {
            /* Those assert are here to recognize the changes made by the
             * runtime. If we fail them, we need to investigate what is going
             * on.
             */
            assert(anv_cmd_graphics_state_has_image_as_attachment(&cmd_buffer->state.gfx,
                                                                  image));
            VkImageLayout subpass_att_layout, subpass_stencil_att_layout;

            vk_command_buffer_get_attachment_layout(
               &cmd_buffer->vk, &image->vk,
               &subpass_att_layout, &subpass_stencil_att_layout);

            old_layout = subpass_att_layout;
            new_layout = subpass_att_layout;
         }

         if (range->aspectMask & VK_IMAGE_ASPECT_DEPTH_BIT) {
            transition_depth_buffer(cmd_buffer, image,
                                    range->baseMipLevel, level_count,
                                    base_layer, layer_count,
                                    old_layout, new_layout,
                                    false /* will_full_fast_clear */);
         }

         if (range->aspectMask & VK_IMAGE_ASPECT_STENCIL_BIT) {
            transition_stencil_buffer(cmd_buffer, image,
                                      range->baseMipLevel, level_count,
                                      base_layer, layer_count,
                                      old_layout, new_layout,
                                      false /* will_full_fast_clear */);
         }

         if (range->aspectMask & VK_IMAGE_ASPECT_ANY_COLOR_BIT_ANV) {
            bool acquire_unmodified =
               img_barrier_has_acquire_unmodified(img_barrier);
            VkImageAspectFlags color_aspects =
               vk_image_expand_aspect_mask(&image->vk, range->aspectMask);
            anv_foreach_image_aspect_bit(aspect_bit, image, color_aspects) {
               transition_color_buffer(cmd_buffer, image, 1UL << aspect_bit,
                                       range->baseMipLevel, level_count,
                                       base_layer, layer_count,
                                       old_layout, new_layout,
                                       img_barrier->srcQueueFamilyIndex,
                                       img_barrier->dstQueueFamilyIndex,
                                       false /* will_full_fast_clear */,
                                       acquire_unmodified);
            }
         }
#if GFX_VER < 20
         /* Mark image as compressed if the destination layout has untracked
          * writes to the aux surface.
          */
         VkImageAspectFlags aspects =
            vk_image_expand_aspect_mask(&image->vk, range->aspectMask);
         anv_foreach_image_aspect_bit(aspect_bit, image, aspects) {
            VkImageAspectFlagBits aspect = 1UL << aspect_bit;
            if (anv_layout_has_untracked_aux_writes(
                   device->info,
                   image, aspect,
                   img_barrier->newLayout,
                   cmd_buffer->queue_family->queueFlags)) {
               for (uint32_t l = 0; l < level_count; l++) {
                  const uint32_t level = range->baseMipLevel + l;
                  const uint32_t aux_layers =
                     anv_image_aux_layers(image, aspect, level);

                  if (base_layer >= aux_layers)
                     break; /* We will only get fewer layers as level increases */

                  uint32_t level_layer_count =
                     MIN2(layer_count, aux_layers - base_layer);

                  set_image_compressed_bit(cmd_buffer, image, aspect,
                                           level,
                                           base_layer, level_layer_count,
                                           true);
               }
            }
         }

         if (anv_image_is_sparse(image) &&
             mask_is_write(src_flags, barrier3 ? barrier3->srcAccessMask3 : 0))
            apply_sparse_flushes = true;
#endif
      }
   }

   enum anv_pipe_bits bits =
      anv_pipe_flush_bits_for_access_flags(cmd_buffer, src_flags, src_flags3) |
      anv_pipe_invalidate_bits_for_access_flags(cmd_buffer, dst_flags, dst_flags3);

   /* What stage require a stall at pixel scoreboard */
   VkPipelineStageFlags2 pb_stall_stages =
      VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT |
      VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT |
      VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT |
      VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT |
      VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
   if (anv_cmd_buffer_is_render_queue(cmd_buffer)) {
      /* On a render queue, the following stages can also use a pixel shader.
       */
      pb_stall_stages |=
         VK_PIPELINE_STAGE_2_TRANSFER_BIT |
         VK_PIPELINE_STAGE_2_RESOLVE_BIT |
         VK_PIPELINE_STAGE_2_BLIT_BIT |
         VK_PIPELINE_STAGE_2_CLEAR_BIT;
   }
   VkPipelineStageFlags2 cs_stall_stages =
      VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT |
      VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT |
      VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR |
      VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR |
      VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
   if (anv_cmd_buffer_is_compute_queue(cmd_buffer)) {
      /* On a compute queue, the following stages can also use a compute
       * shader.
       */
      cs_stall_stages |=
         VK_PIPELINE_STAGE_2_TRANSFER_BIT |
         VK_PIPELINE_STAGE_2_RESOLVE_BIT |
         VK_PIPELINE_STAGE_2_BLIT_BIT |
         VK_PIPELINE_STAGE_2_CLEAR_BIT;
   } else if (anv_cmd_buffer_is_render_queue(cmd_buffer) &&
              cmd_buffer->state.current_pipeline == GPGPU) {
      /* In GPGPU mode, the render queue can also use a compute shader for
       * transfer operations.
       */
      cs_stall_stages |= VK_PIPELINE_STAGE_2_TRANSFER_BIT;
   }

   /* Prior to Gfx20, we can restrict pb-stall/cs-stall to some pipeline
    * modes. Gfx20 doesn't do pipeline switches so we have to assume the worse
    * case.
    */
   const bool needs_pb_stall =
      anv_cmd_buffer_is_render_queue(cmd_buffer) &&
#if GFX_VER < 20
      cmd_buffer->state.current_pipeline == _3D &&
#endif
      (src_stages & pb_stall_stages);
   if (needs_pb_stall) {
      bits |= GFX_VERx10 >= 125 ?
              ANV_PIPE_PSS_STALL_SYNC_BIT :
              ANV_PIPE_STALL_AT_SCOREBOARD_BIT;
   }
   const bool needs_cs_stall =
      anv_cmd_buffer_is_render_or_compute_queue(cmd_buffer) &&
#if GFX_VER < 20
      cmd_buffer->state.current_pipeline == GPGPU &&
#endif
      (src_stages & cs_stall_stages);
   if (needs_cs_stall)
      bits |= ANV_PIPE_CS_STALL_BIT;

#if GFX_VER < 20
   /* Our HW implementation of the sparse feature prior to Xe2 lives in the
    * GAM unit (interface between all the GPU caches and external memory).
    * As a result writes to NULL bound images & buffers that should be
    * ignored are actually still visible in the caches. The only way for us
    * to get correct NULL bound regions to return 0s is to evict the caches
    * to force the caches to be repopulated with 0s.
    *
    * Our understanding is that Xe2 started to tag the L3 cache with some
    * kind physical address information rather. It is therefore able to
    * detect that a cache line in the cache is going to a null tile and so
    * the L3 cache also has a sparse compatible behavior and we don't need
    * to flush anymore.
    */
   if (apply_sparse_flushes)
      bits |= ANV_PIPE_BARRIER_FLUSH_BITS;
#endif

   /* Copies from query pools are executed with a shader writing through the
    * dataport.
    */
   if (flush_query_copies) {
      bits |= (GFX_VER >= 12 ?
               ANV_PIPE_HDC_PIPELINE_FLUSH_BIT : ANV_PIPE_DATA_CACHE_FLUSH_BIT);
   }

   if (dst_flags & VK_ACCESS_INDIRECT_COMMAND_READ_BIT)
      genX(cmd_buffer_flush_generated_draws)(cmd_buffer);

   *out_src_stages = src_stages;
   *out_dst_stages = dst_stages;
   *out_bits = bits;
}

static void
cmd_buffer_barrier(struct anv_cmd_buffer *cmd_buffer,
                   uint32_t n_dep_infos,
                   const VkDependencyInfo *dep_infos,
                   const char *reason)
{
   switch (cmd_buffer->batch.engine_class) {
   case INTEL_ENGINE_CLASS_VIDEO:
      cmd_buffer_barrier_video(cmd_buffer, n_dep_infos, dep_infos);
      break;

   case INTEL_ENGINE_CLASS_COPY:
      cmd_buffer_barrier_blitter(cmd_buffer, n_dep_infos, dep_infos);
      break;

   case INTEL_ENGINE_CLASS_RENDER:
   case INTEL_ENGINE_CLASS_COMPUTE: {
      VkPipelineStageFlags2 src_stages, dst_stages;
      enum anv_pipe_bits bits;
      cmd_buffer_accumulate_barrier_bits(cmd_buffer, n_dep_infos, dep_infos,
                                         &src_stages, &dst_stages, &bits);

      anv_add_pending_pipe_bits(cmd_buffer, bits, reason);
      break;
   }

   default:
      UNREACHABLE("Invalid engine class");
   }
}

void genX(CmdPipelineBarrier2)(
    VkCommandBuffer                             commandBuffer,
    const VkDependencyInfo*                     pDependencyInfo)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);

   cmd_buffer_barrier(cmd_buffer, 1, pDependencyInfo, "pipe barrier");
}

void
genX(batch_emit_breakpoint)(struct anv_batch *batch,
                            struct anv_device *device,
                            bool emit_before_draw_or_dispatch)
{
   uint32_t before_count = 0, after_count = 0;
   uint32_t *counter = NULL;

   if (INTEL_DEBUG(DEBUG_DRAW_BKP)) {
      counter = &device->draw_call_count;
      before_count = intel_debug_bkp_before_draw_count;
      after_count = intel_debug_bkp_after_draw_count;
   } else if (INTEL_DEBUG(DEBUG_DISPATCH_BKP)) {
      counter = &device->dispatch_call_count;
      before_count = intel_debug_bkp_before_dispatch_count;
      after_count = intel_debug_bkp_after_dispatch_count;
   }

   if (counter) {
      uint32_t count = emit_before_draw_or_dispatch ?
         p_atomic_inc_return(counter) :
         p_atomic_read(counter);

      bool should_emit =
         (emit_before_draw_or_dispatch && count == before_count) ||
         (!emit_before_draw_or_dispatch && count == after_count);

      if (should_emit) {
         struct anv_address wait_addr =
            anv_state_pool_state_address(&device->dynamic_state_pool,
                                         device->breakpoint);

         anv_batch_emit(batch, GENX(MI_SEMAPHORE_WAIT), sem) {
            sem.WaitMode            = PollingMode;
            sem.CompareOperation    = COMPARE_SAD_EQUAL_SDD;
            sem.SemaphoreDataDword  = 0x1;
            sem.SemaphoreAddress    = wait_addr;
         }
      }
   }
}

/* Only emit PIPELINE_SELECT, for the whole mode switch and flushing use
 * flush_pipeline_select()
 */
void
genX(emit_pipeline_select)(struct anv_batch *batch, uint32_t pipeline,
                           const struct anv_device *device)
{
   /* Bspec 55860: Xe2+ no longer requires PIPELINE_SELECT */
#if GFX_VER < 20
   anv_batch_emit(batch, GENX(PIPELINE_SELECT), ps) {
      ps.MaskBits = GFX_VERx10 >= 125 ? 0x93 : GFX_VER >= 12 ? 0x13 : 0x3;
#if GFX_VER == 12
      ps.MediaSamplerDOPClockGateEnable = true;
#endif
      ps.PipelineSelection = pipeline;
#if GFX_VERx10 == 125
      /* It might still be better to only enable this when the compute
       * pipeline will have DPAS instructions.
       */
      ps.SystolicModeEnable = pipeline == GPGPU &&
         device->vk.enabled_extensions.KHR_cooperative_matrix &&
         device->vk.enabled_features.cooperativeMatrix;
#endif
   }
#endif /* if GFX_VER < 20 */
}

static void
genX(flush_pipeline_select)(struct anv_cmd_buffer *cmd_buffer,
                            uint32_t pipeline)
{
   UNUSED const struct intel_device_info *devinfo = cmd_buffer->device->info;

   if (cmd_buffer->state.current_pipeline == pipeline)
      return;

#if GFX_VER == 9
   /* From the Broadwell PRM, Volume 2a: Instructions, PIPELINE_SELECT:
    *
    *   Software must clear the COLOR_CALC_STATE Valid field in
    *   3DSTATE_CC_STATE_POINTERS command prior to send a PIPELINE_SELECT
    *   with Pipeline Select set to GPGPU.
    *
    * The internal hardware docs recommend the same workaround for Gfx9
    * hardware too.
    */
   if (pipeline == GPGPU)
      anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_CC_STATE_POINTERS), t);
#endif

#if GFX_VERx10 == 120
   /* Undocumented workaround to force the re-emission of
    * MEDIA_INTERFACE_DESCRIPTOR_LOAD when switching from 3D to Compute
    * pipeline without rebinding a pipeline :
    *    vkCmdBindPipeline(COMPUTE, cs_pipeline);
    *    vkCmdDispatch(...);
    *    vkCmdBindPipeline(GRAPHICS, gfx_pipeline);
    *    vkCmdDraw(...);
    *    vkCmdDispatch(...);
    */
   if (pipeline == _3D)
      cmd_buffer->state.compute.pipeline_dirty = true;
#endif

   /* We apparently cannot flush the tile cache (color/depth) from the GPGPU
    * pipeline. That means query clears will not be visible to query
    * copy/write. So we need to flush it before going to GPGPU mode.
    */
   if (cmd_buffer->state.current_pipeline == _3D &&
       cmd_buffer->state.queries.clear_bits) {
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_QUERY_BITS(cmd_buffer->state.queries.clear_bits),
                                "query clear flush prior to GPGPU");
   }

   /* Flush and invalidate bits done needed prior PIPELINE_SELECT. */
   enum anv_pipe_bits bits = 0;

#if GFX_VER >= 12
   /* From Tigerlake PRM, Volume 2a, PIPELINE_SELECT:
    *
    *   "Software must ensure Render Cache, Depth Cache and HDC Pipeline flush
    *   are flushed through a stalling PIPE_CONTROL command prior to
    *   programming of PIPELINE_SELECT command transitioning Pipeline Select
    *   from 3D to GPGPU/Media.
    *   Software must ensure HDC Pipeline flush and Generic Media State Clear
    *   is issued through a stalling PIPE_CONTROL command prior to programming
    *   of PIPELINE_SELECT command transitioning Pipeline Select from
    *   GPGPU/Media to 3D."
    *
    * Note: Issuing PIPE_CONTROL_MEDIA_STATE_CLEAR causes GPU hangs, probably
    * because PIPE was not in MEDIA mode?!
    */
   bits |= ANV_PIPE_CS_STALL_BIT | ANV_PIPE_HDC_PIPELINE_FLUSH_BIT;

   if (cmd_buffer->state.current_pipeline == _3D) {
      bits |= ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
              ANV_PIPE_DEPTH_CACHE_FLUSH_BIT;
   } else {
      bits |= ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
   }
#else
   /* From "BXML » GT » MI » vol1a GPU Overview » [Instruction]
    * PIPELINE_SELECT [DevBWR+]":
    *
    *   Project: DEVSNB+
    *
    *   Software must ensure all the write caches are flushed through a
    *   stalling PIPE_CONTROL command followed by another PIPE_CONTROL
    *   command to invalidate read only caches prior to programming
    *   MI_PIPELINE_SELECT command to change the Pipeline Select Mode.
    *
    * Note the cmd_buffer_apply_pipe_flushes will split this into two
    * PIPE_CONTROLs.
    */
   bits |= ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
           ANV_PIPE_DEPTH_CACHE_FLUSH_BIT |
           ANV_PIPE_HDC_PIPELINE_FLUSH_BIT |
           ANV_PIPE_CS_STALL_BIT |
           ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT |
           ANV_PIPE_CONSTANT_CACHE_INVALIDATE_BIT |
           ANV_PIPE_STATE_CACHE_INVALIDATE_BIT |
           ANV_PIPE_INSTRUCTION_CACHE_INVALIDATE_BIT |
           ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT;
#endif

   /* Wa_16013063087 -  State Cache Invalidate must be issued prior to
    * PIPELINE_SELECT when switching from 3D to Compute.
    *
    * SW must do this by programming of PIPECONTROL with “CS Stall” followed by
    * a PIPECONTROL with State Cache Invalidate bit set.
    *
    */
   if (cmd_buffer->state.current_pipeline == _3D && pipeline == GPGPU &&
       intel_needs_workaround(cmd_buffer->device->info, 16013063087))
      bits |= ANV_PIPE_STATE_CACHE_INVALIDATE_BIT;

   anv_add_pending_pipe_bits(cmd_buffer, bits, "flush/invalidate PIPELINE_SELECT");
   genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);

#if GFX_VER == 9
   if (pipeline == _3D) {
      /* There is a mid-object preemption workaround which requires you to
       * re-emit MEDIA_VFE_STATE after switching from GPGPU to 3D.  However,
       * even without preemption, we have issues with geometry flickering when
       * GPGPU and 3D are back-to-back and this seems to fix it.  We don't
       * really know why.
       *
       * Also, from the Sky Lake PRM Vol 2a, MEDIA_VFE_STATE:
       *
       *    "A stalling PIPE_CONTROL is required before MEDIA_VFE_STATE unless
       *    the only bits that are changed are scoreboard related ..."
       *
       * This is satisfied by applying pre-PIPELINE_SELECT pipe flushes above.
       */
      anv_batch_emit(&cmd_buffer->batch, GENX(MEDIA_VFE_STATE), vfe) {
         vfe.MaximumNumberofThreads =
            devinfo->max_cs_threads * devinfo->subslice_total - 1;
         vfe.NumberofURBEntries     = 2;
         vfe.URBEntryAllocationSize = 2;
      }

      /* We just emitted a dummy MEDIA_VFE_STATE so now that packet is
       * invalid. Set the compute pipeline to dirty to force a re-emit of the
       * pipeline in case we get back-to-back dispatch calls with the same
       * pipeline and a PIPELINE_SELECT in between.
       */
      cmd_buffer->state.compute.pipeline_dirty = true;
   }
#endif

   genX(emit_pipeline_select)(&cmd_buffer->batch, pipeline, cmd_buffer->device);

#if GFX_VER == 9
   if (devinfo->platform == INTEL_PLATFORM_GLK) {
      /* Project: DevGLK
       *
       * "This chicken bit works around a hardware issue with barrier logic
       *  encountered when switching between GPGPU and 3D pipelines.  To
       *  workaround the issue, this mode bit should be set after a pipeline
       *  is selected."
       */
      anv_batch_write_reg(&cmd_buffer->batch, GENX(SLICE_COMMON_ECO_CHICKEN1), scec1) {
         scec1.GLKBarrierMode = pipeline == GPGPU ? GLK_BARRIER_MODE_GPGPU
                                                  : GLK_BARRIER_MODE_3D_HULL;
         scec1.GLKBarrierModeMask = 1;
      }
   }
#endif

#if GFX_VER == 9
   /* Undocumented workaround, we need to reemit MEDIA_CURBE_LOAD on Gfx9 when
    * switching from 3D->GPGPU, otherwise the shader gets corrupted push
    * constants. Note that this doesn't trigger a push constant reallocation,
    * we just reprogram the same pointer.
    *
    * The issue reproduces pretty much 100% on
    * dEQP-VK.memory_model.transitive.* tests. Reducing the number of
    * iteration in the test from 50 to < 10 makes the tests flaky.
    */
   if (pipeline == GPGPU)
      cmd_buffer->state.push_constants_dirty |= VK_SHADER_STAGE_COMPUTE_BIT;
#endif
   cmd_buffer->state.current_pipeline = pipeline;
}

void
genX(flush_pipeline_select_3d)(struct anv_cmd_buffer *cmd_buffer)
{
   genX(flush_pipeline_select)(cmd_buffer, _3D);
}

void
genX(flush_pipeline_select_gpgpu)(struct anv_cmd_buffer *cmd_buffer)
{
   genX(flush_pipeline_select)(cmd_buffer, GPGPU);
}

void
genX(cmd_buffer_emit_gfx12_depth_wa)(struct anv_cmd_buffer *cmd_buffer,
                                     const struct isl_surf *surf)
{
#if INTEL_NEEDS_WA_1808121037
   const bool is_d16_1x_msaa = surf->format == ISL_FORMAT_R16_UNORM &&
                               surf->samples == 1;

   switch (cmd_buffer->state.gfx.depth_reg_mode) {
   case ANV_DEPTH_REG_MODE_HW_DEFAULT:
      if (!is_d16_1x_msaa)
         return;
      break;
   case ANV_DEPTH_REG_MODE_D16_1X_MSAA:
      if (is_d16_1x_msaa)
         return;
      break;
   case ANV_DEPTH_REG_MODE_UNKNOWN:
      break;
   }

   /* We'll change some CHICKEN registers depending on the depth surface
    * format. Do a depth flush and stall so the pipeline is not using these
    * settings while we change the registers.
    */
   anv_add_pending_pipe_bits(cmd_buffer,
                             ANV_PIPE_DEPTH_CACHE_FLUSH_BIT |
                             ANV_PIPE_DEPTH_STALL_BIT |
                             ANV_PIPE_END_OF_PIPE_SYNC_BIT,
                             "Workaround: Stop pipeline for 1808121037");
   genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);

   /* Wa_1808121037
    *
    * To avoid sporadic corruptions “Set 0x7010[9] when Depth Buffer
    * Surface Format is D16_UNORM , surface type is not NULL & 1X_MSAA”.
    */
   anv_batch_write_reg(&cmd_buffer->batch, GENX(COMMON_SLICE_CHICKEN1), reg) {
      reg.HIZPlaneOptimizationdisablebit = is_d16_1x_msaa;
      reg.HIZPlaneOptimizationdisablebitMask = true;
   }

   cmd_buffer->state.gfx.depth_reg_mode =
      is_d16_1x_msaa ? ANV_DEPTH_REG_MODE_D16_1X_MSAA :
                       ANV_DEPTH_REG_MODE_HW_DEFAULT;
#endif
}

#if GFX_VER == 9
/* From the Skylake PRM, 3DSTATE_VERTEX_BUFFERS:
 *
 *    "The VF cache needs to be invalidated before binding and then using
 *    Vertex Buffers that overlap with any previously bound Vertex Buffer
 *    (at a 64B granularity) since the last invalidation.  A VF cache
 *    invalidate is performed by setting the "VF Cache Invalidation Enable"
 *    bit in PIPE_CONTROL."
 *
 * This is implemented by carefully tracking all vertex and index buffer
 * bindings and flushing if the cache ever ends up with a range in the cache
 * that would exceed 4 GiB.  This is implemented in three parts:
 *
 *    1. genX(cmd_buffer_set_binding_for_gfx8_vb_flush)() which must be called
 *       every time a 3DSTATE_VERTEX_BUFFER packet is emitted and informs the
 *       tracking code of the new binding.  If this new binding would cause
 *       the cache to have a too-large range on the next draw call, a pipeline
 *       stall and VF cache invalidate are added to pending_pipeline_bits.
 *
 *    2. genX(cmd_buffer_apply_pipe_flushes)() resets the cache tracking to
 *       empty whenever we emit a VF invalidate.
 *
 *    3. genX(cmd_buffer_update_dirty_vbs_for_gfx8_vb_flush)() must be called
 *       after every 3DPRIMITIVE and copies the bound range into the dirty
 *       range for each used buffer.  This has to be a separate step because
 *       we don't always re-bind all buffers and so 1. can't know which
 *       buffers are actually bound.
 */
void
genX(cmd_buffer_set_binding_for_gfx8_vb_flush)(struct anv_cmd_buffer *cmd_buffer,
                                               int vb_index,
                                               struct anv_address vb_address,
                                               uint32_t vb_size)
{
   if (GFX_VER > 9)
      return;

   struct anv_vb_cache_range *bound, *dirty;
   if (vb_index == -1) {
      bound = &cmd_buffer->state.gfx.ib_bound_range;
      dirty = &cmd_buffer->state.gfx.ib_dirty_range;
   } else {
      assert(vb_index >= 0);
      assert(vb_index < ARRAY_SIZE(cmd_buffer->state.gfx.vb_bound_ranges));
      assert(vb_index < ARRAY_SIZE(cmd_buffer->state.gfx.vb_dirty_ranges));
      bound = &cmd_buffer->state.gfx.vb_bound_ranges[vb_index];
      dirty = &cmd_buffer->state.gfx.vb_dirty_ranges[vb_index];
   }

   if (anv_gfx8_9_vb_cache_range_needs_workaround(bound, dirty,
                                                  vb_address,
                                                  vb_size)) {
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_CS_STALL_BIT |
                                ANV_PIPE_VF_CACHE_INVALIDATE_BIT,
                                "vb > 32b range");
   }
}

void
genX(cmd_buffer_update_dirty_vbs_for_gfx8_vb_flush)(struct anv_cmd_buffer *cmd_buffer,
                                                    uint32_t access_type,
                                                    uint64_t vb_used)
{
   if (access_type == RANDOM) {
      /* We have an index buffer */
      struct anv_vb_cache_range *bound = &cmd_buffer->state.gfx.ib_bound_range;
      struct anv_vb_cache_range *dirty = &cmd_buffer->state.gfx.ib_dirty_range;

      anv_merge_vb_cache_range(dirty, bound);
   }

   uint64_t mask = vb_used;
   while (mask) {
      int i = u_bit_scan64(&mask);
      assert(i >= 0);
      assert(i < ARRAY_SIZE(cmd_buffer->state.gfx.vb_bound_ranges));
      assert(i < ARRAY_SIZE(cmd_buffer->state.gfx.vb_dirty_ranges));

      struct anv_vb_cache_range *bound, *dirty;
      bound = &cmd_buffer->state.gfx.vb_bound_ranges[i];
      dirty = &cmd_buffer->state.gfx.vb_dirty_ranges[i];

      anv_merge_vb_cache_range(dirty, bound);
   }
}
#endif /* GFX_VER == 9 */

/**
 * Update the pixel hashing modes that determine the balancing of PS threads
 * across subslices and slices.
 *
 * \param width Width bound of the rendering area (already scaled down if \p
 *              scale is greater than 1).
 * \param height Height bound of the rendering area (already scaled down if \p
 *               scale is greater than 1).
 * \param scale The number of framebuffer samples that could potentially be
 *              affected by an individual channel of the PS thread.  This is
 *              typically one for single-sampled rendering, but for operations
 *              like CCS resolves and fast clears a single PS invocation may
 *              update a huge number of pixels, in which case a finer
 *              balancing is desirable in order to maximally utilize the
 *              bandwidth available.  UINT_MAX can be used as shorthand for
 *              "finest hashing mode available".
 */
void
genX(cmd_buffer_emit_hashing_mode)(struct anv_cmd_buffer *cmd_buffer,
                                   unsigned width, unsigned height,
                                   unsigned scale)
{
#if GFX_VER == 9
   const struct intel_device_info *devinfo = cmd_buffer->device->info;
   const unsigned slice_hashing[] = {
      /* Because all Gfx9 platforms with more than one slice require
       * three-way subslice hashing, a single "normal" 16x16 slice hashing
       * block is guaranteed to suffer from substantial imbalance, with one
       * subslice receiving twice as much work as the other two in the
       * slice.
       *
       * The performance impact of that would be particularly severe when
       * three-way hashing is also in use for slice balancing (which is the
       * case for all Gfx9 GT4 platforms), because one of the slices
       * receives one every three 16x16 blocks in either direction, which
       * is roughly the periodicity of the underlying subslice imbalance
       * pattern ("roughly" because in reality the hardware's
       * implementation of three-way hashing doesn't do exact modulo 3
       * arithmetic, which somewhat decreases the magnitude of this effect
       * in practice).  This leads to a systematic subslice imbalance
       * within that slice regardless of the size of the primitive.  The
       * 32x32 hashing mode guarantees that the subslice imbalance within a
       * single slice hashing block is minimal, largely eliminating this
       * effect.
       */
      _32x32,
      /* Finest slice hashing mode available. */
      NORMAL
   };
   const unsigned subslice_hashing[] = {
      /* 16x16 would provide a slight cache locality benefit especially
       * visible in the sampler L1 cache efficiency of low-bandwidth
       * non-LLC platforms, but it comes at the cost of greater subslice
       * imbalance for primitives of dimensions approximately intermediate
       * between 16x4 and 16x16.
       */
      _16x4,
      /* Finest subslice hashing mode available. */
      _8x4
   };
   /* Dimensions of the smallest hashing block of a given hashing mode.  If
    * the rendering area is smaller than this there can't possibly be any
    * benefit from switching to this mode, so we optimize out the
    * transition.
    */
   const unsigned min_size[][2] = {
         { 16, 4 },
         { 8, 4 }
   };
   const unsigned idx = scale > 1;

   if (cmd_buffer->state.current_hash_scale != scale &&
       (width > min_size[idx][0] || height > min_size[idx][1])) {
      anv_add_pending_pipe_bits(cmd_buffer,
                                ANV_PIPE_CS_STALL_BIT |
                                ANV_PIPE_STALL_AT_SCOREBOARD_BIT,
                                "change pixel hash mode");
      genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);

      anv_batch_write_reg(&cmd_buffer->batch, GENX(GT_MODE), gt) {
         gt.SliceHashing = (devinfo->num_slices > 1 ? slice_hashing[idx] : 0);
         gt.SliceHashingMask = (devinfo->num_slices > 1 ? -1 : 0);
         gt.SubsliceHashing = subslice_hashing[idx];
         gt.SubsliceHashingMask = -1;
      }

      cmd_buffer->state.current_hash_scale = scale;
   }
#endif
}

static void
cmd_buffer_emit_depth_stencil(struct anv_cmd_buffer *cmd_buffer)
{
   struct anv_device *device = cmd_buffer->device;
   struct anv_cmd_graphics_state *gfx = &cmd_buffer->state.gfx;

   uint32_t *dw = anv_batch_emit_dwords(&cmd_buffer->batch,
                                        device->isl_dev.ds.size / 4);
   if (dw == NULL)
      return;

   struct isl_view isl_view = {};
   struct isl_depth_stencil_hiz_emit_info info = {
      .view = &isl_view,
      .mocs = anv_mocs(device, NULL, ISL_SURF_USAGE_DEPTH_BIT),
   };

   if (gfx->depth_att.iview != NULL) {
      isl_view = gfx->depth_att.iview->planes[0].isl;
   } else if (gfx->stencil_att.iview != NULL) {
      isl_view = gfx->stencil_att.iview->planes[0].isl;
   }

   if (gfx->view_mask) {
      assert(isl_view.array_len == 0 ||
             isl_view.array_len >= util_last_bit(gfx->view_mask));
      isl_view.array_len = util_last_bit(gfx->view_mask);
   } else {
      assert(isl_view.array_len == 0 ||
             isl_view.array_len >= util_last_bit(gfx->layer_count));
      isl_view.array_len = gfx->layer_count;
   }

   if (gfx->depth_att.iview != NULL) {
      const struct anv_image_view *iview = gfx->depth_att.iview;
      const struct anv_image *image = iview->image;

      const uint32_t depth_plane =
         anv_image_aspect_to_plane(image, VK_IMAGE_ASPECT_DEPTH_BIT);
      const struct anv_surface *depth_surface =
         &image->planes[depth_plane].primary_surface;
      const struct anv_address depth_address =
         anv_image_address(image, &depth_surface->memory_range);

      anv_reloc_list_add_bo(cmd_buffer->batch.relocs, depth_address.bo);

      info.depth_surf = &depth_surface->isl;
      info.depth_address = anv_address_physical(depth_address);
      info.mocs =
         anv_mocs(device, depth_address.bo, ISL_SURF_USAGE_DEPTH_BIT);

      info.hiz_usage = gfx->depth_att.aux_usage;
      if (info.hiz_usage != ISL_AUX_USAGE_NONE) {
         assert(isl_aux_usage_has_hiz(info.hiz_usage));

         const struct anv_surface *hiz_surface =
            &image->planes[depth_plane].aux_surface;
         const struct anv_address hiz_address =
            anv_image_address(image, &hiz_surface->memory_range);

         anv_reloc_list_add_bo(cmd_buffer->batch.relocs, hiz_address.bo);

         info.hiz_surf = &hiz_surface->isl;
         info.hiz_address = anv_address_physical(hiz_address);

         info.depth_clear_value = anv_image_hiz_clear_value(image).f32[0];
      }
   }

   if (gfx->stencil_att.iview != NULL) {
      const struct anv_image_view *iview = gfx->stencil_att.iview;
      const struct anv_image *image = iview->image;

      const uint32_t stencil_plane =
         anv_image_aspect_to_plane(image, VK_IMAGE_ASPECT_STENCIL_BIT);
      const struct anv_surface *stencil_surface =
         &image->planes[stencil_plane].primary_surface;
      const struct anv_address stencil_address =
         anv_image_address(image, &stencil_surface->memory_range);

      anv_reloc_list_add_bo(cmd_buffer->batch.relocs, stencil_address.bo);

      info.stencil_surf = &stencil_surface->isl;

      info.stencil_aux_usage = image->planes[stencil_plane].aux_usage;
      info.stencil_address = anv_address_physical(stencil_address);
      info.mocs =
         anv_mocs(device, stencil_address.bo, ISL_SURF_USAGE_STENCIL_BIT);
   }

   isl_emit_depth_stencil_hiz_s(&device->isl_dev, dw, &info);

   if (intel_needs_workaround(cmd_buffer->device->info, 1408224581) ||
       intel_needs_workaround(cmd_buffer->device->info, 14014097488) ||
       intel_needs_workaround(cmd_buffer->device->info, 14016712196)) {
      /* Wa_1408224581
       *
       * Workaround: Gfx12LP Astep only An additional pipe control with
       * post-sync = store dword operation would be required.( w/a is to have
       * an additional pipe control after the stencil state whenever the
       * surface state bits of this state is changing).
       *
       * This also seems sufficient to handle Wa_14014097488 and
       * Wa_14016712196.
       */
      genx_batch_emit_pipe_control_write(&cmd_buffer->batch, device->info,
                                         cmd_buffer->state.current_pipeline,
                                         WriteImmediateData,
                                         device->workaround_address, 0, 0);
   }

   if (info.depth_surf)
      genX(cmd_buffer_emit_gfx12_depth_wa)(cmd_buffer, info.depth_surf);

   cmd_buffer->state.gfx.hiz_enabled = isl_aux_usage_has_hiz(info.hiz_usage);
}

static void
cmd_buffer_emit_cps_control_buffer(struct anv_cmd_buffer *cmd_buffer,
                                   const struct anv_image_view *fsr_iview)
{
#if GFX_VERx10 >= 125
   struct anv_device *device = cmd_buffer->device;

   if (!device->vk.enabled_extensions.KHR_fragment_shading_rate)
      return;

   uint32_t *dw = anv_batch_emit_dwords(&cmd_buffer->batch,
                                        device->isl_dev.cpb.size / 4);
   if (dw == NULL)
      return;

   struct isl_cpb_emit_info info = { };

   if (fsr_iview) {
      const struct anv_image_binding *binding = &fsr_iview->image->bindings[0];

      anv_reloc_list_add_bo(cmd_buffer->batch.relocs, binding->address.bo);

      struct anv_address addr =
         anv_address_add(binding->address, binding->memory_range.offset);

      info.view = &fsr_iview->planes[0].isl;
      info.surf = &fsr_iview->image->planes[0].primary_surface.isl;
      info.aux_usage = fsr_iview->image->planes[0].aux_usage;
      info.address = anv_address_physical(addr);
      info.mocs =
         anv_mocs(device, fsr_iview->image->bindings[0].address.bo,
                  ISL_SURF_USAGE_CPB_BIT);
   }

   isl_emit_cpb_control_s(&device->isl_dev, dw, &info);

   /* Wa_14016712196:
    * Emit dummy pipe control after state that sends implicit depth flush.
    */
   if (intel_needs_workaround(device->info, 14016712196)) {
      genx_batch_emit_pipe_control_write(&cmd_buffer->batch, device->info,
                                         cmd_buffer->state.current_pipeline,
                                         WriteImmediateData,
                                         device->workaround_address, 0, 0);
   }

#endif /* GFX_VERx10 >= 125 */
}

static VkImageLayout
attachment_initial_layout(const VkRenderingAttachmentInfo *att)
{
   const VkRenderingAttachmentInitialLayoutInfoMESA *layout_info =
      vk_find_struct_const(att->pNext,
                           RENDERING_ATTACHMENT_INITIAL_LAYOUT_INFO_MESA);
   if (layout_info != NULL)
      return layout_info->initialLayout;

   return att->imageLayout;
}

void genX(CmdBeginRendering)(
    VkCommandBuffer                             commandBuffer,
    const VkRenderingInfo*                      pRenderingInfo)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);
   struct anv_cmd_graphics_state *gfx = &cmd_buffer->state.gfx;
   VkResult result;

   if (!anv_cmd_buffer_is_render_queue(cmd_buffer)) {
      assert(!"Trying to start a render pass on non-render queue!");
      anv_batch_set_error(&cmd_buffer->batch, VK_ERROR_UNKNOWN);
      return;
   }

   anv_measure_beginrenderpass(cmd_buffer);
   trace_intel_begin_render_pass(&cmd_buffer->trace);

   gfx->rendering_flags = pRenderingInfo->flags;
   gfx->view_mask = pRenderingInfo->viewMask;
   gfx->layer_count = pRenderingInfo->layerCount;
   gfx->samples = 0;

   if (gfx->render_area.offset.x != pRenderingInfo->renderArea.offset.x ||
       gfx->render_area.offset.y != pRenderingInfo->renderArea.offset.y ||
       gfx->render_area.extent.width != pRenderingInfo->renderArea.extent.width ||
       gfx->render_area.extent.height != pRenderingInfo->renderArea.extent.height) {
      gfx->render_area = pRenderingInfo->renderArea;
      gfx->dirty |= ANV_CMD_DIRTY_RENDER_AREA;
   }

   const bool is_multiview = gfx->view_mask != 0;
   const VkRect2D render_area = gfx->render_area;
   const uint32_t layers =
      is_multiview ? util_last_bit(gfx->view_mask) : gfx->layer_count;

   /* The framebuffer size is at least large enough to contain the render
    * area.  Because a zero renderArea is possible, we MAX with 1.
    */
   struct isl_extent3d fb_size = {
      .w = MAX2(1, render_area.offset.x + render_area.extent.width),
      .h = MAX2(1, render_area.offset.y + render_area.extent.height),
      .d = layers,
   };

   const uint32_t color_att_count = pRenderingInfo->colorAttachmentCount;

   result = anv_cmd_buffer_init_attachments(cmd_buffer, color_att_count);
   if (result != VK_SUCCESS)
      return;

   genX(flush_pipeline_select_3d)(cmd_buffer);

   UNUSED bool render_target_change = false;
   for (uint32_t i = 0; i < gfx->color_att_count; i++) {
      if (pRenderingInfo->pColorAttachments[i].imageView == VK_NULL_HANDLE) {
         render_target_change |= gfx->color_att[i].iview != NULL;

         gfx->color_att[i].vk_format = VK_FORMAT_UNDEFINED;
         gfx->color_att[i].iview = NULL;
         gfx->color_att[i].layout = VK_IMAGE_LAYOUT_UNDEFINED;
         gfx->color_att[i].aux_usage = ISL_AUX_USAGE_NONE;
         continue;
      }

      const VkRenderingAttachmentInfo *att =
         &pRenderingInfo->pColorAttachments[i];
      ANV_FROM_HANDLE(anv_image_view, iview, att->imageView);
      const VkImageLayout initial_layout = attachment_initial_layout(att);

      assert(render_area.offset.x + render_area.extent.width <=
             iview->vk.extent.width);
      assert(render_area.offset.y + render_area.extent.height <=
             iview->vk.extent.height);
      assert(layers <= iview->vk.layer_count);

      fb_size.w = MAX2(fb_size.w, iview->vk.extent.width);
      fb_size.h = MAX2(fb_size.h, iview->vk.extent.height);

      assert(gfx->samples == 0 || gfx->samples == iview->vk.image->samples);
      gfx->samples |= iview->vk.image->samples;

      enum isl_aux_usage aux_usage =
         anv_layout_to_aux_usage(cmd_buffer->device->info,
                                 iview->image,
                                 iview->vk.aspects,
                                 VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                                 att->imageLayout,
                                 cmd_buffer->queue_family->queueFlags);

      render_target_change |= gfx->color_att[i].iview != iview;

      gfx->color_att[i].vk_format = iview->vk.format;
      gfx->color_att[i].iview = iview;
      gfx->color_att[i].layout = att->imageLayout;
      gfx->color_att[i].aux_usage = aux_usage;

      union isl_color_value fast_clear_color = { .u32 = { 0, } };

      if (att->loadOp == VK_ATTACHMENT_LOAD_OP_CLEAR &&
          !(gfx->rendering_flags & VK_RENDERING_RESUMING_BIT)) {
         uint32_t clear_view_mask = pRenderingInfo->viewMask;
         VkClearRect clear_rect = {
            .rect = render_area,
            .baseArrayLayer = iview->vk.base_array_layer,
            .layerCount = layers,
         };
         const union isl_color_value clear_color =
            vk_to_isl_color_with_format(att->clearValue.color,
                                        iview->planes[0].isl.format);

         /* We only support fast-clears on the first layer */
         const bool fast_clear =
            (!is_multiview || (gfx->view_mask & 1)) &&
            anv_can_fast_clear_color(cmd_buffer, iview->image,
                                     iview->vk.aspects,
                                     iview->vk.base_mip_level,
                                     &clear_rect, att->imageLayout,
                                     iview->planes[0].isl.format,
                                     clear_color);

         if (att->imageLayout != initial_layout) {
            assert(render_area.offset.x == 0 && render_area.offset.y == 0 &&
                   render_area.extent.width == iview->vk.extent.width &&
                   render_area.extent.height == iview->vk.extent.height);
            if (is_multiview) {
               u_foreach_bit(view, gfx->view_mask) {
                  transition_color_buffer(cmd_buffer, iview->image,
                                          iview->vk.aspects,
                                          iview->vk.base_mip_level, 1,
                                          iview->vk.base_array_layer + view,
                                          1, /* layer_count */
                                          initial_layout, att->imageLayout,
                                          VK_QUEUE_FAMILY_IGNORED,
                                          VK_QUEUE_FAMILY_IGNORED,
                                          fast_clear,
                                          false /* acquire_unmodified */);
               }
            } else {
               transition_color_buffer(cmd_buffer, iview->image,
                                       iview->vk.aspects,
                                       iview->vk.base_mip_level, 1,
                                       iview->vk.base_array_layer,
                                       gfx->layer_count,
                                       initial_layout, att->imageLayout,
                                       VK_QUEUE_FAMILY_IGNORED,
                                       VK_QUEUE_FAMILY_IGNORED,
                                       fast_clear,
                                       false /* acquire_unmodified */);
            }
         }

         if (fast_clear) {
            /* We only support fast-clears on the first layer */
            assert(iview->vk.base_mip_level == 0 &&
                   iview->vk.base_array_layer == 0);

            fast_clear_color = clear_color;

            if (iview->image->vk.samples == 1) {
               anv_image_ccs_op(cmd_buffer, iview->image,
                                iview->planes[0].isl.format,
                                iview->planes[0].isl.swizzle,
                                iview->vk.aspects,
                                0, 0, 1, ISL_AUX_OP_FAST_CLEAR,
                                &fast_clear_color,
                                false);
            } else {
               anv_image_mcs_op(cmd_buffer, iview->image,
                                iview->planes[0].isl.format,
                                iview->planes[0].isl.swizzle,
                                iview->vk.aspects,
                                0, 1, ISL_AUX_OP_FAST_CLEAR,
                                &fast_clear_color,
                                false);
            }
            clear_view_mask &= ~1u;
            clear_rect.baseArrayLayer++;
            clear_rect.layerCount--;
#if GFX_VER < 20
            genX(set_fast_clear_state)(cmd_buffer, iview->image,
                                       iview->planes[0].isl.format,
                                       iview->planes[0].isl.swizzle,
                                       clear_color);
#endif
         }

         if (is_multiview) {
            u_foreach_bit(view, clear_view_mask) {
               anv_image_clear_color(cmd_buffer, iview->image,
                                     iview->vk.aspects,
                                     aux_usage,
                                     iview->planes[0].isl.format,
                                     iview->planes[0].isl.swizzle,
                                     iview->vk.base_mip_level,
                                     iview->vk.base_array_layer + view, 1,
                                     render_area, clear_color);
            }
         } else if (clear_rect.layerCount > 0) {
            anv_image_clear_color(cmd_buffer, iview->image,
                                  iview->vk.aspects,
                                  aux_usage,
                                  iview->planes[0].isl.format,
                                  iview->planes[0].isl.swizzle,
                                  iview->vk.base_mip_level,
                                  clear_rect.baseArrayLayer,
                                  clear_rect.layerCount,
                                  render_area, clear_color);
         }
      } else {
         /* If not LOAD_OP_CLEAR, we shouldn't have a layout transition. */
         assert(att->imageLayout == initial_layout);
      }

      struct isl_view isl_view = iview->planes[0].isl;
      if (pRenderingInfo->viewMask) {
         assert(isl_view.array_len >= util_last_bit(pRenderingInfo->viewMask));
         isl_view.array_len = util_last_bit(pRenderingInfo->viewMask);
      } else {
         assert(isl_view.array_len >= pRenderingInfo->layerCount);
         isl_view.array_len = pRenderingInfo->layerCount;
      }

      anv_image_fill_surface_state(cmd_buffer->device,
                                   iview->image,
                                   iview->vk.aspects,
                                   &isl_view,
                                   ISL_SURF_USAGE_RENDER_TARGET_BIT,
                                   aux_usage, &fast_clear_color,
                                   0, /* anv_image_view_state_flags */
                                   &gfx->color_att[i].surface_state);

      add_surface_state_relocs(cmd_buffer, &gfx->color_att[i].surface_state);

      if (GFX_VER < 10 &&
          (att->loadOp == VK_ATTACHMENT_LOAD_OP_LOAD ||
           render_area.extent.width != iview->vk.extent.width ||
           render_area.extent.height != iview->vk.extent.height ||
           (gfx->rendering_flags & VK_RENDERING_RESUMING_BIT)) &&
          iview->image->planes[0].aux_usage != ISL_AUX_USAGE_NONE &&
          iview->planes[0].isl.base_level == 0 &&
          iview->planes[0].isl.base_array_layer == 0) {
         struct anv_state surf_state = gfx->color_att[i].surface_state.state;
         genX(cmd_buffer_load_clear_color)(cmd_buffer, surf_state, iview);
      }

      if (att->resolveMode != VK_RESOLVE_MODE_NONE) {
         gfx->color_att[i].resolve_mode = att->resolveMode;
         gfx->color_att[i].resolve_iview =
            anv_image_view_from_handle(att->resolveImageView);
         gfx->color_att[i].resolve_layout = att->resolveImageLayout;
      }
   }

   anv_cmd_graphic_state_update_has_uint_rt(gfx);

   const struct anv_image_view *fsr_iview = NULL;
   const VkRenderingFragmentShadingRateAttachmentInfoKHR *fsr_att =
      vk_find_struct_const(pRenderingInfo->pNext,
                           RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR);
   if (fsr_att != NULL && fsr_att->imageView != VK_NULL_HANDLE) {
      fsr_iview = anv_image_view_from_handle(fsr_att->imageView);
      /* imageLayout and shadingRateAttachmentTexelSize are ignored */
   }

   const struct anv_image_view *ds_iview = NULL;
   const VkRenderingAttachmentInfo *d_att = pRenderingInfo->pDepthAttachment;
   const VkRenderingAttachmentInfo *s_att = pRenderingInfo->pStencilAttachment;
   if ((d_att != NULL && d_att->imageView != VK_NULL_HANDLE) ||
       (s_att != NULL && s_att->imageView != VK_NULL_HANDLE)) {
      const struct anv_image_view *d_iview = NULL, *s_iview = NULL;
      VkImageLayout depth_layout = VK_IMAGE_LAYOUT_UNDEFINED;
      VkImageLayout stencil_layout = VK_IMAGE_LAYOUT_UNDEFINED;
      VkImageLayout initial_depth_layout = VK_IMAGE_LAYOUT_UNDEFINED;
      VkImageLayout initial_stencil_layout = VK_IMAGE_LAYOUT_UNDEFINED;
      enum isl_aux_usage depth_aux_usage = ISL_AUX_USAGE_NONE;
      enum isl_aux_usage stencil_aux_usage = ISL_AUX_USAGE_NONE;
      VkClearDepthStencilValue clear_value = {};

      if (d_att != NULL && d_att->imageView != VK_NULL_HANDLE) {
         d_iview = anv_image_view_from_handle(d_att->imageView);
         initial_depth_layout = attachment_initial_layout(d_att);
         depth_layout = d_att->imageLayout;
         depth_aux_usage =
            anv_layout_to_aux_usage(cmd_buffer->device->info,
                                    d_iview->image,
                                    VK_IMAGE_ASPECT_DEPTH_BIT,
                                    VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
                                    depth_layout,
                                    cmd_buffer->queue_family->queueFlags);
         clear_value.depth = d_att->clearValue.depthStencil.depth;
      }

      if (s_att != NULL && s_att->imageView != VK_NULL_HANDLE) {
         s_iview = anv_image_view_from_handle(s_att->imageView);
         initial_stencil_layout = attachment_initial_layout(s_att);
         stencil_layout = s_att->imageLayout;
         stencil_aux_usage =
            anv_layout_to_aux_usage(cmd_buffer->device->info,
                                    s_iview->image,
                                    VK_IMAGE_ASPECT_STENCIL_BIT,
                                    VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
                                    stencil_layout,
                                    cmd_buffer->queue_family->queueFlags);
         clear_value.stencil = s_att->clearValue.depthStencil.stencil;
      }

      assert(s_iview == NULL || d_iview == NULL || s_iview == d_iview);
      ds_iview = d_iview != NULL ? d_iview : s_iview;
      assert(ds_iview != NULL);

      assert(render_area.offset.x + render_area.extent.width <=
             ds_iview->vk.extent.width);
      assert(render_area.offset.y + render_area.extent.height <=
             ds_iview->vk.extent.height);
      assert(layers <= ds_iview->vk.layer_count);

      fb_size.w = MAX2(fb_size.w, ds_iview->vk.extent.width);
      fb_size.h = MAX2(fb_size.h, ds_iview->vk.extent.height);

      assert(gfx->samples == 0 || gfx->samples == ds_iview->vk.image->samples);
      gfx->samples |= ds_iview->vk.image->samples;

      VkImageAspectFlags clear_aspects = 0;
      if (d_iview != NULL && d_att->loadOp == VK_ATTACHMENT_LOAD_OP_CLEAR &&
          !(gfx->rendering_flags & VK_RENDERING_RESUMING_BIT))
         clear_aspects |= VK_IMAGE_ASPECT_DEPTH_BIT;
      if (s_iview != NULL && s_att->loadOp == VK_ATTACHMENT_LOAD_OP_CLEAR &&
          !(gfx->rendering_flags & VK_RENDERING_RESUMING_BIT))
         clear_aspects |= VK_IMAGE_ASPECT_STENCIL_BIT;

      if (clear_aspects != 0) {
         const bool hiz_clear =
            anv_can_hiz_clear_image(cmd_buffer, ds_iview->image,
                                    depth_layout, clear_aspects,
                                    clear_value.depth,
                                    render_area,
                                    ds_iview->vk.base_mip_level);

         if (depth_layout != initial_depth_layout) {
            assert(render_area.offset.x == 0 && render_area.offset.y == 0 &&
                   render_area.extent.width == d_iview->vk.extent.width &&
                   render_area.extent.height == d_iview->vk.extent.height);

            if (is_multiview) {
               u_foreach_bit(view, gfx->view_mask) {
                  transition_depth_buffer(cmd_buffer, d_iview->image,
                                          d_iview->vk.base_mip_level, 1,
                                          d_iview->vk.base_array_layer + view,
                                          1 /* layer_count */,
                                          initial_depth_layout, depth_layout,
                                          hiz_clear);
               }
            } else {
               transition_depth_buffer(cmd_buffer, d_iview->image,
                                       d_iview->vk.base_mip_level, 1,
                                       d_iview->vk.base_array_layer,
                                       gfx->layer_count,
                                       initial_depth_layout, depth_layout,
                                       hiz_clear);
            }
         }

         if (stencil_layout != initial_stencil_layout) {
            assert(render_area.offset.x == 0 && render_area.offset.y == 0 &&
                   render_area.extent.width == s_iview->vk.extent.width &&
                   render_area.extent.height == s_iview->vk.extent.height);

            if (is_multiview) {
               u_foreach_bit(view, gfx->view_mask) {
                  transition_stencil_buffer(cmd_buffer, s_iview->image,
                                            s_iview->vk.base_mip_level, 1,
                                            s_iview->vk.base_array_layer + view,
                                            1 /* layer_count */,
                                            initial_stencil_layout,
                                            stencil_layout,
                                            hiz_clear);
               }
            } else {
               transition_stencil_buffer(cmd_buffer, s_iview->image,
                                         s_iview->vk.base_mip_level, 1,
                                         s_iview->vk.base_array_layer,
                                         gfx->layer_count,
                                         initial_stencil_layout,
                                         stencil_layout,
                                         hiz_clear);
            }
         }

         if (is_multiview) {
            u_foreach_bit(view, gfx->view_mask) {
               uint32_t level = ds_iview->vk.base_mip_level;
               uint32_t layer = ds_iview->vk.base_array_layer + view;

               if (hiz_clear) {
                  anv_image_hiz_clear(cmd_buffer, ds_iview->image,
                                      clear_aspects,
                                      level, layer, 1,
                                      render_area, &clear_value);
               } else {
                  anv_image_clear_depth_stencil(cmd_buffer, ds_iview->image,
                                                clear_aspects,
                                                depth_aux_usage,
                                                level, layer, 1,
                                                render_area, &clear_value);
               }
            }
         } else {
            uint32_t level = ds_iview->vk.base_mip_level;
            uint32_t base_layer = ds_iview->vk.base_array_layer;
            uint32_t layer_count = gfx->layer_count;

            if (hiz_clear) {
               anv_image_hiz_clear(cmd_buffer, ds_iview->image,
                                   clear_aspects,
                                   level, base_layer, layer_count,
                                   render_area, &clear_value);
            } else {
               anv_image_clear_depth_stencil(cmd_buffer, ds_iview->image,
                                             clear_aspects,
                                             depth_aux_usage,
                                             level, base_layer, layer_count,
                                             render_area, &clear_value);
            }
         }
      } else {
         /* If not LOAD_OP_CLEAR, we shouldn't have a layout transition. */
         assert(depth_layout == initial_depth_layout);
         assert(stencil_layout == initial_stencil_layout);
      }

      if (d_iview != NULL) {
         gfx->depth_att.vk_format = d_iview->vk.format;
         gfx->depth_att.iview = d_iview;
         gfx->depth_att.layout = depth_layout;
         gfx->depth_att.aux_usage = depth_aux_usage;
         if (d_att != NULL && d_att->resolveMode != VK_RESOLVE_MODE_NONE) {
            assert(d_att->resolveImageView != VK_NULL_HANDLE);
            gfx->depth_att.resolve_mode = d_att->resolveMode;
            gfx->depth_att.resolve_iview =
               anv_image_view_from_handle(d_att->resolveImageView);
            gfx->depth_att.resolve_layout = d_att->resolveImageLayout;
         }
      }

      if (s_iview != NULL) {
         gfx->stencil_att.vk_format = s_iview->vk.format;
         gfx->stencil_att.iview = s_iview;
         gfx->stencil_att.layout = stencil_layout;
         gfx->stencil_att.aux_usage = stencil_aux_usage;
         if (s_att->resolveMode != VK_RESOLVE_MODE_NONE) {
            assert(s_att->resolveImageView != VK_NULL_HANDLE);
            gfx->stencil_att.resolve_mode = s_att->resolveMode;
            gfx->stencil_att.resolve_iview =
               anv_image_view_from_handle(s_att->resolveImageView);
            gfx->stencil_att.resolve_layout = s_att->resolveImageLayout;
         }
      }
   }

   /* Finally, now that we know the right size, set up the null surface */
   assert(util_bitcount(gfx->samples) <= 1);
   isl_null_fill_state(&cmd_buffer->device->isl_dev,
                       gfx->null_surface_state.map,
                       .size = fb_size);

   for (uint32_t i = 0; i < gfx->color_att_count; i++) {
      if (pRenderingInfo->pColorAttachments[i].imageView != VK_NULL_HANDLE)
         continue;

      isl_null_fill_state(&cmd_buffer->device->isl_dev,
                          gfx->color_att[i].surface_state.state.map,
                          .size = fb_size);
   }

   /****** We can now start emitting code to begin the render pass ******/

   gfx->dirty |= ANV_CMD_DIRTY_RENDER_TARGETS;

   /* It is possible to start a render pass with an old pipeline.  Because the
    * render pass and subpass index are both baked into the pipeline, this is
    * highly unlikely.  In order to do so, it requires that you have a render
    * pass with a single subpass and that you use that render pass twice
    * back-to-back and use the same pipeline at the start of the second render
    * pass as at the end of the first.  In order to avoid unpredictable issues
    * with this edge case, we just dirty the pipeline at the start of every
    * subpass.
    */
   gfx->dirty |= ANV_CMD_DIRTY_ALL_SHADERS(cmd_buffer->device);

#if GFX_VER >= 11
   if (render_target_change) {
      /* The PIPE_CONTROL command description says:
      *
      *    "Whenever a Binding Table Index (BTI) used by a Render Target Message
      *     points to a different RENDER_SURFACE_STATE, SW must issue a Render
      *     Target Cache Flush by enabling this bit. When render target flush
      *     is set due to new association of BTI, PS Scoreboard Stall bit must
      *     be set in this packet."
      *
      * We assume that a new BeginRendering is always changing the RTs, which
      * may not be true and cause excessive flushing.  We can trivially skip it
      * in the case that there are no RTs (depth-only rendering), though.
      */
      anv_add_pending_pipe_bits(cmd_buffer,
                              ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT |
                              ANV_PIPE_STALL_AT_SCOREBOARD_BIT,
                              "change RT");
   }
#endif

   cmd_buffer_emit_depth_stencil(cmd_buffer);

   cmd_buffer_emit_cps_control_buffer(cmd_buffer, fsr_iview);
}

static void
cmd_buffer_mark_attachment_written(struct anv_cmd_buffer *cmd_buffer,
                                   struct anv_attachment *att,
                                   VkImageAspectFlagBits aspect)
{
#if GFX_VER < 20
   struct anv_cmd_graphics_state *gfx = &cmd_buffer->state.gfx;
   const struct anv_image_view *iview = att->iview;

   if (iview == NULL)
      return;

   if (aspect == 0)
      aspect = iview->vk.aspects;

   if (gfx->view_mask == 0) {
      genX(cmd_buffer_mark_image_written)(cmd_buffer, iview->image,
                                          aspect, att->aux_usage,
                                          iview->planes[0].isl.base_level,
                                          iview->planes[0].isl.base_array_layer,
                                          gfx->layer_count);
   } else {
      uint32_t res_view_mask = gfx->view_mask;
      while (res_view_mask) {
         int i = u_bit_scan(&res_view_mask);

         const uint32_t level = iview->planes[0].isl.base_level;
         const uint32_t layer = iview->planes[0].isl.base_array_layer + i;

         genX(cmd_buffer_mark_image_written)(cmd_buffer, iview->image,
                                             aspect, att->aux_usage,
                                             level, layer, 1);
      }
   }
#endif
}

void genX(CmdEndRendering)(
    VkCommandBuffer                             commandBuffer)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);
   struct anv_cmd_graphics_state *gfx = &cmd_buffer->state.gfx;

   if (anv_batch_has_error(&cmd_buffer->batch))
      return;

   const bool is_multiview = gfx->view_mask != 0;
   const uint32_t layers =
      is_multiview ? util_last_bit(gfx->view_mask) : gfx->layer_count;

   for (uint32_t i = 0; i < gfx->color_att_count; i++) {
      cmd_buffer_mark_attachment_written(cmd_buffer, &gfx->color_att[i], 0);
   }

   cmd_buffer_mark_attachment_written(cmd_buffer, &gfx->depth_att,
                                       VK_IMAGE_ASPECT_DEPTH_BIT);

   cmd_buffer_mark_attachment_written(cmd_buffer, &gfx->stencil_att,
                                       VK_IMAGE_ASPECT_STENCIL_BIT);


   if (!(gfx->rendering_flags & VK_RENDERING_SUSPENDING_BIT)) {
      bool has_color_resolve = false;
      UNUSED bool has_sparse_color_resolve = false;

      for (uint32_t i = 0; i < gfx->color_att_count; i++) {
         if (gfx->color_att[i].resolve_mode != VK_RESOLVE_MODE_NONE) {
            has_color_resolve = true;
            has_sparse_color_resolve |=
               anv_image_is_sparse(gfx->color_att[i].iview->image);
         }
      }

      if (has_color_resolve) {
         /* We are about to do some MSAA resolves.  We need to flush so that
          * the result of writes to the MSAA color attachments show up in the
          * sampler when we blit to the single-sampled resolve target.
          */
         anv_add_pending_pipe_bits(cmd_buffer,
                                   ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT |
                                   ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT,
                                   "MSAA resolve");
      }

      const bool has_depth_resolve =
         gfx->depth_att.resolve_mode != VK_RESOLVE_MODE_NONE;
      const bool has_stencil_resolve =
         gfx->stencil_att.resolve_mode != VK_RESOLVE_MODE_NONE;

      if (has_depth_resolve || has_stencil_resolve) {
         /* We are about to do some MSAA resolves.  We need to flush so that
          * the result of writes to the MSAA depth attachments show up in the
          * sampler when we blit to the single-sampled resolve target.
          */
         anv_add_pending_pipe_bits(cmd_buffer,
                                 ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT |
                                 ANV_PIPE_DEPTH_CACHE_FLUSH_BIT,
                                 "MSAA resolve");
      }

#if GFX_VER < 20
      const bool has_sparse_depth_resolve =
         has_depth_resolve &&
         anv_image_is_sparse(gfx->depth_att.iview->image);
      const bool has_sparse_stencil_resolve =
         has_stencil_resolve &&
         anv_image_is_sparse(gfx->stencil_att.iview->image);
      /* Our HW implementation of the sparse feature prior to Xe2 lives in the
       * GAM unit (interface between all the GPU caches and external memory).
       * As a result writes to NULL bound images & buffers that should be
       * ignored are actually still visible in the caches. The only way for us
       * to get correct NULL bound regions to return 0s is to evict the caches
       * to force the caches to be repopulated with 0s.
       *
       * Our understanding is that Xe2 started to tag the L3 cache with some
       * kind physical address information rather. It is therefore able to
       * detect that a cache line in the cache is going to a null tile and so
       * the L3 cache also has a sparse compatible behavior and we don't need
       * to flush anymore.
       */
      if (has_sparse_color_resolve || has_sparse_depth_resolve ||
          has_sparse_stencil_resolve) {
         /* If the resolve image is sparse we need some extra bits to make
          * sure unbound regions read 0, as residencyNonResidentStrict
          * mandates.
          */
         anv_add_pending_pipe_bits(cmd_buffer, ANV_PIPE_TILE_CACHE_FLUSH_BIT,
                                   "sparse MSAA resolve");
      }
#endif

      for (uint32_t i = 0; i < gfx->color_att_count; i++) {
         const struct anv_attachment *att = &gfx->color_att[i];
         if (att->resolve_mode == VK_RESOLVE_MODE_NONE)
            continue;

         anv_attachment_msaa_resolve(cmd_buffer, att, att->layout,
                                     VK_IMAGE_ASPECT_COLOR_BIT);
      }

      if (has_depth_resolve) {
         const struct anv_image_view *src_iview = gfx->depth_att.iview;

         /* MSAA resolves sample from the source attachment.  Transition the
          * depth attachment first to get rid of any HiZ that we may not be
          * able to handle.
          */
         transition_depth_buffer(cmd_buffer, src_iview->image, 0, 1,
                                 src_iview->planes[0].isl.base_array_layer,
                                 layers,
                                 gfx->depth_att.layout,
                                 VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                 false /* will_full_fast_clear */);

         anv_attachment_msaa_resolve(cmd_buffer, &gfx->depth_att,
                                     VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                     VK_IMAGE_ASPECT_DEPTH_BIT);

         /* Transition the source back to the original layout.  This seems a
          * bit inefficient but, since HiZ resolves aren't destructive, going
          * from less HiZ to more is generally a no-op.
          */
         transition_depth_buffer(cmd_buffer, src_iview->image, 0, 1,
                                 src_iview->planes[0].isl.base_array_layer,
                                 layers,
                                 VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                 gfx->depth_att.layout,
                                 false /* will_full_fast_clear */);
      }

      if (has_stencil_resolve) {
         anv_attachment_msaa_resolve(cmd_buffer, &gfx->stencil_att,
                                     gfx->stencil_att.layout,
                                     VK_IMAGE_ASPECT_STENCIL_BIT);
      }
   }

   trace_intel_end_render_pass(&cmd_buffer->trace,
                               (uintptr_t)(vk_command_buffer_to_handle(&cmd_buffer->vk)),
                               gfx->render_area.extent.width,
                               gfx->render_area.extent.height,
                               gfx->color_att_count,
                               gfx->samples);

   anv_cmd_buffer_reset_rendering(cmd_buffer);
}

void
genX(cmd_emit_conditional_render_predicate)(struct anv_cmd_buffer *cmd_buffer)
{
   struct mi_builder b;
   mi_builder_init(&b, cmd_buffer->device->info, &cmd_buffer->batch);

   mi_store(&b, mi_reg64(MI_PREDICATE_SRC0),
                mi_reg32(ANV_PREDICATE_RESULT_REG));
   mi_store(&b, mi_reg64(MI_PREDICATE_SRC1), mi_imm(0));

   anv_batch_emit(&cmd_buffer->batch, GENX(MI_PREDICATE), mip) {
      mip.LoadOperation    = LOAD_LOADINV;
      mip.CombineOperation = COMBINE_SET;
      mip.CompareOperation = COMPARE_SRCS_EQUAL;
   }
}

void genX(CmdBeginConditionalRenderingEXT)(
   VkCommandBuffer                             commandBuffer,
   const VkConditionalRenderingBeginInfoEXT*   pConditionalRenderingBegin)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);
   ANV_FROM_HANDLE(anv_buffer, buffer, pConditionalRenderingBegin->buffer);
   struct anv_cmd_state *cmd_state = &cmd_buffer->state;
   struct anv_address value_address =
      anv_address_add(buffer->address, pConditionalRenderingBegin->offset);

   const bool isInverted = pConditionalRenderingBegin->flags &
                           VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT;

   cmd_state->conditional_render_enabled = true;

   genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);

   struct mi_builder b;
   mi_builder_init(&b, cmd_buffer->device->info, &cmd_buffer->batch);
   const uint32_t mocs = anv_mocs_for_address(cmd_buffer->device, &value_address);
   mi_builder_set_mocs(&b, mocs);

   /* Section 19.4 of the Vulkan 1.1.85 spec says:
    *
    *    If the value of the predicate in buffer memory changes
    *    while conditional rendering is active, the rendering commands
    *    may be discarded in an implementation-dependent way.
    *    Some implementations may latch the value of the predicate
    *    upon beginning conditional rendering while others
    *    may read it before every rendering command.
    *
    * So it's perfectly fine to read a value from the buffer once.
    */
   struct mi_value value =  mi_mem32(value_address);

   /* Precompute predicate result, it is necessary to support secondary
    * command buffers since it is unknown if conditional rendering is
    * inverted when populating them.
    */
   mi_store(&b, mi_reg64(ANV_PREDICATE_RESULT_REG),
                isInverted ? mi_uge(&b, mi_imm(0), value) :
                             mi_ult(&b, mi_imm(0), value));
}

void genX(CmdEndConditionalRenderingEXT)(
	VkCommandBuffer                             commandBuffer)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);
   struct anv_cmd_state *cmd_state = &cmd_buffer->state;

   cmd_state->conditional_render_enabled = false;
}

/* Set of stage bits for which are pipelined, i.e. they get queued
 * by the command streamer for later execution.
 */
#define ANV_PIPELINE_STAGE_PIPELINED_BITS \
   ~(VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT | \
     VK_PIPELINE_STAGE_2_DRAW_INDIRECT_BIT | \
     VK_PIPELINE_STAGE_2_HOST_BIT | \
     VK_PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT)

void genX(CmdSetEvent2)(
    VkCommandBuffer                             commandBuffer,
    VkEvent                                     _event,
    const VkDependencyInfo*                     pDependencyInfo)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);
   ANV_FROM_HANDLE(anv_event, event, _event);

   switch (cmd_buffer->batch.engine_class) {
   case INTEL_ENGINE_CLASS_VIDEO:
   case INTEL_ENGINE_CLASS_COPY:
      anv_batch_emit(&cmd_buffer->batch, GENX(MI_FLUSH_DW), flush) {
         flush.PostSyncOperation = WriteImmediateData;
         flush.Address = anv_state_pool_state_address(
            &cmd_buffer->device->dynamic_state_pool,
            event->state);
         flush.ImmediateData = VK_EVENT_SET;
      }
      break;

   case INTEL_ENGINE_CLASS_RENDER:
   case INTEL_ENGINE_CLASS_COMPUTE: {
      VkPipelineStageFlags2 src_stages = 0;

      for (uint32_t i = 0; i < pDependencyInfo->memoryBarrierCount; i++)
         src_stages |= pDependencyInfo->pMemoryBarriers[i].srcStageMask;
      for (uint32_t i = 0; i < pDependencyInfo->bufferMemoryBarrierCount; i++)
         src_stages |= pDependencyInfo->pBufferMemoryBarriers[i].srcStageMask;
      for (uint32_t i = 0; i < pDependencyInfo->imageMemoryBarrierCount; i++)
         src_stages |= pDependencyInfo->pImageMemoryBarriers[i].srcStageMask;

      cmd_buffer->state.pending_pipe_bits |= ANV_PIPE_POST_SYNC_BIT;
      genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);

      enum anv_pipe_bits pc_bits = 0;
      if (src_stages & ANV_PIPELINE_STAGE_PIPELINED_BITS) {
         pc_bits |= ANV_PIPE_STALL_AT_SCOREBOARD_BIT;
         pc_bits |= ANV_PIPE_CS_STALL_BIT;
      }

      genx_batch_emit_pipe_control_write
         (&cmd_buffer->batch, cmd_buffer->device->info,
          cmd_buffer->state.current_pipeline, WriteImmediateData,
          anv_state_pool_state_address(&cmd_buffer->device->dynamic_state_pool,
                                       event->state),
          VK_EVENT_SET, pc_bits);
      break;
   }

   default:
      UNREACHABLE("Invalid engine class");
   }
}

void genX(CmdResetEvent2)(
    VkCommandBuffer                             commandBuffer,
    VkEvent                                     _event,
    VkPipelineStageFlags2                       stageMask)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);
   ANV_FROM_HANDLE(anv_event, event, _event);

   switch (cmd_buffer->batch.engine_class) {
   case INTEL_ENGINE_CLASS_VIDEO:
   case INTEL_ENGINE_CLASS_COPY:
      anv_batch_emit(&cmd_buffer->batch, GENX(MI_FLUSH_DW), flush) {
         flush.PostSyncOperation = WriteImmediateData;
         flush.Address = anv_state_pool_state_address(
            &cmd_buffer->device->dynamic_state_pool,
            event->state);
         flush.ImmediateData = VK_EVENT_RESET;
      }
      break;

   case INTEL_ENGINE_CLASS_RENDER:
   case INTEL_ENGINE_CLASS_COMPUTE: {
      cmd_buffer->state.pending_pipe_bits |= ANV_PIPE_POST_SYNC_BIT;
      genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);

      enum anv_pipe_bits pc_bits = 0;
      if (stageMask & ANV_PIPELINE_STAGE_PIPELINED_BITS) {
         pc_bits |= ANV_PIPE_STALL_AT_SCOREBOARD_BIT;
         pc_bits |= ANV_PIPE_CS_STALL_BIT;
      }

      genx_batch_emit_pipe_control_write
         (&cmd_buffer->batch, cmd_buffer->device->info,
          cmd_buffer->state.current_pipeline, WriteImmediateData,
          anv_state_pool_state_address(&cmd_buffer->device->dynamic_state_pool,
                                       event->state),
          VK_EVENT_RESET,
          pc_bits);
      break;
   }

   default:
      UNREACHABLE("Invalid engine class");
   }
}

void genX(CmdWaitEvents2)(
    VkCommandBuffer                             commandBuffer,
    uint32_t                                    eventCount,
    const VkEvent*                              pEvents,
    const VkDependencyInfo*                     pDependencyInfos)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);

   for (uint32_t i = 0; i < eventCount; i++) {
      ANV_FROM_HANDLE(anv_event, event, pEvents[i]);

      anv_batch_emit(&cmd_buffer->batch, GENX(MI_SEMAPHORE_WAIT), sem) {
         sem.WaitMode            = PollingMode;
         sem.CompareOperation    = COMPARE_SAD_EQUAL_SDD;
         sem.SemaphoreDataDword  = VK_EVENT_SET;
         sem.SemaphoreAddress    = anv_state_pool_state_address(
            &cmd_buffer->device->dynamic_state_pool,
            event->state);
      }
   }

   cmd_buffer_barrier(cmd_buffer, eventCount, pDependencyInfos, "wait event");
}

VkResult genX(CmdSetPerformanceOverrideINTEL)(
    VkCommandBuffer                             commandBuffer,
    const VkPerformanceOverrideInfoINTEL*       pOverrideInfo)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);

   switch (pOverrideInfo->type) {
   case VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL: {
      anv_batch_write_reg(&cmd_buffer->batch, GENX(CS_DEBUG_MODE2), csdm2) {
         csdm2._3DRenderingInstructionDisable = pOverrideInfo->enable;
         csdm2.MediaInstructionDisable = pOverrideInfo->enable;
         csdm2._3DRenderingInstructionDisableMask = true;
         csdm2.MediaInstructionDisableMask = true;
      }
      break;
   }

   case VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL:
      if (pOverrideInfo->enable) {
         /* FLUSH ALL THE THINGS! As requested by the MDAPI team. */
         anv_add_pending_pipe_bits(cmd_buffer,
                                   ANV_PIPE_BARRIER_FLUSH_BITS |
                                   ANV_PIPE_INVALIDATE_BITS,
                                   "perf counter isolation");
         genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);
      }
      break;

   default:
      UNREACHABLE("Invalid override");
   }

   return VK_SUCCESS;
}

VkResult genX(CmdSetPerformanceStreamMarkerINTEL)(
    VkCommandBuffer                             commandBuffer,
    const VkPerformanceStreamMarkerInfoINTEL*   pMarkerInfo)
{
   /* TODO: Waiting on the register to write, might depend on generation. */

   return VK_SUCCESS;
}

#define TIMESTAMP 0x2358

void genX(cmd_emit_timestamp)(struct anv_batch *batch,
                              struct anv_device *device,
                              struct anv_address addr,
                              enum anv_timestamp_capture_type type,
                              void *data) {
   /* Make sure ANV_TIMESTAMP_CAPTURE_AT_CS_STALL and
    * ANV_TIMESTAMP_REWRITE_COMPUTE_WALKER capture type are not set for
    * transfer queue.
    */
   if ((batch->engine_class == INTEL_ENGINE_CLASS_COPY) ||
       (batch->engine_class == INTEL_ENGINE_CLASS_VIDEO)) {
      assert(type != ANV_TIMESTAMP_CAPTURE_AT_CS_STALL &&
             type != ANV_TIMESTAMP_REWRITE_COMPUTE_WALKER);
   }

   switch (type) {
   case ANV_TIMESTAMP_CAPTURE_TOP_OF_PIPE: {
      struct mi_builder b;
      mi_builder_init(&b, device->info, batch);
      mi_store(&b, mi_mem64(addr), mi_reg64(TIMESTAMP));
      break;
   }

   case ANV_TIMESTAMP_CAPTURE_END_OF_PIPE: {
      if ((batch->engine_class == INTEL_ENGINE_CLASS_COPY) ||
          (batch->engine_class == INTEL_ENGINE_CLASS_VIDEO)) {
         /* Wa_16018063123 - emit fast color dummy blit before MI_FLUSH_DW. */
         if (INTEL_WA_16018063123_GFX_VER &&
             batch->engine_class == INTEL_ENGINE_CLASS_COPY)
            genX(batch_emit_fast_color_dummy_blit)(batch, device);
         anv_batch_emit(batch, GENX(MI_FLUSH_DW), fd) {
            fd.PostSyncOperation = WriteTimestamp;
            fd.Address = addr;
         }
      } else {
         genx_batch_emit_pipe_control_write(batch, device->info, 0,
                                            WriteTimestamp, addr, 0, 0);
      }
      break;
   }

   case ANV_TIMESTAMP_CAPTURE_AT_CS_STALL:
      genx_batch_emit_pipe_control_write
           (batch, device->info, 0, WriteTimestamp, addr, 0,
            ANV_PIPE_CS_STALL_BIT);
      break;

#if GFX_VERx10 >= 125
   case ANV_TIMESTAMP_REWRITE_COMPUTE_WALKER: {
      uint32_t dwords[GENX(COMPUTE_WALKER_length)];

      GENX(COMPUTE_WALKER_pack)(batch, dwords, &(struct GENX(COMPUTE_WALKER)) {
            .body = {
               .PostSync = (struct GENX(POSTSYNC_DATA)) {
                  .Operation = WriteTimestamp,
                  .DestinationAddress = addr,
                  .MOCS = anv_mocs(device, NULL, 0),
               },
            }
         });

      for (uint32_t i = 0; i < ARRAY_SIZE(dwords); i++) {
         if (dwords[i])
            ((uint32_t *)data)[i] |= dwords[i];
      }
      break;
   }

   case ANV_TIMESTAMP_REWRITE_INDIRECT_DISPATCH: {
      uint32_t dwords[GENX(EXECUTE_INDIRECT_DISPATCH_length)];

      GENX(EXECUTE_INDIRECT_DISPATCH_pack)
      (batch, dwords, &(struct GENX(EXECUTE_INDIRECT_DISPATCH)) {
            .MOCS = anv_mocs(device, NULL, 0),
            .body = {
               .PostSync = (struct GENX(POSTSYNC_DATA)) {
                  .Operation = WriteTimestamp,
                  .DestinationAddress = addr,
                  .MOCS = anv_mocs(device, NULL, 0),
               },
            }
      });

      for (uint32_t i = 0; i < ARRAY_SIZE(dwords); i++) {
         if (dwords[i])
            ((uint32_t *)data)[i] |= dwords[i];
      }
      break;
   }
#endif

   case ANV_TIMESTAMP_REPEAT_LAST:
      /* Noop */
      break;

   default:
      UNREACHABLE("invalid");
   }
}

void genX(cmd_capture_data)(struct anv_batch *batch,
                            struct anv_device *device,
                            struct anv_address dst_addr,
                            struct anv_address src_addr,
                            uint32_t size_B) {
   struct mi_builder b;
   mi_builder_init(&b, device->info, batch);
   mi_builder_set_mocs(&b, isl_mocs(&device->isl_dev, 0, false));
   mi_memcpy(&b, dst_addr, src_addr, size_B);
}

void genX(batch_emit_secondary_call)(struct anv_batch *batch,
                                     struct anv_device *device,
                                     struct anv_address secondary_addr,
                                     struct anv_address secondary_return_addr)
{
   struct mi_builder b;
   mi_builder_init(&b, device->info, batch);
   mi_builder_set_mocs(&b, isl_mocs(&device->isl_dev, 0, false));
   /* Make sure the write in the batch buffer lands before we just execute the
    * jump.
    */
   mi_builder_set_write_check(&b, true);

   /* Emit a write to change the return address of the secondary */
   struct mi_reloc_imm_token reloc =
      mi_store_relocated_imm(&b, mi_mem64(secondary_return_addr));

   /* Ensure the write have landed before CS reads the address written
    * above
    */
   mi_ensure_write_fence(&b);

#if GFX_VER >= 12
   /* Disable prefetcher before jumping into a secondary */
   anv_batch_emit(batch, GENX(MI_ARB_CHECK), arb) {
      arb.PreParserDisableMask = true;
      arb.PreParserDisable = true;
   }
#endif

   /* Jump into the secondary */
   anv_batch_emit(batch, GENX(MI_BATCH_BUFFER_START), bbs) {
      bbs.AddressSpaceIndicator = ASI_PPGTT;
      bbs.SecondLevelBatchBuffer = Firstlevelbatch;
      bbs.BatchBufferStartAddress = secondary_addr;
   }

   /* Replace the return address written by the MI_STORE_DATA_IMM above with
    * the primary's current batch address (immediately after the jump).
    */
   mi_relocate_store_imm(reloc,
                         anv_address_physical(
                            anv_batch_current_address(batch)));
}

void *
genX(batch_emit_return)(struct anv_batch *batch)
{
   return anv_batch_emitn(batch,
                          GENX(MI_BATCH_BUFFER_START_length),
                          GENX(MI_BATCH_BUFFER_START),
                          .AddressSpaceIndicator = ASI_PPGTT,
                          .SecondLevelBatchBuffer = Firstlevelbatch);
}

/* Wa_16018063123 */
ALWAYS_INLINE void
genX(batch_emit_fast_color_dummy_blit)(struct anv_batch *batch,
                                      struct anv_device *device)
{
#if INTEL_WA_16018063123_GFX_VER
   anv_batch_emit(batch, GENX(XY_FAST_COLOR_BLT), blt) {
      blt.DestinationBaseAddress = device->workaround_address;
      blt.DestinationMOCS = device->isl_dev.mocs.blitter_dst;
      blt.DestinationPitch = 63;
      blt.DestinationX2 = 1;
      blt.DestinationY2 = 4;
      blt.DestinationSurfaceWidth = 1;
      blt.DestinationSurfaceHeight = 4;
      blt.DestinationSurfaceType = XY_SURFTYPE_2D;
      blt.DestinationSurfaceQPitch = 4;
      blt.DestinationTiling = XY_TILE_LINEAR;
   }
#else
   UNREACHABLE("Not implemented");
#endif
}

struct anv_state
genX(cmd_buffer_begin_companion_rcs_syncpoint)(
      struct anv_cmd_buffer   *cmd_buffer)
{
#if GFX_VERx10 >= 125
   const struct intel_device_info *info = cmd_buffer->device->info;
   struct anv_state syncpoint =
      anv_cmd_buffer_alloc_temporary_state(cmd_buffer, 2 * sizeof(uint32_t), 4);
   struct anv_address xcs_wait_addr =
      anv_cmd_buffer_temporary_state_address(cmd_buffer, syncpoint);
   struct anv_address rcs_wait_addr = anv_address_add(xcs_wait_addr, 4);

   /* Reset the sync point */
   memset(syncpoint.map, 0, 2 * sizeof(uint32_t));

   struct mi_builder b;

   /* On CCS:
    *    - flush all caches & invalidate
    *    - unblock RCS
    *    - wait on RCS to complete
    *    - clear the value we waited on
    */

   if (anv_cmd_buffer_is_compute_queue(cmd_buffer)) {
      anv_add_pending_pipe_bits(cmd_buffer, ANV_PIPE_BARRIER_FLUSH_BITS |
                                            ANV_PIPE_INVALIDATE_BITS |
                                            ANV_PIPE_STALL_BITS,
                                "post main cmd buffer invalidate");
      genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);
   } else if (anv_cmd_buffer_is_blitter_queue(cmd_buffer)) {
      /* Wa_16018063123 - emit fast color dummy blit before MI_FLUSH_DW. */
      if (INTEL_WA_16018063123_GFX_VER) {
         genX(batch_emit_fast_color_dummy_blit)(&cmd_buffer->batch,
                                                cmd_buffer->device);
      }
      anv_batch_emit(&cmd_buffer->batch, GENX(MI_FLUSH_DW), fd) {
         fd.FlushCCS = true; /* Maybe handle Flush LLC */
      }
   }

   {
      mi_builder_init(&b, info, &cmd_buffer->batch);
      mi_store(&b, mi_mem32(rcs_wait_addr), mi_imm(0x1));
      anv_batch_emit(&cmd_buffer->batch, GENX(MI_SEMAPHORE_WAIT), sem) {
         sem.WaitMode            = PollingMode;
         sem.CompareOperation    = COMPARE_SAD_EQUAL_SDD;
         sem.SemaphoreDataDword  = 0x1;
         sem.SemaphoreAddress    = xcs_wait_addr;
      }
      /* Make sure to reset the semaphore in case the command buffer is run
       * multiple times.
       */
      mi_store(&b, mi_mem32(xcs_wait_addr), mi_imm(0x0));
   }

   /* On RCS:
    *    - wait on CCS signal
    *    - clear the value we waited on
    */
   {
      mi_builder_init(&b, info, &cmd_buffer->companion_rcs_cmd_buffer->batch);
      anv_batch_emit(&cmd_buffer->companion_rcs_cmd_buffer->batch,
                     GENX(MI_SEMAPHORE_WAIT),
                     sem) {
         sem.WaitMode            = PollingMode;
         sem.CompareOperation    = COMPARE_SAD_EQUAL_SDD;
         sem.SemaphoreDataDword  = 0x1;
         sem.SemaphoreAddress    = rcs_wait_addr;
      }
      /* Make sure to reset the semaphore in case the command buffer is run
       * multiple times.
       */
      mi_store(&b, mi_mem32(rcs_wait_addr), mi_imm(0x0));
   }

   return syncpoint;
#else
   UNREACHABLE("Not implemented");
#endif
}

void
genX(cmd_buffer_end_companion_rcs_syncpoint)(struct anv_cmd_buffer *cmd_buffer,
                                             struct anv_state syncpoint)
{
#if GFX_VERx10 >= 125
   struct anv_address xcs_wait_addr =
      anv_cmd_buffer_temporary_state_address(cmd_buffer, syncpoint);

   struct mi_builder b;

   /* On RCS:
    *    - flush all caches & invalidate
    *    - unblock the CCS
    */
   anv_add_pending_pipe_bits(cmd_buffer->companion_rcs_cmd_buffer,
                             ANV_PIPE_BARRIER_FLUSH_BITS |
                             ANV_PIPE_INVALIDATE_BITS |
                             ANV_PIPE_STALL_BITS,
                             "post rcs flush");
   genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer->companion_rcs_cmd_buffer);

   mi_builder_init(&b, cmd_buffer->device->info,
                   &cmd_buffer->companion_rcs_cmd_buffer->batch);
   mi_store(&b, mi_mem32(xcs_wait_addr), mi_imm(0x1));
#else
   UNREACHABLE("Not implemented");
#endif
}

void
genX(write_trtt_entries)(struct anv_async_submit *submit,
                         struct anv_trtt_bind *l3l2_binds,
                         uint32_t n_l3l2_binds,
                         struct anv_trtt_bind *l1_binds,
                         uint32_t n_l1_binds)
{
#if GFX_VER >= 12
   const struct intel_device_info *devinfo =
      submit->queue->device->info;
   struct anv_batch *batch = &submit->batch;

   /* BSpec says:
    *   "DWord Length programmed must not exceed 0x3FE."
    * For a single dword write the programmed length is 2, and for a single
    * qword it's 3. This is the value we actually write to the register field,
    * so it's not considering the bias.
    */
   uint32_t dword_write_len = 2;
   uint32_t qword_write_len = 3;
   uint32_t max_dword_extra_writes = 0x3FE - dword_write_len;
   uint32_t max_qword_extra_writes = (0x3FE - qword_write_len) / 2;

   /* What makes the code below quite complicated is the fact that we can
    * write multiple values with MI_STORE_DATA_IMM as long as the writes go to
    * contiguous addresses.
    */

   for (uint32_t i = 0; i < n_l3l2_binds; i++) {
      int extra_writes = 0;
      for (uint32_t j = i + 1;
           j < n_l3l2_binds && extra_writes <= max_qword_extra_writes;
           j++) {
         if (l3l2_binds[i].pte_addr + (j - i) * 8 == l3l2_binds[j].pte_addr) {
            extra_writes++;
         } else {
            break;
         }
      }
      bool is_last_write = n_l1_binds == 0 &&
                           i + extra_writes + 1 == n_l3l2_binds;

      uint32_t total_len = GENX(MI_STORE_DATA_IMM_length_bias) +
                           qword_write_len + (extra_writes * 2);
      uint32_t *dw;
      dw = anv_batch_emitn(batch, total_len, GENX(MI_STORE_DATA_IMM),
         .ForceWriteCompletionCheck = is_last_write,
         .StoreQword = true,
         .Address = anv_address_from_u64(l3l2_binds[i].pte_addr),
      );
      dw += 3;
      for (uint32_t j = 0; j < extra_writes + 1; j++) {
         uint64_t entry_addr_64b = l3l2_binds[i + j].entry_addr;
         *dw = entry_addr_64b & 0xFFFFFFFF;
         dw++;
         *dw = (entry_addr_64b >> 32) & 0xFFFFFFFF;
         dw++;
      }
      assert(dw == batch->next);

      i += extra_writes;
   }

   for (uint32_t i = 0; i < n_l1_binds; i++) {
      int extra_writes = 0;
      for (uint32_t j = i + 1;
           j < n_l1_binds && extra_writes <= max_dword_extra_writes;
           j++) {
         if (l1_binds[i].pte_addr + (j - i) * 4 ==
             l1_binds[j].pte_addr) {
            extra_writes++;
         } else {
            break;
         }
      }

      bool is_last_write = i + extra_writes + 1 == n_l1_binds;

      uint32_t total_len = GENX(MI_STORE_DATA_IMM_length_bias) +
                           dword_write_len + extra_writes;
      uint32_t *dw;
      dw = anv_batch_emitn(batch, total_len, GENX(MI_STORE_DATA_IMM),
         .ForceWriteCompletionCheck = is_last_write,
         .Address = anv_address_from_u64(l1_binds[i].pte_addr),
      );
      dw += 3;
      for (uint32_t j = 0; j < extra_writes + 1; j++) {
         *dw = (l1_binds[i + j].entry_addr >> 16) & 0xFFFFFFFF;
         dw++;
      }
      assert(dw == batch->next);

      i += extra_writes;
   }

   genx_batch_emit_pipe_control(batch, devinfo, _3D,
                                ANV_PIPE_CS_STALL_BIT |
                                ANV_PIPE_TLB_INVALIDATE_BIT);
#else
   UNREACHABLE("Not implemented");
#endif
}

void
genX(async_submit_end)(struct anv_async_submit *submit)
{
   struct anv_batch *batch = &submit->batch;
   anv_batch_emit(batch, GENX(MI_BATCH_BUFFER_END), bbe);
}

void
genX(CmdWriteBufferMarker2AMD)(VkCommandBuffer commandBuffer,
                               VkPipelineStageFlags2 stage,
                               VkBuffer dstBuffer,
                               VkDeviceSize dstOffset,
                               uint32_t marker)
{
   ANV_FROM_HANDLE(anv_cmd_buffer, cmd_buffer, commandBuffer);
   ANV_FROM_HANDLE(anv_buffer, buffer, dstBuffer);

   /* The barriers inserted by the application to make dstBuffer writable
    * should already have the L1/L2 cache flushes. On platforms where the
    * command streamer is not coherent with L3, we need an additional set of
    * cache flushes.
    */
   enum anv_pipe_bits bits =
      (ANV_DEVINFO_HAS_COHERENT_L3_CS(cmd_buffer->device->info) ? 0 :
       (ANV_PIPE_DATA_CACHE_FLUSH_BIT | ANV_PIPE_TILE_CACHE_FLUSH_BIT)) |
      ANV_PIPE_END_OF_PIPE_SYNC_BIT;

   trace_intel_begin_write_buffer_marker(&cmd_buffer->trace);

   anv_add_pending_pipe_bits(cmd_buffer, bits, "write buffer marker");
   genX(cmd_buffer_apply_pipe_flushes)(cmd_buffer);

   struct mi_builder b;
   mi_builder_init(&b, cmd_buffer->device->info, &cmd_buffer->batch);

   /* Emitting a PIPE_CONTROL with Post-Sync Op = Write Immediate Data
    * would be the logical way to implement this extension, as it could
    * do a pipelined marker write.  Unfortunately, it requires writing
    * whole 64-bit QWords, and VK_AMD_buffer_marker requires writing a
    * 32-bit value.  MI_STORE_DATA_IMM is the only good way to do that,
    * and unfortunately it requires stalling.
    */
   mi_store(&b, mi_mem32(anv_address_add(buffer->address, dstOffset)),
                mi_imm(marker));

   trace_intel_end_write_buffer_marker(&cmd_buffer->trace);
}

void
genX(cmd_write_buffer_cp)(struct anv_cmd_buffer *cmd_buffer,
                          VkDeviceAddress dstAddr,
                          void *data,
                          uint32_t size)
{
   assert(size % 4 == 0);
   struct anv_address addr = anv_address_from_u64(dstAddr);

   struct mi_builder b;
   mi_builder_init(&b, cmd_buffer->device->info, &cmd_buffer->batch);

   for (uint32_t i = 0; i < size; i += 8) {
      mi_builder_set_write_check(&b, i >= size - 8);
      if (size - i < 8) {
         mi_store(&b, mi_mem32(anv_address_add(addr, i)),
                      mi_imm(*((uint32_t *)((char*)data + i))));
      } else {
         mi_store(&b, mi_mem64(anv_address_add(addr, i)),
                      mi_imm(*((uint64_t *)((char*)data + i))));
      }
   }
}

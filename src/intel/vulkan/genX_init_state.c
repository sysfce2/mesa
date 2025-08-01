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

#include "anv_private.h"

#include "common/intel_aux_map.h"
#include "common/intel_sample_positions.h"
#include "common/intel_pixel_hash.h"
#include "genxml/gen_macros.h"
#include "genxml/genX_pack.h"

#include "vk_standard_sample_locations.h"

#include "genX_mi_builder.h"

#include "vk_util.h"
#include "vk_format.h"

static void
genX(emit_slice_hashing_state)(struct anv_device *device,
                               struct anv_batch *batch)
{
#if GFX_VER == 11
   /* Gfx11 hardware has two pixel pipes at most. */
   for (unsigned i = 2; i < ARRAY_SIZE(device->info->ppipe_subslices); i++)
      assert(device->info->ppipe_subslices[i] == 0);

   if (device->info->ppipe_subslices[0] == device->info->ppipe_subslices[1])
     return;

   if (!device->slice_hash.alloc_size) {
      unsigned size = GENX(SLICE_HASH_TABLE_length) * 4;
      device->slice_hash =
         anv_state_pool_alloc(&device->dynamic_state_pool, size, 64);

      const bool flip = device->info->ppipe_subslices[0] <
                     device->info->ppipe_subslices[1];
      struct GENX(SLICE_HASH_TABLE) table;
      intel_compute_pixel_hash_table_3way(16, 16, 3, 3, flip, table.Entry[0]);

      GENX(SLICE_HASH_TABLE_pack)(NULL, device->slice_hash.map, &table);
   }

   anv_batch_emit(batch, GENX(3DSTATE_SLICE_TABLE_STATE_POINTERS), ptr) {
      ptr.SliceHashStatePointerValid = true;
      ptr.SliceHashTableStatePointer = device->slice_hash.offset;
   }

   anv_batch_emit(batch, GENX(3DSTATE_3D_MODE), mode) {
      mode.SliceHashingTableEnable = true;
   }
#elif GFX_VERx10 == 120
   /* For each n calculate ppipes_of[n], equal to the number of pixel pipes
    * present with n active dual subslices.
    */
   unsigned ppipes_of[3] = {};

   for (unsigned n = 0; n < ARRAY_SIZE(ppipes_of); n++) {
      for (unsigned p = 0; p < 3; p++)
         ppipes_of[n] += (device->info->ppipe_subslices[p] == n);
   }

   /* Gfx12 has three pixel pipes. */
   for (unsigned p = 3; p < ARRAY_SIZE(device->info->ppipe_subslices); p++)
      assert(device->info->ppipe_subslices[p] == 0);

   if (ppipes_of[2] == 3 || ppipes_of[0] == 2) {
      /* All three pixel pipes have the maximum number of active dual
       * subslices, or there is only one active pixel pipe: Nothing to do.
       */
      return;
   }

   anv_batch_emit(batch, GENX(3DSTATE_SUBSLICE_HASH_TABLE), p) {
      p.SliceHashControl[0] = TABLE_0;

      if (ppipes_of[2] == 2 && ppipes_of[0] == 1)
         intel_compute_pixel_hash_table_3way(8, 16, 2, 2, 0, p.TwoWayTableEntry[0]);
      else if (ppipes_of[2] == 1 && ppipes_of[1] == 1 && ppipes_of[0] == 1)
         intel_compute_pixel_hash_table_3way(8, 16, 3, 3, 0, p.TwoWayTableEntry[0]);

      if (ppipes_of[2] == 2 && ppipes_of[1] == 1)
         intel_compute_pixel_hash_table_3way(8, 16, 5, 4, 0, p.ThreeWayTableEntry[0]);
      else if (ppipes_of[2] == 2 && ppipes_of[0] == 1)
         intel_compute_pixel_hash_table_3way(8, 16, 2, 2, 0, p.ThreeWayTableEntry[0]);
      else if (ppipes_of[2] == 1 && ppipes_of[1] == 1 && ppipes_of[0] == 1)
         intel_compute_pixel_hash_table_3way(8, 16, 3, 3, 0, p.ThreeWayTableEntry[0]);
      else
         UNREACHABLE("Illegal fusing.");
   }

   anv_batch_emit(batch, GENX(3DSTATE_3D_MODE), p) {
      p.SubsliceHashingTableEnable = true;
      p.SubsliceHashingTableEnableMask = true;
   }
#elif GFX_VERx10 == 125
   /* Calculate the set of present pixel pipes, and another set of
    * present pixel pipes with 2 dual subslices enabled, the latter
    * will appear on the hashing table with twice the frequency of
    * pixel pipes with a single dual subslice present.
    */
   uint32_t ppipe_mask1 = 0, ppipe_mask2 = 0;
   for (unsigned p = 0; p < ARRAY_SIZE(device->info->ppipe_subslices); p++) {
      if (device->info->ppipe_subslices[p] > 0)
         ppipe_mask1 |= (1u << p);
      if (device->info->ppipe_subslices[p] > 1)
         ppipe_mask2 |= (1u << p);
   }
   assert(ppipe_mask1);

   if (!device->slice_hash.alloc_size) {
      unsigned size = GENX(SLICE_HASH_TABLE_length) * 4;
      device->slice_hash =
         anv_state_pool_alloc(&device->dynamic_state_pool, size, 64);

      struct GENX(SLICE_HASH_TABLE) table;

      /* Note that the hardware expects an array with 7 tables, each
       * table is intended to specify the pixel pipe hashing behavior
       * for every possible slice count between 2 and 8, however that
       * doesn't actually work, among other reasons due to hardware
       * bugs that will cause the GPU to erroneously access the table
       * at the wrong index in some cases, so in practice all 7 tables
       * need to be initialized to the same value.
       */
      for (unsigned i = 0; i < 7; i++)
         intel_compute_pixel_hash_table_nway(16, 16, ppipe_mask1, ppipe_mask2,
                                             table.Entry[i][0]);

      GENX(SLICE_HASH_TABLE_pack)(NULL, device->slice_hash.map, &table);
   }

   anv_batch_emit(batch, GENX(3DSTATE_SLICE_TABLE_STATE_POINTERS), ptr) {
      ptr.SliceHashStatePointerValid = true;
      ptr.SliceHashTableStatePointer = device->slice_hash.offset;
   }

   /* TODO: Figure out FCV support for other platforms
    * Testing indicates that FCV is broken gfx125.
    * Let's disable FCV for now till we figure out what's wrong.
    *
    * Alternatively, it can be toggled off via drirc option 'anv_disable_fcv'.
    *
    * Ref: https://gitlab.freedesktop.org/mesa/mesa/-/issues/9987
    * Ref: https://gitlab.freedesktop.org/mesa/mesa/-/issues/10318
    * Ref: https://gitlab.freedesktop.org/mesa/mesa/-/issues/10795
    * Ref: Internal issue 1480 about Unreal Engine 5.1
    */
   anv_batch_emit(batch, GENX(3DSTATE_3D_MODE), mode) {
      mode.SliceHashingTableEnable = true;
      mode.SliceHashingTableEnableMask = true;
      mode.CrossSliceHashingMode = (util_bitcount(ppipe_mask1) > 1 ?
				    hashing32x32 : NormalMode);
      mode.CrossSliceHashingModeMask = -1;
      mode.FastClearOptimizationEnable = !device->physical->disable_fcv;
      mode.FastClearOptimizationEnableMask = !device->physical->disable_fcv;
   }
#endif
}

static void
state_system_mem_fence_address_emit(struct anv_device *device, struct anv_batch *batch)
{
#if GFX_VERx10 >= 200
   struct anv_address addr = { .bo = device->mem_fence_bo };
   anv_batch_emit(batch, GENX(STATE_SYSTEM_MEM_FENCE_ADDRESS), mem_fence_addr) {
      mem_fence_addr.SystemMemoryFenceAddress = addr;
   }
#endif
}

static void
init_common_queue_state(struct anv_queue *queue, struct anv_batch *batch)
{
   UNUSED struct anv_device *device = queue->device;

#if GFX_VER >= 11
   /* Starting with GFX version 11, SLM is no longer part of the L3$ config
    * so it never changes throughout the lifetime of the VkDevice.
    */
   const struct intel_l3_config *cfg = intel_get_default_l3_config(device->info);
   genX(emit_l3_config)(batch, device, cfg);
   device->l3_config = device->l3_slm_config = cfg;
#else
   device->l3_config = intel_get_l3_config(
      device->info,
      intel_get_default_l3_weights(device->info, true, false /* slm */));
   device->l3_slm_config = intel_get_l3_config(
      device->info,
      intel_get_default_l3_weights(device->info, true, true /* slm */));
#endif

#if GFX_VERx10 == 125
   /* Even though L3 partial write merging is supposed to be enabled
    * by default on Gfx12.5 according to the hardware spec, i915
    * appears to accidentally clear the enables during context
    * initialization, so make sure to enable them here since partial
    * write merging has a large impact on rendering performance.
    */
   anv_batch_write_reg(batch, GENX(L3SQCREG5), reg) {
      reg.L3CachePartialWriteMergeTimerInitialValue = 0x7f;
      reg.CompressiblePartialWriteMergeEnable = true;
      reg.CoherentPartialWriteMergeEnable = true;
      reg.CrossTilePartialWriteMergeEnable = true;
   }
#endif

   /* Emit STATE_BASE_ADDRESS on Gfx12+ because we set a default CPS_STATE and
    * those are relative to STATE_BASE_ADDRESS::DynamicStateBaseAddress.
    */
#if GFX_VER >= 12

#if GFX_VERx10 >= 125
   /* Wa_14016407139:
    *
    * "On Surface state base address modification, for 3D workloads, SW must
    *  always program PIPE_CONTROL either with CS Stall or PS sync stall. In
    *  both the cases set Render Target Cache Flush Enable".
    */
   genx_batch_emit_pipe_control(batch, device->info,
                                0,
                                ANV_PIPE_CS_STALL_BIT |
                                ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT);
#endif

   /* GEN:BUG:1607854226:
    *
    *  Non-pipelined state has issues with not applying in MEDIA/GPGPU mode.
    *  Fortunately, we always start the context off in 3D mode.
    */
   uint32_t mocs = device->isl_dev.mocs.internal;
   anv_batch_emit(batch, GENX(STATE_BASE_ADDRESS), sba) {
      sba.GeneralStateBaseAddress = (struct anv_address) { NULL, 0 };
      sba.GeneralStateBufferSize  = DIV_ROUND_UP(
         device->physical->va.first_2mb.size +
         device->physical->va.general_state_pool.size +
         device->physical->va.low_heap.size, 4096);
      sba.GeneralStateMOCS = mocs;
      sba.GeneralStateBaseAddressModifyEnable = true;
      sba.GeneralStateBufferSizeModifyEnable = true;

      sba.StatelessDataPortAccessMOCS = mocs;

      sba.SurfaceStateBaseAddress =
         (struct anv_address) { .offset =
         device->physical->va.internal_surface_state_pool.addr,
      };
      sba.SurfaceStateMOCS = mocs;
      sba.SurfaceStateBaseAddressModifyEnable = true;

      sba.DynamicStateBaseAddress =
         (struct anv_address) { .offset =
         device->physical->va.dynamic_state_pool.addr,
      };
      sba.DynamicStateBufferSize = (device->physical->va.dynamic_state_pool.size +
                                    device->physical->va.dynamic_visible_pool.size) / 4096;
      sba.DynamicStateMOCS = mocs;
      sba.DynamicStateBaseAddressModifyEnable = true;
      sba.DynamicStateBufferSizeModifyEnable = true;

      sba.IndirectObjectBaseAddress = (struct anv_address) { NULL, 0 };
      sba.IndirectObjectBufferSize = 0xfffff;
      sba.IndirectObjectMOCS = mocs;
      sba.IndirectObjectBaseAddressModifyEnable = true;
      sba.IndirectObjectBufferSizeModifyEnable = true;

      sba.InstructionBaseAddress =
         (struct anv_address) { .offset =
         device->physical->va.instruction_state_pool.addr,
      };
      sba.InstructionBufferSize = device->physical->va.instruction_state_pool.size / 4096;
      sba.InstructionMOCS = mocs;
      sba.InstructionBaseAddressModifyEnable = true;
      sba.InstructionBuffersizeModifyEnable = true;

#if GFX_VER >= 11
      sba.BindlessSamplerStateBaseAddress = ANV_NULL_ADDRESS;
      sba.BindlessSamplerStateBufferSize = 0;
      sba.BindlessSamplerStateMOCS = mocs;
      sba.BindlessSamplerStateBaseAddressModifyEnable = true;
#endif

      if (device->physical->indirect_descriptors) {
         sba.BindlessSurfaceStateBaseAddress =
            (struct anv_address) { .offset =
            device->physical->va.bindless_surface_state_pool.addr,
         };
         sba.BindlessSurfaceStateSize =
            anv_physical_device_bindless_heap_size(device->physical, false) /
            ANV_SURFACE_STATE_SIZE - 1;
         sba.BindlessSurfaceStateMOCS = mocs;
         sba.BindlessSurfaceStateBaseAddressModifyEnable = true;
      } else {
         /* Bindless Surface State & Bindless Sampler State are aligned to the
          * same heap
          */
         sba.BindlessSurfaceStateBaseAddress = (struct anv_address) {
            .offset = device->physical->va.internal_surface_state_pool.addr,
         };
         sba.BindlessSurfaceStateSize =
            (device->physical->va.internal_surface_state_pool.size +
             device->physical->va.bindless_surface_state_pool.size) - 1;
         sba.BindlessSurfaceStateMOCS = mocs;
         sba.BindlessSurfaceStateBaseAddressModifyEnable = true;
      }

#if GFX_VERx10 >= 125
      sba.L1CacheControl = L1CC_WB;
#endif
   }

   /* Disable the POOL_ALLOC mechanism in HW. We found that this state can get
    * corrupted (likely due to leaking from another context), the default
    * value should be disabled. It doesn't cost anything to set it once at
    * device initialization.
    */
#if GFX_VER >= 11 && GFX_VERx10 < 125
   anv_batch_emit(batch, GENX(3DSTATE_BINDING_TABLE_POOL_ALLOC), btpa) {
      btpa.MOCS = mocs;
      btpa.BindingTablePoolEnable = false;
   }
#endif

   struct mi_builder b;
   mi_builder_init(&b, device->info, batch);

   mi_store(&b, mi_reg64(ANV_BINDLESS_SURFACE_BASE_ADDR_REG),
                mi_imm(device->physical->va.internal_surface_state_pool.addr));
#endif /* GFX_VER >= 12 */

#if GFX_VERx10 >= 125
   if (ANV_SUPPORT_RT && device->info->has_ray_tracing) {
      anv_batch_emit(batch, GENX(3DSTATE_BTD), btd) {
         /* TODO: This is the timeout after which the bucketed thread
          *       dispatcher will kick off a wave of threads. We go with the
          *       lowest value for now. It could be tweaked on a per
          *       application basis (drirc).
          */
         btd.DispatchTimeoutCounter = _64clocks;
         /* BSpec 43851: "This field must be programmed to 6h i.e. memory
          *               backed buffer must be 128KB."
          */
         btd.PerDSSMemoryBackedBufferSize = 6;
         btd.MemoryBackedBufferBasePointer = (struct anv_address) {
            /* This batch doesn't have a reloc list so we can't use the BO
             * here.  We just use the address directly.
             */
            .offset = device->btd_fifo_bo->offset,
         };
#if INTEL_NEEDS_WA_14017794102 || INTEL_NEEDS_WA_14023061436
         btd.BTDMidthreadpreemption = false;
#endif

#if GFX_VER >= 30
         btd.RTMemStructures64bModeEnable = true;
#endif
      }
   }
#endif

   /* Always use Thread Group Preemption granularity level for Media/GPGPU */
   anv_batch_write_reg(batch, GENX(CS_CHICKEN1), cc1) {
      cc1.MediaAndGPGPUPreemptionControl = ThreadGroupPreemption;
      cc1.MediaAndGPGPUPreemptionControlMask = 0x3;
   }

   state_system_mem_fence_address_emit(device, batch);
}

#if GFX_VER >= 20
#define _3DSTATE_DRAWING_RECTANGLE GENX(3DSTATE_DRAWING_RECTANGLE_FAST)
#else
#define _3DSTATE_DRAWING_RECTANGLE GENX(3DSTATE_DRAWING_RECTANGLE)
#endif

static VkResult
init_render_queue_state(struct anv_queue *queue, bool is_companion_rcs_batch)
{
   struct anv_device *device = queue->device;
   UNUSED const struct intel_device_info *devinfo = queue->device->info;

   struct anv_async_submit *submit;
   VkResult result = anv_async_submit_create(queue,
                                             &device->batch_bo_pool,
                                             is_companion_rcs_batch,
                                             true, &submit);
   if (result != VK_SUCCESS)
      return result;

   struct anv_batch *batch = &submit->batch;

   genX(emit_pipeline_select)(batch, _3D, device);

#if GFX_VER == 9
   anv_batch_write_reg(batch, GENX(CACHE_MODE_1), cm1) {
      cm1.FloatBlendOptimizationEnable = true;
      cm1.FloatBlendOptimizationEnableMask = true;
      cm1.MSCRAWHazardAvoidanceBit = true;
      cm1.MSCRAWHazardAvoidanceBitMask = true;
      cm1.PartialResolveDisableInVC = true;
      cm1.PartialResolveDisableInVCMask = true;
   }
#endif

   anv_batch_emit(batch, GENX(3DSTATE_AA_LINE_PARAMETERS), aa);

   anv_batch_emit(batch, _3DSTATE_DRAWING_RECTANGLE, rect) {
      rect.ClippedDrawingRectangleYMin = 0;
      rect.ClippedDrawingRectangleXMin = 0;
      rect.ClippedDrawingRectangleYMax = UINT16_MAX;
      rect.ClippedDrawingRectangleXMax = UINT16_MAX;
      rect.DrawingRectangleOriginY = 0;
      rect.DrawingRectangleOriginX = 0;
   }

   anv_batch_emit(batch, GENX(3DSTATE_WM_CHROMAKEY), ck);

   /* SKL PRMs, Volume 2a: Command Reference: Instructions: 3DSTATE_WM_HZ_OP:
    *
    *   "3DSTATE_RASTER if used must be programmed prior to using this
    *    packet."
    *
    * Emit this before 3DSTATE_WM_HZ_OP below.
    */
   anv_batch_emit(batch, GENX(3DSTATE_RASTER), rast) {
      rast.APIMode = DX101;
   }

   /* SKL PRMs, Volume 2a: Command Reference: Instructions: 3DSTATE_WM_HZ_OP:
    *
    *    "3DSTATE_MULTISAMPLE packet must be used prior to this packet to
    *     change the Number of Multisamples. This packet must not be used to
    *     change Number of Multisamples in a rendering sequence."
    *
    * Emit this before 3DSTATE_WM_HZ_OP below.
    */
   anv_batch_emit(batch, GENX(3DSTATE_MULTISAMPLE), ms);

   /* The BDW+ docs describe how to use the 3DSTATE_WM_HZ_OP instruction in the
    * section titled, "Optimized Depth Buffer Clear and/or Stencil Buffer
    * Clear." It mentions that the packet overrides GPU state for the clear
    * operation and needs to be reset to 0s to clear the overrides. Depending
    * on the kernel, we may not get a context with the state for this packet
    * zeroed. Do it ourselves just in case. We've observed this to prevent a
    * number of GPU hangs on ICL.
    */
   anv_batch_emit(batch, GENX(3DSTATE_WM_HZ_OP), hzp);

   genX(emit_sample_pattern)(batch, NULL);

#if GFX_VER == 11
   /* The default behavior of bit 5 "Headerless Message for Pre-emptable
    * Contexts" in SAMPLER MODE register is set to 0, which means
    * headerless sampler messages are not allowed for pre-emptable
    * contexts. Set the bit 5 to 1 to allow them.
    */
   anv_batch_write_reg(batch, GENX(SAMPLER_MODE), sm) {
      sm.HeaderlessMessageforPreemptableContexts = true;
      sm.HeaderlessMessageforPreemptableContextsMask = true;
   }

   /* Bit 1 "Enabled Texel Offset Precision Fix" must be set in
    * HALF_SLICE_CHICKEN7 register.
    */
   anv_batch_write_reg(batch, GENX(HALF_SLICE_CHICKEN7), hsc7) {
      hsc7.EnabledTexelOffsetPrecisionFix = true;
      hsc7.EnabledTexelOffsetPrecisionFixMask = true;
   }

   anv_batch_write_reg(batch, GENX(TCCNTLREG), tcc) {
      tcc.L3DataPartialWriteMergingEnable = true;
      tcc.ColorZPartialWriteMergingEnable = true;
      tcc.URBPartialWriteMergingEnable = true;
      tcc.TCDisable = true;
   }
#endif
   genX(emit_slice_hashing_state)(device, batch);

#if GFX_VER >= 11
   /* hardware specification recommends disabling repacking for
    * the compatibility with decompression mechanism in display controller.
    */
   if (device->info->disable_ccs_repack) {
      anv_batch_write_reg(batch, GENX(CACHE_MODE_0), cm0) {
         cm0.DisableRepackingforCompression = true;
         cm0.DisableRepackingforCompressionMask = true;
      }
   }

   /* an unknown issue is causing vs push constants to become
    * corrupted during object-level preemption. For now, restrict
    * to command buffer level preemption to avoid rendering
    * corruption.
    */
   anv_batch_write_reg(batch, GENX(CS_CHICKEN1), cc1) {
      cc1.ReplayMode = MidcmdbufferPreemption;
      cc1.ReplayModeMask = true;

#if GFX_VERx10 == 120
      cc1.DisablePreemptionandHighPriorityPausingdueto3DPRIMITIVECommand = true;
      cc1.DisablePreemptionandHighPriorityPausingdueto3DPRIMITIVECommandMask = true;
#endif
   }

#if INTEL_NEEDS_WA_1806527549
   /* Wa_1806527549 says to disable the following HiZ optimization when the
    * depth buffer is D16_UNORM. We've found the WA to help with more depth
    * buffer configurations however, so we always disable it just to be safe.
    */
   anv_batch_write_reg(batch, GENX(HIZ_CHICKEN), reg) {
      reg.HZDepthTestLEGEOptimizationDisable = true;
      reg.HZDepthTestLEGEOptimizationDisableMask = true;
   }
#endif

#if GFX_VER == 12
   anv_batch_write_reg(batch, GENX(FF_MODE2), reg) {
      /* On Alchemist, the FF_MODE2 docs for the GS timer say:
       *
       *    "The timer value must be set to 224."
       *
       * and Wa_16011163337 indicates this is the case for all Gfx12 parts,
       * and that this is necessary to avoid hanging the HS/DS units.  It
       * also clarifies that 224 is literally 0xE0 in the bits, not 7*32=224.
       *
       * The HS timer docs also have the same quote for Alchemist.  I am
       * unaware of a reason it needs to be set to 224 on Tigerlake, but
       * we do so for consistency if nothing else.
       *
       * For the TDS timer value, the docs say:
       *
       *    "For best performance, a value of 4 should be programmed."
       *
       * i915 also sets it this way on Tigerlake due to workarounds.
       *
       * The default VS timer appears to be 0, so we leave it at that.
       */
      reg.GSTimerValue  = 224;
      reg.HSTimerValue  = 224;
      reg.TDSTimerValue = 4;
      reg.VSTimerValue  = 0;
   }
#endif

#if INTEL_NEEDS_WA_1508744258
   /*    Disable RHWO by setting 0x7010[14] by default except during resolve
    *    pass.
    *
    * We implement global disabling of the optimization here and we toggle it
    * in anv_image_ccs_op().
    */
   anv_batch_write_reg(batch, GENX(COMMON_SLICE_CHICKEN1), c1) {
      c1.RCCRHWOOptimizationDisable = true;
      c1.RCCRHWOOptimizationDisableMask = true;
   }
#endif

#if GFX_VERx10 < 125
#define AA_LINE_QUALITY_REG GENX(3D_CHICKEN3)
#else
#define AA_LINE_QUALITY_REG GENX(CHICKEN_RASTER_1)
#endif

   /* Enable the new line drawing algorithm that produces higher quality
    * lines.
    */
   anv_batch_write_reg(batch, AA_LINE_QUALITY_REG, c3) {
      c3.AALineQualityFix = true;
      c3.AALineQualityFixMask = true;
   }
#endif

#if GFX_VER == 12
   if (device->info->has_aux_map) {
      uint64_t aux_base_addr = intel_aux_map_get_base(device->aux_map_ctx);
      assert(aux_base_addr % (32 * 1024) == 0);
      anv_batch_emit(batch, GENX(MI_LOAD_REGISTER_IMM), lri) {
         lri.RegisterOffset = GENX(GFX_AUX_TABLE_BASE_ADDR_num);
         lri.DataDWord = aux_base_addr & 0xffffffff;
      }
      anv_batch_emit(batch, GENX(MI_LOAD_REGISTER_IMM), lri) {
         lri.RegisterOffset = GENX(GFX_AUX_TABLE_BASE_ADDR_num) + 4;
         lri.DataDWord = aux_base_addr >> 32;
      }
   }
#endif

#if GFX_VERx10 == 125
   anv_batch_write_reg(batch, GENX(CHICKEN_RASTER_2), reg) {
      reg.TBIMRBatchSizeOverride = true;
      reg.TBIMROpenBatchEnable = true;
      reg.TBIMRFastClip = true;
      reg.TBIMRBatchSizeOverrideMask = true;
      reg.TBIMROpenBatchEnableMask = true;
      reg.TBIMRFastClipMask = true;
   }
#endif

   /* Set the "CONSTANT_BUFFER Address Offset Disable" bit, so
    * 3DSTATE_CONSTANT_XS buffer 0 is an absolute address.
    *
    * This is only safe on kernels with context isolation support.
    */
   assert(device->physical->info.has_context_isolation);
   anv_batch_write_reg(batch, GENX(CS_DEBUG_MODE2), csdm2) {
      csdm2.CONSTANT_BUFFERAddressOffsetDisable = true;
      csdm2.CONSTANT_BUFFERAddressOffsetDisableMask = true;
   }

   init_common_queue_state(queue, batch);

   /* Because 3DSTATE_CPS::CoarsePixelShadingStateArrayPointer is relative to
    * the dynamic state base address we need to emit this instruction after
    * STATE_BASE_ADDRESS in init_common_queue_state().
    */
#if GFX_VER >= 30
   anv_batch_emit(batch, GENX(3DSTATE_COARSE_PIXEL), cps);
#elif GFX_VER >= 12
   anv_batch_emit(batch, GENX(3DSTATE_CPS_POINTERS), cps) {
      assert(device->cps_states.alloc_size != 0);
      /* Offset 0 is the disabled state */
      cps.CoarsePixelShadingStateArrayPointer =
         device->cps_states.offset;
   }
#elif GFX_VER == 11
   anv_batch_emit(batch, GENX(3DSTATE_CPS), cps);
#endif

#if GFX_VERx10 >= 125
   anv_batch_emit(batch, GENX(STATE_COMPUTE_MODE), cm) {
#if GFX_VER >= 30
      cm.EnableVariableRegisterSizeAllocation = !INTEL_DEBUG(DEBUG_NO_VRT);
#endif
      cm.Mask1 = 0xffff;
#if GFX_VERx10 >= 200
      cm.Mask2 = 0xffff;
#endif
   }
   anv_batch_emit(batch, GENX(3DSTATE_MESH_CONTROL), zero);
   anv_batch_emit(batch, GENX(3DSTATE_TASK_CONTROL), zero);

   /* We no longer required to explicitly flush or invalidate caches since the
    * PIPELINE_SELECT is getting deprecated on Xe2+.
    */
#if GFX_VER < 20
   genx_batch_emit_pipe_control_write(batch, device->info, _3D, NoWrite,
                                      ANV_NULL_ADDRESS,
                                      0,
                                      ANV_PIPE_FLUSH_BITS | ANV_PIPE_INVALIDATE_BITS);
#endif

   genX(emit_pipeline_select)(batch, GPGPU, device);
   anv_batch_emit(batch, GENX(CFE_STATE), cfe) {
      cfe.MaximumNumberofThreads =
         devinfo->max_cs_threads * devinfo->subslice_total;
   }

   /* We no longer required to explicitly flush or invalidate caches since the
    * PIPELINE_SELECT is getting deprecated on Xe2+.
    */
#if GFX_VER < 20
   genx_batch_emit_pipe_control_write(batch, device->info, _3D, NoWrite,
                                      ANV_NULL_ADDRESS,
                                      0,
                                      ANV_PIPE_FLUSH_BITS | ANV_PIPE_INVALIDATE_BITS);
#endif

   genX(emit_pipeline_select)(batch, _3D, device);
#endif

#if GFX_VER >= 20
   anv_batch_emit(batch, GENX(3DSTATE_3D_MODE), p) {
      p.DX10OGLBorderModeforYCRCB = true;
      p.DX10OGLBorderModeforYCRCBMask = true;
#if INTEL_NEEDS_WA_14019857787
      p.EnableOOOreadsinRCPB = true;
      p.EnableOOOreadsinRCPBMask = true;
#endif
   }
#endif

   anv_batch_emit(batch, GENX(MI_BATCH_BUFFER_END), bbe);

   result = batch->status;
   if (result != VK_SUCCESS) {
      anv_async_submit_destroy(submit);
      return result;
   }

   result = device->kmd_backend->queue_exec_async(submit, 0, NULL, 0, NULL);
   if (result != VK_SUCCESS) {
      anv_async_submit_destroy(submit);
      return result;
   }

   if (is_companion_rcs_batch)
      queue->init_companion_submit = submit;
   else
      queue->init_submit = submit;

   return VK_SUCCESS;
}

static VkResult
init_compute_queue_state(struct anv_queue *queue)
{
   struct anv_device *device = queue->device;
   UNUSED const struct intel_device_info *devinfo = device->info;
   struct anv_async_submit *submit;
   VkResult result = anv_async_submit_create(queue,
                                             &device->batch_bo_pool,
                                             false, true, &submit);
   if (result != VK_SUCCESS)
      return result;

   struct anv_batch *batch = &submit->batch;

   genX(emit_pipeline_select)(batch, GPGPU, queue->device);

#if GFX_VER == 12
   if (queue->device->info->has_aux_map) {
      uint64_t aux_base_addr =
         intel_aux_map_get_base(queue->device->aux_map_ctx);
      assert(aux_base_addr % (32 * 1024) == 0);
      anv_batch_emit(batch, GENX(MI_LOAD_REGISTER_IMM), lri) {
         lri.RegisterOffset = GENX(COMPCS0_AUX_TABLE_BASE_ADDR_num);
         lri.DataDWord = aux_base_addr & 0xffffffff;
      }
      anv_batch_emit(batch, GENX(MI_LOAD_REGISTER_IMM), lri) {
         lri.RegisterOffset = GENX(COMPCS0_AUX_TABLE_BASE_ADDR_num) + 4;
         lri.DataDWord = aux_base_addr >> 32;
      }
   }
#else
   assert(!queue->device->info->has_aux_map);
#endif

   /* Wa_14015782607 - Issue pipe control with HDC_flush and
    * untyped cache flush set to 1 when CCS has NP state update with
    * STATE_COMPUTE_MODE.
    */
   if (intel_needs_workaround(devinfo, 14015782607) &&
       queue->family->engine_class == INTEL_ENGINE_CLASS_COMPUTE) {
      genx_batch_emit_pipe_control(batch, devinfo, GPGPU,
                                   ANV_PIPE_CS_STALL_BIT |
                                   ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT |
                                   ANV_PIPE_HDC_PIPELINE_FLUSH_BIT);
   }

#if GFX_VERx10 >= 125
   /* Wa_14014427904/22013045878 - We need additional invalidate/flush when
    * emitting NP state commands with ATS-M in compute mode.
    */
   if (intel_device_info_is_atsm(devinfo) &&
       queue->family->engine_class == INTEL_ENGINE_CLASS_COMPUTE) {
      genx_batch_emit_pipe_control
         (batch, devinfo, GPGPU,
          ANV_PIPE_CS_STALL_BIT |
          ANV_PIPE_STATE_CACHE_INVALIDATE_BIT |
          ANV_PIPE_CONSTANT_CACHE_INVALIDATE_BIT |
          ANV_PIPE_UNTYPED_DATAPORT_CACHE_FLUSH_BIT |
          ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT |
          ANV_PIPE_INSTRUCTION_CACHE_INVALIDATE_BIT |
          ANV_PIPE_HDC_PIPELINE_FLUSH_BIT);
   }

   anv_batch_emit(batch, GENX(STATE_COMPUTE_MODE), cm) {
#if GFX_VER >= 30
      cm.EnableVariableRegisterSizeAllocationMask = 1;
      cm.EnableVariableRegisterSizeAllocation = !INTEL_DEBUG(DEBUG_NO_VRT);
#endif
#if GFX_VER >= 20
      cm.AsyncComputeThreadLimit = ACTL_Max8;
      cm.ZPassAsyncComputeThreadLimit = ZPACTL_Max60;
      cm.ZAsyncThrottlesettings = ZATS_DefertoAsyncComputeThreadLimit;
      cm.AsyncComputeThreadLimitMask = 0x7;
      cm.ZPassAsyncComputeThreadLimitMask = 0x7;
      cm.ZAsyncThrottlesettingsMask = 0x3;
#else
      cm.PixelAsyncComputeThreadLimit = PACTL_Max24;
      cm.ZPassAsyncComputeThreadLimit = ZPACTL_Max60;
      cm.PixelAsyncComputeThreadLimitMask = 0x7;
      cm.ZPassAsyncComputeThreadLimitMask = 0x7;
      if (intel_device_info_is_mtl_or_arl(devinfo)) {
         cm.ZAsyncThrottlesettings = ZATS_DefertoPixelAsyncComputeThreadLimit;
         cm.ZAsyncThrottlesettingsMask = 0x3;
      }
#endif
   }
#endif

   init_common_queue_state(queue, batch);

#if GFX_VERx10 >= 125
   anv_batch_emit(batch, GENX(CFE_STATE), cfe) {
      cfe.MaximumNumberofThreads =
         devinfo->max_cs_threads * devinfo->subslice_total;
   }
#endif

   anv_batch_emit(batch, GENX(MI_BATCH_BUFFER_END), bbe);

   result = batch->status;
   if (result != VK_SUCCESS) {
      anv_async_submit_destroy(submit);
      return result;
   }

   result = device->kmd_backend->queue_exec_async(submit, 0, NULL, 0, NULL);
   if (result != VK_SUCCESS) {
      anv_async_submit_destroy(submit);
      return result;
   }

   queue->init_submit = submit;

   return VK_SUCCESS;
}

static VkResult
init_copy_video_queue_state(struct anv_queue *queue)
{
   struct anv_device *device = queue->device;
   UNUSED const struct intel_device_info *devinfo = device->info;

   struct anv_async_submit *submit;
   VkResult result = anv_async_submit_create(queue,
                                             &device->batch_bo_pool,
                                             false, true, &submit);
   if (result != VK_SUCCESS)
      return result;

   struct anv_batch *batch = &submit->batch;

#if GFX_VER >= 12
   if (devinfo->has_aux_map) {
      uint64_t reg = GENX(VD0_AUX_TABLE_BASE_ADDR_num);

      if (queue->family->engine_class == INTEL_ENGINE_CLASS_COPY) {
#if GFX_VERx10 >= 125
         reg = GENX(BCS_AUX_TABLE_BASE_ADDR_num);
#endif
      }

      uint64_t aux_base_addr =
         intel_aux_map_get_base(queue->device->aux_map_ctx);
      assert(aux_base_addr % (32 * 1024) == 0);
      anv_batch_emit(batch, GENX(MI_LOAD_REGISTER_IMM), lri) {
         lri.RegisterOffset = reg;
         lri.DataDWord = aux_base_addr & 0xffffffff;
      }
      anv_batch_emit(batch, GENX(MI_LOAD_REGISTER_IMM), lri) {
         lri.RegisterOffset = reg + 4;
         lri.DataDWord = aux_base_addr >> 32;
      }
   }
#else
   assert(!queue->device->info->has_aux_map);
#endif

   state_system_mem_fence_address_emit(device, batch);

   if (batch->start != batch->next) {
      anv_batch_emit(batch, GENX(MI_BATCH_BUFFER_END), bbe);

      result = batch->status;
      if (result != VK_SUCCESS) {
         anv_async_submit_destroy(submit);
         return result;
      }

      result = device->kmd_backend->queue_exec_async(submit, 0, NULL, 0, NULL);
      if (result != VK_SUCCESS) {
         anv_async_submit_destroy(submit);
         return result;
      }

      queue->init_submit = submit;
   } else {
      anv_async_submit_destroy(submit);
   }

   return VK_SUCCESS;
}

void
genX(init_physical_device_state)(ASSERTED struct anv_physical_device *pdevice)
{
   assert(pdevice->info.verx10 == GFX_VERx10);

#if GFX_VERx10 >= 125 && ANV_SUPPORT_RT
   STATIC_ASSERT(sizeof(ANV_RT_UUID_MACRO) == VK_UUID_SIZE);
   memcpy(pdevice->rt_uuid, ANV_RT_UUID_MACRO, VK_UUID_SIZE);
#endif

   pdevice->cmd_emit_timestamp = genX(cmd_emit_timestamp);
   pdevice->cmd_capture_data = genX(cmd_capture_data);

   pdevice->gpgpu_pipeline_value = GPGPU;

   struct GENX(VERTEX_ELEMENT_STATE) empty_ve = {
      .Valid = true,
      .Component0Control = VFCOMP_STORE_0,
      .Component1Control = VFCOMP_STORE_0,
      .Component2Control = VFCOMP_STORE_0,
      .Component3Control = VFCOMP_STORE_0,
   };
   GENX(VERTEX_ELEMENT_STATE_pack)(NULL, pdevice->empty_vs_input, &empty_ve);
}

VkResult
genX(init_device_state)(struct anv_device *device)
{
   VkResult res;

   device->slice_hash = (struct anv_state) { 0 };
   for (uint32_t i = 0; i < device->queue_count; i++) {
      struct anv_queue *queue = &device->queues[i];
      switch (queue->family->engine_class) {
      case INTEL_ENGINE_CLASS_RENDER:
         res = init_render_queue_state(queue, false /* is_companion_rcs_batch */);
         break;
      case INTEL_ENGINE_CLASS_COMPUTE: {
         res = init_compute_queue_state(queue);
         if (res != VK_SUCCESS)
            return res;

         /**
          * Execute RCS init batch by default on the companion RCS command buffer in
          * order to support MSAA copy/clear operations on compute queue.
          */
         res = init_render_queue_state(queue, true /* is_companion_rcs_batch */);
         break;
      }
      case INTEL_ENGINE_CLASS_VIDEO:
         res = init_copy_video_queue_state(queue);
         break;
      case INTEL_ENGINE_CLASS_COPY:
         res = init_copy_video_queue_state(queue);
         if (res != VK_SUCCESS)
            return res;

         /**
          * Execute RCS init batch by default on the companion RCS command buffer in
          * order to support MSAA copy/clear operations on copy queue.
          */
         res = init_render_queue_state(queue, true /* is_companion_rcs_batch */);
         break;
      default:
         res = vk_error(device, VK_ERROR_INITIALIZATION_FAILED);
         break;
      }
      if (res != VK_SUCCESS)
         return res;

      if (!device->trtt.queue &&
          queue->family->queueFlags & VK_QUEUE_SPARSE_BINDING_BIT)
         device->trtt.queue = queue;
   }

   return res;
}

#if GFX_VERx10 >= 125
#define maybe_for_each_shading_rate_op(name) \
   for (VkFragmentShadingRateCombinerOpKHR name = VK_FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR; \
        name <= VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR; \
        name++)
#elif GFX_VER >= 12
#define maybe_for_each_shading_rate_op(name)
#endif

/* Rather than reemitting the CPS_STATE structure everything those changes and
 * for as many viewports as needed, we can just prepare all possible cases and
 * just pick the right offset from the prepacked states when needed.
 */
void
genX(init_cps_device_state)(struct anv_device *device)
{
#if GFX_VER >= 12 && GFX_VER < 30
   void *cps_state_ptr = device->cps_states.map;

   /* Disabled CPS mode */
   for (uint32_t __v = 0; __v < MAX_VIEWPORTS; __v++) {
      /* ICL PRMs, Volume 2d: Command Reference: Structures: 3DSTATE_CPS_BODY:
       *
       *   "It is an INVALID configuration to set the CPS mode other than
       *    CPS_MODE_NONE and request per-sample dispatch in 3DSTATE_PS_EXTRA.
       *    Such configuration should be disallowed at the API level, and
       *    rendering results are undefined."
       *
       * Since we select this state when per coarse pixel is disabled and that
       * includes when per-sample dispatch is enabled, we need to ensure this
       * is set to NONE.
       */
      struct GENX(CPS_STATE) cps_state = {
         .CoarsePixelShadingMode = CPS_MODE_NONE,
      };

      GENX(CPS_STATE_pack)(NULL, cps_state_ptr, &cps_state);
      cps_state_ptr += GENX(CPS_STATE_length) * 4;
   }

   maybe_for_each_shading_rate_op(op0) {
      maybe_for_each_shading_rate_op(op1) {
         for (uint32_t x = 1; x <= 4; x *= 2) {
            for (uint32_t y = 1; y <= 4; y *= 2) {
               struct GENX(CPS_STATE) cps_state = {
                  .CoarsePixelShadingMode = CPS_MODE_CONSTANT,
                  .MinCPSizeX = x,
                  .MinCPSizeY = y,
               };

#if GFX_VERx10 >= 125
               static const uint32_t combiner_ops[] = {
                  [VK_FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR]    = PASSTHROUGH,
                  [VK_FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR] = OVERRIDE,
                  [VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR]     = HIGH_QUALITY,
                  [VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR]     = LOW_QUALITY,
                  [VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR]     = RELATIVE,
               };

               cps_state.Combiner0OpcodeforCPsize = combiner_ops[op0];
               cps_state.Combiner1OpcodeforCPsize = combiner_ops[op1];
#endif /* GFX_VERx10 >= 125 */

               for (uint32_t __v = 0; __v < MAX_VIEWPORTS; __v++) {
                  GENX(CPS_STATE_pack)(NULL, cps_state_ptr, &cps_state);
                  cps_state_ptr += GENX(CPS_STATE_length) * 4;
               }
            }
         }
      }
   }
#endif /* GFX_VER >= 12 && GFX_VER < 30 */
}

void
genX(emit_l3_config)(struct anv_batch *batch,
                     const struct anv_device *device,
                     const struct intel_l3_config *cfg)
{
#if GFX_VER < 20
   UNUSED const struct intel_device_info *devinfo = device->info;

#if GFX_VER >= 12
#define L3_ALLOCATION_REG GENX(L3ALLOC)
#define L3_ALLOCATION_REG_num GENX(L3ALLOC_num)
#else
#define L3_ALLOCATION_REG GENX(L3CNTLREG)
#define L3_ALLOCATION_REG_num GENX(L3CNTLREG_num)
#endif

   anv_batch_write_reg(batch, L3_ALLOCATION_REG, l3cr) {
      if (cfg == NULL || (GFX_VER >= 12 && cfg->n[INTEL_L3P_ALL] > 126)) {
         assert(!cfg || !(cfg->n[INTEL_L3P_SLM] || cfg->n[INTEL_L3P_URB] ||
                          cfg->n[INTEL_L3P_DC] || cfg->n[INTEL_L3P_RO] ||
                          cfg->n[INTEL_L3P_IS] || cfg->n[INTEL_L3P_C] ||
                          cfg->n[INTEL_L3P_T] || cfg->n[INTEL_L3P_TC]));
#if GFX_VER >= 12
         l3cr.L3FullWayAllocationEnable = true;
#else
         UNREACHABLE("Invalid L3$ config");
#endif
      } else {
#if GFX_VER < 11
         l3cr.SLMEnable = cfg->n[INTEL_L3P_SLM];
#endif
#if INTEL_NEEDS_WA_1406697149
         /* Wa_1406697149: Bit 9 "Error Detection Behavior Control" must be
          * set in L3CNTLREG register. The default setting of the bit is not
          * the desirable behavior.
          */
         l3cr.ErrorDetectionBehaviorControl = true;
         l3cr.UseFullWays = true;
#endif /* INTEL_NEEDS_WA_1406697149 */
         assert(cfg->n[INTEL_L3P_IS] == 0);
         assert(cfg->n[INTEL_L3P_C] == 0);
         assert(cfg->n[INTEL_L3P_T] == 0);
         l3cr.URBAllocation = cfg->n[INTEL_L3P_URB];
         l3cr.ROAllocation = cfg->n[INTEL_L3P_RO];
         l3cr.DCAllocation = cfg->n[INTEL_L3P_DC];
         l3cr.AllAllocation = cfg->n[INTEL_L3P_ALL];
      }
   }
#endif /* GFX_VER < 20 */
}

void
genX(emit_sample_pattern)(struct anv_batch *batch,
                          const struct vk_sample_locations_state *sl)
{
   assert(sl == NULL || sl->grid_size.width == 1);
   assert(sl == NULL || sl->grid_size.height == 1);

   /* See the Vulkan 1.0 spec Table 24.1 "Standard sample locations" and
    * VkPhysicalDeviceFeatures::standardSampleLocations.
    */
   anv_batch_emit(batch, GENX(3DSTATE_SAMPLE_PATTERN), sp) {
      /* The Skylake PRM Vol. 2a "3DSTATE_SAMPLE_PATTERN" says:
       *
       *    "When programming the sample offsets (for NUMSAMPLES_4 or _8
       *    and MSRASTMODE_xxx_PATTERN), the order of the samples 0 to 3
       *    (or 7 for 8X, or 15 for 16X) must have monotonically increasing
       *    distance from the pixel center. This is required to get the
       *    correct centroid computation in the device."
       *
       * However, the Vulkan spec seems to require that the the samples occur
       * in the order provided through the API. The standard sample patterns
       * have the above property that they have monotonically increasing
       * distances from the center but client-provided ones do not. As long as
       * this only affects centroid calculations as the docs say, we should be
       * ok because OpenGL and Vulkan only require that the centroid be some
       * lit sample and that it's the same for all samples in a pixel; they
       * have no requirement that it be the one closest to center.
       */
      for (uint32_t i = 1; i <= 16; i *= 2) {
         switch (i) {
         case VK_SAMPLE_COUNT_1_BIT:
            /* We don't do 1x MSAA, and we can't support custom sample
             * positions without MSAA, so always program the default for this
             * case.
             */
            INTEL_SAMPLE_POS_1X(sp._1xSample);
            break;
         case VK_SAMPLE_COUNT_2_BIT:
            if (sl && sl->per_pixel == i) {
               INTEL_SAMPLE_POS_2X_ARRAY(sp._2xSample, sl->locations);
            } else {
               INTEL_SAMPLE_POS_2X(sp._2xSample);
            }
            break;
         case VK_SAMPLE_COUNT_4_BIT:
            if (sl && sl->per_pixel == i) {
               INTEL_SAMPLE_POS_4X_ARRAY(sp._4xSample, sl->locations);
            } else {
               INTEL_SAMPLE_POS_4X(sp._4xSample);
            }
            break;
         case VK_SAMPLE_COUNT_8_BIT:
            if (sl && sl->per_pixel == i) {
               INTEL_SAMPLE_POS_8X_ARRAY(sp._8xSample, sl->locations);
            } else {
               INTEL_SAMPLE_POS_8X(sp._8xSample);
            }
            break;
         case VK_SAMPLE_COUNT_16_BIT:
            if (sl && sl->per_pixel == i) {
               INTEL_SAMPLE_POS_16X_ARRAY(sp._16xSample, sl->locations);
            } else {
               INTEL_SAMPLE_POS_16X(sp._16xSample);
            }
            break;
         default:
            UNREACHABLE("Invalid sample count");
         }
      }
   }
}

static uint32_t
vk_to_intel_tex_filter(VkFilter filter, bool anisotropyEnable)
{
   switch (filter) {
   default:
      UNREACHABLE("Invalid filter");
   case VK_FILTER_NEAREST:
      return anisotropyEnable ?
#if GFX_VER >= 30
             MAPFILTER_ANISOTROPIC_FAST :
#else
             MAPFILTER_ANISOTROPIC :
#endif
             MAPFILTER_NEAREST;
   case VK_FILTER_LINEAR:
      return anisotropyEnable ?
#if GFX_VER >= 30
             MAPFILTER_ANISOTROPIC_FAST :
#else
             MAPFILTER_ANISOTROPIC :
#endif
             MAPFILTER_LINEAR;
   }
}

static uint32_t
vk_to_intel_max_anisotropy(float ratio)
{
   return (CLAMP(ratio, 2, 16) - 2) / 2;
}

static const uint32_t vk_to_intel_mipmap_mode[] = {
   [VK_SAMPLER_MIPMAP_MODE_NEAREST]          = MIPFILTER_NEAREST,
   [VK_SAMPLER_MIPMAP_MODE_LINEAR]           = MIPFILTER_LINEAR
};

static const uint32_t vk_to_intel_tex_address[] = {
   [VK_SAMPLER_ADDRESS_MODE_REPEAT]          = TCM_WRAP,
   [VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT] = TCM_MIRROR,
   [VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE]   = TCM_CLAMP,
   [VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE] = TCM_MIRROR_ONCE,
   [VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER] = TCM_CLAMP_BORDER,
};

/* Vulkan specifies the result of shadow comparisons as:
 *     1     if   ref <op> texel,
 *     0     otherwise.
 *
 * The hardware does:
 *     0     if texel <op> ref,
 *     1     otherwise.
 *
 * So, these look a bit strange because there's both a negation
 * and swapping of the arguments involved.
 */
static const uint32_t vk_to_intel_shadow_compare_op[] = {
   [VK_COMPARE_OP_NEVER]                        = PREFILTEROP_ALWAYS,
   [VK_COMPARE_OP_LESS]                         = PREFILTEROP_LEQUAL,
   [VK_COMPARE_OP_EQUAL]                        = PREFILTEROP_NOTEQUAL,
   [VK_COMPARE_OP_LESS_OR_EQUAL]                = PREFILTEROP_LESS,
   [VK_COMPARE_OP_GREATER]                      = PREFILTEROP_GEQUAL,
   [VK_COMPARE_OP_NOT_EQUAL]                    = PREFILTEROP_EQUAL,
   [VK_COMPARE_OP_GREATER_OR_EQUAL]             = PREFILTEROP_GREATER,
   [VK_COMPARE_OP_ALWAYS]                       = PREFILTEROP_NEVER,
};

static const uint32_t vk_to_intel_sampler_reduction_mode[] = {
   [VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE] = STD_FILTER,
   [VK_SAMPLER_REDUCTION_MODE_MIN]              = MINIMUM,
   [VK_SAMPLER_REDUCTION_MODE_MAX]              = MAXIMUM,
};

VkResult genX(CreateSampler)(
    VkDevice                                    _device,
    const VkSamplerCreateInfo*                  pCreateInfo,
    const VkAllocationCallbacks*                pAllocator,
    VkSampler*                                  pSampler)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   struct anv_sampler *sampler;

   sampler = vk_sampler_create(&device->vk, pCreateInfo,
                               pAllocator, sizeof(*sampler));
   if (!sampler)
      return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);

   const struct vk_format_ycbcr_info *ycbcr_info =
      sampler->vk.format != VK_FORMAT_UNDEFINED ?
      vk_format_get_ycbcr_info(sampler->vk.format) : NULL;
   assert((ycbcr_info == NULL) == (sampler->vk.ycbcr_conversion == NULL));

   sampler->n_planes = ycbcr_info ? ycbcr_info->n_planes : 1;

   uint32_t border_color_stride = 64;
   uint32_t border_color_offset;
   void *border_color_ptr;
   if (sampler->vk.border_color <= VK_BORDER_COLOR_INT_OPAQUE_WHITE) {
      border_color_offset = device->border_colors.offset +
                            pCreateInfo->borderColor *
                            border_color_stride;
      border_color_ptr = device->border_colors.map +
                         pCreateInfo->borderColor * border_color_stride;
   } else {
      assert(vk_border_color_is_custom(sampler->vk.border_color));
      if (pCreateInfo->flags & VK_SAMPLER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT) {
         const VkOpaqueCaptureDescriptorDataCreateInfoEXT *opaque_info =
            vk_find_struct_const(pCreateInfo->pNext,
                                 OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT);
         if (opaque_info) {
            uint32_t alloc_idx = *((const uint32_t *)opaque_info->opaqueCaptureDescriptorData);
            sampler->custom_border_color_state =
               anv_state_reserved_array_pool_alloc_index(&device->custom_border_colors, alloc_idx);
         } else {
            sampler->custom_border_color_state =
               anv_state_reserved_array_pool_alloc(&device->custom_border_colors, true);
         }
      } else {
         sampler->custom_border_color_state =
            anv_state_reserved_array_pool_alloc(&device->custom_border_colors, false);
      }
      if (sampler->custom_border_color_state.alloc_size == 0)
         return vk_error(device, VK_ERROR_OUT_OF_DEVICE_MEMORY);

      border_color_offset = sampler->custom_border_color_state.offset;
      border_color_ptr = sampler->custom_border_color_state.map;

      union isl_color_value color = { .u32 = {
         sampler->vk.border_color_value.uint32[0],
         sampler->vk.border_color_value.uint32[1],
         sampler->vk.border_color_value.uint32[2],
         sampler->vk.border_color_value.uint32[3],
      } };

      const struct anv_format *format_desc =
         sampler->vk.format != VK_FORMAT_UNDEFINED ?
         anv_get_format(device->physical, sampler->vk.format) : NULL;

      if (format_desc && format_desc->n_planes == 1 &&
          !isl_swizzle_is_identity(format_desc->planes[0].swizzle)) {
         const struct anv_format_plane *fmt_plane = &format_desc->planes[0];

         assert(!isl_format_has_int_channel(fmt_plane->isl_format));
         color = isl_color_value_swizzle(color, fmt_plane->swizzle, true);
      }

      memcpy(border_color_ptr, color.u32, sizeof(color));
   }

   const bool seamless_cube =
      !(pCreateInfo->flags & VK_SAMPLER_CREATE_NON_SEAMLESS_CUBE_MAP_BIT_EXT);

   for (unsigned p = 0; p < sampler->n_planes; p++) {
      const bool plane_has_chroma =
         ycbcr_info && ycbcr_info->planes[p].has_chroma;
      const VkFilter min_filter =
         plane_has_chroma ? sampler->vk.ycbcr_conversion->state.chroma_filter :
                            pCreateInfo->minFilter;
      const VkFilter mag_filter =
         plane_has_chroma ? sampler->vk.ycbcr_conversion->state.chroma_filter :
                            pCreateInfo->magFilter;
      const bool force_addr_rounding =
            device->physical->instance->force_filter_addr_rounding;
      const bool enable_min_filter_addr_rounding =
            force_addr_rounding || min_filter != VK_FILTER_NEAREST;
      const bool enable_mag_filter_addr_rounding =
            force_addr_rounding || mag_filter != VK_FILTER_NEAREST;
      /* From Broadwell PRM, SAMPLER_STATE:
       *   "Mip Mode Filter must be set to MIPFILTER_NONE for Planar YUV surfaces."
       */
      enum isl_format plane0_isl_format = sampler->vk.ycbcr_conversion ?
         anv_get_format(device->physical, sampler->vk.format)->planes[0].isl_format :
         ISL_FORMAT_UNSUPPORTED;
      const bool isl_format_is_planar_yuv =
         plane0_isl_format != ISL_FORMAT_UNSUPPORTED &&
         isl_format_is_yuv(plane0_isl_format) &&
         isl_format_is_planar(plane0_isl_format);

      const uint32_t mip_filter_mode =
         isl_format_is_planar_yuv ?
         MIPFILTER_NONE : vk_to_intel_mipmap_mode[pCreateInfo->mipmapMode];

      struct GENX(SAMPLER_STATE) sampler_state = {
         .SamplerDisable = false,
         .TextureBorderColorMode = DX10OGL,

#if GFX_VER >= 11
         /* This field is marked as disabled on Gfx20+ */
         .CPSLODCompensationEnable = device->info->ver < 20,
#endif

         .LODPreClampMode = CLAMP_MODE_OGL,

         .MipModeFilter = mip_filter_mode,
         .MagModeFilter = vk_to_intel_tex_filter(mag_filter, pCreateInfo->anisotropyEnable),
         .MinModeFilter = vk_to_intel_tex_filter(min_filter, pCreateInfo->anisotropyEnable),
         .TextureLODBias = CLAMP(pCreateInfo->mipLodBias, -16, 15.996),
         .AnisotropicAlgorithm =
            pCreateInfo->anisotropyEnable ? EWAApproximation : LEGACY,
         .MinLOD = CLAMP(pCreateInfo->minLod, 0, 14),
         .MaxLOD = CLAMP(pCreateInfo->maxLod, 0, 14),
         .ChromaKeyEnable = 0,
         .ChromaKeyIndex = 0,
         .ChromaKeyMode = 0,
         .ShadowFunction =
            vk_to_intel_shadow_compare_op[pCreateInfo->compareEnable ?
                                        pCreateInfo->compareOp : VK_COMPARE_OP_NEVER],
         .CubeSurfaceControlMode = seamless_cube ? OVERRIDE : PROGRAMMED,

         .LODClampMagnificationMode = MIPNONE,

         .MaximumAnisotropy = vk_to_intel_max_anisotropy(pCreateInfo->maxAnisotropy),
         .RAddressMinFilterRoundingEnable = enable_min_filter_addr_rounding,
         .RAddressMagFilterRoundingEnable = enable_mag_filter_addr_rounding,
         .VAddressMinFilterRoundingEnable = enable_min_filter_addr_rounding,
         .VAddressMagFilterRoundingEnable = enable_mag_filter_addr_rounding,
         .UAddressMinFilterRoundingEnable = enable_min_filter_addr_rounding,
         .UAddressMagFilterRoundingEnable = enable_mag_filter_addr_rounding,
         .TrilinearFilterQuality = 0,
         .NonnormalizedCoordinateEnable = pCreateInfo->unnormalizedCoordinates,
         .TCXAddressControlMode = vk_to_intel_tex_address[pCreateInfo->addressModeU],
         .TCYAddressControlMode = vk_to_intel_tex_address[pCreateInfo->addressModeV],
         .TCZAddressControlMode = vk_to_intel_tex_address[pCreateInfo->addressModeW],

         .ReductionType =
            vk_to_intel_sampler_reduction_mode[sampler->vk.reduction_mode],
         .ReductionTypeEnable =
            sampler->vk.reduction_mode != VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE,
      };

      /* Pack a version of the SAMPLER_STATE without the border color. We'll
       * use it to store into the shader cache and also for hashing.
       */
      GENX(SAMPLER_STATE_pack)(NULL, sampler->state_no_bc[p], &sampler_state);

      /* Put border color after the hashing, we don't want the allocation
       * order of border colors to influence the hash. We just need th
       * parameters to be hashed.
       */
      sampler_state.BorderColorPointer = border_color_offset;
      GENX(SAMPLER_STATE_pack)(NULL, sampler->state[p], &sampler_state);
   }

   memcpy(sampler->embedded_key.sampler,
          sampler->state_no_bc[0],
          sizeof(sampler->embedded_key.sampler));
   memcpy(sampler->embedded_key.color,
          sampler->vk.border_color_value.uint32,
          sizeof(sampler->embedded_key.color));

   /* If we have bindless, allocate enough samplers.  We allocate 32 bytes
    * for each sampler instead of 16 bytes because we want all bindless
    * samplers to be 32-byte aligned so we don't have to use indirect
    * sampler messages on them.
    */
   sampler->bindless_state =
      anv_state_pool_alloc(&device->dynamic_state_pool,
                           sampler->n_planes * 32, 32);
   if (sampler->bindless_state.map) {
      memcpy(sampler->bindless_state.map, sampler->state,
             sampler->n_planes * GENX(SAMPLER_STATE_length) * 4);
   }

   *pSampler = anv_sampler_to_handle(sampler);

   return VK_SUCCESS;
}

void
genX(emit_embedded_sampler)(struct anv_device *device,
                            struct anv_embedded_sampler *sampler,
                            struct anv_pipeline_embedded_sampler_binding *binding)
{
   sampler->ref_cnt = 1;
   memcpy(&sampler->key, &binding->key, sizeof(binding->key));

   sampler->border_color_state =
      anv_state_pool_alloc(&device->dynamic_state_pool,
                           sizeof(struct gfx8_border_color), 64);
   memcpy(sampler->border_color_state.map,
          binding->key.color,
          sizeof(binding->key.color));

   sampler->sampler_state =
      anv_state_pool_alloc(&device->dynamic_state_pool,
                           ANV_SAMPLER_STATE_SIZE, 32);

   struct GENX(SAMPLER_STATE) sampler_state = {
      .BorderColorPointer = sampler->border_color_state.offset,
   };
   uint32_t dwords[GENX(SAMPLER_STATE_length)];
   GENX(SAMPLER_STATE_pack)(NULL, dwords, &sampler_state);

   for (uint32_t i = 0; i < GENX(SAMPLER_STATE_length); i++) {
      ((uint32_t *)sampler->sampler_state.map)[i] =
         dwords[i] | binding->key.sampler[i];
   }
}

/* Wa_14015814527
 *
 * Check if task shader was utilized within cmd_buffer, if so
 * commit empty URB states and null prim.
 */
void
genX(apply_task_urb_workaround)(struct anv_cmd_buffer *cmd_buffer)
{
   if (!anv_cmd_buffer_is_render_queue(cmd_buffer))
      return;

#if GFX_VERx10 >= 125
   const struct intel_device_info *devinfo = &cmd_buffer->device->physical->info;

   if (!intel_needs_workaround(devinfo, 16014390852))
      return;

   if (cmd_buffer->state.current_pipeline != _3D ||
       !cmd_buffer->state.gfx.used_task_shader)
      return;

   cmd_buffer->state.gfx.used_task_shader = false;

   /* Wa_14015821291 mentions that WA below is not required if we have
    * a pipeline flush going on. It will get flushed during
    * cmd_buffer_flush_state before draw.
    */
   if ((cmd_buffer->state.pending_pipe_bits & ANV_PIPE_CS_STALL_BIT))
      return;

   for (int i = 0; i <= MESA_SHADER_GEOMETRY; i++) {
#if GFX_VER >= 12
      anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_URB_ALLOC_VS), urb) {
         urb._3DCommandSubOpcode += i;
      }
#else
      anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_URB_VS), urb) {
         urb._3DCommandSubOpcode += i;
      }
#endif
   }

   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_URB_ALLOC_MESH), zero);
   anv_batch_emit(&cmd_buffer->batch, GENX(3DSTATE_URB_ALLOC_TASK), zero);

   /* Issue 'nullprim' to commit the state. */
   genx_batch_emit_pipe_control_write
      (&cmd_buffer->batch, cmd_buffer->device->info,
       cmd_buffer->state.current_pipeline,
       WriteImmediateData, cmd_buffer->device->workaround_address, 0, 0);
#endif
}

VkResult
genX(init_trtt_context_state)(struct anv_async_submit *submit)
{
#if GFX_VER >= 12
   struct anv_queue *queue = submit->queue;
   struct anv_device *device = queue->device;
   struct anv_trtt *trtt = &device->trtt;
   struct anv_batch *batch = &submit->batch;

   assert((trtt->l3_addr & 0xFFF) == 0);
   uint32_t l3_addr_low = (trtt->l3_addr & 0xFFFFF000) >> 12;
   uint32_t l3_addr_high = (trtt->l3_addr >> 32) & 0xFFFF;

   anv_batch_write_reg(batch, GENX(GFX_TRTT_INVAL), trtt_inval)
      trtt_inval.InvalidTileDetectionValue = ANV_TRTT_L1_INVALID_TILE_VAL;
   anv_batch_write_reg(batch, GENX(GFX_TRTT_NULL), trtt_null)
      trtt_null.NullTileDetectionValue = ANV_TRTT_L1_NULL_TILE_VAL;
   anv_batch_write_reg(batch, GENX(GFX_TRTT_L3_BASE_LOW), trtt_base_low)
      trtt_base_low.TRVAL3PointerLowerAddress = l3_addr_low;
   anv_batch_write_reg(batch, GENX(GFX_TRTT_L3_BASE_HIGH), trtt_base_high)
      trtt_base_high.TRVAL3PointerUpperAddress = l3_addr_high;

   anv_batch_write_reg(batch, GENX(BLT_TRTT_INVAL), trtt_inval)
      trtt_inval.InvalidTileDetectionValue = ANV_TRTT_L1_INVALID_TILE_VAL;
   anv_batch_write_reg(batch, GENX(BLT_TRTT_NULL), trtt_null)
      trtt_null.NullTileDetectionValue = ANV_TRTT_L1_NULL_TILE_VAL;
   anv_batch_write_reg(batch, GENX(BLT_TRTT_L3_BASE_LOW), trtt_base_low)
      trtt_base_low.TRVAL3PointerLowerAddress = l3_addr_low;
   anv_batch_write_reg(batch, GENX(BLT_TRTT_L3_BASE_HIGH), trtt_base_high)
      trtt_base_high.TRVAL3PointerUpperAddress = l3_addr_high;

   anv_batch_write_reg(batch, GENX(COMP_CTX0_TRTT_INVAL), trtt_inval)
      trtt_inval.InvalidTileDetectionValue = ANV_TRTT_L1_INVALID_TILE_VAL;
   anv_batch_write_reg(batch, GENX(COMP_CTX0_TRTT_NULL), trtt_null)
      trtt_null.NullTileDetectionValue = ANV_TRTT_L1_NULL_TILE_VAL;
   anv_batch_write_reg(batch, GENX(COMP_CTX0_TRTT_L3_BASE_LOW), trtt_base_low)
      trtt_base_low.TRVAL3PointerLowerAddress = l3_addr_low;
   anv_batch_write_reg(batch, GENX(COMP_CTX0_TRTT_L3_BASE_HIGH), trtt_base_high)
      trtt_base_high.TRVAL3PointerUpperAddress = l3_addr_high;

#if GFX_VER >= 20
   uint32_t trva_base = device->physical->va.trtt.addr >> 44;
   anv_batch_write_reg(batch, GENX(GFX_TRTT_VA_RANGE), trtt_va_range)
      trtt_va_range.TRVABase = trva_base;
   anv_batch_write_reg(batch, GENX(BLT_TRTT_VA_RANGE), trtt_va_range)
      trtt_va_range.TRVABase = trva_base;
   anv_batch_write_reg(batch, GENX(COMP_CTX0_TRTT_VA_RANGE), trtt_va_range)
      trtt_va_range.TRVABase = trva_base;
#else
   anv_batch_write_reg(batch, GENX(GFX_TRTT_VA_RANGE), trtt_va_range) {
      trtt_va_range.TRVAMaskValue = 0xF;
      trtt_va_range.TRVADataValue = 0xF;
   }
   anv_batch_write_reg(batch, GENX(BLT_TRTT_VA_RANGE), trtt_va_range) {
      trtt_va_range.TRVAMaskValue = 0xF;
      trtt_va_range.TRVADataValue = 0xF;
   }
   anv_batch_write_reg(batch, GENX(COMP_CTX0_TRTT_VA_RANGE), trtt_va_range) {
      trtt_va_range.TRVAMaskValue = 0xF;
      trtt_va_range.TRVADataValue = 0xF;
   }
#endif

   /* Enabling TR-TT needs to be done after setting up the other registers.
    */
   anv_batch_write_reg(batch, GENX(GFX_TRTT_CR), trtt_cr)
      trtt_cr.TRTTEnable = true;
   anv_batch_write_reg(batch, GENX(BLT_TRTT_CR), trtt_cr)
      trtt_cr.TRTTEnable = true;
   anv_batch_write_reg(batch, GENX(COMP_CTX0_TRTT_CR), trtt_cr)
      trtt_cr.TRTTEnable = true;

   if (queue->family->engine_class != INTEL_ENGINE_CLASS_COPY) {
      genx_batch_emit_pipe_control(batch, device->info, _3D,
                                   ANV_PIPE_CS_STALL_BIT |
                                   ANV_PIPE_TLB_INVALIDATE_BIT);
   }
#endif
   return VK_SUCCESS;
}

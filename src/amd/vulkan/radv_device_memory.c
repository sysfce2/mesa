/*
 * Copyright © 2016 Red Hat.
 * Copyright © 2016 Bas Nieuwenhuizen
 *
 * based in part on anv driver which is:
 * Copyright © 2015 Intel Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "radv_device_memory.h"
#include "radv_android.h"
#include "radv_buffer.h"
#include "radv_debug.h"
#include "radv_entrypoints.h"
#include "radv_image.h"
#include "radv_rmv.h"

#include "vk_debug_utils.h"
#include "vk_log.h"

static void
radv_device_memory_emit_report(struct radv_device *device, struct radv_device_memory *mem, bool is_alloc,
                               VkResult result)
{
   if (likely(!device->vk.memory_reports))
      return;

   VkDeviceMemoryReportEventTypeEXT type;
   if (result != VK_SUCCESS) {
      type = VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT;
   } else if (is_alloc) {
      type = mem->import_handle_type ? VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT
                                     : VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT;
   } else {
      type = mem->import_handle_type ? VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT
                                     : VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT;
   }

   vk_emit_device_memory_report(&device->vk, type, mem->bo->obj_id, mem->bo->size, VK_OBJECT_TYPE_DEVICE_MEMORY,
                                (uintptr_t)(mem), mem->heap_index);
}

void
radv_free_memory(struct radv_device *device, const VkAllocationCallbacks *pAllocator, struct radv_device_memory *mem)
{
   if (mem == NULL)
      return;

#if RADV_SUPPORT_ANDROID_HARDWARE_BUFFER
   if (mem->android_hardware_buffer)
      AHardwareBuffer_release(mem->android_hardware_buffer);
#endif

   if (mem->bo) {
      if (device->overallocation_disallowed) {
         mtx_lock(&device->overallocation_mutex);
         device->allocated_memory_size[mem->heap_index] -= mem->alloc_size;
         mtx_unlock(&device->overallocation_mutex);
      }

      if (device->use_global_bo_list)
         device->ws->buffer_make_resident(device->ws, mem->bo, false);
      radv_bo_destroy(device, &mem->base, mem->bo);
      mem->bo = NULL;
   }

   radv_rmv_log_resource_destroy(device, (uint64_t)radv_device_memory_to_handle(mem));
   vk_object_base_finish(&mem->base);
   vk_free2(&device->vk.alloc, pAllocator, mem);
}

VkResult
radv_alloc_memory(struct radv_device *device, const VkMemoryAllocateInfo *pAllocateInfo,
                  const VkAllocationCallbacks *pAllocator, VkDeviceMemory *pMem, bool is_internal)
{
   struct radv_device_memory *mem;
   VkResult result;
   enum radeon_bo_domain domain;
   uint32_t flags = 0;

   assert(pAllocateInfo->sType == VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO);

   const VkImportMemoryFdInfoKHR *import_info = vk_find_struct_const(pAllocateInfo->pNext, IMPORT_MEMORY_FD_INFO_KHR);
   const VkMemoryDedicatedAllocateInfo *dedicate_info =
      vk_find_struct_const(pAllocateInfo->pNext, MEMORY_DEDICATED_ALLOCATE_INFO);
   const VkExportMemoryAllocateInfo *export_info =
      vk_find_struct_const(pAllocateInfo->pNext, EXPORT_MEMORY_ALLOCATE_INFO);
   const struct VkImportAndroidHardwareBufferInfoANDROID *ahb_import_info =
      vk_find_struct_const(pAllocateInfo->pNext, IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID);
   const VkImportMemoryHostPointerInfoEXT *host_ptr_info =
      vk_find_struct_const(pAllocateInfo->pNext, IMPORT_MEMORY_HOST_POINTER_INFO_EXT);
   const struct VkMemoryAllocateFlagsInfo *flags_info =
      vk_find_struct_const(pAllocateInfo->pNext, MEMORY_ALLOCATE_FLAGS_INFO);

   const struct wsi_memory_allocate_info *wsi_info =
      vk_find_struct_const(pAllocateInfo->pNext, WSI_MEMORY_ALLOCATE_INFO_MESA);

   if (pAllocateInfo->allocationSize == 0 && !ahb_import_info &&
       !(export_info &&
         (export_info->handleTypes & VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID))) {
      /* Apparently, this is allowed */
      *pMem = VK_NULL_HANDLE;
      return VK_SUCCESS;
   }

   mem = vk_zalloc2(&device->vk.alloc, pAllocator, sizeof(*mem), 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (mem == NULL)
      return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);

   vk_object_base_init(&device->vk, &mem->base, VK_OBJECT_TYPE_DEVICE_MEMORY);

   if (dedicate_info) {
      mem->image = radv_image_from_handle(dedicate_info->image);
      mem->buffer = radv_buffer_from_handle(dedicate_info->buffer);
   } else {
      mem->image = NULL;
      mem->buffer = NULL;
   }

   if (wsi_info && wsi_info->implicit_sync) {
      flags |= RADEON_FLAG_IMPLICIT_SYNC;

      /* Mark the linear prime buffer (aka the destination of the prime blit
       * as uncached.
       */
      if (mem->buffer)
         flags |= RADEON_FLAG_VA_UNCACHED;
   }

   float priority_float = 0.5;
   const struct VkMemoryPriorityAllocateInfoEXT *priority_ext =
      vk_find_struct_const(pAllocateInfo->pNext, MEMORY_PRIORITY_ALLOCATE_INFO_EXT);
   if (priority_ext)
      priority_float = priority_ext->priority;

   uint64_t replay_address = 0;
   const VkMemoryOpaqueCaptureAddressAllocateInfo *replay_info =
      vk_find_struct_const(pAllocateInfo->pNext, MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO);
   if (replay_info && replay_info->opaqueCaptureAddress)
      replay_address = replay_info->opaqueCaptureAddress;

   unsigned priority =
      MIN2(RADV_BO_PRIORITY_APPLICATION_MAX - 1, (int)(priority_float * RADV_BO_PRIORITY_APPLICATION_MAX));

   mem->user_ptr = NULL;

#if RADV_SUPPORT_ANDROID_HARDWARE_BUFFER
   mem->android_hardware_buffer = NULL;
#endif

   if (ahb_import_info) {
      result = radv_import_ahb_memory(device, mem, priority, ahb_import_info);
      if (result != VK_SUCCESS)
         goto fail;
      if (ahb_import_info->sType == VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID)
         mem->import_handle_type = VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID;
   } else if (export_info &&
              (export_info->handleTypes & VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID)) {
      result = radv_create_ahb_memory(device, mem, priority, pAllocateInfo);
      if (result != VK_SUCCESS)
         goto fail;
      mem->export_handle_type = export_info->handleTypes;
   } else if (import_info) {
      assert(import_info->handleType == VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT ||
             import_info->handleType == VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT);
      result = radv_bo_from_fd(device, import_info->fd, priority, mem, NULL);
      if (result != VK_SUCCESS) {
         goto fail;
      } else {
         close(import_info->fd);
      }
      mem->import_handle_type = import_info->handleType;

      if (mem->image && mem->image->plane_count == 1 && !vk_format_is_depth_or_stencil(mem->image->vk.format) &&
          mem->image->vk.samples == 1 && mem->image->vk.tiling != VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
         struct radeon_bo_metadata metadata;
         device->ws->buffer_get_metadata(device->ws, mem->bo, &metadata);

         struct radv_image_create_info create_info = {.no_metadata_planes = true, .bo_metadata = &metadata};

         /* This gives a basic ability to import radeonsi images
          * that don't have DCC. This is not guaranteed by any
          * spec and can be removed after we support modifiers. */
         result = radv_image_create_layout(device, create_info, NULL, NULL, mem->image);
         if (result != VK_SUCCESS) {
            radv_bo_destroy(device, &mem->base, mem->bo);
            goto fail;
         }
      }
   } else if (host_ptr_info) {
      assert(host_ptr_info->handleType == VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT);
      result = radv_bo_from_ptr(device, host_ptr_info->pHostPointer, pAllocateInfo->allocationSize, priority, mem);
      if (result != VK_SUCCESS) {
         goto fail;
      } else {
         mem->user_ptr = host_ptr_info->pHostPointer;
      }
      mem->import_handle_type = host_ptr_info->handleType;
   } else {
      const struct radv_physical_device *pdev = radv_device_physical(device);
      const struct radv_instance *instance = radv_physical_device_instance(pdev);
      uint64_t alloc_size = align64(pAllocateInfo->allocationSize, 4096);
      uint32_t heap_index;

      heap_index = pdev->memory_properties.memoryTypes[pAllocateInfo->memoryTypeIndex].heapIndex;
      domain = pdev->memory_domains[pAllocateInfo->memoryTypeIndex];
      flags |= pdev->memory_flags[pAllocateInfo->memoryTypeIndex];

      if (export_info && export_info->handleTypes) {
         /* Setting RADEON_FLAG_GTT_WC in case the bo is spilled to GTT.  This is important when the
          * foreign queue is the display engine of iGPU.  The carveout of iGPU can be tiny and the
          * kernel driver refuses to spill without the flag.
          *
          * This covers any external memory user, including WSI.
          */
         if (domain == RADEON_DOMAIN_VRAM)
            flags |= RADEON_FLAG_GTT_WC;
      } else if (!import_info) {
         /* neither export nor import */
         flags |= RADEON_FLAG_NO_INTERPROCESS_SHARING;
         if (device->use_global_bo_list) {
            flags |= RADEON_FLAG_PREFER_LOCAL_BO;
         }
      }

      if (flags_info && flags_info->flags & VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT)
         flags |= RADEON_FLAG_REPLAYABLE;

      if ((flags_info && flags_info->flags & VK_MEMORY_ALLOCATE_ZERO_INITIALIZE_BIT_EXT) ||
          radv_device_should_clear_vram(device))
         flags |= RADEON_FLAG_ZERO_VRAM;

      /* On GFX12, DCC is transparent to the userspace driver and PTE.DCC is
       * set per buffer allocation. Only VRAM can have DCC. When the kernel
       * moves a buffer from VRAM->GTT it decompresses. When the kernel moves
       * it from GTT->VRAM it recompresses but only if WRITE_COMPRESS_DISABLE=0
       * (see DCC tiling flags).
       */
      if (pdev->info.gfx_level >= GFX12 && pdev->info.gfx12_supports_dcc_write_compress_disable &&
          domain == RADEON_DOMAIN_VRAM && (flags & RADEON_FLAG_NO_CPU_ACCESS) &&
          !(instance->debug_flags & RADV_DEBUG_NO_DCC)) {
         flags |= RADEON_FLAG_GFX12_ALLOW_DCC;
      }

      if (device->overallocation_disallowed) {
         uint64_t total_size = pdev->memory_properties.memoryHeaps[heap_index].size;

         mtx_lock(&device->overallocation_mutex);
         if (device->allocated_memory_size[heap_index] + alloc_size > total_size) {
            mtx_unlock(&device->overallocation_mutex);
            result = VK_ERROR_OUT_OF_DEVICE_MEMORY;
            goto fail;
         }
         device->allocated_memory_size[heap_index] += alloc_size;
         mtx_unlock(&device->overallocation_mutex);
      }

      result = radv_bo_create(device, &mem->base, alloc_size, pdev->info.max_alignment, domain, flags, priority,
                              replay_address, is_internal, &mem->bo);

      if (result != VK_SUCCESS) {
         if (device->overallocation_disallowed) {
            mtx_lock(&device->overallocation_mutex);
            device->allocated_memory_size[heap_index] -= alloc_size;
            mtx_unlock(&device->overallocation_mutex);
         }
         goto fail;
      }

      if (flags & RADEON_FLAG_GFX12_ALLOW_DCC) {
         if (mem->image) {
            /* Set BO metadata (including DCC tiling flags) for dedicated
             * allocations because compressed writes are enabled and the kernel
             * requires a DCC view for recompression.
             */
            radv_image_bo_set_metadata(device, mem->image, mem->bo);
         } else {
            /* Otherwise, disable compressed writes to prevent recompression
             * when the BO is moved back to VRAM because it's not yet possible
             * to set DCC tiling flags per range for suballocations. The only
             * problem is that we will loose DCC after migration but that
             * should happen rarely.
             */
            struct radeon_bo_metadata md = {0};

            md.u.gfx12.dcc_write_compress_disable = true;

            device->ws->buffer_set_metadata(device->ws, mem->bo, &md);
         }
      }

      mem->heap_index = heap_index;
      mem->alloc_size = alloc_size;
   }

   if (!wsi_info) {
      if (device->use_global_bo_list) {
         result = device->ws->buffer_make_resident(device->ws, mem->bo, true);
         if (result != VK_SUCCESS)
            goto fail;
      }
   }

   *pMem = radv_device_memory_to_handle(mem);
   radv_rmv_log_heap_create(device, *pMem, is_internal, flags_info ? flags_info->flags : 0);

   radv_device_memory_emit_report(device, mem, /* is_alloc */ true, result);

   return VK_SUCCESS;

fail:
   radv_free_memory(device, pAllocator, mem);
   radv_device_memory_emit_report(device, mem, /* is_alloc */ true, result);

   return result;
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_AllocateMemory(VkDevice _device, const VkMemoryAllocateInfo *pAllocateInfo,
                    const VkAllocationCallbacks *pAllocator, VkDeviceMemory *pMem)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   return radv_alloc_memory(device, pAllocateInfo, pAllocator, pMem, false);
}

VKAPI_ATTR void VKAPI_CALL
radv_FreeMemory(VkDevice _device, VkDeviceMemory _mem, const VkAllocationCallbacks *pAllocator)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   VK_FROM_HANDLE(radv_device_memory, mem, _mem);

   if (mem)
      radv_device_memory_emit_report(device, mem, /* is_alloc */ false, VK_SUCCESS);

   radv_free_memory(device, pAllocator, mem);
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_MapMemory2(VkDevice _device, const VkMemoryMapInfo *pMemoryMapInfo, void **ppData)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   VK_FROM_HANDLE(radv_device_memory, mem, pMemoryMapInfo->memory);
   void *fixed_address = NULL;
   bool use_fixed_address = false;

   if (pMemoryMapInfo->flags & VK_MEMORY_MAP_PLACED_BIT_EXT) {
      const VkMemoryMapPlacedInfoEXT *placed_info =
         vk_find_struct_const(pMemoryMapInfo->pNext, MEMORY_MAP_PLACED_INFO_EXT);
      if (placed_info) {
         fixed_address = placed_info->pPlacedAddress;
         use_fixed_address = true;
      }
   }

   if (mem->user_ptr)
      *ppData = mem->user_ptr;
   else
      *ppData = device->ws->buffer_map(device->ws, mem->bo, use_fixed_address, fixed_address);

   if (*ppData) {
      vk_rmv_log_cpu_map(&device->vk, mem->bo->va, false);
      *ppData = (uint8_t *)*ppData + pMemoryMapInfo->offset;
      return VK_SUCCESS;
   }

   return vk_error(device, VK_ERROR_MEMORY_MAP_FAILED);
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_UnmapMemory2(VkDevice _device, const VkMemoryUnmapInfo *pMemoryUnmapInfo)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   VK_FROM_HANDLE(radv_device_memory, mem, pMemoryUnmapInfo->memory);

   vk_rmv_log_cpu_map(&device->vk, mem->bo->va, true);
   if (mem->user_ptr == NULL)
      device->ws->buffer_unmap(device->ws, mem->bo, (pMemoryUnmapInfo->flags & VK_MEMORY_UNMAP_RESERVE_BIT_EXT));

   return VK_SUCCESS;
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_FlushMappedMemoryRanges(VkDevice _device, uint32_t memoryRangeCount, const VkMappedMemoryRange *pMemoryRanges)
{
   return VK_SUCCESS;
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_InvalidateMappedMemoryRanges(VkDevice _device, uint32_t memoryRangeCount, const VkMappedMemoryRange *pMemoryRanges)
{
   return VK_SUCCESS;
}

VKAPI_ATTR uint64_t VKAPI_CALL
radv_GetDeviceMemoryOpaqueCaptureAddress(VkDevice device, const VkDeviceMemoryOpaqueCaptureAddressInfo *pInfo)
{
   VK_FROM_HANDLE(radv_device_memory, mem, pInfo->memory);
   return radv_buffer_get_va(mem->bo);
}

VKAPI_ATTR void VKAPI_CALL
radv_GetDeviceMemoryCommitment(VkDevice device, VkDeviceMemory memory, VkDeviceSize *pCommittedMemoryInBytes)
{
   *pCommittedMemoryInBytes = 0;
}

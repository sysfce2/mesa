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

/**
 * This file implements VkQueue
 */

#include "anv_private.h"

#include "i915/anv_queue.h"
#include "xe/anv_queue.h"

#include "vk_common_entrypoints.h"

static VkResult
anv_create_engine(struct anv_device *device,
                  struct anv_queue *queue,
                  const VkDeviceQueueCreateInfo *pCreateInfo)
{
   switch (device->info->kmd_type) {
   case INTEL_KMD_TYPE_I915:
      return anv_i915_create_engine(device, queue, pCreateInfo);
   case INTEL_KMD_TYPE_XE:
      return anv_xe_create_engine(device, queue, pCreateInfo);
   default:
      UNREACHABLE("Missing");
      return VK_ERROR_UNKNOWN;
   }
}

static void
anv_destroy_engine(struct anv_queue *queue)
{
   struct anv_device *device = queue->device;
   switch (device->info->kmd_type) {
   case INTEL_KMD_TYPE_I915:
      anv_i915_destroy_engine(device, queue);
      break;
   case INTEL_KMD_TYPE_XE:
      anv_xe_destroy_engine(device, queue);
      break;
   default:
      UNREACHABLE("Missing");
   }
}

VkResult
anv_queue_init(struct anv_device *device, struct anv_queue *queue,
               const VkDeviceQueueCreateInfo *pCreateInfo,
               uint32_t index_in_family)
{
   struct anv_physical_device *pdevice = device->physical;
   assert(queue->vk.queue_family_index < pdevice->queue.family_count);
   struct anv_queue_family *queue_family =
      &device->physical->queue.families[pCreateInfo->queueFamilyIndex];
   VkResult result;

   result = vk_queue_init(&queue->vk, &device->vk, pCreateInfo,
                          index_in_family);
   if (result != VK_SUCCESS)
      return result;

   queue->vk.driver_submit = anv_queue_submit;
   queue->device = device;
   queue->family = queue_family;
   queue->decoder = &device->decoder[queue->vk.queue_family_index];

   result = anv_create_engine(device, queue, pCreateInfo);
   if (result != VK_SUCCESS) {
      vk_queue_finish(&queue->vk);
      return result;
   }

   /* Add a debug fence to wait on submissions if we're using the synchronized
    * submission feature, shader-print feature, or BVH dump.
    */
   if (INTEL_DEBUG(DEBUG_SYNC) ||
       INTEL_DEBUG(DEBUG_SHADER_PRINT) ||
       INTEL_DEBUG_BVH_ANY) {
      result = vk_sync_create(&device->vk,
                              &device->physical->sync_syncobj_type,
                              0, 0, &queue->sync);
      if (result != VK_SUCCESS) {
         anv_queue_finish(queue);
         return result;
      }
   }

   if (queue_family->engine_class == INTEL_ENGINE_CLASS_COPY ||
       queue_family->engine_class == INTEL_ENGINE_CLASS_COMPUTE) {
      result = vk_sync_create(&device->vk,
                              &device->physical->sync_syncobj_type,
                              0, 0, &queue->companion_sync);
      if (result != VK_SUCCESS) {
         anv_queue_finish(queue);
         return result;
      }
   }

   return VK_SUCCESS;
}

void
anv_queue_finish(struct anv_queue *queue)
{
   if (queue->init_submit) {
      anv_async_submit_wait(queue->init_submit);
      anv_async_submit_destroy(queue->init_submit);
   }
   if (queue->init_companion_submit) {
      anv_async_submit_wait(queue->init_companion_submit);
      anv_async_submit_destroy(queue->init_companion_submit);
   }

   if (queue->sync)
      vk_sync_destroy(&queue->device->vk, queue->sync);

   if (queue->companion_sync)
      vk_sync_destroy(&queue->device->vk, queue->companion_sync);

   anv_destroy_engine(queue);
   vk_queue_finish(&queue->vk);
}

VkResult
anv_QueueWaitIdle(VkQueue _queue)
{
   VK_FROM_HANDLE(anv_queue, queue, _queue);
   struct anv_device *device = queue->device;

   switch (device->info->kmd_type) {
   case INTEL_KMD_TYPE_XE:
      if (queue->vk.submit.mode != VK_QUEUE_SUBMIT_MODE_THREADED) {
         int ret = anv_xe_wait_exec_queue_idle(device, queue->exec_queue_id);

         if (ret == 0)
            return VK_SUCCESS;
         if (ret == -ECANCELED)
            return VK_ERROR_DEVICE_LOST;
         return vk_errorf(device, VK_ERROR_UNKNOWN, "anv_xe_wait_exec_queue_idle failed: %m");
      }
      FALLTHROUGH;
   case INTEL_KMD_TYPE_I915:
      return vk_common_QueueWaitIdle(_queue);
   default:
      UNREACHABLE("Missing");
   }

   return VK_SUCCESS;
}

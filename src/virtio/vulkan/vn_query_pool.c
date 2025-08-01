/*
 * Copyright 2019 Google LLC
 * SPDX-License-Identifier: MIT
 *
 * based in part on anv and radv which are:
 * Copyright © 2015 Intel Corporation
 * Copyright © 2016 Red Hat.
 * Copyright © 2016 Bas Nieuwenhuizen
 */

#include "vn_query_pool.h"

#include "venus-protocol/vn_protocol_driver_query_pool.h"

#include "vn_device.h"
#include "vn_feedback.h"
#include "vn_physical_device.h"

/* query pool commands */

VkResult
vn_CreateQueryPool(VkDevice device,
                   const VkQueryPoolCreateInfo *pCreateInfo,
                   const VkAllocationCallbacks *pAllocator,
                   VkQueryPool *pQueryPool)
{
   struct vn_device *dev = vn_device_from_handle(device);
   const VkAllocationCallbacks *alloc =
      pAllocator ? pAllocator : &dev->base.vk.alloc;

   struct vn_query_pool *pool =
      vk_zalloc(alloc, sizeof(*pool), VN_DEFAULT_ALIGN,
                VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (!pool)
      return vn_error(dev->instance, VK_ERROR_OUT_OF_HOST_MEMORY);

   vn_object_base_init(&pool->base, VK_OBJECT_TYPE_QUERY_POOL, &dev->base);

   pool->allocator = *alloc;
   pool->query_count = pCreateInfo->queryCount;

   simple_mtx_init(&pool->mutex, mtx_plain);

   switch (pCreateInfo->queryType) {
   case VK_QUERY_TYPE_OCCLUSION:
      /*
       * Occlusion queries write one integer value - the number of samples
       * passed.
       */
      pool->result_array_size = 1;
      break;
   case VK_QUERY_TYPE_PIPELINE_STATISTICS:
      /*
       * Pipeline statistics queries write one integer value for each bit that
       * is enabled in the pipelineStatistics when the pool is created, and
       * the statistics values are written in bit order starting from the
       * least significant bit.
       */
      pool->result_array_size =
         util_bitcount(pCreateInfo->pipelineStatistics);
      break;
   case VK_QUERY_TYPE_TIMESTAMP:
      /*  Timestamp queries write one integer value. */
      pool->result_array_size = 1;
      break;
   case VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT:
      /*
       * Transform feedback queries write two integers; the first integer is
       * the number of primitives successfully written to the corresponding
       * transform feedback buffer and the second is the number of primitives
       * output to the vertex stream, regardless of whether they were
       * successfully captured or not.
       */
      pool->result_array_size = 2;
      break;
   case VK_QUERY_TYPE_PRIMITIVES_GENERATED_EXT:
      /*
       * Primitives generated queries write one integer value; the number of
       * primitives output to the vertex stream, regardless of whether
       * transform feedback is active or not, or whether they were
       * successfully captured by transform feedback or not. This is identical
       * to the second integer of the transform feedback queries if transform
       * feedback is active.
       */
      pool->result_array_size = 1;
      break;
   case VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR:
   case VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR:
      /*
       * The value written out is the number of bytes required by a compacted
       * or a serialized acceleration structure correspondingly. So the query
       * writes one integer value.
       */
      pool->result_array_size = 1;
      break;
   case VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR:
   case VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR:
      /*
       * The VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR is to query the
       * acceleration structure size on the device timeline.
       *
       * SERIALIZATION_BOTTOM_LEVEL_POINTERS is to query the number of bottom
       * level acceleration structure pointers for serialization.
       *
       * So either of these queries only writes one integer value.
       *
       */
      pool->result_array_size = 1;
      break;
   default:
      UNREACHABLE("bad query type");
      break;
   }

   /* Venus has to handle overflow behavior with query feedback to keep
    * consistency between vkCmdCopyQueryPoolResults and vkGetQueryPoolResults.
    * The default query feedback behavior is to wrap on overflow. However, per
    * spec:
    *
    * If an unsigned integer query’s value overflows the result type, the
    * value may either wrap or saturate.
    *
    * We detect the renderer side implementation to align with the
    * implementation specific behavior when maintenance7 is not enabled.
    */
   const struct vk_features *app_feats = &dev->base.vk.enabled_features;
   if (!app_feats->maintenance7) {
      switch (dev->physical_device->renderer_driver_id) {
      case VK_DRIVER_ID_ARM_PROPRIETARY:
      case VK_DRIVER_ID_MESA_LLVMPIPE:
      case VK_DRIVER_ID_MESA_TURNIP:
         pool->saturate_on_overflow = true;
         break;
      default:
         break;
      };
   }

   VkQueryPool pool_handle = vn_query_pool_to_handle(pool);
   vn_async_vkCreateQueryPool(dev->primary_ring, device, pCreateInfo, NULL,
                              &pool_handle);

   *pQueryPool = pool_handle;

   return VK_SUCCESS;
}

void
vn_DestroyQueryPool(VkDevice device,
                    VkQueryPool queryPool,
                    const VkAllocationCallbacks *pAllocator)
{
   struct vn_device *dev = vn_device_from_handle(device);
   struct vn_query_pool *pool = vn_query_pool_from_handle(queryPool);
   const VkAllocationCallbacks *alloc;

   if (!pool)
      return;

   alloc = pAllocator ? pAllocator : &pool->allocator;

   if (pool->fb_buf)
      vn_feedback_buffer_destroy(dev, pool->fb_buf, alloc);

   simple_mtx_destroy(&pool->mutex);

   vn_async_vkDestroyQueryPool(dev->primary_ring, device, queryPool, NULL);

   vn_object_base_fini(&pool->base);
   vk_free(alloc, pool);
}

void
vn_ResetQueryPool(VkDevice device,
                  VkQueryPool queryPool,
                  uint32_t firstQuery,
                  uint32_t queryCount)
{
   struct vn_device *dev = vn_device_from_handle(device);
   struct vn_query_pool *pool = vn_query_pool_from_handle(queryPool);

   vn_async_vkResetQueryPool(dev->primary_ring, device, queryPool, firstQuery,
                             queryCount);
   if (pool->fb_buf) {
      /* Feedback results are always 64 bit and include availability bit
       * (also 64 bit)
       */
      const uint32_t slot_size = (pool->result_array_size * 8) + 8;
      const uint32_t offset = slot_size * firstQuery;
      memset(pool->fb_buf->data + offset, 0, slot_size * queryCount);
   }
}

static VkResult
vn_get_query_pool_feedback(struct vn_query_pool *pool,
                           uint32_t firstQuery,
                           uint32_t queryCount,
                           void *pData,
                           VkDeviceSize stride,
                           VkQueryResultFlags flags)
{
   VkResult result = VK_SUCCESS;
   /* Feedback results are always 64 bit and include availability bit
    * (also 64 bit)
    */
   const uint32_t slot_array_size = pool->result_array_size + 1;
   uint64_t *src = pool->fb_buf->data;
   src += slot_array_size * firstQuery;

   uint32_t dst_index = 0;
   uint32_t src_index = 0;
   if (flags & VK_QUERY_RESULT_64_BIT) {
      uint64_t *dst = pData;
      uint32_t index_stride = stride / sizeof(uint64_t);
      for (uint32_t i = 0; i < queryCount; i++) {
         /* Copy the result if its available */
         const uint64_t avail = src[src_index + pool->result_array_size];
         if (avail) {
            memcpy(&dst[dst_index], &src[src_index],
                   pool->result_array_size * sizeof(uint64_t));
         } else {
            result = VK_NOT_READY;
            /* valid to return result of 0 if partial bit is set */
            if (flags & VK_QUERY_RESULT_PARTIAL_BIT) {
               memset(&dst[dst_index], 0,
                      pool->result_array_size * sizeof(uint64_t));
            }
         }
         /* Set the availability bit if requested */
         if (flags & VK_QUERY_RESULT_WITH_AVAILABILITY_BIT)
            dst[dst_index + pool->result_array_size] = avail;

         dst_index += index_stride;
         src_index += slot_array_size;
      }
   } else {
      uint32_t *dst = pData;
      uint32_t index_stride = stride / sizeof(uint32_t);
      for (uint32_t i = 0; i < queryCount; i++) {
         /* Copy the result if its available, converting down to uint32_t */
         const uint32_t avail =
            (uint32_t)src[src_index + pool->result_array_size];
         if (avail) {
            for (uint32_t j = 0; j < pool->result_array_size; j++) {
               const uint64_t src_val = src[src_index + j];
               dst[dst_index + j] =
                  src_val > UINT32_MAX && pool->saturate_on_overflow
                     ? UINT32_MAX
                     : (uint32_t)src_val;
            }
         } else {
            result = VK_NOT_READY;
            /* valid to return result of 0 if partial bit is set */
            if (flags & VK_QUERY_RESULT_PARTIAL_BIT) {
               for (uint32_t j = 0; j < pool->result_array_size; j++)
                  dst[dst_index + j] = 0;
            }
         }
         /* Set the availability bit if requested */
         if (flags & VK_QUERY_RESULT_WITH_AVAILABILITY_BIT)
            dst[dst_index + pool->result_array_size] = avail;

         dst_index += index_stride;
         src_index += slot_array_size;
      }
   }
   return result;
}

static void
vn_query_feedback_wait_ready(struct vn_device *dev,
                             struct vn_query_pool *pool,
                             uint32_t first_query,
                             uint32_t query_count)
{
   VN_TRACE_FUNC();

   /* Feedback results are always 64 bit and include availability bit
    * (also 64 bit)
    */
   const uint32_t step = pool->result_array_size + 1;
   const uint64_t *avail = (uint64_t *)pool->fb_buf->data +
                           first_query * step + pool->result_array_size;

   struct vn_relax_state relax_state =
      vn_relax_init(dev->instance, VN_RELAX_REASON_QUERY);
   for (uint32_t i = 0, j = 0; i < query_count; i++, j += step) {
      while (!avail[j]) {
         vn_relax(&relax_state);
      }
   }
   vn_relax_fini(&relax_state);
}

VkResult
vn_GetQueryPoolResults(VkDevice device,
                       VkQueryPool queryPool,
                       uint32_t firstQuery,
                       uint32_t queryCount,
                       size_t dataSize,
                       void *pData,
                       VkDeviceSize stride,
                       VkQueryResultFlags flags)
{
   struct vn_device *dev = vn_device_from_handle(device);
   struct vn_query_pool *pool = vn_query_pool_from_handle(queryPool);
   const VkAllocationCallbacks *alloc = &pool->allocator;
   VkResult result;

   const size_t result_width = flags & VK_QUERY_RESULT_64_BIT ? 8 : 4;
   const size_t result_size = pool->result_array_size * result_width;
   const bool result_always_written =
      flags & (VK_QUERY_RESULT_WAIT_BIT | VK_QUERY_RESULT_PARTIAL_BIT);

   /* Get results from feedback buffers
    * Not possible for VK_QUERY_RESULT_PARTIAL_BIT
    */
   if (pool->fb_buf) {
      /* If wait bit is set, wait poll until query is ready */
      if (flags & VK_QUERY_RESULT_WAIT_BIT)
         vn_query_feedback_wait_ready(dev, pool, firstQuery, queryCount);

      result = vn_get_query_pool_feedback(pool, firstQuery, queryCount, pData,
                                          stride, flags);
      return vn_result(dev->instance, result);
   }

   VkQueryResultFlags packed_flags = flags;
   size_t packed_stride = result_size;
   if (!result_always_written)
      packed_flags |= VK_QUERY_RESULT_WITH_AVAILABILITY_BIT;
   if (packed_flags & VK_QUERY_RESULT_WITH_AVAILABILITY_BIT)
      packed_stride += result_width;

   const size_t packed_size = packed_stride * queryCount;
   void *packed_data;
   if (result_always_written && packed_stride == stride) {
      packed_data = pData;
   } else {
      packed_data = vk_alloc(alloc, packed_size, VN_DEFAULT_ALIGN,
                             VK_SYSTEM_ALLOCATION_SCOPE_COMMAND);
      if (!packed_data)
         return vn_error(dev->instance, VK_ERROR_OUT_OF_HOST_MEMORY);
   }
   result = vn_call_vkGetQueryPoolResults(
      dev->primary_ring, device, queryPool, firstQuery, queryCount,
      packed_size, packed_data, packed_stride, packed_flags);

   if (packed_data == pData)
      return vn_result(dev->instance, result);

   const size_t copy_size =
      result_size +
      (flags & VK_QUERY_RESULT_WITH_AVAILABILITY_BIT ? result_width : 0);
   const void *src = packed_data;
   void *dst = pData;
   if (result == VK_SUCCESS) {
      for (uint32_t i = 0; i < queryCount; i++) {
         memcpy(dst, src, copy_size);
         src += packed_stride;
         dst += stride;
      }
   } else if (result == VK_NOT_READY) {
      assert(!result_always_written &&
             (packed_flags & VK_QUERY_RESULT_WITH_AVAILABILITY_BIT));
      if (flags & VK_QUERY_RESULT_64_BIT) {
         for (uint32_t i = 0; i < queryCount; i++) {
            const bool avail = *(const uint64_t *)(src + result_size);
            if (avail)
               memcpy(dst, src, copy_size);
            else if (flags & VK_QUERY_RESULT_WITH_AVAILABILITY_BIT)
               *(uint64_t *)(dst + result_size) = 0;

            src += packed_stride;
            dst += stride;
         }
      } else {
         for (uint32_t i = 0; i < queryCount; i++) {
            const bool avail = *(const uint32_t *)(src + result_size);
            if (avail)
               memcpy(dst, src, copy_size);
            else if (flags & VK_QUERY_RESULT_WITH_AVAILABILITY_BIT)
               *(uint32_t *)(dst + result_size) = 0;

            src += packed_stride;
            dst += stride;
         }
      }
   }

   vk_free(alloc, packed_data);
   return vn_result(dev->instance, result);
}

VkResult
vn_query_feedback_buffer_init_once(struct vn_device *dev,
                                   struct vn_query_pool *pool)
{
   VkResult result = VK_SUCCESS;

   simple_mtx_lock(&pool->mutex);
   if (pool->fb_buf)
      goto out_unlock;

   const uint32_t fb_buf_size =
      (pool->result_array_size + 1) * sizeof(uint64_t) * pool->query_count;
   struct vn_feedback_buffer *fb_buf;
   result =
      vn_feedback_buffer_create(dev, fb_buf_size, &pool->allocator, &fb_buf);
   if (result == VK_SUCCESS)
      pool->fb_buf = fb_buf;

out_unlock:
   simple_mtx_unlock(&pool->mutex);
   return result;
}

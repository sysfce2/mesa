/*
 * Copyright © 2016 Intel Corporation
 *
 * SPDX-License-Identifier: MIT
 */

#include "nir/radv_meta_nir.h"
#include "radv_formats.h"
#include "radv_meta.h"
#include "radv_sdma.h"
#include "vk_format.h"

static VkFormat
vk_format_for_size(int bs)
{
   switch (bs) {
   case 1:
      return VK_FORMAT_R8_UINT;
   case 2:
      return VK_FORMAT_R8G8_UINT;
   case 4:
      return VK_FORMAT_R8G8B8A8_UINT;
   case 8:
      return VK_FORMAT_R16G16B16A16_UINT;
   case 12:
      return VK_FORMAT_R32G32B32_UINT;
   case 16:
      return VK_FORMAT_R32G32B32A32_UINT;
   default:
      UNREACHABLE("Invalid format block size");
   }
}

static struct radv_meta_blit2d_surf
blit_surf_for_image_level_layer(struct radv_image *image, VkImageLayout layout, const VkImageSubresourceLayers *subres)
{
   VkFormat format = radv_get_aspect_format(image, subres->aspectMask);

   if (!radv_dcc_enabled(image, subres->mipLevel) && !(radv_tc_compat_htile_enabled(image, subres->mipLevel)))
      format = vk_format_for_size(vk_format_get_blocksize(format));

   format = vk_format_no_srgb(format);

   return (struct radv_meta_blit2d_surf){
      .format = format,
      .bs = vk_format_get_blocksize(format),
      .level = subres->mipLevel,
      .layer = subres->baseArrayLayer,
      .image = image,
      .aspect_mask = subres->aspectMask,
      .current_layout = layout,
   };
}

static bool
alloc_transfer_temp_bo(struct radv_cmd_buffer *cmd_buffer)
{
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);

   if (!cmd_buffer->transfer.copy_temp) {
      const VkResult r =
         radv_bo_create(device, &cmd_buffer->vk.base, RADV_SDMA_TRANSFER_TEMP_BYTES, 4096, RADEON_DOMAIN_VRAM,
                        RADEON_FLAG_NO_CPU_ACCESS | RADEON_FLAG_NO_INTERPROCESS_SHARING, RADV_BO_PRIORITY_SCRATCH, 0,
                        true, &cmd_buffer->transfer.copy_temp);

      if (r != VK_SUCCESS) {
         vk_command_buffer_set_error(&cmd_buffer->vk, r);
         return false;
      }
   }

   radv_cs_add_buffer(device->ws, cmd_buffer->cs, cmd_buffer->transfer.copy_temp);
   return true;
}

static void
transfer_copy_memory_image(struct radv_cmd_buffer *cmd_buffer, uint64_t buffer_va, struct radv_image *image,
                           const VkBufferImageCopy2 *region, bool to_image)
{
   const struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   struct radeon_cmdbuf *cs = cmd_buffer->cs;

   struct radv_sdma_surf buf = radv_sdma_get_buf_surf(buffer_va, image, region);
   const struct radv_sdma_surf img = radv_sdma_get_surf(device, image, region->imageSubresource, region->imageOffset);
   const VkExtent3D extent = radv_sdma_get_copy_extent(image, region->imageSubresource, region->imageExtent);

   if (radv_sdma_use_unaligned_buffer_image_copy(device, &buf, &img, extent)) {
      if (!alloc_transfer_temp_bo(cmd_buffer))
         return;

      radv_sdma_copy_buffer_image_unaligned(device, cs, &buf, &img, extent, cmd_buffer->transfer.copy_temp, to_image);
      return;
   }

   radv_sdma_copy_buffer_image(device, cs, &buf, &img, extent, to_image);
}

static void
copy_memory_to_image(struct radv_cmd_buffer *cmd_buffer, uint64_t buffer_addr, uint64_t buffer_size,
                     enum radv_copy_flags src_copy_flags, struct radv_image *image, VkImageLayout layout,
                     const VkBufferImageCopy2 *region)
{
   if (cmd_buffer->qf == RADV_QUEUE_TRANSFER) {
      transfer_copy_memory_image(cmd_buffer, buffer_addr, image, region, true);
      return;
   }

   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   struct radv_meta_saved_state saved_state;
   bool cs;

   /* The Vulkan 1.0 spec says "dstImage must have a sample count equal to
    * VK_SAMPLE_COUNT_1_BIT."
    */
   assert(image->vk.samples == 1);

   cs = cmd_buffer->qf == RADV_QUEUE_COMPUTE || !radv_image_is_renderable(device, image);

   radv_meta_save(&saved_state, cmd_buffer,
                  (cs ? RADV_META_SAVE_COMPUTE_PIPELINE : RADV_META_SAVE_GRAPHICS_PIPELINE) | RADV_META_SAVE_CONSTANTS |
                     RADV_META_SAVE_DESCRIPTORS);

   /**
    * From the Vulkan 1.0.6 spec: 18.3 Copying Data Between Images
    *    extent is the size in texels of the source image to copy in width,
    *    height and depth. 1D images use only x and width. 2D images use x, y,
    *    width and height. 3D images use x, y, z, width, height and depth.
    *
    *
    * Also, convert the offsets and extent from units of texels to units of
    * blocks - which is the highest resolution accessible in this command.
    */
   const VkOffset3D img_offset_el = vk_image_offset_to_elements(&image->vk, region->imageOffset);

   /* Start creating blit rect */
   const VkExtent3D img_extent_el = vk_image_extent_to_elements(&image->vk, region->imageExtent);
   struct radv_meta_blit2d_rect rect = {
      .width = img_extent_el.width,
      .height = img_extent_el.height,
   };

   /* Create blit surfaces */
   struct radv_meta_blit2d_surf img_bsurf = blit_surf_for_image_level_layer(image, layout, &region->imageSubresource);

   if (!radv_is_buffer_format_supported(img_bsurf.format, NULL)) {
      uint32_t queue_mask = radv_image_queue_family_mask(image, cmd_buffer->qf, cmd_buffer->qf);
      bool compressed =
         radv_layout_dcc_compressed(device, image, region->imageSubresource.mipLevel, layout, queue_mask);
      if (compressed) {
         radv_describe_barrier_start(cmd_buffer, RGP_BARRIER_UNKNOWN_REASON);

         radv_decompress_dcc(cmd_buffer, image,
                             &(VkImageSubresourceRange){
                                .aspectMask = region->imageSubresource.aspectMask,
                                .baseMipLevel = region->imageSubresource.mipLevel,
                                .levelCount = 1,
                                .baseArrayLayer = region->imageSubresource.baseArrayLayer,
                                .layerCount = vk_image_subresource_layer_count(&image->vk, &region->imageSubresource),
                             });
         img_bsurf.disable_compression = true;

         radv_describe_barrier_end(cmd_buffer);
      }
      img_bsurf.format = vk_format_for_size(vk_format_get_blocksize(img_bsurf.format));
   }

   const struct vk_image_buffer_layout buf_layout = vk_image_buffer_copy_layout(&image->vk, region);
   struct radv_meta_blit2d_buffer buf_bsurf = {
      .addr = buffer_addr,
      .size = buffer_size,
      .format = img_bsurf.format,
      .offset = region->bufferOffset,
      .pitch = buf_layout.row_stride_B / buf_layout.element_size_B,
      .copy_flags = src_copy_flags,
   };

   if (image->vk.image_type == VK_IMAGE_TYPE_3D)
      img_bsurf.layer = img_offset_el.z;
   /* Loop through each 3D or array slice */
   unsigned num_slices_3d = img_extent_el.depth;
   unsigned num_slices_array = vk_image_subresource_layer_count(&image->vk, &region->imageSubresource);
   unsigned slice_3d = 0;
   unsigned slice_array = 0;
   while (slice_3d < num_slices_3d && slice_array < num_slices_array) {

      rect.dst_x = img_offset_el.x;
      rect.dst_y = img_offset_el.y;

      /* Perform Blit */
      if (cs) {
         radv_meta_buffer_to_image_cs(cmd_buffer, &buf_bsurf, &img_bsurf, &rect);
      } else {
         radv_meta_blit2d(cmd_buffer, NULL, &buf_bsurf, &img_bsurf, &rect);
      }

      /* Once we've done the blit, all of the actual information about
       * the image is embedded in the command buffer so we can just
       * increment the offset directly in the image effectively
       * re-binding it to different backing memory.
       */
      buf_bsurf.offset += buf_layout.image_stride_B;
      img_bsurf.layer++;
      if (image->vk.image_type == VK_IMAGE_TYPE_3D)
         slice_3d++;
      else
         slice_array++;
   }

   radv_meta_restore(&saved_state, cmd_buffer);
}

VKAPI_ATTR void VKAPI_CALL
radv_CmdCopyBufferToImage2(VkCommandBuffer commandBuffer, const VkCopyBufferToImageInfo2 *pCopyBufferToImageInfo)
{
   VK_FROM_HANDLE(radv_cmd_buffer, cmd_buffer, commandBuffer);
   VK_FROM_HANDLE(radv_buffer, src_buffer, pCopyBufferToImageInfo->srcBuffer);
   VK_FROM_HANDLE(radv_image, dst_image, pCopyBufferToImageInfo->dstImage);
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   const struct radv_physical_device *pdev = radv_device_physical(device);

   const enum radv_copy_flags src_copy_flags = radv_get_copy_flags_from_bo(src_buffer->bo);

   radv_suspend_conditional_rendering(cmd_buffer);

   radv_cs_add_buffer(device->ws, cmd_buffer->cs, src_buffer->bo);

   for (unsigned r = 0; r < pCopyBufferToImageInfo->regionCount; r++) {
      const VkBufferImageCopy2 *region = &pCopyBufferToImageInfo->pRegions[r];
      const VkImageAspectFlags aspect_mask = region->imageSubresource.aspectMask;
      const unsigned bind_idx = dst_image->disjoint ? radv_plane_from_aspect(aspect_mask) : 0;

      radv_cs_add_buffer(device->ws, cmd_buffer->cs, dst_image->bindings[bind_idx].bo);

      copy_memory_to_image(cmd_buffer, src_buffer->vk.device_address, src_buffer->vk.size, src_copy_flags, dst_image,
                           pCopyBufferToImageInfo->dstImageLayout, region);
   }

   if (radv_is_format_emulated(pdev, dst_image->vk.format) && cmd_buffer->qf != RADV_QUEUE_TRANSFER) {
      cmd_buffer->state.flush_bits |= RADV_CMD_FLAG_CS_PARTIAL_FLUSH | RADV_CMD_FLAG_PS_PARTIAL_FLUSH |
                                      radv_src_access_flush(cmd_buffer, VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
                                                            VK_ACCESS_TRANSFER_WRITE_BIT, 0, dst_image, NULL) |
                                      radv_dst_access_flush(cmd_buffer, VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
                                                            VK_ACCESS_TRANSFER_READ_BIT, 0, dst_image, NULL);

      const enum util_format_layout format_layout = radv_format_description(dst_image->vk.format)->layout;
      for (unsigned r = 0; r < pCopyBufferToImageInfo->regionCount; r++) {
         if (format_layout == UTIL_FORMAT_LAYOUT_ASTC) {
            radv_meta_decode_astc(cmd_buffer, dst_image, pCopyBufferToImageInfo->dstImageLayout,
                                  &pCopyBufferToImageInfo->pRegions[r].imageSubresource,
                                  pCopyBufferToImageInfo->pRegions[r].imageOffset,
                                  pCopyBufferToImageInfo->pRegions[r].imageExtent);
         } else {
            radv_meta_decode_etc(cmd_buffer, dst_image, pCopyBufferToImageInfo->dstImageLayout,
                                 &pCopyBufferToImageInfo->pRegions[r].imageSubresource,
                                 pCopyBufferToImageInfo->pRegions[r].imageOffset,
                                 pCopyBufferToImageInfo->pRegions[r].imageExtent);
         }
      }
   }

   radv_resume_conditional_rendering(cmd_buffer);
}

static void
copy_image_to_memory(struct radv_cmd_buffer *cmd_buffer, uint64_t buffer_addr, uint64_t buffer_size,
                     enum radv_copy_flags dst_copy_flags, struct radv_image *image, VkImageLayout layout,
                     const VkBufferImageCopy2 *region)
{
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   if (cmd_buffer->qf == RADV_QUEUE_TRANSFER) {
      transfer_copy_memory_image(cmd_buffer, buffer_addr, image, region, false);
      return;
   }

   struct radv_meta_saved_state saved_state;

   radv_meta_save(&saved_state, cmd_buffer,
                  RADV_META_SAVE_COMPUTE_PIPELINE | RADV_META_SAVE_CONSTANTS | RADV_META_SAVE_DESCRIPTORS);

   /**
    * From the Vulkan 1.0.6 spec: 18.3 Copying Data Between Images
    *    extent is the size in texels of the source image to copy in width,
    *    height and depth. 1D images use only x and width. 2D images use x, y,
    *    width and height. 3D images use x, y, z, width, height and depth.
    *
    *
    * Also, convert the offsets and extent from units of texels to units of
    * blocks - which is the highest resolution accessible in this command.
    */
   const VkOffset3D img_offset_el = vk_image_offset_to_elements(&image->vk, region->imageOffset);
   const VkExtent3D bufferExtent = {
      .width = region->bufferRowLength ? region->bufferRowLength : region->imageExtent.width,
      .height = region->bufferImageHeight ? region->bufferImageHeight : region->imageExtent.height,
   };
   const VkExtent3D buf_extent_el = vk_image_extent_to_elements(&image->vk, bufferExtent);

   /* Start creating blit rect */
   const VkExtent3D img_extent_el = vk_image_extent_to_elements(&image->vk, region->imageExtent);
   struct radv_meta_blit2d_rect rect = {
      .width = img_extent_el.width,
      .height = img_extent_el.height,
   };

   /* Create blit surfaces */
   struct radv_meta_blit2d_surf img_info = blit_surf_for_image_level_layer(image, layout, &region->imageSubresource);

   if (!radv_is_buffer_format_supported(img_info.format, NULL)) {
      uint32_t queue_mask = radv_image_queue_family_mask(image, cmd_buffer->qf, cmd_buffer->qf);
      bool compressed =
         radv_layout_dcc_compressed(device, image, region->imageSubresource.mipLevel, layout, queue_mask);
      if (compressed) {
         radv_describe_barrier_start(cmd_buffer, RGP_BARRIER_UNKNOWN_REASON);

         radv_decompress_dcc(cmd_buffer, image,
                             &(VkImageSubresourceRange){
                                .aspectMask = region->imageSubresource.aspectMask,
                                .baseMipLevel = region->imageSubresource.mipLevel,
                                .levelCount = 1,
                                .baseArrayLayer = region->imageSubresource.baseArrayLayer,
                                .layerCount = vk_image_subresource_layer_count(&image->vk, &region->imageSubresource),
                             });
         img_info.disable_compression = true;

         radv_describe_barrier_end(cmd_buffer);
      }
      img_info.format = vk_format_for_size(vk_format_get_blocksize(img_info.format));
   }

   struct radv_meta_blit2d_buffer buf_info = {
      .addr = buffer_addr,
      .size = buffer_size,
      .format = img_info.format,
      .offset = region->bufferOffset,
      .pitch = buf_extent_el.width,
      .copy_flags = dst_copy_flags,
   };

   if (image->vk.image_type == VK_IMAGE_TYPE_3D)
      img_info.layer = img_offset_el.z;
   /* Loop through each 3D or array slice */
   unsigned num_slices_3d = img_extent_el.depth;
   unsigned num_slices_array = vk_image_subresource_layer_count(&image->vk, &region->imageSubresource);
   unsigned slice_3d = 0;
   unsigned slice_array = 0;
   while (slice_3d < num_slices_3d && slice_array < num_slices_array) {

      rect.src_x = img_offset_el.x;
      rect.src_y = img_offset_el.y;

      /* Perform Blit */
      radv_meta_image_to_buffer(cmd_buffer, &img_info, &buf_info, &rect);

      buf_info.offset += buf_extent_el.width * buf_extent_el.height * img_info.bs;
      img_info.layer++;
      if (image->vk.image_type == VK_IMAGE_TYPE_3D)
         slice_3d++;
      else
         slice_array++;
   }

   radv_meta_restore(&saved_state, cmd_buffer);
}

VKAPI_ATTR void VKAPI_CALL
radv_CmdCopyImageToBuffer2(VkCommandBuffer commandBuffer, const VkCopyImageToBufferInfo2 *pCopyImageToBufferInfo)
{
   VK_FROM_HANDLE(radv_cmd_buffer, cmd_buffer, commandBuffer);
   VK_FROM_HANDLE(radv_image, src_image, pCopyImageToBufferInfo->srcImage);
   VK_FROM_HANDLE(radv_buffer, dst_buffer, pCopyImageToBufferInfo->dstBuffer);
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);

   const enum radv_copy_flags dst_copy_flags = radv_get_copy_flags_from_bo(dst_buffer->bo);

   radv_suspend_conditional_rendering(cmd_buffer);

   radv_cs_add_buffer(device->ws, cmd_buffer->cs, dst_buffer->bo);

   for (unsigned r = 0; r < pCopyImageToBufferInfo->regionCount; r++) {
      const VkBufferImageCopy2 *region = &pCopyImageToBufferInfo->pRegions[r];
      const VkImageAspectFlags aspect_mask = region->imageSubresource.aspectMask;
      const unsigned bind_idx = src_image->disjoint ? radv_plane_from_aspect(aspect_mask) : 0;

      radv_cs_add_buffer(device->ws, cmd_buffer->cs, src_image->bindings[bind_idx].bo);

      copy_image_to_memory(cmd_buffer, dst_buffer->vk.device_address, dst_buffer->vk.size, dst_copy_flags, src_image,
                           pCopyImageToBufferInfo->srcImageLayout, region);
   }

   radv_resume_conditional_rendering(cmd_buffer);
}

static void
transfer_copy_image(struct radv_cmd_buffer *cmd_buffer, struct radv_image *src_image, VkImageLayout src_image_layout,
                    struct radv_image *dst_image, VkImageLayout dst_image_layout, const VkImageCopy2 *region)
{
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   struct radeon_cmdbuf *cs = cmd_buffer->cs;
   unsigned int dst_aspect_mask_remaining = region->dstSubresource.aspectMask;

   VkImageSubresourceLayers src_subresource = region->srcSubresource;
   VkImageSubresourceLayers dst_subresource = region->dstSubresource;

   u_foreach_bit (b, region->srcSubresource.aspectMask) {
      src_subresource.aspectMask = BITFIELD_BIT(b);
      dst_subresource.aspectMask = BITFIELD_BIT(u_bit_scan(&dst_aspect_mask_remaining));

      const struct radv_sdma_surf src = radv_sdma_get_surf(device, src_image, src_subresource, region->srcOffset);
      const struct radv_sdma_surf dst = radv_sdma_get_surf(device, dst_image, dst_subresource, region->dstOffset);
      const VkExtent3D extent = radv_sdma_get_copy_extent(src_image, src_subresource, region->extent);

      if (radv_sdma_use_t2t_scanline_copy(device, &src, &dst, extent)) {
         if (!alloc_transfer_temp_bo(cmd_buffer))
            return;

         radv_sdma_copy_image_t2t_scanline(device, cs, &src, &dst, extent, cmd_buffer->transfer.copy_temp);
      } else {
         radv_sdma_copy_image(device, cs, &src, &dst, extent);
      }
   }
}

static VkFormat
radv_get_compat_color_ds_format(VkFormat format)
{
   switch (format) {
   case VK_FORMAT_R8_UNORM:
   case VK_FORMAT_R8_SNORM:
   case VK_FORMAT_R8_UINT:
   case VK_FORMAT_R8_SINT:
      return VK_FORMAT_R8_UINT;
      break;
   case VK_FORMAT_R16_SFLOAT:
   case VK_FORMAT_R16_UNORM:
   case VK_FORMAT_R16_SNORM:
   case VK_FORMAT_R16_UINT:
   case VK_FORMAT_R16_SINT:
      return VK_FORMAT_R16_UNORM;
   case VK_FORMAT_R32_SFLOAT:
   case VK_FORMAT_R32_SINT:
   case VK_FORMAT_R32_UINT:
      return VK_FORMAT_R32_SFLOAT;
   default:
      UNREACHABLE("invalid color format for color to depth/stencil image copy.");
   }
}

static void
copy_image(struct radv_cmd_buffer *cmd_buffer, struct radv_image *src_image, VkImageLayout src_image_layout,
           struct radv_image *dst_image, VkImageLayout dst_image_layout, const VkImageCopy2 *region)
{
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   const struct radv_physical_device *pdev = radv_device_physical(device);

   if (cmd_buffer->qf == RADV_QUEUE_TRANSFER) {
      transfer_copy_image(cmd_buffer, src_image, src_image_layout, dst_image, dst_image_layout, region);
      return;
   }

   struct radv_meta_saved_state saved_state;
   bool cs;

   /* From the Vulkan 1.0 spec:
    *
    *    vkCmdCopyImage can be used to copy image data between multisample images, but both images must have the same
    *    number of samples.
    */
   assert(src_image->vk.samples == dst_image->vk.samples);
   /* From the Vulkan 1.3 spec:
    *
    *    Multi-planar images can only be copied on a per-plane basis, and the subresources used in each region when
    *    copying to or from such images must specify only one plane, though different regions can specify different
    *    planes.
    */
   assert(src_image->plane_count == 1 || util_is_power_of_two_nonzero(region->srcSubresource.aspectMask));
   assert(dst_image->plane_count == 1 || util_is_power_of_two_nonzero(region->dstSubresource.aspectMask));

   cs = cmd_buffer->qf == RADV_QUEUE_COMPUTE || !radv_image_is_renderable(device, dst_image);

   radv_meta_save(&saved_state, cmd_buffer,
                  (cs ? RADV_META_SAVE_COMPUTE_PIPELINE : RADV_META_SAVE_GRAPHICS_PIPELINE) | RADV_META_SAVE_CONSTANTS |
                     RADV_META_SAVE_DESCRIPTORS);

   if (cs) {
      /* For partial copies, HTILE should be decompressed before copying because the metadata is
       * re-initialized to the uncompressed state after.
       */
      uint32_t queue_mask = radv_image_queue_family_mask(dst_image, cmd_buffer->qf, cmd_buffer->qf);

      if (radv_layout_is_htile_compressed(device, dst_image, region->dstSubresource.mipLevel, dst_image_layout,
                                          queue_mask) &&
          (region->dstOffset.x || region->dstOffset.y || region->dstOffset.z ||
           region->extent.width != dst_image->vk.extent.width || region->extent.height != dst_image->vk.extent.height ||
           region->extent.depth != dst_image->vk.extent.depth)) {
         radv_describe_barrier_start(cmd_buffer, RGP_BARRIER_UNKNOWN_REASON);

         u_foreach_bit (i, region->dstSubresource.aspectMask) {
            unsigned aspect_mask = 1u << i;
            radv_expand_depth_stencil(
               cmd_buffer, dst_image,
               &(VkImageSubresourceRange){
                  .aspectMask = aspect_mask,
                  .baseMipLevel = region->dstSubresource.mipLevel,
                  .levelCount = 1,
                  .baseArrayLayer = region->dstSubresource.baseArrayLayer,
                  .layerCount = vk_image_subresource_layer_count(&dst_image->vk, &region->dstSubresource),
               },
               NULL);
         }

         radv_describe_barrier_end(cmd_buffer);
      }
   }

   /* Create blit surfaces */
   struct radv_meta_blit2d_surf b_src =
      blit_surf_for_image_level_layer(src_image, src_image_layout, &region->srcSubresource);

   struct radv_meta_blit2d_surf b_dst =
      blit_surf_for_image_level_layer(dst_image, dst_image_layout, &region->dstSubresource);

   uint32_t dst_queue_mask = radv_image_queue_family_mask(dst_image, cmd_buffer->qf, cmd_buffer->qf);
   bool dst_compressed =
      radv_layout_dcc_compressed(device, dst_image, region->dstSubresource.mipLevel, dst_image_layout, dst_queue_mask);
   uint32_t src_queue_mask = radv_image_queue_family_mask(src_image, cmd_buffer->qf, cmd_buffer->qf);
   bool src_compressed =
      radv_layout_dcc_compressed(device, src_image, region->srcSubresource.mipLevel, src_image_layout, src_queue_mask);
   bool need_dcc_sign_reinterpret = false;

   if (!src_compressed ||
       (radv_dcc_formats_compatible(pdev->info.gfx_level, b_src.format, b_dst.format, &need_dcc_sign_reinterpret) &&
        !need_dcc_sign_reinterpret)) {
      b_src.format = b_dst.format;
   } else if (!dst_compressed) {
      b_dst.format = b_src.format;
   } else {
      radv_describe_barrier_start(cmd_buffer, RGP_BARRIER_UNKNOWN_REASON);

      radv_decompress_dcc(cmd_buffer, dst_image,
                          &(VkImageSubresourceRange){
                             .aspectMask = region->dstSubresource.aspectMask,
                             .baseMipLevel = region->dstSubresource.mipLevel,
                             .levelCount = 1,
                             .baseArrayLayer = region->dstSubresource.baseArrayLayer,
                             .layerCount = vk_image_subresource_layer_count(&dst_image->vk, &region->dstSubresource),
                          });
      b_dst.format = b_src.format;
      b_dst.disable_compression = true;

      radv_describe_barrier_end(cmd_buffer);
   }

   /* Select a compatible color format for color<->depth/stencil copies. */
   if (vk_format_is_color(src_image->vk.format) && vk_format_is_depth_or_stencil(dst_image->vk.format)) {
      b_src.format = radv_get_compat_color_ds_format(src_image->vk.format);
   } else if (vk_format_is_depth_or_stencil(src_image->vk.format) && vk_format_is_color(dst_image->vk.format)) {
      b_dst.format = radv_get_compat_color_ds_format(dst_image->vk.format);
   }

   /**
    * From the Vulkan 1.0.6 spec: 18.4 Copying Data Between Buffers and Images
    *    imageExtent is the size in texels of the image to copy in width, height
    *    and depth. 1D images use only x and width. 2D images use x, y, width
    *    and height. 3D images use x, y, z, width, height and depth.
    *
    * Also, convert the offsets and extent from units of texels to units of
    * blocks - which is the highest resolution accessible in this command.
    */
   const VkOffset3D dst_offset_el = vk_image_offset_to_elements(&dst_image->vk, region->dstOffset);
   const VkOffset3D src_offset_el = vk_image_offset_to_elements(&src_image->vk, region->srcOffset);

   /*
    * From Vulkan 1.0.68, "Copying Data Between Images":
    *    "When copying between compressed and uncompressed formats
    *     the extent members represent the texel dimensions of the
    *     source image and not the destination."
    * However, we must use the destination image type to avoid
    * clamping depth when copying multiple layers of a 2D image to
    * a 3D image.
    */
   const VkExtent3D img_extent_el = vk_image_extent_to_elements(&src_image->vk, region->extent);

   /* Start creating blit rect */
   struct radv_meta_blit2d_rect rect = {
      .width = img_extent_el.width,
      .height = img_extent_el.height,
   };

   unsigned num_slices = vk_image_subresource_layer_count(&src_image->vk, &region->srcSubresource);

   if (src_image->vk.image_type == VK_IMAGE_TYPE_3D) {
      b_src.layer = src_offset_el.z;
      num_slices = img_extent_el.depth;
   }

   if (dst_image->vk.image_type == VK_IMAGE_TYPE_3D)
      b_dst.layer = dst_offset_el.z;

   for (unsigned slice = 0; slice < num_slices; slice++) {
      /* Finish creating blit rect */
      rect.dst_x = dst_offset_el.x;
      rect.dst_y = dst_offset_el.y;
      rect.src_x = src_offset_el.x;
      rect.src_y = src_offset_el.y;

      /* Perform Blit */
      if (cs) {
         radv_meta_image_to_image_cs(cmd_buffer, &b_src, &b_dst, &rect);
      } else {
         if (radv_can_use_fmask_copy(cmd_buffer, b_src.image, b_dst.image, &rect)) {
            radv_fmask_copy(cmd_buffer, &b_src, &b_dst);
         } else {
            radv_meta_blit2d(cmd_buffer, &b_src, NULL, &b_dst, &rect);
         }
      }

      b_src.layer++;
      b_dst.layer++;
   }

   if (cs) {
      /* Fixup HTILE after a copy on compute. */
      uint32_t queue_mask = radv_image_queue_family_mask(dst_image, cmd_buffer->qf, cmd_buffer->qf);

      if (radv_layout_is_htile_compressed(device, dst_image, region->dstSubresource.mipLevel, dst_image_layout,
                                          queue_mask)) {
         cmd_buffer->state.flush_bits |= RADV_CMD_FLAG_CS_PARTIAL_FLUSH | RADV_CMD_FLAG_INV_VCACHE;

         VkImageSubresourceRange range = {
            .aspectMask = region->dstSubresource.aspectMask,
            .baseMipLevel = region->dstSubresource.mipLevel,
            .levelCount = 1,
            .baseArrayLayer = region->dstSubresource.baseArrayLayer,
            .layerCount = vk_image_subresource_layer_count(&dst_image->vk, &region->dstSubresource),
         };

         uint32_t htile_value = radv_get_htile_initial_value(device, dst_image);

         cmd_buffer->state.flush_bits |= radv_clear_htile(cmd_buffer, dst_image, &range, htile_value, false);
      }
   }

   radv_meta_restore(&saved_state, cmd_buffer);
}

VKAPI_ATTR void VKAPI_CALL
radv_CmdCopyImage2(VkCommandBuffer commandBuffer, const VkCopyImageInfo2 *pCopyImageInfo)
{
   VK_FROM_HANDLE(radv_cmd_buffer, cmd_buffer, commandBuffer);
   VK_FROM_HANDLE(radv_image, src_image, pCopyImageInfo->srcImage);
   VK_FROM_HANDLE(radv_image, dst_image, pCopyImageInfo->dstImage);
   struct radv_device *device = radv_cmd_buffer_device(cmd_buffer);
   const struct radv_physical_device *pdev = radv_device_physical(device);

   radv_suspend_conditional_rendering(cmd_buffer);

   for (unsigned r = 0; r < pCopyImageInfo->regionCount; r++) {
      const VkImageCopy2 *region = &pCopyImageInfo->pRegions[r];
      const VkImageAspectFlags src_aspect_mask = region->srcSubresource.aspectMask;
      const unsigned src_bind_idx = src_image->disjoint ? radv_plane_from_aspect(src_aspect_mask) : 0;
      const VkImageAspectFlags dst_aspect_mask = region->dstSubresource.aspectMask;
      const unsigned dst_bind_idx = dst_image->disjoint ? radv_plane_from_aspect(dst_aspect_mask) : 0;

      radv_cs_add_buffer(device->ws, cmd_buffer->cs, src_image->bindings[src_bind_idx].bo);
      radv_cs_add_buffer(device->ws, cmd_buffer->cs, dst_image->bindings[dst_bind_idx].bo);

      copy_image(cmd_buffer, src_image, pCopyImageInfo->srcImageLayout, dst_image, pCopyImageInfo->dstImageLayout,
                 region);
   }

   if (radv_is_format_emulated(pdev, dst_image->vk.format) && cmd_buffer->qf != RADV_QUEUE_TRANSFER) {
      cmd_buffer->state.flush_bits |= RADV_CMD_FLAG_CS_PARTIAL_FLUSH | RADV_CMD_FLAG_PS_PARTIAL_FLUSH |
                                      radv_src_access_flush(cmd_buffer, VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
                                                            VK_ACCESS_TRANSFER_WRITE_BIT, 0, dst_image, NULL) |
                                      radv_dst_access_flush(cmd_buffer, VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
                                                            VK_ACCESS_TRANSFER_READ_BIT, 0, dst_image, NULL);

      const enum util_format_layout format_layout = radv_format_description(dst_image->vk.format)->layout;
      for (unsigned r = 0; r < pCopyImageInfo->regionCount; r++) {
         VkExtent3D dst_extent = pCopyImageInfo->pRegions[r].extent;
         if (src_image->vk.format != dst_image->vk.format) {
            dst_extent.width = dst_extent.width / vk_format_get_blockwidth(src_image->vk.format) *
                               vk_format_get_blockwidth(dst_image->vk.format);
            dst_extent.height = dst_extent.height / vk_format_get_blockheight(src_image->vk.format) *
                                vk_format_get_blockheight(dst_image->vk.format);
         }
         if (format_layout == UTIL_FORMAT_LAYOUT_ASTC) {
            radv_meta_decode_astc(cmd_buffer, dst_image, pCopyImageInfo->dstImageLayout,
                                  &pCopyImageInfo->pRegions[r].dstSubresource, pCopyImageInfo->pRegions[r].dstOffset,
                                  dst_extent);
         } else {
            radv_meta_decode_etc(cmd_buffer, dst_image, pCopyImageInfo->dstImageLayout,
                                 &pCopyImageInfo->pRegions[r].dstSubresource, pCopyImageInfo->pRegions[r].dstOffset,
                                 dst_extent);
         }
      }
   }

   radv_resume_conditional_rendering(cmd_buffer);
}

/*
 * Copyright 2021 Google LLC
 * SPDX-License-Identifier: MIT
 *
 * based in part on anv and radv which are:
 * Copyright © 2015 Intel Corporation
 * Copyright © 2016 Red Hat
 * Copyright © 2016 Bas Nieuwenhuizen
 */

#include "vn_android.h"

#include <dlfcn.h>
#include <vndk/hardware_buffer.h>

#include "util/os_file.h"
#include "util/u_gralloc/u_gralloc.h"
#include "vk_android.h"

#include "vn_buffer.h"
#include "vn_device.h"
#include "vn_device_memory.h"
#include "vn_image.h"
#include "vn_instance.h"
#include "vn_physical_device.h"
#include "vn_queue.h"

struct vn_android_gralloc_buffer_properties {
   uint32_t drm_fourcc;
   uint32_t num_planes;
   uint64_t modifier;

   /* plane order matches VkImageDrmFormatModifierExplicitCreateInfoEXT */
   uint32_t offset[4];
   uint32_t stride[4];
};

static bool
vn_android_gralloc_get_buffer_properties(
   buffer_handle_t handle,
   struct vn_android_gralloc_buffer_properties *out_props)
{
   struct u_gralloc *gralloc = vk_android_get_ugralloc();
   struct u_gralloc_buffer_basic_info info;

   /*
    * We only support (and care of) CrOS and IMapper v4 gralloc modules
    * at this point. They don't need the pixel stride and HAL format
    * to be provided externally to them. It allows integrating u_gralloc
    * with minimal modifications at this point.
    */
   struct u_gralloc_buffer_handle ugb_handle = {
      .handle = handle,
      .pixel_stride = 0,
      .hal_format = 0,
   };

   if (u_gralloc_get_buffer_basic_info(gralloc, &ugb_handle, &info) != 0) {
      vn_log(NULL, "u_gralloc_get_buffer_basic_info failed");
      return false;
   }

   if (info.modifier == DRM_FORMAT_MOD_INVALID) {
      vn_log(NULL, "Unexpected DRM_FORMAT_MOD_INVALID");
      return false;
   }

   assert(info.num_planes <= 4);

   out_props->drm_fourcc = info.drm_fourcc;
   out_props->num_planes = info.num_planes;
   for (uint32_t i = 0; i < info.num_planes; i++) {
      if (!info.strides[i]) {
         out_props->num_planes = i;
         break;
      }
      out_props->stride[i] = info.strides[i];
      out_props->offset[i] = info.offsets[i];
   }

   /* YVU420 has a chroma order of CrCb. So we must swap the planes for CrCb
    * to align with VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM. This is to serve
    * VkImageDrmFormatModifierExplicitCreateInfoEXT explicit plane layouts.
    */
   if (info.drm_fourcc == DRM_FORMAT_YVU420) {
      out_props->stride[1] = info.strides[2];
      out_props->offset[1] = info.offsets[2];
      out_props->stride[2] = info.strides[1];
      out_props->offset[2] = info.offsets[1];
   }

   out_props->modifier = info.modifier;

   return true;
}

static int
vn_android_gralloc_get_dma_buf_fd(const native_handle_t *handle)
{
   /* There can be multiple fds wrapped inside a native_handle_t, but we
    * expect the 1st one pointing to the dma_buf. For multi-planar format,
    * there should only exist one undelying dma_buf. The other fd(s) could be
    * dups to the same dma_buf or point to the shared memory used to store
    * gralloc buffer metadata.
    */
   assert(handle);

   if (handle->numFds < 1) {
      vn_log(NULL, "handle->numFds is %d, expected >= 1", handle->numFds);
      return -1;
   }

   if (handle->data[0] < 0) {
      vn_log(NULL, "handle->data[0] < 0");
      return -1;
   }

   return handle->data[0];
}

const VkFormat *
vn_android_format_to_view_formats(VkFormat format, uint32_t *out_count)
{
   /* For AHB image prop query and creation, venus overrides the tiling to
    * VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT, which requires to chain
    * VkImageFormatListCreateInfo struct in the corresponding pNext when the
    * VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT is set. Those AHB images are assumed
    * to be mutable no more than sRGB-ness, and the implementations can fail
    * whenever going beyond.
    *
    * This helper provides the view formats that have sRGB variants for the
    * image format that venus supports.
    */
   static const VkFormat view_formats_r8g8b8a8[] = {
      VK_FORMAT_R8G8B8A8_UNORM, VK_FORMAT_R8G8B8A8_SRGB
   };
   static const VkFormat view_formats_r8g8b8[] = { VK_FORMAT_R8G8B8_UNORM,
                                                   VK_FORMAT_R8G8B8_SRGB };

   switch (format) {
   case VK_FORMAT_R8G8B8A8_UNORM:
      *out_count = ARRAY_SIZE(view_formats_r8g8b8a8);
      return view_formats_r8g8b8a8;
      break;
   case VK_FORMAT_R8G8B8_UNORM:
      *out_count = ARRAY_SIZE(view_formats_r8g8b8);
      return view_formats_r8g8b8;
      break;
   default:
      /* let the caller handle the fallback case */
      *out_count = 0;
      return NULL;
   }
}

VkFormat
vn_android_drm_format_to_vk_format(uint32_t format)
{
   switch (format) {
   case DRM_FORMAT_ABGR8888:
   case DRM_FORMAT_XBGR8888:
      return VK_FORMAT_R8G8B8A8_UNORM;
   case DRM_FORMAT_BGR888:
      return VK_FORMAT_R8G8B8_UNORM;
   case DRM_FORMAT_RGB565:
      return VK_FORMAT_R5G6B5_UNORM_PACK16;
   case DRM_FORMAT_ABGR16161616F:
      return VK_FORMAT_R16G16B16A16_SFLOAT;
   case DRM_FORMAT_ABGR2101010:
      return VK_FORMAT_A2B10G10R10_UNORM_PACK32;
   case DRM_FORMAT_R8:
      return VK_FORMAT_R8_UNORM;
   case DRM_FORMAT_YVU420:
      return VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM;
   case DRM_FORMAT_NV12:
      return VK_FORMAT_G8_B8R8_2PLANE_420_UNORM;
   default:
      return VK_FORMAT_UNDEFINED;
   }
}

static bool
vn_android_drm_format_is_yuv(uint32_t format)
{
   assert(vn_android_drm_format_to_vk_format(format) != VK_FORMAT_UNDEFINED);

   switch (format) {
   case DRM_FORMAT_YVU420:
   case DRM_FORMAT_NV12:
      return true;
   default:
      return false;
   }
}

static VkResult
vn_android_get_modifier_properties(struct vn_device *dev,
                                   VkFormat format,
                                   uint64_t modifier,
                                   VkDrmFormatModifierPropertiesEXT *out_props)
{
   VkPhysicalDevice physical_device =
      vn_physical_device_to_handle(dev->physical_device);
   VkDrmFormatModifierPropertiesListEXT mod_prop_list = {
      .sType = VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT,
   };
   VkFormatProperties2 format_prop = {
      .sType = VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2,
      .pNext = &mod_prop_list,
   };

   vn_GetPhysicalDeviceFormatProperties2(physical_device, format,
                                         &format_prop);

   if (!mod_prop_list.drmFormatModifierCount) {
      vn_log(dev->instance, "No compatible modifier for VkFormat(%u)",
             format);
      return VK_ERROR_INVALID_EXTERNAL_HANDLE;
   }

   STACK_ARRAY(VkDrmFormatModifierPropertiesEXT, mod_props,
               mod_prop_list.drmFormatModifierCount);

   mod_prop_list.pDrmFormatModifierProperties = mod_props;
   vn_GetPhysicalDeviceFormatProperties2(physical_device, format,
                                         &format_prop);

   bool modifier_found = false;
   for (uint32_t i = 0; i < mod_prop_list.drmFormatModifierCount; i++) {
      if (mod_props[i].drmFormatModifier == modifier) {
         *out_props = mod_props[i];
         modifier_found = true;
         break;
      }
   }

   STACK_ARRAY_FINISH(mod_props);

   if (!modifier_found) {
      vn_log(dev->instance,
             "No matching modifier(%" PRIu64 ") properties for VkFormat(%u)",
             modifier, format);
      return VK_ERROR_INVALID_EXTERNAL_HANDLE;
   }

   return VK_SUCCESS;
}

struct vn_android_image_builder {
   VkImageCreateInfo create;
   VkSubresourceLayout layouts[4];
   VkImageDrmFormatModifierExplicitCreateInfoEXT modifier;
   VkExternalMemoryImageCreateInfo external;
   VkImageFormatListCreateInfo list;
};

static VkResult
vn_android_get_image_builder(struct vn_device *dev,
                             const VkImageCreateInfo *create_info,
                             const native_handle_t *handle,
                             struct vn_android_image_builder *out_builder)
{
   /* Android image builder is only used by ANB or AHB. For ANB, Android
    * Vulkan loader will never pass the below structs. For AHB, struct
    * vn_image_create_deferred_info will never carry below either.
    */
   assert(!vk_find_struct_const(
      create_info->pNext,
      IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT));
   assert(!vk_find_struct_const(create_info->pNext,
                                EXTERNAL_MEMORY_IMAGE_CREATE_INFO));

   struct vn_android_gralloc_buffer_properties buf_props;
   if (!vn_android_gralloc_get_buffer_properties(handle, &buf_props))
      return VK_ERROR_INVALID_EXTERNAL_HANDLE;

   /* fill VkImageCreateInfo */
   memset(out_builder, 0, sizeof(*out_builder));
   out_builder->create = *create_info;
   out_builder->create.tiling = VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT;

   /* fill VkImageDrmFormatModifierExplicitCreateInfoEXT */
   for (uint32_t i = 0; i < buf_props.num_planes; i++) {
      out_builder->layouts[i].offset = buf_props.offset[i];
      out_builder->layouts[i].rowPitch = buf_props.stride[i];
   }
   out_builder->modifier = (VkImageDrmFormatModifierExplicitCreateInfoEXT){
      .sType =
         VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT,
      .pNext = out_builder->create.pNext,
      .drmFormatModifier = buf_props.modifier,
      .drmFormatModifierPlaneCount = buf_props.num_planes,
      .pPlaneLayouts = out_builder->layouts,
   };
   out_builder->create.pNext = &out_builder->modifier;

   /* fill VkExternalMemoryImageCreateInfo */
   out_builder->external = (VkExternalMemoryImageCreateInfo){
      .sType = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO,
      .pNext = out_builder->create.pNext,
      .handleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT,
   };
   out_builder->create.pNext = &out_builder->external;

   /* fill VkImageFormatListCreateInfo if needed
    *
    * vn_image::deferred_info only stores VkImageFormatListCreateInfo with a
    * non-zero viewFormatCount, and that stored struct will be respected.
    */
   if ((create_info->flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT) &&
       !vk_find_struct_const(create_info->pNext,
                             IMAGE_FORMAT_LIST_CREATE_INFO)) {
      /* 12.3. Images
       *
       * If tiling is VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT and flags
       * contains VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT, then the pNext chain
       * must include a VkImageFormatListCreateInfo structure with non-zero
       * viewFormatCount.
       */
      uint32_t vcount = 0;
      const VkFormat *vformats =
         vn_android_format_to_view_formats(create_info->format, &vcount);
      if (!vformats) {
         /* image builder struct persists through the image creation call */
         vformats = &out_builder->create.format;
         vcount = 1;
      }
      out_builder->list = (VkImageFormatListCreateInfo){
         .sType = VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO,
         .pNext = out_builder->create.pNext,
         .viewFormatCount = vcount,
         .pViewFormats = vformats,
      };
      out_builder->create.pNext = &out_builder->list;
   }

   return VK_SUCCESS;
}

static VkResult
vn_android_image_from_anb_internal(struct vn_device *dev,
                                   const VkImageCreateInfo *create_info,
                                   const VkNativeBufferANDROID *anb_info,
                                   const VkAllocationCallbacks *alloc,
                                   struct vn_image **out_img)
{
   /* If anb_info->handle points to a classic resouce created from
    * virtio_gpu_cmd_resource_create_3d, anb_info->stride is the stride of the
    * guest shadow storage other than the host gpu storage.
    *
    * We also need to pass the correct stride to vn_CreateImage, which will be
    * done via VkImageDrmFormatModifierExplicitCreateInfoEXT and will require
    * VK_EXT_image_drm_format_modifier support in the host driver. The struct
    * needs host storage info which can be queried from cros gralloc.
    */
   struct vn_image *img = NULL;
   VkResult result;

   assert(!(create_info->flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT));
   assert(!vk_find_struct_const(create_info->pNext,
                                IMAGE_FORMAT_LIST_CREATE_INFO));
   assert(!vk_find_struct_const(create_info->pNext,
                                IMAGE_STENCIL_USAGE_CREATE_INFO));

   struct vn_android_image_builder builder;
   result = vn_android_get_image_builder(dev, create_info, anb_info->handle,
                                         &builder);
   if (result != VK_SUCCESS)
      return result;

   /* encoder will strip the Android specific pNext structs */
   if (*out_img) {
      /* driver side img obj has been created for deferred init like ahb */
      img = *out_img;
      result = vn_image_init_deferred(dev, &builder.create, img);
      if (result != VK_SUCCESS) {
         vn_log(dev->instance, "anb: vn_image_init_deferred failed");
         return result;
      }
   } else {
      result = vn_image_create(dev, &builder.create, alloc, &img);
      if (result != VK_SUCCESS) {
         vn_log(dev->instance, "anb: vn_image_create failed");
         return result;
      }
   }

   img->wsi.is_wsi = true;

   int dma_buf_fd = vn_android_gralloc_get_dma_buf_fd(anb_info->handle);
   if (dma_buf_fd < 0) {
      result = VK_ERROR_INVALID_EXTERNAL_HANDLE;
      goto fail;
   }

   uint64_t alloc_size = 0;
   uint32_t mem_type_bits = 0;
   result = vn_get_memory_dma_buf_properties(dev, dma_buf_fd, &alloc_size,
                                             &mem_type_bits);
   if (result != VK_SUCCESS)
      goto fail;

   const VkMemoryRequirements *mem_req =
      &img->requirements[0].memory.memoryRequirements;
   if (alloc_size < mem_req->size) {
      vn_log(dev->instance,
             "anb: alloc_size(%" PRIu64 ") mem_req->size(%" PRIu64 ")",
             alloc_size, mem_req->size);
      result = VK_ERROR_INVALID_EXTERNAL_HANDLE;
      goto fail;
   }

   mem_type_bits &= mem_req->memoryTypeBits;
   if (!mem_type_bits) {
      vn_log(dev->instance, "anb: no compatible mem type");
      result = VK_ERROR_INVALID_EXTERNAL_HANDLE;
      goto fail;
   }

   int dup_fd = os_dupfd_cloexec(dma_buf_fd);
   if (dup_fd < 0) {
      vn_log(dev->instance, "anb: os_dupfd_cloexec failed(%d)", errno);
      result = (errno == EMFILE) ? VK_ERROR_TOO_MANY_OBJECTS
                                 : VK_ERROR_OUT_OF_HOST_MEMORY;
      goto fail;
   }

   const bool prefer_dedicated =
      img->requirements[0].dedicated.prefersDedicatedAllocation == VK_TRUE;
   const VkMemoryDedicatedAllocateInfo dedicated_info = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
      .image = vn_image_to_handle(img),
   };
   const VkImportMemoryFdInfoKHR import_fd_info = {
      .sType = VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR,
      .pNext = prefer_dedicated ? &dedicated_info : NULL,
      .handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT,
      .fd = dup_fd,
   };
   const VkMemoryAllocateInfo memory_info = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
      .pNext = &import_fd_info,
      .allocationSize = mem_req->size,
      .memoryTypeIndex = ffs(mem_type_bits) - 1,
   };
   VkDeviceMemory mem_handle;
   result = vn_AllocateMemory(vn_device_to_handle(dev), &memory_info, alloc,
                              &mem_handle);
   if (result != VK_SUCCESS) {
      vn_log(dev->instance, "anb: mem import failed");
      /* only need to close the dup_fd on import failure */
      close(dup_fd);
      goto fail;
   }

   /* Android WSI image owns the memory */
   img->wsi.memory = vn_device_memory_from_handle(mem_handle);
   img->wsi.memory_owned = true;
   *out_img = img;

   return VK_SUCCESS;

fail:
   /* this handles mem free for owned import */
   vn_DestroyImage(vn_device_to_handle(dev), vn_image_to_handle(img), alloc);
   return result;
}

VkResult
vn_android_image_from_anb(struct vn_device *dev,
                          const VkImageCreateInfo *create_info,
                          const VkNativeBufferANDROID *anb_info,
                          const VkAllocationCallbacks *alloc,
                          struct vn_image **out_img)
{
   struct vn_image *img = NULL;
   VkResult result = vn_android_image_from_anb_internal(
      dev, create_info, anb_info, alloc, &img);
   if (result != VK_SUCCESS)
      return result;

   const VkBindImageMemoryInfo bind_info = {
      .sType = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO,
      .image = vn_image_to_handle(img),
      .memory = vn_device_memory_to_handle(img->wsi.memory),
   };
   result = vn_BindImageMemory2(vn_device_to_handle(dev), 1, &bind_info);
   if (result != VK_SUCCESS) {
      vn_DestroyImage(vn_device_to_handle(dev), vn_image_to_handle(img),
                      alloc);
      return result;
   }

   *out_img = img;
   return VK_SUCCESS;
}

struct vn_device_memory *
vn_android_get_wsi_memory_from_bind_info(
   struct vn_device *dev, const VkBindImageMemoryInfo *bind_info)
{
   const VkNativeBufferANDROID *anb_info =
      vk_find_struct_const(bind_info->pNext, NATIVE_BUFFER_ANDROID);
   assert(anb_info && anb_info->handle);

   struct vn_image *img = vn_image_from_handle(bind_info->image);
   VkResult result = vn_android_image_from_anb_internal(
      dev, &img->deferred_info->create, anb_info, &dev->base.vk.alloc, &img);
   if (result != VK_SUCCESS)
      return NULL;

   assert(img->wsi.memory_owned);
   return img->wsi.memory;
}

static VkResult
vn_android_get_ahb_format_properties(
   struct vn_device *dev,
   const struct AHardwareBuffer *ahb,
   VkAndroidHardwareBufferFormatPropertiesANDROID *out_props)
{
   AHardwareBuffer_Desc desc;
   VkFormat format;
   struct vn_android_gralloc_buffer_properties buf_props;
   VkDrmFormatModifierPropertiesEXT mod_props;

   AHardwareBuffer_describe(ahb, &desc);
   if (!(desc.usage & (AHARDWAREBUFFER_USAGE_GPU_SAMPLED_IMAGE |
                       AHARDWAREBUFFER_USAGE_GPU_FRAMEBUFFER |
                       AHARDWAREBUFFER_USAGE_GPU_DATA_BUFFER))) {
      vn_log(dev->instance,
             "AHB usage(%" PRIu64 ") must include at least one GPU bit",
             desc.usage);
      return VK_ERROR_INVALID_EXTERNAL_HANDLE;
   }

   /* Handle the special AHARDWAREBUFFER_FORMAT_BLOB for VkBuffer case. */
   if (desc.format == AHARDWAREBUFFER_FORMAT_BLOB) {
      out_props->format = VK_FORMAT_UNDEFINED;
      return VK_SUCCESS;
   }

   if (!vn_android_gralloc_get_buffer_properties(
          AHardwareBuffer_getNativeHandle(ahb), &buf_props))
      return VK_ERROR_INVALID_EXTERNAL_HANDLE;

   /* We implement AHB extension support with EXT_image_drm_format_modifier.
    * It requires us to have a compatible VkFormat but not DRM formats. So if
    * the ahb is not intended for backing a VkBuffer, error out early if the
    * format is VK_FORMAT_UNDEFINED.
    */
   format = vn_android_drm_format_to_vk_format(buf_props.drm_fourcc);
   if (format == VK_FORMAT_UNDEFINED) {
      vn_log(dev->instance, "Unknown drm_fourcc(%u) from AHB format(0x%X)",
             buf_props.drm_fourcc, desc.format);
      return VK_ERROR_INVALID_EXTERNAL_HANDLE;
   }

   VkResult result = vn_android_get_modifier_properties(
      dev, format, buf_props.modifier, &mod_props);
   if (result != VK_SUCCESS)
      return result;

   if (mod_props.drmFormatModifierPlaneCount != buf_props.num_planes) {
      vn_log(dev->instance,
             "drmFormatModifierPlaneCount(%u) != buf_props.num_planes(%u) "
             "for DRM format modifier(%" PRIu64 ")",
             mod_props.drmFormatModifierPlaneCount, buf_props.num_planes,
             buf_props.modifier);
      return VK_ERROR_INVALID_EXTERNAL_HANDLE;
   }

   /* The spec requires that formatFeatures must include at least one of
    * VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT or
    * VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT.
    */
   const VkFormatFeatureFlags format_features =
      mod_props.drmFormatModifierTilingFeatures |
      VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT;

   /* 11.2.7. Android Hardware Buffer External Memory
    *
    * Implementations may not always be able to determine the color model,
    * numerical range, or chroma offsets of the image contents, so the values
    * in VkAndroidHardwareBufferFormatPropertiesANDROID are only suggestions.
    * Applications should treat these values as sensible defaults to use in the
    * absence of more reliable information obtained through some other means.
    */
   const bool is_yuv = vn_android_drm_format_is_yuv(buf_props.drm_fourcc);
   const VkSamplerYcbcrModelConversion model =
      is_yuv ? VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
             : VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY;

   /* ANGLE expects VK_FORMAT_UNDEFINED with externalFormat resolved from
    * AHARDWAREBUFFER_FORMAT_IMPLEMENTATION_DEFINED and any supported planar
    * AHB formats. Venus supports below explicit ones:
    * - AHARDWAREBUFFER_FORMAT_Y8Cb8Cr8_420 (DRM_FORMAT_NV12)
    * - AHARDWAREBUFFER_FORMAT_YV12 (DRM_FORMAT_YVU420)
    */
   if (desc.format == AHARDWAREBUFFER_FORMAT_IMPLEMENTATION_DEFINED || is_yuv)
      format = VK_FORMAT_UNDEFINED;

   *out_props = (VkAndroidHardwareBufferFormatPropertiesANDROID) {
      .sType = out_props->sType,
      .pNext = out_props->pNext,
      .format = format,
      .externalFormat = buf_props.drm_fourcc,
      .formatFeatures = format_features,
      .samplerYcbcrConversionComponents = {
         .r = VK_COMPONENT_SWIZZLE_IDENTITY,
         .g = VK_COMPONENT_SWIZZLE_IDENTITY,
         .b = VK_COMPONENT_SWIZZLE_IDENTITY,
         .a = VK_COMPONENT_SWIZZLE_IDENTITY,
      },
      .suggestedYcbcrModel = model,
      /* match EGL_YUV_NARROW_RANGE_EXT used in egl platform_android */
      .suggestedYcbcrRange = VK_SAMPLER_YCBCR_RANGE_ITU_NARROW,
      .suggestedXChromaOffset = VK_CHROMA_LOCATION_MIDPOINT,
      .suggestedYChromaOffset = VK_CHROMA_LOCATION_MIDPOINT,
   };

   return VK_SUCCESS;
}

VkResult
vn_GetAndroidHardwareBufferPropertiesANDROID(
   VkDevice device,
   const struct AHardwareBuffer *buffer,
   VkAndroidHardwareBufferPropertiesANDROID *pProperties)
{
   VN_TRACE_FUNC();
   struct vn_device *dev = vn_device_from_handle(device);
   VkResult result = VK_SUCCESS;
   int dma_buf_fd = -1;
   uint64_t alloc_size = 0;
   uint32_t mem_type_bits = 0;

   VkAndroidHardwareBufferFormatProperties2ANDROID *format_props2 =
      vk_find_struct(pProperties->pNext,
                     ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_2_ANDROID);
   VkAndroidHardwareBufferFormatPropertiesANDROID *format_props =
      vk_find_struct(pProperties->pNext,
                     ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID);
   if (format_props2 || format_props) {
      VkAndroidHardwareBufferFormatPropertiesANDROID local_props = {
         .sType =
            VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID,
      };
      if (!format_props)
         format_props = &local_props;

      result =
         vn_android_get_ahb_format_properties(dev, buffer, format_props);
      if (result != VK_SUCCESS)
         return vn_error(dev->instance, result);

      if (format_props2) {
         format_props2->format = format_props->format;
         format_props2->externalFormat = format_props->externalFormat;
         format_props2->formatFeatures =
            (VkFormatFeatureFlags2)format_props->formatFeatures;
         format_props2->samplerYcbcrConversionComponents =
            format_props->samplerYcbcrConversionComponents;
         format_props2->suggestedYcbcrModel =
            format_props->suggestedYcbcrModel;
         format_props2->suggestedYcbcrRange =
            format_props->suggestedYcbcrRange;
         format_props2->suggestedXChromaOffset =
            format_props->suggestedXChromaOffset;
         format_props2->suggestedYChromaOffset =
            format_props->suggestedYChromaOffset;
      }
   }

   dma_buf_fd = vn_android_gralloc_get_dma_buf_fd(
      AHardwareBuffer_getNativeHandle(buffer));
   if (dma_buf_fd < 0)
      return vn_error(dev->instance, VK_ERROR_INVALID_EXTERNAL_HANDLE);

   result = vn_get_memory_dma_buf_properties(dev, dma_buf_fd, &alloc_size,
                                             &mem_type_bits);
   if (result != VK_SUCCESS)
      return vn_error(dev->instance, result);

   pProperties->allocationSize = alloc_size;
   pProperties->memoryTypeBits = mem_type_bits;

   return VK_SUCCESS;
}

static AHardwareBuffer *
vn_android_ahb_allocate(uint32_t width,
                        uint32_t height,
                        uint32_t layers,
                        uint32_t format,
                        uint64_t usage)
{
   AHardwareBuffer *ahb = NULL;
   AHardwareBuffer_Desc desc;
   int ret = 0;

   memset(&desc, 0, sizeof(desc));
   desc.width = width;
   desc.height = height;
   desc.layers = layers;
   desc.format = format;
   desc.usage = usage;

   ret = AHardwareBuffer_allocate(&desc, &ahb);
   if (ret) {
      /* We just log the error code here for now since the platform falsely
       * maps all gralloc allocation failures to oom.
       */
      vn_log(NULL, "AHB alloc(w=%u,h=%u,l=%u,f=%u,u=%" PRIu64 ") failed(%d)",
             width, height, layers, format, usage, ret);
      return NULL;
   }

   return ahb;
}

VkResult
vn_android_device_import_ahb(struct vn_device *dev,
                             struct vn_device_memory *mem,
                             const struct VkMemoryAllocateInfo *alloc_info)
{
   const struct vk_device_memory *mem_vk = &mem->base.vk;
   const native_handle_t *handle = NULL;
   int dma_buf_fd = -1;
   int dup_fd = -1;
   uint64_t alloc_size = 0;
   uint32_t mem_type_bits = 0;
   uint32_t mem_type_index = mem_vk->memory_type_index;
   VkResult result = VK_SUCCESS;

   handle = AHardwareBuffer_getNativeHandle(mem_vk->ahardware_buffer);
   dma_buf_fd = vn_android_gralloc_get_dma_buf_fd(handle);
   if (dma_buf_fd < 0)
      return VK_ERROR_INVALID_EXTERNAL_HANDLE;

   result = vn_get_memory_dma_buf_properties(dev, dma_buf_fd, &alloc_size,
                                             &mem_type_bits);
   if (result != VK_SUCCESS)
      return result;

   const VkMemoryDedicatedAllocateInfo *dedicated_info =
      vk_find_struct_const(alloc_info->pNext, MEMORY_DEDICATED_ALLOCATE_INFO);

   /* If ahb is for an image, finish the deferred image creation first */
   if (dedicated_info && dedicated_info->image != VK_NULL_HANDLE) {
      struct vn_image *img = vn_image_from_handle(dedicated_info->image);
      struct vn_android_image_builder builder;

      result = vn_android_get_image_builder(dev, &img->deferred_info->create,
                                            handle, &builder);
      if (result != VK_SUCCESS)
         return result;

      result = vn_image_init_deferred(dev, &builder.create, img);
      if (result != VK_SUCCESS)
         return result;

      const VkMemoryRequirements *mem_req =
         &img->requirements[0].memory.memoryRequirements;
      if (alloc_size < mem_req->size) {
         vn_log(dev->instance,
                "alloc_size(%" PRIu64 ") mem_req->size(%" PRIu64 ")",
                alloc_size, mem_req->size);
         return VK_ERROR_INVALID_EXTERNAL_HANDLE;
      }

      alloc_size = mem_req->size;

      /* Per spec 11.2.3. Device Memory Allocation
       *
       * If the parameters define an export operation and the external handle
       * type is
       * VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID,
       * implementations should not strictly follow memoryTypeIndex. Instead,
       * they should modify the allocation internally to use the required
       * memory type for the application’s given usage. This is because for an
       * export operation, there is currently no way for the client to know
       * the memory type index before allocating.
       */
      if (!(mem_vk->import_handle_type &
            VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID)) {
         if ((mem_type_bits & mem_req->memoryTypeBits) == 0) {
            vn_log(dev->instance, "memoryTypeBits: img(0x%X) fd(0x%X)",
                   mem_req->memoryTypeBits, mem_type_bits);
            return VK_ERROR_INVALID_EXTERNAL_HANDLE;
         }

         mem_type_index = ffs(mem_type_bits & mem_req->memoryTypeBits) - 1;
      }
   }

   if (dedicated_info && dedicated_info->buffer != VK_NULL_HANDLE) {
      struct vn_buffer *buf = vn_buffer_from_handle(dedicated_info->buffer);
      const VkMemoryRequirements *mem_req =
         &buf->requirements.memory.memoryRequirements;
      if (alloc_size < mem_req->size) {
         vn_log(dev->instance,
                "alloc_size(%" PRIu64 ") mem_req->size(%" PRIu64 ")",
                alloc_size, mem_req->size);
         return VK_ERROR_INVALID_EXTERNAL_HANDLE;
      }

      alloc_size = mem_req->size;

      assert((1 << mem_type_index) & mem_req->memoryTypeBits);
   }

   assert((1 << mem_type_index) & mem_type_bits);

   errno = 0;
   dup_fd = os_dupfd_cloexec(dma_buf_fd);
   if (dup_fd < 0)
      return (errno == EMFILE) ? VK_ERROR_TOO_MANY_OBJECTS
                               : VK_ERROR_OUT_OF_HOST_MEMORY;

   /* Spec requires AHB export info to be present, so we must strip it. In
    * practice, the AHB import path here only needs the main allocation info
    * and the dedicated_info.
    */
   VkMemoryDedicatedAllocateInfo local_dedicated_info;
   /* Override when dedicated_info exists and is not the tail struct. */
   if (dedicated_info && dedicated_info->pNext) {
      local_dedicated_info = *dedicated_info;
      local_dedicated_info.pNext = NULL;
      dedicated_info = &local_dedicated_info;
   }
   const VkMemoryAllocateInfo local_alloc_info = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
      .pNext = dedicated_info,
      .allocationSize = alloc_size,
      .memoryTypeIndex = mem_type_index,
   };
   result =
      vn_device_memory_import_dma_buf(dev, mem, &local_alloc_info, dup_fd);
   if (result != VK_SUCCESS) {
      close(dup_fd);
      return result;
   }

   return VK_SUCCESS;
}

uint32_t
vn_android_get_ahb_buffer_memory_type_bits(struct vn_device *dev)
{
   static const uint32_t format = AHARDWAREBUFFER_FORMAT_BLOB;
   /* ensure dma_buf_memory_type_bits covers host visible usage */
   static const uint64_t usage = AHARDWAREBUFFER_USAGE_GPU_DATA_BUFFER |
                                 AHARDWAREBUFFER_USAGE_CPU_READ_RARELY |
                                 AHARDWAREBUFFER_USAGE_CPU_WRITE_RARELY;
   AHardwareBuffer *ahb = vn_android_ahb_allocate(4096, 1, 1, format, usage);
   if (!ahb)
      return 0;

   int dma_buf_fd =
      vn_android_gralloc_get_dma_buf_fd(AHardwareBuffer_getNativeHandle(ahb));
   if (dma_buf_fd < 0) {
      AHardwareBuffer_release(ahb);
      return 0;
   }

   uint64_t alloc_size = 0;
   uint32_t mem_type_bits = 0;
   VkResult ret = vn_get_memory_dma_buf_properties(
      dev, dma_buf_fd, &alloc_size, &mem_type_bits);
   /* release ahb first as below no longer needs it */
   AHardwareBuffer_release(ahb);

   if (ret != VK_SUCCESS) {
      vn_log(dev->instance, "AHB buffer mem type bits query failed %d", ret);
      return 0;
   }

   return mem_type_bits;
}

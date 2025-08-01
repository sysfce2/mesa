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
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include "drm-uapi/drm_fourcc.h"

#include "anv_private.h"
#include "common/intel_aux_map.h"
#include "util/u_debug.h"
#include "vk_util.h"
#include "util/u_math.h"

#include "vk_format.h"
#include "av1_tables.h"

#define ANV_OFFSET_IMPLICIT UINT64_MAX

static const enum isl_surf_dim
vk_to_isl_surf_dim[] = {
   [VK_IMAGE_TYPE_1D] = ISL_SURF_DIM_1D,
   [VK_IMAGE_TYPE_2D] = ISL_SURF_DIM_2D,
   [VK_IMAGE_TYPE_3D] = ISL_SURF_DIM_3D,
};

static uint64_t MUST_CHECK UNUSED
memory_range_end(struct anv_image_memory_range memory_range)
{
   assert(anv_is_aligned(memory_range.offset, memory_range.alignment));
   return memory_range.offset + memory_range.size;
}

/**
 * Get binding for VkImagePlaneMemoryRequirementsInfo,
 * VkBindImagePlaneMemoryInfo and VkDeviceImageMemoryRequirements.
 */
enum anv_image_memory_binding
anv_image_aspect_to_binding(struct anv_image *image,
                            VkImageAspectFlags aspect)
{
   uint32_t plane = 0;

   assert(image->disjoint);

   if (image->vk.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
      /* Spec requires special aspects for modifier images. */
      assert(aspect == VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT ||
             aspect == VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT ||
             aspect == VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT ||
             aspect == VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT);

      /* We don't advertise DISJOINT for modifiers with aux, and therefore we
       * don't handle queries of the modifier's "aux plane" here.
       */
      assert(!isl_drm_modifier_has_aux(image->vk.drm_format_mod));

      switch(aspect) {
         case VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT: plane = 0; break;
         case VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT: plane = 1; break;
         case VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT: plane = 2; break;
         case VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT: plane = 3; break;
      }
   } else {
      plane = anv_image_aspect_to_plane(image, aspect);
   }

   return ANV_IMAGE_MEMORY_BINDING_PLANE_0 + plane;
}

/**
 * Extend the memory binding's range by appending a new memory range with `size`
 * and `alignment` at `offset`. Return the appended range.
 *
 * Offset is ignored if ANV_OFFSET_IMPLICIT.
 *
 * The given binding must not be ANV_IMAGE_MEMORY_BINDING_MAIN. The function
 * converts to MAIN as needed.
 */
static VkResult MUST_CHECK
image_binding_grow(const struct anv_device *device,
                   struct anv_image *image,
                   enum anv_image_memory_binding binding,
                   uint64_t offset,
                   uint64_t size,
                   uint32_t alignment,
                   struct anv_image_memory_range *out_range)
{
   /* We overwrite 'offset' but need to remember if it was implicit. */
   const bool has_implicit_offset = (offset == ANV_OFFSET_IMPLICIT);

   assert(size > 0);
   assert(util_is_power_of_two_or_zero(alignment));

   switch (binding) {
   case ANV_IMAGE_MEMORY_BINDING_MAIN:
      /* The caller must not pre-translate BINDING_PLANE_i to BINDING_MAIN. */
      UNREACHABLE("ANV_IMAGE_MEMORY_BINDING_MAIN");
   case ANV_IMAGE_MEMORY_BINDING_PLANE_0:
   case ANV_IMAGE_MEMORY_BINDING_PLANE_1:
   case ANV_IMAGE_MEMORY_BINDING_PLANE_2:
      if (!image->disjoint)
         binding = ANV_IMAGE_MEMORY_BINDING_MAIN;
      break;
   case ANV_IMAGE_MEMORY_BINDING_PRIVATE:
      assert(offset == ANV_OFFSET_IMPLICIT);
      break;
   case ANV_IMAGE_MEMORY_BINDING_END:
      UNREACHABLE("ANV_IMAGE_MEMORY_BINDING_END");
   }

   struct anv_image_memory_range *container =
      &image->bindings[binding].memory_range;

   if (has_implicit_offset) {
      offset = align64(container->offset + container->size, alignment);
   } else {
      /* Offset must be validated because it comes from
       * VkImageDrmFormatModifierExplicitCreateInfoEXT.
       */
      if (unlikely(!anv_is_aligned(offset, alignment))) {
         return vk_errorf(device,
                          VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT,
                          "VkImageDrmFormatModifierExplicitCreateInfoEXT::"
                          "pPlaneLayouts[]::offset is misaligned");
      }
   }

   /* Surfaces can be added out of memory-order. Track the end of each memory
    * plane to update the binding size properly.
    */
   uint64_t memory_range_end;
   if (__builtin_add_overflow(offset, size, &memory_range_end)) {
      if (has_implicit_offset) {
         assert(!"overflow");
         return vk_errorf(device, VK_ERROR_UNKNOWN,
                          "internal error: overflow in %s", __func__);
      } else {
         return vk_errorf(device,
                          VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT,
                          "VkImageDrmFormatModifierExplicitCreateInfoEXT::"
                          "pPlaneLayouts[]::offset is too large");
      }
   }

   container->size = MAX2(container->size, memory_range_end);
   container->alignment = MAX2(container->alignment, alignment);

   *out_range = (struct anv_image_memory_range) {
      .binding = binding,
      .alignment = alignment,
      .size = size,
      .offset = offset,
   };

   return VK_SUCCESS;
}

/**
 * Adjust range 'a' to contain range 'b'.
 *
 * For simplicity's sake, the offset of 'a' must be 0 and remains 0.
 * If 'a' and 'b' target different bindings, then no merge occurs.
 */
static void
memory_range_merge(struct anv_image_memory_range *a,
                   const struct anv_image_memory_range b)
{
   if (b.size == 0)
      return;

   if (a->binding != b.binding)
      return;

   assert(a->offset == 0);
   assert(anv_is_aligned(a->offset, a->alignment));
   assert(anv_is_aligned(b.offset, b.alignment));

   a->alignment = MAX2(a->alignment, b.alignment);
   a->size = MAX2(a->size, b.offset + b.size);
}

isl_surf_usage_flags_t
anv_image_choose_isl_surf_usage(struct anv_physical_device *device,
                                VkFormat vk_format,
                                VkImageCreateFlags vk_create_flags,
                                VkImageUsageFlags vk_usage,
                                isl_surf_usage_flags_t isl_extra_usage,
                                VkImageAspectFlagBits aspect,
                                VkImageCompressionFlagsEXT comp_flags)
{
   isl_surf_usage_flags_t isl_usage = isl_extra_usage;
   const struct intel_device_info *devinfo = &device->info;

   /* On platform like MTL, we choose to allocate additional CCS memory at the
    * back of the VkDeviceMemory objects since different images can share the
    * AUX-TT PTE because the HW doesn't care about the image format in the
    * PTE. That means we can always ignore the AUX-TT alignment requirement
    * from an ISL point of view.
    */
   if (device->alloc_aux_tt_mem)
      isl_usage |= ISL_SURF_USAGE_NO_AUX_TT_ALIGNMENT_BIT;

   if (vk_usage & VK_IMAGE_USAGE_SAMPLED_BIT)
      isl_usage |= ISL_SURF_USAGE_TEXTURE_BIT;

   if (vk_usage & VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
      isl_usage |= ISL_SURF_USAGE_TEXTURE_BIT;

   if (vk_usage & VK_IMAGE_USAGE_STORAGE_BIT)
      isl_usage |= ISL_SURF_USAGE_STORAGE_BIT;

   if (vk_usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
      isl_usage |= ISL_SURF_USAGE_RENDER_TARGET_BIT;

   if (vk_usage & VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR) {
      isl_usage |= ISL_SURF_USAGE_CPB_BIT;

      /* The CPS compression scheme matches STC_CCS. So, we can allow
       * compression for BLORP writes, but not for general rendering
       * nor image stores.
       */
      if (devinfo->verx10 == 125 &&
          vk_usage & (VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT |
                      VK_IMAGE_USAGE_STORAGE_BIT))
         isl_usage |= ISL_SURF_USAGE_DISABLE_AUX_BIT;
   }

   /* TODO: consider whether compression with sparse is workable. */
   if (vk_create_flags & VK_IMAGE_CREATE_SPARSE_BINDING_BIT)
      isl_usage |= ISL_SURF_USAGE_SPARSE_BIT |
                   ISL_SURF_USAGE_DISABLE_AUX_BIT;

   if (vk_usage & VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR ||
       vk_usage & VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR ||
       vk_usage & VK_IMAGE_USAGE_VIDEO_ENCODE_DPB_BIT_KHR ||
       vk_usage & VK_IMAGE_USAGE_VIDEO_ENCODE_SRC_BIT_KHR)
      isl_usage |= ISL_SURF_USAGE_VIDEO_DECODE_BIT;

   /* We disable aux surfaces for host read/write images so that we can update
    * the main surface without caring about the auxiliary surface.
    */
   if (vk_usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT)
      isl_usage |= ISL_SURF_USAGE_DISABLE_AUX_BIT;

   if (vk_create_flags & VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT)
      isl_usage |= ISL_SURF_USAGE_CUBE_BIT;

   if (vk_create_flags & (VK_IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT |
                          VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT))
      isl_usage |= ISL_SURF_USAGE_2D_3D_COMPATIBLE_BIT;

   if (vk_create_flags & VK_IMAGE_CREATE_PROTECTED_BIT)
      isl_usage |= ISL_SURF_USAGE_PROTECTED_BIT;

   /* Even if we're only using it for transfer operations, clears to depth and
    * stencil images happen as depth and stencil so they need the right ISL
    * usage bits or else things will fall apart.
    */
   switch (aspect) {
   case VK_IMAGE_ASPECT_DEPTH_BIT:
      isl_usage |= ISL_SURF_USAGE_DEPTH_BIT;
      break;
   case VK_IMAGE_ASPECT_STENCIL_BIT:
      isl_usage |= ISL_SURF_USAGE_STENCIL_BIT;
      break;
   case VK_IMAGE_ASPECT_COLOR_BIT:
   case VK_IMAGE_ASPECT_PLANE_0_BIT:
   case VK_IMAGE_ASPECT_PLANE_1_BIT:
   case VK_IMAGE_ASPECT_PLANE_2_BIT:
      break;
   default:
      UNREACHABLE("bad VkImageAspect");
   }

   if (vk_usage & VK_IMAGE_USAGE_TRANSFER_SRC_BIT) {
      /* blorp implements transfers by sampling from the source image. */
      isl_usage |= ISL_SURF_USAGE_TEXTURE_BIT;
   }

   if (vk_usage & VK_IMAGE_USAGE_TRANSFER_DST_BIT &&
       aspect == VK_IMAGE_ASPECT_COLOR_BIT) {
      /* blorp implements transfers by rendering into the destination image.
       * Only request this with color images, as we deal with depth/stencil
       * formats differently. */
      isl_usage |= ISL_SURF_USAGE_RENDER_TARGET_BIT;
   }

   if (comp_flags & VK_IMAGE_COMPRESSION_DISABLED_EXT)
      isl_usage |= ISL_SURF_USAGE_DISABLE_AUX_BIT;

   /* We only need software detiling for 64bit atomics and we need to disable
    * AUX for software detiling, but we don't support sparseImageInt64Atomics,
    * so don't set the flags when using sparse, as they affect which tiling
    * format ISL will choose.
    */
   if (anv_is_storage_format_atomics_emulated(devinfo, vk_format) &&
       (vk_create_flags & VK_IMAGE_CREATE_SPARSE_BINDING_BIT) == 0) {
      isl_usage |= ISL_SURF_USAGE_DISABLE_AUX_BIT |
                   ISL_SURF_USAGE_SOFTWARE_DETILING;
   }

   return isl_usage;
}

static isl_tiling_flags_t
choose_isl_tiling_flags(const struct intel_device_info *devinfo,
                        const struct anv_image *image,
                        const struct anv_image_create_info *anv_info,
                        const struct isl_drm_modifier_info *isl_mod_info)
{
   const VkImageCreateInfo *base_info = anv_info->vk_info;
   isl_tiling_flags_t flags = 0;

   assert((isl_mod_info != NULL) ==
          (base_info->tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT));

   switch (base_info->tiling) {
   default:
      UNREACHABLE("bad VkImageTiling");
   case VK_IMAGE_TILING_OPTIMAL:
      flags = ISL_TILING_ANY_MASK;
      break;
   case VK_IMAGE_TILING_LINEAR:
      flags = ISL_TILING_LINEAR_BIT;
      break;
   case VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT:
      flags = 1 << isl_mod_info->tiling;
   }

   if (anv_info->isl_tiling_flags) {
      assert(isl_mod_info == NULL);
      flags &= anv_info->isl_tiling_flags;
   }

   if (image->vk.wsi_legacy_scanout) {
      isl_tiling_flags_t legacy_mask = ISL_TILING_LINEAR_BIT;
      if (devinfo->has_tiling_uapi)
         legacy_mask |= ISL_TILING_X_BIT;
      flags &= legacy_mask;
   }

   assert(flags);

   return flags;
}

/**
 * Add the surface to the binding at the given offset.
 *
 * \see image_binding_grow()
 */
static VkResult MUST_CHECK
add_surface(struct anv_device *device,
            struct anv_image *image,
            struct anv_surface *surf,
            enum anv_image_memory_binding binding,
            uint64_t offset)
{
   /* isl surface must be initialized */
   assert(surf->isl.size_B > 0);

   return image_binding_grow(device, image, binding, offset,
                             surf->isl.size_B,
                             surf->isl.alignment_B,
                             &surf->memory_range);
}

static bool
image_may_use_r32_view(VkImageCreateFlags create_flags,
                       VkFormat vk_format,
                       const VkImageFormatListCreateInfo *fmt_list)
{
   if (vk_format == VK_FORMAT_R32_SINT ||
       vk_format == VK_FORMAT_R32_UINT ||
       vk_format == VK_FORMAT_R32_SFLOAT)
      return true;

   if (!(create_flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT))
      return false;

   if (fmt_list) {
      for (uint32_t i = 0; i < fmt_list->viewFormatCount; i++) {
         if (fmt_list->pViewFormats[i] == VK_FORMAT_R32_SINT ||
             fmt_list->pViewFormats[i] == VK_FORMAT_R32_UINT ||
             fmt_list->pViewFormats[i] == VK_FORMAT_R32_SFLOAT)
            return true;
      }

      return false;
   }

   return vk_format_get_blocksizebits(vk_format) == 32;
}

static bool
formats_ccs_e_compatible(const struct anv_physical_device *physical_device,
                         VkImageCreateFlags create_flags,
                         VkImageAspectFlagBits aspect,
                         enum isl_format format, VkImageTiling vk_tiling,
                         const VkImageFormatListCreateInfo *fmt_list)
{
   const struct intel_device_info *devinfo = &physical_device->info;

   if (!anv_format_supports_ccs_e(physical_device, format))
      return false;

   /* For images created without MUTABLE_FORMAT_BIT set, we know that they will
    * always be used with the original format. In particular, they will always
    * be used with a format that supports color compression.
    */
   if (!(create_flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT))
      return true;

   if (!fmt_list || fmt_list->viewFormatCount == 0)
      return false;

   for (uint32_t i = 0; i < fmt_list->viewFormatCount; i++) {
      if (fmt_list->pViewFormats[i] == VK_FORMAT_UNDEFINED)
         continue;

      enum isl_format view_format =
         anv_get_isl_format(physical_device, fmt_list->pViewFormats[i],
                            aspect, vk_tiling);

      if (!isl_formats_are_ccs_e_compatible(devinfo, format, view_format))
         return false;
   }

   return true;
}

bool
anv_format_supports_ccs_e(const struct anv_physical_device *physical_device,
                          const enum isl_format format)
{
   /* CCS_E for YCRCB_NORMAL and YCRCB_SWAP_UV is not currently supported by
    * ANV so leave it disabled for now.
    */
   if (isl_format_is_yuv(format))
      return false;

   return isl_format_supports_ccs_e(&physical_device->info, format);
}

bool
anv_formats_ccs_e_compatible(const struct anv_physical_device *physical_device,
                             VkImageCreateFlags create_flags,
                             VkFormat vk_format, VkImageTiling vk_tiling,
                             VkImageUsageFlags vk_usage,
                             const VkImageFormatListCreateInfo *fmt_list)
{
   const struct intel_device_info *devinfo = &physical_device->info;

   u_foreach_bit(b, vk_format_aspects(vk_format)) {
      VkImageAspectFlagBits aspect = 1 << b;
      enum isl_format format =
         anv_get_isl_format(physical_device, vk_format, aspect, vk_tiling);

      if (!formats_ccs_e_compatible(physical_device, create_flags, aspect,
                                    format, vk_tiling, fmt_list))
         return false;
   }

   if ((vk_usage & VK_IMAGE_USAGE_STORAGE_BIT) &&
       vk_tiling != VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
      /* Only color */
      assert((vk_format_aspects(vk_format) & ~VK_IMAGE_ASPECT_ANY_COLOR_BIT_ANV) == 0);
      if (devinfo->ver == 12) {
         /* From the TGL Bspec 44930 (r47128):
          *
          *    "Memory atomic operation on compressed data is not supported
          *     in Gen12 E2E compression. Result of such operation is
          *     undefined.
          *
          *     Software should ensure at the time of the Atomic operation
          *     the surface is resolved (uncompressed) state."
          *
          * On gfx12.0, compression is not supported with atomic
          * operations. On gfx12.5, the support is there, but it's slow
          * (see HSD 1406337848).
          */
         if (image_may_use_r32_view(create_flags, vk_format, fmt_list))
            return false;

      } else if (devinfo->ver <= 11) {
         /* Storage accesses are not supported on compressed surfaces. */
         return false;
      }
   }

   return true;
}

/**
 * For color images that have an auxiliary surface, request allocation for an
 * additional buffer that mainly stores fast-clear values. Use of this buffer
 * allows us to access the image's subresources while being aware of their
 * fast-clear values in non-trivial cases (e.g., outside of a render pass in
 * which a fast clear has occurred).
 *
 * In order to avoid having multiple clear colors for a single plane of an
 * image (hence a single RENDER_SURFACE_STATE), we only allow fast-clears on
 * the first slice (level 0, layer 0).  At the time of our testing (Jan 17,
 * 2018), there were no known applications which would benefit from fast-
 * clearing more than just the first slice.
 *
 * The fast clear portion of the image is laid out in the following order:
 *
 *  * 1 or 4 dwords (depending on hardware generation) for the clear color
 *  * 1 dword for the anv_fast_clear_type of the clear color
 *  * On gfx9+, 1 dword per level and layer of the image (3D levels count
 *    multiple layers) in level-major order for compression state.
 *
 * For the purpose of discoverability, the algorithm used to manage
 * compression and fast-clears is described here:
 *
 *  * On a transition from UNDEFINED or PREINITIALIZED to a defined layout,
 *    all of the values in the fast clear portion of the image are initialized
 *    to default values.
 *
 *  * On fast-clear, the clear value is written into surface state and also
 *    into the buffer and the fast clear type is set appropriately.  Both
 *    setting the fast-clear value in the buffer and setting the fast-clear
 *    type happen from the GPU using MI commands.
 *
 *  * Whenever a render or blorp operation is performed with CCS_E, we call
 *    genX(cmd_buffer_mark_image_written) to set the compression state to
 *    true (which is represented by UINT32_MAX).
 *
 *  * On pipeline barrier transitions, the worst-case transition is computed
 *    from the image layouts.  The command streamer inspects the fast clear
 *    type and compression state dwords and constructs a predicate.  The
 *    worst-case resolve is performed with the given predicate and the fast
 *    clear and compression state is set accordingly.
 *
 * See anv_layout_to_aux_usage and anv_layout_to_fast_clear_type functions for
 * details on exactly what is allowed in what layouts.
 *
 * On gfx7-9, we do not have a concept of indirect clear colors in hardware.
 * In order to deal with this, we have to do some clear color management.
 *
 *  * For LOAD_OP_LOAD at the top of a renderpass, we have to copy the clear
 *    value from the buffer into the surface state with MI commands.
 *
 *  * For any blorp operations, we pass the address to the clear value into
 *    blorp and it knows to copy the clear color.
 */
static VkResult MUST_CHECK
add_aux_state_tracking_buffer(struct anv_device *device,
                              struct anv_image *image,
                              uint64_t state_offset,
                              uint32_t plane)
{
   assert(image && device);
   /* Xe2+ platforms don't use aux tracking buffers. We shouldn't get here. */
   assert(device->info->ver < 20);
   assert(image->planes[plane].aux_usage != ISL_AUX_USAGE_NONE &&
          image->vk.aspects & (VK_IMAGE_ASPECT_ANY_COLOR_BIT_ANV |
                               VK_IMAGE_ASPECT_DEPTH_BIT));

   unsigned clear_color_state_size;
   if (device->info->ver >= 11) {
      /* When importing an image from another source with a drm modifier that
       * supports clear color, the clear color values are in a 32-byte struct
       * defined in drm_fourcc.h. The fast clear type and compression state
       * are not defined in these drm_fourcc.h, so there won't be memory
       * allocated for these extra meta data by the source.
       *
       * We use the last 2 dwords of the clear color struct's memory to store
       * the fast clear type and the first compression state, so the driver
       * doesn't assume the extra size or need another allocation later.
       *
       * So far, the 2 stolen dwords are either not used in the clear color
       * struct or for features not enabled. There should be no side effect to
       * the hardware and destinations of images exported by this driver.
       *
       * Images with multiple levels or layers are not supported by drm
       * modifiers, so we don't have to apply the above approach or face a
       * bigger shortage from multiple compression states. We just apply the
       * approach to all cases to keep the design unified.
       *
       * As a result, the state starts 8 bytes lower than where it should be.
       */
      assert(device->isl_dev.ss.clear_color_state_size == 32);
      clear_color_state_size = (image->num_view_formats - 1) * 64 + 32 - 8;
   } else {
      /* When sampling or rendering with an sRGB format, HW expects the clear
       * color to be in two different color spaces - sRGB in the former and
       * linear in the latter. Allocate twice the space to support either
       * access.
       */
      assert(device->isl_dev.ss.clear_value_size == 16);
      clear_color_state_size = image->num_view_formats * 16 * 2;
   }

   /* Clear color and fast clear type */
   unsigned state_size = clear_color_state_size + 4;

   /* We only need to track compression on CCS_E surfaces. */
   if (isl_aux_usage_has_ccs_e(image->planes[plane].aux_usage)) {
      if (image->vk.image_type == VK_IMAGE_TYPE_3D) {
         for (uint32_t l = 0; l < image->vk.mip_levels; l++)
            state_size += u_minify(image->vk.extent.depth, l) * 4;
      } else {
         state_size += image->vk.mip_levels * image->vk.array_layers * 4;
      }
   }

   enum anv_image_memory_binding binding =
      ANV_IMAGE_MEMORY_BINDING_PLANE_0 + plane;

   const struct isl_drm_modifier_info *mod_info =
      isl_drm_modifier_get_info(image->vk.drm_format_mod);

   /* If an auxiliary surface is used for an externally-shareable image,
    * we have to hide this from the memory of the image since other
    * processes with access to the memory may not be aware of it or of
    * its current state. So put that auxiliary data into a separate
    * buffer (ANV_IMAGE_MEMORY_BINDING_PRIVATE).
    *
    * But when the image is created with a drm modifier that supports
    * clear color, it will be exported along with main surface.
    */
   if (anv_image_is_externally_shared(image) &&
       !mod_info->supports_clear_color)
      binding = ANV_IMAGE_MEMORY_BINDING_PRIVATE;

   /* The indirect clear color BO requires 64B-alignment on gfx11+. If we're
    * using a modifier with clear color, then some kernels might require a 4k
    * alignment.
    */
   const uint32_t clear_color_alignment =
      (mod_info && mod_info->supports_clear_color) ? 4096 : 64;

   return image_binding_grow(device, image, binding,
                             state_offset, state_size, clear_color_alignment,
                             &image->planes[plane].fast_clear_memory_range);
}

static VkResult MUST_CHECK
add_compression_control_buffer(struct anv_device *device,
                               struct anv_image *image,
                               uint32_t plane,
                               uint32_t binding,
                               uint64_t offset)
{
   assert(device->info->has_aux_map);

   return image_binding_grow(device, image, binding, offset,
                             image->planes[plane].primary_surface.isl.size_B /
                             INTEL_AUX_MAP_MAIN_SIZE_SCALEDOWN,
                             INTEL_AUX_MAP_META_ALIGNMENT_B,
                             &image->planes[plane].compr_ctrl_memory_range);
}

static bool
want_hiz_wt_for_image(const struct intel_device_info *devinfo,
                      const struct anv_image *image)
{
   /* Gen12 only supports single-sampled while Gen20+ supports
    * multi-sampled images.
    */
   if (devinfo->ver < 20 && image->vk.samples > 1)
      return false;

   if ((image->vk.usage & (VK_IMAGE_USAGE_SAMPLED_BIT |
                           VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)) == 0)
      return false;

   /* If this image has the maximum number of samples supported by
    * running platform and will be used as a texture, put the HiZ surface
    * in write-through mode so that we can sample from it.
    *
    * TODO: This is a heuristic trade-off; we haven't tuned it at all.
    */
   return true;
}

/**
 * The return code indicates whether creation of the VkImage should continue
 * or fail, not whether the creation of the aux surface succeeded.  If the aux
 * surface is not required (for example, by neither hardware nor DRM format
 * modifier), then this may return VK_SUCCESS when creation of the aux surface
 * fails.
 *
 * @param offset See add_surface()
 */
static VkResult
add_aux_surface_if_supported(struct anv_device *device,
                             struct anv_image *image,
                             uint32_t plane,
                             struct anv_format_plane plane_format,
                             const VkImageFormatListCreateInfo *fmt_list,
                             uint64_t offset,
                             uint32_t stride,
                             uint64_t aux_state_offset)
{
   VkImageAspectFlags aspect = plane_format.aspect;
   VkResult result;
   bool ok;
   const struct isl_surf *main_surf = &image->planes[plane].primary_surface.isl;

   /* The aux surface must not be already added. */
   assert(!anv_surface_is_valid(&image->planes[plane].aux_surface));

   if (main_surf->usage & ISL_SURF_USAGE_DISABLE_AUX_BIT)
      return VK_SUCCESS;

   uint32_t binding;
   if (image->vk.drm_format_mod == DRM_FORMAT_MOD_INVALID ||
       isl_drm_modifier_has_aux(image->vk.drm_format_mod)) {
      binding = ANV_IMAGE_MEMORY_BINDING_PLANE_0 + plane;
   } else {
      binding = ANV_IMAGE_MEMORY_BINDING_PRIVATE;
   }

   if (main_surf->usage & ISL_SURF_USAGE_DEPTH_BIT) {
      /* We don't advertise that depth buffers could be used as storage
       * images.
       */
       assert(!(image->vk.usage & VK_IMAGE_USAGE_STORAGE_BIT));

      ok = isl_surf_get_hiz_surf(&device->isl_dev, main_surf,
                                 &image->planes[plane].aux_surface.isl);
      if (!ok)
         return VK_SUCCESS;

      if (!isl_surf_supports_ccs(&device->isl_dev, main_surf,
                                 &image->planes[plane].aux_surface.isl)) {
         image->planes[plane].aux_usage = ISL_AUX_USAGE_HIZ;
      } else if (want_hiz_wt_for_image(device->info, image)) {
         assert(device->info->ver >= 12);
         image->planes[plane].aux_usage = ISL_AUX_USAGE_HIZ_CCS_WT;
      } else {
         assert(device->info->ver >= 12);
         image->planes[plane].aux_usage = ISL_AUX_USAGE_HIZ_CCS;
      }

      result = add_surface(device, image, &image->planes[plane].aux_surface,
                           binding, ANV_OFFSET_IMPLICIT);
      if (result != VK_SUCCESS)
         return result;

      if (anv_image_plane_uses_aux_map(device, image, plane)) {
         result = add_compression_control_buffer(device, image, plane,
                                                 binding,
                                                 ANV_OFFSET_IMPLICIT);
         if (result != VK_SUCCESS)
            return result;
      }

      if (device->info->ver == 12 &&
          image->planes[plane].aux_usage == ISL_AUX_USAGE_HIZ_CCS_WT) {
         return add_aux_state_tracking_buffer(device, image, aux_state_offset,
                                              plane);
      }
   } else if (main_surf->usage & (ISL_SURF_USAGE_STENCIL_BIT | ISL_SURF_USAGE_CPB_BIT)) {
      if (!isl_surf_supports_ccs(&device->isl_dev, main_surf, NULL))
         return VK_SUCCESS;

      image->planes[plane].aux_usage = ISL_AUX_USAGE_STC_CCS;

      if (device->info->has_aux_map) {
         result = add_compression_control_buffer(device, image, plane,
                                                 binding,
                                                 ANV_OFFSET_IMPLICIT);
         if (result != VK_SUCCESS)
            return result;
      }
   } else if (image->vk.samples == 1) {
      assert(aspect & VK_IMAGE_ASPECT_ANY_COLOR_BIT_ANV);

      if (device->info->has_flat_ccs || device->info->has_aux_map) {
         ok = isl_surf_supports_ccs(&device->isl_dev, main_surf, NULL);
      } else {
         ok = isl_surf_get_ccs_surf(&device->isl_dev, main_surf,
                                    &image->planes[plane].aux_surface.isl,
                                    stride);
      }
      if (!ok)
         return VK_SUCCESS;

      /* Choose aux usage. */
      if (device->info->verx10 == 125 && !device->physical->disable_fcv) {
         /* FCV is enabled via 3DSTATE_3D_MODE. We'd expect plain CCS_E to
          * perform better because it allows for non-zero fast clear colors,
          * but we've run into regressions in several benchmarks (F1 22 and
          * RDR2) when trying to enable it. When non-zero clear colors are
          * enabled, we've observed many partial resolves. We haven't yet
          * root-caused what layout transitions are causing these resolves,
          * so in the meantime, we choose to reduce our clear color support.
          * With only zero clear colors being supported, we might as well
          * turn on FCV.
          */
         image->planes[plane].aux_usage = ISL_AUX_USAGE_FCV_CCS_E;
      } else if (intel_needs_workaround(device->info, 1607794140)) {
         /* FCV is permanently enabled on this hardware. */
         assert(device->info->verx10 == 120);
         image->planes[plane].aux_usage = ISL_AUX_USAGE_FCV_CCS_E;
      } else if (device->info->ver >= 12) {
         /* Support for CCS_E was already checked for in anv_image_init(). */
         image->planes[plane].aux_usage = ISL_AUX_USAGE_CCS_E;
      } else if (anv_formats_ccs_e_compatible(device->physical,
                                              image->vk.create_flags,
                                              image->vk.format,
                                              image->vk.tiling,
                                              image->vk.usage, fmt_list)) {
         image->planes[plane].aux_usage = ISL_AUX_USAGE_CCS_E;
      } else {
         image->planes[plane].aux_usage = ISL_AUX_USAGE_CCS_D;
      }

      if (device->info->has_flat_ccs) {
         result = VK_SUCCESS;
      } else if (device->info->has_aux_map) {
         result = add_compression_control_buffer(device, image, plane,
                                                 binding, offset);
      } else {
         result = add_surface(device, image,
                              &image->planes[plane].aux_surface, binding,
                              offset);
      }
      if (result != VK_SUCCESS)
         return result;

      if (device->info->ver <= 12)
         return add_aux_state_tracking_buffer(device, image, aux_state_offset,
                                              plane);
   } else if (image->vk.samples > 1) {
      assert(aspect & VK_IMAGE_ASPECT_ANY_COLOR_BIT_ANV);
      assert(!(image->vk.usage & VK_IMAGE_USAGE_STORAGE_BIT));
      ok = isl_surf_get_mcs_surf(&device->isl_dev, main_surf,
                                 &image->planes[plane].aux_surface.isl);
      if (!ok)
         return VK_SUCCESS;

      if (isl_surf_supports_ccs(&device->isl_dev, main_surf,
                                &image->planes[plane].aux_surface.isl)) {
         image->planes[plane].aux_usage = ISL_AUX_USAGE_MCS_CCS;
      } else {
         image->planes[plane].aux_usage = ISL_AUX_USAGE_MCS;
      }

      result = add_surface(device, image, &image->planes[plane].aux_surface,
                           binding, ANV_OFFSET_IMPLICIT);
      if (result != VK_SUCCESS)
         return result;

      if (anv_image_plane_uses_aux_map(device, image, plane)) {
         result = add_compression_control_buffer(device, image, plane,
                                                 binding,
                                                 ANV_OFFSET_IMPLICIT);
         if (result != VK_SUCCESS)
            return result;
      }

      if (device->info->ver <= 12)
         return add_aux_state_tracking_buffer(device, image, aux_state_offset,
                                              plane);
   }

   return VK_SUCCESS;
}

static VkResult
add_video_buffers(struct anv_device *device,
                  struct anv_image *image,
                  const struct VkVideoProfileListInfoKHR *profile_list,
                  bool independent_profile)
{
   ASSERTED VkResult ok;
   unsigned size = 0;

   if (independent_profile) {
      /* Takes the worst case */
      unsigned w_mb = DIV_ROUND_UP(image->vk.extent.width, ANV_MB_WIDTH);
      unsigned h_mb = DIV_ROUND_UP(image->vk.extent.height, ANV_MB_HEIGHT);
      size = w_mb * h_mb * 128;
   } else {
      size = anv_video_get_image_mv_size(device, image, profile_list);
   }

   if (size == 0)
      return VK_SUCCESS;

   ok = image_binding_grow(device, image, ANV_IMAGE_MEMORY_BINDING_PRIVATE,
                           ANV_OFFSET_IMPLICIT, size, 65536, &image->vid_dmv_top_surface);
   if (ok != VK_SUCCESS)
      return ok;

   /* Doesn't work for av1 without provided profiles */
   if (!independent_profile) {
      for (unsigned i = 0; i < profile_list->profileCount; i++) {
         if (profile_list->pProfiles[i].videoCodecOperation == VK_VIDEO_CODEC_OPERATION_DECODE_AV1_BIT_KHR) {
            ok = image_binding_grow(device, image, ANV_IMAGE_MEMORY_BINDING_PRIVATE,
                                    ANV_OFFSET_IMPLICIT, av1_cdf_max_num_bytes, 4096, &image->av1_cdf_table);
         }
      }
   } else {
      /* Nothing to check if it's AV1 decoding, so we need to allocate av1
       * tables all the time.
       */
      ok = image_binding_grow(device, image, ANV_IMAGE_MEMORY_BINDING_PRIVATE,
                              ANV_OFFSET_IMPLICIT, av1_cdf_max_num_bytes, 4096, &image->av1_cdf_table);
   }

   return ok;
}

/**
 * Initialize the anv_image::*_surface selected by \a aspect. Then update the
 * image's memory requirements (that is, the image's size and alignment).
 *
 * @param offset See add_surface()
 */
static VkResult
add_primary_surface(struct anv_device *device,
                    struct anv_image *image,
                    uint32_t plane,
                    struct anv_format_plane plane_format,
                    uint64_t offset,
                    uint32_t stride,
                    isl_tiling_flags_t isl_tiling_flags,
                    isl_surf_usage_flags_t isl_usage)
{
   struct anv_surface *anv_surf = &image->planes[plane].primary_surface;
   bool ok;

   uint32_t width = image->vk.extent.width;
   uint32_t height = image->vk.extent.height;
   const struct vk_format_ycbcr_info *ycbcr_info =
      vk_format_get_ycbcr_info(image->vk.format);
   if (ycbcr_info) {
      assert(plane < ycbcr_info->n_planes);
      width /= ycbcr_info->planes[plane].denominator_scales[0];
      height /= ycbcr_info->planes[plane].denominator_scales[1];
   }

   ok = isl_surf_init(&device->isl_dev, &anv_surf->isl,
      .dim = vk_to_isl_surf_dim[image->vk.image_type],
      .format = plane_format.isl_format,
      .width = width,
      .height = height,
      .depth = image->vk.extent.depth,
      .levels = image->vk.mip_levels,
      .array_len = image->vk.array_layers,
      .samples = image->vk.samples,
      .min_alignment_B = 0,
      .row_pitch_B = stride,
      .usage = isl_usage,
      .tiling_flags = isl_tiling_flags);

   if (!ok) {
      /* TODO: Should return
       * VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT in come cases.
       */
      return VK_ERROR_OUT_OF_DEVICE_MEMORY;
   }

   image->planes[plane].aux_usage = ISL_AUX_USAGE_NONE;

   return add_surface(device, image, anv_surf,
                      ANV_IMAGE_MEMORY_BINDING_PLANE_0 + plane, offset);
}

#ifndef NDEBUG
static bool MUST_CHECK
memory_range_is_aligned(struct anv_image_memory_range memory_range)
{
   return anv_is_aligned(memory_range.offset, memory_range.alignment);
}

static bool MUST_CHECK
memory_ranges_equal(struct anv_image_memory_range a,
                    struct anv_image_memory_range b)
{
   return a.binding == b.binding &&
          a.alignment == b.alignment &&
          a.size == b.size &&
          a.offset == b.offset;
}
#endif

struct check_memory_range_params {
   struct anv_image_memory_range *accum_ranges;
   const struct anv_surface *test_surface;
   const struct anv_image_memory_range *test_range;
   enum anv_image_memory_binding expect_binding;
};

#define check_memory_range(...) \
   check_memory_range_s(&(struct check_memory_range_params) { __VA_ARGS__ })

static void UNUSED
check_memory_range_s(const struct check_memory_range_params *p)
{
   assert((p->test_surface == NULL) != (p->test_range == NULL));

   const struct anv_image_memory_range *test_range =
      p->test_range ?: &p->test_surface->memory_range;

   struct anv_image_memory_range *accum_range =
      &p->accum_ranges[p->expect_binding];

   assert(test_range->binding == p->expect_binding);
   assert(test_range->offset >= memory_range_end(*accum_range));
   assert(memory_range_is_aligned(*test_range));

   if (p->test_surface) {
      assert(anv_surface_is_valid(p->test_surface));
      assert(p->test_surface->memory_range.alignment ==
             p->test_surface->isl.alignment_B);
   }

   memory_range_merge(accum_range, *test_range);
}

/**
 * Validate the image's memory bindings *after* all its surfaces and memory
 * ranges are final.
 *
 * For simplicity's sake, we do not validate free-form layout of the image's
 * memory bindings. We validate the layout described in the comments of struct
 * anv_image.
 */
static void
check_memory_bindings(const struct anv_device *device,
                     const struct anv_image *image)
{
#if MESA_DEBUG
   /* As we inspect each part of the image, we merge the part's memory range
    * into these accumulation ranges.
    */
   struct anv_image_memory_range accum_ranges[ANV_IMAGE_MEMORY_BINDING_END];
   for (int i = 0; i < ANV_IMAGE_MEMORY_BINDING_END; ++i) {
      accum_ranges[i] = (struct anv_image_memory_range) {
         .binding = i,
      };
   }

   for (uint32_t p = 0; p < image->n_planes; ++p) {
      const struct anv_image_plane *plane = &image->planes[p];

      /* The binding that must contain the plane's primary surface. */
      const enum anv_image_memory_binding primary_binding = image->disjoint
         ? ANV_IMAGE_MEMORY_BINDING_PLANE_0 + p
         : ANV_IMAGE_MEMORY_BINDING_MAIN;

      /* Aliasing is incompatible with the private binding because it does not
       * live in a VkDeviceMemory.  The exception is either swapchain images or
       * that the private binding is for a video motion vector buffer.
       */
      assert(!(image->vk.create_flags & VK_IMAGE_CREATE_ALIAS_BIT) ||
             image->from_wsi ||
             (plane->primary_surface.isl.usage & ISL_SURF_USAGE_VIDEO_DECODE_BIT) ||
             image->bindings[ANV_IMAGE_MEMORY_BINDING_PRIVATE].memory_range.size == 0);

      /* Check primary surface */
      check_memory_range(accum_ranges,
                         .test_surface = &plane->primary_surface,
                         .expect_binding = primary_binding);

      /* Check aux_surface */
      const struct anv_image_memory_range *aux_mem_range =
         anv_image_get_aux_memory_range(image, p);
      if (aux_mem_range->size > 0) {
         enum anv_image_memory_binding binding = primary_binding;

         /* If an auxiliary surface is used for an externally-shareable image,
          * we have to hide this from the memory of the image since other
          * processes with access to the memory may not be aware of it or of
          * its current state. So put that auxiliary data into a separate
          * buffer (ANV_IMAGE_MEMORY_BINDING_PRIVATE).
          */
         if (anv_image_is_externally_shared(image) &&
             !isl_drm_modifier_has_aux(image->vk.drm_format_mod)) {
            binding = ANV_IMAGE_MEMORY_BINDING_PRIVATE;
         }

         /* Display hardware requires that the aux surface start at
          * a higher address than the primary surface. The 3D hardware
          * doesn't care, but we enforce the display requirement in case
          * the image is sent to display.
          */
         check_memory_range(accum_ranges,
                            .test_range = aux_mem_range,
                            .expect_binding = binding);
      }

      /* Check fast clear state */
      if (plane->fast_clear_memory_range.size > 0) {
         enum anv_image_memory_binding binding = primary_binding;

         /* If an auxiliary surface is used for an externally-shareable image,
          * we have to hide this from the memory of the image since other
          * processes with access to the memory may not be aware of it or of
          * its current state. So put that auxiliary data into a separate
          * buffer (ANV_IMAGE_MEMORY_BINDING_PRIVATE).
          *
          * But when the image is created with a drm modifier that supports
          * clear color, it will be exported along with main surface.
          */
         if (anv_image_is_externally_shared(image)
             && !isl_drm_modifier_get_info(image->vk.drm_format_mod)->supports_clear_color) {
            binding = ANV_IMAGE_MEMORY_BINDING_PRIVATE;
         }

         /* The indirect clear color BO requires 64B-alignment on gfx11+. */
         assert(plane->fast_clear_memory_range.alignment % 64 == 0);
         check_memory_range(accum_ranges,
                            .test_range = &plane->fast_clear_memory_range,
                            .expect_binding = binding);
      }
   }
#endif
}

/**
 * Check that the fully-initialized anv_image is compatible with its DRM format
 * modifier.
 *
 * Checking compatibility at the end of image creation is prudent, not
 * superfluous, because usage of modifiers triggers numerous special cases
 * throughout queries and image creation, and because
 * vkGetPhysicalDeviceImageFormatProperties2 has difficulty detecting all
 * incompatibilities.
 *
 * Return VK_ERROR_UNKNOWN if the incompatibility is difficult to detect in
 * vkGetPhysicalDeviceImageFormatProperties2.  Otherwise, assert fail.
 *
 * Ideally, if vkGetPhysicalDeviceImageFormatProperties2() succeeds with a given
 * modifier, then vkCreateImage() produces an image that is compatible with the
 * modifier. However, it is difficult to reconcile the two functions to agree
 * due to their complexity. For example, isl_surf_get_ccs_surf() may
 * unexpectedly fail in vkCreateImage(), eliminating the image's aux surface
 * even when the modifier requires one. (Maybe we should reconcile the two
 * functions despite the difficulty).
 */
static VkResult MUST_CHECK
check_drm_format_mod(const struct anv_device *device,
                     const struct anv_image *image)
{
   /* Image must have a modifier if and only if it has modifier tiling. */
   assert((image->vk.drm_format_mod != DRM_FORMAT_MOD_INVALID) ==
          (image->vk.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT));

   if (image->vk.drm_format_mod == DRM_FORMAT_MOD_INVALID)
      return VK_SUCCESS;

   const struct isl_drm_modifier_info *isl_mod_info =
      isl_drm_modifier_get_info(image->vk.drm_format_mod);

   /* Driver must support the modifier. */
   assert(isl_drm_modifier_get_score(device->info, isl_mod_info->modifier));

   /* Enforced by us, not the Vulkan spec. */
   assert(image->vk.image_type == VK_IMAGE_TYPE_2D || image->vk.drm_format_mod == DRM_FORMAT_MOD_LINEAR);
   assert(!(image->vk.aspects & VK_IMAGE_ASPECT_DEPTH_BIT));
   assert(!(image->vk.aspects & VK_IMAGE_ASPECT_STENCIL_BIT));
   assert(image->vk.mip_levels == 1);
   assert(image->vk.array_layers == 1 || image->vk.drm_format_mod == DRM_FORMAT_MOD_LINEAR);
   assert(image->vk.samples == 1);

   for (int i = 0; i < image->n_planes; ++i) {
      const struct anv_image_plane *plane = &image->planes[i];
      ASSERTED const struct isl_format_layout *isl_layout =
         isl_format_get_layout(plane->primary_surface.isl.format);

      /* Enforced by us, not the Vulkan spec. */
      assert(isl_layout->txc == ISL_TXC_NONE);
      assert(isl_layout->colorspace == ISL_COLORSPACE_LINEAR ||
             isl_layout->colorspace == ISL_COLORSPACE_SRGB);

      if (isl_drm_modifier_has_aux(isl_mod_info->modifier)) {
         /* Reject DISJOINT for consistency with the GL driver. */
         assert(!image->disjoint);

         /* The modifier's required aux usage mandates the image's aux usage.
          * The inverse, however, does not hold; if the modifier has no aux
          * usage, then we may enable a private aux surface.
          */
         if ((isl_mod_info->supports_media_compression &&
              plane->aux_usage != ISL_AUX_USAGE_MC) ||
             (isl_mod_info->supports_render_compression &&
              !isl_aux_usage_has_ccs_e(plane->aux_usage))) {
            return vk_errorf(device, VK_ERROR_UNKNOWN,
                             "image with modifier unexpectedly has wrong aux "
                             "usage");
         }
      }
   }

   return VK_SUCCESS;
}

/**
 * Use when the app does not provide
 * VkImageDrmFormatModifierExplicitCreateInfoEXT.
 */
static VkResult MUST_CHECK
add_all_surfaces_implicit_layout(
   struct anv_device *device,
   struct anv_image *image,
   const VkImageFormatListCreateInfo *format_list_info,
   uint32_t stride,
   isl_tiling_flags_t isl_tiling_flags,
   isl_surf_usage_flags_t isl_extra_usage_flags)
{
   VkResult result;

   const struct vk_format_ycbcr_info *ycbcr_info =
      vk_format_get_ycbcr_info(image->vk.format);
   if (ycbcr_info)
      assert(ycbcr_info->n_planes == image->n_planes);

   unsigned num_aspects = 0;
   VkImageAspectFlagBits aspects[3];
   u_foreach_bit(b, image->vk.aspects) {
      assert(num_aspects < 3);
      aspects[num_aspects++] = 1 << b;
   }
   assert(num_aspects == image->n_planes);

   /* The Android hardware buffer YV12 format has the planes ordered as Y-Cr-Cb,
    * while Vulkan expects VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM to be in Y-Cb-Cr.
    * Adjust the order we add the ISL surfaces accordingly so the implicit
    * offset gets calculated correctly.
    */
   if (image->from_ahb && image->vk.format == VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM) {
      assert(num_aspects == 3);
      assert(aspects[1] == VK_IMAGE_ASPECT_PLANE_1_BIT);
      assert(aspects[2] == VK_IMAGE_ASPECT_PLANE_2_BIT);
      aspects[1] = VK_IMAGE_ASPECT_PLANE_2_BIT;
      aspects[2] = VK_IMAGE_ASPECT_PLANE_1_BIT;
   }

   for (unsigned i = 0; i < num_aspects; i++) {
      VkImageAspectFlagBits aspect = aspects[i];
      const uint32_t plane = anv_image_aspect_to_plane(image, aspect);
      const  struct anv_format_plane plane_format =
         anv_get_format_plane(device->physical, image->vk.format,
                              plane, image->vk.tiling);

      enum isl_format isl_fmt = plane_format.isl_format;
      assert(isl_fmt != ISL_FORMAT_UNSUPPORTED);

      uint32_t plane_stride = stride * isl_format_get_layout(isl_fmt)->bpb / 8;
      if (ycbcr_info)
         plane_stride /= ycbcr_info->planes[plane].denominator_scales[0];

      VkImageUsageFlags vk_usage = vk_image_usage(&image->vk, aspect);
      isl_surf_usage_flags_t isl_usage =
         anv_image_choose_isl_surf_usage(device->physical,
                                         image->vk.format,
                                         image->vk.create_flags, vk_usage,
                                         isl_extra_usage_flags, aspect,
                                         image->vk.compr_flags);

      result = add_primary_surface(device, image, plane, plane_format,
                                   ANV_OFFSET_IMPLICIT, plane_stride,
                                   isl_tiling_flags, isl_usage);
      if (result != VK_SUCCESS)
         return result;

      result = add_aux_surface_if_supported(device, image, plane, plane_format,
                                            format_list_info,
                                            ANV_OFFSET_IMPLICIT, plane_stride,
                                            ANV_OFFSET_IMPLICIT);
      if (result != VK_SUCCESS)
         return result;
   }

   return VK_SUCCESS;
}

/**
 * Use when the app provides VkImageDrmFormatModifierExplicitCreateInfoEXT.
 */
static VkResult
add_all_surfaces_explicit_layout(
   struct anv_device *device,
   struct anv_image *image,
   const VkImageFormatListCreateInfo *format_list_info,
   const VkImageDrmFormatModifierExplicitCreateInfoEXT *drm_info,
   isl_tiling_flags_t isl_tiling_flags,
   isl_surf_usage_flags_t isl_extra_usage_flags)
{
   const struct intel_device_info *devinfo = device->info;
   const uint32_t mod_plane_count = drm_info->drmFormatModifierPlaneCount;
   const bool mod_has_aux =
      isl_drm_modifier_has_aux(drm_info->drmFormatModifier);
   VkResult result;

   /* Currently there is no way to properly map memory planes to format planes
    * and aux planes due to the lack of defined ABI for external multi-planar
    * images.
    */
   if (image->n_planes == 1)
      assert(image->vk.aspects == VK_IMAGE_ASPECT_COLOR_BIT);
   else
      assert(!(image->vk.aspects & ~VK_IMAGE_ASPECT_PLANES_BITS_ANV));

   if (mod_has_aux) {
      assert(image->n_planes == 1);

      /* About valid usage in the Vulkan spec:
       *
       * Unlike vanilla vkCreateImage, which produces undefined behavior on user
       * error, here the spec requires the implementation to return
       * VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT if the app provides
       * a bad plane layout. However, the spec does require
       * drmFormatModifierPlaneCount to be valid.
       *
       * Most validation of plane layout occurs in add_surface().
       */
      uint32_t n_mod_planes =
         isl_drm_modifier_get_plane_count(devinfo,
                                          drm_info->drmFormatModifier,
                                          image->n_planes);
      assert(n_mod_planes == mod_plane_count);
   } else {
      assert(image->n_planes == mod_plane_count);
   }

   /* Reject special values in the app-provided plane layouts. */
   for (uint32_t i = 0; i < mod_plane_count; ++i) {
      if (drm_info->pPlaneLayouts[i].rowPitch == 0) {
         return vk_errorf(device,
                          VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT,
                          "VkImageDrmFormatModifierExplicitCreateInfoEXT::"
                          "pPlaneLayouts[%u]::rowPitch is 0", i);
      }

      if (drm_info->pPlaneLayouts[i].offset == ANV_OFFSET_IMPLICIT) {
         return vk_errorf(device,
                          VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT,
                          "VkImageDrmFormatModifierExplicitCreateInfoEXT::"
                          "pPlaneLayouts[%u]::offset is %" PRIu64,
                          i, ANV_OFFSET_IMPLICIT);
      }
   }

   u_foreach_bit(b, image->vk.aspects) {
      const VkImageAspectFlagBits aspect = 1 << b;
      const uint32_t plane = anv_image_aspect_to_plane(image, aspect);
      const struct anv_format_plane format_plane =
         anv_get_format_plane(device->physical, image->vk.format,
                              plane, image->vk.tiling);
      const VkSubresourceLayout *primary_layout = &drm_info->pPlaneLayouts[plane];

      result = add_primary_surface(device, image, plane,
                                   format_plane,
                                   primary_layout->offset,
                                   primary_layout->rowPitch,
                                   isl_tiling_flags,
                                   isl_extra_usage_flags);
      if (result != VK_SUCCESS)
         return result;

      if (mod_has_aux) {
         const VkSubresourceLayout flat_ccs_layout = {
            .offset = ANV_OFFSET_IMPLICIT,
         };

         const VkSubresourceLayout *aux_layout;

         uint64_t aux_state_offset = ANV_OFFSET_IMPLICIT;

         /* We already asserted on image->n_planes == 1 when mod_has_aux is
          * true above, so the indexes of aux and clear color are just hard-
          * coded without ambiguity.
          */
         if (devinfo->has_flat_ccs) {
            aux_layout = &flat_ccs_layout;
            if (isl_drm_modifier_get_info(
                  drm_info->drmFormatModifier)->supports_clear_color) {
               aux_state_offset = drm_info->pPlaneLayouts[1].offset;
            }
         } else {
            aux_layout = &drm_info->pPlaneLayouts[1];
            if (isl_drm_modifier_get_info(
                  drm_info->drmFormatModifier)->supports_clear_color) {
               aux_state_offset = drm_info->pPlaneLayouts[2].offset;
            }
         }

         result = add_aux_surface_if_supported(device, image, plane,
                                               format_plane,
                                               format_list_info,
                                               aux_layout->offset,
                                               aux_layout->rowPitch,
                                               aux_state_offset);
         if (result != VK_SUCCESS)
            return result;

         assert(isl_aux_usage_has_ccs(image->planes[plane].aux_usage));
         if (aux_state_offset != ANV_OFFSET_IMPLICIT) {
            assert(image->planes[plane].fast_clear_memory_range.size <=
                   device->isl_dev.ss.clear_color_state_size);
         }
      }
   }

   return VK_SUCCESS;
}

static const struct isl_drm_modifier_info *
choose_drm_format_mod(const struct anv_physical_device *device,
                      uint32_t modifier_count,
                      const uint64_t *modifiers,
                      isl_surf_usage_flags_t isl_usage_flags)
{
   uint64_t best_mod = UINT64_MAX;
   uint32_t best_score = 0;

   for (uint32_t i = 0; i < modifier_count; ++i) {
      if ((isl_usage_flags & ISL_SURF_USAGE_DISABLE_AUX_BIT) &&
          isl_drm_modifier_has_aux(modifiers[i])) {
         /* When aux is disabled, we simply cannot choose a modifier with
          * compression.
          */
         continue;
      }
      uint32_t score = isl_drm_modifier_get_score(&device->info, modifiers[i]);
      if (score > best_score) {
         best_mod = modifiers[i];
         best_score = score;
      }
   }

   if (best_score > 0)
      return isl_drm_modifier_get_info(best_mod);
   else
      return NULL;
}

static VkImageUsageFlags
anv_image_create_usage(const VkImageCreateInfo *pCreateInfo,
                       VkImageUsageFlags usage)
{
   /* Add TRANSFER_SRC usage for multisample attachment images. This is
    * because we might internally use the TRANSFER_SRC layout on them for
    * blorp operations associated with resolving those into other attachments
    * at the end of a subpass.
    *
    * Without this additional usage, we compute an incorrect AUX state in
    * anv_layout_to_aux_state().
    */
   if (pCreateInfo->samples > VK_SAMPLE_COUNT_1_BIT &&
       (usage & (VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT |
                 VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT)))
      usage |= VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
   return usage;
}

static VkResult MUST_CHECK
alloc_private_binding(struct anv_device *device,
                      struct anv_image *image,
                      const VkImageCreateInfo *create_info)
{
   struct anv_image_binding *binding =
      &image->bindings[ANV_IMAGE_MEMORY_BINDING_PRIVATE];

   if (binding->memory_range.size == 0)
      return VK_SUCCESS;

   VkResult result = anv_device_alloc_bo(device, "image-binding-private",
                                         binding->memory_range.size, 0, 0,
                                         &binding->address.bo);
   ANV_DMR_BO_ALLOC(&image->vk.base, binding->address.bo, result);

   return result;
}

static void
anv_image_finish_sparse_bindings(struct anv_image *image)
{
   struct anv_device *device =
      container_of(image->vk.base.device, struct anv_device, vk);

   assert(anv_image_is_sparse(image));

   for (int i = 0; i < ANV_IMAGE_MEMORY_BINDING_END; i++) {
      struct anv_image_binding *b = &image->bindings[i];

      if (b->sparse_data.size != 0) {
         assert(b->memory_range.size == b->sparse_data.size);
         assert(b->address.offset == b->sparse_data.address);
         anv_free_sparse_bindings(device, &b->sparse_data);
      }
   }
}

static VkResult MUST_CHECK
anv_image_init_sparse_bindings(struct anv_image *image,
                               const struct anv_image_create_info *create_info)
{
   struct anv_device *device =
      container_of(image->vk.base.device, struct anv_device, vk);
   VkResult result;

   assert(anv_image_is_sparse(image));

   enum anv_bo_alloc_flags alloc_flags = 0;
   uint64_t explicit_address = 0;
   if (image->vk.create_flags & VK_IMAGE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT) {
      alloc_flags |= ANV_BO_ALLOC_FIXED_ADDRESS;

      const VkOpaqueCaptureDescriptorDataCreateInfoEXT *opaque_info =
         vk_find_struct_const(create_info->vk_info->pNext,
                              OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT);
      if (opaque_info)
         explicit_address = *((const uint64_t *)opaque_info->opaqueCaptureDescriptorData);
   }

   for (int i = 0; i < ANV_IMAGE_MEMORY_BINDING_END; i++) {
      struct anv_image_binding *b = &image->bindings[i];

      if (b->memory_range.size != 0) {
         assert(b->sparse_data.size == 0);

         /* From the spec, Custom Sparse Image Block Shapes section:
          *   "... the size in bytes of the custom sparse image block shape
          *    will be reported in VkMemoryRequirements::alignment."
          *
          * ISL should have set this for us, so just assert it here.
          */
         assert(b->memory_range.alignment == ANV_SPARSE_BLOCK_SIZE);
         assert(b->memory_range.size % ANV_SPARSE_BLOCK_SIZE == 0);

         result = anv_init_sparse_bindings(device,
                                           b->memory_range.size,
                                           &b->sparse_data,
                                           alloc_flags,
                                           explicit_address,
                                           &b->address);
         if (result != VK_SUCCESS) {
            anv_image_finish_sparse_bindings(image);
            return result;
         }
      }
   }

   return VK_SUCCESS;
}

bool
anv_image_view_formats_incomplete(const struct anv_image *image)
{
   /* See mark_image_view_formats_incomplete(). */
   return image->num_view_formats < ARRAY_SIZE(image->view_formats) &&
          image->view_formats[image->num_view_formats] ==
          ISL_FORMAT_UNSUPPORTED;
}

static void
mark_image_view_formats_incomplete(struct anv_image *image)
{
   /* We need to reserve space to insert the token for an incomplete list. Use
    * up all the space except for the first entry. This helps various code
    * paths that depend on the list to have decent fall-back behavior. For
    * examples, see add_aux_state_tracking_buffer() and
    * set_image_clear_color().
    */
   assert(image->num_view_formats >= 1);
   image->num_view_formats = 1;

   /* Replace the first unused entry with the token for an incomplete list. */
   image->view_formats[image->num_view_formats] = ISL_FORMAT_UNSUPPORTED;
   assert(anv_image_view_formats_incomplete(image));
}

static void
add_image_view_format(struct anv_image *image, enum isl_format view_format)
{
   /* If this list can't be completed, reject all formats. */
   if (anv_image_view_formats_incomplete(image))
      return;

   /* Reject invalid formats. */
   if (view_format == ISL_FORMAT_UNSUPPORTED)
      return;

   /* Reject duplicate formats. */
   for (int i = 0; i < image->num_view_formats; i++)
      if (view_format == image->view_formats[i])
         return;

   if (image->num_view_formats == ARRAY_SIZE(image->view_formats)) {
      /* The array is full, so we can't add any more entries. */
      mark_image_view_formats_incomplete(image);
   } else {
      /* Add to the end of the array. */
      image->view_formats[image->num_view_formats++] = view_format;
   }
}

VkResult
anv_image_init(struct anv_device *device, struct anv_image *image,
               const struct anv_image_create_info *create_info)
{
   const VkImageCreateInfo *pCreateInfo = create_info->vk_info;
   VkResult r;

   vk_image_init(&device->vk, &image->vk, pCreateInfo);

   image->vk.usage = anv_image_create_usage(pCreateInfo, image->vk.usage);
   image->vk.stencil_usage =
      anv_image_create_usage(pCreateInfo, image->vk.stencil_usage);

   isl_surf_usage_flags_t isl_extra_usage_flags =
      create_info->isl_extra_usage_flags;

   for (int i = 0; i < ANV_IMAGE_MEMORY_BINDING_END; ++i) {
      image->bindings[i] = (struct anv_image_binding) {
         .memory_range = { .binding = i },
      };
   }

   /* In case of AHardwareBuffer import, we don't know the layout yet */
   if (image->vk.external_handle_types &
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID) {
      image->from_ahb = true;
#if DETECT_OS_ANDROID
      image->vk.ahb_format = anv_ahb_format_for_vk_format(image->vk.format);
#endif
      return VK_SUCCESS;
   }

   image->n_planes = anv_get_format_planes(device->physical, image->vk.format);

#ifdef VK_USE_PLATFORM_ANDROID_KHR
   /* In the case of gralloc-backed swap chain image, we don't know the
    * layout yet.
    */
   if (vk_find_struct_const(pCreateInfo->pNext,
                            IMAGE_SWAPCHAIN_CREATE_INFO_KHR) != NULL)
      return VK_SUCCESS;
#endif

   const struct wsi_image_create_info *wsi_info =
      vk_find_struct_const(pCreateInfo->pNext, WSI_IMAGE_CREATE_INFO_MESA);
   image->from_wsi = wsi_info != NULL;
   image->wsi_blit_src = wsi_info && wsi_info->blit_src;

   /* The Vulkan 1.2.165 glossary says:
    *
    *    A disjoint image consists of multiple disjoint planes, and is created
    *    with the VK_IMAGE_CREATE_DISJOINT_BIT bit set.
    */
   image->disjoint = image->n_planes > 1 &&
                     (pCreateInfo->flags & VK_IMAGE_CREATE_DISJOINT_BIT);

   if (anv_is_compressed_format_emulated(device->physical, pCreateInfo->format)) {
      assert(image->n_planes == 1 &&
             vk_format_is_compressed(image->vk.format));
      assert(!(image->vk.create_flags & VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT));

      image->emu_plane_format = anv_get_compressed_format_emulation(
         device->physical, image->vk.format);

      /* for fetching the raw copmressed data and storing the decompressed
       * data
       */
      image->vk.create_flags |=
         VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT |
         VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT;
      if (image->vk.image_type == VK_IMAGE_TYPE_3D)
         image->vk.create_flags |= VK_IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT;
      image->vk.usage |=
         VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_STORAGE_BIT;
   }

   /* Disable aux if image supports export without modifiers. */
   if (image->vk.external_handle_types != 0 &&
       image->vk.tiling != VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT)
      isl_extra_usage_flags |= ISL_SURF_USAGE_DISABLE_AUX_BIT;

   if (device->queue_count > 1) {
      /* Notify ISL that the app may access this image from different engines.
       * Note that parallel access to the surface will occur regardless of the
       * sharing mode.
       */
      isl_extra_usage_flags |= ISL_SURF_USAGE_MULTI_ENGINE_PAR_BIT;

      /* If the resource is created with the CONCURRENT sharing mode, we can't
       * support compression because we aren't allowed barriers in order to
       * construct the main surface data with FULL_RESOLVE/PARTIAL_RESOLVE.
       */
      if (image->vk.sharing_mode == VK_SHARING_MODE_CONCURRENT)
         isl_extra_usage_flags |= ISL_SURF_USAGE_DISABLE_AUX_BIT;
   }

   /* Aux is pointless if it will never be used as an attachment. */
   if (vk_format_is_depth_or_stencil(image->vk.format) &&
       !(image->vk.usage & VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT))
      isl_extra_usage_flags |= ISL_SURF_USAGE_DISABLE_AUX_BIT;

   /* TODO: Adjust blorp for multi-LOD HiZ surface on Gen9. */
   if (vk_format_has_depth(image->vk.format) &&
       image->vk.mip_levels > 1 && device->info->ver == 9) {
      anv_perf_warn(VK_LOG_OBJS(&image->vk.base), "Enable multi-LOD HiZ");
      isl_extra_usage_flags |= ISL_SURF_USAGE_DISABLE_AUX_BIT;
   }

   /* Mark WSI images with the right surf usage. */
   if (image->from_wsi)
      isl_extra_usage_flags |= ISL_SURF_USAGE_DISPLAY_BIT;

   const VkImageFormatListCreateInfo *fmt_list =
      vk_find_struct_const(pCreateInfo->pNext,
                           IMAGE_FORMAT_LIST_CREATE_INFO);

   if ((image->vk.aspects & VK_IMAGE_ASPECT_ANY_COLOR_BIT_ANV) &&
       image->vk.samples == 1) {
      if (image->n_planes != 1) {
         /* Multiplanar images seem to hit a sampler bug with CCS and R16G16
          * format. (Putting the clear state a page/4096bytes further fixes
          * the issue).
          */
         isl_extra_usage_flags |= ISL_SURF_USAGE_DISABLE_AUX_BIT;
      }

      if ((image->vk.create_flags & VK_IMAGE_CREATE_ALIAS_BIT) &&
          !image->from_wsi) {
         /* The image may alias a plane of a multiplanar image. Above we ban
          * CCS on multiplanar images.
          *
          * We must also reject aliasing of any image that uses
          * ANV_IMAGE_MEMORY_BINDING_PRIVATE. Since we're already rejecting
          * all aliasing here, there's no need to further analyze if the image
          * needs a private binding.
          */
         isl_extra_usage_flags |= ISL_SURF_USAGE_DISABLE_AUX_BIT;
      }

      if (device->info->ver >= 12 &&
          !anv_formats_ccs_e_compatible(device->physical,
                                        image->vk.create_flags,
                                        image->vk.format, image->vk.tiling,
                                        image->vk.usage, fmt_list)) {
         /* CCS_E is the only aux-mode supported for single sampled color
          * surfaces on gfx12+. If we can't support it, we should configure
          * the main surface without aux support.
          */
         isl_extra_usage_flags |= ISL_SURF_USAGE_DISABLE_AUX_BIT;
      }
   }

   /* Fill out the list of view formats. */
   const enum isl_format image_format =
      anv_get_format_plane(device->physical, image->vk.format, 0,
                           image->vk.tiling).isl_format;
   add_image_view_format(image, image_format);

   if (image->vk.usage & (VK_IMAGE_USAGE_TRANSFER_DST_BIT |
                          VK_IMAGE_USAGE_TRANSFER_SRC_BIT)) {
      if (vk_format_is_color(image->vk.format)) {
         const enum isl_format blorp_copy_format =
            blorp_copy_get_color_format(&device->isl_dev, image_format);
         add_image_view_format(image, blorp_copy_format);

         if (vk_format_is_color_depth_stencil_capable(image->vk.format))
            add_image_view_format(image, ISL_FORMAT_RAW);
      } else {
         /* We don't have a blorp_copy format query for depth-stencil formats. */
         mark_image_view_formats_incomplete(image);
      }
   }

   if (image->vk.create_flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT) {
      if (!fmt_list || fmt_list->viewFormatCount == 0) {
         /* Without a format list provided, we must assume all compatible
          * formats. Instead of adding them all, mark our list as incomplete.
          */
         mark_image_view_formats_incomplete(image);
      } else {
         for (uint32_t i = 0; i < fmt_list->viewFormatCount; i++) {
            const enum isl_format fmt_list_format =
               anv_get_format_plane(device->physical,
                                    fmt_list->pViewFormats[i], 0,
                                    image->vk.tiling).isl_format;
            add_image_view_format(image, fmt_list_format);
         }
      }
   }

   const struct VkImageDrmFormatModifierExplicitCreateInfoEXT *mod_explicit_info = NULL;
   const struct isl_drm_modifier_info *isl_mod_info = NULL;
   if (pCreateInfo->tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
      assert(!image->vk.wsi_legacy_scanout);
      mod_explicit_info =
         vk_find_struct_const(pCreateInfo->pNext,
                              IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT);
      if (mod_explicit_info) {
         isl_mod_info = isl_drm_modifier_get_info(mod_explicit_info->drmFormatModifier);
      } else {
         const struct VkImageDrmFormatModifierListCreateInfoEXT *mod_list_info =
            vk_find_struct_const(pCreateInfo->pNext,
                                 IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT);
         isl_mod_info = choose_drm_format_mod(device->physical,
                                              mod_list_info->drmFormatModifierCount,
                                              mod_list_info->pDrmFormatModifiers,
                                              isl_extra_usage_flags);
      }

      if (!isl_mod_info) {
         return vk_errorf(device, VK_ERROR_UNKNOWN,
                          "Cannot choose a suitable modifier to create image");
      }

      assert(image->vk.drm_format_mod == DRM_FORMAT_MOD_INVALID);
      image->vk.drm_format_mod = isl_mod_info->modifier;

      if (isl_drm_modifier_needs_display_layout(image->vk.drm_format_mod))
         isl_extra_usage_flags |= ISL_SURF_USAGE_DISPLAY_BIT;

      /* Disable compression on gen12+ if the selected/requested modifier
       * doesn't support it. Prior to that we can use a private binding for
       * the aux surface and it should be transparent to users.
       */
      if (device->info->ver >= 12 &&
          !isl_drm_modifier_has_aux(image->vk.drm_format_mod)) {
         isl_extra_usage_flags |= ISL_SURF_USAGE_DISABLE_AUX_BIT;
      }
   }

   if (isl_mod_info && isl_mod_info->supports_clear_color) {
      if (image->num_view_formats > 1) {
         /* We use the number of view formats to determine the number of
          * CLEAR_COLOR structures to append to the image. For an imported
          * dmabuf supporting clear colors, we're limited to a single such
          * struct. So, mark the view format list as incomplete because doing
          * so shrinks the list size to one.
          */
         mark_image_view_formats_incomplete(image);
      }
      assert(image->num_view_formats == 1);
   }
   const isl_tiling_flags_t isl_tiling_flags =
      choose_isl_tiling_flags(device->info, image, create_info, isl_mod_info);
   if (mod_explicit_info) {
      r = add_all_surfaces_explicit_layout(device, image, fmt_list,
                                           mod_explicit_info, isl_tiling_flags,
                                           isl_extra_usage_flags);
   } else {
      r = add_all_surfaces_implicit_layout(device, image, fmt_list, create_info->stride,
                                           isl_tiling_flags,
                                           isl_extra_usage_flags);
   }

   if (r != VK_SUCCESS)
      goto fail;

   if (image->emu_plane_format != VK_FORMAT_UNDEFINED) {
      const uint32_t plane = image->n_planes;
      const struct anv_format_plane plane_format = anv_get_format_plane(
            device->physical, image->emu_plane_format, 0, image->vk.tiling);

      /* According to vk_texcompress_astc_emulation_format() and
       * anv_astc_emu_process(), there are a limited number of formats the
       * emulation plane will be accessed as so we can just hardcode all of
       * those here.
       */
      VkFormat emu_format_list[] = {
         VK_FORMAT_R8G8B8A8_UINT,
         VK_FORMAT_R8G8B8A8_SRGB,
         VK_FORMAT_R8G8B8A8_UNORM,
      };

      VkImageFormatListCreateInfo emu_format_list_info = {
         .sType = VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO,
         .pNext = NULL,
         .viewFormatCount = ARRAY_SIZE(emu_format_list),
         .pViewFormats = emu_format_list
      };

      VkImageFormatListCreateInfo *emu_format_list_info_ptr =
         &emu_format_list_info;

      /* We don't care to provide an accurate list on the older platforms
       * which need denorms flushed as they don't support compression on the
       * storage image usage.
       */
      if (device->physical->flush_astc_ldr_void_extent_denorms)
         emu_format_list_info_ptr = NULL;

      assert(image->vk.create_flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT);

      if (device->info->ver >= 12) {
         /* CCS_E is the only aux-mode supported for single sampled color
          * surfaces on gfx12+. Since we hardcoded all the formats the
          * emulation plane will be accessed as, CCS_E support should
          * be guaranteed.
          */
         assert(anv_formats_ccs_e_compatible(device->physical,
                                             image->vk.create_flags,
                                             image->emu_plane_format,
                                             image->vk.tiling,
                                             image->vk.usage,
                                             emu_format_list_info_ptr));
      }

      isl_surf_usage_flags_t isl_usage =
         anv_image_choose_isl_surf_usage(device->physical,
                                         image->vk.format,
                                         image->vk.create_flags,
                                         image->vk.usage,
                                         0,
                                         VK_IMAGE_ASPECT_COLOR_BIT,
                                         image->vk.compr_flags);

      r = add_primary_surface(device, image, plane, plane_format,
                              ANV_OFFSET_IMPLICIT, 0,
                              isl_tiling_flags, isl_usage);
      if (r != VK_SUCCESS)
         goto fail;

      r = add_aux_surface_if_supported(device, image, plane, plane_format,
                                       emu_format_list_info_ptr,
                                       ANV_OFFSET_IMPLICIT, 0,
                                       ANV_OFFSET_IMPLICIT);
      if (r != VK_SUCCESS)
         goto fail;
   }

   const VkVideoProfileListInfoKHR *video_profile =
      vk_find_struct_const(pCreateInfo->pNext,
                           VIDEO_PROFILE_LIST_INFO_KHR);

   bool independent_video_profile =
      pCreateInfo->flags & VK_IMAGE_CREATE_VIDEO_PROFILE_INDEPENDENT_BIT_KHR;

   if (video_profile || independent_video_profile) {
      r = add_video_buffers(device, image, video_profile, independent_video_profile);
      if (r != VK_SUCCESS)
         goto fail;
   }

   if (!create_info->no_private_binding_alloc) {
      r = alloc_private_binding(device, image, pCreateInfo);
      if (r != VK_SUCCESS)
         goto fail;
   }

   check_memory_bindings(device, image);

   r = check_drm_format_mod(device, image);
   if (r != VK_SUCCESS)
      goto fail;

   if (anv_image_is_sparse(image)) {
      r = anv_image_init_sparse_bindings(image, create_info);
      if (r != VK_SUCCESS)
         goto fail;
   }

   return VK_SUCCESS;

fail:
   vk_image_finish(&image->vk);
   return r;
}

void
anv_image_finish(struct anv_image *image)
{
   struct anv_device *device =
      container_of(image->vk.base.device, struct anv_device, vk);

   if (anv_image_is_sparse(image))
      anv_image_finish_sparse_bindings(image);

   /* Unmap a CCS so that if the bound region of the image is rebound to
    * another image, the AUX tables will be cleared to allow for a new
    * mapping.
    */
   for (int p = 0; p < image->n_planes; ++p) {
      if (image->planes[p].aux_tt.mapped) {
         intel_aux_map_del_mapping(device->aux_map_ctx,
                                   image->planes[p].aux_tt.addr,
                                   image->planes[p].aux_tt.size);
      }
   }

   if (image->from_gralloc) {
      assert(!image->disjoint);
      assert(image->n_planes == 1);
      assert(image->planes[0].primary_surface.memory_range.binding ==
             ANV_IMAGE_MEMORY_BINDING_MAIN);
      assert(image->bindings[ANV_IMAGE_MEMORY_BINDING_MAIN].address.bo != NULL);
      anv_device_release_bo(device, image->bindings[ANV_IMAGE_MEMORY_BINDING_MAIN].address.bo);
   }

   for (uint32_t b = 0; b < ARRAY_SIZE(image->bindings); b++) {
      if (image->bindings[b].host_map != NULL) {
         anv_device_unmap_bo(device,
                             image->bindings[b].address.bo,
                             image->bindings[b].host_map,
                             image->bindings[b].map_size,
                             false /* replace */);
      }
   }

   struct anv_bo *private_bo = image->bindings[ANV_IMAGE_MEMORY_BINDING_PRIVATE].address.bo;
   if (private_bo) {
      if (image->device_registered) {
         pthread_mutex_lock(&device->mutex);
         list_del(&image->link);
         pthread_mutex_unlock(&device->mutex);
      }
      ANV_DMR_BO_FREE(&image->vk.base, private_bo);
      anv_device_release_bo(device, private_bo);
   }

   vk_image_finish(&image->vk);
}

static VkResult
anv_image_init_from_create_info(struct anv_device *device,
                                struct anv_image *image,
                                const VkImageCreateInfo *pCreateInfo,
                                bool no_private_binding_alloc)
{
   if (pCreateInfo->flags & VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT) {
      VkResult result =
         anv_sparse_image_check_support(device->physical,
                                        pCreateInfo->flags,
                                        pCreateInfo->tiling,
                                        pCreateInfo->samples,
                                        pCreateInfo->imageType,
                                        pCreateInfo->format,
                                        NULL /* valid_samples_out */);
      if (result != VK_SUCCESS)
         return result;
   }

   const VkNativeBufferANDROID *gralloc_info =
      vk_find_struct_const(pCreateInfo->pNext, NATIVE_BUFFER_ANDROID);
   if (gralloc_info)
      return anv_image_init_from_gralloc(device, image, pCreateInfo,
                                         gralloc_info);

   struct anv_image_create_info create_info = {
      .vk_info = pCreateInfo,
      .no_private_binding_alloc = no_private_binding_alloc,
   };

   return anv_image_init(device, image, &create_info);
}

VkResult anv_CreateImage(
    VkDevice                                    _device,
    const VkImageCreateInfo*                    pCreateInfo,
    const VkAllocationCallbacks*                pAllocator,
    VkImage*                                    pImage)
{
   ANV_FROM_HANDLE(anv_device, device, _device);

   if ((device->physical->sparse_type == ANV_SPARSE_TYPE_NOT_SUPPORTED) &&
       INTEL_DEBUG(DEBUG_SPARSE) &&
       pCreateInfo->flags & (VK_IMAGE_CREATE_SPARSE_BINDING_BIT |
                             VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT |
                             VK_IMAGE_CREATE_SPARSE_ALIASED_BIT))
      fprintf(stderr, "=== %s %s:%d flags:0x%08x\n", __func__, __FILE__,
              __LINE__, pCreateInfo->flags);

#ifndef VK_USE_PLATFORM_ANDROID_KHR
   /* Skip the WSI common swapchain creation here on Android. Similar to ahw,
    * this case is handled by a partial image init and then resolved when the
    * image is bound and gralloc info is passed.
    */
   const VkImageSwapchainCreateInfoKHR *swapchain_info =
      vk_find_struct_const(pCreateInfo->pNext, IMAGE_SWAPCHAIN_CREATE_INFO_KHR);
   if (swapchain_info && swapchain_info->swapchain != VK_NULL_HANDLE) {
      return wsi_common_create_swapchain_image(&device->physical->wsi_device,
                                               pCreateInfo,
                                               swapchain_info->swapchain,
                                               pImage);
   }
#endif

   struct anv_image *image =
      vk_object_zalloc(&device->vk, pAllocator, sizeof(*image),
                       VK_OBJECT_TYPE_IMAGE);
   if (!image)
      return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);

   VkResult result = anv_image_init_from_create_info(device, image,
                                                     pCreateInfo,
                                                     false);
   if (result != VK_SUCCESS) {
      vk_object_free(&device->vk, pAllocator, image);
      return result;
   }

   ANV_RMV(image_create, device, false, image);

   *pImage = anv_image_to_handle(image);

   return result;
}

void
anv_DestroyImage(VkDevice _device, VkImage _image,
                 const VkAllocationCallbacks *pAllocator)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   ANV_FROM_HANDLE(anv_image, image, _image);

   if (!image)
      return;

   ANV_RMV(image_destroy, device, image);

   assert(&device->vk == image->vk.base.device);
   anv_image_finish(image);

   vk_free2(&device->vk.alloc, pAllocator, image);
}

/* We are binding AHardwareBuffer. Get a description, resolve the
 * format and prepare anv_image properly.
 */
static void
resolve_ahw_image(struct anv_device *device,
                  struct anv_image *image,
                  struct anv_device_memory *mem)
{
#if DETECT_OS_ANDROID && ANDROID_API_LEVEL >= 26
   assert(mem->vk.ahardware_buffer);
   AHardwareBuffer_Desc desc;
   AHardwareBuffer_describe(mem->vk.ahardware_buffer, &desc);
   VkResult result;

   /* Check tiling. */
   enum isl_tiling tiling;
   const native_handle_t *handle =
      AHardwareBuffer_getNativeHandle(mem->vk.ahardware_buffer);
   struct u_gralloc_buffer_handle gr_handle = {
      .handle = handle,
      .hal_format = desc.format,
      .pixel_stride = desc.stride,
   };
   result = anv_android_get_tiling(device, &gr_handle, &tiling);
   assert(result == VK_SUCCESS);
   isl_tiling_flags_t isl_tiling_flags = (1u << tiling);

   /* Check format. */
   VkFormat vk_format = vk_format_from_android(desc.format, desc.usage);
   assert(vk_format != VK_FORMAT_UNDEFINED);

   /* Now we are able to fill anv_image fields properly and create
    * isl_surface for it.
    */
   vk_image_set_format(&image->vk, vk_format);
   image->n_planes = anv_get_format_planes(device->physical, image->vk.format);

   result = add_all_surfaces_implicit_layout(device, image, NULL, desc.stride,
                                             isl_tiling_flags,
                                             ISL_SURF_USAGE_DISABLE_AUX_BIT);
   assert(result == VK_SUCCESS);
#endif
}

static VkResult
resolve_anb_image(struct anv_device *device,
                  struct anv_image *image,
                  const VkNativeBufferANDROID *gralloc_info)
{
#if DETECT_OS_ANDROID && ANDROID_API_LEVEL >= 29
   VkResult result;

   /* Do not close the gralloc handle's dma_buf. The lifetime of the dma_buf
    * must exceed that of the gralloc handle, and we do not own the gralloc
    * handle.
    */
   int dma_buf = gralloc_info->handle->data[0];

   /* If this function fails and if the imported bo was resident in the cache,
    * we should avoid updating the bo's flags. Therefore, we defer updating
    * the flags until success is certain.
    *
    */
   struct anv_bo *bo = NULL;
   result = anv_device_import_bo(device, dma_buf,
                                 ANV_BO_ALLOC_EXTERNAL,
                                 0 /* client_address */,
                                 &bo);
   if (result != VK_SUCCESS) {
      return vk_errorf(device, result,
                       "failed to import dma-buf from VkNativeBufferANDROID");
   }

   /* Check tiling. */
   enum isl_tiling tiling;
   struct u_gralloc_buffer_handle gr_handle = {
      .handle = gralloc_info->handle,
      .hal_format = gralloc_info->format,
      .pixel_stride = gralloc_info->stride,
   };
   result = anv_android_get_tiling(device, &gr_handle, &tiling);
   assert(result == VK_SUCCESS);

   isl_tiling_flags_t isl_tiling_flags = (1u << tiling);

   /* Now we are able to fill anv_image fields properly and create
    * isl_surface for it.
    */
   result = add_all_surfaces_implicit_layout(device, image, NULL, gralloc_info->stride,
                                             isl_tiling_flags,
                                             ISL_SURF_USAGE_DISABLE_AUX_BIT);
   if (result != VK_SUCCESS) {
      anv_device_release_bo(device, bo);
      return vk_errorf(device, result,
                       "failed to add surfaces from VkNativeBufferANDROID");
   }

   VkMemoryRequirements2 mem_reqs = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
   };

   anv_image_get_memory_requirements(device, image, image->vk.aspects,
                                     &mem_reqs);

   VkDeviceSize aligned_image_size =
      align64(mem_reqs.memoryRequirements.size,
              mem_reqs.memoryRequirements.alignment);

   if (bo->size < aligned_image_size) {
      result = vk_errorf(device, VK_ERROR_INVALID_EXTERNAL_HANDLE,
                         "dma-buf from VkNativeBufferANDROID is too small for "
                         "VkImage: %"PRIu64"B < %"PRIu64"B",
                         bo->size, aligned_image_size);
      anv_device_release_bo(device, bo);
      return result;
   }

   assert(!image->disjoint);
   assert(image->n_planes == 1);
   assert(image->planes[0].primary_surface.memory_range.binding ==
          ANV_IMAGE_MEMORY_BINDING_MAIN);
   assert(image->bindings[ANV_IMAGE_MEMORY_BINDING_MAIN].address.bo == NULL);
   assert(image->bindings[ANV_IMAGE_MEMORY_BINDING_MAIN].address.offset == 0);
   image->bindings[ANV_IMAGE_MEMORY_BINDING_MAIN].address.bo = bo;
   image->from_gralloc = true;

   return VK_SUCCESS;
#else
   return VK_ERROR_EXTENSION_NOT_PRESENT;
#endif
}

static bool
anv_image_is_pat_compressible(struct anv_device *device, struct anv_image *image)
{
   if (device->info->ver < 20)
      return false;

   /*
    * Be aware that Vulkan spec requires that Images with some properties
    * always returns the same memory types, so this function also needs to
    * have the same return for the same set of properties.
    *
    *    For images created with a color format, the memoryTypeBits member is
    *    identical for all VkImage objects created with the same combination
    *    of values for the tiling member, the
    *    VK_IMAGE_CREATE_SPARSE_BINDING_BIT bit and
    *    VK_IMAGE_CREATE_PROTECTED_BIT bit of the flags member, the
    *    VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT bit of the flags
    *    member, the VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT bit of the usage
    *    member if the
    *    VkPhysicalDeviceHostImageCopyPropertiesEXT::identicalMemoryTypeRequirements
    *    property is VK_FALSE, handleTypes member of
    *    VkExternalMemoryImageCreateInfo, and the
    *    VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT of the usage member in the
    *    VkImageCreateInfo structure passed to vkCreateImage.
    *
    *    For images created with a depth/stencil format, the memoryTypeBits
    *    member is identical for all VkImage objects created with the same
    *    combination of values for the format member, the tiling member, the
    *    VK_IMAGE_CREATE_SPARSE_BINDING_BIT bit and
    *    VK_IMAGE_CREATE_PROTECTED_BIT bit of the flags member, the
    *    VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT bit of the flags
    *    member, the VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT bit of the usage
    *    member if the
    *    VkPhysicalDeviceHostImageCopyPropertiesEXT::identicalMemoryTypeRequirements
    *    property is VK_FALSE, handleTypes member of
    *    VkExternalMemoryImageCreateInfo, and the
    *    VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT of the usage member in the
    *    VkImageCreateInfo structure passed to vkCreateImage.
    */

   /* Because we cannot report different memory types for uncompressed and
    * compressed modifiers on the VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT,
    * We don't set the right compressed memory type set for Xe2 modifiers, in
    * order to keep the memory types same as the uncompressed modifiers. The
    * image will get compressed memory through a dedicated allocation later.
    */
   if (image->vk.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT)
      return false;

   if (image->vk.external_handle_types)
      return false;

   /* Host accessed images cannot be compressed. */
   if (image->vk.usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT)
      return false;

   return true;
}

void
anv_image_get_memory_requirements(struct anv_device *device,
                                  struct anv_image *image,
                                  VkImageAspectFlags aspects,
                                  VkMemoryRequirements2 *pMemoryRequirements)
{
   /* The Vulkan spec (git aaed022) says:
    *
    *    memoryTypeBits is a bitfield and contains one bit set for every
    *    supported memory type for the resource. The bit `1<<i` is set if and
    *    only if the memory type `i` in the VkPhysicalDeviceMemoryProperties
    *    structure for the physical device is supported.
    */
   uint32_t memory_types;

   if (image->vk.create_flags & VK_IMAGE_CREATE_PROTECTED_BIT) {
      memory_types = device->physical->memory.protected_mem_types;
   } else {
      memory_types = device->physical->memory.default_buffer_mem_types;
      if (anv_image_is_pat_compressible(device, image))
         memory_types |= device->physical->memory.compressed_mem_types;
   }

   if (image->vk.usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT) {
      /* Remove non host visible heaps from the types for host transfers on
       * non ReBAR devices
       */
      if (device->physical->has_small_bar) {
         for (uint32_t i = 0; i < device->physical->memory.type_count; i++) {
            if (!(device->physical->memory.types[i].propertyFlags &
                  VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))
               memory_types &= ~BITFIELD_BIT(i);
         }
      }
   }

   vk_foreach_struct(ext, pMemoryRequirements->pNext) {
      switch (ext->sType) {
      case VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS: {
         VkMemoryDedicatedRequirements *requirements = (void *)ext;
         if (image->vk.wsi_legacy_scanout ||
             image->from_ahb ||
             (isl_drm_modifier_has_aux(image->vk.drm_format_mod) &&
              (anv_image_uses_aux_map(device, image) ||
               device->info->ver >= 20))) {
            /* On pre-Xe2 platforms, if we need to set the tiling for external
             * consumers or the modifier involves AUX tables, we need a
             * dedicated allocation. On Xe2+ platforms, a dedicated allocation
             * is still needed because we need to pass modifier information
             * down to the allocation path. Refer to
             * anv_image_is_pat_compressible().
             *
             * See also anv_AllocateMemory().
             */
            requirements->prefersDedicatedAllocation = true;
            requirements->requiresDedicatedAllocation = true;
         } else {
            requirements->prefersDedicatedAllocation = false;
            requirements->requiresDedicatedAllocation = false;
         }
         break;
      }

      default:
         vk_debug_ignored_stype(ext->sType);
         break;
      }
   }

   /* If the image is disjoint, then we must return the memory requirements for
    * the single plane specified in VkImagePlaneMemoryRequirementsInfo. If
    * non-disjoint, then exactly one set of memory requirements exists for the
    * whole image.
    *
    * This is enforced by the Valid Usage for VkImageMemoryRequirementsInfo2,
    * which requires that the app provide VkImagePlaneMemoryRequirementsInfo if
    * and only if the image is disjoint (that is, multi-planar format and
    * VK_IMAGE_CREATE_DISJOINT_BIT).
    */
   enum anv_image_memory_binding binding;
   if (image->disjoint) {
      assert(util_bitcount(aspects) == 1);
      assert(aspects & image->vk.aspects);
      binding = anv_image_aspect_to_binding(image, aspects);
   } else {
      assert(aspects == image->vk.aspects);
      binding = ANV_IMAGE_MEMORY_BINDING_MAIN;
   }

   pMemoryRequirements->memoryRequirements = (VkMemoryRequirements) {
      .size = image->bindings[binding].memory_range.size,
      .alignment = image->bindings[binding].memory_range.alignment,
      .memoryTypeBits = memory_types,
   };
}

void anv_GetImageMemoryRequirements2(
    VkDevice                                    _device,
    const VkImageMemoryRequirementsInfo2*       pInfo,
    VkMemoryRequirements2*                      pMemoryRequirements)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   ANV_FROM_HANDLE(anv_image, image, pInfo->image);

   VkImageAspectFlags aspects = image->vk.aspects;

   vk_foreach_struct_const(ext, pInfo->pNext) {
      switch (ext->sType) {
      case VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO: {
         assert(image->disjoint);
         const VkImagePlaneMemoryRequirementsInfo *plane_reqs =
            (const VkImagePlaneMemoryRequirementsInfo *) ext;
         aspects = plane_reqs->planeAspect;
         break;
      }

      default:
         vk_debug_ignored_stype(ext->sType);
         break;
      }
   }

   anv_image_get_memory_requirements(device, image, aspects,
                                     pMemoryRequirements);
}

void anv_GetDeviceImageMemoryRequirements(
    VkDevice                                    _device,
    const VkDeviceImageMemoryRequirements*   pInfo,
    VkMemoryRequirements2*                      pMemoryRequirements)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   struct anv_image image = { 0 };

   if ((device->physical->sparse_type == ANV_SPARSE_TYPE_NOT_SUPPORTED) &&
       INTEL_DEBUG(DEBUG_SPARSE) &&
       pInfo->pCreateInfo->flags & (VK_IMAGE_CREATE_SPARSE_BINDING_BIT |
                                    VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT |
                                    VK_IMAGE_CREATE_SPARSE_ALIASED_BIT))
      fprintf(stderr, "=== %s %s:%d flags:0x%08x\n", __func__, __FILE__,
              __LINE__, pInfo->pCreateInfo->flags);

   ASSERTED VkResult result =
      anv_image_init_from_create_info(device, &image, pInfo->pCreateInfo, true);
   assert(result == VK_SUCCESS);

   VkImageAspectFlags aspects =
      image.disjoint ? pInfo->planeAspect : image.vk.aspects;

   anv_image_get_memory_requirements(device, &image, aspects,
                                     pMemoryRequirements);
   anv_image_finish(&image);
}

static void
anv_image_get_sparse_memory_requirements(
      struct anv_device *device,
      struct anv_image *image,
      VkImageAspectFlags aspects,
      uint32_t *pSparseMemoryRequirementCount,
      VkSparseImageMemoryRequirements2 *pSparseMemoryRequirements)
{
   VK_OUTARRAY_MAKE_TYPED(VkSparseImageMemoryRequirements2, reqs,
                          pSparseMemoryRequirements,
                          pSparseMemoryRequirementCount);

   /* From the spec:
    *   "The sparse image must have been created using the
    *    VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT flag to retrieve valid sparse
    *    image memory requirements."
    */
   if (!(image->vk.create_flags & VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT))
      return;

   VkSparseImageMemoryRequirements ds_mem_reqs = {};
   VkSparseImageMemoryRequirements2 *ds_reqs_ptr = NULL;

   u_foreach_bit(b, aspects) {
      VkImageAspectFlagBits aspect = 1 << b;
      const uint32_t plane = anv_image_aspect_to_plane(image, aspect);
      struct isl_surf *surf = &image->planes[plane].primary_surface.isl;

      VkSparseImageFormatProperties format_props =
         anv_sparse_calc_image_format_properties(device->physical, aspect,
                                                 image->vk.image_type,
                                                 image->vk.samples, surf);

      uint32_t miptail_first_lod;
      VkDeviceSize miptail_size, miptail_offset, miptail_stride;
      anv_sparse_calc_miptail_properties(device, image, aspect,
                                         &miptail_first_lod, &miptail_size,
                                         &miptail_offset, &miptail_stride);

      VkSparseImageMemoryRequirements mem_reqs = {
         .formatProperties = format_props,
         .imageMipTailFirstLod = miptail_first_lod,
         .imageMipTailSize = miptail_size,
         .imageMipTailOffset = miptail_offset,
         .imageMipTailStride = miptail_stride,
      };

      /* If both depth and stencil are the same, unify them if possible. */
      if (aspect & (VK_IMAGE_ASPECT_DEPTH_BIT |
                    VK_IMAGE_ASPECT_STENCIL_BIT)) {
         if (!ds_reqs_ptr) {
            ds_mem_reqs = mem_reqs;
         } else if (ds_mem_reqs.formatProperties.imageGranularity.width ==
                       mem_reqs.formatProperties.imageGranularity.width &&
                    ds_mem_reqs.formatProperties.imageGranularity.height ==
                       mem_reqs.formatProperties.imageGranularity.height &&
                    ds_mem_reqs.formatProperties.imageGranularity.depth ==
                       mem_reqs.formatProperties.imageGranularity.depth &&
                    ds_mem_reqs.imageMipTailFirstLod ==
                       mem_reqs.imageMipTailFirstLod &&
                    ds_mem_reqs.imageMipTailSize ==
                       mem_reqs.imageMipTailSize &&
                    ds_mem_reqs.imageMipTailOffset ==
                       mem_reqs.imageMipTailOffset &&
                    ds_mem_reqs.imageMipTailStride ==
                       mem_reqs.imageMipTailStride) {
            ds_reqs_ptr->memoryRequirements.formatProperties.aspectMask |=
               aspect;
            continue;
         }
      }

      vk_outarray_append_typed(VkSparseImageMemoryRequirements2, &reqs, r) {
         r->memoryRequirements = mem_reqs;
         if (aspect & (VK_IMAGE_ASPECT_DEPTH_BIT |
                       VK_IMAGE_ASPECT_STENCIL_BIT))
            ds_reqs_ptr = r;
      }
   }
}

void anv_GetImageSparseMemoryRequirements2(
    VkDevice                                    _device,
    const VkImageSparseMemoryRequirementsInfo2* pInfo,
    uint32_t*                                   pSparseMemoryRequirementCount,
    VkSparseImageMemoryRequirements2*           pSparseMemoryRequirements)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   ANV_FROM_HANDLE(anv_image, image, pInfo->image);

   if (!anv_sparse_residency_is_enabled(device)) {
      if ((device->physical->sparse_type == ANV_SPARSE_TYPE_NOT_SUPPORTED) &&
          INTEL_DEBUG(DEBUG_SPARSE))
         fprintf(stderr, "=== [%s:%d] [%s]\n", __FILE__, __LINE__, __func__);

      *pSparseMemoryRequirementCount = 0;
      return;
   }

   anv_image_get_sparse_memory_requirements(device, image, image->vk.aspects,
                                            pSparseMemoryRequirementCount,
                                            pSparseMemoryRequirements);
}

void anv_GetDeviceImageSparseMemoryRequirements(
    VkDevice                                    _device,
    const VkDeviceImageMemoryRequirements*      pInfo,
    uint32_t*                                   pSparseMemoryRequirementCount,
    VkSparseImageMemoryRequirements2*           pSparseMemoryRequirements)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   struct anv_image image = { 0 };

   if (!anv_sparse_residency_is_enabled(device)) {
      if ((device->physical->sparse_type == ANV_SPARSE_TYPE_NOT_SUPPORTED) &&
          INTEL_DEBUG(DEBUG_SPARSE))
         fprintf(stderr, "=== [%s:%d] [%s]\n", __FILE__, __LINE__, __func__);

      *pSparseMemoryRequirementCount = 0;
      return;
   }

   /* This function is similar to anv_GetDeviceImageMemoryRequirements, in
    * which it actually creates an image, gets the properties and then
    * destroys the image.
    *
    * We could one day refactor things to allow us to gather the properties
    * without having to actually create the image, maybe by reworking ISL to
    * separate creation from parameter computing.
    */
   VkResult result =
      anv_image_init_from_create_info(device, &image, pInfo->pCreateInfo,
                                      true /* no_private_binding_alloc */);
   if (result != VK_SUCCESS) {
      *pSparseMemoryRequirementCount = 0;
      return;
   }

   /* The spec says:
    *  "planeAspect is a VkImageAspectFlagBits value specifying the aspect
    *   corresponding to the image plane to query. This parameter is ignored
    *   unless pCreateInfo::tiling is VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT,
    *   or pCreateInfo::flags has VK_IMAGE_CREATE_DISJOINT_BIT set."
    */
   VkImageAspectFlags aspects =
      (pInfo->pCreateInfo->flags & VK_IMAGE_CREATE_DISJOINT_BIT) ||
      (pInfo->pCreateInfo->tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT)
         ? pInfo->planeAspect : image.vk.aspects;

   anv_image_get_sparse_memory_requirements(device, &image, aspects,
                                            pSparseMemoryRequirementCount,
                                            pSparseMemoryRequirements);

   anv_image_finish(&image);
}

static bool
anv_image_map_aux_tt(struct anv_device *device,
                     struct anv_image *image, uint32_t plane)
{
   const struct anv_address main_addr = anv_image_address(
      image, &image->planes[plane].primary_surface.memory_range);
   struct anv_bo *bo = main_addr.bo;
   assert(bo != NULL);

   /* If the additional memory padding was added at the end of the BO for CCS
    * data, map this region at the granularity of the main/CCS pages.
    *
    * Otherwise the image should have additional CCS data at the computed
    * offset.
    */
   if (device->physical->alloc_aux_tt_mem &&
       (bo->alloc_flags & ANV_BO_ALLOC_AUX_CCS)) {
      uint64_t main_aux_alignment =
         intel_aux_map_get_alignment(device->aux_map_ctx);
      assert(bo->offset % main_aux_alignment == 0);
      const struct anv_address start_addr = (struct anv_address) {
         .bo = bo,
         .offset = ROUND_DOWN_TO(main_addr.offset, main_aux_alignment),
      };
      const struct anv_address aux_addr = (struct anv_address) {
         .bo = bo,
         .offset = bo->ccs_offset +
                   intel_aux_main_to_aux_offset(device->aux_map_ctx,
                                                start_addr.offset),
      };
      const struct isl_surf *surf = &image->planes[plane].primary_surface.isl;
      const uint64_t format_bits =
         intel_aux_map_format_bits_for_isl_surf(surf);
      /* Make sure to have the mapping cover the entire image from the aux
       * aligned start.
       */
      const uint64_t main_size = align(
         (main_addr.offset - start_addr.offset) + surf->size_B,
         main_aux_alignment);

      if (intel_aux_map_add_mapping(device->aux_map_ctx,
                                    anv_address_physical(start_addr),
                                    anv_address_physical(aux_addr),
                                    main_size, format_bits)) {
         image->planes[plane].aux_tt.mapped = true;
         image->planes[plane].aux_tt.addr = anv_address_physical(start_addr);
         image->planes[plane].aux_tt.size = main_size;
         return true;
      }
   } else {
      if (anv_address_allows_aux_map(device, main_addr)) {
         const struct anv_address aux_addr =
            anv_image_address(image,
                              &image->planes[plane].compr_ctrl_memory_range);
         const struct isl_surf *surf =
            &image->planes[plane].primary_surface.isl;
         const uint64_t format_bits =
            intel_aux_map_format_bits_for_isl_surf(surf);
         if (intel_aux_map_add_mapping(device->aux_map_ctx,
                                       anv_address_physical(main_addr),
                                       anv_address_physical(aux_addr),
                                       surf->size_B, format_bits)) {
            image->planes[plane].aux_tt.mapped = true;
            image->planes[plane].aux_tt.addr = anv_address_physical(main_addr);
            image->planes[plane].aux_tt.size = surf->size_B;
            return true;
         }
      }
   }

   return false;

}

static VkResult
anv_image_bind_address(struct anv_device *device,
                        struct anv_image *image,
                        enum anv_image_memory_binding binding,
                        struct anv_address address)
{
   image->bindings[binding].address = address;

   /* Map bindings for images with host transfer usage, so that we don't have
    * to map/unmap things at every host operation. We map cached, that means
    * that the copy operations need to cflush on platforms that have no
    * host_cache+host_coherent memory types.
    */
   if (image->vk.usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT) {
      uint64_t offset = image->bindings[binding].address.offset +
                        image->bindings[binding].memory_range.offset;
      uint64_t map_offset, map_size;
      anv_sanitize_map_params(device, image->bindings[binding].address.bo, offset,
                              image->bindings[binding].memory_range.size,
                              &map_offset, &map_size);

      VkResult result = anv_device_map_bo(device,
                                          image->bindings[binding].address.bo,
                                          map_offset, map_size,
                                          NULL /* placed_addr */,
                                          &image->bindings[binding].host_map);
      if (result != VK_SUCCESS)
         return result;

      image->bindings[binding].map_delta = (offset - map_offset);
      image->bindings[binding].map_size = map_size;
   }

   ANV_RMV(image_bind, device, image, binding);

   return VK_SUCCESS;
}

static VkResult
anv_bind_image_memory(struct anv_device *device,
                      const VkBindImageMemoryInfo *bind_info)
{
   ANV_FROM_HANDLE(anv_device_memory, mem, bind_info->memory);
   ANV_FROM_HANDLE(anv_image, image, bind_info->image);
   bool did_bind = false;
   VkResult result = VK_SUCCESS;

   const VkBindMemoryStatusKHR *bind_status =
      vk_find_struct_const(bind_info->pNext, BIND_MEMORY_STATUS_KHR);

   assert(!anv_image_is_sparse(image));

   /* Resolve will alter the image's aspects, do this first. */
   if (mem && mem->vk.ahardware_buffer)
      resolve_ahw_image(device, image, mem);

   vk_foreach_struct_const(s, bind_info->pNext) {
      switch (s->sType) {
      case VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO: {
         const VkBindImagePlaneMemoryInfo *plane_info =
            (const VkBindImagePlaneMemoryInfo *) s;

         /* Workaround for possible spec bug.
          *
          * Unlike VkImagePlaneMemoryRequirementsInfo, which requires that
          * the image be disjoint (that is, multi-planar format and
          * VK_IMAGE_CREATE_DISJOINT_BIT), VkBindImagePlaneMemoryInfo allows
          * the image to be non-disjoint and requires only that the image
          * have the DISJOINT flag. In this case, regardless of the value of
          * VkImagePlaneMemoryRequirementsInfo::planeAspect, the behavior is
          * the same as if VkImagePlaneMemoryRequirementsInfo were omitted.
          */
         if (!image->disjoint)
            break;

         enum anv_image_memory_binding binding =
            anv_image_aspect_to_binding(image, plane_info->planeAspect);

         anv_image_bind_address(device, image, binding,
                                (struct anv_address) {
                                   .bo = mem->bo,
                                   .offset = bind_info->memoryOffset,
                                });

         did_bind = true;
         break;
      }
      case VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR: {
         /* Ignore this struct on Android, we cannot access swapchain
          * structures there.
          */
#ifndef VK_USE_PLATFORM_ANDROID_KHR
         const VkBindImageMemorySwapchainInfoKHR *swapchain_info =
            (const VkBindImageMemorySwapchainInfoKHR *) s;
         mem = anv_device_memory_from_handle(wsi_common_get_memory(
            swapchain_info->swapchain, swapchain_info->imageIndex));
         struct anv_image *swapchain_image = mem->dedicated_image;
         assert(swapchain_image);
         assert(image->vk.aspects == swapchain_image->vk.aspects);

         /* Remove the internally allocated private binding since we're going
          * to replace everything with BOs from the WSI image, we don't want
          * to leak the current BO.
          */
         struct anv_bo *private_bo =
            image->bindings[ANV_IMAGE_MEMORY_BINDING_PRIVATE].address.bo;
         if (private_bo) {
            assert(image->bindings[ANV_IMAGE_MEMORY_BINDING_PRIVATE].memory_range.size);
            assert(!image->bindings[ANV_IMAGE_MEMORY_BINDING_PRIVATE].host_map);

            ANV_DMR_BO_FREE(&image->vk.base, private_bo);
            anv_device_release_bo(device, private_bo);
            image->bindings[ANV_IMAGE_MEMORY_BINDING_PRIVATE].address.bo = NULL;
         }

         for (int j = 0; j < ARRAY_SIZE(image->bindings); ++j) {
            assert(memory_ranges_equal(image->bindings[j].memory_range,
                                       swapchain_image->bindings[j].memory_range));
            if (image->bindings[j].memory_range.size != 0) {
               anv_image_bind_address(device, image, j,
                                      swapchain_image->bindings[j].address);
            }
         }

         /* We must bump the private binding's bo's refcount because, unlike the other
          * bindings, its lifetime is not application-managed.
          */
         private_bo = image->bindings[ANV_IMAGE_MEMORY_BINDING_PRIVATE].address.bo;
         if (private_bo)
            anv_bo_ref(private_bo);

         did_bind = true;
#endif
         break;
      }
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch"
      case VK_STRUCTURE_TYPE_NATIVE_BUFFER_ANDROID: {
         const VkNativeBufferANDROID *gralloc_info =
            (const VkNativeBufferANDROID *)s;

         result = resolve_anb_image(device, image, gralloc_info);
         if (result != VK_SUCCESS)
            return result;

         did_bind = true;
         break;
      }
#pragma GCC diagnostic pop
      default:
         vk_debug_ignored_stype(s->sType);
         break;
      }
   }

   if (!did_bind) {
      assert(!image->disjoint);
      result = anv_image_bind_address(device, image,
                                      ANV_IMAGE_MEMORY_BINDING_MAIN,
                                      (struct anv_address) {
                                         .bo = mem->bo,
                                         .offset = bind_info->memoryOffset,
                                      });
      did_bind = true;
   }

   /* Now that we have the BO, finalize CCS setup. */
   for (int p = 0; p < image->n_planes; ++p) {
      enum anv_image_memory_binding binding =
         image->planes[p].primary_surface.memory_range.binding;
      const struct anv_bo *bo =
         image->bindings[binding].address.bo;

      if (!bo || !isl_aux_usage_has_ccs(image->planes[p].aux_usage))
         continue;

      /* Do nothing if flat CCS requirements are satisfied.
       *
       * Also, assume that imported BOs with a modifier including
       * CCS live only in local memory. Otherwise the exporter should
       * have failed the creation of the BO.
       */
      if (device->info->has_flat_ccs &&
          (anv_bo_is_vram_only(bo) ||
           (bo->alloc_flags & ANV_BO_ALLOC_COMPRESSED) ||
           (bo->alloc_flags & ANV_BO_ALLOC_IMPORTED)))
         continue;

      /* If the AUX-TT mapping succeeds, there is nothing else to do. */
      if (device->info->has_aux_map && anv_image_map_aux_tt(device, image, p))
         continue;

      /* No special requirements on gfx9-11. */
      if (device->info->ver <= 11)
         continue;

      /* The plane's BO cannot support CCS, disable compression on it. */
      assert(!isl_drm_modifier_has_aux(image->vk.drm_format_mod));

      anv_perf_warn(VK_LOG_OBJS(&image->vk.base),
                    "BO lacks CCS support. Disabling the CCS aux usage.");

      if (image->planes[p].aux_usage == ISL_AUX_USAGE_MCS_CCS) {
         assert(image->planes[p].aux_surface.memory_range.size);
         image->planes[p].aux_usage = ISL_AUX_USAGE_MCS;
      } else if (image->planes[p].aux_usage == ISL_AUX_USAGE_HIZ_CCS ||
                 image->planes[p].aux_usage == ISL_AUX_USAGE_HIZ_CCS_WT) {
         assert(image->planes[p].aux_surface.memory_range.size);
         image->planes[p].aux_usage = ISL_AUX_USAGE_HIZ;
      } else {
         assert(image->planes[p].aux_usage == ISL_AUX_USAGE_CCS_E ||
                image->planes[p].aux_usage == ISL_AUX_USAGE_FCV_CCS_E ||
                image->planes[p].aux_usage == ISL_AUX_USAGE_STC_CCS);
         image->planes[p].aux_usage = ISL_AUX_USAGE_NONE;
      }
   }

   if (image->bindings[ANV_IMAGE_MEMORY_BINDING_PRIVATE].address.bo != NULL &&
       !image->device_registered) {
      pthread_mutex_lock(&device->mutex);
      list_addtail(&image->link, &device->image_private_objects);
      image->device_registered = true;
      pthread_mutex_unlock(&device->mutex);
   }

   if (bind_status)
      *bind_status->pResult = result;

   return result;
}

VkResult anv_BindImageMemory2(
    VkDevice                                    _device,
    uint32_t                                    bindInfoCount,
    const VkBindImageMemoryInfo*                pBindInfos)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   VkResult result = VK_SUCCESS;

   for (uint32_t i = 0; i < bindInfoCount; i++) {
      VkResult res = anv_bind_image_memory(device, &pBindInfos[i]);
      if (result == VK_SUCCESS && res != VK_SUCCESS)
         result = res;
   }

   return result;
}

static void
anv_get_image_subresource_layout(struct anv_device *device,
                                 const struct anv_image *image,
                                 const VkImageSubresource2KHR *subresource,
                                 VkSubresourceLayout2KHR *layout)
{
   const struct isl_surf *isl_surf = NULL;
   const struct anv_image_memory_range *mem_range;
   uint64_t row_pitch_B;

   assert(__builtin_popcount(subresource->imageSubresource.aspectMask) == 1);

   /* The Vulkan spec requires that aspectMask be
    * VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT if tiling is
    * VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT.
    *
    * For swapchain images, the Vulkan spec says that every swapchain image has
    * tiling VK_IMAGE_TILING_OPTIMAL, but we may choose
    * VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT internally.  Vulkan doesn't allow
    * vkGetImageSubresourceLayout for images with VK_IMAGE_TILING_OPTIMAL,
    * therefore it's invalid for the application to call this on a swapchain
    * image.  The WSI code, however, knows when it has internally created
    * a swapchain image with VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT,
    * so it _should_ correctly use VK_IMAGE_ASPECT_MEMORY_PLANE_* in that case.
    * But it incorrectly uses VK_IMAGE_ASPECT_PLANE_*, so we have a temporary
    * workaround.
    *
    * https://gitlab.freedesktop.org/mesa/mesa/-/issues/10176
    */
   if (image->vk.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
      /* TODO(chadv): Drop this workaround when WSI gets fixed. */
      uint32_t mem_plane;
      switch (subresource->imageSubresource.aspectMask) {
      case VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT:
      case VK_IMAGE_ASPECT_PLANE_0_BIT:
         mem_plane = 0;
         break;
      case VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT:
      case VK_IMAGE_ASPECT_PLANE_1_BIT:
         mem_plane = 1;
         break;
      case VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT:
      case VK_IMAGE_ASPECT_PLANE_2_BIT:
         mem_plane = 2;
         break;
      default:
         UNREACHABLE("bad VkImageAspectFlags");
      }

      if (isl_drm_modifier_plane_is_clear_color(image->vk.drm_format_mod,
                                                mem_plane)) {
         assert(image->n_planes == 1);

         mem_range = &image->planes[0].fast_clear_memory_range;
         row_pitch_B = ISL_DRM_CC_PLANE_PITCH_B;
      } else if (mem_plane == 1 &&
                 image->planes[0].compr_ctrl_memory_range.size > 0) {
         assert(image->n_planes == 1);
         assert(isl_drm_modifier_has_aux(image->vk.drm_format_mod));

         mem_range = &image->planes[0].compr_ctrl_memory_range;
         row_pitch_B = image->planes[0].primary_surface.isl.row_pitch_B /
                       INTEL_AUX_MAP_MAIN_PITCH_SCALEDOWN;
      } else if (mem_plane == 1 &&
                 image->planes[0].aux_surface.memory_range.size > 0) {
         assert(image->n_planes == 1);
         assert(image->vk.drm_format_mod == I915_FORMAT_MOD_Y_TILED_CCS);

         mem_range = &image->planes[0].aux_surface.memory_range;
         row_pitch_B = image->planes[0].aux_surface.isl.row_pitch_B;
      } else {
         assert(mem_plane < image->n_planes);

         mem_range = &image->planes[mem_plane].primary_surface.memory_range;
         row_pitch_B =
            image->planes[mem_plane].primary_surface.isl.row_pitch_B;

         isl_surf = &image->planes[mem_plane].primary_surface.isl;
      }

      /* If the memory binding differs between the primary plane and the
       * specified memory plane, the returned offset will be incorrect.
       */
      assert(mem_range->binding ==
             image->planes[0].primary_surface.memory_range.binding);

      /* We are working with a non-arrayed 2D image. */
      assert(image->vk.image_type == VK_IMAGE_TYPE_2D || image->vk.drm_format_mod == DRM_FORMAT_MOD_LINEAR);
      assert(image->vk.array_layers == 1 || image->vk.drm_format_mod == DRM_FORMAT_MOD_LINEAR);
   } else {
      const uint32_t plane =
         anv_image_aspect_to_plane(image,
                                   subresource->imageSubresource.aspectMask);
      isl_surf = &image->planes[plane].primary_surface.isl;
      mem_range = &image->planes[plane].primary_surface.memory_range;
      row_pitch_B = isl_surf->row_pitch_B;
   }

   const uint32_t level = subresource->imageSubresource.mipLevel;
   bool subresource_has_unique_tiles = false;
   if (isl_surf) {
      /* ISL tries to give us a single layer but the Vulkan API expect the
       * entire 3D size.
       */
      const uint32_t layer = subresource->imageSubresource.arrayLayer;
      const uint32_t layers = u_minify(isl_surf->logical_level0_px.d, level);
      uint64_t start_tile_B, end_tile_B;
      subresource_has_unique_tiles =
         isl_surf_image_has_unique_tiles(isl_surf, level, layer, layers,
                                         &start_tile_B, &end_tile_B);
      layout->subresourceLayout.offset = mem_range->offset + start_tile_B;
      layout->subresourceLayout.size = end_tile_B - start_tile_B;
      layout->subresourceLayout.rowPitch = row_pitch_B;
      layout->subresourceLayout.depthPitch =
         isl_surf_get_array_pitch(isl_surf);
      layout->subresourceLayout.arrayPitch =
         isl_surf_get_array_pitch(isl_surf);
   } else {
      layout->subresourceLayout.offset = mem_range->offset;
      layout->subresourceLayout.size = mem_range->size;
      layout->subresourceLayout.rowPitch = row_pitch_B;
      /* Not a surface so those fields don't make sense */
      layout->subresourceLayout.depthPitch = 0;
      layout->subresourceLayout.arrayPitch = 0;
   }

   VkSubresourceHostMemcpySizeEXT *host_memcpy_size =
      vk_find_struct(layout->pNext, SUBRESOURCE_HOST_MEMCPY_SIZE_EXT);
   if (host_memcpy_size) {
      if (!isl_surf) {
         host_memcpy_size->size = 0;
      } else if (subresource_has_unique_tiles) {
         host_memcpy_size->size = layout->subresourceLayout.size;
      } else {
         /* If we cannot do straight memcpy of the image, compute a linear
          * size. This will be the format in which we store the data.
          */
         struct isl_surf lin_surf;
         bool ok =
            isl_surf_init(&device->physical->isl_dev, &lin_surf,
                          .dim          = isl_surf->dim,
                          .format       = isl_surf->format,
                          .width        = u_minify(
                             isl_surf->logical_level0_px.w, level),
                          .height       = u_minify(
                             isl_surf->logical_level0_px.h, level),
                          .depth        = u_minify(
                             isl_surf->logical_level0_px.d, level),
                          .array_len    = 1,
                          .levels       = 1,
                          .samples      = isl_surf->samples,
                          .tiling_flags = ISL_TILING_LINEAR_BIT);
         assert(ok);
         host_memcpy_size->size = lin_surf.size_B;
      }
   }

   VkImageCompressionPropertiesEXT *comp_props =
      vk_find_struct(layout->pNext, IMAGE_COMPRESSION_PROPERTIES_EXT);
   if (comp_props) {
      comp_props->imageCompressionFixedRateFlags =
         VK_IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT;
      comp_props->imageCompressionFlags = VK_IMAGE_COMPRESSION_DISABLED_EXT;
      for (uint32_t p = 0; p < image->n_planes; p++) {
         if (image->planes[p].aux_usage != ISL_AUX_USAGE_NONE) {
            comp_props->imageCompressionFlags = VK_IMAGE_COMPRESSION_DEFAULT_EXT;
            break;
         }
      }
   }
}

void anv_GetDeviceImageSubresourceLayoutKHR(
    VkDevice                                    _device,
    const VkDeviceImageSubresourceInfoKHR*      pInfo,
    VkSubresourceLayout2KHR*                    pLayout)
{
   ANV_FROM_HANDLE(anv_device, device, _device);

   struct anv_image image = { 0 };

   if (anv_image_init_from_create_info(device, &image, pInfo->pCreateInfo,
                                       true) != VK_SUCCESS) {
      pLayout->subresourceLayout = (VkSubresourceLayout) { 0, };
      return;
   }

   anv_get_image_subresource_layout(device, &image, pInfo->pSubresource, pLayout);
}

void anv_GetImageSubresourceLayout2KHR(
    VkDevice                                    _device,
    VkImage                                     _image,
    const VkImageSubresource2KHR*               pSubresource,
    VkSubresourceLayout2KHR*                    pLayout)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   ANV_FROM_HANDLE(anv_image, image, _image);

   anv_get_image_subresource_layout(device, image, pSubresource, pLayout);
}

static VkImageUsageFlags
anv_image_flags_filter_for_queue(VkImageUsageFlags usages,
                                 VkQueueFlagBits queue_flags)
{
   /* Eliminate graphics usages if the queue is not graphics capable */
   if (!(queue_flags & VK_QUEUE_GRAPHICS_BIT)) {
      usages &= ~(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT |
                  VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT |
                  VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT |
                  VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT |
                  VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT |
                  VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR |
                  VK_IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT);
   }

   /* Eliminate sampling & storage usages if the queue is neither graphics nor
    * compute capable
    */
   if (!(queue_flags & (VK_QUEUE_GRAPHICS_BIT | VK_QUEUE_COMPUTE_BIT))) {
      usages &= ~(VK_IMAGE_USAGE_SAMPLED_BIT |
                  VK_IMAGE_USAGE_STORAGE_BIT);
   }

   /* Eliminate transfer usages if the queue is neither transfer, compute or
    * graphics capable
    */
   if (!(queue_flags & (VK_QUEUE_TRANSFER_BIT |
                        VK_QUEUE_COMPUTE_BIT |
                        VK_QUEUE_GRAPHICS_BIT))) {
      usages &= ~(VK_IMAGE_USAGE_TRANSFER_SRC_BIT |
                  VK_IMAGE_USAGE_TRANSFER_DST_BIT);
   }

   return usages;
}

/**
 * This function returns the assumed isl_aux_state for a given VkImageLayout.
 * Because Vulkan image layouts don't map directly to isl_aux_state enums, the
 * returned enum is the assumed worst case.
 *
 * @param devinfo The device information of the Intel GPU.
 * @param image The image that may contain a collection of buffers.
 * @param aspect The aspect of the image to be accessed.
 * @param layout The current layout of the image aspect(s).
 *
 * @return The primary buffer that should be used for the given layout.
 */
enum isl_aux_state ATTRIBUTE_PURE
anv_layout_to_aux_state(const struct intel_device_info * const devinfo,
                        const struct anv_image * const image,
                        const VkImageAspectFlagBits aspect,
                        const VkImageLayout layout,
                        const VkQueueFlagBits queue_flags)
{
   /* Validate the inputs. */

   /* The devinfo is needed as the optimal buffer varies across generations. */
   assert(devinfo != NULL);

   /* The layout of a NULL image is not properly defined. */
   assert(image != NULL);

   /* The aspect must be exactly one of the image aspects. */
   assert(util_bitcount(aspect) == 1 && (aspect & image->vk.aspects));

   /* Determine the optimal buffer. */

   const uint32_t plane = anv_image_aspect_to_plane(image, aspect);

   /* If we don't have an aux buffer then aux state makes no sense */
   const enum isl_aux_usage aux_usage = image->planes[plane].aux_usage;
   assert(aux_usage != ISL_AUX_USAGE_NONE);

   /* Handle a few special cases */
   switch (layout) {
   /* Invalid layouts */
   case VK_IMAGE_LAYOUT_MAX_ENUM:
      UNREACHABLE("Invalid image layout.");

   /* Undefined layouts
    *
    * The pre-initialized layout is equivalent to the undefined layout for
    * optimally-tiled images and for images not bound to host-visible memory.
    * We only do compression on images that have one or both properties.
    */
   case VK_IMAGE_LAYOUT_UNDEFINED:
   case VK_IMAGE_LAYOUT_PREINITIALIZED:
      return ISL_AUX_STATE_AUX_INVALID;

   case VK_IMAGE_LAYOUT_PRESENT_SRC_KHR: {
      assert(image->vk.aspects == VK_IMAGE_ASPECT_COLOR_BIT);

      /* Handle transition to present layout for non wsi images just like
       * normal images. Some apps like gfx-reconstruct incorrectly use this
       * layout on non-wsi image which is against spec. It's easy enough to
       * deal with it here and potentially avoid unnecessary resolve
       * operations.
       */
      if (!image->from_wsi)
         break;

      enum isl_aux_state aux_state =
         isl_drm_modifier_get_default_aux_state(image->vk.drm_format_mod);

      switch (aux_state) {
      case ISL_AUX_STATE_AUX_INVALID:
         /* The modifier does not support compression. But, if we arrived
          * here, then we have enabled compression on it anyway. If this is a
          * WSI blit source, keep compression as we can do a compressed to
          * uncompressed copy.
          */
         if (image->wsi_blit_src)
            return ISL_AUX_STATE_COMPRESSED_CLEAR;

         /* If this is not a WSI blit source, we must resolve the aux surface
          * before we release ownership to the presentation engine (because,
          * having no modifier, the presentation engine will not be aware of
          * the aux surface). The presentation engine will not access the aux
          * surface (because it is unware of it), and so the aux surface will
          * still be resolved when we re-acquire ownership.
          *
          * Therefore, at ownership transfers in either direction, there does
          * exist an aux surface despite the lack of modifier and its state is
          * pass-through.
          */
         return ISL_AUX_STATE_PASS_THROUGH;
      case ISL_AUX_STATE_COMPRESSED_CLEAR:
         return ISL_AUX_STATE_COMPRESSED_CLEAR;
      case ISL_AUX_STATE_COMPRESSED_NO_CLEAR:
         return ISL_AUX_STATE_COMPRESSED_NO_CLEAR;
      default:
         UNREACHABLE("unexpected isl_aux_state");
      }
   }

   default:
      break;
   }

   const bool read_only = vk_image_layout_is_read_only(layout, aspect);

   const VkImageUsageFlags image_aspect_usage =
      anv_image_flags_filter_for_queue(
         vk_image_usage(&image->vk, aspect), queue_flags);
   const VkImageUsageFlags usage =
      vk_image_layout_to_usage_flags(layout, aspect) & image_aspect_usage;

   bool aux_supported = true;
   bool clear_supported = isl_aux_usage_has_fast_clears(aux_usage);

   if ((usage & (VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT |
                 VK_IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT)) &&
       !read_only) {
      /* This image could be used as both an input attachment and a render
       * target (depth, stencil, or color) at the same time and this can cause
       * corruption.
       *
       * We currently only disable aux in this way for depth even though we
       * disable it for color in GL.
       *
       * TODO: Should we be disabling this in more cases?
       */
      if (aspect == VK_IMAGE_ASPECT_DEPTH_BIT && devinfo->ver <= 9) {
         aux_supported = false;
         clear_supported = false;
      }
   }

   if (usage & (VK_IMAGE_USAGE_TRANSFER_SRC_BIT |
                VK_IMAGE_USAGE_SAMPLED_BIT |
                VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)) {
      switch (aux_usage) {
      case ISL_AUX_USAGE_HIZ:
         if (!anv_can_sample_with_hiz(devinfo, image)) {
            aux_supported = false;
            clear_supported = false;
         }
         break;

      case ISL_AUX_USAGE_HIZ_CCS:
         aux_supported = false;
         clear_supported = false;
         break;

      case ISL_AUX_USAGE_HIZ_CCS_WT:
         break;

      case ISL_AUX_USAGE_CCS_D:
         aux_supported = false;
         clear_supported = false;
         break;

      case ISL_AUX_USAGE_MCS:
      case ISL_AUX_USAGE_MCS_CCS:
         if (!anv_can_sample_mcs_with_clear(devinfo, image))
            clear_supported = false;
         break;

      case ISL_AUX_USAGE_CCS_E:
      case ISL_AUX_USAGE_FCV_CCS_E:
      case ISL_AUX_USAGE_STC_CCS:
         break;

      default:
         UNREACHABLE("Unsupported aux usage");
      }
   }

   switch (aux_usage) {
   case ISL_AUX_USAGE_HIZ:
   case ISL_AUX_USAGE_HIZ_CCS:
   case ISL_AUX_USAGE_HIZ_CCS_WT:
      if (aux_supported) {
         assert(clear_supported);
         return ISL_AUX_STATE_COMPRESSED_CLEAR;
      } else if (read_only) {
         return ISL_AUX_STATE_RESOLVED;
      } else {
         return ISL_AUX_STATE_AUX_INVALID;
      }

   case ISL_AUX_USAGE_CCS_D:
      /* We only support clear in exactly one state */
      if (layout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL ||
          layout == VK_IMAGE_LAYOUT_ATTACHMENT_OPTIMAL) {
         assert(aux_supported);
         assert(clear_supported);
         return ISL_AUX_STATE_PARTIAL_CLEAR;
      } else {
         return ISL_AUX_STATE_PASS_THROUGH;
      }

   case ISL_AUX_USAGE_CCS_E:
   case ISL_AUX_USAGE_FCV_CCS_E:
      if (aux_supported) {
         assert(clear_supported);
         return ISL_AUX_STATE_COMPRESSED_CLEAR;
      } else {
         return ISL_AUX_STATE_PASS_THROUGH;
      }

   case ISL_AUX_USAGE_MCS:
   case ISL_AUX_USAGE_MCS_CCS:
      assert(aux_supported);
      if (clear_supported) {
         return ISL_AUX_STATE_COMPRESSED_CLEAR;
      } else {
         return ISL_AUX_STATE_COMPRESSED_NO_CLEAR;
      }

   case ISL_AUX_USAGE_STC_CCS:
      assert(aux_supported);
      assert(!clear_supported);
      return ISL_AUX_STATE_COMPRESSED_NO_CLEAR;

   default:
      UNREACHABLE("Unsupported aux usage");
   }
}

/**
 * This function determines the optimal buffer to use for a given
 * VkImageLayout and other pieces of information needed to make that
 * determination. This does not determine the optimal buffer to use
 * during a resolve operation.
 *
 * @param devinfo The device information of the Intel GPU.
 * @param image The image that may contain a collection of buffers.
 * @param aspect The aspect of the image to be accessed.
 * @param usage The usage which describes how the image will be accessed.
 * @param layout The current layout of the image aspect(s).
 *
 * @return The primary buffer that should be used for the given layout.
 */
enum isl_aux_usage ATTRIBUTE_PURE
anv_layout_to_aux_usage(const struct intel_device_info * const devinfo,
                        const struct anv_image * const image,
                        const VkImageAspectFlagBits aspect,
                        const VkImageUsageFlagBits usage,
                        const VkImageLayout layout,
                        const VkQueueFlagBits queue_flags)
{
   const uint32_t plane = anv_image_aspect_to_plane(image, aspect);

   /* If there is no auxiliary surface allocated, we must use the one and only
    * main buffer.
    */
   if (image->planes[plane].aux_usage == ISL_AUX_USAGE_NONE)
      return ISL_AUX_USAGE_NONE;

   enum isl_aux_state aux_state =
      anv_layout_to_aux_state(devinfo, image, aspect, layout, queue_flags);

   switch (aux_state) {
   case ISL_AUX_STATE_CLEAR:
      UNREACHABLE("We never use this state");

   case ISL_AUX_STATE_PARTIAL_CLEAR:
      assert(image->vk.aspects & VK_IMAGE_ASPECT_ANY_COLOR_BIT_ANV);
      assert(image->planes[plane].aux_usage == ISL_AUX_USAGE_CCS_D);
      assert(image->vk.samples == 1);
      return ISL_AUX_USAGE_CCS_D;

   case ISL_AUX_STATE_COMPRESSED_CLEAR:
   case ISL_AUX_STATE_COMPRESSED_NO_CLEAR:
      return image->planes[plane].aux_usage;

   case ISL_AUX_STATE_RESOLVED:
      /* We can only use RESOLVED in read-only layouts because any write will
       * either land us in AUX_INVALID or COMPRESSED_NO_CLEAR.  We can do
       * writes in PASS_THROUGH without destroying it so that is allowed.
       */
      assert(vk_image_layout_is_read_only(layout, aspect));
      assert(util_is_power_of_two_or_zero(usage));
      if (usage == VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) {
         /* If we have valid HiZ data and are using the image as a read-only
          * depth/stencil attachment, we should enable HiZ so that we can get
          * faster depth testing.
          */
         return image->planes[plane].aux_usage;
      } else {
         return ISL_AUX_USAGE_NONE;
      }

   case ISL_AUX_STATE_PASS_THROUGH:
   case ISL_AUX_STATE_AUX_INVALID:
      return ISL_AUX_USAGE_NONE;
   }

   UNREACHABLE("Invalid isl_aux_state");
}

/**
 * This function returns the level of unresolved fast-clear support of the
 * given image in the given VkImageLayout.
 *
 * @param devinfo The device information of the Intel GPU.
 * @param image The image that may contain a collection of buffers.
 * @param aspect The aspect of the image to be accessed.
 * @param usage The usage which describes how the image will be accessed.
 * @param layout The current layout of the image aspect(s).
 */
enum anv_fast_clear_type ATTRIBUTE_PURE
anv_layout_to_fast_clear_type(const struct intel_device_info * const devinfo,
                              const struct anv_image * const image,
                              const VkImageAspectFlagBits aspect,
                              const VkImageLayout layout,
                              const VkQueueFlagBits queue_flags)
{
   if (INTEL_DEBUG(DEBUG_NO_FAST_CLEAR))
      return ANV_FAST_CLEAR_NONE;

   const uint32_t plane = anv_image_aspect_to_plane(image, aspect);

   /* If there is no auxiliary surface allocated, there are no fast-clears */
   if (image->planes[plane].aux_usage == ISL_AUX_USAGE_NONE)
      return ANV_FAST_CLEAR_NONE;

   /* Bspec 57340 (r68483) has no fast-clear rectangle for linear surfaces. */
   if (image->planes[plane].primary_surface.isl.tiling == ISL_TILING_LINEAR) {
      assert(devinfo->ver >= 20);
      return ANV_FAST_CLEAR_NONE;
   }

   /* Xe2+ platforms don't have fast clear type and can always support
    * arbitrary fast-clear values.
    */
   if (devinfo->ver >= 20)
      return ANV_FAST_CLEAR_ANY;

   enum isl_aux_state aux_state =
      anv_layout_to_aux_state(devinfo, image, aspect, layout, queue_flags);

   const VkImageUsageFlags layout_usage =
      vk_image_layout_to_usage_flags(layout, aspect) & image->vk.usage;

   const struct isl_drm_modifier_info *isl_mod_info =
      image->vk.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT ?
      isl_drm_modifier_get_info(image->vk.drm_format_mod) : NULL;

   switch (aux_state) {
   case ISL_AUX_STATE_CLEAR:
      UNREACHABLE("We never use this state");

   case ISL_AUX_STATE_PARTIAL_CLEAR:
   case ISL_AUX_STATE_COMPRESSED_CLEAR:

      /* Generally, enabling non-zero fast-clears is dependent on knowing which
       * formats will be used with the surface. So, disable them if we lack
       * this knowledge.
       *
       * For dmabufs with clear color modifiers, we already restrict
       * problematic accesses for the clear color during the negotiation
       * phase. So, don't restrict clear color support in this case.
       */
      if (anv_image_view_formats_incomplete(image) &&
          !(isl_mod_info && isl_mod_info->supports_clear_color)) {
         return ANV_FAST_CLEAR_DEFAULT_VALUE;
      }

      /* On gfx12, the FCV feature may convert a block of fragment shader
       * outputs to fast-clears. If this image has multiple subresources,
       * restrict the clear color to zero to keep the fast cleared blocks in
       * sync.
       */
      if (image->planes[plane].aux_usage == ISL_AUX_USAGE_FCV_CCS_E &&
          (image->vk.mip_levels > 1 ||
           image->vk.array_layers > 1 ||
           image->vk.extent.depth > 1)) {
         return ANV_FAST_CLEAR_DEFAULT_VALUE;
      }

      /* On gfx9, we only load clear colors for attachments and for BLORP
       * surfaces. Outside of those surfaces, we can only support the default
       * clear value of zero.
       */
      if (devinfo->ver == 9 &&
          (layout_usage & (VK_IMAGE_USAGE_SAMPLED_BIT |
                           VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT))) {
         return ANV_FAST_CLEAR_DEFAULT_VALUE;
      }

      return ANV_FAST_CLEAR_ANY;

   case ISL_AUX_STATE_COMPRESSED_NO_CLEAR:
   case ISL_AUX_STATE_RESOLVED:
   case ISL_AUX_STATE_PASS_THROUGH:
   case ISL_AUX_STATE_AUX_INVALID:
      return ANV_FAST_CLEAR_NONE;
   }

   UNREACHABLE("Invalid isl_aux_state");
}

bool
anv_can_fast_clear_color(const struct anv_cmd_buffer *cmd_buffer,
                         const struct anv_image *image,
                         VkImageAspectFlags clear_aspect,
                         unsigned level,
                         const struct VkClearRect *clear_rect,
                         VkImageLayout layout,
                         enum isl_format view_format,
                         union isl_color_value clear_color)
{
   if (INTEL_DEBUG(DEBUG_NO_FAST_CLEAR))
      return false;

   /* We only have fast-clears implemented for the render engine. */
   if (cmd_buffer->queue_family->engine_class != INTEL_ENGINE_CLASS_RENDER)
      return false;

   /* Start by getting the fast clear type.  We use the first subpass
    * layout here because we don't want to fast-clear if the first subpass
    * to use the attachment can't handle fast-clears.
    */
   enum anv_fast_clear_type fast_clear_type =
      anv_layout_to_fast_clear_type(cmd_buffer->device->info, image,
                                    clear_aspect, layout,
                                    cmd_buffer->queue_family->queueFlags);
   switch (fast_clear_type) {
   case ANV_FAST_CLEAR_NONE:
      return false;
   case ANV_FAST_CLEAR_DEFAULT_VALUE:
      if (!isl_color_value_is_zero(clear_color, view_format))
         return false;
      break;
   case ANV_FAST_CLEAR_ANY:
      break;
   }

   /* Potentially, we could do partial fast-clears but doing so has crazy
    * alignment restrictions.  It's easier to just restrict to full size
    * fast clears for now.
    */
   if (clear_rect->rect.offset.x != 0 ||
       clear_rect->rect.offset.y != 0 ||
       clear_rect->rect.extent.width != image->vk.extent.width ||
       clear_rect->rect.extent.height != image->vk.extent.height)
      return false;

   /* We only allow fast clears to the first slice of an image (level 0,
    * layer 0) and only for the entire slice.  This guarantees us that, at
    * any given time, there is only one clear color on any given image at
    * any given time.  At the time of our testing (Jan 17, 2018), there
    * were no known applications which would benefit from fast-clearing
    * more than just the first slice.
    */
   if (level > 0) {
      anv_perf_warn(VK_LOG_OBJS(&image->vk.base),
                    "level > 0.  Not fast clearing.");
      return false;
   }

   if (clear_rect->baseArrayLayer > 0) {
      anv_perf_warn(VK_LOG_OBJS(&image->vk.base),
                    "baseArrayLayer > 0.  Not fast clearing.");
      return false;
   }


   if (clear_rect->layerCount > 1) {
      anv_perf_warn(VK_LOG_OBJS(&image->vk.base),
                    "layerCount > 1.  Only fast-clearing the first slice");
   }

   /* Wa_18020603990 - slow clear surfaces up to 256x256, 32bpp. */
   if (intel_needs_workaround(cmd_buffer->device->info, 18020603990)) {
      const struct anv_surface *anv_surf = &image->planes->primary_surface;
      if (isl_format_get_layout(anv_surf->isl.format)->bpb <= 32 &&
          anv_surf->isl.logical_level0_px.w <= 256 &&
          anv_surf->isl.logical_level0_px.h <= 256)
         return false;
   }

   /* On gfx12.0, CCS fast clears don't seem to cover the correct portion of
    * the aux buffer when the pitch is not 512B-aligned.
    */
   if (cmd_buffer->device->info->verx10 == 120 &&
       image->planes->primary_surface.isl.samples == 1 &&
       image->planes->primary_surface.isl.row_pitch_B % 512) {
      anv_perf_warn(VK_LOG_OBJS(&image->vk.base),
                    "Pitch not 512B-aligned. Slow clearing surface.");
      return false;
   }

   /* Wa_16021232440, HSD_16023071695: Disable fast clear when height
    * or width is 16k
    * */
   if (intel_needs_workaround(cmd_buffer->device->info, 16021232440) &&
       (image->vk.extent.height == 16 * 1024 ||
        image->vk.extent.width == 16 * 1024)) {
      return false;
   }

   return true;
}

bool
anv_can_hiz_clear_image(struct anv_cmd_buffer *cmd_buffer,
                        const struct anv_image *image,
                        VkImageLayout layout,
                        VkImageAspectFlags clear_aspects,
                        float depth_clear_value,
                        VkRect2D render_area,
                        const unsigned level)
{
   const struct anv_device *device = cmd_buffer->device;
   const VkQueueFlagBits queue_flags = cmd_buffer->queue_family->queueFlags;

   if (INTEL_DEBUG(DEBUG_NO_FAST_CLEAR))
      return false;

   /* If we're just clearing stencil, we can always HiZ clear */
   if (!(clear_aspects & VK_IMAGE_ASPECT_DEPTH_BIT))
      return true;

   const enum isl_aux_usage clear_aux_usage =
      anv_layout_to_aux_usage(device->info, image,
                              VK_IMAGE_ASPECT_DEPTH_BIT, 0,
                              layout, queue_flags);

   const uint32_t plane =
      anv_image_aspect_to_plane(image, VK_IMAGE_ASPECT_DEPTH_BIT);
   const struct isl_surf *surf = &image->planes[plane].primary_surface.isl;

   if (!isl_aux_usage_has_fast_clears(clear_aux_usage))
      return false;

   if (isl_aux_usage_has_ccs(clear_aux_usage)) {
      /* From the TGL PRM, Vol 9, "Compressed Depth Buffers" (under the
       * "Texture performant" and "ZCS" columns):
       *
       *    Update with clear at either 16x8 or 8x4 granularity, based on
       *    fs_clr or otherwise.
       *
       * Although alignment requirements are only listed for the texture
       * performant mode, test results indicate that requirements exist for
       * the non-texture performant mode as well. Disable partial clears.
       */
      if (render_area.offset.x > 0 ||
          render_area.offset.y > 0 ||
          render_area.extent.width !=
          u_minify(image->vk.extent.width, level) ||
          render_area.extent.height !=
          u_minify(image->vk.extent.height, level)) {
         return false;
      }

      /* When fast-clearing, hardware behaves in unexpected ways if the clear
       * rectangle, aligned to 16x8, could cover neighboring LODs.
       * Fortunately, ISL guarantees that LOD0 will be 8-row aligned and
       * LOD0's height seems to not matter. Also, few applications ever clear
       * LOD1+. Only allow fast-clearing upper LODs if no overlap can occur.
       */
      assert(surf->dim_layout == ISL_DIM_LAYOUT_GFX4_2D);
      assert(surf->array_pitch_el_rows % 8 == 0);
      if (clear_aux_usage == ISL_AUX_USAGE_HIZ_CCS_WT &&
          level >= 1 &&
          (image->vk.extent.width % 32 != 0 ||
           surf->image_alignment_el.h % 8 != 0)) {
         return false;
      }
   }

   if (device->info->ver <= 12 &&
       depth_clear_value != anv_image_hiz_clear_value(image).f32[0])
     return false;

   /* If we got here, then we can fast clear */
   return true;
}

/**
 * This function determines if the layout & usage of an image can have
 * untracked aux writes. When we see a transition that matches this criteria,
 * we need to mark the image as compressed written so that our predicated
 * resolves work properly.
 *
 * @param devinfo The device information of the Intel GPU.
 * @param image The image that may contain a collection of buffers.
 * @param aspect The aspect of the image to be accessed.
 * @param layout The current layout of the image aspect(s).
 */
bool
anv_layout_has_untracked_aux_writes(const struct intel_device_info * const devinfo,
                                    const struct anv_image * const image,
                                    const VkImageAspectFlagBits aspect,
                                    const VkImageLayout layout,
                                    const VkQueueFlagBits queue_flags)
{
   const VkImageUsageFlags image_aspect_usage =
      vk_image_usage(&image->vk, aspect);
   const VkImageUsageFlags usage =
      vk_image_layout_to_usage_flags(layout, aspect) & image_aspect_usage;

   /* Storage is the only usage where we do not write the image through a
    * render target but through a descriptor. Since VK_EXT_descriptor_indexing
    * and the update-after-bind feature, it has become impossible to track
    * writes to images in descriptor at the command buffer build time. So it's
    * not possible to mark an image as compressed like we do in
    * genX_cmd_buffer.c(EndRendering) or anv_blorp.c for all transfer
    * operations.
    */
   if (!(usage & VK_IMAGE_USAGE_STORAGE_BIT))
      return false;

   /* No AUX, no writes to the AUX surface :) */
   const uint32_t plane = anv_image_aspect_to_plane(image, aspect);
   const enum isl_aux_usage aux_usage = image->planes[plane].aux_usage;
   if (aux_usage == ISL_AUX_USAGE_NONE)
      return false;

   return true;
}

void anv_GetRenderingAreaGranularityKHR(
    VkDevice                                    _device,
    const VkRenderingAreaInfoKHR*               pRenderingAreaInfo,
    VkExtent2D*                                 pGranularity)
{
   *pGranularity = (VkExtent2D) {
      .width = 1,
      .height = 1,
   };
}

/*
 * Copyright © 2016 Red Hat.
 * Copyright © 2016 Bas Nieuwenhuizen
 *
 * SPDX-License-Identifier: MIT
 */

#include "radv_formats.h"
#include "radv_android.h"
#include "radv_debug.h"
#include "radv_entrypoints.h"
#include "radv_image.h"

#include "sid.h"

#include "vk_android.h"
#include "vk_enum_defines.h"
#include "vk_format.h"
#include "vk_log.h"
#include "vk_util.h"

#include "util/compiler.h"
#include "util/format_r11g11b10f.h"
#include "util/format_rgb9e5.h"
#include "util/format_srgb.h"
#include "util/half_float.h"
#include "ac_drm_fourcc.h"
#include "ac_formats.h"

uint32_t
radv_translate_buffer_numformat(const struct util_format_description *desc, int first_non_void)
{
   assert(util_format_get_num_planes(desc->format) == 1);

   return ac_translate_buffer_numformat(desc, first_non_void);
}

static bool
radv_is_vertex_buffer_format_supported(VkFormat format)
{
   if (format == VK_FORMAT_UNDEFINED)
      return false;

   if (vk_format_is_srgb(format))
      return false;

   const int first_non_void = vk_format_get_first_non_void_channel(format);
   if (first_non_void < 0)
      return false;

   const struct util_format_description *desc = radv_format_description(format);
   return ac_translate_buffer_dataformat(desc, first_non_void) != V_008F0C_BUF_DATA_FORMAT_INVALID;
}

uint32_t
radv_translate_tex_dataformat(const struct radv_physical_device *pdev, const struct util_format_description *desc,
                              int first_non_void)
{
   assert(util_format_get_num_planes(desc->format) == 1);

   if (desc->layout == UTIL_FORMAT_LAYOUT_SUBSAMPLED &&
       (desc->format == PIPE_FORMAT_R8G8_B8G8_UNORM || desc->format == PIPE_FORMAT_G8R8_B8R8_UNORM ||
        desc->format == PIPE_FORMAT_G8R8_G8B8_UNORM || desc->format == PIPE_FORMAT_R8G8_R8B8_UNORM))
      return ~0U;

   /* Closed VK driver does this also no 2/10/10/10 snorm */
   if (desc->format == PIPE_FORMAT_R10G10B10A2_SNORM)
      return ~0u;

   return ac_translate_tex_dataformat(&pdev->info, desc, first_non_void);
}

uint32_t
radv_translate_tex_numformat(const struct util_format_description *desc, int first_non_void)
{
   assert(util_format_get_num_planes(desc->format) == 1);

   return ac_translate_tex_numformat(desc, first_non_void);
}

static bool
radv_is_sampler_format_supported(const struct radv_physical_device *pdev, VkFormat format, bool *linear_sampling)
{
   const struct util_format_description *desc = radv_format_description(format);
   uint32_t num_format;
   if (format == VK_FORMAT_UNDEFINED || format == VK_FORMAT_R64_UINT || format == VK_FORMAT_R64_SINT)
      return false;
   num_format = radv_translate_tex_numformat(desc, vk_format_get_first_non_void_channel(format));

   if (num_format == V_008F14_IMG_NUM_FORMAT_USCALED || num_format == V_008F14_IMG_NUM_FORMAT_SSCALED)
      return false;

   if (num_format == V_008F14_IMG_NUM_FORMAT_UNORM || num_format == V_008F14_IMG_NUM_FORMAT_SNORM ||
       num_format == V_008F14_IMG_NUM_FORMAT_FLOAT || num_format == V_008F14_IMG_NUM_FORMAT_SRGB)
      *linear_sampling = true;
   else
      *linear_sampling = false;

   return radv_translate_tex_dataformat(pdev, radv_format_description(format),
                                        vk_format_get_first_non_void_channel(format)) != ~0U;
}

bool
radv_is_atomic_format_supported(VkFormat format)
{
   return format == VK_FORMAT_R32_UINT || format == VK_FORMAT_R32_SINT || format == VK_FORMAT_R32_SFLOAT ||
          format == VK_FORMAT_R64_UINT || format == VK_FORMAT_R64_SINT;
}

bool
radv_is_storage_image_format_supported(const struct radv_physical_device *pdev, VkFormat format)
{
   const struct radv_instance *instance = radv_physical_device_instance(pdev);
   const struct util_format_description *desc = radv_format_description(format);
   unsigned data_format, num_format;
   if (format == VK_FORMAT_UNDEFINED)
      return false;

   if (vk_format_has_stencil(format))
      return false;

   if (instance->drirc.disable_depth_storage && vk_format_has_depth(format))
      return false;

   data_format = radv_translate_tex_dataformat(pdev, desc, vk_format_get_first_non_void_channel(format));
   num_format = radv_translate_tex_numformat(desc, vk_format_get_first_non_void_channel(format));

   if (data_format == ~0)
      return false;

   /* Extracted from the GCN3 ISA document. */
   switch (num_format) {
   case V_008F14_IMG_NUM_FORMAT_UNORM:
   case V_008F14_IMG_NUM_FORMAT_SNORM:
   case V_008F14_IMG_NUM_FORMAT_UINT:
   case V_008F14_IMG_NUM_FORMAT_SINT:
   case V_008F14_IMG_NUM_FORMAT_FLOAT:
      break;
   default:
      return false;
   }

   switch (data_format) {
   case V_008F14_IMG_DATA_FORMAT_8:
   case V_008F14_IMG_DATA_FORMAT_16:
   case V_008F14_IMG_DATA_FORMAT_8_8:
   case V_008F14_IMG_DATA_FORMAT_32:
   case V_008F14_IMG_DATA_FORMAT_16_16:
   case V_008F14_IMG_DATA_FORMAT_10_11_11:
   case V_008F14_IMG_DATA_FORMAT_11_11_10:
   case V_008F14_IMG_DATA_FORMAT_10_10_10_2:
   case V_008F14_IMG_DATA_FORMAT_2_10_10_10:
   case V_008F14_IMG_DATA_FORMAT_8_8_8_8:
   case V_008F14_IMG_DATA_FORMAT_32_32:
   case V_008F14_IMG_DATA_FORMAT_16_16_16_16:
   case V_008F14_IMG_DATA_FORMAT_32_32_32_32:
   case V_008F14_IMG_DATA_FORMAT_5_6_5:
   case V_008F14_IMG_DATA_FORMAT_1_5_5_5:
   case V_008F14_IMG_DATA_FORMAT_5_5_5_1:
   case V_008F14_IMG_DATA_FORMAT_4_4_4_4:
      /* TODO: FMASK formats. */
      return true;
   case V_008F14_IMG_DATA_FORMAT_5_9_9_9:
      return pdev->info.gfx_level >= GFX10_3;
   default:
      return false;
   }
}

static bool
radv_is_buffer_dataformat_supported(const struct util_format_description *desc, int first_non_void)
{
   uint32_t data_format, type;

   data_format = ac_translate_buffer_dataformat(desc, first_non_void);
   if (data_format == V_008F0C_BUF_DATA_FORMAT_INVALID)
      return false;

   assert(first_non_void >= 0);

   type = desc->channel[first_non_void].type;

   if (desc->channel[first_non_void].size <= 16 && desc->nr_channels == 3 &&
       desc->format != PIPE_FORMAT_R11G11B10_FLOAT)
      return false;

   /* From the Southern Islands ISA documentation about MTBUF:
    * 'Memory reads of data in memory that is 32 or 64 bits do not
    * undergo any format conversion.'
    */
   if (desc->channel[first_non_void].size == 32 && type != UTIL_FORMAT_TYPE_FLOAT &&
       !desc->channel[first_non_void].pure_integer)
      return false;

   if (desc->channel[first_non_void].size == 64 && (type == UTIL_FORMAT_TYPE_FLOAT || desc->nr_channels != 1))
      return false;

   return true;
}

bool
radv_is_buffer_format_supported(VkFormat format, bool *scaled)
{
   const struct util_format_description *desc = radv_format_description(format);
   unsigned num_format;

   if (format == VK_FORMAT_UNDEFINED)
      return false;

   const int first_non_void = vk_format_get_first_non_void_channel(format);
   if (first_non_void < 0)
      return false;

   if (!radv_is_buffer_dataformat_supported(desc, first_non_void))
      return false;

   num_format = radv_translate_buffer_numformat(desc, first_non_void);
   if (scaled)
      *scaled = (num_format == V_008F0C_BUF_NUM_FORMAT_SSCALED) || (num_format == V_008F0C_BUF_NUM_FORMAT_USCALED);

   return true;
}

static bool
radv_is_colorbuffer_format_blendable(const struct radv_physical_device *pdev, VkFormat format)
{
   const struct util_format_description *desc = radv_format_description(format);
   const uint32_t color_format = ac_get_cb_format(pdev->info.gfx_level, desc->format);
   const uint32_t color_num_format = ac_get_cb_number_type(desc->format);

   assert(color_format != V_028C70_COLOR_INVALID);
   if (color_num_format == V_028C70_NUMBER_UINT || color_num_format == V_028C70_NUMBER_SINT ||
       color_format == V_028C70_COLOR_8_24 || color_format == V_028C70_COLOR_24_8 ||
       color_format == V_028C70_COLOR_X24_8_32_FLOAT)
      return false;

   return true;
}

bool
radv_is_colorbuffer_format_supported(const struct radv_physical_device *pdev, VkFormat format)
{
   const struct util_format_description *desc = radv_format_description(format);
   return ac_is_colorbuffer_format_supported(pdev->info.gfx_level, desc->format);
}

static bool
radv_is_zs_format_supported(VkFormat format)
{
   if (format == VK_FORMAT_D24_UNORM_S8_UINT || format == VK_FORMAT_X8_D24_UNORM_PACK32)
      return false;

   return ac_is_zs_format_supported(radv_format_to_pipe_format(format)) || format == VK_FORMAT_S8_UINT;
}

static bool
radv_is_filter_minmax_format_supported(const struct radv_physical_device *pdev, VkFormat format)
{
   return ac_is_reduction_mode_supported(&pdev->info, radv_format_to_pipe_format(format), false);
}

bool
radv_is_format_emulated(const struct radv_physical_device *pdev, VkFormat format)
{
   if (pdev->emulate_etc2 && vk_texcompress_etc2_emulation_format(format) != VK_FORMAT_UNDEFINED)
      return true;

   if (pdev->emulate_astc && vk_texcompress_astc_emulation_format(format) != VK_FORMAT_UNDEFINED)
      return true;

   return false;
}

static void
radv_physical_device_get_format_properties(struct radv_physical_device *pdev, VkFormat format,
                                           VkFormatProperties3 *out_properties)
{
   VkFormatFeatureFlags2 linear = 0, tiled = 0, buffer = 0;
   const struct util_format_description *desc = radv_format_description(format);
   bool scaled = false;
   /* TODO: implement some software emulation of SUBSAMPLED formats. */
   if (desc->format == PIPE_FORMAT_NONE || desc->layout == UTIL_FORMAT_LAYOUT_SUBSAMPLED) {
      out_properties->linearTilingFeatures = linear;
      out_properties->optimalTilingFeatures = tiled;
      out_properties->bufferFeatures = buffer;
      return;
   }

   if ((desc->layout == UTIL_FORMAT_LAYOUT_ETC && !pdev->info.has_etc_support) ||
       desc->layout == UTIL_FORMAT_LAYOUT_ASTC) {
      if (radv_is_format_emulated(pdev, format)) {
         /* required features for compressed formats */
         tiled = VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT | VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT |
                 VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT | VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT |
                 VK_FORMAT_FEATURE_2_BLIT_SRC_BIT;

         VkFormat emulation_format = vk_texcompress_etc2_emulation_format(format);
         if (emulation_format == VK_FORMAT_UNDEFINED)
            emulation_format = vk_texcompress_astc_emulation_format(format);

         if (radv_is_filter_minmax_format_supported(pdev, emulation_format))
            tiled |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT;
      }
      out_properties->linearTilingFeatures = linear;
      out_properties->optimalTilingFeatures = tiled;
      out_properties->bufferFeatures = buffer;
      return;
   }

   const bool multiplanar = vk_format_get_plane_count(format) > 1;
   if (multiplanar || desc->layout == UTIL_FORMAT_LAYOUT_SUBSAMPLED) {
      uint64_t tiling = VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT | VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT |
                        VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT | VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT;

      if (vk_format_get_ycbcr_info(format)) {
         tiling |= VK_FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT | VK_FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT;

         /* The subsampled formats have no support for linear filters. */
         if (desc->layout != UTIL_FORMAT_LAYOUT_SUBSAMPLED)
            tiling |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT;
      }

      if (multiplanar)
         tiling |= VK_FORMAT_FEATURE_2_DISJOINT_BIT;

      /* Fails for unknown reasons with linear tiling & subsampled formats. */
      if (desc->layout != UTIL_FORMAT_LAYOUT_SUBSAMPLED)
         linear = tiling;

      if (pdev->video_decode_enabled) {
         if (format == VK_FORMAT_G8_B8R8_2PLANE_420_UNORM ||
             format == VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 ||
             format == VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16) {
            linear |= VK_FORMAT_FEATURE_2_VIDEO_DECODE_OUTPUT_BIT_KHR;
            tiling |= VK_FORMAT_FEATURE_2_VIDEO_DECODE_OUTPUT_BIT_KHR | VK_FORMAT_FEATURE_2_VIDEO_DECODE_DPB_BIT_KHR;
         }
      }

      if (pdev->video_encode_enabled) {
         if (format == VK_FORMAT_G8_B8R8_2PLANE_420_UNORM ||
             format == VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 ||
             format == VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16) {
            linear |= VK_FORMAT_FEATURE_2_VIDEO_ENCODE_INPUT_BIT_KHR;
            tiling |= VK_FORMAT_FEATURE_2_VIDEO_ENCODE_INPUT_BIT_KHR | VK_FORMAT_FEATURE_2_VIDEO_ENCODE_DPB_BIT_KHR;
         }
      }

      out_properties->linearTilingFeatures = linear;
      out_properties->optimalTilingFeatures = tiling;
      out_properties->bufferFeatures = 0;
      return;
   }

   if (radv_is_storage_image_format_supported(pdev, format)) {
      tiled |= VK_FORMAT_FEATURE_2_STORAGE_IMAGE_BIT | VK_FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT |
               VK_FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT;
      linear |= VK_FORMAT_FEATURE_2_STORAGE_IMAGE_BIT | VK_FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT |
                VK_FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT;
   }

   if (radv_is_vertex_buffer_format_supported(format))
      buffer |= VK_FORMAT_FEATURE_2_VERTEX_BUFFER_BIT;

   if (radv_is_buffer_format_supported(format, &scaled)) {
      if (format != VK_FORMAT_R64_UINT && format != VK_FORMAT_R64_SINT && !scaled && !vk_format_is_srgb(format))
         buffer |= VK_FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT;
      buffer |= VK_FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT | VK_FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT |
                VK_FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT;
   }

   if (vk_format_is_depth_or_stencil(format)) {
      if (radv_is_zs_format_supported(format)) {
         tiled |= VK_FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT;
         tiled |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT;
         tiled |= VK_FORMAT_FEATURE_2_BLIT_SRC_BIT | VK_FORMAT_FEATURE_2_BLIT_DST_BIT;
         tiled |= VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT | VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT;

         if (radv_is_filter_minmax_format_supported(pdev, format))
            tiled |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT;

         if (vk_format_has_depth(format)) {
            tiled |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT |
                     VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT;
         }

         /* Don't support blitting surfaces with depth/stencil. */
         if (vk_format_has_depth(format) && vk_format_has_stencil(format))
            tiled &= ~VK_FORMAT_FEATURE_2_BLIT_DST_BIT;

         /* Don't support linear depth surfaces */
         linear = 0;
      }
   } else {
      bool linear_sampling;
      if (radv_is_sampler_format_supported(pdev, format, &linear_sampling)) {
         linear |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT | VK_FORMAT_FEATURE_2_BLIT_SRC_BIT;
         tiled |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT | VK_FORMAT_FEATURE_2_BLIT_SRC_BIT;

         if (radv_is_filter_minmax_format_supported(pdev, format)) {
            tiled |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT;
            linear |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT;
         }

         if (linear_sampling) {
            linear |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT;
            tiled |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT;
         }

         /* Don't support blitting/minmax for 96-bit formats. */
         if (vk_format_is_96bit(format)) {
            linear &= ~VK_FORMAT_FEATURE_2_BLIT_SRC_BIT;
            linear &= ~VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT;
         }
      }
      if (radv_is_colorbuffer_format_supported(pdev, format) && desc->channel[0].size != 64) {
         linear |= VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT | VK_FORMAT_FEATURE_2_BLIT_DST_BIT;
         tiled |= VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT | VK_FORMAT_FEATURE_2_BLIT_DST_BIT;
         if (radv_is_colorbuffer_format_blendable(pdev, format)) {
            linear |= VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT;
            tiled |= VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT;
         }
      }
      if (tiled && !scaled) {
         tiled |= VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT | VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT;
      }

      /* Tiled formatting does not support NPOT pixel sizes */
      if (!util_is_power_of_two_or_zero(vk_format_get_blocksize(format)))
         tiled = 0;
   }

   if (linear && !scaled) {
      linear |= VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT | VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT;
   }

   if (radv_is_atomic_format_supported(format)) {
      buffer |= VK_FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT;
      linear |= VK_FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT;
      tiled |= VK_FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT;
   }

   switch (format) {
   case VK_FORMAT_A2R10G10B10_SNORM_PACK32:
   case VK_FORMAT_A2B10G10R10_SNORM_PACK32:
   case VK_FORMAT_A2R10G10B10_SSCALED_PACK32:
   case VK_FORMAT_A2B10G10R10_SSCALED_PACK32:
   case VK_FORMAT_A2R10G10B10_SINT_PACK32:
   case VK_FORMAT_A2B10G10R10_SINT_PACK32:
      buffer &= ~(VK_FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT | VK_FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT);
      linear = 0;
      tiled = 0;
      break;
   case VK_FORMAT_R64_UINT:
   case VK_FORMAT_R64_SINT:
   case VK_FORMAT_R64_SFLOAT:
      tiled |= VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT | VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT;
      linear |= VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT | VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT;
      break;
   default:
      break;
   }

   switch (format) {
   case VK_FORMAT_R32G32_SFLOAT:
   case VK_FORMAT_R32G32B32_SFLOAT:
   case VK_FORMAT_R32G32B32A32_SFLOAT:
   case VK_FORMAT_R16G16_SFLOAT:
   case VK_FORMAT_R16G16B16_SFLOAT:
   case VK_FORMAT_R16G16B16A16_SFLOAT:
   case VK_FORMAT_R16G16_SNORM:
   case VK_FORMAT_R16G16_UNORM:
   case VK_FORMAT_R16G16B16A16_SNORM:
   case VK_FORMAT_R16G16B16A16_UNORM:
   case VK_FORMAT_R8G8_SNORM:
   case VK_FORMAT_R8G8_UNORM:
   case VK_FORMAT_R8G8B8A8_SNORM:
   case VK_FORMAT_R8G8B8A8_UNORM:
   case VK_FORMAT_A2B10G10R10_UNORM_PACK32:
      buffer |= VK_FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR;
      break;
   default:
      break;
   }
   /* addrlib does not support linear compressed textures. */
   if (vk_format_is_compressed(format))
      linear = 0;

   /* From the Vulkan spec 1.2.163:
    *
    * "VK_FORMAT_FEATURE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT must be supported for the
    *  following formats if the attachmentFragmentShadingRate feature is supported:"
    *
    * - VK_FORMAT_R8_UINT
    */
   if (format == VK_FORMAT_R8_UINT) {
      tiled |= VK_FORMAT_FEATURE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR;
   }

   /* It's invalid to expose buffer features with depth/stencil formats. */
   if (vk_format_is_depth_or_stencil(format)) {
      buffer = 0;
   }

   /* No depth/stencil support yet due to VKCTS issues. */
   if (radv_host_image_copy_enabled(pdev) && !vk_format_is_depth_or_stencil(format)) {
      if (linear & VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT)
         linear |= VK_FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT;

      if (tiled & VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT)
         tiled |= VK_FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT;
   }

   out_properties->linearTilingFeatures = linear;
   out_properties->optimalTilingFeatures = tiled;
   out_properties->bufferFeatures = buffer;
}

bool
radv_format_pack_clear_color(VkFormat format, uint32_t clear_vals[2], VkClearColorValue *value)
{
   const struct util_format_description *desc = radv_format_description(format);

   if (format == VK_FORMAT_B10G11R11_UFLOAT_PACK32) {
      clear_vals[0] = float3_to_r11g11b10f(value->float32);
      clear_vals[1] = 0;
      return true;
   } else if (format == VK_FORMAT_E5B9G9R9_UFLOAT_PACK32) {
      clear_vals[0] = float3_to_rgb9e5(value->float32);
      clear_vals[1] = 0;
      return true;
   }

   if (desc->layout != UTIL_FORMAT_LAYOUT_PLAIN) {
      fprintf(stderr, "failed to fast clear for non-plain format %d\n", format);
      return false;
   }

   if (!util_is_power_of_two_or_zero(desc->block.bits)) {
      fprintf(stderr, "failed to fast clear for NPOT format %d\n", format);
      return false;
   }

   if (desc->block.bits > 64) {
      /*
       * We have a 128 bits format, check if the first 3 components are the same.
       * Every elements has to be 32 bits since we don't support 64-bit formats,
       * and we can skip swizzling checks as alpha always comes last for these and
       * we do not care about the rest as they have to be the same.
       */
      if (desc->channel[0].type == UTIL_FORMAT_TYPE_FLOAT) {
         if (value->float32[0] != value->float32[1] || value->float32[0] != value->float32[2])
            return false;
      } else {
         if (value->uint32[0] != value->uint32[1] || value->uint32[0] != value->uint32[2])
            return false;
      }
      clear_vals[0] = value->uint32[0];
      clear_vals[1] = value->uint32[3];
      return true;
   }
   uint64_t clear_val = 0;

   for (unsigned c = 0; c < 4; ++c) {
      if (desc->swizzle[c] >= 4)
         continue;

      const struct util_format_channel_description *channel = &desc->channel[desc->swizzle[c]];
      assert(channel->size);

      uint64_t v = 0;
      if (channel->pure_integer) {
         v = value->uint32[c] & ((1ULL << channel->size) - 1);
      } else if (channel->normalized) {
         if (channel->type == UTIL_FORMAT_TYPE_UNSIGNED && desc->swizzle[c] < 3 &&
             desc->colorspace == UTIL_FORMAT_COLORSPACE_SRGB) {
            assert(channel->size == 8);

            v = util_format_linear_float_to_srgb_8unorm(value->float32[c]);
         } else {
            float f = MIN2(value->float32[c], 1.0f);

            if (channel->type == UTIL_FORMAT_TYPE_UNSIGNED) {
               f = MAX2(f, 0.0f) * ((1ULL << channel->size) - 1);
            } else {
               f = MAX2(f, -1.0f) * ((1ULL << (channel->size - 1)) - 1);
            }

            /* The hardware rounds before conversion. */
            if (f > 0)
               f += 0.5f;
            else
               f -= 0.5f;

            v = (uint64_t)f;
         }
      } else if (channel->type == UTIL_FORMAT_TYPE_FLOAT) {
         if (channel->size == 32) {
            memcpy(&v, &value->float32[c], 4);
         } else if (channel->size == 16) {
            v = _mesa_float_to_float16_rtz(value->float32[c]);
         } else {
            fprintf(stderr, "failed to fast clear for unhandled float size in format %d\n", format);
            return false;
         }
      } else {
         fprintf(stderr, "failed to fast clear for unhandled component type in format %d\n", format);
         return false;
      }
      clear_val |= (v & ((1ULL << channel->size) - 1)) << channel->shift;
   }

   clear_vals[0] = clear_val;
   clear_vals[1] = clear_val >> 32;

   return true;
}

static const struct ac_modifier_options radv_modifier_options = {
   .dcc = true,
   .dcc_retile = true,
};

static VkFormatFeatureFlags2
radv_get_modifier_flags(struct radv_physical_device *pdev, VkFormat format, uint64_t modifier,
                        const VkFormatProperties3 *props)
{
   const struct radv_instance *instance = radv_physical_device_instance(pdev);
   VkFormatFeatureFlags2 features;

   if (vk_format_is_compressed(format) || vk_format_is_depth_or_stencil(format))
      return 0;

   if (modifier == DRM_FORMAT_MOD_LINEAR)
      features = props->linearTilingFeatures;
   else
      features = props->optimalTilingFeatures;

   /* Unconditionally disable DISJOINT support for modifiers for now */
   features &= ~VK_FORMAT_FEATURE_2_DISJOINT_BIT;

   /* Unconditionally disable HOST_TRANSFER support for modifiers for now */
   features &= ~VK_FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT;

   if (ac_modifier_has_dcc(modifier)) {
      /* We don't enable DCC for multi-planar formats */
      if (vk_format_get_plane_count(format) > 1)
         return 0;

      /* Only disable support for STORAGE_IMAGE on modifiers that
       * do not support DCC image stores or when explicitly disabled.
       */
      if (!ac_modifier_supports_dcc_image_stores(pdev->info.gfx_level, modifier) ||
          radv_is_atomic_format_supported(format) ||
          (instance->drirc.disable_dcc_stores && pdev->info.gfx_level < GFX12))
         features &= ~VK_FORMAT_FEATURE_2_STORAGE_IMAGE_BIT;

      if (instance->debug_flags & (RADV_DEBUG_NO_DCC | RADV_DEBUG_NO_DISPLAY_DCC))
         return 0;
   }

   return features;
}

static void
radv_list_drm_format_modifiers(struct radv_physical_device *pdev, VkFormat format,
                               const VkFormatProperties3 *format_props, VkDrmFormatModifierPropertiesListEXT *mod_list)
{
   unsigned mod_count;

   if (!mod_list)
      return;

   if (vk_format_is_compressed(format) || vk_format_is_depth_or_stencil(format)) {
      mod_list->drmFormatModifierCount = 0;
      return;
   }

   VK_OUTARRAY_MAKE_TYPED(VkDrmFormatModifierPropertiesEXT, out, mod_list->pDrmFormatModifierProperties,
                          &mod_list->drmFormatModifierCount);

   ac_get_supported_modifiers(&pdev->info, &radv_modifier_options, radv_format_to_pipe_format(format), &mod_count,
                              NULL);

   uint64_t *mods = malloc(mod_count * sizeof(uint64_t));
   if (!mods) {
      /* We can't return an error here ... */
      mod_list->drmFormatModifierCount = 0;
      return;
   }
   ac_get_supported_modifiers(&pdev->info, &radv_modifier_options, radv_format_to_pipe_format(format), &mod_count,
                              mods);

   for (unsigned i = 0; i < mod_count; ++i) {
      VkFormatFeatureFlags2 features = radv_get_modifier_flags(pdev, format, mods[i], format_props);
      if (!features)
         continue;

      unsigned planes = vk_format_get_plane_count(format);

      if (pdev->info.gfx_level < GFX12) {
         /* DCC is transparent to the userspace driver on GFX12 so it doesn't
          * need additional planes.
          */
         planes += ac_modifier_has_dcc(mods[i]) + ac_modifier_has_dcc_retile(mods[i]);
      }

      vk_outarray_append_typed(VkDrmFormatModifierPropertiesEXT, &out, out_props)
      {
         *out_props = (VkDrmFormatModifierPropertiesEXT){
            .drmFormatModifier = mods[i],
            .drmFormatModifierPlaneCount = planes,
            .drmFormatModifierTilingFeatures = vk_format_features2_to_features(features),
         };
      };
   }

   free(mods);
}

static void
radv_list_drm_format_modifiers_2(struct radv_physical_device *pdev, VkFormat format,
                                 const VkFormatProperties3 *format_props,
                                 VkDrmFormatModifierPropertiesList2EXT *mod_list)
{
   unsigned mod_count;

   if (!mod_list)
      return;

   if (vk_format_is_compressed(format) || vk_format_is_depth_or_stencil(format)) {
      mod_list->drmFormatModifierCount = 0;
      return;
   }

   VK_OUTARRAY_MAKE_TYPED(VkDrmFormatModifierProperties2EXT, out, mod_list->pDrmFormatModifierProperties,
                          &mod_list->drmFormatModifierCount);

   ac_get_supported_modifiers(&pdev->info, &radv_modifier_options, radv_format_to_pipe_format(format), &mod_count,
                              NULL);

   uint64_t *mods = malloc(mod_count * sizeof(uint64_t));
   if (!mods) {
      /* We can't return an error here ... */
      mod_list->drmFormatModifierCount = 0;
      return;
   }
   ac_get_supported_modifiers(&pdev->info, &radv_modifier_options, radv_format_to_pipe_format(format), &mod_count,
                              mods);

   for (unsigned i = 0; i < mod_count; ++i) {
      VkFormatFeatureFlags2 features = radv_get_modifier_flags(pdev, format, mods[i], format_props);
      if (!features)
         continue;

      unsigned planes = vk_format_get_plane_count(format);

      if (pdev->info.gfx_level < GFX12) {
         /* DCC is transparent to the userspace driver on GFX12 so it doesn't
          * need additional planes.
          */
         planes += ac_modifier_has_dcc(mods[i]) + ac_modifier_has_dcc_retile(mods[i]);
      }

      vk_outarray_append_typed(VkDrmFormatModifierProperties2EXT, &out, out_props)
      {
         *out_props = (VkDrmFormatModifierProperties2EXT){
            .drmFormatModifier = mods[i],
            .drmFormatModifierPlaneCount = planes,
            .drmFormatModifierTilingFeatures = features,
         };
      };
   }

   free(mods);
}

static VkResult
radv_check_modifier_support(struct radv_physical_device *pdev, const VkPhysicalDeviceImageFormatInfo2 *info,
                            VkImageFormatProperties *props, VkFormat format, uint64_t modifier)
{
   uint32_t max_width, max_height;

   if (radv_is_format_emulated(pdev, format))
      return VK_ERROR_FORMAT_NOT_SUPPORTED;

   /* We did not add modifiers for sparse textures. */
   if (info->flags &
       (VK_IMAGE_CREATE_SPARSE_BINDING_BIT | VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT | VK_IMAGE_CREATE_SPARSE_ALIASED_BIT))
      return VK_ERROR_FORMAT_NOT_SUPPORTED;

   /*
    * Need to check the modifier is supported in general:
    * "If the drmFormatModifier is incompatible with the parameters specified
    * in VkPhysicalDeviceImageFormatInfo2 and its pNext chain, then
    * vkGetPhysicalDeviceImageFormatProperties2 returns VK_ERROR_FORMAT_NOT_SUPPORTED.
    * The implementation must support the query of any drmFormatModifier,
    * including unknown and invalid modifier values."
    */
   VkDrmFormatModifierPropertiesListEXT mod_list = {
      .sType = VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT,
   };

   VkFormatProperties2 format_props2 = {.sType = VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2, .pNext = &mod_list};

   radv_GetPhysicalDeviceFormatProperties2(radv_physical_device_to_handle(pdev), format, &format_props2);

   if (!mod_list.drmFormatModifierCount)
      return VK_ERROR_FORMAT_NOT_SUPPORTED;

   mod_list.pDrmFormatModifierProperties =
      calloc(mod_list.drmFormatModifierCount, sizeof(*mod_list.pDrmFormatModifierProperties));
   if (!mod_list.pDrmFormatModifierProperties)
      return VK_ERROR_OUT_OF_HOST_MEMORY;

   radv_GetPhysicalDeviceFormatProperties2(radv_physical_device_to_handle(pdev), format, &format_props2);

   bool found = false;
   for (uint32_t i = 0; i < mod_list.drmFormatModifierCount && !found; ++i)
      if (mod_list.pDrmFormatModifierProperties[i].drmFormatModifier == modifier)
         found = true;

   free(mod_list.pDrmFormatModifierProperties);

   if (!found)
      return VK_ERROR_FORMAT_NOT_SUPPORTED;

   bool need_dcc_sign_reinterpret = false;
   if (ac_modifier_has_dcc(modifier) &&
       !radv_are_formats_dcc_compatible(pdev, info->pNext, format, info->flags, &need_dcc_sign_reinterpret) &&
       !need_dcc_sign_reinterpret)
      return VK_ERROR_FORMAT_NOT_SUPPORTED;

   const bool video = info->usage & (VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR | VK_IMAGE_USAGE_VIDEO_ENCODE_SRC_BIT_KHR);
   if (video) {
      if (!ac_modifier_supports_video(&pdev->info, modifier))
         return VK_ERROR_FORMAT_NOT_SUPPORTED;

      /* Decode DPB and output coincide (tier3) requires tiling. */
      if (info->usage & VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR && modifier == DRM_FORMAT_MOD_LINEAR)
         return VK_ERROR_FORMAT_NOT_SUPPORTED;
   }

   /* We can expand this as needed and implemented but there is not much demand
    * for more.
    * Video can't support array layers with swizzle modes that use slice index
    * for addressing. */
   if (ac_modifier_has_dcc(modifier) || video) {
      props->maxMipLevels = 1;
      props->maxArrayLayers = 1;
   }

   ac_modifier_max_extent(&pdev->info, modifier, &max_width, &max_height);
   props->maxExtent.width = MIN2(props->maxExtent.width, max_width);
   props->maxExtent.height = MIN2(props->maxExtent.width, max_height);

   /* We don't support MSAA for modifiers */
   props->sampleCounts &= VK_SAMPLE_COUNT_1_BIT;
   return VK_SUCCESS;
}

VKAPI_ATTR void VKAPI_CALL
radv_GetPhysicalDeviceFormatProperties2(VkPhysicalDevice physicalDevice, VkFormat format,
                                        VkFormatProperties2 *pFormatProperties)
{
   VK_FROM_HANDLE(radv_physical_device, pdev, physicalDevice);
   VkFormatProperties3 format_props;

   radv_physical_device_get_format_properties(pdev, format, &format_props);

   pFormatProperties->formatProperties.linearTilingFeatures =
      vk_format_features2_to_features(format_props.linearTilingFeatures);
   pFormatProperties->formatProperties.optimalTilingFeatures =
      vk_format_features2_to_features(format_props.optimalTilingFeatures);
   pFormatProperties->formatProperties.bufferFeatures = vk_format_features2_to_features(format_props.bufferFeatures);

   VkFormatProperties3 *format_props_extended = vk_find_struct(pFormatProperties, FORMAT_PROPERTIES_3);
   if (format_props_extended) {
      format_props_extended->linearTilingFeatures = format_props.linearTilingFeatures;
      format_props_extended->optimalTilingFeatures = format_props.optimalTilingFeatures;
      format_props_extended->bufferFeatures = format_props.bufferFeatures;
   }

   radv_list_drm_format_modifiers(pdev, format, &format_props,
                                  vk_find_struct(pFormatProperties, DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT));
   radv_list_drm_format_modifiers_2(pdev, format, &format_props,
                                    vk_find_struct(pFormatProperties, DRM_FORMAT_MODIFIER_PROPERTIES_LIST_2_EXT));
}

static VkResult
radv_get_image_format_properties(struct radv_physical_device *pdev, const VkPhysicalDeviceImageFormatInfo2 *info,
                                 VkFormat format, VkImageFormatProperties *pImageFormatProperties)

{
   VkFormatProperties3 format_props;
   VkFormatFeatureFlags2 format_feature_flags;
   VkExtent3D maxExtent;
   uint32_t maxMipLevels;
   uint32_t maxArraySize;
   VkSampleCountFlags sampleCounts = VK_SAMPLE_COUNT_1_BIT;
   const struct util_format_description *desc = radv_format_description(format);
   enum amd_gfx_level gfx_level = pdev->info.gfx_level;
   VkImageTiling tiling = info->tiling;
   const VkPhysicalDeviceImageDrmFormatModifierInfoEXT *mod_info =
      vk_find_struct_const(info->pNext, PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT);
   VkResult result = VK_ERROR_FORMAT_NOT_SUPPORTED;

   radv_physical_device_get_format_properties(pdev, format, &format_props);
   if (tiling == VK_IMAGE_TILING_LINEAR) {
      format_feature_flags = format_props.linearTilingFeatures;
   } else if (tiling == VK_IMAGE_TILING_OPTIMAL) {
      format_feature_flags = format_props.optimalTilingFeatures;
   } else if (tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
      format_feature_flags = radv_get_modifier_flags(pdev, format, mod_info->drmFormatModifier, &format_props);
   } else {
      UNREACHABLE("bad VkImageTiling");
   }

   if (format_feature_flags == 0)
      goto unsupported;

   if (info->type == VK_IMAGE_TYPE_1D && radv_is_format_emulated(pdev, format))
      goto unsupported;
   if (info->type != VK_IMAGE_TYPE_2D && vk_format_is_depth_or_stencil(format))
      goto unsupported;

   switch (info->type) {
   default:
      UNREACHABLE("bad vkimage type\n");
   case VK_IMAGE_TYPE_1D:
      maxExtent.width = 16384;
      maxExtent.height = 1;
      maxExtent.depth = 1;
      maxMipLevels = 15; /* log2(maxWidth) + 1 */
      maxArraySize = gfx_level >= GFX10 ? 8192 : 2048;
      break;
   case VK_IMAGE_TYPE_2D:
      maxExtent.width = 16384;
      maxExtent.height = 16384;
      maxExtent.depth = 1;
      maxMipLevels = 15; /* log2(maxWidth) + 1 */
      maxArraySize = gfx_level >= GFX10 ? 8192 : 2048;
      break;
   case VK_IMAGE_TYPE_3D:
      if (gfx_level >= GFX10) {
         maxExtent.width = 8192;
         maxExtent.height = 8192;
         maxExtent.depth = 8192;
      } else {
         maxExtent.width = 2048;
         maxExtent.height = 2048;
         maxExtent.depth = 2048;
      }
      maxMipLevels = util_logbase2(maxExtent.width) + 1;
      maxArraySize = 1;
      break;
   }

   if (desc->layout == UTIL_FORMAT_LAYOUT_SUBSAMPLED) {
      /* Might be able to support but the entire format support is
       * messy, so taking the lazy way out. */
      maxArraySize = 1;
   }

   if (tiling == VK_IMAGE_TILING_OPTIMAL && info->type == VK_IMAGE_TYPE_2D &&
       (format_feature_flags &
        (VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT | VK_FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT)) &&
       !(info->flags & VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT) &&
       !(info->usage & VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR)) {
      sampleCounts |= VK_SAMPLE_COUNT_2_BIT | VK_SAMPLE_COUNT_4_BIT | VK_SAMPLE_COUNT_8_BIT;
   }

   if (tiling == VK_IMAGE_TILING_LINEAR && vk_format_is_96bit(format)) {
      /* R32G32B32 is a weird format and the driver currently only
       * supports the barely minimum.
       * TODO: Implement more if we really need to.
       */
      if (info->type == VK_IMAGE_TYPE_3D)
         goto unsupported;
      maxArraySize = 1;
      maxMipLevels = 1;
   }

   /* We can't create 3d compressed 128bpp images that can be rendered to on GFX9 */
   if (pdev->info.gfx_level >= GFX9 && info->type == VK_IMAGE_TYPE_3D && vk_format_get_blocksizebits(format) == 128 &&
       vk_format_is_compressed(format) && (info->flags & VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT) &&
       ((info->flags & VK_IMAGE_CREATE_EXTENDED_USAGE_BIT) || (info->usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT))) {
      goto unsupported;
   }

   /* GFX6 has issues with 1D block-compressed formats. */
   if (pdev->info.gfx_level == GFX6 && info->type == VK_IMAGE_TYPE_1D && vk_format_is_block_compressed(format)) {
      maxMipLevels = 1;

      if ((info->flags & VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT) &&
          ((info->flags & VK_IMAGE_CREATE_EXTENDED_USAGE_BIT) || (info->usage & VK_IMAGE_USAGE_STORAGE_BIT))) {
         goto unsupported;
      }
   }

   /* From the Vulkan 1.3.206 spec:
    *
    * "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT specifies that the image can be created with usage flags
    * that are not supported for the format the image is created with but are supported for at least
    * one format a VkImageView created from the image can have."
    */
   VkImageUsageFlags image_usage = info->usage;
   if (info->flags & VK_IMAGE_CREATE_EXTENDED_USAGE_BIT)
      image_usage = 0;

   if (image_usage & VK_IMAGE_USAGE_SAMPLED_BIT) {
      if (!(format_feature_flags & VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT)) {
         goto unsupported;
      }
   }

   if (image_usage & VK_IMAGE_USAGE_STORAGE_BIT) {
      if (!(format_feature_flags & VK_FORMAT_FEATURE_2_STORAGE_IMAGE_BIT)) {
         goto unsupported;
      }
   }

   if (image_usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) {
      if (!(format_feature_flags & VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT)) {
         goto unsupported;
      }
   }

   if (image_usage & VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT) {
      if (!(format_feature_flags & VK_FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT)) {
         goto unsupported;
      }
   }

   if (image_usage & VK_IMAGE_USAGE_TRANSFER_SRC_BIT) {
      if (!(format_feature_flags & VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT)) {
         goto unsupported;
      }
   }

   if (image_usage & VK_IMAGE_USAGE_TRANSFER_DST_BIT) {
      if (!(format_feature_flags & VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT)) {
         goto unsupported;
      }
   }

   if (image_usage & VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT) {
      if (!(format_feature_flags &
            (VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT | VK_FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT))) {
         goto unsupported;
      }
   }

   if (image_usage & VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR) {
      if (!(format_feature_flags & VK_FORMAT_FEATURE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR))
         goto unsupported;
   }

   if (image_usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT) {
      if (!(format_feature_flags & VK_FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT))
         goto unsupported;
   }

   if (info->flags & VK_IMAGE_CREATE_SPARSE_BINDING_BIT) {
      /* Sparse resources with multi-planar formats are unsupported. */
      if (vk_format_get_plane_count(format) > 1)
         goto unsupported;

      /* Sparse resources with host-transfer are unsupported. */
      if (image_usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT)
         goto unsupported;
   }

   if (info->flags & VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT) {
      /* Sparse textures are only supported on GFX8+. */
      if (pdev->info.gfx_level < GFX8)
         goto unsupported;

      if (vk_format_get_plane_count(format) > 1 || info->type == VK_IMAGE_TYPE_1D ||
          info->tiling != VK_IMAGE_TILING_OPTIMAL || vk_format_is_depth_or_stencil(format))
         goto unsupported;
   }

   if ((info->flags & (VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT | VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT)) &&
       radv_is_format_emulated(pdev, format)) {
      goto unsupported;
   }

   *pImageFormatProperties = (VkImageFormatProperties){
      .maxExtent = maxExtent,
      .maxMipLevels = maxMipLevels,
      .maxArrayLayers = maxArraySize,
      .sampleCounts = sampleCounts,

      /* FINISHME: Accurately calculate
       * VkImageFormatProperties::maxResourceSize.
       */
      .maxResourceSize = UINT32_MAX,
   };

   if (mod_info) {
      result = radv_check_modifier_support(pdev, info, pImageFormatProperties, format, mod_info->drmFormatModifier);
      if (result != VK_SUCCESS)
         goto unsupported;
   }

   return VK_SUCCESS;
unsupported:
   *pImageFormatProperties = (VkImageFormatProperties){
      .maxExtent = {0, 0, 0},
      .maxMipLevels = 0,
      .maxArrayLayers = 0,
      .sampleCounts = 0,
      .maxResourceSize = 0,
   };

   return result;
}

static void
get_external_image_format_properties(struct radv_physical_device *pdev,
                                     const VkPhysicalDeviceImageFormatInfo2 *pImageFormatInfo,
                                     VkExternalMemoryHandleTypeFlagBits handleType,
                                     VkExternalMemoryProperties *external_properties,
                                     VkImageFormatProperties *format_properties)
{
   VkExternalMemoryFeatureFlagBits flags = 0;
   VkExternalMemoryHandleTypeFlags export_flags = 0;
   VkExternalMemoryHandleTypeFlags compat_flags = 0;

   if (radv_is_format_emulated(pdev, pImageFormatInfo->format))
      return;

   if (pImageFormatInfo->flags & VK_IMAGE_CREATE_SPARSE_BINDING_BIT)
      return;

   switch (handleType) {
   case VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT:
      if (pImageFormatInfo->tiling != VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT)
         break;
      FALLTHROUGH;
   case VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT:
      flags = VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT | VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT;
      if (handleType == VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT &&
          pImageFormatInfo->tiling != VK_IMAGE_TILING_LINEAR)
         flags |= VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT;

      compat_flags = export_flags = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT;
      if (pImageFormatInfo->tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
         compat_flags |= VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT;
         export_flags |= VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT;
      }
      break;
   case VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID:
      if (!pdev->vk.supported_extensions.ANDROID_external_memory_android_hardware_buffer)
         break;

      if (pImageFormatInfo->type != VK_IMAGE_TYPE_2D)
         break;

      format_properties->maxMipLevels = MIN2(1, format_properties->maxMipLevels);
      format_properties->maxArrayLayers = MIN2(1, format_properties->maxArrayLayers);
      format_properties->sampleCounts &= VK_SAMPLE_COUNT_1_BIT;

      flags = VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT | VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT;

      /* advertise EXPORTABLE only when radv_create_ahb_memory supports the format */
      if (radv_android_gralloc_supports_format(pImageFormatInfo->format, pImageFormatInfo->usage))
         flags |= VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT;

      compat_flags = VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID;
      break;
   case VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT:
      flags = VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT;
      compat_flags = VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT;
      break;
   default:
      break;
   }

   *external_properties = (VkExternalMemoryProperties){
      .externalMemoryFeatures = flags,
      .exportFromImportedHandleTypes = export_flags,
      .compatibleHandleTypes = compat_flags,
   };
}

VKAPI_ATTR VkResult VKAPI_CALL
radv_GetPhysicalDeviceImageFormatProperties2(VkPhysicalDevice physicalDevice,
                                             const VkPhysicalDeviceImageFormatInfo2 *base_info,
                                             VkImageFormatProperties2 *base_props)
{
   VK_FROM_HANDLE(radv_physical_device, pdev, physicalDevice);
   const struct radv_instance *instance = radv_physical_device_instance(pdev);
   const VkPhysicalDeviceExternalImageFormatInfo *external_info = NULL;
   VkExternalImageFormatProperties *external_props = NULL;
   struct VkAndroidHardwareBufferUsageANDROID *android_usage = NULL;
   VkSamplerYcbcrConversionImageFormatProperties *ycbcr_props = NULL;
   VkTextureLODGatherFormatPropertiesAMD *texture_lod_props = NULL;
   VkImageCompressionPropertiesEXT *image_compression_props = NULL;
   VkHostImageCopyDevicePerformanceQueryEXT *host_perf_props = NULL;
   VkResult result;
   VkFormat format = radv_select_android_external_format(base_info->pNext, base_info->format);

   result = radv_get_image_format_properties(pdev, base_info, format, &base_props->imageFormatProperties);
   if (result != VK_SUCCESS)
      return result;

   /* Extract input structs */
   vk_foreach_struct_const (s, base_info->pNext) {
      switch (s->sType) {
      case VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO:
         external_info = (const void *)s;
         break;
      default:
         break;
      }
   }

   /* Extract output structs */
   vk_foreach_struct (s, base_props->pNext) {
      switch (s->sType) {
      case VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES:
         external_props = (void *)s;
         break;
      case VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES:
         ycbcr_props = (void *)s;
         break;
      case VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID:
         android_usage = (void *)s;
         break;
      case VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD:
         texture_lod_props = (void *)s;
         break;
      case VK_STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT:
         image_compression_props = (void *)s;
         break;
      case VK_STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT:
         host_perf_props = (void *)s;
         break;
      default:
         break;
      }
   }

   bool ahb_supported = pdev->vk.supported_extensions.ANDROID_external_memory_android_hardware_buffer;
   if (android_usage && ahb_supported) {
      android_usage->androidHardwareBufferUsage = vk_image_usage_to_ahb_usage(base_info->flags, base_info->usage);
   }

   /* From the Vulkan 1.0.97 spec:
    *
    *    If handleType is 0, vkGetPhysicalDeviceImageFormatProperties2 will
    *    behave as if VkPhysicalDeviceExternalImageFormatInfo was not
    *    present and VkExternalImageFormatProperties will be ignored.
    */
   if (external_info && external_info->handleType != 0) {
      VkExternalImageFormatProperties fallback_external_props;

      if (!external_props) {
         memset(&fallback_external_props, 0, sizeof(fallback_external_props));
         external_props = &fallback_external_props;
      }

      get_external_image_format_properties(pdev, base_info, external_info->handleType,
                                           &external_props->externalMemoryProperties,
                                           &base_props->imageFormatProperties);
      if (!external_props->externalMemoryProperties.externalMemoryFeatures) {
         /* From the Vulkan 1.0.97 spec:
          *
          *    If handleType is not compatible with the [parameters] specified
          *    in VkPhysicalDeviceImageFormatInfo2, then
          *    vkGetPhysicalDeviceImageFormatProperties2 returns
          *    VK_ERROR_FORMAT_NOT_SUPPORTED.
          */
         result = vk_errorf(pdev, VK_ERROR_FORMAT_NOT_SUPPORTED, "unsupported VkExternalMemoryHandleTypeFlagBits %s",
                            vk_ExternalMemoryHandleTypeFlagBits_to_str(external_info->handleType));
         goto fail;
      }
   }

   if (ycbcr_props) {
      ycbcr_props->combinedImageSamplerDescriptorCount = vk_format_get_plane_count(format);
   }

   if (texture_lod_props) {
      if (pdev->info.gfx_level >= GFX9) {
         texture_lod_props->supportsTextureGatherLODBiasAMD = true;
      } else {
         texture_lod_props->supportsTextureGatherLODBiasAMD = !vk_format_is_int(format);
      }
   }

   if (image_compression_props) {
      image_compression_props->imageCompressionFixedRateFlags = VK_IMAGE_COMPRESSION_FIXED_RATE_NONE_EXT;

      if (vk_format_is_depth_or_stencil(format)) {
         image_compression_props->imageCompressionFlags =
            pdev->use_hiz ? VK_IMAGE_COMPRESSION_DEFAULT_EXT : VK_IMAGE_COMPRESSION_DISABLED_EXT;
      } else {
         image_compression_props->imageCompressionFlags =
            ((instance->debug_flags & RADV_DEBUG_NO_DCC) || pdev->info.gfx_level < GFX8)
               ? VK_IMAGE_COMPRESSION_DISABLED_EXT
               : VK_IMAGE_COMPRESSION_DEFAULT_EXT;
      }
   }

   if (host_perf_props) {
      bool might_enable_compression = false;

      if (vk_format_is_depth_or_stencil(format)) {
         might_enable_compression |= pdev->use_hiz && (base_info->usage & VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT);
      } else {
         might_enable_compression |=
            !(instance->debug_flags & RADV_DEBUG_NO_DCC) && (base_info->usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);
      }

      host_perf_props->optimalDeviceAccess = pdev->info.gfx_level >= GFX12 || !might_enable_compression;
      host_perf_props->identicalMemoryLayout = pdev->info.gfx_level >= GFX12 || !might_enable_compression;
   }

   return VK_SUCCESS;

fail:
   if (result == VK_ERROR_FORMAT_NOT_SUPPORTED) {
      /* From the Vulkan 1.0.97 spec:
       *
       *    If the combination of parameters to
       *    vkGetPhysicalDeviceImageFormatProperties2 is not supported by
       *    the implementation for use in vkCreateImage, then all members of
       *    imageFormatProperties will be filled with zero.
       */
      base_props->imageFormatProperties = (VkImageFormatProperties){0};
   }

   return result;
}

static void
fill_sparse_image_format_properties(struct radv_physical_device *pdev, VkImageType type, VkFormat format,
                                    VkSparseImageFormatProperties *prop)
{
   prop->aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
   prop->flags = 0;

   /* On GFX8 we first subdivide by level and then layer, leading to a single
    * miptail. On GFX9+ we first subdivide by layer and then level which results
    * in a miptail per layer. */
   if (pdev->info.gfx_level < GFX9)
      prop->flags |= VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT;

   unsigned w, h;
   unsigned d = 1;
   if (type == VK_IMAGE_TYPE_3D) {
      if (pdev->info.gfx_level >= GFX9) {
         unsigned l2_size = 16 - util_logbase2(vk_format_get_blocksize(format));
         w = (1u << ((l2_size + 2) / 3)) * vk_format_get_blockwidth(format);
         h = (1u << ((l2_size + 1) / 3)) * vk_format_get_blockheight(format);
         d = (1u << ((l2_size + 0) / 3));
      } else {
         /* GFX7/GFX8 thick tiling modes */
         unsigned bs = vk_format_get_blocksize(format);
         unsigned l2_size = 16 - util_logbase2(bs) - (bs <= 4 ? 2 : 0);
         w = (1u << ((l2_size + 1) / 2)) * vk_format_get_blockwidth(format);
         h = (1u << (l2_size / 2)) * vk_format_get_blockheight(format);
         d = bs <= 4 ? 4 : 1;
      }
   } else {
      /* This assumes the sparse image tile size is always 64 KiB (1 << 16) */
      unsigned l2_size = 16 - util_logbase2(vk_format_get_blocksize(format));
      w = (1u << ((l2_size + 1) / 2)) * vk_format_get_blockwidth(format);
      h = (1u << (l2_size / 2)) * vk_format_get_blockheight(format);
   }
   prop->imageGranularity = (VkExtent3D){w, h, d};
}

VKAPI_ATTR void VKAPI_CALL
radv_GetPhysicalDeviceSparseImageFormatProperties2(VkPhysicalDevice physicalDevice,
                                                   const VkPhysicalDeviceSparseImageFormatInfo2 *pFormatInfo,
                                                   uint32_t *pPropertyCount,
                                                   VkSparseImageFormatProperties2 *pProperties)
{
   VK_FROM_HANDLE(radv_physical_device, pdev, physicalDevice);
   VkResult result;

   if (pFormatInfo->samples > VK_SAMPLE_COUNT_1_BIT) {
      *pPropertyCount = 0;
      return;
   }

   const VkPhysicalDeviceImageFormatInfo2 fmt_info = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2,
      .format = pFormatInfo->format,
      .type = pFormatInfo->type,
      .tiling = pFormatInfo->tiling,
      .usage = pFormatInfo->usage,
      .flags = VK_IMAGE_CREATE_SPARSE_BINDING_BIT | VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT};

   VkImageFormatProperties fmt_props;
   result = radv_get_image_format_properties(pdev, &fmt_info, pFormatInfo->format, &fmt_props);
   if (result != VK_SUCCESS) {
      *pPropertyCount = 0;
      return;
   }

   VK_OUTARRAY_MAKE_TYPED(VkSparseImageFormatProperties2, out, pProperties, pPropertyCount);

   vk_outarray_append_typed(VkSparseImageFormatProperties2, &out, prop)
   {
      fill_sparse_image_format_properties(pdev, pFormatInfo->type, pFormatInfo->format, &prop->properties);
   };
}

VKAPI_ATTR void VKAPI_CALL
radv_GetImageSparseMemoryRequirements2(VkDevice _device, const VkImageSparseMemoryRequirementsInfo2 *pInfo,
                                       uint32_t *pSparseMemoryRequirementCount,
                                       VkSparseImageMemoryRequirements2 *pSparseMemoryRequirements)
{
   VK_FROM_HANDLE(radv_device, device, _device);
   VK_FROM_HANDLE(radv_image, image, pInfo->image);
   struct radv_physical_device *pdev = radv_device_physical(device);

   if (!(image->vk.create_flags & VK_IMAGE_CREATE_SPARSE_BINDING_BIT)) {
      *pSparseMemoryRequirementCount = 0;
      return;
   }

   VK_OUTARRAY_MAKE_TYPED(VkSparseImageMemoryRequirements2, out, pSparseMemoryRequirements,
                          pSparseMemoryRequirementCount);

   vk_outarray_append_typed(VkSparseImageMemoryRequirements2, &out, req)
   {
      fill_sparse_image_format_properties(pdev, image->vk.image_type, image->vk.format,
                                          &req->memoryRequirements.formatProperties);
      req->memoryRequirements.imageMipTailFirstLod = image->planes[0].surface.first_mip_tail_level;

      if (req->memoryRequirements.imageMipTailFirstLod < image->vk.mip_levels) {
         if (pdev->info.gfx_level >= GFX9) {
            /* The tail is always a single tile per layer. */
            req->memoryRequirements.imageMipTailSize = 65536;
            req->memoryRequirements.imageMipTailOffset =
               image->planes[0].surface.u.gfx9.prt_level_offset[req->memoryRequirements.imageMipTailFirstLod] & ~65535;
            req->memoryRequirements.imageMipTailStride = image->planes[0].surface.u.gfx9.surf_slice_size;
         } else {
            req->memoryRequirements.imageMipTailOffset =
               (uint64_t)image->planes[0]
                  .surface.u.legacy.level[req->memoryRequirements.imageMipTailFirstLod]
                  .offset_256B *
               256;
            req->memoryRequirements.imageMipTailSize = image->size - req->memoryRequirements.imageMipTailOffset;
            req->memoryRequirements.imageMipTailStride = 0;
         }
      } else {
         req->memoryRequirements.imageMipTailSize = 0;
         req->memoryRequirements.imageMipTailOffset = 0;
         req->memoryRequirements.imageMipTailStride = 0;
      }
   };
}

VKAPI_ATTR void VKAPI_CALL
radv_GetDeviceImageSparseMemoryRequirements(VkDevice device, const VkDeviceImageMemoryRequirements *pInfo,
                                            uint32_t *pSparseMemoryRequirementCount,
                                            VkSparseImageMemoryRequirements2 *pSparseMemoryRequirements)
{
   UNUSED VkResult result;
   VkImage image;

   /* Determining the image size/alignment require to create a surface, which isn't really possible
    * without creating an image.
    */
   result =
      radv_image_create(device, &(struct radv_image_create_info){.vk_info = pInfo->pCreateInfo}, NULL, &image, true);
   assert(result == VK_SUCCESS);

   VkImageSparseMemoryRequirementsInfo2 info2 = {
      .sType = VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2,
      .image = image,
   };

   radv_GetImageSparseMemoryRequirements2(device, &info2, pSparseMemoryRequirementCount, pSparseMemoryRequirements);

   radv_DestroyImage(device, image, NULL);
}

VKAPI_ATTR void VKAPI_CALL
radv_GetPhysicalDeviceExternalBufferProperties(VkPhysicalDevice physicalDevice,
                                               const VkPhysicalDeviceExternalBufferInfo *pExternalBufferInfo,
                                               VkExternalBufferProperties *pExternalBufferProperties)
{
   VkExternalMemoryFeatureFlagBits flags = 0;
   VkExternalMemoryHandleTypeFlags export_flags = 0;
   VkExternalMemoryHandleTypeFlags compat_flags = 0;
   switch (pExternalBufferInfo->handleType) {
   case VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT:
   case VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT:
      flags = VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT | VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT;
      compat_flags = export_flags =
         VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT | VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT;
      break;
   case VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT:
      flags = VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT;
      compat_flags = VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT;
      break;
   default:
      break;
   }
   pExternalBufferProperties->externalMemoryProperties = (VkExternalMemoryProperties){
      .externalMemoryFeatures = flags,
      .exportFromImportedHandleTypes = export_flags,
      .compatibleHandleTypes = compat_flags,
   };
}

/* DCC channel type categories within which formats can be reinterpreted
 * while keeping the same DCC encoding. The swizzle must also match. */
enum dcc_channel_type {
   dcc_channel_float,
   dcc_channel_uint,
   dcc_channel_sint,
   dcc_channel_incompatible,
};

/* Return the type of DCC encoding. */
static void
radv_get_dcc_channel_type(const struct util_format_description *desc, enum dcc_channel_type *type, unsigned *size)
{
   int i = util_format_get_first_non_void_channel(desc->format);
   if (i == -1) {
      *type = dcc_channel_incompatible;
      return;
   }

   switch (desc->channel[i].size) {
   case 32:
   case 16:
   case 10:
   case 8:
      *size = desc->channel[i].size;
      if (desc->channel[i].type == UTIL_FORMAT_TYPE_FLOAT)
         *type = dcc_channel_float;
      else if (desc->channel[i].type == UTIL_FORMAT_TYPE_UNSIGNED)
         *type = dcc_channel_uint;
      else
         *type = dcc_channel_sint;
      break;
   default:
      *type = dcc_channel_incompatible;
      break;
   }
}

/* Return if it's allowed to reinterpret one format as another with DCC enabled. */
bool
radv_dcc_formats_compatible(enum amd_gfx_level gfx_level, VkFormat format1, VkFormat format2, bool *sign_reinterpret)
{
   const struct util_format_description *desc1, *desc2;
   enum dcc_channel_type type1, type2;
   unsigned size1 = 0, size2 = 0;
   int i;

   /* All formats are compatible on GFX11. */
   if (gfx_level >= GFX11)
      return true;

   if (format1 == format2)
      return true;

   desc1 = radv_format_description(format1);
   desc2 = radv_format_description(format2);

   if (desc1->nr_channels != desc2->nr_channels)
      return false;

   /* Swizzles must be the same. */
   for (i = 0; i < desc1->nr_channels; i++)
      if (desc1->swizzle[i] <= PIPE_SWIZZLE_W && desc2->swizzle[i] <= PIPE_SWIZZLE_W &&
          desc1->swizzle[i] != desc2->swizzle[i])
         return false;

   radv_get_dcc_channel_type(desc1, &type1, &size1);
   radv_get_dcc_channel_type(desc2, &type2, &size2);

   if (type1 == dcc_channel_incompatible || type2 == dcc_channel_incompatible ||
       (type1 == dcc_channel_float) != (type2 == dcc_channel_float) || size1 != size2)
      return false;

   if (type1 != type2)
      *sign_reinterpret = true;

   return true;
}

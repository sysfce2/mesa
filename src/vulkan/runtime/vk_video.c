/*
 * Copyright © 2021 Red Hat
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

#include "vk_video.h"
#include "vk_util.h"
#include "vk_log.h"
#include "vk_alloc.h"
#include "vk_device.h"
#include "util/vl_rbsp.h"
#include "util/vl_bitstream.h"

VkResult
vk_video_session_init(struct vk_device *device,
                      struct vk_video_session *vid,
                      const VkVideoSessionCreateInfoKHR *create_info)
{
   vk_object_base_init(device, &vid->base, VK_OBJECT_TYPE_VIDEO_SESSION_KHR);

   vid->flags = create_info->flags;
   vid->op = create_info->pVideoProfile->videoCodecOperation;
   vid->max_coded = create_info->maxCodedExtent;
   vid->picture_format = create_info->pictureFormat;
   vid->ref_format = create_info->referencePictureFormat;
   vid->max_dpb_slots = create_info->maxDpbSlots;
   vid->max_active_ref_pics = create_info->maxActiveReferencePictures;
   vid->luma_bit_depth = create_info->pVideoProfile->lumaBitDepth;
   vid->chroma_bit_depth = create_info->pVideoProfile->chromaBitDepth;

   switch (vid->op) {
   case VK_VIDEO_CODEC_OPERATION_DECODE_H264_BIT_KHR: {
      const struct VkVideoDecodeH264ProfileInfoKHR *h264_profile =
         vk_find_struct_const(create_info->pVideoProfile->pNext,
                              VIDEO_DECODE_H264_PROFILE_INFO_KHR);
      vid->h264.profile_idc = h264_profile->stdProfileIdc;
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_DECODE_H265_BIT_KHR: {
      const struct VkVideoDecodeH265ProfileInfoKHR *h265_profile =
         vk_find_struct_const(create_info->pVideoProfile->pNext,
                              VIDEO_DECODE_H265_PROFILE_INFO_KHR);
      vid->h265.profile_idc = h265_profile->stdProfileIdc;
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_DECODE_AV1_BIT_KHR: {
      const struct VkVideoDecodeAV1ProfileInfoKHR *av1_profile =
         vk_find_struct_const(create_info->pVideoProfile->pNext,
                              VIDEO_DECODE_AV1_PROFILE_INFO_KHR);
      vid->av1.profile = av1_profile->stdProfile;
      vid->av1.film_grain_support = av1_profile->filmGrainSupport;
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_DECODE_VP9_BIT_KHR: {
      const struct VkVideoDecodeVP9ProfileInfoKHR *vp9_profile =
         vk_find_struct_const(create_info->pVideoProfile->pNext,
                              VIDEO_DECODE_VP9_PROFILE_INFO_KHR);
      vid->vp9.profile = vp9_profile->stdProfile;
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_ENCODE_H264_BIT_KHR: {
      const struct VkVideoEncodeH264ProfileInfoKHR *h264_profile =
         vk_find_struct_const(create_info->pVideoProfile->pNext, VIDEO_ENCODE_H264_PROFILE_INFO_KHR);
      vid->h264.profile_idc = h264_profile->stdProfileIdc;
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_ENCODE_H265_BIT_KHR: {
      const struct VkVideoEncodeH265ProfileInfoKHR *h265_profile =
         vk_find_struct_const(create_info->pVideoProfile->pNext, VIDEO_ENCODE_H265_PROFILE_INFO_KHR);
      vid->h265.profile_idc = h265_profile->stdProfileIdc;
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_ENCODE_AV1_BIT_KHR: {
      const struct VkVideoEncodeAV1ProfileInfoKHR *av1_profile =
         vk_find_struct_const(create_info->pVideoProfile->pNext,
                              VIDEO_ENCODE_AV1_PROFILE_INFO_KHR);
      vid->av1.profile = av1_profile->stdProfile;
      break;
   };
   default:
      return VK_ERROR_FEATURE_NOT_PRESENT;
   }

   if (vid->op == VK_VIDEO_CODEC_OPERATION_ENCODE_H264_BIT_KHR ||
       vid->op == VK_VIDEO_CODEC_OPERATION_ENCODE_H265_BIT_KHR ||
       vid->op == VK_VIDEO_CODEC_OPERATION_ENCODE_AV1_BIT_KHR) {
      const struct VkVideoEncodeUsageInfoKHR *encode_usage_profile =
         vk_find_struct_const(create_info->pVideoProfile->pNext, VIDEO_ENCODE_USAGE_INFO_KHR);
      if (encode_usage_profile) {
         vid->enc_usage.video_usage_hints = encode_usage_profile->videoUsageHints;
         vid->enc_usage.video_content_hints = encode_usage_profile->videoContentHints;
         vid->enc_usage.tuning_mode = encode_usage_profile->tuningMode;
      } else {
         vid->enc_usage.video_usage_hints = VK_VIDEO_ENCODE_USAGE_DEFAULT_KHR;
         vid->enc_usage.video_content_hints = VK_VIDEO_ENCODE_CONTENT_DEFAULT_KHR;
         vid->enc_usage.tuning_mode = VK_VIDEO_ENCODE_TUNING_MODE_DEFAULT_KHR;
      }
   }

   return VK_SUCCESS;
}

static void
vk_video_deep_copy_h264_sps(struct vk_video_h264_sps *dst,
                            const StdVideoH264SequenceParameterSet *src)
{
   memcpy(&dst->base, src, sizeof(StdVideoH264SequenceParameterSet));
   if (src->num_ref_frames_in_pic_order_cnt_cycle && src->pOffsetForRefFrame) {
      memcpy(dst->offsets_for_ref_frame, src->pOffsetForRefFrame, sizeof(int32_t) * src->num_ref_frames_in_pic_order_cnt_cycle);
      dst->base.pOffsetForRefFrame = dst->offsets_for_ref_frame;
   }
   if (src->flags.seq_scaling_matrix_present_flag && src->pScalingLists) {
      memcpy(&dst->scaling_lists, src->pScalingLists, sizeof(StdVideoH264ScalingLists));
      dst->base.pScalingLists = &dst->scaling_lists;
   }
   if (src->flags.vui_parameters_present_flag && src->pSequenceParameterSetVui) {
      memcpy(&dst->vui, src->pSequenceParameterSetVui, sizeof(StdVideoH264SequenceParameterSetVui));
      dst->base.pSequenceParameterSetVui = &dst->vui;

      if (src->pSequenceParameterSetVui->pHrdParameters) {
         memcpy(&dst->vui_hrd_parameters, src->pSequenceParameterSetVui->pHrdParameters,
                sizeof(StdVideoH264HrdParameters));
         dst->vui.pHrdParameters = &dst->vui_hrd_parameters;
      }
   }
}

static void
vk_video_deep_copy_h264_pps(struct vk_video_h264_pps *dst,
                            const StdVideoH264PictureParameterSet *src)
{
   memcpy(&dst->base, src, sizeof(StdVideoH264PictureParameterSet));
   if (src->flags.pic_scaling_matrix_present_flag && src->pScalingLists) {
      memcpy(&dst->scaling_lists, src->pScalingLists, sizeof(StdVideoH264ScalingLists));
      dst->base.pScalingLists = &dst->scaling_lists;
   }
}

static void
vk_video_deep_copy_h265_vps(struct vk_video_h265_vps *dst,
                            const StdVideoH265VideoParameterSet *src)
{
   memcpy(&dst->base, src, sizeof(StdVideoH265VideoParameterSet));
   if (src->pDecPicBufMgr) {
      memcpy(&dst->dec_pic_buf_mgr, src->pDecPicBufMgr, sizeof(StdVideoH265DecPicBufMgr));
      dst->base.pDecPicBufMgr = &dst->dec_pic_buf_mgr;
   }
   if (src->pHrdParameters) {
      memcpy(&dst->hrd_parameters, src->pHrdParameters, sizeof(StdVideoH265HrdParameters));
      dst->base.pHrdParameters = &dst->hrd_parameters;
      if (src->pHrdParameters->pSubLayerHrdParametersNal) {
         memcpy(&dst->hrd_parameters_nal, src->pHrdParameters->pSubLayerHrdParametersNal,
                sizeof(StdVideoH265SubLayerHrdParameters));
         dst->hrd_parameters.pSubLayerHrdParametersNal = &dst->hrd_parameters_nal;
      }
      if (src->pHrdParameters->pSubLayerHrdParametersVcl) {
         memcpy(&dst->hrd_parameters_vcl, src->pHrdParameters->pSubLayerHrdParametersVcl,
                sizeof(StdVideoH265SubLayerHrdParameters));
         dst->hrd_parameters.pSubLayerHrdParametersVcl = &dst->hrd_parameters_vcl;
      }
   }

   if (src->pProfileTierLevel) {
      memcpy(&dst->tier_level, src->pProfileTierLevel, sizeof(StdVideoH265ProfileTierLevel));
      dst->base.pProfileTierLevel = &dst->tier_level;
   }
}

static void
vk_video_deep_copy_h265_sps(struct vk_video_h265_sps *dst,
                            const StdVideoH265SequenceParameterSet *src)
{
   memcpy(&dst->base, src, sizeof(StdVideoH265SequenceParameterSet));
   if (src->pProfileTierLevel) {
      memcpy(&dst->tier_level, src->pProfileTierLevel, sizeof(StdVideoH265ProfileTierLevel));
      dst->base.pProfileTierLevel = &dst->tier_level;
   }
   if (src->pDecPicBufMgr) {
      memcpy(&dst->dec_pic_buf_mgr, src->pDecPicBufMgr, sizeof(StdVideoH265DecPicBufMgr));
      dst->base.pDecPicBufMgr = &dst->dec_pic_buf_mgr;
   }
   if (src->flags.sps_scaling_list_data_present_flag && src->pScalingLists) {
      memcpy(&dst->scaling_lists, src->pScalingLists, sizeof(StdVideoH265ScalingLists));
      dst->base.pScalingLists = &dst->scaling_lists;
   }

   if (src->pShortTermRefPicSet) {
      memcpy(&dst->short_term_ref_pic_set, src->pShortTermRefPicSet, sizeof(StdVideoH265ShortTermRefPicSet));
      dst->base.pShortTermRefPicSet = &dst->short_term_ref_pic_set;
   }

   if (src->pLongTermRefPicsSps) {
      memcpy(&dst->long_term_ref_pics_sps, src->pLongTermRefPicsSps, sizeof(StdVideoH265LongTermRefPicsSps));
      dst->base.pLongTermRefPicsSps = &dst->long_term_ref_pics_sps;
   }

   if (src->pSequenceParameterSetVui) {
      memcpy(&dst->vui, src->pSequenceParameterSetVui, sizeof(StdVideoH265SequenceParameterSetVui));
      dst->base.pSequenceParameterSetVui = &dst->vui;

      if (src->pSequenceParameterSetVui->pHrdParameters) {
         memcpy(&dst->hrd_parameters, src->pSequenceParameterSetVui->pHrdParameters, sizeof(StdVideoH265HrdParameters));
         dst->vui.pHrdParameters = &dst->hrd_parameters;
         if (src->pSequenceParameterSetVui->pHrdParameters->pSubLayerHrdParametersNal) {
            memcpy(&dst->hrd_parameters_nal, src->pSequenceParameterSetVui->pHrdParameters->pSubLayerHrdParametersNal,
                   sizeof(StdVideoH265SubLayerHrdParameters));
            dst->hrd_parameters.pSubLayerHrdParametersNal = &dst->hrd_parameters_nal;
         }
         if (src->pSequenceParameterSetVui->pHrdParameters->pSubLayerHrdParametersVcl) {
            memcpy(&dst->hrd_parameters_vcl, src->pSequenceParameterSetVui->pHrdParameters->pSubLayerHrdParametersVcl,
                   sizeof(StdVideoH265SubLayerHrdParameters));
            dst->hrd_parameters.pSubLayerHrdParametersVcl = &dst->hrd_parameters_vcl;
         }
      }
   }
   if (src->flags.sps_palette_predictor_initializers_present_flag && src->pPredictorPaletteEntries) {
      memcpy(&dst->palette_entries, src->pPredictorPaletteEntries, sizeof(StdVideoH265PredictorPaletteEntries));
      dst->base.pPredictorPaletteEntries = &dst->palette_entries;
   }
}

static void
vk_video_deep_copy_h265_pps(struct vk_video_h265_pps *dst,
                            const StdVideoH265PictureParameterSet *src)
{
   memcpy(&dst->base, src, sizeof(StdVideoH265PictureParameterSet));
   if (src->flags.pps_scaling_list_data_present_flag && src->pScalingLists) {
      memcpy(&dst->scaling_lists, src->pScalingLists, sizeof(StdVideoH265ScalingLists));
      dst->base.pScalingLists = &dst->scaling_lists;
   }

   if (src->flags.pps_palette_predictor_initializers_present_flag && src->pPredictorPaletteEntries) {
      memcpy(&dst->palette_entries, src->pPredictorPaletteEntries, sizeof(StdVideoH265PredictorPaletteEntries));
      dst->base.pPredictorPaletteEntries = &dst->palette_entries;
   }
}


#define FIND(PARAMSET, SS, SET, ID)                                     \
   static struct vk_video_##SET *find_##SS##_##SET(const struct vk_video_session_parameters *params, uint32_t id) { \
      for (unsigned i = 0; i < params->SS.SET##_count; i++) {           \
         if (params->SS.SET[i].base.ID == id)                           \
            return &params->SS.SET[i];                                  \
      }                                                                 \
      return NULL;                                                      \
   }                                                                    \
                                                                        \
   static void add_##SS##_##SET(struct vk_video_session_parameters *params, \
                                const PARAMSET *new_set, bool noreplace) {  \
      struct vk_video_##SET *set = find_##SS##_##SET(params, new_set->ID);           \
      if (set) {                                                        \
         if (noreplace)                                                 \
            return;                                                     \
         vk_video_deep_copy_##SET(set, new_set);                        \
      } else                                                            \
         vk_video_deep_copy_##SET(&params->SS.SET[params->SS.SET##_count++], new_set); \
   }                                                                    \
                                                                        \
   static VkResult update_##SS##_##SET(struct vk_video_session_parameters *params, \
                                       uint32_t count, const PARAMSET *updates) { \
      if (params->SS.SET##_count + count >= params->SS.max_##SET##_count) \
         return VK_ERROR_TOO_MANY_OBJECTS;                              \
      for (unsigned _c = 0; _c < count; _c++)                           \
         vk_video_deep_copy_##SET(&params->SS.SET[params->SS.SET##_count + _c], &updates[_c]); \
      params->SS.SET##_count += count;                                  \
      return VK_SUCCESS;                                                \
   }

FIND(StdVideoH264SequenceParameterSet, h264_dec, h264_sps, seq_parameter_set_id)
FIND(StdVideoH264PictureParameterSet, h264_dec, h264_pps, pic_parameter_set_id)
FIND(StdVideoH265VideoParameterSet, h265_dec, h265_vps, vps_video_parameter_set_id)
FIND(StdVideoH265SequenceParameterSet, h265_dec, h265_sps, sps_seq_parameter_set_id)
FIND(StdVideoH265PictureParameterSet, h265_dec, h265_pps, pps_pic_parameter_set_id)

FIND(StdVideoH264SequenceParameterSet, h264_enc, h264_sps, seq_parameter_set_id)
FIND(StdVideoH264PictureParameterSet, h264_enc, h264_pps, pic_parameter_set_id)

FIND(StdVideoH265VideoParameterSet, h265_enc, h265_vps, vps_video_parameter_set_id)
FIND(StdVideoH265SequenceParameterSet, h265_enc, h265_sps, sps_seq_parameter_set_id)
FIND(StdVideoH265PictureParameterSet, h265_enc, h265_pps, pps_pic_parameter_set_id)

static void
init_add_h264_dec_session_parameters(struct vk_video_session_parameters *params,
                                     const struct VkVideoDecodeH264SessionParametersAddInfoKHR *h264_add,
                                     const struct vk_video_session_parameters *templ)
{
   unsigned i;

   if (h264_add) {
      for (i = 0; i < h264_add->stdSPSCount; i++) {
         add_h264_dec_h264_sps(params, &h264_add->pStdSPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h264_dec.h264_sps_count; i++) {
         add_h264_dec_h264_sps(params, &templ->h264_dec.h264_sps[i].base, true);
      }
   }

   if (h264_add) {
      for (i = 0; i < h264_add->stdPPSCount; i++) {
         add_h264_dec_h264_pps(params, &h264_add->pStdPPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h264_dec.h264_pps_count; i++) {
         add_h264_dec_h264_pps(params, &templ->h264_dec.h264_pps[i].base, true);
      }
   }
}

static void
init_add_h264_enc_session_parameters(struct vk_video_session_parameters *params,
                                     const struct VkVideoEncodeH264SessionParametersAddInfoKHR *h264_add,
                                     const struct vk_video_session_parameters *templ)
{
   unsigned i;
   if (h264_add) {
      for (i = 0; i < h264_add->stdSPSCount; i++) {
         add_h264_enc_h264_sps(params, &h264_add->pStdSPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h264_dec.h264_sps_count; i++) {
         add_h264_enc_h264_sps(params, &templ->h264_enc.h264_sps[i].base, true);
      }
   }

   if (h264_add) {
      for (i = 0; i < h264_add->stdPPSCount; i++) {
         add_h264_enc_h264_pps(params, &h264_add->pStdPPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h264_enc.h264_pps_count; i++) {
         add_h264_enc_h264_pps(params, &templ->h264_enc.h264_pps[i].base, true);
      }
   }
}

static void
init_add_h265_dec_session_parameters(struct vk_video_session_parameters *params,
                                 const struct VkVideoDecodeH265SessionParametersAddInfoKHR *h265_add,
                                 const struct vk_video_session_parameters *templ)
{
   unsigned i;

   if (h265_add) {
      for (i = 0; i < h265_add->stdVPSCount; i++) {
         add_h265_dec_h265_vps(params, &h265_add->pStdVPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h265_dec.h265_vps_count; i++) {
         add_h265_dec_h265_vps(params, &templ->h265_dec.h265_vps[i].base, true);
      }
   }
   if (h265_add) {
      for (i = 0; i < h265_add->stdSPSCount; i++) {
         add_h265_dec_h265_sps(params, &h265_add->pStdSPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h265_dec.h265_sps_count; i++) {
         add_h265_dec_h265_sps(params, &templ->h265_dec.h265_sps[i].base, true);
      }
   }

   if (h265_add) {
      for (i = 0; i < h265_add->stdPPSCount; i++) {
         add_h265_dec_h265_pps(params, &h265_add->pStdPPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h265_dec.h265_pps_count; i++) {
         add_h265_dec_h265_pps(params, &templ->h265_dec.h265_pps[i].base, true);
      }
   }
}

static void
init_add_h265_enc_session_parameters(struct vk_video_session_parameters *params,
                                     const struct VkVideoEncodeH265SessionParametersAddInfoKHR *h265_add,
                                     const struct vk_video_session_parameters *templ)
{
   unsigned i;

   if (h265_add) {
      for (i = 0; i < h265_add->stdVPSCount; i++) {
         add_h265_enc_h265_vps(params, &h265_add->pStdVPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h265_enc.h265_vps_count; i++) {
         add_h265_enc_h265_vps(params, &templ->h265_enc.h265_vps[i].base, true);
      }
   }
   if (h265_add) {
      for (i = 0; i < h265_add->stdSPSCount; i++) {
         add_h265_enc_h265_sps(params, &h265_add->pStdSPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h265_enc.h265_sps_count; i++) {
         add_h265_enc_h265_sps(params, &templ->h265_enc.h265_sps[i].base, true);
      }
   }

   if (h265_add) {
      for (i = 0; i < h265_add->stdPPSCount; i++) {
         add_h265_enc_h265_pps(params, &h265_add->pStdPPSs[i], false);
      }
   }
   if (templ) {
      for (i = 0; i < templ->h265_enc.h265_pps_count; i++) {
         add_h265_enc_h265_pps(params, &templ->h265_enc.h265_pps[i].base, true);
      }
   }
}

static void
vk_video_deep_copy_av1_seq_hdr(struct vk_video_av1_seq_hdr *dst,
                               const StdVideoAV1SequenceHeader *src)
{
   memcpy(&dst->base, src, sizeof(StdVideoAV1SequenceHeader));
   if (src->pColorConfig) {
      memcpy(&dst->color_config, src->pColorConfig, sizeof(StdVideoAV1ColorConfig));
      dst->base.pColorConfig = &dst->color_config;
   }
   if (src->pTimingInfo) {
      memcpy(&dst->timing_info, src->pTimingInfo, sizeof(StdVideoAV1TimingInfo));
      dst->base.pTimingInfo = &dst->timing_info;
   }
}

VkResult
vk_video_session_parameters_init(struct vk_device *device,
                                 struct vk_video_session_parameters *params,
                                 const struct vk_video_session *vid,
                                 const struct vk_video_session_parameters *templ,
                                 const VkVideoSessionParametersCreateInfoKHR *create_info)
{
   memset(params, 0, sizeof(*params));
   vk_object_base_init(device, &params->base, VK_OBJECT_TYPE_VIDEO_SESSION_PARAMETERS_KHR);

   params->op = vid->op;
   params->luma_bit_depth = vid->luma_bit_depth;
   params->chroma_bit_depth = vid->chroma_bit_depth;

   switch (vid->op) {
   case VK_VIDEO_CODEC_OPERATION_DECODE_H264_BIT_KHR: {
      const struct VkVideoDecodeH264SessionParametersCreateInfoKHR *h264_create =
         vk_find_struct_const(create_info->pNext, VIDEO_DECODE_H264_SESSION_PARAMETERS_CREATE_INFO_KHR);

      params->h264_dec.max_h264_sps_count = h264_create->maxStdSPSCount;
      params->h264_dec.max_h264_pps_count = h264_create->maxStdPPSCount;

      uint32_t sps_size = params->h264_dec.max_h264_sps_count * sizeof(struct vk_video_h264_sps);
      uint32_t pps_size = params->h264_dec.max_h264_pps_count * sizeof(struct vk_video_h264_pps);

      params->h264_dec.h264_sps = vk_alloc(&device->alloc, sps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      params->h264_dec.h264_pps = vk_alloc(&device->alloc, pps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      if (!params->h264_dec.h264_sps || !params->h264_dec.h264_pps) {
         vk_free(&device->alloc, params->h264_dec.h264_sps);
         vk_free(&device->alloc, params->h264_dec.h264_pps);
         return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);
      }

      init_add_h264_dec_session_parameters(params, h264_create->pParametersAddInfo, templ);
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_DECODE_H265_BIT_KHR: {
      const struct VkVideoDecodeH265SessionParametersCreateInfoKHR *h265_create =
         vk_find_struct_const(create_info->pNext, VIDEO_DECODE_H265_SESSION_PARAMETERS_CREATE_INFO_KHR);

      params->h265_dec.max_h265_vps_count = h265_create->maxStdVPSCount;
      params->h265_dec.max_h265_sps_count = h265_create->maxStdSPSCount;
      params->h265_dec.max_h265_pps_count = h265_create->maxStdPPSCount;

      uint32_t vps_size = params->h265_dec.max_h265_vps_count * sizeof(struct vk_video_h265_vps);
      uint32_t sps_size = params->h265_dec.max_h265_sps_count * sizeof(struct vk_video_h265_sps);
      uint32_t pps_size = params->h265_dec.max_h265_pps_count * sizeof(struct vk_video_h265_pps);

      params->h265_dec.h265_vps = vk_alloc(&device->alloc, vps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      params->h265_dec.h265_sps = vk_alloc(&device->alloc, sps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      params->h265_dec.h265_pps = vk_alloc(&device->alloc, pps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      if (!params->h265_dec.h265_sps || !params->h265_dec.h265_pps || !params->h265_dec.h265_vps) {
         vk_free(&device->alloc, params->h265_dec.h265_vps);
         vk_free(&device->alloc, params->h265_dec.h265_sps);
         vk_free(&device->alloc, params->h265_dec.h265_pps);
         return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);
      }

      init_add_h265_dec_session_parameters(params, h265_create->pParametersAddInfo, templ);
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_DECODE_AV1_BIT_KHR: {
      const struct VkVideoDecodeAV1SessionParametersCreateInfoKHR *av1_create =
         vk_find_struct_const(create_info->pNext, VIDEO_DECODE_AV1_SESSION_PARAMETERS_CREATE_INFO_KHR);
      if (av1_create && av1_create->pStdSequenceHeader) {
         vk_video_deep_copy_av1_seq_hdr(&params->av1_dec.seq_hdr,
                                        av1_create->pStdSequenceHeader);
      }
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_DECODE_VP9_BIT_KHR: {
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_ENCODE_H264_BIT_KHR: {
      const struct VkVideoEncodeH264SessionParametersCreateInfoKHR *h264_create =
         vk_find_struct_const(create_info->pNext, VIDEO_ENCODE_H264_SESSION_PARAMETERS_CREATE_INFO_KHR);

      params->h264_enc.max_h264_sps_count = h264_create->maxStdSPSCount;
      params->h264_enc.max_h264_pps_count = h264_create->maxStdPPSCount;

      uint32_t sps_size = params->h264_enc.max_h264_sps_count * sizeof(struct vk_video_h264_sps);
      uint32_t pps_size = params->h264_enc.max_h264_pps_count * sizeof(struct vk_video_h264_pps);

      params->h264_enc.h264_sps = vk_alloc(&device->alloc, sps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      params->h264_enc.h264_pps = vk_alloc(&device->alloc, pps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      if (!params->h264_enc.h264_sps || !params->h264_enc.h264_pps) {
         vk_free(&device->alloc, params->h264_enc.h264_sps);
         vk_free(&device->alloc, params->h264_enc.h264_pps);
         return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);
      }

      params->h264_enc.profile_idc = vid->h264.profile_idc;
      init_add_h264_enc_session_parameters(params, h264_create->pParametersAddInfo, templ);
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_ENCODE_H265_BIT_KHR: {
      const struct VkVideoEncodeH265SessionParametersCreateInfoKHR *h265_create =
         vk_find_struct_const(create_info->pNext, VIDEO_ENCODE_H265_SESSION_PARAMETERS_CREATE_INFO_KHR);

      params->h265_enc.max_h265_vps_count = h265_create->maxStdVPSCount;
      params->h265_enc.max_h265_sps_count = h265_create->maxStdSPSCount;
      params->h265_enc.max_h265_pps_count = h265_create->maxStdPPSCount;

      uint32_t vps_size = params->h265_enc.max_h265_vps_count * sizeof(struct vk_video_h265_vps);
      uint32_t sps_size = params->h265_enc.max_h265_sps_count * sizeof(struct vk_video_h265_sps);
      uint32_t pps_size = params->h265_enc.max_h265_pps_count * sizeof(struct vk_video_h265_pps);

      params->h265_enc.h265_vps = vk_alloc(&device->alloc, vps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      params->h265_enc.h265_sps = vk_alloc(&device->alloc, sps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      params->h265_enc.h265_pps = vk_alloc(&device->alloc, pps_size, 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
      if (!params->h265_enc.h265_sps || !params->h265_enc.h265_pps || !params->h265_enc.h265_vps) {
         vk_free(&device->alloc, params->h265_enc.h265_vps);
         vk_free(&device->alloc, params->h265_enc.h265_sps);
         vk_free(&device->alloc, params->h265_enc.h265_pps);
         return vk_error(device, VK_ERROR_OUT_OF_HOST_MEMORY);
      }

      init_add_h265_enc_session_parameters(params, h265_create->pParametersAddInfo, templ);
      break;
   }
   case VK_VIDEO_CODEC_OPERATION_ENCODE_AV1_BIT_KHR: {
      const struct VkVideoEncodeAV1SessionParametersCreateInfoKHR *av1_create =
         vk_find_struct_const(create_info->pNext, VIDEO_ENCODE_AV1_SESSION_PARAMETERS_CREATE_INFO_KHR);
      if (av1_create) {
         vk_video_deep_copy_av1_seq_hdr(&params->av1_enc.seq_hdr,
                                        av1_create->pStdSequenceHeader);
      }
      break;
   }
   default:
      UNREACHABLE("Unsupported video codec operation");
      break;
   }
   return VK_SUCCESS;
}

void
vk_video_session_parameters_finish(struct vk_device *device,
                                   struct vk_video_session_parameters *params)
{
   switch (params->op) {
   case VK_VIDEO_CODEC_OPERATION_DECODE_H264_BIT_KHR:
      vk_free(&device->alloc, params->h264_dec.h264_sps);
      vk_free(&device->alloc, params->h264_dec.h264_pps);
      break;
   case VK_VIDEO_CODEC_OPERATION_DECODE_H265_BIT_KHR:
      vk_free(&device->alloc, params->h265_dec.h265_vps);
      vk_free(&device->alloc, params->h265_dec.h265_sps);
      vk_free(&device->alloc, params->h265_dec.h265_pps);
      break;
   case VK_VIDEO_CODEC_OPERATION_ENCODE_H264_BIT_KHR:
      vk_free(&device->alloc, params->h264_enc.h264_sps);
      vk_free(&device->alloc, params->h264_enc.h264_pps);
      break;
   case VK_VIDEO_CODEC_OPERATION_ENCODE_H265_BIT_KHR:
      vk_free(&device->alloc, params->h265_enc.h265_vps);
      vk_free(&device->alloc, params->h265_enc.h265_sps);
      vk_free(&device->alloc, params->h265_enc.h265_pps);
      break;
   default:
      break;
   }
   vk_object_base_finish(&params->base);
}

static VkResult
update_h264_dec_session_parameters(struct vk_video_session_parameters *params,
                                   const struct VkVideoDecodeH264SessionParametersAddInfoKHR *h264_add)
{
   VkResult result = VK_SUCCESS;

   result = update_h264_dec_h264_sps(params, h264_add->stdSPSCount, h264_add->pStdSPSs);
   if (result != VK_SUCCESS)
      return result;

   result = update_h264_dec_h264_pps(params, h264_add->stdPPSCount, h264_add->pStdPPSs);
   return result;
}

static VkResult
update_h264_enc_session_parameters(struct vk_video_session_parameters *params,
                                  const struct VkVideoEncodeH264SessionParametersAddInfoKHR *h264_add)
{
   VkResult result = VK_SUCCESS;
   result = update_h264_enc_h264_sps(params, h264_add->stdSPSCount, h264_add->pStdSPSs);
   if (result != VK_SUCCESS)
      return result;

   result = update_h264_enc_h264_pps(params, h264_add->stdPPSCount, h264_add->pStdPPSs);
   return result;
}

static VkResult
update_h265_enc_session_parameters(struct vk_video_session_parameters *params,
                                   const struct VkVideoEncodeH265SessionParametersAddInfoKHR *h265_add)
{
   VkResult result = VK_SUCCESS;

   result = update_h265_enc_h265_vps(params, h265_add->stdVPSCount, h265_add->pStdVPSs);
   if (result != VK_SUCCESS)
      return result;

   result = update_h265_enc_h265_sps(params, h265_add->stdSPSCount, h265_add->pStdSPSs);
   if (result != VK_SUCCESS)
      return result;

   result = update_h265_enc_h265_pps(params, h265_add->stdPPSCount, h265_add->pStdPPSs);
   return result;
}

static VkResult
update_h265_session_parameters(struct vk_video_session_parameters *params,
                               const struct VkVideoDecodeH265SessionParametersAddInfoKHR *h265_add)
{
   VkResult result = VK_SUCCESS;
   result = update_h265_dec_h265_vps(params, h265_add->stdVPSCount, h265_add->pStdVPSs);
   if (result != VK_SUCCESS)
      return result;

   result = update_h265_dec_h265_sps(params, h265_add->stdSPSCount, h265_add->pStdSPSs);
   if (result != VK_SUCCESS)
      return result;

   result = update_h265_dec_h265_pps(params, h265_add->stdPPSCount, h265_add->pStdPPSs);
   return result;
}

void
vk_video_get_h264_parameters(const struct vk_video_session *session,
                             const struct vk_video_session_parameters *params,
                             const VkVideoDecodeInfoKHR *decode_info,
                             const VkVideoDecodeH264PictureInfoKHR *h264_pic_info,
                             const StdVideoH264SequenceParameterSet **sps_p,
                             const StdVideoH264PictureParameterSet **pps_p)
{
   const StdVideoH264SequenceParameterSet *sps = NULL;
   const StdVideoH264PictureParameterSet *pps = NULL;

   if (session->flags & VK_VIDEO_SESSION_CREATE_INLINE_SESSION_PARAMETERS_BIT_KHR) {
      const struct VkVideoDecodeH264InlineSessionParametersInfoKHR *inline_params =
         vk_find_struct_const(decode_info->pNext, VIDEO_DECODE_H264_INLINE_SESSION_PARAMETERS_INFO_KHR);

      if (inline_params) {
         sps = inline_params->pStdSPS;
         pps = inline_params->pStdPPS;
      }
   }

   if (!sps)
      sps = vk_video_find_h264_dec_std_sps(params, h264_pic_info->pStdPictureInfo->seq_parameter_set_id);
   if (!pps)
      pps = vk_video_find_h264_dec_std_pps(params, h264_pic_info->pStdPictureInfo->pic_parameter_set_id);

   *sps_p = sps;
   *pps_p = pps;
}

void
vk_video_get_h265_parameters(const struct vk_video_session *session,
                             const struct vk_video_session_parameters *params,
                             const VkVideoDecodeInfoKHR *decode_info,
                             const VkVideoDecodeH265PictureInfoKHR *h265_pic_info,
                             const StdVideoH265SequenceParameterSet **sps_p,
                             const StdVideoH265PictureParameterSet **pps_p)
{
   const StdVideoH265SequenceParameterSet *sps = NULL;
   const StdVideoH265PictureParameterSet *pps = NULL;

   if (session->flags & VK_VIDEO_SESSION_CREATE_INLINE_SESSION_PARAMETERS_BIT_KHR) {
      const struct VkVideoDecodeH265InlineSessionParametersInfoKHR *inline_params =
         vk_find_struct_const(decode_info->pNext, VIDEO_DECODE_H265_INLINE_SESSION_PARAMETERS_INFO_KHR);

      if (inline_params) {
         sps = inline_params->pStdSPS;
         pps = inline_params->pStdPPS;
      }
   }

   if (!sps)
      sps = vk_video_find_h265_dec_std_sps(params, h265_pic_info->pStdPictureInfo->pps_seq_parameter_set_id);
   if (!pps)
      pps = vk_video_find_h265_dec_std_pps(params, h265_pic_info->pStdPictureInfo->pps_pic_parameter_set_id);

   *sps_p = sps;
   *pps_p = pps;
}

void
vk_video_get_av1_parameters(const struct vk_video_session *session,
                            const struct vk_video_session_parameters *params,
                            const VkVideoDecodeInfoKHR *decode_info,
                            const StdVideoAV1SequenceHeader **seq_hdr_p)
{
   const StdVideoAV1SequenceHeader *seq_hdr = NULL;

   if (session->flags & VK_VIDEO_SESSION_CREATE_INLINE_SESSION_PARAMETERS_BIT_KHR) {
      const struct VkVideoDecodeAV1InlineSessionParametersInfoKHR *inline_params =
         vk_find_struct_const(decode_info->pNext, VIDEO_DECODE_AV1_INLINE_SESSION_PARAMETERS_INFO_KHR);

      if (inline_params) {
         seq_hdr = inline_params->pStdSequenceHeader;
      }
   }

   if (!seq_hdr)
      seq_hdr = &params->av1_dec.seq_hdr.base;

   *seq_hdr_p = seq_hdr;
}


VkResult
vk_video_session_parameters_update(struct vk_video_session_parameters *params,
                                   const VkVideoSessionParametersUpdateInfoKHR *update)
{
   /* 39.6.5. Decoder Parameter Sets -
    * "The provided H.264 SPS/PPS parameters must be within the limits specified during decoder
    * creation for the decoder specified in VkVideoSessionParametersCreateInfoKHR."
    */

   /*
    * There is no need to deduplicate here.
    * videoSessionParameters must not already contain a StdVideoH264PictureParameterSet entry with
    * both seq_parameter_set_id and pic_parameter_set_id matching any of the elements of
    * VkVideoDecodeH264SessionParametersAddInfoKHR::pStdPPS
    */
   VkResult result = VK_SUCCESS;

   switch (params->op) {
   case VK_VIDEO_CODEC_OPERATION_DECODE_H264_BIT_KHR: {
      const struct VkVideoDecodeH264SessionParametersAddInfoKHR *h264_add =
         vk_find_struct_const(update->pNext, VIDEO_DECODE_H264_SESSION_PARAMETERS_ADD_INFO_KHR);
      return update_h264_dec_session_parameters(params, h264_add);
   }
   case VK_VIDEO_CODEC_OPERATION_DECODE_H265_BIT_KHR: {
      const struct VkVideoDecodeH265SessionParametersAddInfoKHR *h265_add =
         vk_find_struct_const(update->pNext, VIDEO_DECODE_H265_SESSION_PARAMETERS_ADD_INFO_KHR);

      return update_h265_session_parameters(params, h265_add);
   }
   case VK_VIDEO_CODEC_OPERATION_ENCODE_H264_BIT_KHR: {
      const struct VkVideoEncodeH264SessionParametersAddInfoKHR *h264_add =
        vk_find_struct_const(update->pNext, VIDEO_ENCODE_H264_SESSION_PARAMETERS_ADD_INFO_KHR);
      return update_h264_enc_session_parameters(params, h264_add);
   }
   case VK_VIDEO_CODEC_OPERATION_ENCODE_H265_BIT_KHR: {
      const struct VkVideoEncodeH265SessionParametersAddInfoKHR *h265_add =
        vk_find_struct_const(update->pNext, VIDEO_ENCODE_H265_SESSION_PARAMETERS_ADD_INFO_KHR);
      return update_h265_enc_session_parameters(params, h265_add);
   }
   default:
      UNREACHABLE("Unknown codec\n");
   }
   return result;
}

const uint8_t h264_scaling_list_default_4x4_intra[] =
{
   /* Table 7-3 - Default_4x4_Intra */
   6, 13, 13, 20, 20, 20, 28, 28, 28, 28, 32, 32, 32, 37, 37, 42
};

const uint8_t h264_scaling_list_default_4x4_inter[] =
{
   /* Table 7-3 - Default_4x4_Inter */
   10, 14, 14, 20, 20, 20, 24, 24, 24, 24, 27, 27, 27, 30, 30, 34
};

const uint8_t h264_scaling_list_default_8x8_intra[] =
{
   /* Table 7-4 - Default_8x8_Intra */
   6,  10, 10, 13, 11, 13, 16, 16, 16, 16, 18, 18, 18, 18, 18, 23,
   23, 23, 23, 23, 23, 25, 25, 25, 25, 25, 25, 25, 27, 27, 27, 27,
   27, 27, 27, 27, 29, 29, 29, 29, 29, 29, 29, 31, 31, 31, 31, 31,
   31, 33, 33, 33, 33, 33, 36, 36, 36, 36, 38, 38, 38, 40, 40, 42,
};

const uint8_t h264_scaling_list_default_8x8_inter[] =
{
   /* Table 7-4 - Default_8x8_Inter */
   9 , 13, 13, 15, 13, 15, 17, 17, 17, 17, 19, 19, 19, 19, 19, 21,
   21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 24, 24, 24, 24,
   24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 27, 27, 27, 27, 27,
   27, 28, 28, 28, 28, 28, 30, 30, 30, 30, 32, 32, 32, 33, 33, 35,
};

void
vk_video_derive_h264_scaling_list(const StdVideoH264SequenceParameterSet *sps,
                                  const StdVideoH264PictureParameterSet *pps,
                                  StdVideoH264ScalingLists *list)
{
   StdVideoH264ScalingLists temp;

   /* derive SPS scaling list first, because PPS may depend on it in fall-back
    * rule B */
   if (sps->flags.seq_scaling_matrix_present_flag)
   {
      for (int i = 0; i < STD_VIDEO_H264_SCALING_LIST_4X4_NUM_LISTS; i++)
      {
         if (sps->pScalingLists->scaling_list_present_mask & (1 << i))
         {
            if (sps->pScalingLists->use_default_scaling_matrix_mask & (1 << i))
               memcpy(temp.ScalingList4x4[i],
                      (i < 3) ? h264_scaling_list_default_4x4_intra : h264_scaling_list_default_4x4_inter,
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
            else
               memcpy(temp.ScalingList4x4[i],
                      sps->pScalingLists->ScalingList4x4[i],
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
         }
         else /* fall-back rule A */
         {
            if (i == 0)
               memcpy(temp.ScalingList4x4[i],
                      h264_scaling_list_default_4x4_intra,
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
            else if (i == 3)
               memcpy(temp.ScalingList4x4[i],
                      h264_scaling_list_default_4x4_inter,
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
            else
               memcpy(temp.ScalingList4x4[i],
                      temp.ScalingList4x4[i - 1],
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
         }
      }

      for (int j = 0; j < STD_VIDEO_H264_SCALING_LIST_8X8_NUM_LISTS; j++)
      {
         int i = j + STD_VIDEO_H264_SCALING_LIST_4X4_NUM_LISTS;
         if (sps->pScalingLists->scaling_list_present_mask & (1 << i))
         {
            if (sps->pScalingLists->use_default_scaling_matrix_mask & (1 << i))
               memcpy(temp.ScalingList8x8[j],
                      (i == 6 || i == 8 || i == 10) ? h264_scaling_list_default_8x8_intra : h264_scaling_list_default_8x8_inter,
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
            else
               memcpy(temp.ScalingList8x8[j], sps->pScalingLists->ScalingList8x8[j],
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
         }
         else /* fall-back rule A */
         {
            if (i == 6)
               memcpy(temp.ScalingList8x8[j],
                      h264_scaling_list_default_8x8_intra,
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
            else if (i == 7)
               memcpy(temp.ScalingList8x8[j],
                      h264_scaling_list_default_8x8_inter,
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
            else
               memcpy(temp.ScalingList8x8[j], temp.ScalingList8x8[j - 2],
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
         }
      }
   }
   else
   {
      memset(temp.ScalingList4x4, 0x10,
             STD_VIDEO_H264_SCALING_LIST_4X4_NUM_LISTS *
             STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
      memset(temp.ScalingList8x8, 0x10,
             STD_VIDEO_H264_SCALING_LIST_8X8_NUM_LISTS *
             STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
   }

   if (pps->flags.pic_scaling_matrix_present_flag)
   {
      for (int i = 0; i < STD_VIDEO_H264_SCALING_LIST_4X4_NUM_LISTS; i++)
      {
         if (pps->pScalingLists->scaling_list_present_mask & (1 << i))
         {
            if (pps->pScalingLists->use_default_scaling_matrix_mask & (1 << i))
               memcpy(list->ScalingList4x4[i],
                      (i < 3) ? h264_scaling_list_default_4x4_intra : h264_scaling_list_default_4x4_inter,
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
            else
               memcpy(list->ScalingList4x4[i],
                      pps->pScalingLists->ScalingList4x4[i],
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
         }
         else if (sps->flags.seq_scaling_matrix_present_flag) /* fall-back rule B */
         {
            if (i == 0 || i == 3)
               memcpy(list->ScalingList4x4[i], temp.ScalingList4x4[i],
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
            else
               memcpy(list->ScalingList4x4[i], list->ScalingList4x4[i - 1],
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
         }
         else /* fall-back rule A */
         {
            if (i == 0)
               memcpy(list->ScalingList4x4[i],
                      h264_scaling_list_default_4x4_intra,
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
            else if (i == 3)
               memcpy(list->ScalingList4x4[i],
                      h264_scaling_list_default_4x4_inter,
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
            else
               memcpy(list->ScalingList4x4[i],
                      list->ScalingList4x4[i - 1],
                      STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
         }
      }

      for (int j = 0; j < STD_VIDEO_H264_SCALING_LIST_8X8_NUM_LISTS; j++)
      {
         int i = j + STD_VIDEO_H264_SCALING_LIST_4X4_NUM_LISTS;
         if (pps->pScalingLists->scaling_list_present_mask & (1 << i))
         {
            if (pps->pScalingLists->use_default_scaling_matrix_mask & (1 << i))
               memcpy(list->ScalingList8x8[j],
                      (i == 6 || i == 8 || i == 10) ? h264_scaling_list_default_8x8_intra : h264_scaling_list_default_8x8_inter,
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
            else
               memcpy(list->ScalingList8x8[j],
                      pps->pScalingLists->ScalingList8x8[j],
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
         }
         else if (sps->flags.seq_scaling_matrix_present_flag) /* fall-back rule B */
         {
            if (i == 6 || i == 7)
               memcpy(list->ScalingList8x8[j], temp.ScalingList8x8[j],
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
            else
               memcpy(list->ScalingList8x8[j], list->ScalingList8x8[j - 2],
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
         }
         else /* fall-back rule A */
         {
            if (i == 6)
               memcpy(list->ScalingList8x8[j],
                      h264_scaling_list_default_8x8_intra,
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
            else if (i == 7)
               memcpy(list->ScalingList8x8[j],
                      h264_scaling_list_default_8x8_inter,
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
            else
               memcpy(list->ScalingList8x8[j], list->ScalingList8x8[j - 2],
                      STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
         }
      }
   }
   else
   {
      memcpy(list->ScalingList4x4, temp.ScalingList4x4,
            STD_VIDEO_H264_SCALING_LIST_4X4_NUM_LISTS *
            STD_VIDEO_H264_SCALING_LIST_4X4_NUM_ELEMENTS);
      memcpy(list->ScalingList8x8, temp.ScalingList8x8,
            STD_VIDEO_H264_SCALING_LIST_8X8_NUM_LISTS *
            STD_VIDEO_H264_SCALING_LIST_8X8_NUM_ELEMENTS);
   }
}

const static struct StdVideoH265ScalingLists h265_scaling_list_default =
{
   .ScalingList4x4 =
   {
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16}
   },
   .ScalingList8x8 =
   {
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 17, 16, 17, 18,
       17, 18, 18, 17, 18, 21, 19, 20, 21, 20, 19, 21, 24, 22, 22, 24,
       24, 22, 22, 24, 25, 25, 27, 30, 27, 25, 25, 29, 31, 35, 35, 31,
       29, 36, 41, 44, 41, 36, 47, 54, 54, 47, 65, 70, 65, 88, 88, 115},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 17, 16, 17, 18,
       17, 18, 18, 17, 18, 21, 19, 20, 21, 20, 19, 21, 24, 22, 22, 24,
       24, 22, 22, 24, 25, 25, 27, 30, 27, 25, 25, 29, 31, 35, 35, 31,
       29, 36, 41, 44, 41, 36, 47, 54, 54, 47, 65, 70, 65, 88, 88, 115},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 17, 16, 17, 18,
       17, 18, 18, 17, 18, 21, 19, 20, 21, 20, 19, 21, 24, 22, 22, 24,
       24, 22, 22, 24, 25, 25, 27, 30, 27, 25, 25, 29, 31, 35, 35, 31,
       29, 36, 41, 44, 41, 36, 47, 54, 54, 47, 65, 70, 65, 88, 88, 115},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18,
       18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 24, 24, 24, 24,
       24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28,
       28, 33, 33, 33, 33, 33, 41, 41, 41, 41, 54, 54, 54, 71, 71, 91},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18,
       18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 24, 24, 24, 24,
       24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28,
       28, 33, 33, 33, 33, 33, 41, 41, 41, 41, 54, 54, 54, 71, 71, 91},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18,
       18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 24, 24, 24, 24,
       24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28,
       28, 33, 33, 33, 33, 33, 41, 41, 41, 41, 54, 54, 54, 71, 71, 91},
   },
   .ScalingList16x16 =
   {
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 17, 16, 17, 18,
       17, 18, 18, 17, 18, 21, 19, 20, 21, 20, 19, 21, 24, 22, 22, 24,
       24, 22, 22, 24, 25, 25, 27, 30, 27, 25, 25, 29, 31, 35, 35, 31,
       29, 36, 41, 44, 41, 36, 47, 54, 54, 47, 65, 70, 65, 88, 88, 115},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 17, 16, 17, 18,
       17, 18, 18, 17, 18, 21, 19, 20, 21, 20, 19, 21, 24, 22, 22, 24,
       24, 22, 22, 24, 25, 25, 27, 30, 27, 25, 25, 29, 31, 35, 35, 31,
       29, 36, 41, 44, 41, 36, 47, 54, 54, 47, 65, 70, 65, 88, 88, 115},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 17, 16, 17, 18,
       17, 18, 18, 17, 18, 21, 19, 20, 21, 20, 19, 21, 24, 22, 22, 24,
       24, 22, 22, 24, 25, 25, 27, 30, 27, 25, 25, 29, 31, 35, 35, 31,
       29, 36, 41, 44, 41, 36, 47, 54, 54, 47, 65, 70, 65, 88, 88, 115},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18,
       18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 24, 24, 24, 24,
       24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28,
       28, 33, 33, 33, 33, 33, 41, 41, 41, 41, 54, 54, 54, 71, 71, 91},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18,
       18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 24, 24, 24, 24,
       24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28,
       28, 33, 33, 33, 33, 33, 41, 41, 41, 41, 54, 54, 54, 71, 71, 91},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18,
       18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 24, 24, 24, 24,
       24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28,
       28, 33, 33, 33, 33, 33, 41, 41, 41, 41, 54, 54, 54, 71, 71, 91},
   },
   .ScalingList32x32 =
   {
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 17, 16, 17, 18,
       17, 18, 18, 17, 18, 21, 19, 20, 21, 20, 19, 21, 24, 22, 22, 24,
       24, 22, 22, 24, 25, 25, 27, 30, 27, 25, 25, 29, 31, 35, 35, 31,
       29, 36, 41, 44, 41, 36, 47, 54, 54, 47, 65, 70, 65, 88, 88, 115},
      {16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18,
       18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 24, 24, 24, 24,
       24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 28, 28, 28, 28, 28,
       28, 33, 33, 33, 33, 33, 41, 41, 41, 41, 54, 54, 54, 71, 71, 91}
   },
   .ScalingListDCCoef16x16 = {16, 16, 16, 16, 16, 16},
   .ScalingListDCCoef32x32 = {16, 16},
};


void
vk_video_derive_h265_scaling_list(const StdVideoH265SequenceParameterSet *sps,
                                  const StdVideoH265PictureParameterSet *pps,
                                  const StdVideoH265ScalingLists **list)
{
   if (pps->flags.pps_scaling_list_data_present_flag)
      *list = pps->pScalingLists;
   else if (sps->flags.sps_scaling_list_data_present_flag)
      *list = sps->pScalingLists;
   else if (sps->flags.scaling_list_enabled_flag)
      *list = &h265_scaling_list_default;
   else
      *list = NULL;
}

const StdVideoH264SequenceParameterSet *
vk_video_find_h264_dec_std_sps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h264_dec_h264_sps(params, id)->base;
}

const StdVideoH264PictureParameterSet *
vk_video_find_h264_dec_std_pps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h264_dec_h264_pps(params, id)->base;
}

const StdVideoH265VideoParameterSet *
vk_video_find_h265_dec_std_vps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h265_dec_h265_vps(params, id)->base;
}

const StdVideoH265SequenceParameterSet *
vk_video_find_h265_dec_std_sps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h265_dec_h265_sps(params, id)->base;
}

const StdVideoH265PictureParameterSet *
vk_video_find_h265_dec_std_pps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h265_dec_h265_pps(params, id)->base;
}

int
vk_video_h265_poc_by_slot(const struct VkVideoDecodeInfoKHR *frame_info, int slot)
{
   for (unsigned i = 0; i < frame_info->referenceSlotCount; i++) {
      const VkVideoDecodeH265DpbSlotInfoKHR *dpb_slot_info =
         vk_find_struct_const(frame_info->pReferenceSlots[i].pNext, VIDEO_DECODE_H265_DPB_SLOT_INFO_KHR);
      if (frame_info->pReferenceSlots[i].slotIndex == slot)
         return dpb_slot_info->pStdReferenceInfo->PicOrderCntVal;
   }

   assert(0);

   return 0;
}

void
vk_fill_video_h265_reference_info(const VkVideoDecodeInfoKHR *frame_info,
                                  const struct VkVideoDecodeH265PictureInfoKHR *pic,
                                  const struct vk_video_h265_slice_params *slice_params,
                                  struct vk_video_h265_reference ref_slots[][8])
{
   uint8_t list_cnt = slice_params->slice_type == STD_VIDEO_H265_SLICE_TYPE_B ? 2 : 1;
   uint8_t list_idx;
   int i, j;

   for (list_idx = 0; list_idx < list_cnt; list_idx++) {
      /* The order is
       *  L0: Short term current before set - Short term current after set - long term current
       *  L1: Short term current after set - short term current before set - long term current
       */
      const uint8_t *rps[3] = {
         list_idx ? pic->pStdPictureInfo->RefPicSetStCurrAfter : pic->pStdPictureInfo->RefPicSetStCurrBefore,
         list_idx ? pic->pStdPictureInfo->RefPicSetStCurrBefore : pic->pStdPictureInfo->RefPicSetStCurrAfter,
         pic->pStdPictureInfo->RefPicSetLtCurr
      };

      uint8_t ref_idx = 0;
      for (i = 0; i < 3; i++) {
         const uint8_t *cur_rps = rps[i];

         for (j = 0; (cur_rps[j] != 0xff) && ((j + ref_idx) < 8); j++) {
            ref_slots[list_idx][j + ref_idx].slot_index = cur_rps[j];
            ref_slots[list_idx][j + ref_idx].pic_order_cnt = vk_video_h265_poc_by_slot(frame_info, cur_rps[j]);
         }
         ref_idx += j;
      }

      /* TODO: should handle cases where rpl_modification_flag is true. */
      assert(!slice_params->rpl_modification_flag[0] && !slice_params->rpl_modification_flag[1]);
   }
}

static void
h265_pred_weight_table(struct vk_video_h265_slice_params *params,
                       struct vl_rbsp *rbsp,
                       const StdVideoH265SequenceParameterSet *sps,
                       StdVideoH265SliceType slice_type)
{
   unsigned chroma_array_type = sps->flags.separate_colour_plane_flag ? 0 : sps->chroma_format_idc;
   unsigned i, j;

   params->luma_log2_weight_denom = vl_rbsp_ue(rbsp);

   assert(params->luma_log2_weight_denom >= 0 && params->luma_log2_weight_denom < 8);

   if (chroma_array_type != 0) {
      params->chroma_log2_weight_denom = params->luma_log2_weight_denom + vl_rbsp_se(rbsp);
      assert(params->chroma_log2_weight_denom >= 0 && params->chroma_log2_weight_denom < 8);
   }

   for (i = 0; i < params->num_ref_idx_l0_active; ++i) {
      params->luma_weight_l0_flag[i] = vl_rbsp_u(rbsp, 1);
      if (!params->luma_weight_l0_flag[i]) {
         params->luma_weight_l0[i] = 1 << params->luma_log2_weight_denom;
         params->luma_offset_l0[i] = 0;
      }
   }

   for (i = 0; i < params->num_ref_idx_l0_active; ++i) {
      if (chroma_array_type == 0) {
         params->chroma_weight_l0_flag[i] = 0;
      } else {
         params->chroma_weight_l0_flag[i] = vl_rbsp_u(rbsp, 1);
      }
   }

   for (i = 0; i < params->num_ref_idx_l0_active; ++i) {
      if (params->luma_weight_l0_flag[i]) {
         params->delta_luma_weight_l0[i] = vl_rbsp_se(rbsp);
         params->luma_weight_l0[i] = (1 << params->luma_log2_weight_denom) + params->delta_luma_weight_l0[i];
         params->luma_offset_l0[i] = vl_rbsp_se(rbsp);
      }

      if (params->chroma_weight_l0_flag[i]) {
         for (j = 0; j < 2; j++) {
            params->delta_chroma_weight_l0[i][j] = vl_rbsp_se(rbsp);
            params->delta_chroma_offset_l0[i][j] = vl_rbsp_se(rbsp);

            params->chroma_weight_l0[i][j] =
               (1 << params->chroma_log2_weight_denom) + params->delta_chroma_weight_l0[i][j];
            params->chroma_offset_l0[i][j] = CLAMP(params->delta_chroma_offset_l0[i][j] -
               ((128 * params->chroma_weight_l0[i][j]) >> params->chroma_log2_weight_denom) + 128, -128, 127);
         }
      } else {
         for (j = 0; j < 2; j++) {
            params->chroma_weight_l0[i][j] = 1 << params->chroma_log2_weight_denom;
            params->chroma_offset_l0[i][j] = 0;
         }
      }
   }

   if (slice_type == STD_VIDEO_H265_SLICE_TYPE_B) {
      for (i = 0; i < params->num_ref_idx_l1_active; ++i) {
         params->luma_weight_l1_flag[i] = vl_rbsp_u(rbsp, 1);
         if (!params->luma_weight_l1_flag[i]) {
            params->luma_weight_l1[i] = 1 << params->luma_log2_weight_denom;
            params->luma_offset_l1[i] = 0;
         }
      }

      for (i = 0; i < params->num_ref_idx_l1_active; ++i) {
         if (chroma_array_type == 0) {
            params->chroma_weight_l1_flag[i] = 0;
         } else {
            params->chroma_weight_l1_flag[i] = vl_rbsp_u(rbsp, 1);
         }
      }

      for (i = 0; i < params->num_ref_idx_l1_active; ++i) {
         if (params->luma_weight_l1_flag[i]) {
            params->delta_luma_weight_l1[i] = vl_rbsp_se(rbsp);
            params->luma_weight_l1[i] =
               (1 << params->luma_log2_weight_denom) + params->delta_luma_weight_l1[i];
            params->luma_offset_l1[i] = vl_rbsp_se(rbsp);
         }

         if (params->chroma_weight_l1_flag[i]) {
            for (j = 0; j < 2; j++) {
               params->delta_chroma_weight_l1[i][j] = vl_rbsp_se(rbsp);
               params->delta_chroma_offset_l1[i][j] = vl_rbsp_se(rbsp);

               params->chroma_weight_l1[i][j] =
                  (1 << params->chroma_log2_weight_denom) + params->delta_chroma_weight_l1[i][j];
               params->chroma_offset_l1[i][j] = CLAMP(params->delta_chroma_offset_l1[i][j] -
                  ((128 * params->chroma_weight_l1[i][j]) >> params->chroma_log2_weight_denom) + 128, -128, 127);
            }
         } else {
            for (j = 0; j < 2; j++) {
               params->chroma_weight_l1[i][j] = 1 << params->chroma_log2_weight_denom;
               params->chroma_offset_l1[i][j] = 0;
            }
         }
      }
   }
}

void
vk_video_parse_h265_slice_header(const struct VkVideoDecodeInfoKHR *frame_info,
                                 const VkVideoDecodeH265PictureInfoKHR *pic_info,
                                 const StdVideoH265SequenceParameterSet *sps,
                                 const StdVideoH265PictureParameterSet *pps,
                                 void *slice_data,
                                 uint32_t slice_size,
                                 struct vk_video_h265_slice_params *params)
{
   struct vl_vlc vlc;
   const void *slice_headers[1] = { slice_data };
   vl_vlc_init(&vlc, 1, slice_headers, &slice_size);

   assert(vl_vlc_peekbits(&vlc, 24) == 0x000001);

   vl_vlc_eatbits(&vlc, 24);

   /* forbidden_zero_bit */
   vl_vlc_eatbits(&vlc, 1);

   if (vl_vlc_valid_bits(&vlc) < 15)
      vl_vlc_fillbits(&vlc);

   vl_vlc_get_uimsbf(&vlc, 6); /* nal_unit_type */
   vl_vlc_get_uimsbf(&vlc, 6); /* nuh_layer_id */
   vl_vlc_get_uimsbf(&vlc, 3); /* nuh_temporal_id_plus1 */

   struct vl_rbsp rbsp;
   vl_rbsp_init(&rbsp, &vlc, 128, /* emulation_bytes */ true);

   memset(params, 0, sizeof(*params));

   params->slice_size = slice_size;
   params->first_slice_segment_in_pic_flag = vl_rbsp_u(&rbsp, 1);

   /* no_output_of_prior_pics_flag */
   if (pic_info->pStdPictureInfo->flags.IrapPicFlag)
      vl_rbsp_u(&rbsp, 1);

   /* pps id */
   vl_rbsp_ue(&rbsp);

   if (!params->first_slice_segment_in_pic_flag) {
      int size, num;
      int bits_slice_segment_address = 0;

      if (pps->flags.dependent_slice_segments_enabled_flag)
         params->dependent_slice_segment = vl_rbsp_u(&rbsp, 1);

      size = 1 << (sps->log2_min_luma_coding_block_size_minus3 + 3 +
                   sps->log2_diff_max_min_luma_coding_block_size);

      num = ((sps->pic_width_in_luma_samples + size - 1) / size) *
            ((sps->pic_height_in_luma_samples + size - 1) / size);

      while (num > (1 << bits_slice_segment_address))
         bits_slice_segment_address++;

      /* slice_segment_address */
      params->slice_segment_address = vl_rbsp_u(&rbsp, bits_slice_segment_address);
   }

   if (params->dependent_slice_segment)
      return;

   for (unsigned i = 0; i < pps->num_extra_slice_header_bits; ++i)
      /* slice_reserved_flag */
      vl_rbsp_u(&rbsp, 1);

   /* slice_type */
   params->slice_type = vl_rbsp_ue(&rbsp);

   if (pps->flags.output_flag_present_flag)
      /* pic output flag */
      vl_rbsp_u(&rbsp, 1);

   if (sps->flags.separate_colour_plane_flag)
      /* colour_plane_id */
      vl_rbsp_u(&rbsp, 2);

   if (!pic_info->pStdPictureInfo->flags.IdrPicFlag) {
      /* slice_pic_order_cnt_lsb */
      params->pic_order_cnt_lsb =
         vl_rbsp_u(&rbsp, sps->log2_max_pic_order_cnt_lsb_minus4 + 4);

      /* short_term_ref_pic_set_sps_flag */
      if (!vl_rbsp_u(&rbsp, 1)) {
         uint8_t rps_predict = 0;

         if (sps->num_short_term_ref_pic_sets)
            rps_predict = vl_rbsp_u(&rbsp, 1);

         if (rps_predict) {
            /* delta_idx */
            vl_rbsp_ue(&rbsp);
            /* delta_rps_sign */
            vl_rbsp_u(&rbsp, 1);
            /* abs_delta_rps */
            vl_rbsp_ue(&rbsp);

            for (int i = 0 ; i <= pic_info->pStdPictureInfo->NumDeltaPocsOfRefRpsIdx; i++) {
               uint8_t used = vl_rbsp_u(&rbsp, 1);
               if (!used)
                  vl_rbsp_u(&rbsp, 1);
            }
         } else {
            /* num_negative_pics */
            unsigned num_neg_pics = vl_rbsp_ue(&rbsp);
            /* num_positive_pics */
            unsigned num_pos_pics = vl_rbsp_ue(&rbsp);

            for(unsigned i = 0 ; i < num_neg_pics; ++i) {
               /* delta_poc_s0_minus1 */
               vl_rbsp_ue(&rbsp);
               /* used_by_curr_pic_s0_flag */
               vl_rbsp_u(&rbsp, 1);
            }

            for(unsigned i = 0; i < num_pos_pics; ++i) {
               /* delta_poc_s1_minus1 */
               vl_rbsp_ue(&rbsp);
               /* used_by_curr_pic_s0_flag */
               vl_rbsp_u(&rbsp, 1);
            }
         }

      } else {
         unsigned num_st_rps = sps->num_short_term_ref_pic_sets;

         int numbits = util_logbase2_ceil(num_st_rps);
         if (numbits > 0)
            /* short_term_ref_pic_set_idx */
            vl_rbsp_u(&rbsp, numbits);
      }

      if (sps->flags.long_term_ref_pics_present_flag) {
         unsigned num_lt_sps = 0;

         if (sps->num_long_term_ref_pics_sps > 0)
            num_lt_sps = vl_rbsp_ue(&rbsp);

         unsigned num_lt_pics = vl_rbsp_ue(&rbsp);
         unsigned num_refs = num_lt_pics + num_lt_sps;

         for (unsigned i = 0; i < num_refs; i++) {
            if (i < num_lt_sps) {
               if (sps->num_long_term_ref_pics_sps > 1)
                  /* lt_idx_sps */
                  vl_rbsp_u(&rbsp,
                        util_logbase2_ceil(sps->num_long_term_ref_pics_sps));
            } else {
               /* poc_lsb_lt */
               vl_rbsp_u(&rbsp, sps->log2_max_pic_order_cnt_lsb_minus4 + 4);
               /* used_by_curr_pic_lt_flag */
               vl_rbsp_u(&rbsp, 1);
            }

            /* poc_msb_present */
            if (vl_rbsp_u(&rbsp, 1)) {
               /* delta_poc_msb_cycle_lt */
               vl_rbsp_ue(&rbsp);
            }
         }
      }

      if (sps->flags.sps_temporal_mvp_enabled_flag)
         params->temporal_mvp_enable = vl_rbsp_u(&rbsp, 1);
   }

   if (sps->flags.sample_adaptive_offset_enabled_flag) {
      params->sao_luma_flag = vl_rbsp_u(&rbsp, 1);
      if (!sps->flags.separate_colour_plane_flag && sps->chroma_format_idc)
         params->sao_chroma_flag = vl_rbsp_u(&rbsp, 1);
   }

   params->max_num_merge_cand = 5;

   if (params->slice_type != STD_VIDEO_H265_SLICE_TYPE_I) {

      params->num_ref_idx_l0_active = pps->num_ref_idx_l0_default_active_minus1 + 1;

      if (params->slice_type == STD_VIDEO_H265_SLICE_TYPE_B)
         params->num_ref_idx_l1_active = pps->num_ref_idx_l1_default_active_minus1 + 1;
      else
         params->num_ref_idx_l1_active = 0;

      /* num_ref_idx_active_override_flag */
      if (vl_rbsp_u(&rbsp, 1)) {
         params->num_ref_idx_l0_active = vl_rbsp_ue(&rbsp) + 1;
         if (params->slice_type == STD_VIDEO_H265_SLICE_TYPE_B)
            params->num_ref_idx_l1_active = vl_rbsp_ue(&rbsp) + 1;
      }

      if (pps->flags.lists_modification_present_flag) {
         params->rpl_modification_flag[0] = vl_rbsp_u(&rbsp, 1);
         if (params->rpl_modification_flag[0]) {
            for (int i = 0; i < params->num_ref_idx_l0_active; i++) {
               /* list_entry_l0 */
               vl_rbsp_u(&rbsp,
                     util_logbase2_ceil(params->num_ref_idx_l0_active + params->num_ref_idx_l1_active));
            }
         }

         if (params->slice_type == STD_VIDEO_H265_SLICE_TYPE_B) {
            params->rpl_modification_flag[1] = vl_rbsp_u(&rbsp, 1);
            if (params->rpl_modification_flag[1]) {
               for (int i = 0; i < params->num_ref_idx_l1_active; i++) {
                  /* list_entry_l1 */
                  vl_rbsp_u(&rbsp,
                        util_logbase2_ceil(params->num_ref_idx_l0_active + params->num_ref_idx_l1_active));
               }
            }
         }
      }

      if (params->slice_type == STD_VIDEO_H265_SLICE_TYPE_B)
         params->mvd_l1_zero_flag = vl_rbsp_u(&rbsp, 1);

      if (pps->flags.cabac_init_present_flag)
         /* cabac_init_flag */
         params->cabac_init_idc = vl_rbsp_u(&rbsp, 1);

      if (params->temporal_mvp_enable) {
         if (params->slice_type == STD_VIDEO_H265_SLICE_TYPE_B)
            params->collocated_list = !vl_rbsp_u(&rbsp, 1);

         if (params->collocated_list == 0) {
            if (params->num_ref_idx_l0_active > 1)
               params->collocated_ref_idx = vl_rbsp_ue(&rbsp);
         }  else if (params->collocated_list == 1) {
            if (params->num_ref_idx_l1_active > 1)
               params->collocated_ref_idx = vl_rbsp_ue(&rbsp);
         }
      }

      if ((pps->flags.weighted_pred_flag && params->slice_type == STD_VIDEO_H265_SLICE_TYPE_P) ||
            (pps->flags.weighted_bipred_flag && params->slice_type == STD_VIDEO_H265_SLICE_TYPE_B)) {
         h265_pred_weight_table(params, &rbsp, sps, params->slice_type);
      }

      params->max_num_merge_cand -= vl_rbsp_ue(&rbsp);
   }

   params->slice_qp_delta = vl_rbsp_se(&rbsp);

   if (pps->flags.pps_slice_chroma_qp_offsets_present_flag) {
      params->slice_cb_qp_offset = CLAMP(vl_rbsp_se(&rbsp), -12, 12);
      params->slice_cr_qp_offset = CLAMP(vl_rbsp_se(&rbsp), -12, 12);
   }

   if (pps->flags.pps_slice_act_qp_offsets_present_flag) {
      /* act_y_qp_offset */
      vl_rbsp_se(&rbsp);
      /* act_cb_qp_offset */
      vl_rbsp_se(&rbsp);
      /* act_cr_qp_offset */
      vl_rbsp_se(&rbsp);
   }

   if (pps->flags.chroma_qp_offset_list_enabled_flag)
      /* cu_chroma_qp_offset_enabled_flag */
      vl_rbsp_u(&rbsp, 1);

   bool deblocking_filter_override_flag = false;
   if (pps->flags.deblocking_filter_override_enabled_flag) {
      /* deblocking_filter_override_flag */
      deblocking_filter_override_flag = vl_rbsp_u(&rbsp, 1);
   }

   if (deblocking_filter_override_flag) {
      bool deblocking_filter_disabled_flag = false;

      deblocking_filter_disabled_flag = vl_rbsp_u(&rbsp, 1);

      if (!deblocking_filter_disabled_flag) {
         params->beta_offset_div2 = CLAMP(vl_rbsp_se(&rbsp), -6, 6);
         params->tc_offset_div2 = CLAMP(vl_rbsp_se(&rbsp), -6, 6);
      }
   }

   if (pps->flags.pps_loop_filter_across_slices_enabled_flag &&
         (params->sao_luma_flag || params->sao_chroma_flag ||
          !params->disable_deblocking_filter_idc))
      params->loop_filter_across_slices_enable = vl_rbsp_u(&rbsp, 1);

   if (pps->flags.tiles_enabled_flag || pps->flags.entropy_coding_sync_enabled_flag) {
      unsigned num_entry_points_offsets = vl_rbsp_ue(&rbsp);

      if (num_entry_points_offsets > 0) {
         unsigned offset_len = vl_rbsp_ue(&rbsp) + 1;
         for (unsigned i = 0; i < num_entry_points_offsets; i++) {
            /* entry_point_offset_minus1 */
            vl_rbsp_u(&rbsp, offset_len);
         }
      }
   }

   if (pps->flags.pps_extension_present_flag) {
      unsigned length = vl_rbsp_ue(&rbsp);
      for (unsigned i = 0; i < length; i++)
         /* slice_reserved_undetermined_flag */
         vl_rbsp_u(&rbsp, 1);
   }

   unsigned header_bits =
      (slice_size * 8 - 24 /* start code */) - vl_vlc_bits_left(&rbsp.nal) - rbsp.removed;
   params->slice_data_bytes_offset = (header_bits + 8) / 8;
}

void
vk_video_get_profile_alignments(const VkVideoProfileListInfoKHR *profile_list,
                                uint32_t *width_align_out, uint32_t *height_align_out)
{
   uint32_t width_align = 1, height_align = 1;

   if (!profile_list) {
      width_align = MAX2(width_align, VK_VIDEO_H264_MACROBLOCK_WIDTH);
      height_align = MAX2(height_align, VK_VIDEO_H264_MACROBLOCK_HEIGHT);
      width_align = MAX2(width_align, VK_VIDEO_H265_CTU_MAX_WIDTH);
      height_align = MAX2(height_align, VK_VIDEO_H265_CTU_MAX_HEIGHT);
      width_align = MAX2(width_align, VK_VIDEO_AV1_BLOCK_WIDTH);
      height_align = MAX2(height_align, VK_VIDEO_AV1_BLOCK_HEIGHT);
   } else {
      for (unsigned i = 0; i < profile_list->profileCount; i++) {
         if (profile_list->pProfiles[i].videoCodecOperation == VK_VIDEO_CODEC_OPERATION_DECODE_H264_BIT_KHR ||
             profile_list->pProfiles[i].videoCodecOperation == VK_VIDEO_CODEC_OPERATION_ENCODE_H264_BIT_KHR
            ) {
            width_align = MAX2(width_align, VK_VIDEO_H264_MACROBLOCK_WIDTH);
            height_align = MAX2(height_align, VK_VIDEO_H264_MACROBLOCK_HEIGHT);
         }
         if (profile_list->pProfiles[i].videoCodecOperation == VK_VIDEO_CODEC_OPERATION_DECODE_H265_BIT_KHR ||
             profile_list->pProfiles[i].videoCodecOperation == VK_VIDEO_CODEC_OPERATION_ENCODE_H265_BIT_KHR
            ) {
            width_align = MAX2(width_align, VK_VIDEO_H265_CTU_MAX_WIDTH);
            height_align = MAX2(height_align, VK_VIDEO_H265_CTU_MAX_HEIGHT);
         }
         if (profile_list->pProfiles[i].videoCodecOperation == VK_VIDEO_CODEC_OPERATION_DECODE_AV1_BIT_KHR ||
             profile_list->pProfiles[i].videoCodecOperation == VK_VIDEO_CODEC_OPERATION_ENCODE_AV1_BIT_KHR) {
            width_align = MAX2(width_align, VK_VIDEO_AV1_BLOCK_WIDTH);
            height_align = MAX2(height_align, VK_VIDEO_AV1_BLOCK_HEIGHT);
         }
      }
   }
   *width_align_out = width_align;
   *height_align_out = height_align;
}

static const uint8_t vk_video_h264_levels[] = {10, 11, 12, 13, 20, 21, 22, 30, 31, 32, 40, 41, 42, 50, 51, 52, 60, 61, 62};
uint8_t
vk_video_get_h264_level(StdVideoH264LevelIdc level)
{
   assert(level <= STD_VIDEO_H264_LEVEL_IDC_6_2);
   return vk_video_h264_levels[level];
}

const StdVideoH264SequenceParameterSet *
vk_video_find_h264_enc_std_sps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h264_enc_h264_sps(params, id)->base;
}

const StdVideoH264PictureParameterSet *
vk_video_find_h264_enc_std_pps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h264_enc_h264_pps(params, id)->base;
}

const StdVideoH265VideoParameterSet *
vk_video_find_h265_enc_std_vps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h265_enc_h265_vps(params, id)->base;
}

const StdVideoH265SequenceParameterSet *
vk_video_find_h265_enc_std_sps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h265_enc_h265_sps(params, id)->base;
}

const StdVideoH265PictureParameterSet *
vk_video_find_h265_enc_std_pps(const struct vk_video_session_parameters *params,
                               uint32_t id)
{
   return &find_h265_enc_h265_pps(params, id)->base;
}

enum H264NALUType
{
   H264_NAL_UNSPECIFIED           = 0,
   H264_NAL_SLICE                 = 1,
   H264_NAL_SLICEDATA_A           = 2,
   H264_NAL_SLICEDATA_B           = 3,
   H264_NAL_SLICEDATA_C           = 4,
   H264_NAL_IDR                   = 5,
   H264_NAL_SEI                   = 6,
   H264_NAL_SPS                   = 7,
   H264_NAL_PPS                   = 8,
   H264_NAL_ACCESS_UNIT_DEMILITER = 9,
   H264_NAL_END_OF_SEQUENCE       = 10,
   H264_NAL_END_OF_STREAM         = 11,
   H264_NAL_FILLER_DATA           = 12,
   H264_NAL_SPS_EXTENSION         = 13,
   H264_NAL_PREFIX                = 14,
   /* 15...18 RESERVED */
   H264_NAL_AUXILIARY_SLICE = 19,
   /* 20...23 RESERVED */
   /* 24...31 UNSPECIFIED */
};

enum HEVCNALUnitType {
   HEVC_NAL_TRAIL_N        = 0,
   HEVC_NAL_TRAIL_R        = 1,
   HEVC_NAL_TSA_N          = 2,
   HEVC_NAL_TSA_R          = 3,
   HEVC_NAL_STSA_N         = 4,
   HEVC_NAL_STSA_R         = 5,
   HEVC_NAL_RADL_N         = 6,
   HEVC_NAL_RADL_R         = 7,
   HEVC_NAL_RASL_N         = 8,
   HEVC_NAL_RASL_R         = 9,
   HEVC_NAL_VCL_N10        = 10,
   HEVC_NAL_VCL_R11        = 11,
   HEVC_NAL_VCL_N12        = 12,
   HEVC_NAL_VCL_R13        = 13,
   HEVC_NAL_VCL_N14        = 14,
   HEVC_NAL_VCL_R15        = 15,
   HEVC_NAL_BLA_W_LP       = 16,
   HEVC_NAL_BLA_W_RADL     = 17,
   HEVC_NAL_BLA_N_LP       = 18,
   HEVC_NAL_IDR_W_RADL     = 19,
   HEVC_NAL_IDR_N_LP       = 20,
   HEVC_NAL_CRA_NUT        = 21,
   HEVC_NAL_VPS_NUT        = 32,
   HEVC_NAL_SPS_NUT        = 33,
   HEVC_NAL_PPS_NUT        = 34,
};

unsigned
vk_video_get_h265_nal_unit(const StdVideoEncodeH265PictureInfo *pic_info)
{
   switch (pic_info->pic_type) {
   case STD_VIDEO_H265_PICTURE_TYPE_IDR:
      return HEVC_NAL_IDR_W_RADL;
   case STD_VIDEO_H265_PICTURE_TYPE_I:
      return HEVC_NAL_CRA_NUT;
   case STD_VIDEO_H265_PICTURE_TYPE_P:
      if (pic_info->TemporalId > 0)
         if (pic_info->flags.is_reference)
            return HEVC_NAL_TSA_R;
         else
            return HEVC_NAL_TSA_N;
      else
         if (pic_info->flags.is_reference)
            return HEVC_NAL_TRAIL_R;
         else
            return HEVC_NAL_TRAIL_N;
   case STD_VIDEO_H265_PICTURE_TYPE_B:
      if (pic_info->flags.IrapPicFlag)
         if (pic_info->flags.is_reference)
            return HEVC_NAL_RASL_R;
         else
            return HEVC_NAL_RASL_N;
      else if (pic_info->TemporalId > 0)
         if (pic_info->flags.is_reference)
            return HEVC_NAL_TSA_R;
         else
            return HEVC_NAL_TSA_N;
      else
          if (pic_info->flags.is_reference)
            return HEVC_NAL_TRAIL_R;
         else
            return HEVC_NAL_TRAIL_N;
      break;
   default:
      assert(0);
      break;
   }
   return 0;
}

static const uint8_t vk_video_h265_levels[] = {
   30, 60, 63, 90, 93, 120, 123, 150, 153, 156, 180, 183, 186
};

static uint8_t
vk_video_get_h265_level(StdVideoH265LevelIdc level)
{
   assert(level <= STD_VIDEO_H265_LEVEL_IDC_6_2);
   return vk_video_h265_levels[level];
}

static void
emit_nalu_header(struct vl_bitstream_encoder *enc,
                 int nal_ref, int nal_unit)
{
   enc->prevent_start_code = false;

   vl_bitstream_put_bits(enc, 24, 0);
   vl_bitstream_put_bits(enc, 8, 1);
   vl_bitstream_put_bits(enc, 1, 0);
   vl_bitstream_put_bits(enc, 2, nal_ref); /* SPS NAL REF */
   vl_bitstream_put_bits(enc, 5, nal_unit); /* SPS NAL UNIT */
   vl_bitstream_flush(enc);

   enc->prevent_start_code = true;
}

static void
encode_hrd_params(struct vl_bitstream_encoder *enc,
                  const StdVideoH264HrdParameters *hrd)
{
   vl_bitstream_exp_golomb_ue(enc, hrd->cpb_cnt_minus1);
   vl_bitstream_put_bits(enc, 4, hrd->bit_rate_scale);
   vl_bitstream_put_bits(enc, 4, hrd->cpb_size_scale);
   for (int sched_sel_idx = 0; sched_sel_idx <= hrd->cpb_cnt_minus1; sched_sel_idx++) {
      vl_bitstream_exp_golomb_ue(enc, hrd->bit_rate_value_minus1[sched_sel_idx]);
      vl_bitstream_exp_golomb_ue(enc, hrd->cpb_size_value_minus1[sched_sel_idx]);
      vl_bitstream_put_bits(enc, 1, hrd->cbr_flag[sched_sel_idx]);
   }
   vl_bitstream_put_bits(enc, 5, hrd->initial_cpb_removal_delay_length_minus1);
   vl_bitstream_put_bits(enc, 5, hrd->cpb_removal_delay_length_minus1);
   vl_bitstream_put_bits(enc, 5, hrd->dpb_output_delay_length_minus1);
   vl_bitstream_put_bits(enc, 5, hrd->time_offset_length);
}

void
vk_video_encode_h264_sps(const StdVideoH264SequenceParameterSet *sps,
                         size_t size_limit,
                         size_t *data_size_ptr,
                         void *data_ptr)
{
   struct vl_bitstream_encoder enc;
   uint32_t data_size = *data_size_ptr;

   vl_bitstream_encoder_clear(&enc, data_ptr, data_size, size_limit);

   emit_nalu_header(&enc, 3, H264_NAL_SPS);

   vl_bitstream_put_bits(&enc, 8, sps->profile_idc);
   vl_bitstream_put_bits(&enc, 1, sps->flags.constraint_set0_flag);
   vl_bitstream_put_bits(&enc, 1, sps->flags.constraint_set1_flag);
   vl_bitstream_put_bits(&enc, 1, sps->flags.constraint_set2_flag);
   vl_bitstream_put_bits(&enc, 1, sps->flags.constraint_set3_flag);
   vl_bitstream_put_bits(&enc, 1, sps->flags.constraint_set4_flag);
   vl_bitstream_put_bits(&enc, 1, sps->flags.constraint_set5_flag);
   vl_bitstream_put_bits(&enc, 2, 0);
   vl_bitstream_put_bits(&enc, 8, vk_video_get_h264_level(sps->level_idc));
   vl_bitstream_exp_golomb_ue(&enc, sps->seq_parameter_set_id);

   if (sps->profile_idc == STD_VIDEO_H264_PROFILE_IDC_HIGH /* high10 as well */) {
      vl_bitstream_exp_golomb_ue(&enc, sps->chroma_format_idc);
      vl_bitstream_exp_golomb_ue(&enc, sps->bit_depth_luma_minus8);
      vl_bitstream_exp_golomb_ue(&enc, sps->bit_depth_chroma_minus8);
      vl_bitstream_put_bits(&enc, 1, sps->flags.qpprime_y_zero_transform_bypass_flag);
      vl_bitstream_put_bits(&enc, 1, sps->flags.seq_scaling_matrix_present_flag);
   }

   vl_bitstream_exp_golomb_ue(&enc, sps->log2_max_frame_num_minus4);

   vl_bitstream_exp_golomb_ue(&enc, sps->pic_order_cnt_type);
   if (sps->pic_order_cnt_type == 0)
      vl_bitstream_exp_golomb_ue(&enc, sps->log2_max_pic_order_cnt_lsb_minus4);

   vl_bitstream_exp_golomb_ue(&enc, sps->max_num_ref_frames);
   vl_bitstream_put_bits(&enc, 1, sps->flags.gaps_in_frame_num_value_allowed_flag);
   vl_bitstream_exp_golomb_ue(&enc, sps->pic_width_in_mbs_minus1);
   vl_bitstream_exp_golomb_ue(&enc, sps->pic_height_in_map_units_minus1);

   vl_bitstream_put_bits(&enc, 1, sps->flags.frame_mbs_only_flag);
   vl_bitstream_put_bits(&enc, 1, sps->flags.direct_8x8_inference_flag);

   vl_bitstream_put_bits(&enc, 1, sps->flags.frame_cropping_flag);
   if (sps->flags.frame_cropping_flag) {
      vl_bitstream_exp_golomb_ue(&enc, sps->frame_crop_left_offset);
      vl_bitstream_exp_golomb_ue(&enc, sps->frame_crop_right_offset);
      vl_bitstream_exp_golomb_ue(&enc, sps->frame_crop_top_offset);
      vl_bitstream_exp_golomb_ue(&enc, sps->frame_crop_bottom_offset);
   }

   vl_bitstream_put_bits(&enc, 1, sps->flags.vui_parameters_present_flag); /* vui parameters present flag */
   if (sps->flags.vui_parameters_present_flag) {
      const StdVideoH264SequenceParameterSetVui *vui = sps->pSequenceParameterSetVui;
      vl_bitstream_put_bits(&enc, 1, vui->flags.aspect_ratio_info_present_flag);

      if (vui->flags.aspect_ratio_info_present_flag) {
         vl_bitstream_put_bits(&enc, 8, vui->aspect_ratio_idc);
         if (vui->aspect_ratio_idc == STD_VIDEO_H264_ASPECT_RATIO_IDC_EXTENDED_SAR) {
            vl_bitstream_put_bits(&enc, 16, vui->sar_width);
            vl_bitstream_put_bits(&enc, 16, vui->sar_height);
         }
      }

      vl_bitstream_put_bits(&enc, 1, vui->flags.overscan_info_present_flag);
      if (vui->flags.overscan_info_present_flag)
         vl_bitstream_put_bits(&enc, 1, vui->flags.overscan_appropriate_flag);
      vl_bitstream_put_bits(&enc, 1, vui->flags.video_signal_type_present_flag);
      if (vui->flags.video_signal_type_present_flag) {
         vl_bitstream_put_bits(&enc, 3, vui->video_format);
         vl_bitstream_put_bits(&enc, 1, vui->flags.video_full_range_flag);
         vl_bitstream_put_bits(&enc, 1, vui->flags.color_description_present_flag);
         if (vui->flags.color_description_present_flag) {
            vl_bitstream_put_bits(&enc, 8, vui->colour_primaries);
            vl_bitstream_put_bits(&enc, 8, vui->transfer_characteristics);
            vl_bitstream_put_bits(&enc, 8, vui->matrix_coefficients);
         }
      }

      vl_bitstream_put_bits(&enc, 1, vui->flags.chroma_loc_info_present_flag);
      if (vui->flags.chroma_loc_info_present_flag) {
         vl_bitstream_exp_golomb_ue(&enc, vui->chroma_sample_loc_type_top_field);
         vl_bitstream_exp_golomb_ue(&enc, vui->chroma_sample_loc_type_bottom_field);
      }
      vl_bitstream_put_bits(&enc, 1, vui->flags.timing_info_present_flag);
      if (vui->flags.timing_info_present_flag) {
         vl_bitstream_put_bits(&enc, 32, vui->num_units_in_tick);
         vl_bitstream_put_bits(&enc, 32, vui->time_scale);
         vl_bitstream_put_bits(&enc, 1, vui->flags.fixed_frame_rate_flag);
      }
      vl_bitstream_put_bits(&enc, 1, vui->flags.nal_hrd_parameters_present_flag);
      if (vui->flags.nal_hrd_parameters_present_flag)
         encode_hrd_params(&enc, vui->pHrdParameters);
      vl_bitstream_put_bits(&enc, 1, vui->flags.vcl_hrd_parameters_present_flag);
      if (vui->flags.vcl_hrd_parameters_present_flag)
         encode_hrd_params(&enc, vui->pHrdParameters);
      if (vui->flags.nal_hrd_parameters_present_flag || vui->flags.vcl_hrd_parameters_present_flag)
         vl_bitstream_put_bits(&enc, 1, 0);
      vl_bitstream_put_bits(&enc, 1, 0);
      vl_bitstream_put_bits(&enc, 1, vui->flags.bitstream_restriction_flag);
      if (vui->flags.bitstream_restriction_flag) {
         vl_bitstream_put_bits(&enc, 1, 0);
         vl_bitstream_exp_golomb_ue(&enc, 0);
         vl_bitstream_exp_golomb_ue(&enc, 0);
         vl_bitstream_exp_golomb_ue(&enc, 0);
         vl_bitstream_exp_golomb_ue(&enc, 0);
         vl_bitstream_exp_golomb_ue(&enc, vui->max_num_reorder_frames);
         vl_bitstream_exp_golomb_ue(&enc, vui->max_dec_frame_buffering);
      }
   }

   vl_bitstream_rbsp_trailing(&enc);

   vl_bitstream_flush(&enc);
   *data_size_ptr += vl_bitstream_get_byte_count(&enc);
   vl_bitstream_encoder_free(&enc);
}

void
vk_video_encode_h264_pps(const StdVideoH264PictureParameterSet *pps,
                         bool high_profile,
                         size_t size_limit,
                         size_t *data_size_ptr,
                         void *data_ptr)
{
   struct vl_bitstream_encoder enc;
   uint32_t data_size = *data_size_ptr;

   vl_bitstream_encoder_clear(&enc, data_ptr, data_size, size_limit);

   emit_nalu_header(&enc, 3, H264_NAL_PPS);

   vl_bitstream_exp_golomb_ue(&enc, pps->pic_parameter_set_id);
   vl_bitstream_exp_golomb_ue(&enc, pps->seq_parameter_set_id);
   vl_bitstream_put_bits(&enc, 1, pps->flags.entropy_coding_mode_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.bottom_field_pic_order_in_frame_present_flag);
   vl_bitstream_exp_golomb_ue(&enc, 0); /* num_slice_groups_minus1 */

   vl_bitstream_exp_golomb_ue(&enc, pps->num_ref_idx_l0_default_active_minus1);
   vl_bitstream_exp_golomb_ue(&enc, pps->num_ref_idx_l1_default_active_minus1);
   vl_bitstream_put_bits(&enc, 1, pps->flags.weighted_pred_flag);
   vl_bitstream_put_bits(&enc, 2, pps->weighted_bipred_idc);
   vl_bitstream_exp_golomb_se(&enc, pps->pic_init_qp_minus26);
   vl_bitstream_exp_golomb_se(&enc, pps->pic_init_qs_minus26);
   vl_bitstream_exp_golomb_se(&enc, pps->chroma_qp_index_offset);
   vl_bitstream_put_bits(&enc, 1, pps->flags.deblocking_filter_control_present_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.constrained_intra_pred_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.redundant_pic_cnt_present_flag);

   /* high profile */
   if (high_profile) {
      vl_bitstream_put_bits(&enc, 1, pps->flags.transform_8x8_mode_flag);
      vl_bitstream_put_bits(&enc, 1, pps->flags.pic_scaling_matrix_present_flag);
      vl_bitstream_exp_golomb_se(&enc, pps->second_chroma_qp_index_offset);
   }
   vl_bitstream_rbsp_trailing(&enc);

   vl_bitstream_flush(&enc);
   *data_size_ptr += vl_bitstream_get_byte_count(&enc);
   vl_bitstream_encoder_free(&enc);
}

static void
emit_nalu_h265_header(struct vl_bitstream_encoder *enc,
                      int nal_unit_type, int temporal_id)
{
   enc->prevent_start_code = false;

   vl_bitstream_put_bits(enc, 24, 0);
   vl_bitstream_put_bits(enc, 8, 1);
   vl_bitstream_put_bits(enc, 1, 0);
   vl_bitstream_put_bits(enc, 6, nal_unit_type);   /* SPS NAL REF */
   vl_bitstream_put_bits(enc, 6, 0);               /* nuh_layer_id */
   vl_bitstream_put_bits(enc, 3, temporal_id + 1); /* nuh_temporal_id_plus1 */
   vl_bitstream_flush(enc);

   enc->prevent_start_code = true;
}

static void
encode_h265_profile_tier_level(struct vl_bitstream_encoder *enc,
                               const StdVideoH265ProfileTierLevel *ptl,
                               unsigned int max_sub_layers_minus1)
{
   vl_bitstream_put_bits(enc, 2, 0);
   vl_bitstream_put_bits(enc, 1, ptl->flags.general_tier_flag);
   vl_bitstream_put_bits(enc, 5, ptl->general_profile_idc);

   for (int j = 0; j < 32; j++)
      vl_bitstream_put_bits(enc, 1, j == ptl->general_profile_idc);

   vl_bitstream_put_bits(enc, 1, ptl->flags.general_progressive_source_flag);
   vl_bitstream_put_bits(enc, 1, ptl->flags.general_interlaced_source_flag);
   vl_bitstream_put_bits(enc, 1, ptl->flags.general_non_packed_constraint_flag);
   vl_bitstream_put_bits(enc, 1, ptl->flags.general_frame_only_constraint_flag);
   vl_bitstream_put_bits(enc, 31, 0);
   vl_bitstream_put_bits(enc, 13, 0);
   vl_bitstream_put_bits(enc, 8, vk_video_get_h265_level(ptl->general_level_idc));

   if (max_sub_layers_minus1 > 0) {
      /* sub_layer_(profile|level)_present_flag, plus padding */
      vl_bitstream_put_bits(enc, 16, 0);
   }
}

void
vk_video_encode_h265_vps(const StdVideoH265VideoParameterSet *vps,
                         size_t size_limit,
                         size_t *data_size_ptr,
                         void *data_ptr)
{
   struct vl_bitstream_encoder enc;
   uint32_t data_size = *data_size_ptr;

   vl_bitstream_encoder_clear(&enc, data_ptr, data_size, size_limit);

   emit_nalu_h265_header(&enc, HEVC_NAL_VPS_NUT, 0);

   vl_bitstream_put_bits(&enc, 4, vps->vps_video_parameter_set_id);
   vl_bitstream_put_bits(&enc, 2, 3);
   vl_bitstream_put_bits(&enc, 6, 0);//vps->vps_max_layers_minus1);
   vl_bitstream_put_bits(&enc, 3, vps->vps_max_sub_layers_minus1);
   vl_bitstream_put_bits(&enc, 1, vps->flags.vps_temporal_id_nesting_flag);
   vl_bitstream_put_bits(&enc, 16, 0xffff);

   encode_h265_profile_tier_level(&enc, vps->pProfileTierLevel, vps->vps_max_sub_layers_minus1);

   vl_bitstream_put_bits(&enc, 1, vps->flags.vps_sub_layer_ordering_info_present_flag);

   int i = vps->flags.vps_sub_layer_ordering_info_present_flag ? 0 : vps->vps_max_sub_layers_minus1;
   for (; i <= vps->vps_max_sub_layers_minus1; i++) {
      vl_bitstream_exp_golomb_ue(&enc, vps->pDecPicBufMgr->max_dec_pic_buffering_minus1[i]);
      vl_bitstream_exp_golomb_ue(&enc, vps->pDecPicBufMgr->max_num_reorder_pics[i]);
      vl_bitstream_exp_golomb_ue(&enc, vps->pDecPicBufMgr->max_latency_increase_plus1[i]);
   }

   vl_bitstream_put_bits(&enc, 6, 0);//vps->vps_max_layer_id);
   vl_bitstream_exp_golomb_ue(&enc, 0);//vps->vps_num_layer_sets_minus1);
   vl_bitstream_put_bits(&enc, 1, vps->flags.vps_timing_info_present_flag);

   if (vps->flags.vps_timing_info_present_flag) {
      vl_bitstream_put_bits(&enc, 32, vps->vps_num_units_in_tick);
      vl_bitstream_put_bits(&enc, 32, vps->vps_time_scale);
      vl_bitstream_put_bits(&enc, 1, vps->flags.vps_poc_proportional_to_timing_flag);
      if (vps->flags.vps_poc_proportional_to_timing_flag)
         vl_bitstream_exp_golomb_ue(&enc, vps->vps_num_ticks_poc_diff_one_minus1);
      vl_bitstream_exp_golomb_ue(&enc, 0);
   }

   vl_bitstream_put_bits(&enc, 1, 0);   /* vps extension flag */
   vl_bitstream_rbsp_trailing(&enc);

   vl_bitstream_flush(&enc);
   *data_size_ptr += vl_bitstream_get_byte_count(&enc);
   vl_bitstream_encoder_free(&enc);
}

static void
encode_rps(struct vl_bitstream_encoder *enc,
           const StdVideoH265SequenceParameterSet *sps,
           int st_rps_idx)
{
   const StdVideoH265ShortTermRefPicSet *rps = &sps->pShortTermRefPicSet[st_rps_idx];
   if (st_rps_idx != 0)
      vl_bitstream_put_bits(enc, 1, rps->flags.inter_ref_pic_set_prediction_flag);

   if (rps->flags.inter_ref_pic_set_prediction_flag) {
      int ref_rps_idx = st_rps_idx - (rps->delta_idx_minus1 + 1);
      vl_bitstream_put_bits(enc, 1, rps->flags.delta_rps_sign);
      vl_bitstream_exp_golomb_ue(enc, rps->abs_delta_rps_minus1);

      const StdVideoH265ShortTermRefPicSet *rps_ref = &sps->pShortTermRefPicSet[ref_rps_idx];
      int num_delta_pocs = rps_ref->num_negative_pics + rps_ref->num_positive_pics;

      for (int j = 0; j < num_delta_pocs; j++) {
         vl_bitstream_put_bits(enc, 1, !!(rps->used_by_curr_pic_flag & (1 << j)));
         if (!(rps->used_by_curr_pic_flag & (1 << j))) {
            vl_bitstream_put_bits(enc, 1, !!(rps->use_delta_flag & (1 << j)));
         }
      }
   } else {
      vl_bitstream_exp_golomb_ue(enc, rps->num_negative_pics);
      vl_bitstream_exp_golomb_ue(enc, rps->num_positive_pics);

      for (int i = 0; i < rps->num_negative_pics; i++) {
         vl_bitstream_exp_golomb_ue(enc, rps->delta_poc_s0_minus1[i]);
         vl_bitstream_put_bits(enc, 1, !!(rps->used_by_curr_pic_s0_flag & (1 << i)));
      }
      for (int i = 0; i < rps->num_positive_pics; i++) {
         vl_bitstream_exp_golomb_ue(enc, rps->delta_poc_s1_minus1[i]);
         vl_bitstream_put_bits(enc, 1, !!(rps->used_by_curr_pic_s1_flag & (1 << i)));
      }
   }
}

void
vk_video_encode_h265_sps(const StdVideoH265SequenceParameterSet *sps,
                         size_t size_limit,
                         size_t *data_size_ptr,
                         void *data_ptr)
{
   struct vl_bitstream_encoder enc;
   uint32_t data_size = *data_size_ptr;

   vl_bitstream_encoder_clear(&enc, data_ptr, data_size, size_limit);

   emit_nalu_h265_header(&enc, HEVC_NAL_SPS_NUT, 0);

   vl_bitstream_put_bits(&enc, 4, sps->sps_video_parameter_set_id);
   vl_bitstream_put_bits(&enc, 3, sps->sps_max_sub_layers_minus1);
   vl_bitstream_put_bits(&enc, 1, sps->flags.sps_temporal_id_nesting_flag);

   encode_h265_profile_tier_level(&enc, sps->pProfileTierLevel, sps->sps_max_sub_layers_minus1);

   vl_bitstream_exp_golomb_ue(&enc, sps->sps_seq_parameter_set_id);
   vl_bitstream_exp_golomb_ue(&enc, sps->chroma_format_idc);

   vl_bitstream_exp_golomb_ue(&enc, sps->pic_width_in_luma_samples);
   vl_bitstream_exp_golomb_ue(&enc, sps->pic_height_in_luma_samples);

   vl_bitstream_put_bits(&enc, 1, sps->flags.conformance_window_flag);

   if (sps->flags.conformance_window_flag) {
      vl_bitstream_exp_golomb_ue(&enc, sps->conf_win_left_offset);
      vl_bitstream_exp_golomb_ue(&enc, sps->conf_win_right_offset);
      vl_bitstream_exp_golomb_ue(&enc, sps->conf_win_top_offset);
      vl_bitstream_exp_golomb_ue(&enc, sps->conf_win_bottom_offset);
   }

   vl_bitstream_exp_golomb_ue(&enc, sps->bit_depth_luma_minus8);
   vl_bitstream_exp_golomb_ue(&enc, sps->bit_depth_chroma_minus8);

   vl_bitstream_exp_golomb_ue(&enc, sps->log2_max_pic_order_cnt_lsb_minus4);
   vl_bitstream_put_bits(&enc, 1, sps->flags.sps_sub_layer_ordering_info_present_flag);

   int i = sps->flags.sps_sub_layer_ordering_info_present_flag ? 0 : sps->sps_max_sub_layers_minus1;
   for (; i <= sps->sps_max_sub_layers_minus1; i++) {
      vl_bitstream_exp_golomb_ue(&enc, sps->pDecPicBufMgr->max_dec_pic_buffering_minus1[i]);
      vl_bitstream_exp_golomb_ue(&enc, sps->pDecPicBufMgr->max_num_reorder_pics[i]);
      vl_bitstream_exp_golomb_ue(&enc, sps->pDecPicBufMgr->max_latency_increase_plus1[i]);
   }

   vl_bitstream_exp_golomb_ue(&enc, sps->log2_min_luma_coding_block_size_minus3);
   vl_bitstream_exp_golomb_ue(&enc, sps->log2_diff_max_min_luma_coding_block_size);
   vl_bitstream_exp_golomb_ue(&enc, sps->log2_min_luma_transform_block_size_minus2);
   vl_bitstream_exp_golomb_ue(&enc, sps->log2_diff_max_min_luma_transform_block_size);

   vl_bitstream_exp_golomb_ue(&enc, sps->max_transform_hierarchy_depth_inter);
   vl_bitstream_exp_golomb_ue(&enc, sps->max_transform_hierarchy_depth_intra);

   vl_bitstream_put_bits(&enc, 1, sps->flags.scaling_list_enabled_flag);

   vl_bitstream_put_bits(&enc, 1, sps->flags.amp_enabled_flag);
   vl_bitstream_put_bits(&enc, 1, sps->flags.sample_adaptive_offset_enabled_flag);

   vl_bitstream_put_bits(&enc, 1, sps->flags.pcm_enabled_flag);

   if (sps->flags.pcm_enabled_flag) {
      vl_bitstream_put_bits(&enc, 4, sps->bit_depth_luma_minus8 + 7);
      vl_bitstream_put_bits(&enc, 4, sps->bit_depth_chroma_minus8 + 7);
      vl_bitstream_exp_golomb_ue(&enc, sps->log2_min_luma_coding_block_size_minus3);
      vl_bitstream_exp_golomb_ue(&enc, sps->log2_diff_max_min_luma_coding_block_size);
      vl_bitstream_put_bits(&enc, 1, sps->flags.pcm_loop_filter_disabled_flag);
   }

   vl_bitstream_exp_golomb_ue(&enc, sps->num_short_term_ref_pic_sets);
   for (int i = 0; i < sps->num_short_term_ref_pic_sets; i++)
      encode_rps(&enc, sps, i);

   vl_bitstream_put_bits(&enc, 1, sps->flags.long_term_ref_pics_present_flag);
   if (sps->flags.long_term_ref_pics_present_flag) {
      vl_bitstream_exp_golomb_ue(&enc, sps->num_long_term_ref_pics_sps);
      for (int i = 0; i < sps->num_long_term_ref_pics_sps; i++) {
         vl_bitstream_put_bits(&enc, sps->log2_max_pic_order_cnt_lsb_minus4 + 4, sps->pLongTermRefPicsSps->lt_ref_pic_poc_lsb_sps[i]);
         vl_bitstream_put_bits(&enc, 1, sps->pLongTermRefPicsSps->used_by_curr_pic_lt_sps_flag);
      }
   }

   vl_bitstream_put_bits(&enc, 1, sps->flags.sps_temporal_mvp_enabled_flag);
   vl_bitstream_put_bits(&enc, 1, sps->flags.strong_intra_smoothing_enabled_flag);
   vl_bitstream_put_bits(&enc, 1, sps->flags.vui_parameters_present_flag);

   if (sps->flags.vui_parameters_present_flag) {
      const StdVideoH265SequenceParameterSetVui *vui = sps->pSequenceParameterSetVui;
      vl_bitstream_put_bits(&enc, 1, vui->flags.aspect_ratio_info_present_flag);
      if (vui->flags.aspect_ratio_info_present_flag) {
         vl_bitstream_put_bits(&enc, 8, vui->aspect_ratio_idc);
         if (vui->aspect_ratio_idc == STD_VIDEO_H265_ASPECT_RATIO_IDC_EXTENDED_SAR) {
            vl_bitstream_put_bits(&enc, 16, vui->sar_width);
            vl_bitstream_put_bits(&enc, 16, vui->sar_height);
         }
      }
      vl_bitstream_put_bits(&enc, 1, vui->flags.overscan_info_present_flag);
      if (vui->flags.overscan_info_present_flag)
         vl_bitstream_put_bits(&enc, 1, vui->flags.overscan_appropriate_flag);
      vl_bitstream_put_bits(&enc, 1, vui->flags.video_signal_type_present_flag);
      if (vui->flags.video_signal_type_present_flag) {
         vl_bitstream_put_bits(&enc, 3, vui->video_format);
         vl_bitstream_put_bits(&enc, 1, vui->flags.video_full_range_flag);
         vl_bitstream_put_bits(&enc, 1, vui->flags.colour_description_present_flag);
         if (vui->flags.colour_description_present_flag) {
            vl_bitstream_put_bits(&enc, 8, vui->colour_primaries);
            vl_bitstream_put_bits(&enc, 8, vui->transfer_characteristics);
            vl_bitstream_put_bits(&enc, 8, vui->matrix_coeffs);
         }
      }
      vl_bitstream_put_bits(&enc, 1, vui->flags.chroma_loc_info_present_flag);
      if (vui->flags.chroma_loc_info_present_flag) {
         vl_bitstream_exp_golomb_ue(&enc, vui->chroma_sample_loc_type_top_field);
         vl_bitstream_exp_golomb_ue(&enc, vui->chroma_sample_loc_type_bottom_field);
      }
      vl_bitstream_put_bits(&enc, 1, vui->flags.neutral_chroma_indication_flag);
      vl_bitstream_put_bits(&enc, 1, vui->flags.field_seq_flag);
      vl_bitstream_put_bits(&enc, 1, vui->flags.frame_field_info_present_flag);
      vl_bitstream_put_bits(&enc, 1, vui->flags.default_display_window_flag);
      if (vui->flags.default_display_window_flag) {
         vl_bitstream_exp_golomb_ue(&enc, vui->def_disp_win_left_offset);
         vl_bitstream_exp_golomb_ue(&enc, vui->def_disp_win_right_offset);
         vl_bitstream_exp_golomb_ue(&enc, vui->def_disp_win_top_offset);
         vl_bitstream_exp_golomb_ue(&enc, vui->def_disp_win_bottom_offset);
      }
      vl_bitstream_put_bits(&enc, 1, vui->flags.vui_timing_info_present_flag);
      if (vui->flags.vui_timing_info_present_flag) {
         vl_bitstream_put_bits(&enc, 32, vui->vui_num_units_in_tick);
         vl_bitstream_put_bits(&enc, 32, vui->vui_time_scale);
         vl_bitstream_put_bits(&enc, 1, vui->flags.vui_poc_proportional_to_timing_flag);
         if (vui->flags.vui_poc_proportional_to_timing_flag)
            vl_bitstream_exp_golomb_ue(&enc, vui->vui_num_ticks_poc_diff_one_minus1);
         vl_bitstream_put_bits(&enc, 1, 0);//vui->flags.vui_hrd_parameters_present_flag);
         // HRD
      }

      vl_bitstream_put_bits(&enc, 1, vui->flags.bitstream_restriction_flag);
      if (vui->flags.bitstream_restriction_flag) {
         vl_bitstream_put_bits(&enc, 1, vui->flags.tiles_fixed_structure_flag);
         vl_bitstream_put_bits(&enc, 1, vui->flags.motion_vectors_over_pic_boundaries_flag);
         vl_bitstream_put_bits(&enc, 1, vui->flags.restricted_ref_pic_lists_flag);
         vl_bitstream_exp_golomb_ue(&enc, vui->min_spatial_segmentation_idc);
         vl_bitstream_exp_golomb_ue(&enc, vui->max_bytes_per_pic_denom);
         vl_bitstream_exp_golomb_ue(&enc, vui->max_bits_per_min_cu_denom);
         vl_bitstream_exp_golomb_ue(&enc, vui->log2_max_mv_length_horizontal);
            vl_bitstream_exp_golomb_ue(&enc, vui->log2_max_mv_length_vertical);
      }
   }

   vl_bitstream_put_bits(&enc, 1, 0);   /* sps extension flg */
   vl_bitstream_rbsp_trailing(&enc);

   vl_bitstream_flush(&enc);
   *data_size_ptr += vl_bitstream_get_byte_count(&enc);
   vl_bitstream_encoder_free(&enc);
}

void
vk_video_encode_h265_pps(const StdVideoH265PictureParameterSet *pps,
                         size_t size_limit,
                         size_t *data_size_ptr,
                         void *data_ptr)
{
   struct vl_bitstream_encoder enc;
   uint32_t data_size = *data_size_ptr;

   vl_bitstream_encoder_clear(&enc, data_ptr, data_size, size_limit);

   emit_nalu_h265_header(&enc, HEVC_NAL_PPS_NUT, 0);
   vl_bitstream_exp_golomb_ue(&enc, pps->pps_pic_parameter_set_id);
   vl_bitstream_exp_golomb_ue(&enc, pps->pps_seq_parameter_set_id);

   vl_bitstream_put_bits(&enc, 1, pps->flags.dependent_slice_segments_enabled_flag);

   vl_bitstream_put_bits(&enc, 1, pps->flags.output_flag_present_flag);
   vl_bitstream_put_bits(&enc, 3, pps->num_extra_slice_header_bits);

   vl_bitstream_put_bits(&enc, 1, pps->flags.sign_data_hiding_enabled_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.cabac_init_present_flag);

   vl_bitstream_exp_golomb_ue(&enc, pps->num_ref_idx_l0_default_active_minus1);
   vl_bitstream_exp_golomb_ue(&enc, pps->num_ref_idx_l1_default_active_minus1);

   vl_bitstream_exp_golomb_se(&enc, pps->init_qp_minus26);

   vl_bitstream_put_bits(&enc, 1, pps->flags.constrained_intra_pred_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.transform_skip_enabled_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.cu_qp_delta_enabled_flag);

   if (pps->flags.cu_qp_delta_enabled_flag)
      vl_bitstream_exp_golomb_ue(&enc, pps->diff_cu_qp_delta_depth);

   vl_bitstream_exp_golomb_se(&enc, pps->pps_cb_qp_offset);
   vl_bitstream_exp_golomb_se(&enc, pps->pps_cr_qp_offset);

   vl_bitstream_put_bits(&enc, 1, pps->flags.pps_slice_chroma_qp_offsets_present_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.weighted_pred_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.weighted_bipred_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.transquant_bypass_enabled_flag);

   vl_bitstream_put_bits(&enc, 1, pps->flags.tiles_enabled_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.entropy_coding_sync_enabled_flag);

   assert (!pps->flags.tiles_enabled_flag);

   vl_bitstream_put_bits(&enc, 1, pps->flags.pps_loop_filter_across_slices_enabled_flag);
   vl_bitstream_put_bits(&enc, 1, pps->flags.deblocking_filter_control_present_flag);

   if (pps->flags.deblocking_filter_control_present_flag) {
      vl_bitstream_put_bits(&enc, 1, pps->flags.deblocking_filter_override_enabled_flag);
      vl_bitstream_put_bits(&enc, 1, pps->flags.pps_deblocking_filter_disabled_flag);
      if (!pps->flags.pps_deblocking_filter_disabled_flag) {
         vl_bitstream_exp_golomb_se(&enc, pps->pps_beta_offset_div2);
         vl_bitstream_exp_golomb_se(&enc, pps->pps_tc_offset_div2);
      }
   }

   vl_bitstream_put_bits(&enc, 1, pps->flags.pps_scaling_list_data_present_flag);
   assert (!pps->flags.pps_scaling_list_data_present_flag);

   vl_bitstream_put_bits(&enc, 1, pps->flags.lists_modification_present_flag);
   vl_bitstream_exp_golomb_ue(&enc, pps->log2_parallel_merge_level_minus2);
   vl_bitstream_put_bits(&enc, 1, pps->flags.slice_segment_header_extension_present_flag);

   vl_bitstream_put_bits(&enc, 1, 0); /* pps extension flag */
   vl_bitstream_rbsp_trailing(&enc);

   vl_bitstream_flush(&enc);
   *data_size_ptr += vl_bitstream_get_byte_count(&enc);
   vl_bitstream_encoder_free(&enc);
}

void
vk_video_encode_h264_slice_header(const StdVideoEncodeH264PictureInfo *pic_info,
                                  const StdVideoH264SequenceParameterSet *sps,
                                  const StdVideoH264PictureParameterSet *pps,
                                  const StdVideoEncodeH264SliceHeader *slice_header,
                                  const int8_t slice_qp_delta,
                                  size_t *data_size_ptr,
                                  void *data_ptr)
{
   struct vl_bitstream_encoder enc;
   uint32_t data_size = *data_size_ptr;

   int is_idr = !!pic_info->flags.IdrPicFlag;
   int is_ref = !!pic_info->flags.is_reference;
   uint32_t slice_type = slice_header->slice_type % 5;

   vl_bitstream_encoder_clear(&enc, data_ptr, data_size, VL_BITSTREAM_MAX_BUFFER);

   if (slice_type == STD_VIDEO_H264_SLICE_TYPE_I) {
      emit_nalu_header(&enc, 3, is_idr ? H264_NAL_IDR : H264_NAL_SLICE);
   } else if (slice_type == STD_VIDEO_H264_SLICE_TYPE_P) {
      assert(!is_idr);
      emit_nalu_header(&enc, 2, H264_NAL_SLICE);
   } else {
      assert(slice_type == STD_VIDEO_H264_SLICE_TYPE_B);
      assert(!is_idr);
      emit_nalu_header(&enc, is_ref ? 1 : 0, H264_NAL_SLICE);
   }

   vl_bitstream_put_bits(&enc, 1, slice_header->first_mb_in_slice);
   vl_bitstream_exp_golomb_ue(&enc, slice_header->slice_type);
   vl_bitstream_exp_golomb_ue(&enc, pic_info->pic_parameter_set_id);

   if (sps->flags.separate_colour_plane_flag)
      /* colour plane id */
      vl_bitstream_put_bits(&enc, 2, 0);

   vl_bitstream_put_bits(&enc, sps->log2_max_frame_num_minus4 + 4, pic_info->frame_num);

   /* frame_mbs_only_flag == 1 */
   if (!sps->flags.frame_mbs_only_flag) {
      /* FIXME: */
      assert(0);
   }

   if (pic_info->flags.IdrPicFlag)
      vl_bitstream_exp_golomb_ue(&enc, pic_info->idr_pic_id);

   if (sps->pic_order_cnt_type == STD_VIDEO_H264_POC_TYPE_0) {
      vl_bitstream_put_bits(&enc, sps->log2_max_pic_order_cnt_lsb_minus4 + 4, pic_info->PicOrderCnt);
      /* pic_order_present_flag == 0 */
      if (pps->flags.bottom_field_pic_order_in_frame_present_flag) {
         assert(0);
         vl_bitstream_exp_golomb_se(&enc, 0);
      }
   } else if (sps->pic_order_cnt_type == STD_VIDEO_H264_POC_TYPE_1) {
      assert(0);

      if (!sps->flags.delta_pic_order_always_zero_flag) {
      }
   } else if (sps->pic_order_cnt_type == STD_VIDEO_H264_POC_TYPE_2) {
   } else {
      assert(0);
   }

   /* redundant_pic_cnt_present_flag == 0 */
   if (pps->flags.redundant_pic_cnt_present_flag) {
      vl_bitstream_exp_golomb_ue(&enc, 0);
   }

   /* slice type */
   if (slice_type == STD_VIDEO_H264_SLICE_TYPE_P) {
      vl_bitstream_put_bits(&enc, 1, slice_header->flags.num_ref_idx_active_override_flag);

      if (slice_header->flags.num_ref_idx_active_override_flag) {
         vl_bitstream_exp_golomb_ue(&enc, pic_info->pRefLists->num_ref_idx_l0_active_minus1);
      }

      vl_bitstream_put_bits(&enc, 1, pic_info->pRefLists->flags.ref_pic_list_modification_flag_l0);

      if (pic_info->pRefLists->flags.ref_pic_list_modification_flag_l0) {
         assert(0);
         /* TODO */
         for (unsigned i = 0; i < pic_info->pRefLists->refList0ModOpCount; i++) {
         }
      }
   } else if (slice_type == STD_VIDEO_H264_SLICE_TYPE_B) {
      vl_bitstream_put_bits(&enc, 1, slice_header->flags.direct_spatial_mv_pred_flag);
      vl_bitstream_put_bits(&enc, 1, slice_header->flags.num_ref_idx_active_override_flag);

      if (slice_header->flags.num_ref_idx_active_override_flag) {
         vl_bitstream_exp_golomb_ue(&enc, pic_info->pRefLists->num_ref_idx_l0_active_minus1);
         vl_bitstream_exp_golomb_ue(&enc, pic_info->pRefLists->num_ref_idx_l1_active_minus1);
      }

      vl_bitstream_put_bits(&enc, 1, pic_info->pRefLists->flags.ref_pic_list_modification_flag_l0);
      vl_bitstream_put_bits(&enc, 1, pic_info->pRefLists->flags.ref_pic_list_modification_flag_l1);

      if (pic_info->pRefLists->flags.ref_pic_list_modification_flag_l0) {
         assert(0);
         for (unsigned i = 0; i < pic_info->pRefLists->refList0ModOpCount; i++) {
         }
      }
      if (pic_info->pRefLists->flags.ref_pic_list_modification_flag_l1) {
         assert(0);
         for (unsigned i = 0; i < pic_info->pRefLists->refList1ModOpCount; i++) {
         }
      }
   }

   if ((pps->flags.weighted_pred_flag && (slice_type == STD_VIDEO_H264_SLICE_TYPE_P)) ||
       ((pps->weighted_bipred_idc == 1) && (slice_type == STD_VIDEO_H264_SLICE_TYPE_B))) {
      /* FIXME: fill weight/offset table */
      assert(0);
   }

   /* dec_ref_pic_marking */
   /* nal_ref_idc != 0 */
   if (slice_type != STD_VIDEO_H264_SLICE_TYPE_B || pic_info->flags.is_reference) {
      unsigned char no_output_of_prior_pics_flag = 0;
      unsigned char long_term_reference_flag = 0;
      unsigned char adaptive_ref_pic_marking_mode_flag = 0;

      if (pic_info->flags.IdrPicFlag) {
         vl_bitstream_put_bits(&enc, 1, no_output_of_prior_pics_flag);
         vl_bitstream_put_bits(&enc, 1, long_term_reference_flag);
      } else {
         vl_bitstream_put_bits(&enc, 1, adaptive_ref_pic_marking_mode_flag);
      }
   }

   if (pps->flags.entropy_coding_mode_flag && (slice_type != STD_VIDEO_H264_SLICE_TYPE_I))
      vl_bitstream_exp_golomb_ue(&enc, slice_header->cabac_init_idc);

   vl_bitstream_exp_golomb_se(&enc, slice_qp_delta);

   if (pps->flags.deblocking_filter_control_present_flag) {
      vl_bitstream_exp_golomb_ue(&enc, slice_header->disable_deblocking_filter_idc);

      if (slice_header->disable_deblocking_filter_idc != 1) {
         vl_bitstream_exp_golomb_se(&enc, slice_header->slice_alpha_c0_offset_div2);
         vl_bitstream_exp_golomb_se(&enc, slice_header->slice_beta_offset_div2);
      }
   }

   if (pps->flags.entropy_coding_mode_flag) {
      int left = vl_bitstream_get_num_bits_for_byte_align(&enc);
      int val = (1 << left) - 1;

      if (left)
         vl_bitstream_put_bits(&enc, left, val);

      ASSERTED bool is_aligned = vl_bitstream_is_byte_aligned(&enc);
      assert(is_aligned);
   }

   vl_bitstream_rbsp_trailing(&enc);
   vl_bitstream_flush(&enc);
   *data_size_ptr += vl_bitstream_get_byte_count(&enc);
   vl_bitstream_encoder_free(&enc);

   return;
}

void
vk_video_encode_h265_slice_header(const StdVideoEncodeH265PictureInfo *pic_info,
                                  const StdVideoH265VideoParameterSet *vps,
                                  const StdVideoH265SequenceParameterSet *sps,
                                  const StdVideoH265PictureParameterSet *pps,
                                  const StdVideoEncodeH265SliceSegmentHeader *slice_header,
                                  const int8_t slice_qp_delta,
                                  size_t *data_size_ptr,
                                  void *data_ptr)
{
   struct vl_bitstream_encoder enc;
   uint32_t data_size = *data_size_ptr;

   vl_bitstream_encoder_clear(&enc, data_ptr, data_size, VL_BITSTREAM_MAX_BUFFER);
   emit_nalu_h265_header(&enc, vk_video_get_h265_nal_unit(pic_info), pic_info->TemporalId);

   vl_bitstream_put_bits(&enc, 1, slice_header->flags.first_slice_segment_in_pic_flag);
   if (pic_info->flags.IrapPicFlag) {
      vl_bitstream_put_bits(&enc, 1, pic_info->flags.no_output_of_prior_pics_flag);
   }

   vl_bitstream_exp_golomb_ue(&enc, pic_info->pps_pic_parameter_set_id);

   if (!slice_header->flags.first_slice_segment_in_pic_flag) {
      unsigned size, num;
      unsigned bits_slice_segment_address = 0;

      if (pps->flags.dependent_slice_segments_enabled_flag)
         vl_bitstream_put_bits(&enc, 1, slice_header->flags.dependent_slice_segment_flag);

      size = 1 << (sps->log2_min_luma_coding_block_size_minus3 + 3 +
                   sps->log2_diff_max_min_luma_coding_block_size);

      num = ((sps->pic_width_in_luma_samples + size - 1) / size) *
            ((sps->pic_height_in_luma_samples + size - 1) / size);

      while (num > (1 << bits_slice_segment_address))
         bits_slice_segment_address++;

      vl_bitstream_put_bits(&enc, bits_slice_segment_address, slice_header->slice_segment_address);
   }

   if (slice_header->flags.dependent_slice_segment_flag)
      goto finish;

   for (unsigned i = 0; i < pps->num_extra_slice_header_bits; ++i)
      /* slice_reserved_flag */
      vl_bitstream_put_bits(&enc, 1, 0);

   vl_bitstream_exp_golomb_ue(&enc, slice_header->slice_type);

   if (pps->flags.output_flag_present_flag)
      vl_bitstream_put_bits(&enc, 1, pic_info->flags.pic_output_flag);

   if (sps->flags.separate_colour_plane_flag)
      /* colour_plane_id */
      vl_bitstream_put_bits(&enc, 2, 0);

   if (pic_info->pic_type != STD_VIDEO_H265_PICTURE_TYPE_IDR) {
      /* slice_pic_order_cnt_lsb */
      uint32_t slice_pic_order_cnt_lsb =
         pic_info->PicOrderCntVal & ((1 << (sps->log2_max_pic_order_cnt_lsb_minus4 + 4)) - 1);

      vl_bitstream_put_bits(&enc, sps->log2_max_pic_order_cnt_lsb_minus4 + 4, slice_pic_order_cnt_lsb);
      vl_bitstream_put_bits(&enc, 1, pic_info->flags.short_term_ref_pic_set_sps_flag);

      if (!pic_info->flags.short_term_ref_pic_set_sps_flag) {
         const StdVideoH265ShortTermRefPicSet* st_rps = pic_info->pShortTermRefPicSet;
         unsigned num_st_rps = sps->num_short_term_ref_pic_sets;
         bool rps_predict = false;

         if (num_st_rps) {
            rps_predict = st_rps->flags.inter_ref_pic_set_prediction_flag;
            vl_bitstream_put_bits(&enc, 1, st_rps->flags.inter_ref_pic_set_prediction_flag);
         }

         if (rps_predict) {
            vl_bitstream_exp_golomb_ue(&enc, st_rps->delta_idx_minus1);
            vl_bitstream_put_bits(&enc, 1, st_rps->flags.delta_rps_sign);
            vl_bitstream_exp_golomb_ue(&enc, st_rps->abs_delta_rps_minus1);

            for (unsigned i = 0; i <= st_rps->num_negative_pics + st_rps->num_positive_pics; i++) {
               vl_bitstream_put_bits(&enc, 1, st_rps->used_by_curr_pic_flag);
               if (!st_rps->used_by_curr_pic_flag) {
                  vl_bitstream_put_bits(&enc, 1, st_rps->use_delta_flag);
               }
            }
         } else {
            vl_bitstream_exp_golomb_ue(&enc, st_rps->num_negative_pics);
            vl_bitstream_exp_golomb_ue(&enc, st_rps->num_positive_pics);

            for (unsigned i = 0; i < st_rps->num_negative_pics; i++) {
               vl_bitstream_exp_golomb_ue(&enc, st_rps->delta_poc_s0_minus1[i]);
               vl_bitstream_put_bits(&enc, 1, st_rps->used_by_curr_pic_s0_flag);
            }
            for (unsigned i = 0; i < st_rps->num_positive_pics; i++) {
               vl_bitstream_exp_golomb_ue(&enc, st_rps->delta_poc_s1_minus1[i]);
               vl_bitstream_put_bits(&enc, 1, st_rps->used_by_curr_pic_s1_flag);
            }
         }
      } else {
         unsigned num_st_rps = sps->num_short_term_ref_pic_sets;

         int numbits = util_logbase2_ceil(num_st_rps);
         vl_bitstream_put_bits(&enc, numbits, pic_info->short_term_ref_pic_set_idx);
      }

      if (sps->flags.long_term_ref_pics_present_flag) {
         const StdVideoEncodeH265LongTermRefPics* lt_pics = pic_info->pLongTermRefPics;
         unsigned num_lt_sps = 0;
         unsigned num_lt_pics = lt_pics->num_long_term_pics;

         if (sps->num_long_term_ref_pics_sps > 0) {
            num_lt_sps = lt_pics->num_long_term_sps;
            vl_bitstream_exp_golomb_ue(&enc, num_lt_sps);
         }

         vl_bitstream_exp_golomb_ue(&enc, num_lt_pics);

         unsigned num_refs = num_lt_sps + num_lt_pics;

         for (unsigned i = 0; i < num_refs; i++) {
            if (i < num_lt_sps) {
               if (sps->num_long_term_ref_pics_sps > 1) {
                  vl_bitstream_put_bits(&enc, util_logbase2_ceil(sps->num_long_term_ref_pics_sps),
                        lt_pics->lt_idx_sps[i]);
               }
            } else {
               vl_bitstream_put_bits(&enc, sps->log2_max_pic_order_cnt_lsb_minus4 + 4,
                     lt_pics->poc_lsb_lt[i]),
               vl_bitstream_put_bits(&enc, 1, lt_pics->used_by_curr_pic_lt_flag);
            }

            vl_bitstream_put_bits(&enc, 1, lt_pics->delta_poc_msb_present_flag[i]);
            if (lt_pics->delta_poc_msb_present_flag[i]) {
               vl_bitstream_exp_golomb_ue(&enc, lt_pics->delta_poc_msb_cycle_lt[i]);
            }
         }
      }

      if (sps->flags.sps_temporal_mvp_enabled_flag)
         vl_bitstream_put_bits(&enc, 1, pic_info->flags.slice_temporal_mvp_enabled_flag);
   }

   if (sps->flags.sample_adaptive_offset_enabled_flag) {
      vl_bitstream_put_bits(&enc, 1, slice_header->flags.slice_sao_luma_flag);
      if (sps->chroma_format_idc)
         vl_bitstream_put_bits(&enc, 1, slice_header->flags.slice_sao_chroma_flag);
   }

   if (slice_header->slice_type != STD_VIDEO_H265_SLICE_TYPE_I) {
      unsigned num_ref_idx_l0_active = pps->num_ref_idx_l0_default_active_minus1 + 1;
      unsigned num_ref_idx_l1_active = pps->num_ref_idx_l1_default_active_minus1 + 1;

      vl_bitstream_put_bits(&enc, 1, slice_header->flags.num_ref_idx_active_override_flag);
      if (slice_header->flags.num_ref_idx_active_override_flag) {
         vl_bitstream_exp_golomb_ue(&enc, pic_info->pRefLists->num_ref_idx_l0_active_minus1);
         num_ref_idx_l0_active = pic_info->pRefLists->num_ref_idx_l0_active_minus1 + 1;

         if (slice_header->slice_type == STD_VIDEO_H265_SLICE_TYPE_B) {
            vl_bitstream_exp_golomb_ue(&enc, pic_info->pRefLists->num_ref_idx_l1_active_minus1);
            num_ref_idx_l1_active = pic_info->pRefLists->num_ref_idx_l1_active_minus1 + 1;
         }
      }

      if (pps->flags.lists_modification_present_flag) {
         vl_bitstream_put_bits(&enc, 1, pic_info->pRefLists->flags.ref_pic_list_modification_flag_l0);
         if (pic_info->pRefLists->flags.ref_pic_list_modification_flag_l0) {

            for (int i = 0; i < num_ref_idx_l0_active; i++) {
               vl_bitstream_put_bits(&enc, util_logbase2_ceil(num_ref_idx_l0_active + num_ref_idx_l1_active),
                     pic_info->pRefLists->list_entry_l0[i]);
            }
         }

         if (slice_header->slice_type == STD_VIDEO_H265_SLICE_TYPE_B) {
            vl_bitstream_put_bits(&enc, 1, pic_info->pRefLists->flags.ref_pic_list_modification_flag_l1);

            if (pic_info->pRefLists->flags.ref_pic_list_modification_flag_l1) {
               for (int i = 0; i < num_ref_idx_l1_active; i++) {
                  vl_bitstream_put_bits(&enc, util_logbase2_ceil(num_ref_idx_l0_active + num_ref_idx_l1_active),
                        pic_info->pRefLists->list_entry_l1[i]);
               }
            }
         }
      }

      if (slice_header->slice_type == STD_VIDEO_H265_SLICE_TYPE_B)
         vl_bitstream_put_bits(&enc, 1, slice_header->flags.mvd_l1_zero_flag);

      if (pps->flags.cabac_init_present_flag)
         /* cabac_init_flag */
         vl_bitstream_put_bits(&enc, 1, slice_header->flags.cabac_init_flag);

      if (pic_info->flags.slice_temporal_mvp_enabled_flag) {
         unsigned collocated_list = 0;
         if (slice_header->slice_type == STD_VIDEO_H265_SLICE_TYPE_B) {
            collocated_list = 1;
            vl_bitstream_put_bits(&enc, 1, collocated_list);
         }

         if (collocated_list == 0) {
            if (num_ref_idx_l0_active > 1)
               vl_bitstream_exp_golomb_ue(&enc, slice_header->collocated_ref_idx);
         }  else if (collocated_list == 1) {
            if (num_ref_idx_l1_active > 1)
               vl_bitstream_exp_golomb_ue(&enc, slice_header->collocated_ref_idx);
         }
      }

      if ((pps->flags.weighted_pred_flag && slice_header->slice_type == STD_VIDEO_H265_SLICE_TYPE_P) ||
            (pps->flags.weighted_bipred_flag && slice_header->slice_type == STD_VIDEO_H265_SLICE_TYPE_B)) {
         /* FIXME : h265_pred_weight_table */
         assert(0);
      }

      vl_bitstream_exp_golomb_ue(&enc, 5 - slice_header->MaxNumMergeCand);
   }

   vl_bitstream_exp_golomb_se(&enc, slice_qp_delta);

   if (pps->flags.pps_slice_chroma_qp_offsets_present_flag) {
      vl_bitstream_exp_golomb_se(&enc, slice_header->slice_cb_qp_offset);
      vl_bitstream_exp_golomb_se(&enc, slice_header->slice_cr_qp_offset);
   }

   if (pps->flags.chroma_qp_offset_list_enabled_flag)
      vl_bitstream_put_bits(&enc, 1, slice_header->flags.cu_chroma_qp_offset_enabled_flag);

   if (pps->flags.deblocking_filter_control_present_flag) {
      if (pps->flags.deblocking_filter_override_enabled_flag) {
         vl_bitstream_put_bits(&enc, 1, slice_header->flags.deblocking_filter_override_flag);

         if (slice_header->flags.deblocking_filter_override_flag) {
            vl_bitstream_put_bits(&enc, 1, slice_header->flags.slice_deblocking_filter_disabled_flag);

            if (!slice_header->flags.slice_deblocking_filter_disabled_flag) {
               vl_bitstream_exp_golomb_se(&enc, slice_header->slice_beta_offset_div2);
               vl_bitstream_exp_golomb_se(&enc, slice_header->slice_tc_offset_div2);
            }
         }
      }
   }

   if (pps->flags.pps_loop_filter_across_slices_enabled_flag &&
         (slice_header->flags.slice_sao_luma_flag || slice_header->flags.slice_sao_chroma_flag ||
          !slice_header->flags.slice_deblocking_filter_disabled_flag))
      vl_bitstream_put_bits(&enc, 1, slice_header->flags.slice_loop_filter_across_slices_enabled_flag);

   if (pps->flags.tiles_enabled_flag || pps->flags.entropy_coding_sync_enabled_flag) {
      assert(0);
   }

   if (pps->flags.pps_extension_present_flag) {
      assert(0);
   }

finish:
   vl_bitstream_rbsp_trailing(&enc);
   vl_bitstream_flush(&enc);
   *data_size_ptr += vl_bitstream_get_byte_count(&enc);
   vl_bitstream_encoder_free(&enc);

   return;
}

enum AV1NALUType
{
   OBU_SEQUENCE_HEADER = 1,
   OBU_TEMPORAL_DELIMITER = 2,
   OBU_FRAME_HEADER = 3,
   OBU_TILE_GROUP = 4,
   OBU_METADATA = 5,
   OBU_FRAME = 6,
   OBU_REDUNDANT_FRAME_HEADER = 7,
   OBU_PADDING = 15,
};

static void
emit_obu_av1_header(struct vl_bitstream_encoder *enc,
                    enum AV1NALUType obu_type,
                    uint32_t obu_extension_flag,
                    uint32_t temporal_id,
                    uint32_t spatial_id)
{
   vl_bitstream_put_bits(enc, 1, 0); // obu_forbidden_bit
   vl_bitstream_put_bits(enc, 4, obu_type);   // type
   vl_bitstream_put_bits(enc, 1, obu_extension_flag);
   vl_bitstream_put_bits(enc, 1, 1);   // obu_has_size_field
   vl_bitstream_put_bits(enc, 1, 0);   // reserved
   if (obu_extension_flag) {
      // obu_extension_header()
      vl_bitstream_put_bits(enc, 3, temporal_id);
      vl_bitstream_put_bits(enc, 2, spatial_id);
      vl_bitstream_put_bits(enc, 3, 0);   // extension_header_reserved_3bits
   }
}

static void vk_video_encode_av1_code_leb128(uint8_t *buf, uint32_t num_bytes, uint32_t value)
{
   uint8_t leb128_byte = 0;
   uint32_t i = 0;

   do {
      leb128_byte = (value & 0x7f);
      value >>= 7;
      if (num_bytes > 1)
         leb128_byte |= 0x80;

      *(buf + i) = leb128_byte;
      num_bytes--;
      i++;
   } while((leb128_byte & 0x80));
}

static StdVideoEncodeAV1OperatingPointInfo default_av1_operating_point = {
   .flags = {
      .decoder_model_present_for_this_op = 0,
      .low_delay_mode_flag = 0,
      .initial_display_delay_present_for_this_op = 0,
   },
   .operating_point_idc = 0,
   .seq_level_idx = STD_VIDEO_AV1_LEVEL_6_1,
   .seq_tier = 0,
   .decoder_buffer_delay = 0,
   .encoder_buffer_delay = 0,
   .initial_display_delay_minus_1 = 0,
};

VkResult
vk_video_encode_av1_seq_hdr(const struct vk_video_session_parameters *params,
                            size_t size_limit,
                            size_t *data_size_ptr,
                            void *data_ptr)
{
   struct vl_bitstream_encoder enc;
   uint32_t data_size = *data_size_ptr;
   uint32_t obu_extension_flag = 0;
   uint32_t temporal_id = 0;
   uint32_t spatial_id = 0;
   uint8_t* size_offset = NULL;
   uint32_t obu_size;
   uint8_t obu_size_bin[2];
   const int num_obu_size_bytes = 2;
   const StdVideoAV1ColorConfig* color = &params->av1_enc.seq_hdr.color_config;
   const StdVideoAV1TimingInfo* timing_info = &params->av1_enc.seq_hdr.timing_info;
   const StdVideoAV1SequenceHeader *seq_hdr = &params->av1_enc.seq_hdr.base;
   uint8_t decoder_model_present_flag = 0;
   const StdVideoEncodeAV1DecoderModelInfo* decoder_model = &params->av1_enc.decoder_model;
   int num_op_points = MAX2(params->av1_enc.num_op_points, 1);
   const StdVideoEncodeAV1OperatingPointInfo* op_points = params->av1_enc.num_op_points ?
      params->av1_enc.op_points : &default_av1_operating_point;

   assert(params->op == VK_VIDEO_CODEC_OPERATION_ENCODE_AV1_BIT_KHR);

   vl_bitstream_encoder_clear(&enc, data_ptr, data_size, size_limit);
   /* AV1 does not need start code prevention */
   enc.prevent_start_code = false;

   if (!color || (num_op_points > 0 && !op_points))
      return VK_ERROR_INVALID_VIDEO_STD_PARAMETERS_KHR;

   emit_obu_av1_header(&enc, OBU_SEQUENCE_HEADER, obu_extension_flag, temporal_id, spatial_id);

   /* obu_size, use two bytes for header, the size will be written in afterwards */
   size_offset = vl_bitstream_get_byte_offset(&enc);
   vl_bitstream_put_bits(&enc, num_obu_size_bytes * 8, 0);

   /* sequence_header_obu() */
   /*  seq_profile  */
   vl_bitstream_put_bits(&enc, 3, seq_hdr->seq_profile);

   /*  still_picture */
   vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.still_picture);
   /*  reduced_still_picture_header */
   vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.reduced_still_picture_header);

   if (seq_hdr->flags.reduced_still_picture_header) {
      vl_bitstream_put_bits(&enc, 5, op_points[0].seq_level_idx);
   } else {
      /*  timing_info_present_flag  */
      vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.timing_info_present_flag);

      if (seq_hdr->flags.timing_info_present_flag) {
         if (!timing_info)
            return VK_ERROR_INVALID_VIDEO_STD_PARAMETERS_KHR;

         /*  num_units_in_display_tick  */
         vl_bitstream_put_bits(&enc, 32, timing_info->num_units_in_display_tick);
         /*  time_scale  */
         vl_bitstream_put_bits(&enc, 32, timing_info->time_scale);
         /*  equal_picture_interval  */
         vl_bitstream_put_bits(&enc, 1, timing_info->flags.equal_picture_interval);
         /*  num_ticks_per_picture_minus_1  */
         if (timing_info->flags.equal_picture_interval)
            vl_bitstream_put_uvlc(&enc, timing_info->num_ticks_per_picture_minus_1);

         /*  decoder_model_info_present_flag  */
         if (decoder_model) {
            decoder_model_present_flag = 1;
            vl_bitstream_put_bits(&enc, 1, 1);
            vl_bitstream_put_bits(&enc, 5, decoder_model->buffer_delay_length_minus_1);
            vl_bitstream_put_bits(&enc, 32, decoder_model->num_units_in_decoding_tick);
            vl_bitstream_put_bits(&enc, 5, decoder_model->buffer_removal_time_length_minus_1);
            vl_bitstream_put_bits(&enc, 5, decoder_model->frame_presentation_time_length_minus_1);
         } else {
            vl_bitstream_put_bits(&enc, 1, 0);
         }
      }

      /*  initial_display_delay_present_flag  */
      vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.initial_display_delay_present_flag);

      /*  operating_points_cnt_minus_1  */
      vl_bitstream_put_bits(&enc, 5, num_op_points - 1);

      for (uint32_t i = 0; i < num_op_points; i++) {
         const StdVideoEncodeAV1OperatingPointInfo *op_point_info = &op_points[i];
         vl_bitstream_put_bits(&enc, 12, op_point_info->operating_point_idc);
         vl_bitstream_put_bits(&enc, 5, op_point_info->seq_level_idx);
         if (op_point_info->seq_level_idx > 7)
            vl_bitstream_put_bits(&enc, 1, op_point_info->seq_tier);

         if (decoder_model_present_flag) {
            vl_bitstream_put_bits(&enc, 1, op_point_info->flags.decoder_model_present_for_this_op);
            if (op_point_info->flags.decoder_model_present_for_this_op) {
               vl_bitstream_put_uvlc(&enc, op_point_info->decoder_buffer_delay);
               vl_bitstream_put_uvlc(&enc, op_point_info->encoder_buffer_delay);
               vl_bitstream_put_bits(&enc, 1, op_point_info->flags.low_delay_mode_flag);
            }
         }

         if (seq_hdr->flags.initial_display_delay_present_flag) {
            vl_bitstream_put_bits(&enc, 1, op_point_info->flags.initial_display_delay_present_for_this_op);
            if (op_point_info->flags.initial_display_delay_present_for_this_op) {
               vl_bitstream_put_bits(&enc, 4, op_point_info->initial_display_delay_minus_1);
            }
         }
      }
   }

   /*  frame_width_bits_minus_1  */
   vl_bitstream_put_bits(&enc, 4, seq_hdr->frame_width_bits_minus_1);
   /*  frame_height_bits_minus_1  */
   vl_bitstream_put_bits(&enc, 4, seq_hdr->frame_height_bits_minus_1);
   /*  max_frame_width_minus_1  */
   vl_bitstream_put_bits(&enc, seq_hdr->frame_width_bits_minus_1 + 1, seq_hdr->max_frame_width_minus_1);
   /*  max_frame_height_minus_1  */
   vl_bitstream_put_bits(&enc, seq_hdr->frame_height_bits_minus_1 + 1, seq_hdr->max_frame_height_minus_1);

   if (!seq_hdr->flags.reduced_still_picture_header)
      vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.frame_id_numbers_present_flag);

   if (seq_hdr->flags.frame_id_numbers_present_flag) {
      vl_bitstream_put_bits(&enc, 4, seq_hdr->delta_frame_id_length_minus_2);
      vl_bitstream_put_bits(&enc, 3, seq_hdr->additional_frame_id_length_minus_1);
   }

   /*  use_128x128_superblock  */
   vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.use_128x128_superblock);
   /*  enable_filter_intra  */
   vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_filter_intra);
   /*  enable_intra_edge_filter  */
   vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_intra_edge_filter);

   if (!seq_hdr->flags.reduced_still_picture_header) {
      /*  enable_interintra_compound  */
      vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_interintra_compound);
      /*  enable_masked_compound  */
      vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_masked_compound);
      /*  enable_warped_motion  */
      vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_warped_motion);
      /*  enable_dual_filter  */
      vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_dual_filter);
      /*  enable_order_hint  */
      vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_order_hint);

      if (seq_hdr->flags.enable_order_hint) {
         /*  enable_jnt_comp  */
         vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_jnt_comp);
         /*  enable_ref_frame_mvs  */
         vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_ref_frame_mvs);
      }

      if (seq_hdr->seq_force_screen_content_tools == 2 /* SELECT_SCREEN_CONTENT_TOOLS */)
         vl_bitstream_put_bits(&enc, 1, 1); /* seq_choose_screen_content_tools = 1 */
      else {
         vl_bitstream_put_bits(&enc, 1, 0); /* seq_choose_screen_content_tools = 0 */
         vl_bitstream_put_bits(&enc, 1, seq_hdr->seq_force_screen_content_tools);
      }

      if (seq_hdr->seq_force_screen_content_tools > 0) {
         if (seq_hdr->seq_force_integer_mv == 2 /* SELECT_INTEGER_MV */)
            vl_bitstream_put_bits(&enc, 1, seq_hdr->seq_force_integer_mv); /* seq_choose_integer_mv = 1 */
         else {
            vl_bitstream_put_bits(&enc, 1, 0); /* seq_choose_integer_mv = 0 */
            vl_bitstream_put_bits(&enc, 1, seq_hdr->seq_force_integer_mv);
         }
      }

      if (seq_hdr->flags.enable_order_hint)
         vl_bitstream_put_bits(&enc, 3, seq_hdr->order_hint_bits_minus_1);
   } /* !reduced_still_picture_header */

   vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_superres);
   vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_cdef);
   vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.enable_restoration);

   /* color_config() */
   /*  high_bitdepth  */
   vl_bitstream_put_bits(&enc, 1, color->BitDepth > 8);
   /*  mono_chrome  */
   vl_bitstream_put_bits(&enc, 1, color->flags.mono_chrome);
   /*  color_description_present_flag  */
   vl_bitstream_put_bits(&enc, 1, color->flags.color_description_present_flag);

   if (color->flags.color_description_present_flag) {
      /*  color_primaries  */
      vl_bitstream_put_bits(&enc, 8, color->color_primaries);
      /*  transfer_characteristics  */
      vl_bitstream_put_bits(&enc, 8, color->transfer_characteristics);
      /*  matrix_coefficients  */
      vl_bitstream_put_bits(&enc, 8, color->matrix_coefficients);
   }

   /*  color_range  */
   vl_bitstream_put_bits(&enc, 1, color->flags.color_range);
   /*  chroma_sample_position  */
   vl_bitstream_put_bits(&enc, 2, color->chroma_sample_position);
   /*  separate_uv_delta_q  */
   vl_bitstream_put_bits(&enc, 1, color->flags.separate_uv_delta_q);
   /*  film_grain_params_present  */
   vl_bitstream_put_bits(&enc, 1, seq_hdr->flags.film_grain_params_present);

   /*  trailing_one_bit  */
   vl_bitstream_rbsp_trailing(&enc);

   /* obu_size doesn't include the bytes within obu_header or obu_size syntax
    * element (6.2.1), here we use num_obu_size_bytes for obu_size syntax
    * which needs to be removed from the size.
    */
   obu_size = (uint32_t)(vl_bitstream_get_byte_offset(&enc) - size_offset - num_obu_size_bytes);
   vk_video_encode_av1_code_leb128(obu_size_bin, 2, obu_size);

   /* update obu_size */
   for (int i = 0; i < sizeof(obu_size_bin); i++) {
      *(size_offset++) = obu_size_bin[i];
   }

   vl_bitstream_flush(&enc);
   *data_size_ptr += vl_bitstream_get_byte_count(&enc);
   vl_bitstream_encoder_free(&enc);

   return VK_SUCCESS;
}

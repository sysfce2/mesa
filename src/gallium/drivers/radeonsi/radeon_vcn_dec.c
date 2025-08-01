/**************************************************************************
 *
 * Copyright 2017 Advanced Micro Devices, Inc.
 *
 * SPDX-License-Identifier: MIT
 *
 **************************************************************************/

#include "radeon_vcn_dec.h"

#include "pipe/p_video_codec.h"
#include "radeonsi/si_pipe.h"
#include "util/u_memory.h"
#include "util/u_video.h"
#include "util/vl_zscan_data.h"
#include "pspdecryptionparam.h"
#include "cencdecryptionparam.h"

#include <assert.h>
#include <stdio.h>

#include "ac_vcn_av1_default.h"
#include "ac_debug.h"

#define FB_BUFFER_OFFSET             0x2000
#define FB_BUFFER_SIZE               2048
#define IT_SCALING_TABLE_SIZE        992
#define VP9_PROBS_TABLE_SIZE         (RDECODE_VP9_PROBS_DATA_SIZE + 256)

#define NUM_MPEG2_REFS 6
#define NUM_H264_REFS  17
#define NUM_VC1_REFS   5
#define NUM_VP9_REFS   8
#define NUM_AV1_REFS   8
#define NUM_AV1_REFS_PER_FRAME 7

static enum pipe_format get_buffer_format(struct radeon_decoder *dec);
static unsigned calc_dpb_size(struct radeon_decoder *dec);
static unsigned calc_ctx_size_h264_perf(struct radeon_decoder *dec);
static unsigned calc_ctx_size_h265_main(struct radeon_decoder *dec);
static unsigned calc_ctx_size_h265_main10(struct radeon_decoder *dec,
                                          struct pipe_h265_picture_desc *pic);

static void radeon_dec_destroy_associated_data(void *data)
{
   /* NOOP, since we only use an intptr */
}

static rvcn_dec_message_avc_t get_h264_msg(struct radeon_decoder *dec,
                                           struct pipe_video_buffer *target,
                                           struct pipe_h264_picture_desc *pic)
{
   rvcn_dec_message_avc_t result;
   unsigned i, j, k, num_refs = 0;

   memset(&result, 0, sizeof(result));
   switch (pic->base.profile) {
   case PIPE_VIDEO_PROFILE_MPEG4_AVC_BASELINE:
   case PIPE_VIDEO_PROFILE_MPEG4_AVC_CONSTRAINED_BASELINE:
      result.profile = RDECODE_H264_PROFILE_BASELINE;
      break;

   case PIPE_VIDEO_PROFILE_MPEG4_AVC_MAIN:
      result.profile = RDECODE_H264_PROFILE_MAIN;
      break;

   case PIPE_VIDEO_PROFILE_MPEG4_AVC_HIGH:
      result.profile = RDECODE_H264_PROFILE_HIGH;
      break;

   default:
      assert(0);
      break;
   }

   result.level = dec->base.level;

   result.sps_info_flags = 0;
   result.sps_info_flags |= pic->pps->sps->direct_8x8_inference_flag
                            << RDECODE_SPS_INFO_H264_DIRECT_8X8_INFERENCE_FLAG_SHIFT;
   result.sps_info_flags |= pic->pps->sps->mb_adaptive_frame_field_flag
                            << RDECODE_SPS_INFO_H264_MB_ADAPTIVE_FRAME_FIELD_FLAG_SHIFT;
   result.sps_info_flags |= pic->pps->sps->frame_mbs_only_flag
                            << RDECODE_SPS_INFO_H264_FRAME_MBS_ONLY_FLAG_SHIFT;
   result.sps_info_flags |= pic->pps->sps->delta_pic_order_always_zero_flag
                            << RDECODE_SPS_INFO_H264_DELTA_PIC_ORDER_ALWAYS_ZERO_FLAG_SHIFT;
   result.sps_info_flags |= pic->pps->sps->gaps_in_frame_num_value_allowed_flag
                            << RDECODE_SPS_INFO_H264_GAPS_IN_FRAME_NUM_VALUE_ALLOWED_FLAG_SHIFT;
   result.sps_info_flags |= ((dec->dpb_type >= DPB_DYNAMIC_TIER_2) ? 0 : 1)
                              << RDECODE_SPS_INFO_H264_EXTENSION_SUPPORT_FLAG_SHIFT;

   result.chroma_format = pic->pps->sps->chroma_format_idc;
   result.bit_depth_luma_minus8 = pic->pps->sps->bit_depth_luma_minus8;
   result.bit_depth_chroma_minus8 = pic->pps->sps->bit_depth_chroma_minus8;
   result.log2_max_frame_num_minus4 = pic->pps->sps->log2_max_frame_num_minus4;
   result.pic_order_cnt_type = pic->pps->sps->pic_order_cnt_type;
   result.log2_max_pic_order_cnt_lsb_minus4 = pic->pps->sps->log2_max_pic_order_cnt_lsb_minus4;

   result.pps_info_flags = 0;
   result.pps_info_flags |= pic->pps->transform_8x8_mode_flag << 0;
   result.pps_info_flags |= pic->pps->redundant_pic_cnt_present_flag << 1;
   result.pps_info_flags |= pic->pps->constrained_intra_pred_flag << 2;
   result.pps_info_flags |= pic->pps->deblocking_filter_control_present_flag << 3;
   result.pps_info_flags |= pic->pps->weighted_bipred_idc << 4;
   result.pps_info_flags |= pic->pps->weighted_pred_flag << 6;
   result.pps_info_flags |= pic->pps->bottom_field_pic_order_in_frame_present_flag << 7;
   result.pps_info_flags |= pic->pps->entropy_coding_mode_flag << 8;

   result.num_slice_groups_minus1 = pic->pps->num_slice_groups_minus1;
   result.slice_group_map_type = pic->pps->slice_group_map_type;
   result.slice_group_change_rate_minus1 = pic->pps->slice_group_change_rate_minus1;
   result.pic_init_qp_minus26 = pic->pps->pic_init_qp_minus26;
   result.chroma_qp_index_offset = pic->pps->chroma_qp_index_offset;
   result.second_chroma_qp_index_offset = pic->pps->second_chroma_qp_index_offset;

   memcpy(result.scaling_list_4x4, pic->pps->ScalingList4x4, 6 * 16);
   memcpy(result.scaling_list_8x8, pic->pps->ScalingList8x8, 2 * 64);

   memcpy(dec->it, result.scaling_list_4x4, 6 * 16);
   memcpy((dec->it + 96), result.scaling_list_8x8, 2 * 64);

   result.num_ref_frames = pic->num_ref_frames;

   result.num_ref_idx_l0_active_minus1 = pic->num_ref_idx_l0_active_minus1;
   result.num_ref_idx_l1_active_minus1 = pic->num_ref_idx_l1_active_minus1;

   result.frame_num = pic->frame_num;
   memcpy(result.frame_num_list, pic->frame_num_list, 4 * 16);
   result.curr_field_order_cnt_list[0] = pic->field_order_cnt[0];
   result.curr_field_order_cnt_list[1] = pic->field_order_cnt[1];
   memcpy(result.field_order_cnt_list, pic->field_order_cnt_list, 4 * 16 * 2);
   result.non_existing_frame_flags = 0;
   result.used_for_reference_flags = 0;

   if (dec->dpb_type < DPB_DYNAMIC_TIER_2) {
      result.decoded_pic_idx = pic->frame_num;
      goto end;
   }

   result.decoded_pic_idx = 0xff;
   memset(result.ref_frame_list, 0xff, sizeof(result.ref_frame_list));

   for (i = 0; i < ARRAY_SIZE(pic->ref) + 1; i++) {
      if (dec->render_pic_list[i]) {
         bool found = false;
         for (j = 0; j < ARRAY_SIZE(pic->ref); j++) {
            if (dec->render_pic_list[i] == pic->ref[j]) {
               result.ref_frame_list[j] = i;
               if (pic->is_long_term[j])
                  result.ref_frame_list[j] |= 0x80;
               if (pic->top_is_reference[j])
                  result.used_for_reference_flags |= (1 << (2 * j));
               if (pic->bottom_is_reference[j])
                  result.used_for_reference_flags |= (1 << (2 * j + 1));
               result.curr_pic_ref_frame_num++;
               dec->ref_codec.bufs[num_refs].buf = pic->ref[j];
               dec->ref_codec.bufs[num_refs++].index = i;
               found = true;
            }
         }
         if (!found)
            dec->render_pic_list[i] = NULL;
      }
      if (dec->render_pic_list[i] == target)
         result.decoded_pic_idx = i;
   }

   /* Target surface can also be a reference (other field) */
   if (result.decoded_pic_idx == 0xff) {
      for (i = 0; i < ARRAY_SIZE(pic->ref) + 1; i++) {
         if (!dec->render_pic_list[i]) {
            dec->render_pic_list[i] = target;
            result.decoded_pic_idx = i;
            break;
         }
      }
   }

   for (i = 0; i < ARRAY_SIZE(pic->ref) && pic->ref[i]; i++) {
      if (pic->ref[i] && result.ref_frame_list[i] == 0xff) {
         result.non_existing_frame_flags |= 1 << i;
      } else {
         for (j = 0; j < ARRAY_SIZE(dec->h264_valid_ref_num); j++) {
            if ((dec->h264_valid_ref_num[j] != (unsigned)-1)
                && (dec->h264_valid_ref_num[j] == result.frame_num_list[i]))
               break;
         }
         for (k = 0; k < ARRAY_SIZE(dec->h264_valid_poc_num); k++) {
            if ((dec->h264_valid_poc_num[k] != (unsigned)-1)
                  && ((dec->h264_valid_poc_num[k] == result.field_order_cnt_list[i][0])
                    || dec->h264_valid_poc_num[k] == result.field_order_cnt_list[i][1]))
               break;
         }
         if ((j == ARRAY_SIZE(dec->h264_valid_ref_num)) && (k == ARRAY_SIZE(dec->h264_valid_poc_num))) {
            result.non_existing_frame_flags |= 1 << i;
            result.curr_pic_ref_frame_num--;
            result.ref_frame_list[i] = 0xff;
         }
      }
   }

   /* need at least one reference for P/B frames */
   if (result.curr_pic_ref_frame_num == 0 && pic->slice_parameter.slice_info_present) {
      for (i = 0; i < pic->slice_count; i++) {
         if (pic->slice_parameter.slice_type[i] % 5 != 2) {
            result.curr_pic_ref_frame_num++;
            result.ref_frame_list[0] = 0;
            result.non_existing_frame_flags &= ~1;
            break;
         }
      }
   }

   for (i = 0; i < ARRAY_SIZE(result.ref_frame_list); i++) {
      if (result.ref_frame_list[i] != 0xff) {
         dec->h264_valid_ref_num[i]         = result.frame_num_list[i];
         dec->h264_valid_poc_num[2 * i]     = pic->top_is_reference[i] ?
                      result.field_order_cnt_list[i][0] : (unsigned) -1;
         dec->h264_valid_poc_num[2 * i + 1] = pic->bottom_is_reference[i] ?
                      result.field_order_cnt_list[i][1] : (unsigned) -1;
      } else {
         dec->h264_valid_ref_num[i]         =
         dec->h264_valid_poc_num[2 * i]     =
         dec->h264_valid_poc_num[2 * i + 1] = (unsigned) -1;
      }
   }

   dec->h264_valid_ref_num[ARRAY_SIZE(dec->h264_valid_ref_num) - 1] = result.frame_num;
   dec->h264_valid_poc_num[ARRAY_SIZE(dec->h264_valid_poc_num) - 2] =
                     pic->field_pic_flag && pic->bottom_field_flag ?
                     (unsigned) -1 : result.curr_field_order_cnt_list[0];
   dec->h264_valid_poc_num[ARRAY_SIZE(dec->h264_valid_poc_num) - 1] =
                     pic->field_pic_flag && !pic->bottom_field_flag ?
                     (unsigned) -1 : result.curr_field_order_cnt_list[1];

   if (dec->dpb_type >= DPB_DYNAMIC_TIER_2) {
      dec->ref_codec.bts = CODEC_8_BITS;
      dec->ref_codec.index = result.decoded_pic_idx;
      dec->ref_codec.ref_size = 16;
      dec->ref_codec.num_refs = num_refs;
      STATIC_ASSERT(sizeof(dec->ref_codec.ref_list) == sizeof(result.ref_frame_list));
      memcpy(dec->ref_codec.ref_list, result.ref_frame_list, sizeof(result.ref_frame_list));
   }

end:
   return result;
}

static rvcn_dec_message_hevc_t get_h265_msg(struct radeon_decoder *dec,
                                            struct pipe_video_buffer *target,
                                            struct pipe_h265_picture_desc *pic)
{
   rvcn_dec_message_hevc_t result;
   unsigned i, j, num_refs = 0, valid_ref = UINT32_MAX;

   memset(&result, 0, sizeof(result));
   result.sps_info_flags = 0;
   result.sps_info_flags |= pic->pps->sps->scaling_list_enabled_flag << 0;
   result.sps_info_flags |= pic->pps->sps->amp_enabled_flag << 1;
   result.sps_info_flags |= pic->pps->sps->sample_adaptive_offset_enabled_flag << 2;
   result.sps_info_flags |= pic->pps->sps->pcm_enabled_flag << 3;
   result.sps_info_flags |= pic->pps->sps->pcm_loop_filter_disabled_flag << 4;
   result.sps_info_flags |= pic->pps->sps->long_term_ref_pics_present_flag << 5;
   result.sps_info_flags |= pic->pps->sps->sps_temporal_mvp_enabled_flag << 6;
   result.sps_info_flags |= pic->pps->sps->strong_intra_smoothing_enabled_flag << 7;
   result.sps_info_flags |= pic->pps->sps->separate_colour_plane_flag << 8;
   if (pic->NumShortTermPictureSliceHeaderBits != 0) {
      result.sps_info_flags |= 1 << 11;
      result.st_rps_bits = pic->NumShortTermPictureSliceHeaderBits;
   }

   result.chroma_format = pic->pps->sps->chroma_format_idc;
   result.bit_depth_luma_minus8 = pic->pps->sps->bit_depth_luma_minus8;
   result.bit_depth_chroma_minus8 = pic->pps->sps->bit_depth_chroma_minus8;
   result.log2_max_pic_order_cnt_lsb_minus4 = pic->pps->sps->log2_max_pic_order_cnt_lsb_minus4;
   result.sps_max_dec_pic_buffering_minus1 = pic->pps->sps->sps_max_dec_pic_buffering_minus1;
   result.log2_min_luma_coding_block_size_minus3 =
      pic->pps->sps->log2_min_luma_coding_block_size_minus3;
   result.log2_diff_max_min_luma_coding_block_size =
      pic->pps->sps->log2_diff_max_min_luma_coding_block_size;
   result.log2_min_transform_block_size_minus2 =
      pic->pps->sps->log2_min_transform_block_size_minus2;
   result.log2_diff_max_min_transform_block_size =
      pic->pps->sps->log2_diff_max_min_transform_block_size;
   result.max_transform_hierarchy_depth_inter = pic->pps->sps->max_transform_hierarchy_depth_inter;
   result.max_transform_hierarchy_depth_intra = pic->pps->sps->max_transform_hierarchy_depth_intra;
   result.pcm_sample_bit_depth_luma_minus1 = pic->pps->sps->pcm_sample_bit_depth_luma_minus1;
   result.pcm_sample_bit_depth_chroma_minus1 = pic->pps->sps->pcm_sample_bit_depth_chroma_minus1;
   result.log2_min_pcm_luma_coding_block_size_minus3 =
      pic->pps->sps->log2_min_pcm_luma_coding_block_size_minus3;
   result.log2_diff_max_min_pcm_luma_coding_block_size =
      pic->pps->sps->log2_diff_max_min_pcm_luma_coding_block_size;
   result.num_short_term_ref_pic_sets = pic->pps->sps->num_short_term_ref_pic_sets;

   result.pps_info_flags = 0;
   result.pps_info_flags |= pic->pps->dependent_slice_segments_enabled_flag << 0;
   result.pps_info_flags |= pic->pps->output_flag_present_flag << 1;
   result.pps_info_flags |= pic->pps->sign_data_hiding_enabled_flag << 2;
   result.pps_info_flags |= pic->pps->cabac_init_present_flag << 3;
   result.pps_info_flags |= pic->pps->constrained_intra_pred_flag << 4;
   result.pps_info_flags |= pic->pps->transform_skip_enabled_flag << 5;
   result.pps_info_flags |= pic->pps->cu_qp_delta_enabled_flag << 6;
   result.pps_info_flags |= pic->pps->pps_slice_chroma_qp_offsets_present_flag << 7;
   result.pps_info_flags |= pic->pps->weighted_pred_flag << 8;
   result.pps_info_flags |= pic->pps->weighted_bipred_flag << 9;
   result.pps_info_flags |= pic->pps->transquant_bypass_enabled_flag << 10;
   result.pps_info_flags |= pic->pps->tiles_enabled_flag << 11;
   result.pps_info_flags |= pic->pps->entropy_coding_sync_enabled_flag << 12;
   result.pps_info_flags |= pic->pps->uniform_spacing_flag << 13;
   result.pps_info_flags |= pic->pps->loop_filter_across_tiles_enabled_flag << 14;
   result.pps_info_flags |= pic->pps->pps_loop_filter_across_slices_enabled_flag << 15;
   result.pps_info_flags |= pic->pps->deblocking_filter_override_enabled_flag << 16;
   result.pps_info_flags |= pic->pps->pps_deblocking_filter_disabled_flag << 17;
   result.pps_info_flags |= pic->pps->lists_modification_present_flag << 18;
   result.pps_info_flags |= pic->pps->slice_segment_header_extension_present_flag << 19;

   result.num_extra_slice_header_bits = pic->pps->num_extra_slice_header_bits;
   result.num_long_term_ref_pic_sps = pic->pps->sps->num_long_term_ref_pics_sps;
   result.num_ref_idx_l0_default_active_minus1 = pic->pps->num_ref_idx_l0_default_active_minus1;
   result.num_ref_idx_l1_default_active_minus1 = pic->pps->num_ref_idx_l1_default_active_minus1;
   result.pps_cb_qp_offset = pic->pps->pps_cb_qp_offset;
   result.pps_cr_qp_offset = pic->pps->pps_cr_qp_offset;
   result.pps_beta_offset_div2 = pic->pps->pps_beta_offset_div2;
   result.pps_tc_offset_div2 = pic->pps->pps_tc_offset_div2;
   result.diff_cu_qp_delta_depth = pic->pps->diff_cu_qp_delta_depth;
   result.num_tile_columns_minus1 = pic->pps->num_tile_columns_minus1;
   result.num_tile_rows_minus1 = pic->pps->num_tile_rows_minus1;
   result.log2_parallel_merge_level_minus2 = pic->pps->log2_parallel_merge_level_minus2;
   result.init_qp_minus26 = pic->pps->init_qp_minus26;

   for (i = 0; i < 19; ++i)
      result.column_width_minus1[i] = pic->pps->column_width_minus1[i];

   for (i = 0; i < 21; ++i)
      result.row_height_minus1[i] = pic->pps->row_height_minus1[i];

   result.num_delta_pocs_ref_rps_idx = pic->NumDeltaPocsOfRefRpsIdx;
   result.curr_poc = pic->CurrPicOrderCntVal;

   result.curr_idx = 0x7F;
   memset(result.ref_pic_list, 0x7F, sizeof(result.ref_pic_list));

   for (i = 0; i < ARRAY_SIZE(pic->ref) + 1; i++) {
      if (dec->render_pic_list[i]) {
         bool found = false;
         for (j = 0; j < ARRAY_SIZE(pic->ref); j++) {
            if (dec->render_pic_list[i] == pic->ref[j]) {
               result.poc_list[j] = pic->PicOrderCntVal[j];
               result.ref_pic_list[j] = i;
               dec->ref_codec.bufs[num_refs].buf = pic->ref[j];
               dec->ref_codec.bufs[num_refs++].index = i;
               valid_ref = j;
               found = true;
            }
         }
         if (!found)
            dec->render_pic_list[i] = NULL;
      }
      if (result.curr_idx == 0x7F && !dec->render_pic_list[i]) {
         dec->render_pic_list[i] = target;
         result.curr_idx = i;
      }
   }

   if (valid_ref != UINT32_MAX) {
      for (i = 0; i < ARRAY_SIZE(pic->ref); i++) {
         if (pic->ref[i] && result.ref_pic_list[i] == 0x7F) {
            result.poc_list[i] = pic->PicOrderCntVal[valid_ref];
            result.ref_pic_list[i] = result.ref_pic_list[valid_ref];
            num_refs++;
         }
      }
   }

   for (i = 0; i < 8; ++i) {
      result.ref_pic_set_st_curr_before[i] = 0xFF;
      result.ref_pic_set_st_curr_after[i] = 0xFF;
      result.ref_pic_set_lt_curr[i] = 0xFF;
   }

   for (i = 0; i < pic->NumPocStCurrBefore; ++i)
      result.ref_pic_set_st_curr_before[i] = pic->RefPicSetStCurrBefore[i];

   for (i = 0; i < pic->NumPocStCurrAfter; ++i)
      result.ref_pic_set_st_curr_after[i] = pic->RefPicSetStCurrAfter[i];

   for (i = 0; i < pic->NumPocLtCurr; ++i)
      result.ref_pic_set_lt_curr[i] = pic->RefPicSetLtCurr[i];

   for (i = 0; i < 6; ++i)
      result.ucScalingListDCCoefSizeID2[i] = pic->pps->sps->ScalingListDCCoeff16x16[i];

   for (i = 0; i < 2; ++i)
      result.ucScalingListDCCoefSizeID3[i] = pic->pps->sps->ScalingListDCCoeff32x32[i];

   memcpy(dec->it, pic->pps->sps->ScalingList4x4, 6 * 16);
   memcpy(dec->it + 96, pic->pps->sps->ScalingList8x8, 6 * 64);
   memcpy(dec->it + 480, pic->pps->sps->ScalingList16x16, 6 * 64);
   memcpy(dec->it + 864, pic->pps->sps->ScalingList32x32, 2 * 64);

   if (pic->base.profile == PIPE_VIDEO_PROFILE_HEVC_MAIN_10) {
      if (target->buffer_format == PIPE_FORMAT_P010 || target->buffer_format == PIPE_FORMAT_P016) {
         result.p010_mode = 1;
         result.msb_mode = 1;
      } else {
         result.p010_mode = 0;
         result.luma_10to8 = 5;
         result.chroma_10to8 = 5;
         result.hevc_reserved[0] = 4; /* sclr_luma10to8 */
         result.hevc_reserved[1] = 4; /* sclr_chroma10to8 */
      }
   }

   if (dec->dpb_type >= DPB_DYNAMIC_TIER_2) {
      dec->ref_codec.bts = (pic->base.profile == PIPE_VIDEO_PROFILE_HEVC_MAIN_10) ?
         CODEC_10_BITS : CODEC_8_BITS;
      dec->ref_codec.index = result.curr_idx;
      dec->ref_codec.ref_size = 15;
      dec->ref_codec.num_refs = num_refs;
      STATIC_ASSERT(sizeof(dec->ref_codec.ref_list) == sizeof(result.ref_pic_list));
      memcpy(dec->ref_codec.ref_list, result.ref_pic_list, sizeof(result.ref_pic_list));
   }
   return result;
}

static rvcn_dec_message_vp9_t get_vp9_msg(struct radeon_decoder *dec,
                                          struct pipe_video_buffer *target,
                                          struct pipe_vp9_picture_desc *pic)
{
   rvcn_dec_message_vp9_t result;
   unsigned i, j, num_refs = 0, valid_ref = UINT32_MAX;

   memset(&result, 0, sizeof(result));

   /* segment table */
   rvcn_dec_vp9_probs_segment_t *prbs = (rvcn_dec_vp9_probs_segment_t *)(dec->probs);

   if (pic->picture_parameter.pic_fields.segmentation_enabled) {
      for (i = 0; i < 8; ++i) {
         prbs->seg.feature_data[i] =
            (pic->slice_parameter.seg_param[i].alt_quant & 0xffff) |
            ((pic->slice_parameter.seg_param[i].alt_lf & 0xff) << 16) |
            ((pic->slice_parameter.seg_param[i].segment_flags.segment_reference & 0xf) << 24);
         prbs->seg.feature_mask[i] =
            (pic->slice_parameter.seg_param[i].alt_quant_enabled << 0) |
            (pic->slice_parameter.seg_param[i].alt_lf_enabled << 1) |
            (pic->slice_parameter.seg_param[i].segment_flags.segment_reference_enabled << 2) |
            (pic->slice_parameter.seg_param[i].segment_flags.segment_reference_skipped << 3);
      }

      for (i = 0; i < 7; ++i)
         prbs->seg.tree_probs[i] = pic->picture_parameter.mb_segment_tree_probs[i];

      for (i = 0; i < 3; ++i)
         prbs->seg.pred_probs[i] = pic->picture_parameter.segment_pred_probs[i];

      prbs->seg.abs_delta = pic->picture_parameter.abs_delta;
   } else
      memset(prbs->segment_data, 0, sizeof(prbs->segment_data));

   result.frame_header_flags = (pic->picture_parameter.pic_fields.frame_type
                                << RDECODE_FRAME_HDR_INFO_VP9_FRAME_TYPE_SHIFT) &
                               RDECODE_FRAME_HDR_INFO_VP9_FRAME_TYPE_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_fields.error_resilient_mode
                                 << RDECODE_FRAME_HDR_INFO_VP9_ERROR_RESILIENT_MODE_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_ERROR_RESILIENT_MODE_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_fields.intra_only
                                 << RDECODE_FRAME_HDR_INFO_VP9_INTRA_ONLY_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_INTRA_ONLY_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_fields.allow_high_precision_mv
                                 << RDECODE_FRAME_HDR_INFO_VP9_ALLOW_HIGH_PRECISION_MV_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_ALLOW_HIGH_PRECISION_MV_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_fields.frame_parallel_decoding_mode
                                 << RDECODE_FRAME_HDR_INFO_VP9_FRAME_PARALLEL_DECODING_MODE_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_FRAME_PARALLEL_DECODING_MODE_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_fields.refresh_frame_context
                                 << RDECODE_FRAME_HDR_INFO_VP9_REFRESH_FRAME_CONTEXT_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_REFRESH_FRAME_CONTEXT_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_fields.segmentation_enabled
                                 << RDECODE_FRAME_HDR_INFO_VP9_SEGMENTATION_ENABLED_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_SEGMENTATION_ENABLED_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_fields.segmentation_update_map
                                 << RDECODE_FRAME_HDR_INFO_VP9_SEGMENTATION_UPDATE_MAP_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_SEGMENTATION_UPDATE_MAP_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_fields.segmentation_temporal_update
                                 << RDECODE_FRAME_HDR_INFO_VP9_SEGMENTATION_TEMPORAL_UPDATE_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_SEGMENTATION_TEMPORAL_UPDATE_MASK;

   result.frame_header_flags |= (pic->picture_parameter.mode_ref_delta_enabled
                                 << RDECODE_FRAME_HDR_INFO_VP9_MODE_REF_DELTA_ENABLED_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_MODE_REF_DELTA_ENABLED_MASK;

   result.frame_header_flags |= (pic->picture_parameter.mode_ref_delta_update
                                 << RDECODE_FRAME_HDR_INFO_VP9_MODE_REF_DELTA_UPDATE_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_VP9_MODE_REF_DELTA_UPDATE_MASK;

   result.frame_header_flags |=
      ((dec->show_frame && !pic->picture_parameter.pic_fields.error_resilient_mode &&
        dec->last_width == dec->base.width && dec->last_height == dec->base.height)
       << RDECODE_FRAME_HDR_INFO_VP9_USE_PREV_IN_FIND_MV_REFS_SHIFT) &
      RDECODE_FRAME_HDR_INFO_VP9_USE_PREV_IN_FIND_MV_REFS_MASK;
   dec->show_frame = pic->picture_parameter.pic_fields.show_frame;

   result.frame_header_flags |=  (1 << RDECODE_FRAME_HDR_INFO_VP9_USE_UNCOMPRESSED_HEADER_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_VP9_USE_UNCOMPRESSED_HEADER_MASK;

   result.interp_filter = pic->picture_parameter.pic_fields.mcomp_filter_type;

   result.frame_context_idx = pic->picture_parameter.pic_fields.frame_context_idx;
   result.reset_frame_context = pic->picture_parameter.pic_fields.reset_frame_context;

   result.filter_level = pic->picture_parameter.filter_level;
   result.sharpness_level = pic->picture_parameter.sharpness_level;

   for (i = 0; i < 8; ++i)
      memcpy(result.lf_adj_level[i], pic->slice_parameter.seg_param[i].filter_level, 4 * 2);

   if (pic->picture_parameter.pic_fields.lossless_flag) {
      result.base_qindex = 0;
      result.y_dc_delta_q = 0;
      result.uv_ac_delta_q = 0;
      result.uv_dc_delta_q = 0;
   } else {
      result.base_qindex = pic->picture_parameter.base_qindex;
      result.y_dc_delta_q = pic->picture_parameter.y_dc_delta_q;
      result.uv_ac_delta_q = pic->picture_parameter.uv_ac_delta_q;
      result.uv_dc_delta_q = pic->picture_parameter.uv_dc_delta_q;
   }

   result.log2_tile_cols = pic->picture_parameter.log2_tile_columns;
   result.log2_tile_rows = pic->picture_parameter.log2_tile_rows;
   result.chroma_format = 1;
   result.bit_depth_luma_minus8 = result.bit_depth_chroma_minus8 =
      (pic->picture_parameter.bit_depth - 8);

   result.vp9_frame_size = align(dec->bs_size, 128);
   result.uncompressed_header_size = pic->picture_parameter.frame_header_length_in_bytes;
   result.compressed_header_size = pic->picture_parameter.first_partition_size;

   result.curr_pic_idx = 0x7F;
   memset(result.ref_frame_map, 0x7F, sizeof(result.ref_frame_map));

   for (i = 0; i < NUM_VP9_REFS + 1; i++) {
      if (dec->render_pic_list[i]) {
         bool found = false;
         for (j = 0; j < NUM_VP9_REFS; j++) {
            if (dec->render_pic_list[i] == pic->ref[j]) {
               result.ref_frame_map[j] = i;
               dec->ref_codec.bufs[num_refs].buf = pic->ref[j];
               dec->ref_codec.bufs[num_refs++].index = i;
               valid_ref = j;
               found = true;
            }
         }
         if (!found)
            dec->render_pic_list[i] = NULL;
      }
      if (result.curr_pic_idx == 0x7F && !dec->render_pic_list[i]) {
         dec->render_pic_list[i] = target;
         result.curr_pic_idx = i;
      }
   }

   for (i = 0; i < NUM_VP9_REFS; i++) {
      if (pic->ref[i] && result.ref_frame_map[i] == 0x7F) {
         result.ref_frame_map[i] = valid_ref == UINT32_MAX ? 0 : result.ref_frame_map[valid_ref];
         num_refs++;
      }
   }

   result.frame_refs[0] = result.ref_frame_map[pic->picture_parameter.pic_fields.last_ref_frame];
   result.ref_frame_sign_bias[0] = pic->picture_parameter.pic_fields.last_ref_frame_sign_bias;
   result.frame_refs[1] = result.ref_frame_map[pic->picture_parameter.pic_fields.golden_ref_frame];
   result.ref_frame_sign_bias[1] = pic->picture_parameter.pic_fields.golden_ref_frame_sign_bias;
   result.frame_refs[2] = result.ref_frame_map[pic->picture_parameter.pic_fields.alt_ref_frame];
   result.ref_frame_sign_bias[2] = pic->picture_parameter.pic_fields.alt_ref_frame_sign_bias;

   if (pic->base.profile == PIPE_VIDEO_PROFILE_VP9_PROFILE2) {
      if (target->buffer_format == PIPE_FORMAT_P010 || target->buffer_format == PIPE_FORMAT_P016) {
         result.p010_mode = 1;
         result.msb_mode = 1;
      } else {
         result.p010_mode = 0;
         result.luma_10to8 = 1;
         result.chroma_10to8 = 1;
      }
   }

   if (dec->dpb_type >= DPB_DYNAMIC_TIER_2) {
      dec->ref_codec.bts = (pic->base.profile == PIPE_VIDEO_PROFILE_VP9_PROFILE2) ?
         CODEC_10_BITS : CODEC_8_BITS;
      dec->ref_codec.index = result.curr_pic_idx;
      dec->ref_codec.ref_size = 8;
      dec->ref_codec.num_refs = num_refs;
      memset(dec->ref_codec.ref_list, 0x7f, sizeof(dec->ref_codec.ref_list));
      memcpy(dec->ref_codec.ref_list, result.ref_frame_map, sizeof(result.ref_frame_map));
   }

   dec->last_width = dec->base.width;
   dec->last_height = dec->base.height;

   return result;
}

static void set_drm_keyblob(rvcn_dec_message_drm_keyblob_t *drm_keyblob, amd_secure_buffer_format *secure_buf)
{
   memcpy(drm_keyblob->contentKey, secure_buf->key_blob.local_policy.wrapped_key, 16);
   memcpy(drm_keyblob->signature, secure_buf->key_blob.local_policy.signature, 16);
   memcpy(&drm_keyblob->policyIndex, secure_buf->key_blob.local_policy.native_policy.enabled_policy_index, 4);
   memcpy(drm_keyblob->policyArray, secure_buf->key_blob.local_policy.native_policy.policy_array, 32);
}

static void set_drm_keys_cenc(rvcn_dec_message_drm_t *drm, amd_secure_buffer_format *secure_buf)
{
   drm->drm_offset = 0;
   drm->drm_cmd = 0;
   drm->drm_cntl = 0;
   drm->drm_max_res = 0;

   memcpy(drm->drm_wrapped_key, secure_buf->key_blob.wrapped_key, 16);
   memcpy(drm->drm_key, secure_buf->key_blob.wrapped_key_iv, 16);
   memcpy(drm->drm_counter, secure_buf->desc.iv, 16);

   drm->drm_subsample_size = secure_buf->desc.subsamples_length;

   drm->drm_cmd |= 1 << DRM_CMD_KEY_SHIFT;
   drm->drm_cmd |= 1 << DRM_CMD_CNT_KEY_SHIFT;
   drm->drm_cmd |= 1 << DRM_CMD_CNT_DATA_SHIFT;
   drm->drm_cmd |= 1 << DRM_CMD_OFFSET_SHIFT;
   drm->drm_cmd |= 1 << DRM_CMD_GEN_MASK_SHIFT;
   drm->drm_cmd |= 0xFF << DRM_CMD_BYTE_MASK_SHIFT;
   drm->drm_cmd |= secure_buf->key_blob.u.s.drm_session_id << DRM_CMD_SESSION_SEL_SHIFT;
   drm->drm_cmd |= 1 << DRM_CMD_UNWRAP_KEY_SHIFT;

   drm->drm_cntl |= 0x3 << DRM_CNTL_CENC_ENABLE_SHIFT;
}

static void set_drm_keys(rvcn_dec_message_drm_t *drm, DECRYPT_PARAMETERS *decrypted)
{
   int cbc = decrypted->u.s.cbc;
   int ctr = decrypted->u.s.ctr;
   int id = decrypted->u.s.drm_id;
   int ekc = 1;
   int data1 = 1;
   int data2 = 1;

   drm->drm_cmd = 0;
   drm->drm_cntl = 0;

   drm->drm_cntl = 1 << DRM_CNTL_BYPASS_SHIFT;

   if (cbc || ctr) {
      drm->drm_cntl = 0 << DRM_CNTL_BYPASS_SHIFT;
      drm->drm_cmd |= 0xff << DRM_CMD_BYTE_MASK_SHIFT;

      if (ctr)
         drm->drm_cmd |= 0x00 << DRM_CMD_ALGORITHM_SHIFT;
      else if (cbc)
         drm->drm_cmd |= 0x02 << DRM_CMD_ALGORITHM_SHIFT;

      drm->drm_cmd |= 1 << DRM_CMD_GEN_MASK_SHIFT;
      drm->drm_cmd |= ekc << DRM_CMD_UNWRAP_KEY_SHIFT;
      drm->drm_cmd |= 0 << DRM_CMD_OFFSET_SHIFT;
      drm->drm_cmd |= data2 << DRM_CMD_CNT_DATA_SHIFT;
      drm->drm_cmd |= data1 << DRM_CMD_CNT_KEY_SHIFT;
      drm->drm_cmd |= ekc << DRM_CMD_KEY_SHIFT;
      drm->drm_cmd |= id << DRM_CMD_SESSION_SEL_SHIFT;

      if (ekc)
         memcpy(drm->drm_wrapped_key, decrypted->encrypted_key, 16);
      if (data1)
         memcpy(drm->drm_key, decrypted->session_iv, 16);
      if (data2)
         memcpy(drm->drm_counter, decrypted->encrypted_iv, 16);
      drm->drm_offset = 0;
   }
}

static void rvcn_dec_av1_film_grain_surface(struct pipe_video_buffer **target,
                                            struct pipe_av1_picture_desc *pic)
{
   if (!pic->picture_parameter.film_grain_info.film_grain_info_fields.apply_grain ||
       !pic->film_grain_target)
      return;

   *target = pic->film_grain_target;
}

static rvcn_dec_message_av1_t get_av1_msg(struct radeon_decoder *dec,
                                          struct pipe_video_buffer *target,
                                          struct pipe_av1_picture_desc *pic)
{
   rvcn_dec_message_av1_t result;
   unsigned i, j, num_refs = 0, valid_ref = UINT32_MAX;
   uint16_t tile_count = pic->picture_parameter.tile_cols * pic->picture_parameter.tile_rows;

   memset(&result, 0, sizeof(result));

   result.frame_header_flags = (pic->picture_parameter.pic_info_fields.show_frame
                                << RDECODE_FRAME_HDR_INFO_AV1_SHOW_FRAME_SHIFT) &
                                RDECODE_FRAME_HDR_INFO_AV1_SHOW_FRAME_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_info_fields.disable_cdf_update
                                 << RDECODE_FRAME_HDR_INFO_AV1_DISABLE_CDF_UPDATE_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_DISABLE_CDF_UPDATE_MASK;

   result.frame_header_flags |= ((!pic->picture_parameter.pic_info_fields.disable_frame_end_update_cdf)
                                 << RDECODE_FRAME_HDR_INFO_AV1_REFRESH_FRAME_CONTEXT_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_REFRESH_FRAME_CONTEXT_MASK;

   result.frame_header_flags |= ((pic->picture_parameter.pic_info_fields.frame_type ==
                                 2 /* INTRA_ONLY_FRAME */) << RDECODE_FRAME_HDR_INFO_AV1_INTRA_ONLY_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_INTRA_ONLY_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_info_fields.allow_intrabc
                                 << RDECODE_FRAME_HDR_INFO_AV1_ALLOW_INTRABC_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ALLOW_INTRABC_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_info_fields.allow_high_precision_mv
                                 << RDECODE_FRAME_HDR_INFO_AV1_ALLOW_HIGH_PRECISION_MV_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ALLOW_HIGH_PRECISION_MV_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seq_info_fields.mono_chrome
                                 << RDECODE_FRAME_HDR_INFO_AV1_MONOCHROME_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_MONOCHROME_MASK;

   result.frame_header_flags |= (pic->picture_parameter.mode_control_fields.skip_mode_present
                                 << RDECODE_FRAME_HDR_INFO_AV1_SKIP_MODE_FLAG_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_SKIP_MODE_FLAG_MASK;

   result.frame_header_flags |= (((pic->picture_parameter.qmatrix_fields.qm_y == 0xf) ? 0 : 1)
                                 << RDECODE_FRAME_HDR_INFO_AV1_USING_QMATRIX_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_USING_QMATRIX_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seq_info_fields.enable_filter_intra
                                 << RDECODE_FRAME_HDR_INFO_AV1_ENABLE_FILTER_INTRA_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ENABLE_FILTER_INTRA_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seq_info_fields.enable_intra_edge_filter
                                 << RDECODE_FRAME_HDR_INFO_AV1_ENABLE_INTRA_EDGE_FILTER_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ENABLE_INTRA_EDGE_FILTER_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seq_info_fields.enable_interintra_compound
                                 << RDECODE_FRAME_HDR_INFO_AV1_ENABLE_INTERINTRA_COMPOUND_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ENABLE_INTERINTRA_COMPOUND_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seq_info_fields.enable_masked_compound
                                 << RDECODE_FRAME_HDR_INFO_AV1_ENABLE_MASKED_COMPOUND_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ENABLE_MASKED_COMPOUND_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_info_fields.allow_warped_motion
                                 << RDECODE_FRAME_HDR_INFO_AV1_ALLOW_WARPED_MOTION_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ALLOW_WARPED_MOTION_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seq_info_fields.enable_dual_filter
                                 << RDECODE_FRAME_HDR_INFO_AV1_ENABLE_DUAL_FILTER_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ENABLE_DUAL_FILTER_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seq_info_fields.enable_order_hint
                                 << RDECODE_FRAME_HDR_INFO_AV1_ENABLE_ORDER_HINT_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ENABLE_ORDER_HINT_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seq_info_fields.enable_jnt_comp
                                 << RDECODE_FRAME_HDR_INFO_AV1_ENABLE_JNT_COMP_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ENABLE_JNT_COMP_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_info_fields.use_ref_frame_mvs
                                 << RDECODE_FRAME_HDR_INFO_AV1_ALLOW_REF_FRAME_MVS_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ALLOW_REF_FRAME_MVS_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_info_fields.allow_screen_content_tools
                                 << RDECODE_FRAME_HDR_INFO_AV1_ALLOW_SCREEN_CONTENT_TOOLS_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_ALLOW_SCREEN_CONTENT_TOOLS_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_info_fields.force_integer_mv
                                 << RDECODE_FRAME_HDR_INFO_AV1_CUR_FRAME_FORCE_INTEGER_MV_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_CUR_FRAME_FORCE_INTEGER_MV_MASK;

   result.frame_header_flags |= (pic->picture_parameter.loop_filter_info_fields.mode_ref_delta_enabled
                                 << RDECODE_FRAME_HDR_INFO_AV1_MODE_REF_DELTA_ENABLED_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_MODE_REF_DELTA_ENABLED_MASK;

   result.frame_header_flags |= (pic->picture_parameter.loop_filter_info_fields.mode_ref_delta_update
                                 << RDECODE_FRAME_HDR_INFO_AV1_MODE_REF_DELTA_UPDATE_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_MODE_REF_DELTA_UPDATE_MASK;

   result.frame_header_flags |= (pic->picture_parameter.mode_control_fields.delta_q_present_flag
                                 << RDECODE_FRAME_HDR_INFO_AV1_DELTA_Q_PRESENT_FLAG_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_DELTA_Q_PRESENT_FLAG_MASK;

   result.frame_header_flags |= (pic->picture_parameter.mode_control_fields.delta_lf_present_flag
                                 << RDECODE_FRAME_HDR_INFO_AV1_DELTA_LF_PRESENT_FLAG_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_DELTA_LF_PRESENT_FLAG_MASK;

   result.frame_header_flags |= (pic->picture_parameter.mode_control_fields.reduced_tx_set_used
                                 << RDECODE_FRAME_HDR_INFO_AV1_REDUCED_TX_SET_USED_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_REDUCED_TX_SET_USED_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seg_info.segment_info_fields.enabled
                                 << RDECODE_FRAME_HDR_INFO_AV1_SEGMENTATION_ENABLED_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_SEGMENTATION_ENABLED_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seg_info.segment_info_fields.update_map
                                 << RDECODE_FRAME_HDR_INFO_AV1_SEGMENTATION_UPDATE_MAP_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_SEGMENTATION_UPDATE_MAP_MASK;

   result.frame_header_flags |= (pic->picture_parameter.seg_info.segment_info_fields.temporal_update
                                 << RDECODE_FRAME_HDR_INFO_AV1_SEGMENTATION_TEMPORAL_UPDATE_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_SEGMENTATION_TEMPORAL_UPDATE_MASK;

   result.frame_header_flags |= (pic->picture_parameter.mode_control_fields.delta_lf_multi
                                 << RDECODE_FRAME_HDR_INFO_AV1_DELTA_LF_MULTI_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_DELTA_LF_MULTI_MASK;

   result.frame_header_flags |= (pic->picture_parameter.pic_info_fields.is_motion_mode_switchable
                                 << RDECODE_FRAME_HDR_INFO_AV1_SWITCHABLE_SKIP_MODE_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_SWITCHABLE_SKIP_MODE_MASK;

   result.frame_header_flags |= ((!pic->picture_parameter.refresh_frame_flags)
                                 << RDECODE_FRAME_HDR_INFO_AV1_SKIP_REFERENCE_UPDATE_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_SKIP_REFERENCE_UPDATE_MASK;

   result.frame_header_flags |= ((!pic->picture_parameter.seq_info_fields.ref_frame_mvs)
                                 << RDECODE_FRAME_HDR_INFO_AV1_DISABLE_REF_FRAME_MVS_SHIFT) &
                                 RDECODE_FRAME_HDR_INFO_AV1_DISABLE_REF_FRAME_MVS_MASK;

   result.current_frame_id = pic->picture_parameter.current_frame_id;
   result.frame_offset = pic->picture_parameter.order_hint;

   result.profile = pic->picture_parameter.profile;
   result.is_annexb = 0;
   result.frame_type = pic->picture_parameter.pic_info_fields.frame_type;
   result.primary_ref_frame = pic->picture_parameter.primary_ref_frame;
   result.sb_size = pic->picture_parameter.seq_info_fields.use_128x128_superblock;
   result.interp_filter = pic->picture_parameter.interp_filter;
   for (i = 0; i < 2; ++i)
      result.filter_level[i] = pic->picture_parameter.filter_level[i];
   result.filter_level_u = pic->picture_parameter.filter_level_u;
   result.filter_level_v = pic->picture_parameter.filter_level_v;
   result.sharpness_level = pic->picture_parameter.loop_filter_info_fields.sharpness_level;
   for (i = 0; i < 8; ++i)
      result.ref_deltas[i] = pic->picture_parameter.ref_deltas[i];
   for (i = 0; i < 2; ++i)
      result.mode_deltas[i] = pic->picture_parameter.mode_deltas[i];
   result.base_qindex = pic->picture_parameter.base_qindex;
   result.y_dc_delta_q = pic->picture_parameter.y_dc_delta_q;
   result.u_dc_delta_q = pic->picture_parameter.u_dc_delta_q;
   result.v_dc_delta_q = pic->picture_parameter.v_dc_delta_q;
   result.u_ac_delta_q = pic->picture_parameter.u_ac_delta_q;
   result.v_ac_delta_q = pic->picture_parameter.v_ac_delta_q;
   result.qm_y = pic->picture_parameter.qmatrix_fields.qm_y | 0xf0;
   result.qm_u = pic->picture_parameter.qmatrix_fields.qm_u | 0xf0;
   result.qm_v = pic->picture_parameter.qmatrix_fields.qm_v | 0xf0;
   result.delta_q_res = 1 << pic->picture_parameter.mode_control_fields.log2_delta_q_res;
   result.delta_lf_res = 1 << pic->picture_parameter.mode_control_fields.log2_delta_lf_res;

   result.tile_cols = pic->picture_parameter.tile_cols;
   result.tile_rows = pic->picture_parameter.tile_rows;
   result.tx_mode = pic->picture_parameter.mode_control_fields.tx_mode;
   result.reference_mode = (pic->picture_parameter.mode_control_fields.reference_select == 1) ? 2 : 0;
   result.chroma_format = pic->picture_parameter.seq_info_fields.mono_chrome ? 0 : 1;
   result.tile_size_bytes = 0xff;
   result.context_update_tile_id = pic->picture_parameter.context_update_tile_id;
   for (i = 0; i < 65; ++i) {
      result.tile_col_start_sb[i] = pic->picture_parameter.tile_col_start_sb[i];
      result.tile_row_start_sb[i] = pic->picture_parameter.tile_row_start_sb[i];
   }
   result.max_width = pic->picture_parameter.max_width;
   result.max_height = pic->picture_parameter.max_height;
   if (pic->picture_parameter.pic_info_fields.use_superres) {
      result.width = (pic->picture_parameter.frame_width * 8 + pic->picture_parameter.superres_scale_denominator / 2) /
         pic->picture_parameter.superres_scale_denominator;
      result.superres_scale_denominator = pic->picture_parameter.superres_scale_denominator;
   } else {
      result.width = pic->picture_parameter.frame_width;
      result.superres_scale_denominator = pic->picture_parameter.superres_scale_denominator;
   }
   result.height = pic->picture_parameter.frame_height;
   result.superres_upscaled_width = pic->picture_parameter.frame_width;
   result.order_hint_bits = pic->picture_parameter.order_hint_bits_minus_1 + 1;

   result.curr_pic_idx = 0x7F;
   memset(result.ref_frame_map, 0x7F, sizeof(result.ref_frame_map));

   for (i = 0; i < NUM_AV1_REFS + 1; i++) {
      if (dec->render_pic_list[i]) {
         bool found = false;
         for (j = 0; j < NUM_AV1_REFS; j++) {
            if (dec->render_pic_list[i] == pic->ref[j]) {
               result.ref_frame_map[j] = i;
               dec->ref_codec.bufs[num_refs].buf = pic->ref[j];
               dec->ref_codec.bufs[num_refs++].index = i;
               valid_ref = j;
               found = true;
            }
         }
         if (!found)
            dec->render_pic_list[i] = NULL;
      }
      if (result.curr_pic_idx == 0x7F && !dec->render_pic_list[i]) {
         dec->render_pic_list[i] = target;
         result.curr_pic_idx = i;
      }
   }

   for (i = 0; i < NUM_AV1_REFS; i++) {
      if (pic->ref[i] && result.ref_frame_map[i] == 0x7F) {
         result.ref_frame_map[i] = valid_ref == UINT32_MAX ? 0 : result.ref_frame_map[valid_ref];
         num_refs++;
      }
   }

   for (i = 0; i < NUM_AV1_REFS_PER_FRAME; ++i)
       result.frame_refs[i] = result.ref_frame_map[pic->picture_parameter.ref_frame_idx[i]];

   result.bit_depth_luma_minus8 = result.bit_depth_chroma_minus8 = pic->picture_parameter.bit_depth_idx << 1;

   for (i = 0; i < 8; ++i) {
      for (j = 0; j < 8; ++j)
         result.feature_data[i][j] = pic->picture_parameter.seg_info.feature_data[i][j];
      result.feature_mask[i] = pic->picture_parameter.seg_info.feature_mask[i];
   }
   memcpy(dec->probs, &pic->picture_parameter.seg_info.feature_data, 128);
   memcpy((dec->probs + 128), &pic->picture_parameter.seg_info.feature_mask, 8);

   result.cdef_damping = pic->picture_parameter.cdef_damping_minus_3 + 3;
   result.cdef_bits = pic->picture_parameter.cdef_bits;
   for (i = 0; i < 8; ++i) {
      result.cdef_strengths[i] = pic->picture_parameter.cdef_y_strengths[i];
      result.cdef_uv_strengths[i] = pic->picture_parameter.cdef_uv_strengths[i];
   }
   result.frame_restoration_type[0] = pic->picture_parameter.loop_restoration_fields.yframe_restoration_type;
   result.frame_restoration_type[1] = pic->picture_parameter.loop_restoration_fields.cbframe_restoration_type;
   result.frame_restoration_type[2] = pic->picture_parameter.loop_restoration_fields.crframe_restoration_type;
   for (i = 0; i < 3; ++i) {
      int log2_num = 0;
      int unit_size = pic->picture_parameter.lr_unit_size[i];
      if (unit_size) {
         while (unit_size >>= 1)
            log2_num++;
         result.log2_restoration_unit_size_minus5[i] = log2_num - 5;
      } else {
         result.log2_restoration_unit_size_minus5[i] = 0;
      }
   }

   if (pic->picture_parameter.bit_depth_idx) {
      if (target->buffer_format == PIPE_FORMAT_P010 ||
          target->buffer_format == PIPE_FORMAT_P012 ||
          target->buffer_format == PIPE_FORMAT_P016) {
         result.p010_mode = 1;
         result.msb_mode = 1;
      } else {
         result.luma_10to8 = 1;
         result.chroma_10to8 = 1;
      }
   }

   result.preskip_segid = 0;
   result.last_active_segid = 0;
   for (i = 0; i < 8; i++) {
      for (j = 0; j < 8; j++) {
         if (pic->picture_parameter.seg_info.feature_mask[i] & (1 << j)) {
            result.last_active_segid = i;
            if (j >= 5)
               result.preskip_segid = 1;
         }
      }
   }

   result.seg_lossless_flag = 0;
   for (i = 0; i < 8; ++i) {
      int av1_get_qindex, qindex;
      int segfeature_active = pic->picture_parameter.seg_info.feature_mask[i] & (1 << 0);
      if (segfeature_active) {
         int seg_qindex = pic->picture_parameter.base_qindex +
                          pic->picture_parameter.seg_info.feature_data[i][0];
         av1_get_qindex = seg_qindex < 0 ? 0 : (seg_qindex > 255 ? 255 : seg_qindex);
      } else {
         av1_get_qindex = pic->picture_parameter.base_qindex;
      }
      qindex = pic->picture_parameter.seg_info.segment_info_fields.enabled ?
               av1_get_qindex :
               pic->picture_parameter.base_qindex;
      result.seg_lossless_flag |= (((qindex == 0) && result.y_dc_delta_q == 0 &&
                                    result.u_dc_delta_q == 0 && result.v_dc_delta_q == 0 &&
                                    result.u_ac_delta_q == 0 && result.v_ac_delta_q == 0) << i);
   }

   rvcn_dec_film_grain_params_t* fg_params = &result.film_grain;
   fg_params->apply_grain = pic->picture_parameter.film_grain_info.film_grain_info_fields.apply_grain;
   if (fg_params->apply_grain) {
      rvcn_dec_av1_fg_init_buf_t *fg_buf = (rvcn_dec_av1_fg_init_buf_t *)(dec->probs + 256);

      fg_params->random_seed = pic->picture_parameter.film_grain_info.grain_seed;
      fg_params->grain_scale_shift =
         pic->picture_parameter.film_grain_info.film_grain_info_fields.grain_scale_shift;
      fg_params->scaling_shift =
         pic->picture_parameter.film_grain_info.film_grain_info_fields.grain_scaling_minus_8 + 8;
      fg_params->chroma_scaling_from_luma =
         pic->picture_parameter.film_grain_info.film_grain_info_fields.chroma_scaling_from_luma;
      fg_params->num_y_points = pic->picture_parameter.film_grain_info.num_y_points;
      fg_params->num_cb_points = pic->picture_parameter.film_grain_info.num_cb_points;
      fg_params->num_cr_points = pic->picture_parameter.film_grain_info.num_cr_points;
      fg_params->cb_mult = pic->picture_parameter.film_grain_info.cb_mult;
      fg_params->cb_luma_mult = pic->picture_parameter.film_grain_info.cb_luma_mult;
      fg_params->cb_offset = pic->picture_parameter.film_grain_info.cb_offset;
      fg_params->cr_mult = pic->picture_parameter.film_grain_info.cr_mult;
      fg_params->cr_luma_mult = pic->picture_parameter.film_grain_info.cr_luma_mult;
      fg_params->cr_offset = pic->picture_parameter.film_grain_info.cr_offset;
      fg_params->bit_depth_minus_8 = pic->picture_parameter.bit_depth_idx << 1;

      for (i = 0; i < fg_params->num_y_points; ++i) {
         fg_params->scaling_points_y[i][0] = pic->picture_parameter.film_grain_info.point_y_value[i];
         fg_params->scaling_points_y[i][1] = pic->picture_parameter.film_grain_info.point_y_scaling[i];
      }
      for (i = 0; i < fg_params->num_cb_points; ++i) {
         fg_params->scaling_points_cb[i][0] = pic->picture_parameter.film_grain_info.point_cb_value[i];
         fg_params->scaling_points_cb[i][1] = pic->picture_parameter.film_grain_info.point_cb_scaling[i];
      }
      for (i = 0; i < fg_params->num_cr_points; ++i) {
         fg_params->scaling_points_cr[i][0] = pic->picture_parameter.film_grain_info.point_cr_value[i];
         fg_params->scaling_points_cr[i][1] = pic->picture_parameter.film_grain_info.point_cr_scaling[i];
      }

      fg_params->ar_coeff_lag = pic->picture_parameter.film_grain_info.film_grain_info_fields.ar_coeff_lag;
      fg_params->ar_coeff_shift =
         pic->picture_parameter.film_grain_info.film_grain_info_fields.ar_coeff_shift_minus_6 + 6;

      for (i = 0; i < 24; ++i)
         fg_params->ar_coeffs_y[i] = pic->picture_parameter.film_grain_info.ar_coeffs_y[i];

      for (i = 0; i < 25; ++i) {
         fg_params->ar_coeffs_cb[i] = pic->picture_parameter.film_grain_info.ar_coeffs_cb[i];
         fg_params->ar_coeffs_cr[i] = pic->picture_parameter.film_grain_info.ar_coeffs_cr[i];
      }

      fg_params->overlap_flag = pic->picture_parameter.film_grain_info.film_grain_info_fields.overlap_flag;
      fg_params->clip_to_restricted_range =
         pic->picture_parameter.film_grain_info.film_grain_info_fields.clip_to_restricted_range;

      ac_vcn_av1_init_film_grain_buffer(dec->av1_version, fg_params, fg_buf);
   }

   result.uncompressed_header_size = 0;
   for (i = 0; i < 7; ++i) {
      result.global_motion[i + 1].wmtype = (rvcn_dec_transformation_type_e)pic->picture_parameter.wm[i].wmtype;
      for (j = 0; j < 6; ++j)
         result.global_motion[i + 1].wmmat[j] = pic->picture_parameter.wm[i].wmmat[j];
   }
   for (i = 0; i < tile_count && i < 256; ++i) {
      result.tile_info[i].offset = pic->slice_parameter.slice_data_offset[i];
      result.tile_info[i].size = pic->slice_parameter.slice_data_size[i];
   }

   if (dec->dpb_type >= DPB_DYNAMIC_TIER_2) {
      dec->ref_codec.bts = pic->picture_parameter.bit_depth_idx;
      dec->ref_codec.index = result.curr_pic_idx;
      dec->ref_codec.ref_size = 8;
      dec->ref_codec.num_refs = num_refs;
      memset(dec->ref_codec.ref_list, 0x7f, sizeof(dec->ref_codec.ref_list));
      memcpy(dec->ref_codec.ref_list, result.ref_frame_map, sizeof(result.ref_frame_map));

      /* Film grain is applied to decode target only. */
      if (dec->dpb_type == DPB_DYNAMIC_TIER_3 && pic->film_grain_target) {
         dec->ref_codec.bufs[dec->ref_codec.num_refs].buf = target;
         dec->ref_codec.bufs[dec->ref_codec.num_refs++].index = result.curr_pic_idx;
      }
   }

   return result;
}

static unsigned calc_ctx_size_h265_main(struct radeon_decoder *dec)
{
   unsigned width = align(dec->base.width, VL_MACROBLOCK_WIDTH);
   unsigned height = align(dec->base.height, VL_MACROBLOCK_HEIGHT);

   unsigned max_references = dec->base.max_references + 1;

   if (dec->base.width * dec->base.height >= 4096 * 2000)
      max_references = MAX2(max_references, 8);
   else
      max_references = MAX2(max_references, 17);

   width = align(width, 16);
   height = align(height, 16);
   return ((width + 255) / 16) * ((height + 255) / 16) * 16 * max_references + 52 * 1024;
}

static unsigned calc_ctx_size_h265_main10(struct radeon_decoder *dec,
                                          struct pipe_h265_picture_desc *pic)
{
   unsigned log2_ctb_size, width_in_ctb, height_in_ctb, num_16x16_block_per_ctb;
   unsigned context_buffer_size_per_ctb_row, cm_buffer_size, max_mb_address, db_left_tile_pxl_size;
   unsigned db_left_tile_ctx_size = 4096 / 16 * (32 + 16 * 4);

   unsigned width = align(dec->base.width, VL_MACROBLOCK_WIDTH);
   unsigned height = align(dec->base.height, VL_MACROBLOCK_HEIGHT);
   unsigned coeff_10bit =
      (pic->pps->sps->bit_depth_luma_minus8 || pic->pps->sps->bit_depth_chroma_minus8) ? 2 : 1;

   unsigned max_references = dec->base.max_references + 1;

   if (dec->base.width * dec->base.height >= 4096 * 2000)
      max_references = MAX2(max_references, 8);
   else
      max_references = MAX2(max_references, 17);

   log2_ctb_size = pic->pps->sps->log2_min_luma_coding_block_size_minus3 + 3 +
                   pic->pps->sps->log2_diff_max_min_luma_coding_block_size;

   width_in_ctb = (width + ((1 << log2_ctb_size) - 1)) >> log2_ctb_size;
   height_in_ctb = (height + ((1 << log2_ctb_size) - 1)) >> log2_ctb_size;

   num_16x16_block_per_ctb = ((1 << log2_ctb_size) >> 4) * ((1 << log2_ctb_size) >> 4);
   context_buffer_size_per_ctb_row = align(width_in_ctb * num_16x16_block_per_ctb * 16, 256);
   max_mb_address = (unsigned)ceil(height * 8 / 2048.0);

   cm_buffer_size = max_references * context_buffer_size_per_ctb_row * height_in_ctb;
   db_left_tile_pxl_size = coeff_10bit * (max_mb_address * 2 * 2048 + 1024);

   return cm_buffer_size + db_left_tile_ctx_size + db_left_tile_pxl_size;
}

static rvcn_dec_message_vc1_t get_vc1_msg(struct pipe_vc1_picture_desc *pic)
{
   rvcn_dec_message_vc1_t result;

   memset(&result, 0, sizeof(result));
   switch (pic->base.profile) {
   case PIPE_VIDEO_PROFILE_VC1_SIMPLE:
      result.profile = RDECODE_VC1_PROFILE_SIMPLE;
      result.level = 1;
      break;

   case PIPE_VIDEO_PROFILE_VC1_MAIN:
      result.profile = RDECODE_VC1_PROFILE_MAIN;
      result.level = 2;
      break;

   case PIPE_VIDEO_PROFILE_VC1_ADVANCED:
      result.profile = RDECODE_VC1_PROFILE_ADVANCED;
      result.level = 4;
      break;

   default:
      assert(0);
   }

   result.sps_info_flags |= pic->postprocflag << 7;
   result.sps_info_flags |= pic->pulldown << 6;
   result.sps_info_flags |= pic->interlace << 5;
   result.sps_info_flags |= pic->tfcntrflag << 4;
   result.sps_info_flags |= pic->finterpflag << 3;
   result.sps_info_flags |= pic->psf << 1;

   result.pps_info_flags |= pic->range_mapy_flag << 31;
   result.pps_info_flags |= pic->range_mapy << 28;
   result.pps_info_flags |= pic->range_mapuv_flag << 27;
   result.pps_info_flags |= pic->range_mapuv << 24;
   result.pps_info_flags |= pic->multires << 21;
   result.pps_info_flags |= pic->maxbframes << 16;
   result.pps_info_flags |= pic->overlap << 11;
   result.pps_info_flags |= pic->quantizer << 9;
   result.pps_info_flags |= pic->panscan_flag << 7;
   result.pps_info_flags |= pic->refdist_flag << 6;
   result.pps_info_flags |= pic->vstransform << 0;

   if (pic->base.profile != PIPE_VIDEO_PROFILE_VC1_SIMPLE) {
      result.pps_info_flags |= pic->syncmarker << 20;
      result.pps_info_flags |= pic->rangered << 19;
      result.pps_info_flags |= pic->loopfilter << 5;
      result.pps_info_flags |= pic->fastuvmc << 4;
      result.pps_info_flags |= pic->extended_mv << 3;
      result.pps_info_flags |= pic->extended_dmv << 8;
      result.pps_info_flags |= pic->dquant << 1;
   }

   result.chroma_format = 1;

   return result;
}

static uint32_t get_ref_pic_idx(struct radeon_decoder *dec, struct pipe_video_buffer *ref)
{
   uint32_t min = MAX2(dec->frame_number, NUM_MPEG2_REFS) - NUM_MPEG2_REFS;
   uint32_t max = MAX2(dec->frame_number, 1) - 1;
   uintptr_t frame;

   /* seems to be the most sane fallback */
   if (!ref)
      return max;

   /* get the frame number from the associated data */
   frame = (uintptr_t)vl_video_buffer_get_associated_data(ref, &dec->base);

   /* limit the frame number to a valid range */
   return MAX2(MIN2(frame, max), min);
}

static rvcn_dec_message_mpeg2_vld_t get_mpeg2_msg(struct radeon_decoder *dec,
                                                  struct pipe_video_buffer *target,
                                                  struct pipe_mpeg12_picture_desc *pic)
{
   const int *zscan = pic->alternate_scan ? vl_zscan_alternate : vl_zscan_normal;
   rvcn_dec_message_mpeg2_vld_t result;
   unsigned i;

   memset(&result, 0, sizeof(result));
   result.decoded_pic_idx = dec->frame_number;

   result.forward_ref_pic_idx = get_ref_pic_idx(dec, pic->ref[0]);
   result.backward_ref_pic_idx = get_ref_pic_idx(dec, pic->ref[1]);

   vl_video_buffer_set_associated_data(target, &dec->base, (void *)(uintptr_t)dec->frame_number,
                                       &radeon_dec_destroy_associated_data);

   if (pic->intra_matrix) {
      result.load_intra_quantiser_matrix = 1;
      for (i = 0; i < 64; ++i) {
         result.intra_quantiser_matrix[i] = pic->intra_matrix[zscan[i]];
      }
   }
   if (pic->non_intra_matrix) {
      result.load_nonintra_quantiser_matrix = 1;
      for (i = 0; i < 64; ++i) {
         result.nonintra_quantiser_matrix[i] = pic->non_intra_matrix[zscan[i]];
      }
   }

   result.profile_and_level_indication = 0;
   result.chroma_format = 0x1;

   result.picture_coding_type = pic->picture_coding_type;
   result.f_code[0][0] = pic->f_code[0][0] + 1;
   result.f_code[0][1] = pic->f_code[0][1] + 1;
   result.f_code[1][0] = pic->f_code[1][0] + 1;
   result.f_code[1][1] = pic->f_code[1][1] + 1;
   result.intra_dc_precision = pic->intra_dc_precision;
   result.pic_structure = pic->picture_structure;
   result.top_field_first = pic->top_field_first;
   result.frame_pred_frame_dct = pic->frame_pred_frame_dct;
   result.concealment_motion_vectors = pic->concealment_motion_vectors;
   result.q_scale_type = pic->q_scale_type;
   result.intra_vlc_format = pic->intra_vlc_format;
   result.alternate_scan = pic->alternate_scan;

   return result;
}

static rvcn_dec_message_mpeg4_asp_vld_t get_mpeg4_msg(struct radeon_decoder *dec,
                                                      struct pipe_video_buffer *target,
                                                      struct pipe_mpeg4_picture_desc *pic)
{
   rvcn_dec_message_mpeg4_asp_vld_t result;
   unsigned i;

   memset(&result, 0, sizeof(result));
   result.decoded_pic_idx = dec->frame_number;

   result.forward_ref_pic_idx = get_ref_pic_idx(dec, pic->ref[0]);
   result.backward_ref_pic_idx = get_ref_pic_idx(dec, pic->ref[1]);

   vl_video_buffer_set_associated_data(target, &dec->base, (void *)(uintptr_t)dec->frame_number,
                                       &radeon_dec_destroy_associated_data);

   result.variant_type = 0;
   result.profile_and_level_indication = 0xF0;

   result.video_object_layer_verid = 0x5;
   result.video_object_layer_shape = 0x0;

   result.video_object_layer_width = dec->base.width;
   result.video_object_layer_height = dec->base.height;

   result.vop_time_increment_resolution = pic->vop_time_increment_resolution;

   result.short_video_header = pic->short_video_header;
   result.interlaced = pic->interlaced;
   result.load_intra_quant_mat = 1;
   result.load_nonintra_quant_mat = 1;
   result.quarter_sample = pic->quarter_sample;
   result.complexity_estimation_disable = 1;
   result.resync_marker_disable = pic->resync_marker_disable;
   result.newpred_enable = 0;
   result.reduced_resolution_vop_enable = 0;

   result.quant_type = pic->quant_type;

   for (i = 0; i < 64; ++i) {
      result.intra_quant_mat[i] = pic->intra_matrix[vl_zscan_normal[i]];
      result.nonintra_quant_mat[i] = pic->non_intra_matrix[vl_zscan_normal[i]];
   }

   return result;
}

static void rvcn_dec_message_create(struct radeon_decoder *dec)
{
   rvcn_dec_message_header_t *header = dec->msg;
   rvcn_dec_message_create_t *create = dec->msg + sizeof(rvcn_dec_message_header_t);
   unsigned sizes = sizeof(rvcn_dec_message_header_t) + sizeof(rvcn_dec_message_create_t);

   memset(dec->msg, 0, sizes);
   header->header_size = sizeof(rvcn_dec_message_header_t);
   header->total_size = sizes;
   header->num_buffers = 1;
   header->msg_type = RDECODE_MSG_CREATE;
   header->stream_handle = dec->stream_handle;
   header->status_report_feedback_number = 0;

   header->index[0].message_id = RDECODE_MESSAGE_CREATE;
   header->index[0].offset = sizeof(rvcn_dec_message_header_t);
   header->index[0].size = sizeof(rvcn_dec_message_create_t);
   header->index[0].filled = 0;

   create->stream_type = dec->stream_type;
   create->session_flags = 0;
   create->width_in_samples = dec->base.width;
   create->height_in_samples = dec->base.height;
}

static unsigned rvcn_dec_dynamic_dpb_t2_message(struct radeon_decoder *dec, rvcn_dec_message_decode_t *decode,
      rvcn_dec_message_dynamic_dpb_t2_t *dynamic_dpb_t2, bool encrypted)
{
   struct rvcn_dec_dynamic_dpb_t2 *dpb = NULL;
   struct si_resource *res;
   unsigned width, height;
   uint64_t addr;
   int i;

   width = align(decode->width_in_samples, dec->db_alignment);
   height = align(decode->height_in_samples, dec->db_alignment);

   list_for_each_entry_safe(struct rvcn_dec_dynamic_dpb_t2, d, &dec->dpb_ref_list, list) {
      bool found = false;

      res = (struct si_resource *)d->buf;
      for (i = 0; i < dec->ref_codec.ref_size; ++i) {
         if (((dec->ref_codec.ref_list[i] & 0x7f) != 0x7f) && (d->index == (dec->ref_codec.ref_list[i] & 0x7f))) {
            addr = dec->ws->buffer_get_virtual_address(res->buf);
            dec->ws->cs_add_buffer(&dec->cs, res->buf, RADEON_USAGE_READWRITE | RADEON_USAGE_SYNCHRONIZED, RADEON_DOMAIN_VRAM);
            dynamic_dpb_t2->dpbAddrLo[i] = addr;
            dynamic_dpb_t2->dpbAddrHi[i] = addr >> 32;
            ++dynamic_dpb_t2->dpbArraySize;
            found = true;
         }
      }
      if (!found) {
         if (d->buf->width0 != width || d->buf->height0 != height) {
            list_del(&d->list);
            list_addtail(&d->list, &dec->dpb_unref_list);
         } else {
            d->index = 0x7f;
         }
      }
   }

   list_for_each_entry_safe(struct rvcn_dec_dynamic_dpb_t2, d, &dec->dpb_ref_list, list) {
      if (d->buf->width0 == width && d->buf->height0 == height && d->index == dec->ref_codec.index) {
         dpb = d;
         break;
      }
   }

   if (!dpb) {
      list_for_each_entry_safe(struct rvcn_dec_dynamic_dpb_t2, d, &dec->dpb_ref_list, list) {
         if (d->index == 0x7f) {
            d->index = dec->ref_codec.index;
            dpb = d;
            break;
         }
      }
   }

   list_for_each_entry_safe(struct rvcn_dec_dynamic_dpb_t2, d, &dec->dpb_unref_list, list) {
      list_del(&d->list);
      pipe_resource_reference(&d->buf, NULL);
      FREE(d);
   }

   if (!dpb) {
      dpb = CALLOC_STRUCT(rvcn_dec_dynamic_dpb_t2);
      if (!dpb)
         return 1;
      dpb->index = dec->ref_codec.index;

      struct pipe_resource templat;
      memset(&templat, 0, sizeof(templat));
      templat.format = get_buffer_format(dec);
      templat.target = PIPE_TEXTURE_2D;
      templat.width0 = width;
      templat.height0 = height;
      templat.depth0 = 1;
      templat.array_size = 1;
      templat.usage = PIPE_USAGE_DEFAULT;
      templat.bind = PIPE_BIND_VIDEO_DECODE_DPB;
      if (encrypted)
         templat.bind |= PIPE_BIND_PROTECTED;
      dpb->buf = dec->screen->resource_create(dec->screen, &templat);
      if (!dpb->buf) {
         RADEON_DEC_ERR("Can't allocate dpb buffer.\n");
         FREE(dpb);
         return 1;
      }
      list_addtail(&dpb->list, &dec->dpb_ref_list);
   }

   if (dynamic_dpb_t2->dpbArraySize < dec->ref_codec.num_refs) {
      struct rvcn_dec_dynamic_dpb_t2 *d =
         list_first_entry(&dec->dpb_ref_list, struct rvcn_dec_dynamic_dpb_t2, list);

      res = (struct si_resource *)d->buf;
      addr = dec->ws->buffer_get_virtual_address(res->buf);
      for (i = 0; i < dec->ref_codec.num_refs; ++i) {
         if (dynamic_dpb_t2->dpbAddrLo[i] || dynamic_dpb_t2->dpbAddrHi[i])
            continue;
         dynamic_dpb_t2->dpbAddrLo[i] = addr;
         dynamic_dpb_t2->dpbAddrHi[i] = addr >> 32;
         ++dynamic_dpb_t2->dpbArraySize;
      }
      assert(dynamic_dpb_t2->dpbArraySize == dec->ref_codec.num_refs);
   }

   struct si_texture *dpb_luma, *dpb_chroma;

   dpb_luma   = (struct si_texture *)dpb->buf;
   dpb_chroma = (struct si_texture *)dpb->buf->next;

   decode->db_swizzle_mode = dpb_luma->surface.u.gfx9.swizzle_mode;

   dec->ws->cs_add_buffer(&dec->cs, (dpb_luma->buffer).buf, RADEON_USAGE_READWRITE | RADEON_USAGE_SYNCHRONIZED, RADEON_DOMAIN_VRAM);
   addr = dec->ws->buffer_get_virtual_address((dpb_luma->buffer).buf);

   dynamic_dpb_t2->dpbLumaPitch = dpb_luma->surface.u.gfx9.surf_pitch;
   dynamic_dpb_t2->dpbLumaAlignedHeight = dpb_luma->surface.u.gfx9.surf_height;
   dynamic_dpb_t2->dpbLumaAlignedSize = dpb_luma->surface.u.gfx9.surf_slice_size;
   dynamic_dpb_t2->dpbChromaPitch = dpb_chroma->surface.u.gfx9.surf_pitch;
   dynamic_dpb_t2->dpbChromaAlignedHeight = dpb_chroma->surface.u.gfx9.surf_height;
   dynamic_dpb_t2->dpbChromaAlignedSize = dpb_chroma->surface.u.gfx9.surf_slice_size;

   dynamic_dpb_t2->dpbCurrLo = addr;
   dynamic_dpb_t2->dpbCurrHi = addr >> 32;
   decode->decode_flags = 1;
   dynamic_dpb_t2->dpbConfigFlags = 0;

   return 0;
}

static bool rvcn_dec_can_use_udt(struct pipe_video_buffer *target, struct pipe_picture_desc *picture)
{
   struct si_texture *luma = (struct si_texture *)((struct vl_video_buffer *)target)->resources[0];

   /* UDT requires tiling */
   if (luma->surface.is_linear)
      return false;

   /* UDT can't be used with 12bit AV1 */
   if (u_reduce_video_profile(picture->profile) == PIPE_VIDEO_FORMAT_AV1 &&
       ((struct pipe_av1_picture_desc *)picture)->picture_parameter.bit_depth_idx == 2)
      return false;

   return true;
}

static struct pb_buffer_lean *rvcn_dec_message_decode(struct radeon_decoder *dec,
                                                      struct pipe_video_buffer *target,
                                                      struct pipe_picture_desc *picture)
{
   DECRYPT_PARAMETERS *decrypt = (DECRYPT_PARAMETERS *)picture->decrypt_key;
   amd_secure_buffer_format *secure_buf = (amd_secure_buffer_format *)picture->decrypt_key;
   bool encrypted = picture->protected_playback;
   struct si_texture *luma;
   struct si_texture *chroma;
   struct pipe_video_buffer *out_surf = target;
   ASSERTED struct si_screen *sscreen = (struct si_screen *)dec->screen;
   rvcn_dec_message_header_t *header;
   rvcn_dec_message_index_t *index_codec;
   rvcn_dec_message_index_t *index_drm = NULL;
   rvcn_dec_message_index_t *index_drm_keyblob = NULL;
   rvcn_dec_message_index_t *index_dynamic_dpb = NULL;
   rvcn_dec_message_decode_t *decode;
   unsigned sizes = 0, offset_decode, offset_codec;
   unsigned offset_drm = 0, offset_drm_keyblob = 0, offset_dynamic_dpb = 0;
   void *codec;
   rvcn_dec_message_drm_t *drm = NULL;
   rvcn_dec_message_drm_keyblob_t *drm_keyblob = NULL;
   rvcn_dec_message_dynamic_dpb_t *dynamic_dpb = NULL;
   rvcn_dec_message_dynamic_dpb_t2_t *dynamic_dpb_t2 = NULL;
   bool dpb_resize = false;

   if (dec->stream_type == RDECODE_CODEC_AV1)
      rvcn_dec_av1_film_grain_surface(&out_surf, (struct pipe_av1_picture_desc *)picture);

   luma   = (struct si_texture *)((struct vl_video_buffer *)out_surf)->resources[0];
   chroma = (struct si_texture *)((struct vl_video_buffer *)out_surf)->resources[1];

   if (dec->frame_number == 1 && dec->dpb_type == DPB_DYNAMIC_TIER_3 && !rvcn_dec_can_use_udt(out_surf, picture))
      dec->dpb_type = DPB_DYNAMIC_TIER_2;

   header = dec->msg;
   sizes += sizeof(rvcn_dec_message_header_t);

   index_codec = (void*)header + sizes;
   sizes += sizeof(rvcn_dec_message_index_t);

   if (encrypted) {
      index_drm = (void*)header + sizes;
      sizes += sizeof(rvcn_dec_message_index_t);
   }

   if (picture->cenc) {
      index_drm_keyblob = (void*)header + sizes;
      sizes += sizeof(rvcn_dec_message_index_t);
   }

   if (dec->dpb_type == DPB_DYNAMIC_TIER_1 || dec->dpb_type == DPB_DYNAMIC_TIER_2) {
      index_dynamic_dpb = (void*)header + sizes;
      sizes += sizeof(rvcn_dec_message_index_t);
   }

   offset_decode = sizes;
   decode = (void*)header + sizes;
   sizes += sizeof(rvcn_dec_message_decode_t);

   if (encrypted) {
      offset_drm = sizes;
      drm = (void*)header + sizes;
      sizes += sizeof(rvcn_dec_message_drm_t);
   }

   if (picture->cenc) {
      offset_drm_keyblob = sizes;
      drm_keyblob = (void*)header + sizes;
      sizes += sizeof(rvcn_dec_message_drm_keyblob_t);
   }

   if (dec->dpb_type == DPB_DYNAMIC_TIER_1 || dec->dpb_type == DPB_DYNAMIC_TIER_2) {
      offset_dynamic_dpb = sizes;
      if (dec->dpb_type == DPB_DYNAMIC_TIER_1) {
         dynamic_dpb = (void*)header + sizes;
         sizes += sizeof(rvcn_dec_message_dynamic_dpb_t);
      }
      else if (dec->dpb_type == DPB_DYNAMIC_TIER_2) {
         dynamic_dpb_t2 = (void*)header + sizes;
         sizes += sizeof(rvcn_dec_message_dynamic_dpb_t2_t);
      }
   }

   offset_codec = sizes;
   codec = (void*)header + sizes;

   memset(dec->msg, 0, sizes);
   header->header_size = sizeof(rvcn_dec_message_header_t);
   header->total_size = sizes;
   header->msg_type = RDECODE_MSG_DECODE;
   header->stream_handle = dec->stream_handle;
   header->status_report_feedback_number = dec->frame_number;

   header->index[0].message_id = RDECODE_MESSAGE_DECODE;
   header->index[0].offset = offset_decode;
   header->index[0].size = sizeof(rvcn_dec_message_decode_t);
   header->index[0].filled = 0;
   header->num_buffers = 1;

   index_codec->offset = offset_codec;
   index_codec->size = 0;
   index_codec->filled = 0;
   ++header->num_buffers;

   if (encrypted) {
      index_drm->message_id = RDECODE_MESSAGE_DRM;
      index_drm->offset = offset_drm;
      index_drm->size = sizeof(rvcn_dec_message_drm_t);
      index_drm->filled = 0;
      ++header->num_buffers;
   }

   if (picture->cenc) {
      index_drm_keyblob->message_id = RDECODE_MESSAGE_DRM_KEYBLOB;
      index_drm_keyblob->offset = offset_drm_keyblob;
      index_drm_keyblob->size = sizeof(rvcn_dec_message_drm_keyblob_t);
      index_drm_keyblob->filled = 0;
      ++header->num_buffers;
   }

   if (dec->dpb_type == DPB_DYNAMIC_TIER_1 || dec->dpb_type == DPB_DYNAMIC_TIER_2) {
      index_dynamic_dpb->message_id = RDECODE_MESSAGE_DYNAMIC_DPB;
      index_dynamic_dpb->offset = offset_dynamic_dpb;
      index_dynamic_dpb->filled = 0;
      ++header->num_buffers;
      if (dec->dpb_type == DPB_DYNAMIC_TIER_1)
         index_dynamic_dpb->size = sizeof(rvcn_dec_message_dynamic_dpb_t);
      else if (dec->dpb_type == DPB_DYNAMIC_TIER_2)
         index_dynamic_dpb->size = sizeof(rvcn_dec_message_dynamic_dpb_t2_t);
   }

   decode->stream_type = dec->stream_type;
   decode->decode_flags = 0;
   decode->width_in_samples = dec->base.width;
   decode->height_in_samples = dec->base.height;

   if (dec->dpb_type == DPB_DYNAMIC_TIER_3)
      decode->decode_flags = RDECODE_FLAGS_UNIFIED_DT_MASK;

   decode->bsd_size = align(dec->bs_size, 128);

   if (dec->dpb_type < DPB_DYNAMIC_TIER_2) {
      bool r;
      if (!dec->dpb.res && dec->dpb_size) {
         if (encrypted) {
            r = si_vid_create_tmz_buffer(dec->screen, &dec->dpb, dec->dpb_size, PIPE_USAGE_DEFAULT);
         } else {
            r = si_vid_create_buffer(dec->screen, &dec->dpb, dec->dpb_size, PIPE_USAGE_DEFAULT);
         }
         assert(encrypted == (bool)(dec->dpb.res->flags & RADEON_FLAG_ENCRYPTED));
         if (!r) {
            RADEON_DEC_ERR("Can't allocate dpb.\n");
            return NULL;
         }
      } else if (dec->dpb_type == DPB_DYNAMIC_TIER_1 && dec->dpb.res &&
                 (dec->max_width < dec->base.width || dec->max_height < dec->base.height)) {
         struct rvid_buf_offset_info buf_offset_info;

         buf_offset_info.num_units = (NUM_VP9_REFS + 1);
         buf_offset_info.old_offset = (align(dec->max_width, dec->db_alignment) *
            align(dec->max_height, dec->db_alignment) * 3 / 2);
         buf_offset_info.new_offset = (align(dec->base.width, dec->db_alignment) *
            align(dec->base.height, dec->db_alignment) * 3 / 2);

         dec->dpb_size = calc_dpb_size(dec);
         r = si_vid_resize_buffer(dec->base.context, &dec->dpb, dec->dpb_size, &buf_offset_info);
         if (!r) {
            RADEON_DEC_ERR("Can't resize dpb.\n");
            return NULL;
         }
         dec->max_width = dec->base.width;
         dec->max_height = dec->base.height;
         dpb_resize = true;
      }
   }

   if (!dec->ctx.res) {
      enum pipe_video_format fmt = u_reduce_video_profile(picture->profile);
      if (dec->stream_type == RDECODE_CODEC_H264_PERF) {
         unsigned ctx_size = calc_ctx_size_h264_perf(dec);
         bool r;
         if (encrypted && dec->tmz_ctx) {
            r = si_vid_create_tmz_buffer(dec->screen, &dec->ctx, ctx_size, PIPE_USAGE_DEFAULT);
         } else {
            r = si_vid_create_buffer(dec->screen, &dec->ctx, ctx_size, PIPE_USAGE_DEFAULT);
         }
         assert((encrypted && dec->tmz_ctx) == (bool)(dec->ctx.res->flags & RADEON_FLAG_ENCRYPTED));

         if (!r) {
            RADEON_DEC_ERR("Can't allocate context buffer.\n");
            return NULL;
         }
      } else if (fmt == PIPE_VIDEO_FORMAT_VP9) {
         unsigned ctx_size;
         uint8_t *ptr;
         bool r;

         /* default probability + probability data */
         ctx_size = 2304 * 5;

         if (((struct si_screen *)dec->screen)->info.vcn_ip_version >= VCN_2_0_0) {
            /* SRE collocated context data */
            ctx_size += 32 * 2 * 128 * 68;
            /* SMP collocated context data */
            ctx_size += 9 * 64 * 2 * 128 * 68;
            /* SDB left tile pixel */
            ctx_size += 8 * 2 * 2 * 8192;
         } else {
            ctx_size += 32 * 2 * 64 * 64;
            ctx_size += 9 * 64 * 2 * 64 * 64;
            ctx_size += 8 * 2 * 4096;
         }

         if (dec->base.profile == PIPE_VIDEO_PROFILE_VP9_PROFILE2)
            ctx_size += 8 * 2 * 4096;

         if (encrypted && dec->tmz_ctx) {
            r = si_vid_create_tmz_buffer(dec->screen, &dec->ctx, ctx_size, PIPE_USAGE_DEFAULT);
         } else {
            r = si_vid_create_buffer(dec->screen, &dec->ctx, ctx_size, PIPE_USAGE_DEFAULT);
         }
         if (!r) {
            RADEON_DEC_ERR("Can't allocate context buffer.\n");
            return NULL;
         }

         /* ctx needs probs table */
         ptr = dec->ws->buffer_map(dec->ws, dec->ctx.res->buf, NULL,
                                   PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);
         ac_vcn_vp9_fill_probs_table(ptr);
         dec->ws->buffer_unmap(dec->ws, dec->ctx.res->buf);
         dec->bs_ptr = NULL;
      } else if (fmt == PIPE_VIDEO_FORMAT_HEVC) {
         unsigned ctx_size;
         bool r;
         if (dec->base.profile == PIPE_VIDEO_PROFILE_HEVC_MAIN_10)
            ctx_size = calc_ctx_size_h265_main10(dec, (struct pipe_h265_picture_desc *)picture);
         else
            ctx_size = calc_ctx_size_h265_main(dec);

         if (encrypted && dec->tmz_ctx) {
            r = si_vid_create_tmz_buffer(dec->screen, &dec->ctx, ctx_size, PIPE_USAGE_DEFAULT);
         } else {
            r = si_vid_create_buffer(dec->screen, &dec->ctx, ctx_size, PIPE_USAGE_DEFAULT);
         }
         if (!r) {
            RADEON_DEC_ERR("Can't allocate context buffer.\n");
            return NULL;
         }
      }
   }
   if (encrypted != dec->ws->cs_is_secure(&dec->cs)) {
      dec->ws->cs_flush(&dec->cs, RADEON_FLUSH_TOGGLE_SECURE_SUBMISSION, NULL);
   }

   if (picture->cenc) {
      set_drm_keyblob(drm_keyblob, secure_buf);
   }

   decode->dpb_size = (dec->dpb_type < DPB_DYNAMIC_TIER_2) ? dec->dpb.res->buf->size : 0;

   /* When texture being created, the bo will be created with total size of planes,
    * and all planes point to the same buffer */
   assert(si_resource(((struct vl_video_buffer *)out_surf)->resources[0])->buf->size ==
      si_resource(((struct vl_video_buffer *)out_surf)->resources[1])->buf->size);

   decode->dt_size = si_resource(((struct vl_video_buffer *)out_surf)->resources[0])->buf->size;

   decode->sct_size = 0;
   decode->sc_coeff_size = 0;

   decode->sw_ctxt_size = RDECODE_SESSION_CONTEXT_SIZE;
   decode->db_pitch = align(dec->base.width, dec->db_alignment);

   if ((((struct si_screen*)dec->screen)->info.vcn_ip_version >= VCN_3_0_0) &&
       (dec->stream_type == RDECODE_CODEC_VP9 || dec->stream_type == RDECODE_CODEC_AV1 ||
        dec->base.profile == PIPE_VIDEO_PROFILE_HEVC_MAIN_10))
      decode->db_aligned_height = align(dec->base.height, 64);

   decode->db_surf_tile_config = 0;
   decode->db_array_mode = dec->addr_gfx_mode;

   decode->dt_pitch = luma->surface.u.gfx9.surf_pitch * luma->surface.blk_w;
   decode->dt_uv_pitch = chroma->surface.u.gfx9.surf_pitch * chroma->surface.blk_w;

   if (luma->surface.meta_offset) {
      RADEON_DEC_ERR("DCC surfaces not supported.\n");
      return NULL;
   }

   decode->dt_tiling_mode = 0;
   decode->dt_swizzle_mode = luma->surface.u.gfx9.swizzle_mode;
   decode->dt_array_mode = dec->addr_gfx_mode;
   decode->dt_field_mode = ((struct vl_video_buffer *)out_surf)->base.interlaced;
   decode->dt_surf_tile_config = 0;
   decode->dt_uv_surf_tile_config = 0;

   decode->dt_luma_top_offset = luma->surface.u.gfx9.surf_offset | (luma->surface.tile_swizzle << 8);
   decode->dt_chroma_top_offset = chroma->surface.u.gfx9.surf_offset| (chroma->surface.tile_swizzle << 8);
   if (decode->dt_field_mode) {
      decode->dt_luma_bottom_offset =
         decode->dt_luma_top_offset + luma->surface.u.gfx9.surf_slice_size;
      decode->dt_chroma_bottom_offset =
         decode->dt_chroma_top_offset + chroma->surface.u.gfx9.surf_slice_size;
   } else {
      decode->dt_luma_bottom_offset = decode->dt_luma_top_offset;
      decode->dt_chroma_bottom_offset = decode->dt_chroma_top_offset;
   }
   decode->mif_wrc_en = sscreen->info.vcn_ip_version >= VCN_3_0_0;
   if (dec->stream_type == RDECODE_CODEC_AV1)
      decode->db_pitch_uv = chroma->surface.u.gfx9.surf_pitch * chroma->surface.blk_w;

   if (picture->cenc) {
      if (!dec->subsample.res && !si_vid_create_buffer(dec->screen, &dec->subsample,
                                                       RDECODE_MAX_SUBSAMPLE_SIZE,
                                                       PIPE_USAGE_DEFAULT)) {
         RADEON_DEC_ERR("Can't allocate subsample buffer.\n");
         return NULL;
      }
      int ss_length = MIN2(secure_buf->desc.subsamples_length, MAX_SUBSAMPLES);
      int total_ss_size = 0;
      uint32_t *ss_ptr = dec->ws->buffer_map(dec->ws, dec->subsample.res->buf, &dec->cs,
                                             PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);
      if (!ss_ptr) {
         RADEON_DEC_ERR("Failed to map subsample buffer memory.\n");
         return NULL;
      }
      for (int i = 0; i < ss_length; i++) {
         memcpy(&ss_ptr[i * 2], &secure_buf->desc.subsamples[i].num_bytes_clear, 8);
         total_ss_size += ss_ptr[i * 2] + ss_ptr[i * 2 + 1];
      }
      assert(total_ss_size <= decode->bsd_size);
      if (ss_ptr[ss_length * 2 - 1] != 0)
         ss_ptr[ss_length * 2 - 1] += (decode->bsd_size - total_ss_size);
      else
         ss_ptr[ss_length * 2 - 2] += (decode->bsd_size - total_ss_size);
      dec->ws->buffer_unmap(dec->ws, dec->subsample.res->buf);
   }

   if (encrypted) {
      assert(sscreen->info.has_tmz_support);
      if (picture->cenc)
         set_drm_keys_cenc(drm, secure_buf);
      else
         set_drm_keys(drm, decrypt);
   }

   if (dec->dpb_type == DPB_DYNAMIC_TIER_1) {
      decode->decode_flags |= (RDECODE_FLAGS_USE_DYNAMIC_DPB_MASK | RDECODE_FLAGS_USE_PAL_MASK);
      // Add decode flag for RESIZE_DPB ,when we do resize
      if (dpb_resize == true)
        decode->decode_flags |= RDECODE_FLAGS_DPB_RESIZE_MASK;

      dynamic_dpb->dpbArraySize = NUM_VP9_REFS + 1;
      dynamic_dpb->dpbLumaPitch = align(dec->max_width, dec->db_alignment);
      dynamic_dpb->dpbLumaAlignedHeight = align(dec->max_height, dec->db_alignment);
      dynamic_dpb->dpbLumaAlignedSize =
         dynamic_dpb->dpbLumaPitch * dynamic_dpb->dpbLumaAlignedHeight;
      dynamic_dpb->dpbChromaPitch = dynamic_dpb->dpbLumaPitch >> 1;
      dynamic_dpb->dpbChromaAlignedHeight = dynamic_dpb->dpbLumaAlignedHeight >> 1;
      dynamic_dpb->dpbChromaAlignedSize =
         dynamic_dpb->dpbChromaPitch * dynamic_dpb->dpbChromaAlignedHeight * 2;
      dynamic_dpb->dpbReserved0[0] = dec->db_alignment;

      if (dec->base.profile == PIPE_VIDEO_PROFILE_VP9_PROFILE2) {
         dynamic_dpb->dpbLumaAlignedSize = dynamic_dpb->dpbLumaAlignedSize * 3 / 2;
         dynamic_dpb->dpbChromaAlignedSize = dynamic_dpb->dpbChromaAlignedSize * 3 / 2;
      }
   }

   switch (u_reduce_video_profile(picture->profile)) {
   case PIPE_VIDEO_FORMAT_MPEG4_AVC: {
      rvcn_dec_message_avc_t avc = get_h264_msg(dec, target, (struct pipe_h264_picture_desc *)picture);
      memcpy(codec, (void *)&avc, sizeof(rvcn_dec_message_avc_t));
      index_codec->message_id = RDECODE_MESSAGE_AVC;
      index_codec->size = sizeof(rvcn_dec_message_avc_t);
      break;
   }
   case PIPE_VIDEO_FORMAT_HEVC: {
      rvcn_dec_message_hevc_t hevc =
         get_h265_msg(dec, target, (struct pipe_h265_picture_desc *)picture);

      memcpy(codec, (void *)&hevc, sizeof(rvcn_dec_message_hevc_t));
      index_codec->message_id = RDECODE_MESSAGE_HEVC;
      index_codec->size = sizeof(rvcn_dec_message_hevc_t);
      break;
   }
   case PIPE_VIDEO_FORMAT_VC1: {
      rvcn_dec_message_vc1_t vc1 = get_vc1_msg((struct pipe_vc1_picture_desc *)picture);

      memcpy(codec, (void *)&vc1, sizeof(rvcn_dec_message_vc1_t));
      if ((picture->profile == PIPE_VIDEO_PROFILE_VC1_SIMPLE) ||
          (picture->profile == PIPE_VIDEO_PROFILE_VC1_MAIN)) {
         decode->width_in_samples = align(decode->width_in_samples, 16) / 16;
         decode->height_in_samples = align(decode->height_in_samples, 16) / 16;
      }
      index_codec->message_id = RDECODE_MESSAGE_VC1;
      index_codec->size = sizeof(rvcn_dec_message_vc1_t);
      break;
   }
   case PIPE_VIDEO_FORMAT_MPEG12: {
      rvcn_dec_message_mpeg2_vld_t mpeg2 =
         get_mpeg2_msg(dec, target, (struct pipe_mpeg12_picture_desc *)picture);

      memcpy(codec, (void *)&mpeg2, sizeof(rvcn_dec_message_mpeg2_vld_t));
      index_codec->message_id = RDECODE_MESSAGE_MPEG2_VLD;
      index_codec->size = sizeof(rvcn_dec_message_mpeg2_vld_t);
      break;
   }
   case PIPE_VIDEO_FORMAT_MPEG4: {
      rvcn_dec_message_mpeg4_asp_vld_t mpeg4 =
         get_mpeg4_msg(dec, target, (struct pipe_mpeg4_picture_desc *)picture);

      memcpy(codec, (void *)&mpeg4, sizeof(rvcn_dec_message_mpeg4_asp_vld_t));
      index_codec->message_id = RDECODE_MESSAGE_MPEG4_ASP_VLD;
      index_codec->size = sizeof(rvcn_dec_message_mpeg4_asp_vld_t);
      break;
   }
   case PIPE_VIDEO_FORMAT_VP9: {
      rvcn_dec_message_vp9_t vp9 =
         get_vp9_msg(dec, target, (struct pipe_vp9_picture_desc *)picture);

      memcpy(codec, (void *)&vp9, sizeof(rvcn_dec_message_vp9_t));
      index_codec->message_id = RDECODE_MESSAGE_VP9;
      index_codec->size = sizeof(rvcn_dec_message_vp9_t);
      break;
   }
   case PIPE_VIDEO_FORMAT_AV1: {
      rvcn_dec_message_av1_t av1 =
         get_av1_msg(dec, target, (struct pipe_av1_picture_desc *)picture);

      memcpy(codec, (void *)&av1, sizeof(rvcn_dec_message_av1_t));
      index_codec->message_id = RDECODE_MESSAGE_AV1;
      index_codec->size = sizeof(rvcn_dec_message_av1_t);

      if (dec->ctx.res == NULL) {
         unsigned ctx_size = ac_vcn_dec_calc_ctx_size_av1(dec->av1_version);
         uint8_t *ptr;

         if (!si_vid_create_buffer(dec->screen, &dec->ctx, ctx_size, PIPE_USAGE_DEFAULT))
            RADEON_DEC_ERR("Can't allocate context buffer.\n");

         ptr = dec->ws->buffer_map(dec->ws, dec->ctx.res->buf, NULL, PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);

         ac_vcn_av1_init_probs(dec->av1_version, ptr);
         dec->ws->buffer_unmap(dec->ws, dec->ctx.res->buf);
      }

      break;
   }
   default:
      assert(0);
      return NULL;
   }

   header->total_size += index_codec->size;

   if (dec->ctx.res)
      decode->hw_ctxt_size = dec->ctx.res->buf->size;

   if (dec->dpb_type == DPB_DYNAMIC_TIER_2) {
      if (rvcn_dec_dynamic_dpb_t2_message(dec, decode, dynamic_dpb_t2, encrypted))
         return NULL;
   } else if (((struct si_screen *)dec->screen)->info.vcn_ip_version == VCN_5_0_0 &&
                dec->dpb_type == DPB_MAX_RES)
      decode->db_swizzle_mode = RDECODE_VCN5_256B_D;

   return luma->buffer.buf;
}

static void rvcn_dec_message_destroy(struct radeon_decoder *dec)
{
   rvcn_dec_message_header_t *header = dec->msg;

   memset(dec->msg, 0, sizeof(rvcn_dec_message_header_t));
   header->header_size = sizeof(rvcn_dec_message_header_t);
   header->total_size = sizeof(rvcn_dec_message_header_t) - sizeof(rvcn_dec_message_index_t);
   header->num_buffers = 0;
   header->msg_type = RDECODE_MSG_DESTROY;
   header->stream_handle = dec->stream_handle;
   header->status_report_feedback_number = 0;
}

static void rvcn_dec_message_feedback(struct radeon_decoder *dec)
{
   rvcn_dec_feedback_header_t *header = (void *)dec->fb;

   header->header_size = sizeof(rvcn_dec_feedback_header_t);
   header->total_size = sizeof(rvcn_dec_feedback_header_t);
   header->num_buffers = 0;
}

static void rvcn_dec_sq_tail(struct radeon_decoder *dec)
{
   if (dec->vcn_dec_sw_ring == false)
      return;

   rvcn_sq_tail(&dec->cs, &dec->sq);
}
/* flush IB to the hardware */
static int flush(struct radeon_decoder *dec, unsigned flags,
                 struct pipe_fence_handle **fence)
{
   struct si_screen *sscreen = (struct si_screen *)dec->screen;

   rvcn_dec_sq_tail(dec);

   if (sscreen->debug_flags & DBG(IB)) {
      struct ac_ib_parser ib_parser = {
         .f = stderr,
         .ib = dec->cs.current.buf,
         .num_dw = dec->cs.current.cdw,
         .gfx_level = sscreen->info.gfx_level,
         .vcn_version = sscreen->info.vcn_ip_version,
         .family = sscreen->info.family,
         .ip_type = dec->stream_type == RDECODE_CODEC_JPEG ? AMD_IP_VCN_JPEG :
                    dec->vcn_dec_sw_ring ? AMD_IP_VCN_ENC : AMD_IP_VCN_DEC,
      };
      ac_parse_ib(&ib_parser, "IB");
   }

   return dec->ws->cs_flush(&dec->cs, flags, fence);
}

/* add a new set register command to the IB */
static void set_reg(struct radeon_decoder *dec, unsigned reg, uint32_t val)
{
   radeon_emit(&dec->cs, RDECODE_PKT0(reg >> 2, 0));
   radeon_emit(&dec->cs, val);
}

/* send a command to the VCPU through the GPCOM registers */
static void send_cmd(struct radeon_decoder *dec, unsigned cmd, struct pb_buffer_lean *buf, uint32_t off,
                     unsigned usage, enum radeon_bo_domain domain)
{
   uint64_t addr;

   dec->ws->cs_add_buffer(&dec->cs, buf, usage | RADEON_USAGE_SYNCHRONIZED, domain);
   addr = dec->ws->buffer_get_virtual_address(buf);
   addr = addr + off;

   if (dec->vcn_dec_sw_ring == false) {
      set_reg(dec, dec->reg.data0, addr);
      set_reg(dec, dec->reg.data1, addr >> 32);
      set_reg(dec, dec->reg.cmd, cmd << 1);
      return;
   }

   if (!dec->cs.current.cdw) {
      rvcn_sq_header(&dec->cs, &dec->sq, false);
      rvcn_decode_ib_package_t *ib_header =
         (rvcn_decode_ib_package_t *)&(dec->cs.current.buf[dec->cs.current.cdw]);

      ib_header->package_size = sizeof(struct rvcn_decode_buffer_s) +
         sizeof(struct rvcn_decode_ib_package_s);
      dec->cs.current.cdw++;
      ib_header->package_type = (RDECODE_IB_PARAM_DECODE_BUFFER);
      dec->cs.current.cdw++;

      dec->decode_buffer =
         (rvcn_decode_buffer_t *)&(dec->cs.current.buf[dec->cs.current.cdw]);

      dec->cs.current.cdw += sizeof(struct rvcn_decode_buffer_s) / 4;
      memset(dec->decode_buffer, 0, sizeof(struct rvcn_decode_buffer_s));
   }

   switch(cmd) {
      case RDECODE_CMD_MSG_BUFFER:
            dec->decode_buffer->valid_buf_flag |= RDECODE_CMDBUF_FLAGS_MSG_BUFFER;
            dec->decode_buffer->msg_buffer_address_hi = (addr >> 32);
            dec->decode_buffer->msg_buffer_address_lo = (addr);
         break;
      case RDECODE_CMD_DPB_BUFFER:
            dec->decode_buffer->valid_buf_flag |= (RDECODE_CMDBUF_FLAGS_DPB_BUFFER);
            dec->decode_buffer->dpb_buffer_address_hi = (addr >> 32);
            dec->decode_buffer->dpb_buffer_address_lo = (addr);
         break;
      case RDECODE_CMD_DECODING_TARGET_BUFFER:
            dec->decode_buffer->valid_buf_flag |= (RDECODE_CMDBUF_FLAGS_DECODING_TARGET_BUFFER);
            dec->decode_buffer->target_buffer_address_hi = (addr >> 32);
            dec->decode_buffer->target_buffer_address_lo = (addr);
         break;
      case RDECODE_CMD_FEEDBACK_BUFFER:
            dec->decode_buffer->valid_buf_flag |= (RDECODE_CMDBUF_FLAGS_FEEDBACK_BUFFER);
            dec->decode_buffer->feedback_buffer_address_hi = (addr >> 32);
            dec->decode_buffer->feedback_buffer_address_lo = (addr);
         break;
      case RDECODE_CMD_PROB_TBL_BUFFER:
            dec->decode_buffer->valid_buf_flag |= (RDECODE_CMDBUF_FLAGS_PROB_TBL_BUFFER);
            dec->decode_buffer->prob_tbl_buffer_address_hi = (addr >> 32);
            dec->decode_buffer->prob_tbl_buffer_address_lo = (addr);
         break;
      case RDECODE_CMD_SESSION_CONTEXT_BUFFER:
            dec->decode_buffer->valid_buf_flag |= (RDECODE_CMDBUF_FLAGS_SESSION_CONTEXT_BUFFER);
            dec->decode_buffer->session_contex_buffer_address_hi = (addr >> 32);
            dec->decode_buffer->session_contex_buffer_address_lo = (addr);
         break;
      case RDECODE_CMD_BITSTREAM_BUFFER:
            dec->decode_buffer->valid_buf_flag |= (RDECODE_CMDBUF_FLAGS_BITSTREAM_BUFFER);
            dec->decode_buffer->bitstream_buffer_address_hi = (addr >> 32);
            dec->decode_buffer->bitstream_buffer_address_lo = (addr);
         break;
      case RDECODE_CMD_IT_SCALING_TABLE_BUFFER:
            dec->decode_buffer->valid_buf_flag |= (RDECODE_CMDBUF_FLAGS_IT_SCALING_BUFFER);
            dec->decode_buffer->it_sclr_table_buffer_address_hi = (addr >> 32);
            dec->decode_buffer->it_sclr_table_buffer_address_lo = (addr);
         break;
      case RDECODE_CMD_CONTEXT_BUFFER:
            dec->decode_buffer->valid_buf_flag |= (RDECODE_CMDBUF_FLAGS_CONTEXT_BUFFER);
            dec->decode_buffer->context_buffer_address_hi = (addr >> 32);
            dec->decode_buffer->context_buffer_address_lo = (addr);
         break;
      case RDECODE_CMD_SUBSAMPLE:
            dec->decode_buffer->valid_buf_flag |= (RDECODE_CMDBUF_FLAGS_SUBSAMPLE_SIZE_INFO_BUFFER);
            dec->decode_buffer->subsample_hi = (addr >> 32);
            dec->decode_buffer->subsample_lo = (addr);
         break;
      default:
            printf("Not Support!");
   }
}

/* do the codec needs an IT buffer ?*/
static bool have_it(struct radeon_decoder *dec)
{
   return dec->stream_type == RDECODE_CODEC_H264_PERF || dec->stream_type == RDECODE_CODEC_H265;
}

/* do the codec needs an probs buffer? */
static bool have_probs(struct radeon_decoder *dec)
{
   return (dec->stream_type == RDECODE_CODEC_VP9 || dec->stream_type == RDECODE_CODEC_AV1);
}

/* map the next available message/feedback/itscaling buffer */
static void map_msg_fb_it_probs_buf(struct radeon_decoder *dec)
{
   struct rvid_buffer *buf;
   uint8_t *ptr;

   /* grab the current message/feedback buffer */
   buf = &dec->msg_fb_it_probs_buffers[dec->cur_buffer];

   /* and map it for CPU access */
   ptr =
      dec->ws->buffer_map(dec->ws, buf->res->buf, NULL, PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);

   /* calc buffer offsets */
   dec->msg = ptr;

   dec->fb = (uint32_t *)(ptr + FB_BUFFER_OFFSET);
   if (have_it(dec))
      dec->it = (uint8_t *)(ptr + FB_BUFFER_OFFSET + FB_BUFFER_SIZE);
   else if (have_probs(dec))
      dec->probs = (uint8_t *)(ptr + FB_BUFFER_OFFSET + FB_BUFFER_SIZE);
}

/* unmap and send a message command to the VCPU */
static void send_msg_buf(struct radeon_decoder *dec)
{
   struct rvid_buffer *buf;

   /* ignore the request if message/feedback buffer isn't mapped */
   if (!dec->msg || !dec->fb)
      return;

   /* grab the current message buffer */
   buf = &dec->msg_fb_it_probs_buffers[dec->cur_buffer];

   /* unmap the buffer */
   dec->ws->buffer_unmap(dec->ws, buf->res->buf);
   dec->bs_ptr = NULL;
   dec->msg = NULL;
   dec->fb = NULL;
   dec->it = NULL;
   dec->probs = NULL;

   send_cmd(dec, RDECODE_CMD_SESSION_CONTEXT_BUFFER, dec->sessionctx.res->buf, 0,
            RADEON_USAGE_READWRITE, RADEON_DOMAIN_VRAM);

   /* and send it to the hardware */
   send_cmd(dec, RDECODE_CMD_MSG_BUFFER, buf->res->buf, 0, RADEON_USAGE_READ, RADEON_DOMAIN_GTT);
}

/* cycle to the next set of buffers */
static void next_buffer(struct radeon_decoder *dec)
{
   ++dec->cur_buffer;
   dec->cur_buffer %= dec->num_dec_bufs;
}

static unsigned calc_ctx_size_h264_perf(struct radeon_decoder *dec)
{
   unsigned width_in_mb, height_in_mb, ctx_size;
   unsigned width = align(dec->base.width, VL_MACROBLOCK_WIDTH);
   unsigned height = align(dec->base.height, VL_MACROBLOCK_HEIGHT);

   unsigned max_references = dec->base.max_references + 1;

   // picture width & height in 16 pixel units
   width_in_mb = width / VL_MACROBLOCK_WIDTH;
   height_in_mb = align(height / VL_MACROBLOCK_HEIGHT, 2);

   unsigned fs_in_mb = width_in_mb * height_in_mb;
   unsigned num_dpb_buffer_lean;
   switch (dec->base.level) {
   case 30:
      num_dpb_buffer_lean = 8100 / fs_in_mb;
      break;
   case 31:
      num_dpb_buffer_lean = 18000 / fs_in_mb;
      break;
   case 32:
      num_dpb_buffer_lean = 20480 / fs_in_mb;
      break;
   case 41:
      num_dpb_buffer_lean = 32768 / fs_in_mb;
      break;
   case 42:
      num_dpb_buffer_lean = 34816 / fs_in_mb;
      break;
   case 50:
      num_dpb_buffer_lean = 110400 / fs_in_mb;
      break;
   case 51:
      num_dpb_buffer_lean = 184320 / fs_in_mb;
      break;
   default:
      num_dpb_buffer_lean = 184320 / fs_in_mb;
      break;
   }
   num_dpb_buffer_lean++;
   max_references = MAX2(MIN2(NUM_H264_REFS, num_dpb_buffer_lean), max_references);
   ctx_size = max_references * align(width_in_mb * height_in_mb * 192, 256);

   return ctx_size;
}

/* calculate size of reference picture buffer */
static unsigned calc_dpb_size(struct radeon_decoder *dec)
{
   unsigned width_in_mb, height_in_mb, image_size, dpb_size;

   // always align them to MB size for dpb calculation
   unsigned width = align(dec->base.width, VL_MACROBLOCK_WIDTH);
   unsigned height = align(dec->base.height, VL_MACROBLOCK_HEIGHT);

   // always one more for currently decoded picture
   unsigned max_references = dec->base.max_references + 1;

   // aligned size of a single frame
   image_size = align(width, dec->db_alignment) * align(height, dec->db_alignment);
   image_size += image_size / 2;
   image_size = align(image_size, 1024);

   // picture width & height in 16 pixel units
   width_in_mb = width / VL_MACROBLOCK_WIDTH;
   height_in_mb = align(height / VL_MACROBLOCK_HEIGHT, 2);

   switch (u_reduce_video_profile(dec->base.profile)) {
   case PIPE_VIDEO_FORMAT_MPEG4_AVC: {
      unsigned fs_in_mb = width_in_mb * height_in_mb;
      unsigned num_dpb_buffer_lean;

      switch (dec->base.level) {
      case 30:
         num_dpb_buffer_lean = 8100 / fs_in_mb;
         break;
      case 31:
         num_dpb_buffer_lean = 18000 / fs_in_mb;
         break;
      case 32:
         num_dpb_buffer_lean = 20480 / fs_in_mb;
         break;
      case 41:
         num_dpb_buffer_lean = 32768 / fs_in_mb;
         break;
      case 42:
         num_dpb_buffer_lean = 34816 / fs_in_mb;
         break;
      case 50:
         num_dpb_buffer_lean = 110400 / fs_in_mb;
         break;
      case 51:
         num_dpb_buffer_lean = 184320 / fs_in_mb;
         break;
      default:
         num_dpb_buffer_lean = 184320 / fs_in_mb;
         break;
      }
      num_dpb_buffer_lean++;
      max_references = MAX2(MIN2(NUM_H264_REFS, num_dpb_buffer_lean), max_references);
      dpb_size = image_size * max_references;
      break;
   }

   case PIPE_VIDEO_FORMAT_HEVC:
      if (dec->base.width * dec->base.height >= 4096 * 2000)
         max_references = MAX2(max_references, 8);
      else
         max_references = MAX2(max_references, 17);

      if (dec->base.profile == PIPE_VIDEO_PROFILE_HEVC_MAIN_10)
         dpb_size = align((align(width, dec->db_alignment) *
                    align(height, dec->db_alignment) * 9) / 4, 256) * max_references;
      else
         dpb_size = align((align(width, dec->db_alignment) *
                    align(height, dec->db_alignment) * 3) / 2, 256) * max_references;
      break;

   case PIPE_VIDEO_FORMAT_VC1:
      // the firmware seems to always assume a minimum of ref frames
      max_references = MAX2(NUM_VC1_REFS, max_references);

      // reference picture buffer
      dpb_size = image_size * max_references;

      // CONTEXT_BUFFER
      dpb_size += width_in_mb * height_in_mb * 128;

      // IT surface buffer
      dpb_size += width_in_mb * 64;

      // DB surface buffer
      dpb_size += width_in_mb * 128;

      // BP
      dpb_size += align(MAX2(width_in_mb, height_in_mb) * 7 * 16, 64);
      break;

   case PIPE_VIDEO_FORMAT_MPEG12:
      // reference picture buffer, must be big enough for all frames
      dpb_size = image_size * NUM_MPEG2_REFS;
      break;

   case PIPE_VIDEO_FORMAT_MPEG4:
      // reference picture buffer
      dpb_size = image_size * max_references;

      // CM
      dpb_size += width_in_mb * height_in_mb * 64;

      // IT surface buffer
      dpb_size += align(width_in_mb * height_in_mb * 32, 64);

      dpb_size = MAX2(dpb_size, 30 * 1024 * 1024);
      break;

   case PIPE_VIDEO_FORMAT_VP9:
      max_references = MAX2(max_references, 9);

      if (dec->dpb_type == DPB_MAX_RES)
         dpb_size = (((struct si_screen *)dec->screen)->info.vcn_ip_version >= VCN_2_0_0)
            ? (8192 * 4320 * 3 / 2) * max_references
            : (4096 * 3000 * 3 / 2) * max_references;
      else
         dpb_size = (align(dec->base.width, dec->db_alignment) *
            align(dec->base.height, dec->db_alignment) * 3 / 2) * max_references;

      if (dec->base.profile == PIPE_VIDEO_PROFILE_VP9_PROFILE2)
         dpb_size = dpb_size * 3 / 2;
      break;

   case PIPE_VIDEO_FORMAT_AV1:
      max_references = MAX2(max_references, 9);
      dpb_size = 8192 * 4320 * 3 / 2 * max_references * 3 / 2;
      break;

   case PIPE_VIDEO_FORMAT_JPEG:
      dpb_size = 0;
      break;

   default:
      // something is missing here
      assert(0);

      // at least use a sane default value
      dpb_size = 32 * 1024 * 1024;
      break;
   }
   return dpb_size;
}

/**
 * destroy this video decoder
 */
static void radeon_dec_destroy(struct pipe_video_codec *decoder)
{
   struct radeon_decoder *dec = (struct radeon_decoder *)decoder;
   unsigned i;

   assert(decoder);

   if (dec->bs_ptr) {
      dec->ws->buffer_unmap(dec->ws, dec->bs_buffers[dec->cur_buffer].res->buf);
      dec->bs_ptr = NULL;
   }

   if (dec->msg) {
      dec->ws->buffer_unmap(dec->ws, dec->msg_fb_it_probs_buffers[dec->cur_buffer].res->buf);
      dec->msg = NULL;
   }

   if (dec->stream_type != RDECODE_CODEC_JPEG) {
      struct pipe_fence_handle *fence = NULL;
      map_msg_fb_it_probs_buf(dec);
      rvcn_dec_message_destroy(dec);
      send_msg_buf(dec);
      flush(dec, 0, &fence);
      dec->ws->fence_wait(dec->ws, fence, OS_TIMEOUT_INFINITE);
      dec->ws->fence_reference(dec->ws, &fence, NULL);
   }

   dec->ws->cs_destroy(&dec->cs);
   if (dec->ectx)
      dec->ectx->destroy(dec->ectx);

   if (dec->stream_type == RDECODE_CODEC_JPEG) {
      for (i = 0; i < dec->njctx; i++) {
         dec->ws->cs_destroy(&dec->jcs[i]);
         dec->ws->ctx_destroy(dec->jctx[i]);
      }
   }

   if (dec->msg_fb_it_probs_buffers && dec->bs_buffers) {
      for (i = 0; i < dec->num_dec_bufs; ++i) {
            si_vid_destroy_buffer(&dec->msg_fb_it_probs_buffers[i]);
            si_vid_destroy_buffer(&dec->bs_buffers[i]);
      }
      FREE(dec->msg_fb_it_probs_buffers);
      FREE(dec->bs_buffers);
   }
   dec->num_dec_bufs = 0;

   if (dec->dpb_type != DPB_DYNAMIC_TIER_2) {
      si_vid_destroy_buffer(&dec->dpb);
   } else {
      list_for_each_entry_safe(struct rvcn_dec_dynamic_dpb_t2, d, &dec->dpb_ref_list, list) {
         list_del(&d->list);
         pipe_resource_reference(&d->buf, NULL);
         FREE(d);
      }
   }
   si_vid_destroy_buffer(&dec->ctx);
   si_vid_destroy_buffer(&dec->sessionctx);
   si_vid_destroy_buffer(&dec->subsample);

   FREE(dec->jcs);
   FREE(dec->jctx);
   FREE(dec);
}

/**
 * start decoding of a new frame
 */
static void radeon_dec_begin_frame(struct pipe_video_codec *decoder,
                                   struct pipe_video_buffer *target,
                                   struct pipe_picture_desc *picture)
{
   struct radeon_decoder *dec = (struct radeon_decoder *)decoder;

   assert(decoder);

   if (dec->error)
      return;

   switch (dec->stream_type) {
   case RDECODE_CODEC_VP9: {
      struct pipe_vp9_picture_desc *pic = (struct pipe_vp9_picture_desc *)picture;
      /* Only 10 bit is supported for Profile 2 */
      if (pic->picture_parameter.bit_depth > 10) {
         dec->error = true;
         return;
      }
      break;
   }
   case RDECODE_CODEC_AV1: {
      struct pipe_av1_picture_desc *pic = (struct pipe_av1_picture_desc *)picture;
      /* Only 4:2:0 is supported for Profile 2 */
      if (!pic->picture_parameter.seq_info_fields.subsampling_x ||
          !pic->picture_parameter.seq_info_fields.subsampling_y) {
         dec->error = true;
         return;
      }
      break;
   }
   default:
      break;
   }

   dec->bs_size = 0;
   dec->bs_ptr = dec->ws->buffer_map(dec->ws, dec->bs_buffers[dec->cur_buffer].res->buf, NULL,
                                     PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);
}

/**
 * decode a macroblock
 */
static void radeon_dec_decode_macroblock(struct pipe_video_codec *decoder,
                                         struct pipe_video_buffer *target,
                                         struct pipe_picture_desc *picture,
                                         const struct pipe_macroblock *macroblocks,
                                         unsigned num_macroblocks)
{
   /* not supported (yet) */
   assert(0);
}

/**
 * decode a bitstream
 */
static void radeon_dec_decode_bitstream(struct pipe_video_codec *decoder,
                                        struct pipe_video_buffer *target,
                                        struct pipe_picture_desc *picture, unsigned num_buffers,
                                        const void *const *buffers, const unsigned *sizes)
{
   struct radeon_decoder *dec = (struct radeon_decoder *)decoder;
   unsigned i;

   assert(decoder);

   if (dec->error)
      return;

   if (!dec->bs_ptr) {
      RADEON_DEC_ERR("Invalid bitstream ptr!\n");
      return;
   }

   unsigned long total_bs_size = dec->bs_size;
   for (i = 0; i < num_buffers; ++i)
      total_bs_size += sizes[i];

   struct rvid_buffer *buf = &dec->bs_buffers[dec->cur_buffer];

   if (total_bs_size > buf->res->buf->size) {
      dec->ws->buffer_unmap(dec->ws, buf->res->buf);
      dec->bs_ptr = NULL;

      total_bs_size = align(total_bs_size, 128);

      if (!dec->bs_size) {
         struct rvid_buffer old_buf = *buf;
         if (!si_vid_create_buffer(dec->screen, buf, total_bs_size, buf->usage)) {
            RADEON_DEC_ERR("Can't create bitstream buffer!");
            return;
         }
         si_vid_destroy_buffer(&old_buf);
      } else if (!si_vid_resize_buffer(dec->base.context, buf, total_bs_size, NULL)) {
         RADEON_DEC_ERR("Can't resize bitstream buffer!");
         return;
      }

      dec->bs_ptr = dec->ws->buffer_map(dec->ws, buf->res->buf, NULL,
                                        PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);
      if (!dec->bs_ptr)
         return;

      dec->bs_ptr += dec->bs_size;
   }

   for (i = 0; i < num_buffers; ++i) {
      memcpy(dec->bs_ptr, buffers[i], sizes[i]);
      dec->bs_size += sizes[i];
      dec->bs_ptr += sizes[i];
   }
}

static void send_ref_buffers(struct radeon_decoder *dec)
{
   uint32_t size = sizeof(rvcn_dec_ref_buffers_header_t) +
      sizeof(rvcn_dec_ref_buffer_t) * dec->ref_codec.num_refs;
   rvcn_decode_ib_package_t *ib_header =
      (rvcn_decode_ib_package_t *)&(dec->cs.current.buf[dec->cs.current.cdw]);

   ib_header->package_size = size + sizeof(rvcn_decode_ib_package_t);
   ib_header->package_type = RDECODE_IB_PARAM_DYNAMIC_REFLIST_BUFFER;

   dec->cs.current.cdw += 2;

   rvcn_dec_ref_buffers_header_t *refs =
      (rvcn_dec_ref_buffers_header_t *)&(dec->cs.current.buf[dec->cs.current.cdw]);

   dec->cs.current.cdw += size / 4;

   refs->size = size;
   refs->num_bufs = dec->ref_codec.num_refs;

   for (uint32_t i = 0; i < refs->num_bufs; i++) {
      struct vl_video_buffer *buf = (struct vl_video_buffer *)dec->ref_codec.bufs[i].buf;
      struct si_texture *y = (struct si_texture *)buf->resources[0];
      struct si_texture *uv = (struct si_texture *)buf->resources[1];
      uint64_t y_addr = y->buffer.gpu_address + y->surface.u.gfx9.surf_offset;
      uint64_t uv_addr = uv->buffer.gpu_address + uv->surface.u.gfx9.surf_offset;
      rvcn_dec_ref_buffer_t *ref = &refs->pBufs[i];
      ref->index = dec->ref_codec.bufs[i].index;
      ref->y_pitch = y->surface.u.gfx9.surf_pitch;
      ref->y_aligned_height = y->surface.u.gfx9.surf_height;
      ref->y_aligned_size = y->surface.u.gfx9.surf_slice_size;
      ref->y_ref_buffer_address_hi = y_addr >> 32;
      ref->y_ref_buffer_address_lo = y_addr;
      ref->uv_pitch = uv->surface.u.gfx9.surf_pitch;
      ref->uv_aligned_height = uv->surface.u.gfx9.surf_height;
      ref->uv_aligned_size = uv->surface.u.gfx9.surf_slice_size;
      ref->uv_ref_buffer_address_hi = uv_addr >> 32;
      ref->uv_ref_buffer_address_lo = uv_addr;
      dec->ws->cs_add_buffer(&dec->cs, y->buffer.buf, RADEON_USAGE_READWRITE | RADEON_USAGE_SYNCHRONIZED, RADEON_DOMAIN_VRAM);
   }

   dec->decode_buffer->valid_buf_flag |= RDECODE_CMDBUF_FLAGS_REF_BUFFER;
}

/**
 * send cmd for vcn dec
 */
bool send_cmd_dec(struct radeon_decoder *dec, struct pipe_video_buffer *target,
                  struct pipe_picture_desc *picture)
{
   struct pb_buffer_lean *dt;
   struct rvid_buffer *msg_fb_it_probs_buf, *bs_buf;

   msg_fb_it_probs_buf = &dec->msg_fb_it_probs_buffers[dec->cur_buffer];
   bs_buf = &dec->bs_buffers[dec->cur_buffer];

   memset(dec->bs_ptr, 0, align(dec->bs_size, 128) - dec->bs_size);
   dec->ws->buffer_unmap(dec->ws, bs_buf->res->buf);
   dec->bs_ptr = NULL;

   map_msg_fb_it_probs_buf(dec);
   dt = rvcn_dec_message_decode(dec, target, picture);
   if (!dt)
      return false;
   rvcn_dec_message_feedback(dec);
   send_msg_buf(dec);

   if (dec->dpb_type < DPB_DYNAMIC_TIER_2)
      send_cmd(dec, RDECODE_CMD_DPB_BUFFER, dec->dpb.res->buf, 0, RADEON_USAGE_READWRITE,
            RADEON_DOMAIN_VRAM);
   if (dec->ctx.res)
      send_cmd(dec, RDECODE_CMD_CONTEXT_BUFFER, dec->ctx.res->buf, 0, RADEON_USAGE_READWRITE,
               RADEON_DOMAIN_VRAM);
   send_cmd(dec, RDECODE_CMD_BITSTREAM_BUFFER, bs_buf->res->buf, 0, RADEON_USAGE_READ,
            RADEON_DOMAIN_GTT);
   send_cmd(dec, RDECODE_CMD_DECODING_TARGET_BUFFER, dt, 0, RADEON_USAGE_WRITE, RADEON_DOMAIN_VRAM);
   send_cmd(dec, RDECODE_CMD_FEEDBACK_BUFFER, msg_fb_it_probs_buf->res->buf, FB_BUFFER_OFFSET,
            RADEON_USAGE_WRITE, RADEON_DOMAIN_GTT);
   if (have_it(dec))
      send_cmd(dec, RDECODE_CMD_IT_SCALING_TABLE_BUFFER, msg_fb_it_probs_buf->res->buf,
               FB_BUFFER_OFFSET + FB_BUFFER_SIZE, RADEON_USAGE_READ, RADEON_DOMAIN_GTT);
   else if (have_probs(dec))
      send_cmd(dec, RDECODE_CMD_PROB_TBL_BUFFER, msg_fb_it_probs_buf->res->buf,
               FB_BUFFER_OFFSET + FB_BUFFER_SIZE, RADEON_USAGE_READ, RADEON_DOMAIN_GTT);
   if (picture->cenc)
      send_cmd(dec, RDECODE_CMD_SUBSAMPLE, dec->subsample.res->buf, 0, RADEON_USAGE_READ,
               RADEON_DOMAIN_VRAM);

   if (dec->dpb_type == DPB_DYNAMIC_TIER_3)
      send_ref_buffers(dec);

   if (dec->vcn_dec_sw_ring == false)
      set_reg(dec, dec->reg.cntl, 1);

   return true;
}

/**
 * end decoding of the current frame
 */
static int radeon_dec_end_frame(struct pipe_video_codec *decoder, struct pipe_video_buffer *target,
                                struct pipe_picture_desc *picture)
{
   struct radeon_decoder *dec = (struct radeon_decoder *)decoder;

   assert(decoder);

   if (dec->error)
      return 1;

   dec->frame_number++;

   if (!dec->send_cmd(dec, target, picture))
      return 1;

   flush(dec, picture->flush_flags, picture->out_fence);

   next_buffer(dec);
   return 0;
}

static bool radeon_dec_jpeg_check_format(struct radeon_decoder *dec, enum pipe_format format,
                                         unsigned sampling_factor)
{
   enum pipe_format expected_format;

   switch (sampling_factor) {
   case 0x221111:
      expected_format = PIPE_FORMAT_NV12;
      break;
   case 0x211111:
   case 0x221212:
   case 0x222121:
      expected_format = PIPE_FORMAT_YUYV;
      break;
   case 0x111111:
   case 0x222222:
   case 0x444444:
      expected_format = PIPE_FORMAT_Y8_U8_V8_444_UNORM;
      break;
   case 0x121111:
      expected_format = PIPE_FORMAT_Y8_U8_V8_440_UNORM;
      break;
   case 0x11:
   case 0x44:
      expected_format = PIPE_FORMAT_Y8_400_UNORM;
      break;
   default:
      RADEON_DEC_ERR("Unsupported sampling factor 0x%x\n", sampling_factor);
      return false;
   }

   /* Format conversion */
   if (format == PIPE_FORMAT_R8G8B8A8_UNORM ||
       format == PIPE_FORMAT_A8R8G8B8_UNORM ||
       format == PIPE_FORMAT_R8_G8_B8_UNORM)
      return true;

   return expected_format == format;
}

/**
 * end decoding of the current jpeg frame
 */
static int radeon_dec_jpeg_end_frame(struct pipe_video_codec *decoder, struct pipe_video_buffer *target,
                                     struct pipe_picture_desc *picture)
{
   struct radeon_decoder *dec = (struct radeon_decoder *)decoder;
   struct pipe_mjpeg_picture_desc *pic = (struct pipe_mjpeg_picture_desc *)picture;

   assert(decoder);

   if (!radeon_dec_jpeg_check_format(dec, target->buffer_format, pic->picture_parameter.sampling_factor))
      RADEON_DEC_ERR("Decode format check failed\n");

   if (dec->error)
      return 1;

   dec->jpg.crop_x = ROUND_DOWN_TO(pic->picture_parameter.crop_x, VL_MACROBLOCK_WIDTH);
   dec->jpg.crop_y = ROUND_DOWN_TO(pic->picture_parameter.crop_y, VL_MACROBLOCK_HEIGHT);
   dec->jpg.crop_width = align(pic->picture_parameter.crop_width, VL_MACROBLOCK_WIDTH);
   dec->jpg.crop_height = align(pic->picture_parameter.crop_height, VL_MACROBLOCK_HEIGHT);
   if (dec->jpg.crop_x + dec->jpg.crop_width > pic->picture_parameter.picture_width)
      dec->jpg.crop_width = 0;
   if (dec->jpg.crop_y + dec->jpg.crop_height > pic->picture_parameter.picture_height)
      dec->jpg.crop_height = 0;
   dec->send_cmd(dec, target, picture);
   dec->ws->cs_flush(&dec->jcs[dec->cb_idx], picture->flush_flags, picture->out_fence);
   next_buffer(dec);
   dec->cb_idx = (dec->cb_idx+1) % dec->njctx;
   return 0;
}

/**
 * flush any outstanding command buffers to the hardware
 */
static void radeon_dec_flush(struct pipe_video_codec *decoder)
{
}

static int radeon_dec_fence_wait(struct pipe_video_codec *decoder,
                                 struct pipe_fence_handle *fence,
                                 uint64_t timeout)
{
   struct radeon_decoder *dec = (struct radeon_decoder *)decoder;

   return dec->ws->fence_wait(dec->ws, fence, timeout);
}

static void radeon_dec_destroy_fence(struct pipe_video_codec *decoder,
                                     struct pipe_fence_handle *fence)
{
   struct radeon_decoder *dec = (struct radeon_decoder *)decoder;

   dec->ws->fence_reference(dec->ws, &fence, NULL);
}

static bool radeon_dec_enable_tier3(struct si_context *sctx, uint32_t codec)
{
   if (sctx->vcn_ip_ver < VCN_5_0_0)
      return false;

   return true;
}

static bool radeon_dec_enable_tier2(struct si_context *sctx, uint32_t codec)
{
   if (sctx->vcn_ip_ver < VCN_3_0_0)
      return false;

   return codec == RDECODE_CODEC_VP9 || codec == RDECODE_CODEC_AV1 ||
          codec == RDECODE_CODEC_H265 || codec == RDECODE_CODEC_H264_PERF;
}

static bool radeon_dec_enable_tier1(struct si_context *sctx, uint32_t codec)
{
   if (sctx->vcn_ip_ver > VCN_2_6_0)
      return false;

   return codec == RDECODE_CODEC_VP9;
}

/**
 * create and HW decoder
 */
struct pipe_video_codec *radeon_create_decoder(struct pipe_context *context,
                                               const struct pipe_video_codec *templ)
{
   struct si_context *sctx = (struct si_context *)context;
   struct radeon_winsys *ws = sctx->ws;
   unsigned width = templ->width, height = templ->height;
   unsigned bs_buf_size, stream_type = 0, ring = AMD_IP_VCN_DEC;
   struct radeon_decoder *dec;
   int r, i;

   switch (u_reduce_video_profile(templ->profile)) {
   case PIPE_VIDEO_FORMAT_MPEG12:
      stream_type = RDECODE_CODEC_MPEG2_VLD;
      break;
   case PIPE_VIDEO_FORMAT_MPEG4:
      width = align(width, VL_MACROBLOCK_WIDTH);
      height = align(height, VL_MACROBLOCK_HEIGHT);
      stream_type = RDECODE_CODEC_MPEG4;
      break;
   case PIPE_VIDEO_FORMAT_VC1:
      stream_type = RDECODE_CODEC_VC1;
      break;
   case PIPE_VIDEO_FORMAT_MPEG4_AVC:
      width = align(width, VL_MACROBLOCK_WIDTH);
      height = align(height, VL_MACROBLOCK_HEIGHT);
      stream_type = RDECODE_CODEC_H264_PERF;
      break;
   case PIPE_VIDEO_FORMAT_HEVC:
      stream_type = RDECODE_CODEC_H265;
      break;
   case PIPE_VIDEO_FORMAT_VP9:
      stream_type = RDECODE_CODEC_VP9;
      break;
   case PIPE_VIDEO_FORMAT_AV1:
      stream_type = RDECODE_CODEC_AV1;
      break;
   case PIPE_VIDEO_FORMAT_JPEG:
      stream_type = RDECODE_CODEC_JPEG;
      ring = AMD_IP_VCN_JPEG;
      break;
   default:
      assert(0);
      break;
   }

   dec = CALLOC_STRUCT(radeon_decoder);

   if (!dec)
      return NULL;

   if (sctx->vcn_has_ctx) {
      dec->ectx = context->screen->context_create(context->screen, NULL, PIPE_CONTEXT_COMPUTE_ONLY);
      if (!dec->ectx)
         sctx->vcn_has_ctx = false;
   }

   dec->base = *templ;
   dec->base.context = (sctx->vcn_has_ctx) ? dec->ectx : context;
   dec->base.width = width;
   dec->base.height = height;
   dec->max_width = width;
   dec->max_height = height;
   dec->base.destroy = radeon_dec_destroy;
   dec->base.begin_frame = radeon_dec_begin_frame;
   dec->base.decode_macroblock = radeon_dec_decode_macroblock;
   dec->base.decode_bitstream = radeon_dec_decode_bitstream;
   dec->base.end_frame = radeon_dec_end_frame;
   dec->base.flush = radeon_dec_flush;
   dec->base.fence_wait = radeon_dec_fence_wait;
   dec->base.destroy_fence = radeon_dec_destroy_fence;

   dec->stream_type = stream_type;
   dec->stream_handle = si_vid_alloc_stream_handle();
   dec->screen = context->screen;
   dec->ws = ws;

   if (u_reduce_video_profile(templ->profile) != PIPE_VIDEO_FORMAT_JPEG &&
       (sctx->vcn_ip_ver >= VCN_4_0_0)) {
      dec->vcn_dec_sw_ring = true;
      ring = AMD_IP_VCN_UNIFIED;
   }

   dec->sq.signature_ib_total_size_in_dw = NULL;
   dec->sq.signature_ib_checksum = NULL;
   dec->sq.engine_ib_size_of_packages = NULL;

   if (!ws->cs_create(&dec->cs,
                      (sctx->vcn_has_ctx) ? ((struct si_context *)dec->ectx)->ctx : sctx->ctx,
                      ring, NULL, NULL)) {
      RADEON_DEC_ERR("Can't get command submission context.\n");
      goto error;
   }

   if (dec->stream_type == RDECODE_CODEC_JPEG) {

      if (((struct si_screen*)dec->screen)->info.ip[AMD_IP_VCN_JPEG].num_instances > 1 &&
          ((struct si_screen*)dec->screen)->info.ip[AMD_IP_VCN_JPEG].num_instances <= MAX_JPEG_INST)
         dec->njctx = ((struct si_screen*)dec->screen)->info.ip[AMD_IP_VCN_JPEG].num_instances;
      else
         dec->njctx = 1;

      dec->jctx = (struct radeon_winsys_ctx **) CALLOC(dec->njctx,
                                                       sizeof(struct radeon_winsys_ctx *));
      dec->jcs = (struct radeon_cmdbuf *) CALLOC(dec->njctx, sizeof(struct radeon_cmdbuf));
      if(!dec->jctx || !dec->jcs)
         goto err;
      for (i = 0; i < dec->njctx; i++) {
      /* Initialize the context handle and the command stream. */
         dec->jctx[i] = dec->ws->ctx_create(dec->ws, sctx->context_flags);
         if (!sctx->ctx)
            goto error;
         if (!dec->ws->cs_create(&dec->jcs[i], dec->jctx[i], ring, NULL, NULL)) {
            RADEON_DEC_ERR("Can't get additional command submission context for mJPEG.\n");
            goto error;
         }
      }
      dec->base.end_frame = radeon_dec_jpeg_end_frame;
      dec->cb_idx = 0;
   }

   if ((sctx->vcn_ip_ver >= VCN_3_0_0) && (stream_type == RDECODE_CODEC_H264_PERF)) {
      for (i = 0; i < ARRAY_SIZE(dec->h264_valid_ref_num); i++)
         dec->h264_valid_ref_num[i] = (unsigned) -1;
      for (i = 0; i < ARRAY_SIZE(dec->h264_valid_poc_num); i++)
         dec->h264_valid_poc_num[i] = (unsigned) -1;
   }

   if (dec->stream_type == RDECODE_CODEC_JPEG) {
      if (sctx->vcn_ip_ver == VCN_4_0_3)
         dec->num_dec_bufs = dec->njctx;
      else
         dec->num_dec_bufs = dec->njctx * NUM_BUFFERS;
   } else
      dec->num_dec_bufs = NUM_BUFFERS;

   bs_buf_size = align(width * height / 32, 128);
   dec->msg_fb_it_probs_buffers = (struct rvid_buffer *) CALLOC(dec->num_dec_bufs, sizeof(struct rvid_buffer));
   dec->bs_buffers = (struct rvid_buffer *) CALLOC(dec->num_dec_bufs, sizeof(struct rvid_buffer));
   if(!dec->msg_fb_it_probs_buffers || !dec->bs_buffers)
      goto error;

   for (i = 0; i < dec->num_dec_bufs; ++i) {
      unsigned msg_fb_it_probs_size = FB_BUFFER_OFFSET + FB_BUFFER_SIZE;
      if (have_it(dec))
         msg_fb_it_probs_size += IT_SCALING_TABLE_SIZE;
      else if (have_probs(dec))
         msg_fb_it_probs_size += (dec->stream_type == RDECODE_CODEC_VP9) ?
                                 VP9_PROBS_TABLE_SIZE :
                                 sizeof(rvcn_dec_av1_segment_fg_t);
      /* use vram to improve performance, workaround an unknown bug */
      if (!si_vid_create_buffer(dec->screen, &dec->msg_fb_it_probs_buffers[i], msg_fb_it_probs_size,
                                PIPE_USAGE_DEFAULT)) {
         RADEON_DEC_ERR("Can't allocate message buffers.\n");
         goto error;
      }

      if (!si_vid_create_buffer(dec->screen, &dec->bs_buffers[i], bs_buf_size,
                                PIPE_USAGE_STAGING)) {
         RADEON_DEC_ERR("Can't allocate bitstream buffers.\n");
         goto error;
      }

      if (have_probs(dec) && dec->stream_type == RDECODE_CODEC_VP9) {
         struct rvid_buffer *buf;
         void *ptr;

         buf = &dec->msg_fb_it_probs_buffers[i];
         ptr = dec->ws->buffer_map(dec->ws, buf->res->buf, NULL,
                                   PIPE_MAP_WRITE | RADEON_MAP_TEMPORARY);
         ptr += FB_BUFFER_OFFSET + FB_BUFFER_SIZE;
         ac_vcn_vp9_fill_probs_table(ptr);
         dec->ws->buffer_unmap(dec->ws, buf->res->buf);
         dec->bs_ptr = NULL;
      }
   }

   if (radeon_dec_enable_tier3(sctx, stream_type))
      dec->dpb_type = DPB_DYNAMIC_TIER_3;
   else if (radeon_dec_enable_tier2(sctx, stream_type))
      dec->dpb_type = DPB_DYNAMIC_TIER_2;
   else if (radeon_dec_enable_tier1(sctx, stream_type))
      dec->dpb_type = DPB_DYNAMIC_TIER_1;
   else
      dec->dpb_type = DPB_MAX_RES;

   dec->db_alignment = (sctx->vcn_ip_ver >= VCN_2_0_0 &&
                   dec->base.width > 32 && (dec->stream_type == RDECODE_CODEC_VP9 ||
                   dec->stream_type == RDECODE_CODEC_AV1 ||
                   dec->base.profile == PIPE_VIDEO_PROFILE_HEVC_MAIN_10)) ? 64 : 32;

   if (sctx->vcn_ip_ver >= VCN_5_0_0) {
      if (stream_type == RDECODE_CODEC_VP9 ||
          stream_type == RDECODE_CODEC_AV1 ||
          stream_type == RDECODE_CODEC_H265 ||
          stream_type == RDECODE_CODEC_H264_PERF)
         dec->db_alignment = 64;
   }

   if (dec->dpb_type < DPB_DYNAMIC_TIER_2)
      dec->dpb_size = calc_dpb_size(dec);

   if (!si_vid_create_buffer(dec->screen, &dec->sessionctx, RDECODE_SESSION_CONTEXT_SIZE,
                             PIPE_USAGE_DEFAULT)) {
      RADEON_DEC_ERR("Can't allocate session ctx.\n");
      goto error;
   }

   dec->addr_gfx_mode = RDECODE_ARRAY_MODE_LINEAR;
   dec->av1_version = RDECODE_AV1_VER_0;

   switch (sctx->vcn_ip_ver) {
   case VCN_1_0_0:
   case VCN_1_0_1:
      dec->reg.data0 = RDECODE_VCN1_GPCOM_VCPU_DATA0;
      dec->reg.data1 = RDECODE_VCN1_GPCOM_VCPU_DATA1;
      dec->reg.cmd = RDECODE_VCN1_GPCOM_VCPU_CMD;
      dec->reg.cntl = RDECODE_VCN1_ENGINE_CNTL;
      dec->jpg_reg.version = RDECODE_JPEG_REG_VER_V1;
      break;
   case VCN_2_0_0:
   case VCN_2_0_2:
   case VCN_2_0_3:
   case VCN_2_2_0:
      dec->reg.data0 = RDECODE_VCN2_GPCOM_VCPU_DATA0;
      dec->reg.data1 = RDECODE_VCN2_GPCOM_VCPU_DATA1;
      dec->reg.cmd = RDECODE_VCN2_GPCOM_VCPU_CMD;
      dec->reg.cntl = RDECODE_VCN2_ENGINE_CNTL;
      dec->jpg_reg.version = RDECODE_JPEG_REG_VER_V2;
      break;
   case VCN_2_5_0:
   case VCN_2_6_0:
   case VCN_3_0_0:
   case VCN_3_0_2:
   case VCN_3_0_16:
   case VCN_3_0_33:
   case VCN_3_1_1:
   case VCN_3_1_2:
      dec->reg.data0 = RDECODE_VCN2_5_GPCOM_VCPU_DATA0;
      dec->reg.data1 = RDECODE_VCN2_5_GPCOM_VCPU_DATA1;
      dec->reg.cmd = RDECODE_VCN2_5_GPCOM_VCPU_CMD;
      dec->reg.cntl = RDECODE_VCN2_5_ENGINE_CNTL;
      dec->jpg_reg.version = RDECODE_JPEG_REG_VER_V2;
      break;
   case VCN_4_0_3:
      dec->jpg_reg.version = RDECODE_JPEG_REG_VER_V3;
      dec->addr_gfx_mode = RDECODE_ARRAY_MODE_ADDRLIB_SEL_GFX9;
      dec->av1_version = RDECODE_AV1_VER_1;
      break;
   case VCN_4_0_0:
   case VCN_4_0_2:
   case VCN_4_0_4:
   case VCN_4_0_5:
   case VCN_4_0_6:
      dec->jpg_reg.version = RDECODE_JPEG_REG_VER_V2;
      dec->addr_gfx_mode = RDECODE_ARRAY_MODE_ADDRLIB_SEL_GFX11;
      dec->av1_version = RDECODE_AV1_VER_1;
      break;
   case VCN_5_0_0:
      dec->jpg_reg.version = RDECODE_JPEG_REG_VER_V3;
      dec->addr_gfx_mode = RDECODE_ARRAY_MODE_ADDRLIB_SEL_GFX11;
      dec->av1_version = RDECODE_AV1_VER_2;
      break;
   case VCN_5_0_1:
      dec->jpg_reg.version = RDECODE_JPEG_REG_VER_V3;
      dec->addr_gfx_mode = RDECODE_ARRAY_MODE_ADDRLIB_SEL_GFX9;
      dec->av1_version = RDECODE_AV1_VER_2;
      break;
   default:
      RADEON_DEC_ERR("VCN is not supported.\n");
      goto error;
   }

   if (dec->stream_type != RDECODE_CODEC_JPEG) {
      map_msg_fb_it_probs_buf(dec);
      rvcn_dec_message_create(dec);
      send_msg_buf(dec);
      r = flush(dec, 0, NULL);
      if (r)
         goto error;
   } else if (dec->jpg_reg.version != RDECODE_JPEG_REG_VER_V1) {
      dec->jpg_reg.jrbc_ib_cond_rd_timer = vcnipUVD_JRBC_IB_COND_RD_TIMER;
      dec->jpg_reg.jrbc_ib_ref_data = vcnipUVD_JRBC_IB_REF_DATA;
      dec->jpg_reg.jpeg_rb_base = vcnipUVD_JPEG_RB_BASE;
      dec->jpg_reg.jpeg_rb_size = vcnipUVD_JPEG_RB_SIZE;
      dec->jpg_reg.jpeg_rb_wptr = vcnipUVD_JPEG_RB_WPTR;
      dec->jpg_reg.jpeg_int_en = vcnipUVD_JPEG_INT_EN;
      dec->jpg_reg.jpeg_cntl = vcnipUVD_JPEG_CNTL;
      dec->jpg_reg.jpeg_rb_rptr = vcnipUVD_JPEG_RB_RPTR;
      if (dec->jpg_reg.version == RDECODE_JPEG_REG_VER_V2) {
         dec->jpg_reg.jpeg_dec_soft_rst = vcnipUVD_JPEG_DEC_SOFT_RST;
         dec->jpg_reg.lmi_jpeg_read_64bit_bar_high = vcnipUVD_LMI_JPEG_READ_64BIT_BAR_HIGH;
         dec->jpg_reg.lmi_jpeg_read_64bit_bar_low = vcnipUVD_LMI_JPEG_READ_64BIT_BAR_LOW;
         dec->jpg_reg.jpeg_pitch = vcnipUVD_JPEG_PITCH;
         dec->jpg_reg.jpeg_uv_pitch = vcnipUVD_JPEG_UV_PITCH;
         dec->jpg_reg.dec_addr_mode = vcnipJPEG_DEC_ADDR_MODE;
         dec->jpg_reg.dec_y_gfx10_tiling_surface = vcnipJPEG_DEC_Y_GFX10_TILING_SURFACE;
         dec->jpg_reg.dec_uv_gfx10_tiling_surface = vcnipJPEG_DEC_UV_GFX10_TILING_SURFACE;
         dec->jpg_reg.lmi_jpeg_write_64bit_bar_high = vcnipUVD_LMI_JPEG_WRITE_64BIT_BAR_HIGH;
         dec->jpg_reg.lmi_jpeg_write_64bit_bar_low = vcnipUVD_LMI_JPEG_WRITE_64BIT_BAR_LOW;
         dec->jpg_reg.jpeg_tier_cntl2 = vcnipUVD_JPEG_TIER_CNTL2;
         dec->jpg_reg.jpeg_outbuf_cntl = vcnipUVD_JPEG_OUTBUF_CNTL;
         dec->jpg_reg.jpeg_outbuf_rptr = vcnipUVD_JPEG_OUTBUF_RPTR;
         dec->jpg_reg.jpeg_outbuf_wptr = vcnipUVD_JPEG_OUTBUF_WPTR;
         dec->jpg_reg.jpeg_index = vcnipUVD_JPEG_INDEX;
         dec->jpg_reg.jpeg_data = vcnipUVD_JPEG_DATA;
      } else {
         dec->jpg_reg.jpeg_dec_soft_rst = vcnipUVD_JPEG_DEC_SOFT_RST_1;
         dec->jpg_reg.lmi_jpeg_read_64bit_bar_high = vcnipUVD_LMI_JPEG_READ_64BIT_BAR_HIGH_1;
         dec->jpg_reg.lmi_jpeg_read_64bit_bar_low = vcnipUVD_LMI_JPEG_READ_64BIT_BAR_LOW_1;
         dec->jpg_reg.jpeg_pitch = vcnipUVD_JPEG_PITCH_1;
         dec->jpg_reg.jpeg_uv_pitch = vcnipUVD_JPEG_UV_PITCH_1;
         dec->jpg_reg.dec_addr_mode = vcnipJPEG_DEC_ADDR_MODE_1;
         dec->jpg_reg.dec_y_gfx10_tiling_surface = vcnipJPEG_DEC_Y_GFX10_TILING_SURFACE_1;
         dec->jpg_reg.dec_uv_gfx10_tiling_surface = vcnipJPEG_DEC_UV_GFX10_TILING_SURFACE_1;
         dec->jpg_reg.lmi_jpeg_write_64bit_bar_high = vcnipUVD_LMI_JPEG_WRITE_64BIT_BAR_HIGH_1;
         dec->jpg_reg.lmi_jpeg_write_64bit_bar_low = vcnipUVD_LMI_JPEG_WRITE_64BIT_BAR_LOW_1;
         dec->jpg_reg.jpeg_tier_cntl2 = vcnipUVD_JPEG_TIER_CNTL2_1;
         dec->jpg_reg.jpeg_outbuf_cntl = vcnipUVD_JPEG_OUTBUF_CNTL_1;
         dec->jpg_reg.jpeg_outbuf_rptr = vcnipUVD_JPEG_OUTBUF_RPTR_1;
         dec->jpg_reg.jpeg_outbuf_wptr = vcnipUVD_JPEG_OUTBUF_WPTR_1;
         dec->jpg_reg.jpeg_luma_base0_0 = vcnipUVD_JPEG_LUMA_BASE0_0;
         dec->jpg_reg.jpeg_chroma_base0_0 = vcnipUVD_JPEG_CHROMA_BASE0_0;
         dec->jpg_reg.jpeg_chromav_base0_0 = vcnipUVD_JPEG_CHROMAV_BASE0_0;
      }
   }

   next_buffer(dec);

   if (stream_type == RDECODE_CODEC_JPEG)
      dec->send_cmd = send_cmd_jpeg;
   else
      dec->send_cmd = send_cmd_dec;

   list_inithead(&dec->dpb_ref_list);
   list_inithead(&dec->dpb_unref_list);

   dec->tmz_ctx = sctx->vcn_ip_ver < VCN_2_2_0 && sctx->vcn_ip_ver != VCN_UNKNOWN;

   return &dec->base;

error:
   dec->ws->cs_destroy(&dec->cs);
   if (dec->ectx)
      dec->ectx->destroy(dec->ectx);

   if (dec->stream_type == RDECODE_CODEC_JPEG) {
      for (i = 0; i < dec->njctx; i++) {
         dec->ws->cs_destroy(&dec->jcs[i]);
         dec->ws->ctx_destroy(dec->jctx[i]);
      }
   }

   if (dec->msg_fb_it_probs_buffers && dec->bs_buffers) {
      for (i = 0; i < dec->num_dec_bufs; ++i) {
            si_vid_destroy_buffer(&dec->msg_fb_it_probs_buffers[i]);
            si_vid_destroy_buffer(&dec->bs_buffers[i]);
      }
      FREE(dec->msg_fb_it_probs_buffers);
      FREE(dec->bs_buffers);
   }

   if (dec->dpb_type < DPB_DYNAMIC_TIER_2)
      si_vid_destroy_buffer(&dec->dpb);
   si_vid_destroy_buffer(&dec->ctx);
   si_vid_destroy_buffer(&dec->sessionctx);

err:
   if (dec->jcs)
      FREE(dec->jcs);
   if (dec->jctx)
      FREE(dec->jctx);
   FREE(dec);

   return NULL;
}

static enum pipe_format get_buffer_format(struct radeon_decoder *dec)
{
   switch (dec->ref_codec.bts) {
   case CODEC_10_BITS:
      return PIPE_FORMAT_P010;
   case CODEC_12_BITS:
      return PIPE_FORMAT_P012;
   default:
      return PIPE_FORMAT_NV12;
   }
}

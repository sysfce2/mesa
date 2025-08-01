/*
 * Copyright © Microsoft Corporation
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "d3d12_screen.h"
#include "d3d12_video_screen.h"
#include "d3d12_format.h"
#include "util/u_video.h"
#include <directx/d3d12video.h>
#include <cmath>

#include <wrl/client.h>
using Microsoft::WRL::ComPtr;

#include "d3d12_video_types.h"

struct d3d12_encode_codec_support {
   enum pipe_video_profile profile;
   union {
      struct {
         enum pipe_video_h264_enc_dbk_filter_mode_flags disable_dbk_filter_mode_flags;
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_H264 d3d12_caps;
      } h264_support;
      struct {
         enum pipe_h265_enc_pred_direction prediction_direction;
         union pipe_h265_enc_cap_features hevc_features;
         union pipe_h265_enc_cap_block_sizes hevc_block_sizes;
         union pipe_h265_enc_cap_range_extension hevc_range_ext;
         union pipe_h265_enc_cap_range_extension_flags hevc_range_ext_flags;
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC1 d3d12_caps;
      } hevc_support;
      struct {
         union pipe_av1_enc_cap_features features;
         union pipe_av1_enc_cap_features_ext1 features_ext1;
         union pipe_av1_enc_cap_features_ext2 features_ext2;
         D3D12_VIDEO_ENCODER_AV1_CODEC_CONFIGURATION_SUPPORT d3d12_caps;
         D3D12_VIDEO_ENCODER_CODEC_AV1_PICTURE_CONTROL_SUPPORT d3d12_picture_control;
      } av1_support;
   };
};

struct d3d12_video_resolution_to_level_mapping_entry
{
   D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC resolution;
   uint32_t level;
};

static void
get_level_resolution_video_decode_support(D3D12_VIDEO_DECODE_CONFIGURATION decoderConfig,
                                              DXGI_FORMAT format,
                                              struct pipe_screen *pscreen,
                                              bool &outSupportAny,
                                              D3D12_FEATURE_DATA_VIDEO_DECODE_SUPPORT &outMaxSupportedConfig,
                                              d3d12_video_resolution_to_level_mapping_entry &outMaxResol,
                                              d3d12_video_resolution_to_level_mapping_entry &outMinResol)
{
   outSupportAny = false;
   outMaxSupportedConfig = {};
   outMinResol = {};
   outMaxResol = {};

   ComPtr<ID3D12VideoDevice> spD3D12VideoDevice;
   struct d3d12_screen *pD3D12Screen = (struct d3d12_screen *) pscreen;
   if (FAILED(pD3D12Screen->dev->QueryInterface(IID_PPV_ARGS(spD3D12VideoDevice.GetAddressOf())))) {
      // No video support in underlying d3d12 device (decode needs ID3D12VideoDevice)
      return;
   }

   d3d12_video_resolution_to_level_mapping_entry resolutionsLevelList[] = {
      { { 8192, 4352 }, 61 },   // 8k
      { { 8192, 4320 }, 61 },   // 8k
      { { 7680, 4800 }, 61 },   // 8k - alternative
      { { 7680, 4320 }, 61 },   // 8k - alternative
      { { 4096, 2304 }, 52 },   // 2160p (4K)
      { { 4096, 2160 }, 52 },   // 2160p (4K) - alternative
      { { 2560, 1440 }, 51 },   // 1440p
      { { 1920, 1200 }, 5 },    // 1200p
      { { 1920, 1080 }, 42 },   // 1080p
      { { 1280, 720 }, 4 },     // 720p
      { { 800, 600 }, 31 },
      { { 352, 480 }, 3 },
      { { 352, 240 }, 2 },
      { { 176, 144 }, 11 },
      { { 128, 96 }, 11 },
      { { 64, 64 }, 11 },
   };

   D3D12_FEATURE_DATA_VIDEO_DECODE_SUPPORT decodeSupport = {};
   decodeSupport.Configuration = decoderConfig;
   decodeSupport.DecodeFormat = format;

   uint32_t idxResol = 0;
   while (idxResol < ARRAY_SIZE(resolutionsLevelList)) {

      decodeSupport.Width = resolutionsLevelList[idxResol].resolution.Width;
      decodeSupport.Height = resolutionsLevelList[idxResol].resolution.Height;

      if (SUCCEEDED(spD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_DECODE_SUPPORT,
                                                            &decodeSupport,
                                                            sizeof(decodeSupport)))) {
         if (((decodeSupport.SupportFlags & D3D12_VIDEO_DECODE_SUPPORT_FLAG_SUPPORTED) != 0) ||
             decodeSupport.DecodeTier > D3D12_VIDEO_DECODE_TIER_NOT_SUPPORTED) {

            // Save the first (maximum)
            if(!outSupportAny) {
               outMaxSupportedConfig = decodeSupport;
               outMaxResol = resolutionsLevelList[idxResol];
               outSupportAny = true;
            }

            // Keep saving the other supported values to get the minimum
            outMinResol = resolutionsLevelList[idxResol];
         }
      }
      idxResol++;
   }
}

static bool
d3d12_has_video_decode_support(struct pipe_screen *pscreen, enum pipe_video_profile profile)
{
   ComPtr<ID3D12VideoDevice> spD3D12VideoDevice;
   struct d3d12_screen *pD3D12Screen = (struct d3d12_screen *) pscreen;
   if (FAILED(pD3D12Screen->dev->QueryInterface(IID_PPV_ARGS(spD3D12VideoDevice.GetAddressOf())))) {
      // No video support in underlying d3d12 device (needs ID3D12VideoDevice)
      return 0;
   }

   D3D12_FEATURE_DATA_VIDEO_FEATURE_AREA_SUPPORT VideoFeatureAreaSupport = {};
   if (FAILED(spD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_FEATURE_AREA_SUPPORT,
                                                      &VideoFeatureAreaSupport,
                                                      sizeof(VideoFeatureAreaSupport)))) {
      return false;
   }

   // Supported profiles below
   bool supportsProfile = false;
#if D3D12_VIDEO_ANY_DECODER_ENABLED
   switch (profile) {
#if VIDEO_CODEC_H264DEC
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_BASELINE:
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_EXTENDED:
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_CONSTRAINED_BASELINE:
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_MAIN:
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_HIGH:
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_HIGH10:
      {
         supportsProfile = true;
      } break;
#endif
#if VIDEO_CODEC_H265DEC
      case PIPE_VIDEO_PROFILE_HEVC_MAIN:
      case PIPE_VIDEO_PROFILE_HEVC_MAIN_10:
      {
         supportsProfile = true;
      } break;
#endif
#if VIDEO_CODEC_AV1DEC
      case PIPE_VIDEO_PROFILE_AV1_MAIN:
      {
         supportsProfile = true;
      } break;
#endif
#if VIDEO_CODEC_VP9DEC
      case PIPE_VIDEO_PROFILE_VP9_PROFILE0:
      case PIPE_VIDEO_PROFILE_VP9_PROFILE2:
      {
         supportsProfile = true;
      } break;
#endif
      default:
         supportsProfile = false;
   }
#endif // D3D12_VIDEO_ANY_DECODER_ENABLED
   return VideoFeatureAreaSupport.VideoDecodeSupport && supportsProfile;
}

static bool
d3d12_video_encode_max_supported_level_for_profile(const D3D12_VIDEO_ENCODER_CODEC &argCodec,
                                                   const D3D12_VIDEO_ENCODER_PROFILE_DESC &argTargetProfile,
                                                   D3D12_VIDEO_ENCODER_LEVEL_SETTING &minLvl,
                                                   D3D12_VIDEO_ENCODER_LEVEL_SETTING &maxLvl,
                                                   ID3D12VideoDevice3 *pD3D12VideoDevice)
{
   D3D12_FEATURE_DATA_VIDEO_ENCODER_CODEC capCodecData = {};
   capCodecData.NodeIndex = 0;
   capCodecData.Codec = argCodec;
   capCodecData.IsSupported = false;

   if ((FAILED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_CODEC,
                                                     &capCodecData,
                                                     sizeof(capCodecData))))
                                                     || !capCodecData.IsSupported) {
      return false;
   }

   D3D12_FEATURE_DATA_VIDEO_ENCODER_PROFILE_LEVEL capLevelData = {};
   capLevelData.NodeIndex = 0;
   capLevelData.Codec = argCodec;
   capLevelData.Profile = argTargetProfile;
   capLevelData.MinSupportedLevel = minLvl;
   capLevelData.MaxSupportedLevel = maxLvl;

   if (FAILED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_PROFILE_LEVEL,
                                                     &capLevelData,
                                                     sizeof(capLevelData)))) {
      return false;
   }

   return capLevelData.IsSupported;
}

static bool
d3d12_video_encode_supported_resolution_range(const D3D12_VIDEO_ENCODER_CODEC &argTargetCodec,
                                              D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC &minResolution,
                                              D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC &maxResolution,
                                              union pipe_enc_cap_surface_alignment& alignment,
                                              ID3D12VideoDevice3 *pD3D12VideoDevice)
{
   D3D12_FEATURE_DATA_VIDEO_ENCODER_OUTPUT_RESOLUTION_RATIOS_COUNT capResRatiosCountData = { 0, argTargetCodec, 0 };

   if (FAILED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_OUTPUT_RESOLUTION_RATIOS_COUNT,
                                                     &capResRatiosCountData,
                                                     sizeof(capResRatiosCountData)))) {
      return false;
   }

   D3D12_FEATURE_DATA_VIDEO_ENCODER_OUTPUT_RESOLUTION capOutputResolutionData = {};
   capOutputResolutionData.NodeIndex = 0;
   capOutputResolutionData.Codec = argTargetCodec;
   capOutputResolutionData.ResolutionRatiosCount = capResRatiosCountData.ResolutionRatiosCount;

   std::vector<D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_RATIO_DESC> ratiosTmpOutput;
   if (capResRatiosCountData.ResolutionRatiosCount > 0) {
      ratiosTmpOutput.resize(capResRatiosCountData.ResolutionRatiosCount);
      capOutputResolutionData.pResolutionRatios = ratiosTmpOutput.data();
   } else {
      capOutputResolutionData.pResolutionRatios = nullptr;
   }

   if (FAILED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_OUTPUT_RESOLUTION,
                                                     &capOutputResolutionData,
                                                     sizeof(capOutputResolutionData))) ||
       !capOutputResolutionData.IsSupported) {
      return false;
   }

   minResolution = capOutputResolutionData.MinResolutionSupported;
   maxResolution = capOutputResolutionData.MaxResolutionSupported;
   alignment.bits.log2_width_alignment = static_cast<uint32_t>(std::log2(capOutputResolutionData.ResolutionWidthMultipleRequirement));
   alignment.bits.log2_height_alignment = static_cast<uint32_t>(std::log2(capOutputResolutionData.ResolutionHeightMultipleRequirement));

   return true;
}

static uint32_t
d3d12_video_encode_supported_references_per_frame_structures(const D3D12_VIDEO_ENCODER_CODEC &codec,
                                                             D3D12_VIDEO_ENCODER_PROFILE_DESC profile,
                                                             ID3D12VideoDevice3 *pD3D12VideoDevice,
                                                             struct d3d12_encode_codec_support& codecSupport,
                                                             uint32_t& maxLongTermReferences,
                                                             uint32_t& maxDPBCapacity)
{
   uint32_t supportedMaxRefFrames = 0u;
   D3D12_FEATURE_DATA_VIDEO_ENCODER_CODEC_PICTURE_CONTROL_SUPPORT capPictureControlData = {};
   capPictureControlData.NodeIndex = 0;
   capPictureControlData.Codec = codec;

   maxLongTermReferences = 0u;
   maxDPBCapacity = 0u;

   if(codec == D3D12_VIDEO_ENCODER_CODEC_H264) {
      D3D12_VIDEO_ENCODER_CODEC_PICTURE_CONTROL_SUPPORT_H264 h264PictureControl = {};
      capPictureControlData.Profile = profile;
      capPictureControlData.PictureSupport.pH264Support = &h264PictureControl;
      capPictureControlData.PictureSupport.DataSize = sizeof(h264PictureControl);
      HRESULT hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_CODEC_PICTURE_CONTROL_SUPPORT,
                                                            &capPictureControlData,
                                                            sizeof(capPictureControlData));
      if (FAILED(hr)) {
         debug_printf("CheckFeatureSupport failed with HR %x\n", hr);
      }

      if (capPictureControlData.IsSupported) {
         /* This attribute determines the maximum number of reference
         * frames supported for encoding.
         *
         * Note: for H.264 encoding, the value represents the maximum number
         * of reference frames for both the reference picture list 0 (bottom
         * 16 bits) and the reference picture list 1 (top 16 bits).
         */
         uint32_t MaxL0ReferencesForB = capPictureControlData.PictureSupport.pH264Support->MaxL0ReferencesForB;
         uint32_t maxRefForL0 = std::min(capPictureControlData.PictureSupport.pH264Support->MaxL0ReferencesForP,
                                         MaxL0ReferencesForB ? MaxL0ReferencesForB : UINT_MAX);
         uint32_t maxRefForL1 = capPictureControlData.PictureSupport.pH264Support->MaxL1ReferencesForB;

         maxLongTermReferences = capPictureControlData.PictureSupport.pH264Support->MaxLongTermReferences;

         //
         // Workaround for D3D12 Runtime validation that counts MaxDPBCapacity without the current reconpic
         //
         maxDPBCapacity = std::min(15u, capPictureControlData.PictureSupport.pH264Support->MaxDPBCapacity);
         maxRefForL0 = std::min(maxDPBCapacity, maxRefForL0);
         maxRefForL1 = std::min(maxDPBCapacity, maxRefForL1);
         maxLongTermReferences = std::min(maxDPBCapacity, maxLongTermReferences);

         supportedMaxRefFrames = (maxRefForL0 & 0xffff) | ((maxRefForL1 & 0xffff) << 16);
      }
   } else if(codec == D3D12_VIDEO_ENCODER_CODEC_HEVC) {
      D3D12_VIDEO_ENCODER_CODEC_PICTURE_CONTROL_SUPPORT_HEVC hevcPictureControl = {};
      capPictureControlData.Profile = profile;
      capPictureControlData.PictureSupport.pHEVCSupport = &hevcPictureControl;
      capPictureControlData.PictureSupport.DataSize = sizeof(hevcPictureControl);
      HRESULT hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_CODEC_PICTURE_CONTROL_SUPPORT,
                                                            &capPictureControlData,
                                                            sizeof(capPictureControlData));
      if (FAILED(hr)) {
         debug_printf("CheckFeatureSupport failed with HR %x\n", hr);
      }

      if (capPictureControlData.IsSupported) {
         /* This attribute determines the maximum number of reference
         * frames supported for encoding.
         *
         * Note: for H.265 encoding, the value represents the maximum number
         * of reference frames for both the reference picture list 0 (bottom
         * 16 bits) and the reference picture list 1 (top 16 bits).
         */
         uint32_t MaxL0ReferencesForB = capPictureControlData.PictureSupport.pHEVCSupport->MaxL0ReferencesForB;
         uint32_t maxRefForL0 = std::min(capPictureControlData.PictureSupport.pHEVCSupport->MaxL0ReferencesForP,
                                         MaxL0ReferencesForB ? MaxL0ReferencesForB : UINT_MAX);
         uint32_t maxRefForL1 = capPictureControlData.PictureSupport.pHEVCSupport->MaxL1ReferencesForB;

         maxLongTermReferences = capPictureControlData.PictureSupport.pHEVCSupport->MaxLongTermReferences;

         //
         // Workaround for D3D12 Runtime validation that counts MaxDPBCapacity without the current reconpic
         //
         maxDPBCapacity = std::min(14u, capPictureControlData.PictureSupport.pH264Support->MaxDPBCapacity);
         maxRefForL0 = std::min(maxDPBCapacity, maxRefForL0);
         maxRefForL1 = std::min(maxDPBCapacity, maxRefForL1);
         maxLongTermReferences = std::min(maxDPBCapacity, maxLongTermReferences);

         supportedMaxRefFrames = (maxRefForL0 & 0xffff) | ((maxRefForL1 & 0xffff) << 16);
      }
   }
   else if(codec == D3D12_VIDEO_ENCODER_CODEC_AV1){
      codecSupport.av1_support.d3d12_picture_control = {};
      capPictureControlData.Profile = profile;
      capPictureControlData.PictureSupport.pAV1Support = &codecSupport.av1_support.d3d12_picture_control;
      capPictureControlData.PictureSupport.DataSize = sizeof(codecSupport.av1_support.d3d12_picture_control);
      HRESULT hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_CODEC_PICTURE_CONTROL_SUPPORT,
                                                            &capPictureControlData,
                                                            sizeof(capPictureControlData));
      if (FAILED(hr)) {
         debug_printf("CheckFeatureSupport failed with HR %x\n", hr);
      }

      if (capPictureControlData.IsSupported) {
         /* This attribute determines the maximum number of reference
         * frames supported for encoding.
         */
         supportedMaxRefFrames = capPictureControlData.PictureSupport.pAV1Support->MaxUniqueReferencesPerFrame;
         if (capPictureControlData.PictureSupport.pAV1Support->PredictionMode)
            supportedMaxRefFrames = (supportedMaxRefFrames & 0xffff) | ((supportedMaxRefFrames & 0xffff) << 16);
      }
   }
   return supportedMaxRefFrames;
}

static void
d3d12_video_encode_supported_tile_structures(const D3D12_VIDEO_ENCODER_CODEC &codec,
                                             const D3D12_VIDEO_ENCODER_PROFILE_DESC &profile,
                                             const D3D12_VIDEO_ENCODER_LEVEL_SETTING &level,
                                             ID3D12VideoDevice3 *pD3D12VideoDevice,
                                             D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC maxRes,
                                             uint32_t& supportedSliceStructures, // out
                                             D3D12_VIDEO_ENCODER_AV1_FRAME_SUBREGION_LAYOUT_CONFIG_SUPPORT& av1TileSupport // out
)
{
   // Assume no support and then add as queries succeed
   supportedSliceStructures = PIPE_VIDEO_CAP_SLICE_STRUCTURE_NONE;

   // Only codecs supporting tiles should use this method. For slices related info, use d3d12_video_encode_supported_slice_structures
   assert (codec == D3D12_VIDEO_ENCODER_CODEC_AV1);
   
   D3D12_FEATURE_DATA_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_CONFIG capDataTilesSupport = { };
   capDataTilesSupport.NodeIndex = 0;
   capDataTilesSupport.Codec = codec;
   capDataTilesSupport.Profile = profile;
   capDataTilesSupport.Level = level;
   capDataTilesSupport.FrameResolution = maxRes; // Query for worst case resolution
   av1TileSupport = { };
   capDataTilesSupport.CodecSupport.DataSize = sizeof(av1TileSupport);
   capDataTilesSupport.CodecSupport.pAV1Support = &av1TileSupport;
   av1TileSupport.Use128SuperBlocks = false; // return units in 64x64 default size
   constexpr UINT superBlockSize = 64u;

   {
      capDataTilesSupport.SubregionMode = D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_UNIFORM_GRID_PARTITION;
      // Try with simple one tile request
      av1TileSupport.TilesConfiguration.ColCount = 1;
      av1TileSupport.TilesConfiguration.RowCount = 1;
      av1TileSupport.TilesConfiguration.ColWidths[0] = (capDataTilesSupport.FrameResolution.Width / superBlockSize);
      av1TileSupport.TilesConfiguration.RowHeights[0] = (capDataTilesSupport.FrameResolution.Height / superBlockSize);
      HRESULT hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_CONFIG,
                                                  &capDataTilesSupport,
                                                  sizeof(capDataTilesSupport));
      if(SUCCEEDED(hr) && !capDataTilesSupport.IsSupported) {
         // Try with minimum driver indicated params for the given resolution
         // (ie. this could happen for high resolutions like 8K where AV1 max_tile_width (4096) is exceeded)
         av1TileSupport.TilesConfiguration.ColCount = std::max(av1TileSupport.MinTileCols, 1u);
         av1TileSupport.TilesConfiguration.RowCount = std::max(av1TileSupport.MinTileRows, 1u);
         hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_CONFIG,
                                                  &capDataTilesSupport,
                                                  sizeof(capDataTilesSupport));
      }

      // Try with lower resolution as fallback
      D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC fallbackRes = { 1920u, 1080u };
      if(SUCCEEDED(hr) && !capDataTilesSupport.IsSupported && (fallbackRes.Width <= maxRes.Width)
        && (fallbackRes.Height <= maxRes.Height) ) {
         auto oldRes = capDataTilesSupport.FrameResolution;
         capDataTilesSupport.FrameResolution = fallbackRes;
         hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_CONFIG,
                                                  &capDataTilesSupport,
                                                  sizeof(capDataTilesSupport));
         capDataTilesSupport.FrameResolution = oldRes;
      }

      if(SUCCEEDED(hr) && capDataTilesSupport.IsSupported)
         supportedSliceStructures |= (PIPE_VIDEO_CAP_SLICE_STRUCTURE_POWER_OF_TWO_ROWS |
                                      PIPE_VIDEO_CAP_SLICE_STRUCTURE_EQUAL_ROWS |
                                      PIPE_VIDEO_CAP_SLICE_STRUCTURE_EQUAL_MULTI_ROWS);
   }

   {
      capDataTilesSupport.SubregionMode = D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_CONFIGURABLE_GRID_PARTITION;
      // Try with simple one tile request
      av1TileSupport.TilesConfiguration.ColCount = 1;
      av1TileSupport.TilesConfiguration.RowCount = 1;
      av1TileSupport.TilesConfiguration.ColWidths[0] = capDataTilesSupport.FrameResolution.Width;
      av1TileSupport.TilesConfiguration.RowHeights[0] = capDataTilesSupport.FrameResolution.Height;
      HRESULT hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_CONFIG,
                                                  &capDataTilesSupport,
                                                  sizeof(capDataTilesSupport));
      if(SUCCEEDED(hr) && !capDataTilesSupport.IsSupported) {
         // Try with minimum driver indicated params for the given resolution
         // (ie. this could happen for high resolutions like 8K where AV1 max_tile_width (4096) is exceeded)
         av1TileSupport.TilesConfiguration.ColCount = std::max(av1TileSupport.MinTileCols, 1u);
         av1TileSupport.TilesConfiguration.RowCount = std::max(av1TileSupport.MinTileRows, 1u);
         // Try for uniform grid tiles
         UINT tileWPixel = static_cast<UINT>(capDataTilesSupport.FrameResolution.Width / av1TileSupport.TilesConfiguration.ColCount);
         UINT tileHPixel = static_cast<UINT>(capDataTilesSupport.FrameResolution.Height / av1TileSupport.TilesConfiguration.RowCount);
         for (UINT i = 0; i < av1TileSupport.TilesConfiguration.ColCount; i++)
            av1TileSupport.TilesConfiguration.ColWidths[i] = (tileWPixel / superBlockSize);
         for (UINT j = 0; j < av1TileSupport.TilesConfiguration.RowCount; j++)
            av1TileSupport.TilesConfiguration.RowHeights[j] = (tileHPixel / superBlockSize);

         hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_CONFIG,
                                                  &capDataTilesSupport,
                                                  sizeof(capDataTilesSupport));
      }

      if(SUCCEEDED(hr) && capDataTilesSupport.IsSupported)
         supportedSliceStructures |= (PIPE_VIDEO_CAP_SLICE_STRUCTURE_ARBITRARY_MACROBLOCKS |
                                      PIPE_VIDEO_CAP_SLICE_STRUCTURE_ARBITRARY_ROWS);
   }
}

static uint32_t
d3d12_video_encode_supported_slice_structures(const D3D12_VIDEO_ENCODER_CODEC &codec,
                                              D3D12_VIDEO_ENCODER_PROFILE_DESC profile,
                                              D3D12_VIDEO_ENCODER_LEVEL_SETTING level,
                                              ID3D12VideoDevice3 *pD3D12VideoDevice)
{
   // Only codecs supporting slices should use this method. For tile related info, use d3d12_video_encode_supported_tile_structures
   assert ((codec == D3D12_VIDEO_ENCODER_CODEC_H264) || (codec == D3D12_VIDEO_ENCODER_CODEC_HEVC));

   uint32_t supportedSliceStructuresBitMask = PIPE_VIDEO_CAP_SLICE_STRUCTURE_NONE;

   D3D12_FEATURE_DATA_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE capDataSubregionLayout = {};
   capDataSubregionLayout.NodeIndex = 0;
   capDataSubregionLayout.Codec = codec;
   capDataSubregionLayout.Profile = profile;
   capDataSubregionLayout.Level = level;

   /**
    * pipe_video_cap_slice_structure
    *
    * This attribute determines slice structures supported by the
    * driver for encoding. This attribute is a hint to the user so
    * that he can choose a suitable surface size and how to arrange
    * the encoding process of multiple slices per frame.
    *
    * More specifically, for H.264 encoding, this attribute
    * determines the range of accepted values to
    * h264_slice_descriptor::macroblock_address and
    * h264_slice_descriptor::num_macroblocks.
    *
    * For HEVC, similarly determines the ranges for
    * slice_segment_address
    * num_ctu_in_slice
    */
   capDataSubregionLayout.SubregionMode =
      D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_UNIFORM_PARTITIONING_SUBREGIONS_PER_FRAME;
   HRESULT hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE,
                                                       &capDataSubregionLayout,
                                                       sizeof(capDataSubregionLayout));
   if (FAILED(hr)) {
      debug_printf("CheckFeatureSupport failed with HR %x\n", hr);
   } else if (capDataSubregionLayout.IsSupported) {
      /* This would be setting N subregions per frame in this D3D12 mode where N = (height/blocksize) / K */
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_EQUAL_MULTI_ROWS;
      /* Assuming height/blocksize >= max_supported_slices, which is reported
      in PIPE_VIDEO_CAP_ENC_MAX_SLICES_PER_FRAME and should be checked by the client*/
      /* This would be setting N subregions per frame in this D3D12 mode where N = (height/blocksize) */
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_EQUAL_ROWS;
      /* This is ok, would be setting K rows per subregions in this D3D12 mode (and rounding the last one) */
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_POWER_OF_TWO_ROWS;
   }

   capDataSubregionLayout.SubregionMode =
      D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_UNIFORM_PARTITIONING_ROWS_PER_SUBREGION;
   hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE,
                                                         &capDataSubregionLayout,
                                                         sizeof(capDataSubregionLayout));
   if (FAILED(hr)) {
      debug_printf("CheckFeatureSupport failed with HR %x\n", hr);
   } else if (capDataSubregionLayout.IsSupported) {
      /* This would be setting K rows per subregions in this D3D12 mode */
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_EQUAL_MULTI_ROWS;
      /* Assuming height/blocksize >= max_supported_slices, which is reported
      in PIPE_VIDEO_CAP_ENC_MAX_SLICES_PER_FRAME and should be checked by the client*/
      /* This would be setting 1 row per subregion in this D3D12 mode */
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_EQUAL_ROWS;
      /* This is ok, would be setting K rows per subregions in this D3D12 mode (and rounding the last one) */
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_POWER_OF_TWO_ROWS;
   }

   capDataSubregionLayout.SubregionMode =
      D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_SQUARE_UNITS_PER_SUBREGION_ROW_UNALIGNED;
   hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE,
                                                         &capDataSubregionLayout,
                                                         sizeof(capDataSubregionLayout));
   if (FAILED(hr)) {
      debug_printf("CheckFeatureSupport failed with HR %x\n", hr);
   } else if (capDataSubregionLayout.IsSupported) {
      /* This would be setting K rows per subregions in this D3D12 mode */
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_EQUAL_MULTI_ROWS;
      /* Assuming height/blocksize >= max_supported_slices, which is reported
      in PIPE_VIDEO_CAP_ENC_MAX_SLICES_PER_FRAME and should be checked by the client*/
      /* This would be setting 1 row per subregion in this D3D12 mode */
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_EQUAL_ROWS;
      /* This is ok, would be setting K rows per subregions in this D3D12 mode (and rounding the last one) */
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_POWER_OF_TWO_ROWS;
      /* This is ok, would be setting K MBs per subregions in this D3D12 mode*/
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_ARBITRARY_MACROBLOCKS;
   }

   capDataSubregionLayout.SubregionMode = D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_BYTES_PER_SUBREGION;
   hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE,
                                                         &capDataSubregionLayout,
                                                         sizeof(capDataSubregionLayout));
   if (FAILED(hr)) {
      debug_printf("CheckFeatureSupport failed with HR %x\n", hr);
   } else if (capDataSubregionLayout.IsSupported) {
      supportedSliceStructuresBitMask |= PIPE_VIDEO_CAP_SLICE_STRUCTURE_MAX_SLICE_SIZE;
   }

   return supportedSliceStructuresBitMask;
}

/*
   All these structures must be present in memory (stack scope) when calling
   CheckFeatureSupport and for any subsequent read from d3d12_video_encode_support_caps
   capEncoderSupportData1 in/out parameter
*/
struct d3d12_encode_support_cap_allocations
{
   D3D12_VIDEO_ENCODER_RATE_CONTROL_CQP rcCqp = { 25, 25, 25 };
   D3D12_VIDEO_ENCODER_PROFILE_H264 h264prof = {};
   D3D12_VIDEO_ENCODER_LEVELS_H264 h264lvl = {};
   D3D12_VIDEO_ENCODER_SEQUENCE_GOP_STRUCTURE_H264 h264Gop = { 1, 0, 0, 0, 0 };
   D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_H264 h264Config = {};
   D3D12_VIDEO_ENCODER_PROFILE_HEVC hevcprof = { };
   D3D12_VIDEO_ENCODER_LEVEL_TIER_CONSTRAINTS_HEVC hevcLvl = { };
   D3D12_VIDEO_ENCODER_SEQUENCE_GOP_STRUCTURE_HEVC hevcGop = { 1, 0, 0 };
   D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC hevcConfig = {};
   D3D12_VIDEO_ENCODER_AV1_PROFILE av1prof = { };
   D3D12_VIDEO_ENCODER_AV1_LEVEL_TIER_CONSTRAINTS av1Lvl = { };
   D3D12_VIDEO_ENCODER_AV1_SEQUENCE_STRUCTURE av1Gop = { 1, 0 };
   D3D12_VIDEO_ENCODER_AV1_CODEC_CONFIGURATION av1Config = {};
};


static bool
d3d12_video_encode_support_caps(const D3D12_VIDEO_ENCODER_CODEC &argTargetCodec,
                                D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC maxResolution,
                                DXGI_FORMAT encodeFormat,
                                ID3D12VideoDevice3 *pD3D12VideoDevice,
                                D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT codecSupport,
#if D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE
                                D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT2 &capEncoderSupportData1,
                                D3D12_FEATURE_DATA_VIDEO_ENCODER_RESOLUTION_SUPPORT_LIMITS1 &resolutionDepCaps,
#else
                                D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1 &capEncoderSupportData1,
                                D3D12_FEATURE_DATA_VIDEO_ENCODER_RESOLUTION_SUPPORT_LIMITS &resolutionDepCaps,
#endif
                                uint32_t &maxQualityLevels,
                                struct d3d12_encode_support_cap_allocations &cap_allocations)
{
   capEncoderSupportData1.NodeIndex = 0;
   capEncoderSupportData1.Codec = argTargetCodec;
   capEncoderSupportData1.InputFormat = encodeFormat;
   capEncoderSupportData1.RateControl = {};
   capEncoderSupportData1.RateControl.Mode = D3D12_VIDEO_ENCODER_RATE_CONTROL_MODE_CQP;
   capEncoderSupportData1.RateControl.TargetFrameRate.Numerator = 60;
   capEncoderSupportData1.RateControl.TargetFrameRate.Denominator = 1;
   capEncoderSupportData1.RateControl.ConfigParams.pConfiguration_CQP = &cap_allocations.rcCqp;
   capEncoderSupportData1.RateControl.ConfigParams.DataSize = sizeof(cap_allocations.rcCqp);
   capEncoderSupportData1.IntraRefresh = D3D12_VIDEO_ENCODER_INTRA_REFRESH_MODE_NONE;
   capEncoderSupportData1.ResolutionsListCount = 1;
   capEncoderSupportData1.pResolutionList = &maxResolution;
   capEncoderSupportData1.MaxReferenceFramesInDPB = 1;
   switch (argTargetCodec) {
      case D3D12_VIDEO_ENCODER_CODEC_H264:
      {
         // assert(codecSupport.pH264Support); // Fill this in caller if ever used
         capEncoderSupportData1.SuggestedProfile.pH264Profile = &cap_allocations.h264prof;
         capEncoderSupportData1.SuggestedProfile.DataSize = sizeof(cap_allocations.h264prof);
         capEncoderSupportData1.SuggestedLevel.pH264LevelSetting = &cap_allocations.h264lvl;
         capEncoderSupportData1.SuggestedLevel.DataSize = sizeof(cap_allocations.h264lvl);
         capEncoderSupportData1.CodecGopSequence.pH264GroupOfPictures = &cap_allocations.h264Gop;
         capEncoderSupportData1.CodecGopSequence.DataSize = sizeof(cap_allocations.h264Gop);
         capEncoderSupportData1.CodecConfiguration.DataSize = sizeof(cap_allocations.h264Config);
         capEncoderSupportData1.CodecConfiguration.pH264Config = &cap_allocations.h264Config;
      } break;

      case D3D12_VIDEO_ENCODER_CODEC_HEVC:
      {
         /* Only read from codecSupport.pHEVCSupport in this case (union of pointers definition) */
         assert(codecSupport.pHEVCSupport);
         cap_allocations.hevcConfig = {
            D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_FLAG_DISABLE_LOOP_FILTER_ACROSS_SLICES,
            codecSupport.pHEVCSupport->MinLumaCodingUnitSize,
            codecSupport.pHEVCSupport->MaxLumaCodingUnitSize,
            codecSupport.pHEVCSupport->MinLumaTransformUnitSize,
            codecSupport.pHEVCSupport->MaxLumaTransformUnitSize,
            codecSupport.pHEVCSupport->max_transform_hierarchy_depth_inter,
            codecSupport.pHEVCSupport->max_transform_hierarchy_depth_intra,
         };

         if ((codecSupport.pHEVCSupport->SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_ASYMETRIC_MOTION_PARTITION_REQUIRED) != 0)
            cap_allocations.hevcConfig.ConfigurationFlags |= D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_FLAG_USE_ASYMETRIC_MOTION_PARTITION;

         capEncoderSupportData1.SuggestedProfile.pHEVCProfile = &cap_allocations.hevcprof;
         capEncoderSupportData1.SuggestedProfile.DataSize = sizeof(cap_allocations.hevcprof);
         capEncoderSupportData1.SuggestedLevel.pHEVCLevelSetting = &cap_allocations.hevcLvl;
         capEncoderSupportData1.SuggestedLevel.DataSize = sizeof(cap_allocations.hevcLvl);
         capEncoderSupportData1.CodecGopSequence.pHEVCGroupOfPictures = &cap_allocations.hevcGop;
         capEncoderSupportData1.CodecGopSequence.DataSize = sizeof(cap_allocations.hevcGop);
         capEncoderSupportData1.CodecConfiguration.DataSize = sizeof(cap_allocations.hevcConfig);
         capEncoderSupportData1.CodecConfiguration.pHEVCConfig = &cap_allocations.hevcConfig;
      } break;

      case D3D12_VIDEO_ENCODER_CODEC_AV1:
      {
         capEncoderSupportData1.SuggestedProfile.pAV1Profile = &cap_allocations.av1prof;
         capEncoderSupportData1.SuggestedProfile.DataSize = sizeof(cap_allocations.av1prof);
         capEncoderSupportData1.SuggestedLevel.pAV1LevelSetting = &cap_allocations.av1Lvl;
         capEncoderSupportData1.SuggestedLevel.DataSize = sizeof(cap_allocations.av1Lvl);
         capEncoderSupportData1.CodecGopSequence.pAV1SequenceStructure = &cap_allocations.av1Gop;
         capEncoderSupportData1.CodecGopSequence.DataSize = sizeof(cap_allocations.av1Gop);
         D3D12_FEATURE_DATA_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT capCodecConfigData = { };
         capCodecConfigData.NodeIndex = 0;
         capCodecConfigData.Codec = D3D12_VIDEO_ENCODER_CODEC_AV1;
         capCodecConfigData.Profile.pAV1Profile = &cap_allocations.av1prof;
         capCodecConfigData.Profile.DataSize = sizeof(cap_allocations.av1prof);
         D3D12_VIDEO_ENCODER_AV1_CODEC_CONFIGURATION_SUPPORT av1CodecSupport = { };
         capCodecConfigData.CodecSupportLimits.pAV1Support = &av1CodecSupport;
         capCodecConfigData.CodecSupportLimits.DataSize = sizeof(av1CodecSupport);
         HRESULT hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT, &capCodecConfigData, sizeof(capCodecConfigData));
         if (FAILED(hr)) {
            debug_printf("CheckFeatureSupport D3D12_FEATURE_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT failed with HR %x\n", hr);
            return false;
         } else if (!capCodecConfigData.IsSupported) {
            debug_printf("CheckFeatureSupport D3D12_FEATURE_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT IsSupported is false\n");
            return false;
         }
         cap_allocations.av1Config.OrderHintBitsMinus1 = 7;
         cap_allocations.av1Config.FeatureFlags = av1CodecSupport.RequiredFeatureFlags;
         capEncoderSupportData1.CodecConfiguration.DataSize = sizeof(cap_allocations.av1Config);
         capEncoderSupportData1.CodecConfiguration.pAV1Config = &cap_allocations.av1Config;
      } break;
      default:
      {
         UNREACHABLE("Unsupported D3D12_VIDEO_ENCODER_CODEC");
      } break;
   }

   // prepare inout storage for the resolution dependent result.
   resolutionDepCaps = {};
   capEncoderSupportData1.pResolutionDependentSupport = &resolutionDepCaps;

#if D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE
   HRESULT hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_SUPPORT2,
                                                       &capEncoderSupportData1,
                                                       sizeof(capEncoderSupportData1));

   if (FAILED(hr)) {
      debug_printf("CheckFeatureSupport D3D12_FEATURE_VIDEO_ENCODER_SUPPORT2 failed with HR %x\n", hr);
      debug_printf("Falling back to check previous query version D3D12_FEATURE_VIDEO_ENCODER_SUPPORT1...\n");

      // D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT2 extends D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1
      // in a binary compatible way, so just cast it and try with the older query D3D12_FEATURE_VIDEO_ENCODER_SUPPORT1
      D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1 * casted_down_cap_data = reinterpret_cast<D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1*>(&capEncoderSupportData1);
      hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_SUPPORT1,
                                                  casted_down_cap_data,
                                                  sizeof(D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1));
   }

#else
   HRESULT hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_SUPPORT1,
                                                       &capEncoderSupportData1,
                                                       sizeof(capEncoderSupportData1));
#endif

   if (FAILED(hr)) {
      debug_printf("CheckFeatureSupport D3D12_FEATURE_VIDEO_ENCODER_SUPPORT1 failed with HR %x\n", hr);
      debug_printf("Falling back to check previous query version D3D12_FEATURE_VIDEO_ENCODER_SUPPORT...\n");

      // D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1 extends D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT
      // in a binary compatible way, so just cast it and try with the older query D3D12_FEATURE_VIDEO_ENCODER_SUPPORT
      D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT * casted_down_cap_data = reinterpret_cast<D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT*>(&capEncoderSupportData1);
      hr = pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_SUPPORT,
                                                                         casted_down_cap_data,
                                                                         sizeof(D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT));
      if (FAILED(hr)) {
         debug_printf("CheckFeatureSupport D3D12_FEATURE_VIDEO_ENCODER_SUPPORT failed with HR %x\n", hr);
         return false;
      }
   }

   // Convert between D3D12 definition and PIPE definition

   // D3D12: QualityVsSpeed must be in the range [0, D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1.MaxQualityVsSpeed]
   // The lower the value, the fastest the encode operation

   // PIPE: The quality level range can be queried through the VAConfigAttribEncQualityRange attribute. 
   // A lower value means higher quality, and a value of 1 represents the highest quality. 
   // The quality level setting is used as a trade-off between quality and speed/power 
   // consumption, with higher quality corresponds to lower speed and higher power consumption.
   maxQualityLevels = capEncoderSupportData1.MaxQualityVsSpeed + 1; // VA range starts from 1, D3D12 starts from 0

   bool configSupported =
      (((capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_GENERAL_SUPPORT_OK) != 0) &&
         (capEncoderSupportData1.ValidationFlags == D3D12_VIDEO_ENCODER_VALIDATION_FLAG_NONE));

   return configSupported;
}

bool
static d3d12_video_encode_get_av1_codec_support ( const D3D12_VIDEO_ENCODER_CODEC &argCodec,
                                                   const D3D12_VIDEO_ENCODER_PROFILE_DESC &argTargetProfile,
                                                   ID3D12VideoDevice3 *pD3D12VideoDevice,
                                                   D3D12_VIDEO_ENCODER_AV1_CODEC_CONFIGURATION_SUPPORT &av1Support)
{
   D3D12_FEATURE_DATA_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT capCodecConfigData = { };
   capCodecConfigData.NodeIndex = 0;
   capCodecConfigData.Codec = D3D12_VIDEO_ENCODER_CODEC_AV1;
   capCodecConfigData.Profile = argTargetProfile;
   capCodecConfigData.CodecSupportLimits.pAV1Support = &av1Support;
   capCodecConfigData.CodecSupportLimits.DataSize = sizeof(D3D12_VIDEO_ENCODER_AV1_CODEC_CONFIGURATION_SUPPORT);
   if(SUCCEEDED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT, &capCodecConfigData, sizeof(capCodecConfigData)))
      && capCodecConfigData.IsSupported) {
         return true;
   }

   memset(&av1Support, 0, sizeof(D3D12_VIDEO_ENCODER_AV1_CODEC_CONFIGURATION_SUPPORT));
   return false;
}

bool
static d3d12_video_encode_get_h264_codec_support(const D3D12_VIDEO_ENCODER_PROFILE_DESC &argTargetProfile,
                                                 ID3D12VideoDevice3 *pD3D12VideoDevice,
                                                 D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_H264 &supportedCaps)
{
   D3D12_FEATURE_DATA_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT capCodecConfigData = { };
   capCodecConfigData.NodeIndex = 0;
   capCodecConfigData.Codec = D3D12_VIDEO_ENCODER_CODEC_H264;
   capCodecConfigData.Profile = argTargetProfile;
   capCodecConfigData.CodecSupportLimits.pH264Support = &supportedCaps;
   capCodecConfigData.CodecSupportLimits.DataSize = sizeof(supportedCaps);

   if(FAILED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT, &capCodecConfigData, sizeof(capCodecConfigData)))
      || !capCodecConfigData.IsSupported)
   {
         debug_printf("D3D12_FEATURE_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT call failed.");
         return false;
   }

   return true;
}

#if VIDEO_CODEC_H265ENC
bool
static d3d12_video_encode_get_hevc_codec_support ( const D3D12_VIDEO_ENCODER_CODEC &argCodec,
                                                   const D3D12_VIDEO_ENCODER_PROFILE_DESC &argTargetProfile,
                                                   ID3D12VideoDevice3 *pD3D12VideoDevice,
                                                   D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC1 &supportedCaps)
{
   constexpr unsigned c_hevcConfigurationSets = 6u;
   const D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC1 hevcConfigurationSets[c_hevcConfigurationSets] =
   {
      {
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_NONE,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_16x16,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_32x32,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_4x4,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_32x32,
         3u,
         3u,
      },
      {
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_NONE,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_8x8,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_32x32,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_4x4,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_32x32,
         3u,
         3u,
      },
      {
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_NONE,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_8x8,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_32x32,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_4x4,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_32x32,            
         0u,            
         0u,
      },
      {
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_NONE,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_8x8,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_32x32,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_4x4,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_32x32,            
         2u,            
         2u,
      },
      {
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_NONE,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_8x8,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_64x64,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_4x4,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_32x32,            
         2u,            
         2u,
      },
      {
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_NONE,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_8x8,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_CUSIZE_64x64,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_4x4,
         D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_TUSIZE_32x32,
         4u,
         4u,
      },
   };

   D3D12_FEATURE_DATA_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT capCodecConfigData = { };
   capCodecConfigData.NodeIndex = 0;
   capCodecConfigData.Codec = D3D12_VIDEO_ENCODER_CODEC_HEVC;
   capCodecConfigData.Profile = argTargetProfile;

   for (uint32_t i = 0 ; i < c_hevcConfigurationSets ; i++) {
      supportedCaps = hevcConfigurationSets[i];
      capCodecConfigData.CodecSupportLimits = ConvertHEVCSupportFromProfile((*capCodecConfigData.Profile.pHEVCProfile), &supportedCaps);
      if(SUCCEEDED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT, &capCodecConfigData, sizeof(capCodecConfigData)))
         && capCodecConfigData.IsSupported) {
            return true;
      }
   }

   memset(&supportedCaps, 0, sizeof(D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC));
   return false;
}
#endif // VIDEO_CODEC_H265ENC

static bool
d3d12_has_video_process_support(struct pipe_screen *pscreen,
                                D3D12_FEATURE_DATA_VIDEO_PROCESS_SUPPORT &supportCaps,
                                D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC& outMinSupportedInput,
                                D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC& outMaxSupportedInput)
{
   ComPtr<ID3D12VideoDevice> spD3D12VideoDevice;
   struct d3d12_screen *pD3D12Screen = (struct d3d12_screen *) pscreen;
   if (FAILED(pD3D12Screen->dev->QueryInterface(IID_PPV_ARGS(spD3D12VideoDevice.GetAddressOf())))) {
      // No video process support in underlying d3d12 device (needs ID3D12VideoDevice)
      return false;
   }

   D3D12_FEATURE_DATA_VIDEO_FEATURE_AREA_SUPPORT VideoFeatureAreaSupport = {};
   if (FAILED(spD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_FEATURE_AREA_SUPPORT,
                                                      &VideoFeatureAreaSupport,
                                                      sizeof(VideoFeatureAreaSupport)))) {
      return false;
   }

   D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC resolutionsList[] = {
      { 8192, 8192 },   // 8k
      { 8192, 4320 },   // 8k - alternative
      { 7680, 4800 },   // 8k - alternative
      { 7680, 4320 },   // 8k - alternative
      { 4096, 2304 },   // 2160p (4K)
      { 4096, 2160 },   // 2160p (4K) - alternative
      { 2560, 1440 },   // 1440p
      { 1920, 1200 },   // 1200p
      { 1920, 1080 },   // 1080p
      { 1280, 720 },    // 720p
      { 800, 600 },
      { 352, 480 },
      { 352, 240 },
      { 176, 144 },
      { 128, 128 },
      { 96, 96 },
      { 64, 64 },
      { 32, 32 },
      { 16, 16 },
      { 8, 8 },
      { 4, 4 },
      { 2, 2 },
      { 1, 1 },
   };

   outMinSupportedInput = {};
   outMaxSupportedInput = {};
   uint32_t idxResol = 0;
   bool bSupportsAny = false;
   while (idxResol < ARRAY_SIZE(resolutionsList)) {
      supportCaps.InputSample.Width = resolutionsList[idxResol].Width;
      supportCaps.InputSample.Height = resolutionsList[idxResol].Height;
      if (SUCCEEDED(spD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_PROCESS_SUPPORT, &supportCaps, sizeof(supportCaps)))) {
         if ((supportCaps.SupportFlags & D3D12_VIDEO_PROCESS_SUPPORT_FLAG_SUPPORTED) != 0)
         {
            // Save the first (maximum)
            if(!bSupportsAny) {
               outMaxSupportedInput = resolutionsList[idxResol];
               bSupportsAny = true;
            }

            // Keep saving the other supported values to get the minimum
            outMinSupportedInput = resolutionsList[idxResol];
         }
      }
      idxResol++;
   }

   return VideoFeatureAreaSupport.VideoProcessSupport && bSupportsAny;
}

#if D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE

static
union pipe_enc_cap_two_pass
query_two_pass_support(struct pipe_screen *pscreen,
                       D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO sessionInfo,
                       ID3D12VideoDevice3* pD3D12VideoDevice,
                       bool bDriverSupportsReadableDPBReconPic)
{
   union pipe_enc_cap_two_pass two_pass_support = {};
   D3D12_FEATURE_DATA_VIDEO_ENCODER_RATE_CONTROL_FRAME_ANALYSIS twoPassCapData =
   {
      // UINT NodeIndex;
      0u,
      //D3D12_VIDEO_ENCODER_CODEC Codec;
      sessionInfo.Codec,
      //D3D12_VIDEO_ENCODER_PROFILE_DESC Profile;
      sessionInfo.Profile,
      //D3D12_VIDEO_ENCODER_LEVEL_SETTING Level;
      sessionInfo.Level,
      //DXGI_FORMAT InputFormat;
      sessionInfo.InputFormat,
      //D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC InputResolution;
      sessionInfo.InputResolution,
      //D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION CodecConfiguration;
      sessionInfo.CodecConfiguration,
      //D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE SubregionFrameEncoding;
      sessionInfo.SubregionFrameEncoding,
      //D3D12_VIDEO_ENCODER_PICTURE_CONTROL_SUBREGIONS_LAYOUT_DATA SubregionFrameEncodingData;
      sessionInfo.SubregionFrameEncodingData,
      //D3D12_VIDEO_ENCODER_QPMAP_CONFIGURATION QPMap;
      {
         //BOOL Enabled;
         FALSE,
         //D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE MapSource;
         D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_CPU_BUFFER,
      },
      //D3D12_VIDEO_ENCODER_DIRTY_REGIONS_CONFIGURATION DirtyRegions;
      {
         //BOOL Enabled;
         FALSE,
         //D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE MapSource;
         D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_CPU_BUFFER,
         //D3D12_VIDEO_ENCODER_DIRTY_REGIONS_MAP_VALUES_MODE MapValuesType;
         D3D12_VIDEO_ENCODER_DIRTY_REGIONS_MAP_VALUES_MODE_DIRTY,
      },
      //D3D12_VIDEO_ENCODER_MOTION_SEARCH_CONFIGURATION MotionSearch;
      {
         //BOOL Enabled;
         FALSE,
         //D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE MapSource;
         D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_CPU_BUFFER,
         //D3D12_VIDEO_ENCODER_FRAME_MOTION_SEARCH_MODE MotionSearchMode;
         D3D12_VIDEO_ENCODER_FRAME_MOTION_SEARCH_MODE_FULL_SEARCH,
         //BOOL BidirectionalRefFrameEnabled;
         FALSE,
      },
      //UINT Pow2DownscaleFactor;
      0u,
      //D3D12_VIDEO_ENCODER_RATE_CONTROL_FRAME_ANALYSIS_SUPPORT_FLAGS SupportFlags;
      D3D12_VIDEO_ENCODER_RATE_CONTROL_FRAME_ANALYSIS_SUPPORT_FLAG_NONE,
   };

   //
   // Find out if the driver supports D3D12_VIDEO_SCALE_SUPPORT_FLAG_DPB_ENCODER_RESOURCES
   //

   D3D12_FEATURE_DATA_VIDEO_PROCESS_SUPPORT vpblitCapsData =
   {
      0, // NodeIndex
      {
         sessionInfo.InputResolution.Width,
         sessionInfo.InputResolution.Height,
         {
            sessionInfo.InputFormat,
            DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709
         }
      },
      D3D12_VIDEO_FIELD_TYPE_NONE,
      D3D12_VIDEO_FRAME_STEREO_FORMAT_NONE,
      {
         30,
         1
      },
      {
         sessionInfo.InputFormat,
         DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709
      },
      D3D12_VIDEO_FRAME_STEREO_FORMAT_NONE,
      {
         30,
         1
      },
   };

   bool bDriverSupportsOpaqueDPBReconScaling = false;
   D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC minSupportedInput = {};
   D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC maxSupportedInput = {};
   if (d3d12_has_video_process_support(pscreen, vpblitCapsData, minSupportedInput, maxSupportedInput) &&
       (sessionInfo.InputResolution.Width <= maxSupportedInput.Width) &&
       (sessionInfo.InputResolution.Height <= maxSupportedInput.Height))
   {
      bDriverSupportsOpaqueDPBReconScaling = (vpblitCapsData.ScaleSupport.Flags & D3D12_VIDEO_SCALE_SUPPORT_FLAG_DPB_ENCODER_RESOURCES) != 0;
   }

   //
   // Find the min/max supported Pow2DownscaleFactor values.
   //

   constexpr unsigned c_MaxPow2DownscaleToTry = 3u;
   // set a value bigger than c_MaxPow2DownscaleToTry and find the min in the loop below using std::min
   two_pass_support.bits.min_pow2_downscale_factor = c_MaxPow2DownscaleToTry + 1u;

   for (unsigned CurPow2DownscaleFactor = 0; CurPow2DownscaleFactor <= c_MaxPow2DownscaleToTry; CurPow2DownscaleFactor++)
   {
      //
      // Query the cap with CurPow2DownscaleFactor
      //

      twoPassCapData.Pow2DownscaleFactor = CurPow2DownscaleFactor;

      if ((SUCCEEDED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_RATE_CONTROL_FRAME_ANALYSIS, &twoPassCapData, sizeof(twoPassCapData)))) &&
          (twoPassCapData.SupportFlags & D3D12_VIDEO_ENCODER_RATE_CONTROL_FRAME_ANALYSIS_SUPPORT_FLAG_SUPPORTED))
      {
         two_pass_support.bits.supports_two_pass = 1u;
         two_pass_support.bits.min_pow2_downscale_factor = std::min(two_pass_support.bits.min_pow2_downscale_factor, twoPassCapData.Pow2DownscaleFactor);
         two_pass_support.bits.max_pow2_downscale_factor = std::max(two_pass_support.bits.min_pow2_downscale_factor, twoPassCapData.Pow2DownscaleFactor);
         two_pass_support.bits.supports_dynamic_1st_pass_skip = (twoPassCapData.SupportFlags & D3D12_VIDEO_ENCODER_RATE_CONTROL_FRAME_ANALYSIS_SUPPORT_FLAG_DYNAMIC_1ST_PASS_SKIP) ? 1u : 0u;

         if (twoPassCapData.SupportFlags & D3D12_VIDEO_ENCODER_RATE_CONTROL_FRAME_ANALYSIS_SUPPORT_FLAG_EXTERNAL_DPB_DOWNSCALING)
         {
            //
            // For external DPB recon downscaling, one of these must be true:
            // 1. The DPB pictures are non-opaque layout (e.g without the reference only resource flag)
            // 2. The DPB pictures are opaque layout, but the driver also supports
            // D3D12_VIDEO_SCALE_SUPPORT_FLAG_DPB_ENCODER_RESOURCES (in the VPblit caps)
            //
            if (bDriverSupportsReadableDPBReconPic)
            {
               two_pass_support.bits.supports_1pass_recon_writing_skip = 1u;
            }
            else
            {
               two_pass_support.bits.supports_1pass_recon_writing_skip = bDriverSupportsOpaqueDPBReconScaling ? 1u : 0u;
            }
         }
      }
   }

   if (!two_pass_support.bits.supports_two_pass)
   {
      two_pass_support = {}; // Clean min_pow2_downscale_factor
   }

   return two_pass_support;
}

static
union pipe_enc_cap_dirty_info
query_dirty_rects_support(D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO sessionInfo,
                          ID3D12VideoDevice3* pD3D12VideoDevice,
                          D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE mapSource)
{
   D3D12_FEATURE_DATA_VIDEO_ENCODER_DIRTY_REGIONS capDirtyRegions =
   {
      // UINT NodeIndex;
      0u,
      sessionInfo,
      // D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE MapSource;
      mapSource,
      // D3D12_VIDEO_ENCODER_DIRTY_REGIONS_MAP_VALUES_MODE MapValuesType;
      D3D12_VIDEO_ENCODER_DIRTY_REGIONS_MAP_VALUES_MODE_DIRTY,
      // D3D12_VIDEO_ENCODER_DIRTY_REGIONS_SUPPORT_FLAGS SupportFlags;
      D3D12_VIDEO_ENCODER_DIRTY_REGIONS_SUPPORT_FLAG_NONE,
      // UINT MapSourcePreferenceRanking;
      0u
   };

   union pipe_enc_cap_dirty_info dirty_rects_support = {};
   if (SUCCEEDED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_DIRTY_REGIONS, &capDirtyRegions, sizeof(capDirtyRegions))))
   {
      dirty_rects_support.bits.supports_full_frame_skip = (capDirtyRegions.SupportFlags & D3D12_VIDEO_ENCODER_DIRTY_REGIONS_SUPPORT_FLAG_REPEAT_FRAME) ? 1u : 0u;
      dirty_rects_support.bits.supports_info_type_dirty = (capDirtyRegions.SupportFlags & D3D12_VIDEO_ENCODER_DIRTY_REGIONS_SUPPORT_FLAG_DIRTY_REGIONS) ? 1u : 0u;
      dirty_rects_support.bits.supports_require_full_row = (capDirtyRegions.SupportFlags & D3D12_VIDEO_ENCODER_DIRTY_REGIONS_SUPPORT_FLAG_DIRTY_REGIONS_REQUIRE_FULL_ROW) ? 1u : 0u;
   }

   capDirtyRegions.MapValuesType = D3D12_VIDEO_ENCODER_DIRTY_REGIONS_MAP_VALUES_MODE_SKIP;
   if (SUCCEEDED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_DIRTY_REGIONS, &capDirtyRegions, sizeof(capDirtyRegions))))
   {
      dirty_rects_support.bits.supports_info_type_skip = (capDirtyRegions.SupportFlags & D3D12_VIDEO_ENCODER_DIRTY_REGIONS_SUPPORT_FLAG_DIRTY_REGIONS) ? 1u : 0u;
   }

   return dirty_rects_support;
}

static
union pipe_enc_cap_dirty_info
get_dirty_rects_support(D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO sessionInfo,
                        ID3D12VideoDevice3* pD3D12VideoDevice,
                        D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE mapSource)
{
   // Check first with the usual subregion partitioning mode passed by caller
   union pipe_enc_cap_dirty_info support = {};
   support = query_dirty_rects_support(sessionInfo, pD3D12VideoDevice, mapSource);

   // If there is no support, check if there is with AUTO slice mode
   if(!support.bits.supports_info_type_dirty)
   {
      // Try with the other subregion partitioning mode
      sessionInfo.SubregionFrameEncoding = D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_AUTO;
      sessionInfo.SubregionFrameEncodingData.DataSize = 0u;
      sessionInfo.SubregionFrameEncodingData.pSlicesPartition_H264 = NULL;
      sessionInfo.SubregionFrameEncodingData.pSlicesPartition_HEVC = NULL;
      sessionInfo.SubregionFrameEncodingData.pTilesPartition_AV1 = NULL;
      support = query_dirty_rects_support(sessionInfo, pD3D12VideoDevice, mapSource);
      if (support.bits.supports_info_type_dirty)
      {
         support.bits.supports_require_auto_slice_mode = 1u;
      }
   }

   // If still there is no support, check if for HEVC there is also a need to disable SAO/LOOP filters
   if(!support.bits.supports_info_type_dirty &&
      (sessionInfo.Codec == D3D12_VIDEO_ENCODER_CODEC_HEVC))
   {
      // Turn on disabling loop filter flag
      sessionInfo.CodecConfiguration.pHEVCConfig->ConfigurationFlags |= D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_FLAG_DISABLE_LOOP_FILTER_ACROSS_SLICES;
      // Turn off enabling SAO filter flag
      sessionInfo.CodecConfiguration.pHEVCConfig->ConfigurationFlags &= ~D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_HEVC_FLAG_ENABLE_SAO_FILTER;
      support = query_dirty_rects_support(sessionInfo, pD3D12VideoDevice, mapSource);
      if (support.bits.supports_info_type_dirty)
      {
         // If there is support, mark it as requiring special configuration
         support.bits.supports_require_auto_slice_mode = 1u; // This was included in the changes to sessionInfo above.
         support.bits.supports_require_sao_filter_disabled = 1u;
         support.bits.supports_require_loop_filter_disabled = 1u;

      }
   }

   return support;
}

static
union pipe_enc_cap_move_rect
get_move_rects_support(D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO sessionInfo,
                       ID3D12VideoDevice3* pD3D12VideoDevice)
{
   D3D12_FEATURE_DATA_VIDEO_ENCODER_MOTION_SEARCH capMotionVectors =
   {
      // UINT NodeIndex;                                                                                 // input
      0u,
      // D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO SessionInfo;                                         // input
      sessionInfo,
      // D3D12_VIDEO_ENCODER_FRAME_MOTION_SEARCH_MODE MotionSearchMode;                                  // input
      D3D12_VIDEO_ENCODER_FRAME_MOTION_SEARCH_MODE_FULL_SEARCH,
      // D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE MapSource;                                                 // input
      D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_CPU_BUFFER,
      // BOOL BidirectionalRefFrameEnabled;                                                              // input
      false,
      // D3D12_VIDEO_ENCODER_MOTION_SEARCH_SUPPORT_FLAGS SupportFlags;                                   // output
      D3D12_VIDEO_ENCODER_MOTION_SEARCH_SUPPORT_FLAG_NONE,
      // UINT MaxMotionHints;                                                                            // output
      0u,
      // UINT MinDeviation;                                                                              // output
      0u,
      // UINT MaxDeviation;                                                                              // output
      0u,
      // UINT MapSourcePreferenceRanking;                                                                // output
      0u,
      // D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAGS MotionUnitPrecisionSupport; // output
      D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAG_NONE
   };

   union pipe_enc_cap_move_rect move_rects_support = {};
   if ((SUCCEEDED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_MOTION_SEARCH, &capMotionVectors, sizeof(capMotionVectors)))) &&
       (capMotionVectors.SupportFlags & D3D12_VIDEO_ENCODER_MOTION_SEARCH_SUPPORT_FLAG_SUPPORTED))
   {
      move_rects_support.bits.max_motion_hints = std::min(capMotionVectors.MaxMotionHints, 1u << 16u);
      move_rects_support.bits.supports_overlapped_rects = (capMotionVectors.SupportFlags & D3D12_VIDEO_ENCODER_MOTION_SEARCH_SUPPORT_FLAG_MULTIPLE_HINTS) ? 1u : 0u;
      move_rects_support.bits.supports_precision_full_pixel = (capMotionVectors.MotionUnitPrecisionSupport & D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAG_FULL_PIXEL) ? 1u : 0u;
      move_rects_support.bits.supports_precision_half_pixel = (capMotionVectors.MotionUnitPrecisionSupport & D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAG_HALF_PIXEL) ? 1u : 0u;
      move_rects_support.bits.supports_precision_quarter_pixel = (capMotionVectors.MotionUnitPrecisionSupport & D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAG_QUARTER_PIXEL) ? 1u : 0u;
   }

   return move_rects_support;
}

static
void
get_gpu_output_stats_support(D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO sessionInfo,
                             D3D12_VIDEO_ENCODER_SUPPORT_FLAGS capEncoderSupportFlags,
                             ID3D12VideoDevice3* pD3D12VideoDevice,
                             union pipe_enc_cap_gpu_stats_map &gpu_stats_qp,
                             union pipe_enc_cap_gpu_stats_map &gpu_stats_satd,
                             union pipe_enc_cap_gpu_stats_map &gpu_stats_rcbits,
                             union pipe_enc_cap_gpu_stats_psnr& psnr_support)
{
   gpu_stats_qp.bits.supported = ((capEncoderSupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_PER_BLOCK_QP_MAP_METADATA_AVAILABLE)) ? 1u : 0u;
   gpu_stats_satd.bits.supported = ((capEncoderSupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_PER_BLOCK_SATD_MAP_METADATA_AVAILABLE)) ? 1u : 0u;
   gpu_stats_rcbits.bits.supported = ((capEncoderSupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_PER_BLOCK_RC_BIT_ALLOCATION_MAP_METADATA_AVAILABLE  )) ? 1u : 0u;
   psnr_support.bits.supports_y_channel = ((capEncoderSupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_FRAME_PSNR_METADATA_AVAILABLE)) ? 1u : 0u;

   D3D12_VIDEO_ENCODER_OPTIONAL_METADATA_ENABLE_FLAGS optionalMetadataFlags = D3D12_VIDEO_ENCODER_OPTIONAL_METADATA_ENABLE_FLAG_NONE;
   if (gpu_stats_qp.bits.supported)
      optionalMetadataFlags |= D3D12_VIDEO_ENCODER_OPTIONAL_METADATA_ENABLE_FLAG_QP_MAP;
   if (gpu_stats_satd.bits.supported)
      optionalMetadataFlags |= D3D12_VIDEO_ENCODER_OPTIONAL_METADATA_ENABLE_FLAG_SATD_MAP;
   if (gpu_stats_rcbits.bits.supported)
      optionalMetadataFlags |= D3D12_VIDEO_ENCODER_OPTIONAL_METADATA_ENABLE_FLAG_RC_BIT_ALLOCATION_MAP;
   if (psnr_support.bits.supports_y_channel)
      optionalMetadataFlags |= D3D12_VIDEO_ENCODER_OPTIONAL_METADATA_ENABLE_FLAG_FRAME_PSNR;

   D3D12_FEATURE_DATA_VIDEO_ENCODER_RESOURCE_REQUIREMENTS1 capStatsResourceReqs =
   {
      // UINT NodeIndex;                                                                                     // input
      0u,
      // D3D12_VIDEO_ENCODER_CODEC Codec;                                                                    // input
      sessionInfo.Codec,
      // D3D12_VIDEO_ENCODER_PROFILE_DESC Profile;                                                           // input
      sessionInfo.Profile,
      // DXGI_FORMAT InputFormat;                                                                            // input
      sessionInfo.InputFormat,
      // D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC PictureTargetResolution;                                // input
      sessionInfo.InputResolution,
      // BOOL IsSupported;                                                                                   // output
      FALSE,
      // UINT CompressedBitstreamBufferAccessAlignment;                                                      // output
      0u,
      // UINT EncoderMetadataBufferAccessAlignment;                                                          // output
      0u,
      // UINT MaxEncoderOutputMetadataBufferSize;                                                            // output
      0u,
      // D3D12_VIDEO_ENCODER_OPTIONAL_METADATA_ENABLE_FLAGS OptionalMetadata;                                // input
      optionalMetadataFlags,
      // D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION CodecConfiguration;                                         // input
      sessionInfo.CodecConfiguration,
      // D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC EncoderOutputMetadataQPMapTextureDimensions;            // output
      {0u, 0u},
      // D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC EncoderOutputMetadataSATDMapTextureDimensions;          // output
      {0u, 0u},
      // D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC EncoderOutputMetadataBitAllocationMapTextureDimensions; // output
      {0u, 0u},
      // UINT EncoderOutputMetadataFramePSNRComponentsNumber;                                                // output
      0u,
      // UINT EncoderOutputMetadataSubregionsPSNRComponentsNumber;                                           // output
      0u,
      // UINT EncoderOutputMetadataSubregionsPSNRResolvedMetadataBufferSize;                                 // output
      0u,
   };

   if (SUCCEEDED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_RESOURCE_REQUIREMENTS1, &capStatsResourceReqs, sizeof(capStatsResourceReqs))))
   {
      if (gpu_stats_qp.bits.supported) {
         gpu_stats_qp.bits.pipe_pixel_format = (sessionInfo.Codec == D3D12_VIDEO_ENCODER_CODEC_AV1) ? PIPE_FORMAT_R16_SINT : PIPE_FORMAT_R8_SINT;
         uint32_t block_size = static_cast<uint32_t>(std::ceil(capStatsResourceReqs.PictureTargetResolution.Width / static_cast<double>(capStatsResourceReqs.EncoderOutputMetadataQPMapTextureDimensions.Width)));
         gpu_stats_qp.bits.log2_values_block_size = static_cast<uint32_t>(std::log2(block_size));
      }

      if (gpu_stats_satd.bits.supported) {
         gpu_stats_satd.bits.pipe_pixel_format = PIPE_FORMAT_R32_UINT;
         uint32_t block_size = static_cast<uint32_t>(std::ceil(capStatsResourceReqs.PictureTargetResolution.Width / static_cast<double>(capStatsResourceReqs.EncoderOutputMetadataSATDMapTextureDimensions.Width)));
         gpu_stats_satd.bits.log2_values_block_size = static_cast<uint32_t>(std::log2(block_size));
      }

      if (gpu_stats_rcbits.bits.supported) {
         gpu_stats_rcbits.bits.pipe_pixel_format = PIPE_FORMAT_R32_UINT;
         uint32_t block_size = static_cast<uint32_t>(std::ceil(capStatsResourceReqs.PictureTargetResolution.Width / static_cast<double>(capStatsResourceReqs.EncoderOutputMetadataBitAllocationMapTextureDimensions.Width)));
         gpu_stats_rcbits.bits.log2_values_block_size = static_cast<uint32_t>(std::log2(block_size));
      }

      if ((capEncoderSupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_FRAME_PSNR_METADATA_AVAILABLE))
      {
         psnr_support.bits.supports_y_channel = (capStatsResourceReqs.EncoderOutputMetadataFramePSNRComponentsNumber >= 1);
         psnr_support.bits.supports_u_channel = (capStatsResourceReqs.EncoderOutputMetadataFramePSNRComponentsNumber >= 2);
         psnr_support.bits.supports_u_channel = (capStatsResourceReqs.EncoderOutputMetadataFramePSNRComponentsNumber >= 3);
      }
   }
}

static
union pipe_enc_cap_sliced_notifications
get_sliced_encode_support(D3D12_VIDEO_ENCODER_SUPPORT_FLAGS capEncoderSupportFlags)
{
   union pipe_enc_cap_sliced_notifications sliced_encode_support = {};
   sliced_encode_support.bits.supported = ((capEncoderSupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_SUBREGION_NOTIFICATION_SINGLE_BUFFER_AVAILABLE) ||
                                           (capEncoderSupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_SUBREGION_NOTIFICATION_ARRAY_OF_BUFFERS_AVAILABLE)) ? 1u : 0u;
   sliced_encode_support.bits.multiple_buffers_required = (capEncoderSupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_SUBREGION_NOTIFICATION_ARRAY_OF_BUFFERS_AVAILABLE) ? 1u : 0u;
   return sliced_encode_support;
}

static
union pipe_enc_cap_motion_vector_map
get_motion_gpuinput_support(D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO sessionInfo,
                           ID3D12VideoDevice3* pD3D12VideoDevice)
{
   D3D12_FEATURE_DATA_VIDEO_ENCODER_MOTION_SEARCH capMotionInput =
   {
      // UINT NodeIndex;
      0u,
      // D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO SessionInfo;
      sessionInfo,
      // D3D12_VIDEO_ENCODER_FRAME_MOTION_SEARCH_MODE MotionSearchMode;
      D3D12_VIDEO_ENCODER_FRAME_MOTION_SEARCH_MODE_FULL_SEARCH,
      // D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE MapSource;
      D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_GPU_TEXTURE,
      // BOOL BidirectionalRefFrameEnabled;
      false,
      // D3D12_VIDEO_ENCODER_MOTION_SEARCH_SUPPORT_FLAGS SupportFlags;
      D3D12_VIDEO_ENCODER_MOTION_SEARCH_SUPPORT_FLAG_NONE,
      // UINT MaxMotionHints;
      0u,
      // UINT MinDeviation;
      0u,
      // UINT MaxDeviation;
      0u,
      // UINT MapSourcePreferenceRanking;
      0u,
      // D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAGS MotionUnitPrecisionSupport;
      D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAG_NONE
   };

   union pipe_enc_cap_motion_vector_map motion_gpu_support = {};
   if (SUCCEEDED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_MOTION_SEARCH, &capMotionInput, sizeof(capMotionInput))))
   {

      if (capMotionInput.SupportFlags & D3D12_VIDEO_ENCODER_MOTION_SEARCH_SUPPORT_FLAG_SUPPORTED)
      {
         motion_gpu_support.bits.max_motion_hints = std::min(31u, capMotionInput.MaxMotionHints);
         assert(motion_gpu_support.bits.max_motion_hints <= PIPE_ENC_MOVE_MAP_MAX_HINTS);
         if (motion_gpu_support.bits.max_motion_hints > 0)
         {
            motion_gpu_support.bits.supports_precision_full_pixel = (capMotionInput.MotionUnitPrecisionSupport & D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAG_FULL_PIXEL) ? 1u : 0u;
            motion_gpu_support.bits.supports_precision_half_pixel = (capMotionInput.MotionUnitPrecisionSupport & D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAG_HALF_PIXEL) ? 1u : 0u;
            motion_gpu_support.bits.supports_precision_quarter_pixel = (capMotionInput.MotionUnitPrecisionSupport & D3D12_VIDEO_ENCODER_FRAME_INPUT_MOTION_UNIT_PRECISION_SUPPORT_FLAG_QUARTER_PIXEL) ? 1u : 0u;
            motion_gpu_support.bits.support_multiple_dpb_refs = 0u; // (capMotionInput.SupportFlags & D3D12_VIDEO_ENCODER_MOTION_SEARCH_SUPPORT_FLAG_GPU_TEXTURE_MULTIPLE_REFERENCES) ? 1u : 0u;
            motion_gpu_support.bits.pipe_pixel_vectors_map_format = PIPE_FORMAT_R16G16_SINT; // As per DX12 spec
            motion_gpu_support.bits.pipe_pixel_vectors_metadata_map_format = PIPE_FORMAT_R8_UINT; // As per DX12 spec
         }
      }
   }

   return motion_gpu_support;
}

static
union pipe_enc_cap_qpmap
get_qpmap_gpuinput_support(D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO sessionInfo,
                           ID3D12VideoDevice3* pD3D12VideoDevice)
{
   D3D12_FEATURE_DATA_VIDEO_ENCODER_QPMAP_INPUT capQPInput =
   {
      //  UINT NodeIndex;
      0u,
      //  D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO SessionInfo;
      sessionInfo,
      //  D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE MapSource;
      D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_GPU_TEXTURE,
      //  BOOL IsSupported;
      FALSE,
      //  UINT MapSourcePreferenceRanking;
      0u,
      //  UINT BlockSize;
      0u,
   };

   union pipe_enc_cap_qpmap qpmap_gpu_support = {};
   if (SUCCEEDED(pD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_QPMAP_INPUT, &capQPInput, sizeof(capQPInput))))
   {
      qpmap_gpu_support.bits.supported = capQPInput.IsSupported ? 1u : 0u;
      if (qpmap_gpu_support.bits.supported)
      {
         qpmap_gpu_support.bits.pipe_pixel_format = PIPE_FORMAT_R8_UINT; // As per DX12 spec
         qpmap_gpu_support.bits.log2_values_block_size = static_cast<uint32_t>(std::log2(capQPInput.BlockSize));
      }
   }

   return qpmap_gpu_support;
}

#endif // D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE

static bool
d3d12_has_video_encode_support(struct pipe_screen *pscreen,
                               enum pipe_video_profile profile,
                               uint32_t &maxLvlSpec,
                               D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC &minRes,
                               D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC &maxRes,
                               union pipe_enc_cap_surface_alignment &alignRes,
                               uint32_t &maxSlices,
                               uint32_t &supportedSliceStructures,
                               uint32_t &maxReferencesPerFrame,
                               uint32_t &maxLongTermReferences,
                               uint32_t &maxDPBCapacity,
                               struct d3d12_encode_codec_support& codecSupport,
                               uint32_t &isRCMaxFrameSizeSupported,
                               uint32_t &maxQualityLevels,
                               uint32_t &max_tile_rows,
                               uint32_t &max_tile_cols,
                               uint32_t &maxIRDuration,
                               union pipe_enc_cap_roi &roi_support,
                               bool &bVideoEncodeRequiresTextureArray,
                               union pipe_enc_cap_dirty_info &dirty_rects_support,
                               union pipe_enc_cap_move_rect &move_rects_support,
                               union pipe_enc_cap_gpu_stats_map &gpu_stats_qp,
                               union pipe_enc_cap_gpu_stats_map &gpu_stats_satd,
                               union pipe_enc_cap_gpu_stats_map &gpu_stats_rcbits,
                               union pipe_enc_cap_sliced_notifications &sliced_encode_support,
                               union pipe_enc_cap_dirty_info &dirty_rects_support_gpu,
                               union pipe_enc_cap_qpmap &qpmap_support,
                               union pipe_enc_cap_motion_vector_map &gpu_motion_input_support,
                               union pipe_enc_cap_two_pass &two_pass_support,
                               union pipe_enc_cap_gpu_stats_psnr& psnr_support)
{
   ComPtr<ID3D12VideoDevice3> spD3D12VideoDevice;
   struct d3d12_screen *pD3D12Screen = (struct d3d12_screen *) pscreen;
   if (FAILED(pD3D12Screen->dev->QueryInterface(IID_PPV_ARGS(spD3D12VideoDevice.GetAddressOf())))) {
      // No video encode support in underlying d3d12 device (needs ID3D12VideoDevice3)
      return 0;
   }

   D3D12_FEATURE_DATA_VIDEO_FEATURE_AREA_SUPPORT VideoFeatureAreaSupport = {};
   if (FAILED(spD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_FEATURE_AREA_SUPPORT,
                                                      &VideoFeatureAreaSupport,
                                                      sizeof(VideoFeatureAreaSupport)))) {
      return false;
   }
   D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT d3d12_codec_support = { };
   bool supportsProfile = false;
   switch (profile) {
#if VIDEO_CODEC_H264ENC
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_CONSTRAINED_BASELINE:
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_BASELINE:
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_MAIN:
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_HIGH:
      case PIPE_VIDEO_PROFILE_MPEG4_AVC_HIGH10:
      {
         D3D12_VIDEO_ENCODER_PROFILE_DESC profDesc = {};
         D3D12_VIDEO_ENCODER_PROFILE_H264 profH264 =
            d3d12_video_encoder_convert_profile_to_d3d12_enc_profile_h264(profile);
         profDesc.DataSize = sizeof(profH264);
         profDesc.pH264Profile = &profH264;
         D3D12_VIDEO_ENCODER_CODEC codecDesc = d3d12_video_encoder_convert_codec_to_d3d12_enc_codec(profile);
         D3D12_VIDEO_ENCODER_LEVELS_H264 minLvlSettingH264 = static_cast<D3D12_VIDEO_ENCODER_LEVELS_H264>(0);
         D3D12_VIDEO_ENCODER_LEVELS_H264 maxLvlSettingH264 = static_cast<D3D12_VIDEO_ENCODER_LEVELS_H264>(0);
         D3D12_VIDEO_ENCODER_LEVEL_SETTING minLvl = {};
         D3D12_VIDEO_ENCODER_LEVEL_SETTING maxLvl = {};
         minLvl.pH264LevelSetting = &minLvlSettingH264;
         minLvl.DataSize = sizeof(minLvlSettingH264);
         maxLvl.pH264LevelSetting = &maxLvlSettingH264;
         maxLvl.DataSize = sizeof(maxLvlSettingH264);
         if (d3d12_video_encode_max_supported_level_for_profile(codecDesc,
                                                                profDesc,
                                                                minLvl,
                                                                maxLvl,
                                                                spD3D12VideoDevice.Get())) {
            d3d12_video_encoder_convert_from_d3d12_level_h264(maxLvlSettingH264, maxLvlSpec);
            supportsProfile = true;

            DXGI_FORMAT encodeFormat = d3d12_convert_pipe_video_profile_to_dxgi_format(profile);
            supportsProfile = supportsProfile &&
                              d3d12_video_encode_supported_resolution_range(codecDesc, minRes, maxRes, alignRes, spD3D12VideoDevice.Get());

            D3D12_VIDEO_ENCODER_PROFILE_DESC profile;
            profile.pH264Profile = &profH264;
            profile.DataSize = sizeof(profH264);
            D3D12_VIDEO_ENCODER_LEVEL_SETTING level;
            level.pH264LevelSetting = &maxLvlSettingH264;
            level.DataSize = sizeof(maxLvlSettingH264);
            supportedSliceStructures = d3d12_video_encode_supported_slice_structures(codecDesc,
                                                                                     profile,
                                                                                     level,
                                                                                     spD3D12VideoDevice.Get());
#if D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE
            D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT2 capEncoderSupportData1 = {};
            D3D12_FEATURE_DATA_VIDEO_ENCODER_RESOLUTION_SUPPORT_LIMITS1 resolutionDepCaps;
#else
            D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1 capEncoderSupportData1 = {};
            D3D12_FEATURE_DATA_VIDEO_ENCODER_RESOLUTION_SUPPORT_LIMITS resolutionDepCaps;
#endif
            capEncoderSupportData1.SubregionFrameEncoding = (supportedSliceStructures == PIPE_VIDEO_CAP_SLICE_STRUCTURE_NONE) ?
                                                             D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_FULL_FRAME :
                                                             D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_UNIFORM_PARTITIONING_SUBREGIONS_PER_FRAME;
            D3D12_VIDEO_ENCODER_PICTURE_CONTROL_SUBREGIONS_LAYOUT_DATA_SLICES sliceData = { };
            capEncoderSupportData1.SubregionFrameEncodingData.DataSize = sizeof(sliceData);
            capEncoderSupportData1.SubregionFrameEncodingData.pSlicesPartition_H264 = &sliceData;
            d3d12_encode_support_cap_allocations cap_allocations = {};
            supportsProfile = supportsProfile && d3d12_video_encode_support_caps(codecDesc,
                                                                                 maxRes,
                                                                                 encodeFormat,
                                                                                 spD3D12VideoDevice.Get(),
                                                                                 d3d12_codec_support,
                                                                                 capEncoderSupportData1,
                                                                                 resolutionDepCaps,
                                                                                 maxQualityLevels,
                                                                                 cap_allocations);
            bVideoEncodeRequiresTextureArray = (capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_RECONSTRUCTED_FRAMES_REQUIRE_TEXTURE_ARRAYS) != 0;
            if (supportedSliceStructures == PIPE_VIDEO_CAP_SLICE_STRUCTURE_NONE)
               maxSlices = 0;
            else
               maxSlices = resolutionDepCaps.MaxSubregionsNumber;

            maxIRDuration = resolutionDepCaps.MaxIntraRefreshFrameDuration;
            isRCMaxFrameSizeSupported = ((capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_RATE_CONTROL_MAX_FRAME_SIZE_AVAILABLE) != 0) ? 1 : 0;
            maxReferencesPerFrame =
               d3d12_video_encode_supported_references_per_frame_structures(codecDesc,
                                                                            profile,
                                                                            spD3D12VideoDevice.Get(),
                                                                            codecSupport,
                                                                            maxLongTermReferences,
                                                                            maxDPBCapacity);

            memset(&roi_support, 0, sizeof(roi_support));
            roi_support.bits.roi_rc_qp_delta_support = ((capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_RATE_CONTROL_DELTA_QP_AVAILABLE) != 0) ? 1 : 0;
            roi_support.bits.num_roi_regions = roi_support.bits.roi_rc_qp_delta_support ? PIPE_ENC_ROI_REGION_NUM_MAX : 0;
            roi_support.bits.log2_roi_min_block_pixel_size = static_cast<uint32_t>(std::log2(capEncoderSupportData1.pResolutionDependentSupport[0].QPMapRegionPixelsSize));

            supportsProfile = d3d12_video_encode_get_h264_codec_support(profDesc,
                                                                        spD3D12VideoDevice.Get(),
                                                                        codecSupport.h264_support.d3d12_caps);

#if D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE

            D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO sessionInfo =
            {
               // D3D12_VIDEO_ENCODER_CODEC Codec;
               capEncoderSupportData1.Codec,
               // D3D12_VIDEO_ENCODER_PROFILE_DESC Profile;
               profDesc,
               // D3D12_VIDEO_ENCODER_LEVEL_SETTING Level;
               maxLvl,
               // DXGI_FORMAT InputFormat;
               capEncoderSupportData1.InputFormat,
               // D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC InputResolution;
               maxRes,
               // D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION CodecConfiguration;
               capEncoderSupportData1.CodecConfiguration,
               // D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE SubregionFrameEncoding;
               capEncoderSupportData1.SubregionFrameEncoding,
               // D3D12_VIDEO_ENCODER_PICTURE_CONTROL_SUBREGIONS_LAYOUT_DATA SubregionFrameEncodingData;
               capEncoderSupportData1.SubregionFrameEncodingData,
            };

            dirty_rects_support = get_dirty_rects_support(sessionInfo, spD3D12VideoDevice.Get(), D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_CPU_BUFFER);
            dirty_rects_support_gpu = get_dirty_rects_support(sessionInfo, spD3D12VideoDevice.Get(), D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_GPU_TEXTURE);
            move_rects_support = get_move_rects_support(sessionInfo, spD3D12VideoDevice.Get());
            get_gpu_output_stats_support(sessionInfo, capEncoderSupportData1.SupportFlags, spD3D12VideoDevice.Get(), gpu_stats_qp, gpu_stats_satd, gpu_stats_rcbits, psnr_support);
            sliced_encode_support = get_sliced_encode_support(capEncoderSupportData1.SupportFlags);
            qpmap_support = get_qpmap_gpuinput_support(sessionInfo, spD3D12VideoDevice.Get());
            gpu_motion_input_support = get_motion_gpuinput_support(sessionInfo, spD3D12VideoDevice.Get());
            two_pass_support = query_two_pass_support(pscreen, sessionInfo, spD3D12VideoDevice.Get(),
                                          ((capEncoderSupportData1.SupportFlags &
                                             D3D12_VIDEO_ENCODER_SUPPORT_FLAG_READABLE_RECONSTRUCTED_PICTURE_LAYOUT_AVAILABLE) != 0));
#endif
         }
      } break;
#endif
#if VIDEO_CODEC_H265ENC
      case PIPE_VIDEO_PROFILE_HEVC_MAIN:
      case PIPE_VIDEO_PROFILE_HEVC_MAIN_10:
      case PIPE_VIDEO_PROFILE_HEVC_MAIN_444:
      case PIPE_VIDEO_PROFILE_HEVC_MAIN10_444:
      case PIPE_VIDEO_PROFILE_HEVC_MAIN_422:
      case PIPE_VIDEO_PROFILE_HEVC_MAIN10_422:
      {

         bool bRuntimeSupportsProfile = true;
         if ((profile != PIPE_VIDEO_PROFILE_HEVC_MAIN) &&
             (profile != PIPE_VIDEO_PROFILE_HEVC_MAIN_10))
         {
#if D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE
            // Video encode support in underlying d3d12 device needs ID3D12VideoDevice4
            // for this HEVC 422/444 d3d12 gallium driver implementation
            ComPtr<ID3D12VideoDevice4> spD3D12VideoDevice4;
            bRuntimeSupportsProfile = SUCCEEDED(spD3D12VideoDevice->QueryInterface(IID_PPV_ARGS(spD3D12VideoDevice4.GetAddressOf())));
#else
            bRuntimeSupportsProfile = false;
#endif // D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE
         }

         D3D12_VIDEO_ENCODER_PROFILE_DESC profDesc = {};
         D3D12_VIDEO_ENCODER_PROFILE_HEVC profHEVC =
            d3d12_video_encoder_convert_profile_to_d3d12_enc_profile_hevc(profile);
         profDesc.DataSize = sizeof(profHEVC);
         profDesc.pHEVCProfile = &profHEVC;
         D3D12_VIDEO_ENCODER_CODEC codecDesc = d3d12_video_encoder_convert_codec_to_d3d12_enc_codec(profile);
         D3D12_VIDEO_ENCODER_LEVEL_TIER_CONSTRAINTS_HEVC minLvlSettingHEVC = { };
         D3D12_VIDEO_ENCODER_LEVEL_TIER_CONSTRAINTS_HEVC maxLvlSettingHEVC = { };
         D3D12_VIDEO_ENCODER_LEVEL_SETTING minLvl = {};
         D3D12_VIDEO_ENCODER_LEVEL_SETTING maxLvl = {};
         minLvl.pHEVCLevelSetting = &minLvlSettingHEVC;
         minLvl.DataSize = sizeof(minLvlSettingHEVC);
         maxLvl.pHEVCLevelSetting = &maxLvlSettingHEVC;
         maxLvl.DataSize = sizeof(maxLvlSettingHEVC);

         if (bRuntimeSupportsProfile && d3d12_video_encode_max_supported_level_for_profile(codecDesc,
                                                                                           profDesc,
                                                                                           minLvl,
                                                                                           maxLvl,
                                                                                           spD3D12VideoDevice.Get())) {
            d3d12_video_encoder_convert_from_d3d12_level_hevc(maxLvlSettingHEVC.Level, maxLvlSpec);

            D3D12_VIDEO_ENCODER_PROFILE_DESC d3d12_profile;
            d3d12_profile.pHEVCProfile = &profHEVC;
            d3d12_profile.DataSize = sizeof(profHEVC);
            D3D12_VIDEO_ENCODER_LEVEL_SETTING level;
            level.pHEVCLevelSetting = &maxLvlSettingHEVC;
            level.DataSize = sizeof(maxLvlSettingHEVC);
            supportedSliceStructures = d3d12_video_encode_supported_slice_structures(codecDesc,
                                                                                     d3d12_profile,
                                                                                     level,
                                                                                     spD3D12VideoDevice.Get());

            maxReferencesPerFrame =
               d3d12_video_encode_supported_references_per_frame_structures(codecDesc,
                                                                            d3d12_profile,
                                                                            spD3D12VideoDevice.Get(),
                                                                            codecSupport,
                                                                            maxLongTermReferences,
                                                                            maxDPBCapacity);

            supportsProfile = d3d12_video_encode_get_hevc_codec_support(codecDesc,
                                                                        profDesc,
                                                                        spD3D12VideoDevice.Get(),
                                                                        codecSupport.hevc_support.d3d12_caps);
            if (supportsProfile) {
               d3d12_codec_support.DataSize = sizeof(codecSupport.hevc_support.d3d12_caps);
               d3d12_codec_support.pHEVCSupport1 = &codecSupport.hevc_support.d3d12_caps;

               /* get_video_param sets pipe_features.bits.config_supported = 1
                  to distinguish between supported cap with all bits off and unsupported by driver
                  with value = 0
               */
               codecSupport.hevc_support.hevc_block_sizes.bits.config_supported = 1;
               codecSupport.hevc_support.hevc_features.bits.config_supported = 1;

               // Fill codecSupport.hevc_support

               uint8_t minCuSize = d3d12_video_encoder_convert_12cusize_to_pixel_size_hevc(codecSupport.hevc_support.d3d12_caps.MinLumaCodingUnitSize);
               uint8_t maxCuSize = d3d12_video_encoder_convert_12cusize_to_pixel_size_hevc(codecSupport.hevc_support.d3d12_caps.MaxLumaCodingUnitSize);
               uint8_t MinCbLog2SizeY = static_cast<uint8_t>(std::log2(minCuSize));
               uint8_t CtbLog2SizeY = static_cast<uint8_t>(std::log2(maxCuSize));
               uint8_t minTuSize = d3d12_video_encoder_convert_12tusize_to_pixel_size_hevc(codecSupport.hevc_support.d3d12_caps.MinLumaTransformUnitSize);
               uint8_t maxTuSize = d3d12_video_encoder_convert_12tusize_to_pixel_size_hevc(codecSupport.hevc_support.d3d12_caps.MaxLumaTransformUnitSize);

               codecSupport.hevc_support.hevc_block_sizes.bits.log2_max_coding_tree_block_size_minus3
                              = static_cast<uint8_t>(CtbLog2SizeY - 3);
               codecSupport.hevc_support.hevc_block_sizes.bits.log2_min_coding_tree_block_size_minus3
                              = static_cast<uint8_t>(CtbLog2SizeY - 3);

               codecSupport.hevc_support.hevc_block_sizes.bits.log2_min_luma_coding_block_size_minus3
                              = static_cast<uint8_t>(MinCbLog2SizeY - 3);

               codecSupport.hevc_support.hevc_block_sizes.bits.log2_max_luma_transform_block_size_minus2
                              = static_cast<uint8_t>(std::log2(maxTuSize) - 2);

               codecSupport.hevc_support.hevc_block_sizes.bits.log2_min_luma_transform_block_size_minus2
                              = static_cast<uint8_t>(std::log2(minTuSize) - 2);

               codecSupport.hevc_support.hevc_block_sizes.bits.max_max_transform_hierarchy_depth_inter
                              = codecSupport.hevc_support.d3d12_caps.max_transform_hierarchy_depth_inter;

               codecSupport.hevc_support.hevc_block_sizes.bits.min_max_transform_hierarchy_depth_inter
                              = codecSupport.hevc_support.d3d12_caps.max_transform_hierarchy_depth_inter;

               codecSupport.hevc_support.hevc_block_sizes.bits.max_max_transform_hierarchy_depth_intra
                              = codecSupport.hevc_support.d3d12_caps.max_transform_hierarchy_depth_intra;

               codecSupport.hevc_support.hevc_block_sizes.bits.min_max_transform_hierarchy_depth_intra
                              = codecSupport.hevc_support.d3d12_caps.max_transform_hierarchy_depth_intra;

               codecSupport.hevc_support.hevc_block_sizes.bits.log2_max_pcm_coding_block_size_minus3 = 0; // No PCM Supported
               codecSupport.hevc_support.hevc_block_sizes.bits.log2_min_pcm_coding_block_size_minus3 = 0; // No PCM Supported

               // HEVC range ext caps
               codecSupport.hevc_support.hevc_range_ext.value = 0;
               codecSupport.hevc_support.hevc_range_ext.bits.supported_diff_cu_chroma_qp_offset_depth_values = codecSupport.hevc_support.d3d12_caps.allowed_diff_cu_chroma_qp_offset_depth_values;
               codecSupport.hevc_support.hevc_range_ext.bits.supported_log2_sao_offset_scale_luma_values = codecSupport.hevc_support.d3d12_caps.allowed_log2_sao_offset_scale_luma_values;
               codecSupport.hevc_support.hevc_range_ext.bits.supported_log2_sao_offset_scale_chroma_values = codecSupport.hevc_support.d3d12_caps.allowed_log2_sao_offset_scale_chroma_values;
               codecSupport.hevc_support.hevc_range_ext.bits.supported_log2_max_transform_skip_block_size_minus2_values = codecSupport.hevc_support.d3d12_caps.allowed_log2_max_transform_skip_block_size_minus2_values;
               codecSupport.hevc_support.hevc_range_ext.bits.min_chroma_qp_offset_list_len_minus1_values = 0;
               codecSupport.hevc_support.hevc_range_ext.bits.max_chroma_qp_offset_list_len_minus1_values = 0;
               if (codecSupport.hevc_support.d3d12_caps.allowed_chroma_qp_offset_list_len_minus1_values)
               {
                  codecSupport.hevc_support.hevc_range_ext.bits.min_chroma_qp_offset_list_len_minus1_values = 5;
                  codecSupport.hevc_support.hevc_range_ext.bits.max_chroma_qp_offset_list_len_minus1_values = 0;
                  for (uint32_t i = 0; i < 6 /*Codec valid range for support for chroma_qp_offset_list_len_minus1 is [0, 5]*/; i++)
                  {
                     if ((codecSupport.hevc_support.d3d12_caps.allowed_chroma_qp_offset_list_len_minus1_values & (1 << i)) != 0)
                     {
                        codecSupport.hevc_support.hevc_range_ext.bits.min_chroma_qp_offset_list_len_minus1_values = std::min(codecSupport.hevc_support.hevc_range_ext.bits.min_chroma_qp_offset_list_len_minus1_values, i);
                        codecSupport.hevc_support.hevc_range_ext.bits.max_chroma_qp_offset_list_len_minus1_values = std::max(codecSupport.hevc_support.hevc_range_ext.bits.max_chroma_qp_offset_list_len_minus1_values, i);
                     }
                  }
               }

               codecSupport.hevc_support.hevc_range_ext_flags.value = 0;
               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_TRANSFORM_SKIP_ROTATION_ENABLED_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_transform_skip_rotation_enabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_TRANSFORM_SKIP_ROTATION_ENABLED_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_transform_skip_rotation_enabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_TRANSFORM_SKIP_CONTEXT_ENABLED_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_transform_skip_context_enabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_TRANSFORM_SKIP_CONTEXT_ENABLED_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_transform_skip_context_enabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_IMPLICIT_RDPCM_ENABLED_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_implicit_rdpcm_enabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_IMPLICIT_RDPCM_ENABLED_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_implicit_rdpcm_enabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_EXPLICIT_RDPCM_ENABLED_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_explicit_rdpcm_enabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_EXPLICIT_RDPCM_ENABLED_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_explicit_rdpcm_enabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_EXTENDED_PRECISION_PROCESSING_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_extended_precision_processing_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_EXTENDED_PRECISION_PROCESSING_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_extended_precision_processing_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_INTRA_SMOOTHING_DISABLED_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_intra_smoothing_disabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_INTRA_SMOOTHING_DISABLED_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_intra_smoothing_disabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_HIGH_PRECISION_OFFSETS_ENABLED_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_high_precision_offsets_enabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_HIGH_PRECISION_OFFSETS_ENABLED_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_high_precision_offsets_enabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_PERSISTENT_RICE_ADAPTATION_ENABLED_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_persistent_rice_adaptation_enabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_PERSISTENT_RICE_ADAPTATION_ENABLED_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_persistent_rice_adaptation_enabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_CABAC_BYPASS_ALIGNMENT_ENABLED_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_cabac_bypass_alignment_enabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_CABAC_BYPASS_ALIGNMENT_ENABLED_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_cabac_bypass_alignment_enabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_CROSS_COMPONENT_PREDICTION_ENABLED_FLAG_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_cross_component_prediction_enabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_CROSS_COMPONENT_PREDICTION_ENABLED_FLAG_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_cross_component_prediction_enabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_CHROMA_QP_OFFSET_LIST_ENABLED_FLAG_SUPPORT)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_chroma_qp_offset_list_enabled_flag |= PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_CHROMA_QP_OFFSET_LIST_ENABLED_FLAG_REQUIRED)
                  codecSupport.hevc_support.hevc_range_ext_flags.bits.supports_chroma_qp_offset_list_enabled_flag |= PIPE_ENC_FEATURE_REQUIRED;

               // Feature flags

               uint32_t ref_l0 = maxReferencesPerFrame       & 0xffff;
               uint32_t ref_l1 = maxReferencesPerFrame >> 16 & 0xffff;

               codecSupport.hevc_support.prediction_direction = PIPE_H265_PRED_DIRECTION_ALL;
               if(ref_l0)
                  codecSupport.hevc_support.prediction_direction |= PIPE_H265_PRED_DIRECTION_PREVIOUS;
               if(ref_l1)
                  codecSupport.hevc_support.prediction_direction |= PIPE_H265_PRED_DIRECTION_FUTURE;

               codecSupport.hevc_support.hevc_features.bits.separate_colour_planes = PIPE_ENC_FEATURE_NOT_SUPPORTED;
               if (codecSupport.hevc_support.d3d12_caps.SupportFlags1 & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG1_SEPARATE_COLOUR_PLANE_SUPPORT)
                  codecSupport.hevc_support.hevc_features.bits.separate_colour_planes = PIPE_ENC_FEATURE_SUPPORTED;

               if (codecSupport.hevc_support.d3d12_caps.SupportFlags1 & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG1_SEPARATE_COLOUR_PLANE_REQUIRED)
                  codecSupport.hevc_support.hevc_features.bits.separate_colour_planes = PIPE_ENC_FEATURE_REQUIRED;
               codecSupport.hevc_support.hevc_features.bits.scaling_lists = PIPE_ENC_FEATURE_NOT_SUPPORTED;
               codecSupport.hevc_support.hevc_features.bits.pcm = PIPE_ENC_FEATURE_NOT_SUPPORTED;
               codecSupport.hevc_support.hevc_features.bits.temporal_mvp = PIPE_ENC_FEATURE_NOT_SUPPORTED;
               codecSupport.hevc_support.hevc_features.bits.strong_intra_smoothing = PIPE_ENC_FEATURE_NOT_SUPPORTED;
               codecSupport.hevc_support.hevc_features.bits.dependent_slices = PIPE_ENC_FEATURE_NOT_SUPPORTED;
               codecSupport.hevc_support.hevc_features.bits.sign_data_hiding = PIPE_ENC_FEATURE_NOT_SUPPORTED;
               codecSupport.hevc_support.hevc_features.bits.weighted_prediction = PIPE_ENC_FEATURE_NOT_SUPPORTED;
               codecSupport.hevc_support.hevc_features.bits.transquant_bypass = PIPE_ENC_FEATURE_NOT_SUPPORTED;
               codecSupport.hevc_support.hevc_features.bits.deblocking_filter_disable = PIPE_ENC_FEATURE_NOT_SUPPORTED;

               /* cu_qp_delta always required to be 1 in https://github.com/microsoft/DirectX-Specs/blob/master/d3d/D3D12VideoEncoding.md */
               codecSupport.hevc_support.hevc_features.bits.cu_qp_delta = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);

               if ((codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_P_FRAMES_IMPLEMENTED_AS_LOW_DELAY_B_FRAMES) != 0)
                  codecSupport.hevc_support.prediction_direction |= PIPE_H265_PRED_DIRECTION_BI_NOT_EMPTY;

               if ((codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_ASYMETRIC_MOTION_PARTITION_SUPPORT) != 0)
                  codecSupport.hevc_support.hevc_features.bits.amp = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_ASYMETRIC_MOTION_PARTITION_REQUIRED) != 0)
                  codecSupport.hevc_support.hevc_features.bits.amp = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);

               if ((codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_SAO_FILTER_SUPPORT) != 0)
                  codecSupport.hevc_support.hevc_features.bits.sao = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_CONSTRAINED_INTRAPREDICTION_SUPPORT) != 0)
                  codecSupport.hevc_support.hevc_features.bits.constrained_intra_pred = PIPE_ENC_FEATURE_SUPPORTED;
               if ((codecSupport.hevc_support.d3d12_caps.SupportFlags & D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_HEVC_FLAG_TRANSFORM_SKIP_SUPPORT) != 0)
                  codecSupport.hevc_support.hevc_features.bits.transform_skip = PIPE_ENC_FEATURE_SUPPORTED;

               DXGI_FORMAT encodeFormat = d3d12_convert_pipe_video_profile_to_dxgi_format(profile);
               supportsProfile = supportsProfile &&
                                 d3d12_video_encode_supported_resolution_range(codecDesc, minRes, maxRes, alignRes, spD3D12VideoDevice.Get());

#if D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE
               D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT2 capEncoderSupportData1 = {};
               D3D12_FEATURE_DATA_VIDEO_ENCODER_RESOLUTION_SUPPORT_LIMITS1 resolutionDepCaps;
#else
               D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1 capEncoderSupportData1 = {};
               D3D12_FEATURE_DATA_VIDEO_ENCODER_RESOLUTION_SUPPORT_LIMITS resolutionDepCaps;
#endif
               capEncoderSupportData1.SubregionFrameEncoding = (supportedSliceStructures == PIPE_VIDEO_CAP_SLICE_STRUCTURE_NONE) ?
                                                                D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_FULL_FRAME :
                                                                D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_UNIFORM_PARTITIONING_SUBREGIONS_PER_FRAME;
               D3D12_VIDEO_ENCODER_PICTURE_CONTROL_SUBREGIONS_LAYOUT_DATA_SLICES sliceData = { };
               capEncoderSupportData1.SubregionFrameEncodingData.DataSize = sizeof(sliceData);
               capEncoderSupportData1.SubregionFrameEncodingData.pSlicesPartition_HEVC = &sliceData;
               d3d12_encode_support_cap_allocations cap_allocations = {};
               supportsProfile = supportsProfile && d3d12_video_encode_support_caps(codecDesc,
                                                                                    maxRes,
                                                                                    encodeFormat,
                                                                                    spD3D12VideoDevice.Get(),
                                                                                    d3d12_codec_support,
                                                                                    capEncoderSupportData1,
                                                                                    resolutionDepCaps,
                                                                                    maxQualityLevels,
                                                                                    cap_allocations);
               bVideoEncodeRequiresTextureArray = (capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_RECONSTRUCTED_FRAMES_REQUIRE_TEXTURE_ARRAYS) != 0;
               if (supportedSliceStructures == PIPE_VIDEO_CAP_SLICE_STRUCTURE_NONE)
                  maxSlices = 0;
               else
                  maxSlices = resolutionDepCaps.MaxSubregionsNumber;

               maxIRDuration = resolutionDepCaps.MaxIntraRefreshFrameDuration;
               isRCMaxFrameSizeSupported = ((capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_RATE_CONTROL_MAX_FRAME_SIZE_AVAILABLE) != 0) ? 1 : 0;

               memset(&roi_support, 0, sizeof(roi_support));
               roi_support.bits.roi_rc_qp_delta_support = ((capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_RATE_CONTROL_DELTA_QP_AVAILABLE) != 0) ? 1 : 0;
               roi_support.bits.num_roi_regions = roi_support.bits.roi_rc_qp_delta_support ? PIPE_ENC_ROI_REGION_NUM_MAX : 0;
               roi_support.bits.log2_roi_min_block_pixel_size = static_cast<uint32_t>(std::log2(capEncoderSupportData1.pResolutionDependentSupport[0].QPMapRegionPixelsSize));

#if D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE

               D3D12_VIDEO_ENCODER_INPUT_MAP_SESSION_INFO sessionInfo =
               {
                  // D3D12_VIDEO_ENCODER_CODEC Codec;
                  capEncoderSupportData1.Codec,
                  // D3D12_VIDEO_ENCODER_PROFILE_DESC Profile;
                  profDesc,
                  // D3D12_VIDEO_ENCODER_LEVEL_SETTING Level;
                  maxLvl,
                  // DXGI_FORMAT InputFormat;
                  capEncoderSupportData1.InputFormat,
                  // D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC InputResolution;
                  capEncoderSupportData1.pResolutionList[0],
                  // D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION CodecConfiguration;
                  capEncoderSupportData1.CodecConfiguration,
                  // D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE SubregionFrameEncoding;
                  capEncoderSupportData1.SubregionFrameEncoding,
                  // D3D12_VIDEO_ENCODER_PICTURE_CONTROL_SUBREGIONS_LAYOUT_DATA SubregionFrameEncodingData;
                  capEncoderSupportData1.SubregionFrameEncodingData,
               };

               dirty_rects_support = get_dirty_rects_support(sessionInfo, spD3D12VideoDevice.Get(), D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_CPU_BUFFER);
               dirty_rects_support_gpu = get_dirty_rects_support(sessionInfo, spD3D12VideoDevice.Get(), D3D12_VIDEO_ENCODER_INPUT_MAP_SOURCE_GPU_TEXTURE);
               move_rects_support = get_move_rects_support(sessionInfo, spD3D12VideoDevice.Get());
               get_gpu_output_stats_support(sessionInfo, capEncoderSupportData1.SupportFlags, spD3D12VideoDevice.Get(), gpu_stats_qp, gpu_stats_satd, gpu_stats_rcbits, psnr_support);
               sliced_encode_support = get_sliced_encode_support(capEncoderSupportData1.SupportFlags);
               gpu_motion_input_support = get_motion_gpuinput_support(sessionInfo, spD3D12VideoDevice.Get());
               two_pass_support = query_two_pass_support(pscreen, sessionInfo, spD3D12VideoDevice.Get(),
                                                         ((capEncoderSupportData1.SupportFlags &
                                                            D3D12_VIDEO_ENCODER_SUPPORT_FLAG_READABLE_RECONSTRUCTED_PICTURE_LAYOUT_AVAILABLE) != 0));
#endif
            }
         }
      } break;
#endif
#if VIDEO_CODEC_AV1ENC
      case PIPE_VIDEO_PROFILE_AV1_MAIN:
      {
         D3D12_VIDEO_ENCODER_PROFILE_DESC profDesc = {};
         D3D12_VIDEO_ENCODER_AV1_PROFILE profAV1 =
            d3d12_video_encoder_convert_profile_to_d3d12_enc_profile_av1(profile);
         profDesc.DataSize = sizeof(profAV1);
         profDesc.pAV1Profile = &profAV1;
         D3D12_VIDEO_ENCODER_CODEC codecDesc = d3d12_video_encoder_convert_codec_to_d3d12_enc_codec(profile);
         D3D12_VIDEO_ENCODER_AV1_LEVEL_TIER_CONSTRAINTS minLvlSettingAV1 = { };
         D3D12_VIDEO_ENCODER_AV1_LEVEL_TIER_CONSTRAINTS maxLvlSettingAV1 = { };
         D3D12_VIDEO_ENCODER_LEVEL_SETTING minLvl = {};
         D3D12_VIDEO_ENCODER_LEVEL_SETTING maxLvl = {};
         minLvl.pAV1LevelSetting = &minLvlSettingAV1;
         minLvl.DataSize = sizeof(minLvlSettingAV1);
         maxLvl.pAV1LevelSetting = &maxLvlSettingAV1;
         maxLvl.DataSize = sizeof(maxLvlSettingAV1);
         if (d3d12_video_encode_max_supported_level_for_profile(codecDesc,
                                                                profDesc,
                                                                minLvl,
                                                                maxLvl,
                                                                spD3D12VideoDevice.Get())) {
            d3d12_video_encoder_convert_d3d12_to_spec_level_av1(maxLvlSettingAV1.Level, maxLvlSpec);

            D3D12_VIDEO_ENCODER_PROFILE_DESC d3d12_profile;
            d3d12_profile.pAV1Profile = &profAV1;
            d3d12_profile.DataSize = sizeof(profAV1);

            maxReferencesPerFrame =
               d3d12_video_encode_supported_references_per_frame_structures(codecDesc,
                                                                            d3d12_profile,
                                                                            spD3D12VideoDevice.Get(),
                                                                            codecSupport,
                                                                            maxLongTermReferences,
                                                                            maxDPBCapacity);

            supportsProfile = d3d12_video_encode_get_av1_codec_support(codecDesc,
                                                                        profDesc,
                                                                        spD3D12VideoDevice.Get(),
                                                                        codecSupport.av1_support.d3d12_caps);
            if (supportsProfile) {
               d3d12_codec_support.DataSize = sizeof(codecSupport.av1_support.d3d12_caps);
               d3d12_codec_support.pAV1Support = &codecSupport.av1_support.d3d12_caps;

               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_128x128_SUPERBLOCK) != 0)
                  codecSupport.av1_support.features.bits.support_128x128_superblock = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_128x128_SUPERBLOCK) != 0)
                  codecSupport.av1_support.features.bits.support_128x128_superblock = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);
               
               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_FILTER_INTRA) != 0)
                  codecSupport.av1_support.features.bits.support_filter_intra = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_FILTER_INTRA) != 0)
                  codecSupport.av1_support.features.bits.support_filter_intra = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);   
                  
               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_INTRA_EDGE_FILTER) != 0)
                  codecSupport.av1_support.features.bits.support_intra_edge_filter = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_INTRA_EDGE_FILTER) != 0)
                  codecSupport.av1_support.features.bits.support_intra_edge_filter = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);   
               
               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_INTERINTRA_COMPOUND) != 0)
                  codecSupport.av1_support.features.bits.support_interintra_compound = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_INTERINTRA_COMPOUND) != 0)
                  codecSupport.av1_support.features.bits.support_interintra_compound = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);   
               
               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_MASKED_COMPOUND) != 0)
                  codecSupport.av1_support.features.bits.support_masked_compound = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_MASKED_COMPOUND) != 0)
                  codecSupport.av1_support.features.bits.support_masked_compound = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);                  

               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_WARPED_MOTION) != 0)
                  codecSupport.av1_support.features.bits.support_warped_motion = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_WARPED_MOTION) != 0)
                  codecSupport.av1_support.features.bits.support_warped_motion = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);    
               
               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_PALETTE_ENCODING) != 0)
                  codecSupport.av1_support.features.bits.support_palette_mode = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_PALETTE_ENCODING) != 0)
                  codecSupport.av1_support.features.bits.support_palette_mode = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);    

               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_DUAL_FILTER) != 0)
                  codecSupport.av1_support.features.bits.support_dual_filter = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_DUAL_FILTER) != 0)
                  codecSupport.av1_support.features.bits.support_dual_filter = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);    
                  
               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_JNT_COMP) != 0)
                  codecSupport.av1_support.features.bits.support_jnt_comp = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_JNT_COMP) != 0)
                  codecSupport.av1_support.features.bits.support_jnt_comp = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);    

               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_FRAME_REFERENCE_MOTION_VECTORS) != 0)
                  codecSupport.av1_support.features.bits.support_ref_frame_mvs = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_FRAME_REFERENCE_MOTION_VECTORS) != 0)
                  codecSupport.av1_support.features.bits.support_ref_frame_mvs = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);    

               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_SUPER_RESOLUTION) != 0)
                  codecSupport.av1_support.features.bits.support_superres = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_SUPER_RESOLUTION) != 0)
                  codecSupport.av1_support.features.bits.support_superres = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);    
               
               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_LOOP_RESTORATION_FILTER) != 0)
                  codecSupport.av1_support.features.bits.support_restoration = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_LOOP_RESTORATION_FILTER) != 0)
                  codecSupport.av1_support.features.bits.support_restoration = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);    

               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_INTRA_BLOCK_COPY) != 0)
                  codecSupport.av1_support.features.bits.support_allow_intrabc = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_INTRA_BLOCK_COPY) != 0)
                  codecSupport.av1_support.features.bits.support_allow_intrabc = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);    

               if ((codecSupport.av1_support.d3d12_caps.SupportedFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_CDEF_FILTERING) != 0)
                  codecSupport.av1_support.features.bits.support_cdef_channel_strength = PIPE_ENC_FEATURE_SUPPORTED;

               if ((codecSupport.av1_support.d3d12_caps.RequiredFeatureFlags & D3D12_VIDEO_ENCODER_AV1_FEATURE_FLAG_CDEF_FILTERING) != 0)
                  codecSupport.av1_support.features.bits.support_cdef_channel_strength = (PIPE_ENC_FEATURE_SUPPORTED | PIPE_ENC_FEATURE_REQUIRED);   

               // pipe_av1_enc_cap_features_ext1
               if ((codecSupport.av1_support.d3d12_caps.SupportedInterpolationFilters & D3D12_VIDEO_ENCODER_AV1_INTERPOLATION_FILTERS_FLAG_EIGHTTAP) != 0)
                  codecSupport.av1_support.features_ext1.bits.interpolation_filter |= PIPE_VIDEO_CAP_ENC_AV1_INTERPOLATION_FILTER_EIGHT_TAP;

               if ((codecSupport.av1_support.d3d12_caps.SupportedInterpolationFilters & D3D12_VIDEO_ENCODER_AV1_INTERPOLATION_FILTERS_FLAG_EIGHTTAP_SMOOTH) != 0)
                  codecSupport.av1_support.features_ext1.bits.interpolation_filter |= PIPE_VIDEO_CAP_ENC_AV1_INTERPOLATION_FILTER_EIGHT_TAP_SMOOTH;

               if ((codecSupport.av1_support.d3d12_caps.SupportedInterpolationFilters & D3D12_VIDEO_ENCODER_AV1_INTERPOLATION_FILTERS_FLAG_EIGHTTAP_SHARP) != 0)
                  codecSupport.av1_support.features_ext1.bits.interpolation_filter |= PIPE_VIDEO_CAP_ENC_AV1_INTERPOLATION_FILTER_EIGHT_TAP_SHARP;
               
               if ((codecSupport.av1_support.d3d12_caps.SupportedInterpolationFilters & D3D12_VIDEO_ENCODER_AV1_INTERPOLATION_FILTERS_FLAG_BILINEAR) != 0)
                  codecSupport.av1_support.features_ext1.bits.interpolation_filter |= PIPE_VIDEO_CAP_ENC_AV1_INTERPOLATION_FILTER_BILINEAR;
               
               if ((codecSupport.av1_support.d3d12_caps.SupportedInterpolationFilters & D3D12_VIDEO_ENCODER_AV1_INTERPOLATION_FILTERS_FLAG_SWITCHABLE) != 0)
                  codecSupport.av1_support.features_ext1.bits.interpolation_filter |= PIPE_VIDEO_CAP_ENC_AV1_INTERPOLATION_FILTER_SWITCHABLE;

               if ((codecSupport.av1_support.d3d12_caps.SupportedSegmentationModes & D3D12_VIDEO_ENCODER_AV1_SEGMENTATION_MODE_FLAG_DISABLED) != 0)
                  codecSupport.av1_support.features_ext1.bits.segment_feature_support = 0;

               if ((codecSupport.av1_support.d3d12_caps.SupportedSegmentationModes & D3D12_VIDEO_ENCODER_AV1_SEGMENTATION_MODE_FLAG_ALT_Q) != 0)
                  codecSupport.av1_support.features_ext1.bits.segment_feature_support |= 0x1;

               if ((codecSupport.av1_support.d3d12_caps.SupportedSegmentationModes & D3D12_VIDEO_ENCODER_AV1_SEGMENTATION_MODE_FLAG_ALT_LF_Y_V) != 0)
                  codecSupport.av1_support.features_ext1.bits.segment_feature_support |= 0x2;

               if ((codecSupport.av1_support.d3d12_caps.SupportedSegmentationModes & D3D12_VIDEO_ENCODER_AV1_SEGMENTATION_MODE_FLAG_ALT_LF_Y_H) != 0)
                  codecSupport.av1_support.features_ext1.bits.segment_feature_support |= 0x4;

               if ((codecSupport.av1_support.d3d12_caps.SupportedSegmentationModes & D3D12_VIDEO_ENCODER_AV1_SEGMENTATION_MODE_FLAG_ALT_LF_U) != 0)
                  codecSupport.av1_support.features_ext1.bits.segment_feature_support |= 0x8;

               if ((codecSupport.av1_support.d3d12_caps.SupportedSegmentationModes & D3D12_VIDEO_ENCODER_AV1_SEGMENTATION_MODE_FLAG_ALT_LF_V) != 0)
                  codecSupport.av1_support.features_ext1.bits.segment_feature_support |= 0x10;

               if ((codecSupport.av1_support.d3d12_caps.SupportedSegmentationModes & D3D12_VIDEO_ENCODER_AV1_SEGMENTATION_MODE_FLAG_REF_FRAME) != 0)
                  codecSupport.av1_support.features_ext1.bits.segment_feature_support |= 0x20;

               if ((codecSupport.av1_support.d3d12_caps.SupportedSegmentationModes & D3D12_VIDEO_ENCODER_AV1_SEGMENTATION_MODE_FLAG_ALT_SKIP) != 0)
                  codecSupport.av1_support.features_ext1.bits.segment_feature_support |= 0x40;

               if ((codecSupport.av1_support.d3d12_caps.SupportedSegmentationModes & D3D12_VIDEO_ENCODER_AV1_SEGMENTATION_MODE_FLAG_ALT_GLOBALMV) != 0)
                  codecSupport.av1_support.features_ext1.bits.segment_feature_support |= 0x80;

               // pipe_av1_enc_cap_features_ext2
               codecSupport.av1_support.features_ext2.bits.obu_size_bytes_minus1 = 4 - 1; // Default 4 bytes (reported minus 1)

               // tx_mode_support query cap
               {
                  // libva tx_mode_support, PIPE_XX and D3D12 flags are defined with the same numerical values.
                  static_assert(static_cast<uint32_t>(D3D12_VIDEO_ENCODER_AV1_TX_MODE_FLAG_SELECT) ==
                     static_cast<uint32_t>(PIPE_VIDEO_CAP_ENC_AV1_TX_MODE_SELECT));
                  static_assert(static_cast<uint32_t>(D3D12_VIDEO_ENCODER_AV1_TX_MODE_FLAG_LARGEST) ==
                     static_cast<uint32_t>(PIPE_VIDEO_CAP_ENC_AV1_TX_MODE_LARGEST));
                  static_assert(static_cast<uint32_t>(D3D12_VIDEO_ENCODER_AV1_TX_MODE_FLAG_ONLY4x4) ==
                     static_cast<uint32_t>(PIPE_VIDEO_CAP_ENC_AV1_TX_MODE_ONLY_4X4));

                  // Iterate over the tx_modes and generate the D3D12_VIDEO_ENCODER_AV1_TX_MODE_FLAGS d3d12SupportFlag
                  for(uint8_t i = D3D12_VIDEO_ENCODER_AV1_TX_MODE_ONLY4x4; i <= D3D12_VIDEO_ENCODER_AV1_TX_MODE_SELECT; i++)
                  {
                     uint32_t d3d12SupportFlag = (1 << i); // See definition of D3D12_VIDEO_ENCODER_AV1_TX_MODE_FLAGS
                     // Check the current d3d12SupportFlag (ie. D3D12_VIDEO_ENCODER_AV1_TX_MODE_FLAG_XXX) is supported for all frame types
                     bool tx_mode_supported = true;
                     for(uint8_t j = D3D12_VIDEO_ENCODER_AV1_FRAME_TYPE_KEY_FRAME; j <= D3D12_VIDEO_ENCODER_AV1_FRAME_TYPE_SWITCH_FRAME; j++)
                     {
                        // Check frame supported by picture control caps, otherwise don't check against this frame type
                        if(codecSupport.av1_support.d3d12_picture_control.SupportedFrameTypes & (1 << j /* See D3D12_VIDEO_ENCODER_AV1_FRAME_TYPE_FLAGS */))
                           tx_mode_supported &= ((codecSupport.av1_support.d3d12_caps.SupportedTxModes[j] & d3d12SupportFlag) != 0);
                     }

                     // When supported for all frames, report it as part of the bitmask
                     if (tx_mode_supported)
                        codecSupport.av1_support.features_ext2.bits.tx_mode_support |= d3d12SupportFlag;
                  }

                  // As per d3d12 spec, driver must support at least one default mode for all frame types
                  // Workaround for mismatch between VAAPI/D3D12 and TxMode support for all/some frame types
                  if (!codecSupport.av1_support.features_ext2.bits.tx_mode_support)
                  {
                     debug_printf("[d3d12_has_video_encode_support] Reporting features_ext2.bits.tx_mode_support = D3D12_VIDEO_ENCODER_AV1_TX_MODE_FLAG_SELECT"
                     " due to mismatch between D3D12/VAAPI TxMode support semantic");
                     codecSupport.av1_support.features_ext2.bits.tx_mode_support = D3D12_VIDEO_ENCODER_AV1_TX_MODE_FLAG_SELECT;
                  }
               }

               supportsProfile = supportsProfile &&
                                 d3d12_video_encode_supported_resolution_range(codecDesc, minRes, maxRes, alignRes, spD3D12VideoDevice.Get());

               D3D12_VIDEO_ENCODER_AV1_FRAME_SUBREGION_LAYOUT_CONFIG_SUPPORT av1TileSupport = {};
               d3d12_video_encode_supported_tile_structures(codecDesc,
                                                            profDesc,
                                                            maxLvl,
                                                            spD3D12VideoDevice.Get(),
                                                            maxRes,
                                                            supportedSliceStructures,
                                                            av1TileSupport // out
                                                         );
               // Cannot pass pipe 2 bit-field as reference, use aux variable instead.
               codecSupport.av1_support.features_ext2.bits.tile_size_bytes_minus1 = av1TileSupport.TileSizeBytesMinus1;
               max_tile_rows = av1TileSupport.MaxTileRows;
               max_tile_cols = av1TileSupport.MaxTileCols;

               DXGI_FORMAT encodeFormat = d3d12_convert_pipe_video_profile_to_dxgi_format(profile);

#if D3D12_VIDEO_USE_NEW_ENCODECMDLIST4_INTERFACE
               D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT2 capEncoderSupportData1 = {};
               D3D12_FEATURE_DATA_VIDEO_ENCODER_RESOLUTION_SUPPORT_LIMITS1 resolutionDepCaps;
#else
               D3D12_FEATURE_DATA_VIDEO_ENCODER_SUPPORT1 capEncoderSupportData1 = {};
               D3D12_FEATURE_DATA_VIDEO_ENCODER_RESOLUTION_SUPPORT_LIMITS resolutionDepCaps;
#endif
               capEncoderSupportData1.SubregionFrameEncoding = (supportedSliceStructures == PIPE_VIDEO_CAP_SLICE_STRUCTURE_NONE) ?
                                                                D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_FULL_FRAME :
                                                                D3D12_VIDEO_ENCODER_FRAME_SUBREGION_LAYOUT_MODE_UNIFORM_GRID_PARTITION;

               capEncoderSupportData1.SubregionFrameEncodingData.DataSize = sizeof(av1TileSupport.TilesConfiguration);
               capEncoderSupportData1.SubregionFrameEncodingData.pTilesPartition_AV1 = &av1TileSupport.TilesConfiguration;
               d3d12_encode_support_cap_allocations cap_allocations = {};
               supportsProfile = supportsProfile && d3d12_video_encode_support_caps(codecDesc,
                                                                                    maxRes,
                                                                                    encodeFormat,
                                                                                    spD3D12VideoDevice.Get(),
                                                                                    d3d12_codec_support,
                                                                                    capEncoderSupportData1,
                                                                                    resolutionDepCaps,
                                                                                    maxQualityLevels,
                                                                                    cap_allocations);
               bVideoEncodeRequiresTextureArray = (capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_RECONSTRUCTED_FRAMES_REQUIRE_TEXTURE_ARRAYS) != 0;                                                                                 
               if (supportedSliceStructures == PIPE_VIDEO_CAP_SLICE_STRUCTURE_NONE)
                  maxSlices = 0;
               else
                  maxSlices = resolutionDepCaps.MaxSubregionsNumber;

               maxIRDuration = resolutionDepCaps.MaxIntraRefreshFrameDuration;
               codecSupport.av1_support.features_ext2.bits.max_tile_num_minus1 = maxSlices - 1;

               isRCMaxFrameSizeSupported = ((capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_RATE_CONTROL_MAX_FRAME_SIZE_AVAILABLE) != 0) ? 1 : 0;
               memset(&roi_support, 0, sizeof(roi_support));
               roi_support.bits.roi_rc_qp_delta_support = ((capEncoderSupportData1.SupportFlags & D3D12_VIDEO_ENCODER_SUPPORT_FLAG_RATE_CONTROL_DELTA_QP_AVAILABLE) != 0) ? 1 : 0;
               roi_support.bits.num_roi_regions = roi_support.bits.roi_rc_qp_delta_support ? PIPE_ENC_ROI_REGION_NUM_MAX : 0;
               roi_support.bits.log2_roi_min_block_pixel_size = static_cast<uint32_t>(std::log2(capEncoderSupportData1.pResolutionDependentSupport[0].QPMapRegionPixelsSize));
            }
         }
      } break;
#endif // #if VIDEO_CODEC_AV1ENC
      default:
         supportsProfile = false;
   }

   return VideoFeatureAreaSupport.VideoEncodeSupport && supportsProfile;
}

static int
d3d12_screen_get_video_param_decode(struct pipe_screen *pscreen,
                                    enum pipe_video_profile profile,
                                    enum pipe_video_entrypoint entrypoint,
                                    enum pipe_video_cap param)
{
   switch (param) {
      case PIPE_VIDEO_CAP_REQUIRES_FLUSH_ON_END_FRAME:
         /* As sometimes we need to copy the output
            and sync with the context, we handle the
            flush internally on end frame for decode
         */
         return 0;
      case PIPE_VIDEO_CAP_NPOT_TEXTURES:
         return 1;
      case PIPE_VIDEO_CAP_MAX_WIDTH:
      case PIPE_VIDEO_CAP_MAX_HEIGHT:
      case PIPE_VIDEO_CAP_MAX_LEVEL:
      case PIPE_VIDEO_CAP_MIN_WIDTH:
      case PIPE_VIDEO_CAP_MIN_HEIGHT:
      case PIPE_VIDEO_CAP_SUPPORTED:
      {
         if (d3d12_has_video_decode_support(pscreen, profile)) {
            DXGI_FORMAT format = d3d12_convert_pipe_video_profile_to_dxgi_format(profile);
            auto pipeFmt = d3d12_get_pipe_format(format);
            bool formatSupported = pscreen->is_video_format_supported(pscreen, pipeFmt, profile, entrypoint);
            if (formatSupported) {
               GUID decodeGUID = d3d12_video_decoder_convert_pipe_video_profile_to_d3d12_profile(profile);
               GUID emptyGUID = {};
               if (decodeGUID != emptyGUID) {
                  bool supportAny = false;
                  D3D12_FEATURE_DATA_VIDEO_DECODE_SUPPORT outSupportedConfig = {};
                  D3D12_VIDEO_DECODE_CONFIGURATION decoderConfig = { decodeGUID,
                                                                     D3D12_BITSTREAM_ENCRYPTION_TYPE_NONE,
                                                                     D3D12_VIDEO_FRAME_CODED_INTERLACE_TYPE_NONE };

                  d3d12_video_resolution_to_level_mapping_entry lowestSupportedConfig = {};
                  d3d12_video_resolution_to_level_mapping_entry bestSupportedConfig = {};
                     get_level_resolution_video_decode_support(decoderConfig,
                                                                   format,
                                                                   pscreen,
                                                                   supportAny,
                                                                   outSupportedConfig,
                                                                   bestSupportedConfig,
                                                                   lowestSupportedConfig);
                  if (supportAny) {
                     if (param == PIPE_VIDEO_CAP_MAX_WIDTH) {
                        return bestSupportedConfig.resolution.Width;
                     } else if (param == PIPE_VIDEO_CAP_MAX_HEIGHT) {
                        return bestSupportedConfig.resolution.Height;
                     } else if (param == PIPE_VIDEO_CAP_MIN_WIDTH) {
                        return lowestSupportedConfig.resolution.Width;
                     } else if (param == PIPE_VIDEO_CAP_MIN_HEIGHT) {
                        return lowestSupportedConfig.resolution.Height;
                     } else if (param == PIPE_VIDEO_CAP_MAX_LEVEL) {
                        return bestSupportedConfig.level;
                     } else if (param == PIPE_VIDEO_CAP_SUPPORTED) {
                        return 1;
                     }
                  }
               }
            }
         }
         return 0;
      } break;
      case PIPE_VIDEO_CAP_PREFERRED_FORMAT:
         return (profile == PIPE_VIDEO_PROFILE_UNKNOWN) ? PIPE_FORMAT_NV12 : d3d12_get_pipe_format(d3d12_convert_pipe_video_profile_to_dxgi_format(profile));
      case PIPE_VIDEO_CAP_PREFERS_INTERLACED:
         return false;
      case PIPE_VIDEO_CAP_SUPPORTS_INTERLACED:
         return true;
      case PIPE_VIDEO_CAP_SUPPORTS_PROGRESSIVE:
         return true;
      case PIPE_VIDEO_CAP_SUPPORTS_CONTIGUOUS_PLANES_MAP:
         return true;
      default:
         debug_printf("[d3d12_screen_get_video_param] unknown video param: %d\n", param);
         return 0;
   }
}

static int
d3d12_screen_get_video_param_postproc(struct pipe_screen *pscreen,
                                    enum pipe_video_profile profile,
                                    enum pipe_video_entrypoint entrypoint,
                                    enum pipe_video_cap param)
{
   switch (param) {
      case PIPE_VIDEO_CAP_REQUIRES_FLUSH_ON_END_FRAME:
         return 1;
      case PIPE_VIDEO_CAP_NPOT_TEXTURES:
         return 1;
      case PIPE_VIDEO_CAP_MAX_WIDTH:
      case PIPE_VIDEO_CAP_MAX_HEIGHT:
      case PIPE_VIDEO_CAP_MIN_WIDTH:
      case PIPE_VIDEO_CAP_MIN_HEIGHT:
      case PIPE_VIDEO_CAP_SUPPORTED:
      case PIPE_VIDEO_CAP_PREFERRED_FORMAT:
      case PIPE_VIDEO_CAP_SUPPORTS_INTERLACED:
      case PIPE_VIDEO_CAP_SUPPORTS_PROGRESSIVE:
      case PIPE_VIDEO_CAP_SUPPORTS_CONTIGUOUS_PLANES_MAP:
      case PIPE_VIDEO_CAP_VPP_MAX_INPUT_WIDTH:
      case PIPE_VIDEO_CAP_VPP_MAX_INPUT_HEIGHT:
      case PIPE_VIDEO_CAP_VPP_MIN_INPUT_WIDTH:
      case PIPE_VIDEO_CAP_VPP_MIN_INPUT_HEIGHT:
      case PIPE_VIDEO_CAP_VPP_MAX_OUTPUT_WIDTH:
      case PIPE_VIDEO_CAP_VPP_MAX_OUTPUT_HEIGHT:
      case PIPE_VIDEO_CAP_VPP_MIN_OUTPUT_WIDTH:
      case PIPE_VIDEO_CAP_VPP_MIN_OUTPUT_HEIGHT:
      case PIPE_VIDEO_CAP_VPP_ORIENTATION_MODES:
      case PIPE_VIDEO_CAP_VPP_BLEND_MODES:
      {
         // Assume defaults for now, we don't have the input args passed by get_video_param to be accurate here.
         const D3D12_VIDEO_FIELD_TYPE FieldType = D3D12_VIDEO_FIELD_TYPE_NONE;
         const D3D12_VIDEO_FRAME_STEREO_FORMAT StereoFormat = D3D12_VIDEO_FRAME_STEREO_FORMAT_NONE;
         const DXGI_RATIONAL FrameRate = { 30, 1 };
         const DXGI_FORMAT InputFormat = DXGI_FORMAT_NV12;
         const DXGI_COLOR_SPACE_TYPE InputColorSpace = DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709;
         const DXGI_FORMAT OutputFormat = DXGI_FORMAT_NV12;
         const DXGI_COLOR_SPACE_TYPE OutputColorSpace = DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709;
         const UINT Width = 1280;
         const UINT Height = 720;
         D3D12_FEATURE_DATA_VIDEO_PROCESS_SUPPORT supportCaps =
         {
            0, // NodeIndex
            { Width, Height, { InputFormat, InputColorSpace } },
            FieldType,
            StereoFormat,
            FrameRate,
            { OutputFormat, OutputColorSpace },
            StereoFormat,
            FrameRate,
         };

         D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC minSupportedInput = {};
         D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC maxSupportedInput = {};
         if (d3d12_has_video_process_support(pscreen, supportCaps, minSupportedInput, maxSupportedInput)) {
            if (param == PIPE_VIDEO_CAP_SUPPORTED) {
               return true;
            } else if (param == PIPE_VIDEO_CAP_PREFERRED_FORMAT) {
               return  PIPE_FORMAT_NV12;
            } else if (param == PIPE_VIDEO_CAP_SUPPORTS_INTERLACED) {
               return false;
            } else if (param == PIPE_VIDEO_CAP_MIN_WIDTH) {
               return minSupportedInput.Width;
            } else if (param == PIPE_VIDEO_CAP_MIN_HEIGHT) {
               return minSupportedInput.Height;
            } else if (param == PIPE_VIDEO_CAP_MAX_WIDTH) {
               return maxSupportedInput.Width;
            } else if (param == PIPE_VIDEO_CAP_MAX_HEIGHT) {
               return maxSupportedInput.Height;
            } else if (param == PIPE_VIDEO_CAP_SUPPORTS_CONTIGUOUS_PLANES_MAP) {
               return true;
            } else if (param == PIPE_VIDEO_CAP_SUPPORTS_PROGRESSIVE) {
               return true;
            } else if (param == PIPE_VIDEO_CAP_VPP_MAX_INPUT_WIDTH) {
               return supportCaps.ScaleSupport.OutputSizeRange.MaxWidth;
            } else if (param == PIPE_VIDEO_CAP_VPP_MAX_INPUT_HEIGHT) {
               return supportCaps.ScaleSupport.OutputSizeRange.MaxHeight;
            } else if (param == PIPE_VIDEO_CAP_VPP_MIN_INPUT_WIDTH) {
               return supportCaps.ScaleSupport.OutputSizeRange.MinWidth;
            } else if (param == PIPE_VIDEO_CAP_VPP_MIN_INPUT_HEIGHT) {
               return supportCaps.ScaleSupport.OutputSizeRange.MinHeight;
            } else if (param == PIPE_VIDEO_CAP_VPP_MAX_OUTPUT_WIDTH) {
               return supportCaps.ScaleSupport.OutputSizeRange.MaxWidth;
            } else if (param == PIPE_VIDEO_CAP_VPP_MAX_OUTPUT_HEIGHT) {
               return supportCaps.ScaleSupport.OutputSizeRange.MaxHeight;
            } else if (param == PIPE_VIDEO_CAP_VPP_MIN_OUTPUT_WIDTH) {
               return supportCaps.ScaleSupport.OutputSizeRange.MinWidth;
            } else if (param == PIPE_VIDEO_CAP_VPP_MIN_OUTPUT_HEIGHT) {
               return supportCaps.ScaleSupport.OutputSizeRange.MinHeight;
            } else if (param == PIPE_VIDEO_CAP_VPP_BLEND_MODES) {
               uint32_t blend_modes = PIPE_VIDEO_VPP_BLEND_MODE_NONE;
               if (((supportCaps.FeatureSupport & D3D12_VIDEO_PROCESS_FEATURE_FLAG_ALPHA_BLENDING) != 0)
                  && ((supportCaps.FeatureSupport & D3D12_VIDEO_PROCESS_FEATURE_FLAG_ALPHA_FILL) != 0))
                  {
                     blend_modes |= PIPE_VIDEO_VPP_BLEND_MODE_GLOBAL_ALPHA;
                  }
                  return blend_modes;
            } else if (param == PIPE_VIDEO_CAP_VPP_ORIENTATION_MODES) {
                uint32_t orientation_modes = PIPE_VIDEO_VPP_ORIENTATION_DEFAULT;
                if((supportCaps.FeatureSupport & D3D12_VIDEO_PROCESS_FEATURE_FLAG_FLIP) != 0) {
                  orientation_modes |= PIPE_VIDEO_VPP_FLIP_HORIZONTAL;
                  orientation_modes |= PIPE_VIDEO_VPP_FLIP_VERTICAL;
                }

                if((supportCaps.FeatureSupport & D3D12_VIDEO_PROCESS_FEATURE_FLAG_ROTATION) != 0) {
                  orientation_modes |= PIPE_VIDEO_VPP_ROTATION_90;
                  orientation_modes |= PIPE_VIDEO_VPP_ROTATION_180;
                  orientation_modes |= PIPE_VIDEO_VPP_ROTATION_270;
                }
                return orientation_modes;
            }
         }
         return 0;
      } break;
      default:
         return 0;
   }
}

static int
d3d12_screen_get_video_param_encode(struct pipe_screen *pscreen,
                                    enum pipe_video_profile profile,
                                    enum pipe_video_entrypoint entrypoint,
                                    enum pipe_video_cap param)
{
   bool bVideoEncodeRequiresTextureArray = false;
   uint32_t maxLvlEncode = 0u;
   D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC minResEncode = {};
   D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC maxResEncode = {};
   union pipe_enc_cap_surface_alignment alignResEncode = {};
   uint32_t maxSlices = 0u;
   uint32_t supportedSliceStructures = 0u;
   uint32_t maxReferencesPerFrame = 0u;
   uint32_t maxLongTermReferenceFrame = 0u;
   uint32_t maxDPBCapacity = 0u;
   uint32_t isRCMaxFrameSizeSupported = 0u;
   uint32_t maxQualityLevels = 0u;
   uint32_t max_tile_rows = 0u;
   uint32_t max_tile_cols = 0u;
   uint32_t maxIRDuration = 0u;
   union pipe_enc_cap_roi roi_support = {};
   union pipe_enc_cap_dirty_info dirty_rects_support = {};
   union pipe_enc_cap_dirty_info dirty_rects_support_gpu = {};
   union pipe_enc_cap_move_rect move_rects_support = {};
   struct d3d12_encode_codec_support codec_specific_support;
   union pipe_enc_cap_gpu_stats_map gpu_stats_qp = {};
   union pipe_enc_cap_gpu_stats_map gpu_stats_satd = {};
   union pipe_enc_cap_gpu_stats_map gpu_stats_rcbits = {};
   union pipe_enc_cap_sliced_notifications sliced_encode_support = {};
   union pipe_enc_cap_qpmap gpu_qpmap_input = {};
   union pipe_enc_cap_motion_vector_map gpu_motion_input = {};
   union pipe_enc_cap_two_pass two_pass_support = {};
   union pipe_enc_cap_gpu_stats_psnr psnr_support = {};
   memset(&codec_specific_support, 0, sizeof(codec_specific_support));
   switch (param) {
      case PIPE_VIDEO_CAP_REQUIRES_FLUSH_ON_END_FRAME:
         return 1;
      case PIPE_VIDEO_CAP_NPOT_TEXTURES:
         return 1;
      case PIPE_VIDEO_CAP_MAX_TEMPORAL_LAYERS:
      {
            switch (u_reduce_video_profile(profile)) {
#if VIDEO_CODEC_H264ENC
               case PIPE_VIDEO_FORMAT_MPEG4_AVC:
                  return D3D12_VIDEO_ENC_H264_MAX_TEMPORAL_LAYERS;
#endif
#if VIDEO_CODEC_H265ENC
               case PIPE_VIDEO_FORMAT_HEVC:
                  return D3D12_VIDEO_ENC_HEVC_MAX_TEMPORAL_LAYERS;
#endif
#if VIDEO_CODEC_AV1ENC
               case PIPE_VIDEO_FORMAT_AV1:
                  return D3D12_VIDEO_ENC_AV1_MAX_TEMPORAL_LAYERS;
#endif
               default:
                  UNREACHABLE("Unsupported pipe_video_format");
            }
      } break;
      case PIPE_VIDEO_CAP_ENC_SUPPORTS_FEEDBACK_METADATA:
         return (PIPE_VIDEO_FEEDBACK_METADATA_TYPE_BITSTREAM_SIZE |
                 PIPE_VIDEO_FEEDBACK_METADATA_TYPE_ENCODE_RESULT |
                 PIPE_VIDEO_FEEDBACK_METADATA_TYPE_CODEC_UNIT_LOCATION |
                 PIPE_VIDEO_FEEDBACK_METADATA_TYPE_MAX_FRAME_SIZE_OVERFLOW |
                 PIPE_VIDEO_FEEDBACK_METADATA_TYPE_MAX_SLICE_SIZE_OVERFLOW |
                 PIPE_VIDEO_FEEDBACK_METADATA_TYPE_AVERAGE_FRAME_QP);
      case PIPE_VIDEO_CAP_MAX_WIDTH:
      case PIPE_VIDEO_CAP_MAX_HEIGHT:
      case PIPE_VIDEO_CAP_MIN_WIDTH:
      case PIPE_VIDEO_CAP_MIN_HEIGHT:
      case PIPE_VIDEO_CAP_MAX_LEVEL:
      case PIPE_VIDEO_CAP_SUPPORTED:
      case PIPE_VIDEO_CAP_ENC_MAX_SLICES_PER_FRAME:
      case PIPE_VIDEO_CAP_ENC_SLICES_STRUCTURE:
      case PIPE_VIDEO_CAP_ENC_MAX_REFERENCES_PER_FRAME:
      case PIPE_VIDEO_CAP_ENC_HEVC_FEATURE_FLAGS:
      case PIPE_VIDEO_CAP_ENC_HEVC_BLOCK_SIZES:
      case PIPE_VIDEO_CAP_ENC_HEVC_PREDICTION_DIRECTION:
      case PIPE_VIDEO_CAP_ENC_AV1_FEATURE:
      case PIPE_VIDEO_CAP_ENC_AV1_FEATURE_EXT1:
      case PIPE_VIDEO_CAP_ENC_AV1_FEATURE_EXT2:
      case PIPE_VIDEO_CAP_ENC_SUPPORTS_TILE:
      case PIPE_VIDEO_CAP_ENC_SUPPORTS_MAX_FRAME_SIZE:
      case PIPE_VIDEO_CAP_ENC_QUALITY_LEVEL:
      case PIPE_VIDEO_CAP_ENC_MAX_TILE_ROWS:
      case PIPE_VIDEO_CAP_ENC_MAX_TILE_COLS:
      case PIPE_VIDEO_CAP_ENC_INTRA_REFRESH_MAX_DURATION:
      case PIPE_VIDEO_CAP_ENC_INTRA_REFRESH:
      case PIPE_VIDEO_CAP_ENC_H264_DISABLE_DBK_FILTER_MODES_SUPPORTED:
      case PIPE_VIDEO_CAP_ENC_H264_SUPPORTS_CABAC_ENCODE:
      case PIPE_VIDEO_CAP_ENC_ROI:
      case PIPE_VIDEO_CAP_ENC_SURFACE_ALIGNMENT:
      case PIPE_VIDEO_CAP_ENC_HEVC_RANGE_EXTENSION_SUPPORT:
      case PIPE_VIDEO_CAP_ENC_HEVC_RANGE_EXTENSION_FLAGS_SUPPORT:
      case PIPE_VIDEO_CAP_ENC_MAX_LONG_TERM_REFERENCES_PER_FRAME:
      case PIPE_VIDEO_CAP_ENC_MAX_DPB_CAPACITY:
      case PIPE_VIDEO_CAP_ENC_DIRTY_RECTS:
      case PIPE_VIDEO_CAP_ENC_MOVE_RECTS:
      case PIPE_VIDEO_CAP_ENC_GPU_STATS_QP_MAP:
      case PIPE_VIDEO_CAP_ENC_GPU_STATS_SATD_MAP:
      case PIPE_VIDEO_CAP_ENC_GPU_STATS_RATE_CONTROL_BITS_MAP:
      case PIPE_VIDEO_CAP_ENC_SLICED_NOTIFICATIONS:
      case PIPE_VIDEO_CAP_ENC_DIRTY_MAPS:
      case PIPE_VIDEO_CAP_ENC_QP_MAPS:
      case PIPE_VIDEO_CAP_ENC_MOTION_VECTOR_MAPS:
      case PIPE_VIDEO_CAP_ENC_TWO_PASS:
      case PIPE_VIDEO_CAP_ENC_GPU_STATS_PSNR:
      {
         if (d3d12_has_video_encode_support(pscreen,
                                            profile,
                                            maxLvlEncode,
                                            minResEncode,
                                            maxResEncode,
                                            alignResEncode,
                                            maxSlices,
                                            supportedSliceStructures,
                                            maxReferencesPerFrame,
                                            maxLongTermReferenceFrame,
                                            maxDPBCapacity,
                                            codec_specific_support,
                                            isRCMaxFrameSizeSupported,
                                            maxQualityLevels,
                                            max_tile_rows,
                                            max_tile_cols,
                                            maxIRDuration,
                                            roi_support,
                                            bVideoEncodeRequiresTextureArray,
                                            dirty_rects_support,
                                            move_rects_support,
                                            gpu_stats_qp,
                                            gpu_stats_satd,
                                            gpu_stats_rcbits,
                                            sliced_encode_support,
                                            dirty_rects_support_gpu,
                                            gpu_qpmap_input,
                                            gpu_motion_input,
                                            two_pass_support,
                                            psnr_support)) {

            DXGI_FORMAT format = d3d12_convert_pipe_video_profile_to_dxgi_format(profile);
            auto pipeFmt = d3d12_get_pipe_format(format);
            bool formatSupported = pscreen->is_video_format_supported(pscreen, pipeFmt, profile, entrypoint);
            if (formatSupported) {
               if (param == PIPE_VIDEO_CAP_MAX_WIDTH) {
                  return maxResEncode.Width;
               } else if (param == PIPE_VIDEO_CAP_MAX_HEIGHT) {
                  return maxResEncode.Height;
               } else if (param == PIPE_VIDEO_CAP_MIN_WIDTH) {
                  return minResEncode.Width;
               } else if (param == PIPE_VIDEO_CAP_MIN_HEIGHT) {
                  return minResEncode.Height;
               } else if (param == PIPE_VIDEO_CAP_MAX_LEVEL) {
                  return maxLvlEncode;
               } else if (param == PIPE_VIDEO_CAP_SUPPORTED) {
                  return 1;
               } else if (param == PIPE_VIDEO_CAP_ENC_MAX_SLICES_PER_FRAME) {
                  return maxSlices;
               } else if (param == PIPE_VIDEO_CAP_ENC_SLICES_STRUCTURE) {
                  return supportedSliceStructures;
               } else if (param == PIPE_VIDEO_CAP_ENC_MAX_TILE_ROWS) {
                  return max_tile_rows;
               } else if (param == PIPE_VIDEO_CAP_ENC_MAX_TILE_COLS) {
                  return max_tile_cols;
               } else if(param == PIPE_VIDEO_CAP_ENC_H264_DISABLE_DBK_FILTER_MODES_SUPPORTED) {
                  return codec_specific_support.h264_support.disable_dbk_filter_mode_flags;
               } else if (param == PIPE_VIDEO_CAP_ENC_MAX_REFERENCES_PER_FRAME) {
                  return maxReferencesPerFrame;
               } else if (param == PIPE_VIDEO_CAP_ENC_MAX_LONG_TERM_REFERENCES_PER_FRAME) {
                  return maxLongTermReferenceFrame;
               } else if (param == PIPE_VIDEO_CAP_ENC_MAX_DPB_CAPACITY) {
                  return maxDPBCapacity;
               } else if (param == PIPE_VIDEO_CAP_ENC_INTRA_REFRESH_MAX_DURATION) {
                  return maxIRDuration;
               } else if (param == PIPE_VIDEO_CAP_ENC_INTRA_REFRESH) {
                  return (maxIRDuration == 0) ? 0 :
                         (PIPE_VIDEO_ENC_INTRA_REFRESH_ROW |
                          PIPE_VIDEO_ENC_INTRA_REFRESH_ADAPTIVE |
                          PIPE_VIDEO_ENC_INTRA_REFRESH_CYCLIC |
                          PIPE_VIDEO_ENC_INTRA_REFRESH_P_FRAME |
                          PIPE_VIDEO_ENC_INTRA_REFRESH_B_FRAME |
                          PIPE_VIDEO_ENC_INTRA_REFRESH_MULTI_REF);
               } else if (param == PIPE_VIDEO_CAP_ENC_SUPPORTS_MAX_FRAME_SIZE) {
                  return isRCMaxFrameSizeSupported;
               } else if (param == PIPE_VIDEO_CAP_ENC_HEVC_FEATURE_FLAGS) {
                  /* get_video_param sets hevc_features.bits.config_supported = 1
                     to distinguish between supported cap with all bits off and unsupported by driver
                     with value = 0
                  */
                  return codec_specific_support.hevc_support.hevc_features.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_HEVC_BLOCK_SIZES) {
                  /* get_video_param sets hevc_block_sizes.bits.config_supported = 1
                     to distinguish between supported cap with all bits off and unsupported by driver
                     with value = 0
                  */
                  return codec_specific_support.hevc_support.hevc_block_sizes.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_HEVC_RANGE_EXTENSION_SUPPORT) {
                  return codec_specific_support.hevc_support.hevc_range_ext.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_HEVC_RANGE_EXTENSION_FLAGS_SUPPORT) {
                  return codec_specific_support.hevc_support.hevc_range_ext_flags.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_H264_SUPPORTS_CABAC_ENCODE) {
                  return (codec_specific_support.h264_support.d3d12_caps.SupportFlags &
                     D3D12_VIDEO_ENCODER_CODEC_CONFIGURATION_SUPPORT_H264_FLAG_CABAC_ENCODING_SUPPORT);
               } else if (param == PIPE_VIDEO_CAP_ENC_HEVC_PREDICTION_DIRECTION) {
                  if (PIPE_VIDEO_FORMAT_HEVC == u_reduce_video_profile(profile))
                     return codec_specific_support.hevc_support.prediction_direction;
                  return 0;
               }
               else if (param == PIPE_VIDEO_CAP_ENC_AV1_FEATURE) {
                 return codec_specific_support.av1_support.features.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_AV1_FEATURE_EXT1) {
                 return codec_specific_support.av1_support.features_ext1.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_AV1_FEATURE_EXT2) {
                 return codec_specific_support.av1_support.features_ext2.value;
               } else if(param == PIPE_VIDEO_CAP_ENC_SUPPORTS_TILE) {
                  return (profile == PIPE_VIDEO_PROFILE_AV1_MAIN) && (maxSlices != 0);
               } else if(param == PIPE_VIDEO_CAP_ENC_QUALITY_LEVEL) {
                  return maxQualityLevels;
               } else if(param == PIPE_VIDEO_CAP_ENC_ROI) {
                  assert(roi_support.bits.num_roi_regions <= PIPE_ENC_ROI_REGION_NUM_MAX);
                  return static_cast<int>(roi_support.value);
               } else if(param == PIPE_VIDEO_CAP_ENC_SURFACE_ALIGNMENT) {
                  return alignResEncode.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_DIRTY_RECTS) {
                  return dirty_rects_support.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_MOVE_RECTS) {
                  return move_rects_support.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_GPU_STATS_QP_MAP) {
                  return gpu_stats_qp.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_GPU_STATS_SATD_MAP) {
                  return gpu_stats_satd.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_GPU_STATS_RATE_CONTROL_BITS_MAP) {
                  return gpu_stats_rcbits.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_SLICED_NOTIFICATIONS) {
                  return sliced_encode_support.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_DIRTY_MAPS) {
                  return dirty_rects_support_gpu.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_QP_MAPS) {
                  return gpu_qpmap_input.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_MOTION_VECTOR_MAPS) {
                  return gpu_motion_input.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_TWO_PASS ) {
                  return two_pass_support.value;
               } else if (param == PIPE_VIDEO_CAP_ENC_GPU_STATS_PSNR ) {
                  return psnr_support.value;
               }
            }
         } else if (param == PIPE_VIDEO_CAP_ENC_QUALITY_LEVEL) {
            return 1; // This needs to be 1 for backcompat of frontend/va calling for PIPE_VIDEO_CAP_ENC_QUALITY_LEVEL > 0
         }
         return 0;
      } break;
      case PIPE_VIDEO_CAP_PREFERRED_FORMAT:
         return (profile == PIPE_VIDEO_PROFILE_UNKNOWN) ? PIPE_FORMAT_NV12 : d3d12_get_pipe_format(d3d12_convert_pipe_video_profile_to_dxgi_format(profile));
      case PIPE_VIDEO_CAP_PREFERS_INTERLACED:
         return false;
      case PIPE_VIDEO_CAP_SUPPORTS_INTERLACED:
         return false;
      case PIPE_VIDEO_CAP_SUPPORTS_PROGRESSIVE:
         return true;
      case PIPE_VIDEO_CAP_SUPPORTS_CONTIGUOUS_PLANES_MAP:
         return true;
      case PIPE_VIDEO_CAP_ENC_RATE_CONTROL_QVBR:
      {
         D3D12_FEATURE_DATA_VIDEO_ENCODER_RATE_CONTROL_MODE capRateControlModeData =
         {
            0,
            d3d12_video_encoder_convert_codec_to_d3d12_enc_codec(profile),
            D3D12_VIDEO_ENCODER_RATE_CONTROL_MODE_QVBR,
            false
         };

         ComPtr<ID3D12VideoDevice3> spD3D12VideoDevice;
         struct d3d12_screen *pD3D12Screen = (struct d3d12_screen *) pscreen;
         if (FAILED(pD3D12Screen->dev->QueryInterface(IID_PPV_ARGS(spD3D12VideoDevice.GetAddressOf())))) {
            // No video encode support in underlying d3d12 device (needs ID3D12VideoDevice3)
            return 0;
         }

         if (SUCCEEDED(spD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_RATE_CONTROL_MODE, &capRateControlModeData, sizeof(capRateControlModeData)))
            && capRateControlModeData.IsSupported)
            return 1; // Driver returns QVBR support OK

         // No QVBR support
         return 0;
      } break;
      default:
         debug_printf("[d3d12_screen_get_video_param] unknown video param: %d\n", param);
         return 0;
   }
}

bool
d3d12_video_encode_requires_texture_array_dpb(struct d3d12_screen* pScreen, enum pipe_video_profile profile)
{
   bool bVideoEncodeRequiresTextureArray = false;
   uint32_t maxLvlEncode = 0u;
   D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC minResEncode = {};
   D3D12_VIDEO_ENCODER_PICTURE_RESOLUTION_DESC maxResEncode = {};
   union pipe_enc_cap_surface_alignment alignResEncode = {};
   uint32_t maxSlices = 0u;
   uint32_t supportedSliceStructures = 0u;
   uint32_t maxReferencesPerFrame = 0u;
   uint32_t maxLongTermReferenceFrame = 0u;
   uint32_t maxDPBCapacity = 0u;
   uint32_t isRCMaxFrameSizeSupported = 0u;
   uint32_t maxQualityLevels = 0u;
   uint32_t max_tile_rows = 0u;
   uint32_t max_tile_cols = 0u;
   uint32_t maxIRDuration = 0u;
   union pipe_enc_cap_roi roi_support = {};
   struct d3d12_encode_codec_support codec_specific_support;
   memset(&codec_specific_support, 0, sizeof(codec_specific_support));
   union pipe_enc_cap_dirty_info dirty_rects_support = {};
   union pipe_enc_cap_dirty_info dirty_rects_support_gpu = {};
   union pipe_enc_cap_move_rect move_rects_support = {};
   union pipe_enc_cap_gpu_stats_map gpu_stats_qp = {};
   union pipe_enc_cap_gpu_stats_map gpu_stats_satd = {};
   union pipe_enc_cap_gpu_stats_map gpu_stats_rcbits = {};
   union pipe_enc_cap_sliced_notifications sliced_encode_support = {};
   union pipe_enc_cap_qpmap gpu_qpmap_input = {};
   union pipe_enc_cap_motion_vector_map gpu_motion_input = {};
   union pipe_enc_cap_two_pass two_pass_support = {};
   union pipe_enc_cap_gpu_stats_psnr psnr_support = {};
   if (d3d12_has_video_encode_support(&pScreen->base,
                                      profile,
                                      maxLvlEncode,
                                      minResEncode,
                                      maxResEncode,
                                      alignResEncode,
                                      maxSlices,
                                      supportedSliceStructures,
                                      maxReferencesPerFrame,
                                      maxLongTermReferenceFrame,
                                      maxDPBCapacity,
                                      codec_specific_support,
                                      isRCMaxFrameSizeSupported,
                                      maxQualityLevels,
                                      max_tile_rows,
                                      max_tile_cols,
                                      maxIRDuration,
                                      roi_support,
                                      bVideoEncodeRequiresTextureArray,
                                      dirty_rects_support,
                                      move_rects_support,
                                      gpu_stats_qp,
                                      gpu_stats_satd,
                                      gpu_stats_rcbits,
                                      sliced_encode_support,
                                      dirty_rects_support_gpu,
                                      gpu_qpmap_input,
                                      gpu_motion_input,
                                      two_pass_support,
                                      psnr_support))
   {
      return bVideoEncodeRequiresTextureArray;
   }
   return false;
}

static int
d3d12_screen_get_video_param(struct pipe_screen *pscreen,
                             enum pipe_video_profile profile,
                             enum pipe_video_entrypoint entrypoint,
                             enum pipe_video_cap param)
{
   if (entrypoint == PIPE_VIDEO_ENTRYPOINT_BITSTREAM) {
      return d3d12_screen_get_video_param_decode(pscreen, profile, entrypoint, param);
   } else if (entrypoint == PIPE_VIDEO_ENTRYPOINT_ENCODE) {
      return d3d12_screen_get_video_param_encode(pscreen, profile, entrypoint, param);
   } else if (entrypoint == PIPE_VIDEO_ENTRYPOINT_PROCESSING) {
      return d3d12_screen_get_video_param_postproc(pscreen, profile, entrypoint, param);
   }

   // Some frontends call get_video_param with PIPE_VIDEO_ENTRYPOINT_UNKNOWN
   // to get some capabilities not entrypoint specific.
   switch (param) {
      case PIPE_VIDEO_CAP_SKIP_CLEAR_SURFACE:
      {
          // D3D12 does not require clearing the surface on creation for video
          // as it doesn't use D3D12_HEAP_FLAG_CREATE_NOT_ZEROED
          // Furthermore, on PIPE_CONTEXT_MEDIA_ONLY contexts, the
          // clear_render_target function is not implemented
         return 1;
      } break;
      default:
         debug_printf("[d3d12_screen_get_video_param] unknown video param: %d\n", param);
         return 0;
   }

   return 0;
}

static bool
is_d3d12_video_encode_format_supported(struct pipe_screen *screen,
                                           pipe_format format,
                                           enum pipe_video_profile profile)
{
#if VIDEO_CODEC_H264ENC
   D3D12_VIDEO_ENCODER_PROFILE_H264 profH264 = {};
#endif
#if VIDEO_CODEC_H265ENC
   D3D12_VIDEO_ENCODER_PROFILE_HEVC profHEVC = {};
#endif
#if VIDEO_CODEC_AV1ENC
   D3D12_VIDEO_ENCODER_AV1_PROFILE profAV1 = {};
#endif
   D3D12_FEATURE_DATA_VIDEO_ENCODER_INPUT_FORMAT capDataFmt = {};
   capDataFmt.NodeIndex = 0;
   capDataFmt.Codec = d3d12_video_encoder_convert_codec_to_d3d12_enc_codec(profile);
   capDataFmt.Format = d3d12_get_format(format);
   switch (u_reduce_video_profile(profile)) {
#if VIDEO_CODEC_H264ENC
      case PIPE_VIDEO_FORMAT_MPEG4_AVC:
      {
         profH264 = d3d12_video_encoder_convert_profile_to_d3d12_enc_profile_h264(profile);
         capDataFmt.Profile.DataSize = sizeof(profH264);
         capDataFmt.Profile.pH264Profile = &profH264;
      } break;
#endif
#if VIDEO_CODEC_H265ENC
      case PIPE_VIDEO_FORMAT_HEVC:
      {
         profHEVC = d3d12_video_encoder_convert_profile_to_d3d12_enc_profile_hevc(profile);
         capDataFmt.Profile.DataSize = sizeof(profHEVC);
         capDataFmt.Profile.pHEVCProfile = &profHEVC;
      } break;
#endif
#if VIDEO_CODEC_AV1ENC
      case PIPE_VIDEO_FORMAT_AV1:
      {
         profAV1 = d3d12_video_encoder_convert_profile_to_d3d12_enc_profile_av1(profile);
         capDataFmt.Profile.DataSize = sizeof(profAV1);
         capDataFmt.Profile.pAV1Profile = &profAV1;
      } break;
#endif
      default:
      {
         UNREACHABLE("Unsupported pipe_video_format");
      } break;
   }
   ComPtr<ID3D12VideoDevice3> spD3D12VideoDevice;
   struct d3d12_screen *pD3D12Screen = (struct d3d12_screen *) screen;
   if (FAILED(pD3D12Screen->dev->QueryInterface(IID_PPV_ARGS(spD3D12VideoDevice.GetAddressOf())))) {
      // No video encode support in underlying d3d12 device (needs ID3D12VideoDevice3)
      return false;
   }
   HRESULT hr = spD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_ENCODER_INPUT_FORMAT,
                                                         &capDataFmt,
                                                         sizeof(capDataFmt));
   return SUCCEEDED(hr) && capDataFmt.IsSupported;
}

static bool
is_d3d12_video_decode_format_supported(struct pipe_screen *screen,
                                       pipe_format format,
                                       enum pipe_video_profile profile)
{
   ComPtr<ID3D12VideoDevice> spD3D12VideoDevice;
   struct d3d12_screen *pD3D12Screen = (struct d3d12_screen *) screen;
   if (FAILED(pD3D12Screen->dev->QueryInterface(IID_PPV_ARGS(spD3D12VideoDevice.GetAddressOf()))))
      return false; // No video decode support in underlying d3d12 device (needs ID3D12VideoDevice)

   GUID decodeGUID = d3d12_video_decoder_convert_pipe_video_profile_to_d3d12_profile(profile);
   GUID emptyGUID = {};
   assert (decodeGUID != emptyGUID);

   D3D12_VIDEO_DECODE_CONFIGURATION decoderConfig = { decodeGUID,
                                                      D3D12_BITSTREAM_ENCRYPTION_TYPE_NONE,
                                                      D3D12_VIDEO_FRAME_CODED_INTERLACE_TYPE_NONE };

   D3D12_FEATURE_DATA_VIDEO_DECODE_FORMAT_COUNT decodeFormatCount = {0 /* NodeIndex*/, decoderConfig };
   if(FAILED(spD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_DECODE_FORMAT_COUNT,
                                                        &decodeFormatCount,
                                                        sizeof(decodeFormatCount))))
      return false;

    std::vector<DXGI_FORMAT> supportedDecodeFormats;
    supportedDecodeFormats.resize(decodeFormatCount.FormatCount);

    D3D12_FEATURE_DATA_VIDEO_DECODE_FORMATS decodeFormats =
    {
        0, // NodeIndex
        decoderConfig,
        static_cast<UINT>(supportedDecodeFormats.size()),
        supportedDecodeFormats.data()
    };

   if(FAILED(spD3D12VideoDevice->CheckFeatureSupport(D3D12_FEATURE_VIDEO_DECODE_FORMATS,
                                                         &decodeFormats,
                                                         sizeof(decodeFormats))))
      return false;

   DXGI_FORMAT requestedDXGIFormat = d3d12_get_format(format);
   for (DXGI_FORMAT fmt : supportedDecodeFormats)
      if (fmt == requestedDXGIFormat)
         return true;
   return false;
}

static bool
is_d3d12_video_process_format_supported(struct pipe_screen *screen,
                                        pipe_format format)
{
   // Return both VPBlit support and format is in known list
   return (screen->get_video_param(screen,
                        PIPE_VIDEO_PROFILE_UNKNOWN,
                        PIPE_VIDEO_ENTRYPOINT_PROCESSING,
                        PIPE_VIDEO_CAP_SUPPORTED))
   &&
   ((format == PIPE_FORMAT_NV12) || (format == PIPE_FORMAT_P010)
      || (format == PIPE_FORMAT_R8G8B8A8_UNORM) || (format == PIPE_FORMAT_R8G8B8A8_UINT)
      || (format == PIPE_FORMAT_R8G8B8X8_UNORM) || (format == PIPE_FORMAT_R8G8B8X8_UINT));
}

static bool
is_d3d12_video_allowed_format(enum pipe_format format, enum pipe_video_entrypoint entrypoint)
{
   if (entrypoint == PIPE_VIDEO_ENTRYPOINT_BITSTREAM) {
      return ((format == PIPE_FORMAT_NV12) || (format == PIPE_FORMAT_P010));
   } else if (entrypoint == PIPE_VIDEO_ENTRYPOINT_ENCODE) {
      return ((format == PIPE_FORMAT_NV12) ||
              (format == PIPE_FORMAT_P010) ||
              (format == PIPE_FORMAT_AYUV) ||
              (format == PIPE_FORMAT_YUYV /*maps to DXGI_FORMAT_YUY2*/) ||
              (format == PIPE_FORMAT_Y210) ||
              (format == PIPE_FORMAT_Y410));
   } else if (entrypoint == PIPE_VIDEO_ENTRYPOINT_PROCESSING) {
      return (format == PIPE_FORMAT_NV12) || (format == PIPE_FORMAT_P010)
         || (format == PIPE_FORMAT_R8G8B8A8_UNORM) || (format == PIPE_FORMAT_R8G8B8A8_UINT)
         || (format == PIPE_FORMAT_R8G8B8X8_UNORM) || (format == PIPE_FORMAT_R8G8B8X8_UINT);
   }
   return false;
}

static bool
d3d12_video_buffer_is_format_supported(struct pipe_screen *screen,
                                       enum pipe_format format,
                                       enum pipe_video_profile profile,
                                       enum pipe_video_entrypoint entrypoint)
{
   // Check in allowed list of formats first
   if(!is_d3d12_video_allowed_format(format, entrypoint))
      return false;

   // If the VA frontend asks for all profiles, assign
   // a default profile based on the bitdepth
   if(u_reduce_video_profile(profile) == PIPE_VIDEO_FORMAT_UNKNOWN)
   {
      profile = (format == PIPE_FORMAT_P010) ? PIPE_VIDEO_PROFILE_HEVC_MAIN_10 : PIPE_VIDEO_PROFILE_MPEG4_AVC_MAIN;
   }

   // Then check is the underlying driver supports the allowed formats
   if (entrypoint == PIPE_VIDEO_ENTRYPOINT_BITSTREAM) {
      return is_d3d12_video_decode_format_supported(screen, format, profile);
   } else if (entrypoint == PIPE_VIDEO_ENTRYPOINT_ENCODE) {
      return is_d3d12_video_encode_format_supported(screen, format, profile);
   } else if (entrypoint == PIPE_VIDEO_ENTRYPOINT_PROCESSING) {
      return is_d3d12_video_process_format_supported(screen, format);
   }
   return false;
}

void
d3d12_screen_video_init(struct pipe_screen *pscreen)
{
   pscreen->get_video_param = d3d12_screen_get_video_param;
   pscreen->is_video_format_supported = d3d12_video_buffer_is_format_supported;
}

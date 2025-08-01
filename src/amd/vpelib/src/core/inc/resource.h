/* Copyright 2022 Advanced Micro Devices, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE COPYRIGHT HOLDER(S) OR AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Authors: AMD
 *
 */

#pragma once

#include "vpe_types.h"
#include "cmd_builder.h"
#include "vpec.h"
#include "cdc.h"
#include "dpp.h"
#include "mpc.h"
#include "opp.h"
#include "vector.h"
#include "hw_shared.h"
#include "color_bg.h"

#ifdef __cplusplus
extern "C" {
#endif

struct vpe_priv;
struct vpe_cmd_output;
struct vpe_cmd_info;
struct segment_ctx;
enum vpe_stream_type;

#define MIN_VPE_CMD    (1024)
#define MIN_NUM_CONFIG (16)

enum vpe_cmd_ops;

/** struct resource stores all the hw subblocks function pointers
 * which assist in constructing the command packets.
 *
 * As differnt asic may have its own deviation in the subblocks,
 * each hw ip has its own set of function pointers to expose
 * the programming interface of the blocks.
 *
 * The upper level should have a sequencer that constructs the
 * final programming sequence using subblock functions
 */
struct resource {
    struct vpe_priv *vpe_priv;
    struct vpec      vpec;

    bool (*check_h_mirror_support)(bool *input_mirror, bool *output_miror);

    enum vpe_status (*calculate_segments)(
        struct vpe_priv *vpe_priv, const struct vpe_build_param *params);

    enum vpe_status(*check_bg_color_support)(struct vpe_priv* vpe_priv, struct vpe_color* bg_color);

    void (*bg_color_convert)(enum color_space output_cs, struct transfer_func *output_tf,
        enum vpe_surface_pixel_format pixel_format, struct vpe_color *mpc_bg_color,
        struct vpe_color *opp_bg_color, bool enable_3dlut);

    enum vpe_status (*set_num_segments)(struct vpe_priv *vpe_priv, struct stream_ctx *stream_ctx,
        struct scaler_data *scl_data, struct vpe_rect *src_rect, struct vpe_rect *dst_rect,
        uint32_t *max_seg_width, uint32_t recout_width_alignment);

    bool (*split_bg_gap)(struct vpe_rect *gaps, const struct vpe_rect *target_rect,
        uint32_t max_width, uint16_t max_gaps, uint16_t *num_gaps, uint16_t num_instances);

    void (*calculate_dst_viewport_and_active)(
        struct segment_ctx *segment_ctx, uint32_t max_seg_width);

    uint16_t (*get_bg_stream_idx)(struct vpe_priv *vpe_priv);

    uint16_t (*find_bg_gaps)(struct vpe_priv *vpe_priv, const struct vpe_rect *target_rect,
        struct vpe_rect *gaps, uint16_t max_gaps);

    void (*create_bg_segments)(
        struct vpe_priv *vpe_priv, struct vpe_rect *gaps, uint16_t gaps_cnt, enum vpe_cmd_ops ops);

    enum vpe_status (*populate_cmd_info)(struct vpe_priv *vpe_priv);

    int32_t (*program_frontend)(struct vpe_priv *vpe_priv, uint32_t pipe_idx, uint32_t cmd_idx,
        uint32_t cmd_input_idx, bool seg_only);

    int32_t (*program_backend)(
        struct vpe_priv *vpe_priv, uint32_t pipe_idx, uint32_t cmd_idx, bool seg_only);

    void (*get_bufs_req)(struct vpe_priv *vpe_priv, struct vpe_bufs_req *req);

    enum vpe_status (*check_mirror_rotation_support)(const struct vpe_stream *stream);

    enum vpe_status (*update_blnd_gamma)(struct vpe_priv *vpe_priv,
        const struct vpe_build_param *param, const struct vpe_stream *stream,
        struct transfer_func *blnd_tf);

    enum vpe_status (*update_output_gamma)(struct vpe_priv *vpe_priv,
        const struct vpe_build_param *param, struct transfer_func *output_tf,
        bool geometric_scaling);

    bool (*validate_cached_param)(struct vpe_priv *vpe_priv, const struct vpe_build_param *param);
    // Indicates the nominal range hdr input content should be in during processing.
    int internal_hdr_normalization;

    // vpep components
    struct cdc_fe     *cdc_fe[MAX_INPUT_PIPE];
    struct cdc_be     *cdc_be[MAX_OUTPUT_PIPE];
    struct dpp        *dpp[MAX_INPUT_PIPE];
    struct opp        *opp[MAX_INPUT_PIPE];
    struct mpc        *mpc[MAX_INPUT_PIPE];
    struct cmd_builder cmd_builder;
};

/** translate the vpe ip version into vpe hw level */
enum vpe_ip_level vpe_resource_parse_ip_version(
    uint8_t major, uint8_t minor, uint8_t rev_id);

/** initialize the resource ased on vpe hw level */
enum vpe_status vpe_construct_resource(
    struct vpe_priv *vpe_priv, enum vpe_ip_level level, struct resource *resource);

/** destroy the resource */
void vpe_destroy_resource(struct vpe_priv *vpe_priv, struct resource *res);

/** alloc segment ctx*/
enum vpe_status vpe_alloc_segment_ctx(
    struct vpe_priv *vpe_priv, struct stream_ctx *stream_ctx, uint16_t num_segments);

/** stream ctx */
struct stream_ctx *vpe_alloc_stream_ctx(struct vpe_priv *vpe_priv, uint32_t num_streams);

void vpe_free_stream_ctx(struct vpe_priv *vpe_priv);

/** pipe resource management */
void vpe_pipe_reset(struct vpe_priv *vpe_priv);

void vpe_pipe_reclaim(struct vpe_priv *vpe_priv, struct vpe_cmd_info *cmd_info);

struct pipe_ctx *vpe_pipe_find_owner(struct vpe_priv *vpe_priv, uint32_t stream_idx, bool *reuse);

/** resource helper */
void vpe_clip_stream(
    struct vpe_rect *src_rect, struct vpe_rect *dst_rect, const struct vpe_rect *target_rect);

void calculate_scaling_ratios(struct scaler_data *scl_data, struct vpe_rect *src_rect,
    struct vpe_rect *dst_rect, enum vpe_surface_pixel_format format);

uint16_t vpe_get_num_segments(struct vpe_priv *vpe_priv, const struct vpe_rect *src,
    const struct vpe_rect *dst, const uint32_t max_seg_width);

bool should_generate_cmd_info(enum vpe_stream_type stream_type);

enum vpe_status vpe_resource_build_scaling_params(struct segment_ctx *segment);

void vpe_handle_output_h_mirror(struct vpe_priv *vpe_priv);

void vpe_resource_build_bit_depth_reduction_params(
    struct opp *opp, struct bit_depth_reduction_params *fmt_bit_depth);

/** resource function call backs*/
void vpe_frontend_config_callback(
    void *ctx, uint64_t cfg_base_gpu, uint64_t cfg_base_cpu, uint64_t size, uint32_t pipe_idx);

void vpe_backend_config_callback(
    void *ctx, uint64_t cfg_base_gpu, uint64_t cfg_base_cpu, uint64_t size, uint32_t pipe_idx);

bool vpe_rec_is_equal(struct vpe_rect rec1, struct vpe_rect rec2);

const struct vpe_caps *vpe_get_capability(enum vpe_ip_level ip_level);

void vpe_setup_check_funcs(struct vpe_check_support_funcs *funcs, enum vpe_ip_level ip_level);

#ifdef __cplusplus
}
#endif

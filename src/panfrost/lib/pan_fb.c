/*
 * Copyright (C) 2026 Collabora, Ltd.
 * SPDX-License-Identifier: MIT
 */
#include "pan_fb.h"

#include "pan_afbc.h"
#include "pan_afrc.h"
#include "pan_desc.h"
#include "pan_format.h"
#include "pan_image.h"
#include "pan_props.h"
#include "pan_util.h"

static unsigned
pan_bytes_per_pixel_tib(enum pipe_format format)
{
   const struct pan_blendable_format *bf =
      GENX(pan_blendable_format_from_pipe_format)(format);
   return pan_format_tib_size(format, bf->internal);
}

void
GENX(pan_select_fb_tile_size)(struct pan_fb_layout *fb)
{
   uint32_t rt_B_per_sa = 0;
   for (unsigned rt = 0; rt < fb->rt_count; rt++) {
      enum pipe_format format = fb->rt_formats[rt];
      if (format == PIPE_FORMAT_NONE)
         continue;

      const struct pan_blendable_format *bf =
         GENX(pan_blendable_format_from_pipe_format)(format);
      rt_B_per_sa += pan_format_tib_size(format, bf->internal);
   }

   /* The PLS area overlaps with the color targets */
   rt_B_per_sa = MAX2(rt_B_per_sa, fb->pls_size_B);

   const uint32_t rt_B_per_px = rt_B_per_sa * fb->sample_count;

   /* We always have depth and it's is always stored in a 32-bit float.
    * Stencil requires depth to be allocated, but doesn't have it's own
    * budget; it's tied to the depth buffer.
    */
   const uint32_t z_B_per_px = sizeof(float) * fb->sample_count;

   fb->tile_size_px =
      MIN2(fb->tile_rt_budget_B >> util_logbase2_ceil(rt_B_per_px),
           fb->tile_z_budget_B >> util_logbase2_ceil(z_B_per_px));

   /* Check if we're using too much tile-memory; if we are, try disabling
    * pipelining. This works because we're starting with an optimistic half
    * of the tile-budget, so we actually have another half that can be used.
    *
    * On v6 GPUs, doing this is not allowed; they *have* to pipeline.
    */
    if (PAN_ARCH != 6 && fb->tile_size_px < 4 * 4)
       fb->tile_size_px *= 2;

   /* Clamp tile size to hardware limits */
   fb->tile_size_px =
      MIN2(fb->tile_size_px, pan_max_effective_tile_size(PAN_ARCH));
   assert(util_is_power_of_two_nonzero(fb->tile_size_px));
   assert(fb->tile_size_px >= 4 * 4);

   /* Colour buffer allocations must be 1K aligned. */
   fb->tile_rt_alloc_B = ALIGN_POT(rt_B_per_px * fb->tile_size_px, 1024);

#if PAN_ARCH == 6
   assert(fb->tile_rt_alloc_B <= fb->tile_rt_budget_B && "tile too big");
#else
   assert(fb->tile_rt_alloc_B <= fb->tile_rt_budget_B * 2 && "tile too big");
#endif
}

static void
align_fb_tiling_area_for_image_plane(struct pan_fb_layout *fb,
                                     struct pan_image_plane_ref pref)
{
   if (!pref.image)
      return;

   struct pan_image_block_size block_size;
   if (drm_is_afbc(pref.image->props.modifier)) {
      /* For AFBC render targets, the hardware always writes full superblocks.
       * In order to ensure we don't write garbage, we need to expand the
       * render area accordingly and load the border pixels.
       */
      block_size = pan_afbc_renderblock_size(pref.image->props.modifier);
      assert(block_size.width >= 16 && block_size.height >= 16);
   } else if (drm_is_afrc(pref.image->props.modifier)) {
      /* For AFRC render targets, the hardware always writes full clumps.  In
       * order to ensure we don't write garbage, we need to expand the render
       * area accordingly and load the border pixels.
       */
      bool scan = pan_afrc_is_scan(pref.image->props.modifier);
      block_size = pan_afrc_clump_size(pref.image->props.format, scan);
   } else {
      /* No alignment requirements */
      return;
   }

   fb->tiling_area_px = pan_fb_bbox_align(fb->tiling_area_px,
                                          block_size.width,
                                          block_size.height);
}

void
GENX(pan_align_fb_tiling_area)(struct pan_fb_layout *fb,
                               const struct pan_fb_store *store)
{
   if (store == NULL)
      return;

   for (unsigned rt = 0; rt < fb->rt_count; rt++) {
      if (store->rts[rt].store) {
         align_fb_tiling_area_for_image_plane(fb,
            pan_image_view_get_color_plane(store->rts[rt].iview));
      }
   }

   if (store->zs.store) {
      align_fb_tiling_area_for_image_plane(fb,
         pan_image_view_get_zs_plane(store->zs.iview));
   }

   if (store->s.store) {
      align_fb_tiling_area_for_image_plane(fb,
         pan_image_view_get_s_plane(store->s.iview));
   }
}

void
GENX(pan_fill_fb_info)(const struct pan_fb_desc_info *info,
                       struct pan_fb_info *fbinfo)
{
   struct pan_bbox bbox = { 0, 0, 0, 0 };
   if (info->fb->width_px > 0 && info->fb->width_px > 0) {
      const struct pan_fb_bbox fb_area_px =
         pan_fb_bbox_from_xywh(0, 0, info->fb->width_px, info->fb->height_px);

      assert(pan_fb_bbox_is_valid(info->fb->tiling_area_px));
      const struct pan_fb_bbox bbox_px =
         pan_fb_bbox_clamp(info->fb->tiling_area_px, fb_area_px);

      bbox = (struct pan_bbox) {
         .minx = bbox_px.min_x,
         .miny = bbox_px.min_y,
         .maxx = bbox_px.max_x,
         .maxy = bbox_px.max_y,
      };
   }

   *fbinfo = (struct pan_fb_info) {
      .width = info->fb->width_px,
      .height = info->fb->height_px,
      .draw_extent = bbox,
      .frame_bounding_box = bbox,
      .nr_samples = info->fb->sample_count,
      .rt_count = info->fb->rt_count,
      .pls_enabled = info->fb->pls_size_B > 0,
      .bifrost.pre_post = {
         .dcds.gpu = info->frame_shaders.dcd_pointer,
         .modes = {
            info->frame_shaders.modes[0],
            info->frame_shaders.modes[1],
            info->frame_shaders.modes[2],
         },
      },

      .tile_buf_budget = info->fb->tile_rt_budget_B,
      .z_tile_buf_budget = info->fb->tile_z_budget_B,
      .tile_size = info->fb->tile_size_px,
      .cbuf_allocation = info->fb->tile_rt_alloc_B,

      .sample_positions = info->sample_pos_array_pointer,
      .sprite_coord_origin = info->sprite_coord_origin_max_y,
      .first_provoking_vertex = info->provoking_vertex_first,
      .allow_hsr_prepass = info->provoking_vertex_first,
   };

   /* There are cases where we only want to fill out a partial fb_info */
   if (info->load == NULL && info->store == NULL)
      return;

   for (unsigned rt = 0; rt < info->fb->rt_count; rt++) {
      fbinfo->rts[rt] = (struct pan_fb_color_attachment) {
         .view = info->store->rts[rt].iview,
         .clear = info->load->rts[rt].border_load == PAN_FB_LOAD_CLEAR ||
                  info->load->rts[rt].in_bounds_load == PAN_FB_LOAD_CLEAR,
         .preload = info->load->rts[rt].in_bounds_load == PAN_FB_LOAD_IMAGE,
         .discard = !info->store->rts[rt].store,
      };

      if (fbinfo->rts[rt].clear) {
         pan_pack_color(GENX(pan_blendable_formats),
                        fbinfo->rts[rt].clear_value,
                        &info->load->rts[rt].clear.color,
                        info->fb->rt_formats[rt],
                        false /* dithered */);
      }
   }

   if (info->fb->z_format != PIPE_FORMAT_NONE) {
      fbinfo->zs.view.zs = info->store->zs.iview;
      fbinfo->zs.clear.z = info->load->z.border_load == PAN_FB_LOAD_CLEAR ||
                           info->load->z.in_bounds_load == PAN_FB_LOAD_CLEAR;
      fbinfo->zs.preload.z = info->load->z.in_bounds_load == PAN_FB_LOAD_IMAGE;
      fbinfo->zs.discard.z = !info->store->zs.store;
      if (fbinfo->zs.clear.z)
         fbinfo->zs.clear_value.depth = info->load->z.clear.depth;
   }

   if (info->fb->s_format != PIPE_FORMAT_NONE) {
      fbinfo->zs.view.s = info->store->s.iview;
      fbinfo->zs.clear.s = info->load->s.border_load == PAN_FB_LOAD_CLEAR ||
                           info->load->s.in_bounds_load == PAN_FB_LOAD_CLEAR;
      fbinfo->zs.preload.s = info->load->s.in_bounds_load == PAN_FB_LOAD_IMAGE;
      fbinfo->zs.discard.s = !info->store->s.store;
      if (fbinfo->zs.clear.s)
         fbinfo->zs.clear_value.stencil = info->load->s.clear.stencil;
   }
}

uint32_t
GENX(pan_emit_fb_desc)(const struct pan_fb_desc_info *info, void *out)
{
   struct pan_fb_info old_fb;
   GENX(pan_fill_fb_info)(info, &old_fb);

   return GENX(pan_emit_fbd)(&old_fb, info->layer, info->tls,
                             info->tiler_ctx, out);
}

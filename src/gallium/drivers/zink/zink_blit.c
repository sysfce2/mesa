#include "zink_clear.h"
#include "zink_context.h"
#include "zink_format.h"
#include "zink_inlines.h"
#include "zink_kopper.h"
#include "zink_helpers.h"
#include "zink_query.h"
#include "zink_resource.h"
#include "zink_screen.h"

#include "util/u_blitter.h"
#include "util/u_rect.h"
#include "util/u_surface.h"
#include "util/format/u_format.h"

static void
apply_dst_clears(struct zink_context *ctx, const struct pipe_blit_info *info, bool discard_only)
{
   if (info->scissor_enable) {
      struct u_rect rect = { info->scissor.minx, info->scissor.maxx,
                             info->scissor.miny, info->scissor.maxy };
      zink_fb_clears_apply_or_discard(ctx, info->dst.resource, rect, info->dst.box.z, info->dst.box.depth, discard_only);
   } else
      zink_fb_clears_apply_or_discard(ctx, info->dst.resource, zink_rect_from_box(&info->dst.box), info->dst.box.z, info->dst.box.depth, discard_only);
}

static bool
blit_resolve(struct zink_context *ctx, const struct pipe_blit_info *info, bool *needs_present_readback)
{
   if (util_format_get_mask(info->dst.format) != info->mask ||
       util_format_get_mask(info->src.format) != info->mask ||
       util_format_is_depth_or_stencil(info->dst.format) ||
       info->scissor_enable ||
       info->swizzle_enable ||
       info->alpha_blend)
      return false;

   if (info->src.box.width < 0 ||
       info->dst.box.width < 0 ||
       info->src.box.height < 0 ||
       info->dst.box.height < 0 ||
       info->src.box.depth < 0 ||
       info->dst.box.depth < 0)
      return false;
   /* vulkan resolves can't downscale */
   if (info->src.box.width > info->dst.box.width ||
       info->src.box.height > info->dst.box.height ||
       info->src.box.depth > info->dst.box.depth)
      return false;

   if (info->render_condition_enable &&
       ctx->render_condition_active)
      return false;

   struct zink_resource *src = zink_resource(info->src.resource);
   struct zink_resource *use_src = src;
   struct zink_resource *dst = zink_resource(info->dst.resource);

   struct zink_screen *screen = zink_screen(ctx->base.screen);
   /* aliased/swizzled formats need u_blitter */
   if (src->format != zink_get_format(screen, info->src.format) ||
       dst->format != zink_get_format(screen, info->dst.format))
      return false;
   if (src->format != dst->format)
      return false;


   apply_dst_clears(ctx, info, false);
   zink_fb_clears_apply_region(ctx, info->src.resource, zink_rect_from_box(&info->src.box), info->src.box.z, info->src.box.depth);

   if (src->obj->dt)
      *needs_present_readback = zink_kopper_acquire_readback(ctx, src, &use_src);

   if (zink_screen(ctx->base.screen)->driver_workarounds.general_layout) {
      zink_resource_image_transfer_dst_barrier(ctx, dst, info->dst.level, &info->dst.box, false);
      screen->image_barrier(ctx, use_src,
                              VK_IMAGE_LAYOUT_GENERAL,
                              VK_ACCESS_TRANSFER_READ_BIT | VK_ACCESS_TRANSFER_WRITE_BIT,
                              VK_PIPELINE_STAGE_TRANSFER_BIT);
   } else {
      zink_resource_setup_transfer_layouts(ctx, use_src, dst);
   }
   VkCommandBuffer cmdbuf = *needs_present_readback ?
                            ctx->bs->cmdbuf :
                            zink_get_cmdbuf(ctx, src, dst);
   zink_batch_reference_resource_rw(ctx, use_src, false);
   zink_batch_reference_resource_rw(ctx, dst, true);

   bool marker = zink_cmd_debug_marker_begin(ctx, cmdbuf, "blit_resolve(%s->%s, %dx%d->%dx%d)",
                                             util_format_short_name(info->src.format),
                                             util_format_short_name(info->src.format),
                                             info->src.box.width, info->src.box.height,
                                             info->dst.box.width, info->dst.box.height);
   VkImageResolve region = {0};

   region.srcSubresource.aspectMask = src->aspect;
   region.srcSubresource.mipLevel = info->src.level;
   region.srcOffset.x = info->src.box.x;
   region.srcOffset.y = info->src.box.y;

   if (src->base.b.array_size > 1) {
      region.srcOffset.z = 0;
      region.srcSubresource.baseArrayLayer = info->src.box.z;
      region.srcSubresource.layerCount = info->src.box.depth;
   } else {
      assert(info->src.box.depth == 1);
      region.srcOffset.z = info->src.box.z;
      region.srcSubresource.baseArrayLayer = 0;
      region.srcSubresource.layerCount = 1;
   }

   region.dstSubresource.aspectMask = dst->aspect;
   region.dstSubresource.mipLevel = info->dst.level;
   region.dstOffset.x = info->dst.box.x;
   region.dstOffset.y = info->dst.box.y;

   if (dst->base.b.array_size > 1) {
      region.dstOffset.z = 0;
      region.dstSubresource.baseArrayLayer = info->dst.box.z;
      region.dstSubresource.layerCount = info->dst.box.depth;
   } else {
      assert(info->dst.box.depth == 1);
      region.dstOffset.z = info->dst.box.z;
      region.dstSubresource.baseArrayLayer = 0;
      region.dstSubresource.layerCount = 1;
   }

   region.extent.width = info->dst.box.width;
   region.extent.height = info->dst.box.height;
   region.extent.depth = info->dst.box.depth;
   if (region.srcOffset.x + region.extent.width >= u_minify(src->base.b.width0, region.srcSubresource.mipLevel))
      region.extent.width = u_minify(src->base.b.width0, region.srcSubresource.mipLevel) - region.srcOffset.x;
   if (region.dstOffset.x + region.extent.width >= u_minify(dst->base.b.width0, region.dstSubresource.mipLevel))
      region.extent.width = u_minify(dst->base.b.width0, region.dstSubresource.mipLevel) - region.dstOffset.x;
   if (region.srcOffset.y + region.extent.height >= u_minify(src->base.b.height0, region.srcSubresource.mipLevel))
      region.extent.height = u_minify(src->base.b.height0, region.srcSubresource.mipLevel) - region.srcOffset.y;
   if (region.dstOffset.y + region.extent.height >= u_minify(dst->base.b.height0, region.dstSubresource.mipLevel))
      region.extent.height = u_minify(dst->base.b.height0, region.dstSubresource.mipLevel) - region.dstOffset.y;
   if (region.srcOffset.z + region.extent.depth >= u_minify(src->base.b.depth0, region.srcSubresource.mipLevel))
      region.extent.depth = u_minify(src->base.b.depth0, region.srcSubresource.mipLevel) - region.srcOffset.z;
   if (region.dstOffset.z + region.extent.depth >= u_minify(dst->base.b.depth0, region.dstSubresource.mipLevel))
      region.extent.depth = u_minify(dst->base.b.depth0, region.dstSubresource.mipLevel) - region.dstOffset.z;
   VKCTX(CmdResolveImage)(cmdbuf, use_src->obj->image, src->layout,
                     dst->obj->image, dst->layout,
                     1, &region);
   zink_cmd_debug_marker_end(ctx, cmdbuf, marker);

   return true;
}

static bool
blit_native(struct zink_context *ctx, const struct pipe_blit_info *info, bool *needs_present_readback)
{
   if (util_format_get_mask(info->dst.format) != info->mask ||
       util_format_get_mask(info->src.format) != info->mask ||
       info->scissor_enable ||
       info->swizzle_enable ||
       info->alpha_blend)
      return false;

   if (info->render_condition_enable &&
       ctx->render_condition_active)
      return false;

   if (util_format_is_depth_or_stencil(info->dst.format) &&
       (info->dst.format != info->src.format || info->filter == PIPE_TEX_FILTER_LINEAR))
      return false;

   /* vkCmdBlitImage must not be used for multisampled source or destination images. */
   if (info->src.resource->nr_samples > 1 || info->dst.resource->nr_samples > 1)
      return false;

   struct zink_resource *src = zink_resource(info->src.resource);
   struct zink_resource *use_src = src;
   struct zink_resource *dst = zink_resource(info->dst.resource);

   struct zink_screen *screen = zink_screen(ctx->base.screen);
   if (src->format != zink_get_format(screen, info->src.format) ||
       dst->format != zink_get_format(screen, info->dst.format))
      return false;
   if (src->format != VK_FORMAT_A8_UNORM_KHR && zink_format_is_emulated_alpha(info->src.format))
      return false;

   if (!(src->obj->vkfeats & VK_FORMAT_FEATURE_BLIT_SRC_BIT) ||
       !(dst->obj->vkfeats & VK_FORMAT_FEATURE_BLIT_DST_BIT))
      return false;

   if ((util_format_is_pure_sint(info->src.format) !=
        util_format_is_pure_sint(info->dst.format)) ||
       (util_format_is_pure_uint(info->src.format) !=
        util_format_is_pure_uint(info->dst.format)))
      return false;

   if (info->filter == PIPE_TEX_FILTER_LINEAR &&
       !(src->obj->vkfeats & VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT))
      return false;


   VkImageBlit region = {0};
   region.srcSubresource.aspectMask = src->aspect;
   region.srcSubresource.mipLevel = info->src.level;
   region.srcOffsets[0].x = info->src.box.x;
   region.srcOffsets[0].y = info->src.box.y;
   region.srcOffsets[1].x = info->src.box.x + info->src.box.width;
   region.srcOffsets[1].y = info->src.box.y + info->src.box.height;

   enum pipe_texture_target src_target = src->base.b.target;
   if (src->need_2D)
      src_target = src_target == PIPE_TEXTURE_1D ? PIPE_TEXTURE_2D : PIPE_TEXTURE_2D_ARRAY;
   switch (src_target) {
   case PIPE_TEXTURE_CUBE:
   case PIPE_TEXTURE_CUBE_ARRAY:
   case PIPE_TEXTURE_2D_ARRAY:
   case PIPE_TEXTURE_1D_ARRAY:
      /* these use layer */
      region.srcSubresource.baseArrayLayer = info->src.box.z;
      /* VUID-vkCmdBlitImage-srcImage-00240 */
      if (region.srcSubresource.baseArrayLayer && dst->base.b.target == PIPE_TEXTURE_3D)
         return false;
      region.srcSubresource.layerCount = info->src.box.depth;
      region.srcOffsets[0].z = 0;
      region.srcOffsets[1].z = 1;
      break;
   case PIPE_TEXTURE_3D:
      /* this uses depth */
      region.srcSubresource.baseArrayLayer = 0;
      region.srcSubresource.layerCount = 1;
      region.srcOffsets[0].z = info->src.box.z;
      region.srcOffsets[1].z = info->src.box.z + info->src.box.depth;
      break;
   default:
      /* these must only copy one layer */
      region.srcSubresource.baseArrayLayer = 0;
      region.srcSubresource.layerCount = 1;
      region.srcOffsets[0].z = 0;
      region.srcOffsets[1].z = 1;
   }

   region.dstSubresource.aspectMask = dst->aspect;
   region.dstSubresource.mipLevel = info->dst.level;
   region.dstOffsets[0].x = info->dst.box.x;
   region.dstOffsets[0].y = info->dst.box.y;
   region.dstOffsets[1].x = info->dst.box.x + info->dst.box.width;
   region.dstOffsets[1].y = info->dst.box.y + info->dst.box.height;
   assert(region.dstOffsets[0].x != region.dstOffsets[1].x);
   assert(region.dstOffsets[0].y != region.dstOffsets[1].y);

   enum pipe_texture_target dst_target = dst->base.b.target;
   if (dst->need_2D)
      dst_target = dst_target == PIPE_TEXTURE_1D ? PIPE_TEXTURE_2D : PIPE_TEXTURE_2D_ARRAY;
   switch (dst_target) {
   case PIPE_TEXTURE_CUBE:
   case PIPE_TEXTURE_CUBE_ARRAY:
   case PIPE_TEXTURE_2D_ARRAY:
   case PIPE_TEXTURE_1D_ARRAY:
      /* these use layer */
      region.dstSubresource.baseArrayLayer = info->dst.box.z;
      /* VUID-vkCmdBlitImage-srcImage-00240 */
      if (region.dstSubresource.baseArrayLayer && src->base.b.target == PIPE_TEXTURE_3D)
         return false;
      region.dstSubresource.layerCount = info->dst.box.depth;
      region.dstOffsets[0].z = 0;
      region.dstOffsets[1].z = 1;
      break;
   case PIPE_TEXTURE_3D:
      /* this uses depth */
      region.dstSubresource.baseArrayLayer = 0;
      region.dstSubresource.layerCount = 1;
      region.dstOffsets[0].z = info->dst.box.z;
      region.dstOffsets[1].z = info->dst.box.z + info->dst.box.depth;
      break;
   default:
      /* these must only copy one layer */
      region.dstSubresource.baseArrayLayer = 0;
      region.dstSubresource.layerCount = 1;
      region.dstOffsets[0].z = 0;
      region.dstOffsets[1].z = 1;
   }
   assert(region.dstOffsets[0].z != region.dstOffsets[1].z);

   apply_dst_clears(ctx, info, false);
   zink_fb_clears_apply_region(ctx, info->src.resource, zink_rect_from_box(&info->src.box), info->src.box.z, info->src.box.depth);

   if (src->obj->dt)
      *needs_present_readback = zink_kopper_acquire_readback(ctx, src, &use_src);

   if (zink_screen(ctx->base.screen)->driver_workarounds.general_layout) {
      zink_resource_image_transfer_dst_barrier(ctx, dst, info->dst.level, &info->dst.box, false);
      screen->image_barrier(ctx, use_src,
                              VK_IMAGE_LAYOUT_GENERAL,
                              VK_ACCESS_TRANSFER_READ_BIT | VK_ACCESS_TRANSFER_WRITE_BIT,
                              VK_PIPELINE_STAGE_TRANSFER_BIT);
   } else {
      zink_resource_setup_transfer_layouts(ctx, use_src, dst);
   }
   VkCommandBuffer cmdbuf = *needs_present_readback ?
                            ctx->bs->cmdbuf :
                            zink_get_cmdbuf(ctx, src, dst);
   zink_batch_reference_resource_rw(ctx, use_src, false);
   zink_batch_reference_resource_rw(ctx, dst, true);

   bool marker = zink_cmd_debug_marker_begin(ctx, cmdbuf, "blit_native(%s->%s, %dx%d->%dx%d)",
                                             util_format_short_name(info->src.format),
                                             util_format_short_name(info->src.format),
                                             info->src.box.width, info->src.box.height,
                                             info->dst.box.width, info->dst.box.height);

   VKCTX(CmdBlitImage)(cmdbuf, use_src->obj->image, src->layout,
                  dst->obj->image, dst->layout,
                  1, &region,
                  zink_filter(info->filter));

   zink_cmd_debug_marker_end(ctx, cmdbuf, marker);

   return true;
}

static bool
try_copy_region(struct pipe_context *pctx, const struct pipe_blit_info *info)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_resource *src = zink_resource(info->src.resource);
   struct zink_resource *dst = zink_resource(info->dst.resource);
   /* if we're copying between resources with matching aspects then we can probably just copy_region */
   if (src->aspect != dst->aspect)
      return false;
   struct pipe_blit_info new_info = *info;

   if (src->aspect & VK_IMAGE_ASPECT_STENCIL_BIT &&
       new_info.render_condition_enable &&
       !ctx->render_condition_active)
      new_info.render_condition_enable = false;

   return util_try_blit_via_copy_region(pctx, &new_info, ctx->render_condition_active);
}

void
zink_blit(struct pipe_context *pctx,
          const struct pipe_blit_info *info)
{
   struct zink_context *ctx = zink_context(pctx);
   const struct util_format_description *src_desc = util_format_description(info->src.format);
   const struct util_format_description *dst_desc = util_format_description(info->dst.format);

   struct zink_resource *src = zink_resource(info->src.resource);
   struct zink_resource *use_src = src;
   struct zink_resource *dst = zink_resource(info->dst.resource);
   bool needs_present_readback = false;

   if (ctx->awaiting_resolve && ctx->in_rp && ctx->dynamic_fb.tc_info.has_resolve) {
      struct pipe_resource *resolve = ctx->fb_state.resolve;
      if (!resolve)
         resolve = ctx->dynamic_fb.tc_info.resolve;
      if (resolve == info->dst.resource) {
         zink_batch_no_rp_safe(ctx);
         ctx->awaiting_resolve = false;
         return;
      }
   }

   if (zink_is_swapchain(dst)) {
      if (!zink_kopper_acquire(ctx, dst, UINT64_MAX))
         return;
   }

   if (src_desc == dst_desc ||
       src_desc->nr_channels != 4 || src_desc->layout != UTIL_FORMAT_LAYOUT_PLAIN ||
       (src_desc->nr_channels == 4 && src_desc->channel[3].type != UTIL_FORMAT_TYPE_VOID)) {
      /* we can't blit RGBX -> RGBA formats directly since they're emulated
       * so we have to use sampler views
       */
      if (info->src.resource->nr_samples > 1 &&
          info->dst.resource->nr_samples <= 1) {
         if (blit_resolve(ctx, info, &needs_present_readback))
            goto end;
      } else {
         if (try_copy_region(pctx, info))
            goto end;
         if (blit_native(ctx, info, &needs_present_readback))
            goto end;
      }
   }



   bool stencil_blit = false;
   if (!util_blitter_is_blit_supported(ctx->blitter, info)) {
      if (util_format_is_depth_or_stencil(info->src.resource->format)) {
         if (info->mask & PIPE_MASK_Z) {
            struct pipe_blit_info depth_blit = *info;
            depth_blit.mask = PIPE_MASK_Z;
            if (util_blitter_is_blit_supported(ctx->blitter, &depth_blit)) {
               zink_blit_begin(ctx, ZINK_BLIT_SAVE_FB | ZINK_BLIT_SAVE_FS | ZINK_BLIT_SAVE_TEXTURES);
               util_blitter_blit(ctx->blitter, &depth_blit, NULL);
            } else {
               mesa_loge("ZINK: depth blit unsupported %s -> %s",
                         util_format_short_name(info->src.resource->format),
                         util_format_short_name(info->dst.resource->format));
            }
         }
         if (info->mask & PIPE_MASK_S)
            stencil_blit = true;
      }
      if (!stencil_blit) {
         mesa_loge("ZINK: blit unsupported %s -> %s",
                 util_format_short_name(info->src.resource->format),
                 util_format_short_name(info->dst.resource->format));
         goto end;
      }
   }

   if (src->obj->dt) {
      zink_fb_clears_apply_region(ctx, info->src.resource, zink_rect_from_box(&info->src.box), info->src.box.z, info->src.box.depth);
      needs_present_readback = zink_kopper_acquire_readback(ctx, src, &use_src);
   }

   /* this is discard_only because we're about to start a renderpass that will
    * flush all pending clears anyway
    */
   apply_dst_clears(ctx, info, true);
   zink_fb_clears_apply_region(ctx, info->src.resource, zink_rect_from_box(&info->src.box), info->src.box.z, info->src.box.depth);
   unsigned rp_clears_enabled = ctx->rp_clears_enabled;
   unsigned clears_enabled = ctx->clears_enabled;
   if (!dst->fb_bind_count) {
      /* avoid applying clears from fb unbind by storing and re-setting them after the blit */
      ctx->rp_clears_enabled = 0;
      ctx->clears_enabled = 0;
   } else {
      unsigned bit;
      /* convert to PIPE_CLEAR_XYZ */
      if (dst->fb_binds & BITFIELD_BIT(PIPE_MAX_COLOR_BUFS))
         bit = PIPE_CLEAR_DEPTHSTENCIL;
      else
         bit = dst->fb_binds << 2;
      rp_clears_enabled &= ~bit;
      clears_enabled &= ~bit;
      ctx->rp_clears_enabled &= bit;
      ctx->clears_enabled &= bit;
   }

   /* this will draw a full-resource quad, so ignore existing data */
   bool whole = util_blit_covers_whole_resource(info);
   if (whole)
      pctx->invalidate_resource(pctx, info->dst.resource);

   ctx->unordered_blitting = !(info->render_condition_enable && ctx->render_condition_active) &&
                             !needs_present_readback &&
                             zink_get_cmdbuf(ctx, src, dst) == ctx->bs->reordered_cmdbuf;
   VkCommandBuffer cmdbuf = ctx->bs->cmdbuf;
   VkPipeline pipeline = ctx->gfx_pipeline_state.pipeline;
   bool in_rp = ctx->in_rp;
   uint64_t tc_data = ctx->dynamic_fb.tc_info.data;
   bool queries_disabled = ctx->queries_disabled;
   bool rp_changed = ctx->rp_changed || (!ctx->fb_state.zsbuf.texture && util_format_is_depth_or_stencil(info->dst.format));
   unsigned ds3_states = ctx->ds3_states;
   bool rp_tc_info_updated = ctx->rp_tc_info_updated;
   if (ctx->unordered_blitting) {
      /* for unordered blit, swap the unordered cmdbuf for the main one for the whole op to avoid conditional hell */
      ctx->bs->cmdbuf = ctx->bs->reordered_cmdbuf;
      ctx->in_rp = false;
      ctx->rp_changed = true;
      ctx->queries_disabled = true;
      ctx->pipeline_changed[0] = true;
      zink_reset_ds3_states(ctx);
      zink_select_draw_vbo(ctx);
   }
   zink_blit_begin(ctx, ZINK_BLIT_SAVE_FB | ZINK_BLIT_SAVE_FS | ZINK_BLIT_SAVE_TEXTURES);
   if (zink_format_needs_mutable(info->src.format, info->src.resource->format))
      zink_resource_object_init_mutable(ctx, src);
   if (zink_format_needs_mutable(info->dst.format, info->dst.resource->format))
      zink_resource_object_init_mutable(ctx, dst);
   zink_blit_barriers(ctx, use_src, dst, whole);
   ctx->blitting = true;
   ctx->blit_scissor = info->scissor_enable;
   ctx->blit_nearest = info->filter == PIPE_TEX_FILTER_NEAREST;

   if (stencil_blit) {
      struct pipe_surface dst_templ;
      util_blitter_default_dst_texture(&dst_templ, info->dst.resource, info->dst.level, info->dst.box.z);

      util_blitter_clear_depth_stencil(ctx->blitter, &dst_templ, PIPE_CLEAR_STENCIL,
                                       0, 0, info->dst.box.x, info->dst.box.y,
                                       info->dst.box.width, info->dst.box.height);
      zink_blit_begin(ctx, ZINK_BLIT_SAVE_FB | ZINK_BLIT_SAVE_FS | ZINK_BLIT_SAVE_TEXTURES | ZINK_BLIT_SAVE_FS_CONST_BUF);
      util_blitter_stencil_fallback(ctx->blitter,
                                    info->dst.resource,
                                    info->dst.level,
                                    &info->dst.box,
                                    info->src.resource,
                                    info->src.level,
                                    &info->src.box,
                                    info->scissor_enable ? &info->scissor : NULL);
   } else {
      struct pipe_blit_info new_info = *info;
      new_info.src.resource = &use_src->base.b;
      util_blitter_blit(ctx->blitter, &new_info, NULL);
   }
   ctx->blitting = false;
   ctx->rp_clears_enabled = rp_clears_enabled;
   ctx->clears_enabled = clears_enabled;
   if (ctx->unordered_blitting) {
      zink_batch_no_rp(ctx);
      ctx->in_rp = in_rp;
      ctx->gfx_pipeline_state.rp_state = zink_update_rendering_info(ctx);
      ctx->rp_changed = rp_changed;
      ctx->rp_tc_info_updated |= rp_tc_info_updated;
      ctx->queries_disabled = queries_disabled;
      ctx->dynamic_fb.tc_info.data = tc_data;
      ctx->bs->cmdbuf = cmdbuf;
      ctx->gfx_pipeline_state.pipeline = pipeline;
      ctx->pipeline_changed[0] = true;
      ctx->ds3_states = ds3_states;
      zink_select_draw_vbo(ctx);
   }
   ctx->unordered_blitting = false;
end:
   if (needs_present_readback) {
      src->obj->unordered_read = false;
      dst->obj->unordered_write = false;
      zink_kopper_present_readback(ctx, src);
   }
}

/* similar to radeonsi */
void
zink_blit_begin(struct zink_context *ctx, enum zink_blit_flags flags)
{
   util_blitter_save_vertex_elements(ctx->blitter, ctx->element_state);
   util_blitter_save_viewport(ctx->blitter, ctx->vp_state.viewport_states);

   util_blitter_save_vertex_buffers(ctx->blitter, ctx->vertex_buffers,
                                    util_last_bit(ctx->gfx_pipeline_state.vertex_buffers_enabled_mask));
   util_blitter_save_vertex_shader(ctx->blitter, ctx->gfx_stages[MESA_SHADER_VERTEX]);
   util_blitter_save_tessctrl_shader(ctx->blitter, ctx->gfx_stages[MESA_SHADER_TESS_CTRL]);
   util_blitter_save_tesseval_shader(ctx->blitter, ctx->gfx_stages[MESA_SHADER_TESS_EVAL]);
   util_blitter_save_geometry_shader(ctx->blitter, ctx->gfx_stages[MESA_SHADER_GEOMETRY]);
   util_blitter_save_rasterizer(ctx->blitter, ctx->rast_state);
   util_blitter_save_so_targets(ctx->blitter, ctx->num_so_targets, ctx->so_targets, MESA_PRIM_UNKNOWN);

   if (flags & ZINK_BLIT_SAVE_FS_CONST_BUF)
      util_blitter_save_fragment_constant_buffer_slot(ctx->blitter, ctx->ubos[MESA_SHADER_FRAGMENT]);

   if (flags & ZINK_BLIT_SAVE_FS) {
      util_blitter_save_blend(ctx->blitter, ctx->gfx_pipeline_state.blend_state);
      util_blitter_save_depth_stencil_alpha(ctx->blitter, ctx->dsa_state);
      util_blitter_save_stencil_ref(ctx->blitter, &ctx->stencil_ref);
      util_blitter_save_sample_mask(ctx->blitter, ctx->gfx_pipeline_state.sample_mask, ctx->gfx_pipeline_state.min_samples + 1);
      util_blitter_save_scissor(ctx->blitter, ctx->vp_state.scissor_states);
      /* also util_blitter_save_window_rectangles when we have that? */

      util_blitter_save_fragment_shader(ctx->blitter, ctx->gfx_stages[MESA_SHADER_FRAGMENT]);
   }

   if (flags & ZINK_BLIT_SAVE_FB)
      util_blitter_save_framebuffer(ctx->blitter, &ctx->fb_state);


   if (flags & ZINK_BLIT_SAVE_TEXTURES) {
      util_blitter_save_fragment_sampler_states(ctx->blitter,
                                                ctx->di.num_samplers[MESA_SHADER_FRAGMENT],
                                                (void**)ctx->sampler_states[MESA_SHADER_FRAGMENT]);
      util_blitter_save_fragment_sampler_views(ctx->blitter,
                                               ctx->di.num_sampler_views[MESA_SHADER_FRAGMENT],
                                               ctx->sampler_views[MESA_SHADER_FRAGMENT]);
   }

   if (flags & ZINK_BLIT_NO_COND_RENDER && ctx->render_condition_active)
      zink_stop_conditional_render(ctx);
}

void
zink_blit_barriers(struct zink_context *ctx, struct zink_resource *src, struct zink_resource *dst, bool whole_dst)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   if (src && zink_is_swapchain(src)) {
      if (!zink_kopper_acquire(ctx, src, UINT64_MAX))
         return;
   } else if (dst && zink_is_swapchain(dst)) {
      if (!zink_kopper_acquire(ctx, dst, UINT64_MAX))
         return;
   }

   VkAccessFlagBits flags;
   VkPipelineStageFlagBits pipeline;
   if (util_format_is_depth_or_stencil(dst->base.b.format)) {
      flags = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
      if (!whole_dst)
         flags |= VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT;
      pipeline = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT | VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT;
   } else {
      flags = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
      if (!whole_dst)
         flags |= VK_ACCESS_COLOR_ATTACHMENT_READ_BIT;
      pipeline = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
   }
   if (src == dst) {
      VkImageLayout layout = !screen->driver_workarounds.general_layout && screen->info.have_EXT_attachment_feedback_loop_layout ?
                             VK_IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT :
                             VK_IMAGE_LAYOUT_GENERAL;
      screen->image_barrier(ctx, src, layout, VK_ACCESS_SHADER_READ_BIT | flags, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT | pipeline);
   } else {
      if (src) {
         VkImageLayout layout = screen->driver_workarounds.general_layout ? VK_IMAGE_LAYOUT_GENERAL :
                                util_format_is_depth_or_stencil(src->base.b.format) && src->obj->vkusage & VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT ?
                                VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL :
                                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
         screen->image_barrier(ctx, src, layout,
                              VK_ACCESS_SHADER_READ_BIT, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT);
         if (!ctx->unordered_blitting)
            src->obj->unordered_read = false;
      }
      VkImageLayout layout = screen->driver_workarounds.general_layout ? VK_IMAGE_LAYOUT_GENERAL :
                             util_format_is_depth_or_stencil(dst->base.b.format) ?
                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL :
                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
      screen->image_barrier(ctx, dst, layout, flags, pipeline);
   }
   if (!ctx->unordered_blitting)
      dst->obj->unordered_read = dst->obj->unordered_write = false;
}

bool
zink_blit_region_fills(struct u_rect region, unsigned width, unsigned height)
{
   struct u_rect intersect = {0, width, 0, height};
   struct u_rect r = {
      MIN2(region.x0, region.x1),
      MAX2(region.x0, region.x1),
      MIN2(region.y0, region.y1),
      MAX2(region.y0, region.y1),
   };

   if (!u_rect_test_intersection(&r, &intersect))
      /* is this even a thing? */
      return false;

   u_rect_find_intersection(&r, &intersect);
   if (intersect.x0 != 0 || intersect.y0 != 0 ||
       intersect.x1 != width || intersect.y1 != height)
      return false;

   return true;
}

bool
zink_blit_region_covers(struct u_rect region, struct u_rect covers)
{
   struct u_rect r = {
      MIN2(region.x0, region.x1),
      MAX2(region.x0, region.x1),
      MIN2(region.y0, region.y1),
      MAX2(region.y0, region.y1),
   };
   struct u_rect c = {
      MIN2(covers.x0, covers.x1),
      MAX2(covers.x0, covers.x1),
      MIN2(covers.y0, covers.y1),
      MAX2(covers.y0, covers.y1),
   };
   struct u_rect intersect;
   if (!u_rect_test_intersection(&r, &c))
      return false;

    u_rect_union(&intersect, &r, &c);
    return intersect.x0 == c.x0 && intersect.y0 == c.y0 &&
           intersect.x1 == c.x1 && intersect.y1 == c.y1;
}

void
zink_draw_rectangle(struct blitter_context *blitter, void *vertex_elements_cso,
                    blitter_get_vs_func get_vs, int x1, int y1, int x2, int y2,
                    float depth, unsigned num_instances, enum blitter_attrib_type type,
                    const struct blitter_attrib *attrib)
{
   struct zink_context *ctx = zink_context(blitter->pipe);

   struct blitter_attrib new_attrib = *attrib;

   /* Avoid inconsistencies in rounding between both triangles which can show with
    * nearest filtering by expanding the rect so only one triangle is effectively drawn.
    */
   if (ctx->blit_scissor && ctx->blit_nearest) {
      int64_t new_x1 = (int64_t)x1 * 2 - x2;
      int64_t new_y2 = (int64_t)y2 * 2 - y1;
      if (new_x1 < INT32_MAX && new_x1 > INT32_MIN &&
          new_y2 < INT32_MAX && new_y2 > INT32_MIN) {
         x1 = new_x1;
         y2 = new_y2;

         if (type == UTIL_BLITTER_ATTRIB_TEXCOORD_XY ||
             type == UTIL_BLITTER_ATTRIB_TEXCOORD_XYZW) {
            new_attrib.texcoord.x1 += new_attrib.texcoord.x1 - new_attrib.texcoord.x2;
            new_attrib.texcoord.y2 += new_attrib.texcoord.y2 - new_attrib.texcoord.y1;
         }
      }
   }

   util_blitter_draw_rectangle(blitter, vertex_elements_cso, get_vs, x1, y1, x2, y2,
                               depth, num_instances, type, &new_attrib);
}

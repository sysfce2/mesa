/*
 * Copyright 2018 Collabora Ltd.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "zink_clear.h"
#include "zink_context.h"
#include "zink_descriptors.h"
#include "zink_fence.h"
#include "zink_format.h"
#include "zink_helpers.h"
#include "zink_inlines.h"
#include "zink_kopper.h"
#include "zink_program.h"
#include "zink_query.h"
#include "zink_render_pass.h"
#include "zink_resource.h"
#include "zink_screen.h"
#include "zink_state.h"
#include "zink_surface.h"


#include "nir/pipe_nir.h"
#include "util/u_blitter.h"
#include "util/u_debug.h"
#include "util/format_srgb.h"
#include "util/format/u_format.h"
#include "util/u_helpers.h"
#include "util/u_inlines.h"
#include "util/u_sample_positions.h"
#include "util/u_string.h"
#include "util/u_thread.h"
#include "util/perf/u_trace.h"
#include "util/u_cpu_detect.h"
#include "util/thread_sched.h"
#include "util/strndup.h"
#include "nir.h"
#include "nir_builder.h"

#include "vk_format.h"

#include "driver_trace/tr_context.h"

#include "util/u_memory.h"
#include "util/u_upload_mgr.h"

#define XXH_INLINE_ALL
#include "util/xxhash.h"

static void
update_tc_info(struct zink_context *ctx)
{
   if (ctx->track_renderpasses) {
      const struct tc_renderpass_info *info = threaded_context_get_renderpass_info(ctx->tc);
      ctx->rp_changed |= ctx->dynamic_fb.tc_info.data != info->data;
      ctx->dynamic_fb.tc_info.data = info->data;
      ctx->dynamic_fb.tc_info.resolve = info->resolve;
      ctx->awaiting_resolve = ctx->dynamic_fb.tc_info.has_resolve;
   } else {
      struct tc_renderpass_info info = ctx->dynamic_fb.tc_info;
      bool zsbuf_used = !ctx->zsbuf_unused;
      bool zsbuf_write = zink_is_zsbuf_write(ctx);
      ctx->dynamic_fb.tc_info.data32[0] = 0;
      if (ctx->clears_enabled & PIPE_CLEAR_DEPTHSTENCIL)
         ctx->dynamic_fb.tc_info.zsbuf_clear_partial = true;
      if (ctx->rp_clears_enabled & PIPE_CLEAR_DEPTHSTENCIL)
         ctx->dynamic_fb.tc_info.zsbuf_clear = true;
      if (ctx->dynamic_fb.tc_info.zsbuf_clear != info.zsbuf_clear)
         ctx->rp_loadop_changed = true;
      if (zink_is_zsbuf_write(ctx) != zsbuf_write)
         ctx->rp_layout_changed = true;
      ctx->rp_changed |= zink_is_zsbuf_used(ctx) != zsbuf_used;
   }
}

ALWAYS_INLINE static void
check_resource_for_batch_ref(struct zink_context *ctx, struct zink_resource *res)
{
   if (!zink_resource_has_binds(res)) {
      /* avoid desync between usage and tracking:
       * - if usage exists, it must be removed before the context is destroyed
       * - having usage does not imply having tracking
       * - if tracking will be added here, also reapply usage to avoid dangling usage once tracking is removed
       * TODO: somehow fix this for perf because it's an extra hash lookup
       */
      if (!res->obj->dt && zink_resource_has_usage(res))
         zink_batch_reference_resource_rw(ctx, res, !!res->obj->bo->writes.u);
      else
         zink_batch_reference_resource(ctx, res);
   }
}

static void
zink_context_destroy(struct pipe_context *pctx)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_screen *screen = zink_screen(pctx->screen);

   struct pipe_framebuffer_state fb = {0};
   pctx->set_framebuffer_state(pctx, &fb);

   if (screen->base.num_contexts == 1 && screen->renderdoc_capturing) {
      screen->renderdoc_capture_all = false;
      ctx->bs->has_work = true;
      pctx->flush(pctx, NULL, 0);
   }

   if (util_queue_is_initialized(&screen->flush_queue))
      util_queue_finish(&screen->flush_queue);
   if (ctx->bs && !screen->device_lost) {
      simple_mtx_lock(&screen->queue_lock);
      VkResult result = VKSCR(QueueWaitIdle)(screen->queue);
      simple_mtx_unlock(&screen->queue_lock);

      if (result != VK_SUCCESS)
         mesa_loge("ZINK: vkQueueWaitIdle failed (%s)", vk_Result_to_str(result));

      if (screen->queue_sparse && screen->queue_sparse != screen->queue) {
         simple_mtx_lock(&screen->queue_lock);
         VkResult result = VKSCR(QueueWaitIdle)(screen->queue_sparse);
         simple_mtx_unlock(&screen->queue_lock);

         if (result != VK_SUCCESS)
            mesa_loge("ZINK: vkQueueWaitIdle failed (%s)", vk_Result_to_str(result));
      }
   }

   for (unsigned i = 0; i < ARRAY_SIZE(ctx->program_cache); i++) {
      simple_mtx_lock((&ctx->program_lock[i]));
      hash_table_foreach(&ctx->program_cache[i], entry) {
         struct zink_program *pg = entry->data;
         zink_program_finish(ctx, pg);
         pg->removed = true;
      }
      simple_mtx_unlock((&ctx->program_lock[i]));
   }

   if (ctx->blitter)
      util_blitter_destroy(ctx->blitter);

   pipe_resource_reference(&ctx->dummy_xfb_buffer, NULL);

   zink_descriptors_deinit_bindless(ctx);

   struct zink_batch_state *bs = ctx->batch_states;
   while (bs) {
      struct zink_batch_state *bs_next = bs->next;
      zink_clear_batch_state(ctx, bs);
      /* restore link as we insert them into the screens free_batch_states
       * list below
       */
      bs->next = bs_next;
      bs = bs_next;
   }
   bs = ctx->free_batch_states;
   while (bs) {
      struct zink_batch_state *bs_next = bs->next;
      zink_clear_batch_state(ctx, bs);
      bs->ctx = NULL;
      /* restore link as we insert them into the screens free_batch_states
       * list below
       */
      bs->next = bs_next;
      bs = bs_next;
   }
   simple_mtx_lock(&screen->free_batch_states_lock);
   if (ctx->batch_states) {
      if (screen->free_batch_states)
         screen->last_free_batch_state->next = ctx->batch_states;
      else {
         screen->free_batch_states = ctx->batch_states;
         screen->last_free_batch_state = screen->free_batch_states;
      }
   }
   while (screen->last_free_batch_state && screen->last_free_batch_state->next)
      screen->last_free_batch_state = screen->last_free_batch_state->next;
   if (ctx->free_batch_states) {
      if (screen->free_batch_states)
         screen->last_free_batch_state->next = ctx->free_batch_states;
      else {
         screen->free_batch_states = ctx->free_batch_states;
         screen->last_free_batch_state = ctx->last_free_batch_state;
      }
   }
   while (screen->last_free_batch_state && screen->last_free_batch_state->next)
      screen->last_free_batch_state = screen->last_free_batch_state->next;
   if (ctx->bs) {
      zink_clear_batch_state(ctx, ctx->bs);
      if (screen->free_batch_states)
         screen->last_free_batch_state->next = ctx->bs;
      else {
         screen->free_batch_states = ctx->bs;
         screen->last_free_batch_state = screen->free_batch_states;
      }
   }
   while (screen->last_free_batch_state && screen->last_free_batch_state->next)
      screen->last_free_batch_state = screen->last_free_batch_state->next;
   simple_mtx_unlock(&screen->free_batch_states_lock);

   for (unsigned i = 0; i < 2; i++) {
      util_idalloc_fini(&ctx->di.bindless[i].tex_slots);
      util_idalloc_fini(&ctx->di.bindless[i].img_slots);
      if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB)
         free(ctx->di.bindless[i].db.buffer_infos);
      else
         free(ctx->di.bindless[i].t.buffer_infos);
      free(ctx->di.bindless[i].img_infos);
      util_dynarray_fini(&ctx->di.bindless[i].updates);
      util_dynarray_fini(&ctx->di.bindless[i].resident);
   }

   if (ctx->null_fs)
      pctx->delete_fs_state(pctx, ctx->null_fs);

   zink_context_destroy_query_pools(ctx);
   set_foreach(&ctx->gfx_inputs, he) {
      struct zink_gfx_input_key *ikey = (void*)he->key;
      VKSCR(DestroyPipeline)(screen->dev, ikey->pipeline, NULL);
   }
   set_foreach(&ctx->gfx_outputs, he) {
      struct zink_gfx_output_key *okey = (void*)he->key;
      VKSCR(DestroyPipeline)(screen->dev, okey->pipeline, NULL);
   }
   u_upload_destroy(pctx->stream_uploader);
   u_upload_destroy(pctx->const_uploader);
   slab_destroy_child(&ctx->transfer_pool);
   for (unsigned i = 0; i < ARRAY_SIZE(ctx->program_cache); i++)
      _mesa_hash_table_clear(&ctx->program_cache[i], NULL);
   for (unsigned i = 0; i < ARRAY_SIZE(ctx->program_lock); i++)
      simple_mtx_destroy(&ctx->program_lock[i]);
   slab_destroy_child(&ctx->transfer_pool_unsync);

   zink_descriptors_deinit(ctx);

   if (!(ctx->flags & ZINK_CONTEXT_COPY_ONLY))
      p_atomic_dec(&screen->base.num_contexts);

   util_dynarray_foreach(&ctx->di.global_bindings, struct pipe_resource *, res) {
      pipe_resource_reference(res, NULL);
   }
   util_dynarray_fini(&ctx->di.global_bindings);

   ralloc_free(ctx);
}

static void
check_device_lost(struct zink_context *ctx)
{
   if (!zink_screen(ctx->base.screen)->device_lost || ctx->is_device_lost)
      return;
   debug_printf("ZINK: device lost detected!\n");
   if (ctx->reset.reset)
      ctx->reset.reset(ctx->reset.data, PIPE_GUILTY_CONTEXT_RESET);
   ctx->is_device_lost = true;
}

static enum pipe_reset_status
zink_get_device_reset_status(struct pipe_context *pctx)
{
   struct zink_context *ctx = zink_context(pctx);

   enum pipe_reset_status status = PIPE_NO_RESET;

   if (ctx->is_device_lost) {
      // Since we don't know what really happened to the hardware, just
      // assume that we are in the wrong
      status = PIPE_GUILTY_CONTEXT_RESET;

      debug_printf("ZINK: device lost detected!\n");

      if (ctx->reset.reset)
         ctx->reset.reset(ctx->reset.data, status);
   }

   return status;
}

static void
zink_set_device_reset_callback(struct pipe_context *pctx,
                               const struct pipe_device_reset_callback *cb)
{
   struct zink_context *ctx = zink_context(pctx);
   bool had_reset = !!ctx->reset.reset;

   if (cb)
      ctx->reset = *cb;
   else
      memset(&ctx->reset, 0, sizeof(ctx->reset));

   bool have_reset = !!ctx->reset.reset;
   if (had_reset != have_reset) {
      if (have_reset)
         p_atomic_inc(&zink_screen(pctx->screen)->robust_ctx_count);
      else
         p_atomic_dec(&zink_screen(pctx->screen)->robust_ctx_count);
   }
}

static void
zink_set_context_param(struct pipe_context *pctx, enum pipe_context_param param,
                       unsigned value)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_screen *screen = zink_screen(ctx->base.screen);

   switch (param) {
   case PIPE_CONTEXT_PARAM_UPDATE_THREAD_SCHEDULING:
      if (screen->threaded_submit)
         util_thread_sched_apply_policy(screen->flush_queue.threads[0],
                                        UTIL_THREAD_DRIVER_SUBMIT, value, NULL);
      break;
   default:
      break;
   }
}

static void
zink_set_debug_callback(struct pipe_context *pctx, const struct util_debug_callback *cb)
{
   struct zink_context *ctx = zink_context(pctx);

   if (cb)
      ctx->dbg = *cb;
   else
      memset(&ctx->dbg, 0, sizeof(ctx->dbg));
}

static VkSamplerMipmapMode
sampler_mipmap_mode(enum pipe_tex_mipfilter filter)
{
   switch (filter) {
   case PIPE_TEX_MIPFILTER_NEAREST: return VK_SAMPLER_MIPMAP_MODE_NEAREST;
   case PIPE_TEX_MIPFILTER_LINEAR: return VK_SAMPLER_MIPMAP_MODE_LINEAR;
   case PIPE_TEX_MIPFILTER_NONE:
      UNREACHABLE("PIPE_TEX_MIPFILTER_NONE should be dealt with earlier");
   }
   UNREACHABLE("unexpected filter");
}

static VkSamplerAddressMode
sampler_address_mode(enum pipe_tex_wrap filter)
{
   switch (filter) {
   case PIPE_TEX_WRAP_REPEAT: return VK_SAMPLER_ADDRESS_MODE_REPEAT;
   case PIPE_TEX_WRAP_CLAMP_TO_EDGE: return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
   case PIPE_TEX_WRAP_CLAMP_TO_BORDER: return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
   case PIPE_TEX_WRAP_MIRROR_REPEAT: return VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
   case PIPE_TEX_WRAP_MIRROR_CLAMP_TO_EDGE: return VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE;
   case PIPE_TEX_WRAP_MIRROR_CLAMP_TO_BORDER: return VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE; /* not technically correct, but kinda works */
   default: break;
   }
   UNREACHABLE("unexpected wrap");
}

/* unnormalizedCoordinates only support CLAMP_TO_EDGE or CLAMP_TO_BORDER */
static VkSamplerAddressMode
sampler_address_mode_unnormalized(enum pipe_tex_wrap filter)
{
   switch (filter) {
   case PIPE_TEX_WRAP_CLAMP_TO_EDGE: return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
   case PIPE_TEX_WRAP_CLAMP_TO_BORDER: return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
   default: break;
   }
   UNREACHABLE("unexpected wrap");
}

static VkCompareOp
compare_op(enum pipe_compare_func op)
{
   switch (op) {
      case PIPE_FUNC_NEVER: return VK_COMPARE_OP_NEVER;
      case PIPE_FUNC_LESS: return VK_COMPARE_OP_LESS;
      case PIPE_FUNC_EQUAL: return VK_COMPARE_OP_EQUAL;
      case PIPE_FUNC_LEQUAL: return VK_COMPARE_OP_LESS_OR_EQUAL;
      case PIPE_FUNC_GREATER: return VK_COMPARE_OP_GREATER;
      case PIPE_FUNC_NOTEQUAL: return VK_COMPARE_OP_NOT_EQUAL;
      case PIPE_FUNC_GEQUAL: return VK_COMPARE_OP_GREATER_OR_EQUAL;
      case PIPE_FUNC_ALWAYS: return VK_COMPARE_OP_ALWAYS;
   }
   UNREACHABLE("unexpected compare");
}

static inline bool
wrap_needs_border_color(unsigned wrap)
{
   return wrap == PIPE_TEX_WRAP_CLAMP || wrap == PIPE_TEX_WRAP_CLAMP_TO_BORDER ||
          wrap == PIPE_TEX_WRAP_MIRROR_CLAMP || wrap == PIPE_TEX_WRAP_MIRROR_CLAMP_TO_BORDER;
}

static VkBorderColor
get_border_color(const union pipe_color_union *color, bool is_integer, bool need_custom)
{
   if (is_integer) {
      if (color->ui[0] == 0 && color->ui[1] == 0 && color->ui[2] == 0 && color->ui[3] == 0)
         return VK_BORDER_COLOR_INT_TRANSPARENT_BLACK;
      if (color->ui[0] == 0 && color->ui[1] == 0 && color->ui[2] == 0 && color->ui[3] == 1)
         return VK_BORDER_COLOR_INT_OPAQUE_BLACK;
      if (color->ui[0] == 1 && color->ui[1] == 1 && color->ui[2] == 1 && color->ui[3] == 1)
         return VK_BORDER_COLOR_INT_OPAQUE_WHITE;
      return need_custom ? VK_BORDER_COLOR_INT_CUSTOM_EXT : VK_BORDER_COLOR_INT_TRANSPARENT_BLACK;
   }

   if (color->f[0] == 0 && color->f[1] == 0 && color->f[2] == 0 && color->f[3] == 0)
      return VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
   if (color->f[0] == 0 && color->f[1] == 0 && color->f[2] == 0 && color->f[3] == 1)
      return VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK;
   if (color->f[0] == 1 && color->f[1] == 1 && color->f[2] == 1 && color->f[3] == 1)
      return VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE;
   return need_custom ? VK_BORDER_COLOR_FLOAT_CUSTOM_EXT : VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
}

static void *
zink_create_sampler_state(struct pipe_context *pctx,
                          const struct pipe_sampler_state *state)
{
   struct zink_screen *screen = zink_screen(pctx->screen);
   ASSERTED struct zink_context *zink = zink_context(pctx);
   bool need_custom = false;
   bool need_clamped_border_color = false;
   VkSamplerCreateInfo sci = {0};
   VkSamplerCustomBorderColorCreateInfoEXT cbci = {0};
   VkSamplerCustomBorderColorCreateInfoEXT cbci_clamped = {0};
   sci.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
   if (screen->info.have_EXT_non_seamless_cube_map && !state->seamless_cube_map)
      sci.flags |= VK_SAMPLER_CREATE_NON_SEAMLESS_CUBE_MAP_BIT_EXT;
   if (state->unnormalized_coords) {
      assert(zink->flags & PIPE_CONTEXT_COMPUTE_ONLY);
      sci.unnormalizedCoordinates = state->unnormalized_coords;
   }
   sci.magFilter = zink_filter(state->mag_img_filter);
   if (sci.unnormalizedCoordinates)
      sci.minFilter = sci.magFilter;
   else
      sci.minFilter = zink_filter(state->min_img_filter);

   VkSamplerReductionModeCreateInfo rci;
   rci.sType = VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO;
   rci.pNext = NULL;
   switch (state->reduction_mode) {
   case PIPE_TEX_REDUCTION_MIN:
      rci.reductionMode = VK_SAMPLER_REDUCTION_MODE_MIN;
      break;
   case PIPE_TEX_REDUCTION_MAX:
      rci.reductionMode = VK_SAMPLER_REDUCTION_MODE_MAX;
      break;
   default:
      rci.reductionMode = VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE;
      break;
   }
   if (state->reduction_mode)
      sci.pNext = &rci;

   if (sci.unnormalizedCoordinates) {
      sci.mipmapMode = VK_SAMPLER_MIPMAP_MODE_NEAREST;
   } else if (state->min_mip_filter != PIPE_TEX_MIPFILTER_NONE) {
      sci.mipmapMode = sampler_mipmap_mode(state->min_mip_filter);
      sci.minLod = state->min_lod;
      sci.maxLod = MAX2(state->max_lod, state->min_lod);
   } else {
      sci.mipmapMode = VK_SAMPLER_MIPMAP_MODE_NEAREST;
      sci.minLod = CLAMP(state->min_lod, 0.0f, 0.25f);
      sci.maxLod = CLAMP(state->max_lod, 0.0f, 0.25f);
   }

   if (!sci.unnormalizedCoordinates) {
      sci.addressModeU = sampler_address_mode(state->wrap_s);
      sci.addressModeV = sampler_address_mode(state->wrap_t);
      sci.addressModeW = sampler_address_mode(state->wrap_r);
   } else {
      sci.addressModeU = sampler_address_mode_unnormalized(state->wrap_s);
      sci.addressModeV = sampler_address_mode_unnormalized(state->wrap_t);
      sci.addressModeW = sampler_address_mode_unnormalized(state->wrap_r);
   }

   sci.mipLodBias = CLAMP(state->lod_bias,
                          -screen->info.props.limits.maxSamplerLodBias,
                          screen->info.props.limits.maxSamplerLodBias);

   need_custom |= wrap_needs_border_color(state->wrap_s);
   need_custom |= wrap_needs_border_color(state->wrap_t);
   need_custom |= wrap_needs_border_color(state->wrap_r);

   if (state->compare_mode == PIPE_TEX_COMPARE_NONE)
      sci.compareOp = VK_COMPARE_OP_NEVER;
   else {
      sci.compareOp = compare_op(state->compare_func);
      sci.compareEnable = VK_TRUE;
   }

   bool is_integer = state->border_color_is_integer;

   sci.borderColor = get_border_color(&state->border_color, is_integer, need_custom);
   if (sci.borderColor > VK_BORDER_COLOR_INT_OPAQUE_WHITE && need_custom) {
      if (!screen->info.border_color_feats.customBorderColorWithoutFormat &&
          zink_driverid(screen) != VK_DRIVER_ID_MESA_TURNIP) {
         static bool warned = false;
         warn_missing_feature(warned, "customBorderColorWithoutFormat");
      }
      if (screen->info.have_EXT_custom_border_color &&
          (screen->info.border_color_feats.customBorderColorWithoutFormat || state->border_color_format)) {
         if (!screen->info.have_EXT_border_color_swizzle) {
            static bool warned = false;
            warn_missing_feature(warned, "VK_EXT_border_color_swizzle");
         }

         if (!is_integer && !screen->have_D24_UNORM_S8_UINT) {
            union pipe_color_union clamped_border_color;
            for (unsigned i = 0; i < 4; ++i) {
               /* Use channel 0 on purpose, so that we can use OPAQUE_WHITE
                * when the border color is 1.0. */
               clamped_border_color.f[i] = CLAMP(state->border_color.f[0], 0, 1);
            }
            if (memcmp(&state->border_color, &clamped_border_color, sizeof(clamped_border_color)) != 0) {
               need_clamped_border_color = true;
               cbci_clamped.sType = VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT;
               cbci_clamped.format = VK_FORMAT_UNDEFINED;
               /* these are identical unions */
               memcpy(&cbci_clamped.customBorderColor, &clamped_border_color, sizeof(union pipe_color_union));
            }
         }
         cbci.sType = VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT;
         if (screen->info.border_color_feats.customBorderColorWithoutFormat) {
            cbci.format = VK_FORMAT_UNDEFINED;
            /* these are identical unions */
            memcpy(&cbci.customBorderColor, &state->border_color, sizeof(union pipe_color_union));
         } else {
            if (util_format_is_depth_or_stencil(state->border_color_format)) {
               if (is_integer) {
                  cbci.format = VK_FORMAT_S8_UINT;
                  for (unsigned i = 0; i < 4; i++)
                     cbci.customBorderColor.uint32[i] = CLAMP(state->border_color.ui[i], 0, 255);
               } else {
                  cbci.format = zink_get_format(screen, util_format_get_depth_only(state->border_color_format));
                  /* these are identical unions */
                  memcpy(&cbci.customBorderColor, &state->border_color, sizeof(union pipe_color_union));
               }
            } else {
               cbci.format = zink_get_format(screen, state->border_color_format);
               union pipe_color_union color;
               for (unsigned i = 0; i < 4; i++) {
                  zink_format_clamp_channel_srgb(util_format_description(state->border_color_format), &color, &state->border_color, i);
               }
               zink_convert_color(screen, state->border_color_format, (void*)&cbci.customBorderColor, &color);
            }
         }
         cbci.pNext = sci.pNext;
         sci.pNext = &cbci;
         UNUSED uint32_t check = p_atomic_inc_return(&screen->cur_custom_border_color_samplers);
         assert(check <= screen->info.border_color_props.maxCustomBorderColorSamplers);
      } else
         sci.borderColor = VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK; // TODO with custom shader if we're super interested?
   }

   if (state->max_anisotropy > 1) {
      sci.maxAnisotropy = state->max_anisotropy;
      sci.anisotropyEnable = VK_TRUE;
   }

   struct zink_sampler_state *sampler = CALLOC_STRUCT(zink_sampler_state);
   if (!sampler)
      return NULL;

   VkResult result = VKSCR(CreateSampler)(screen->dev, &sci, NULL, &sampler->sampler);
   if (result != VK_SUCCESS) {
      mesa_loge("ZINK: vkCreateSampler failed (%s)", vk_Result_to_str(result));
      FREE(sampler);
      return NULL;
   }
   if (need_clamped_border_color) {
      sci.pNext = &cbci_clamped;
      result = VKSCR(CreateSampler)(screen->dev, &sci, NULL, &sampler->sampler_clamped);
      if (result != VK_SUCCESS) {
         mesa_loge("ZINK: vkCreateSampler failed (%s)", vk_Result_to_str(result));
         VKSCR(DestroySampler)(screen->dev, sampler->sampler, NULL);
         FREE(sampler);
         return NULL;
      }
   }
   sampler->custom_border_color = need_custom;
   if (!screen->info.have_EXT_non_seamless_cube_map)
      sampler->emulate_nonseamless = !state->seamless_cube_map;

   return sampler;
}

ALWAYS_INLINE static VkImageLayout
get_layout_for_binding(const struct zink_context *ctx, struct zink_resource *res, enum zink_descriptor_type type, bool is_compute)
{
   if (res->obj->is_buffer)
      return 0;
   if (zink_screen(ctx->base.screen)->driver_workarounds.general_layout)
      return VK_IMAGE_LAYOUT_GENERAL;
   switch (type) {
   case ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW:
      return zink_descriptor_util_image_layout_eval(ctx, res, is_compute);
   case ZINK_DESCRIPTOR_TYPE_IMAGE:
      return VK_IMAGE_LAYOUT_GENERAL;
   default:
      break;
   }
   return 0;
}

ALWAYS_INLINE static struct zink_surface *
get_imageview_for_binding(struct zink_context *ctx, gl_shader_stage stage, enum zink_descriptor_type type, unsigned idx, enum pipe_format *format)
{
   switch (type) {
   case ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW: {
      struct zink_sampler_view *sampler_view = zink_sampler_view(ctx->sampler_views[stage][idx]);
      if (!sampler_view || !sampler_view->base.texture)
         return NULL;
      *format = sampler_view->base.format;
      /* if this is a non-seamless cube sampler, return the cube array view */
      if (ctx->di.emulate_nonseamless[stage] & ctx->di.cubes[stage] & BITFIELD_BIT(idx))
         return sampler_view->cube_array;
      bool needs_zs_shader_swizzle = (ctx->di.zs_swizzle[stage].mask & BITFIELD_BIT(idx)) &&
                                     zink_screen(ctx->base.screen)->driver_compiler_workarounds.needs_zs_shader_swizzle;
      bool needs_shadow_shader_swizzle = (stage == MESA_SHADER_FRAGMENT) && ctx->gfx_stages[MESA_SHADER_FRAGMENT] &&
                                         (ctx->di.zs_swizzle[MESA_SHADER_FRAGMENT].mask & ctx->gfx_stages[MESA_SHADER_FRAGMENT]->fs.legacy_shadow_mask & BITFIELD_BIT(idx));
      if (sampler_view->zs_view && (needs_zs_shader_swizzle || needs_shadow_shader_swizzle))
         return sampler_view->zs_view;
      return sampler_view->image_view;
   }
   case ZINK_DESCRIPTOR_TYPE_IMAGE: {
      struct zink_image_view *image_view = &ctx->image_views[stage][idx];
      *format = image_view->base.format;
      return image_view->base.resource ? image_view->surface : NULL;
   }
   default:
      break;
   }
   UNREACHABLE("ACK");
   return VK_NULL_HANDLE;
}

ALWAYS_INLINE static struct zink_buffer_view *
get_bufferview_for_binding(struct zink_context *ctx, gl_shader_stage stage, enum zink_descriptor_type type, unsigned idx)
{
   switch (type) {
   case ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW: {
      struct zink_sampler_view *sampler_view = zink_sampler_view(ctx->sampler_views[stage][idx]);
      return sampler_view->base.texture ? sampler_view->buffer_view : NULL;
   }
   case ZINK_DESCRIPTOR_TYPE_IMAGE: {
      struct zink_image_view *image_view = &ctx->image_views[stage][idx];
      return image_view->base.resource ? image_view->buffer_view : NULL;
   }
   default:
      break;
   }
   UNREACHABLE("ACK");
   return VK_NULL_HANDLE;
}

ALWAYS_INLINE static struct zink_resource *
update_descriptor_state_ubo_db(struct zink_context *ctx, gl_shader_stage shader, unsigned slot, struct zink_resource *res)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_UBO][shader][slot] = res;
   if (res) {
      ctx->di.db.ubos[shader][slot].address = res->obj->bda + ctx->ubos[shader][slot].buffer_offset;
      ctx->di.db.ubos[shader][slot].range = MIN2(ctx->ubos[shader][slot].buffer_size, screen->info.props.limits.maxUniformBufferRange);
#ifndef NDEBUG
      static bool warned = false;
      if (!warned && ctx->ubos[shader][slot].buffer_size > screen->info.props.limits.maxUniformBufferRange) {
         mesa_loge("ZINK: app is binding too-big UBO! Clamping!");
         warned = true;
      }
#endif
   } else {
      ctx->di.db.ubos[shader][slot].address = 0;
      ctx->di.db.ubos[shader][slot].range = VK_WHOLE_SIZE;
   }
   return res;
}

ALWAYS_INLINE static struct zink_resource *
update_descriptor_state_ubo_lazy(struct zink_context *ctx, gl_shader_stage shader, unsigned slot, struct zink_resource *res)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   ctx->di.t.ubos[shader][slot].offset = ctx->ubos[shader][slot].buffer_offset;
   ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_UBO][shader][slot] = res;
   if (res) {
      ctx->di.t.ubos[shader][slot].buffer = res->obj->buffer;
      ctx->di.t.ubos[shader][slot].range = MIN2(ctx->ubos[shader][slot].buffer_size, screen->info.props.limits.maxUniformBufferRange);
#ifndef NDEBUG
      static bool warned = false;
      if (!warned && ctx->ubos[shader][slot].buffer_size > screen->info.props.limits.maxUniformBufferRange) {
         mesa_loge("ZINK: app is binding too-big UBO! Clamping!");
         warned = true;
      }
#endif
   } else {
      ctx->di.t.ubos[shader][slot].buffer = VK_NULL_HANDLE;
      ctx->di.t.ubos[shader][slot].range = VK_WHOLE_SIZE;
   }
   return res;
}

ALWAYS_INLINE static struct zink_resource *
update_descriptor_state_ssbo_db(struct zink_context *ctx, gl_shader_stage shader, unsigned slot, struct zink_resource *res)
{
   ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_SSBO][shader][slot] = res;
   if (res) {
      ctx->di.db.ssbos[shader][slot].address = res->obj->bda + ctx->ssbos[shader][slot].buffer_offset;
      ctx->di.db.ssbos[shader][slot].range = ctx->ssbos[shader][slot].buffer_size;
   } else {
      ctx->di.db.ssbos[shader][slot].address = 0;
      ctx->di.db.ssbos[shader][slot].range = VK_WHOLE_SIZE;
   }
   return res;
}

ALWAYS_INLINE static struct zink_resource *
update_descriptor_state_ssbo_lazy(struct zink_context *ctx, gl_shader_stage shader, unsigned slot, struct zink_resource *res)
{
   ctx->di.t.ssbos[shader][slot].offset = ctx->ssbos[shader][slot].buffer_offset;
   ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_SSBO][shader][slot] = res;
   if (res) {
      ctx->di.t.ssbos[shader][slot].buffer = res->obj->buffer;
      ctx->di.t.ssbos[shader][slot].range = ctx->ssbos[shader][slot].buffer_size;
   } else {
      ctx->di.t.ssbos[shader][slot].buffer = VK_NULL_HANDLE;
      ctx->di.t.ssbos[shader][slot].range = VK_WHOLE_SIZE;
   }
   return res;
}

ALWAYS_INLINE static bool
sampler_surface_needs_clamped(enum pipe_format format)
{
   return (format == PIPE_FORMAT_Z24X8_UNORM && util_format_get_depth_only(format) == PIPE_FORMAT_Z32_FLOAT) ||
          (format == PIPE_FORMAT_Z24_UNORM_S8_UINT && util_format_get_depth_only(format) == PIPE_FORMAT_Z32_FLOAT);
}

ALWAYS_INLINE static struct zink_resource *
update_descriptor_state_sampler(struct zink_context *ctx, gl_shader_stage shader, unsigned slot, struct zink_resource *res)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   const enum zink_descriptor_type type = ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW;
   ctx->di.descriptor_res[type][shader][slot] = res;
   if (res) {
      if (res->obj->is_buffer) {
         if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
            ctx->di.db.tbos[shader][slot].address = res->obj->bda + ctx->sampler_views[shader][slot]->u.buf.offset;
            ctx->di.db.tbos[shader][slot].range = zink_sampler_view(ctx->sampler_views[shader][slot])->tbo_size;
            ctx->di.db.tbos[shader][slot].format = zink_get_format(screen, ctx->sampler_views[shader][slot]->format);
         } else {
            struct zink_buffer_view *bv = get_bufferview_for_binding(ctx, shader, type, slot);
            ctx->di.t.tbos[shader][slot] = bv->buffer_view;
         }
      } else {
         enum pipe_format format;
         struct zink_surface *surface = get_imageview_for_binding(ctx, shader, type, slot, &format);
         ctx->di.textures[shader][slot].imageLayout = ctx->blitting ? res->layout : get_layout_for_binding(ctx, res, type, shader == MESA_SHADER_COMPUTE);
         ctx->di.textures[shader][slot].imageView = surface->image_view;
         if (!screen->have_D24_UNORM_S8_UINT &&
             ctx->sampler_states[shader][slot] && ctx->sampler_states[shader][slot]->sampler_clamped) {
            struct zink_sampler_state *state = ctx->sampler_states[shader][slot];
            VkSampler sampler = sampler_surface_needs_clamped(format) ?
                                state->sampler_clamped :
                                state->sampler;
            if (ctx->di.textures[shader][slot].sampler != sampler) {
               ctx->invalidate_descriptor_state(ctx, shader, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, slot, 1);
               ctx->di.textures[shader][slot].sampler = sampler;
            }
         }
      }
   } else {
      ctx->di.textures[shader][slot].imageView = VK_NULL_HANDLE;
      ctx->di.textures[shader][slot].imageLayout = VK_IMAGE_LAYOUT_UNDEFINED;
      if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
         ctx->di.db.tbos[shader][slot].address = 0;
         ctx->di.db.tbos[shader][slot].range = VK_WHOLE_SIZE;
      } else {
         ctx->di.t.tbos[shader][slot] = VK_NULL_HANDLE;
      }
   }
   return res;
}

void
zink_update_shadow_samplerviews(struct zink_context *ctx, unsigned mask)
{
   u_foreach_bit(slot, mask)
      update_descriptor_state_sampler(ctx, MESA_SHADER_FRAGMENT, slot, ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW][MESA_SHADER_FRAGMENT][slot]);
}

ALWAYS_INLINE static struct zink_resource *
update_descriptor_state_image(struct zink_context *ctx, gl_shader_stage shader, unsigned slot, struct zink_resource *res)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   const enum zink_descriptor_type type = ZINK_DESCRIPTOR_TYPE_IMAGE;
   ctx->di.descriptor_res[type][shader][slot] = res;
   if (res) {
      if (res->obj->is_buffer) {
         if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
            ctx->di.db.texel_images[shader][slot].address = res->obj->bda + ctx->image_views[shader][slot].base.u.buf.offset;
            ctx->di.db.texel_images[shader][slot].range = ctx->image_views[shader][slot].base.u.buf.size;
            ctx->di.db.texel_images[shader][slot].format = zink_get_format(screen, ctx->image_views[shader][slot].base.format);
         } else {
            struct zink_buffer_view *bv = get_bufferview_for_binding(ctx, shader, type, slot);
            ctx->di.t.texel_images[shader][slot] = bv->buffer_view;
         }
      } else {
         enum pipe_format format;
         struct zink_surface *surface = get_imageview_for_binding(ctx, shader, type, slot, &format);
         ctx->di.images[shader][slot].imageLayout = VK_IMAGE_LAYOUT_GENERAL;
         ctx->di.images[shader][slot].imageView = surface->image_view;
      }
   } else {
      memset(&ctx->di.images[shader][slot], 0, sizeof(ctx->di.images[shader][slot]));
      if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
         ctx->di.db.texel_images[shader][slot].address = 0;
         ctx->di.db.texel_images[shader][slot].range = VK_WHOLE_SIZE;
      } else {
         ctx->di.t.texel_images[shader][slot] = VK_NULL_HANDLE;
      }
   }
   return res;
}

static void
update_nonseamless_shader_key(struct zink_context *ctx, gl_shader_stage pstage)
{
   const uint32_t new_mask = ctx->di.emulate_nonseamless[pstage] & ctx->di.cubes[pstage];
   if (pstage == MESA_SHADER_COMPUTE) {
      if (ctx->compute_pipeline_state.key.base.nonseamless_cube_mask != new_mask)
         ctx->compute_dirty = true;
      ctx->compute_pipeline_state.key.base.nonseamless_cube_mask = new_mask;
   } else {
      if (zink_get_shader_key_base(ctx, pstage)->nonseamless_cube_mask != new_mask)
         zink_set_shader_key_base(ctx, pstage)->nonseamless_cube_mask = new_mask;
   }
}

static void
zink_bind_sampler_states(struct pipe_context *pctx,
                         gl_shader_stage shader,
                         unsigned start_slot,
                         unsigned num_samplers,
                         void **samplers)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_screen *screen = zink_screen(pctx->screen);
   for (unsigned i = 0; i < num_samplers; ++i) {
      struct zink_sampler_state *state = samplers[i];
      if (samplers[i] == ctx->sampler_states[shader][start_slot + i])
         continue;
      ctx->invalidate_descriptor_state(ctx, shader, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, start_slot, 1);
      ctx->sampler_states[shader][start_slot + i] = state;
      if (state) {
         ctx->di.textures[shader][start_slot + i].sampler = state->sampler;
         if (state->sampler_clamped && !screen->have_D24_UNORM_S8_UINT) {
            enum pipe_format format;
            struct zink_surface *surface = get_imageview_for_binding(ctx, shader, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, start_slot + i, &format);
            if (surface && sampler_surface_needs_clamped(format))
               ctx->di.textures[shader][start_slot + i].sampler = state->sampler_clamped;
         }
      } else {
         ctx->di.textures[shader][start_slot + i].sampler = VK_NULL_HANDLE;
      }
   }
   ctx->di.num_samplers[shader] = start_slot + num_samplers;
}

static void
zink_bind_sampler_states_nonseamless(struct pipe_context *pctx,
                                     gl_shader_stage shader,
                                     unsigned start_slot,
                                     unsigned num_samplers,
                                     void **samplers)
{
   struct zink_context *ctx = zink_context(pctx);
   uint32_t old_mask = ctx->di.emulate_nonseamless[shader];
   uint32_t mask = BITFIELD_RANGE(start_slot, num_samplers);
   ctx->di.emulate_nonseamless[shader] &= ~mask;
   for (unsigned i = 0; i < num_samplers; ++i) {
      struct zink_sampler_state *state = samplers[i];
      const uint32_t bit = BITFIELD_BIT(start_slot + i);
      if (!state)
         continue;
      if (state->emulate_nonseamless)
         ctx->di.emulate_nonseamless[shader] |= bit;
      if (state->emulate_nonseamless != (old_mask & bit) && (ctx->di.cubes[shader] & bit)) {
         enum pipe_format format;
         struct zink_surface *surface = get_imageview_for_binding(ctx, shader, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, start_slot + i, &format);
         if (surface && ctx->di.images[shader][start_slot + i].imageView != surface->image_view) {
            ctx->di.images[shader][start_slot + i].imageView = surface->image_view;
            update_descriptor_state_sampler(ctx, shader, start_slot + i, ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW][shader][start_slot + i]);
            ctx->invalidate_descriptor_state(ctx, shader, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, start_slot + i, 1);
         }
      }
   }
   zink_bind_sampler_states(pctx, shader, start_slot, num_samplers, samplers);
   update_nonseamless_shader_key(ctx, shader);
}

static void
zink_delete_sampler_state(struct pipe_context *pctx,
                          void *sampler_state)
{
   struct zink_sampler_state *sampler = sampler_state;
   struct zink_batch_state *bs = zink_context(pctx)->bs;
   /* may be called if context_create fails */
   if (bs) {
      util_dynarray_append(&bs->zombie_samplers, VkSampler,
                           sampler->sampler);
      if (sampler->sampler_clamped)
         util_dynarray_append(&bs->zombie_samplers, VkSampler,
                              sampler->sampler_clamped);
   }
   if (sampler->custom_border_color)
      p_atomic_dec(&zink_screen(pctx->screen)->cur_custom_border_color_samplers);
   FREE(sampler);
}

static VkImageAspectFlags
sampler_aspect_from_format(enum pipe_format fmt)
{
   if (util_format_is_depth_or_stencil(fmt)) {
      const struct util_format_description *desc = util_format_description(fmt);
      if (util_format_has_depth(desc))
         return VK_IMAGE_ASPECT_DEPTH_BIT;
      assert(util_format_has_stencil(desc));
      return VK_IMAGE_ASPECT_STENCIL_BIT;
   } else
     return VK_IMAGE_ASPECT_COLOR_BIT;
}

static uint32_t
hash_bufferview_key(void *key)
{
   return _mesa_hash_data(key, sizeof(struct zink_bufferview_key));
}

static VkBufferViewCreateInfo
create_bvci(struct zink_context *ctx, struct zink_resource *res, enum pipe_format format, uint32_t offset, uint32_t range)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   VkBufferViewCreateInfo bvci;
   // Zero whole struct (including alignment holes), so hash_bufferview
   // does not access potentially uninitialized data.
   memset(&bvci, 0, sizeof(bvci));
   bvci.sType = VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO;
   bvci.pNext = NULL;
   if (zink_get_format_props(screen, format)->bufferFeatures & VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT)
      bvci.buffer = res->obj->storage_buffer ? res->obj->storage_buffer : res->obj->buffer;
   else
      bvci.buffer = res->obj->buffer;
   bvci.format = zink_get_format(screen, format);
   assert(bvci.format);
   bvci.offset = offset;
   bvci.range = !offset && range == res->base.b.width0 ? VK_WHOLE_SIZE : range;
   unsigned blocksize = util_format_get_blocksize(format);
   if (bvci.range != VK_WHOLE_SIZE) {
      /* clamp out partial texels */
      bvci.range -= bvci.range % blocksize;
      if (bvci.offset + bvci.range >= res->base.b.width0)
         bvci.range = VK_WHOLE_SIZE;
   }
   uint64_t clamp = (uint64_t)blocksize * (uint64_t)screen->info.props.limits.maxTexelBufferElements;
   if (bvci.range == VK_WHOLE_SIZE && res->base.b.width0 > clamp)
      bvci.range = clamp;
   bvci.flags = 0;
   return bvci;
}

static struct zink_buffer_view *
get_buffer_view(struct zink_context *ctx, struct zink_resource *res, enum pipe_format format, unsigned offset, unsigned size)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   struct zink_buffer_view *buffer_view = NULL;
   struct zink_bufferview_key key = {
      format,
      offset,
      size
   };
   VkBufferViewCreateInfo bvci = create_bvci(ctx, res, format, offset, size);

   uint32_t hash = hash_bufferview_key(&key);
   simple_mtx_lock(&res->obj->surface_mtx);
   bool found = false;
   struct set_entry *he = _mesa_set_search_or_add_pre_hashed(&res->obj->surface_cache, hash, &key, &found);
   if (found) {
      buffer_view = (void*)he->key;
   } else {
      VkBufferView view;
      VkResult result = VKSCR(CreateBufferView)(screen->dev, &bvci, NULL, &view);
      if (result != VK_SUCCESS) {
         _mesa_set_remove(&res->obj->surface_cache, he);
         mesa_loge("ZINK: vkCreateBufferView failed (%s)", vk_Result_to_str(result));
         goto out;
      }
      buffer_view = CALLOC_STRUCT(zink_buffer_view);
      if (!buffer_view) {
         _mesa_set_remove(&res->obj->surface_cache, he);
         VKSCR(DestroyBufferView)(screen->dev, view, NULL);
         goto out;
      }
      buffer_view->pres = &res->base.b;
      buffer_view->key = key;
      buffer_view->buffer_view = view;
      he->key = buffer_view;
   }
out:
   simple_mtx_unlock(&res->obj->surface_mtx);
   return buffer_view;
}

enum pipe_swizzle
zink_clamp_void_swizzle(const struct util_format_description *desc, enum pipe_swizzle swizzle)
{
   switch (swizzle) {
   case PIPE_SWIZZLE_X:
   case PIPE_SWIZZLE_Y:
   case PIPE_SWIZZLE_Z:
   case PIPE_SWIZZLE_W:
      return desc->channel[swizzle].type == UTIL_FORMAT_TYPE_VOID ? PIPE_SWIZZLE_1 : swizzle;
   default:
      break;
   }
   return swizzle;
}

ALWAYS_INLINE static enum pipe_swizzle
clamp_zs_swizzle(enum pipe_swizzle swizzle)
{
   switch (swizzle) {
   case PIPE_SWIZZLE_X:
   case PIPE_SWIZZLE_Y:
   case PIPE_SWIZZLE_Z:
   case PIPE_SWIZZLE_W:
      return PIPE_SWIZZLE_X;
   default:
      break;
   }
   return swizzle;
}

ALWAYS_INLINE static enum pipe_swizzle
clamp_alpha_swizzle(enum pipe_swizzle swizzle)
{
   if (swizzle == PIPE_SWIZZLE_W)
      return PIPE_SWIZZLE_X;
   if (swizzle < PIPE_SWIZZLE_W)
      return PIPE_SWIZZLE_0;
   return swizzle;
}

ALWAYS_INLINE static enum pipe_swizzle
clamp_luminance_swizzle(enum pipe_swizzle swizzle)
{
   if (swizzle == PIPE_SWIZZLE_W)
      return PIPE_SWIZZLE_1;
   if (swizzle < PIPE_SWIZZLE_W)
      return PIPE_SWIZZLE_X;
   return swizzle;
}

ALWAYS_INLINE static enum pipe_swizzle
clamp_luminance_alpha_swizzle(enum pipe_swizzle swizzle)
{
   if (swizzle == PIPE_SWIZZLE_W)
      return PIPE_SWIZZLE_Y;
   if (swizzle < PIPE_SWIZZLE_W)
      return PIPE_SWIZZLE_X;
   return swizzle;
}

ALWAYS_INLINE static bool
viewtype_is_cube(const VkImageViewCreateInfo *ivci)
{
   return ivci->viewType == VK_IMAGE_VIEW_TYPE_CUBE ||
          ivci->viewType == VK_IMAGE_VIEW_TYPE_CUBE_ARRAY;
}

static struct pipe_resource *
rebind_buffer_as_image(struct pipe_context *pctx, struct pipe_resource *pres, enum pipe_format pformat, const struct pipe_tex2d_from_buf *tex2d, bool storage)
{
   struct winsys_handle whandle = {.type = WINSYS_HANDLE_TYPE_FD,};
   unsigned bind = storage ? PIPE_BIND_SHADER_IMAGE : PIPE_BIND_SAMPLER_VIEW;
   if (zink_resource_usage_is_unflushed(zink_resource(pres)))
      pctx->flush(pctx, NULL, 0);
   if (!pctx->screen->resource_get_handle(pctx->screen, pctx, pres, &whandle, 0))
      abort();
   struct pipe_resource tmpl = {
      .format = pformat,
      .target = PIPE_TEXTURE_2D,
      .bind = PIPE_BIND_LINEAR | bind,
      .width0 = tex2d->width,
      .height0 = tex2d->height,
      .depth0 = 1,
      .array_size = 1,
   };
   whandle.offset = tex2d->offset;
   whandle.stride = util_format_get_stride(pformat, tex2d->row_stride);
   whandle.modifier = 0;

   struct pipe_resource *import = pctx->screen->resource_from_handle(pctx->screen, &tmpl, &whandle, 0);
   if (import)
      /* this isn't actually used cross-process, so don't emit extra sync */
      zink_resource(import)->obj->exportable = false;
#if !defined(_WIN32)
   close(whandle.handle);
#endif
   return import;
}

static struct pipe_surface
pipe_surface_templ_from_sampler_view(const struct pipe_sampler_view *state, struct pipe_resource *pres, enum pipe_texture_target target)
{
   struct pipe_surface templ = {0};
   if (!state->is_tex2d_from_buf)
      templ.level = state->u.tex.first_level;
   templ.format = state->format;
   /* avoid needing mutable for depth/stencil sampling */
   if (util_format_is_depth_and_stencil(pres->format))
      templ.format = pres->format;
   if (target != PIPE_TEXTURE_3D && !state->is_tex2d_from_buf) {
      templ.first_layer = state->u.tex.first_layer;
      templ.last_layer = state->u.tex.last_layer;
   }
   templ.texture = pres;
   return templ;
}

static struct pipe_sampler_view *
zink_create_sampler_view(struct pipe_context *pctx, struct pipe_resource *pres,
                         const struct pipe_sampler_view *state)
{
   struct zink_screen *screen = zink_screen(pctx->screen);
   struct zink_resource *res = zink_resource(pres);
   struct zink_context *ctx = zink_context(pctx);
   struct zink_sampler_view *sampler_view = CALLOC_STRUCT_CL(zink_sampler_view);
   enum pipe_texture_target target = state->is_tex2d_from_buf ? PIPE_TEXTURE_2D : state->target;
   bool err;

   if (!sampler_view) {
      mesa_loge("ZINK: failed to allocate sampler_view!");
      return NULL;
   }
      
   sampler_view->base = *state;
   sampler_view->base.texture = NULL;
   pipe_resource_reference(&sampler_view->base.texture, pres);
   sampler_view->rebind_count = res->rebind_count;
   sampler_view->base.reference.count = 1;
   sampler_view->base.context = pctx;

   if (target != PIPE_BUFFER) {
      VkImageViewCreateInfo ivci;

      if (state->is_tex2d_from_buf) {
         struct pipe_resource *import = rebind_buffer_as_image(pctx, pres, state->format, &state->u.tex2d_from_buf, false);
         assert(import);
         sampler_view->import2d = zink_resource(import);
         res = sampler_view->import2d;
         pres = import;
      }
      struct pipe_surface templ = pipe_surface_templ_from_sampler_view(state, pres, target);

      if (zink_is_swapchain(res)) {
         if (!zink_kopper_acquire(ctx, res, UINT64_MAX)) {
            FREE_CL(sampler_view);
            return NULL;
         }
      }

      ivci = create_ivci(screen, res, &templ, target);
      ivci.subresourceRange.levelCount = state->is_tex2d_from_buf ? 1 : (state->u.tex.last_level - state->u.tex.first_level + 1);
      ivci.subresourceRange.aspectMask = util_format_is_depth_or_stencil(state->format) ? sampler_aspect_from_format(state->format) : res->aspect;
      bool red_depth_sampler_view = false;
      /* samplers for stencil aspects of packed formats need to always use stencil swizzle */
      if (ivci.subresourceRange.aspectMask & (VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT)) {
         ivci.components.r = zink_component_mapping(clamp_zs_swizzle(sampler_view->base.swizzle_r));
         ivci.components.g = zink_component_mapping(clamp_zs_swizzle(sampler_view->base.swizzle_g));
         ivci.components.b = zink_component_mapping(clamp_zs_swizzle(sampler_view->base.swizzle_b));
         ivci.components.a = zink_component_mapping(clamp_zs_swizzle(sampler_view->base.swizzle_a));

         /* If we're sampling depth and we might need to do shader rewrites for
          * legacy shadow sampling, then set up an extra image view that just
          * returns the red (depth) component, so you can always have the shadow
          * result available in the red component for the in-shader swizzling.
          * (Or if we have PVR's needs_zs_shader_swizzle and are sampling ONE
          * value for stencil, which also uses that view).
          */
         if (ivci.subresourceRange.aspectMask == VK_IMAGE_ASPECT_DEPTH_BIT ||
             zink_screen(ctx->base.screen)->driver_compiler_workarounds.needs_zs_shader_swizzle) {
            VkComponentSwizzle *swizzle = (VkComponentSwizzle*)&ivci.components;
            for (unsigned i = 0; i < 4; i++) {
               if (swizzle[i] == VK_COMPONENT_SWIZZLE_ONE ||
                   (swizzle[i] == VK_COMPONENT_SWIZZLE_ZERO && ivci.subresourceRange.aspectMask == VK_IMAGE_ASPECT_DEPTH_BIT))
                  red_depth_sampler_view = true;
            }
            /* this is the data that will be used in shader rewrites */
            sampler_view->swizzle.s[0] = clamp_zs_swizzle(sampler_view->base.swizzle_r);
            sampler_view->swizzle.s[1] = clamp_zs_swizzle(sampler_view->base.swizzle_g);
            sampler_view->swizzle.s[2] = clamp_zs_swizzle(sampler_view->base.swizzle_b);
            sampler_view->swizzle.s[3] = clamp_zs_swizzle(sampler_view->base.swizzle_a);
         }
      } else {
         enum pipe_swizzle swizzle[4] = {
            sampler_view->base.swizzle_r,
            sampler_view->base.swizzle_g,
            sampler_view->base.swizzle_b,
            sampler_view->base.swizzle_a
         };
         /* if we have e.g., R8G8B8X8, then we have to ignore alpha since we're just emulating
          * these formats
          */
         if (zink_format_is_voidable_rgba_variant(state->format)) {
            const struct util_format_description *view_desc = util_format_description(state->format);
            for (int i = 0; i < 4; ++i)
               swizzle[i] = zink_clamp_void_swizzle(view_desc, swizzle[i]);
         } else if (util_format_is_alpha(state->format) && res->format != VK_FORMAT_A8_UNORM_KHR) {
            for (int i = 0; i < 4; ++i)
               swizzle[i] = clamp_alpha_swizzle(swizzle[i]);
         } else if (util_format_is_luminance(pres->format) ||
                    util_format_is_luminance_alpha(pres->format)) {
            if (util_format_is_luminance(pres->format)) {
               for (int i = 0; i < 4; ++i)
                  swizzle[i] = clamp_luminance_swizzle(swizzle[i]);
            } else {
               for (int i = 0; i < 4; ++i)
                  swizzle[i] = clamp_luminance_alpha_swizzle(swizzle[i]);
            }
            if (state->format != pres->format) {
               /* luminance / luminance-alpha formats can be reinterpreted
                * as red / red-alpha formats by the state-tracker, and we
                * need to whack the green/blue channels here to the
                * correct values for that to work.
                */
               enum pipe_format linear = util_format_linear(pres->format);
               if (state->format == util_format_luminance_to_red(linear)) {
                  assert(swizzle[1] == PIPE_SWIZZLE_X ||
                         swizzle[1] == PIPE_SWIZZLE_0);
                  assert(swizzle[2] == PIPE_SWIZZLE_X ||
                         swizzle[2] == PIPE_SWIZZLE_0);
                  swizzle[1] = swizzle[2] = PIPE_SWIZZLE_0;
               } else
                  assert(state->format == linear);
            }
         } else if (util_format_is_red_alpha(pres->format)) {
            /* RA formats are mapped to RG with adjusted swizzle */
            assert(util_format_is_red_green(vk_format_to_pipe_format(ivci.format)));
            swizzle[3] = PIPE_SWIZZLE_Y;
         }

         ivci.components.r = zink_component_mapping(swizzle[0]);
         ivci.components.g = zink_component_mapping(swizzle[1]);
         ivci.components.b = zink_component_mapping(swizzle[2]);
         ivci.components.a = zink_component_mapping(swizzle[3]);
      }
      assert(ivci.format);

      sampler_view->ivci = ivci;
      sampler_view->image_view = zink_get_surface(ctx, &templ, &ivci);
      if (!screen->info.have_EXT_non_seamless_cube_map && viewtype_is_cube(&ivci)) {
         ivci.viewType = VK_IMAGE_VIEW_TYPE_2D_ARRAY;
         sampler_view->cube_array = zink_get_surface(ctx, &templ, &ivci);
      } else if (red_depth_sampler_view) {
         /* there is only one component, and real swizzling can't be done here,
          * so ensure the shader gets the sampled data
          */
         ivci.components.r = VK_COMPONENT_SWIZZLE_R;
         ivci.components.g = VK_COMPONENT_SWIZZLE_R;
         ivci.components.b = VK_COMPONENT_SWIZZLE_R;
         ivci.components.a = VK_COMPONENT_SWIZZLE_R;
         sampler_view->zs_view = zink_get_surface(ctx, &templ, &ivci);
      }
      err = !sampler_view->image_view;
   } else {
      if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
         /* always enforce limit clamping */
         unsigned blocksize = util_format_get_blocksize(state->format);
         sampler_view->tbo_size = MIN2(state->u.buf.size / blocksize, screen->info.props.limits.maxTexelBufferElements) * blocksize;
         return &sampler_view->base;
      }
      sampler_view->rebind_count = res->rebind_count;
      sampler_view->buffer_view = get_buffer_view(ctx, res, state->format, state->u.buf.offset, state->u.buf.size);
      err = !sampler_view->buffer_view;
   }
   if (err) {
      FREE_CL(sampler_view);
      return NULL;
   }
   return &sampler_view->base;
}

static void
zink_sampler_view_destroy(struct pipe_context *pctx,
                          struct pipe_sampler_view *pview)
{
   struct zink_sampler_view *view = zink_sampler_view(pview);
   zink_resource_reference(&view->import2d, NULL);
   pipe_resource_reference(&pview->texture, NULL);
   FREE_CL(view);
}

static void
zink_get_sample_position(struct pipe_context *ctx,
                         unsigned sample_count,
                         unsigned sample_index,
                         float *out_value)
{
   /* TODO: handle this I guess */
   assert(zink_screen(ctx->screen)->info.props.limits.standardSampleLocations);
   u_default_get_sample_position(ctx, sample_count, sample_index, out_value);
}

static void
zink_set_polygon_stipple(struct pipe_context *pctx,
                         const struct pipe_poly_stipple *ps)
{
}

ALWAYS_INLINE static void
update_res_bind_count(struct zink_context *ctx, struct zink_resource *res, bool is_compute, bool decrement)
{
   if (decrement) {
      assert(res->bind_count[is_compute]);
      if (!--res->bind_count[is_compute])
         _mesa_set_remove_key(ctx->need_barriers[is_compute], res);
      check_resource_for_batch_ref(ctx, res);
   } else
      res->bind_count[is_compute]++;
}

ALWAYS_INLINE static void
update_existing_vbo(struct zink_context *ctx, unsigned slot)
{
   if (!ctx->vertex_buffers[slot].buffer.resource)
      return;
   struct zink_resource *res = zink_resource(ctx->vertex_buffers[slot].buffer.resource);
   res->vbo_bind_count--;
   res->vbo_bind_mask &= ~BITFIELD_BIT(slot);
   if (!res->vbo_bind_count) {
      res->gfx_barrier &= ~VK_PIPELINE_STAGE_VERTEX_INPUT_BIT;
      res->barrier_access[0] &= ~VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT;
   }
   update_res_bind_count(ctx, res, false, true);
}

static void
zink_set_vertex_buffers_internal(struct pipe_context *pctx,
                                 unsigned num_buffers,
                                 const struct pipe_vertex_buffer *buffers,
                                 bool optimal)
{
   struct zink_context *ctx = zink_context(pctx);
   const bool have_input_state = zink_screen(pctx->screen)->info.have_EXT_vertex_input_dynamic_state;
   const bool need_state_change = !zink_screen(pctx->screen)->info.have_EXT_extended_dynamic_state &&
                                  !have_input_state;
   unsigned last_count = util_last_bit(ctx->gfx_pipeline_state.vertex_buffers_enabled_mask);
   uint32_t enabled_buffers = BITFIELD_MASK(num_buffers);

   assert(!num_buffers || buffers);

   for (unsigned i = 0; i < num_buffers; ++i) {
      const struct pipe_vertex_buffer *vb = buffers + i;
      struct pipe_vertex_buffer *ctx_vb = &ctx->vertex_buffers[i];
      update_existing_vbo(ctx, i);
      pipe_resource_reference(&ctx_vb->buffer.resource, NULL);
      ctx_vb->buffer.resource = vb->buffer.resource;

      if (vb->buffer.resource) {
         struct zink_resource *res = zink_resource(vb->buffer.resource);
         res->vbo_bind_mask |= BITFIELD_BIT(i);
         res->vbo_bind_count++;
         res->gfx_barrier |= VK_PIPELINE_STAGE_VERTEX_INPUT_BIT;
         res->barrier_access[0] |= VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT;
         update_res_bind_count(ctx, res, false, false);
         ctx_vb->buffer_offset = vb->buffer_offset;
         /* always barrier before possible rebind */
         zink_screen(ctx->base.screen)->buffer_barrier(ctx, res, VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT,
                                      VK_PIPELINE_STAGE_VERTEX_INPUT_BIT);
         zink_batch_resource_usage_set(ctx->bs, res, false, true);
         res->obj->unordered_read = false;
      } else {
         enabled_buffers &= ~BITFIELD_BIT(i);
      }
   }
   for (unsigned i = num_buffers; i < last_count; i++) {
      update_existing_vbo(ctx, i);
      pipe_resource_reference(&ctx->vertex_buffers[i].buffer.resource, NULL);
   }
   if (!optimal) {
      if (need_state_change)
         ctx->vertex_state_changed = true;
      else if (!have_input_state && ctx->gfx_pipeline_state.vertex_buffers_enabled_mask != enabled_buffers)
         ctx->vertex_state_changed = true;
   }
   ctx->gfx_pipeline_state.vertex_buffers_enabled_mask = enabled_buffers;
   ctx->vertex_buffers_dirty = num_buffers > 0;
#ifndef NDEBUG
   u_foreach_bit(b, enabled_buffers)
      assert(ctx->vertex_buffers[b].buffer.resource);
#endif
}

static void
zink_set_vertex_buffers(struct pipe_context *pctx,
                        unsigned num_buffers,
                        const struct pipe_vertex_buffer *buffers)
{
   zink_set_vertex_buffers_internal(pctx, num_buffers, buffers, false);
}

static void
zink_set_vertex_buffers_optimal(struct pipe_context *pctx,
                                 unsigned num_buffers,
                                 const struct pipe_vertex_buffer *buffers)
{
   zink_set_vertex_buffers_internal(pctx, num_buffers, buffers, true);
}

static void
zink_set_viewport_states(struct pipe_context *pctx,
                         unsigned start_slot,
                         unsigned num_viewports,
                         const struct pipe_viewport_state *state)
{
   struct zink_context *ctx = zink_context(pctx);

   for (unsigned i = 0; i < num_viewports; ++i)
      ctx->vp_state.viewport_states[start_slot + i] = state[i];

   ctx->vp_state_changed = true;
}

static void
zink_set_scissor_states(struct pipe_context *pctx,
                        unsigned start_slot, unsigned num_scissors,
                        const struct pipe_scissor_state *states)
{
   struct zink_context *ctx = zink_context(pctx);

   for (unsigned i = 0; i < num_scissors; i++)
      ctx->vp_state.scissor_states[start_slot + i] = states[i];
   ctx->scissor_changed = true;
}

static void
zink_set_inlinable_constants(struct pipe_context *pctx,
                             gl_shader_stage shader,
                             uint num_values, uint32_t *values)
{
   struct zink_context *ctx = (struct zink_context *)pctx;
   const uint32_t bit = BITFIELD_BIT(shader);
   uint32_t *inlinable_uniforms;
   struct zink_shader_key *key = NULL;

   if (shader == MESA_SHADER_COMPUTE) {
      key = &ctx->compute_pipeline_state.key;
   } else {
      assert(!zink_screen(pctx->screen)->optimal_keys ||
             (shader == MESA_SHADER_GEOMETRY &&
              ctx->gfx_stages[MESA_SHADER_GEOMETRY] &&
              ctx->gfx_stages[MESA_SHADER_GEOMETRY]->non_fs.is_generated));
      key = &ctx->gfx_pipeline_state.shader_keys.key[shader];
   }
   inlinable_uniforms = key->base.inlined_uniform_values;
   if (!(ctx->inlinable_uniforms_valid_mask & bit) ||
       memcmp(inlinable_uniforms, values, num_values * 4)) {
      memcpy(inlinable_uniforms, values, num_values * 4);
      if (shader == MESA_SHADER_COMPUTE)
         ctx->compute_dirty = true;
      else
         ctx->dirty_gfx_stages |= bit;
      ctx->inlinable_uniforms_valid_mask |= bit;
      key->inline_uniforms = true;
   }
}

ALWAYS_INLINE static void
unbind_descriptor_stage(struct zink_resource *res, gl_shader_stage pstage)
{
   if (!res->sampler_binds[pstage] && !res->image_binds[pstage] && !res->all_bindless)
      res->gfx_barrier &= ~zink_pipeline_flags_from_pipe_stage(pstage);
}

ALWAYS_INLINE static void
unbind_buffer_descriptor_stage(struct zink_resource *res, gl_shader_stage pstage)
{
   if (!res->ubo_bind_mask[pstage] && !res->ssbo_bind_mask[pstage])
      unbind_descriptor_stage(res, pstage);
}

ALWAYS_INLINE static void
unbind_ubo(struct zink_context *ctx, struct zink_resource *res, gl_shader_stage pstage, unsigned slot)
{
   if (!res)
      return;
   res->ubo_bind_mask[pstage] &= ~BITFIELD_BIT(slot);
   res->ubo_bind_count[pstage == MESA_SHADER_COMPUTE]--;
   unbind_buffer_descriptor_stage(res, pstage);
   if (!res->ubo_bind_count[pstage == MESA_SHADER_COMPUTE])
      res->barrier_access[pstage == MESA_SHADER_COMPUTE] &= ~VK_ACCESS_UNIFORM_READ_BIT;
   update_res_bind_count(ctx, res, pstage == MESA_SHADER_COMPUTE, true);
}

static void
invalidate_inlined_uniforms(struct zink_context *ctx, gl_shader_stage pstage)
{
   unsigned bit = BITFIELD_BIT(pstage);
   if (!(ctx->inlinable_uniforms_valid_mask & bit))
      return;
   ctx->inlinable_uniforms_valid_mask &= ~bit;
   if (pstage == MESA_SHADER_COMPUTE) {
      ctx->compute_dirty = true;
      return;
   }
   assert(!zink_screen(ctx->base.screen)->optimal_keys || (pstage == MESA_SHADER_GEOMETRY && ctx->is_generated_gs_bound));
   ctx->dirty_gfx_stages |= bit;
   struct zink_shader_key *key = &ctx->gfx_pipeline_state.shader_keys.key[pstage];
   key->inline_uniforms = false;
}

ALWAYS_INLINE static void
zink_set_constant_buffer_internal(struct pipe_context *pctx,
                         gl_shader_stage shader, uint index,
                         bool take_ownership,
                         const struct pipe_constant_buffer *cb,
                         bool use_db)
{
   struct zink_context *ctx = zink_context(pctx);
   bool update = false;

   struct zink_resource *res = zink_resource(ctx->ubos[shader][index].buffer);
   if (cb) {
      struct pipe_resource *buffer = cb->buffer;
      unsigned offset = cb->buffer_offset;
      struct zink_screen *screen = zink_screen(pctx->screen);
      if (cb->user_buffer) {
         u_upload_data(ctx->base.const_uploader, 0, cb->buffer_size,
                       screen->info.props.limits.minUniformBufferOffsetAlignment,
                       cb->user_buffer, &offset, &buffer);
      }
      struct zink_resource *new_res = zink_resource(buffer);
      if (new_res) {
         if (new_res != res) {
            unbind_ubo(ctx, res, shader, index);
            new_res->ubo_bind_count[shader == MESA_SHADER_COMPUTE]++;
            new_res->ubo_bind_mask[shader] |= BITFIELD_BIT(index);
            new_res->gfx_barrier |= zink_pipeline_flags_from_pipe_stage(shader);
            new_res->barrier_access[shader == MESA_SHADER_COMPUTE] |= VK_ACCESS_UNIFORM_READ_BIT;
            update_res_bind_count(ctx, new_res, shader == MESA_SHADER_COMPUTE, false);
         }
         zink_screen(ctx->base.screen)->buffer_barrier(ctx, new_res, VK_ACCESS_UNIFORM_READ_BIT,
                                      new_res->gfx_barrier);
         zink_batch_resource_usage_set(ctx->bs, new_res, false, true);
         if (!ctx->unordered_blitting)
            new_res->obj->unordered_read = false;
      }
      update |= ctx->ubos[shader][index].buffer_offset != offset ||
                !!res != !!buffer || (res && res->obj->buffer != new_res->obj->buffer) ||
                ctx->ubos[shader][index].buffer_size != cb->buffer_size;

      if (take_ownership) {
         pipe_resource_reference(&ctx->ubos[shader][index].buffer, NULL);
         ctx->ubos[shader][index].buffer = buffer;
      } else {
         pipe_resource_reference(&ctx->ubos[shader][index].buffer, buffer);
      }
      ctx->ubos[shader][index].buffer_offset = offset;
      ctx->ubos[shader][index].buffer_size = cb->buffer_size;
      ctx->ubos[shader][index].user_buffer = NULL;

      if (cb->user_buffer)
         pipe_resource_reference(&buffer, NULL);

      if (index + 1 >= ctx->di.num_ubos[shader])
         ctx->di.num_ubos[shader] = index + 1;
      
      if (use_db) {
         update_descriptor_state_ubo_db(ctx, shader, index, new_res);
      } else {
         update_descriptor_state_ubo_lazy(ctx, shader, index, new_res);
      }
   } else {
      ctx->ubos[shader][index].buffer_offset = 0;
      ctx->ubos[shader][index].buffer_size = 0;
      ctx->ubos[shader][index].user_buffer = NULL;
      if (res) {
         unbind_ubo(ctx, res, shader, index);
         if (use_db) {
            update_descriptor_state_ubo_db(ctx, shader, index, NULL);
         } else {
            update_descriptor_state_ubo_lazy(ctx, shader, index, NULL);
         }
      }
      update = !!ctx->ubos[shader][index].buffer;

      pipe_resource_reference(&ctx->ubos[shader][index].buffer, NULL);
      if (ctx->di.num_ubos[shader] == index + 1)
         ctx->di.num_ubos[shader]--;
   }
   if (index == 0) {
      /* Invalidate current inlinable uniforms. */
      invalidate_inlined_uniforms(ctx, shader);
   }

   if (update)
      ctx->invalidate_descriptor_state(ctx, shader, ZINK_DESCRIPTOR_TYPE_UBO, index, 1);
}

static void
zink_set_constant_buffer_db(struct pipe_context *pctx,
                         gl_shader_stage shader, uint index,
                         bool take_ownership,
                         const struct pipe_constant_buffer *cb)
{
   zink_set_constant_buffer_internal(pctx, shader, index, take_ownership, cb, true);
}

static void
zink_set_constant_buffer_lazy(struct pipe_context *pctx,
                         gl_shader_stage shader, uint index,
                         bool take_ownership,
                         const struct pipe_constant_buffer *cb)
{
   zink_set_constant_buffer_internal(pctx, shader, index, take_ownership, cb, false);
}

ALWAYS_INLINE static void
unbind_descriptor_reads(struct zink_resource *res, bool is_compute)
{
   if (!res->sampler_bind_count[is_compute] && !res->image_bind_count[is_compute] && !res->all_bindless)
      res->barrier_access[is_compute] &= ~VK_ACCESS_SHADER_READ_BIT;
}

ALWAYS_INLINE static void
unbind_buffer_descriptor_reads(struct zink_resource *res, bool is_compute)
{
   if (!res->ssbo_bind_count[is_compute] && !res->all_bindless)
      unbind_descriptor_reads(res, is_compute);
}

ALWAYS_INLINE static void
unbind_ssbo(struct zink_context *ctx, struct zink_resource *res, gl_shader_stage pstage, unsigned slot, bool writable)
{
   if (!res)
      return;
   res->ssbo_bind_mask[pstage] &= ~BITFIELD_BIT(slot);
   res->ssbo_bind_count[pstage == MESA_SHADER_COMPUTE]--;
   unbind_buffer_descriptor_stage(res, pstage);
   unbind_buffer_descriptor_reads(res, pstage == MESA_SHADER_COMPUTE);
   update_res_bind_count(ctx, res, pstage == MESA_SHADER_COMPUTE, true);
   if (writable)
      res->write_bind_count[pstage == MESA_SHADER_COMPUTE]--;
   if (!res->write_bind_count[pstage == MESA_SHADER_COMPUTE])
      res->barrier_access[pstage == MESA_SHADER_COMPUTE] &= ~VK_ACCESS_SHADER_WRITE_BIT;
}

ALWAYS_INLINE static void
zink_set_shader_buffers_internal(struct pipe_context *pctx,
                        gl_shader_stage p_stage,
                        unsigned start_slot, unsigned count,
                        const struct pipe_shader_buffer *buffers,
                        unsigned writable_bitmask,
                        bool use_db)
{
   struct zink_context *ctx = zink_context(pctx);
   bool update = false;
   unsigned max_slot = 0;

   unsigned modified_bits = u_bit_consecutive(start_slot, count);
   unsigned old_writable_mask = ctx->writable_ssbos[p_stage];
   assert(!ctx->unordered_blitting);
   ctx->writable_ssbos[p_stage] &= ~modified_bits;
   ctx->writable_ssbos[p_stage] |= writable_bitmask << start_slot;

   for (unsigned i = 0; i < count; i++) {
      unsigned slot = start_slot + i;
      struct pipe_shader_buffer *ssbo = &ctx->ssbos[p_stage][slot];
      struct zink_resource *res = ssbo->buffer ? zink_resource(ssbo->buffer) : NULL;
      bool was_writable = old_writable_mask & BITFIELD64_BIT(slot);
      if (buffers && buffers[i].buffer) {
         struct zink_resource *new_res = zink_resource(buffers[i].buffer);
         if (new_res != res) {
            unbind_ssbo(ctx, res, p_stage, slot, was_writable);
            new_res->ssbo_bind_mask[p_stage] |= BITFIELD_BIT(slot);
            new_res->ssbo_bind_count[p_stage == MESA_SHADER_COMPUTE]++;
            new_res->gfx_barrier |= zink_pipeline_flags_from_pipe_stage(p_stage);
            update_res_bind_count(ctx, new_res, p_stage == MESA_SHADER_COMPUTE, false);
         }
         VkAccessFlags access = VK_ACCESS_SHADER_READ_BIT;
         if (ctx->writable_ssbos[p_stage] & BITFIELD64_BIT(slot)) {
            new_res->write_bind_count[p_stage == MESA_SHADER_COMPUTE]++;
            access |= VK_ACCESS_SHADER_WRITE_BIT;
         }
         pipe_resource_reference(&ssbo->buffer, &new_res->base.b);
         new_res->barrier_access[p_stage == MESA_SHADER_COMPUTE] |= access;
         ssbo->buffer_offset = buffers[i].buffer_offset;
         ssbo->buffer_size = MIN2(buffers[i].buffer_size, new_res->base.b.width0 - ssbo->buffer_offset);
         util_range_add(&new_res->base.b, &new_res->valid_buffer_range, ssbo->buffer_offset,
                        ssbo->buffer_offset + ssbo->buffer_size);
         zink_screen(ctx->base.screen)->buffer_barrier(ctx, new_res, access,
                                      new_res->gfx_barrier);
         zink_batch_resource_usage_set(ctx->bs, new_res, access & VK_ACCESS_SHADER_WRITE_BIT, true);
         update = true;
         max_slot = MAX2(max_slot, slot);
         if (use_db) {
            update_descriptor_state_ssbo_db(ctx, p_stage, slot, new_res);
         } else {
            update_descriptor_state_ssbo_lazy(ctx, p_stage, slot, new_res);
         }
         if (zink_resource_access_is_write(access))
            new_res->obj->unordered_write = false;
         new_res->obj->unordered_read = false;
      } else {
         if (res)
            update = true;
         ssbo->buffer_offset = 0;
         ssbo->buffer_size = 0;
         if (res) {
            unbind_ssbo(ctx, res, p_stage, slot, was_writable);
            if (use_db) {
               update_descriptor_state_ssbo_db(ctx, p_stage, slot, NULL);
            } else {
               update_descriptor_state_ssbo_lazy(ctx, p_stage, slot, NULL);
            }

         }
         pipe_resource_reference(&ssbo->buffer, NULL);
      }
   }
   if (start_slot + count >= ctx->di.num_ssbos[p_stage])
      ctx->di.num_ssbos[p_stage] = max_slot + 1;
   if (update)
      ctx->invalidate_descriptor_state(ctx, p_stage, ZINK_DESCRIPTOR_TYPE_SSBO, start_slot, count);
}

static void
zink_set_shader_buffers_db(struct pipe_context *pctx,
                        gl_shader_stage p_stage,
                        unsigned start_slot, unsigned count,
                        const struct pipe_shader_buffer *buffers,
                        unsigned writable_bitmask)
{
   zink_set_shader_buffers_internal(pctx, p_stage, start_slot, count, buffers, writable_bitmask, true);
}

static void
zink_set_shader_buffers_lazy(struct pipe_context *pctx,
                        gl_shader_stage p_stage,
                        unsigned start_slot, unsigned count,
                        const struct pipe_shader_buffer *buffers,
                        unsigned writable_bitmask)
{
   zink_set_shader_buffers_internal(pctx, p_stage, start_slot, count, buffers, writable_bitmask, false);
}

static void
update_binds_for_samplerviews(struct zink_context *ctx, struct zink_resource *res, bool is_compute)
{
    VkImageLayout layout = get_layout_for_binding(ctx, res, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, is_compute);
    if (is_compute) {
       u_foreach_bit(slot, res->sampler_binds[MESA_SHADER_COMPUTE]) {
          if (ctx->di.textures[MESA_SHADER_COMPUTE][slot].imageLayout != layout) {
             update_descriptor_state_sampler(ctx, MESA_SHADER_COMPUTE, slot, res);
             ctx->invalidate_descriptor_state(ctx, MESA_SHADER_COMPUTE, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, slot, 1);
          }
       }
    } else {
       for (unsigned i = 0; i < ZINK_GFX_SHADER_COUNT; i++) {
          u_foreach_bit(slot, res->sampler_binds[i]) {
             if (ctx->di.textures[i][slot].imageLayout != layout) {
                update_descriptor_state_sampler(ctx, i, slot, res);
                ctx->invalidate_descriptor_state(ctx, i, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, slot, 1);
             }
          }
       }
    }
}

static void
flush_pending_clears(struct zink_context *ctx, struct zink_resource *res, int z, int depth)
{
   if (res->fb_bind_count && ctx->clears_enabled)
      zink_fb_clears_apply(ctx, &res->base.b, z, depth);
}

static inline void
unbind_shader_image_counts(struct zink_context *ctx, struct zink_resource *res, bool is_compute, bool writable)
{
   update_res_bind_count(ctx, res, is_compute, true);
   if (writable)
      res->write_bind_count[is_compute]--;
   res->image_bind_count[is_compute]--;
   /* if this was the last image bind, the sampler bind layouts must be updated */
   if (!zink_screen(ctx->base.screen)->driver_workarounds.general_layout && !res->obj->is_buffer && !res->image_bind_count[is_compute] && res->bind_count[is_compute])
      update_binds_for_samplerviews(ctx, res, is_compute);
}

ALWAYS_INLINE static bool
check_for_layout_update(struct zink_context *ctx, struct zink_resource *res, bool is_compute)
{
   assert(!zink_screen(ctx->base.screen)->driver_workarounds.general_layout);
   VkImageLayout layout = res->bind_count[is_compute] ? zink_descriptor_util_image_layout_eval(ctx, res, is_compute) : VK_IMAGE_LAYOUT_UNDEFINED;
   VkImageLayout other_layout = res->bind_count[!is_compute] ? zink_descriptor_util_image_layout_eval(ctx, res, !is_compute) : VK_IMAGE_LAYOUT_UNDEFINED;
   bool ret = false;
   if (!is_compute && res->fb_binds && !(ctx->feedback_loops & res->fb_binds)) {
      /* always double check feedback loops */
      ret = !!_mesa_set_add(ctx->need_barriers[0], res);
   } else {
      if (res->bind_count[is_compute] && ((layout && res->layout != layout) ||
                                          /* Check if we need a queue family transfer */
                                          (res->queue != zink_screen(ctx->base.screen)->gfx_queue && res->queue != VK_QUEUE_FAMILY_IGNORED)))
         ret = !!_mesa_set_add(ctx->need_barriers[is_compute], res);
      if (res->bind_count[!is_compute] && other_layout && (layout != other_layout || res->layout != other_layout))
         ret = !!_mesa_set_add(ctx->need_barriers[!is_compute], res);
   }
   return ret;
}

static void
unbind_shader_image(struct zink_context *ctx, gl_shader_stage stage, unsigned slot)
{
   struct zink_image_view *image_view = &ctx->image_views[stage][slot];
   bool is_compute = stage == MESA_SHADER_COMPUTE;
   if (!image_view->base.resource)
      return;

   struct zink_resource *res = zink_resource(image_view->base.resource);
   res->image_binds[stage] &= ~BITFIELD_BIT(slot);
   unbind_shader_image_counts(ctx, res, is_compute, image_view->base.access & PIPE_IMAGE_ACCESS_WRITE);
   if (!res->write_bind_count[is_compute])
      res->barrier_access[stage == MESA_SHADER_COMPUTE] &= ~VK_ACCESS_SHADER_WRITE_BIT;
   
   if (image_view->import2d) {
      unbind_buffer_descriptor_stage(res, stage);
      unbind_buffer_descriptor_reads(res, stage == MESA_SHADER_COMPUTE);
      _mesa_set_remove_key(ctx->need_barriers[is_compute], image_view->import2d);
   } else if (image_view->base.resource->target == PIPE_BUFFER) {
      unbind_buffer_descriptor_stage(res, stage);
      unbind_buffer_descriptor_reads(res, stage == MESA_SHADER_COMPUTE);
   } else {
      unbind_descriptor_stage(res, stage);
      unbind_descriptor_reads(res, stage == MESA_SHADER_COMPUTE);
      if (!zink_screen(ctx->base.screen)->driver_workarounds.general_layout && !res->image_bind_count[is_compute])
         check_for_layout_update(ctx, res, is_compute);
   }
   pipe_resource_reference(&image_view->base.resource, NULL);
   image_view->base.resource = NULL;
   image_view->surface = NULL;
   image_view->import2d = NULL;
}

static struct zink_buffer_view *
create_image_bufferview(struct zink_context *ctx, const struct pipe_image_view *view)
{
   struct zink_resource *res = zink_resource(view->resource);
   struct zink_buffer_view *buffer_view = get_buffer_view(ctx, res, view->format, view->u.buf.offset, view->u.buf.size);
   if (!buffer_view)
      return NULL;
   util_range_add(&res->base.b, &res->valid_buffer_range, view->u.buf.offset,
                  view->u.buf.offset + view->u.buf.size);
   return buffer_view;
}

static void
finalize_image_bind(struct zink_context *ctx, struct zink_resource *res, bool is_compute, VkAccessFlags flags, VkPipelineStageFlags pipeline)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   bool general_layout = screen->driver_workarounds.general_layout;
   if (general_layout) {
      /* no need to check later */
      screen->image_barrier(ctx, res, VK_IMAGE_LAYOUT_GENERAL, flags, pipeline);
      res->obj->unordered_write = false;
      res->obj->unordered_read = false;
   } else {
      /* if this is the first image bind and there are sampler binds, the image's sampler layout
      * must be updated to GENERAL
      */
      if (res->image_bind_count[is_compute] == 1 &&
         res->bind_count[is_compute] > 1)
         update_binds_for_samplerviews(ctx, res, is_compute);
      if (!check_for_layout_update(ctx, res, is_compute)) {
         /* no deferred barrier: unset unordered usage immediately */
         res->obj->unordered_write = false;
         res->obj->unordered_read = false;
      }
   }
}

static struct zink_surface *
create_image_surface(struct zink_context *ctx, const struct pipe_image_view *view, bool is_compute, struct zink_resource **import2d)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   struct pipe_resource *pres = view->resource;
   struct zink_resource *res = zink_resource(pres);
   struct pipe_surface tmpl = {0};
   bool tex2d_from_buf = view->access & PIPE_IMAGE_ACCESS_TEX2D_FROM_BUFFER;
   enum pipe_texture_target target = tex2d_from_buf ? PIPE_TEXTURE_2D : res->base.b.target;
   unsigned depth = 1, z = 0;
   tmpl.format = view->format;
   if (tex2d_from_buf) {
      struct pipe_resource *import = rebind_buffer_as_image(&ctx->base, pres, view->format, &view->u.tex2d_from_buf, true);
      assert(import);
      res = zink_resource(import);
      pres = import;
      *import2d = res;
   } else {
      tmpl.level = view->u.tex.level;
      tmpl.first_layer = view->u.tex.first_layer;
      tmpl.last_layer = view->u.tex.last_layer;
      depth += tmpl.last_layer - tmpl.first_layer;
      z = tmpl.first_layer;
   }
   tmpl.texture = pres;
   switch (target) {
   case PIPE_TEXTURE_3D:
      if (depth < u_minify(res->base.b.depth0, view->u.tex.level)) {
         assert(depth == 1);
         target = PIPE_TEXTURE_2D;
         if (!screen->info.have_EXT_image_2d_view_of_3d ||
             !screen->info.view2d_feats.image2DViewOf3D) {
            static bool warned = false;
            warn_missing_feature(warned, "image2DViewOf3D");
         }
      } else {
         assert(tmpl.first_layer == 0);
         tmpl.last_layer = 0;
      }
      break;
   case PIPE_TEXTURE_2D_ARRAY:
   case PIPE_TEXTURE_1D_ARRAY:
      if (depth < res->base.b.array_size && depth == 1)
         target = target == PIPE_TEXTURE_2D_ARRAY ? PIPE_TEXTURE_2D : PIPE_TEXTURE_1D;
      break;
   default: break;
   }
   VkImageViewCreateInfo ivci = create_ivci(screen, res, &tmpl, target);
   struct zink_surface *surface = zink_get_surface(ctx, &tmpl, &ivci);
   if (!surface)
      return NULL;
   if (is_compute)
      flush_pending_clears(ctx, res, z, depth);
   return surface;
}

static void
bind_shaderimage_resource_stage(struct zink_context *ctx, const struct pipe_image_view *b, struct zink_resource *res, bool is_compute)
{
   update_res_bind_count(ctx, res, is_compute, false);
   res->image_bind_count[is_compute]++;
   /* always increment write_bind_count on new bind */
   if (b->access & PIPE_IMAGE_ACCESS_WRITE)
      res->write_bind_count[is_compute]++;
}

static void
zink_set_shader_images(struct pipe_context *pctx,
                       gl_shader_stage shader_type,
                       unsigned start_slot, unsigned count,
                       unsigned unbind_num_trailing_slots,
                       const struct pipe_image_view *images)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_screen *screen = zink_screen(pctx->screen);
   bool update = false;
   bool is_compute = shader_type == MESA_SHADER_COMPUTE;
   assert(!ctx->unordered_blitting);
   for (unsigned i = 0; i < count; i++) {
      struct zink_image_view *a = &ctx->image_views[shader_type][start_slot + i];
      const struct pipe_image_view *b = images ? &images[i] : NULL;
      struct zink_resource *res = b ? zink_resource(b->resource) : NULL;
      bool tex2d_from_buf = b && b->access & PIPE_IMAGE_ACCESS_TEX2D_FROM_BUFFER;
      if (b && b->resource) {
         if (!zink_resource_object_init_storage(ctx, res)) {
            debug_printf("couldn't create storage image!");
            continue;
         }

         VkAccessFlags access = 0;
         if (b->access & PIPE_IMAGE_ACCESS_WRITE) {
            access |= VK_ACCESS_SHADER_WRITE_BIT;
         }
         if (b->access & PIPE_IMAGE_ACCESS_READ) {
            access |= VK_ACCESS_SHADER_READ_BIT;
         }

         bool changed = false;
         if (!a->base.resource || a->base.resource != b->resource) {
            /* this needs a full unbind+bind */
            changed = true;
            unbind_shader_image(ctx, shader_type, start_slot + i);
            bind_shaderimage_resource_stage(ctx, b, res, is_compute);
         } else {
            /* resource matches: check for write flag change and partial rebind */

            /* previous bind didn't have write: increment */
            if ((b->access & PIPE_IMAGE_ACCESS_WRITE) && !(a->base.access & PIPE_IMAGE_ACCESS_WRITE))
               res->write_bind_count[is_compute]++;
            /* previous bind had write: decrement */
            else if (!(b->access & PIPE_IMAGE_ACCESS_WRITE) && (a->base.access & PIPE_IMAGE_ACCESS_WRITE)) {
               res->write_bind_count[is_compute]--;
               if (!res->write_bind_count[is_compute])
                  res->barrier_access[is_compute] &= ~VK_ACCESS_SHADER_WRITE_BIT;
            }

            /* this may need a partial rebind */
            changed = a->base.format != b->format || zink_resource(a->base.resource)->obj != res->obj;
            if (!changed) {
               if (tex2d_from_buf) {
                  changed = !!memcmp(&a->base.u.tex2d_from_buf, &b->u.tex2d_from_buf, sizeof(b->u.tex2d_from_buf));
               } else if (b->resource->target == PIPE_BUFFER) {
                  /* db mode has no partial rebind */
                  if (zink_descriptor_mode != ZINK_DESCRIPTOR_MODE_DB)
                     changed = !!memcmp(&a->base.u.buf, &b->u.buf, sizeof(b->u.buf));
               } else {
                  /* no memcmp, these are bitfields */
                  changed = a->base.u.tex.first_layer != b->u.tex.first_layer ||
                            a->base.u.tex.last_layer != b->u.tex.last_layer ||
                            a->base.u.tex.level != b->u.tex.level;
               }
            }
         }

         if (changed) {
            /* this is a partial rebind */
            if (b->resource->target == PIPE_BUFFER && !tex2d_from_buf) {
               /* db has no partial rebind */
               if (zink_descriptor_mode != ZINK_DESCRIPTOR_MODE_DB) {
                  /* bufferview rebind: get updated bufferview and unref old one */
                  struct zink_buffer_view *bv = create_image_bufferview(ctx, b);
                  /* identical rebind was already checked above */
                  assert(bv && bv != a->buffer_view);
                  /* ref already added by create */
                  a->buffer_view = bv;
               }
            } else {
               struct zink_resource *import2d = NULL;
               struct zink_surface *surface = create_image_surface(ctx, b, is_compute, &import2d);
               if (tex2d_from_buf) {
                  a->import2d = import2d;
                  bind_shaderimage_resource_stage(ctx, b, a->import2d, is_compute);
               }
               a->rebind_count = res->rebind_count;
               a->surface = surface;
            }
         }

         /* these operations occur regardless of binding/rebinding */
         res->gfx_barrier |= zink_pipeline_flags_from_pipe_stage(shader_type);
         res->barrier_access[is_compute] |= access;
         if (tex2d_from_buf) {
            a->import2d->gfx_barrier |= zink_pipeline_flags_from_pipe_stage(shader_type);
            a->import2d->barrier_access[is_compute] |= access;
            screen->buffer_barrier(ctx, res, access,
                                         res->gfx_barrier);
            zink_batch_resource_usage_set(ctx->bs, res,
                                          zink_resource_access_is_write(access), true);
            finalize_image_bind(ctx, a->import2d, is_compute, access, res->gfx_barrier);
            zink_batch_resource_usage_set(ctx->bs, a->import2d,
                                          zink_resource_access_is_write(access), false);
            if (zink_resource_access_is_write(access))
               res->obj->unordered_write = false;
            res->obj->unordered_read = false;
         } else if (b->resource->target == PIPE_BUFFER) {
            screen->buffer_barrier(ctx, res, access,
                                         res->gfx_barrier);
            zink_batch_resource_usage_set(ctx->bs, res,
                                          zink_resource_access_is_write(access), true);
            if (zink_resource_access_is_write(access))
               res->obj->unordered_write = false;
            res->obj->unordered_read = false;
         } else {
            finalize_image_bind(ctx, res, is_compute, access, res->gfx_barrier);
            zink_batch_resource_usage_set(ctx->bs, res,
                                          zink_resource_access_is_write(access), false);
         }
         util_copy_image_view(&a->base, images + i);
         if (b->resource->target == PIPE_BUFFER && !tex2d_from_buf) {
            /* always enforce limit clamping */
            unsigned blocksize = util_format_get_blocksize(a->base.format);
            a->base.u.buf.size = MIN2(a->base.u.buf.size / blocksize, screen->info.props.limits.maxTexelBufferElements) * blocksize;
         }
         update = true;
         res->image_binds[shader_type] |= BITFIELD_BIT(start_slot + i);
         if (tex2d_from_buf)
            a->import2d->image_binds[shader_type] |= BITFIELD_BIT(start_slot + i);
      } else if (a->base.resource) {
         update = true;
         unbind_shader_image(ctx, shader_type, start_slot + i);
      }
      update_descriptor_state_image(ctx, shader_type, start_slot + i, tex2d_from_buf ? a->import2d : res);
   }
   for (unsigned i = 0; i < unbind_num_trailing_slots; i++) {
      update |= !!ctx->image_views[shader_type][start_slot + count + i].base.resource;
      unbind_shader_image(ctx, shader_type, start_slot + count + i);
      update_descriptor_state_image(ctx, shader_type, start_slot + count + i, NULL);
   }
   ctx->di.num_images[shader_type] = start_slot + count;
   if (update)
      ctx->invalidate_descriptor_state(ctx, shader_type, ZINK_DESCRIPTOR_TYPE_IMAGE, start_slot, count);
}

static void
update_feedback_loop_dynamic_state(struct zink_context *ctx)
{
   if (!zink_screen(ctx->base.screen)->info.have_EXT_attachment_feedback_loop_dynamic_state)
      return;
   VkImageAspectFlags aspects = 0;
   if (ctx->feedback_loops & BITFIELD_MASK(PIPE_MAX_COLOR_BUFS))
      aspects |= VK_IMAGE_ASPECT_COLOR_BIT;
   if (ctx->feedback_loops & BITFIELD_BIT(PIPE_MAX_COLOR_BUFS))
      aspects |= VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT;
   VKCTX(CmdSetAttachmentFeedbackLoopEnableEXT)(ctx->bs->cmdbuf, aspects);
}

static void
update_feedback_loop_state(struct zink_context *ctx, unsigned idx, unsigned feedback_loops)
{
   if (feedback_loops != ctx->feedback_loops) {
      if (idx == PIPE_MAX_COLOR_BUFS && !zink_screen(ctx->base.screen)->driver_workarounds.always_feedback_loop_zs) {
         if (ctx->gfx_pipeline_state.feedback_loop_zs)
            ctx->gfx_pipeline_state.dirty = true;
         ctx->gfx_pipeline_state.feedback_loop_zs = false;
      } else if (idx < PIPE_MAX_COLOR_BUFS && !zink_screen(ctx->base.screen)->driver_workarounds.always_feedback_loop) {
         if (ctx->gfx_pipeline_state.feedback_loop)
            ctx->gfx_pipeline_state.dirty = true;
         ctx->gfx_pipeline_state.feedback_loop = false;
      }
      update_feedback_loop_dynamic_state(ctx);
   }
   ctx->feedback_loops = feedback_loops;
}

ALWAYS_INLINE static void
unbind_samplerview_res(struct zink_context *ctx, gl_shader_stage stage, unsigned slot, struct zink_resource *res)
{
   bool general_layout = zink_screen(ctx->base.screen)->driver_workarounds.general_layout;
   res->sampler_bind_count[stage == MESA_SHADER_COMPUTE]--;
   if (stage != MESA_SHADER_COMPUTE && !res->sampler_bind_count[0] && res->fb_bind_count) {
      u_foreach_bit(idx, res->fb_binds) {
         if (!general_layout && ctx->feedback_loops & BITFIELD_BIT(idx)) {
            ctx->dynamic_fb.attachments[idx].imageLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
            ctx->rp_layout_changed = true;
         }
         update_feedback_loop_state(ctx, idx, ctx->feedback_loops & ~BITFIELD_BIT(idx));
      }
   }
   update_res_bind_count(ctx, res, stage == MESA_SHADER_COMPUTE, true);
   res->sampler_binds[stage] &= ~BITFIELD_BIT(slot);
   if (res->obj->is_buffer) {
      unbind_buffer_descriptor_stage(res, stage);
      unbind_buffer_descriptor_reads(res, stage == MESA_SHADER_COMPUTE);
   } else {
      unbind_descriptor_stage(res, stage);
      unbind_descriptor_reads(res, stage == MESA_SHADER_COMPUTE);
      if (!general_layout && !res->sampler_bind_count[stage == MESA_SHADER_COMPUTE])
         check_for_layout_update(ctx, res, stage == MESA_SHADER_COMPUTE);
   }
   assert(slot < 32);
   ctx->di.zs_swizzle[stage].mask &= ~BITFIELD_BIT(slot);
}

ALWAYS_INLINE static void
unbind_samplerview(struct zink_context *ctx, gl_shader_stage stage, unsigned slot)
{
   struct zink_sampler_view *sv = zink_sampler_view(ctx->sampler_views[stage][slot]);
   if (!sv || !sv->base.texture)
      return;
   struct zink_resource *res = zink_resource(sv->base.texture);
   unbind_samplerview_res(ctx, stage, slot, res);
   if (sv->import2d)
      unbind_samplerview_res(ctx, stage, slot, sv->import2d);
   assert(slot < 32);
   ctx->di.zs_swizzle[stage].mask &= ~BITFIELD_BIT(slot);
}

static void
bind_samplerview_resource_stage(struct zink_context *ctx, struct zink_resource *res, gl_shader_stage shader_type)
{
   update_res_bind_count(ctx, res, shader_type == MESA_SHADER_COMPUTE, false);
   res->sampler_bind_count[shader_type == MESA_SHADER_COMPUTE]++;
   res->gfx_barrier |= zink_pipeline_flags_from_pipe_stage(shader_type);
   res->barrier_access[shader_type == MESA_SHADER_COMPUTE] |= VK_ACCESS_SHADER_READ_BIT;
}

static void
zink_set_sampler_views(struct pipe_context *pctx,
                       gl_shader_stage shader_type,
                       unsigned start_slot,
                       unsigned num_views,
                       unsigned unbind_num_trailing_slots,
                       struct pipe_sampler_view **views)
{
   struct zink_context *ctx = zink_context(pctx);
   bool general_layout = zink_screen(ctx->base.screen)->driver_workarounds.general_layout;
   const uint32_t mask = BITFIELD_RANGE(start_slot, num_views);
   uint32_t shadow_mask = ctx->di.zs_swizzle[shader_type].mask;
   ctx->di.cubes[shader_type] &= ~mask;

   bool update = false;
   bool shadow_update = false;
   if (views) {
      for (unsigned i = 0; i < num_views; ++i) {
         struct pipe_sampler_view *pview = views[i];
         struct zink_sampler_view *a = zink_sampler_view(ctx->sampler_views[shader_type][start_slot + i]);
         struct zink_sampler_view *b = zink_sampler_view(pview);

         if (a == b) {
            continue;
         }

         struct zink_resource *res = b ? zink_resource(b->base.texture) : NULL;
         if (b && b->base.texture) {
            if (!a || zink_resource(a->base.texture) != res || a->import2d != b->import2d) {
               if (a)
                  unbind_samplerview(ctx, shader_type, start_slot + i);
               bind_samplerview_resource_stage(ctx, res, shader_type);
               if (b->import2d) {
                  bind_samplerview_resource_stage(ctx, b->import2d, shader_type);
                  zink_batch_resource_usage_set(ctx->bs, res, false, true);
               }
            }
            /* switch to possible tex2d_from_buf resource: none of this buffer stuff matters */
            if (b->import2d)
               res = b->import2d;
            if (res->base.b.target == PIPE_BUFFER && !b->import2d) {
               if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
                  if (!a || a->base.texture != b->base.texture || zink_resource(a->base.texture)->obj != res->obj ||
                     memcmp(&a->base.u.buf, &b->base.u.buf, sizeof(b->base.u.buf)))
                     update = true;
               } else if (b->rebind_count != res->rebind_count) {
                  /* if this resource has been rebound while it wasn't set here,
                  * its backing resource will have changed and thus we need to update
                  * the bufferview
                  */
                  b->buffer_view = get_buffer_view(ctx, res, b->base.format, b->base.u.buf.offset, b->base.u.buf.size);
                  b->rebind_count = res->rebind_count;
                  update = true;
               } else if (!a || a->buffer_view != b->buffer_view)
                     update = true;
               zink_screen(ctx->base.screen)->buffer_barrier(ctx, res, VK_ACCESS_SHADER_READ_BIT,
                                          res->gfx_barrier);
               zink_batch_resource_usage_set(ctx->bs, res, false, true);
               if (!ctx->unordered_blitting)
                  res->obj->unordered_read = false;
            } else {
               if (res->rebind_count != b->rebind_count) {
                  b->rebind_count = res->rebind_count;
                  struct pipe_surface tmpl = pipe_surface_templ_from_sampler_view(&b->base, &res->base.b, b->base.target);
                  b->image_view = zink_get_surface(ctx, &tmpl, &b->ivci);
                  update = true;
               } else  if (a != b)
                  update = true;
               if (shader_type == MESA_SHADER_COMPUTE && res->fb_bind_count)
                  flush_pending_clears(ctx, res, b->base.u.tex.first_layer, b->base.u.tex.last_layer - b->base.u.tex.first_layer + 1);
               if (b->cube_array) {
                  ctx->di.cubes[shader_type] |= BITFIELD_BIT(start_slot + i);
               }

               if (general_layout) {
                  if (!ctx->blitting)
                     zink_screen(ctx->base.screen)->image_barrier(ctx, res, VK_IMAGE_LAYOUT_GENERAL, VK_ACCESS_SHADER_READ_BIT, res->gfx_barrier);
                  if (!ctx->unordered_blitting)
                     /* no deferred barrier: unset unordered usage immediately */
                     res->obj->unordered_read = false;
               } else if (!check_for_layout_update(ctx, res, shader_type == MESA_SHADER_COMPUTE) && !ctx->unordered_blitting) {
                  /* no deferred barrier: unset unordered usage immediately */
                  res->obj->unordered_read = false;
                  res->obj->unordered_write = false;
               }
               if (!a)
                  update = true;
               zink_batch_resource_usage_set(ctx->bs, res, false, false);
               if (b->zs_view) {
                  assert(start_slot + i < 32); //bitfield size
                  ctx->di.zs_swizzle[shader_type].mask |= BITFIELD_BIT(start_slot + i);
                  /* this is already gonna be slow, so don't bother trying to micro-optimize */
                  shadow_update |= memcmp(&ctx->di.zs_swizzle[shader_type].swizzle[start_slot + i],
                                          &b->swizzle, sizeof(struct zink_zs_swizzle));
                  memcpy(&ctx->di.zs_swizzle[shader_type].swizzle[start_slot + i], &b->swizzle, sizeof(struct zink_zs_swizzle));
               } else {
                  assert(start_slot + i < 32); //bitfield size
                  ctx->di.zs_swizzle[shader_type].mask &= ~BITFIELD_BIT(start_slot + i);
               }
            }
            res->sampler_binds[shader_type] |= BITFIELD_BIT(start_slot + i);
         } else if (a) {
            unbind_samplerview(ctx, shader_type, start_slot + i);
            update = true;
         }
         pipe_sampler_view_reference(&ctx->sampler_views[shader_type][start_slot + i], pview);
         update_descriptor_state_sampler(ctx, shader_type, start_slot + i, res);
      }
   } else {
      unbind_num_trailing_slots += num_views;
      num_views = 0;
   }
   for (unsigned i = 0; i < unbind_num_trailing_slots; ++i) {
      unsigned slot = start_slot + num_views + i;
      update |= !!ctx->sampler_views[shader_type][slot];
      unbind_samplerview(ctx, shader_type, slot);
      pipe_sampler_view_reference(&ctx->sampler_views[shader_type][slot], NULL);
      update_descriptor_state_sampler(ctx, shader_type, slot, NULL);
   }
   ctx->di.num_sampler_views[shader_type] = start_slot + num_views;
   if (update) {
      struct zink_screen *screen = zink_screen(pctx->screen);
      ctx->invalidate_descriptor_state(ctx, shader_type, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, start_slot, num_views);
      if (!screen->info.have_EXT_non_seamless_cube_map)
         update_nonseamless_shader_key(ctx, shader_type);
      shadow_update |= shadow_mask != ctx->di.zs_swizzle[shader_type].mask;
      zink_set_zs_needs_shader_swizzle_key(ctx, shader_type, shadow_update);
   }
}

static uint64_t
zink_create_texture_handle(struct pipe_context *pctx, struct pipe_sampler_view *view, const struct pipe_sampler_state *state)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_resource *res = zink_resource(view->texture);
   struct zink_sampler_view *sv = zink_sampler_view(view);
   struct zink_bindless_descriptor *bd;
   bd = calloc(1, sizeof(struct zink_bindless_descriptor));
   if (!bd)
      return 0;

   bd->sampler = pctx->create_sampler_state(pctx, state);
   if (!bd->sampler) {
      free(bd);
      return 0;
   }

   bd->ds.is_buffer = res->base.b.target == PIPE_BUFFER;
   pipe_resource_reference(&bd->pres, view->texture);
   if (res->base.b.target == PIPE_BUFFER) {
      bd->ds.db.format = view->format;
      bd->ds.db.offset = view->u.buf.offset;
      bd->ds.db.size = view->u.buf.size;
   } else {
      bd->ds.surface = sv->image_view;
      bd->first_layer = view->u.tex.first_layer;
      bd->last_layer = view->u.tex.last_layer;
   }
   uint64_t handle = util_idalloc_alloc(&ctx->di.bindless[bd->ds.is_buffer].tex_slots);
   if (bd->ds.is_buffer)
      handle += ZINK_MAX_BINDLESS_HANDLES;
   bd->handle = handle;
   _mesa_hash_table_insert(&ctx->di.bindless[bd->ds.is_buffer].tex_handles, (void*)(uintptr_t)handle, bd);
   return handle;
}

static void
zink_delete_texture_handle(struct pipe_context *pctx, uint64_t handle)
{
   struct zink_context *ctx = zink_context(pctx);
   bool is_buffer = ZINK_BINDLESS_IS_BUFFER(handle);
   struct hash_entry *he = _mesa_hash_table_search(&ctx->di.bindless[is_buffer].tex_handles, (void*)(uintptr_t)handle);
   assert(he);
   struct zink_bindless_descriptor *bd = he->data;
   struct zink_descriptor_surface *ds = &bd->ds;
   _mesa_hash_table_remove(&ctx->di.bindless[is_buffer].tex_handles, he);
   uint32_t h = handle;
   util_dynarray_append(&ctx->bs->bindless_releases[0], uint32_t, h);

   pipe_resource_reference(&bd->pres, NULL);
   if (!ds->is_buffer) {
      pctx->delete_sampler_state(pctx, bd->sampler);
   }
   free(ds);
}

static void
zero_bindless_descriptor(struct zink_context *ctx, uint32_t handle, bool is_buffer, bool is_image)
{
   if (is_buffer) {
      if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
         ctx->di.bindless[is_image].db.buffer_infos[handle].address = 0;
         ctx->di.bindless[is_image].db.buffer_infos[handle].range = 0;
      } else {
         VkBufferView *bv = &ctx->di.bindless[is_image].t.buffer_infos[handle];
         *bv = VK_NULL_HANDLE;
      }
   } else {
      VkDescriptorImageInfo *ii = &ctx->di.bindless[is_image].img_infos[handle];
      memset(ii, 0, sizeof(*ii));
   }
}

static void
unbind_bindless_descriptor(struct zink_context *ctx, struct zink_resource *res)
{
   if (!res->bindless[1]) {
      /* check to remove write access */
      for (unsigned i = 0; i < 2; i++) {
         if (!res->write_bind_count[i])
            res->barrier_access[i] &= ~VK_ACCESS_SHADER_WRITE_BIT;
      }
   }
   bool is_buffer = res->base.b.target == PIPE_BUFFER;
   if (!res->all_bindless) {
      /* check to remove read access */
      if (is_buffer) {
         for (unsigned i = 0; i < 2; i++)
            unbind_buffer_descriptor_reads(res, i);
      } else {
         for (unsigned i = 0; i < 2; i++)
            unbind_descriptor_reads(res, i);
      }
   }
   if (zink_screen(ctx->base.screen)->driver_workarounds.general_layout)
      return;
   for (unsigned i = 0; i < 2; i++) {
      if (!res->image_bind_count[i])
         check_for_layout_update(ctx, res, i);
   }
}

static void
zink_make_texture_handle_resident(struct pipe_context *pctx, uint64_t handle, bool resident)
{
   struct zink_context *ctx = zink_context(pctx);
   bool general_layout = zink_screen(ctx->base.screen)->driver_workarounds.general_layout;
   bool is_buffer = ZINK_BINDLESS_IS_BUFFER(handle);
   struct hash_entry *he = _mesa_hash_table_search(&ctx->di.bindless[is_buffer].tex_handles, (void*)(uintptr_t)handle);
   assert(he);
   struct zink_bindless_descriptor *bd = he->data;
   struct zink_descriptor_surface *ds = &bd->ds;
   struct zink_resource *res = zink_resource(bd->pres);
   if (is_buffer)
      handle -= ZINK_MAX_BINDLESS_HANDLES;
   if (resident) {
      update_res_bind_count(ctx, res, false, false);
      update_res_bind_count(ctx, res, true, false);
      res->bindless[0]++;
      if (is_buffer) {
         if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
            ctx->di.bindless[0].db.buffer_infos[handle].address = res->obj->bda + ds->db.offset;
            ctx->di.bindless[0].db.buffer_infos[handle].range = ds->db.size;
            ctx->di.bindless[0].db.buffer_infos[handle].format = zink_get_format(zink_screen(ctx->base.screen), ds->db.format);
         } else {
            struct zink_buffer_view *bv = get_buffer_view(ctx, res, ds->db.format, ds->db.offset, ds->db.size);
            ctx->di.bindless[0].t.buffer_infos[handle] = bv->buffer_view;
         }
         zink_screen(ctx->base.screen)->buffer_barrier(ctx, res, VK_ACCESS_SHADER_READ_BIT, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT | VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT);
         zink_batch_resource_usage_set(ctx->bs, res, false, true);
         res->obj->unordered_read = false;
      } else {
         VkDescriptorImageInfo *ii = &ctx->di.bindless[0].img_infos[handle];
         ii->sampler = bd->sampler->sampler;
         ii->imageView = ds->surface->image_view;
         ii->imageLayout = zink_descriptor_util_image_layout_eval(ctx, res, false);
         flush_pending_clears(ctx, res, bd->first_layer, bd->last_layer - bd->first_layer + 1);
         if (general_layout) {
            res->obj->unordered_read = false;
            zink_screen(ctx->base.screen)->image_barrier(ctx, res, VK_IMAGE_LAYOUT_GENERAL, VK_ACCESS_SHADER_READ_BIT, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT | VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT);
         } else {
            if (!check_for_layout_update(ctx, res, false)) {
               res->obj->unordered_read = false;
               res->obj->unordered_write = false;
            }
            if (!check_for_layout_update(ctx, res, true)) {
               res->obj->unordered_read = false;
               res->obj->unordered_write = false;
            }
         }
         zink_batch_resource_usage_set(ctx->bs, res, false, false);
         res->obj->unordered_write = false;
      }
      res->gfx_barrier |= VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT | VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT;
      res->barrier_access[0] |= VK_ACCESS_SHADER_READ_BIT;
      res->barrier_access[1] |= VK_ACCESS_SHADER_READ_BIT;
      util_dynarray_append(&ctx->di.bindless[0].resident, struct zink_bindless_descriptor *, bd);
      uint32_t h = is_buffer ? handle + ZINK_MAX_BINDLESS_HANDLES : handle;
      util_dynarray_append(&ctx->di.bindless[0].updates, uint32_t, h);
   } else {
      zero_bindless_descriptor(ctx, handle, is_buffer, false);
      util_dynarray_delete_unordered(&ctx->di.bindless[0].resident, struct zink_bindless_descriptor *, bd);
      update_res_bind_count(ctx, res, false, true);
      update_res_bind_count(ctx, res, true, true);
      res->bindless[0]--;
      unbind_bindless_descriptor(ctx, res);
   }
   ctx->di.bindless_dirty[0] = true;
}

static uint64_t
zink_create_image_handle(struct pipe_context *pctx, const struct pipe_image_view *view)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_resource *res = zink_resource(view->resource);
   struct zink_bindless_descriptor *bd;
   if (!zink_resource_object_init_storage(ctx, res)) {
      debug_printf("couldn't create storage image!");
      return 0;
   }
   bd = calloc(1, sizeof(struct zink_bindless_descriptor));
   if (!bd)
      return 0;
   bd->sampler = NULL;

   bd->ds.is_buffer = res->base.b.target == PIPE_BUFFER;
   pipe_resource_reference(&bd->pres, view->resource);
   if (res->base.b.target == PIPE_BUFFER)
      if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
         bd->ds.db.format = view->format;
         bd->ds.db.offset = view->u.buf.offset;
         bd->ds.db.size = view->u.buf.size;
      } else {
         bd->ds.bufferview = create_image_bufferview(ctx, view);
      }
   else {
      struct zink_resource *import2d = NULL;
      bd->ds.surface = create_image_surface(ctx, view, false, &import2d);
      assert(!import2d);
   }
   uint64_t handle = util_idalloc_alloc(&ctx->di.bindless[bd->ds.is_buffer].img_slots);
   if (bd->ds.is_buffer)
      handle += ZINK_MAX_BINDLESS_HANDLES;
   bd->handle = handle;
   _mesa_hash_table_insert(&ctx->di.bindless[bd->ds.is_buffer].img_handles, (void*)(uintptr_t)handle, bd);
   return handle;
}

static void
zink_delete_image_handle(struct pipe_context *pctx, uint64_t handle)
{
   struct zink_context *ctx = zink_context(pctx);
   bool is_buffer = ZINK_BINDLESS_IS_BUFFER(handle);
   struct hash_entry *he = _mesa_hash_table_search(&ctx->di.bindless[is_buffer].img_handles, (void*)(uintptr_t)handle);
   assert(he);
   struct zink_bindless_descriptor *bd = he->data;
   struct zink_descriptor_surface *ds = &bd->ds;
   _mesa_hash_table_remove(&ctx->di.bindless[is_buffer].img_handles, he);
   uint32_t h = handle;
   util_dynarray_append(&ctx->bs->bindless_releases[1], uint32_t, h);

   pipe_resource_reference(&bd->pres, NULL);
   free(ds);
}

static void
zink_make_image_handle_resident(struct pipe_context *pctx, uint64_t handle, unsigned paccess, bool resident)
{
   struct zink_context *ctx = zink_context(pctx);
   bool is_buffer = ZINK_BINDLESS_IS_BUFFER(handle);
   struct hash_entry *he = _mesa_hash_table_search(&ctx->di.bindless[is_buffer].img_handles, (void*)(uintptr_t)handle);
   assert(he);
   struct zink_bindless_descriptor *bd = he->data;
   struct zink_descriptor_surface *ds = &bd->ds;
   bd->access = paccess;
   struct zink_resource *res = zink_resource(bd->pres);
   VkAccessFlags access = 0;
   if (paccess & PIPE_IMAGE_ACCESS_WRITE) {
      if (resident) {
         res->write_bind_count[0]++;
         res->write_bind_count[1]++;
      } else {
         res->write_bind_count[0]--;
         res->write_bind_count[1]--;
      }
      access |= VK_ACCESS_SHADER_WRITE_BIT;
   }
   if (paccess & PIPE_IMAGE_ACCESS_READ) {
      access |= VK_ACCESS_SHADER_READ_BIT;
   }
   if (is_buffer)
      handle -= ZINK_MAX_BINDLESS_HANDLES;
   if (resident) {
      update_res_bind_count(ctx, res, false, false);
      update_res_bind_count(ctx, res, true, false);
      res->image_bind_count[0]++;
      res->image_bind_count[1]++;
      res->bindless[1]++;
      if (is_buffer) {
         if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
            ctx->di.bindless[0].db.buffer_infos[handle].address = res->obj->bda + ds->db.offset;
            ctx->di.bindless[0].db.buffer_infos[handle].range = ds->db.size;
            ctx->di.bindless[0].db.buffer_infos[handle].format = zink_get_format(zink_screen(ctx->base.screen), ds->db.format);
         } else {
            struct zink_buffer_view *bv = get_buffer_view(ctx, res, ds->db.format, ds->db.offset, ds->db.size);
            ctx->di.bindless[1].t.buffer_infos[handle] = bv->buffer_view;
         }
         zink_screen(ctx->base.screen)->buffer_barrier(ctx, res, access, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT | VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT);
         zink_batch_resource_usage_set(ctx->bs, res, zink_resource_access_is_write(access), true);
         if (zink_resource_access_is_write(access))
            res->obj->unordered_write = false;
         res->obj->unordered_read = false;
      } else {
         VkDescriptorImageInfo *ii = &ctx->di.bindless[1].img_infos[handle];
         ii->sampler = VK_NULL_HANDLE;
         ii->imageView = ds->surface->image_view;
         ii->imageLayout = VK_IMAGE_LAYOUT_GENERAL;
         finalize_image_bind(ctx, res, false, access, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT | VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT);
         finalize_image_bind(ctx, res, true, access, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT | VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT);
         zink_batch_resource_usage_set(ctx->bs, res, zink_resource_access_is_write(access), false);
         res->obj->unordered_write = false;
      }
      res->gfx_barrier |= VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT | VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT;
      res->barrier_access[0] |= access;
      res->barrier_access[1] |= access;
      util_dynarray_append(&ctx->di.bindless[1].resident, struct zink_bindless_descriptor *, bd);
      uint32_t h = is_buffer ? handle + ZINK_MAX_BINDLESS_HANDLES : handle;
      util_dynarray_append(&ctx->di.bindless[1].updates, uint32_t, h);
   } else {
      zero_bindless_descriptor(ctx, handle, is_buffer, true);
      util_dynarray_delete_unordered(&ctx->di.bindless[1].resident, struct zink_bindless_descriptor *, bd);
      unbind_shader_image_counts(ctx, res, false, false);
      unbind_shader_image_counts(ctx, res, true, false);
      res->bindless[1]--;
      unbind_bindless_descriptor(ctx, res);
   }
   ctx->di.bindless_dirty[1] = true;
}

static void
zink_set_global_binding(struct pipe_context *pctx,
                        unsigned first, unsigned count,
                        struct pipe_resource **resources,
                        uint32_t **handles)
{
   struct zink_context *ctx = zink_context(pctx);

   size_t size = ctx->di.global_bindings.capacity;
   if (!util_dynarray_resize(&ctx->di.global_bindings, struct pipe_resource*, first + count + 8))
      UNREACHABLE("zink: out of memory somehow");
   if (size != ctx->di.global_bindings.capacity) {
      uint8_t *data = ctx->di.global_bindings.data;
      memset(data + size, 0, ctx->di.global_bindings.capacity - size);
   }

   struct pipe_resource **globals = ctx->di.global_bindings.data;
   for (unsigned i = 0; i < count; i++) {
      if (resources && resources[i]) {
         struct zink_resource *res = zink_resource(resources[i]);

         util_range_add(&res->base.b, &res->valid_buffer_range, 0, res->base.b.width0);
         pipe_resource_reference(&globals[first + i], resources[i]);

         uint64_t addr = 0;
         memcpy(&addr, handles[i], sizeof(addr));
         addr += zink_resource_get_address(zink_screen(pctx->screen), res);
         memcpy(handles[i], &addr, sizeof(addr));
         zink_resource_usage_set(res, ctx->bs, true);
         zink_screen(ctx->base.screen)->buffer_barrier(ctx, res, VK_ACCESS_SHADER_READ_BIT | VK_ACCESS_SHADER_WRITE_BIT, VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT);
         res->obj->unordered_read = res->obj->unordered_write = false;
      } else if (globals[i]) {
         zink_batch_reference_resource(ctx, zink_resource(globals[first + i]));
         pipe_resource_reference(&globals[first + i], NULL);
      }
   }
}

static void
zink_set_stencil_ref(struct pipe_context *pctx,
                     const struct pipe_stencil_ref ref)
{
   struct zink_context *ctx = zink_context(pctx);
   ctx->stencil_ref = ref;
   ctx->stencil_ref_changed = true;
}

static void
zink_set_tess_state(struct pipe_context *pctx,
                    const float default_outer_level[4],
                    const float default_inner_level[2])
{
   struct zink_context *ctx = zink_context(pctx);
   memcpy(&ctx->default_inner_level, default_inner_level, sizeof(ctx->default_inner_level));
   memcpy(&ctx->default_outer_level, default_outer_level, sizeof(ctx->default_outer_level));
}

static void
zink_set_patch_vertices(struct pipe_context *pctx, uint8_t patch_vertices)
{
   struct zink_context *ctx = zink_context(pctx);
   if (zink_set_tcs_key_patches(ctx, patch_vertices)) {
      ctx->gfx_pipeline_state.dyn_state2.vertices_per_patch = patch_vertices;
      if (zink_screen(ctx->base.screen)->info.dynamic_state2_feats.extendedDynamicState2PatchControlPoints)
         VKCTX(CmdSetPatchControlPointsEXT)(ctx->bs->cmdbuf, patch_vertices);
      else
         ctx->gfx_pipeline_state.dirty = true;
   }
}

static void
init_null_fbfetch(struct zink_context *ctx)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   ctx->di.null_fbfetch_init = true;
   if (zink_descriptor_mode != ZINK_DESCRIPTOR_MODE_DB)
      return;
   VkDescriptorGetInfoEXT info;
   info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_GET_INFO_EXT;
   info.pNext = NULL;
   info.type = VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT;
   info.data.pInputAttachmentImage = &ctx->di.fbfetch;
   if (screen->info.db_props.inputAttachmentDescriptorSize)
      VKSCR(GetDescriptorEXT)(screen->dev, &info, screen->info.db_props.inputAttachmentDescriptorSize, ctx->di.fbfetch_db);
}

bool
zink_update_fbfetch(struct zink_context *ctx)
{
   const bool had_fbfetch = ctx->di.fbfetch.imageLayout == VK_IMAGE_LAYOUT_GENERAL;
   if (!ctx->gfx_stages[MESA_SHADER_FRAGMENT] ||
       !ctx->gfx_stages[MESA_SHADER_FRAGMENT]->info.fs.uses_fbfetch_output) {
      if (!had_fbfetch)
         return false;
      zink_batch_no_rp(ctx);
      ctx->di.fbfetch.imageLayout = VK_IMAGE_LAYOUT_UNDEFINED;
      ctx->di.fbfetch.imageView = VK_NULL_HANDLE;
      ctx->invalidate_descriptor_state(ctx, MESA_SHADER_FRAGMENT, ZINK_DESCRIPTOR_TYPE_UBO, 0, 1);
      return true;
   }

   bool changed = !had_fbfetch;
   if (ctx->fb_state.cbufs[0].texture) {
      struct zink_surface *surf = zink_create_fb_surface(&ctx->base, &ctx->fb_state.cbufs[0]);
      if (!surf)
         /* swapchain image: retry later */
         return false;
      VkImageView fbfetch = surf->image_view;
      changed |= fbfetch != ctx->di.fbfetch.imageView;
      ctx->di.fbfetch.imageView = fbfetch;

      bool fbfetch_ms = ctx->fb_state.cbufs[0].texture->nr_samples > 1;
      if (zink_get_fs_base_key(ctx)->fbfetch_ms != fbfetch_ms)
         zink_set_fs_base_key(ctx)->fbfetch_ms = fbfetch_ms;
   } else {
      ctx->di.fbfetch.imageView = VK_NULL_HANDLE;
   }
   bool ret = false;
   ctx->di.fbfetch.imageLayout = VK_IMAGE_LAYOUT_GENERAL;
   if (changed) {
      ctx->invalidate_descriptor_state(ctx, MESA_SHADER_FRAGMENT, ZINK_DESCRIPTOR_TYPE_UBO, 0, 1);
      if (!had_fbfetch) {
         ret = true;
         zink_batch_no_rp(ctx);
      }
   }
   return ret;
}

void
zink_update_vk_sample_locations(struct zink_context *ctx)
{
   if (ctx->gfx_pipeline_state.sample_locations_enabled && ctx->sample_locations_changed) {
      unsigned samples = ctx->gfx_pipeline_state.rast_samples + 1;
      unsigned idx = util_logbase2_ceil(MAX2(samples, 1));
      VkExtent2D grid_size = zink_screen(ctx->base.screen)->maxSampleLocationGridSize[idx];
 
      for (unsigned pixel = 0; pixel < grid_size.width * grid_size.height; pixel++) {
         for (unsigned sample = 0; sample < samples; sample++) {
            unsigned pixel_x = pixel % grid_size.width;
            unsigned pixel_y = pixel / grid_size.width;
            unsigned wi = pixel * samples + sample;
            unsigned ri = (pixel_y * grid_size.width + pixel_x % grid_size.width);
            ri = ri * samples + sample;
            ctx->vk_sample_locations[wi].x = (ctx->sample_locations[ri] & 0xf) / 16.0f;
            ctx->vk_sample_locations[wi].y = (16 - (ctx->sample_locations[ri] >> 4)) / 16.0f;
         }
      }
   }
}

static unsigned
find_rp_state(struct zink_context *ctx)
{
   bool found = false;
   /* calc the state idx using the samples to account for msrtss */
   unsigned idx = zink_screen(ctx->base.screen)->info.have_EXT_multisampled_render_to_single_sampled && ctx->transient_attachments ? 
                  util_logbase2_ceil(ctx->gfx_pipeline_state.rast_samples + 1) : 0;
   struct set_entry *he = _mesa_set_search_or_add(&ctx->rendering_state_cache[idx], &ctx->gfx_pipeline_state.rendering_info, &found);
   struct zink_rendering_info *info;
   if (found) {
      info = (void*)he->key;
      return info->id;
   }
   info = ralloc(ctx, struct zink_rendering_info);
   memcpy(info, &ctx->gfx_pipeline_state.rendering_info, sizeof(VkPipelineRenderingCreateInfo));
   info->id = ctx->rendering_state_cache[idx].entries;
   he->key = info;
   return info->id;
}

unsigned
zink_update_rendering_info(struct zink_context *ctx)
{
   for (int i = 0; i < ctx->fb_state.nr_cbufs; i++) {
      ctx->gfx_pipeline_state.rendering_formats[i] = ctx->fb_formats[i];
   }
   ctx->gfx_pipeline_state.rendering_info.viewMask = ctx->fb_state.viewmask;
   ctx->gfx_pipeline_state.rendering_info.depthAttachmentFormat = VK_FORMAT_UNDEFINED;
   ctx->gfx_pipeline_state.rendering_info.stencilAttachmentFormat = VK_FORMAT_UNDEFINED;
   if (ctx->fb_state.zsbuf.texture && zink_is_zsbuf_used(ctx)) {
      bool has_depth = util_format_has_depth(util_format_description(ctx->fb_state.zsbuf.format));
      bool has_stencil = util_format_has_stencil(util_format_description(ctx->fb_state.zsbuf.format));

      if (has_depth)
         ctx->gfx_pipeline_state.rendering_info.depthAttachmentFormat = ctx->fb_formats[PIPE_MAX_COLOR_BUFS];
      if (has_stencil)
         ctx->gfx_pipeline_state.rendering_info.stencilAttachmentFormat = ctx->fb_formats[PIPE_MAX_COLOR_BUFS];
   }
   return find_rp_state(ctx);
}

static unsigned
calc_max_dummy_fbo_size(struct zink_context *ctx)
{
   unsigned size = MAX2(ctx->fb_state.width, ctx->fb_state.height);
   return size ? size : MIN2(256, zink_screen(ctx->base.screen)->info.props.limits.maxImageDimension2D);
}

ALWAYS_INLINE static void
update_res_sampler_layouts(struct zink_context *ctx, struct zink_resource *res)
{
   unsigned find = res->sampler_bind_count[0];
   for (unsigned i = 0; find && i < MESA_SHADER_COMPUTE; i++) {
      u_foreach_bit(slot, res->sampler_binds[i]) {
         /* only set layout, skip rest of update */
         if (ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW][i][slot] == res)
            ctx->di.textures[i][slot].imageLayout = zink_descriptor_util_image_layout_eval(ctx, res, false);
         find--;
         if (!find) break;
      }
   }
}

static bool
prep_fb_attachment(struct zink_context *ctx, struct zink_resource *res, unsigned i)
{
   VkAccessFlags access;
   VkPipelineStageFlags pipeline;
   if (zink_is_swapchain(res)) {
      if (!zink_kopper_acquire(ctx, res, UINT64_MAX))
         return false;
      if (!i)
         zink_update_fbfetch(ctx);
   }
   if (ctx->blitting) {
      zink_batch_resource_usage_set(ctx->bs, res, true, false);
      return true;
   }
   VkImageLayout layout;
   /* depth attachment is stored as the last attachment, but bitfields always use PIPE_MAX_COLOR_BUFS */
   int idx = i == ctx->fb_state.nr_cbufs ? PIPE_MAX_COLOR_BUFS : i;
   if (ctx->feedback_loops & BITFIELD_BIT(idx)) {
      /* reevaluate feedback loop in case layout change eliminates the loop */
      if (!res->sampler_bind_count[0] || (idx == PIPE_MAX_COLOR_BUFS && !zink_is_zsbuf_write(ctx)))
         update_feedback_loop_state(ctx, i, ctx->feedback_loops & ~BITFIELD_BIT(idx));
   }
   if (ctx->track_renderpasses) {
      layout = zink_tc_renderpass_info_parse(ctx, &ctx->dynamic_fb.tc_info, idx, &pipeline, &access);
      assert(i < ctx->fb_state.nr_cbufs || layout != VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL || !zink_fb_clear_enabled(ctx, PIPE_MAX_COLOR_BUFS));
      if (i == ctx->fb_state.nr_cbufs && zink_fb_clear_enabled(ctx, PIPE_MAX_COLOR_BUFS))
         assert(ctx->dynamic_fb.tc_info.zsbuf_clear || ctx->dynamic_fb.tc_info.zsbuf_clear_partial || ctx->dynamic_fb.tc_info.zsbuf_load);
   } else {
      struct zink_rt_attrib rt;
      if (i < ctx->fb_state.nr_cbufs)
         zink_init_color_attachment(ctx, i, &rt);
      else
         zink_init_zs_attachment(ctx, &rt);
      layout = zink_render_pass_attachment_get_barrier_info(&rt, i < ctx->fb_state.nr_cbufs, &pipeline, &access);
      /* avoid unnecessary read-only layout change */
      if (layout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL &&
            res->layout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL &&
            !res->bind_count[0])
         layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
   }
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   if (screen->driver_workarounds.general_layout)
      layout = VK_IMAGE_LAYOUT_GENERAL;
   /*
      The image subresources for a storage image must be in the VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR or
      VK_IMAGE_LAYOUT_GENERAL layout in order to access its data in a shader.
      - 14.1.1. Storage Image
    */
   else if (res->image_bind_count[0])
      layout = VK_IMAGE_LAYOUT_GENERAL;
   else if (!screen->info.have_EXT_attachment_feedback_loop_layout &&
            layout == VK_IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT)
      layout = VK_IMAGE_LAYOUT_GENERAL;
   /* some drivers don't care about zs layouts for attachments, so this saves some layout transition cycles */
   else if (layout != VK_IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT &&
            i >= ctx->fb_state.nr_cbufs && screen->driver_workarounds.general_depth_layout)
      layout = VK_IMAGE_LAYOUT_GENERAL;
   screen->image_barrier(ctx, res, layout, access, pipeline);
   if (!(res->aspect & VK_IMAGE_ASPECT_COLOR_BIT))
      ctx->zsbuf_readonly = res->layout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL;
   res->obj->unordered_read = res->obj->unordered_write = false;
   if (!screen->driver_workarounds.general_layout && i == ctx->fb_state.nr_cbufs && res->sampler_bind_count[0])
      update_res_sampler_layouts(ctx, res);
   zink_batch_resource_usage_set(ctx->bs, res, true, false);
   return true;
}

static unsigned
begin_rendering(struct zink_context *ctx, bool check_msaa_expand)
{
   unsigned clear_buffers = 0;
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   zink_update_vk_sample_locations(ctx);
   if (ctx->has_swapchain)
      zink_render_fixup_swapchain(ctx);
   bool has_depth = false;
   bool has_stencil = false;
   bool changed_layout = false;
   bool changed_size = false;
   bool zsbuf_used = zink_is_zsbuf_used(ctx);
   bool has_msrtss = screen->info.have_EXT_multisampled_render_to_single_sampled;
   bool use_tc_info = !ctx->blitting && ctx->track_renderpasses;
   uint32_t msaa_expand_mask = 0;
   /* j/k this is super nonconformant */
   bool very_legal_and_conformant_msaa_opt = ctx->dynamic_fb.tc_info.has_resolve && ctx->dynamic_fb.tc_info.ended && (zink_debug & ZINK_DEBUG_MSAAOPT);
   ctx->dynamic_fb.attachments[0].pNext = NULL;
   if (ctx->rp_changed || ctx->rp_layout_changed || (!ctx->in_rp && ctx->rp_loadop_changed)) {
      /* init imageviews, base loadOp, formats */
      for (int i = 0; i < ctx->fb_state.nr_cbufs; i++) {
         struct zink_resource *res = zink_resource(ctx->fb_state.cbufs[i].texture);
         if (!res)
            continue;

         if (!res->valid || very_legal_and_conformant_msaa_opt)
            ctx->dynamic_fb.attachments[i].loadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
         else
            ctx->dynamic_fb.attachments[i].loadOp = VK_ATTACHMENT_LOAD_OP_LOAD;
         if (use_tc_info) {
            if (very_legal_and_conformant_msaa_opt || ctx->dynamic_fb.tc_info.cbuf_invalidate & BITFIELD_BIT(i))
               ctx->dynamic_fb.attachments[i].storeOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
            else
               ctx->dynamic_fb.attachments[i].storeOp = VK_ATTACHMENT_STORE_OP_STORE;
         }
         if (ctx->dynamic_fb.attachments[i].loadOp == VK_ATTACHMENT_LOAD_OP_LOAD)
            msaa_expand_mask |= BITFIELD_BIT(i);
      }

      /* unset depth and stencil info: reset below */
      VkImageLayout zlayout = ctx->dynamic_fb.info.pDepthAttachment ? ctx->dynamic_fb.info.pDepthAttachment->imageLayout : VK_IMAGE_LAYOUT_UNDEFINED;
      VkImageLayout slayout = ctx->dynamic_fb.info.pStencilAttachment ? ctx->dynamic_fb.info.pStencilAttachment->imageLayout : VK_IMAGE_LAYOUT_UNDEFINED;
      ctx->dynamic_fb.info.pDepthAttachment = NULL;
      ctx->dynamic_fb.info.pStencilAttachment = NULL;

      if (ctx->fb_state.zsbuf.texture && zsbuf_used) {
         struct zink_resource *res = zink_resource(ctx->fb_state.zsbuf.texture);
         has_depth = util_format_has_depth(util_format_description(ctx->fb_state.zsbuf.format));
         has_stencil = util_format_has_stencil(util_format_description(ctx->fb_state.zsbuf.format));

         /* depth may or may not be used but init it anyway */
         if (res->valid && !very_legal_and_conformant_msaa_opt)
            ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].loadOp = VK_ATTACHMENT_LOAD_OP_LOAD;
         else
            ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].loadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;

         if (use_tc_info) {
            if ((very_legal_and_conformant_msaa_opt && !ctx->fb_state.nr_cbufs) || ctx->dynamic_fb.tc_info.zsbuf_invalidate)
               ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].storeOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
            else
               ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].storeOp = VK_ATTACHMENT_STORE_OP_STORE;
         }

         /* maybe TODO but also not handled by legacy rp...
         if (ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].loadOp == VK_ATTACHMENT_LOAD_OP_LOAD)
            msaa_expand_mask |= BITFIELD_BIT(PIPE_MAX_COLOR_BUFS);
         */
         /* stencil may or may not be used but init it anyway */
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS+1].loadOp = ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].loadOp;
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS+1].storeOp = ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].storeOp;

         if (has_depth) {
            ctx->dynamic_fb.info.pDepthAttachment = &ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS];
            /* stencil info only set for clears below */
         }
         if (has_stencil) {
            /* must be stencil-only */
            ctx->dynamic_fb.info.pStencilAttachment = &ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS + 1];
         }
      } else {
         ctx->dynamic_fb.info.pDepthAttachment = NULL;
      }
      if (zlayout != (ctx->dynamic_fb.info.pDepthAttachment ? ctx->dynamic_fb.info.pDepthAttachment->imageLayout : VK_IMAGE_LAYOUT_UNDEFINED))
         changed_layout = true;
      if (slayout != (ctx->dynamic_fb.info.pStencilAttachment ? ctx->dynamic_fb.info.pStencilAttachment->imageLayout : VK_IMAGE_LAYOUT_UNDEFINED))
         changed_layout = true;

      /* similar to begin_render_pass(), but just filling in VkRenderingInfo */
      for (int i = 0; i < ctx->fb_state.nr_cbufs; i++) {
         /* these are no-ops */
         if (!ctx->fb_state.cbufs[i].texture || !zink_fb_clear_enabled(ctx, i))
            continue;
         /* these need actual clear calls inside the rp */
         struct zink_framebuffer_clear_data *clear = zink_fb_clear_element(&ctx->fb_clears[i], 0);
         if (zink_fb_clear_needs_explicit(&ctx->fb_clears[i])) {
            clear_buffers |= (PIPE_CLEAR_COLOR0 << i);
            if (zink_fb_clear_count(&ctx->fb_clears[i]) < 2 ||
                zink_fb_clear_element_needs_explicit(clear))
               continue;
         }
         /* we now know there's one clear that can be done here */
         memcpy(&ctx->dynamic_fb.attachments[i].clearValue, &clear->color, sizeof(float) * 4);
         ctx->dynamic_fb.attachments[i].loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
      }
      if (ctx->fb_state.zsbuf.texture && zink_fb_clear_enabled(ctx, PIPE_MAX_COLOR_BUFS)) {
         struct zink_framebuffer_clear *fb_clear = &ctx->fb_clears[PIPE_MAX_COLOR_BUFS];
         struct zink_framebuffer_clear_data *clear = zink_fb_clear_element(fb_clear, 0);
         if (!zink_fb_clear_element_needs_explicit(clear)) {
            /* base zs clear info */
            ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].clearValue.depthStencil.depth = clear->zs.depth;
            ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].clearValue.depthStencil.stencil = clear->zs.stencil;
            /* always init separate stencil attachment */
            ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS+1].clearValue.depthStencil.stencil = clear->zs.stencil;
            if ((zink_fb_clear_element(fb_clear, 0)->zs.bits & PIPE_CLEAR_DEPTH))
               /* initiate a depth clear */
               ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
            if ((zink_fb_clear_element(fb_clear, 0)->zs.bits & PIPE_CLEAR_STENCIL)) {
               /* use a stencil clear, also set stencil attachment */
               ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS+1].loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
            }
         }
      }
      if (changed_size || changed_layout)
         ctx->rp_changed = true;
      ctx->rp_loadop_changed = false;
      ctx->rp_layout_changed = false;
   }
   msaa_expand_mask &= ctx->transient_attachments;
   if (!has_msrtss && msaa_expand_mask && check_msaa_expand) {
      zink_render_msaa_expand(ctx, msaa_expand_mask);
      return begin_rendering(ctx, false);
   }
   /* always assemble clear_buffers mask:
    * if a scissored clear must be triggered during glFlush,
    * the renderpass metadata may be unchanged (e.g., LOAD from previous rp),
    * but the buffer mask must still be returned
    */
   if (ctx->clears_enabled) {
      for (int i = 0; i < ctx->fb_state.nr_cbufs; i++) {
         /* these are no-ops */
         if (!ctx->fb_state.cbufs[i].texture || !zink_fb_clear_enabled(ctx, i))
            continue;
         /* these need actual clear calls inside the rp */
         if (zink_fb_clear_needs_explicit(&ctx->fb_clears[i]))
            clear_buffers |= (PIPE_CLEAR_COLOR0 << i);
      }
      if (ctx->fb_state.zsbuf.texture && zink_fb_clear_enabled(ctx, PIPE_MAX_COLOR_BUFS)) {
         struct zink_framebuffer_clear *fb_clear = &ctx->fb_clears[PIPE_MAX_COLOR_BUFS];
         struct zink_framebuffer_clear_data *clear = zink_fb_clear_element(fb_clear, 0);
         if (zink_fb_clear_needs_explicit(fb_clear)) {
            for (int j = !zink_fb_clear_element_needs_explicit(clear);
                 (clear_buffers & PIPE_CLEAR_DEPTHSTENCIL) != PIPE_CLEAR_DEPTHSTENCIL && j < zink_fb_clear_count(fb_clear);
                 j++)
               clear_buffers |= zink_fb_clear_element(fb_clear, j)->zs.bits;
         }
      }
   }

   if (!ctx->rp_changed && ctx->in_rp)
      return 0;
   ctx->rp_changed = false;

   /* update pipeline info id for compatibility VUs */
   unsigned rp_state = zink_update_rendering_info(ctx);
   /* validate zs VUs: attachment must be null or format must be valid */
   assert(!ctx->dynamic_fb.info.pDepthAttachment || ctx->gfx_pipeline_state.rendering_info.depthAttachmentFormat);
   assert(!ctx->dynamic_fb.info.pStencilAttachment || ctx->gfx_pipeline_state.rendering_info.stencilAttachmentFormat);
   bool rp_changed = ctx->gfx_pipeline_state.rp_state != rp_state;

   if (!rp_changed && ctx->in_rp)
      return 0;

   zink_batch_no_rp(ctx);
   for (int i = 0; i < ctx->fb_state.nr_cbufs; i++) {
      VkImageView iv = VK_NULL_HANDLE;
      struct zink_resource *res = zink_resource(ctx->fb_state.cbufs[i].texture);
      if (res) {
         /* swapchain acquire can fail */
         if (prep_fb_attachment(ctx, res, i))
            /* swapchain acquire can change this surface */
            iv = zink_create_fb_surface(&ctx->base, &ctx->fb_state.cbufs[i])->image_view;
         if (ctx->fb_state.cbufs[i].nr_samples && !has_msrtss) {
            ctx->dynamic_fb.attachments[i].resolveMode = VK_RESOLVE_MODE_AVERAGE_BIT;
            ctx->dynamic_fb.attachments[i].resolveImageView = iv;
            ctx->dynamic_fb.attachments[i].resolveImageLayout = res->layout;

            iv = zink_create_transient_surface(ctx, &ctx->fb_state.cbufs[i], ctx->fb_state.cbufs[i].nr_samples)->image_view;
            struct zink_resource *transient_res = res->transient;
            prep_fb_attachment(ctx, transient_res, i);
            ctx->dynamic_fb.attachments[i].imageLayout = transient_res->layout;
         } else {
            ctx->dynamic_fb.attachments[i].imageLayout = res->layout;
            ctx->dynamic_fb.attachments[i].resolveMode = VK_RESOLVE_MODE_NONE;
            ctx->dynamic_fb.attachments[i].resolveImageView = VK_NULL_HANDLE;
            ctx->dynamic_fb.attachments[i].resolveImageLayout = VK_IMAGE_LAYOUT_UNDEFINED;
         }
         if (!iv)
            /* dead swapchain */
            return 0;

      } else {
         ctx->dynamic_fb.attachments[i].resolveMode = VK_RESOLVE_MODE_NONE;
      }
      ctx->dynamic_fb.attachments[i].imageView = iv;
   }
   if (ctx->has_swapchain) {
      ASSERTED struct zink_resource *res = zink_resource(ctx->fb_state.cbufs[0].texture);
      zink_render_fixup_swapchain(ctx);
      if (res->use_damage) {
         ctx->dynamic_fb.info.renderArea = res->damage;
      } else {
         ctx->dynamic_fb.info.renderArea.offset.x = 0;
         ctx->dynamic_fb.info.renderArea.offset.y = 0;
         ctx->dynamic_fb.info.renderArea.extent.width = ctx->fb_state.width;
         ctx->dynamic_fb.info.renderArea.extent.height = ctx->fb_state.height;
      }
      /* clamp for late swapchain resize */
      if (res->base.b.width0 < ctx->dynamic_fb.info.renderArea.extent.width)
         ctx->dynamic_fb.info.renderArea.extent.width = res->base.b.width0;
      if (res->base.b.height0 < ctx->dynamic_fb.info.renderArea.extent.height)
         ctx->dynamic_fb.info.renderArea.extent.height = res->base.b.height0;
   }
   if (ctx->fb_state.zsbuf.texture && zsbuf_used) {
      struct zink_resource *res = zink_resource(ctx->fb_state.zsbuf.texture);
      prep_fb_attachment(ctx, res, ctx->fb_state.nr_cbufs);
      VkImageView iv = zink_create_fb_surface(&ctx->base, &ctx->fb_state.zsbuf)->image_view;
      if (ctx->fb_state.zsbuf.nr_samples && !has_msrtss) {
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].resolveImageView = iv;
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].resolveImageLayout = res->layout;

         iv = zink_create_transient_surface(ctx, &ctx->fb_state.zsbuf, ctx->fb_state.zsbuf.nr_samples)->image_view;
         struct zink_resource *transient_res = res->transient;
         prep_fb_attachment(ctx, transient_res, ctx->fb_state.nr_cbufs);
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].imageLayout = transient_res->layout;
      } else {
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].imageLayout = res->layout;
      }
      ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].imageView = iv;

      assert(ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].imageLayout != VK_IMAGE_LAYOUT_UNDEFINED);
      ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS+1].imageView = iv;
      ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS+1].imageLayout = res->layout;
      assert(ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS+1].imageLayout != VK_IMAGE_LAYOUT_UNDEFINED);
      if (ctx->transient_attachments & BITFIELD_BIT(PIPE_MAX_COLOR_BUFS)) {
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].resolveMode = VK_RESOLVE_MODE_SAMPLE_ZERO_BIT;
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS + 1].resolveMode = VK_RESOLVE_MODE_SAMPLE_ZERO_BIT;
      } else {
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].resolveMode = 0;
         ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS + 1].resolveMode = 0;
      }
   }
   if (use_tc_info && ctx->dynamic_fb.tc_info.has_resolve) {
      struct zink_resource *res = zink_resource(ctx->fb_state.resolve);
      if (!res)
         res = zink_resource(ctx->dynamic_fb.tc_info.resolve);
      assert(res);
      zink_batch_resource_usage_set(ctx->bs, res, true, false);
      bool is_depth = util_format_is_depth_or_stencil(res->base.b.format);
      enum pipe_format format = res->base.b.format;
      if (!ctx->fb_state.resolve)
         format = is_depth ? ctx->fb_state.zsbuf.format : ctx->fb_state.cbufs[0].format;
      if (zink_format_needs_mutable(res->base.b.format, format))
         /* mutable not set by default */
         zink_resource_object_init_mutable(ctx, res);
      struct pipe_surface tmpl = {
         .format = format,
         .texture = &res->base.b
      };
      if (zink_is_swapchain(res)) {
         if (!zink_kopper_acquire(ctx, res, UINT64_MAX))
            return 0;
      }
      VkImageViewCreateInfo ivci = create_ivci(screen, res, &tmpl, ctx->dynamic_fb.info.layerCount > 1 ? PIPE_TEXTURE_2D_ARRAY : PIPE_TEXTURE_2D);
      struct zink_surface *surf = zink_get_surface(ctx, &tmpl, &ivci);
      VkImageLayout layout = is_depth ? VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL : VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
      if (screen->driver_workarounds.general_layout)
         layout = VK_IMAGE_LAYOUT_GENERAL;
      unsigned idx = util_format_is_depth_or_stencil(res->base.b.format) ? PIPE_MAX_COLOR_BUFS : 0;
      screen->image_barrier(ctx, res, layout, VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);
      res->obj->unordered_read = res->obj->unordered_write = false;
      ctx->dynamic_fb.attachments[idx].resolveMode = VK_RESOLVE_MODE_AVERAGE_BIT;
      ctx->dynamic_fb.attachments[idx].resolveImageLayout = res->layout;
      ctx->dynamic_fb.attachments[idx].resolveImageView = surf->image_view;
      if (idx == PIPE_MAX_COLOR_BUFS) {
         ctx->dynamic_fb.attachments[idx + 1].resolveMode = VK_RESOLVE_MODE_AVERAGE_BIT;
         ctx->dynamic_fb.attachments[idx + 1].resolveImageLayout = res->layout;
         ctx->dynamic_fb.attachments[idx + 1].resolveImageView = surf->image_view;
      }
   }
   ctx->zsbuf_unused = !zsbuf_used;
   assert(ctx->fb_state.width >= ctx->dynamic_fb.info.renderArea.extent.width);
   assert(ctx->fb_state.height >= ctx->dynamic_fb.info.renderArea.extent.height);
   ctx->gfx_pipeline_state.dirty |= rp_changed;
   ctx->gfx_pipeline_state.rp_state = rp_state;

   VkMultisampledRenderToSingleSampledInfoEXT msrtss = {
      VK_STRUCTURE_TYPE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_INFO_EXT,
      NULL,
      VK_TRUE,
      ctx->gfx_pipeline_state.rast_samples + 1,
   };

   if (has_msrtss)
      ctx->dynamic_fb.info.pNext = ctx->transient_attachments ? &msrtss : NULL;
   VKCTX(CmdBeginRendering)(ctx->bs->cmdbuf, &ctx->dynamic_fb.info);
   ctx->in_rp = true;
   return clear_buffers;
}

/* same as u_framebuffer_get_num_layers, but clamp to lowest layer count */
static unsigned
framebuffer_get_num_layers(const struct pipe_framebuffer_state *fb)
{
   unsigned i, num_layers = UINT32_MAX;
   if (!(fb->nr_cbufs || fb->zsbuf.texture))
      return MAX2(fb->layers, 1);

   for (i = 0; i < fb->nr_cbufs; i++) {
      if (fb->cbufs[i].texture) {
         unsigned num = fb->cbufs[i].last_layer -
         fb->cbufs[i].first_layer + 1;
         num_layers = MIN2(num_layers, num);
      }
   }
   if (fb->zsbuf.texture) {
      unsigned num = fb->zsbuf.last_layer -
      fb->zsbuf.first_layer + 1;
      num_layers = MIN2(num_layers, num);
   }
   return MAX2(num_layers, 1);
}

ALWAYS_INLINE static void
update_layered_rendering_state(struct zink_context *ctx)
{
   if (!zink_screen(ctx->base.screen)->driver_compiler_workarounds.needs_sanitised_layer)
      return;
   unsigned framebuffer_is_layered = framebuffer_get_num_layers(&ctx->fb_state) > 1;
   VKCTX(CmdPushConstants)(
         ctx->bs->cmdbuf,
         zink_screen(ctx->base.screen)->gfx_push_constant_layout,
         VK_SHADER_STAGE_ALL_GRAPHICS,
         offsetof(struct zink_gfx_push_constant, framebuffer_is_layered), sizeof(unsigned),
         &framebuffer_is_layered);
}

ALWAYS_INLINE static void
batch_ref_fb_surface(struct zink_context *ctx, const struct pipe_surface *psurf)
{
   if (!psurf->texture)
      return;
   struct zink_resource *res = zink_resource(psurf->texture);
   zink_batch_reference_resource(ctx, res);
   if (res->transient)
      zink_batch_reference_resource(ctx, res->transient);
}

void
zink_batch_rp(struct zink_context *ctx)
{
   assert(!(ctx->in_rp && ctx->rp_changed));
   if (!ctx->track_renderpasses && !ctx->blitting) {
      if (ctx->rp_tc_info_updated)
         zink_parse_tc_info(ctx);
   }
   if (ctx->in_rp && !ctx->rp_layout_changed)
      return;
   bool in_rp = ctx->in_rp;
   if (!in_rp && ctx->void_clears) {
      union pipe_color_union color;
      color.f[0] = color.f[1] = color.f[2] = 0;
      color.f[3] = 1.0;
      ctx->base.clear(&ctx->base, ctx->void_clears, NULL, &color, 0, 0);
      ctx->void_clears = 0;
   }
   if (!ctx->blitting) {
      if (ctx->rp_tc_info_updated)
         update_tc_info(ctx);
      ctx->rp_tc_info_updated = false;
   }
   bool maybe_has_query_ends = !ctx->track_renderpasses || ctx->dynamic_fb.tc_info.has_query_ends;
   ctx->queries_in_rp = maybe_has_query_ends;
   /* if possible, out-of-renderpass resume any queries that were stopped when previous rp ended */
   if (!ctx->queries_disabled && !maybe_has_query_ends) {
      zink_resume_queries(ctx);
      zink_query_update_gs_states(ctx);
   }
   unsigned clear_buffers = begin_rendering(ctx, true);
   assert(!ctx->rp_changed);
   if (ctx->unordered_blitting)
      ctx->bs->has_reordered_work = true;
   else
      ctx->bs->has_work = true;

   /* update the render-passes HUD query */
   ctx->hud.render_passes++;

   if (!in_rp && ctx->in_rp) {
      /* only hit this for valid swapchain and new renderpass */
      if (ctx->render_condition.query)
         zink_start_conditional_render(ctx);
      zink_clear_framebuffer(ctx, clear_buffers);
   }
   /* unable to previously determine that queries didn't split renderpasses: ensure queries start inside renderpass */
   if (!ctx->queries_disabled && maybe_has_query_ends) {
      zink_resume_queries(ctx);
      zink_query_update_gs_states(ctx);
   }
}

void
zink_batch_no_rp_safe(struct zink_context *ctx)
{
   if (!ctx->in_rp)
      return;
   if (ctx->render_condition.query)
      zink_stop_conditional_render(ctx);
   /* suspend all queries that were started in a renderpass
    * they can then be resumed upon beginning a new renderpass
    */
   if (!ctx->queries_disabled)
      zink_query_renderpass_suspend(ctx);
   VKCTX(CmdEndRendering)(ctx->bs->cmdbuf);
   ctx->in_rp = false;
}

void
zink_batch_no_rp(struct zink_context *ctx)
{
   if (!ctx->in_rp)
      return;
   if (ctx->track_renderpasses && !ctx->blitting)
      tc_renderpass_info_reset(&ctx->dynamic_fb.tc_info);
   zink_batch_no_rp_safe(ctx);
}

static uint32_t
hash_rendering_state(const void *key)
{
   const VkPipelineRenderingCreateInfo *info = key;
   uint32_t hash = 0;
   /*
    uint32_t           viewMask;
    uint32_t           colorAttachmentCount;
    const VkFormat*    pColorAttachmentFormats;
    VkFormat           depthAttachmentFormat;
    VkFormat           stencilAttachmentFormat;
    * this data is not optimally arranged, so it must be manually hashed
    */
   hash = XXH32(&info->colorAttachmentCount, sizeof(uint32_t), hash);
   hash = XXH32(&info->viewMask, sizeof(uint32_t), hash);
   hash = XXH32(&info->depthAttachmentFormat, sizeof(uint32_t), hash);
   hash = XXH32(&info->stencilAttachmentFormat, sizeof(VkFormat), hash);
   return XXH32(info->pColorAttachmentFormats, sizeof(VkFormat) * info->colorAttachmentCount, hash);
}

static bool
equals_rendering_state(const void *a, const void *b)
{
   const VkPipelineRenderingCreateInfo *ai = a;
   const VkPipelineRenderingCreateInfo *bi = b;
   return ai->colorAttachmentCount == bi->colorAttachmentCount &&
          ai->depthAttachmentFormat == bi->depthAttachmentFormat &&
          ai->viewMask == bi->viewMask &&
          ai->stencilAttachmentFormat == bi->stencilAttachmentFormat &&
          !memcmp(ai->pColorAttachmentFormats, bi->pColorAttachmentFormats, sizeof(VkFormat) * ai->colorAttachmentCount);
}

void
zink_init_vk_sample_locations(struct zink_context *ctx, VkSampleLocationsInfoEXT *loc)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   unsigned idx = util_logbase2_ceil(MAX2(ctx->gfx_pipeline_state.rast_samples + 1, 1));
   loc->sType = VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT;
   loc->pNext = NULL;
   loc->sampleLocationsPerPixel = 1 << idx;
   loc->sampleLocationsCount = ctx->gfx_pipeline_state.rast_samples + 1;
   loc->sampleLocationGridSize = screen->maxSampleLocationGridSize[idx];
   loc->pSampleLocations = ctx->vk_sample_locations;
}

static void
zink_evaluate_depth_buffer(struct pipe_context *pctx)
{
   struct zink_context *ctx = zink_context(pctx);

   if (!ctx->fb_state.zsbuf.texture)
      return;

   struct zink_resource *res = zink_resource(ctx->fb_state.zsbuf.texture);
   res->obj->needs_zs_evaluate = true;
   zink_init_vk_sample_locations(ctx, &res->obj->zs_evaluate);
   zink_batch_no_rp(ctx);
}

static void
sync_flush(struct zink_context *ctx, struct zink_batch_state *bs)
{
   if (zink_screen(ctx->base.screen)->threaded_submit)
      util_queue_fence_wait(&bs->flush_completed);
}

static inline VkAccessFlags
get_access_flags_for_binding(struct zink_context *ctx, enum zink_descriptor_type type, gl_shader_stage stage, unsigned idx)
{
   VkAccessFlags flags = 0;
   switch (type) {
   case ZINK_DESCRIPTOR_TYPE_UBO:
      return VK_ACCESS_UNIFORM_READ_BIT;
   case ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW:
      return VK_ACCESS_SHADER_READ_BIT;
   case ZINK_DESCRIPTOR_TYPE_SSBO: {
      flags = VK_ACCESS_SHADER_READ_BIT;
      if (ctx->writable_ssbos[stage] & (1 << idx))
         flags |= VK_ACCESS_SHADER_WRITE_BIT;
      return flags;
   }
   case ZINK_DESCRIPTOR_TYPE_IMAGE: {
      struct zink_image_view *image_view = &ctx->image_views[stage][idx];
      if (image_view->base.access & PIPE_IMAGE_ACCESS_READ)
         flags |= VK_ACCESS_SHADER_READ_BIT;
      if (image_view->base.access & PIPE_IMAGE_ACCESS_WRITE)
         flags |= VK_ACCESS_SHADER_WRITE_BIT;
      return flags;
   }
   default:
      break;
   }
   UNREACHABLE("ACK");
   return 0;
}

static void
update_resource_refs_for_stage(struct zink_context *ctx, gl_shader_stage stage)
{
   unsigned max_slot[] = {
      [ZINK_DESCRIPTOR_TYPE_UBO] = ctx->di.num_ubos[stage],
      [ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW] = ctx->di.num_samplers[stage],
      [ZINK_DESCRIPTOR_TYPE_SSBO] = ctx->di.num_ssbos[stage],
      [ZINK_DESCRIPTOR_TYPE_IMAGE] = ctx->di.num_images[stage]
   };
   for (unsigned i = 0; i < ZINK_DESCRIPTOR_BASE_TYPES; i++) {
      for (unsigned j = 0; j < max_slot[i]; j++) {
         if (ctx->di.descriptor_res[i][stage][j]) {
            struct zink_resource *res = ctx->di.descriptor_res[i][stage][j];
            if (!res)
               continue;
            bool is_buffer = res->obj->is_buffer;
            bool is_write = zink_resource_access_is_write(get_access_flags_for_binding(ctx, i, stage, j));
            if (zink_is_swapchain(res)) {
               if (!zink_kopper_acquire(ctx, res, UINT64_MAX))
                  /* technically this is a failure condition, but there's no safe way out */
                  continue;
            }
            zink_batch_resource_usage_set(ctx->bs, res, is_write, is_buffer);
            if (!ctx->unordered_blitting) {
               if (is_write || !res->obj->is_buffer)
                  res->obj->unordered_read = res->obj->unordered_write = false;
               else
                  res->obj->unordered_read = false;
            }
         }
      }
   }
}

void
zink_update_descriptor_refs(struct zink_context *ctx, bool compute)
{
   if (compute) {
      update_resource_refs_for_stage(ctx, MESA_SHADER_COMPUTE);
      if (ctx->curr_compute)
         zink_batch_reference_program(ctx, &ctx->curr_compute->base);
   } else {
      for (unsigned i = 0; i < ZINK_GFX_SHADER_COUNT; i++)
         update_resource_refs_for_stage(ctx, i);
      unsigned vertex_buffers_enabled_mask = ctx->gfx_pipeline_state.vertex_buffers_enabled_mask;
      unsigned last_vbo = util_last_bit(vertex_buffers_enabled_mask);
      for (unsigned i = 0; i < last_vbo + 1; i++) {
         struct zink_resource *res = zink_resource(ctx->vertex_buffers[i].buffer.resource);
         if (res) {
            zink_batch_resource_usage_set(ctx->bs, res, false, true);
            if (!ctx->unordered_blitting)
               res->obj->unordered_read = false;
         }
      }
      if (ctx->curr_program)
         zink_batch_reference_program(ctx, &ctx->curr_program->base);
   }
   if (ctx->di.bindless_refs_dirty) {
      ctx->di.bindless_refs_dirty = false;
      for (unsigned i = 0; i < 2; i++) {
         util_dynarray_foreach(&ctx->di.bindless[i].resident, struct zink_bindless_descriptor*, bd) {
            struct zink_resource *res = zink_resource((*bd)->pres);
            zink_batch_resource_usage_set(ctx->bs, res, (*bd)->access & PIPE_IMAGE_ACCESS_WRITE, res->obj->is_buffer);
            if (!ctx->unordered_blitting) {
               if ((*bd)->access & PIPE_IMAGE_ACCESS_WRITE || !res->obj->is_buffer)
                  res->obj->unordered_read = res->obj->unordered_write = false;
               else
                  res->obj->unordered_read = false;
            }
         }
      }
   }

   unsigned global_count = util_dynarray_num_elements(&ctx->di.global_bindings, struct zink_resource*);
   struct zink_resource **globals = ctx->di.global_bindings.data;
   for (unsigned i = 0; i < global_count; i++) {
      struct zink_resource *res = globals[i];
      if (!res)
         continue;
      zink_batch_resource_usage_set(ctx->bs, res, true, true);
      res->obj->unordered_read = res->obj->unordered_write = false;
   }
}

static void
reapply_color_write(struct zink_context *ctx)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   assert(screen->info.have_EXT_color_write_enable);
   const VkBool32 enables[PIPE_MAX_COLOR_BUFS] = {1, 1, 1, 1, 1, 1, 1, 1};
   const VkBool32 disables[PIPE_MAX_COLOR_BUFS] = {0};
   const unsigned max_att = MIN2(PIPE_MAX_COLOR_BUFS, screen->info.props.limits.maxColorAttachments);
   VKCTX(CmdSetColorWriteEnableEXT)(ctx->bs->cmdbuf, max_att, ctx->disable_color_writes ? disables : enables);
   VKCTX(CmdSetColorWriteEnableEXT)(ctx->bs->reordered_cmdbuf, max_att, enables);
   assert(screen->info.have_EXT_extended_dynamic_state);
   if (ctx->dsa_state)
      VKCTX(CmdSetDepthWriteEnable)(ctx->bs->cmdbuf, ctx->disable_color_writes ? VK_FALSE : ctx->dsa_state->hw_state.depth_write);
}

static void
stall(struct zink_context *ctx)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   sync_flush(ctx, ctx->last_batch_state);
   zink_screen_timeline_wait(screen, ctx->last_batch_state->fence.batch_id, OS_TIMEOUT_INFINITE);
}

void
zink_reset_ds3_states(struct zink_context *ctx)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   if (!screen->info.have_EXT_extended_dynamic_state3)
      return;
   if (screen->have_full_ds3)
      ctx->ds3_states = UINT32_MAX;
   else
      ctx->ds3_states = BITFIELD_MASK(ZINK_DS3_BLEND_A2C);
   if (!screen->info.dynamic_state3_feats.extendedDynamicState3AlphaToOneEnable)
      ctx->ds3_states &= ~BITFIELD_BIT(ZINK_DS3_BLEND_A21);
   if (!screen->info.dynamic_state3_feats.extendedDynamicState3LineStippleEnable)
      ctx->ds3_states &= ~BITFIELD_BIT(ZINK_DS3_RAST_STIPPLE_ON);
   if (screen->driver_workarounds.no_linestipple)
      ctx->ds3_states &= ~BITFIELD_BIT(ZINK_DS3_RAST_STIPPLE);
}

static void
flush_batch(struct zink_context *ctx, bool sync)
{
   assert(!ctx->unordered_blitting);
   if (ctx->clears_enabled)
      /* start rp to do all the clears */
      zink_batch_rp(ctx);
   zink_batch_no_rp_safe(ctx);

   util_queue_fence_wait(&ctx->unsync_fence);
   util_queue_fence_reset(&ctx->flush_fence);
   zink_end_batch(ctx);
   ctx->deferred_fence = NULL;

   if (sync)
      sync_flush(ctx, ctx->bs);

   if (ctx->bs->is_device_lost) {
      check_device_lost(ctx);
   } else {
      struct zink_screen *screen = zink_screen(ctx->base.screen);
      zink_start_batch(ctx);
      if (screen->info.have_EXT_transform_feedback && ctx->num_so_targets)
         ctx->dirty_so_targets = true;
      ctx->pipeline_changed[0] = ctx->pipeline_changed[1] = true;
      zink_select_draw_vbo(ctx);
      zink_select_launch_grid(ctx);

      if (ctx->oom_stall)
         stall(ctx);
      zink_reset_ds3_states(ctx);

      ctx->oom_flush = false;
      ctx->oom_stall = false;
      ctx->dd.bindless_bound = false;
      ctx->di.bindless_refs_dirty = true;
      ctx->sample_locations_changed = ctx->gfx_pipeline_state.sample_locations_enabled;
      if (zink_screen(ctx->base.screen)->info.dynamic_state2_feats.extendedDynamicState2PatchControlPoints) {
         VKCTX(CmdSetPatchControlPointsEXT)(ctx->bs->cmdbuf, ctx->gfx_pipeline_state.dyn_state2.vertices_per_patch);
         VKCTX(CmdSetPatchControlPointsEXT)(ctx->bs->reordered_cmdbuf, 1);
      }
      update_feedback_loop_dynamic_state(ctx);
      if (screen->info.have_EXT_color_write_enable)
         reapply_color_write(ctx);
      update_layered_rendering_state(ctx);
      tc_renderpass_info_reset(&ctx->dynamic_fb.tc_info);
      ctx->rp_tc_info_updated = true;
   }
   util_queue_fence_signal(&ctx->flush_fence);
}

void
zink_flush_queue(struct zink_context *ctx)
{
   flush_batch(ctx, true);
}

static void
pre_sync_transfer_barrier(struct zink_context *ctx, struct zink_resource *res, bool unsync)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   VkImageLayout layout = screen->driver_workarounds.general_layout ? VK_IMAGE_LAYOUT_GENERAL :
                           /* assume that all depth buffers which are not swapchain images will be used for sampling to avoid splitting renderpasses */
                           !(res->base.b.bind & PIPE_BIND_DISPLAY_TARGET) && util_format_is_depth_or_stencil(res->base.b.format) ?
                           VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL :
                           /* assume that all color buffers which are not swapchain images will be used for sampling to avoid splitting renderpasses */
                           VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
   if (unsync)
      screen->image_barrier_unsync(ctx, res, layout, VK_ACCESS_SHADER_READ_BIT, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT);
   else
      screen->image_barrier(ctx, res, layout, VK_ACCESS_SHADER_READ_BIT, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT);
}

static void
unbind_fb_surface(struct zink_context *ctx, const struct pipe_surface *surf, unsigned idx, bool changed)
{
   bool general_layout = zink_screen(ctx->base.screen)->driver_workarounds.general_layout;
   if (!surf->texture)
      return;
   ctx->dynamic_fb.attachments[idx].imageView = VK_NULL_HANDLE;
   if (!surf)
      return;
   struct zink_resource *res = zink_resource(surf->texture);
   if (changed) {
      ctx->rp_changed = true;
      if (surf->nr_samples && res->transient)
         res->transient->valid = false;
   }
   res->fb_bind_count--;
   if (!res->fb_bind_count && !res->bind_count[0])
      _mesa_set_remove_key(ctx->need_barriers[0], res);
   unsigned feedback_loops = ctx->feedback_loops;
   if (!general_layout && ctx->feedback_loops & BITFIELD_BIT(idx)) {
      ctx->dynamic_fb.attachments[idx].imageLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
      ctx->rp_layout_changed = true;
   }
   ctx->feedback_loops &= ~BITFIELD_BIT(idx);
   if (feedback_loops != ctx->feedback_loops) {
      if (idx == PIPE_MAX_COLOR_BUFS && !zink_screen(ctx->base.screen)->driver_workarounds.always_feedback_loop_zs) {
         if (ctx->gfx_pipeline_state.feedback_loop_zs)
            ctx->gfx_pipeline_state.dirty = true;
         ctx->gfx_pipeline_state.feedback_loop_zs = false;
      } else if (idx < PIPE_MAX_COLOR_BUFS && !zink_screen(ctx->base.screen)->driver_workarounds.always_feedback_loop) {
         if (ctx->gfx_pipeline_state.feedback_loop)
            ctx->gfx_pipeline_state.dirty = true;
         ctx->gfx_pipeline_state.feedback_loop = false;
      }
      if (zink_screen(ctx->base.screen)->info.have_KHR_unified_image_layouts && zink_screen(ctx->base.screen)->info.have_EXT_attachment_feedback_loop_layout) {
         ctx->dynamic_fb.fbfetch_att[idx].feedbackLoopEnable = VK_FALSE;
         if (idx == PIPE_MAX_COLOR_BUFS)
            ctx->dynamic_fb.fbfetch_att[idx + 1].feedbackLoopEnable = VK_FALSE;
      }
   }
   res->fb_binds &= ~BITFIELD_BIT(idx);
   batch_ref_fb_surface(ctx, surf);
   /* this is called just before the resource loses a reference, so a refcount==1 means the resource will be destroyed */
   if (!res->fb_bind_count && res->base.b.reference.count > 1) {
      if (ctx->track_renderpasses && !ctx->blitting) {
         pre_sync_transfer_barrier(ctx, res, false);
      }
      if (!general_layout && res->sampler_bind_count[0]) {
         update_res_sampler_layouts(ctx, res);
         if (res->layout != VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL && !ctx->blitting)
            _mesa_set_add(ctx->need_barriers[0], res);
      }
   }
}

void
zink_set_null_fs(struct zink_context *ctx)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   bool prev_disable_fs = ctx->disable_fs;
   ctx->disable_fs = ctx->rast_state && ctx->rast_state->base.rasterizer_discard &&
                     (ctx->primitives_generated_active || (!ctx->queries_disabled && ctx->primitives_generated_suspended));
   struct zink_shader *zs = ctx->gfx_stages[MESA_SHADER_FRAGMENT];
   unsigned compact = screen->compact_descriptors ? ZINK_DESCRIPTOR_COMPACT : 0;
   /* can't use CWE if side effects */
   bool no_cwe = (zs && (zs->ssbos_used || zs->bindless || zs->num_bindings[ZINK_DESCRIPTOR_TYPE_IMAGE - compact])) ||
                 ctx->fs_query_active || ctx->occlusion_query_active || !screen->info.have_EXT_color_write_enable;
   bool prev_disable_color_writes = ctx->disable_color_writes;
   ctx->disable_color_writes = ctx->disable_fs && !no_cwe;

   if (ctx->disable_fs == prev_disable_fs) {
      /* if this is a true no-op then return */
      if (!ctx->disable_fs || ctx->disable_color_writes == !no_cwe)
         return;
      /* else changing disable modes */
   }

   /* either of these cases requires removing the previous mode */
   if (!ctx->disable_fs || (prev_disable_fs && prev_disable_color_writes != !no_cwe)) {
      if (prev_disable_color_writes)
         reapply_color_write(ctx);
      else
         ctx->base.bind_fs_state(&ctx->base, ctx->saved_fs);
      ctx->saved_fs = NULL;
      /* fs/CWE reenabled, fs active, done */
      if (!ctx->disable_fs)
         return;
   }

   /* always use CWE when possible */
   if (!no_cwe) {
      reapply_color_write(ctx);
      return;
   }
   /* otherwise need to bind a null fs */
   if (!ctx->null_fs) {
      nir_shader *nir = nir_builder_init_simple_shader(MESA_SHADER_FRAGMENT, &screen->nir_options, "null_fs").shader;
      nir->info.separate_shader = true;
      ctx->null_fs = pipe_shader_from_nir(&ctx->base, nir);
   }
   ctx->saved_fs = ctx->gfx_stages[MESA_SHADER_FRAGMENT];
   ctx->base.bind_fs_state(&ctx->base, ctx->null_fs);
}

static bool
framebuffer_surface_needs_mutable(const struct pipe_resource *pres, const struct pipe_surface *templ)
{
   const struct zink_resource *res = (const struct zink_resource*)pres;
   if (!res->obj->dt && zink_format_needs_mutable(pres->format, templ->format))
      /* mutable not set by default */
      return !(res->base.b.bind & ZINK_BIND_MUTABLE);
   return false;
}

static void
zink_set_framebuffer_state(struct pipe_context *pctx,
                           const struct pipe_framebuffer_state *state)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_screen *screen = zink_screen(pctx->screen);
   unsigned samples = state->nr_cbufs || state->zsbuf.texture ? 0 : state->samples;
   unsigned w = ctx->fb_state.width;
   unsigned h = ctx->fb_state.height;
   unsigned layers = MAX2(framebuffer_get_num_layers(state), 1);

   bool flush_clears = ctx->clears_enabled &&
                       (ctx->dynamic_fb.info.layerCount != layers ||
                        state->width != w || state->height != h);
   if (ctx->clears_enabled && !flush_clears) {
      for (int i = 0; i < ctx->fb_state.nr_cbufs; i++) {
         if (i >= state->nr_cbufs || !ctx->fb_state.cbufs[i].texture || !state->cbufs[i].texture)
            flush_clears |= zink_fb_clear_enabled(ctx, i);
         else if (zink_fb_clear_enabled(ctx, i) && !pipe_surface_equal(&ctx->fb_state.cbufs[i], &state->cbufs[i])) {
            const struct pipe_surface *a = &ctx->fb_state.cbufs[i];
            const struct pipe_surface *b = &state->cbufs[i];
            /* check everything except format */
            if (!a || !b->texture || framebuffer_surface_needs_mutable(b->texture, b) || a->level != b->level || a->first_layer != b->first_layer || a->last_layer != b->last_layer ||
                a->texture != b->texture)
               flush_clears = true;
            else if (ctx->fb_state.cbufs[i].format != state->cbufs[i].format)
               zink_fb_clear_rewrite(ctx, i, ctx->fb_state.cbufs[i].format, state->cbufs[i].format);
         }
      }
   }
   bool zsbuf_changed = !pipe_surface_equal(&ctx->fb_state.zsbuf, &state->zsbuf);
   if (zsbuf_changed)
      flush_clears |= zink_fb_clear_enabled(ctx, PIPE_MAX_COLOR_BUFS);
   if (flush_clears) {
      bool queries_disabled = ctx->queries_disabled;
      ctx->queries_disabled = true;
      zink_batch_rp(ctx);
      ctx->queries_disabled = queries_disabled;
   }
   /* need to ensure we start a new rp on next draw */
   zink_batch_no_rp_safe(ctx);
   for (int i = 0; i < ctx->fb_state.nr_cbufs; i++) {
      const struct pipe_surface *psurf = &ctx->fb_state.cbufs[i];
      unbind_fb_surface(ctx, psurf, i, i >= state->nr_cbufs || !pipe_surface_equal(&ctx->fb_state.cbufs[i], &state->cbufs[i]));
      if (psurf && ctx->needs_present == zink_resource(psurf->texture))
         zink_resource_reference(&ctx->needs_present, NULL);
   }
   if (ctx->fb_state.zsbuf.texture) {
      const struct pipe_surface *psurf = &ctx->fb_state.zsbuf;
      struct zink_resource *res = zink_resource(psurf->texture);
      bool changed = zsbuf_changed;
      unbind_fb_surface(ctx, psurf, PIPE_MAX_COLOR_BUFS, changed);
      if (changed && unlikely(res->obj->needs_zs_evaluate))
         /* have to flush zs eval while the sample location data still exists,
          * so just throw some random barrier */
         zink_screen(ctx->base.screen)->image_barrier(ctx, res, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                                     VK_ACCESS_SHADER_READ_BIT, VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT);
   }
   /* renderpass changes if the number or types of attachments change */
   ctx->rp_changed |= ctx->fb_state.nr_cbufs != state->nr_cbufs;
   ctx->rp_changed |= !!ctx->fb_state.zsbuf.texture != !!state->zsbuf.texture;
   if (ctx->tc && screen->driver_workarounds.track_renderpasses)
      ctx->rp_changed |= ctx->fb_state.resolve != state->resolve;
   if (ctx->fb_state.nr_cbufs != state->nr_cbufs) {
      ctx->blend_state_changed |= screen->have_full_ds3;
      if (state->nr_cbufs && screen->have_full_ds3)
         ctx->ds3_states |= BITFIELD_BIT(ZINK_DS3_BLEND_ON) | BITFIELD_BIT(ZINK_DS3_BLEND_WRITE) | BITFIELD_BIT(ZINK_DS3_BLEND_EQ);
   }

   util_copy_framebuffer_state(&ctx->fb_state, state);
   ctx->rp_changed |= zink_update_fbfetch(ctx);
   ctx->transient_attachments = 0;
   ctx->fb_layer_mismatch = 0;

   ctx->dynamic_fb.info.renderArea.offset.x = 0;
   ctx->dynamic_fb.info.renderArea.offset.y = 0;
   ctx->dynamic_fb.info.renderArea.extent.width = state->width;
   ctx->dynamic_fb.info.renderArea.extent.height = state->height;
   ctx->dynamic_fb.info.colorAttachmentCount = ctx->fb_state.nr_cbufs;
   ctx->rp_changed |= ctx->dynamic_fb.info.layerCount != layers;
   ctx->dynamic_fb.info.layerCount = layers;
   ctx->rp_changed |= ctx->dynamic_fb.info.viewMask != state->viewmask;
   ctx->dynamic_fb.info.viewMask = state->viewmask;
   ctx->gfx_pipeline_state.rendering_info.colorAttachmentCount = ctx->fb_state.nr_cbufs;

   ctx->void_clears = 0;
   ctx->has_swapchain = false;
   for (int i = 0; i < ctx->fb_state.nr_cbufs; i++) {
      const struct pipe_surface *psurf = &ctx->fb_state.cbufs[i];
      struct zink_resource *res = zink_resource(psurf->texture);
      ctx->fb_formats[i] = zink_get_format(screen, ctx->fb_state.cbufs[i].format);
      if (res) {
         ctx->has_swapchain |= zink_is_swapchain(res);
         if (framebuffer_surface_needs_mutable(psurf->texture, psurf))
            zink_resource_object_init_mutable(ctx, res);
         if (ctx->fb_state.cbufs[i].nr_samples)
            ctx->transient_attachments |= BITFIELD_BIT(i);
         if (!samples)
            samples = MAX3(ctx->fb_state.cbufs[i].nr_samples, psurf->texture->nr_samples, 1);
         if (psurf->last_layer - psurf->first_layer > layers)
            ctx->fb_layer_mismatch |= BITFIELD_BIT(i);
         if (res->obj->dt) {
            /* #6274 */
            if (!zink_screen(ctx->base.screen)->info.have_KHR_swapchain_mutable_format &&
                psurf->format != res->base.b.format) {
               static bool warned = false;
               if (!warned) {
                  mesa_loge("zink: SRGB framebuffer unsupported without KHR_swapchain_mutable_format");
                  warned = true;
               }
            }
         }
         res->fb_bind_count++;
         res->fb_binds |= BITFIELD_BIT(i);
         if (util_format_has_alpha1(psurf->format)) {
            if (!res->valid && !zink_fb_clear_full_exists(ctx, i))
               ctx->void_clears |= (PIPE_CLEAR_COLOR0 << i);
         }
      }
   }
   if (ctx->fb_state.zsbuf.texture) {
      struct pipe_surface *psurf = &ctx->fb_state.zsbuf;
      struct zink_resource *res = zink_resource(psurf->texture);
      if (framebuffer_surface_needs_mutable(psurf->texture, psurf))
         zink_resource_object_init_mutable(ctx, res);
      ctx->fb_formats[PIPE_MAX_COLOR_BUFS] = zink_get_format(screen, ctx->fb_state.zsbuf.format);
      if (ctx->fb_state.zsbuf.nr_samples)
         ctx->transient_attachments |= BITFIELD_BIT(PIPE_MAX_COLOR_BUFS);
      if (!samples)
         samples = MAX3(ctx->fb_state.zsbuf.nr_samples, psurf->texture->nr_samples, 1);
      if (psurf->last_layer - psurf->first_layer > layers)
         ctx->fb_layer_mismatch |= BITFIELD_BIT(PIPE_MAX_COLOR_BUFS);
      res->fb_bind_count++;
      res->fb_binds |= BITFIELD_BIT(PIPE_MAX_COLOR_BUFS);
   }

   ctx->fb_state.samples = MAX2(samples, 1);
   if (ctx->fb_state.width != w || ctx->fb_state.height != h)
      ctx->scissor_changed = true;

   uint8_t rast_samples = ctx->fb_state.samples - 1;
   if (rast_samples != ctx->gfx_pipeline_state.rast_samples)
      zink_update_fs_key_samples(ctx);
   if (ctx->gfx_pipeline_state.rast_samples != rast_samples) {
      ctx->sample_locations_changed |= ctx->gfx_pipeline_state.sample_locations_enabled;
      if (screen->have_full_ds3)
         ctx->sample_mask_changed = true;
      else
         ctx->gfx_pipeline_state.dirty = true;
   }
   ctx->gfx_pipeline_state.rast_samples = rast_samples;

   /* this is an ideal time to oom flush since it won't split a renderpass */
   if (ctx->oom_flush && !ctx->unordered_blitting)
      flush_batch(ctx, false);
   else
      update_layered_rendering_state(ctx);

   ctx->rp_tc_info_updated = !ctx->blitting;
}

static void
zink_set_blend_color(struct pipe_context *pctx,
                     const struct pipe_blend_color *color)
{
   struct zink_context *ctx = zink_context(pctx);
   memcpy(ctx->blend_constants, color->color, sizeof(float) * 4);

   ctx->blend_color_changed = true;
}

static void
zink_set_sample_mask(struct pipe_context *pctx, unsigned sample_mask)
{
   struct zink_context *ctx = zink_context(pctx);
   if (ctx->gfx_pipeline_state.sample_mask == sample_mask)
      return;
   ctx->gfx_pipeline_state.sample_mask = sample_mask;
   if (zink_screen(pctx->screen)->have_full_ds3)
      ctx->sample_mask_changed = true;
   else
      ctx->gfx_pipeline_state.dirty = true;
}

static void
zink_set_min_samples(struct pipe_context *pctx, unsigned min_samples)
{
   struct zink_context *ctx = zink_context(pctx);
   ctx->gfx_pipeline_state.min_samples = min_samples - 1;
   ctx->gfx_pipeline_state.dirty = true;
}

static void
zink_set_sample_locations(struct pipe_context *pctx, size_t size, const uint8_t *locations)
{
   struct zink_context *ctx = zink_context(pctx);

   ctx->gfx_pipeline_state.sample_locations_enabled = size && locations;
   ctx->sample_locations_changed = ctx->gfx_pipeline_state.sample_locations_enabled;
   if (size > sizeof(ctx->sample_locations))
      size = sizeof(ctx->sample_locations);

   if (locations)
      memcpy(ctx->sample_locations, locations, size);
}

static void
zink_flush(struct pipe_context *pctx,
           struct pipe_fence_handle **pfence,
           unsigned flags)
{
   struct zink_context *ctx = zink_context(pctx);
   bool deferred = flags & PIPE_FLUSH_DEFERRED;
   bool deferred_fence = false;
   struct zink_batch_state *bs = NULL;
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   VkSemaphore export_sem = VK_NULL_HANDLE;

   /* triggering clears will force state->has_work */
   if (!deferred && ctx->clears_enabled) {
      /* if fbfetch outputs are active, disable them when flushing clears */
      unsigned fbfetch_outputs = ctx->fbfetch_outputs;
      if (fbfetch_outputs) {
         ctx->fbfetch_outputs = 0;
         ctx->rp_changed = true;
      }
      if (ctx->fb_state.zsbuf.texture)
         zink_blit_barriers(ctx, NULL, zink_resource(ctx->fb_state.zsbuf.texture), false);

      for (unsigned i = 0; i < ctx->fb_state.nr_cbufs; i++) {
         if (ctx->fb_state.cbufs[i].texture)
            zink_blit_barriers(ctx, NULL, zink_resource(ctx->fb_state.cbufs[i].texture), false);
      }
      ctx->blitting = true;
      /* start rp to do all the clears */
      zink_batch_rp(ctx);
      ctx->blitting = false;
      ctx->fbfetch_outputs = fbfetch_outputs;
      ctx->rp_changed |= fbfetch_outputs > 0;
   }

   if (flags & PIPE_FLUSH_END_OF_FRAME) {
      p_atomic_inc(&screen->renderdoc_frame);
      if (ctx->needs_present && ctx->needs_present->obj->dt_idx != UINT32_MAX &&
          zink_is_swapchain(ctx->needs_present)) {
         zink_kopper_readback_update(ctx, ctx->needs_present);
         screen->image_barrier(ctx, ctx->needs_present, VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, 0, VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT);
      }
      zink_resource_reference(&ctx->needs_present, NULL);
   }

   if (flags & PIPE_FLUSH_FENCE_FD) {
      assert(!deferred && pfence);
      const VkExportSemaphoreCreateInfo esci = {
         .sType = VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO,
         .handleTypes = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT,
      };
      const VkSemaphoreCreateInfo sci = {
         .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
         .pNext = &esci,
      };
      VkResult result = VKSCR(CreateSemaphore)(screen->dev, &sci, NULL, &export_sem);
      if (zink_screen_handle_vkresult(screen, result)) {
         assert(!ctx->bs->signal_semaphore);
         ctx->bs->signal_semaphore = export_sem;
         ctx->bs->has_work = true;
      } else {
         mesa_loge("ZINK: vkCreateSemaphore failed (%s)", vk_Result_to_str(result));

         /* let flush proceed and ensure a null sem for fence_get_fd to return -1 */
         export_sem = VK_NULL_HANDLE;
      }
   }

   bool has_work = ctx->bs->has_work | ctx->bs->has_reordered_work | ctx->bs->has_unsync;
   if (!has_work) {
      if (pfence) {
         /* reuse last fence */
         bs = ctx->last_batch_state;
      }
      if (!deferred) {
         struct zink_batch_state *last = ctx->last_batch_state;
         if (last) {
            sync_flush(ctx, last);
            if (last->is_device_lost)
               check_device_lost(ctx);
         }
      }
      if (ctx->tc && !ctx->track_renderpasses)
      tc_driver_internal_flush_notify(ctx->tc);
   } else {
      bs = ctx->bs;
      if (deferred && !(flags & PIPE_FLUSH_FENCE_FD) && pfence)
         deferred_fence = true;
      else
         flush_batch(ctx, true);
   }

   if (pfence) {
      struct zink_tc_fence *mfence;

      if (flags & TC_FLUSH_ASYNC) {
         mfence = zink_tc_fence(*pfence);
         assert(mfence);
      } else {
         mfence = zink_create_tc_fence();

         screen->base.fence_reference(&screen->base, pfence, NULL);
         *pfence = (struct pipe_fence_handle *)mfence;
      }

      assert(!mfence->fence);
      mfence->fence = &bs->fence;
      mfence->sem = export_sem;
      if (bs) {
         mfence->submit_count = bs->usage.submit_count;
         util_dynarray_append(&bs->fence.mfences, struct zink_tc_fence *, mfence);
      }
      if (export_sem) {
         pipe_reference(NULL, &mfence->reference);
         util_dynarray_append(&ctx->bs->fences, struct zink_tc_fence*, mfence);
      }

      if (deferred_fence) {
         assert(bs);
         mfence->deferred_ctx = pctx;
         assert(!ctx->deferred_fence || ctx->deferred_fence == &bs->fence);
         ctx->deferred_fence = &bs->fence;
      }

      if (!bs || flags & TC_FLUSH_ASYNC) {
         if (!util_queue_fence_is_signalled(&mfence->ready))
            util_queue_fence_signal(&mfence->ready);
      }
   }
   if (bs) {
      if (!(flags & (PIPE_FLUSH_DEFERRED | PIPE_FLUSH_ASYNC)))
         sync_flush(ctx, bs);
   }
}

void
zink_fence_wait(struct pipe_context *pctx)
{
   struct zink_context *ctx = zink_context(pctx);

   if (ctx->bs->has_work || ctx->bs->has_reordered_work || ctx->bs->has_unsync)
      pctx->flush(pctx, NULL, PIPE_FLUSH_HINT_FINISH);
   if (ctx->last_batch_state)
      stall(ctx);
}

void
zink_wait_on_batch(struct zink_context *ctx, uint64_t batch_id)
{
   struct zink_batch_state *bs;
   if (!batch_id) {
      /* not submitted yet */
      flush_batch(ctx, true);
      bs = ctx->last_batch_state;
      assert(bs);
      batch_id = bs->fence.batch_id;
   }
   assert(batch_id);
   if (!zink_screen_timeline_wait(zink_screen(ctx->base.screen), batch_id, UINT64_MAX))
      check_device_lost(ctx);
}

bool
zink_check_batch_completion(struct zink_context *ctx, uint64_t batch_id)
{
   assert(ctx->bs);
   if (!batch_id)
      /* not submitted yet */
      return false;

   if (zink_screen_check_last_finished(zink_screen(ctx->base.screen), batch_id))
      return true;

   bool success = zink_screen_timeline_wait(zink_screen(ctx->base.screen), batch_id, 0);
   if (!success)
      check_device_lost(ctx);
   return success;
}

static void
zink_texture_barrier(struct pipe_context *pctx, unsigned flags)
{
   struct zink_context *ctx = zink_context(pctx);
   VkAccessFlags dst = flags == PIPE_TEXTURE_BARRIER_FRAMEBUFFER ?
                       VK_ACCESS_INPUT_ATTACHMENT_READ_BIT :
                       VK_ACCESS_SHADER_READ_BIT;

   /* if this is a fb barrier, flush all pending clears */
   if (ctx->rp_clears_enabled && dst == VK_ACCESS_INPUT_ATTACHMENT_READ_BIT)
      zink_batch_rp(ctx);

   /* this is not an in-renderpass barrier */
   if (!ctx->fbfetch_outputs)
      zink_batch_no_rp(ctx);

   if (zink_screen(ctx->base.screen)->info.have_KHR_synchronization2) {
      VkDependencyInfo dep = {0};
      dep.sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO;
      dep.dependencyFlags = VK_DEPENDENCY_BY_REGION_BIT;
      dep.memoryBarrierCount = 1;

      VkMemoryBarrier2 dmb = {0};
      dmb.sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER_2;
      dmb.pNext = NULL;
      dmb.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
      dmb.dstAccessMask = dst;
      dmb.srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
      dmb.dstStageMask = VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT;
      dep.pMemoryBarriers = &dmb;

      /* if zs fbfetch is a thing?
      if (ctx->fb_state.zsbuf) {
         const VkPipelineStageFlagBits2 depth_flags = VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT | VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT;
         dmb.dstAccessMask |= VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT;
         dmb.srcStageMask |= depth_flags;
         dmb.dstStageMask |= depth_flags;
      }
      */
      VKCTX(CmdPipelineBarrier2)(ctx->bs->cmdbuf, &dep);
   } else {
      VkMemoryBarrier bmb = {0};
      bmb.sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER;
      bmb.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
      bmb.dstAccessMask = dst;
      VKCTX(CmdPipelineBarrier)(
         ctx->bs->cmdbuf,
         VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
         VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
         0,
         1, &bmb,
         0, NULL,
         0, NULL
      );
   }
   ctx->bs->has_work = true;
}

static inline void
mem_barrier(struct zink_context *ctx, VkPipelineStageFlags src_stage, VkPipelineStageFlags dst_stage, VkAccessFlags src, VkAccessFlags dst)
{
   VkMemoryBarrier mb;
   mb.sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER;
   mb.pNext = NULL;
   mb.srcAccessMask = src;
   mb.dstAccessMask = dst;
   zink_batch_no_rp(ctx);
   VKCTX(CmdPipelineBarrier)(ctx->bs->cmdbuf, src_stage, dst_stage, 0, 1, &mb, 0, NULL, 0, NULL);
   ctx->bs->has_work = true;
}

void
zink_flush_memory_barrier(struct zink_context *ctx, bool is_compute)
{
   const VkPipelineStageFlags gfx_flags = VK_PIPELINE_STAGE_VERTEX_SHADER_BIT |
                                          VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT |
                                          VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT |
                                          VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT |
                                          VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
   const VkPipelineStageFlags cs_flags = VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT;
   VkPipelineStageFlags src = ctx->last_work_was_compute ? cs_flags : gfx_flags;
   VkPipelineStageFlags dst = is_compute ? cs_flags : gfx_flags;

   if (ctx->memory_barrier & (PIPE_BARRIER_TEXTURE | PIPE_BARRIER_SHADER_BUFFER | PIPE_BARRIER_IMAGE))
      mem_barrier(ctx, src, dst, VK_ACCESS_SHADER_WRITE_BIT, VK_ACCESS_SHADER_READ_BIT);

   if (ctx->memory_barrier & PIPE_BARRIER_CONSTANT_BUFFER)
      mem_barrier(ctx, src, dst,
                  VK_ACCESS_SHADER_WRITE_BIT,
                  VK_ACCESS_UNIFORM_READ_BIT);

   if (ctx->memory_barrier & PIPE_BARRIER_INDIRECT_BUFFER)
      mem_barrier(ctx, src, VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT,
                  VK_ACCESS_SHADER_WRITE_BIT,
                  VK_ACCESS_INDIRECT_COMMAND_READ_BIT);
   if (!is_compute) {
      if (ctx->memory_barrier & PIPE_BARRIER_VERTEX_BUFFER)
         mem_barrier(ctx, gfx_flags, VK_PIPELINE_STAGE_VERTEX_INPUT_BIT,
                     VK_ACCESS_SHADER_WRITE_BIT,
                     VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT);

      if (ctx->memory_barrier & PIPE_BARRIER_INDEX_BUFFER)
         mem_barrier(ctx, gfx_flags, VK_PIPELINE_STAGE_VERTEX_INPUT_BIT,
                     VK_ACCESS_SHADER_WRITE_BIT,
                     VK_ACCESS_INDEX_READ_BIT);
      if (ctx->memory_barrier & PIPE_BARRIER_FRAMEBUFFER)
         zink_texture_barrier(&ctx->base, 0);
      if (ctx->memory_barrier & PIPE_BARRIER_STREAMOUT_BUFFER)
         mem_barrier(ctx, VK_PIPELINE_STAGE_VERTEX_SHADER_BIT |
                            VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT |
                            VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT,
                     VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT,
                     VK_ACCESS_SHADER_READ_BIT,
                     VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT |
                     VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT);
   }
   ctx->memory_barrier = 0;
}

static void
zink_memory_barrier(struct pipe_context *pctx, unsigned flags)
{
   struct zink_context *ctx = zink_context(pctx);

   flags &= ~PIPE_BARRIER_UPDATE;
   if (!flags)
      return;

   if (flags & PIPE_BARRIER_MAPPED_BUFFER) {
      /* TODO: this should flush all persistent buffers in use as I think */
      flags &= ~PIPE_BARRIER_MAPPED_BUFFER;
   }
   ctx->memory_barrier = flags;
}

static void
zink_flush_resource(struct pipe_context *pctx,
                    struct pipe_resource *pres)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_resource *res = zink_resource(pres);
   if (res->obj->dt) {
      if (zink_kopper_acquired(res->obj->dt, res->obj->dt_idx) && (!ctx->clears_enabled || !res->fb_bind_count)) {
         zink_batch_no_rp_safe(ctx);
         zink_kopper_readback_update(ctx, res);
         zink_screen(ctx->base.screen)->image_barrier(ctx, res, VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, 0, VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT);
         zink_batch_reference_resource_rw(ctx, res, true);
      } else {
         zink_resource_reference(&ctx->needs_present, res);
      }
      ctx->swapchain = res;
   } else if (res->dmabuf)
      res->queue = VK_QUEUE_FAMILY_FOREIGN_EXT;
}

static struct pipe_stream_output_target *
zink_create_stream_output_target(struct pipe_context *pctx,
                                 struct pipe_resource *pres,
                                 unsigned buffer_offset,
                                 unsigned buffer_size)
{
   struct zink_so_target *t;
   t = CALLOC_STRUCT(zink_so_target);
   if (!t)
      return NULL;

   t->counter_buffer = pipe_buffer_create(pctx->screen, PIPE_BIND_STREAM_OUTPUT, PIPE_USAGE_DEFAULT, 4);
   if (!t->counter_buffer) {
      FREE(t);
      return NULL;
   }

   t->base.reference.count = 1;
   t->base.context = pctx;
   pipe_resource_reference(&t->base.buffer, pres);
   t->base.buffer_offset = buffer_offset;
   t->base.buffer_size = buffer_size;

   zink_resource(t->base.buffer)->so_valid = true;

   return &t->base;
}

static void
zink_stream_output_target_destroy(struct pipe_context *pctx,
                                  struct pipe_stream_output_target *psot)
{
   struct zink_so_target *t = (struct zink_so_target *)psot;
   pipe_resource_reference(&t->counter_buffer, NULL);
   pipe_resource_reference(&t->base.buffer, NULL);
   FREE(t);
}

static void
zink_set_stream_output_targets(struct pipe_context *pctx,
                               unsigned num_targets,
                               struct pipe_stream_output_target **targets,
                               const unsigned *offsets,
                               enum mesa_prim output_prim)
{
   struct zink_context *ctx = zink_context(pctx);

   /* always set counter_buffer_valid=false on unbind:
    * - on resume (indicated by offset==-1), set counter_buffer_valid=true
    * - otherwise the counter buffer is invalidated
    */

   if (num_targets == 0) {
      for (unsigned i = 0; i < ctx->num_so_targets; i++) {
         if (ctx->so_targets[i]) {
            struct zink_resource *so = zink_resource(ctx->so_targets[i]->buffer);
            if (so) {
               so->so_bind_count--;
               update_res_bind_count(ctx, so, false, true);
            }
         }
         pipe_so_target_reference(&ctx->so_targets[i], NULL);
      }
      ctx->num_so_targets = 0;
   } else {
      for (unsigned i = 0; i < num_targets; i++) {
         struct zink_so_target *t = zink_so_target(targets[i]);
         pipe_so_target_reference(&ctx->so_targets[i], targets[i]);
         if (!t)
            continue;
         if (offsets[0] != (unsigned)-1)
            t->counter_buffer_valid = false;
         struct zink_resource *so = zink_resource(ctx->so_targets[i]->buffer);
         if (so) {
            so->so_bind_count++;
            update_res_bind_count(ctx, so, false, false);
         }
      }
      for (unsigned i = num_targets; i < ctx->num_so_targets; i++) {
         if (ctx->so_targets[i]) {
            struct zink_resource *so = zink_resource(ctx->so_targets[i]->buffer);
            if (so) {
               so->so_bind_count--;
               update_res_bind_count(ctx, so, false, true);
            }
         }
         pipe_so_target_reference(&ctx->so_targets[i], NULL);
      }
      ctx->num_so_targets = num_targets;

      /* TODO: possibly avoid rebinding on resume if resuming from same buffers? */
      ctx->dirty_so_targets = true;
   }
}

ALWAYS_INLINE static struct zink_resource *
rebind_ubo(struct zink_context *ctx, gl_shader_stage shader, unsigned slot)
{
   struct zink_resource *res;
   if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
      res = update_descriptor_state_ubo_db(ctx, shader, slot, ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_UBO][shader][slot]);
   } else {
      res = update_descriptor_state_ubo_lazy(ctx, shader, slot, ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_UBO][shader][slot]);
   }
   if (res) {
      res->obj->unordered_read = false;
      res->obj->access |= VK_ACCESS_SHADER_READ_BIT;
      res->obj->access_stage |= mesa_to_vk_shader_stage(shader);
   }
   ctx->invalidate_descriptor_state(ctx, shader, ZINK_DESCRIPTOR_TYPE_UBO, slot, 1);
   return res;
}

ALWAYS_INLINE static struct zink_resource *
rebind_ssbo(struct zink_context *ctx, gl_shader_stage shader, unsigned slot)
{
   const struct pipe_shader_buffer *ssbo = &ctx->ssbos[shader][slot];
   struct zink_resource *res = zink_resource(ssbo->buffer);
   if (!res)
      return NULL;
   util_range_add(&res->base.b, &res->valid_buffer_range, ssbo->buffer_offset,
                  ssbo->buffer_offset + ssbo->buffer_size);
   if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
      update_descriptor_state_ssbo_db(ctx, shader, slot, res);
   } else {
      update_descriptor_state_ssbo_lazy(ctx, shader, slot, res);
   }
   if (res) {
      res->obj->unordered_read = false;
      res->obj->access |= VK_ACCESS_SHADER_READ_BIT;
      res->obj->access_stage |= mesa_to_vk_shader_stage(shader);
      if (ctx->writable_ssbos[shader] & BITFIELD_BIT(slot)) {
         res->obj->unordered_write = false;
         res->obj->access |= VK_ACCESS_SHADER_WRITE_BIT;
      }
   }
   ctx->invalidate_descriptor_state(ctx, shader, ZINK_DESCRIPTOR_TYPE_SSBO, slot, 1);
   return res;
}

ALWAYS_INLINE static struct zink_resource *
rebind_tbo(struct zink_context *ctx, gl_shader_stage shader, unsigned slot)
{
   struct zink_sampler_view *sampler_view = zink_sampler_view(ctx->sampler_views[shader][slot]);
   if (!sampler_view || sampler_view->base.texture->target != PIPE_BUFFER)
      return NULL;
   struct zink_resource *res = zink_resource(sampler_view->base.texture);
   if (zink_descriptor_mode != ZINK_DESCRIPTOR_MODE_DB) {
      sampler_view->buffer_view = get_buffer_view(ctx, res, sampler_view->base.format, sampler_view->base.u.buf.offset, sampler_view->base.u.buf.size);
   }
   update_descriptor_state_sampler(ctx, shader, slot, res);
   if (res) {
      res->obj->unordered_read = false;
      res->obj->access |= VK_ACCESS_SHADER_READ_BIT;
      res->obj->access_stage |= mesa_to_vk_shader_stage(shader);
   }
   ctx->invalidate_descriptor_state(ctx, shader, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, slot, 1);
   return res;
}

ALWAYS_INLINE static struct zink_resource *
rebind_ibo(struct zink_context *ctx, gl_shader_stage shader, unsigned slot)
{
   struct zink_image_view *image_view = &ctx->image_views[shader][slot];
   struct zink_resource *res = zink_resource(image_view->base.resource);
   if (!res || res->base.b.target != PIPE_BUFFER)
      return NULL;
   if (!zink_resource_object_init_storage(ctx, res)) {
      debug_printf("couldn't create storage image!");
      return NULL;
   }
   if (zink_descriptor_mode != ZINK_DESCRIPTOR_MODE_DB) {
      image_view->buffer_view = get_buffer_view(ctx, res, image_view->base.format, image_view->base.u.buf.offset, image_view->base.u.buf.size);
   }
   if (res) {
      res->obj->unordered_read = false;
      res->obj->access |= VK_ACCESS_SHADER_READ_BIT;
      res->obj->access_stage |= mesa_to_vk_shader_stage(shader);
      if (image_view->base.access & PIPE_IMAGE_ACCESS_WRITE) {
         res->obj->unordered_write = false;
         res->obj->access |= VK_ACCESS_SHADER_WRITE_BIT;
      }
   }
   util_range_add(&res->base.b, &res->valid_buffer_range, image_view->base.u.buf.offset,
                  image_view->base.u.buf.offset + image_view->base.u.buf.size);
   update_descriptor_state_image(ctx, shader, slot, res);
   ctx->invalidate_descriptor_state(ctx, shader, ZINK_DESCRIPTOR_TYPE_IMAGE, slot, 1);
   return res;
}

static unsigned
rebind_buffer(struct zink_context *ctx, struct zink_resource *res, uint32_t rebind_mask, const unsigned expected_num_rebinds)
{
   unsigned num_rebinds = 0;
   bool has_write = false;

   if (!zink_resource_has_binds(res))
      return 0;

   assert(!res->bindless[1]); //TODO
   if ((rebind_mask & BITFIELD_BIT(TC_BINDING_STREAMOUT_BUFFER)) || (!rebind_mask && res->so_bind_count && ctx->num_so_targets)) {
      for (unsigned i = 0; i < ctx->num_so_targets; i++) {
         if (ctx->so_targets[i]) {
            struct zink_resource *so = zink_resource(ctx->so_targets[i]->buffer);
            if (so && so == res) {
               ctx->dirty_so_targets = true;
               num_rebinds++;
            }
         }
      }
      rebind_mask &= ~BITFIELD_BIT(TC_BINDING_STREAMOUT_BUFFER);
   }
   if (expected_num_rebinds && num_rebinds >= expected_num_rebinds && !rebind_mask)
      goto end;

   if ((rebind_mask & BITFIELD_BIT(TC_BINDING_VERTEX_BUFFER)) || (!rebind_mask && res->vbo_bind_mask)) {
      u_foreach_bit(slot, res->vbo_bind_mask) {
         if (ctx->vertex_buffers[slot].buffer.resource != &res->base.b) //wrong context
            goto end;
         res->obj->access |= VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT;
         res->obj->access_stage |= VK_PIPELINE_STAGE_VERTEX_INPUT_BIT;
         res->obj->unordered_read = false;
         num_rebinds++;
      }
      rebind_mask &= ~BITFIELD_BIT(TC_BINDING_VERTEX_BUFFER);
      ctx->vertex_buffers_dirty = true;
   }
   if (expected_num_rebinds && num_rebinds >= expected_num_rebinds && !rebind_mask)
      goto end;

   const uint32_t ubo_mask = rebind_mask ?
                             rebind_mask & BITFIELD_RANGE(TC_BINDING_UBO_VS, MESA_SHADER_STAGES) :
                             ((res->ubo_bind_count[0] ? BITFIELD_RANGE(TC_BINDING_UBO_VS, (MESA_SHADER_STAGES - 1)) : 0) |
                              (res->ubo_bind_count[1] ? BITFIELD_BIT(TC_BINDING_UBO_CS) : 0));
   u_foreach_bit(shader, ubo_mask >> TC_BINDING_UBO_VS) {
      u_foreach_bit(slot, res->ubo_bind_mask[shader]) {
         if (&res->base.b != ctx->ubos[shader][slot].buffer) //wrong context
            goto end;
         rebind_ubo(ctx, shader, slot);
         num_rebinds++;
      }
   }
   rebind_mask &= ~BITFIELD_RANGE(TC_BINDING_UBO_VS, MESA_SHADER_STAGES);
   if (expected_num_rebinds && num_rebinds >= expected_num_rebinds && !rebind_mask)
      goto end;

   const unsigned ssbo_mask = rebind_mask ?
                              rebind_mask & BITFIELD_RANGE(TC_BINDING_SSBO_VS, MESA_SHADER_STAGES) :
                              BITFIELD_RANGE(TC_BINDING_SSBO_VS, MESA_SHADER_STAGES);
   u_foreach_bit(shader, ssbo_mask >> TC_BINDING_SSBO_VS) {
      u_foreach_bit(slot, res->ssbo_bind_mask[shader]) {
         struct pipe_shader_buffer *ssbo = &ctx->ssbos[shader][slot];
         if (&res->base.b != ssbo->buffer) //wrong context
            goto end;
         rebind_ssbo(ctx, shader, slot);
         has_write |= (ctx->writable_ssbos[shader] & BITFIELD64_BIT(slot)) != 0;
         num_rebinds++;
      }
   }
   rebind_mask &= ~BITFIELD_RANGE(TC_BINDING_SSBO_VS, MESA_SHADER_STAGES);
   if (expected_num_rebinds && num_rebinds >= expected_num_rebinds && !rebind_mask)
      goto end;
   const unsigned sampler_mask = rebind_mask ?
                                 rebind_mask & BITFIELD_RANGE(TC_BINDING_SAMPLERVIEW_VS, MESA_SHADER_STAGES) :
                                 BITFIELD_RANGE(TC_BINDING_SAMPLERVIEW_VS, MESA_SHADER_STAGES);
   u_foreach_bit(shader, sampler_mask >> TC_BINDING_SAMPLERVIEW_VS) {
      u_foreach_bit(slot, res->sampler_binds[shader]) {
         struct zink_sampler_view *sampler_view = zink_sampler_view(ctx->sampler_views[shader][slot]);
         if (&res->base.b != sampler_view->base.texture) //wrong context
            goto end;
         rebind_tbo(ctx, shader, slot);
         num_rebinds++;
      }
   }
   rebind_mask &= ~BITFIELD_RANGE(TC_BINDING_SAMPLERVIEW_VS, MESA_SHADER_STAGES);
   if (expected_num_rebinds && num_rebinds >= expected_num_rebinds && !rebind_mask)
      goto end;

   const unsigned image_mask = rebind_mask ?
                               rebind_mask & BITFIELD_RANGE(TC_BINDING_IMAGE_VS, MESA_SHADER_STAGES) :
                               BITFIELD_RANGE(TC_BINDING_IMAGE_VS, MESA_SHADER_STAGES);
   unsigned num_image_rebinds_remaining = rebind_mask ? expected_num_rebinds - num_rebinds : res->image_bind_count[0] + res->image_bind_count[1];
   u_foreach_bit(shader, image_mask >> TC_BINDING_IMAGE_VS) {
      for (unsigned slot = 0; num_image_rebinds_remaining && slot < ctx->di.num_images[shader]; slot++) {
         struct zink_resource *cres = ctx->di.descriptor_res[ZINK_DESCRIPTOR_TYPE_IMAGE][shader][slot];
         if (res != cres)
            continue;

         rebind_ibo(ctx, shader, slot);
         const struct zink_image_view *image_view = &ctx->image_views[shader][slot];
         has_write |= (image_view->base.access & PIPE_IMAGE_ACCESS_WRITE) != 0;
         num_image_rebinds_remaining--;
         num_rebinds++;
      }
   }
end:
   if (num_rebinds)
      zink_batch_resource_usage_set(ctx->bs, res, has_write, true);
   return num_rebinds;
}

void
zink_copy_buffer(struct zink_context *ctx, struct zink_resource *dst, struct zink_resource *src,
                 unsigned dst_offset, unsigned src_offset, unsigned size, bool unsync)
{
   if (unsync) {
      util_queue_fence_wait(&ctx->flush_fence);
      util_queue_fence_reset(&ctx->unsync_fence);
   }

   VkBufferCopy region;
   region.srcOffset = src_offset;
   region.dstOffset = dst_offset;
   region.size = size;

   struct pipe_box box;
   u_box_3d((int)src_offset, 0, 0, (int)size, 0, 0, &box);
   /* must barrier if something wrote the valid buffer range */
   bool valid_write = zink_check_valid_buffer_src_access(ctx, src, src_offset, size);
   bool unordered_src = !valid_write && !zink_check_unordered_transfer_access(src, 0, &box);
   /* unsync should only occur during subdata with staging resource */
   if (!unsync)
      zink_screen(ctx->base.screen)->buffer_barrier(ctx, src, VK_ACCESS_TRANSFER_READ_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT);
   bool unordered_dst = zink_resource_buffer_transfer_dst_barrier(ctx, dst, dst_offset, size);
   bool can_unorder = unordered_dst && unordered_src && !ctx->no_reorder;
   VkCommandBuffer cmdbuf = unsync ? ctx->bs->unsynchronized_cmdbuf : 
                                     can_unorder ? ctx->bs->reordered_cmdbuf : zink_get_cmdbuf(ctx, src, dst);
   ctx->bs->has_reordered_work |= can_unorder;
   ctx->bs->has_unsync |= unsync;
   zink_batch_reference_resource_rw(ctx, src, false);
   zink_batch_reference_resource_rw(ctx, dst, true);
   if (unlikely(zink_debug & ZINK_DEBUG_SYNC)) {
      VkMemoryBarrier mb;
      mb.sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER;
      mb.pNext = NULL;
      mb.srcAccessMask = VK_ACCESS_MEMORY_WRITE_BIT;
      mb.dstAccessMask = VK_ACCESS_MEMORY_READ_BIT;
      VKCTX(CmdPipelineBarrier)(cmdbuf,
                                VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
                                VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
                                0, 1, &mb, 0, NULL, 0, NULL);
   }
   bool marker = zink_cmd_debug_marker_begin(ctx, cmdbuf, "copy_buffer(%d)", size);
   VKCTX(CmdCopyBuffer)(cmdbuf, src->obj->buffer, dst->obj->buffer, 1, &region);
   zink_cmd_debug_marker_end(ctx, cmdbuf, marker);

   if (unsync)
      util_queue_fence_signal(&ctx->unsync_fence);
}

void
zink_copy_image_buffer(struct zink_context *ctx, struct zink_resource *dst, struct zink_resource *src,
                       unsigned buffer_offset,
                       unsigned buffer_stride,
                       unsigned buffer_layer_stride,
                       unsigned level, const struct pipe_box *box, enum pipe_map_flags map_flags)
{
   struct zink_resource *img = dst->base.b.target == PIPE_BUFFER ? src : dst;
   struct zink_resource *use_img = img;
   struct zink_resource *buf = dst->base.b.target == PIPE_BUFFER ? dst : src;
   bool needs_present_readback = false;
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   bool buf2img = buf == src;
   bool img_needs_transfer_barrier = !screen->driver_workarounds.general_layout && buf2img && ctx->track_renderpasses;
   bool unsync = !!(map_flags & PIPE_MAP_UNSYNCHRONIZED);
   if (unsync) {
      util_queue_fence_wait(&ctx->flush_fence);
      util_queue_fence_reset(&ctx->unsync_fence);
   }

   if (buf2img) {
      if (zink_is_swapchain(img)) {
         if (!zink_kopper_acquire(ctx, img, UINT64_MAX))
            return;
      }
      zink_resource_image_transfer_dst_barrier(ctx, img, level, box, unsync);
      if (!unsync)
         screen->buffer_barrier(ctx, buf, VK_ACCESS_TRANSFER_READ_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT);
   } else {
      assert(!(map_flags & PIPE_MAP_UNSYNCHRONIZED));
      if (zink_is_swapchain(img))
         needs_present_readback = zink_kopper_acquire_readback(ctx, img, &use_img);
      VkImageLayout layout = screen->driver_workarounds.general_layout ? VK_IMAGE_LAYOUT_GENERAL : VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
      screen->image_barrier(ctx, use_img, layout, VK_ACCESS_TRANSFER_READ_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT);
      zink_resource_buffer_transfer_dst_barrier(ctx, buf, box->x, box->width);
   }

   VkBufferImageCopy region = {0};
   region.bufferOffset = buffer_offset;
   region.bufferRowLength = buffer_stride;
   region.bufferImageHeight = buffer_layer_stride;
   region.imageSubresource.mipLevel = level;
   enum pipe_texture_target img_target = img->base.b.target;
   if (img->need_2D)
      img_target = img_target == PIPE_TEXTURE_1D ? PIPE_TEXTURE_2D : PIPE_TEXTURE_2D_ARRAY;
   switch (img_target) {
   case PIPE_TEXTURE_CUBE:
   case PIPE_TEXTURE_CUBE_ARRAY:
   case PIPE_TEXTURE_2D_ARRAY:
   case PIPE_TEXTURE_1D_ARRAY:
      /* these use layer */
      region.imageSubresource.baseArrayLayer = box->z;
      region.imageSubresource.layerCount = box->depth;
      region.imageOffset.z = 0;
      region.imageExtent.depth = 1;
      break;
   case PIPE_TEXTURE_3D:
      /* this uses depth */
      region.imageSubresource.baseArrayLayer = 0;
      region.imageSubresource.layerCount = 1;
      region.imageOffset.z = box->z;
      region.imageExtent.depth = box->depth;
      break;
   default:
      /* these must only copy one layer */
      region.imageSubresource.baseArrayLayer = 0;
      region.imageSubresource.layerCount = 1;
      region.imageOffset.z = 0;
      region.imageExtent.depth = 1;
   }
   region.imageOffset.x = box->x;
   region.imageOffset.y = box->y;

   region.imageExtent.width = box->width;
   region.imageExtent.height = box->height;

   VkCommandBuffer cmdbuf = unsync ?
                            ctx->bs->unsynchronized_cmdbuf :
                            /* never promote to unordered if swapchain was acquired */
                            needs_present_readback ?
                            ctx->bs->cmdbuf :
                            buf2img ? zink_get_cmdbuf(ctx, buf, use_img) : zink_get_cmdbuf(ctx, use_img, buf);
   if (unsync) {
      zink_batch_resource_usage_set(ctx->bs, use_img, buf2img, use_img->obj->is_buffer);
      if (!zink_batch_reference_resource_move_unsync(ctx, use_img))
         zink_resource_object_reference(NULL, NULL, use_img->obj);
      zink_batch_resource_usage_set(ctx->bs, buf, !buf2img, buf->obj->is_buffer);
      if (!zink_batch_reference_resource_move_unsync(ctx, buf))
         zink_resource_object_reference(NULL, NULL, buf->obj);
      ctx->bs->has_unsync = true;
      use_img->obj->unsync_access = true;
   } else {
      zink_batch_reference_resource_rw(ctx, use_img, buf2img);
      zink_batch_reference_resource_rw(ctx, buf, !buf2img);

      /* hacky detection of pre-rp buf2img from tc; reordered/unsync versions get their own sync */
      if (buf2img && cmdbuf == ctx->bs->cmdbuf)
         img_needs_transfer_barrier = ctx->track_renderpasses;
   }

   /* we're using u_transfer_helper_deinterleave, which means we'll be getting PIPE_MAP_* usage
    * to indicate whether to copy either the depth or stencil aspects
    */
   unsigned aspects = 0;
   if (map_flags) {
      assert((map_flags & (PIPE_MAP_DEPTH_ONLY | PIPE_MAP_STENCIL_ONLY)) !=
             (PIPE_MAP_DEPTH_ONLY | PIPE_MAP_STENCIL_ONLY));
      if (map_flags & PIPE_MAP_DEPTH_ONLY)
         aspects = VK_IMAGE_ASPECT_DEPTH_BIT;
      else if (map_flags & PIPE_MAP_STENCIL_ONLY)
         aspects = VK_IMAGE_ASPECT_STENCIL_BIT;
   }
   if (!aspects)
      aspects = img->aspect;
   if (unlikely(zink_debug & ZINK_DEBUG_SYNC)) {
      VkMemoryBarrier mb;
      mb.sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER;
      mb.pNext = NULL;
      mb.srcAccessMask = VK_ACCESS_MEMORY_WRITE_BIT;
      mb.dstAccessMask = VK_ACCESS_MEMORY_READ_BIT;
      VKCTX(CmdPipelineBarrier)(cmdbuf,
                                VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
                                VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
                                0, 1, &mb, 0, NULL, 0, NULL);
   }
   while (aspects) {
      int aspect = 1 << u_bit_scan(&aspects);
      region.imageSubresource.aspectMask = aspect;

      /* MSAA transfers should have already been handled by U_TRANSFER_HELPER_MSAA_MAP, since
       * there's no way to resolve using this interface:
       *
       * srcImage must have a sample count equal to VK_SAMPLE_COUNT_1_BIT
       * - vkCmdCopyImageToBuffer spec
       *
       * dstImage must have a sample count equal to VK_SAMPLE_COUNT_1_BIT
       * - vkCmdCopyBufferToImage spec
       */
      assert(img->base.b.nr_samples <= 1);
      bool marker;
      if (buf2img) {
         marker = zink_cmd_debug_marker_begin(ctx, cmdbuf, "copy_buffer2image(%s, %dx%dx%d)",
                                                   util_format_short_name(dst->base.b.format),
                                                   region.imageExtent.width,
                                                   region.imageExtent.height,
                                                   MAX2(region.imageSubresource.layerCount, region.imageExtent.depth));
         VKCTX(CmdCopyBufferToImage)(cmdbuf, buf->obj->buffer, use_img->obj->image, use_img->layout, 1, &region);
      } else {
         marker = zink_cmd_debug_marker_begin(ctx, cmdbuf, "copy_image2buffer(%s, %dx%dx%d)",
                                                   util_format_short_name(src->base.b.format),
                                                   region.imageExtent.width,
                                                   region.imageExtent.height,
                                                   MAX2(region.imageSubresource.layerCount, region.imageExtent.depth));
         VKCTX(CmdCopyImageToBuffer)(cmdbuf, use_img->obj->image, use_img->layout, buf->obj->buffer, 1, &region);
      }
      zink_cmd_debug_marker_end(ctx, cmdbuf, marker);
   }
   if (unsync)
      util_queue_fence_signal(&ctx->unsync_fence);
   if (needs_present_readback) {
      assert(!unsync);
      if (buf2img) {
         img->obj->unordered_write = false;
         buf->obj->unordered_read = false;
      } else {
         img->obj->unordered_read = false;
         buf->obj->unordered_write = false;
      }
      zink_kopper_present_readback(ctx, img);
   }

   if (img_needs_transfer_barrier)
      pre_sync_transfer_barrier(ctx, img, unsync);

   if (ctx->oom_flush && !ctx->in_rp && !ctx->unordered_blitting && !unsync)
      flush_batch(ctx, false);
}

static void
zink_image_copy_buffer(struct pipe_context *pctx,
                       struct pipe_resource *pdst,
                       struct pipe_resource *psrc,
                       unsigned buffer_offset,
                       unsigned buffer_stride,
                       unsigned buffer_layer_stride,
                       unsigned level,
                       const struct pipe_box *box)
{
   /* convert bytes to texels */
   struct pipe_resource *img = pdst->target != PIPE_BUFFER ? pdst : psrc;
   unsigned stride = buffer_stride ?
                     buffer_stride / util_format_get_blocksize(img->format) * util_format_get_blockwidth(img->format) :
                     util_format_get_nblocksx(img->format, box->width);
   unsigned layer_stride = buffer_layer_stride ?
                           buffer_layer_stride / buffer_stride * util_format_get_blockheight(img->format) :
                           util_format_get_nblocksy(img->format, box->height);

   zink_copy_image_buffer(zink_context(pctx), zink_resource(pdst), zink_resource(psrc),
                          buffer_offset, stride, layer_stride, level, box, 0);
}

static void
zink_resource_copy_region(struct pipe_context *pctx,
                          struct pipe_resource *pdst,
                          unsigned dst_level, unsigned dstx, unsigned dsty, unsigned dstz,
                          struct pipe_resource *psrc,
                          unsigned src_level, const struct pipe_box *src_box)
{
   struct zink_resource *dst = zink_resource(pdst);
   struct zink_resource *src = zink_resource(psrc);
   struct zink_context *ctx = zink_context(pctx);
   if (dst->base.b.target != PIPE_BUFFER && src->base.b.target != PIPE_BUFFER) {
      VkImageCopy region;
      /* fill struct holes */
      memset(&region, 0, sizeof(region));
      if (util_format_get_num_planes(src->base.b.format) == 1 &&
          util_format_get_num_planes(dst->base.b.format) == 1) {
      /* If neither the calling command’s srcImage nor the calling command’s dstImage
       * has a multi-planar image format then the aspectMask member of srcSubresource
       * and dstSubresource must match
       *
       * -VkImageCopy spec
       */
         assert(src->aspect == dst->aspect);
      } else
         UNREACHABLE("planar formats not yet handled");


      region.srcSubresource.aspectMask = src->aspect;
      region.srcSubresource.mipLevel = src_level;
      enum pipe_texture_target src_target = src->base.b.target;
      if (src->need_2D)
         src_target = src_target == PIPE_TEXTURE_1D ? PIPE_TEXTURE_2D : PIPE_TEXTURE_2D_ARRAY;
      switch (src_target) {
      case PIPE_TEXTURE_CUBE:
      case PIPE_TEXTURE_CUBE_ARRAY:
      case PIPE_TEXTURE_2D_ARRAY:
      case PIPE_TEXTURE_1D_ARRAY:
         /* these use layer */
         region.srcSubresource.baseArrayLayer = src_box->z;
         region.srcSubresource.layerCount = src_box->depth;
         region.srcOffset.z = 0;
         region.extent.depth = 1;
         break;
      case PIPE_TEXTURE_3D:
         /* this uses depth */
         region.srcSubresource.baseArrayLayer = 0;
         region.srcSubresource.layerCount = 1;
         region.srcOffset.z = src_box->z;
         region.extent.depth = src_box->depth;
         break;
      default:
         /* these must only copy one layer */
         region.srcSubresource.baseArrayLayer = 0;
         region.srcSubresource.layerCount = 1;
         region.srcOffset.z = 0;
         region.extent.depth = 1;
      }

      region.srcOffset.x = src_box->x;
      region.srcOffset.y = src_box->y;

      region.dstSubresource.aspectMask = dst->aspect;
      region.dstSubresource.mipLevel = dst_level;
      enum pipe_texture_target dst_target = dst->base.b.target;
      if (dst->need_2D)
         dst_target = dst_target == PIPE_TEXTURE_1D ? PIPE_TEXTURE_2D : PIPE_TEXTURE_2D_ARRAY;
      switch (dst_target) {
      case PIPE_TEXTURE_CUBE:
      case PIPE_TEXTURE_CUBE_ARRAY:
      case PIPE_TEXTURE_2D_ARRAY:
      case PIPE_TEXTURE_1D_ARRAY:
         /* these use layer */
         region.dstSubresource.baseArrayLayer = dstz;
         region.dstSubresource.layerCount = src_box->depth;
         region.dstOffset.z = 0;
         break;
      case PIPE_TEXTURE_3D:
         /* this uses depth */
         region.dstSubresource.baseArrayLayer = 0;
         region.dstSubresource.layerCount = 1;
         region.dstOffset.z = dstz;
         break;
      default:
         /* these must only copy one layer */
         region.dstSubresource.baseArrayLayer = 0;
         region.dstSubresource.layerCount = 1;
         region.dstOffset.z = 0;
      }

      region.dstOffset.x = dstx;
      region.dstOffset.y = dsty;
      region.extent.width = src_box->width;
      region.extent.height = src_box->height;

      /* ignore no-op copies */
      if (src == dst &&
          !memcmp(&region.dstOffset, &region.srcOffset, sizeof(region.srcOffset)) &&
          !memcmp(&region.dstSubresource, &region.srcSubresource, sizeof(region.srcSubresource)))
         return;

      zink_fb_clears_apply_or_discard(ctx, pdst, (struct u_rect){dstx, dstx + src_box->width, dsty, dsty + src_box->height}, dstz, src_box->depth, false);
      zink_fb_clears_apply_region(ctx, psrc, zink_rect_from_box(src_box), src_box->z, src_box->depth);

      struct zink_screen *screen = zink_screen(ctx->base.screen);
      if (screen->driver_workarounds.general_layout) {
         struct pipe_box box = {
            dstx, src_box->width,
            dsty, src_box->height,
            dstz, src_box->depth
         };
         zink_resource_image_transfer_dst_barrier(ctx, dst, dst_level, &box, false);
         screen->image_barrier(ctx, src,
                               VK_IMAGE_LAYOUT_GENERAL,
                               VK_ACCESS_TRANSFER_READ_BIT | VK_ACCESS_TRANSFER_WRITE_BIT,
                               VK_PIPELINE_STAGE_TRANSFER_BIT);
      } else {
         zink_resource_setup_transfer_layouts(ctx, src, dst);
      }
      VkCommandBuffer cmdbuf = zink_get_cmdbuf(ctx, src, dst);
      zink_batch_reference_resource_rw(ctx, src, false);
      zink_batch_reference_resource_rw(ctx, dst, true);

      if (unlikely(zink_debug & ZINK_DEBUG_SYNC)) {
         VkMemoryBarrier mb;
         mb.sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER;
         mb.pNext = NULL;
         mb.srcAccessMask = VK_ACCESS_MEMORY_WRITE_BIT;
         mb.dstAccessMask = VK_ACCESS_MEMORY_READ_BIT;
         VKCTX(CmdPipelineBarrier)(cmdbuf,
                                   VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
                                   VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
                                   0, 1, &mb, 0, NULL, 0, NULL);
      }
      bool marker = zink_cmd_debug_marker_begin(ctx, cmdbuf, "copy_image(%s->%s, %dx%dx%d)",
                                                util_format_short_name(psrc->format),
                                                util_format_short_name(pdst->format),
                                                region.extent.width,
                                                region.extent.height,
                                                MAX2(region.srcSubresource.layerCount, region.extent.depth));
      VKCTX(CmdCopyImage)(cmdbuf, src->obj->image, src->layout,
                     dst->obj->image, dst->layout,
                     1, &region);
      zink_cmd_debug_marker_end(ctx, cmdbuf, marker);
   } else if (dst->base.b.target == PIPE_BUFFER &&
              src->base.b.target == PIPE_BUFFER) {
      zink_copy_buffer(ctx, dst, src, dstx, src_box->x, src_box->width, false);
   } else {
      bool buf2img = psrc->target == PIPE_BUFFER;
      struct pipe_box box = *src_box;
      unsigned buffer_offset = buf2img ? src_box->x : dstx;
      unsigned level = buf2img ? dst_level : src_level;
      if (buf2img) {
         box.x = dstx;
         box.y = dsty;
         box.z = dstz;
      }
      zink_copy_image_buffer(ctx, dst, src, buffer_offset, 0, 0, level, &box, 0);
   }
   if (ctx->oom_flush && !ctx->in_rp && !ctx->unordered_blitting)
      flush_batch(ctx, false);
}

static bool
zink_resource_commit(struct pipe_context *pctx, struct pipe_resource *pres, unsigned level, struct pipe_box *box, bool commit)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_resource *res = zink_resource(pres);

   /* if any current usage exists, flush the queue */
   if (zink_resource_has_unflushed_usage(res))
      zink_flush_queue(ctx);

   bool ret = zink_bo_commit(ctx, res, level, box, commit, &ctx->bs->sparse_semaphore);
   if (ret) {
      zink_batch_reference_resource_rw(ctx, res, true);
      ctx->bs->has_work = true;
   } else {
      check_device_lost(ctx);
   }

   return ret;
}

static void
check_fb_rebind(struct zink_context *ctx, struct zink_resource *res)
{
   if (res->aspect & VK_IMAGE_ASPECT_COLOR_BIT) {
      for (unsigned i = 0; i < ctx->fb_state.nr_cbufs; i++) {
         if (zink_resource(ctx->fb_state.cbufs[i].texture) == res)
            return;
      }
   } else {
      if (zink_resource(ctx->fb_state.zsbuf.texture) == res)
         return;
   }
   /* next renderpass will automatically pull in new surface */
   zink_batch_no_rp(ctx);
   ctx->rp_changed = true;
}

static void
rebind_image(struct zink_context *ctx, struct zink_resource *res)
{
   assert(!ctx->blitting);
   if (res->fb_binds)
      check_fb_rebind(ctx, res);
   if (!zink_resource_has_binds(res))
      return;
   bool general_layout = zink_screen(ctx->base.screen)->driver_workarounds.general_layout;
   for (unsigned i = 0; i < MESA_SHADER_STAGES; i++) {
      if (res->sampler_binds[i]) {
         for (unsigned j = 0; j < ctx->di.num_sampler_views[i]; j++) {
            struct zink_sampler_view *sv = zink_sampler_view(ctx->sampler_views[i][j]);
            if (sv && sv->base.texture == &res->base.b) {
               sv->rebind_count = res->rebind_count;
               struct pipe_surface tmpl = pipe_surface_templ_from_sampler_view(&sv->base, &res->base.b, sv->base.target);
               sv->image_view = zink_get_surface(ctx, &tmpl, &sv->ivci);
               ctx->invalidate_descriptor_state(ctx, i, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, j, 1);
               update_descriptor_state_sampler(ctx, i, j, res);
            }
         }
      }
      if (!res->image_bind_count[i == MESA_SHADER_COMPUTE])
         continue;
      for (unsigned j = 0; j < ctx->di.num_images[i]; j++) {
         if (zink_resource(ctx->image_views[i][j].base.resource) == res) {
            ctx->invalidate_descriptor_state(ctx, i, ZINK_DESCRIPTOR_TYPE_IMAGE, j, 1);
            update_descriptor_state_image(ctx, i, j, res);
            if (!general_layout)
               _mesa_set_add(ctx->need_barriers[i == MESA_SHADER_COMPUTE], res);
         }
      }
   }
}

bool
zink_resource_rebind(struct zink_context *ctx, struct zink_resource *res)
{
   if (res->base.b.target == PIPE_BUFFER) {
      /* force counter buffer reset */
      res->so_valid = false;
      return rebind_buffer(ctx, res, 0, 0) == res->bind_count[0] + res->bind_count[1];
   }
   rebind_image(ctx, res);
   return false;
}

void
zink_rebind_all_buffers(struct zink_context *ctx)
{
   ctx->vertex_buffers_dirty = ctx->gfx_pipeline_state.vertex_buffers_enabled_mask > 0;
   ctx->dirty_so_targets = ctx->num_so_targets > 0;
   if (ctx->num_so_targets)
      zink_screen(ctx->base.screen)->buffer_barrier(ctx, zink_resource(ctx->dummy_xfb_buffer),
                                   VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT, VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT);
   for (unsigned shader = MESA_SHADER_VERTEX; shader < MESA_SHADER_STAGES; shader++) {
      for (unsigned slot = 0; slot < ctx->di.num_ubos[shader]; slot++) {
         struct zink_resource *res = rebind_ubo(ctx, shader, slot);
         if (res)
            zink_batch_resource_usage_set(ctx->bs, res, false, true);
      }
      for (unsigned slot = 0; slot < ctx->di.num_sampler_views[shader]; slot++) {
         struct zink_resource *res = rebind_tbo(ctx, shader, slot);
         if (res)
            zink_batch_resource_usage_set(ctx->bs, res, false, true);
      }
      for (unsigned slot = 0; slot < ctx->di.num_ssbos[shader]; slot++) {
         struct zink_resource *res = rebind_ssbo(ctx, shader, slot);
         if (res)
            zink_batch_resource_usage_set(ctx->bs, res, (ctx->writable_ssbos[shader] & BITFIELD64_BIT(slot)) != 0, true);
      }
      for (unsigned slot = 0; slot < ctx->di.num_images[shader]; slot++) {
         struct zink_resource *res = rebind_ibo(ctx, shader, slot);
         if (res)
            zink_batch_resource_usage_set(ctx->bs, res, (ctx->image_views[shader][slot].base.access & PIPE_IMAGE_ACCESS_WRITE) != 0, true);
      }
   }
}

void
zink_rebind_all_images(struct zink_context *ctx)
{
   assert(!ctx->blitting);
   if (ctx->in_rp) {
      bool changed = false;
      for (unsigned i = 0; i < ctx->fb_state.nr_cbufs; i++) {
         if (!ctx->fb_state.cbufs[i].texture)
            continue;
         struct zink_resource *res = zink_resource(ctx->fb_state.cbufs[i].texture);
         struct zink_surface *surf = zink_create_fb_surface(&ctx->base, &ctx->fb_state.cbufs[i]);
         VkImageView iv = surf ? surf->image_view : VK_NULL_HANDLE;
         if (res->transient) {
            changed |= ctx->dynamic_fb.attachments[i].resolveImageView != iv;
         } else {
            changed |= ctx->dynamic_fb.attachments[i].imageView != iv;
         }
      }
      if (ctx->fb_state.zsbuf.texture) {
         struct zink_resource *res = zink_resource(ctx->fb_state.zsbuf.texture);
         struct zink_surface *surf = zink_create_fb_surface(&ctx->base, &ctx->fb_state.zsbuf);
         VkImageView iv = surf ? surf->image_view : VK_NULL_HANDLE;
         if (res->transient) {
            changed |= ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].resolveImageView != iv;
         } else {
            changed |= ctx->dynamic_fb.attachments[PIPE_MAX_COLOR_BUFS].imageView != iv;
         }
      }
      if (changed) {
         zink_batch_no_rp(ctx);
         ctx->rp_changed = true;
      }
   }
   bool general_layout = zink_screen(ctx->base.screen)->driver_workarounds.general_layout;
   for (unsigned i = 0; i < MESA_SHADER_STAGES; i++) {
      for (unsigned j = 0; j < ctx->di.num_sampler_views[i]; j++) {
         struct zink_sampler_view *sv = zink_sampler_view(ctx->sampler_views[i][j]);
         if (!sv || !sv->image_view || sv->base.texture->target == PIPE_BUFFER)
            continue;
         struct zink_resource *res = zink_resource(sv->base.texture);
         if (res->rebind_count != sv->rebind_count) {
            sv->rebind_count = res->rebind_count;
            struct pipe_surface tmpl = pipe_surface_templ_from_sampler_view(&sv->base, &res->base.b, sv->base.target);
            sv->image_view = zink_get_surface(ctx, &tmpl, &sv->ivci);
            ctx->invalidate_descriptor_state(ctx, i, ZINK_DESCRIPTOR_TYPE_SAMPLER_VIEW, j, 1);
            update_descriptor_state_sampler(ctx, i, j, res);
         }
      }
      for (unsigned j = 0; j < ctx->di.num_images[i]; j++) {
         struct zink_image_view *image_view = &ctx->image_views[i][j];
         struct zink_resource *res = zink_resource(image_view->base.resource);
         if (!res || res->base.b.target == PIPE_BUFFER)
            continue;
         if (ctx->image_views[i][j].rebind_count != res->rebind_count) {
            struct zink_resource *import2d = NULL;
            image_view->surface = create_image_surface(ctx, &image_view->base, i == MESA_SHADER_COMPUTE, &import2d);
            assert(!import2d);
            image_view->rebind_count = res->rebind_count;
            ctx->invalidate_descriptor_state(ctx, i, ZINK_DESCRIPTOR_TYPE_IMAGE, j, 1);
            update_descriptor_state_image(ctx, i, j, res);
            if (!general_layout)
               _mesa_set_add(ctx->need_barriers[i == MESA_SHADER_COMPUTE], res);
         }
      }
   }
}

static void
zink_context_replace_buffer_storage(struct pipe_context *pctx, struct pipe_resource *dst,
                                    struct pipe_resource *src, unsigned num_rebinds,
                                    uint32_t rebind_mask, uint32_t delete_buffer_id)
{
   struct zink_resource *d = zink_resource(dst);
   struct zink_resource *s = zink_resource(src);
   struct zink_context *ctx = zink_context(pctx);
   struct zink_screen *screen = zink_screen(pctx->screen);

   assert(d->internal_format == s->internal_format);
   assert(d->obj);
   assert(s->obj);
   util_idalloc_mt_free(&screen->buffer_ids, delete_buffer_id);
   zink_batch_reference_resource(ctx, d);
   /* don't be too creative */
   zink_resource_object_reference(screen, &d->obj, s->obj);
   d->valid_buffer_range = s->valid_buffer_range;
   s->real_buffer_range = &d->valid_buffer_range;
   zink_resource_copies_reset(d);
   /* force counter buffer reset */
   d->so_valid = false;
   d->rebind_count++;
   /* FIXME: tc buffer sharedness tracking */
   if (!num_rebinds) {
      num_rebinds = d->bind_count[0] + d->bind_count[1];
      rebind_mask = 0;
   }
   unsigned rebind_count = num_rebinds ? rebind_buffer(ctx, d, rebind_mask, num_rebinds) : 0;
   if (rebind_count != d->bind_count[0] + d->bind_count[1])
      ctx->buffer_rebind_counter = p_atomic_inc_return(&screen->buffer_rebind_counter);
}

static bool
zink_context_is_resource_busy(struct pipe_screen *pscreen, struct pipe_resource *pres, unsigned usage)
{
   struct zink_screen *screen = zink_screen(pscreen);
   struct zink_resource *res = zink_resource(pres);
   uint32_t check_usage = 0;
   if (usage & PIPE_MAP_UNSYNCHRONIZED && (!res->obj->unsync_access || zink_is_swapchain(res)))
      return true;
   if (usage & PIPE_MAP_READ)
      check_usage |= ZINK_RESOURCE_ACCESS_WRITE;
   if (usage & PIPE_MAP_WRITE)
      check_usage |= ZINK_RESOURCE_ACCESS_RW;
   return !zink_resource_usage_check_completion(screen, res, check_usage);
}

static void
zink_emit_string_marker(struct pipe_context *pctx,
                        const char *string, int len)
{
   struct zink_screen *screen = zink_screen(pctx->screen);
   struct zink_context *ctx = zink_context(pctx);

   /* make sure string is nul-terminated */
   char buf[512], *temp = NULL;
   if (len < ARRAY_SIZE(buf)) {
      memcpy(buf, string, len);
      buf[len] = '\0';
      string = buf;
   } else
      string = temp = strndup(string, len);

   VkDebugUtilsLabelEXT label = {
      VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT, NULL,
      string,
      { 0 }
   };
   screen->vk.CmdInsertDebugUtilsLabelEXT(ctx->bs->cmdbuf, &label);
   free(temp);
}

static void
zink_tc_parse_dsa(void *state, struct tc_renderpass_info *info)
{
   struct zink_depth_stencil_alpha_state *cso = state;
   info->zsbuf_write_dsa |= (cso->hw_state.depth_write || cso->hw_state.stencil_test);
   info->zsbuf_read_dsa |= (cso->hw_state.depth_test || cso->hw_state.stencil_test);
   /* TODO: if zsbuf fbfetch is ever supported */
}

static void
zink_tc_parse_fs(void *state, struct tc_renderpass_info *info)
{
   struct zink_shader *zs = state;
   info->zsbuf_write_fs |= zs->info.outputs_written & (BITFIELD64_BIT(FRAG_RESULT_DEPTH) | BITFIELD64_BIT(FRAG_RESULT_STENCIL));
   /* TODO: if >1 fbfetch attachment is ever supported */
   info->cbuf_fbfetch |= zs->info.fs.uses_fbfetch_output ? BITFIELD_BIT(0) : 0;
}

void
zink_parse_tc_info(struct zink_context *ctx)
{
   struct tc_renderpass_info *info = &ctx->dynamic_fb.tc_info;
   /* reset cso info first */
   info->data16[2] = 0;
   if (ctx->gfx_stages[MESA_SHADER_FRAGMENT])
      zink_tc_parse_fs(ctx->gfx_stages[MESA_SHADER_FRAGMENT], info);
   if (ctx->dsa_state)
      zink_tc_parse_dsa(ctx->dsa_state, info);
   if (ctx->zsbuf_unused == zink_is_zsbuf_used(ctx))
      ctx->rp_layout_changed = true;
}

struct pipe_context *
zink_context_create(struct pipe_screen *pscreen, void *priv, unsigned flags)
{
   struct zink_screen *screen = zink_screen(pscreen);
   struct zink_context *ctx = rzalloc(NULL, struct zink_context);
   bool is_copy_only = (flags & ZINK_CONTEXT_COPY_ONLY) > 0;
   bool is_compute_only = (flags & PIPE_CONTEXT_COMPUTE_ONLY) > 0;
   bool is_robust = (flags & PIPE_CONTEXT_ROBUST_BUFFER_ACCESS) > 0;
   if (!ctx)
      goto fail;

   ctx->flags = flags;
   ctx->pipeline_changed[0] = ctx->pipeline_changed[1] = true;
   ctx->gfx_pipeline_state.dirty = true;
   ctx->gfx_pipeline_state.dyn_state2.vertices_per_patch = 1;
   ctx->gfx_pipeline_state.uses_dynamic_stride = screen->info.have_EXT_extended_dynamic_state ||
                                                 screen->info.have_EXT_vertex_input_dynamic_state;
#if defined(MVK_VERSION)
   ctx->gfx_pipeline_state.uses_dynamic_stride = ctx->gfx_pipeline_state.uses_dynamic_stride && screen->have_dynamic_state_vertex_input_binding_stride;
#endif
   ctx->compute_pipeline_state.dirty = true;
   ctx->rp_changed = true;
   ctx->sample_mask_changed = true;
   ctx->gfx_pipeline_state.gfx_prim_mode = MESA_PRIM_COUNT;
   ctx->gfx_pipeline_state.shader_rast_prim = MESA_PRIM_COUNT;
   ctx->gfx_pipeline_state.rast_prim = MESA_PRIM_COUNT;

   zink_init_draw_functions(ctx, screen);
   zink_init_grid_functions(ctx);

   ctx->base.screen = pscreen;
   ctx->base.priv = priv;

   ctx->base.destroy = zink_context_destroy;
   ctx->base.set_debug_callback = zink_set_debug_callback;
   ctx->base.get_device_reset_status = zink_get_device_reset_status;
   ctx->base.set_device_reset_callback = zink_set_device_reset_callback;

   zink_context_state_init(&ctx->base);

   ctx->base.create_sampler_state = zink_create_sampler_state;
   ctx->base.bind_sampler_states = screen->info.have_EXT_non_seamless_cube_map ? zink_bind_sampler_states : zink_bind_sampler_states_nonseamless;
   ctx->base.delete_sampler_state = zink_delete_sampler_state;

   ctx->base.create_sampler_view = zink_create_sampler_view;
   ctx->base.set_sampler_views = zink_set_sampler_views;
   ctx->base.sampler_view_destroy = zink_sampler_view_destroy;
   ctx->base.sampler_view_release = u_default_sampler_view_release;
   ctx->base.get_sample_position = zink_get_sample_position;
   ctx->base.set_sample_locations = zink_set_sample_locations;

   zink_program_init(ctx);

   if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
      ctx->base.set_constant_buffer = zink_set_constant_buffer_db;
      ctx->base.set_shader_buffers = zink_set_shader_buffers_db;
   } else {
      ctx->base.set_constant_buffer = zink_set_constant_buffer_lazy;
      ctx->base.set_shader_buffers = zink_set_shader_buffers_lazy;
   }
   ctx->base.set_polygon_stipple = zink_set_polygon_stipple;
   if (screen->info.have_EXT_vertex_input_dynamic_state && screen->info.have_EXT_extended_dynamic_state)
      ctx->base.set_vertex_buffers = zink_set_vertex_buffers_optimal;
   else
      ctx->base.set_vertex_buffers = zink_set_vertex_buffers;
   ctx->base.set_viewport_states = zink_set_viewport_states;
   ctx->base.set_scissor_states = zink_set_scissor_states;
   ctx->base.set_inlinable_constants = zink_set_inlinable_constants;
   ctx->base.set_shader_images = zink_set_shader_images;
   ctx->base.set_framebuffer_state = zink_set_framebuffer_state;
   ctx->base.set_stencil_ref = zink_set_stencil_ref;
   ctx->base.set_blend_color = zink_set_blend_color;
   ctx->base.set_tess_state = zink_set_tess_state;
   ctx->base.set_patch_vertices = zink_set_patch_vertices;

   ctx->base.set_min_samples = zink_set_min_samples;
   ctx->gfx_pipeline_state.min_samples = 0;
   ctx->base.set_sample_mask = zink_set_sample_mask;
   ctx->gfx_pipeline_state.sample_mask = UINT32_MAX;

   ctx->base.clear = zink_clear;
   ctx->base.clear_texture = zink_clear_texture_dynamic;
   ctx->base.clear_buffer = zink_clear_buffer;
   ctx->base.clear_render_target = zink_clear_render_target;
   ctx->base.clear_depth_stencil = zink_clear_depth_stencil;

   ctx->base.create_fence_fd = zink_create_fence_fd;
   ctx->base.fence_server_sync = zink_fence_server_sync;
   ctx->base.fence_server_signal = zink_fence_server_signal;
   ctx->base.flush = zink_flush;
   ctx->base.memory_barrier = zink_memory_barrier;
   ctx->base.texture_barrier = zink_texture_barrier;
   ctx->base.evaluate_depth_buffer = zink_evaluate_depth_buffer;

   ctx->base.resource_commit = zink_resource_commit;
   ctx->base.resource_copy_region = zink_resource_copy_region;
   ctx->base.image_copy_buffer = zink_image_copy_buffer;
   ctx->base.blit = zink_blit;
   ctx->base.create_stream_output_target = zink_create_stream_output_target;
   ctx->base.stream_output_target_destroy = zink_stream_output_target_destroy;

   ctx->base.set_stream_output_targets = zink_set_stream_output_targets;
   ctx->base.flush_resource = zink_flush_resource;
   if (screen->info.have_KHR_buffer_device_address)
      ctx->base.set_global_binding = zink_set_global_binding;

   ctx->base.emit_string_marker = zink_emit_string_marker;

   zink_context_resource_init(&ctx->base);
   zink_context_query_init(&ctx->base);

   util_queue_fence_init(&ctx->flush_fence);
   util_queue_fence_init(&ctx->unsync_fence);

   list_inithead(&ctx->query_pools);
   _mesa_set_init(&ctx->update_barriers[0][0], ctx, _mesa_hash_pointer, _mesa_key_pointer_equal);
   _mesa_set_init(&ctx->update_barriers[1][0], ctx, _mesa_hash_pointer, _mesa_key_pointer_equal);
   _mesa_set_init(&ctx->update_barriers[0][1], ctx, _mesa_hash_pointer, _mesa_key_pointer_equal);
   _mesa_set_init(&ctx->update_barriers[1][1], ctx, _mesa_hash_pointer, _mesa_key_pointer_equal);
   ctx->need_barriers[0] = &ctx->update_barriers[0][0];
   ctx->need_barriers[1] = &ctx->update_barriers[1][0];

   slab_create_child(&ctx->transfer_pool, &screen->transfer_pool);
   slab_create_child(&ctx->transfer_pool_unsync, &screen->transfer_pool);

   ctx->base.stream_uploader = u_upload_create_default(&ctx->base);
   ctx->base.const_uploader = u_upload_create_default(&ctx->base);
   for (int i = 0; i < ARRAY_SIZE(ctx->fb_clears); i++)
      util_dynarray_init(&ctx->fb_clears[i].clears, ctx);

   if (!is_copy_only) {
      ctx->blitter = util_blitter_create(&ctx->base);
      if (!ctx->blitter)
         goto fail;
      if (screen->driver_workarounds.inconsistent_interpolation)
         ctx->blitter->draw_rectangle = zink_draw_rectangle;
   }

   zink_set_last_vertex_key(ctx)->last_vertex_stage = true;
   ctx->gfx_pipeline_state.shader_keys.last_vertex.key.vs_base.last_vertex_stage = true;
   zink_set_tcs_key_patches(ctx, 1);
   if (!screen->optimal_keys) {
      ctx->gfx_pipeline_state.shader_keys.key[MESA_SHADER_VERTEX].size = sizeof(struct zink_vs_key_base);
      ctx->gfx_pipeline_state.shader_keys.key[MESA_SHADER_TESS_EVAL].size = sizeof(struct zink_vs_key_base);
      ctx->gfx_pipeline_state.shader_keys.key[MESA_SHADER_TESS_CTRL].size = sizeof(struct zink_tcs_key);
      ctx->gfx_pipeline_state.shader_keys.key[MESA_SHADER_GEOMETRY].size = sizeof(struct zink_gs_key);
      ctx->gfx_pipeline_state.shader_keys.key[MESA_SHADER_FRAGMENT].size = sizeof(struct zink_fs_key);

      /* this condition must be updated if new fields are added to zink_cs_key */
      if (screen->driver_compiler_workarounds.lower_robustImageAccess2)
    	  ctx->compute_pipeline_state.key.size = sizeof(struct zink_cs_key);

      if (is_robust && screen->driver_compiler_workarounds.lower_robustImageAccess2) {
         ctx->compute_pipeline_state.key.key.cs.robust_access = true;
         for (gl_shader_stage pstage = MESA_SHADER_VERTEX; pstage < MESA_SHADER_FRAGMENT; pstage++)
            ctx->gfx_pipeline_state.shader_keys.key[pstage].key.vs_base.robust_access = true;
         ctx->gfx_pipeline_state.shader_keys.key[MESA_SHADER_FRAGMENT].key.fs.robust_access = true;
      }
   }
   for (unsigned i = 0; i < ARRAY_SIZE(ctx->rendering_state_cache); i++)
      _mesa_set_init(&ctx->rendering_state_cache[i], ctx, hash_rendering_state, equals_rendering_state);
   ctx->dynamic_fb.info.pColorAttachments = ctx->dynamic_fb.attachments;
   ctx->dynamic_fb.info.sType = VK_STRUCTURE_TYPE_RENDERING_INFO;

   bool general_layout = screen->driver_workarounds.general_layout;
   for (unsigned i = 0; i < ARRAY_SIZE(ctx->dynamic_fb.attachments); i++) {
      VkRenderingAttachmentInfo *att = &ctx->dynamic_fb.attachments[i];
      att->sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO;
      att->imageLayout = general_layout ? VK_IMAGE_LAYOUT_GENERAL : VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
      att->resolveImageLayout = general_layout ? VK_IMAGE_LAYOUT_GENERAL : VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
      att->storeOp = VK_ATTACHMENT_STORE_OP_STORE;
      if (screen->info.have_KHR_unified_image_layouts && screen->info.have_EXT_attachment_feedback_loop_layout)
         att->pNext = &ctx->dynamic_fb.fbfetch_att[i];
   }
   for (unsigned i = 0; i < ARRAY_SIZE(ctx->dynamic_fb.fbfetch_att); i++) {
      ctx->dynamic_fb.fbfetch_att[i] = (VkAttachmentFeedbackLoopInfoEXT){
         VK_STRUCTURE_TYPE_ATTACHMENT_FEEDBACK_LOOP_INFO_EXT,
         NULL,
         VK_FALSE
      };
   }

   ctx->gfx_pipeline_state.rendering_info.sType = VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO;
   ctx->gfx_pipeline_state.rendering_info.pColorAttachmentFormats = ctx->gfx_pipeline_state.rendering_formats;
   ctx->gfx_pipeline_state.feedback_loop = screen->driver_workarounds.always_feedback_loop;
   ctx->gfx_pipeline_state.feedback_loop_zs = screen->driver_workarounds.always_feedback_loop_zs;

   const uint32_t data[] = {0};
   if (!is_copy_only) {
      ctx->dummy_xfb_buffer = pipe_buffer_create(&screen->base,
         PIPE_BIND_STREAM_OUTPUT, PIPE_USAGE_IMMUTABLE, sizeof(data));
      if (!ctx->dummy_xfb_buffer)
         goto fail;
   }
   if (!is_copy_only) {
      if (!zink_descriptors_init(ctx))
         goto fail;
   }

   if (!is_copy_only && !is_compute_only) {
      ctx->base.create_texture_handle = zink_create_texture_handle;
      ctx->base.delete_texture_handle = zink_delete_texture_handle;
      ctx->base.make_texture_handle_resident = zink_make_texture_handle_resident;
      ctx->base.create_image_handle = zink_create_image_handle;
      ctx->base.delete_image_handle = zink_delete_image_handle;
      ctx->base.make_image_handle_resident = zink_make_image_handle_resident;
      for (unsigned i = 0; i < 2; i++) {
         _mesa_hash_table_init(&ctx->di.bindless[i].img_handles, ctx, _mesa_hash_pointer, _mesa_key_pointer_equal);
         _mesa_hash_table_init(&ctx->di.bindless[i].tex_handles, ctx, _mesa_hash_pointer, _mesa_key_pointer_equal);

         /* allocate 1024 slots and reserve slot 0 */
         util_idalloc_init(&ctx->di.bindless[i].tex_slots, ZINK_MAX_BINDLESS_HANDLES);
         util_idalloc_alloc(&ctx->di.bindless[i].tex_slots);
         util_idalloc_init(&ctx->di.bindless[i].img_slots, ZINK_MAX_BINDLESS_HANDLES);
         util_idalloc_alloc(&ctx->di.bindless[i].img_slots);
         if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
            ctx->di.bindless[i].db.buffer_infos = malloc(sizeof(VkDescriptorAddressInfoEXT) * ZINK_MAX_BINDLESS_HANDLES);
            if (!ctx->di.bindless[i].db.buffer_infos) {
               mesa_loge("ZINK: failed to allocate ctx->di.bindless[%d].db.buffer_infos!",i);
               goto fail;
            }
            for (unsigned j = 0; j < ZINK_MAX_BINDLESS_HANDLES; j++) {
               ctx->di.bindless[i].db.buffer_infos[j].sType = VK_STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT;
               ctx->di.bindless[i].db.buffer_infos[j].pNext = NULL;
            }
         } else {
            ctx->di.bindless[i].t.buffer_infos = malloc(sizeof(VkBufferView) * ZINK_MAX_BINDLESS_HANDLES);
            if (!ctx->di.bindless[i].t.buffer_infos) {
               mesa_loge("ZINK: failed to allocate ctx->di.bindless[%d].t.buffer_infos!",i);
               goto fail;
            }
         }
         ctx->di.bindless[i].img_infos = malloc(sizeof(VkDescriptorImageInfo) * ZINK_MAX_BINDLESS_HANDLES);
         if (!ctx->di.bindless[i].img_infos) {
            mesa_loge("ZINK: failed to allocate ctx->di.bindless[%d].img_infos!",i);
            goto fail;
         }
         util_dynarray_init(&ctx->di.bindless[i].updates, NULL);
         util_dynarray_init(&ctx->di.bindless[i].resident, NULL);
      }
   }

   zink_start_batch(ctx);
   if (!ctx->bs)
      goto fail;

   if (screen->compact_descriptors)
      ctx->invalidate_descriptor_state = zink_context_invalidate_descriptor_state_compact;
   else
      ctx->invalidate_descriptor_state = zink_context_invalidate_descriptor_state;
   if (!is_copy_only && !is_compute_only) {
      pipe_buffer_write_nooverlap(&ctx->base, ctx->dummy_xfb_buffer, 0, sizeof(data), data);
      if (screen->info.have_EXT_color_write_enable)
         reapply_color_write(ctx);

      /* set on startup just to avoid validation errors if a draw comes through without
      * a tess shader later
      */
      if (screen->info.dynamic_state2_feats.extendedDynamicState2PatchControlPoints) {
         VKCTX(CmdSetPatchControlPointsEXT)(ctx->bs->cmdbuf, 1);
         VKCTX(CmdSetPatchControlPointsEXT)(ctx->bs->reordered_cmdbuf, 1);
      }
   }
   if (!is_copy_only) {
      for (unsigned i = 0; i < MESA_SHADER_STAGES; i++) {
         /* need to update these based on screen config for null descriptors */
         for (unsigned j = 0; j < ARRAY_SIZE(ctx->di.t.ubos[i]); j++) {
            if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
               update_descriptor_state_ubo_db(ctx, i, j, NULL);
               ctx->di.db.ubos[i][j].sType = VK_STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT;
            } else {
               update_descriptor_state_ubo_lazy(ctx, i, j, NULL);
            }
         }
         for (unsigned j = 0; j < ARRAY_SIZE(ctx->di.textures[i]); j++) {
            update_descriptor_state_sampler(ctx, i, j, NULL);
            if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB)
               ctx->di.db.tbos[i][j].sType = VK_STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT;
         }
         for (unsigned j = 0; j < ARRAY_SIZE(ctx->di.t.ssbos[i]); j++) {
            if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB) {
               update_descriptor_state_ssbo_db(ctx, i, j, NULL);
               ctx->di.db.ssbos[i][j].sType = VK_STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT;
            } else {
               update_descriptor_state_ssbo_lazy(ctx, i, j, NULL);
            }
         }
         for (unsigned j = 0; j < ARRAY_SIZE(ctx->di.images[i]); j++) {
            update_descriptor_state_image(ctx, i, j, NULL);
            if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB)
               ctx->di.db.texel_images[i][j].sType = VK_STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT;
         }
      }

      p_atomic_inc(&screen->base.num_contexts);
   }

   zink_select_draw_vbo(ctx);
   zink_select_launch_grid(ctx);

   if (!is_copy_only && zink_debug & ZINK_DEBUG_SHADERDB) {
      if (!screen->info.have_EXT_vertex_input_dynamic_state) {
         struct pipe_vertex_element velems[32] = {0};
         for (unsigned i = 0; i < ARRAY_SIZE(velems); i++)
            velems[i].src_format = PIPE_FORMAT_R8G8B8_UNORM;
         void *state = ctx->base.create_vertex_elements_state(&ctx->base, ARRAY_SIZE(velems), velems);
         ctx->base.bind_vertex_elements_state(&ctx->base, state);
      }
      ctx->gfx_pipeline_state.sample_mask = BITFIELD_MASK(32);

      struct pipe_resource templ = {0};
      templ.width0 = 256;
      templ.height0 = 256;
      templ.depth0 = 1;
      templ.format = PIPE_FORMAT_R8G8B8A8_UNORM;
      templ.target = PIPE_TEXTURE_2D;
      templ.bind = PIPE_BIND_RENDER_TARGET | PIPE_BIND_SAMPLER_VIEW;

      struct pipe_resource *pres = pscreen->resource_create(pscreen, &templ);
      struct pipe_framebuffer_state fb = {0};
      fb.nr_cbufs = 1;
      fb.cbufs[0].texture = pres;
      fb.cbufs[0].format = PIPE_FORMAT_R8G8B8A8_UNORM;
      fb.width = fb.height = 256;
      ctx->base.set_framebuffer_state(&ctx->base, &fb);
      pipe_resource_reference(&pres, NULL);
      ctx->disable_fs = true;
      struct pipe_depth_stencil_alpha_state dsa = {0};
      void *state = ctx->base.create_depth_stencil_alpha_state(&ctx->base, &dsa);
      ctx->base.bind_depth_stencil_alpha_state(&ctx->base, state);

      struct pipe_blend_state blend = {
         .rt[0].colormask = 0xF
      };

      void *blend_state = ctx->base.create_blend_state(&ctx->base, &blend);
      ctx->base.bind_blend_state(&ctx->base, blend_state);

      zink_batch_rp(ctx);
   }

   if (is_compute_only || zink_debug & ZINK_DEBUG_NOREORDER)
      ctx->no_reorder = true;

   if (!(flags & PIPE_CONTEXT_PREFER_THREADED) || flags & PIPE_CONTEXT_COMPUTE_ONLY) {
      return &ctx->base;
   }

   struct threaded_context *tc = (struct threaded_context*)threaded_context_create(&ctx->base, &screen->transfer_pool,
                                                     zink_context_replace_buffer_storage,
                                                     &(struct threaded_context_options){
                                                        .create_fence = zink_create_tc_fence_for_tc,
                                                        .is_resource_busy = zink_context_is_resource_busy,
                                                        .driver_calls_flush_notify = !screen->driver_workarounds.track_renderpasses,
                                                        .unsynchronized_get_device_reset_status = true,
                                                        .unsynchronized_texture_subdata = true,
                                                        .parse_renderpass_info = screen->driver_workarounds.track_renderpasses,
                                                        .dsa_parse = zink_tc_parse_dsa,
                                                        .fs_parse = zink_tc_parse_fs,
                                                     },
                                                     &ctx->tc);

   if (tc && (struct zink_context*)tc != ctx) {
      ctx->track_renderpasses = screen->driver_workarounds.track_renderpasses;
      threaded_context_init_bytes_mapped_limit(tc, 4);
      ctx->base.set_context_param = zink_set_context_param;
   }

   return (struct pipe_context*)tc;

fail:
   if (ctx)
      zink_context_destroy(&ctx->base);
   return NULL;
}

struct zink_context *
zink_tc_context_unwrap(struct pipe_context *pctx)
{
   /* need to get the actual zink_context, not the threaded context */
   pctx = threaded_context_unwrap_sync(pctx);
   pctx = trace_get_possibly_threaded_context(pctx);
   return zink_context(pctx);
}

struct feedback_loop_surface {
   uint16_t first_layer;
   uint16_t last_layer;
   uint16_t level;
   uint16_t idx;
};

static bool
add_implicit_feedback_loop(struct zink_context *ctx, struct zink_resource *res)
{
   /* can only feedback loop with fb+sampler bind; image bind must be GENERAL */
   if (!res->fb_bind_count || !res->sampler_bind_count[0] || res->image_bind_count[0])
      return false;
   if (!(res->aspect & VK_IMAGE_ASPECT_COLOR_BIT) && !zink_is_zsbuf_write(ctx))
      /* if zsbuf isn't used then it effectively has no fb binds */
      /* if zsbuf isn't written to then it'll be fine with read-only access */
      return false;

   /* build attachment array for miplevel/layer checks */
   struct feedback_loop_surface psurfs[PIPE_MAX_COLOR_BUFS];
   unsigned surf_idx = 0;
   u_foreach_bit(idx, res->fb_binds) {
      const struct pipe_surface *psurf;
      if (idx == PIPE_MAX_COLOR_BUFS)
         psurf = &ctx->fb_state.zsbuf;
      else
         psurf = &ctx->fb_state.cbufs[idx];
      if (psurf->texture == &res->base.b) {
         psurfs[surf_idx++] = (struct feedback_loop_surface){
            .first_layer = psurf->first_layer,
            .last_layer = psurf->last_layer,
            .level = psurf->level,
            .idx = idx,
         };
      }
   }

   uint32_t is_feedback = 0;
   /* avoid false positives when a texture is bound but not used */
   u_foreach_bit(vkstage, res->gfx_barrier) {
      VkPipelineStageFlags vkstagebit = BITFIELD_BIT(vkstage);
      if (vkstagebit < VK_PIPELINE_STAGE_VERTEX_SHADER_BIT || vkstagebit > VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
         continue;

      /* in-range VkPipelineStageFlagBits can be converted to VkShaderStageFlags with a bitshift */
      gl_shader_stage stage = vk_to_mesa_shader_stage((VkShaderStageFlagBits)(vkstagebit >> 3));
      /* check against bound stages */
      if (!ctx->gfx_stages[stage])
         continue;

      /* check shader texture usage against resource's sampler binds */
      uint32_t texuse = res->sampler_binds[stage] & ctx->gfx_stages[stage]->info.textures_used[0];
      if (!texuse)
         continue;

      /* check miplevel/layer: a feedback loop only exists if these overlap between fb and sampler */
      u_foreach_bit(slot, texuse) {
         struct pipe_sampler_view *sv = ctx->sampler_views[stage][slot];

         for (unsigned i = 0; i < surf_idx; i++) {
            if (sv->u.tex.first_level > psurfs[i].level || sv->u.tex.last_level < psurfs[i].level)
               continue;
            if (sv->u.tex.first_layer > psurfs[i].last_layer || sv->u.tex.last_layer < psurfs[i].first_layer)
               continue;
            is_feedback |= BITFIELD_BIT(psurfs[i].idx);
         }
      }
   }
   if (!is_feedback)
      return false;
   if (ctx->feedback_loops & is_feedback)
      /* already added */
      return true;
   /* new feedback loop detected */
   if (res->aspect == VK_IMAGE_ASPECT_COLOR_BIT) {
      if (!ctx->gfx_pipeline_state.feedback_loop)
         ctx->gfx_pipeline_state.dirty = true;
      ctx->gfx_pipeline_state.feedback_loop = true;
   } else {
      if (!ctx->gfx_pipeline_state.feedback_loop_zs)
         ctx->gfx_pipeline_state.dirty = true;
      ctx->gfx_pipeline_state.feedback_loop_zs = true;
   }
   ctx->rp_layout_changed = true;
   ctx->feedback_loops |= is_feedback;
   if (zink_screen(ctx->base.screen)->info.have_KHR_unified_image_layouts && zink_screen(ctx->base.screen)->info.have_EXT_attachment_feedback_loop_layout) {
      u_foreach_bit(idx, is_feedback) {
         ctx->dynamic_fb.fbfetch_att[idx].feedbackLoopEnable = VK_TRUE;
      }
   } else {
      u_foreach_bit(idx, is_feedback) {
         if (zink_screen(ctx->base.screen)->info.have_EXT_attachment_feedback_loop_layout)
            ctx->dynamic_fb.attachments[idx].imageLayout = VK_IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT;
         else
            ctx->dynamic_fb.attachments[idx].imageLayout = VK_IMAGE_LAYOUT_GENERAL;
      }
   }
   update_feedback_loop_dynamic_state(ctx);
   return true;
}

void
zink_update_barriers(struct zink_context *ctx, bool is_compute,
                     struct pipe_resource *index, struct pipe_resource *indirect, struct pipe_resource *indirect_draw_count)
{
   assert(!ctx->blitting);
   if (!ctx->need_barriers[is_compute]->entries)
      return;
   struct set *need_barriers = ctx->need_barriers[is_compute];
   ctx->barrier_set_idx[is_compute] = !ctx->barrier_set_idx[is_compute];
   ctx->need_barriers[is_compute] = &ctx->update_barriers[is_compute][ctx->barrier_set_idx[is_compute]];
   bool general_layout = zink_screen(ctx->base.screen)->driver_workarounds.general_layout;
   ASSERTED bool check_rp = ctx->in_rp && ctx->dynamic_fb.tc_info.zsbuf_invalidate;
   set_foreach(need_barriers, he) {
      struct zink_resource *res = (struct zink_resource *)he->key;
      if (res->bind_count[is_compute]) {
         VkPipelineStageFlagBits pipeline = is_compute ? VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT : res->gfx_barrier;
         if (res->base.b.target == PIPE_BUFFER) {
            zink_screen(ctx->base.screen)->buffer_barrier(ctx, res, res->barrier_access[is_compute], pipeline);
         } else if (general_layout) {
            /* let sync figure this out */
            zink_screen(ctx->base.screen)->image_barrier(ctx, res, VK_IMAGE_LAYOUT_GENERAL, res->barrier_access[is_compute], pipeline);
         } else {
            bool is_feedback = is_compute ? false : add_implicit_feedback_loop(ctx, res);
            VkImageLayout layout = zink_descriptor_util_image_layout_eval(ctx, res, is_compute);
            /* GENERAL is only used for feedback loops and storage image binds */
            if (is_feedback || layout != VK_IMAGE_LAYOUT_GENERAL || res->image_bind_count[is_compute])
               zink_screen(ctx->base.screen)->image_barrier(ctx, res, layout, res->barrier_access[is_compute], pipeline);
            assert(!check_rp || check_rp == ctx->in_rp);
            if (!general_layout && is_feedback)
               update_res_sampler_layouts(ctx, res);
         }
         if (zink_resource_access_is_write(res->barrier_access[is_compute]) ||
             // TODO: figure out a way to link up layouts between unordered and main cmdbuf
             res->base.b.target != PIPE_BUFFER)
            res->obj->unordered_write = false;
         res->obj->unordered_read = false;
         /* always barrier on draw if this resource has either multiple image write binds or
          * image write binds and image read binds
          */
         if (res->write_bind_count[is_compute] && res->bind_count[is_compute] > 1)
            _mesa_set_add_pre_hashed(ctx->need_barriers[is_compute], he->hash, res);
      }
      _mesa_set_remove(need_barriers, he);
      if (!need_barriers->entries)
         break;
   }
}

/**
 * Emits a debug marker in the cmd stream to be captured by perfetto during
 * execution on the GPU.
 */
bool
zink_cmd_debug_marker_begin(struct zink_context *ctx, VkCommandBuffer cmdbuf, const char *fmt, ...)
{
   if (!zink_tracing)
      return false;

   char *name;
   va_list va;
   va_start(va, fmt);
   int ret = vasprintf(&name, fmt, va);
   va_end(va);

   if (ret == -1)
      return false;

   VkDebugUtilsLabelEXT info = { 0 };
   info.sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT;
   info.pLabelName = name;

   VKCTX(CmdBeginDebugUtilsLabelEXT)(cmdbuf ? cmdbuf : ctx->bs->cmdbuf, &info);

   free(name);
   return true;
}

void
zink_cmd_debug_marker_end(struct zink_context *ctx, VkCommandBuffer cmdbuf, bool emitted)
{
   if (emitted)
      VKCTX(CmdEndDebugUtilsLabelEXT)(cmdbuf);
}

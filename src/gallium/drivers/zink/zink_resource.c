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

#include "zink_resource.h"

#include "zink_batch.h"
#include "zink_clear.h"
#include "zink_context.h"
#include "zink_fence.h"
#include "zink_format.h"
#include "zink_program.h"
#include "zink_screen.h"
#include "zink_surface.h"
#include "zink_kopper.h"

#ifdef VK_USE_PLATFORM_METAL_EXT
#include "QuartzCore/CAMetalLayer.h"
#endif

#include "vk_format.h"
#include "util/u_blitter.h"
#include "util/u_debug.h"
#include "util/format/u_format.h"
#include "util/u_transfer_helper.h"
#include "util/u_inlines.h"
#include "util/u_memory.h"
#include "util/u_upload_mgr.h"
#include "util/os_file.h"
#include "frontend/winsys_handle.h"

#if !defined(__APPLE__)
#define ZINK_USE_DMABUF
#endif

#if defined(ZINK_USE_DMABUF) && !defined(_WIN32)
#include "drm-uapi/drm_fourcc.h"
#else
/* these won't actually be used */
#define DRM_FORMAT_MOD_INVALID 0
#define DRM_FORMAT_MOD_LINEAR 0
#endif

#ifdef __APPLE__
#include "MoltenVK/mvk_vulkan.h"
// Source of MVK_VERSION
#include "MoltenVK/mvk_config.h"
#endif /* __APPLE__ */

#define ZINK_EXTERNAL_MEMORY_HANDLE 999



struct zink_debug_mem_entry {
   uint32_t count;
   uint64_t size;
   const char *name;
};

static const char *
zink_debug_mem_add(struct zink_screen *screen, uint64_t size, const char *name)
{
   assert(name);

   simple_mtx_lock(&screen->debug_mem_lock);
   struct hash_entry *entry = _mesa_hash_table_search(screen->debug_mem_sizes, name);
   struct zink_debug_mem_entry *debug_bos;

   if (!entry) {
      debug_bos = calloc(1, sizeof(struct zink_debug_mem_entry));
      debug_bos->name = strdup(name);
      _mesa_hash_table_insert(screen->debug_mem_sizes, debug_bos->name, debug_bos);
   } else {
      debug_bos = (struct zink_debug_mem_entry *) entry->data;
   }

   debug_bos->count++;
   debug_bos->size += align(size, 4096);
   simple_mtx_unlock(&screen->debug_mem_lock);

   return debug_bos->name;
}

static void
zink_debug_mem_del(struct zink_screen *screen, struct zink_bo *bo)
{
   simple_mtx_lock(&screen->debug_mem_lock);
   struct hash_entry *entry = _mesa_hash_table_search(screen->debug_mem_sizes, bo->name);
   /* If we're finishing the BO, it should have been added already */
   assert(entry);

   struct zink_debug_mem_entry *debug_bos = entry->data;
   debug_bos->count--;
   debug_bos->size -= align(zink_bo_get_size(bo), 4096);
   if (!debug_bos->count) {
      _mesa_hash_table_remove(screen->debug_mem_sizes, entry);
      free((void*)debug_bos->name);
      free(debug_bos);
   }
   simple_mtx_unlock(&screen->debug_mem_lock);
}

static int
debug_bos_count_compare(const void *in_a, const void *in_b)
{
   struct zink_debug_mem_entry *a = *(struct zink_debug_mem_entry **)in_a;
   struct zink_debug_mem_entry *b = *(struct zink_debug_mem_entry **)in_b;
   return a->count - b->count;
}

void
zink_debug_mem_print_stats(struct zink_screen *screen)
{
   simple_mtx_lock(&screen->debug_mem_lock);

   /* Put the HT's sizes data in an array so we can sort by number of allocations. */
   struct util_dynarray dyn;
   util_dynarray_init(&dyn, NULL);

   uint32_t size = 0;
   uint32_t count = 0;
   hash_table_foreach(screen->debug_mem_sizes, entry)
   {
      struct zink_debug_mem_entry *debug_bos = entry->data;
      util_dynarray_append(&dyn, struct zink_debug_mem_entry *, debug_bos);
      size += debug_bos->size / 1024;
      count += debug_bos->count;
   }

   qsort(dyn.data,
         util_dynarray_num_elements(&dyn, struct zink_debug_mem_entry *),
         sizeof(struct zink_debug_mem_entryos_entry *), debug_bos_count_compare);

   util_dynarray_foreach(&dyn, struct zink_debug_mem_entry *, entryp)
   {
      struct zink_debug_mem_entry *debug_bos = *entryp;
      mesa_logi("%30s: %4d bos, %lld kb\n", debug_bos->name, debug_bos->count,
                (long long) (debug_bos->size / 1024));
   }

   mesa_logi("submitted %d bos (%d MB)\n", count, DIV_ROUND_UP(size, 1024));

   util_dynarray_fini(&dyn);

   simple_mtx_unlock(&screen->debug_mem_lock);
}

void
zink_resource_image_hic_transition(struct zink_screen *screen, struct zink_resource *res, VkImageLayout layout)
{
   if (!(res->obj->vkusage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT))
      return;
   VkHostImageLayoutTransitionInfoEXT t = {
      VK_STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT,
      NULL,
      res->obj->image,
      res->layout,
      layout,
      {res->aspect, 0, VK_REMAINING_MIP_LEVELS, 0, VK_REMAINING_ARRAY_LAYERS}
   };
   VKSCR(TransitionImageLayoutEXT)(screen->dev, 1, &t);
   res->layout = layout;
}

static bool
equals_bufferview_key(const void *a, const void *b)
{
   return memcmp(a, b, sizeof(struct zink_bufferview_key)) == 0;
}

static void
zink_transfer_flush_region(struct pipe_context *pctx,
                           struct pipe_transfer *ptrans,
                           const struct pipe_box *box);

void
debug_describe_zink_resource_object(char *buf, const struct zink_resource_object *ptr)
{
   sprintf(buf, "zink_resource_object");
}

void
zink_destroy_resource_surface_cache(struct zink_screen *screen, struct set *ht, bool is_buffer)
{
   if (is_buffer) {
      set_foreach_remove(ht, he) {
         struct zink_buffer_view *bv = (void*)he->key;
         VKSCR(DestroyBufferView)(screen->dev, bv->buffer_view, NULL);
         FREE(bv);
      }
      ralloc_free(ht->table);
   } else {
      set_foreach_remove(ht, he) {
         struct zink_surface *surf = (void*)he->key;
         VKSCR(DestroyImageView)(screen->dev, surf->image_view, NULL);
         FREE(surf);
      }
      ralloc_free(ht->table);
   }
}

void
zink_destroy_resource_object(struct zink_screen *screen, struct zink_resource_object *obj)
{
   if (!obj->dt)
      zink_destroy_resource_surface_cache(screen, &obj->surface_cache, obj->is_buffer);
   simple_mtx_destroy(&obj->surface_mtx);
   if (!obj->dt && zink_debug & ZINK_DEBUG_MEM)
      zink_debug_mem_del(screen, obj->bo);
   for (unsigned i = 0; i < ARRAY_SIZE(obj->copies); i++)
      util_dynarray_fini(&obj->copies[i]);
   if (obj->is_buffer) {
      VKSCR(DestroyBuffer)(screen->dev, obj->buffer, NULL);
      VKSCR(DestroyBuffer)(screen->dev, obj->storage_buffer, NULL);
   } else if (obj->dt) {
      zink_kopper_displaytarget_destroy(screen, obj->dt);
   } else if (!obj->is_aux) {
      VKSCR(DestroyImage)(screen->dev, obj->image, NULL);
   } else {
#if defined(ZINK_USE_DMABUF) && !defined(_WIN32)
      close(obj->handle);
#endif
   }

   if (obj->dt) {
      FREE(obj->bo); //this is a dummy struct
   } else
      zink_bo_unref(screen, obj->bo);
   FREE(obj);
}

static void
zink_resource_destroy(struct pipe_screen *pscreen,
                      struct pipe_resource *pres)
{
   struct zink_screen *screen = zink_screen(pscreen);
   struct zink_resource *res = zink_resource(pres);
   /* prevent double-free when unrefing internal surfaces */
   res->base.b.reference.count = 999;
   if (pres->target == PIPE_BUFFER) {
      util_range_destroy(&res->valid_buffer_range);
      util_idalloc_mt_free(&screen->buffer_ids, res->base.buffer_id_unique);
   }

   free(res->modifiers);
   struct pipe_resource *pres_transient = res->transient ? &res->transient->base.b : NULL;
   pipe_resource_reference(&pres_transient, NULL);
   zink_resource_object_reference(screen, &res->obj, NULL);
   threaded_resource_deinit(pres);
   FREE_CL(res);
}

static VkImageAspectFlags
aspect_from_format(enum pipe_format fmt)
{
   if (util_format_is_depth_or_stencil(fmt)) {
      VkImageAspectFlags aspect = 0;
      const struct util_format_description *desc = util_format_description(fmt);
      if (util_format_has_depth(desc))
         aspect |= VK_IMAGE_ASPECT_DEPTH_BIT;
      if (util_format_has_stencil(desc))
         aspect |= VK_IMAGE_ASPECT_STENCIL_BIT;
      return aspect;
   } else
     return VK_IMAGE_ASPECT_COLOR_BIT;
}

static VkBufferCreateInfo
create_bci(struct zink_screen *screen, const struct pipe_resource *templ, unsigned bind)
{
   VkBufferCreateInfo bci;
   bci.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
   bci.pNext = NULL;
   bci.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
   bci.queueFamilyIndexCount = 0;
   bci.pQueueFamilyIndices = NULL;
   bci.size = templ->width0;
   bci.flags = 0;
   assert(bci.size > 0);

   if (bind & ZINK_BIND_DESCRIPTOR) {
      /* gallium sizes are all uint32_t, while the total size of this buffer may exceed that limit */
      bci.usage = 0;
      bci.usage |= VK_BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT |
                   VK_BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT;
   } else {
      bci.usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT |
                  VK_BUFFER_USAGE_TRANSFER_DST_BIT |
                  VK_BUFFER_USAGE_STORAGE_BUFFER_BIT;

      bci.usage |= VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT |
                  VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT |
                  VK_BUFFER_USAGE_VERTEX_BUFFER_BIT |
                  VK_BUFFER_USAGE_INDEX_BUFFER_BIT |
                  VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;

      if (screen->info.have_EXT_transform_feedback) {
         bci.usage |= VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT |
                      VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT;
      }
   }
   if (screen->info.have_KHR_buffer_device_address)
      bci.usage |= VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT;

   if (bind & PIPE_BIND_SHADER_IMAGE)
      bci.usage |= VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT;

   if (bind & PIPE_BIND_QUERY_BUFFER)
      bci.usage |= VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT;

   if (templ->flags & PIPE_RESOURCE_FLAG_SPARSE)
      bci.flags |= VK_BUFFER_CREATE_SPARSE_BINDING_BIT | VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT;
   return bci;
}

static bool
check_ici(struct zink_screen *screen, VkImageCreateInfo *ici, uint64_t modifier)
{
   VkImageFormatProperties image_props;
   VkResult ret;
   bool optimalDeviceAccess = true;
   assert(modifier == DRM_FORMAT_MOD_INVALID ||
          (VKSCR(GetPhysicalDeviceImageFormatProperties2) && screen->info.have_EXT_image_drm_format_modifier));
   if (VKSCR(GetPhysicalDeviceImageFormatProperties2)) {
      VkImageFormatProperties2 props2;
      props2.sType = VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2;
      props2.pNext = NULL;
      VkSamplerYcbcrConversionImageFormatProperties ycbcr_props;
      ycbcr_props.sType = VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES;
      ycbcr_props.pNext = NULL;
      if (screen->info.have_KHR_sampler_ycbcr_conversion)
         props2.pNext = &ycbcr_props;
      VkHostImageCopyDevicePerformanceQueryEXT hic = {
         VK_STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT,
         props2.pNext,
      };
      if (screen->info.have_EXT_host_image_copy && ici->usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT)
         props2.pNext = &hic;
      VkPhysicalDeviceImageFormatInfo2 info;
      info.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2;
      /* possibly VkImageFormatListCreateInfo */
      info.pNext = ici->pNext;
      info.format = ici->format;
      info.type = ici->imageType;
      info.tiling = ici->tiling;
      info.usage = ici->usage;
      info.flags = ici->flags;

      VkPhysicalDeviceImageDrmFormatModifierInfoEXT mod_info;
      if (modifier != DRM_FORMAT_MOD_INVALID) {
         mod_info.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT;
         mod_info.pNext = info.pNext;
         mod_info.drmFormatModifier = modifier;
         mod_info.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
         mod_info.queueFamilyIndexCount = 0;
         mod_info.pQueueFamilyIndices = NULL;
         info.pNext = &mod_info;
      }

      ret = VKSCR(GetPhysicalDeviceImageFormatProperties2)(screen->pdev, &info, &props2);
      image_props = props2.imageFormatProperties;
      if (screen->info.have_EXT_host_image_copy && ici->usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT)
         optimalDeviceAccess = hic.optimalDeviceAccess;
#if defined(MVK_VERSION)
      // MoltenVK cannot allocate a depth buffer with VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT
      // As hic.identicalMemoryLayout is set, it is not needed?
      if (screen->info.have_EXT_host_image_copy && ici->usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT && hic.identicalMemoryLayout)
         optimalDeviceAccess = false;
#endif
   } else
      ret = VKSCR(GetPhysicalDeviceImageFormatProperties)(screen->pdev, ici->format, ici->imageType,
                                                   ici->tiling, ici->usage, ici->flags, &image_props);
   if (ret != VK_SUCCESS)
      return false;
   if (ici->extent.depth > image_props.maxExtent.depth ||
       ici->extent.height > image_props.maxExtent.height ||
       ici->extent.width > image_props.maxExtent.width)
      return false;
   if (ici->mipLevels > image_props.maxMipLevels)
      return false;
   if (ici->arrayLayers > image_props.maxArrayLayers)
      return false;
   if (!(ici->samples & image_props.sampleCounts))
      return false;
   if (!optimalDeviceAccess)
      return false;
   return true;
}

static VkImageUsageFlags
get_image_usage_for_feats(struct zink_screen *screen, VkFormatFeatureFlags2 feats, const struct pipe_resource *templ, unsigned bind, bool *need_extended)
{
   VkImageUsageFlags usage = 0;
   bool is_planar = util_format_get_num_planes(templ->format) > 1;
   *need_extended = false;

   if (bind & ZINK_BIND_TRANSIENT)
      usage |= VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT;
   else {
      /* sadly, gallium doesn't let us know if it'll ever need this, so we have to assume */
      if (is_planar || (feats & VK_FORMAT_FEATURE_TRANSFER_SRC_BIT))
         usage |= VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
      if (is_planar || (feats & VK_FORMAT_FEATURE_TRANSFER_DST_BIT))
         usage |= VK_IMAGE_USAGE_TRANSFER_DST_BIT;
      if (feats & VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT)
         usage |= VK_IMAGE_USAGE_SAMPLED_BIT;

      if ((is_planar || (feats & VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT)) && (bind & PIPE_BIND_SHADER_IMAGE)) {
         assert(templ->nr_samples <= 1 || screen->info.feats.features.shaderStorageImageMultisample);
         usage |= VK_IMAGE_USAGE_STORAGE_BIT;
      }
   }

   if (bind & PIPE_BIND_RENDER_TARGET) {
      if (feats & VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT) {
         usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
         if (!(bind & ZINK_BIND_TRANSIENT) && (bind & (PIPE_BIND_LINEAR | PIPE_BIND_SHARED)) != (PIPE_BIND_LINEAR | PIPE_BIND_SHARED))
            usage |= VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT;
         if (!(bind & ZINK_BIND_TRANSIENT) && screen->info.have_EXT_attachment_feedback_loop_layout)
            usage |= VK_IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT;
      } else {
         /* trust that gallium isn't going to give us anything wild */
         *need_extended = true;
         return 0;
      }
   } else if ((bind & PIPE_BIND_SAMPLER_VIEW) && !util_format_is_depth_or_stencil(templ->format)) {
      if (!(feats & VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT)) {
         /* ensure we can u_blitter this later */
         *need_extended = true;
         return 0;
      }
      usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
   }

   if (bind & PIPE_BIND_DEPTH_STENCIL) {
      if (feats & VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT)
         usage |= VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
      else
         return 0;
      if (screen->info.have_EXT_attachment_feedback_loop_layout && !(bind & ZINK_BIND_TRANSIENT))
         usage |= VK_IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT;
   /* this is unlikely to occur and has been included for completeness */
   } else if (bind & PIPE_BIND_SAMPLER_VIEW && !(usage & VK_IMAGE_USAGE_TRANSFER_DST_BIT)) {
      if (feats & VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT)
         usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
      else
         return 0;
   }

   if (bind & PIPE_BIND_STREAM_OUTPUT)
      usage |= VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT;

   /* Add host transfer if not sparse */
   if (!(templ->flags & PIPE_RESOURCE_FLAG_SPARSE) &&
       screen->info.have_EXT_host_image_copy &&
       feats & VK_FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT)
      usage |= VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT;

   return usage;
}

static VkFormatFeatureFlags
find_modifier_feats(const struct zink_modifier_props *prop, uint64_t modifier)
{
   for (unsigned j = 0; j < prop->drmFormatModifierCount; j++) {
      if (prop->pDrmFormatModifierProperties[j].drmFormatModifier == modifier) {
         return prop->pDrmFormatModifierProperties[j].drmFormatModifierTilingFeatures;
      }
   }
   return 0;
}

/* check HIC optimalness */
static bool
suboptimal_check_ici(struct zink_screen *screen, VkImageCreateInfo *ici, uint64_t mod)
{
   if (check_ici(screen, ici, mod))
      return true;

   ici->usage &= ~VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT;
   if (check_ici(screen, ici, mod))
      return true;

   ici->usage |= VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT;
   return false;
}

/* If the driver can't do mutable with this ICI, then try again after removing mutable (and
 * thus also the list of formats we might might mutate to)
 */
static bool
double_check_ici(struct zink_screen *screen, VkImageCreateInfo *ici, VkImageUsageFlags usage, uint64_t mod, bool require_mutable)
{
   if (!usage)
      return false;

   ici->usage = usage;

   if (ici->usage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT) {
      if (suboptimal_check_ici(screen, ici, mod))
         return true;
   }
   if (check_ici(screen, ici, mod))
      return true;
   if (require_mutable)
      return false;
   const void *pNext = ici->pNext;
   if (pNext) {
      VkBaseOutStructure *prev = NULL;
      VkBaseOutStructure *fmt_list = NULL;
      vk_foreach_struct(strct, (void*)ici->pNext) {
         if (strct->sType == VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO) {
            fmt_list = strct;
            if (prev) {
               prev->pNext = strct->pNext;
            } else {
               ici->pNext = strct->pNext;
            }
            fmt_list->pNext = NULL;
            break;
         }
         prev = strct;
      }
      if (!fmt_list)
         return false;
      ici->flags &= ~VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT;
      if (suboptimal_check_ici(screen, ici, mod))
         return true;
      fmt_list->pNext = (void*)ici->pNext;
      ici->pNext = fmt_list;
      ici->flags |= VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT;
   }
   return false;
}

static bool
find_good_mod(struct zink_screen *screen, VkImageCreateInfo *ici, const struct pipe_resource *templ, unsigned bind, unsigned modifiers_count, uint64_t *modifiers, uint64_t *good_mod)
{
   bool found = false;
   VkImageUsageFlags good_usage = 0;
   const struct zink_modifier_props *prop = zink_get_modifier_props(screen, templ->format);
   for (unsigned i = 0; i < modifiers_count; i++) {
      bool need_extended = false;

      if (modifiers[i] == DRM_FORMAT_MOD_LINEAR)
         continue;

      VkFormatFeatureFlags feats = find_modifier_feats(prop, modifiers[i]);
      if (!feats)
         continue;

      if (feats & VK_FORMAT_FEATURE_DISJOINT_BIT && util_format_get_num_planes(templ->format))
         ici->flags |= VK_IMAGE_CREATE_DISJOINT_BIT;
      VkImageUsageFlags usage = get_image_usage_for_feats(screen, feats, templ, bind, &need_extended);
      assert(!need_extended);
      if (double_check_ici(screen, ici, usage, modifiers[i], true)) {
         /* assume "best" modifiers are last in array; just return last good modifier */
         found = true;
         *good_mod = modifiers[i];
         good_usage = usage;
      }
   }
   if (found)
      ici->usage = good_usage;
   return found;
}

/* subfunctions of this call must set ici->usage on success */
static bool
set_image_usage(struct zink_screen *screen, VkImageCreateInfo *ici, const struct pipe_resource *templ, unsigned bind, unsigned modifiers_count, uint64_t *modifiers, uint64_t *mod)
{
   VkImageTiling tiling = ici->tiling;
   bool need_extended = false;
   *mod = DRM_FORMAT_MOD_INVALID;
   if (modifiers_count) {
      assert(tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT);
      uint64_t good_mod = 0;
      if (screen->info.have_EXT_image_drm_format_modifier &&
          find_good_mod(screen, ici, templ, bind, modifiers_count, modifiers, &good_mod)) {
         *mod = good_mod;
         return true;
      }
      /* only try linear if no other options available */
      const struct zink_modifier_props *prop = zink_get_modifier_props(screen, templ->format);
      VkFormatFeatureFlags feats = find_modifier_feats(prop, DRM_FORMAT_MOD_LINEAR);
      if (feats) {
         if (feats & VK_FORMAT_FEATURE_DISJOINT_BIT && util_format_get_num_planes(templ->format) > 1)
            ici->flags |= VK_IMAGE_CREATE_DISJOINT_BIT;
         VkImageUsageFlags usage = get_image_usage_for_feats(screen, feats, templ, bind, &need_extended);
         assert(!need_extended);
         if (double_check_ici(screen, ici, usage, DRM_FORMAT_MOD_LINEAR, true)) {
            *mod = DRM_FORMAT_MOD_LINEAR;
            return true;
         }
      }
   } else {
      const struct zink_format_props *props = zink_get_format_props(screen, templ->format);
      VkFormatFeatureFlags2 feats = tiling == VK_IMAGE_TILING_LINEAR ? props->linearTilingFeatures : props->optimalTilingFeatures;
      if (feats & VK_FORMAT_FEATURE_DISJOINT_BIT && util_format_get_num_planes(templ->format) > 1)
         ici->flags |= VK_IMAGE_CREATE_DISJOINT_BIT;
      if (ici->flags & VK_IMAGE_CREATE_EXTENDED_USAGE_BIT)
         feats = UINT32_MAX;
      VkImageUsageFlags usage = get_image_usage_for_feats(screen, feats, templ, bind, &need_extended);
      if (need_extended) {
         ici->flags |= VK_IMAGE_CREATE_EXTENDED_USAGE_BIT | VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT;
         feats = UINT32_MAX;
         usage = get_image_usage_for_feats(screen, feats, templ, bind, &need_extended);
      }
      if (double_check_ici(screen, ici, usage, DRM_FORMAT_MOD_INVALID, true))
         return true;
      if (util_format_is_depth_or_stencil(templ->format)) {
         if (!(templ->bind & PIPE_BIND_DEPTH_STENCIL)) {
            usage &= ~VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
            /* mutable doesn't apply to depth/stencil formats */
            if (double_check_ici(screen, ici, usage, DRM_FORMAT_MOD_INVALID, true))
               return true;
         }
      } else if (!(templ->bind & PIPE_BIND_RENDER_TARGET)) {
         usage &= ~VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
         if (double_check_ici(screen, ici, usage, DRM_FORMAT_MOD_INVALID, true))
            return true;
         usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
         if (double_check_ici(screen, ici, usage, DRM_FORMAT_MOD_INVALID, false))
            return true;
         usage &= ~VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
         if (double_check_ici(screen, ici, usage, DRM_FORMAT_MOD_INVALID, false))
            return true;
      } else {
         if (double_check_ici(screen, ici, usage, DRM_FORMAT_MOD_INVALID, false))
            return true;
      }
   }
   ici->usage = 0;
   *mod = DRM_FORMAT_MOD_INVALID;
   return false;
}

static bool
try_set_image_usage_or_EXTENDED(struct zink_screen *screen, VkImageCreateInfo *ici, const struct pipe_resource *templ, unsigned bind, unsigned modifiers_count, uint64_t *modifiers, uint64_t *mod)
{
   VkImageCreateFlags flags = ici->flags;
   /* retry with EXTENDED: trust that the frontend isn't giving us anything insane and pray */
   ici->flags |= VK_IMAGE_CREATE_EXTENDED_USAGE_BIT | VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT;
   bool success = set_image_usage(screen, ici, templ, bind, modifiers_count, modifiers, mod);
   if (!success)
      ici->flags = flags;
   return success;
}

static uint64_t
eval_ici(struct zink_screen *screen, VkImageCreateInfo *ici, const struct pipe_resource *templ, unsigned bind, unsigned modifiers_count, uint64_t *modifiers, bool *success)
{
   /* sampleCounts will be set to VK_SAMPLE_COUNT_1_BIT if at least one of the following conditions is true:
    * - flags contains VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
    *
    * 44.1.1. Supported Sample Counts
    */
   bool want_cube = ici->samples == 1 &&
                    (templ->target == PIPE_TEXTURE_CUBE ||
                    templ->target == PIPE_TEXTURE_CUBE_ARRAY ||
                    (templ->target == PIPE_TEXTURE_2D_ARRAY && ici->extent.width == ici->extent.height && ici->arrayLayers >= 6));

   if (ici->tiling != VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT)
      modifiers_count = 0;

   uint64_t mod = DRM_FORMAT_MOD_INVALID;
   /* this should work most of the time */
   if (!set_image_usage(screen, ici, templ, bind, modifiers_count, modifiers, &mod)) {
      bool success = false;
      /* dmabuf doesn't retry with EXTENDED */
      if (ici->tiling == VK_IMAGE_TILING_OPTIMAL)
         success = try_set_image_usage_or_EXTENDED(screen, ici, templ, bind, modifiers_count, modifiers, &mod);
      if (!success) {
         ici->tiling = VK_IMAGE_TILING_LINEAR;
         modifiers_count = 0;
         /* in theory LINEAR should work */
         if (!set_image_usage(screen, ici, templ, bind, modifiers_count, modifiers, &mod)) {
            /* ...in theory LINEAR + EXTENDED should definitely work */
            if (!try_set_image_usage_or_EXTENDED(screen, ici, templ, bind, modifiers_count, modifiers, &mod))
               /* ...so that was a lie */
               return DRM_FORMAT_MOD_INVALID;
         }
      }
   }
   if (want_cube) {
      ici->flags |= VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT;
      VkImageUsageFlags usage = ici->usage;
      if (!set_image_usage(screen, ici, templ, bind, modifiers_count, modifiers, &mod)) {
         ici->flags &= ~VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT;
         ici->usage = usage;
      }
   }

   *success = true;
   return mod;
}

static void
init_ici(struct zink_screen *screen, VkImageCreateInfo *ici, const struct pipe_resource *templ, unsigned bind, unsigned modifiers_count)
{
   ici->sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
   ici->format = zink_get_format(screen, templ->format);
   ici->extent.width = templ->width0;
   ici->extent.height = templ->height0;
   ici->extent.depth = templ->depth0;
   ici->mipLevels = templ->last_level + 1;
   ici->arrayLayers = MAX2(templ->array_size, 1);
   ici->samples = templ->nr_samples ? templ->nr_samples : VK_SAMPLE_COUNT_1_BIT;

   /* pNext may already be set */
   if (bind & ZINK_BIND_MUTABLE)
      ici->flags = VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT;
   else
      ici->flags = 0;
   if (ici->flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT)
      /* unset VkImageFormatListCreateInfo if mutable */
      ici->pNext = NULL;
   else if (ici->pNext)
      /* add mutable if VkImageFormatListCreateInfo */
      ici->flags |= VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT;
   ici->usage = 0;
   ici->queueFamilyIndexCount = 0;
   ici->pQueueFamilyIndices = NULL;

   /* assume we're going to be doing some CompressedTexSubImage */
   if (util_format_is_compressed(templ->format) && (ici->flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT) &&
       !vk_find_struct_const(ici->pNext, IMAGE_FORMAT_LIST_CREATE_INFO))
      ici->flags |= VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT;

   if (templ->flags & PIPE_RESOURCE_FLAG_SPARSE)
      ici->flags |= VK_IMAGE_CREATE_SPARSE_BINDING_BIT | VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT;

   bool need_2D = false;
   switch (templ->target) {
   case PIPE_TEXTURE_1D:
   case PIPE_TEXTURE_1D_ARRAY:
      if (templ->flags & PIPE_RESOURCE_FLAG_SPARSE)
         need_2D |= screen->need_2D_sparse;
      if (util_format_is_depth_or_stencil(templ->format))
         need_2D |= screen->need_2D_zs;
      ici->imageType = need_2D ? VK_IMAGE_TYPE_2D : VK_IMAGE_TYPE_1D;
      break;

   case PIPE_TEXTURE_CUBE:
   case PIPE_TEXTURE_CUBE_ARRAY:
   case PIPE_TEXTURE_2D:
   case PIPE_TEXTURE_2D_ARRAY:
   case PIPE_TEXTURE_RECT:
      ici->imageType = VK_IMAGE_TYPE_2D;
      break;

   case PIPE_TEXTURE_3D:
      ici->imageType = VK_IMAGE_TYPE_3D;
      if (!(templ->flags & PIPE_RESOURCE_FLAG_SPARSE)) {
         ici->flags |= VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT;
      }
      if (!(templ->flags & PIPE_RESOURCE_FLAG_SPARSE) || screen->info.maint9_props.image2DViewOf3DSparse) {
         if (screen->info.have_EXT_image_2d_view_of_3d)
            ici->flags |= VK_IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT;
      }
      break;

   case PIPE_BUFFER:
      UNREACHABLE("PIPE_BUFFER should already be handled");

   default:
      UNREACHABLE("Unknown target");
   }

   if (screen->info.have_EXT_sample_locations &&
       bind & PIPE_BIND_DEPTH_STENCIL &&
       util_format_has_depth(util_format_description(templ->format)))
      ici->flags |= VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT;

   if (screen->info.have_EXT_image_drm_format_modifier && modifiers_count)
      ici->tiling = VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT;
   else if (bind & (PIPE_BIND_LINEAR | ZINK_BIND_DMABUF))
      ici->tiling = VK_IMAGE_TILING_LINEAR;
   else
      ici->tiling = VK_IMAGE_TILING_OPTIMAL;
   /* XXX: does this have perf implications anywhere? hopefully not */
   if (ici->samples == VK_SAMPLE_COUNT_1_BIT &&
      screen->info.have_EXT_multisampled_render_to_single_sampled &&
      ici->tiling != VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT)
      ici->flags |= VK_IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT;
   ici->sharingMode = VK_SHARING_MODE_EXCLUSIVE;
   ici->initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

   if (templ->target == PIPE_TEXTURE_CUBE)
      ici->arrayLayers *= 6;
}

static const VkImageAspectFlags plane_aspects[] = {
   VK_IMAGE_ASPECT_PLANE_0_BIT,
   VK_IMAGE_ASPECT_PLANE_1_BIT,
   VK_IMAGE_ASPECT_PLANE_2_BIT,
};

static inline bool
get_image_memory_requirement(struct zink_screen *screen, struct zink_resource_object *obj,
                             unsigned num_planes, VkMemoryRequirements *reqs)
{
   bool need_dedicated = false;
   if (VKSCR(GetImageMemoryRequirements2)) {
      VkMemoryRequirements2 req2;
      req2.sType = VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2;
      VkImageMemoryRequirementsInfo2 info2;
      info2.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2;
      info2.pNext = NULL;
      info2.image = obj->image;
      VkMemoryDedicatedRequirements ded;
      ded.sType = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS;
      ded.pNext = NULL;
      req2.pNext = &ded;
      VkImagePlaneMemoryRequirementsInfo plane;
      plane.sType = VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO;
      plane.pNext = NULL;
      if (num_planes > 1)
         info2.pNext = &plane;
      unsigned offset = 0;
      for (unsigned i = 0; i < num_planes; i++) {
         assert(i < ARRAY_SIZE(plane_aspects));
         plane.planeAspect = plane_aspects[i];
         VKSCR(GetImageMemoryRequirements2)(screen->dev, &info2, &req2);
         if (!i)
            reqs->alignment = req2.memoryRequirements.alignment;
         obj->plane_offsets[i] = offset;
         offset += req2.memoryRequirements.size;
         reqs->size += req2.memoryRequirements.size;
         reqs->memoryTypeBits |= req2.memoryRequirements.memoryTypeBits;
         need_dedicated |= ded.prefersDedicatedAllocation || ded.requiresDedicatedAllocation;
      }
   } else {
      VKSCR(GetImageMemoryRequirements)(screen->dev, obj->image, reqs);
   }
   return need_dedicated;
}

static inline VkFormatFeatureFlags
get_format_feature_flags(VkImageCreateInfo ici, struct zink_screen *screen, const struct pipe_resource *templ)
{
   VkFormatFeatureFlags feats = 0;
   switch (ici.tiling) {
   case VK_IMAGE_TILING_LINEAR:
      feats = zink_get_format_props(screen, templ->format)->linearTilingFeatures;
      break;
   case VK_IMAGE_TILING_OPTIMAL:
      feats = zink_get_format_props(screen, templ->format)->optimalTilingFeatures;
      break;
   case VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT:
      feats = VK_FORMAT_FEATURE_FLAG_BITS_MAX_ENUM;
      /*
         If is tiling then VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT, the value of
         imageCreateFormatFeatures is found by calling vkGetPhysicalDeviceFormatProperties2
         with VkImageFormatProperties::format equal to VkImageCreateInfo::format and with
         VkDrmFormatModifierPropertiesListEXT chained into VkImageFormatProperties2; by
         collecting all members of the returned array
         VkDrmFormatModifierPropertiesListEXT::pDrmFormatModifierProperties
         whose drmFormatModifier belongs to imageCreateDrmFormatModifiers; and by taking the bitwise
         intersection, over the collected array members, of drmFormatModifierTilingFeatures.
         (The resultant imageCreateFormatFeatures may be empty).
         * -Chapter 12. Resource Creation
       */
      for (unsigned i = 0; i < screen->modifier_props[templ->format].drmFormatModifierCount; i++)
         feats &= screen->modifier_props[templ->format].pDrmFormatModifierProperties[i].drmFormatModifierTilingFeatures;
      break;
   default:
         UNREACHABLE("unknown tiling");
   }
   return feats;
}

#if !defined(_WIN32)
   #define ZINK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_BIT VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT
#else
   #define ZINK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_BIT VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT
#endif


struct mem_alloc_info {
   struct winsys_handle *whandle;
   VkMemoryPropertyFlags flags;
   enum zink_alloc_flag aflags;
   bool need_dedicated;
   bool shared;
   const void *user_mem;
   VkExternalMemoryHandleTypeFlags external;
   VkExternalMemoryHandleTypeFlags export_types;
};

static inline bool
get_export_flags(struct zink_screen *screen, const struct pipe_resource *templ, struct mem_alloc_info *alloc_info)
{
   bool needs_export = (templ->bind & (ZINK_BIND_VIDEO | ZINK_BIND_DMABUF)) != 0;
   if (alloc_info->whandle) {
      if (alloc_info->whandle->type == WINSYS_HANDLE_TYPE_FD ||
          alloc_info->whandle->type == ZINK_EXTERNAL_MEMORY_HANDLE)
         needs_export |= true;
      else
         UNREACHABLE("unknown handle type");
   }
   if (needs_export) {
      if (alloc_info->whandle && alloc_info->whandle->type == ZINK_EXTERNAL_MEMORY_HANDLE) {
         alloc_info->external = ZINK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_BIT;
      } else if (screen->info.have_EXT_external_memory_dma_buf) {
         alloc_info->external = VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT;
         alloc_info->export_types |= VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT;
      } else {
         return false;
      }
   }
   if (alloc_info->user_mem) {
      assert(!alloc_info->whandle);
      alloc_info->external = VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT;
      alloc_info->export_types = VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT;
   }
   /* we may export WINSYS_HANDLE_TYPE_FD handle which is dma-buf */
   if (templ->bind & PIPE_BIND_SHARED && screen->info.have_EXT_external_memory_dma_buf)
      alloc_info->export_types |= VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT;
   return true;
}

enum resource_object_create_result {
   roc_success,
   roc_success_early_return,
   roc_fail_and_free_object,
   roc_fail_and_cleanup_object,
   roc_fail_and_cleanup_all
};

static inline enum resource_object_create_result
allocate_bo(struct zink_screen *screen, const struct pipe_resource *templ,
            VkMemoryRequirements *reqs, struct zink_resource_object *obj,
            struct mem_alloc_info *alloc_info)
{
   VkMemoryAllocateInfo mai;
   mai.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
   mai.pNext = NULL;
   mai.allocationSize = reqs->size;
   enum zink_heap heap = zink_heap_from_domain_flags(alloc_info->flags, alloc_info->aflags);
   if (templ->flags & PIPE_RESOURCE_FLAG_MAP_COHERENT) {
      if (!(vk_domain_from_heap(heap) & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT))
         heap = zink_heap_from_domain_flags(alloc_info->flags & ~VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, alloc_info->aflags);
   }

   VkMemoryDedicatedAllocateInfo ded_alloc_info = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
      .pNext = mai.pNext,
      .image = obj->image,
      .buffer = VK_NULL_HANDLE,
   };

   if (screen->info.have_KHR_dedicated_allocation && alloc_info->need_dedicated) {
      ded_alloc_info.pNext = mai.pNext;
      mai.pNext = &ded_alloc_info;
   }

   VkExportMemoryAllocateInfo emai;
   if ((templ->bind & ZINK_BIND_VIDEO) || ((templ->bind & PIPE_BIND_SHARED) && alloc_info->shared) || (templ->bind & ZINK_BIND_DMABUF)) {
      emai.sType = VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO;
      emai.handleTypes = alloc_info->export_types;

      emai.pNext = mai.pNext;
      mai.pNext = &emai;
      obj->exportable = true;
   }

#ifdef ZINK_USE_DMABUF

#if !defined(_WIN32)
   VkImportMemoryFdInfoKHR imfi = {
      VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR,
      NULL,
   };

   if (alloc_info->whandle) {
      imfi.pNext = NULL;
      imfi.handleType = alloc_info->external;
      imfi.fd = os_dupfd_cloexec(alloc_info->whandle->handle);
      if (imfi.fd < 0) {
         mesa_loge("ZINK: failed to dup dmabuf fd: %s\n", strerror(errno));
         return roc_fail_and_cleanup_object;
      }

      imfi.pNext = mai.pNext;
      mai.pNext = &imfi;
   }
#else
   VkImportMemoryWin32HandleInfoKHR imfi = {
      VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR,
      NULL,
   };

   if (alloc_info->whandle) {
      HANDLE source_target = GetCurrentProcess();
      HANDLE out_handle;

      bool result = DuplicateHandle(source_target, alloc_info->whandle->handle, source_target, &out_handle, 0, false, DUPLICATE_SAME_ACCESS);

      if (!result || !out_handle) {
         mesa_loge("ZINK: failed to DuplicateHandle with winerr: %08x\n", (int)GetLastError());
         return roc_fail_and_cleanup_object;
      }

      imfi.pNext = NULL;
      imfi.handleType = alloc_info->external;
      imfi.handle = out_handle;

      imfi.pNext = mai.pNext;
      mai.pNext = &imfi;
   }
#endif

#endif

   VkImportMemoryHostPointerInfoEXT imhpi = {
      VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT,
      NULL,
   };
   if (alloc_info->user_mem) {
      imhpi.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT;
      imhpi.pHostPointer = (void*)alloc_info->user_mem;
      imhpi.pNext = mai.pNext;
      mai.pNext = &imhpi;
   }

   unsigned alignment = MAX2(reqs->alignment, 256);
   if (templ->usage == PIPE_USAGE_STAGING && obj->is_buffer)
      alignment = MAX2(alignment, screen->info.props.limits.minMemoryMapAlignment);
   obj->alignment = alignment;

   if (zink_mem_type_idx_from_types(screen, heap, reqs->memoryTypeBits) == UINT32_MAX) {
      /* not valid based on reqs; demote to more compatible type */
      switch (heap) {
      case ZINK_HEAP_DEVICE_LOCAL_VISIBLE:
         heap = ZINK_HEAP_DEVICE_LOCAL;
         break;
      case ZINK_HEAP_HOST_VISIBLE_COHERENT_CACHED:
         heap = ZINK_HEAP_HOST_VISIBLE_COHERENT;
         break;
      default:
         break;
      }
      assert(zink_mem_type_idx_from_types(screen, heap, reqs->memoryTypeBits) != UINT32_MAX);
   }

   while (1) {
      /* iterate over all available memory types to reduce chance of oom */
      for (unsigned i = 0; !obj->bo && i < screen->heap_count[heap]; i++) {
         if (!(reqs->memoryTypeBits & BITFIELD_BIT(screen->heap_map[heap][i])))
            continue;

         mai.memoryTypeIndex = screen->heap_map[heap][i];
         obj->bo = zink_bo(zink_bo_create(screen, reqs->size, alignment, heap, mai.pNext ? ZINK_ALLOC_NO_SUBALLOC : 0, mai.memoryTypeIndex, mai.pNext));
      }

      if (obj->bo || heap != ZINK_HEAP_DEVICE_LOCAL_VISIBLE)
         break;

      /* demote BAR allocations to a different heap on failure to avoid oom */
      if (templ->flags & PIPE_RESOURCE_FLAG_MAP_COHERENT || templ->usage == PIPE_USAGE_DYNAMIC)
          heap = ZINK_HEAP_HOST_VISIBLE_COHERENT;
      else
          heap = ZINK_HEAP_DEVICE_LOCAL;
   };

   return obj->bo ? roc_success : roc_fail_and_cleanup_object;
}

static inline bool
update_alloc_info_flags(struct zink_screen *screen, const struct pipe_resource *templ,
                        VkMemoryRequirements *reqs, struct mem_alloc_info *alloc_info)
{
   if (templ->flags & PIPE_RESOURCE_FLAG_MAP_COHERENT || templ->usage == PIPE_USAGE_DYNAMIC)
      alloc_info->flags |= VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
   else if (!(alloc_info->flags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) &&
            templ->usage == PIPE_USAGE_STAGING)
      alloc_info->flags |= VK_MEMORY_PROPERTY_HOST_COHERENT_BIT | VK_MEMORY_PROPERTY_HOST_CACHED_BIT;

   if (templ->bind & ZINK_BIND_TRANSIENT)
      alloc_info->flags |= VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT;

   if (alloc_info->user_mem) {
      VkExternalMemoryHandleTypeFlagBits handle_type = VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT;
      VkMemoryHostPointerPropertiesEXT memory_host_pointer_properties = {0};
      memory_host_pointer_properties.sType = VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT;
      memory_host_pointer_properties.pNext = NULL;
      VkResult res = VKSCR(GetMemoryHostPointerPropertiesEXT)(screen->dev, handle_type, alloc_info->user_mem, &memory_host_pointer_properties);
      if (res != VK_SUCCESS) {
         mesa_loge("ZINK: vkGetMemoryHostPointerPropertiesEXT failed");
         return false;
      }
      reqs->memoryTypeBits &= memory_host_pointer_properties.memoryTypeBits;
      alloc_info->flags &= ~VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
   }

   alloc_info->aflags = templ->flags & PIPE_RESOURCE_FLAG_SPARSE ? ZINK_ALLOC_SPARSE : 0;
   return true;
}

static inline void
update_obj_info(struct zink_screen *screen, struct zink_resource_object *obj,
                const struct pipe_resource *templ, struct mem_alloc_info *alloc_info)
{
   if (alloc_info->aflags == ZINK_ALLOC_SPARSE) {
      obj->size = templ->width0;
   } else {
      obj->offset = zink_bo_get_offset(obj->bo);
      obj->size = zink_bo_get_size(obj->bo);
   }

   obj->coherent = screen->info.mem_props.memoryTypes[obj->bo->base.base.placement].propertyFlags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
   if (!(templ->flags & PIPE_RESOURCE_FLAG_SPARSE)) {
      obj->host_visible = screen->info.mem_props.memoryTypes[obj->bo->base.base.placement].propertyFlags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
   }
}

static inline void
debug_resource_mem(struct zink_resource_object *obj, const struct pipe_resource *templ, struct zink_screen *screen)
{
      char buf[4096];
      unsigned idx = 0;
      if (obj->is_buffer) {
         size_t size = (size_t)DIV_ROUND_UP(obj->size, 1024);
         if (templ->bind == PIPE_BIND_QUERY_BUFFER && templ->usage == PIPE_USAGE_STAGING) //internal qbo
            idx += snprintf(buf, sizeof(buf), "QBO(%zu)", size);
         else
            idx += snprintf(buf, sizeof(buf), "BUF(%zu)", size);
      } else {
         idx += snprintf(buf, sizeof(buf), "IMG(%s:%ux%ux%u)", util_format_short_name(templ->format), templ->width0, templ->height0, templ->depth0);
      }
      /*
      zink_vkflags_func flag_func = obj->is_buffer ? (zink_vkflags_func)vk_BufferCreateFlagBits_to_str : (zink_vkflags_func)vk_ImageCreateFlagBits_to_str;
      zink_vkflags_func usage_func = obj->is_buffer ? (zink_vkflags_func)vk_BufferUsageFlagBits_to_str : (zink_vkflags_func)vk_ImageUsageFlagBits_to_str;
      if (obj->vkflags) {
         buf[idx++] = '[';
         idx += zink_string_vkflags_unroll(&buf[idx], sizeof(buf) - idx, obj->vkflags, flag_func);
         buf[idx++] = ']';
      }
      if (obj->vkusage) {
         buf[idx++] = '[';
         idx += zink_string_vkflags_unroll(&buf[idx], sizeof(buf) - idx, obj->vkusage, usage_func);
         buf[idx++] = ']';
      }
      */
      buf[idx] = 0;
      obj->bo->name = zink_debug_mem_add(screen, obj->size, buf);
}

static inline enum resource_object_create_result
allocate_bo_and_update_obj(struct zink_screen *screen, const struct pipe_resource *templ,
                           VkMemoryRequirements *reqs, struct zink_resource_object *obj,
                           struct mem_alloc_info *alloc_info)
{
   if (!update_alloc_info_flags(screen, templ, reqs, alloc_info))
      return roc_fail_and_free_object;

   enum resource_object_create_result retval = allocate_bo(screen, templ, reqs, obj, alloc_info);
   assert(retval != roc_success_early_return);
   if (retval != roc_success)
      return retval;

   update_obj_info(screen, obj, templ, alloc_info);

   if (zink_debug & ZINK_DEBUG_MEM)
      debug_resource_mem(obj, templ, screen);
   return roc_success;
}

static inline enum resource_object_create_result
create_buffer(struct zink_screen *screen, struct zink_resource_object *obj,
              const struct pipe_resource *templ, uint64_t *modifiers,
              int modifiers_count, struct mem_alloc_info *alloc_info)
{
   VkBufferCreateInfo bci = create_bci(screen, templ, templ->bind);
   VkExternalMemoryBufferCreateInfo embci;
   VkMemoryRequirements reqs = {0};

   embci.sType = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO;
   if (alloc_info->external) {
      embci.pNext = bci.pNext;
      embci.handleTypes = alloc_info->export_types;
      bci.pNext = &embci;
   }

   if (VKSCR(CreateBuffer)(screen->dev, &bci, NULL, &obj->buffer) != VK_SUCCESS) {
      mesa_loge("ZINK: vkCreateBuffer failed");
      return roc_fail_and_free_object;
   }

   if (!(templ->bind & (PIPE_BIND_SHADER_IMAGE | ZINK_BIND_DESCRIPTOR))) {
       bci.usage |= VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT;
     if (VKSCR(CreateBuffer)(screen->dev, &bci, NULL, &obj->storage_buffer) != VK_SUCCESS) {
        mesa_loge("ZINK: vkCreateBuffer failed");
        VKSCR(DestroyBuffer)(screen->dev, obj->buffer, NULL);
        return roc_fail_and_free_object;
     }
   }

   assert(!modifiers_count);
   VKSCR(GetBufferMemoryRequirements)(screen->dev, obj->buffer, &reqs);

   if (templ->usage == PIPE_USAGE_STAGING)
      alloc_info->flags = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT | VK_MEMORY_PROPERTY_HOST_CACHED_BIT;
   else if (templ->usage == PIPE_USAGE_STREAM)
      alloc_info->flags = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
   else if (templ->usage == PIPE_USAGE_IMMUTABLE)
      alloc_info->flags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
   else
      alloc_info->flags = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;

   obj->is_buffer = true;
   obj->transfer_dst = true;
   obj->vkflags = bci.flags;
   obj->vkusage = bci.usage;

   enum resource_object_create_result retval = allocate_bo_and_update_obj(screen, templ, &reqs, obj,  alloc_info);
   assert(retval != roc_success_early_return);
   if (retval != roc_success)
      return retval;

   if (!(templ->flags & PIPE_RESOURCE_FLAG_SPARSE)) {
      if (VKSCR(BindBufferMemory)(screen->dev, obj->buffer, zink_bo_get_mem(obj->bo), obj->offset) != VK_SUCCESS) {
         mesa_loge("ZINK: vkBindBufferMemory failed");
         return roc_fail_and_cleanup_all ;
      }
      if (obj->storage_buffer && VKSCR(BindBufferMemory)(screen->dev, obj->storage_buffer, zink_bo_get_mem(obj->bo), obj->offset) != VK_SUCCESS) {
         mesa_loge("ZINK: vkBindBufferMemory failed");
         return roc_fail_and_cleanup_all;
      }
   }
   _mesa_set_init(&obj->surface_cache, NULL, NULL, equals_bufferview_key);
   return roc_success;
}

static inline enum resource_object_create_result
create_image(struct zink_screen *screen, struct zink_resource_object *obj,
             const struct pipe_resource *templ, bool *linear,
             uint64_t *modifiers, int modifiers_count,
             struct mem_alloc_info *alloc_info)
{
   VkMemoryRequirements reqs = {0};
   bool winsys_modifier = (alloc_info->export_types & VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT) &&
                          alloc_info->whandle &&
                          alloc_info->whandle->modifier != DRM_FORMAT_MOD_INVALID;
   uint64_t *ici_modifiers = winsys_modifier ? &alloc_info->whandle->modifier : modifiers;
   unsigned ici_modifier_count = winsys_modifier ? 1 : modifiers_count;
   unsigned num_planes = util_format_get_num_planes(templ->format);
   VkImageCreateInfo ici;
   enum pipe_format srgb = PIPE_FORMAT_NONE;
   /* we often need to be able to mutate between srgb and linear, but we don't need general
    * image view/shader image format compatibility (that path means losing fast clears or compression on some hardware).
    */
   if (!(templ->bind & ZINK_BIND_MUTABLE) && (!alloc_info->whandle || alloc_info->whandle->type == ZINK_EXTERNAL_MEMORY_HANDLE)) {
      srgb = util_format_is_srgb(templ->format) ? util_format_linear(templ->format) : util_format_srgb(templ->format);
      /* why do these helpers have different default return values? */
      if (srgb == templ->format)
         srgb = PIPE_FORMAT_NONE;
   }
   VkFormat formats[4] = {VK_FORMAT_UNDEFINED};
   VkImageFormatListCreateInfo format_list;
   if (srgb) {
      formats[0] = zink_get_format(screen, templ->format);
      formats[1] = zink_get_format(screen, srgb);
   } else if (templ->bind & ZINK_BIND_VIDEO) {
      formats[0] = zink_get_format(screen, templ->format);
      for (unsigned i = 0; i < num_planes; i++)
         formats[i + 1] = zink_get_format(screen, util_format_get_plane_format(templ->format, i));
   }
   /* only use format list if multiple formats have supported vk equivalents */
   if (formats[0] && formats[1]) {
      format_list.sType = VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO;
      format_list.pNext = NULL;
      format_list.viewFormatCount = formats[2] ? 3 : 2;
      format_list.pViewFormats = formats;
      ici.pNext = &format_list;
   } else {
      ici.pNext = NULL;
   }
   init_ici(screen, &ici, templ, templ->bind, ici_modifier_count);

   bool success = false;
   uint64_t mod = eval_ici(screen, &ici, templ, templ->bind, ici_modifier_count, ici_modifiers, &success);
   if (ici.format == VK_FORMAT_A8_UNORM_KHR && !success) {
      ici.format = zink_get_format(screen, zink_format_get_emulated_alpha(templ->format));
      mod = eval_ici(screen, &ici, templ, templ->bind, ici_modifier_count, ici_modifiers, &success);
   }
   /* images with modifiers MUST have a format list if they are mutable */
   if (modifiers_count && ici.flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT) {
      const VkImageFormatListCreateInfo *format_list = vk_find_struct_const(&ici, IMAGE_FORMAT_LIST_CREATE_INFO);
      assert(format_list);
      if (!format_list)
         return roc_fail_and_free_object;
   }
   if (!success)
      return roc_fail_and_free_object;

   if (ici.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT && srgb &&
      util_format_get_nr_components(srgb) == 4 &&
      !(ici.flags & VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT)) {
         mesa_loge("zink: refusing to create possibly-srgb dmabuf due to missing driver support: %s not supported!", util_format_name(srgb));
         return roc_fail_and_free_object;
   }
   VkExternalMemoryImageCreateInfo emici;
   VkImageDrmFormatModifierExplicitCreateInfoEXT idfmeci;
   VkImageDrmFormatModifierListCreateInfoEXT idfmlci;
   VkSubresourceLayout plane_layouts[4];
   VkSubresourceLayout plane_layout = {
      .offset = alloc_info->whandle ? alloc_info->whandle->offset : 0,
      .size = 0,
      .rowPitch = alloc_info->whandle ? alloc_info->whandle->stride : 0,
      .arrayPitch = 0,
      .depthPitch = 0,
   };

   obj->render_target = (ici.usage & VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT) != 0;

   if (ici.tiling == VK_IMAGE_TILING_OPTIMAL) {
      alloc_info->external &= ~VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT;
      alloc_info->export_types &= ~VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT;
   }

   if (alloc_info->shared || alloc_info->external) {
      emici.sType = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO;
      emici.pNext = ici.pNext;
      emici.handleTypes = alloc_info->export_types;
      assert(!(emici.handleTypes & VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT) || ici.tiling != VK_IMAGE_TILING_OPTIMAL);
      ici.pNext = &emici;

      assert(ici.tiling != VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT || mod != DRM_FORMAT_MOD_INVALID);
      if (alloc_info->whandle && ici.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
         assert(mod == alloc_info->whandle->modifier || !winsys_modifier);
         idfmeci.sType = VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT;
         idfmeci.pNext = ici.pNext;
         idfmeci.drmFormatModifier = mod;
         idfmeci.drmFormatModifierPlaneCount = obj->plane_count;

         plane_layouts[0] = plane_layout;
         struct pipe_resource *pnext = templ->next;
         for (unsigned i = 1; i < obj->plane_count; i++, pnext = pnext->next) {
            struct zink_resource *next = zink_resource(pnext);
            obj->plane_offsets[i] = plane_layouts[i].offset = next->obj->plane_offsets[i];
            obj->plane_strides[i] = plane_layouts[i].rowPitch = next->obj->plane_strides[i];
            plane_layouts[i].size = 0;
            plane_layouts[i].arrayPitch = 0;
            plane_layouts[i].depthPitch = 0;
         }
         idfmeci.pPlaneLayouts = plane_layouts;

         ici.pNext = &idfmeci;
      } else if (ici.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
         idfmlci.sType = VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT;
         idfmlci.pNext = ici.pNext;
         idfmlci.drmFormatModifierCount = modifiers_count;
         idfmlci.pDrmFormatModifiers = modifiers;
         ici.pNext = &idfmlci;
      } else if (ici.tiling == VK_IMAGE_TILING_OPTIMAL) {
         alloc_info->shared = false;
      }
   } else if (alloc_info->user_mem) {
      emici.sType = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO;
      emici.pNext = ici.pNext;
      emici.handleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT;
      ici.pNext = &emici;
   } else {
      /* If the frontend passed modifiers it should have also passed
       * PIPE_BIND_SHARED
       */
      assert(ici.tiling != VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT);
   }

   if (linear)
      *linear = ici.tiling == VK_IMAGE_TILING_LINEAR;

   if (ici.usage & VK_IMAGE_USAGE_TRANSFER_DST_BIT)
      obj->transfer_dst = true;

#if defined(ZINK_USE_DMABUF) && !defined(_WIN32)
   if (obj->is_aux) {
      obj->modifier = mod;
      obj->modifier_aspect = VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT << alloc_info->whandle->plane;
      obj->plane_offsets[alloc_info->whandle->plane] = alloc_info->whandle->offset;
      obj->plane_strides[alloc_info->whandle->plane] = alloc_info->whandle->stride;
      obj->handle = os_dupfd_cloexec(alloc_info->whandle->handle);
      if (obj->handle < 0) {
         mesa_loge("ZINK: failed to dup dmabuf fd: %s\n", strerror(errno));
         return roc_fail_and_free_object;
      }
      return roc_success_early_return;
   }
#endif

   if (!(templ->bind & ZINK_BIND_VIDEO)) {
      obj->vkfeats = get_format_feature_flags(ici, screen, templ);
      if (obj->vkfeats & VK_FORMAT_FEATURE_DISJOINT_BIT)
         ici.flags |= VK_IMAGE_CREATE_DISJOINT_BIT;
   }
   if (util_format_is_yuv(templ->format)) {
   } else if (alloc_info->whandle) {
      obj->plane_strides[alloc_info->whandle->plane] = alloc_info->whandle->stride;
   }

   VkResult result = VKSCR(CreateImage)(screen->dev, &ici, NULL, &obj->image);
   if (result != VK_SUCCESS) {
      mesa_loge("ZINK: vkCreateImage failed (%s)", vk_Result_to_str(result));
      return roc_fail_and_free_object;
   }

   if (ici.tiling == VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT) {
      VkImageDrmFormatModifierPropertiesEXT modprops = {0};
      modprops.sType = VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT;
      result = VKSCR(GetImageDrmFormatModifierPropertiesEXT)(screen->dev, obj->image, &modprops);
      if (result != VK_SUCCESS) {
         mesa_loge("ZINK: vkGetImageDrmFormatModifierPropertiesEXT failed");
         return roc_fail_and_free_object;
      }
      obj->modifier = modprops.drmFormatModifier;
      unsigned num_dmabuf_planes = screen->base.get_dmabuf_modifier_planes(&screen->base, obj->modifier, templ->format);
      obj->modifier_aspect = VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT;
      if (num_dmabuf_planes > 1)
         obj->modifier_aspect |= VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT;
      if (num_dmabuf_planes > 2)
         obj->modifier_aspect |= VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT;
      if (num_dmabuf_planes > 3)
         obj->modifier_aspect |= VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT;
      assert(num_dmabuf_planes <= 4);
   }

   alloc_info->need_dedicated = get_image_memory_requirement(screen, obj, num_planes, &reqs);
   if (templ->usage == PIPE_USAGE_STAGING && ici.tiling == VK_IMAGE_TILING_LINEAR)
      alloc_info->flags = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
   else
      alloc_info->flags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;

   obj->vkflags = ici.flags;
   obj->vkusage = ici.usage;

   enum resource_object_create_result retval = allocate_bo_and_update_obj(screen, templ, &reqs, obj,  alloc_info);
   assert(retval != roc_success_early_return);
   if (retval != roc_success)
      return retval;

   if (ici.flags & VK_IMAGE_CREATE_DISJOINT_BIT) {
      VkBindImageMemoryInfo infos[3];
      VkBindImagePlaneMemoryInfo planes[3];
      for (unsigned i = 0; i < num_planes; i++) {
         infos[i].sType = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO;
         infos[i].image = obj->image;
         infos[i].memory = zink_bo_get_mem(obj->bo);
         infos[i].memoryOffset = obj->plane_offsets[i];
         if (templ->bind & ZINK_BIND_VIDEO) {
            infos[i].pNext = &planes[i];
            planes[i].sType = VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO;
            planes[i].pNext = NULL;
            planes[i].planeAspect = plane_aspects[i];
         }
      }
      if (VKSCR(BindImageMemory2)(screen->dev, num_planes, infos) != VK_SUCCESS) {
         mesa_loge("ZINK: vkBindImageMemory2 failed");
         return roc_fail_and_cleanup_all;
      }
   } else {
      if (!(templ->flags & PIPE_RESOURCE_FLAG_SPARSE))
         if (VKSCR(BindImageMemory)(screen->dev, obj->image, zink_bo_get_mem(obj->bo), obj->offset) != VK_SUCCESS) {
            mesa_loge("ZINK: vkBindImageMemory failed");
            return roc_fail_and_cleanup_all;
         }
   }
   _mesa_set_init(&obj->surface_cache, NULL, NULL, equals_surface_key);
   return roc_success;
}

static struct zink_resource_object *
resource_object_create(struct zink_screen *screen, const struct pipe_resource *templ, struct winsys_handle *whandle, bool *linear,
                       uint64_t *modifiers, int modifiers_count, const void *loader_private, const void *user_mem)
{
   struct zink_resource_object *obj = CALLOC_STRUCT(zink_resource_object);
   unsigned max_level = 0;
   if (!obj)
      return NULL;
   u_rwlock_init(&obj->copy_lock);
   obj->unordered_read = true;
   obj->unordered_write = true;
   obj->unsync_access = true;
   obj->modifier = DRM_FORMAT_MOD_INVALID;
   obj->last_dt_idx = obj->dt_idx = UINT32_MAX; //TODO: unionize

   struct mem_alloc_info alloc_info = {
      .whandle = whandle,
      .need_dedicated = false,
      .export_types = ZINK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_BIT,
      .shared = templ->bind & PIPE_BIND_SHARED,
      .user_mem = user_mem
   };

   /* figure out aux plane count */
   if (whandle && whandle->plane >= util_format_get_num_planes(whandle->format))
      obj->is_aux = true;
   struct pipe_resource *pnext = templ->next;
   for (obj->plane_count = 1; pnext; obj->plane_count++, pnext = pnext->next) {
      struct zink_resource *next = zink_resource(pnext);
      if (!next->obj->is_aux)
         break;
   }

   if (!get_export_flags(screen, templ, &alloc_info)) {
      /* can't export anything, fail early */
      return NULL;
   }

   pipe_reference_init(&obj->reference, 1);
   if (loader_private) {
      obj->bo = CALLOC_STRUCT(zink_bo);
      if (!obj->bo) {
         mesa_loge("ZINK: failed to allocate obj->bo!");
         return NULL;
      }

      obj->transfer_dst = true;
      return obj;
   }

   enum resource_object_create_result create_result;
   if (templ->target == PIPE_BUFFER) {
      max_level = 1;
      create_result = create_buffer(screen, obj, templ, modifiers, modifiers_count, &alloc_info);
   } else {
      max_level = templ->last_level + 1;
      create_result = create_image(screen, obj, templ, linear, modifiers, modifiers_count,
                                   &alloc_info);
   }

   switch (create_result) {
   case roc_success:
      for (unsigned i = 0; i < max_level; i++)
         util_dynarray_init(&obj->copies[i], NULL);
      FALLTHROUGH;
   case roc_success_early_return:
      return obj;

   case roc_fail_and_cleanup_all:
      zink_bo_unref(screen, obj->bo);
      FALLTHROUGH;
   case roc_fail_and_cleanup_object:
      if (templ->target == PIPE_BUFFER) {
         VKSCR(DestroyBuffer)(screen->dev, obj->buffer, NULL);
         VKSCR(DestroyBuffer)(screen->dev, obj->storage_buffer, NULL);
      } else
         VKSCR(DestroyImage)(screen->dev, obj->image, NULL);
      FALLTHROUGH;
   case roc_fail_and_free_object:
      FREE(obj);
      return NULL;
   default:
      UNREACHABLE("Invalid create object result code");
   }
}

static struct pipe_resource *
resource_create(struct pipe_screen *pscreen,
                const struct pipe_resource *templ,
                struct winsys_handle *whandle,
                unsigned external_usage,
                const uint64_t *modifiers, int modifiers_count,
                const void *loader_private, const void *user_mem)
{
   struct zink_screen *screen = zink_screen(pscreen);
   struct zink_resource *res = CALLOC_STRUCT_CL(zink_resource);

   if (!res) {
      mesa_loge("ZINK: failed to allocate res!");
      return NULL;
   }

   if (modifiers_count > 0 && screen->info.have_EXT_image_drm_format_modifier) {
      /* for rebinds */
      res->modifiers_count = modifiers_count;
      res->modifiers = mem_dup(modifiers, modifiers_count * sizeof(uint64_t));
      if (!res->modifiers) {
         FREE_CL(res);
         return NULL;
      }
   }

   res->base.b = *templ;

   bool allow_cpu_storage = (templ->target == PIPE_BUFFER) &&
                            (templ->usage != PIPE_USAGE_STREAM) &&
                            (templ->width0 < 0x1000);
   threaded_resource_init(&res->base.b, allow_cpu_storage);
   pipe_reference_init(&res->base.b.reference, 1);
   res->base.b.screen = pscreen;

   bool linear = false;
   struct pipe_resource templ2 = *templ;
   if (templ2.flags & PIPE_RESOURCE_FLAG_SPARSE &&
       (util_res_sample_count(templ) == 1 || screen->info.feats.features.shaderStorageImageMultisample))
      templ2.bind |= PIPE_BIND_SHADER_IMAGE;
   res->obj = resource_object_create(screen, &templ2, whandle, &linear, res->modifiers, res->modifiers_count, loader_private, user_mem);
   if (!res->obj) {
      free(res->modifiers);
      FREE_CL(res);
      return NULL;
   }

   res->queue = VK_QUEUE_FAMILY_IGNORED;
   res->internal_format = templ->format;
   if (templ->target == PIPE_BUFFER) {
      util_range_init(&res->valid_buffer_range);
      res->base.b.bind |= PIPE_BIND_SHADER_IMAGE;
      if (!screen->resizable_bar && templ->width0 >= 8196) {
         /* We don't want to evict buffers from VRAM by mapping them for CPU access,
          * because they might never be moved back again. If a buffer is large enough,
          * upload data by copying from a temporary GTT buffer. 8K might not seem much,
          * but there can be 100000 buffers.
          *
          * This tweak improves performance for viewperf.
          */
         res->base.b.flags |= PIPE_RESOURCE_FLAG_DONT_MAP_DIRECTLY;
      }
      if (zink_descriptor_mode == ZINK_DESCRIPTOR_MODE_DB)
         zink_resource_get_address(screen, res);
   } else {
      if (templ->flags & PIPE_RESOURCE_FLAG_SPARSE)
         res->base.b.bind |= PIPE_BIND_SHADER_IMAGE;
      if (templ->flags & PIPE_RESOURCE_FLAG_SPARSE) {
         uint32_t count = 1;
         VKSCR(GetImageSparseMemoryRequirements)(screen->dev, res->obj->image, &count, &res->sparse);
         res->base.b.nr_sparse_levels = res->sparse.imageMipTailFirstLod;
      }
      res->format = zink_get_format(screen, templ->format);
      if (templ->target == PIPE_TEXTURE_1D || templ->target == PIPE_TEXTURE_1D_ARRAY) {
         res->need_2D = (screen->need_2D_zs && util_format_is_depth_or_stencil(templ->format)) ||
                        (screen->need_2D_sparse && (templ->flags & PIPE_RESOURCE_FLAG_SPARSE));
      }
      res->dmabuf = whandle && whandle->type == WINSYS_HANDLE_TYPE_FD;
      if (res->dmabuf)
         res->queue = VK_QUEUE_FAMILY_FOREIGN_EXT;
      res->layout = res->dmabuf ? VK_IMAGE_LAYOUT_PREINITIALIZED : VK_IMAGE_LAYOUT_UNDEFINED;
      res->linear = linear;
      res->aspect = aspect_from_format(templ->format);
   }

   if (loader_private) {
      if (templ->bind & PIPE_BIND_DISPLAY_TARGET) {
         /* backbuffer */
         res->obj->dt = zink_kopper_displaytarget_create(screen,
                                                         res->base.b.bind,
                                                         res->base.b.format,
                                                         templ->width0,
                                                         templ->height0,
                                                         64, loader_private,
                                                         &res->dt_stride);
         if (!res->obj->dt) {
            mesa_loge("zink: could not create swapchain");
            FREE(res->obj);
            free(res->modifiers);
            FREE_CL(res);
            return NULL;
         }
         struct kopper_displaytarget *cdt = res->obj->dt;
         if (cdt->swapchain->num_acquires) {
            /* this should be a reused swapchain after a MakeCurrent dance that deleted the original resource */
            for (unsigned i = 0; i < cdt->swapchain->num_images; i++) {
               if (!cdt->swapchain->images[i].acquired)
                  continue;
               res->obj->dt_idx = i;
               res->obj->image = cdt->swapchain->images[i].image;
               res->layout = cdt->swapchain->images[i].layout;
            }
         }
      } else {
         /* frontbuffer */
         struct zink_resource *back = (void*)loader_private;
         struct kopper_displaytarget *cdt = back->obj->dt;
         cdt->refcount++;
         assert(back->obj->dt);
         res->obj->dt = back->obj->dt;
      }
      struct kopper_displaytarget *cdt = res->obj->dt;
      if (zink_kopper_has_srgb(cdt))
         res->obj->vkflags |= VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT;
      if (cdt->swapchain->scci.flags == VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR)
         res->obj->vkflags = VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT | VK_IMAGE_CREATE_EXTENDED_USAGE_BIT;
      res->obj->vkusage = cdt->swapchain->scci.imageUsage;
      res->base.b.bind |= PIPE_BIND_DISPLAY_TARGET;
      res->linear = false;
      res->swapchain = true;
   }

   if (!res->obj->host_visible) {
      res->base.b.flags |= PIPE_RESOURCE_FLAG_DONT_MAP_DIRECTLY;
      res->base.allow_cpu_storage = false;
   }
   simple_mtx_init(&res->obj->surface_mtx, mtx_plain);
   if (res->obj->is_buffer) {
      res->base.buffer_id_unique = util_idalloc_mt_alloc(&screen->buffer_ids);
   } else {
      /* immediately switch to GENERAL layout if possible to avoid extra sync */
      if (res->obj->image && res->queue != VK_QUEUE_FAMILY_FOREIGN_EXT && screen->driver_workarounds.general_layout)
         zink_resource_image_hic_transition(screen, res, VK_IMAGE_LAYOUT_GENERAL);
   }
   if (res->obj->exportable)
      res->base.b.bind |= ZINK_BIND_DMABUF;
   return &res->base.b;
}

static struct pipe_resource *
zink_resource_create(struct pipe_screen *pscreen,
                     const struct pipe_resource *templ)
{
   return resource_create(pscreen, templ, NULL, 0, NULL, 0, NULL, NULL);
}

static struct pipe_resource *
zink_resource_create_with_modifiers(struct pipe_screen *pscreen, const struct pipe_resource *templ,
                                    const uint64_t *modifiers, int modifiers_count)
{
   return resource_create(pscreen, templ, NULL, 0, modifiers, modifiers_count, NULL, NULL);
}

static struct pipe_resource *
zink_resource_create_drawable(struct pipe_screen *pscreen,
                              const struct pipe_resource *templ,
                              const void *loader_private)
{
   return resource_create(pscreen, templ, NULL, 0, NULL, 0, loader_private, NULL);
}

static bool
add_resource_bind(struct zink_context *ctx, struct zink_resource *res, unsigned bind)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   assert((res->base.b.bind & bind) == 0);
   res->base.b.bind |= bind;
   struct zink_resource_object *old_obj = res->obj;
   ASSERTED uint64_t mod = DRM_FORMAT_MOD_INVALID;
   if (bind & ZINK_BIND_DMABUF && !res->modifiers_count && !res->obj->is_buffer && screen->info.have_EXT_image_drm_format_modifier) {
      res->modifiers_count = 1;
      res->modifiers = malloc(res->modifiers_count * sizeof(uint64_t));
      if (!res->modifiers) {
         mesa_loge("ZINK: failed to allocate res->modifiers!");
         return false;
      }

      mod = res->modifiers[0] = DRM_FORMAT_MOD_LINEAR;
   }
   struct zink_resource_object *new_obj = resource_object_create(screen, &res->base.b, NULL, &res->linear, res->modifiers, res->modifiers_count, NULL, NULL);
   if (!new_obj) {
      debug_printf("new backing resource alloc failed!\n");
      res->base.b.bind &= ~bind;
      return false;
   }
   assert(mod == DRM_FORMAT_MOD_INVALID || new_obj->modifier == DRM_FORMAT_MOD_LINEAR);
   struct zink_resource staging = *res;
   staging.obj = old_obj;
   staging.all_binds = 0;
   res->layout = VK_IMAGE_LAYOUT_UNDEFINED;
   res->obj = new_obj;
   res->queue = VK_QUEUE_FAMILY_IGNORED;
   bool valid_contents = (res->obj->is_buffer && (res->valid_buffer_range.end || res->base.valid_buffer_range.end)) ||
                         (!res->obj->is_buffer && res->valid);
   if (valid_contents) {
      for (unsigned i = 0; i <= res->base.b.last_level; i++) {
         struct pipe_box box;
         u_box_3d(0, 0, 0,
                  u_minify(res->base.b.width0, i),
                  u_minify(res->base.b.height0, i), res->base.b.array_size, &box);
         box.depth = util_num_layers(&res->base.b, i);
         ctx->base.resource_copy_region(&ctx->base, &res->base.b, i, 0, 0, 0, &staging.base.b, i, &box);
      }
   }
   res->rebind_count++;
   if (old_obj->exportable) {
      simple_mtx_lock(&ctx->bs->exportable_lock);
      _mesa_set_remove_key(&ctx->bs->dmabuf_exports, &staging);
      simple_mtx_unlock(&ctx->bs->exportable_lock);
   }
   zink_resource_object_reference(screen, &old_obj, NULL);
   return true;
}

static bool
zink_resource_is_aux_plane(struct pipe_resource *pres)
{
   struct zink_resource *rsc = zink_resource(pres);
   return rsc->obj->is_aux;
}

static bool
zink_resource_get_param(struct pipe_screen *pscreen, struct pipe_context *pctx,
                        struct pipe_resource *pres,
                        unsigned plane,
                        unsigned layer,
                        unsigned level,
                        enum pipe_resource_param param,
                        unsigned handle_usage,
                        uint64_t *value)
{
   while (plane && pres->next && !zink_resource_is_aux_plane(pres->next)) {
      --plane;
      pres = pres->next;
   }

   struct zink_screen *screen = zink_screen(pscreen);
   struct zink_resource *res = zink_resource(pres);
   struct zink_resource_object *obj = res->obj;
   struct winsys_handle whandle;
   VkImageAspectFlags aspect;
   if (obj->modifier_aspect) {
      switch (plane) {
      case 0:
         aspect = VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT;
         break;
      case 1:
         aspect = VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT;
         break;
      case 2:
         aspect = VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT;
         break;
      case 3:
         aspect = VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT;
         break;
      default:
         UNREACHABLE("how many planes you got in this thing?");
      }
   } else if (util_format_is_yuv(pres->format)) {
      aspect = VK_IMAGE_ASPECT_PLANE_0_BIT;
   } else {
      aspect = res->aspect;
   }
   switch (param) {
   case PIPE_RESOURCE_PARAM_NPLANES:
      if (screen->info.have_EXT_image_drm_format_modifier)
         *value = screen->base.get_dmabuf_modifier_planes(&screen->base, obj->modifier, res->internal_format);
      else
         *value = 1;
      break;

   case PIPE_RESOURCE_PARAM_STRIDE: {
      VkImageSubresource sub_res = {0};
      VkSubresourceLayout sub_res_layout = {0};

      sub_res.aspectMask = aspect;

      VKSCR(GetImageSubresourceLayout)(screen->dev, obj->image, &sub_res, &sub_res_layout);

      *value = sub_res_layout.rowPitch;
      break;
   }

   case PIPE_RESOURCE_PARAM_OFFSET: {
         VkImageSubresource isr = {
            aspect,
            level,
            layer
         };
         VkSubresourceLayout srl;
         VKSCR(GetImageSubresourceLayout)(screen->dev, obj->image, &isr, &srl);
         *value = srl.offset;
         break;
   }

   case PIPE_RESOURCE_PARAM_MODIFIER: {
      *value = obj->modifier;
      break;
   }

   case PIPE_RESOURCE_PARAM_LAYER_STRIDE: {
         VkImageSubresource isr = {
            aspect,
            level,
            layer
         };
         VkSubresourceLayout srl;
         VKSCR(GetImageSubresourceLayout)(screen->dev, obj->image, &isr, &srl);
         if (res->base.b.target == PIPE_TEXTURE_3D)
            *value = srl.depthPitch;
         else
            *value = srl.arrayPitch;
         break;
   }

      return false;
   case PIPE_RESOURCE_PARAM_HANDLE_TYPE_KMS:
   case PIPE_RESOURCE_PARAM_HANDLE_TYPE_SHARED:
   case PIPE_RESOURCE_PARAM_HANDLE_TYPE_FD: {
#ifdef ZINK_USE_DMABUF
      memset(&whandle, 0, sizeof(whandle));
      if (param == PIPE_RESOURCE_PARAM_HANDLE_TYPE_SHARED)
         whandle.type = WINSYS_HANDLE_TYPE_SHARED;
      if (param == PIPE_RESOURCE_PARAM_HANDLE_TYPE_KMS)
         whandle.type = WINSYS_HANDLE_TYPE_KMS;
      else if (param == PIPE_RESOURCE_PARAM_HANDLE_TYPE_FD)
         whandle.type = WINSYS_HANDLE_TYPE_FD;

      if (!pscreen->resource_get_handle(pscreen, pctx, pres, &whandle, handle_usage))
         return false;

#ifdef _WIN32
      *value = (uintptr_t)whandle.handle;
#else
      *value = whandle.handle;
#endif
      break;
#else
      (void)whandle;
      return false;
#endif
   }
   }
   return true;
}

static bool
zink_resource_get_handle(struct pipe_screen *pscreen,
                         struct pipe_context *context,
                         struct pipe_resource *tex,
                         struct winsys_handle *whandle,
                         unsigned usage)
{
   if (tex->target == PIPE_BUFFER)
      tc_buffer_disable_cpu_storage(tex);
   if (whandle->type == WINSYS_HANDLE_TYPE_FD || whandle->type == WINSYS_HANDLE_TYPE_KMS) {
#ifdef ZINK_USE_DMABUF
      while (whandle->plane && tex->next && !zink_resource_is_aux_plane(tex->next)) {
         tex = tex->next;
      }

      struct zink_resource *res = zink_resource(tex);
      struct zink_screen *screen = zink_screen(pscreen);
      struct zink_resource_object *obj = res->obj;

#if !defined(_WIN32)
      if (whandle->type == WINSYS_HANDLE_TYPE_KMS && screen->drm_fd == -1) {
         whandle->handle = -1;
      } else {
         if (!res->obj->exportable) {
            assert(!zink_resource_usage_is_unflushed(res));
            if (!screen->info.have_EXT_image_drm_format_modifier) {
               static bool warned = false;
               warn_missing_feature(warned, "EXT_image_drm_format_modifier");
               return false;
            }
            unsigned bind = ZINK_BIND_DMABUF;
            if (!(res->base.b.bind & PIPE_BIND_SHARED))
               bind |= PIPE_BIND_SHARED;
            zink_screen_lock_context(screen);
            if (!add_resource_bind(screen->copy_context, res, bind)) {
               zink_screen_unlock_context(screen);
               return false;
            }
            if (res->all_binds)
               p_atomic_inc(&screen->image_rebind_counter);
            screen->copy_context->base.flush(&screen->copy_context->base, NULL, 0);
            zink_screen_unlock_context(screen);
            obj = res->obj;
         }

         VkMemoryGetFdInfoKHR fd_info = {0};
         int fd;
         fd_info.sType = VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR;
         fd_info.memory = zink_bo_get_mem(obj->bo);
         if (whandle->type == WINSYS_HANDLE_TYPE_FD)
            fd_info.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT;
         else
            fd_info.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT;
         VkResult result = VKSCR(GetMemoryFdKHR)(screen->dev, &fd_info, &fd);
         if (result != VK_SUCCESS) {
            mesa_loge("ZINK: vkGetMemoryFdKHR failed");
            return false;
         }
         if (whandle->type == WINSYS_HANDLE_TYPE_KMS) {
            uint32_t h;
            bool ret = zink_bo_get_kms_handle(screen, obj->bo, fd, &h);
            close(fd);
            if (!ret)
               return false;
            fd = h;
         }

         whandle->handle = fd;
      }
#else
      VkMemoryGetWin32HandleInfoKHR handle_info = {0};
      HANDLE handle;
      handle_info.sType = VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR;
      //TODO: remove for wsi
      handle_info.memory = zink_bo_get_mem(obj->bo);
      handle_info.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT;
      VkResult result = VKSCR(GetMemoryWin32HandleKHR)(screen->dev, &handle_info, &handle);
      if (result != VK_SUCCESS)
         return false;
      whandle->handle = handle;
#endif
      uint64_t value;
      zink_resource_get_param(pscreen, context, tex, 0, 0, 0, PIPE_RESOURCE_PARAM_MODIFIER, 0, &value);
      whandle->modifier = value;
      if (!res->obj->is_buffer) {
         zink_resource_get_param(pscreen, context, tex, 0, 0, 0, PIPE_RESOURCE_PARAM_OFFSET, 0, &value);
         whandle->offset = value;
         zink_resource_get_param(pscreen, context, tex, 0, 0, 0, PIPE_RESOURCE_PARAM_STRIDE, 0, &value);
         whandle->stride = value;
      }
#else
      return false;
#endif
   }
   return true;
}

static struct pipe_resource *
zink_resource_from_handle(struct pipe_screen *pscreen,
                 const struct pipe_resource *templ,
                 struct winsys_handle *whandle,
                 unsigned usage)
{
#ifdef ZINK_USE_DMABUF
   if (whandle->modifier != DRM_FORMAT_MOD_INVALID &&
       !zink_screen(pscreen)->info.have_EXT_image_drm_format_modifier)
      return NULL;

   struct pipe_resource templ2 = *templ;
   if (templ->format == PIPE_FORMAT_NONE)
      templ2.format = whandle->format;

   uint64_t modifier = DRM_FORMAT_MOD_LINEAR;
   int modifier_count = 1;
   if (templ->target == PIPE_BUFFER) {
      modifier_count = 0;
   } else {
      if (whandle->modifier != DRM_FORMAT_MOD_INVALID)
         modifier = whandle->modifier;
      else {
         if (!zink_screen(pscreen)->driver_workarounds.can_do_invalid_linear_modifier) {
            mesa_loge("zink: display server doesn't support DRI3 modifiers and driver can't handle INVALID<->LINEAR!");
            return NULL;
         }
         whandle->modifier = modifier;
      }
   }
   templ2.bind |= ZINK_BIND_DMABUF;
   struct pipe_resource *pres = resource_create(pscreen, &templ2, whandle, usage, &modifier, modifier_count, NULL, NULL);
   if (pres) {
      struct zink_resource *res = zink_resource(pres);
      if (pres->target != PIPE_BUFFER)
         res->valid = true;
      else
         tc_buffer_disable_cpu_storage(pres);
      res->internal_format = whandle->format;
   }
   return pres;
#else
   return NULL;
#endif
}

static struct pipe_resource *
zink_resource_from_user_memory(struct pipe_screen *pscreen,
                 const struct pipe_resource *templ,
                 void *user_memory)
{
   struct zink_screen *screen = zink_screen(pscreen);
   VkDeviceSize alignMask = screen->info.ext_host_mem_props.minImportedHostPointerAlignment - 1;

   /* Validate the user_memory pointer and fail early.
    * minImportedHostPointerAlignment is required to be POT */
   if (((uintptr_t)user_memory) & alignMask)
      return NULL;

   return resource_create(pscreen, templ, NULL, 0, NULL, 0, NULL, user_memory);
}

struct zink_memory_object {
   struct pipe_memory_object b;
   struct winsys_handle whandle;
};

static struct pipe_memory_object *
zink_memobj_create_from_handle(struct pipe_screen *pscreen, struct winsys_handle *whandle, bool dedicated)
{
   struct zink_memory_object *memobj = CALLOC_STRUCT(zink_memory_object);
   if (!memobj)
      return NULL;
   memcpy(&memobj->whandle, whandle, sizeof(struct winsys_handle));
   memobj->whandle.type = ZINK_EXTERNAL_MEMORY_HANDLE;

#ifdef ZINK_USE_DMABUF

#if !defined(_WIN32)
   memobj->whandle.handle = os_dupfd_cloexec(whandle->handle);
#else
   HANDLE source_target = GetCurrentProcess();
   HANDLE out_handle;

   DuplicateHandle(source_target, whandle->handle, source_target, &out_handle, 0, false, DUPLICATE_SAME_ACCESS);
   memobj->whandle.handle = out_handle;

#endif /* _WIN32 */
#endif /* ZINK_USE_DMABUF */

   return (struct pipe_memory_object *)memobj;
}

static void
zink_memobj_destroy(struct pipe_screen *pscreen, struct pipe_memory_object *pmemobj)
{
#ifdef ZINK_USE_DMABUF
   struct zink_memory_object *memobj = (struct zink_memory_object *)pmemobj;

#if !defined(_WIN32)
   close(memobj->whandle.handle);
#else
   CloseHandle(memobj->whandle.handle);
#endif /* _WIN32 */
#endif /* ZINK_USE_DMABUF */

   FREE(pmemobj);
}

static struct pipe_resource *
zink_resource_from_memobj(struct pipe_screen *pscreen,
                          const struct pipe_resource *templ,
                          struct pipe_memory_object *pmemobj,
                          uint64_t offset)
{
   struct zink_memory_object *memobj = (struct zink_memory_object *)pmemobj;

   struct pipe_resource *pres = resource_create(pscreen, templ, &memobj->whandle, 0, NULL, 0, NULL, NULL);
   if (pres) {
      if (pres->target != PIPE_BUFFER)
         zink_resource(pres)->valid = true;
      else
         tc_buffer_disable_cpu_storage(pres);
   }
   return pres;
}

static bool
invalidate_buffer(struct zink_context *ctx, struct zink_resource *res)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);

   assert(res->base.b.target == PIPE_BUFFER);

   if (res->base.b.flags & PIPE_RESOURCE_FLAG_SPARSE)
      return false;

   /* never invalidate a resource with a fixed address */
   if (res->base.b.flags & PIPE_RESOURCE_FLAG_FIXED_ADDRESS)
      return false;

   struct pipe_box box;
   u_box_3d(0, 0, 0, res->base.b.width0, 0, 0, &box);
   if (res->valid_buffer_range.start > res->valid_buffer_range.end &&
       !zink_resource_copy_box_intersects(res, 0, &box))
      return false;

   if (res->so_valid)
      ctx->dirty_so_targets = true;
   /* force counter buffer reset */
   res->so_valid = false;

   util_range_set_empty(&res->valid_buffer_range);
   if (!zink_resource_has_usage(res))
      return false;

   struct zink_resource_object *new_obj = resource_object_create(screen, &res->base.b, NULL, NULL, NULL, 0, NULL, 0);
   if (!new_obj) {
      debug_printf("new backing resource alloc failed!\n");
      return false;
   }
   bool needs_bda = !!res->obj->bda;
   /* this ref must be transferred before rebind or else BOOM */
   zink_batch_reference_resource_move(ctx, res);
   res->obj = new_obj;
   res->queue = VK_QUEUE_FAMILY_IGNORED;
   if (needs_bda)
      zink_resource_get_address(screen, res);
   if (!zink_resource_rebind(ctx, res))
      ctx->buffer_rebind_counter = p_atomic_inc_return(&screen->buffer_rebind_counter);
   return true;
}


static void
zink_resource_invalidate(struct pipe_context *pctx, struct pipe_resource *pres)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_resource *res = zink_resource(pres);
   if (pres->target == PIPE_BUFFER)
      invalidate_buffer(ctx, res);
   else {
      if (res->valid && res->fb_bind_count) {
         bool found = false;
         if (res->aspect & (VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT)) {
            found = pres == ctx->fb_state.zsbuf.texture;
         } else {
            for (unsigned i = 0; i < ctx->fb_state.nr_cbufs; i++) {
               found |= pres == ctx->fb_state.cbufs[i].texture;
            }
         }
         ctx->rp_loadop_changed |= found;
      }
      res->valid = false;
   }
}

static void
zink_transfer_copy_bufimage(struct zink_context *ctx,
                            struct zink_resource *dst,
                            struct zink_resource *src,
                            struct zink_transfer *trans)
{
   assert((trans->base.b.usage & (PIPE_MAP_DEPTH_ONLY | PIPE_MAP_STENCIL_ONLY)) !=
          (PIPE_MAP_DEPTH_ONLY | PIPE_MAP_STENCIL_ONLY));

   bool buf2img = src->base.b.target == PIPE_BUFFER;

   int x = buf2img ? trans->offset : 0;
   assert(dst->obj->transfer_dst);
   zink_copy_image_buffer(ctx, dst, src, x, 0, 0, trans->base.b.level, &trans->base.b.box, trans->base.b.usage);
}

ALWAYS_INLINE static void
align_offset_size(const VkDeviceSize alignment, VkDeviceSize *offset, VkDeviceSize *size, VkDeviceSize obj_size)
{
   VkDeviceSize align = *offset % alignment;
   if (alignment - 1 > *offset)
      *offset = 0;
   else
      *offset -= align, *size += align;
   align = alignment - (*size % alignment);
   if (*offset + *size + align > obj_size)
      *size = obj_size - *offset;
   else
      *size += align;
}

VkMappedMemoryRange
zink_resource_init_mem_range(struct zink_screen *screen, struct zink_resource_object *obj, VkDeviceSize offset, VkDeviceSize size)
{
   assert(obj->size);
   align_offset_size(screen->info.props.limits.nonCoherentAtomSize, &offset, &size, obj->size);
   VkMappedMemoryRange range = {
      VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE,
      NULL,
      zink_bo_get_mem(obj->bo),
      offset,
      size
   };
   assert(range.size);
   return range;
}

static void *
map_resource(struct zink_screen *screen, struct zink_resource *res)
{
   assert(res->obj->host_visible);
   return zink_bo_map(screen, res->obj->bo);
}

static void
unmap_resource(struct zink_screen *screen, struct zink_resource *res)
{
   zink_bo_unmap(screen, res->obj->bo);
}

static struct zink_transfer *
create_transfer(struct zink_context *ctx, struct pipe_resource *pres, unsigned usage, const struct pipe_box *box)
{
   struct zink_transfer *trans;

   if (usage & PIPE_MAP_THREAD_SAFE)
      trans = calloc(1, sizeof(*trans));
   else if (usage & TC_TRANSFER_MAP_THREADED_UNSYNC)
      trans = slab_zalloc(&ctx->transfer_pool_unsync);
   else
      trans = slab_zalloc(&ctx->transfer_pool);
   if (!trans)
      return NULL;

   pipe_resource_reference(&trans->base.b.resource, pres);

   trans->base.b.usage = usage;
   trans->base.b.box = *box;
   return trans;
}

static void
destroy_transfer(struct zink_context *ctx, struct zink_transfer *trans)
{
   if (trans->base.b.usage & PIPE_MAP_THREAD_SAFE) {
      free(trans);
   } else {
      /* Don't use pool_transfers_unsync. We are always in the driver
       * thread. Freeing an object into a different pool is allowed.
       */
      slab_free(&ctx->transfer_pool, trans);
   }
}

static void *
zink_buffer_map(struct pipe_context *pctx,
                    struct pipe_resource *pres,
                    unsigned level,
                    unsigned usage,
                    const struct pipe_box *box,
                    struct pipe_transfer **transfer)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_screen *screen = zink_screen(pctx->screen);
   struct zink_resource *res = zink_resource(pres);
   struct zink_transfer *trans = create_transfer(ctx, pres, usage, box);
   if (!trans)
      return NULL;

   void *ptr = NULL;

   if (res->base.is_user_ptr)
      usage |= PIPE_MAP_PERSISTENT;

   /* See if the buffer range being mapped has never been initialized,
    * in which case it can be mapped unsynchronized. */
   if (!(usage & (PIPE_MAP_UNSYNCHRONIZED | TC_TRANSFER_MAP_NO_INFER_UNSYNCHRONIZED)) &&
       usage & PIPE_MAP_WRITE && !res->base.is_shared &&
       !util_ranges_intersect(&res->valid_buffer_range, box->x, box->x + box->width) &&
       !zink_resource_copy_box_intersects(res, 0, box) &&
       /* never discard exported buffers */
       res->obj->modifier == DRM_FORMAT_MOD_INVALID) {
      usage |= PIPE_MAP_UNSYNCHRONIZED;
   }

   /* If discarding the entire range, discard the whole resource instead. */
   if (usage & PIPE_MAP_DISCARD_RANGE && box->x == 0 && box->width == res->base.b.width0) {
      usage |= PIPE_MAP_DISCARD_WHOLE_RESOURCE;
   }

   /* If a buffer in VRAM is too large and the range is discarded, don't
    * map it directly. This makes sure that the buffer stays in VRAM.
    */
   bool force_discard_range = false;
   if (usage & (PIPE_MAP_DISCARD_WHOLE_RESOURCE | PIPE_MAP_DISCARD_RANGE) &&
       !(usage & PIPE_MAP_PERSISTENT) &&
       res->base.b.flags & PIPE_RESOURCE_FLAG_DONT_MAP_DIRECTLY) {
      usage &= ~(PIPE_MAP_DISCARD_WHOLE_RESOURCE | PIPE_MAP_UNSYNCHRONIZED);
      usage |= PIPE_MAP_DISCARD_RANGE;
      force_discard_range = true;
   }

   if (usage & PIPE_MAP_DISCARD_WHOLE_RESOURCE &&
       !(usage & (PIPE_MAP_UNSYNCHRONIZED | TC_TRANSFER_MAP_NO_INVALIDATE))) {
      assert(usage & PIPE_MAP_WRITE);

      if (invalidate_buffer(ctx, res)) {
         /* At this point, the buffer is always idle. */
         usage |= PIPE_MAP_UNSYNCHRONIZED;
      } else {
         /* Fall back to a temporary buffer. */
         usage |= PIPE_MAP_DISCARD_RANGE;
      }
   }

   unsigned map_offset = box->x;
   /* ideally never ever read or write to non-cached mem */
   bool is_cached_mem = (screen->info.mem_props.memoryTypes[res->obj->bo->base.base.placement].propertyFlags & VK_STAGING_RAM) == VK_STAGING_RAM;
   /* but this is only viable with a certain amount of vram since it may fully duplicate lots of large buffers */
   bool host_mem_type_check = res->obj->host_visible;
   if (screen->always_cached_upload)
      host_mem_type_check &= is_cached_mem;
   if (usage & PIPE_MAP_DISCARD_RANGE && !(usage & PIPE_MAP_PERSISTENT) &&
       (!host_mem_type_check || !(usage & (PIPE_MAP_UNSYNCHRONIZED)))) {

      /* Check if mapping this buffer would cause waiting for the GPU.
       */

      if (!host_mem_type_check || force_discard_range ||
          !zink_resource_usage_check_completion(screen, res, ZINK_RESOURCE_ACCESS_RW)) {
         /* Do a wait-free write-only transfer using a temporary buffer. */
         unsigned offset;

         if (usage & PIPE_MAP_UNSYNCHRONIZED) {
            trans->offset = box->x % MAX2(screen->info.props.limits.minMemoryMapAlignment, 1 << MIN_SLAB_ORDER);
            trans->staging_res = pipe_buffer_create(&screen->base, PIPE_BIND_LINEAR, PIPE_USAGE_STAGING, box->width + trans->offset);
            trans->unsync_upload = true;
         } else {
            /* If we are not called from the driver thread, we have
            * to use the uploader from u_threaded_context, which is
            * local to the calling thread.
            */
            struct u_upload_mgr *mgr;
            if (usage & TC_TRANSFER_MAP_THREADED_UNSYNC)
               mgr = ctx->tc->base.stream_uploader;
            else
               mgr = ctx->base.stream_uploader;
            u_upload_alloc(mgr, 0, box->width,
                        screen->info.props.limits.minMemoryMapAlignment, &offset,
                        (struct pipe_resource **)&trans->staging_res, (void **)&ptr);
            trans->offset = offset;
         }
         res = zink_resource(trans->staging_res);
         usage |= PIPE_MAP_UNSYNCHRONIZED;
         map_offset = trans->offset;
         ptr = ((uint8_t *)ptr);
      } else {
         /* At this point, the buffer is always idle (we checked it above). */
         usage |= PIPE_MAP_UNSYNCHRONIZED;
      }
   } else if (usage & ZINK_MAP_QBO) {
      usage |= PIPE_MAP_UNSYNCHRONIZED;
   } else if (usage & PIPE_MAP_DONTBLOCK) {
      /* sparse/device-local will always need to wait since it has to copy */
      if (!res->obj->host_visible)
         goto success;
      if (!zink_resource_usage_check_completion(screen, res, ZINK_RESOURCE_ACCESS_WRITE))
         goto success;
      usage |= PIPE_MAP_UNSYNCHRONIZED;
   } else if ((usage & PIPE_MAP_READ) && !(usage & PIPE_MAP_PERSISTENT) && !host_mem_type_check) {
      /* any read, non-HV write, or unmappable that reaches this point needs staging */
      if ((usage & PIPE_MAP_READ) || !res->obj->host_visible || res->base.b.flags & PIPE_RESOURCE_FLAG_DONT_MAP_DIRECTLY) {
overwrite:
         trans->offset = box->x % MAX2(screen->info.props.limits.minMemoryMapAlignment, 1 << MIN_SLAB_ORDER);
         trans->staging_res = pipe_buffer_create(&screen->base, PIPE_BIND_LINEAR, PIPE_USAGE_STAGING, box->width + trans->offset);
         if (!trans->staging_res)
            goto fail;
         struct zink_resource *staging_res = zink_resource(trans->staging_res);
         if (usage & (PIPE_MAP_THREAD_SAFE | PIPE_MAP_UNSYNCHRONIZED | TC_TRANSFER_MAP_THREADED_UNSYNC)) {
            assert(ctx != screen->copy_context);
            /* this map can't access the passed context: use the copy context */
            zink_screen_lock_context(screen);
            ctx = screen->copy_context;
         }
         if (usage & PIPE_MAP_READ)
            zink_copy_buffer(ctx, staging_res, res, trans->offset, box->x, box->width, false);
         res = staging_res;
         usage &= ~PIPE_MAP_UNSYNCHRONIZED;
         map_offset = trans->offset;
      }
   }

   if (!(usage & PIPE_MAP_UNSYNCHRONIZED)) {
      if (usage & PIPE_MAP_WRITE) {
         if (!(usage & PIPE_MAP_READ)) {
            zink_resource_usage_try_wait(ctx, res, ZINK_RESOURCE_ACCESS_RW);
            if (zink_resource_has_unflushed_usage(res))
               goto overwrite;
         }
         zink_resource_usage_wait(ctx, res, ZINK_RESOURCE_ACCESS_RW);
      } else
         zink_resource_usage_wait(ctx, res, ZINK_RESOURCE_ACCESS_WRITE);
      if (!res->real_buffer_range) {
         res->obj->access = 0;
         res->obj->access_stage = 0;
         res->obj->last_write = 0;
         zink_resource_copies_reset(res);
      }
   }

   if (!ptr) {
      /* if writing to a streamout buffer, ensure synchronization next time it's used */
      if (usage & PIPE_MAP_WRITE && res->so_valid) {
         ctx->dirty_so_targets = true;
         /* force counter buffer reset */
         res->so_valid = false;
      }
      ptr = map_resource(screen, res);
      if (!ptr)
         goto fail;
      ptr = ((uint8_t *)ptr) + map_offset;
   }

   if (!res->obj->coherent
#if defined(MVK_VERSION)
      // Work around for MoltenVk limitation specifically on coherent memory
      // MoltenVk returns blank memory ranges when there should be data present
      // This is a known limitation of MoltenVK.
      // See https://github.com/KhronosGroup/MoltenVK/blob/master/Docs/MoltenVK_Runtime_UserGuide.md#known-moltenvk-limitations

       || screen->instance_info->have_MVK_moltenvk
#endif
      ) {
      VkDeviceSize size = box->width;
      VkDeviceSize offset = res->obj->offset + trans->offset;
      VkMappedMemoryRange range = zink_resource_init_mem_range(screen, res->obj, offset, size);
      if (VKSCR(InvalidateMappedMemoryRanges)(screen->dev, 1, &range) != VK_SUCCESS) {
         mesa_loge("ZINK: vkInvalidateMappedMemoryRanges failed");
         zink_bo_unmap(screen, res->obj->bo);
         goto fail;
      }
   }
   trans->base.b.usage = usage;
   if (usage & PIPE_MAP_WRITE) {
      util_range_add(&res->base.b, &res->valid_buffer_range, box->x, box->x + box->width);

      struct zink_resource *orig_res = zink_resource(trans->base.b.resource);
      util_range_add(&orig_res->base.b, &orig_res->valid_buffer_range, box->x, box->x + box->width);
      if (orig_res->real_buffer_range)
         util_range_add(&orig_res->base.b, orig_res->real_buffer_range, box->x, box->x + box->width);
   }

success:
   /* ensure the copy context gets unlocked */
   if (ctx == screen->copy_context)
      zink_screen_unlock_context(screen);
   *transfer = &trans->base.b;
   return ptr;

fail:
   if (ctx == screen->copy_context)
      zink_screen_unlock_context(screen);
   destroy_transfer(ctx, trans);
   return NULL;
}

static void *
zink_image_map(struct pipe_context *pctx,
                  struct pipe_resource *pres,
                  unsigned level,
                  unsigned usage,
                  const struct pipe_box *box,
                  struct pipe_transfer **transfer)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_screen *screen = zink_screen(pctx->screen);
   struct zink_resource *res = zink_resource(pres);
   struct zink_transfer *trans = create_transfer(ctx, pres, usage, box);
   if (!trans)
      return NULL;

   trans->base.b.level = level;
   if (zink_is_swapchain(res))
      /* this is probably a multi-chain which has already been acquired */
      zink_kopper_acquire(ctx, res, 0);

   void *ptr;
   if (!(usage & PIPE_MAP_UNSYNCHRONIZED)) {
      if (usage & PIPE_MAP_WRITE && !(usage & PIPE_MAP_READ))
         /* this is like a blit, so we can potentially dump some clears or maybe we have to  */
         zink_fb_clears_apply_or_discard(ctx, pres, zink_rect_from_box(box), box->z, box->depth, false);
      else if (usage & PIPE_MAP_READ)
         /* if the map region intersects with any clears then we have to apply them */
         zink_fb_clears_apply_region(ctx, pres, zink_rect_from_box(box),box->z, box->depth);
   }
   if (!res->linear || !res->obj->host_visible) {
      enum pipe_format format = pres->format;
      if (usage & PIPE_MAP_DEPTH_ONLY)
         format = util_format_get_depth_only(pres->format);
      else if (usage & PIPE_MAP_STENCIL_ONLY)
         format = PIPE_FORMAT_S8_UINT;
      trans->base.b.stride = util_format_get_stride(format, box->width);
      trans->base.b.layer_stride = util_format_get_2d_size(format,
                                                         trans->base.b.stride,
                                                         box->height);

      struct pipe_resource templ = *pres;
      templ.next = NULL;
      templ.format = format;
      templ.usage = usage & PIPE_MAP_READ ? PIPE_USAGE_STAGING : PIPE_USAGE_STREAM;
      templ.target = PIPE_BUFFER;
      templ.bind = PIPE_BIND_LINEAR;
      templ.width0 = trans->base.b.layer_stride * box->depth;
      templ.height0 = templ.depth0 = 0;
      templ.last_level = 0;
      templ.array_size = 1;
      templ.flags = 0;

      trans->staging_res = zink_resource_create(pctx->screen, &templ);
      if (!trans->staging_res)
         goto fail;

      struct zink_resource *staging_res = zink_resource(trans->staging_res);

      if (usage & PIPE_MAP_READ) {
         assert(!(usage & TC_TRANSFER_MAP_THREADED_UNSYNC));
         /* force multi-context sync */
         if (zink_resource_usage_is_unflushed_write(res))
            zink_resource_usage_wait(ctx, res, ZINK_RESOURCE_ACCESS_WRITE);
         zink_transfer_copy_bufimage(ctx, staging_res, res, trans);
         /* need to wait for rendering to finish */
         zink_fence_wait(pctx);
      }

      ptr = map_resource(screen, staging_res);
   } else {
      assert(res->linear);
      ptr = map_resource(screen, res);
      if (!ptr)
         goto fail;
      if (zink_resource_has_usage(res)) {
         assert(!(usage & PIPE_MAP_UNSYNCHRONIZED));
         if (usage & PIPE_MAP_WRITE)
            zink_fence_wait(pctx);
         else
            zink_resource_usage_wait(ctx, res, ZINK_RESOURCE_ACCESS_WRITE);
      }
      VkImageSubresource isr = {
         res->modifiers ? res->obj->modifier_aspect : res->aspect,
         level,
         0
      };
      VkSubresourceLayout srl;
      VKSCR(GetImageSubresourceLayout)(screen->dev, res->obj->image, &isr, &srl);
      trans->base.b.stride = srl.rowPitch;
      if (res->base.b.target == PIPE_TEXTURE_3D)
         trans->base.b.layer_stride = srl.depthPitch;
      else
         trans->base.b.layer_stride = srl.arrayPitch;
      trans->offset = srl.offset;
      trans->depthPitch = srl.depthPitch;
      const struct util_format_description *desc = util_format_description(res->base.b.format);
      unsigned offset = srl.offset +
                        box->z * srl.depthPitch +
                        (box->y / desc->block.height) * srl.rowPitch +
                        (box->x / desc->block.width) * (desc->block.bits / 8);
      if (!res->obj->coherent) {
         VkDeviceSize size = (VkDeviceSize)box->width * box->height * desc->block.bits / 8;
         VkMappedMemoryRange range = zink_resource_init_mem_range(screen, res->obj, res->obj->offset + offset, size);
         if (VKSCR(FlushMappedMemoryRanges)(screen->dev, 1, &range) != VK_SUCCESS) {
            mesa_loge("ZINK: vkFlushMappedMemoryRanges failed");
         }
      }
      ptr = ((uint8_t *)ptr) + offset;
   }
   if (!ptr)
      goto fail;
   if (usage & PIPE_MAP_WRITE) {
      if (!res->valid && res->fb_bind_count) {
         assert(!(usage & PIPE_MAP_UNSYNCHRONIZED));
         ctx->rp_loadop_changed = true;
      }
      res->valid = true;
   }

   if (sizeof(void*) == 4)
      trans->base.b.usage |= ZINK_MAP_TEMPORARY;

   *transfer = &trans->base.b;
   return ptr;

fail:
   destroy_transfer(ctx, trans);
   return NULL;
}

static void
zink_image_subdata(struct pipe_context *pctx,
                  struct pipe_resource *pres,
                  unsigned level,
                  unsigned usage,
                  const struct pipe_box *box,
                  const void *data,
                  unsigned stride,
                  uintptr_t layer_stride)
{
   struct zink_screen *screen = zink_screen(pctx->screen);
   struct zink_context *ctx = zink_context(pctx);
   struct zink_resource *res = zink_resource(pres);

   /* flush clears to avoid subdata conflict */
   if (!(usage & TC_TRANSFER_MAP_THREADED_UNSYNC) &&
       (res->obj->vkusage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT))
      zink_fb_clears_apply_or_discard(ctx, pres, zink_rect_from_box(box), box->z, box->depth, false);
   /* only use HIC if supported on image and no pending usage */
   while (res->obj->vkusage & VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT &&
          zink_resource_usage_check_completion(screen, res, ZINK_RESOURCE_ACCESS_RW)) {
      /* uninit images are always supported */
      bool change_layout = res->layout == VK_IMAGE_LAYOUT_UNDEFINED || res->layout == VK_IMAGE_LAYOUT_PREINITIALIZED;
      if (!change_layout) {
         /* image in some other layout: test for support */
         bool can_copy_layout = false;
         for (unsigned i = 0; i < screen->info.hic_props.copyDstLayoutCount; i++) {
            if (screen->info.hic_props.pCopyDstLayouts[i] == res->layout) {
               can_copy_layout = true;
               break;
            }
         }
         /* some layouts don't permit HIC copies */
         if (!can_copy_layout)
            break;
      }
      bool is_arrayed = false;
      switch (pres->target) {
      case PIPE_TEXTURE_1D_ARRAY:
      case PIPE_TEXTURE_2D_ARRAY:
      case PIPE_TEXTURE_CUBE:
      case PIPE_TEXTURE_CUBE_ARRAY:
         is_arrayed = true;
         break;
      default: break;
      }
      /* recalc strides into texel strides because HIC spec is insane */
      unsigned vk_stride = util_format_get_stride(pres->format, 1);
      stride /= vk_stride;
      unsigned vk_layer_stride = util_format_get_2d_size(pres->format, stride, 1) * vk_stride;
      layer_stride /= vk_layer_stride;

      /* only pre-transition uninit images to avoid thrashing */
      if (change_layout)
         zink_resource_image_hic_transition(screen, res, VK_IMAGE_LAYOUT_GENERAL);
      VkMemoryToImageCopyEXT region = {
         VK_STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT,
         NULL,
         data,
         stride,
         layer_stride,
         {res->aspect, level, is_arrayed ? box->z : 0, is_arrayed ? box->depth : 1},
         {box->x, box->y, is_arrayed ? 0 : box->z},
         {box->width, box->height, is_arrayed ? 1 : box->depth}
      };
      VkCopyMemoryToImageInfoEXT copy = {
         VK_STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT,
         NULL,
         0,
         res->obj->image,
         res->layout,
         1,
         &region
      };
      VKSCR(CopyMemoryToImageEXT)(screen->dev, &copy);
      if (change_layout && !screen->driver_workarounds.general_layout && screen->can_hic_shader_read &&
          !pres->last_level && !box->x && !box->y && !box->z &&
          box->width == pres->width0 && box->height == pres->height0 &&
          ((is_arrayed && box->depth == pres->array_size) || (!is_arrayed && box->depth == pres->depth0))) {
         /* assume full copy single-mip images use shader read access */
         zink_resource_image_hic_transition(screen, res, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL);
         /* assume multi-mip where further subdata calls may happen */
      }
      /* make sure image is marked as having data */
      res->valid = true;
      return;
   }
   /* fallback case for per-resource unsupported or device-level unsupported */
   u_default_texture_subdata(pctx, pres, level, usage, box, data, stride, layer_stride);
}

static void
zink_transfer_flush_region(struct pipe_context *pctx,
                           struct pipe_transfer *ptrans,
                           const struct pipe_box *box)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_resource *res = zink_resource(ptrans->resource);
   struct zink_transfer *trans = (struct zink_transfer *)ptrans;

   if (trans->base.b.usage & PIPE_MAP_WRITE) {
      struct zink_screen *screen = zink_screen(pctx->screen);
      struct zink_resource *m = trans->staging_res ? zink_resource(trans->staging_res) :
                                                     res;
      ASSERTED VkDeviceSize size, src_offset, dst_offset = 0;
      if (m->obj->is_buffer) {
         size = box->width;
         src_offset = box->x + (trans->staging_res ? trans->offset : ptrans->box.x);
         dst_offset = box->x + ptrans->box.x;
      } else {
         size = (VkDeviceSize)box->width * box->height * util_format_get_blocksize(m->base.b.format);
         src_offset = trans->offset +
                  box->z * trans->depthPitch +
                  util_format_get_2d_size(m->base.b.format, trans->base.b.stride, box->y) +
                  util_format_get_stride(m->base.b.format, box->x);
         assert(src_offset + size <= res->obj->size);
      }
      if (!m->obj->coherent) {
         VkMappedMemoryRange range = zink_resource_init_mem_range(screen, m->obj, m->obj->offset, m->obj->size);
         if (VKSCR(FlushMappedMemoryRanges)(screen->dev, 1, &range) != VK_SUCCESS) {
            mesa_loge("ZINK: vkFlushMappedMemoryRanges failed");
         }
      }
      if (trans->staging_res) {
         struct zink_resource *staging_res = zink_resource(trans->staging_res);

         if (ptrans->resource->target == PIPE_BUFFER)
            zink_copy_buffer(ctx, res, staging_res, dst_offset, src_offset, size, trans->unsync_upload);
         else
            zink_transfer_copy_bufimage(ctx, res, staging_res, trans);
      }
   }
}

/* used to determine whether to emit a TRANSFER_DST barrier on copies */
bool
zink_resource_copy_box_intersects(struct zink_resource *res, unsigned level, const struct pipe_box *box)
{
   /* if there are no valid copy rects tracked, this needs a barrier */
   if (!res->obj->copies_valid)
      return true;
   /* untracked huge miplevel */
   if (level >= ARRAY_SIZE(res->obj->copies))
      return true;
   u_rwlock_rdlock(&res->obj->copy_lock);
   struct pipe_box *b = res->obj->copies[level].data;
   unsigned num_boxes = util_dynarray_num_elements(&res->obj->copies[level], struct pipe_box);
   bool (*intersect)(const struct pipe_box *, const struct pipe_box *);
   /* determine intersection function based on dimensionality */
   switch (res->base.b.target) {
   case PIPE_BUFFER:
   case PIPE_TEXTURE_1D:
      intersect = u_box_test_intersection_1d;
      break;

   case PIPE_TEXTURE_1D_ARRAY:
   case PIPE_TEXTURE_2D:
      intersect = u_box_test_intersection_2d;
      break;

   default:
      intersect = u_box_test_intersection_3d;
      break;
   }
   /* if any of the tracked boxes intersect with this one, a barrier is needed */
   bool ret = false;
   for (unsigned i = 0; i < num_boxes; i++) {
      if (intersect(box, b + i)) {
         ret = true;
         break;
      }
   }
   u_rwlock_rdunlock(&res->obj->copy_lock);
   /* no intersection = no barrier */
   return ret;
}

/* track a new region for TRANSFER_DST barrier emission */
void
zink_resource_copy_box_add(struct zink_context *ctx, struct zink_resource *res, unsigned level, const struct pipe_box *box)
{
   u_rwlock_wrlock(&res->obj->copy_lock);
   if (res->obj->copies_valid) {
      struct pipe_box *b = res->obj->copies[level].data;
      unsigned num_boxes = util_dynarray_num_elements(&res->obj->copies[level], struct pipe_box);
      for (unsigned i = 0; i < num_boxes; i++) {
         switch (res->base.b.target) {
         case PIPE_BUFFER:
         case PIPE_TEXTURE_1D:
            /* no-op included region */
            if (b[i].x <= box->x && b[i].x + b[i].width >= box->x + box->width)
               goto out;

            /* try to merge adjacent regions */
            if (b[i].x == box->x + box->width) {
               b[i].x -= box->width;
               b[i].width += box->width;
               goto out;
            }
            if (b[i].x + b[i].width == box->x) {
               b[i].width += box->width;
               goto out;
            }

            /* try to merge into region */
            if (box->x <= b[i].x && box->x + box->width >= b[i].x + b[i].width) {
               *b = *box;
               goto out;
            }
            break;

         case PIPE_TEXTURE_1D_ARRAY:
         case PIPE_TEXTURE_2D:
            /* no-op included region */
            if (b[i].x <= box->x && b[i].x + b[i].width >= box->x + box->width &&
                b[i].y <= box->y && b[i].y + b[i].height >= box->y + box->height)
               goto out;

            /* try to merge adjacent regions */
            if (b[i].y == box->y && b[i].height == box->height) {
               if (b[i].x == box->x + box->width) {
                  b[i].x -= box->width;
                  b[i].width += box->width;
                  goto out;
               }
               if (b[i].x + b[i].width == box->x) {
                  b[i].width += box->width;
                  goto out;
               }
            } else if (b[i].x == box->x && b[i].width == box->width) {
               if (b[i].y == box->y + box->height) {
                  b[i].y -= box->height;
                  b[i].height += box->height;
                  goto out;
               }
               if (b[i].y + b[i].height == box->y) {
                  b[i].height += box->height;
                  goto out;
               }
            }

            /* try to merge into region */
            if (box->x <= b[i].x && box->x + box->width >= b[i].x + b[i].width &&
                box->y <= b[i].y && box->y + box->height >= b[i].y + b[i].height) {
               *b = *box;
               goto out;
            }
            break;

         default:
            /* no-op included region */
            if (b[i].x <= box->x && b[i].x + b[i].width >= box->x + box->width &&
                b[i].y <= box->y && b[i].y + b[i].height >= box->y + box->height &&
                b[i].z <= box->z && b[i].z + b[i].depth >= box->z + box->depth)
               goto out;

               /* try to merge adjacent regions */
            if (b[i].z == box->z && b[i].depth == box->depth) {
               if (b[i].y == box->y && b[i].height == box->height) {
                  if (b[i].x == box->x + box->width) {
                     b[i].x -= box->width;
                     b[i].width += box->width;
                     goto out;
                  }
                  if (b[i].x + b[i].width == box->x) {
                     b[i].width += box->width;
                     goto out;
                  }
               } else if (b[i].x == box->x && b[i].width == box->width) {
                  if (b[i].y == box->y + box->height) {
                     b[i].y -= box->height;
                     b[i].height += box->height;
                     goto out;
                  }
                  if (b[i].y + b[i].height == box->y) {
                     b[i].height += box->height;
                     goto out;
                  }
               }
            } else if (b[i].x == box->x && b[i].width == box->width) {
               if (b[i].y == box->y && b[i].height == box->height) {
                  if (b[i].z == box->z + box->depth) {
                     b[i].z -= box->depth;
                     b[i].depth += box->depth;
                     goto out;
                  }
                  if (b[i].z + b[i].depth == box->z) {
                     b[i].depth += box->depth;
                     goto out;
                  }
               } else if (b[i].z == box->z && b[i].depth == box->depth) {
                  if (b[i].y == box->y + box->height) {
                     b[i].y -= box->height;
                     b[i].height += box->height;
                     goto out;
                  }
                  if (b[i].y + b[i].height == box->y) {
                     b[i].height += box->height;
                     goto out;
                  }
               }
            } else if (b[i].y == box->y && b[i].height == box->height) {
               if (b[i].z == box->z && b[i].depth == box->depth) {
                  if (b[i].x == box->x + box->width) {
                     b[i].x -= box->width;
                     b[i].width += box->width;
                     goto out;
                  }
                  if (b[i].x + b[i].width == box->x) {
                     b[i].width += box->width;
                     goto out;
                  }
               } else if (b[i].x == box->x && b[i].width == box->width) {
                  if (b[i].z == box->z + box->depth) {
                     b[i].z -= box->depth;
                     b[i].depth += box->depth;
                     goto out;
                  }
                  if (b[i].z + b[i].depth == box->z) {
                     b[i].depth += box->depth;
                     goto out;
                  }
               }
            }

            /* try to merge into region */
            if (box->x <= b[i].x && box->x + box->width >= b[i].x + b[i].width &&
                box->y <= b[i].y && box->y + box->height >= b[i].y + b[i].height &&
                box->z <= b[i].z && box->z + box->depth >= b[i].z + b[i].depth)
               goto out;

            break;
         }
      }
   }
   util_dynarray_append(&res->obj->copies[level], struct pipe_box, *box);
   if (!res->copies_warned && util_dynarray_num_elements(&res->obj->copies[level], struct pipe_box) > 100) {
      perf_debug(ctx, "zink: PERF WARNING! > 100 copy boxes detected for %p\n", res);
      mesa_logw("zink: PERF WARNING! > 100 copy boxes detected for %p\n", res);
      res->copies_warned = true;
   }
   res->obj->copies_valid = true;
out:
   u_rwlock_wrunlock(&res->obj->copy_lock);
}

void
zink_resource_copies_reset(struct zink_resource *res)
{
   if (!res->obj->copies_valid)
      return;
   u_rwlock_wrlock(&res->obj->copy_lock);
   unsigned max_level = res->base.b.target == PIPE_BUFFER ? 1 : (res->base.b.last_level + 1);
   if (res->base.b.target == PIPE_BUFFER) {
      /* flush transfer regions back to valid range on reset */
      struct pipe_box *b = res->obj->copies[0].data;
      unsigned num_boxes = util_dynarray_num_elements(&res->obj->copies[0], struct pipe_box);
      for (unsigned i = 0; i < num_boxes; i++)
         util_range_add(&res->base.b, &res->valid_buffer_range, b[i].x, b[i].x + b[i].width);
   }
   for (unsigned i = 0; i < max_level; i++)
      util_dynarray_clear(&res->obj->copies[i]);
   res->obj->copies_valid = false;
   res->obj->copies_need_reset = false;
   u_rwlock_wrunlock(&res->obj->copy_lock);
}

static void
transfer_unmap(struct pipe_context *pctx, struct pipe_transfer *ptrans)
{
   struct zink_context *ctx = zink_context(pctx);
   struct zink_transfer *trans = (struct zink_transfer *)ptrans;

   if (!(trans->base.b.usage & (PIPE_MAP_FLUSH_EXPLICIT | PIPE_MAP_COHERENT))) {
      /* flush_region is relative to the mapped region: use only the extents */
      struct pipe_box box = ptrans->box;
      box.x = box.y = box.z = 0;
      zink_transfer_flush_region(pctx, ptrans, &box);
   }

   if (trans->staging_res)
      pipe_resource_reference(&trans->staging_res, NULL);
   pipe_resource_reference(&trans->base.b.resource, NULL);

   destroy_transfer(ctx, trans);
}

static void
do_transfer_unmap(struct zink_screen *screen, struct zink_transfer *trans)
{
   struct zink_resource *res = zink_resource(trans->staging_res);
   if (!res)
      res = zink_resource(trans->base.b.resource);
   unmap_resource(screen, res);
}

void
zink_screen_buffer_unmap(struct pipe_screen *pscreen, struct pipe_transfer *ptrans)
{
   struct zink_screen *screen = zink_screen(pscreen);
   struct zink_transfer *trans = (struct zink_transfer *)ptrans;
   if (trans->base.b.usage & PIPE_MAP_ONCE && !trans->staging_res)
      do_transfer_unmap(screen, trans);
   transfer_unmap(NULL, ptrans);
}

static void
zink_buffer_unmap(struct pipe_context *pctx, struct pipe_transfer *ptrans)
{
   struct zink_screen *screen = zink_screen(pctx->screen);
   struct zink_transfer *trans = (struct zink_transfer *)ptrans;
   if (trans->base.b.usage & PIPE_MAP_ONCE && !trans->staging_res)
      do_transfer_unmap(screen, trans);
   transfer_unmap(pctx, ptrans);
}

static void
zink_image_unmap(struct pipe_context *pctx, struct pipe_transfer *ptrans)
{
   struct zink_screen *screen = zink_screen(pctx->screen);
   struct zink_transfer *trans = (struct zink_transfer *)ptrans;
   if (sizeof(void*) == 4)
      do_transfer_unmap(screen, trans);
   transfer_unmap(pctx, ptrans);
}

static void
zink_buffer_subdata(struct pipe_context *ctx, struct pipe_resource *buffer,
                    unsigned usage, unsigned offset, unsigned size, const void *data)
{
   struct pipe_transfer *transfer = NULL;
   struct pipe_box box;
   uint8_t *map = NULL;

   usage |= PIPE_MAP_WRITE;

   if (!(usage & PIPE_MAP_DIRECTLY))
      usage |= PIPE_MAP_DISCARD_RANGE;

   u_box_1d(offset, size, &box);
   map = zink_buffer_map(ctx, buffer, 0, usage, &box, &transfer);
   if (!map)
      return;

   memcpy(map, data, size);
   zink_buffer_unmap(ctx, transfer);
}

static uint64_t
zink_resource_get_address_gallium(struct pipe_screen *pscreen, struct pipe_resource *pres)
{
   if (pres->flags & PIPE_RESOURCE_FLAG_FIXED_ADDRESS)
      return zink_resource_get_address(zink_screen(pres->screen), zink_resource(pres));
   return 0;
}

static struct pipe_resource *
zink_resource_get_separate_stencil(struct pipe_resource *pres)
{
   /* For packed depth-stencil, we treat depth as the primary resource
    * and store S8 as the "second plane" resource.
    */
   if (pres->next && pres->next->format == PIPE_FORMAT_S8_UINT)
      return pres->next;

   return NULL;

}

static bool
resource_object_add_bind(struct zink_context *ctx, struct zink_resource *res, unsigned bind)
{
   /* base resource already has the cap */
   if (res->base.b.bind & bind)
      return true;
   if (res->obj->is_buffer) {
      UNREACHABLE("zink: all buffers should have this bit");
      return true;
   }
   assert(!res->obj->dt);
   zink_fb_clears_apply(ctx, &res->base.b, 0, INT32_MAX);
   bool ret = add_resource_bind(ctx, res, bind);
   if (ret)
      zink_resource_rebind(ctx, res);

   return ret;
}

bool
zink_resource_object_init_storage(struct zink_context *ctx, struct zink_resource *res)
{
   return resource_object_add_bind(ctx, res, PIPE_BIND_SHADER_IMAGE);
}

bool
zink_resource_object_init_mutable(struct zink_context *ctx, struct zink_resource *res)
{
   return resource_object_add_bind(ctx, res, ZINK_BIND_MUTABLE);
}

VkDeviceAddress
zink_resource_get_address(struct zink_screen *screen, struct zink_resource *res)
{
   assert(res->obj->is_buffer);
   if (!res->obj->bda) {
      VkBufferDeviceAddressInfo info = {
         VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
         NULL,
         res->obj->buffer
      };
      res->obj->bda = VKSCR(GetBufferDeviceAddress)(screen->dev, &info);
   }
   return res->obj->bda;
}

void
zink_resource_setup_transfer_layouts(struct zink_context *ctx, struct zink_resource *src, struct zink_resource *dst)
{
   struct zink_screen *screen = zink_screen(ctx->base.screen);
   if (src == dst) {
      /* The Vulkan 1.1 specification says the following about valid usage
       * of vkCmdBlitImage:
       *
       * "srcImageLayout must be VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR,
       *  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL or VK_IMAGE_LAYOUT_GENERAL"
       *
       * and:
       *
       * "dstImageLayout must be VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR,
       *  VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL or VK_IMAGE_LAYOUT_GENERAL"
       *
       * Since we cant have the same image in two states at the same time,
       * we're effectively left with VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR or
       * VK_IMAGE_LAYOUT_GENERAL. And since this isn't a present-related
       * operation, VK_IMAGE_LAYOUT_GENERAL seems most appropriate.
       */
      screen->image_barrier(ctx, src,
                            VK_IMAGE_LAYOUT_GENERAL,
                            VK_ACCESS_TRANSFER_READ_BIT | VK_ACCESS_TRANSFER_WRITE_BIT,
                            VK_PIPELINE_STAGE_TRANSFER_BIT);
   } else {
      screen->image_barrier(ctx, src,
                            screen->driver_workarounds.general_layout ? VK_IMAGE_LAYOUT_GENERAL : VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                            VK_ACCESS_TRANSFER_READ_BIT,
                            VK_PIPELINE_STAGE_TRANSFER_BIT);

      screen->image_barrier(ctx, dst,
                            screen->driver_workarounds.general_layout ? VK_IMAGE_LAYOUT_GENERAL : VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                            VK_ACCESS_TRANSFER_WRITE_BIT,
                            VK_PIPELINE_STAGE_TRANSFER_BIT);
   }
}

void
zink_get_depth_stencil_resources(struct pipe_resource *res,
                                 struct zink_resource **out_z,
                                 struct zink_resource **out_s)
{
   if (!res) {
      if (out_z) *out_z = NULL;
      if (out_s) *out_s = NULL;
      return;
   }

   if (res->format != PIPE_FORMAT_S8_UINT) {
      if (out_z) *out_z = zink_resource(res);
      if (out_s) *out_s = zink_resource(zink_resource_get_separate_stencil(res));
   } else {
      if (out_z) *out_z = NULL;
      if (out_s) *out_s = zink_resource(res);
   }
}

static void
zink_resource_set_separate_stencil(struct pipe_resource *pres,
                                   struct pipe_resource *stencil)
{
   assert(util_format_has_depth(util_format_description(pres->format)));
   pipe_resource_reference(&pres->next, stencil);
}

static enum pipe_format
zink_resource_get_internal_format(struct pipe_resource *pres)
{
   struct zink_resource *res = zink_resource(pres);
   return res->internal_format;
}

static const struct u_transfer_vtbl transfer_vtbl = {
   .resource_create       = zink_resource_create,
   .resource_destroy      = zink_resource_destroy,
   .transfer_map          = zink_image_map,
   .transfer_unmap        = zink_image_unmap,
   .transfer_flush_region = zink_transfer_flush_region,
   .get_internal_format   = zink_resource_get_internal_format,
   .set_stencil           = zink_resource_set_separate_stencil,
   .get_stencil           = zink_resource_get_separate_stencil,
};

bool
zink_screen_resource_init(struct pipe_screen *pscreen)
{
   struct zink_screen *screen = zink_screen(pscreen);
   pscreen->resource_create = u_transfer_helper_resource_create;
   pscreen->resource_create_with_modifiers = zink_resource_create_with_modifiers;
   pscreen->resource_create_drawable = zink_resource_create_drawable;
   pscreen->resource_destroy = u_transfer_helper_resource_destroy;
   pscreen->transfer_helper = u_transfer_helper_create(&transfer_vtbl,
      U_TRANSFER_HELPER_SEPARATE_Z32S8 | U_TRANSFER_HELPER_SEPARATE_STENCIL |
      U_TRANSFER_HELPER_INTERLEAVE_IN_PLACE |
      U_TRANSFER_HELPER_MSAA_MAP |
      (!screen->have_D24_UNORM_S8_UINT ? U_TRANSFER_HELPER_Z24_IN_Z32F : 0));

   if (screen->info.have_KHR_external_memory_fd || screen->info.have_KHR_external_memory_win32) {
      pscreen->resource_get_handle = zink_resource_get_handle;
      pscreen->resource_from_handle = zink_resource_from_handle;
   }
   if (screen->info.have_EXT_external_memory_host) {
      pscreen->resource_from_user_memory = zink_resource_from_user_memory;
   }
   if (screen->instance_info->have_KHR_external_memory_capabilities) {
      pscreen->memobj_create_from_handle = zink_memobj_create_from_handle;
      pscreen->memobj_destroy = zink_memobj_destroy;
      pscreen->resource_from_memobj = zink_resource_from_memobj;
   }
   if (screen->info.have_KHR_buffer_device_address)
      pscreen->resource_get_address = zink_resource_get_address_gallium;
   pscreen->resource_get_param = zink_resource_get_param;
   return true;
}

void
zink_context_resource_init(struct pipe_context *pctx)
{
   pctx->buffer_map = zink_buffer_map;
   pctx->buffer_unmap = zink_buffer_unmap;
   pctx->texture_map = u_transfer_helper_transfer_map;
   pctx->texture_unmap = u_transfer_helper_transfer_unmap;

   pctx->transfer_flush_region = u_transfer_helper_transfer_flush_region;
   pctx->buffer_subdata = zink_buffer_subdata;
   pctx->texture_subdata = zink_image_subdata;
   pctx->invalidate_resource = zink_resource_invalidate;
}

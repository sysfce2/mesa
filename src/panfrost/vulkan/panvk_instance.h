/*
 * Copyright © 2021 Collabora Ltd.
 * SPDX-License-Identifier: MIT
 */

#ifndef PANVK_INSTANCE_H
#define PANVK_INSTANCE_H

#include <stdint.h>

#include "vk_instance.h"
#include "util/xmlconfig.h"

#include "lib/kmod/pan_kmod.h"

enum panvk_debug_flags {
   PANVK_DEBUG_STARTUP = 1 << 0,
   PANVK_DEBUG_NIR = 1 << 1,
   PANVK_DEBUG_TRACE = 1 << 2,
   PANVK_DEBUG_SYNC = 1 << 3,
   PANVK_DEBUG_AFBC = 1 << 4,
   PANVK_DEBUG_LINEAR = 1 << 5,
   PANVK_DEBUG_DUMP = 1 << 6,
   PANVK_DEBUG_NO_KNOWN_WARN = 1 << 7,
   PANVK_DEBUG_CS = 1 << 8,
   PANVK_DEBUG_COPY_GFX = 1 << 9,
   PANVK_DEBUG_FORCE_SIMULTANEOUS = 1 << 10,
   PANVK_DEBUG_IMPLICIT_OTHERS_INV = 1 << 11,
};

#if defined(VK_USE_PLATFORM_WAYLAND_KHR) || \
    defined(VK_USE_PLATFORM_XCB_KHR) || \
    defined(VK_USE_PLATFORM_XLIB_KHR) || \
    defined(VK_USE_PLATFORM_DISPLAY_KHR)
#define PANVK_USE_WSI_PLATFORM
#endif

struct panvk_instance {
   struct vk_instance vk;

   uint32_t api_version;

   enum panvk_debug_flags debug_flags;

   struct driOptionCache dri_options;
   struct driOptionCache available_dri_options;

   uint8_t driver_build_sha[20];
   uint32_t force_vk_vendor;

   bool enable_vertex_pipeline_stores_atomics;
   bool force_enable_shader_atomics;

   struct {
      struct pan_kmod_allocator allocator;
   } kmod;
};

VK_DEFINE_HANDLE_CASTS(panvk_instance, vk.base, VkInstance,
                       VK_OBJECT_TYPE_INSTANCE)

static inline struct panvk_instance *
to_panvk_instance(struct vk_instance *instance)
{
   return container_of(instance, struct panvk_instance, vk);
}

#endif

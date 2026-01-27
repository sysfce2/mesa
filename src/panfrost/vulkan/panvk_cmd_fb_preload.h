/*
 * Copyright © 2021 Collabora Ltd.
 * SPDX-License-Identifier: MIT
 */

#ifndef PANVK_FB_PRELOAD_H
#define PANVK_FB_PRELOAD_H

#include "panvk_cmd_buffer.h"
#include "pan_desc.h"
#include "pan_fb.h"

VkResult
panvk_per_arch(cmd_fb_preload_fbinfo)(struct panvk_cmd_buffer *cmdbuf,
                                      const struct pan_fb_info *fbinfo,
                                      struct pan_fb_frame_shaders *fs_out);

VkResult panvk_per_arch(cmd_fb_preload)(struct panvk_cmd_buffer *cmdbuf,
                                        const struct pan_fb_layout *fb,
                                        const struct pan_fb_load *load,
                                        struct pan_fb_frame_shaders *fs_out);

static inline struct pan_fb_bifrost_info
pan_fb_to_fbinfo_frame_shaders(struct pan_fb_frame_shaders fs,
                               uint32_t idx)
{
   return (struct pan_fb_bifrost_info) {
      .pre_post = {
         .dcds.gpu = fs.dcd_pointer + idx * 3 * pan_size(DRAW),
         .modes = { fs.modes[0], fs.modes[1], fs.modes[2] },
      }
   };
}

#endif

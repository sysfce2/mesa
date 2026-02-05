/*
 * Copyright (C) 2026 Collabora, Ltd.
 * SPDX-License-Identifier: MIT
 */

#ifndef __PAN_FB_H
#define __PAN_FB_H

#include "genxml/gen_macros.h"
#include "util/format/u_formats.h"
#include "compiler/shader_enums.h"

struct nir_shader;
struct nir_shader_compiler_options;
struct pan_image_view;

#define PAN_MAX_RTS 8

/** Bounding box used by the framebuffer abstraction */
struct pan_fb_bbox {
   /** Minimum x/y value */
   uint16_t min_x, min_y;

   /** Maximum x/y value
    *
    * Like the hardware, the maximums here are inclusive.  A pixel is
    * in-bounds if min_x <= x <= max_x and min_y <= y <= max_y.
    */
   uint16_t max_x, max_y;
};

static inline bool
pan_fb_bbox_is_valid(struct pan_fb_bbox b)
{
   return b.min_x <= b.max_x && b.min_y <= b.max_y;
}

static inline bool
pan_fb_bbox_contains_bbox(struct pan_fb_bbox a, struct pan_fb_bbox b)
{
   return a.min_x <= b.min_x && b.max_x <= a.max_x &&
          a.min_y <= b.min_y && b.max_y <= a.max_y;
}

static inline bool
pan_fb_bbox_equal(struct pan_fb_bbox a, struct pan_fb_bbox b)
{
   return a.min_x == b.min_x && b.max_x == a.max_x &&
          a.min_y == b.min_y && b.max_y == a.max_y;
}

static inline struct pan_fb_bbox
pan_fb_bbox_clamp(struct pan_fb_bbox a, struct pan_fb_bbox b)
{
   assert(pan_fb_bbox_is_valid(a) && pan_fb_bbox_is_valid(a));
   return (struct pan_fb_bbox) {
      .min_x = CLAMP(a.min_x, b.min_x, b.max_x),
      .min_y = CLAMP(a.min_y, b.min_y, b.max_y),
      .max_x = CLAMP(a.max_x, b.min_x, b.max_x),
      .max_y = CLAMP(a.max_y, b.min_y, b.max_y),
   };
}

static inline struct pan_fb_bbox
pan_fb_bbox_align(struct pan_fb_bbox b, uint32_t align_x, uint32_t align_y)
{
   assert(util_is_power_of_two_nonzero(align_x));
   assert(util_is_power_of_two_nonzero(align_y));
   assert(align_x <= UINT16_MAX && align_y <= UINT16_MAX);

   b.min_x = ROUND_DOWN_TO(b.min_x, align_x);
   b.min_y = ROUND_DOWN_TO(b.min_y, align_y);
   b.max_x = align((uint32_t)b.max_x + 1, align_x) - 1;
   b.max_y = align((uint32_t)b.max_y + 1, align_y) - 1;

   return b;
}

static inline struct pan_fb_bbox
pan_fb_bbox_from_xywh(uint32_t x, uint32_t y, uint32_t w, uint32_t h)
{
   assert(x + w > x && x + w - 1 <= UINT16_MAX);
   assert(y + h > y && y + h - 1 <= UINT16_MAX);
   return (struct pan_fb_bbox) {
      .min_x = x,
      .min_y = y,
      .max_x = x + w - 1,
      .max_y = y + h - 1,
   };
}

/** Framebuffer layout
 *
 * This describes the layout of framebuffer in tile memory as well as various
 * properties that are properties of the framebuffer itself, nit any of the
 * image views attached to it.
 */
struct pan_fb_layout {
   /** Dimensions of the framebuffer itself, in pixels */
   uint32_t width_px, height_px;

   /** The render area
    *
    * This is the API render area (as defined by Vulkan) and may be smaller
    * than the actual bounding box of the framebuffer.  The in_bounds_load
    * and border_load members of pan_fb_load_target are relative to the
    * render area.
    */
   struct pan_fb_bbox render_area_px;

   /** The tiling area
    *
    * This is based on the render area but may be larger to accomodate tiling
    * restrictions.
    */
   struct pan_fb_bbox tiling_area_px;

   /** Sample count for this framebuffer */
   uint8_t sample_count;

   /** Color render target formats
    *
    * If a render target is unused, set it to PIPE_FORMAT_NONE.
    */
   uint8_t rt_count;
   enum pipe_format rt_formats[PAN_MAX_RTS];

   /** Depth format for this framebuffer, or PIPE_FORMAT_NONE */
   enum pipe_format z_format;

   /** Stencil format for this framebuffer, or PIPE_FORMAT_NONE */
   enum pipe_format s_format;

   /** Amount of space reserved for PLS, in bytes.
    *
    * This may overlap the color render targets in tile memory.
    */
   uint16_t pls_size_B;

   uint32_t tile_rt_budget_B;
   uint32_t tile_z_budget_B;

   /** Optimal tile buffer size, in pixels
    *
    * This must be a power of two.
    */
   unsigned tile_size_px;

   /** Amount of tile memory allocated for color render targets */
   unsigned tile_rt_alloc_B;
};

static inline bool
pan_fb_has_zs(const struct pan_fb_layout *fb)
{
   /* An intentional choice is made here to base has_zs on the formats in
    * the pan_fb_layout and not the stores being done in the end.  PanVK in
    * particular relies on the size of the final framebuffer descriptor being
    * the same for all the different variants with incremental rendering.
    */
   return fb->z_format != PIPE_FORMAT_NONE ||
          fb->s_format != PIPE_FORMAT_NONE;
}

static inline bool
pan_fb_is_fully_covered(const struct pan_fb_layout *fb)
{
   const struct pan_fb_bbox fb_area_px =
      pan_fb_bbox_from_xywh(0, 0, fb->width_px, fb->height_px);
   return pan_fb_bbox_equal(fb->render_area_px, fb_area_px);
}

static inline bool
pan_fb_has_partial_tiles(const struct pan_fb_layout *fb)
{
   assert(pan_fb_bbox_contains_bbox(fb->tiling_area_px, fb->render_area_px));
   return !pan_fb_bbox_equal(fb->tiling_area_px, fb->render_area_px);
}

#ifdef PAN_ARCH
void GENX(pan_select_fb_tile_size)(struct pan_fb_layout *fb);
#endif

enum ENUM_PACKED pan_fb_load_op {
   PAN_FB_LOAD_NONE = 0,
   PAN_FB_LOAD_CLEAR,
   PAN_FB_LOAD_IMAGE,
   PAN_FB_LOAD_OP_COUNT,
};

/** Describes how MSAA is handled when copying
 *
 * The source or destination may or may not be multisampled independently and
 * this controls how that is handled in the copy.
 */
enum ENUM_PACKED pan_fb_msaa_copy_op {
   /** Copies all samples of the source to the destination
    *
    * This requires that the source and destination sample counts be the same.
    * If the source and destination are both single-sampled, this is equivalent
    * as PAN_FB_MSAA_COPY_SINGLE or PAN_FB_MSAA_COPY_SAMPLE_0.
    */
   PAN_FB_MSAA_COPY_ALL = 0,

   /** Copies the single sample from the source to the destination
    *
    * This requires that the source be single-sampled.  Semantically, this is
    * the same as PAN_FB_MSAA_COPY_SAMPLE_0, but it's useful to make this
    * distinction in shader keys so single and multi-sampled sources can be
    * differentiated.
    */
   PAN_FB_MSAA_COPY_SINGLE,

   /** Copies sample 0 from the source to all samples in the destination */
   PAN_FB_MSAA_COPY_SAMPLE_0,

   /** Copies the average of the samples in the source to the destination */
   PAN_FB_MSAA_COPY_AVERAGE,

   PAN_FB_MSAA_COPY_OP_COUNT,
};

struct pan_fb_load_target {
   /** Load op for pixels inside the bounding box */
   enum pan_fb_load_op in_bounds_load;

   /** Load op for border (out of bounds) pixels
    *
    * This should almost always be LOAD_OP_LOAD, unless you have a very good
    * reason to make a different choice.
    */
   enum pan_fb_load_op border_load;

   /** Always load, even if no primitives intersect the tile
    *
    * This should usually be set for clears.
    */
   bool always;

   /** MSAA mode used when load op is IMAGE */
   enum pan_fb_msaa_copy_op msaa;

   /* Image view to load from, or NULL if neither load op is IMAGE */
   const struct pan_image_view *iview;

   /** Clear value, if either load op is CLEAR */
   union {
      union pipe_color_union color;
      float depth;
      uint8_t stencil;
   } clear;
};

static inline struct pan_fb_load_target
pan_fb_load_iview(const struct pan_image_view *iview)
{
   return (struct pan_fb_load_target) {
      .in_bounds_load = PAN_FB_LOAD_IMAGE,
      .border_load = PAN_FB_LOAD_IMAGE,
      .msaa = PAN_FB_MSAA_COPY_ALL,
      .iview = iview,
   };
}

struct pan_fb_load {
   /** Color render target loads */
   struct pan_fb_load_target rts[PAN_MAX_RTS];

   /** Depth target load */
   struct pan_fb_load_target z;

   /** Stencil target load */
   struct pan_fb_load_target s;
};

static inline bool
pan_fb_has_image_load(const struct pan_fb_load *load, bool include_border)
{
   if (load->z.in_bounds_load == PAN_FB_LOAD_IMAGE ||
       load->s.in_bounds_load == PAN_FB_LOAD_IMAGE)
      return true;

   if (include_border &&
       (load->z.border_load == PAN_FB_LOAD_IMAGE ||
        load->s.border_load == PAN_FB_LOAD_IMAGE))
      return true;

   for (unsigned rt = 0; rt < PAN_MAX_RTS; rt++) {
      if (load->rts[rt].in_bounds_load == PAN_FB_LOAD_IMAGE)
         return true;
      if (include_border && load->rts[rt].border_load == PAN_FB_LOAD_IMAGE)
         return true;
   }

   return false;
}

struct pan_fb_store_target {
   /** Whether or not to do a store */
   bool store;

   /** Always store, even if no primitives intersect the tile
    *
    * clean_tile_write_enable will be set if either store.always is set or
    * load.always is set for the corresponding load[s].
    */
   bool always;

   /** MSAA mode to use when copying to the destination image
    *
    * This can only be set to AVERAGE for blendable color formats.
    */
   enum pan_fb_msaa_copy_op msaa;

   /** Image view to store to */
   const struct pan_image_view *iview;
};

static inline struct pan_fb_store_target
pan_fb_store_iview(const struct pan_image_view *iview)
{
   return (struct pan_fb_store_target) {
      .store = true,
      .msaa = PAN_FB_MSAA_COPY_ALL,
      .iview = iview,
   };
}

static inline struct pan_fb_store_target
pan_fb_always_store_iview_s0(const struct pan_image_view *iview)
{
   return (struct pan_fb_store_target) {
      .store = true,
      .always = true,
      .msaa = PAN_FB_MSAA_COPY_SAMPLE_0,
      .iview = iview,
   };
}

struct pan_fb_store {
   /* Color render target stores */
   struct pan_fb_store_target rts[PAN_MAX_RTS];

   /* Depth/stencil target store
    *
    * If the attached image view has a combined depth/stencil format, both
    * depth and stencil will be written.
    */
   struct pan_fb_store_target zs;

   /* Stencil target store */
   struct pan_fb_store_target s;
};

#ifdef PAN_ARCH
void GENX(pan_align_fb_tiling_area)(struct pan_fb_layout *fb,
                                    const struct pan_fb_store *store);
#endif

static_assert(PAN_FB_LOAD_OP_COUNT <= (1 << 2),
              "pan_fb_load_op fits in 2 bits");
static_assert(PAN_FB_MSAA_COPY_OP_COUNT <= (1 << 2),
              "pan_fb_msaa_copy_op fits in 2 bits");

/* Asserts for glsl_sampler_dim glsl_base_type have to be runtime because
 * there is no MAX value we can use.
 */

PRAGMA_DIAGNOSTIC_PUSH
PRAGMA_DIAGNOSTIC_ERROR(-Wpadded)
struct pan_fb_shader_load_key_target {
   uint16_t in_bounds_load : 2;
   uint16_t border_load : 2;
   uint16_t msaa : 2;
   uint16_t dim : 2;
   uint16_t is_array : 1;
   uint16_t glsl_type : 5;
   uint16_t _pad : 2;
};
PRAGMA_DIAGNOSTIC_POP
static_assert(sizeof(struct pan_fb_shader_load_key_target) == 2,
              "This struct has no holes");

PRAGMA_DIAGNOSTIC_PUSH
PRAGMA_DIAGNOSTIC_ERROR(-Wpadded)
struct pan_fb_load_shader_key {
   struct pan_fb_shader_load_key_target rts[PAN_MAX_RTS];
   struct pan_fb_shader_load_key_target z, s;
};
PRAGMA_DIAGNOSTIC_POP
static_assert(sizeof(struct pan_fb_load_shader_key) == 2 * (PAN_MAX_RTS + 2),
              "This struct has no holes");

#ifdef PAN_ARCH
bool GENX(pan_fb_load_shader_key_fill)(struct pan_fb_load_shader_key *key,
                                       const struct pan_fb_layout *fb,
                                       const struct pan_fb_load *load,
                                       bool zs_prepass);

struct nir_shader *
GENX(pan_get_fb_load_shader)(const struct pan_fb_load_shader_key *key,
                             const struct nir_shader_compiler_options *nir_options);
#endif

#endif /* __PAN_FB_H */

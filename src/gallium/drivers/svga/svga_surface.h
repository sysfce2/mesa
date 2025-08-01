/*
 * Copyright (c) 2008-2024 Broadcom. All Rights Reserved.
 * The term “Broadcom” refers to Broadcom Inc.
 * and/or its subsidiaries.
 * SPDX-License-Identifier: MIT
 */

#ifndef SVGA_SURFACE_H
#define SVGA_SURFACE_H


#include "util/compiler.h"
#include "pipe/p_state.h"
#include "util/u_inlines.h"
#include "svga_screen_cache.h"

struct pipe_context;
struct pipe_screen;
struct svga_context;
struct svga_texture;
struct svga_winsys_surface;
enum SVGA3dSurfaceFormat;


struct svga_surface
{
   struct pipe_surface base;

   struct svga_host_surface_cache_key key;

   /*
    * Note that the handle may point at a secondary / backing resource
    * created by svga_texture_view_surface() which is something other
    * than svga_texture(base->texture)->handle.
    */
   struct svga_winsys_surface *handle;

   unsigned real_layer;
   unsigned real_level;
   unsigned real_zslice;

   bool dirty;

   /* VGPU10 */
   SVGA3dRenderTargetViewId view_id;

   /*
    * As with 'handle' above, this may point to a secondary / backing resource.
    * We can't have one resource bound as both a render target and a shader
    * resource at the same time.  But we sometimes want to do that, such as
    * for mipmap generation where we sample from one level and render into
    * another.
    * In this situation, the backed surface is the render target while the
    * original surface is the shader resource.
    */
   struct svga_surface *backed;
   unsigned age;                   /* timestamp when the backed resource is
                                    * synced with the original resource.
                                    */
};


static inline void
svga_surface_reference(struct svga_surface **dst,
                       struct svga_surface *src)
{
   pipe_surface_reference((struct pipe_surface **) dst,
                          (struct pipe_surface *) src);
}


static inline void
svga_surface_unref(struct pipe_context *pipe,
                   struct svga_surface **s)
{
   pipe_surface_unref(pipe, (struct pipe_surface **) s);
}


static inline bool
svga_surface_equal(const struct svga_surface *s1, const struct svga_surface *s2)
{
   if (s1 == NULL && s2 == NULL)
      return true;
   return pipe_surface_equal((struct pipe_surface *) s1,
                             (struct pipe_surface *) s2);
}


void
svga_mark_surfaces_dirty(struct svga_context *svga);

extern void
svga_propagate_surface(struct svga_context *svga, struct svga_surface *surf,
                       bool reset);

void
svga_propagate_rendertargets(struct svga_context *svga);

extern bool
svga_surface_needs_propagation(const struct svga_surface *s);

struct svga_winsys_surface *
svga_texture_view_surface(struct svga_context *svga,
                          struct svga_texture *tex,
                          unsigned bind_flags,
                          SVGA3dSurfaceAllFlags flags,
                          SVGA3dSurfaceFormat format,
                          unsigned start_mip,
                          unsigned num_mip,
                          int layer_pick,
                          unsigned num_layers,
                          int zslice_pick,
                          bool cacheable,
                          struct svga_host_surface_cache_key *key); /* OUT */

void
svga_texture_copy_region(struct svga_context *svga,
                         struct svga_winsys_surface *src_handle,
                         unsigned srcSubResource,
                         unsigned src_x, unsigned src_y, unsigned src_z,
                         struct svga_winsys_surface *dst_handle,
                         unsigned dstSubResource,
                         unsigned dst_x, unsigned dst_y, unsigned dst_z,
                         unsigned width, unsigned height, unsigned depth);

void
svga_texture_copy_handle(struct svga_context *svga,
                         struct svga_winsys_surface *src_handle,
                         unsigned src_x, unsigned src_y, unsigned src_z,
                         unsigned src_level, unsigned src_face,
                         struct svga_winsys_surface *dst_handle,
                         unsigned dst_x, unsigned dst_y, unsigned dst_z,
                         unsigned dst_level, unsigned dst_face,
                         unsigned width, unsigned height, unsigned depth);


struct pipe_surface *
svga_create_surface(struct pipe_context *pipe,
                    struct pipe_resource *pt,
                    const struct pipe_surface *surf_tmpl);

static inline struct svga_surface *
svga_surface(struct pipe_surface *surface)
{
   return (struct svga_surface *)surface;
}


static inline const struct svga_surface *
svga_surface_const(const struct pipe_surface *surface)
{
   return (const struct svga_surface *)surface;
}

struct pipe_surface *
svga_validate_surface_view(struct svga_context *svga, struct svga_surface *s);


static inline SVGA3dResourceType
svga_resource_type(enum pipe_texture_target target)
{
   switch (target) {
   case PIPE_BUFFER:
      return SVGA3D_RESOURCE_BUFFER;
   case PIPE_TEXTURE_1D:
   case PIPE_TEXTURE_1D_ARRAY:
      return SVGA3D_RESOURCE_TEXTURE1D;
   case PIPE_TEXTURE_RECT:
   case PIPE_TEXTURE_2D:
   case PIPE_TEXTURE_2D_ARRAY:
   case PIPE_TEXTURE_CUBE:
   case PIPE_TEXTURE_CUBE_ARRAY:
      /* drawing to cube map is treated as drawing to 2D array */
      return SVGA3D_RESOURCE_TEXTURE2D;
   case PIPE_TEXTURE_3D:
      return SVGA3D_RESOURCE_TEXTURE3D;
   default:
      assert(!"Unexpected texture target");
      return SVGA3D_RESOURCE_TEXTURE2D;
   }
}

#endif

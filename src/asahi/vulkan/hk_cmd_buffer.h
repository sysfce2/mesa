/*
 * Copyright 2024 Valve Corporation
 * Copyright 2024 Alyssa Rosenzweig
 * Copyright 2022-2023 Collabora Ltd. and Red Hat Inc.
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include "util/macros.h"

#include "util/list.h"
#include "agx_abi.h"
#include "agx_helpers.h"
#include "agx_linker.h"
#include "agx_pack.h"
#include "agx_tilebuffer.h"
#include "agx_uvs.h"
#include "libagx_dgc.h"
#include "pool.h"
#include "shader_enums.h"

#include "hk_private.h"
#include "hk_shader.h"

#include "hk_cmd_pool.h"
#include "hk_descriptor_set.h"

#include "asahi/lib/agx_nir_lower_vbo.h"
#include "util/u_dynarray.h"
#include "vulkan/vulkan_core.h"

#include "vk_command_buffer.h"

#include <stdio.h>

struct hk_buffer;
struct hk_cmd_bo;
struct hk_cmd_pool;
struct hk_image_view;
struct hk_push_descriptor_set;
struct hk_shader;
struct hk_linked_shader;
struct agx_usc_builder;
struct vk_shader;

/** Root descriptor table. */
struct hk_root_descriptor_table {
   /* Address of this descriptor itself. Must be first for reflection. */
   uint64_t root_desc_addr;

   /* Descriptor set base addresses. Must follow root_desc_addr to match our
    * push layout.
    */
   uint64_t sets[HK_MAX_SETS];

   union {
      struct {
         uint32_t view_index;
         uint32_t ppp_multisamplectl;

         /* Vertex input state */
         uint64_t attrib_base[AGX_MAX_VBUFS];
         uint32_t attrib_clamps[AGX_MAX_VBUFS];
         uint32_t attrib_strides[AGX_MAX_VBUFS];

         /* Pointer to the VS->TCS, VS->GS, or TES->GS buffer. */
         uint64_t vertex_output_buffer;

         /* Mask of outputs flowing VS->TCS, VS->GS, or TES->GS . */
         uint64_t vertex_outputs;

         /* Address of input assembly buffer if geom/tess is used, else 0 */
         uint64_t input_assembly;

         /* Address of tessellation param buffer if tessellation used, else 0 */
         uint64_t tess_params;

         /* Address of geometry param buffer if GS is used, else 0 */
         uint64_t geometry_params;

         /* Pipeline statistics queries. This is a base address with flags. */
         uint64_t pipeline_stats;
         VkQueryPipelineStatisticFlags pipeline_stats_flags;

         float blend_constant[4];
         uint16_t no_epilog_discard;
         uint16_t _pad1;
         uint16_t api_sample_mask;
         uint16_t _pad2;
         uint16_t force_never_in_shader;
         uint16_t _pad3;
         uint16_t provoking;
         uint16_t _pad4;

         /* True if there is an API geometry shader. If false, there may still
          * be a geometry shader in use (notably for transform feedback) but it
          * should not contribute to pipeline statistics.
          */
         uint16_t api_gs;
         uint16_t _pad5;

         uint16_t rasterization_stream;
         uint16_t _pad6;

         /* Mapping from varying slots written by the last vertex stage to UVS
          * indices. This mapping must be compatible with the fragment shader.
          */
         uint8_t uvs_index[VARYING_SLOT_MAX];
      } draw;
      struct {
         uint64_t group_count_addr;
         uint32_t base_group[3];
      } cs;
   };

   /* Client push constants */
   uint8_t push[HK_MAX_PUSH_SIZE];

   /* Dynamic buffer bindings */
   struct hk_buffer_address dynamic_buffers[HK_MAX_DYNAMIC_BUFFERS];

   /* Start index in dynamic_buffers where each set starts */
   uint8_t set_dynamic_buffer_start[HK_MAX_SETS];
};

/* helper macro for computing root descriptor byte offsets */
#define hk_root_descriptor_offset(member)                                      \
   offsetof(struct hk_root_descriptor_table, member)

struct hk_descriptor_state {
   bool root_dirty;
   struct hk_root_descriptor_table root;

   uint32_t set_sizes[HK_MAX_SETS];
   struct hk_descriptor_set *sets[HK_MAX_SETS];
   uint32_t sets_dirty;

   struct hk_push_descriptor_set *push[HK_MAX_SETS];
   uint32_t push_dirty;
};

struct hk_attachment {
   VkFormat vk_format;
   struct hk_image_view *iview;

   VkResolveModeFlagBits resolve_mode;
   struct hk_image_view *resolve_iview;

   bool clear;
   uint32_t clear_colour[4];
};

struct hk_bg_eot {
   uint64_t usc;
   struct agx_counts_packed counts;
};

struct hk_render_registers {
   uint32_t width, height, layers;
   uint32_t zls_width, zls_height;
   uint32_t isp_bgobjdepth;
   uint32_t isp_bgobjvals;
   struct agx_zls_control_packed zls_control, zls_control_partial;
   uint32_t depth_dimensions;
   bool process_empty_tiles;
   enum u_tristate dbias_is_int;

   struct {
      uint32_t dimensions;
      uint64_t buffer, meta;
      uint32_t stride, meta_stride;
   } depth;

   struct {
      uint64_t buffer, meta;
      uint32_t stride, meta_stride;
   } stencil;

   struct {
      struct hk_bg_eot main;
      struct hk_bg_eot partial;
   } bg;

   struct {
      struct hk_bg_eot main;
      struct hk_bg_eot partial;
   } eot;
};

struct hk_rendering_state {
   VkRenderingFlagBits flags;

   VkRect2D area;
   uint32_t layer_count;
   uint32_t view_mask;

   uint32_t color_att_count;
   struct hk_attachment color_att[HK_MAX_RTS];
   struct hk_attachment depth_att;
   struct hk_attachment stencil_att;

   struct agx_tilebuffer_layout tilebuffer;
   struct hk_render_registers cr;
};

struct hk_index_buffer_state {
   struct hk_addr_range buffer;
   enum agx_index_size size;
   uint32_t restart;
};

/* Dirty tracking bits for state not tracked by vk_dynamic_graphics_state or
 * shaders_dirty.
 */
enum hk_dirty {
   HK_DIRTY_VB = BITFIELD_BIT(0),
   HK_DIRTY_OCCLUSION = BITFIELD_BIT(1),
   HK_DIRTY_PROVOKING = BITFIELD_BIT(2),
   HK_DIRTY_VARYINGS = BITFIELD_BIT(3),
};

struct hk_graphics_state {
   struct hk_rendering_state render;
   struct hk_descriptor_state descriptors;

   enum hk_dirty dirty;

   uint64_t root;
   uint64_t draw_params;
   uint64_t draw_id_ptr;

   uint32_t shaders_dirty;
   struct hk_api_shader *shaders[MESA_SHADER_MESH + 1];

   /* Vertex buffers */
   struct hk_addr_range vb[AGX_MAX_VBUFS];

   /* Transform feedback buffers */
   struct hk_addr_range xfb[4];

   /* Is transform feedback enabled? */
   bool xfb_enabled;

   /* Internal transform feedback offset vec4.
    *
    * TODO: Strictly could be global.
    */
   uint64_t xfb_offsets;

   /* Pointer to the GPU memory backing active transform feedback queries,
    * per-stream. Zero if no query is bound.
    */
   uint64_t xfb_query[4];

   struct hk_index_buffer_state index;
   enum agx_primitive topology;
   enum agx_object_type object_type;

   /* Provoking vertex 0, 1, or 2. Usually 0 or 2 for FIRST/LAST. 1 can only be
    * set for tri fans.
    */
   uint8_t provoking;

   struct {
      enum agx_visibility_mode mode;

      /* If enabled, index of the current occlusion query in the occlusion heap.
       * There can only be one active at a time (hardware contraint).
       */
      uint16_t index;
   } occlusion;

   /* Fast linked shader data structures */
   uint64_t varyings;
   struct agx_varyings_vs linked_varyings;

   uint32_t linked_dirty;
   struct hk_linked_shader *linked[PIPE_SHADER_TYPES];
   bool generate_primitive_id;

   /* Whether blend constants are required by the active blend state */
   bool uses_blend_constant;

   /* Tessellation state */
   struct {
      uint64_t out_draws;
      uint64_t grids;
      struct hk_tess_info info;
      enum mesa_prim prim;
   } tess;

   /* Needed by vk_command_buffer::dynamic_graphics_state */
   struct vk_vertex_input_state _dynamic_vi;
   struct vk_sample_locations_state _dynamic_sl;
};

struct hk_compute_state {
   struct hk_descriptor_state descriptors;
   struct hk_api_shader *shader;
};

struct hk_cmd_push {
   void *map;
   uint64_t addr;
   uint32_t range;
   bool no_prefetch;
};

struct hk_scratch_req {
   bool main;
   bool preamble;
};

/*
 * Represents a firmware timestamp request.  Handle is a kernel timestamp object
 * handle, not a GEM handle.
 *
 * The kernel/firmware uses the handle/offset_B to write. We use the address to
 * read the results back. We could deduplicate this, but this is convenient.
 */
struct agx_timestamp_req {
   uint64_t addr;
   uint32_t handle;
   uint32_t offset_B;
};

/*
 * hk_cs represents a single control stream, to be enqueued either to the
 * CDM or VDM for compute/3D respectively.
 */
enum hk_cs_type {
   HK_CS_CDM,
   HK_CS_VDM,
};

struct hk_cs {
   struct list_head node;

   /* Parent command buffer. Convenience. */
   struct hk_cmd_buffer *cmd;

   /* Data master */
   enum hk_cs_type type;

   /* Address of the root control stream for the job */
   uint64_t addr;

   /* Fat pointer to the start of the current chunk of the control stream */
   struct agx_ptr chunk;

   /* Start pointer of the root control stream */
   void *start;

   /* Current pointer within the control stream */
   void *current;

   /* End pointer of the current chunk of the control stream */
   void *end;

   /* Whether there is more than just the root chunk */
   bool stream_linked;

   /* Whether the sampler heap is required. Although we always must maintain the
    * heap for correctness, it's often not necessary since we can push lots of
    * samplers (especially for GL/DX11-era engines).
    */
   bool uses_sampler_heap;

   /* Scratch requirements */
   struct {
      union {
         struct hk_scratch_req vs;
         struct hk_scratch_req cs;
      };

      struct hk_scratch_req fs;
   } scratch;

   /* Immediate writes, type libagx_imm_write. These all happen in parallel at
    * the end of the control stream. This accelerates queries. Implies CDM.
    */
   struct util_dynarray imm_writes;

   /* Statistics */
   struct {
      uint32_t calls, cmds, flushes, merged;
   } stats;

   /* Timestamp writes. Currently just compute end / fragment end. We could
    * flesh this out later if we want finer info. (We will, but it's not
    * required for conformance.)
    */
   struct {
      struct agx_timestamp_req end;
   } timestamp;

   /* Remaining state is for graphics only, ignored for compute */
   struct agx_tilebuffer_layout tib;

   struct util_dynarray scissor, depth_bias;
   uint64_t uploaded_scissor, uploaded_zbias;

   /* We can only set ppp_multisamplectl once per batch. has_sample_locations
    * tracks if we've committed to a set of sample locations yet. vk_meta
    * operations do not set has_sample_locations since they don't care and it
    * would interfere with the app-provided samples.
    *
    */
   bool has_sample_locations;
   uint32_t ppp_multisamplectl;

   struct hk_render_registers cr;

   /* Active restart index if one is set. Zero if there is no restart index set
    * yet, since Vulkan does not allow zero restart indices (unlike OpenGL).
    * This is used in place of dirty tracking, because dirty tracking
    * restart indices is complicated and just checking the saved value is cheap.
    */
   uint32_t restart_index;
};

/*
 * Helper to merge two compute control streams, concatenating the second control
 * stream to the first one. Must sync with hk_cs.
 */
static inline void
hk_cs_merge_cdm(struct hk_cs *a, const struct hk_cs *b)
{
   assert(a->type == HK_CS_CDM && b->type == HK_CS_CDM);
   assert(a->cmd == b->cmd);
   assert(!a->timestamp.end.handle);

   agx_cdm_jump(a->current, b->addr);
   a->current = b->current;
   a->stream_linked = true;

   a->uses_sampler_heap |= b->uses_sampler_heap;
   a->scratch.cs.main |= b->scratch.cs.main;
   a->scratch.cs.preamble |= b->scratch.cs.preamble;

   a->timestamp = b->timestamp;

   a->stats.calls += b->stats.calls;
   a->stats.cmds += b->stats.cmds;
   a->stats.flushes += b->stats.flushes;
   a->stats.merged++;
}

static inline uint64_t
hk_cs_current_addr(struct hk_cs *cs)
{
   return cs->chunk.gpu + ((uint8_t *)cs->current - (uint8_t *)cs->chunk.cpu);
}

struct hk_uploader {
   /** List of hk_cmd_bo */
   struct list_head bos;

   /* Current addresses */
   uint8_t *map;
   uint64_t base;
   uint32_t offset;
};

struct hk_cmd_buffer {
   struct vk_command_buffer vk;

   struct {
      struct hk_graphics_state gfx;
      struct hk_compute_state cs;
   } state;

   struct {
      struct hk_uploader main, usc;
   } uploader;

   /* List of all recorded control streams */
   struct list_head control_streams;

   /* Current recorded control stream */
   struct {
      /* VDM stream for 3D */
      struct hk_cs *gfx;

      /* CDM stream for compute */
      struct hk_cs *cs;

      /* CDM stream that executes immediately before the current graphics
       * control stream. Used for geometry shading, tessellation, etc.
       */
      struct hk_cs *pre_gfx;

      /* CDM stream that will execute after the current graphics control stream
       * finishes. Used for queries.
       */
      struct hk_cs *post_gfx;
   } current_cs;

   /* Are we currently inside a vk_meta operation? This alters sample location
    * behaviour.
    */
   bool in_meta;

   /* XXX: move me?
    *
    * Indirect draw generated by the indirect GS translator.
    */
   uint64_t geom_indirect;
   uint64_t geom_index_buffer;
   uint32_t geom_index_count;
   uint32_t geom_instance_count;

   /* Does the command buffer use the geometry heap? */
   bool uses_heap;

   /* Owned large BOs */
   struct util_dynarray large_bos;
};

VK_DEFINE_HANDLE_CASTS(hk_cmd_buffer, vk.base, VkCommandBuffer,
                       VK_OBJECT_TYPE_COMMAND_BUFFER)

extern const struct vk_command_buffer_ops hk_cmd_buffer_ops;

static inline struct hk_cmd_pool *
hk_cmd_buffer_pool(struct hk_cmd_buffer *cmd)
{
   return (struct hk_cmd_pool *)cmd->vk.pool;
}

/*
 * The hardware vertex shader is supplied by the last geometry stage. The
 * geometry pipeline is vertex->tess->geometry so we search backwards.
 */
static inline struct hk_shader *
hk_bound_hw_vs(struct hk_graphics_state *gfx)
{
   struct hk_api_shader *vs = gfx->shaders[MESA_SHADER_VERTEX];
   struct hk_api_shader *tes = gfx->shaders[MESA_SHADER_TESS_EVAL];
   struct hk_api_shader *gs = gfx->shaders[MESA_SHADER_GEOMETRY];

   if (gs)
      return &gs->variants[HK_GS_VARIANT_RAST];
   else if (tes)
      return &tes->variants[HK_VS_VARIANT_HW];
   else
      return &vs->variants[HK_VS_VARIANT_HW];
}

static inline struct hk_shader *
hk_bound_sw_vs(struct hk_graphics_state *gfx)
{
   struct hk_api_shader *vs = gfx->shaders[MESA_SHADER_VERTEX];
   struct hk_shader *hw_vs = hk_bound_hw_vs(gfx);

   if (hw_vs == &vs->variants[HK_VS_VARIANT_HW])
      return hw_vs;
   else
      return &vs->variants[HK_VS_VARIANT_SW];
}

static inline struct hk_shader *
hk_bound_sw_vs_before_gs(struct hk_graphics_state *gfx)
{
   struct hk_api_shader *vs = gfx->shaders[MESA_SHADER_VERTEX];
   struct hk_api_shader *tes = gfx->shaders[MESA_SHADER_TESS_EVAL];
   struct hk_api_shader *api = tes ?: vs;

   return &api->variants[HK_VS_VARIANT_SW];
}

struct agx_ptr hk_pool_alloc_internal(struct hk_cmd_buffer *cmd, uint32_t size,
                                      uint32_t alignment, bool usc);

uint64_t hk_pool_upload(struct hk_cmd_buffer *cmd, const void *data,
                        uint32_t size, uint32_t alignment);

static inline struct agx_ptr
hk_pool_alloc(struct hk_cmd_buffer *cmd, uint32_t size, uint32_t alignment)
{
   return hk_pool_alloc_internal(cmd, size, alignment, false);
}

static inline struct agx_ptr
hk_pool_usc_alloc(struct hk_cmd_buffer *cmd, uint32_t size, uint32_t alignment)
{
   return hk_pool_alloc_internal(cmd, size, alignment, true);
}

void hk_cs_init_graphics(struct hk_cmd_buffer *cmd, struct hk_cs *cs);
uint32_t hk_default_sample_positions(unsigned nr_samples);

static inline struct hk_cs *
hk_cmd_buffer_get_cs_general(struct hk_cmd_buffer *cmd, struct hk_cs **ptr,
                             bool compute)
{
   if ((*ptr) == NULL) {
      /* Allocate root control stream */
      size_t initial_size = 65536;
      struct agx_ptr root = hk_pool_alloc(cmd, initial_size, 1024);
      if (!root.cpu)
         return NULL;

      /* Allocate hk_cs for the new stream */
      struct hk_cs *cs = malloc(sizeof(*cs));
      *cs = (struct hk_cs){
         .cmd = cmd,
         .type = compute ? HK_CS_CDM : HK_CS_VDM,
         .addr = root.gpu,
         .start = root.cpu,
         .chunk = root,
         .current = root.cpu,
         .end = root.cpu + initial_size,
      };

      list_inithead(&cs->node);

      bool before_gfx = (ptr == &cmd->current_cs.pre_gfx);

      /* Insert into the command buffer. We usually append to the end of the
       * command buffer, except for pre-graphics streams which go right before
       * the graphics workload. (This implies a level of out-of-order processing
       * that's allowed by Vulkan and required for efficient
       * geometry/tessellation shaders.)
       */
      if (before_gfx && cmd->current_cs.gfx) {
         list_addtail(&cs->node, &cmd->current_cs.gfx->node);
      } else {
         list_addtail(&cs->node, &cmd->control_streams);
      }

      *ptr = cs;

      if (!compute)
         hk_cs_init_graphics(cmd, cs);
   }

   assert(*ptr != NULL);
   return *ptr;
}

static inline struct hk_cs *
hk_cmd_buffer_get_cs(struct hk_cmd_buffer *cmd, bool compute)
{
   struct hk_cs **ptr = compute ? &cmd->current_cs.cs : &cmd->current_cs.gfx;
   return hk_cmd_buffer_get_cs_general(cmd, ptr, compute);
}

void hk_ensure_cs_has_space(struct hk_cmd_buffer *cmd, struct hk_cs *cs,
                            size_t space);

static inline uint64_t
hk_cs_alloc_for_indirect(struct hk_cs *cs, size_t size_B)
{
   hk_ensure_cs_has_space(cs->cmd, cs, size_B);

   uint64_t addr = hk_cs_current_addr(cs);
   cs->current += size_B;
   return addr;
}

static void
hk_cmd_buffer_dirty_all(struct hk_cmd_buffer *cmd)
{
   struct vk_dynamic_graphics_state *dyn = &cmd->vk.dynamic_graphics_state;
   struct hk_graphics_state *gfx = &cmd->state.gfx;

   vk_dynamic_graphics_state_dirty_all(dyn);
   gfx->dirty = ~0;
   gfx->shaders_dirty = ~0;
   gfx->linked_dirty = ~0;
   gfx->descriptors.root_dirty = true;
}

static inline void
hk_cs_destroy(struct hk_cs *cs)
{
   if (cs->type == HK_CS_VDM) {
      util_dynarray_fini(&cs->scissor);
      util_dynarray_fini(&cs->depth_bias);
   } else {
      util_dynarray_fini(&cs->imm_writes);
   }

   free(cs);
}

void hk_dispatch_imm_writes(struct hk_cmd_buffer *cmd, struct hk_cs *cs);

static void
hk_cmd_buffer_end_compute_internal(struct hk_cmd_buffer *cmd,
                                   struct hk_cs **ptr)
{
   if (*ptr) {
      struct hk_cs *cs = *ptr;

      /* This control stream may write immediates as it ends. Queue the writes
       * now that we're done emitting everything else.
       */
      if (cs->imm_writes.size) {
         hk_dispatch_imm_writes(cmd, cs);
      }
   }

   *ptr = NULL;
}

static void
hk_cmd_buffer_end_compute(struct hk_cmd_buffer *cmd)
{
   hk_cmd_buffer_end_compute_internal(cmd, &cmd->current_cs.cs);
}

void hk_optimize_empty_vdm(struct hk_cmd_buffer *cmd);

static void
hk_cmd_buffer_end_graphics(struct hk_cmd_buffer *cmd)
{
   struct hk_cs *cs = cmd->current_cs.gfx;

   if (cs && cs->stats.cmds == 0) {
      hk_optimize_empty_vdm(cmd);
   } else if (cs) {
      /* Scissor and depth bias arrays are staged to dynamic arrays on the
       * CPU. When we end the control stream, they're done growing and are
       * ready for upload.
       */
      cs->uploaded_scissor =
         hk_pool_upload(cmd, cs->scissor.data, cs->scissor.size, 64);

      cs->uploaded_zbias =
         hk_pool_upload(cmd, cs->depth_bias.data, cs->depth_bias.size, 64);

      /* TODO: maybe free scissor/depth_bias now? */
      cs->current = agx_vdm_terminate(cs->current);
   }

   cmd->current_cs.gfx = NULL;

   hk_cmd_buffer_end_compute_internal(cmd, &cmd->current_cs.pre_gfx);
   hk_cmd_buffer_end_compute_internal(cmd, &cmd->current_cs.post_gfx);

   assert(cmd->current_cs.pre_gfx == NULL);
   assert(cmd->current_cs.post_gfx == NULL);

   /* We just flushed out the heap use. If we want to use it again, we'll need
    * to queue a free for it again.
    */
   cmd->uses_heap = false;
}

static inline uint64_t
hk_pipeline_stat_addr(struct hk_cmd_buffer *cmd,
                      VkQueryPipelineStatisticFlagBits stat)
{
   struct hk_root_descriptor_table *root = &cmd->state.gfx.descriptors.root;
   VkQueryPipelineStatisticFlags flags = root->draw.pipeline_stats_flags;

   if (flags & stat) {
      assert(!cmd->in_meta && "queries paused for meta");
      assert(util_bitcount(stat) == 1 && "by construction");

      /* Prefix sum to determine the compacted index in the query pool */
      uint32_t index = util_bitcount(flags & (stat - 1));

      return root->draw.pipeline_stats + (sizeof(uint64_t) * index);
   } else {
      /* Query disabled */
      return AGX_SCRATCH_PAGE_ADDRESS;
   }
}

static inline bool
hk_stat_enabled(uint64_t addr)
{
   return addr != AGX_SCRATCH_PAGE_ADDRESS;
}

void hk_cmd_buffer_begin_graphics(struct hk_cmd_buffer *cmd,
                                  const VkCommandBufferBeginInfo *pBeginInfo);
void hk_cmd_buffer_begin_compute(struct hk_cmd_buffer *cmd,
                                 const VkCommandBufferBeginInfo *pBeginInfo);

void hk_cmd_invalidate_graphics_state(struct hk_cmd_buffer *cmd);
void hk_cmd_invalidate_compute_state(struct hk_cmd_buffer *cmd);

void hk_cmd_bind_shaders(struct vk_command_buffer *vk_cmd, uint32_t stage_count,
                         const gl_shader_stage *stages,
                         struct vk_shader **const shaders);

void hk_cmd_bind_graphics_shader(struct hk_cmd_buffer *cmd,
                                 const gl_shader_stage stage,
                                 struct hk_api_shader *shader);

void hk_cmd_bind_compute_shader(struct hk_cmd_buffer *cmd,
                                struct hk_api_shader *shader);

void hk_cmd_bind_vertex_buffer(struct hk_cmd_buffer *cmd, uint32_t vb_idx,
                               struct hk_addr_range addr_range);

static inline struct hk_descriptor_state *
hk_get_descriptors_state(struct hk_cmd_buffer *cmd,
                         VkPipelineBindPoint bind_point)
{
   switch (bind_point) {
   case VK_PIPELINE_BIND_POINT_GRAPHICS:
      return &cmd->state.gfx.descriptors;
   case VK_PIPELINE_BIND_POINT_COMPUTE:
      return &cmd->state.cs.descriptors;
   default:
      UNREACHABLE("Unhandled bind point");
   }
};

void hk_cmd_buffer_flush_push_descriptors(struct hk_cmd_buffer *cmd,
                                          struct hk_descriptor_state *desc);

void hk_meta_resolve_rendering(struct hk_cmd_buffer *cmd,
                               const VkRenderingInfo *pRenderingInfo);

uint64_t hk_cmd_buffer_upload_root(struct hk_cmd_buffer *cmd,
                                   VkPipelineBindPoint bind_point);

void hk_reserve_scratch(struct hk_cmd_buffer *cmd, struct hk_cs *cs,
                        struct hk_shader *s);

uint32_t hk_upload_usc_words(struct hk_cmd_buffer *cmd, struct hk_shader *s,
                             struct hk_linked_shader *linked);

void hk_usc_upload_spilled_rt_descs(struct agx_usc_builder *b,
                                    struct hk_cmd_buffer *cmd);

void hk_cdm_cache_flush(struct hk_device *dev, struct hk_cs *cs);

void hk_dispatch_with_usc_launch(struct hk_device *dev, struct hk_cs *cs,
                                 struct agx_cdm_launch_word_0_packed launch,
                                 uint32_t usc, struct agx_grid grid,
                                 struct agx_workgroup local_size);

void hk_dispatch_with_usc(struct hk_device *dev, struct hk_cs *cs,
                          struct agx_shader_info *info, uint32_t usc,
                          struct agx_grid grid,
                          struct agx_workgroup local_size);

static inline void
hk_dispatch_with_local_size(struct hk_cmd_buffer *cmd, struct hk_cs *cs,
                            struct hk_shader *s, struct agx_grid grid,
                            struct agx_workgroup local_size)
{
   if (agx_is_shader_empty(&s->b))
      return;

   struct hk_device *dev = hk_cmd_buffer_device(cmd);
   uint32_t usc = hk_upload_usc_words(cmd, s, s->only_linked);

   hk_reserve_scratch(cmd, cs, s);
   hk_dispatch_with_usc(dev, cs, &s->b.info, usc, grid, local_size);
}

void hk_dispatch_precomp(struct hk_cmd_buffer *cmd, struct agx_grid grid,
                         enum agx_barrier barrier, enum libagx_program idx,
                         void *data, size_t data_size);

#define MESA_DISPATCH_PRECOMP hk_dispatch_precomp

void hk_queue_write(struct hk_cmd_buffer *cmd, uint64_t address, uint32_t value,
                    bool after_gfx);

void agx_fill_velem_keys(const struct vk_vertex_input_state *vi,
                         uint64_t attribs_read, struct agx_velem_key *keys);

struct agx_robustness hk_prolog_robustness(struct hk_device *dev);

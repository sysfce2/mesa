/*
 * Copyright © 2020 Advanced Micro Devices, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

/* Draw function marshalling for glthread.
 *
 * The purpose of these glDraw wrappers is to upload non-VBO vertex and
 * index data, so that glthread doesn't have to execute synchronously.
 */

#include "c99_alloca.h"

#include "api_exec_decl.h"
#include "main/glthread_marshal.h"
#include "dispatch.h"
#include "main/varray.h"

static inline unsigned
get_index_size(GLenum type)
{
   return 1 << _mesa_get_index_size_shift(type);
}

static inline GLindextype
encode_index_type(GLenum type)
{
   /* Map invalid values less than GL_UNSIGNED_BYTE to GL_UNSIGNED_BYTE - 1,
    * and invalid values greater than GL_UNSIGNED_INT to GL_UNSIGNED_INT + 1,
    * Then subtract GL_UNSIGNED_BYTE - 1. Final encoding:
    *    0 = invalid value
    *    1 = GL_UNSIGNED_BYTE
    *    2 = invalid value
    *    3 = GL_UNSIGNED_SHORT
    *    4 = invalid value
    *    5 = GL_UNSIGNED_INT
    *    6 = invalid value
    */
   const unsigned min = GL_UNSIGNED_BYTE - 1;
   const unsigned max = GL_UNSIGNED_INT + 1;
   return (GLindextype){CLAMP(type, min, max) - min};
}

static ALWAYS_INLINE struct gl_buffer_object *
upload_indices(struct gl_context *ctx, unsigned count, unsigned index_size,
               const GLvoid **indices)
{
   struct gl_buffer_object *upload_buffer = NULL;
   unsigned upload_offset = 0;

   assert(count);

   _mesa_glthread_upload(ctx, *indices, index_size * count,
                         &upload_offset, &upload_buffer, NULL, 0);
   *indices = (const GLvoid*)(intptr_t)upload_offset;

   if (!upload_buffer)
      _mesa_marshal_InternalSetError(GL_OUT_OF_MEMORY);

   return upload_buffer;
}

static ALWAYS_INLINE struct gl_buffer_object *
upload_multi_indices(struct gl_context *ctx, unsigned total_count,
                     unsigned index_size, unsigned draw_count,
                     const GLsizei *count, const GLvoid *const *indices,
                     const GLvoid **out_indices)
{
   struct gl_buffer_object *upload_buffer = NULL;
   unsigned upload_offset = 0;
   uint8_t *upload_ptr = NULL;

   assert(total_count);

   _mesa_glthread_upload(ctx, NULL, index_size * total_count,
                         &upload_offset, &upload_buffer, &upload_ptr, 0);
   if (!upload_buffer) {
      _mesa_marshal_InternalSetError(GL_OUT_OF_MEMORY);
      return NULL;
   }

   for (unsigned i = 0, offset = 0; i < draw_count; i++) {
      if (!count[i]) {
         /* Set some valid value so as not to leave it uninitialized. */
         out_indices[i] = (const GLvoid*)(intptr_t)upload_offset;
         continue;
      }

      unsigned size = count[i] * index_size;

      memcpy(upload_ptr + offset, indices[i], size);
      out_indices[i] = (const GLvoid*)(intptr_t)(upload_offset + offset);
      offset += size;
   }

   return upload_buffer;
}

static ALWAYS_INLINE bool
upload_vertices(struct gl_context *ctx, unsigned user_buffer_mask,
                unsigned start_vertex, unsigned num_vertices,
                unsigned start_instance, unsigned num_instances,
                struct gl_buffer_object **buffers, int *offsets)
{
   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;
   unsigned attrib_mask_iter = vao->Enabled;
   unsigned num_buffers = 0;

   assert((num_vertices || !(user_buffer_mask & ~vao->NonZeroDivisorMask)) &&
          (num_instances || !(user_buffer_mask & vao->NonZeroDivisorMask)));

   if (unlikely(vao->BufferInterleaved & user_buffer_mask)) {
      /* Slower upload path where some buffers reference multiple attribs,
       * so we have to use 2 while loops instead of 1.
       */
      unsigned start_offset[VERT_ATTRIB_MAX];
      unsigned end_offset[VERT_ATTRIB_MAX];
      uint32_t buffer_mask = 0;

      while (attrib_mask_iter) {
         unsigned i = u_bit_scan(&attrib_mask_iter);
         unsigned binding_index = vao->Attrib[i].BufferIndex;

         if (!(user_buffer_mask & (1 << binding_index)))
            continue;

         unsigned stride = vao->Attrib[binding_index].Stride;
         unsigned instance_div = vao->Attrib[binding_index].Divisor;
         unsigned element_size = vao->Attrib[i].ElementSize;
         unsigned offset = vao->Attrib[i].RelativeOffset;
         unsigned size;

         if (instance_div) {
            /* Per-instance attrib. */

            /* Figure out how many instances we'll render given instance_div.  We
             * can't use the typical div_round_up() pattern because the CTS uses
             * instance_div = ~0 for a test, which overflows div_round_up()'s
             * addition.
             */
            unsigned count = num_instances / instance_div;
            if (count * instance_div != num_instances)
               count++;

            offset += stride * start_instance;
            size = stride * (count - 1) + element_size;
         } else {
            /* Per-vertex attrib. */
            offset += stride * start_vertex;
            size = stride * (num_vertices - 1) + element_size;
         }

         unsigned binding_index_bit = 1u << binding_index;

         /* Update upload offsets. */
         if (!(buffer_mask & binding_index_bit)) {
            start_offset[binding_index] = offset;
            end_offset[binding_index] = offset + size;
         } else {
            if (offset < start_offset[binding_index])
               start_offset[binding_index] = offset;
            if (offset + size > end_offset[binding_index])
               end_offset[binding_index] = offset + size;
         }

         buffer_mask |= binding_index_bit;
      }

      /* Upload buffers. */
      while (buffer_mask) {
         struct gl_buffer_object *upload_buffer = NULL;
         unsigned upload_offset = 0;
         unsigned start, end;

         unsigned binding_index = u_bit_scan(&buffer_mask);

         start = start_offset[binding_index];
         end = end_offset[binding_index];
         assert(start < end);

         /* If the draw start index is non-zero, glthread can upload to offset 0,
         * which means the attrib offset has to be -(first * stride).
         * So use signed vertex buffer offsets when possible to save memory.
         */
         const void *ptr = vao->Attrib[binding_index].Pointer;
         _mesa_glthread_upload(ctx, (uint8_t*)ptr + start,
                               end - start, &upload_offset,
                               &upload_buffer, NULL, ctx->Const.VertexBufferOffsetIsInt32 ? 0 : start);
         if (!upload_buffer) {
            for (unsigned i = 0; i < num_buffers; i++)
               _mesa_reference_buffer_object(ctx, &buffers[i], NULL);

            _mesa_marshal_InternalSetError(GL_OUT_OF_MEMORY);
            return false;
         }

         buffers[num_buffers] = upload_buffer;
         offsets[num_buffers] = upload_offset - start;
         num_buffers++;
      }

      return true;
   }

   /* Faster path where all attribs are separate. */
   while (attrib_mask_iter) {
      unsigned i = u_bit_scan(&attrib_mask_iter);
      unsigned binding_index = vao->Attrib[i].BufferIndex;

      if (!(user_buffer_mask & (1 << binding_index)))
         continue;

      struct gl_buffer_object *upload_buffer = NULL;
      unsigned upload_offset = 0;
      unsigned stride = vao->Attrib[binding_index].Stride;
      unsigned instance_div = vao->Attrib[binding_index].Divisor;
      unsigned element_size = vao->Attrib[i].ElementSize;
      unsigned offset = vao->Attrib[i].RelativeOffset;
      unsigned size;

      if (instance_div) {
         /* Per-instance attrib. */

         /* Figure out how many instances we'll render given instance_div.  We
          * can't use the typical div_round_up() pattern because the CTS uses
          * instance_div = ~0 for a test, which overflows div_round_up()'s
          * addition.
          */
         unsigned count = num_instances / instance_div;
         if (count * instance_div != num_instances)
            count++;

         offset += stride * start_instance;
         size = stride * (count - 1) + element_size;
      } else {
         /* Per-vertex attrib. */
         offset += stride * start_vertex;
         size = stride * (num_vertices - 1) + element_size;
      }

      /* If the draw start index is non-zero, glthread can upload to offset 0,
       * which means the attrib offset has to be -(first * stride).
       * So use signed vertex buffer offsets when possible to save memory.
       */
      const void *ptr = vao->Attrib[binding_index].Pointer;
      _mesa_glthread_upload(ctx, (uint8_t*)ptr + offset,
                            size, &upload_offset, &upload_buffer, NULL,
                            ctx->Const.VertexBufferOffsetIsInt32 ? 0 : offset);
      if (!upload_buffer) {
         for (unsigned i = 0; i < num_buffers; i++)
            _mesa_reference_buffer_object(ctx, &buffers[i], NULL);

         _mesa_marshal_InternalSetError(GL_OUT_OF_MEMORY);
         return false;
      }

      buffers[num_buffers] = upload_buffer;
      offsets[num_buffers] = upload_offset - offset;
      num_buffers++;
   }

   return true;
}

/* DrawArraysInstanced without user buffers. */
uint32_t
_mesa_unmarshal_DrawArraysInstanced(struct gl_context *ctx,
                                    const struct marshal_cmd_DrawArraysInstanced *restrict cmd)
{
   const GLenum mode = cmd->mode;
   const GLint first = cmd->first;
   const GLsizei count = cmd->count;
   const GLsizei instance_count = cmd->primcount;

   CALL_DrawArraysInstanced(ctx->Dispatch.Current, (mode, first, count, instance_count));
   return align(sizeof(*cmd), 8) / 8;
}

struct marshal_cmd_DrawArraysInstancedBaseInstanceDrawID
{
   struct marshal_cmd_base cmd_base;
   GLenum8 mode;
   GLint first;
   GLsizei count;
   GLsizei instance_count;
   GLuint baseinstance;
   GLuint drawid;
};

uint32_t
_mesa_unmarshal_DrawArraysInstancedBaseInstanceDrawID(struct gl_context *ctx,
                                                const struct marshal_cmd_DrawArraysInstancedBaseInstanceDrawID *cmd)
{
   const GLenum mode = cmd->mode;
   const GLint first = cmd->first;
   const GLsizei count = cmd->count;
   const GLsizei instance_count = cmd->instance_count;
   const GLuint baseinstance = cmd->baseinstance;

   ctx->DrawID = cmd->drawid;
   CALL_DrawArraysInstancedBaseInstance(ctx->Dispatch.Current,
                                        (mode, first, count, instance_count,
                                         baseinstance));
   ctx->DrawID = 0;
   return align(sizeof(*cmd), 8) / 8;
}

/* DrawArraysInstancedBaseInstance with user buffers. */
struct marshal_cmd_DrawArraysUserBuf
{
   struct marshal_cmd_base cmd_base;
   GLenum8 mode;
   uint16_t num_slots;
   GLint first;
   GLsizei count;
   GLsizei instance_count;
   GLuint baseinstance;
   GLuint drawid;
   GLuint user_buffer_mask;
};

uint32_t
_mesa_unmarshal_DrawArraysUserBuf(struct gl_context *ctx,
                                  const struct marshal_cmd_DrawArraysUserBuf *restrict cmd)
{
   const GLuint user_buffer_mask = cmd->user_buffer_mask;

   /* Bind uploaded buffers if needed. */
   if (user_buffer_mask) {
      struct gl_buffer_object **buffers = (struct gl_buffer_object **)(cmd + 1);
      const int *offsets = (const int *)(buffers + util_bitcount(user_buffer_mask));

      _mesa_InternalBindVertexBuffers(ctx, buffers, offsets, user_buffer_mask);
   }

   const GLenum mode = cmd->mode;
   const GLint first = cmd->first;
   const GLsizei count = cmd->count;
   const GLsizei instance_count = cmd->instance_count;
   const GLuint baseinstance = cmd->baseinstance;

   ctx->DrawID = cmd->drawid;
   CALL_DrawArraysInstancedBaseInstance(ctx->Dispatch.Current,
                                        (mode, first, count, instance_count,
                                         baseinstance));
   ctx->DrawID = 0;
   return cmd->num_slots;
}

static inline unsigned
get_user_buffer_mask(struct gl_context *ctx)
{
   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;

   /* BufferEnabled means which attribs are enabled in terms of buffer
    * binding slots (not attrib slots).
    *
    * UserPointerMask means which buffer bindings don't have a buffer bound.
    *
    * NonNullPointerMask means which buffer bindings have a NULL pointer.
    * Those are not uploaded. This can happen when an attrib is enabled, but
    * the shader doesn't use it, so it's ignored by mesa/state_tracker.
    */
   return vao->BufferEnabled & vao->UserPointerMask & vao->NonNullPointerMask;
}

static ALWAYS_INLINE void
draw_arrays(GLuint drawid, GLenum mode, GLint first, GLsizei count,
            GLsizei instance_count, GLuint baseinstance,
            bool compiled_into_dlist, bool no_error)
{
   GET_CURRENT_CONTEXT(ctx);

   /* The main benefit of no_error is that we can discard no-op draws
    * immediately.
    */
   if (no_error && (count <= 0 || instance_count <= 0))
      return;

   if (unlikely(compiled_into_dlist && ctx->GLThread.ListMode)) {
      _mesa_glthread_finish_before(ctx, "DrawArrays");
      /* Use the function that's compiled into a display list. */
      CALL_DrawArrays(ctx->Dispatch.Current, (mode, first, count));
      return;
   }

   unsigned user_buffer_mask =
      _mesa_is_desktop_gl_core(ctx) ? 0 : get_user_buffer_mask(ctx);

   /* Fast path when nothing needs to be done.
    *
    * This is also an error path. Zero counts should still call the driver
    * for possible GL errors.
    */
   if (!user_buffer_mask ||
       (!no_error &&
        (count <= 0 || instance_count <= 0 ||   /* GL_INVALID_VALUE / no-op */
         ctx->GLThread.inside_begin_end ||      /* GL_INVALID_OPERATION */
         ctx->Dispatch.Current == ctx->Dispatch.ContextLost || /* GL_INVALID_OPERATION */
         ctx->GLThread.ListMode))) {            /* GL_INVALID_OPERATION */
      if (baseinstance == 0 && drawid == 0) {
         int cmd_size = sizeof(struct marshal_cmd_DrawArraysInstanced);
         struct marshal_cmd_DrawArraysInstanced *cmd =
            _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawArraysInstanced, cmd_size);

         cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
         cmd->first = first;
         cmd->count = count;
         cmd->primcount = instance_count;
      } else {
         int cmd_size = sizeof(struct marshal_cmd_DrawArraysInstancedBaseInstanceDrawID);
         struct marshal_cmd_DrawArraysInstancedBaseInstanceDrawID *cmd =
            _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawArraysInstancedBaseInstanceDrawID, cmd_size);

         cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
         cmd->first = first;
         cmd->count = count;
         cmd->instance_count = instance_count;
         cmd->baseinstance = baseinstance;
         cmd->drawid = drawid;
      }
      return;
   }

   /* Upload and draw. */
   struct gl_buffer_object *buffers[VERT_ATTRIB_MAX];
   int offsets[VERT_ATTRIB_MAX];

   if (!upload_vertices(ctx, user_buffer_mask, first, count, baseinstance,
                        instance_count, buffers, offsets))
      return; /* the error is set by upload_vertices */

   unsigned num_buffers = util_bitcount(user_buffer_mask);
   int buffers_size = num_buffers * sizeof(buffers[0]);
   int offsets_size = num_buffers * sizeof(int);
   int cmd_size = sizeof(struct marshal_cmd_DrawArraysUserBuf) +
                  buffers_size + offsets_size;
   struct marshal_cmd_DrawArraysUserBuf *cmd;

   cmd = _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawArraysUserBuf,
                                         cmd_size);
   cmd->num_slots = align(cmd_size, 8) / 8;
   cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
   cmd->first = first;
   cmd->count = count;
   cmd->instance_count = instance_count;
   cmd->baseinstance = baseinstance;
   cmd->drawid = drawid;
   cmd->user_buffer_mask = user_buffer_mask;

   if (user_buffer_mask) {
      char *variable_data = (char*)(cmd + 1);
      memcpy(variable_data, buffers, buffers_size);
      variable_data += buffers_size;
      memcpy(variable_data, offsets, offsets_size);
   }
}

/* MultiDrawArrays with user buffers. */
struct marshal_cmd_MultiDrawArraysUserBuf
{
   struct marshal_cmd_base cmd_base;
   GLenum8 mode;
   uint16_t num_slots;
   GLsizei draw_count;
   GLuint user_buffer_mask;
};

uint32_t
_mesa_unmarshal_MultiDrawArraysUserBuf(struct gl_context *ctx,
                                       const struct marshal_cmd_MultiDrawArraysUserBuf *restrict cmd)
{
   const GLenum mode = cmd->mode;
   const GLsizei draw_count = cmd->draw_count;
   const GLsizei real_draw_count = MAX2(draw_count, 0);
   const GLuint user_buffer_mask = cmd->user_buffer_mask;

   const char *variable_data = (const char *)(cmd + 1);
   const GLint *first = (GLint *)variable_data;
   variable_data += sizeof(GLint) * real_draw_count;
   const GLsizei *count = (GLsizei *)variable_data;

   /* Bind uploaded buffers if needed. */
   if (user_buffer_mask) {
      variable_data += sizeof(GLsizei) * real_draw_count;
      const int *offsets = (const int *)variable_data;
      variable_data += sizeof(int) * util_bitcount(user_buffer_mask);

      /* Align for pointers. */
      if ((uintptr_t)variable_data % sizeof(uintptr_t))
         variable_data += 4;

      struct gl_buffer_object **buffers = (struct gl_buffer_object **)variable_data;

      _mesa_InternalBindVertexBuffers(ctx, buffers, offsets, user_buffer_mask);
   }

   CALL_MultiDrawArrays(ctx->Dispatch.Current,
                        (mode, first, count, draw_count));
   return cmd->num_slots;
}

void GLAPIENTRY
_mesa_marshal_MultiDrawArrays(GLenum mode, const GLint *first,
                              const GLsizei *count, GLsizei draw_count)
{
   GET_CURRENT_CONTEXT(ctx);

   if (unlikely(ctx->GLThread.ListMode)) {
      _mesa_glthread_finish_before(ctx, "MultiDrawArrays");
      CALL_MultiDrawArrays(ctx->Dispatch.Current,
                           (mode, first, count, draw_count));
      return;
   }

   struct gl_buffer_object *buffers[VERT_ATTRIB_MAX];
   int offsets[VERT_ATTRIB_MAX];
   unsigned user_buffer_mask =
      _mesa_is_desktop_gl_core(ctx) || draw_count <= 0 ||
      ctx->Dispatch.Current == ctx->Dispatch.ContextLost ||
      ctx->GLThread.inside_begin_end ? 0 : get_user_buffer_mask(ctx);

   if (user_buffer_mask) {
      unsigned min_index = ~0;
      unsigned max_index_exclusive = 0;

      for (int i = 0; i < draw_count; i++) {
         GLsizei vertex_count = count[i];

         if (vertex_count < 0) {
            /* This will just call the driver to set the GL error. */
            min_index = ~0;
            break;
         }
         if (vertex_count == 0)
            continue;

         min_index = MIN2(min_index, first[i]);
         max_index_exclusive = MAX2(max_index_exclusive, first[i] + vertex_count);
      }

      if (min_index >= max_index_exclusive) {
         /* Nothing to do, but call the driver to set possible GL errors. */
         user_buffer_mask = 0;
      } else {
         /* Upload. */
         unsigned num_vertices = max_index_exclusive - min_index;

         if (!upload_vertices(ctx, user_buffer_mask, min_index, num_vertices,
                              0, 1, buffers, offsets))
            return; /* the error is set by upload_vertices */
      }
   }

   /* Add the call into the batch buffer. */
   int real_draw_count = MAX2(draw_count, 0);
   int first_size = sizeof(GLint) * real_draw_count;
   int count_size = sizeof(GLsizei) * real_draw_count;
   unsigned num_buffers = util_bitcount(user_buffer_mask);
   int buffers_size = num_buffers * sizeof(buffers[0]);
   int offsets_size = num_buffers * sizeof(int);
   int cmd_size = sizeof(struct marshal_cmd_MultiDrawArraysUserBuf) +
                  first_size + count_size + buffers_size + offsets_size;
   struct marshal_cmd_MultiDrawArraysUserBuf *cmd;

   /* Make sure cmd can fit in the batch buffer */
   if (cmd_size <= MARSHAL_MAX_CMD_SIZE) {
      cmd = _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_MultiDrawArraysUserBuf,
                                            cmd_size);
      cmd->num_slots = align(cmd_size, 8) / 8;
      cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
      cmd->draw_count = draw_count;
      cmd->user_buffer_mask = user_buffer_mask;

      char *variable_data = (char*)(cmd + 1);
      memcpy(variable_data, first, first_size);
      variable_data += first_size;
      memcpy(variable_data, count, count_size);

      if (user_buffer_mask) {
         variable_data += count_size;
         memcpy(variable_data, offsets, offsets_size);
         variable_data += offsets_size;

         /* Align for pointers. */
         if ((uintptr_t)variable_data % sizeof(uintptr_t))
            variable_data += 4;

         memcpy(variable_data, buffers, buffers_size);
      }
   } else {
      /* The call is too large, so sync and execute the unmarshal code here. */
      _mesa_glthread_finish_before(ctx, "MultiDrawArrays");

      if (user_buffer_mask) {
         _mesa_InternalBindVertexBuffers(ctx, buffers, offsets,
                                         user_buffer_mask);
      }

      CALL_MultiDrawArrays(ctx->Dispatch.Current,
                           (mode, first, count, draw_count));
   }
}

uint32_t
_mesa_unmarshal_DrawElements(struct gl_context *ctx,
                             const struct marshal_cmd_DrawElements *restrict cmd)
{
   const GLenum mode = cmd->mode;
   const GLsizei count = cmd->count;
   const GLenum type = _mesa_decode_index_type(cmd->type);
   const GLvoid *indices = cmd->indices;

   CALL_DrawElements(ctx->Dispatch.Current, (mode, count, type, indices));
   return align(sizeof(*cmd), 8) / 8;
}

uint32_t
_mesa_unmarshal_DrawElementsPacked(struct gl_context *ctx,
                                   const struct marshal_cmd_DrawElementsPacked *restrict cmd)
{
   const GLenum mode = cmd->mode;
   const GLsizei count = cmd->count;
   const GLenum type = _mesa_decode_index_type(cmd->type);
   const GLvoid *indices = (void*)(uintptr_t)cmd->indices;

   CALL_DrawElements(ctx->Dispatch.Current, (mode, count, type, indices));
   return align(sizeof(*cmd), 8) / 8;
}

uint32_t
_mesa_unmarshal_DrawElementsInstancedBaseVertex(struct gl_context *ctx,
                                                const struct marshal_cmd_DrawElementsInstancedBaseVertex *restrict cmd)
{
   const GLenum mode = cmd->mode;
   const GLsizei count = cmd->count;
   const GLenum type = _mesa_decode_index_type(cmd->type);
   const GLvoid *indices = cmd->indices;
   const GLsizei instance_count = cmd->primcount;
   const GLint basevertex = cmd->basevertex;

   CALL_DrawElementsInstancedBaseVertex(ctx->Dispatch.Current,
                                        (mode, count, type, indices,
                                         instance_count, basevertex));
   return align(sizeof(*cmd), 8) / 8;
}

uint32_t
_mesa_unmarshal_DrawElementsInstancedBaseInstance(struct gl_context *ctx,
                                                  const struct marshal_cmd_DrawElementsInstancedBaseInstance *restrict cmd)
{
   const GLenum mode = cmd->mode;
   const GLsizei count = cmd->count;
   const GLenum type = _mesa_decode_index_type(cmd->type);
   const GLvoid *indices = cmd->indices;
   const GLsizei instance_count = cmd->primcount;
   const GLint baseinstance = cmd->baseinstance;

   CALL_DrawElementsInstancedBaseInstance(ctx->Dispatch.Current,
                                          (mode, count, type, indices,
                                           instance_count, baseinstance));
   return align(sizeof(*cmd), 8) / 8;
}

uint32_t
_mesa_unmarshal_DrawElementsInstancedBaseVertexBaseInstanceDrawID(struct gl_context *ctx,
                                                                  const struct marshal_cmd_DrawElementsInstancedBaseVertexBaseInstanceDrawID *restrict cmd)
{
   const GLenum mode = cmd->mode;
   const GLsizei count = cmd->count;
   const GLenum type = _mesa_decode_index_type(cmd->type);
   const GLvoid *indices = cmd->indices;
   const GLsizei instance_count = cmd->instance_count;
   const GLint basevertex = cmd->basevertex;
   const GLuint baseinstance = cmd->baseinstance;

   ctx->DrawID = cmd->drawid;
   CALL_DrawElementsInstancedBaseVertexBaseInstance(ctx->Dispatch.Current,
                                                    (mode, count, type, indices,
                                                     instance_count, basevertex,
                                                     baseinstance));
   ctx->DrawID = 0;

   return align(sizeof(*cmd), 8) / 8;
}

uint32_t
_mesa_unmarshal_DrawElementsUserBuf(struct gl_context *ctx,
                                    const struct marshal_cmd_DrawElementsUserBuf *restrict cmd)
{
   const GLuint user_buffer_mask = cmd->user_buffer_mask;

   /* Bind uploaded buffers if needed. */
   if (user_buffer_mask) {
      struct gl_buffer_object **buffers = (struct gl_buffer_object **)(cmd + 1);
      const int *offsets = (const int *)(buffers + util_bitcount(user_buffer_mask));

      _mesa_InternalBindVertexBuffers(ctx, buffers, offsets, user_buffer_mask);
   }

   /* Draw. */
   CALL_DrawElementsUserBuf(ctx->Dispatch.Current, (cmd));

   struct gl_buffer_object *index_buffer = cmd->index_buffer;
   _mesa_reference_buffer_object(ctx, &index_buffer, NULL);
   return cmd->num_slots;
}

uint32_t
_mesa_unmarshal_DrawElementsUserBufPacked(struct gl_context *ctx,
                                    const struct marshal_cmd_DrawElementsUserBufPacked *restrict cmd)
{
   const GLuint user_buffer_mask = cmd->user_buffer_mask;

   /* Bind uploaded buffers if needed. */
   if (user_buffer_mask) {
      struct gl_buffer_object **buffers = (struct gl_buffer_object **)(cmd + 1);
      const int *offsets = (const int *)(buffers + util_bitcount(user_buffer_mask));

      _mesa_InternalBindVertexBuffers(ctx, buffers, offsets, user_buffer_mask);
   }

   /* Draw. */
   CALL_DrawElementsUserBufPacked(ctx->Dispatch.Current, (cmd));

   struct gl_buffer_object *index_buffer = cmd->index_buffer;
   _mesa_reference_buffer_object(ctx, &index_buffer, NULL);
   return cmd->num_slots;
}

static inline bool
should_convert_to_begin_end(struct gl_context *ctx, unsigned count,
                            unsigned num_upload_vertices,
                            unsigned instance_count, struct glthread_vao *vao)
{
   /* Some of these are limitations of _mesa_glthread_UnrollDrawElements.
    * Others prevent syncing, such as disallowing buffer objects because we
    * can't map them without syncing.
    */
   return ctx->API == API_OPENGL_COMPAT &&
          util_is_vbo_upload_ratio_too_large(count, num_upload_vertices) &&
          instance_count == 1 &&                /* no instancing */
          vao->CurrentElementBufferName == 0 && /* only user indices */
          !ctx->GLThread._PrimitiveRestart &&   /* no primitive restart */
          vao->UserPointerMask == vao->BufferEnabled && /* no VBOs */
          !(vao->NonZeroDivisorMask & vao->BufferEnabled); /* no instanced attribs */
}

static ALWAYS_INLINE void
draw_elements(GLuint drawid, GLenum mode, GLsizei count, GLenum type,
              const GLvoid *indices, GLsizei instance_count, GLint basevertex,
              GLuint baseinstance, bool index_bounds_valid, GLuint min_index,
              GLuint max_index, bool compiled_into_dlist, bool no_error)
{
   GET_CURRENT_CONTEXT(ctx);

   /* The main benefit of no_error is that we can discard no-op draws
    * immediately. These are plentiful in Viewperf2020/Catia1.
    */
   if (no_error && (count <= 0 || instance_count <= 0))
      return;

   if (unlikely(compiled_into_dlist && ctx->GLThread.ListMode)) {
      _mesa_glthread_finish_before(ctx, "DrawElements");

      /* Only use the ones that are compiled into display lists. */
      if (basevertex) {
         CALL_DrawElementsBaseVertex(ctx->Dispatch.Current,
                                     (mode, count, type, indices, basevertex));
      } else if (index_bounds_valid) {
         CALL_DrawRangeElements(ctx->Dispatch.Current,
                                (mode, min_index, max_index, count, type, indices));
      } else {
         CALL_DrawElements(ctx->Dispatch.Current, (mode, count, type, indices));
      }
      return;
   }

   if (unlikely(!no_error && index_bounds_valid && max_index < min_index)) {
      _mesa_marshal_InternalSetError(GL_INVALID_VALUE);
      return;
   }

   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;
   unsigned user_buffer_mask =
      _mesa_is_desktop_gl_core(ctx) ? 0 : get_user_buffer_mask(ctx);
   bool has_user_indices = vao->CurrentElementBufferName == 0 && indices;

   /* Fast path when nothing needs to be done.
    *
    * This is also an error path. Zero counts should still call the driver
    * for possible GL errors.
    */
   if ((!user_buffer_mask && !has_user_indices) ||
       (!no_error &&
        /* zeros are discarded for no_error at the beginning */
        (count <= 0 || instance_count <= 0 ||   /* GL_INVALID_VALUE / no-op */
         !_mesa_is_index_type_valid(type) ||    /* GL_INVALID_VALUE */
         ctx->Dispatch.Current == ctx->Dispatch.ContextLost || /* GL_INVALID_OPERATION */
         ctx->GLThread.inside_begin_end ||      /* GL_INVALID_OPERATION */
         ctx->GLThread.ListMode ||              /* GL_INVALID_OPERATION */
         mode >= 32 || !((1u << mode) & ctx->SupportedPrimMask) /* GL_INVALID_ENUM */
         ))) {
      if (drawid == 0 && baseinstance == 0) {
         if (instance_count == 1 && basevertex == 0) {
            if ((count & 0xffff) == count && (uintptr_t)indices <= UINT16_MAX) {
               /* Packed version of DrawElements: 16-bit count and 16-bit index offset,
                * reducing the call size by 8 bytes.
                * This is the most common case in Viewperf2020/Catia1.
                */
               int cmd_size = sizeof(struct marshal_cmd_DrawElementsPacked);
               struct marshal_cmd_DrawElementsPacked *cmd =
                     _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawElementsPacked, cmd_size);

               cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
               cmd->type = encode_index_type(type);
               cmd->count = count;
               cmd->indices = (uintptr_t)indices;
            } else {
               int cmd_size = sizeof(struct marshal_cmd_DrawElements);
               struct marshal_cmd_DrawElements *cmd =
                     _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawElements, cmd_size);

               cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
               cmd->type = encode_index_type(type);
               cmd->count = count;
               cmd->indices = indices;
            }
         } else {
            int cmd_size = sizeof(struct marshal_cmd_DrawElementsInstancedBaseVertex);
            struct marshal_cmd_DrawElementsInstancedBaseVertex *cmd =
                  _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawElementsInstancedBaseVertex, cmd_size);

            cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
            cmd->type = encode_index_type(type);
            cmd->count = count;
            cmd->primcount = instance_count;
            cmd->basevertex = basevertex;
            cmd->indices = indices;
         }
      } else if (drawid == 0 && basevertex == 0) {
         int cmd_size = sizeof(struct marshal_cmd_DrawElementsInstancedBaseInstance);
         struct marshal_cmd_DrawElementsInstancedBaseInstance *cmd =
               _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawElementsInstancedBaseInstance, cmd_size);

         cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
         cmd->type = encode_index_type(type);
         cmd->count = count;
         cmd->primcount = instance_count;
         cmd->baseinstance = baseinstance;
         cmd->indices = indices;
      } else {
         int cmd_size = sizeof(struct marshal_cmd_DrawElementsInstancedBaseVertexBaseInstanceDrawID);
         struct marshal_cmd_DrawElementsInstancedBaseVertexBaseInstanceDrawID *cmd =
            _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawElementsInstancedBaseVertexBaseInstanceDrawID, cmd_size);

         cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
         cmd->type = encode_index_type(type);
         cmd->count = count;
         cmd->instance_count = instance_count;
         cmd->basevertex = basevertex;
         cmd->baseinstance = baseinstance;
         cmd->drawid = drawid;
         cmd->indices = indices;
      }
      return;
   }

   bool need_index_bounds = user_buffer_mask & ~vao->NonZeroDivisorMask;
   unsigned index_size = get_index_size(type);

   if (need_index_bounds && !index_bounds_valid) {
      /* Compute the index bounds. */
      if (has_user_indices) {
         min_index = ~0;
         max_index = 0;
         vbo_get_minmax_index_mapped(count, index_size,
                                     ctx->GLThread._RestartIndex[index_size - 1],
                                     ctx->GLThread._PrimitiveRestart, indices,
                                     &min_index, &max_index);
      } else {
         /* Indices in a buffer. */
         _mesa_glthread_finish_before(ctx, "DrawElements - need index bounds");
         vbo_get_minmax_index(ctx, ctx->Array.VAO->IndexBufferObj,
                              NULL, (intptr_t)indices, count, index_size,
                              ctx->GLThread._PrimitiveRestart,
                              ctx->GLThread._RestartIndex[index_size - 1],
                              &min_index, &max_index);
      }
      index_bounds_valid = true;
   }

   unsigned start_vertex = min_index + basevertex;
   unsigned num_vertices = max_index + 1 - min_index;

   /* If the vertex range to upload is much greater than the vertex count (e.g.
    * only 3 vertices with indices 0, 1, 999999), uploading the whole range
    * would take too much time. If all buffers are user buffers, have glthread
    * fetch all indices and vertices and convert the draw into glBegin/glEnd.
    * For such pathological cases, it's the fastest way.
    *
    * The game Cogs benefits from this - its FPS increases from 0 to 197.
    */
   if (should_convert_to_begin_end(ctx, count, num_vertices, instance_count,
                                   vao)) {
      _mesa_glthread_UnrollDrawElements(ctx, mode, count, type, indices,
                                        basevertex);
      return;
   }

   struct gl_buffer_object *buffers[VERT_ATTRIB_MAX];
   int offsets[VERT_ATTRIB_MAX];

   if (user_buffer_mask) {
      if (!upload_vertices(ctx, user_buffer_mask, start_vertex, num_vertices,
                           baseinstance, instance_count, buffers, offsets))
         return; /* the error is set by upload_vertices */
   }

   /* Upload indices. */
   struct gl_buffer_object *index_buffer = NULL;
   if (has_user_indices) {
      index_buffer = upload_indices(ctx, count, index_size, &indices);
      if (!index_buffer)
         return; /* the error is set by upload_indices */
   }

   /* Draw asynchronously. */
   unsigned num_buffers = util_bitcount(user_buffer_mask);
   int buffers_size = num_buffers * sizeof(buffers[0]);
   int offsets_size = num_buffers * sizeof(int);
   char *variable_data;

   if (instance_count == 1 && basevertex == 0 && baseinstance == 0 &&
       drawid == 0 && (count & 0xffff) == count &&
       (uintptr_t)indices <= UINT32_MAX) {
      int cmd_size = sizeof(struct marshal_cmd_DrawElementsUserBufPacked) +
                     buffers_size + offsets_size;
      struct marshal_cmd_DrawElementsUserBufPacked *cmd;

      cmd = _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawElementsUserBufPacked, cmd_size);
      cmd->num_slots = align(cmd_size, 8) / 8;
      cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
      cmd->type = encode_index_type(type);
      cmd->count = count; /* truncated */
      cmd->indices = (uintptr_t)indices; /* truncated */
      cmd->user_buffer_mask = user_buffer_mask;
      cmd->index_buffer = index_buffer;
      variable_data = (char*)(cmd + 1);
   } else {
      int cmd_size = sizeof(struct marshal_cmd_DrawElementsUserBuf) +
                     buffers_size + offsets_size;
      struct marshal_cmd_DrawElementsUserBuf *cmd;

      cmd = _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawElementsUserBuf, cmd_size);
      cmd->num_slots = align(cmd_size, 8) / 8;
      cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
      cmd->type = encode_index_type(type);
      cmd->count = count;
      cmd->indices = indices;
      cmd->instance_count = instance_count;
      cmd->basevertex = basevertex;
      cmd->baseinstance = baseinstance;
      cmd->user_buffer_mask = user_buffer_mask;
      cmd->index_buffer = index_buffer;
      cmd->drawid = drawid;
      variable_data = (char*)(cmd + 1);
   }

   if (user_buffer_mask) {
      memcpy(variable_data, buffers, buffers_size);
      variable_data += buffers_size;
      memcpy(variable_data, offsets, offsets_size);
   }
}

struct marshal_cmd_MultiDrawElementsUserBuf
{
   struct marshal_cmd_base cmd_base;
   bool has_base_vertex;
   GLenum8 mode;
   GLindextype type;
   uint16_t num_slots;
   GLsizei draw_count;
   GLuint user_buffer_mask;
   struct gl_buffer_object *index_buffer;
};

uint32_t
_mesa_unmarshal_MultiDrawElementsUserBuf(struct gl_context *ctx,
                                         const struct marshal_cmd_MultiDrawElementsUserBuf *restrict cmd)
{
   const GLsizei draw_count = cmd->draw_count;
   const GLsizei real_draw_count = MAX2(draw_count, 0);
   const GLuint user_buffer_mask = cmd->user_buffer_mask;
   const bool has_base_vertex = cmd->has_base_vertex;

   const char *variable_data = (const char *)(cmd + 1);
   const GLsizei *count = (GLsizei *)variable_data;
   variable_data += sizeof(GLsizei) * real_draw_count;
   const GLsizei *basevertex = NULL;
   if (has_base_vertex) {
      basevertex = (GLsizei *)variable_data;
      variable_data += sizeof(GLsizei) * real_draw_count;
   }
   const int *offsets = NULL;
   if (user_buffer_mask) {
      offsets = (const int *)variable_data;
      variable_data += sizeof(int) * util_bitcount(user_buffer_mask);
   }

   /* Align for pointers. */
   if ((uintptr_t)variable_data % sizeof(uintptr_t))
      variable_data += 4;

   const GLvoid *const *indices = (const GLvoid *const *)variable_data;
   variable_data += sizeof(const GLvoid *const *) * real_draw_count;

   /* Bind uploaded buffers if needed. */
   if (user_buffer_mask) {
      struct gl_buffer_object **buffers = (struct gl_buffer_object **)variable_data;

      _mesa_InternalBindVertexBuffers(ctx, buffers, offsets, user_buffer_mask);
   }

   /* Draw. */
   const GLenum mode = cmd->mode;
   const GLenum type = _mesa_decode_index_type(cmd->type);
   struct gl_buffer_object *index_buffer = cmd->index_buffer;

   CALL_MultiDrawElementsUserBuf(ctx->Dispatch.Current,
                                 ((GLintptr)index_buffer, mode, count, type,
                                  indices, draw_count, basevertex));
   _mesa_reference_buffer_object(ctx, &index_buffer, NULL);
   return cmd->num_slots;
}

static void
multi_draw_elements_async(struct gl_context *ctx, GLenum mode,
                          const GLsizei *count, GLenum type,
                          const GLvoid *const *indices, GLsizei draw_count,
                          const GLsizei *basevertex,
                          struct gl_buffer_object *index_buffer,
                          unsigned user_buffer_mask,
                          struct gl_buffer_object **buffers,
                          const int *offsets)
{
   int real_draw_count = MAX2(draw_count, 0);
   int count_size = sizeof(GLsizei) * real_draw_count;
   int indices_size = sizeof(indices[0]) * real_draw_count;
   int basevertex_size = basevertex ? sizeof(GLsizei) * real_draw_count : 0;
   unsigned num_buffers = util_bitcount(user_buffer_mask);
   int buffers_size = num_buffers * sizeof(buffers[0]);
   int offsets_size = num_buffers * sizeof(int);
   int cmd_size = sizeof(struct marshal_cmd_MultiDrawElementsUserBuf) +
                  count_size + indices_size + basevertex_size + buffers_size +
                  offsets_size;
   struct marshal_cmd_MultiDrawElementsUserBuf *cmd;

   /* Make sure cmd can fit the queue buffer */
   if (cmd_size <= MARSHAL_MAX_CMD_SIZE) {
      cmd = _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_MultiDrawElementsUserBuf, cmd_size);
      cmd->num_slots = align(cmd_size, 8) / 8;
      cmd->mode = MIN2(mode, 0xff); /* primitive types go from 0 to 14 */
      cmd->type = encode_index_type(type);
      cmd->draw_count = draw_count;
      cmd->user_buffer_mask = user_buffer_mask;
      cmd->index_buffer = index_buffer;
      cmd->has_base_vertex = basevertex != NULL;

      char *variable_data = (char*)(cmd + 1);
      memcpy(variable_data, count, count_size);
      variable_data += count_size;
      if (basevertex) {
         memcpy(variable_data, basevertex, basevertex_size);
         variable_data += basevertex_size;
      }
      if (user_buffer_mask) {
         memcpy(variable_data, offsets, offsets_size);
         variable_data += offsets_size;
      }

      /* Align for pointers. */
      if ((uintptr_t)variable_data % sizeof(uintptr_t))
         variable_data += 4;

      memcpy(variable_data, indices, indices_size);
      variable_data += indices_size;

      if (user_buffer_mask)
         memcpy(variable_data, buffers, buffers_size);
   } else {
      /* The call is too large, so sync and execute the unmarshal code here. */
      _mesa_glthread_finish_before(ctx, "DrawElements");

      /* Bind uploaded buffers if needed. */
      if (user_buffer_mask) {
         _mesa_InternalBindVertexBuffers(ctx, buffers, offsets,
                                         user_buffer_mask);
      }

      /* Draw. */
      CALL_MultiDrawElementsUserBuf(ctx->Dispatch.Current,
                                    ((GLintptr)index_buffer, mode, count,
                                     type, indices, draw_count, basevertex));
      _mesa_reference_buffer_object(ctx, &index_buffer, NULL);
   }
}

void GLAPIENTRY
_mesa_marshal_MultiDrawElementsBaseVertex(GLenum mode, const GLsizei *count,
                                          GLenum type,
                                          const GLvoid *const *indices,
                                          GLsizei draw_count,
                                          const GLsizei *basevertex)
{
   GET_CURRENT_CONTEXT(ctx);

   if (unlikely(ctx->GLThread.ListMode)) {
      _mesa_glthread_finish_before(ctx, "MultiDrawElements");

      if (basevertex) {
         CALL_MultiDrawElementsBaseVertex(ctx->Dispatch.Current,
                                          (mode, count, type, indices, draw_count,
                                           basevertex));
      } else {
         CALL_MultiDrawElements(ctx->Dispatch.Current,
                                (mode, count, type, indices, draw_count));
      }
      return;
   }

   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;
   unsigned user_buffer_mask = 0;
   bool has_user_indices = false;

   /* Non-VBO vertex arrays are used only if this is true.
    * When nothing needs to be uploaded or the draw is no-op or generates
    * a GL error, we don't upload anything.
    */
   if (draw_count > 0 && _mesa_is_index_type_valid(type) &&
       ctx->Dispatch.Current != ctx->Dispatch.ContextLost &&
       !ctx->GLThread.inside_begin_end &&
       !(mode >= 32 || !((1u << mode) & ctx->SupportedPrimMask))) {
      user_buffer_mask = _mesa_is_desktop_gl_core(ctx) ? 0 : get_user_buffer_mask(ctx);
      has_user_indices = vao->CurrentElementBufferName == 0;
   }

   /* Fast path when we don't need to upload anything. */
   if (!user_buffer_mask && !has_user_indices) {
      multi_draw_elements_async(ctx, mode, count, type, indices,
                                draw_count, basevertex, NULL, 0, NULL, NULL);
      return;
   }

   bool need_index_bounds = user_buffer_mask & ~vao->NonZeroDivisorMask;
   unsigned index_size = get_index_size(type);
   unsigned min_index = ~0;
   unsigned max_index = 0;
   unsigned total_count = 0;
   unsigned num_vertices = 0;

   /* This is always true if there is per-vertex data that needs to be
    * uploaded.
    */
   if (need_index_bounds) {
      bool synced = false;

      /* Compute the index bounds. */
      for (unsigned i = 0; i < draw_count; i++) {
         GLsizei vertex_count = count[i];

         if (vertex_count < 0) {
            /* Just call the driver to set the error. */
            multi_draw_elements_async(ctx, mode, count, type, indices, draw_count,
                                      basevertex, NULL, 0, NULL, NULL);
            return;
         }
         if (vertex_count == 0)
            continue;

         unsigned min = ~0, max = 0;
         if (has_user_indices) {
            vbo_get_minmax_index_mapped(vertex_count, index_size,
                                        ctx->GLThread._RestartIndex[index_size - 1],
                                        ctx->GLThread._PrimitiveRestart, indices[i],
                                        &min, &max);
         } else {
            if (!synced) {
               _mesa_glthread_finish_before(ctx, "MultiDrawElements - need index bounds");
               synced = true;
            }
            vbo_get_minmax_index(ctx, ctx->Array.VAO->IndexBufferObj,
                                 NULL, (intptr_t)indices[i], vertex_count,
                                 index_size, ctx->GLThread._PrimitiveRestart,
                                 ctx->GLThread._RestartIndex[index_size - 1],
                                 &min, &max);
         }

         if (basevertex) {
            min += basevertex[i];
            max += basevertex[i];
         }
         min_index = MIN2(min_index, min);
         max_index = MAX2(max_index, max);
         total_count += vertex_count;
      }

      num_vertices = max_index + 1 - min_index;

      if (total_count == 0 || num_vertices == 0) {
         /* Nothing to do, but call the driver to set possible GL errors. */
         multi_draw_elements_async(ctx, mode, count, type, indices, draw_count,
                                   basevertex, NULL, 0, NULL, NULL);
         return;
      }
   } else if (has_user_indices) {
      /* Only compute total_count for the upload of indices. */
      for (unsigned i = 0; i < draw_count; i++) {
         GLsizei vertex_count = count[i];

         if (vertex_count < 0) {
            /* Just call the driver to set the error. */
            multi_draw_elements_async(ctx, mode, count, type, indices, draw_count,
                                      basevertex, NULL, 0, NULL, NULL);
            return;
         }
         if (vertex_count == 0)
            continue;

         total_count += vertex_count;
      }

      if (total_count == 0) {
         /* Nothing to do, but call the driver to set possible GL errors. */
         multi_draw_elements_async(ctx, mode, count, type, indices, draw_count,
                                   basevertex, NULL, 0, NULL, NULL);
         return;
      }
   }

   /* Upload vertices. */
   struct gl_buffer_object *buffers[VERT_ATTRIB_MAX];
   int offsets[VERT_ATTRIB_MAX];

   if (user_buffer_mask) {
      if (!upload_vertices(ctx, user_buffer_mask, min_index, num_vertices,
                           0, 1, buffers, offsets))
         return; /* the error is set by upload_vertices */
   }

   /* Upload indices. */
   struct gl_buffer_object *index_buffer = NULL;
   if (has_user_indices) {
      const GLvoid **out_indices = alloca(sizeof(indices[0]) * draw_count);

      index_buffer = upload_multi_indices(ctx, total_count, index_size,
                                          draw_count, count, indices,
                                          out_indices);
      if (!index_buffer)
         return; /* the error is set by upload_multi_indices */

      indices = out_indices;
   }

   /* Draw asynchronously. */
   multi_draw_elements_async(ctx, mode, count, type, indices, draw_count,
                             basevertex, index_buffer, user_buffer_mask,
                             buffers, offsets);
}

void GLAPIENTRY
_mesa_marshal_MultiModeDrawArraysIBM(const GLenum *mode, const GLint *first,
                                     const GLsizei *count, GLsizei primcount,
                                     GLint modestride)
{
   for (int i = 0 ; i < primcount; i++) {
      if (count[i] > 0) {
         GLenum m = *((GLenum *)((GLubyte *)mode + i * modestride));
         _mesa_marshal_DrawArrays(m, first[i], count[i]);
      }
   }
}

void GLAPIENTRY
_mesa_marshal_MultiModeDrawElementsIBM(const GLenum *mode,
                                       const GLsizei *count, GLenum type,
                                       const GLvoid * const *indices,
                                       GLsizei primcount, GLint modestride)
{
   for (int i = 0 ; i < primcount; i++) {
      if (count[i] > 0) {
         GLenum m = *((GLenum *)((GLubyte *)mode + i * modestride));
         _mesa_marshal_DrawElements(m, count[i], type, indices[i]);
      }
   }
}

static const void *
map_draw_indirect_params(struct gl_context *ctx, GLintptr offset,
                         unsigned count, unsigned stride)
{
   struct gl_buffer_object *obj = ctx->DrawIndirectBuffer;

   if (!obj)
      return (void*)offset;

   return _mesa_bufferobj_map_range(ctx, offset,
                                    MIN2((size_t)count * stride, obj->Size),
                                    GL_MAP_READ_BIT, obj, MAP_INTERNAL);
}

static void
unmap_draw_indirect_params(struct gl_context *ctx)
{
   if (ctx->DrawIndirectBuffer)
      _mesa_bufferobj_unmap(ctx, ctx->DrawIndirectBuffer, MAP_INTERNAL);
}

static unsigned
read_draw_indirect_count(struct gl_context *ctx, GLintptr offset)
{
   unsigned result = 0;

   if (ctx->ParameterBuffer) {
      _mesa_bufferobj_get_subdata(ctx, offset, sizeof(result), &result,
                                  ctx->ParameterBuffer);
   }
   return result;
}

static void
lower_draw_arrays_indirect(struct gl_context *ctx, GLenum mode,
                           GLintptr indirect, GLsizei stride,
                           unsigned draw_count)
{
   /* If <stride> is zero, the elements are tightly packed. */
   if (stride == 0)
      stride = 4 * sizeof(GLuint);      /* sizeof(DrawArraysIndirectCommand) */

   const uint32_t *params =
      map_draw_indirect_params(ctx, indirect, draw_count, stride);

   for (unsigned i = 0; i < draw_count; i++) {
      draw_arrays(i, mode,
                  params[i * stride / 4 + 2],
                  params[i * stride / 4 + 0],
                  params[i * stride / 4 + 1],
                  params[i * stride / 4 + 3], false, false);
   }

   unmap_draw_indirect_params(ctx);
}

static void
lower_draw_elements_indirect(struct gl_context *ctx, GLenum mode, GLenum type,
                             GLintptr indirect, GLsizei stride,
                             unsigned draw_count)
{
   /* If <stride> is zero, the elements are tightly packed. */
   if (stride == 0)
      stride = 5 * sizeof(GLuint);      /* sizeof(DrawArraysIndirectCommand) */

   const uint32_t *params =
      map_draw_indirect_params(ctx, indirect, draw_count, stride);

   for (unsigned i = 0; i < draw_count; i++) {
      draw_elements(i, mode,
                    params[i * stride / 4 + 0],
                    type,
                    (GLvoid*)((uintptr_t)params[i * stride / 4 + 2] *
                              get_index_size(type)),
                    params[i * stride / 4 + 1],
                    params[i * stride / 4 + 3],
                    params[i * stride / 4 + 4],
                    false, 0, 0, false, false);
   }
   unmap_draw_indirect_params(ctx);
}

static inline bool
draw_indirect_async_allowed(struct gl_context *ctx, unsigned user_buffer_mask)
{
   return ctx->API != API_OPENGL_COMPAT ||
          /* This will just generate GL_INVALID_OPERATION, as it should. */
          ctx->GLThread.inside_begin_end ||
          ctx->GLThread.ListMode ||
          ctx->Dispatch.Current == ctx->Dispatch.ContextLost ||
          /* If the DrawIndirect buffer is bound, it behaves like profile != compat
           * if there are no user VBOs. */
          (ctx->GLThread.CurrentDrawIndirectBufferName && !user_buffer_mask);
}

uint32_t
_mesa_unmarshal_DrawArraysIndirect(struct gl_context *ctx,
                                   const struct marshal_cmd_DrawArraysIndirect *cmd)
{
   GLenum mode = cmd->mode;
   const GLvoid * indirect = cmd->indirect;

   CALL_DrawArraysIndirect(ctx->Dispatch.Current, (mode, indirect));

   return align(sizeof(struct marshal_cmd_DrawArraysIndirect), 8) / 8;
}

void GLAPIENTRY
_mesa_marshal_DrawArraysIndirect(GLenum mode, const GLvoid *indirect)
{
   GET_CURRENT_CONTEXT(ctx);
   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;
   unsigned user_buffer_mask =
      _mesa_is_gles31(ctx) ? 0 : vao->UserPointerMask & vao->BufferEnabled;

   if (draw_indirect_async_allowed(ctx, user_buffer_mask)) {
      int cmd_size = sizeof(struct marshal_cmd_DrawArraysIndirect);
      struct marshal_cmd_DrawArraysIndirect *cmd;

      cmd = _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawArraysIndirect, cmd_size);
      cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
      cmd->indirect = indirect;
      return;
   }

   _mesa_glthread_finish_before(ctx, "DrawArraysIndirect");
   lower_draw_arrays_indirect(ctx, mode, (GLintptr)indirect, 0, 1);
}

uint32_t
_mesa_unmarshal_DrawElementsIndirect(struct gl_context *ctx,
                                     const struct marshal_cmd_DrawElementsIndirect *cmd)
{
   GLenum mode = cmd->mode;
   const GLenum type = _mesa_decode_index_type(cmd->type);
   const GLvoid * indirect = cmd->indirect;

   CALL_DrawElementsIndirect(ctx->Dispatch.Current, (mode, type, indirect));
   return align(sizeof(struct marshal_cmd_DrawElementsIndirect), 8) / 8;
}

void GLAPIENTRY
_mesa_marshal_DrawElementsIndirect(GLenum mode, GLenum type, const GLvoid *indirect)
{
   GET_CURRENT_CONTEXT(ctx);
   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;
   unsigned user_buffer_mask =
      _mesa_is_gles31(ctx) ? 0 : vao->UserPointerMask & vao->BufferEnabled;

   if (draw_indirect_async_allowed(ctx, user_buffer_mask) ||
       !_mesa_is_index_type_valid(type)) {
      int cmd_size = sizeof(struct marshal_cmd_DrawElementsIndirect);
      struct marshal_cmd_DrawElementsIndirect *cmd;

      cmd = _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_DrawElementsIndirect, cmd_size);
      cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
      cmd->type = encode_index_type(type);
      cmd->indirect = indirect;
      return;
   }

   _mesa_glthread_finish_before(ctx, "DrawElementsIndirect");
   lower_draw_elements_indirect(ctx, mode, type, (GLintptr)indirect, 0, 1);
}

uint32_t
_mesa_unmarshal_MultiDrawArraysIndirect(struct gl_context *ctx,
                                        const struct marshal_cmd_MultiDrawArraysIndirect *cmd)
{
   GLenum mode = cmd->mode;
   const GLvoid * indirect = cmd->indirect;
   GLsizei primcount = cmd->primcount;
   GLsizei stride = cmd->stride;

   CALL_MultiDrawArraysIndirect(ctx->Dispatch.Current,
                                (mode, indirect, primcount, stride));
   return align(sizeof(struct marshal_cmd_MultiDrawArraysIndirect), 8) / 8;
}

void GLAPIENTRY
_mesa_marshal_MultiDrawArraysIndirect(GLenum mode, const GLvoid *indirect,
                                      GLsizei primcount, GLsizei stride)
{
   GET_CURRENT_CONTEXT(ctx);
   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;
   unsigned user_buffer_mask =
      _mesa_is_gles31(ctx) ? 0 : vao->UserPointerMask & vao->BufferEnabled;

   if (draw_indirect_async_allowed(ctx, user_buffer_mask) ||
       primcount <= 0) {
      int cmd_size = sizeof(struct marshal_cmd_MultiDrawArraysIndirect);
      struct marshal_cmd_MultiDrawArraysIndirect *cmd;

      cmd = _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_MultiDrawArraysIndirect,
                                            cmd_size);
      cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
      cmd->indirect = indirect;
      cmd->primcount = primcount;
      cmd->stride = stride;
      return;
   }

   /* Lower the draw to direct due to non-VBO vertex arrays. */
   _mesa_glthread_finish_before(ctx, "MultiDrawArraysIndirect");
   lower_draw_arrays_indirect(ctx, mode, (GLintptr)indirect, stride, primcount);
}

uint32_t
_mesa_unmarshal_MultiDrawElementsIndirect(struct gl_context *ctx,
                                          const struct marshal_cmd_MultiDrawElementsIndirect *cmd)
{
   GLenum mode = cmd->mode;
   const GLenum type = _mesa_decode_index_type(cmd->type);
   const GLvoid * indirect = cmd->indirect;
   GLsizei primcount = cmd->primcount;
   GLsizei stride = cmd->stride;

   CALL_MultiDrawElementsIndirect(ctx->Dispatch.Current,
                                  (mode, type, indirect, primcount, stride));
   return align(sizeof(struct marshal_cmd_MultiDrawElementsIndirect), 8) / 8;
}

void GLAPIENTRY
_mesa_marshal_MultiDrawElementsIndirect(GLenum mode, GLenum type,
                                        const GLvoid *indirect,
                                        GLsizei primcount, GLsizei stride)
{
   GET_CURRENT_CONTEXT(ctx);
   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;
   unsigned user_buffer_mask =
      _mesa_is_gles31(ctx) ? 0 : vao->UserPointerMask & vao->BufferEnabled;

   if (draw_indirect_async_allowed(ctx, user_buffer_mask) ||
       primcount <= 0 ||
       !_mesa_is_index_type_valid(type)) {
      int cmd_size = sizeof(struct marshal_cmd_MultiDrawElementsIndirect);
      struct marshal_cmd_MultiDrawElementsIndirect *cmd;

      cmd = _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_MultiDrawElementsIndirect,
                                            cmd_size);
      cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
      cmd->type = encode_index_type(type);
      cmd->indirect = indirect;
      cmd->primcount = primcount;
      cmd->stride = stride;
      return;
   }

   /* Lower the draw to direct due to non-VBO vertex arrays. */
   _mesa_glthread_finish_before(ctx, "MultiDrawElementsIndirect");
   lower_draw_elements_indirect(ctx, mode, type, (GLintptr)indirect, stride,
                                primcount);
}

uint32_t
_mesa_unmarshal_MultiDrawArraysIndirectCountARB(struct gl_context *ctx,
                                                const struct marshal_cmd_MultiDrawArraysIndirectCountARB *cmd)
{
   GLenum mode = cmd->mode;
   GLintptr indirect = cmd->indirect;
   GLintptr drawcount = cmd->drawcount;
   GLsizei maxdrawcount = cmd->maxdrawcount;
   GLsizei stride = cmd->stride;

   CALL_MultiDrawArraysIndirectCountARB(ctx->Dispatch.Current,
                                        (mode, indirect, drawcount,
                                         maxdrawcount, stride));
   return align(sizeof(struct marshal_cmd_MultiDrawArraysIndirectCountARB), 8) / 8;
}

void GLAPIENTRY
_mesa_marshal_MultiDrawArraysIndirectCountARB(GLenum mode, GLintptr indirect,
                                              GLintptr drawcount,
                                              GLsizei maxdrawcount,
                                              GLsizei stride)
{
   GET_CURRENT_CONTEXT(ctx);
   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;
   unsigned user_buffer_mask =
      _mesa_is_gles31(ctx) ? 0 : vao->UserPointerMask & vao->BufferEnabled;

   if (draw_indirect_async_allowed(ctx, user_buffer_mask) ||
       /* This will just generate GL_INVALID_OPERATION because Draw*IndirectCount
        * functions forbid a user indirect buffer in the Compat profile. */
       !ctx->GLThread.CurrentDrawIndirectBufferName) {
      int cmd_size =
         sizeof(struct marshal_cmd_MultiDrawArraysIndirectCountARB);
      struct marshal_cmd_MultiDrawArraysIndirectCountARB *cmd =
         _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_MultiDrawArraysIndirectCountARB,
                                         cmd_size);

      cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
      cmd->indirect = indirect;
      cmd->drawcount = drawcount;
      cmd->maxdrawcount = maxdrawcount;
      cmd->stride = stride;
      return;
   }

   /* Lower the draw to direct due to non-VBO vertex arrays. */
   _mesa_glthread_finish_before(ctx, "MultiDrawArraysIndirectCountARB");
   lower_draw_arrays_indirect(ctx, mode, indirect, stride,
                              read_draw_indirect_count(ctx, drawcount));
}

uint32_t
_mesa_unmarshal_MultiDrawElementsIndirectCountARB(struct gl_context *ctx,
                                                  const struct marshal_cmd_MultiDrawElementsIndirectCountARB *cmd)
{
   GLenum mode = cmd->mode;
   const GLenum type = _mesa_decode_index_type(cmd->type);
   GLintptr indirect = cmd->indirect;
   GLintptr drawcount = cmd->drawcount;
   GLsizei maxdrawcount = cmd->maxdrawcount;
   GLsizei stride = cmd->stride;

   CALL_MultiDrawElementsIndirectCountARB(ctx->Dispatch.Current, (mode, type, indirect, drawcount, maxdrawcount, stride));

   return align(sizeof(struct marshal_cmd_MultiDrawElementsIndirectCountARB), 8) / 8;
}

void GLAPIENTRY
_mesa_marshal_MultiDrawElementsIndirectCountARB(GLenum mode, GLenum type,
                                                GLintptr indirect,
                                                GLintptr drawcount,
                                                GLsizei maxdrawcount,
                                                GLsizei stride)
{
   GET_CURRENT_CONTEXT(ctx);
   struct glthread_vao *vao = ctx->GLThread.CurrentVAO;
   unsigned user_buffer_mask =
      _mesa_is_gles31(ctx) ? 0 : vao->UserPointerMask & vao->BufferEnabled;

   if (draw_indirect_async_allowed(ctx, user_buffer_mask) ||
       /* This will just generate GL_INVALID_OPERATION because Draw*IndirectCount
        * functions forbid a user indirect buffer in the Compat profile. */
       !ctx->GLThread.CurrentDrawIndirectBufferName ||
       !_mesa_is_index_type_valid(type)) {
      int cmd_size = sizeof(struct marshal_cmd_MultiDrawElementsIndirectCountARB);
      struct marshal_cmd_MultiDrawElementsIndirectCountARB *cmd =
         _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_MultiDrawElementsIndirectCountARB, cmd_size);

      cmd->mode = MIN2(mode, 0xff); /* clamped to 0xff (invalid enum) */
      cmd->type = encode_index_type(type);
      cmd->indirect = indirect;
      cmd->drawcount = drawcount;
      cmd->maxdrawcount = maxdrawcount;
      cmd->stride = stride;
      return;
   }

   /* Lower the draw to direct due to non-VBO vertex arrays. */
   _mesa_glthread_finish_before(ctx, "MultiDrawElementsIndirectCountARB");
   lower_draw_elements_indirect(ctx, mode, type, indirect, stride,
                                read_draw_indirect_count(ctx, drawcount));
}

void GLAPIENTRY
_mesa_marshal_DrawArrays(GLenum mode, GLint first, GLsizei count)
{
   draw_arrays(0, mode, first, count, 1, 0, true, false);
}

void GLAPIENTRY
_mesa_marshal_DrawArrays_no_error(GLenum mode, GLint first, GLsizei count)
{
   draw_arrays(0, mode, first, count, 1, 0, true, true);
}

void GLAPIENTRY
_mesa_marshal_DrawArraysInstanced(GLenum mode, GLint first, GLsizei count,
                                  GLsizei instance_count)
{
   draw_arrays(0, mode, first, count, instance_count, 0, false, false);
}

void GLAPIENTRY
_mesa_marshal_DrawArraysInstanced_no_error(GLenum mode, GLint first, GLsizei count,
                                           GLsizei instance_count)
{
   draw_arrays(0, mode, first, count, instance_count, 0, false, true);
}

void GLAPIENTRY
_mesa_marshal_DrawArraysInstancedBaseInstance(GLenum mode, GLint first,
                                              GLsizei count, GLsizei instance_count,
                                              GLuint baseinstance)
{
   draw_arrays(0, mode, first, count, instance_count, baseinstance, false, false);
}

void GLAPIENTRY
_mesa_marshal_DrawArraysInstancedBaseInstance_no_error(GLenum mode, GLint first,
                                                       GLsizei count, GLsizei instance_count,
                                                       GLuint baseinstance)
{
   draw_arrays(0, mode, first, count, instance_count, baseinstance, false, true);
}

void GLAPIENTRY
_mesa_marshal_DrawElements(GLenum mode, GLsizei count, GLenum type,
                           const GLvoid *indices)
{
   draw_elements(0, mode, count, type, indices, 1, 0,
                 0, false, 0, 0, true, false);
}

void GLAPIENTRY
_mesa_marshal_DrawElements_no_error(GLenum mode, GLsizei count, GLenum type,
                                    const GLvoid *indices)
{
   draw_elements(0, mode, count, type, indices, 1, 0,
                 0, false, 0, 0, true, true);
}

void GLAPIENTRY
_mesa_marshal_DrawRangeElements(GLenum mode, GLuint start, GLuint end,
                                GLsizei count, GLenum type,
                                const GLvoid *indices)
{
   draw_elements(0, mode, count, type, indices, 1, 0,
                 0, true, start, end, true, false);
}

void GLAPIENTRY
_mesa_marshal_DrawRangeElements_no_error(GLenum mode, GLuint start, GLuint end,
                                         GLsizei count, GLenum type,
                                         const GLvoid *indices)
{
   draw_elements(0, mode, count, type, indices, 1, 0,
                 0, true, start, end, true, true);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsInstanced(GLenum mode, GLsizei count, GLenum type,
                                    const GLvoid *indices, GLsizei instance_count)
{
   draw_elements(0, mode, count, type, indices, instance_count, 0,
                 0, false, 0, 0, false, false);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsInstanced_no_error(GLenum mode, GLsizei count,
                                             GLenum type, const GLvoid *indices,
                                             GLsizei instance_count)
{
   draw_elements(0, mode, count, type, indices, instance_count, 0,
                 0, false, 0, 0, false, true);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsBaseVertex(GLenum mode, GLsizei count, GLenum type,
                                     const GLvoid *indices, GLint basevertex)
{
   draw_elements(0, mode, count, type, indices, 1, basevertex,
                 0, false, 0, 0, true, false);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsBaseVertex_no_error(GLenum mode, GLsizei count,
                                              GLenum type, const GLvoid *indices,
                                              GLint basevertex)
{
   draw_elements(0, mode, count, type, indices, 1, basevertex,
                 0, false, 0, 0, true, true);
}

void GLAPIENTRY
_mesa_marshal_DrawRangeElementsBaseVertex(GLenum mode, GLuint start, GLuint end,
                                          GLsizei count, GLenum type,
                                          const GLvoid *indices, GLint basevertex)
{
   draw_elements(0, mode, count, type, indices, 1, basevertex,
                 0, true, start, end, true, false);
}

void GLAPIENTRY
_mesa_marshal_DrawRangeElementsBaseVertex_no_error(GLenum mode, GLuint start,
                                                   GLuint end, GLsizei count, GLenum type,
                                                   const GLvoid *indices, GLint basevertex)
{
   draw_elements(0, mode, count, type, indices, 1, basevertex,
                 0, true, start, end, true, true);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsInstancedBaseVertex(GLenum mode, GLsizei count,
                                              GLenum type, const GLvoid *indices,
                                              GLsizei instance_count, GLint basevertex)
{
   draw_elements(0, mode, count, type, indices, instance_count, basevertex,
                 0, false, 0, 0, false, false);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsInstancedBaseVertex_no_error(GLenum mode, GLsizei count,
                                                       GLenum type, const GLvoid *indices,
                                                       GLsizei instance_count, GLint basevertex)
{
   draw_elements(0, mode, count, type, indices, instance_count, basevertex,
                 0, false, 0, 0, false, true);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsInstancedBaseInstance(GLenum mode, GLsizei count,
                                                GLenum type, const GLvoid *indices,
                                                GLsizei instance_count, GLuint baseinstance)
{
   draw_elements(0, mode, count, type, indices, instance_count, 0,
                 baseinstance, false, 0, 0, false, false);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsInstancedBaseInstance_no_error(GLenum mode, GLsizei count,
                                                         GLenum type, const GLvoid *indices,
                                                         GLsizei instance_count, GLuint baseinstance)
{
   draw_elements(0, mode, count, type, indices, instance_count, 0,
                 baseinstance, false, 0, 0, false, true);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsInstancedBaseVertexBaseInstance(GLenum mode, GLsizei count,
                                                          GLenum type, const GLvoid *indices,
                                                          GLsizei instance_count, GLint basevertex,
                                                          GLuint baseinstance)
{
   draw_elements(0, mode, count, type, indices, instance_count, basevertex,
                 baseinstance, false, 0, 0, false, false);
}

void GLAPIENTRY
_mesa_marshal_DrawElementsInstancedBaseVertexBaseInstance_no_error(GLenum mode, GLsizei count,
                                                                   GLenum type, const GLvoid *indices,
                                                                   GLsizei instance_count,
                                                                   GLint basevertex, GLuint baseinstance)
{
   draw_elements(0, mode, count, type, indices, instance_count, basevertex,
                 baseinstance, false, 0, 0, false, true);
}

void GLAPIENTRY
_mesa_marshal_MultiDrawElements(GLenum mode, const GLsizei *count,
                                GLenum type, const GLvoid *const *indices,
                                GLsizei draw_count)
{
   _mesa_marshal_MultiDrawElementsBaseVertex(mode, count, type, indices,
                                             draw_count, NULL);
}

uint32_t
_mesa_unmarshal_DrawArrays(struct gl_context *ctx,
                           const struct marshal_cmd_DrawArrays *restrict cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_DrawArraysInstancedBaseInstance(struct gl_context *ctx,
                                                const struct marshal_cmd_DrawArraysInstancedBaseInstance *restrict cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_MultiDrawArrays(struct gl_context *ctx,
                                const struct marshal_cmd_MultiDrawArrays *restrict cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_DrawRangeElements(struct gl_context *ctx,
                                  const struct marshal_cmd_DrawRangeElements *restrict cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_DrawRangeElementsBaseVertex(struct gl_context *ctx,
                                            const struct marshal_cmd_DrawRangeElementsBaseVertex *cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_DrawElementsInstanced(struct gl_context *ctx,
                                      const struct marshal_cmd_DrawElementsInstanced *restrict cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_DrawElementsBaseVertex(struct gl_context *ctx,
                                       const struct marshal_cmd_DrawElementsBaseVertex *restrict cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_DrawElementsInstancedBaseVertexBaseInstance(struct gl_context *ctx,
                                                            const struct marshal_cmd_DrawElementsInstancedBaseVertexBaseInstance *restrict cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_MultiDrawElements(struct gl_context *ctx,
                                  const struct marshal_cmd_MultiDrawElements *restrict cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_MultiDrawElementsBaseVertex(struct gl_context *ctx,
                                            const struct marshal_cmd_MultiDrawElementsBaseVertex *restrict cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_MultiModeDrawArraysIBM(struct gl_context *ctx,
                                       const struct marshal_cmd_MultiModeDrawArraysIBM *cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

uint32_t
_mesa_unmarshal_MultiModeDrawElementsIBM(struct gl_context *ctx,
                                         const struct marshal_cmd_MultiModeDrawElementsIBM *cmd)
{
   UNREACHABLE("should never end up here");
   return 0;
}

void GLAPIENTRY
_mesa_marshal_DrawArraysUserBuf(void)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_marshal_DrawElementsUserBuf(const GLvoid *cmd)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_marshal_DrawElementsUserBufPacked(const GLvoid *cmd)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_marshal_MultiDrawArraysUserBuf(void)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_marshal_MultiDrawElementsUserBuf(GLintptr indexBuf, GLenum mode,
                                       const GLsizei *count, GLenum type,
                                       const GLvoid * const *indices,
                                       GLsizei primcount,
                                       const GLint *basevertex)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_marshal_DrawArraysInstancedBaseInstanceDrawID(void)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY _mesa_marshal_DrawElementsPacked(GLenum mode, GLenum type,
                                                 GLushort count, GLushort indices)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_marshal_DrawElementsInstancedBaseVertexBaseInstanceDrawID(GLenum mode, GLsizei count,
                                                                GLenum type, const GLvoid *indices,
                                                                GLsizei instance_count, GLint basevertex,
                                                                GLuint baseinstance, GLuint drawid)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_DrawArraysUserBuf(void)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_MultiDrawArraysUserBuf(void)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_DrawArraysInstancedBaseInstanceDrawID(void)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY _mesa_DrawElementsPacked(GLenum mode, GLenum type,
                                         GLushort count, GLushort indices)
{
   UNREACHABLE("should never end up here");
}

void GLAPIENTRY
_mesa_DrawElementsInstancedBaseVertexBaseInstanceDrawID(GLenum mode, GLsizei count,
                                                        GLenum type, const GLvoid *indices,
                                                        GLsizei instance_count, GLint basevertex,
                                                        GLuint baseinstance, GLuint drawid)
{
   UNREACHABLE("should never end up here");
}

uint32_t
_mesa_unmarshal_PushMatrix(struct gl_context *ctx,
                           const struct marshal_cmd_PushMatrix *restrict cmd)
{
   const unsigned push_matrix_size = 1;
   const unsigned mult_matrixf_size = 9;
   const unsigned draw_elements_size =
      (align(sizeof(struct marshal_cmd_DrawElements), 8) / 8);
   const unsigned draw_elements_packed_size =
      (align(sizeof(struct marshal_cmd_DrawElementsPacked), 8) / 8);
   const unsigned pop_matrix_size = 1;
   uint64_t *next1 = _mesa_glthread_next_cmd((uint64_t *)cmd, push_matrix_size);
   uint64_t *next2;

   /* Viewperf has these call patterns. */
   switch (_mesa_glthread_get_cmd(next1)->cmd_id) {
   case DISPATCH_CMD_DrawElements:
      /* Execute this sequence:
       *    glPushMatrix
       *    (glMultMatrixf with identity is eliminated by the marshal function)
       *    glDrawElements
       *    glPopMatrix
       * as:
       *    glDrawElements
       */
      next2 = _mesa_glthread_next_cmd(next1, draw_elements_size);

      if (_mesa_glthread_get_cmd(next2)->cmd_id == DISPATCH_CMD_PopMatrix) {
         /* The beauty of this is that this is inlined. */
         _mesa_unmarshal_DrawElements(ctx, (void*)next1);
         return push_matrix_size + draw_elements_size + pop_matrix_size;
      }
      break;

   case DISPATCH_CMD_DrawElementsPacked:
      next2 = _mesa_glthread_next_cmd(next1, draw_elements_packed_size);

      if (_mesa_glthread_get_cmd(next2)->cmd_id == DISPATCH_CMD_PopMatrix) {
         /* The beauty of this is that this is inlined. */
         _mesa_unmarshal_DrawElementsPacked(ctx, (void*)next1);
         return push_matrix_size + draw_elements_packed_size + pop_matrix_size;
      }
      break;

   case DISPATCH_CMD_MultMatrixf:
      /* Skip this sequence:
       *    glPushMatrix
       *    glMultMatrixf
       *    glPopMatrix
       */
      next2 = _mesa_glthread_next_cmd(next1, mult_matrixf_size);

      if (_mesa_glthread_get_cmd(next2)->cmd_id == DISPATCH_CMD_PopMatrix)
         return push_matrix_size + mult_matrixf_size + pop_matrix_size;
      break;
   }

   CALL_PushMatrix(ctx->Dispatch.Current, ());
   return push_matrix_size;
}

void GLAPIENTRY
_mesa_marshal_PushMatrix(void)
{
   GET_CURRENT_CONTEXT(ctx);

   _mesa_glthread_allocate_command(ctx, DISPATCH_CMD_PushMatrix,
                                   sizeof(struct marshal_cmd_PushMatrix));
   _mesa_glthread_PushMatrix(ctx);
}

/**************************************************************************
 * 
 * Copyright 2007 VMware, Inc.
 * Copyright 2010 VMware, Inc.
 * All Rights Reserved.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sub license, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS AND/OR ITS SUPPLIERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 **************************************************************************/

/**
 * \brief  Quad depth / stencil testing
 */

#include "pipe/p_defines.h"
#include "util/format/u_format.h"
#include "util/u_math.h"
#include "util/u_memory.h"
#include "tgsi/tgsi_scan.h"
#include "sp_context.h"
#include "sp_quad.h"
#include "sp_quad_pipe.h"
#include "sp_tile_cache.h"
#include "sp_state.h"           /* for sp_fragment_shader */


struct depth_data {
   enum pipe_format format;
   unsigned bzzzz[TGSI_QUAD_SIZE];  /**< Z values fetched from depth buffer */
   unsigned qzzzz[TGSI_QUAD_SIZE];  /**< Z values from the quad */
   uint8_t stencilVals[TGSI_QUAD_SIZE];
   bool use_shader_stencil_refs;
   uint8_t shader_stencil_refs[TGSI_QUAD_SIZE];
   struct softpipe_cached_tile *tile;
   float minval, maxval;
   bool clamp;
};



static void
get_depth_stencil_values( struct depth_data *data,
                          const struct quad_header *quad )
{
   unsigned j;
   const struct softpipe_cached_tile *tile = data->tile;

   switch (data->format) {
   case PIPE_FORMAT_Z16_UNORM:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         data->bzzzz[j] = tile->data.depth16[y][x];
      }
      break;
   case PIPE_FORMAT_Z32_UNORM:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         data->bzzzz[j] = tile->data.depth32[y][x];
      }
      break;
   case PIPE_FORMAT_Z24X8_UNORM:
   case PIPE_FORMAT_Z24_UNORM_S8_UINT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         data->bzzzz[j] = tile->data.depth32[y][x] & 0xffffff;
         data->stencilVals[j] = tile->data.depth32[y][x] >> 24;
      }
      break;
   case PIPE_FORMAT_X8Z24_UNORM:
   case PIPE_FORMAT_S8_UINT_Z24_UNORM:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         data->bzzzz[j] = tile->data.depth32[y][x] >> 8;
         data->stencilVals[j] = tile->data.depth32[y][x] & 0xff;
      }
      break;
   case PIPE_FORMAT_S8_UINT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         data->bzzzz[j] = 0;
         data->stencilVals[j] = tile->data.stencil8[y][x];
      }
      break;
   case PIPE_FORMAT_Z32_FLOAT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         data->bzzzz[j] = tile->data.depth32[y][x];
      }
      break;
   case PIPE_FORMAT_Z32_FLOAT_S8X24_UINT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         data->bzzzz[j] = tile->data.depth64[y][x] & 0xffffffff;
         data->stencilVals[j] = (tile->data.depth64[y][x] >> 32) & 0xff;
      }
      break;
   default:
      assert(0);
   }
}


/**
 * If the shader has not been run, interpolate the depth values
 * ourselves.
 */
static void
interpolate_quad_depth( struct quad_header *quad )
{
   const float fx = (float) quad->input.x0;
   const float fy = (float) quad->input.y0;
   const float dzdx = quad->posCoef->dadx[2];
   const float dzdy = quad->posCoef->dady[2];
   const float z0 = quad->posCoef->a0[2] + dzdx * fx + dzdy * fy;

   quad->output.depth[0] = z0;
   quad->output.depth[1] = z0 + dzdx;
   quad->output.depth[2] = z0 + dzdy;
   quad->output.depth[3] = z0 + dzdx + dzdy;
}


/**
 * Compute the depth_data::qzzzz[] values from the float fragment Z values.
 */
static void
convert_quad_depth( struct depth_data *data, 
                    const struct quad_header *quad )
{
   unsigned j;
   float dvals[TGSI_QUAD_SIZE];

   /* Convert quad's float depth values to int depth values (qzzzz).
    * If the Z buffer stores integer values, we _have_ to do the depth
    * compares with integers (not floats).  Otherwise, the float->int->float
    * conversion of Z values (which isn't an identity function) will cause
    * Z-fighting errors.
    */
   if (data->clamp) {
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         dvals[j] = CLAMP(quad->output.depth[j], data->minval, data->maxval);
      }
   } else {
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         dvals[j] = quad->output.depth[j];
      }
   }

   switch (data->format) {
   case PIPE_FORMAT_Z16_UNORM:
      {
         float scale = 65535.0;

         for (j = 0; j < TGSI_QUAD_SIZE; j++) {
            data->qzzzz[j] = (unsigned) (dvals[j] * scale);
         }
      }
      break;
   case PIPE_FORMAT_Z32_UNORM:
      {
         double scale = (double) (uint) ~0UL;

         for (j = 0; j < TGSI_QUAD_SIZE; j++) {
            data->qzzzz[j] = (unsigned) (dvals[j] * scale);
         }
      }
      break;
   case PIPE_FORMAT_Z24X8_UNORM:
   case PIPE_FORMAT_Z24_UNORM_S8_UINT:
      {
         float scale = (float) ((1 << 24) - 1);

         for (j = 0; j < TGSI_QUAD_SIZE; j++) {
            data->qzzzz[j] = (unsigned) (dvals[j] * scale);
         }
      }
      break;
   case PIPE_FORMAT_X8Z24_UNORM:
   case PIPE_FORMAT_S8_UINT_Z24_UNORM:
      {
         float scale = (float) ((1 << 24) - 1);

         for (j = 0; j < TGSI_QUAD_SIZE; j++) {
            data->qzzzz[j] = (unsigned) (dvals[j] * scale);
         }
      }
      break;
   case PIPE_FORMAT_Z32_FLOAT:
   case PIPE_FORMAT_Z32_FLOAT_S8X24_UINT:
      {
         union fi fui;

         for (j = 0; j < TGSI_QUAD_SIZE; j++) {
            fui.f = dvals[j];
            data->qzzzz[j] = fui.ui;
         }
      }
      break;
   default:
      assert(0);
   }
}


/**
 * Compute the depth_data::shader_stencil_refs[] values from the float
 * fragment stencil values.
 */
static void
convert_quad_stencil( struct depth_data *data, 
                      const struct quad_header *quad )
{
   unsigned j;

   data->use_shader_stencil_refs = true;
   /* Copy quads stencil values
    */
   switch (data->format) {
   case PIPE_FORMAT_Z24X8_UNORM:
   case PIPE_FORMAT_Z24_UNORM_S8_UINT:
   case PIPE_FORMAT_X8Z24_UNORM:
   case PIPE_FORMAT_S8_UINT_Z24_UNORM:
   case PIPE_FORMAT_S8_UINT:
   case PIPE_FORMAT_Z32_FLOAT:
   case PIPE_FORMAT_Z32_FLOAT_S8X24_UINT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         data->shader_stencil_refs[j] = ((unsigned)(quad->output.stencil[j]));
      }
      break;
   default:
      assert(0);
   }
}


/**
 * Write data->bzzzz[] values and data->stencilVals into the Z/stencil buffer.
 */
static void
write_depth_stencil_values( struct depth_data *data,
                            struct quad_header *quad )
{
   struct softpipe_cached_tile *tile = data->tile;
   unsigned j;

   /* put updated Z values back into cached tile */
   switch (data->format) {
   case PIPE_FORMAT_Z16_UNORM:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         tile->data.depth16[y][x] = (uint16_t) data->bzzzz[j];
      }
      break;
   case PIPE_FORMAT_Z24X8_UNORM:
   case PIPE_FORMAT_Z32_UNORM:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         tile->data.depth32[y][x] = data->bzzzz[j];
      }
      break;
   case PIPE_FORMAT_Z24_UNORM_S8_UINT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         tile->data.depth32[y][x] = (data->stencilVals[j] << 24) | data->bzzzz[j];
      }
      break;
   case PIPE_FORMAT_S8_UINT_Z24_UNORM:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         tile->data.depth32[y][x] = (data->bzzzz[j] << 8) | data->stencilVals[j];
      }
      break;
   case PIPE_FORMAT_X8Z24_UNORM:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         tile->data.depth32[y][x] = data->bzzzz[j] << 8;
      }
      break;
   case PIPE_FORMAT_S8_UINT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         tile->data.stencil8[y][x] = data->stencilVals[j];
      }
      break;
   case PIPE_FORMAT_Z32_FLOAT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         tile->data.depth32[y][x] = data->bzzzz[j];
      }
      break;
   case PIPE_FORMAT_Z32_FLOAT_S8X24_UINT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         int x = quad->input.x0 % TILE_SIZE + (j & 1);
         int y = quad->input.y0 % TILE_SIZE + (j >> 1);
         tile->data.depth64[y][x] = (uint64_t)data->bzzzz[j] | ((uint64_t)data->stencilVals[j] << 32);
      }
      break;
   default:
      assert(0);
   }
}



/** Only 8-bit stencil supported */
#define STENCIL_MAX 0xff


/**
 * Do the basic stencil test (compare stencil buffer values against the
 * reference value.
 *
 * \param data->stencilVals  the stencil values from the stencil buffer
 * \param func  the stencil func (PIPE_FUNC_x)
 * \param ref  the stencil reference value
 * \param valMask  the stencil value mask indicating which bits of the stencil
 *                 values and ref value are to be used.
 * \return mask indicating which pixels passed the stencil test
 */
static unsigned
do_stencil_test(struct depth_data *data,
                unsigned func,
                unsigned ref, unsigned valMask)
{
   unsigned passMask = 0x0;
   unsigned j;
   uint8_t refs[TGSI_QUAD_SIZE];

   for (j = 0; j < TGSI_QUAD_SIZE; j++) {
      if (data->use_shader_stencil_refs)
         refs[j] = data->shader_stencil_refs[j] & valMask;
      else 
         refs[j] = ref & valMask;
   }

   switch (func) {
   case PIPE_FUNC_NEVER:
      /* passMask = 0x0 */
      break;
   case PIPE_FUNC_LESS:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (refs[j] < (data->stencilVals[j] & valMask)) {
            passMask |= (1 << j);
         }
      }
      break;
   case PIPE_FUNC_EQUAL:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (refs[j] == (data->stencilVals[j] & valMask)) {
            passMask |= (1 << j);
         }
      }
      break;
   case PIPE_FUNC_LEQUAL:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (refs[j] <= (data->stencilVals[j] & valMask)) {
            passMask |= (1 << j);
         }
      }
      break;
   case PIPE_FUNC_GREATER:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (refs[j] > (data->stencilVals[j] & valMask)) {
            passMask |= (1 << j);
         }
      }
      break;
   case PIPE_FUNC_NOTEQUAL:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (refs[j] != (data->stencilVals[j] & valMask)) {
            passMask |= (1 << j);
         }
      }
      break;
   case PIPE_FUNC_GEQUAL:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (refs[j] >= (data->stencilVals[j] & valMask)) {
            passMask |= (1 << j);
         }
      }
      break;
   case PIPE_FUNC_ALWAYS:
      passMask = MASK_ALL;
      break;
   default:
      assert(0);
   }

   return passMask;
}


/**
 * Apply the stencil operator to stencil values.
 *
 * \param data->stencilVals  the stencil buffer values (read and written)
 * \param mask  indicates which pixels to update
 * \param op  the stencil operator (PIPE_STENCIL_OP_x)
 * \param ref  the stencil reference value
 * \param wrtMask  writemask controlling which bits are changed in the
 *                 stencil values
 */
static void
apply_stencil_op(struct depth_data *data,
                 unsigned mask, unsigned op, uint8_t ref, uint8_t wrtMask)
{
   unsigned j;
   uint8_t newstencil[TGSI_QUAD_SIZE];
   uint8_t refs[TGSI_QUAD_SIZE];

   for (j = 0; j < TGSI_QUAD_SIZE; j++) {
      newstencil[j] = data->stencilVals[j];
      if (data->use_shader_stencil_refs)
         refs[j] = data->shader_stencil_refs[j];
      else
         refs[j] = ref;
   }

   switch (op) {
   case PIPE_STENCIL_OP_KEEP:
      /* no-op */
      break;
   case PIPE_STENCIL_OP_ZERO:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (mask & (1 << j)) {
            newstencil[j] = 0;
         }
      }
      break;
   case PIPE_STENCIL_OP_REPLACE:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (mask & (1 << j)) {
            newstencil[j] = refs[j];
         }
      }
      break;
   case PIPE_STENCIL_OP_INCR:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (mask & (1 << j)) {
            if (data->stencilVals[j] < STENCIL_MAX) {
               newstencil[j] = data->stencilVals[j] + 1;
            }
         }
      }
      break;
   case PIPE_STENCIL_OP_DECR:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (mask & (1 << j)) {
            if (data->stencilVals[j] > 0) {
               newstencil[j] = data->stencilVals[j] - 1;
            }
         }
      }
      break;
   case PIPE_STENCIL_OP_INCR_WRAP:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (mask & (1 << j)) {
            newstencil[j] = data->stencilVals[j] + 1;
         }
      }
      break;
   case PIPE_STENCIL_OP_DECR_WRAP:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (mask & (1 << j)) {
            newstencil[j] = data->stencilVals[j] - 1;
         }
      }
      break;
   case PIPE_STENCIL_OP_INVERT:
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (mask & (1 << j)) {
            newstencil[j] = ~data->stencilVals[j];
         }
      }
      break;
   default:
      assert(0);
   }

   /*
    * update the stencil values
    */
   if (wrtMask != STENCIL_MAX) {
      /* apply bit-wise stencil buffer writemask */
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         data->stencilVals[j] = (wrtMask & newstencil[j]) | (~wrtMask & data->stencilVals[j]);
      }
   }
   else {
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         data->stencilVals[j] = newstencil[j];
      }
   }
}

   

/**
 * To increase efficiency, we should probably have multiple versions
 * of this function that are specifically for Z16, Z32 and FP Z buffers.
 * Try to effectively do that with codegen...
 */
static bool
depth_test_quad(struct quad_stage *qs, 
                struct depth_data *data,
                struct quad_header *quad)
{
   struct softpipe_context *softpipe = qs->softpipe;
   unsigned zmask = 0;
   unsigned j;

#define DEPTHTEST(l, op, r) do { \
      if (data->format == PIPE_FORMAT_Z32_FLOAT_S8X24_UINT || \
          data->format == PIPE_FORMAT_Z32_FLOAT) { \
         for (j = 0; j < TGSI_QUAD_SIZE; j++) { \
            if (((float *)l)[j] op ((float *)r)[j]) \
               zmask |= (1 << j); \
         } \
      } else { \
         for (j = 0; j < TGSI_QUAD_SIZE; j++) { \
            if (l[j] op r[j]) \
               zmask |= (1 << j); \
         } \
      } \
   } while (0)

   switch (softpipe->depth_stencil->depth_func) {
   case PIPE_FUNC_NEVER:
      /* zmask = 0 */
      break;
   case PIPE_FUNC_LESS:
      /* Note this is pretty much a single sse or cell instruction.  
       * Like this:  quad->mask &= (quad->outputs.depth < zzzz);
       */
      DEPTHTEST(data->qzzzz,  <, data->bzzzz);
      break;
   case PIPE_FUNC_EQUAL:
      DEPTHTEST(data->qzzzz, ==, data->bzzzz);
      break;
   case PIPE_FUNC_LEQUAL:
      DEPTHTEST(data->qzzzz, <=, data->bzzzz);
      break;
   case PIPE_FUNC_GREATER:
      DEPTHTEST(data->qzzzz,  >, data->bzzzz);
      break;
   case PIPE_FUNC_NOTEQUAL:
      DEPTHTEST(data->qzzzz, !=, data->bzzzz);
      break;
   case PIPE_FUNC_GEQUAL:
      DEPTHTEST(data->qzzzz, >=, data->bzzzz);
      break;
   case PIPE_FUNC_ALWAYS:
      zmask = MASK_ALL;
      break;
   default:
      assert(0);
   }

   quad->inout.mask &= zmask;
   if (quad->inout.mask == 0)
      return false;

   /* Update our internal copy only if writemask set.  Even if
    * depth.writemask is FALSE, may still need to write out buffer
    * data due to stencil changes.
    */
   if (softpipe->depth_stencil->depth_writemask) {
      for (j = 0; j < TGSI_QUAD_SIZE; j++) {
         if (quad->inout.mask & (1 << j)) {
            data->bzzzz[j] = data->qzzzz[j];
         }
      }
   }

   return true;
}



/**
 * Do stencil (and depth) testing.  Stenciling depends on the outcome of
 * depth testing.
 */
static void
depth_stencil_test_quad(struct quad_stage *qs, 
                        struct depth_data *data,
                        struct quad_header *quad)
{
   struct softpipe_context *softpipe = qs->softpipe;
   unsigned func, zFailOp, zPassOp, failOp;
   uint8_t ref, wrtMask, valMask;
   uint face = quad->input.facing;

   if (!softpipe->depth_stencil->stencil[1].enabled) {
      /* single-sided stencil test, use front (face=0) state */
      face = 0;
   }

   /* 0 = front-face, 1 = back-face */
   assert(face == 0 || face == 1);

   /* choose front or back face function, operator, etc */
   /* XXX we could do these initializations once per primitive */
   func    = softpipe->depth_stencil->stencil[face].func;
   failOp  = softpipe->depth_stencil->stencil[face].fail_op;
   zFailOp = softpipe->depth_stencil->stencil[face].zfail_op;
   zPassOp = softpipe->depth_stencil->stencil[face].zpass_op;
   ref     = softpipe->stencil_ref.ref_value[face];
   wrtMask = softpipe->depth_stencil->stencil[face].writemask;
   valMask = softpipe->depth_stencil->stencil[face].valuemask;

   /* do the stencil test first */
   {
      unsigned passMask, failMask;
      passMask = do_stencil_test(data, func, ref, valMask);
      failMask = quad->inout.mask & ~passMask;
      quad->inout.mask &= passMask;

      if (failOp != PIPE_STENCIL_OP_KEEP) {
         apply_stencil_op(data, failMask, failOp, ref, wrtMask);
      }
   }

   if (quad->inout.mask) {
      /* now the pixels that passed the stencil test are depth tested */
      if (softpipe->depth_stencil->depth_enabled) {
         const unsigned origMask = quad->inout.mask;

         depth_test_quad(qs, data, quad);  /* quad->mask is updated */

         /* update stencil buffer values according to z pass/fail result */
         if (zFailOp != PIPE_STENCIL_OP_KEEP) {
            const unsigned zFailMask = origMask & ~quad->inout.mask;
            apply_stencil_op(data, zFailMask, zFailOp, ref, wrtMask);
         }

         if (zPassOp != PIPE_STENCIL_OP_KEEP) {
            const unsigned zPassMask = origMask & quad->inout.mask;
            apply_stencil_op(data, zPassMask, zPassOp, ref, wrtMask);
         }
      }
      else {
         /* no depth test, apply Zpass operator to stencil buffer values */
         apply_stencil_op(data, quad->inout.mask, zPassOp, ref, wrtMask);
      }
   }
}


#define ALPHATEST( FUNC, COMP )                                         \
   static unsigned                                                      \
   alpha_test_quads_##FUNC( struct quad_stage *qs,                      \
                           struct quad_header *quads[],                 \
                           unsigned nr )                                \
   {                                                                    \
      const float ref = qs->softpipe->depth_stencil->alpha_ref_value;   \
      const uint cbuf = 0; /* only output[0].alpha is tested */         \
      unsigned pass_nr = 0;                                             \
      unsigned i;                                                       \
                                                                        \
      for (i = 0; i < nr; i++) {                                        \
         const float *aaaa = quads[i]->output.color[cbuf][3];           \
         unsigned passMask = 0;                                         \
                                                                        \
         if (aaaa[0] COMP ref) passMask |= (1 << 0);                    \
         if (aaaa[1] COMP ref) passMask |= (1 << 1);                    \
         if (aaaa[2] COMP ref) passMask |= (1 << 2);                    \
         if (aaaa[3] COMP ref) passMask |= (1 << 3);                    \
                                                                        \
         quads[i]->inout.mask &= passMask;                              \
                                                                        \
         if (quads[i]->inout.mask)                                      \
            quads[pass_nr++] = quads[i];                                \
      }                                                                 \
                                                                        \
      return pass_nr;                                                   \
   }


ALPHATEST( LESS,     < )
ALPHATEST( EQUAL,    == )
ALPHATEST( LEQUAL,   <= )
ALPHATEST( GREATER,  > )
ALPHATEST( NOTEQUAL, != )
ALPHATEST( GEQUAL,   >= )


/* XXX: Incorporate into shader using KILL_IF.
 */
static unsigned
alpha_test_quads(struct quad_stage *qs, 
                 struct quad_header *quads[], 
                 unsigned nr)
{
   switch (qs->softpipe->depth_stencil->alpha_func) {
   case PIPE_FUNC_LESS:
      return alpha_test_quads_LESS( qs, quads, nr );
   case PIPE_FUNC_EQUAL:
      return alpha_test_quads_EQUAL( qs, quads, nr );
   case PIPE_FUNC_LEQUAL:
      return alpha_test_quads_LEQUAL( qs, quads, nr );
   case PIPE_FUNC_GREATER:
      return alpha_test_quads_GREATER( qs, quads, nr );
   case PIPE_FUNC_NOTEQUAL:
      return alpha_test_quads_NOTEQUAL( qs, quads, nr );
   case PIPE_FUNC_GEQUAL:
      return alpha_test_quads_GEQUAL( qs, quads, nr );
   case PIPE_FUNC_ALWAYS:
      return nr;
   case PIPE_FUNC_NEVER:
   default:
      return 0;
   }
}


/**
 * EXT_depth_bounds_test has some careful language about precision:
 *
 *     At what precision is the depth bounds test carried out?
 *
 *       RESOLUTION:  For the purposes of the test, the bounds are converted
 *       to fixed-point as though they were to be written to the depth buffer,
 *       and the comparison uses those quantized bounds.
 *
 * We choose the obvious interpretation that Z32F needs no such conversion.
 */
static unsigned
depth_bounds_test_quads(struct quad_stage *qs,
                        struct quad_header *quads[],
                        unsigned nr,
                        struct depth_data *data)
{
   struct pipe_depth_stencil_alpha_state *dsa = qs->softpipe->depth_stencil;
   unsigned i = 0, pass_nr = 0;
   enum pipe_format format = util_format_get_depth_only(data->format);
   double min = dsa->depth_bounds_min;
   double max = dsa->depth_bounds_max;

   for (i = 0; i < nr; i++) {
      unsigned j = 0, passMask = 0;

      get_depth_stencil_values(data, quads[i]);

      if (format == PIPE_FORMAT_Z32_FLOAT) {
         for (j = 0; j < TGSI_QUAD_SIZE; j++) {
            double z = uif(data->bzzzz[j]);

            if (z >= min && z <= max)
               passMask |= (1 << j);
         }
      } else {
         unsigned imin, imax;

         if (format == PIPE_FORMAT_Z16_UNORM) {
            imin = ((unsigned) (min * 65535.0)) & 0xffff;
            imax = ((unsigned) (max * 65535.0)) & 0xffff;
         } else if (format == PIPE_FORMAT_Z32_UNORM) {
            imin = (unsigned) (min * 4294967295.0);
            imax = (unsigned) (max * 4294967295.0);
         } else if (format == PIPE_FORMAT_Z24X8_UNORM ||
                    format == PIPE_FORMAT_X8Z24_UNORM) {
            imin = ((unsigned) (min * 16777215.0)) & 0xffffff;
            imax = ((unsigned) (max * 16777215.0)) & 0xffffff;
         } else {
            UNREACHABLE("Unknown depth buffer format");
         }

         for (j = 0; j < TGSI_QUAD_SIZE; j++) {
            unsigned iz = data->bzzzz[j];

            if (iz >= imin && iz <= imax)
               passMask |= (1 << j);
         }
      }

      quads[i]->inout.mask &= passMask;

      if (quads[i]->inout.mask)
         quads[pass_nr++] = quads[i];
   }

   return pass_nr;
}


static unsigned mask_count[16] = 
{
   0,                           /* 0x0 */
   1,                           /* 0x1 */
   1,                           /* 0x2 */
   2,                           /* 0x3 */
   1,                           /* 0x4 */
   2,                           /* 0x5 */
   2,                           /* 0x6 */
   3,                           /* 0x7 */
   1,                           /* 0x8 */
   2,                           /* 0x9 */
   2,                           /* 0xa */
   3,                           /* 0xb */
   2,                           /* 0xc */
   3,                           /* 0xd */
   3,                           /* 0xe */
   4,                           /* 0xf */
};



/**
 * General depth/stencil test function.  Used when there's no fast-path.
 */
static void
depth_test_quads_fallback(struct quad_stage *qs, 
                          struct quad_header *quads[],
                          unsigned nr)
{
   unsigned i, pass = 0;
   const struct tgsi_shader_info *fsInfo = &qs->softpipe->fs_variant->info;
   bool interp_depth = !fsInfo->writes_z || qs->softpipe->early_depth;
   bool shader_stencil_ref = fsInfo->writes_stencil;
   bool have_zs = !!qs->softpipe->framebuffer.zsbuf.texture;
   struct depth_data data;
   unsigned vp_idx = quads[0]->input.viewport_index;

   data.use_shader_stencil_refs = false;

   if (have_zs && (qs->softpipe->depth_stencil->depth_enabled ||
                   qs->softpipe->depth_stencil->stencil[0].enabled ||
                   qs->softpipe->depth_stencil->depth_bounds_test)) {
      float near_val, far_val;

      data.format = qs->softpipe->framebuffer.zsbuf.format;
      data.tile = sp_get_cached_tile(qs->softpipe->zsbuf_cache, 
                                     quads[0]->input.x0, 
                                     quads[0]->input.y0, quads[0]->input.layer);
      data.clamp = !qs->softpipe->rasterizer->depth_clip_near;

      near_val = qs->softpipe->viewports[vp_idx].translate[2] - qs->softpipe->viewports[vp_idx].scale[2];
      far_val = near_val + (qs->softpipe->viewports[vp_idx].scale[2] * 2.0);
      data.minval = MIN2(near_val, far_val);
      data.maxval = MAX2(near_val, far_val);
   }

   /* EXT_depth_bounds_test says:
    *
    *     Where should the depth bounds test take place in the OpenGL fragment
    *     processing pipeline?
    *
    *       RESOLUTION:  After scissor test, before alpha test. In practice,
    *       this is a logical placement of the test.  An implementation is
    *       free to perform the test in a manner that is consistent with the
    *       specified ordering.
    */

   if (have_zs && qs->softpipe->depth_stencil->depth_bounds_test) {
      nr = depth_bounds_test_quads(qs, quads, nr, &data);
   }

   if (qs->softpipe->depth_stencil->alpha_enabled) {
      nr = alpha_test_quads(qs, quads, nr);
   }

   if (have_zs && (qs->softpipe->depth_stencil->depth_enabled ||
                   qs->softpipe->depth_stencil->stencil[0].enabled)) {
      for (i = 0; i < nr; i++) {
         get_depth_stencil_values(&data, quads[i]);

         if (qs->softpipe->depth_stencil->depth_enabled) {
            if (interp_depth)
               interpolate_quad_depth(quads[i]);

            convert_quad_depth(&data, quads[i]);
         }

         if (qs->softpipe->depth_stencil->stencil[0].enabled) {
            if (shader_stencil_ref)
               convert_quad_stencil(&data, quads[i]);
            
            depth_stencil_test_quad(qs, &data, quads[i]);
            write_depth_stencil_values(&data, quads[i]);
         }
         else {
            if (!depth_test_quad(qs, &data, quads[i]))
               continue;

            if (qs->softpipe->depth_stencil->depth_writemask)
               write_depth_stencil_values(&data, quads[i]);
         }

         quads[pass++] = quads[i];
      }

      nr = pass;
   }

   if (qs->softpipe->active_query_count) {
      for (i = 0; i < nr; i++) 
         qs->softpipe->occlusion_count += mask_count[quads[i]->inout.mask];
   }

   if (nr)
      qs->next->run(qs->next, quads, nr);
}


/**
 * Special-case Z testing for 16-bit Zbuffer and Z buffer writes enabled.
 */

#define NAME depth_interp_z16_less_write
#define OPERATOR <
#include "sp_quad_depth_test_tmp.h"

#define NAME depth_interp_z16_equal_write
#define OPERATOR ==
#include "sp_quad_depth_test_tmp.h"

#define NAME depth_interp_z16_lequal_write
#define OPERATOR <=
#include "sp_quad_depth_test_tmp.h"

#define NAME depth_interp_z16_greater_write
#define OPERATOR >
#include "sp_quad_depth_test_tmp.h"

#define NAME depth_interp_z16_notequal_write
#define OPERATOR !=
#include "sp_quad_depth_test_tmp.h"

#define NAME depth_interp_z16_gequal_write
#define OPERATOR >=
#include "sp_quad_depth_test_tmp.h"

#define NAME depth_interp_z16_always_write
#define ALWAYS 1
#include "sp_quad_depth_test_tmp.h"



static void
depth_noop(struct quad_stage *qs, 
           struct quad_header *quads[],
           unsigned nr)
{
   qs->next->run(qs->next, quads, nr);
}



static void
choose_depth_test(struct quad_stage *qs, 
                  struct quad_header *quads[],
                  unsigned nr)
{
   const struct tgsi_shader_info *fsInfo = &qs->softpipe->fs_variant->info;

   bool interp_depth = !fsInfo->writes_z || qs->softpipe->early_depth;

   bool alpha = qs->softpipe->depth_stencil->alpha_enabled;

   bool depth = qs->softpipe->depth_stencil->depth_enabled;

   unsigned depthfunc = qs->softpipe->depth_stencil->depth_func;

   bool stencil = qs->softpipe->depth_stencil->stencil[0].enabled;

   bool depthwrite = qs->softpipe->depth_stencil->depth_writemask;

   bool occlusion = qs->softpipe->active_query_count;

   bool clipped = !qs->softpipe->rasterizer->depth_clip_near;

   bool depth_bounds = qs->softpipe->depth_stencil->depth_bounds_test;

   if(!qs->softpipe->framebuffer.zsbuf.texture)
      depth = depthwrite = stencil = false;

   /* default */
   qs->run = depth_test_quads_fallback;

   /* look for special cases */
   if (!alpha &&
       !depth &&
       !occlusion &&
       !clipped &&
       !stencil &&
       !depth_bounds) {
      qs->run = depth_noop;
   }
   else if (!alpha && 
            interp_depth && 
            depth && 
            depthwrite && 
            !occlusion &&
            !clipped &&
            !stencil &&
            !depth_bounds)
   {
      if (qs->softpipe->framebuffer.zsbuf.format == PIPE_FORMAT_Z16_UNORM) {
         switch (depthfunc) {
         case PIPE_FUNC_NEVER:
            qs->run = depth_test_quads_fallback;
            break;
         case PIPE_FUNC_LESS:
            qs->run = depth_interp_z16_less_write;
            break;
         case PIPE_FUNC_EQUAL:
            qs->run = depth_interp_z16_equal_write;
            break;
         case PIPE_FUNC_LEQUAL:
            qs->run = depth_interp_z16_lequal_write;
            break;
         case PIPE_FUNC_GREATER:
            qs->run = depth_interp_z16_greater_write;
            break;
         case PIPE_FUNC_NOTEQUAL:
            qs->run = depth_interp_z16_notequal_write;
            break;
         case PIPE_FUNC_GEQUAL:
            qs->run = depth_interp_z16_gequal_write;
            break;
         case PIPE_FUNC_ALWAYS:
            qs->run = depth_interp_z16_always_write;
            break;
         default:
            qs->run = depth_test_quads_fallback;
            break;
         }
      }
   }

   /* next quad/fragment stage */
   qs->run( qs, quads, nr );
}



static void
depth_test_begin(struct quad_stage *qs)
{
   qs->run = choose_depth_test;
   qs->next->begin(qs->next);
}


static void
depth_test_destroy(struct quad_stage *qs)
{
   FREE( qs );
}


struct quad_stage *
sp_quad_depth_test_stage(struct softpipe_context *softpipe)
{
   struct quad_stage *stage = CALLOC_STRUCT(quad_stage);

   stage->softpipe = softpipe;
   stage->begin = depth_test_begin;
   stage->run = choose_depth_test;
   stage->destroy = depth_test_destroy;

   return stage;
}

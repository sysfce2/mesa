/*
 * Copyright (c) 2012-2015 Etnaviv Project
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sub license,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial portions
 * of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Authors:
 *    Wladimir J. van der Laan <laanwj@gmail.com>
 */

#ifndef H_ETNAVIV_BLEND
#define H_ETNAVIV_BLEND

#include "pipe/p_context.h"
#include "pipe/p_state.h"

struct etna_context;

struct etna_rt_blend_state {
   uint32_t PE_ALPHA_CONFIG;
   uint32_t PE_COLOR_FORMAT;
   uint32_t PE_HALTI5_COLORMASK;
   bool alpha_enable : 1;
   bool separate_alpha : 1;
   bool fo_allowed : 1;
};

struct etna_blend_state {
   struct pipe_blend_state base;
   struct etna_rt_blend_state rt[PIPE_MAX_COLOR_BUFS];

   uint32_t PE_LOGIC_OP;
   uint32_t PE_DITHER[2];
   uint32_t PS_MSAA_CONFIG;
};

static inline struct etna_blend_state *
etna_blend_state(struct pipe_blend_state *blend)
{
   return (struct etna_blend_state *)blend;
}

void *
etna_blend_state_create(struct pipe_context *pctx,
                        const struct pipe_blend_state *so);

bool
etna_update_blend(struct etna_context *ctx);

void
etna_set_blend_color(struct pipe_context *pctx, const struct pipe_blend_color *bc);

bool
etna_update_blend_color(struct etna_context *ctx);

#endif

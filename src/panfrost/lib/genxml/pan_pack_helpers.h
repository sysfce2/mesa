/*
 * Copyright 2024 Collabora Ltd.
 * SPDX-License-Identifier: MIT
 */

#ifndef PAN_PAN_HELPERS_H
#define PAN_PAN_HELPERS_H

#include <inttypes.h>
#include <stdio.h>

#include "util/bitpack_helpers.h"

#define __gen_unpack_float(x, y, z) uif(__gen_unpack_uint(x, y, z))

static inline uint32_t
__gen_padded(uint32_t v, uint32_t start, uint32_t end)
{
   unsigned shift = __builtin_ctz(v);
   unsigned odd = v >> (shift + 1);

#ifndef NDEBUG
   assert((v >> shift) & 1);
   assert(shift <= 31);
   assert(odd <= 7);
   assert((end - start + 1) == 8);
#endif

   return util_bitpack_uint(shift | (odd << 5), start, end);
}

static inline uint64_t
__gen_unpack_uint(const uint8_t *restrict cl, uint32_t start, uint32_t end)
{
   uint64_t val = 0;
   const int width = end - start + 1;
   const uint64_t mask = (width == 64 ? ~0 : (1ull << width) - 1);

   for (uint32_t byte = start / 8; byte <= end / 8; byte++) {
      val |= ((uint64_t)cl[byte]) << ((byte - start / 8) * 8);
   }

   return (val >> (start % 8)) & mask;
}

static inline uint64_t
__gen_unpack_sint(const uint8_t *restrict cl, uint32_t start, uint32_t end)
{
   int size = end - start + 1;
   int64_t val = __gen_unpack_uint(cl, start, end);

   return util_sign_extend(val, size);
}

static inline float
__gen_unpack_ulod(const uint8_t *restrict cl, uint32_t start, uint32_t end)
{
   uint32_t u = __gen_unpack_uint(cl, start, end);

   return ((float)u) / 256.0;
}

static inline float
__gen_unpack_slod(const uint8_t *restrict cl, uint32_t start, uint32_t end)
{
   int32_t u = __gen_unpack_sint(cl, start, end);

   return ((float)u) / 256.0;
}

static inline uint64_t
__gen_unpack_padded(const uint8_t *restrict cl, uint32_t start, uint32_t end)
{
   unsigned val = __gen_unpack_uint(cl, start, end);
   unsigned shift = val & 0b11111;
   unsigned odd = val >> 5;

   return (2 * odd + 1) << shift;
}

#define PREFIX1(A)          MALI_##A
#define PREFIX2(A, B)       MALI_##A##_##B
#define PREFIX4(A, B, C, D) MALI_##A##_##B##_##C##_##D

#define pan_pack(dst, T, name)                                                 \
   for (struct PREFIX1(T) name = {PREFIX2(T, header)},                         \
                          *_loop_terminate = &name;                            \
        __builtin_expect(_loop_terminate != NULL, 1); ({                       \
           PREFIX2(T, pack)((uint32_t *)(dst), &name);                         \
           _loop_terminate = NULL;                                             \
        }))

#define pan_pack_nodefaults(dst, T, name)                                      \
   for (struct PREFIX1(T) name = {0}, *_loop_terminate = &name;                \
        __builtin_expect(_loop_terminate != NULL, 1); ({                       \
           PREFIX2(T, pack)((uint32_t *)(dst), &name);                         \
           _loop_terminate = NULL;                                             \
        }))

#define pan_unpack(src, T, name)                                               \
   struct PREFIX1(T) name;                                                     \
   PREFIX2(T, unpack)((uint8_t *)(src), &name)

#define pan_print(fp, T, var, indent) PREFIX2(T, print)(fp, &(var), indent)

#define pan_size(T)      PREFIX2(T, LENGTH)
#define pan_alignment(T) PREFIX2(T, ALIGN)

#define pan_section_offset(A, S) PREFIX4(A, SECTION, S, OFFSET)

#define pan_section_ptr(base, A, S)                                            \
   ((void *)((uint8_t *)(base) + pan_section_offset(A, S)))

#define pan_section_pack(dst, A, S, name)                                      \
   for (PREFIX4(A, SECTION, S, TYPE) name = {PREFIX4(A, SECTION, S, header)},  \
                                     *_loop_terminate = (void *)(dst);         \
        __builtin_expect(_loop_terminate != NULL, 1); ({                       \
           PREFIX4(A, SECTION, S, pack)(pan_section_ptr(dst, A, S), &name);    \
           _loop_terminate = NULL;                                             \
        }))

#define pan_section_unpack(src, A, S, name)                                    \
   PREFIX4(A, SECTION, S, TYPE) name;                                          \
   PREFIX4(A, SECTION, S, unpack)(pan_section_ptr(src, A, S), &name)

#define pan_section_print(fp, A, S, var, indent)                               \
   PREFIX4(A, SECTION, S, print)(fp, &(var), indent)

static inline void
pan_merge_helper(uint32_t *dst, const uint32_t *src, size_t bytes)
{
   assert((bytes & 3) == 0);

   for (unsigned i = 0; i < (bytes / 4); ++i)
      dst[i] |= src[i];
}

#define pan_merge(packed1, packed2, type)                                      \
   pan_merge_helper((packed1).opaque, (packed2).opaque, pan_size(type))

static inline const char *
mali_component_swizzle(unsigned val)
{
   static const char swiz_name[] = "RGBA01??";
   static char out_str[5], *outp;
   outp = out_str;
   for (int i = 0; i < 12; i += 3) {
      *outp++ = swiz_name[(val >> i) & 7];
   }
   *outp = 0;
   return out_str;
}

/* From presentations, 16x16 tiles externally. Use shift for fast computation
 * of tile numbers. */

#define MALI_TILE_SHIFT  4
#define MALI_TILE_LENGTH (1 << MALI_TILE_SHIFT)

#endif
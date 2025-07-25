#ifndef NV_PUSH_H
#define NV_PUSH_H

#include "nvtypes.h"
#include "util/macros.h"

#include <assert.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

struct nv_device_info;

struct nv_push {
   uint32_t *start;
   uint32_t *end;
   uint32_t *limit;

   /* A pointer to the last method header */
   uint32_t *last_hdr;

   /* The value in the last method header, used to avoid read-back */
   uint32_t last_hdr_dw;
};

static inline void
nv_push_init(struct nv_push *push, uint32_t *start, size_t dw_count)
{
   push->start = start;
   push->end = start;
   push->limit = start + dw_count;
   push->last_hdr = NULL;
   push->last_hdr_dw = 0;
}

static inline size_t
nv_push_dw_count(const struct nv_push *push)
{
   assert(push->start <= push->end);
   assert(push->end <= push->limit);
   return push->end - push->start;
}

#ifndef NDEBUG
void nv_push_validate(struct nv_push *push);
#else
static inline void nv_push_validate(struct nv_push *push) { }
#endif

void vk_push_print(FILE *fp, const struct nv_push *push,
                   const struct nv_device_info *devinfo);

#define SUBC_NV9097 0
#define SUBC_NVA097 0
#define SUBC_NVB097 0
#define SUBC_NVB197 0
#define SUBC_NVC097 0
#define SUBC_NVC397 0
#define SUBC_NVC597 0
#define SUBC_NVC797 0
#define SUBC_NVC86F 0
#define SUBC_NVCB97 0
#define SUBC_NVCD97 0

#define SUBC_NV90C0 1
#define SUBC_NVA0C0 1
#define SUBC_NVB0C0 1
#define SUBC_NVB1C0 1
#define SUBC_NVC0C0 1
#define SUBC_NVC3C0 1
#define SUBC_NVC6C0 1

#define SUBC_NV9039 2

#define SUBC_NV902D 3

#define SUBC_NV90B5 4
#define SUBC_NVC1B5 4

static inline uint32_t
NVC0_FIFO_PKHDR_SQ(int subc, int mthd, unsigned size)
{
   return 0x20000000 | (size << 16) | (subc << 13) | (mthd >> 2);
}

static inline void
__push_verify(struct nv_push *push)
{
   if (push->last_hdr == NULL)
      return;

   /* check for immd */
   if (push->last_hdr_dw >> 29 == 4)
      return;

   ASSERTED uint32_t last_count = (push->last_hdr_dw & 0x1fff0000);
   assert(last_count);
}

static inline void
__push_hdr(struct nv_push *push, uint32_t hdr)
{
   __push_verify(push);

   *push->end = hdr;
   push->last_hdr_dw = hdr;
   push->last_hdr = push->end;
   push->end++;
}

static inline void
__push_mthd_size(struct nv_push *push, int subc, uint32_t mthd, unsigned size)
{
   __push_hdr(push, NVC0_FIFO_PKHDR_SQ(subc, mthd, size));
}

static inline void
__push_mthd(struct nv_push *push, int subc, uint32_t mthd)
{
   __push_mthd_size(push, subc, mthd, 0);
}

#define P_MTHD(push, class, mthd) __push_mthd(push, SUBC_##class, class##_##mthd)

static inline uint32_t
NVC0_FIFO_PKHDR_IL(int subc, int mthd, uint16_t data)
{
   assert(!(data & ~0x1fff));
   return 0x80000000 | (data << 16) | (subc << 13) | (mthd >> 2);
}

static inline void
__push_immd(struct nv_push *push, int subc, uint32_t mthd, uint32_t val)
{
   __push_hdr(push, NVC0_FIFO_PKHDR_IL(subc, mthd, val));
}

#define P_IMMD(push, class, mthd, args...) do {                         \
   uint32_t __val;                                                      \
   VA_##class##_##mthd(__val, args);                                    \
   if (__builtin_constant_p(__val & ~0x1fff) && !(__val & ~0x1fff)) {   \
      __push_immd(push, SUBC_##class, class##_##mthd, __val);           \
   } else {                                                             \
      __push_mthd_size(push, SUBC_##class, class##_##mthd, 0);          \
      nv_push_val(push, class##_##mthd, __val);                        \
   }                                                                    \
} while(0)

static inline uint32_t
NVC0_FIFO_PKHDR_1I(int subc, int mthd, unsigned size)
{
   return 0xa0000000 | (size << 16) | (subc << 13) | (mthd >> 2);
}

static inline void
__push_1inc(struct nv_push *push, int subc, uint32_t mthd)
{
   __push_hdr(push, NVC0_FIFO_PKHDR_1I(subc, mthd, 0));
}

#define P_1INC(push, class, mthd) __push_1inc(push, SUBC_##class, class##_##mthd)

static inline uint32_t
NVC0_FIFO_PKHDR_0I(int subc, int mthd, unsigned size)
{
   return 0x60000000 | (size << 16) | (subc << 13) | (mthd >> 2);
}

static inline void
__push_0inc(struct nv_push *push, int subc, uint32_t mthd)
{
   __push_hdr(push, NVC0_FIFO_PKHDR_0I(subc, mthd, 0));
}

#define P_0INC(push, class, mthd) __push_0inc(push, SUBC_##class, class##_##mthd)

#define NV_PUSH_MAX_COUNT 0x1fff

static inline bool
nv_push_update_count(struct nv_push *push, uint16_t count)
{
   assert(push->last_hdr != NULL);

   assert(count <= NV_PUSH_MAX_COUNT);
   if (count > NV_PUSH_MAX_COUNT)
      return false;

   uint32_t hdr_dw = push->last_hdr_dw;

   /* size is encoded at 28:16 */
   uint32_t new_count = (count + (hdr_dw >> 16)) & NV_PUSH_MAX_COUNT;
   bool overflow = new_count < count;
   /* if we would overflow, don't change anything and just let it be */
   assert(!overflow);
   if (overflow)
      return false;

   hdr_dw &= ~0x1fff0000;
   hdr_dw |= new_count << 16;
   push->last_hdr_dw = hdr_dw;
   *push->last_hdr = hdr_dw;
   return true;
}

static inline void
P_INLINE_DATA(struct nv_push *push, uint32_t value)
{
   assert(push->end < push->limit);
   if (nv_push_update_count(push, 1)) {
      /* push new value */
      *push->end = value;
      push->end++;
   }
}

static inline void
P_INLINE_FLOAT(struct nv_push *push, float value)
{
   assert(push->end < push->limit);
   if (nv_push_update_count(push, 1)) {
      /* push new value */
      *(float *)push->end = value;
      push->end++;
   }
}

static inline void
P_INLINE_ARRAY(struct nv_push *push, const uint32_t *data, int num_dw)
{
   assert(push->end + num_dw <= push->limit);
   if (nv_push_update_count(push, num_dw)) {
      /* push new value */
      memcpy(push->end, data, num_dw * 4);
      push->end += num_dw;
   }
}

/* internally used by generated inlines. */
static inline void
nv_push_val(struct nv_push *push, uint32_t idx, uint32_t val)
{
   ASSERTED uint32_t last_hdr_dw = push->last_hdr_dw;
   ASSERTED bool is_0inc = (last_hdr_dw & 0xe0000000) == 0x60000000;
   ASSERTED bool is_1inc = (last_hdr_dw & 0xe0000000) == 0xa0000000;
   ASSERTED bool is_immd = (last_hdr_dw & 0xe0000000) == 0x80000000;
   ASSERTED uint16_t last_method = (last_hdr_dw & 0x1fff) << 2;

   uint16_t distance = push->end - push->last_hdr - 1;
   if (is_0inc)
      distance = 0;
   else if (is_1inc)
      distance = MIN2(1, distance);
   last_method += distance * 4;

   /* can't have empty headers ever */
   assert(last_hdr_dw);
   assert(!is_immd);
   assert(last_method == idx);
   assert(push->end < push->limit);

   P_INLINE_DATA(push, val);
}

static inline void
nv_push_raw(struct nv_push *push, uint32_t *raw_dw, uint32_t dw_count)
{
   assert(push->end + dw_count <= push->limit);
   memcpy(push->end, raw_dw, dw_count * 4);
   push->end += dw_count;
   push->last_hdr = NULL;
}

#endif /* NV_PUSH_H */

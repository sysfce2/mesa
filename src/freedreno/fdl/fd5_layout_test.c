/*
 * Copyright © 2020 Google LLC
 * SPDX-License-Identifier: MIT
 */

#include "freedreno_layout.h"
#include "fd_layout_test.h"
#include "adreno_common.xml.h"
#include "util/half_float.h"
#include "util/u_math.h"
#include "a5xx.xml.h"

#include <stdio.h>

/* Testcases generated from cffdump --script texturator-to-unit-test-5xx.lua
 * on a Pixel 2
 */
static const struct testcase testcases[] = {
   /* Basic POT, non-UBWC layout test */
   {
      .format = PIPE_FORMAT_R9G9B9E5_FLOAT,
      .layout =
         {
            .tile_mode = TILE5_3,
            .width0 = 32,
            .height0 = 32,
            .slices =
               {
                  {.offset = 0, .pitch = 256},
                  {.offset = 8192, .pitch = 256},
                  {.offset = 12288, .pitch = 256},
                  {.offset = 14336, .pitch = 256},
                  {.offset = 15360, .pitch = 256},
                  {.offset = 15872, .pitch = 256},
               },
         },
   },

   /* Some 3D cases of sizes from the CTS, when I was suspicious of our 3D
    * layout.
    */
   {
      .format = PIPE_FORMAT_R9G9B9E5_FLOAT,
      .is_3d = true,
      .layout =
         {
            .tile_mode = TILE5_3,
            .ubwc = false,
            .width0 = 59,
            .height0 = 37,
            .depth0 = 11,
            .slices =
               {
                  {.offset = 0, .pitch = 256},
                  {.offset = 135168, .pitch = 256},
                  {.offset = 176128, .pitch = 256},
                  {.offset = 192512, .pitch = 256},
                  {.offset = 200704, .pitch = 256},
                  {.offset = 208896, .pitch = 256},
               },
         },
   },
   {
      .format = PIPE_FORMAT_R32G32_FLOAT,
      .is_3d = true,
      .layout =
         {
            .tile_mode = TILE5_3,
            .ubwc = false,
            .width0 = 63,
            .height0 = 29,
            .depth0 = 11,
            .slices =
               {
                  {.offset = 0, .pitch = 512},
                  {.offset = 180224, .pitch = 512},
                  {.offset = 221184, .pitch = 512},
                  {.offset = 237568, .pitch = 512},
                  {.offset = 245760, .pitch = 512},
                  {.offset = 253952, .pitch = 512},
               },
         },
   },
};

int
main(int argc, char **argv)
{
   int ret = 0;

   struct fd_dev_id a540_dev_id = {
      .gpu_id = 540,
   };
   for (int i = 0; i < ARRAY_SIZE(testcases); i++) {
      if (!fdl_test_layout(&testcases[i], &a540_dev_id))
         ret = 1;
   }

   return ret;
}

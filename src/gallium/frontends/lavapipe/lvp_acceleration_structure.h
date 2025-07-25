
/*
 * Copyright © 2021 Google
 * Copyright © 2023 Valve Corporation
 * SPDX-License-Identifier: MIT
 */

#ifndef LVP_ACCELERATION_STRUCTURE_H
#define LVP_ACCELERATION_STRUCTURE_H

#include "lvp_private.h"
#include "bvh/vk_bvh.h"

#define LVP_GEOMETRY_OPAQUE (1u << 31)

#define LVP_INSTANCE_FORCE_OPAQUE                 (1u << 31)
#define LVP_INSTANCE_NO_FORCE_NOT_OPAQUE          (1u << 30)
#define LVP_INSTANCE_TRIANGLE_FACING_CULL_DISABLE (1u << 29)
#define LVP_INSTANCE_TRIANGLE_FLIP_FACING         (1u << 28)

#define lvp_bvh_node_triangle 0
#define lvp_bvh_node_internal 1
#define lvp_bvh_node_instance 2
#define lvp_bvh_node_aabb     3

/* 48 bytes */
struct lvp_bvh_triangle_node {
   float coords[3][3];

   uint32_t padding;

   uint32_t primitive_id;
   /* flags in upper 4 bits */
   uint32_t geometry_id_and_flags;
};

/* 32 bytes */
struct lvp_bvh_aabb_node {
   vk_aabb bounds;

   uint32_t primitive_id;
   /* flags in upper 4 bits */
   uint32_t geometry_id_and_flags;
};

/* 120 bytes */
struct lvp_bvh_instance_node {
   uint64_t bvh_ptr;

   /* lower 24 bits are the custom instance index, upper 8 bits are the visibility mask */
   uint32_t custom_instance_and_mask;
   /* lower 24 bits are the sbt offset, upper 8 bits are VkGeometryInstanceFlagsKHR */
   uint32_t sbt_offset_and_flags;

   mat3x4 wto_matrix;
   uint32_t padding;

   uint32_t instance_id;

   /* Object to world matrix transposed from the initial transform. */
   mat3x4 otw_matrix;
};

/* 56 bytes */
struct lvp_bvh_box_node {
   vk_aabb bounds[2];
   uint32_t children[2];
};

#define LVP_BVH_NODE_PREFETCH_SIZE 56

struct lvp_bvh_header {
   vk_aabb bounds;

   uint32_t compacted_size;
   uint32_t serialization_size;
   uint32_t instance_count;
   uint32_t leaf_nodes_offset;
};

struct lvp_accel_struct_serialization_header {
   uint8_t driver_uuid[VK_UUID_SIZE];
   uint8_t accel_struct_compat[VK_UUID_SIZE];
   uint64_t serialization_size;
   uint64_t compacted_size;
   uint64_t instance_count;
   uint64_t instances[];
};

/* The root node is the first node after the header. */
#define LVP_BVH_ROOT_NODE_OFFSET (sizeof(struct lvp_bvh_header))
#define LVP_BVH_ROOT_NODE        (LVP_BVH_ROOT_NODE_OFFSET | lvp_bvh_node_internal)
#define LVP_BVH_INVALID_NODE     0xFFFFFFFF

VkResult
lvp_device_init_accel_struct_state(struct lvp_device *device);

void
lvp_device_finish_accel_struct_state(struct lvp_device *device);

#endif

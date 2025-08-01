/**************************************************************************
 *
 * Copyright 2011 Intel Corporation
 * Copyright 2012 Francisco Jerez
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
 * IN NO EVENT SHALL VMWARE AND/OR ITS SUPPLIERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Authors:
 *    Kristian Høgsberg <krh@bitplanet.net>
 *    Benjamin Franzke <benjaminfranzke@googlemail.com>
 *
 **************************************************************************/

#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <xf86drm.h>
#include <unistd.h>
#include <fcntl.h>

#include "loader.h"
#include "target-helpers/drm_helper_public.h"
#include "frontend/drm_driver.h"
#include "pipe_loader_priv.h"

#include "util/log.h"
#include "util/os_file.h"
#include "util/u_memory.h"
#include "util/u_debug.h"
#include "util/xmlconfig.h"

#include "virtio/virtio-gpu/drm_hw.h"
#include "drm-uapi/virtgpu_drm.h"

#define DRM_RENDER_NODE_DEV_NAME_FORMAT "%s/renderD%d"
#define DRM_RENDER_NODE_MAX_NODES 63
#define DRM_RENDER_NODE_MIN_MINOR 128
#define DRM_RENDER_NODE_MAX_MINOR (DRM_RENDER_NODE_MIN_MINOR + DRM_RENDER_NODE_MAX_NODES)

struct pipe_loader_drm_device {
   struct pipe_loader_device base;
   const struct drm_driver_descriptor *dd;
   int fd;
};

#define pipe_loader_drm_device(dev) ((struct pipe_loader_drm_device *)dev)

static const struct pipe_loader_ops pipe_loader_drm_ops;

static const struct drm_driver_descriptor *driver_descriptors[] = {
   &i915_driver_descriptor,
   &iris_driver_descriptor,
   &crocus_driver_descriptor,
   &nouveau_driver_descriptor,
   &r300_driver_descriptor,
   &r600_driver_descriptor,
   &radeonsi_driver_descriptor,
   &vmwgfx_driver_descriptor,
   &kgsl_driver_descriptor,
   &msm_driver_descriptor,
   &virtio_gpu_driver_descriptor,
   &v3d_driver_descriptor,
   &vc4_driver_descriptor,
   &panfrost_driver_descriptor,
   &panthor_driver_descriptor,
   &asahi_driver_descriptor,
   &etnaviv_driver_descriptor,
   &rocket_driver_descriptor,
   &tegra_driver_descriptor,
   &lima_driver_descriptor,
   &zink_driver_descriptor,
};

static const struct drm_driver_descriptor *
get_driver_descriptor(const char *driver_name)
{
   for (int i = 0; i < ARRAY_SIZE(driver_descriptors); i++) {
      if (strcmp(driver_descriptors[i]->driver_name, driver_name) == 0)
         return driver_descriptors[i];
   }
   return &kmsro_driver_descriptor;
}

static int
get_nctx_caps(int fd, struct virgl_renderer_capset_drm *caps)
{
   struct drm_virtgpu_get_caps args = {
         .cap_set_id = VIRTGPU_DRM_CAPSET_DRM,
         .cap_set_ver = 0,
         .addr = (uintptr_t)caps,
         .size = sizeof(*caps),
   };

   return drmIoctl(fd, DRM_IOCTL_VIRTGPU_GET_CAPS, &args);
}

static bool
pipe_loader_drm_probe_fd_nodup(struct pipe_loader_device **dev, int fd, bool zink)
{
   struct pipe_loader_drm_device *ddev = CALLOC_STRUCT(pipe_loader_drm_device);
   int vendor_id, chip_id;

   if (!ddev)
      return false;

   if (loader_get_pci_id_for_fd(fd, &vendor_id, &chip_id)) {
      ddev->base.type = PIPE_LOADER_DEVICE_PCI;
      ddev->base.u.pci.vendor_id = vendor_id;
      ddev->base.u.pci.chip_id = chip_id;
   } else {
      ddev->base.type = PIPE_LOADER_DEVICE_PLATFORM;
   }
   ddev->base.ops = &pipe_loader_drm_ops;
   ddev->fd = fd;

   if (zink)
      ddev->base.driver_name = strdup("zink");
   else
      ddev->base.driver_name = loader_get_driver_for_fd(fd);
   if (!ddev->base.driver_name)
      goto fail;

   /* For the closed source AMD OpenGL driver, we want libgbm to load
    * "amdgpu_dri.so", but we want Gallium multimedia drivers to load
    * "radeonsi". So change amdgpu to radeonsi for Gallium.
    */
   if (strcmp(ddev->base.driver_name, "amdgpu") == 0) {
      FREE(ddev->base.driver_name);
      ddev->base.driver_name = strdup("radeonsi");
   }

   if (strcmp(ddev->base.driver_name, "virtio_gpu") == 0) {
      struct virgl_renderer_capset_drm caps;
      if (get_nctx_caps(fd, &caps) == 0) {
         for (int i = 0; i < ARRAY_SIZE(driver_descriptors); i++) {
            if (!driver_descriptors[i]->probe_nctx)
               continue;
            if (!driver_descriptors[i]->probe_nctx(fd, &caps))
               continue;

            FREE(ddev->base.driver_name);
            ddev->base.driver_name = strdup(driver_descriptors[i]->driver_name);
            break;
         }
      }
   }

   ddev->dd = get_driver_descriptor(ddev->base.driver_name);

   /* vgem is a virtual device; don't try using it with kmsro */
   if (strcmp(ddev->base.driver_name, "vgem") == 0)
      goto fail;

   /* kmsro supports lots of drivers, try as a fallback */
   if (!ddev->dd && !zink)
      ddev->dd = get_driver_descriptor("kmsro");

   if (!ddev->dd)
      goto fail;

   *dev = &ddev->base;
   return true;

  fail:
   FREE(ddev->base.driver_name);
   FREE(ddev);
   return false;
}

bool
pipe_loader_drm_probe_fd(struct pipe_loader_device **dev, int fd, bool zink)
{
   bool ret;
   int new_fd;

   if (fd < 0 || (new_fd = os_dupfd_cloexec(fd)) < 0)
     return false;

   ret = pipe_loader_drm_probe_fd_nodup(dev, new_fd, zink);
   if (!ret)
      close(new_fd);

   return ret;
}

static int
open_drm_render_node_minor(int minor)
{
   char path[PATH_MAX];
   snprintf(path, sizeof(path), DRM_RENDER_NODE_DEV_NAME_FORMAT, DRM_DIR_NAME,
            minor);
   return loader_open_device(path);
}

static int
pipe_loader_drm_probe_internal(struct pipe_loader_device **devs, int ndev, bool zink)
{
   int i, j, fd;

   for (i = DRM_RENDER_NODE_MIN_MINOR, j = 0;
        i <= DRM_RENDER_NODE_MAX_MINOR; i++) {
      struct pipe_loader_device *dev;

      fd = open_drm_render_node_minor(i);
      if (fd < 0)
         continue;

      if (!pipe_loader_drm_probe_fd_nodup(&dev, fd, zink)) {
         close(fd);
         continue;
      }

      if (j < ndev) {
         devs[j] = dev;
      } else {
         close(fd);
         dev->ops->release(&dev);
      }
      j++;
   }

   return j;
}

int
pipe_loader_drm_probe(struct pipe_loader_device **devs, int ndev)
{
   return pipe_loader_drm_probe_internal(devs, ndev, false);
}

#define DRM_ACCEL_DEV_NAME_FORMAT "%s/accel%d"
#define DRM_ACCEL_MAX_MINOR 255
#define DRM_ACCEL_DIR_NAME  "/dev/accel"

static int
open_accel_minor(int minor)
{
   char path[PATH_MAX];
   snprintf(path, sizeof(path), DRM_ACCEL_DEV_NAME_FORMAT, DRM_ACCEL_DIR_NAME,
            minor);
   return loader_open_device(path);
}

static bool
pipe_loader_accel_probe_fd_nodup(struct pipe_loader_device **dev, int fd)
{
   struct pipe_loader_drm_device *ddev = CALLOC_STRUCT(pipe_loader_drm_device);

   if (!ddev)
      return false;

   ddev->base.type = PIPE_LOADER_DEVICE_PLATFORM;
   ddev->base.ops = &pipe_loader_drm_ops;
   ddev->fd = fd;

   ddev->base.driver_name = loader_get_kernel_driver_name(fd);
   if (!ddev->base.driver_name)
      goto fail;

   ddev->dd = get_driver_descriptor(ddev->base.driver_name);
   if (!ddev->dd)
      goto fail;

   *dev = &ddev->base;
   return true;

  fail:
   FREE(ddev->base.driver_name);
   FREE(ddev);
   return false;
}

int
pipe_loader_accel_probe(struct pipe_loader_device **devs, int ndev)
{
   int i, j, fd;

   for (i = 0, j = 0; i <= DRM_ACCEL_MAX_MINOR; i++) {
      struct pipe_loader_device *dev;

      fd = open_accel_minor(i);
      if (fd < 0)
         continue;

      if (!pipe_loader_accel_probe_fd_nodup(&dev, fd)) {
         close(fd);
         continue;
      }

      if (j < ndev) {
         devs[j] = dev;
      } else {
         close(fd);
         dev->ops->release(&dev);
      }
      j++;
   }

   return j;
}

#ifdef HAVE_ZINK
int
pipe_loader_drm_zink_probe(struct pipe_loader_device **devs, int ndev)
{
   return pipe_loader_drm_probe_internal(devs, ndev, true);
}
#endif

static void
pipe_loader_drm_release(struct pipe_loader_device **dev)
{
   struct pipe_loader_drm_device *ddev = pipe_loader_drm_device(*dev);

   close(ddev->fd);
   FREE(ddev->base.driver_name);
   pipe_loader_base_release(dev);
}

int
pipe_loader_get_compatible_render_capable_device_fd(int kms_only_fd)
{
   unsigned int n_devices = 0;
   int result = -1;
   int *gpu_fds = pipe_loader_get_compatible_render_capable_device_fds(kms_only_fd, &n_devices);

   if (n_devices > 0) {
      result = gpu_fds[0];
      for(unsigned int i = 1; i < n_devices; i++)
         close(gpu_fds[i]);
   }

   free(gpu_fds);

   return result;
}

int *
pipe_loader_get_compatible_render_capable_device_fds(int kms_only_fd, unsigned int *n_devices)
{
   bool is_platform_device;
   struct pipe_loader_device *dev;
   const char * const drivers[] = {
#if defined GALLIUM_ASAHI
      "asahi",
#endif
#if defined GALLIUM_ETNAVIV
      "etnaviv",
#endif
#if defined GALLIUM_FREEDRENO
      "msm",
#endif
#if defined GALLIUM_LIMA
      "lima",
#endif
#if defined GALLIUM_PANFROST
      "panfrost",
      "panthor",
#endif
#if defined GALLIUM_ROCKET
      "rocket",
#endif
#if defined GALLIUM_V3D
      "v3d",
#endif
#if defined GALLIUM_VC4
      "vc4",
#endif
   };

   if (!pipe_loader_drm_probe_fd(&dev, kms_only_fd, false))
      return NULL;
   is_platform_device = (dev->type == PIPE_LOADER_DEVICE_PLATFORM);
   pipe_loader_release(&dev, 1);

   /* For display-only devices that are not on the platform bus, we can't assume
    * that any of the rendering devices are compatible. */
   if (!is_platform_device)
      return NULL;

   /* For platform display-only devices, we try to find a render-capable device
    * on the platform bus and that should be compatible with the display-only
    * device. */
   if (ARRAY_SIZE(drivers) == 0)
      return NULL;

   return loader_open_render_node_platform_devices(drivers, ARRAY_SIZE(drivers), n_devices);
}

static const struct driOptionDescription *
pipe_loader_drm_get_driconf(struct pipe_loader_device *dev, unsigned *count)
{
   struct pipe_loader_drm_device *ddev = pipe_loader_drm_device(dev);

   *count = ddev->dd->driconf_count;
   return ddev->dd->driconf;
}

static struct pipe_screen *
pipe_loader_drm_create_screen(struct pipe_loader_device *dev,
                              const struct pipe_screen_config *config, bool sw_vk)
{
   struct pipe_loader_drm_device *ddev = pipe_loader_drm_device(dev);

   return ddev->dd->create_screen(ddev->fd, config);
}

const struct driOptionDescription *
pipe_loader_drm_get_driconf_by_name(const char *driver_name, unsigned *count)
{
   driOptionDescription *driconf = NULL;
   const struct drm_driver_descriptor *dd =
      get_driver_descriptor(driver_name);

   if (!dd) {
      *count = 0;
   } else {
      *count = dd->driconf_count;
      size_t size = sizeof(*driconf) * *count;
      size_t base_size = size;
      /* factor in all the statically allocated string lengths */
      for (unsigned i = 0; i < dd->driconf_count; i++) {
         if (dd->driconf[i].desc)
            size += strlen(dd->driconf[i].desc) + 1;
         if (dd->driconf[i].info.name)
            size += strlen(dd->driconf[i].info.name) + 1;
         if (dd->driconf[i].info.type == DRI_STRING)
            size += strlen(dd->driconf[i].value._string) + 1;
      }
      driconf = malloc(size);
      memcpy(driconf, dd->driconf, size);

      uint8_t *ptr = (void*)driconf;
      ptr += base_size;
      /* manually set up pointers and copy in all the statically allocated strings */
      for (unsigned i = 0; i < dd->driconf_count; i++) {
         if (dd->driconf[i].desc) {
            driconf[i].desc = (void*)ptr;
            size_t str_size = strlen(dd->driconf[i].desc) + 1;
            memcpy((void*)driconf[i].desc, dd->driconf[i].desc, str_size);
            ptr += str_size;
         }
         if (dd->driconf[i].info.name) {
            driconf[i].info.name = (void*)ptr;
            size_t str_size = strlen(dd->driconf[i].info.name) + 1;
            memcpy((void*)driconf[i].info.name, dd->driconf[i].info.name, str_size);
            ptr += str_size;
         }
         if (dd->driconf[i].info.type == DRI_STRING) {
            driconf[i].value._string = (void*)ptr;
            size_t str_size = strlen(dd->driconf[i].value._string) + 1;
            memcpy((void*)driconf[i].value._string, dd->driconf[i].value._string, str_size);
            ptr += str_size;
         }
      }
   }

   return driconf;
}

static const struct pipe_loader_ops pipe_loader_drm_ops = {
   .create_screen = pipe_loader_drm_create_screen,
   .get_driconf = pipe_loader_drm_get_driconf,
   .release = pipe_loader_drm_release
};

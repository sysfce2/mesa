/*
 * Copyright 2023 Google
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include "VirtGpu.h"
#include <xf86drm.h>

class DrmVirtGpuResource : public std::enable_shared_from_this<DrmVirtGpuResource>,
                             public VirtGpuResource {
   public:
    DrmVirtGpuResource(int64_t deviceHandle, uint32_t blobHandle, uint32_t resourceHandle,
                         uint64_t size);
    ~DrmVirtGpuResource();

    void intoRaw() override;
    uint32_t getResourceHandle() const override;
    uint32_t getBlobHandle() const override;
    uint64_t getSize() const override;
    int wait() override;

    VirtGpuResourceMappingPtr createMapping(void) override;
    int exportBlob(struct VirtGpuExternalHandle& handle) override;

    int transferFromHost(uint32_t x, uint32_t y, uint32_t w, uint32_t h) override;
    int transferToHost(uint32_t x, uint32_t y, uint32_t w, uint32_t h) override;

   private:
    // Not owned.  Really should use a ScopedFD for this, but doesn't matter since we have a
    // singleton deviceimplemenentation anyways.
    int64_t mDeviceHandle;

    uint32_t mBlobHandle;
    uint32_t mResourceHandle;
    uint64_t mSize;
};

class DrmVirtGpuResourceMapping : public VirtGpuResourceMapping {
   public:
    DrmVirtGpuResourceMapping(VirtGpuResourcePtr blob, uint8_t* ptr, uint64_t size);
    ~DrmVirtGpuResourceMapping(void);

    uint8_t* asRawPtr(void) override;

   private:
    VirtGpuResourcePtr mBlob;
    uint8_t* mPtr;
    uint64_t mSize;
};

class DrmVirtGpuDevice : public VirtGpuDevice {
   public:
    DrmVirtGpuDevice(enum VirtGpuCapset capset);
    int32_t init(int32_t descriptor);

    virtual ~DrmVirtGpuDevice();

    virtual int64_t getDeviceHandle(void);

    virtual struct VirtGpuCaps getCaps(void);

    VirtGpuResourcePtr createBlob(const struct VirtGpuCreateBlob& blobCreate) override;
    VirtGpuResourcePtr createResource(uint32_t width, uint32_t height, uint32_t stride,
                                      uint32_t size, uint32_t virglFormat, uint32_t target,
                                      uint32_t bind) override;

    virtual VirtGpuResourcePtr importBlob(const struct VirtGpuExternalHandle& handle);
    virtual int execBuffer(struct VirtGpuExecBuffer& execbuffer, const VirtGpuResource* blob);

    virtual bool getDrmInfo(VirtGpuDrmInfo* drmInfo) override;
    virtual bool getPciBusInfo(VirtGpuPciBusInfo* pciBusInfo) override;

   private:
    int64_t mDeviceHandle = -1;
    struct VirtGpuCaps mCaps;

    int openDevice();

    bool mHasPrimary;
    int mPrimaryMajor;
    int mPrimaryMinor;
    int mRenderMajor;
    int mRenderMinor;

    int mBusType;
    drmPciBusInfo mPciBusInfo;
};

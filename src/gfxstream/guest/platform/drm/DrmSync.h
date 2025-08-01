/*
 * Copyright 2023 Google
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include "Sync.h"

namespace gfxstream {

class DrmSyncHelper : public SyncHelper {
   public:
    DrmSyncHelper();

    int wait(int syncFd, int timeoutMilliseconds) override;

    void debugPrint(int syncFd) override;

    int dup(int syncFd) override;

    int close(int syncFd) override;
};

}  // namespace gfxstream

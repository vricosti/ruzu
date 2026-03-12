// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/hos_binder_driver.h
//! Port of zuyu/src/core/hle/service/nvnflinger/hos_binder_driver.cpp
//!
//! IHOSBinderDriver is the service interface for binder transactions.
//! It delegates to HosBinderDriverServer for actual binder management.
//! Full IPC handler implementation depends on the ServiceFramework infrastructure.

use std::sync::Arc;

use super::hos_binder_driver_server::HosBinderDriverServer;
use super::surface_flinger::SurfaceFlinger;

/// The IHOSBinderDriver service provides the display driver binder interface.
///
/// In upstream C++, this is a ServiceFramework with IPC handlers for
/// TransactParcel, AdjustRefcount, GetNativeHandle, and
/// TransactParcelAuto. The actual work is delegated to
/// HosBinderDriverServer.
pub struct IHosBinderDriver {
    server: Arc<HosBinderDriverServer>,
    surface_flinger: Arc<SurfaceFlinger>,
}

impl IHosBinderDriver {
    pub fn new(
        server: Arc<HosBinderDriverServer>,
        surface_flinger: Arc<SurfaceFlinger>,
    ) -> Self {
        Self {
            server,
            surface_flinger,
        }
    }

    pub fn get_server(&self) -> &Arc<HosBinderDriverServer> {
        &self.server
    }

    pub fn get_surface_flinger(&self) -> Arc<SurfaceFlinger> {
        Arc::clone(&self.surface_flinger)
    }

    /// Handle TransactParcel IPC command.
    pub fn transact_parcel(
        &self,
        id: i32,
        code: u32,
        parcel_data: &[u8],
        parcel_reply: &mut [u8],
        flags: u32,
    ) {
        self.server.transact(id, code, parcel_data, parcel_reply, flags);
    }

    /// Handle GetNativeHandle IPC command.
    pub fn get_native_handle(&self, id: i32, type_id: u32) -> Option<u32> {
        self.server.get_native_handle(id, type_id)
    }
}

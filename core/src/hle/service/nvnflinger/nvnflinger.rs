// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/nvnflinger.h
//! Port of zuyu/src/core/hle/service/nvnflinger/nvnflinger.cpp
//!
//! Nvnflinger is the display compositor service. It manages the lifecycle
//! of the HOS binder driver, the surface flinger, and provides the main
//! service registration.

use std::sync::Arc;

use super::hos_binder_driver::IHosBinderDriver;
use super::hos_binder_driver_server::HosBinderDriverServer;
use super::surface_flinger::SurfaceFlinger;

/// Nvnflinger manages the display services infrastructure.
///
/// In upstream C++, Nvnflinger creates and owns:
/// - HosBinderDriverServer (binder registry)
/// - SurfaceFlinger (compositor)
/// - IHOSBinderDriver (service interface)
///
/// It registers "dispdrv" as a named service.
pub struct Nvnflinger {
    server: Arc<HosBinderDriverServer>,
    surface_flinger: Arc<SurfaceFlinger>,
    binder_driver: Arc<IHosBinderDriver>,
}

impl Nvnflinger {
    pub fn new() -> Self {
        let server = HosBinderDriverServer::new();
        let surface_flinger = SurfaceFlinger::new(Arc::clone(&server));
        let binder_driver = Arc::new(IHosBinderDriver::new(
            Arc::clone(&server),
            Arc::clone(&surface_flinger),
        ));

        Self {
            server,
            surface_flinger,
            binder_driver,
        }
    }

    pub fn get_server(&self) -> &Arc<HosBinderDriverServer> {
        &self.server
    }

    pub fn get_surface_flinger(&self) -> &Arc<SurfaceFlinger> {
        &self.surface_flinger
    }

    pub fn get_binder_driver(&self) -> &Arc<IHosBinderDriver> {
        &self.binder_driver
    }
}

impl Default for Nvnflinger {
    fn default() -> Self {
        Self::new()
    }
}

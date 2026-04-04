// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/display_layer_manager.h
//! Port of zuyu/src/core/hle/service/am/display_layer_manager.cpp

use std::collections::BTreeSet;
use std::sync::Arc;

use crate::hle::kernel::k_process::KProcess;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::vi::container::Container;
use crate::hle::service::vi::application_display_service::IApplicationDisplayService;
use crate::hle::service::vi::manager_display_service::IManagerDisplayService;
use crate::hle::service::vi::vi;
use crate::hle::service::vi::vi_results;

use super::am_types::{AppletId, LibraryAppletMode};

/// Port of DisplayLayerManager
///
/// Manages VI display layers for an applet.
/// Stubbed: requires VI service integration.
pub struct DisplayLayerManager {
    container: Option<Arc<Container>>,
    display_service: Option<Arc<IApplicationDisplayService>>,
    manager_display_service: Option<Arc<IManagerDisplayService>>,
    process: Option<Arc<std::sync::Mutex<KProcess>>>,
    managed_display_layers: BTreeSet<u64>,
    managed_display_recording_layers: BTreeSet<u64>,
    system_shared_buffer_id: u64,
    system_shared_layer_id: u64,
    applet_id: AppletId,
    buffer_sharing_enabled: bool,
    blending_enabled: bool,
    visible: bool,
}

impl Default for DisplayLayerManager {
    fn default() -> Self {
        Self::new()
    }
}

impl DisplayLayerManager {
    pub fn new() -> Self {
        Self {
            container: None,
            display_service: None,
            manager_display_service: None,
            process: None,
            managed_display_layers: BTreeSet::new(),
            managed_display_recording_layers: BTreeSet::new(),
            system_shared_buffer_id: 0,
            system_shared_layer_id: 0,
            applet_id: AppletId::default(),
            buffer_sharing_enabled: false,
            blending_enabled: false,
            visible: true,
        }
    }

    fn default_display_name() -> [u8; 0x40] {
        let mut display_name = [0u8; 0x40];
        display_name[..7].copy_from_slice(b"Default");
        display_name
    }

    pub fn initialize(
        &mut self,
        process: Arc<std::sync::Mutex<KProcess>>,
        applet_id: AppletId,
        mode: LibraryAppletMode,
    ) {
        self.container = vi::get_shared_container();
        self.display_service = self
            .container
            .as_ref()
            .map(|container| Arc::new(IApplicationDisplayService::new(Arc::clone(container))));
        self.manager_display_service = self
            .container
            .as_ref()
            .map(|container| Arc::new(IManagerDisplayService::new(Arc::clone(container))));
        self.process = Some(process);
        self.system_shared_buffer_id = 0;
        self.system_shared_layer_id = 0;
        self.applet_id = applet_id;
        self.buffer_sharing_enabled = false;
        self.blending_enabled = mode == LibraryAppletMode::PartialForeground
            || mode == LibraryAppletMode::PartialForegroundIndirectDisplay;
    }

    pub fn finalize(&mut self) {
        if let Some(manager_display_service) = self.manager_display_service.as_ref() {
            for &layer_id in &self.managed_display_layers {
                let _ = manager_display_service.destroy_managed_layer(layer_id);
            }
            for &layer_id in &self.managed_display_recording_layers {
                let _ = manager_display_service.destroy_managed_layer(layer_id);
            }
            if self.buffer_sharing_enabled {
                if let Some(process) = self.process.as_ref() {
                    manager_display_service.destroy_shared_layer_session(process);
                }
            }
        }
        self.managed_display_layers.clear();
        self.managed_display_recording_layers.clear();
        self.manager_display_service = None;
        self.display_service = None;
        self.container = None;
        self.process = None;
    }

    pub fn create_managed_display_layer(&mut self) -> Result<u64, ResultCode> {
        let Some(display_service) = self.display_service.as_ref() else {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        };
        let Some(manager_display_service) = self.manager_display_service.as_ref() else {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        };
        let Some(process) = self.process.as_ref() else {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        };

        let display_id = display_service.open_display(&Self::default_display_name())?;
        let layer_id =
            manager_display_service.create_managed_layer(0, display_id, process.lock().unwrap().get_process_id())?;
        manager_display_service.set_layer_visibility(self.visible, layer_id)?;
        self.managed_display_layers.insert(layer_id);
        Ok(layer_id)
    }

    pub fn create_managed_display_separable_layer(&mut self) -> Result<(u64, u64), ResultCode> {
        let layer_id = self.create_managed_display_layer()?;
        Ok((layer_id, 0))
    }

    pub fn is_system_buffer_sharing_enabled(&mut self) -> ResultCode {
        if self.buffer_sharing_enabled {
            return RESULT_SUCCESS;
        }

        if self.manager_display_service.is_none()
            || self.display_service.is_none()
            || self.process.is_none()
        {
            return vi_results::RESULT_OPERATION_FAILED;
        }

        if self.applet_id == AppletId::Application {
            return vi_results::RESULT_PERMISSION_DENIED;
        }

        let display_service = self.display_service.as_ref().unwrap();
        let manager_display_service = self.manager_display_service.as_ref().unwrap();
        let process = self.process.as_ref().unwrap();

        let Ok(display_id) = display_service.open_display(&Self::default_display_name())
        else {
            return vi_results::RESULT_OPERATION_FAILED;
        };

        let Ok((buffer_id, layer_id)) = manager_display_service.create_shared_layer_session(
            process,
            display_id,
            self.blending_enabled,
        ) else {
            return vi_results::RESULT_OPERATION_FAILED;
        };

        self.system_shared_buffer_id = buffer_id;
        self.system_shared_layer_id = layer_id;
        self.buffer_sharing_enabled = true;
        let _ = manager_display_service.set_layer_visibility(self.visible, self.system_shared_layer_id);
        RESULT_SUCCESS
    }

    pub fn get_system_shared_layer_handle(&mut self) -> Result<(u64, u64), ResultCode> {
        if self.is_system_buffer_sharing_enabled().is_error() {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }
        Ok((self.system_shared_buffer_id, self.system_shared_layer_id))
    }

    pub fn set_window_visibility(&mut self, visible: bool) {
        if self.visible == visible {
            return;
        }
        self.visible = visible;
        if let Some(manager_display_service) = self.manager_display_service.as_ref() {
            if self.system_shared_layer_id != 0 {
                let _ = manager_display_service
                    .set_layer_visibility(visible, self.system_shared_layer_id);
            }
            for &layer_id in &self.managed_display_layers {
                let _ = manager_display_service.set_layer_visibility(visible, layer_id);
            }
        }
    }

    pub fn get_window_visibility(&self) -> bool {
        self.visible
    }
}

impl Drop for DisplayLayerManager {
    fn drop(&mut self) {
        self.finalize();
    }
}

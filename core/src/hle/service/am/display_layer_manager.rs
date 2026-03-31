// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/display_layer_manager.h
//! Port of zuyu/src/core/hle/service/am/display_layer_manager.cpp

use std::collections::BTreeSet;
use std::sync::Arc;

use crate::hle::kernel::k_process::KProcess;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::vi::container::Container;
use crate::hle::service::vi::vi;
use crate::hle::service::vi::vi_results;

use super::am_types::{AppletId, LibraryAppletMode};

/// Port of DisplayLayerManager
///
/// Manages VI display layers for an applet.
/// Stubbed: requires VI service integration.
pub struct DisplayLayerManager {
    container: Option<Arc<Container>>,
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
        self.process = Some(process);
        self.system_shared_buffer_id = 0;
        self.system_shared_layer_id = 0;
        self.applet_id = applet_id;
        self.buffer_sharing_enabled = false;
        self.blending_enabled = mode == LibraryAppletMode::PartialForeground
            || mode == LibraryAppletMode::PartialForegroundIndirectDisplay;
    }

    pub fn finalize(&mut self) {
        if let Some(container) = self.container.as_ref() {
            for &layer_id in &self.managed_display_layers {
                let _ = container.destroy_managed_layer(layer_id);
            }
            for &layer_id in &self.managed_display_recording_layers {
                let _ = container.destroy_managed_layer(layer_id);
            }
        }
        self.managed_display_layers.clear();
        self.managed_display_recording_layers.clear();
        self.container = None;
        self.process = None;
    }

    pub fn create_managed_display_layer(&mut self) -> Result<u64, ResultCode> {
        let Some(container) = self.container.as_ref() else {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        };
        let Some(process) = self.process.as_ref() else {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        };

        let display_id = container.open_display(&Self::default_display_name())?;
        let layer_id =
            container.create_managed_layer(display_id, process.lock().unwrap().get_process_id())?;
        container.set_layer_visibility(layer_id, self.visible)?;
        self.managed_display_layers.insert(layer_id);
        Ok(layer_id)
    }

    pub fn create_managed_display_separable_layer(&mut self) -> Result<(u64, u64), ResultCode> {
        let layer_id = self.create_managed_display_layer()?;
        Ok((layer_id, 0))
    }

    pub fn is_system_buffer_sharing_enabled(&self) -> ResultCode {
        RESULT_SUCCESS
    }

    pub fn get_system_shared_layer_handle(&self) -> Result<(u64, u64), ResultCode> {
        Ok((self.system_shared_buffer_id, self.system_shared_layer_id))
    }

    pub fn set_window_visibility(&mut self, visible: bool) {
        if self.visible == visible {
            return;
        }
        self.visible = visible;
        if let Some(container) = self.container.as_ref() {
            if self.system_shared_layer_id != 0 {
                let _ = container.set_layer_visibility(self.system_shared_layer_id, visible);
            }
            for &layer_id in &self.managed_display_layers {
                let _ = container.set_layer_visibility(layer_id, visible);
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

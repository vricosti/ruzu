// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/display_layer_manager.h
//! Port of zuyu/src/core/hle/service/am/display_layer_manager.cpp

use std::collections::BTreeSet;

use super::am_types::{AppletId, LibraryAppletMode};

/// Port of DisplayLayerManager
///
/// Manages VI display layers for an applet.
/// Stubbed: requires VI service integration.
pub struct DisplayLayerManager {
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

    pub fn initialize(&mut self, _applet_id: AppletId, mode: LibraryAppletMode) {
        self.system_shared_buffer_id = 0;
        self.system_shared_layer_id = 0;
        self.applet_id = _applet_id;
        self.buffer_sharing_enabled = false;
        self.blending_enabled = mode == LibraryAppletMode::PartialForeground
            || mode == LibraryAppletMode::PartialForegroundIndirectDisplay;
    }

    pub fn finalize(&mut self) {
        // Upstream calls m_manager_display_service->DestroyManagedLayer() for each
        // managed display layer and recording layer, then destroys the shared layer
        // session if buffer sharing was enabled. This requires VI
        // IManagerDisplayService integration which is not yet available.
        // Once VI services are wired, this should iterate managed_display_layers
        // and managed_display_recording_layers calling DestroyManagedLayer on each,
        // then call DestroySharedLayerSession if buffer_sharing_enabled.
        self.managed_display_layers.clear();
        self.managed_display_recording_layers.clear();
    }

    pub fn set_window_visibility(&mut self, visible: bool) {
        if self.visible == visible {
            return;
        }
        self.visible = visible;
        // Upstream calls m_manager_display_service->SetLayerVisibility() for the
        // system_shared_layer_id (if nonzero) and for each managed_display_layer.
        // This requires VI IManagerDisplayService integration which is not yet
        // available. Once wired, iterate managed_display_layers and the shared
        // layer calling SetLayerVisibility(visible, layer_id) on each.
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

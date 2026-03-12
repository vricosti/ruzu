// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/container.h
//! Port of zuyu/src/core/hle/service/vi/container.cpp
//!
//! The Container manages displays, layers, and the binder/surface flinger
//! infrastructure. It is the central coordinator for the VI service.

use std::sync::Mutex;

use crate::hle::result::ResultCode;

use super::conductor::Conductor;
use super::display_list::DisplayList;
use super::layer_list::LayerList;
use super::vi_results;
use super::vi_types::DisplayName;

/// Container manages displays, layers, binder driver, and surface flinger.
///
/// In upstream C++, the Container owns:
/// - DisplayList (the set of displays)
/// - LayerList (the set of layers)
/// - IHOSBinderDriver (binder interface)
/// - SurfaceFlinger (compositor)
/// - SharedBufferManager
/// - Conductor (vsync manager)
///
/// Full construction depends on the system service manager and nvdrv
/// infrastructure.
pub struct Container {
    inner: Mutex<ContainerInner>,
}

struct ContainerInner {
    displays: DisplayList,
    layers: LayerList,
    conductor: Option<Conductor>,
    is_shut_down: bool,
}

impl Container {
    /// Create a new Container with the standard display set.
    ///
    /// In upstream, this:
    /// 1. Creates displays: Default, External, Edid, Internal, Null
    /// 2. Gets the binder driver from the service manager
    /// 3. Creates the SharedBufferManager
    /// 4. Creates the Conductor
    pub fn new() -> Self {
        let mut displays = DisplayList::default();
        let default_name = Self::make_display_name(b"Default");
        let external_name = Self::make_display_name(b"External");
        let edid_name = Self::make_display_name(b"Edid");
        let internal_name = Self::make_display_name(b"Internal");
        let null_name = Self::make_display_name(b"Null");

        displays.create_display(&default_name);
        displays.create_display(&external_name);
        displays.create_display(&edid_name);
        displays.create_display(&internal_name);
        displays.create_display(&null_name);

        let mut display_ids = Vec::new();
        displays.for_each_display(|d| {
            display_ids.push(d.get_id());
        });

        let conductor = Conductor::new(&display_ids);

        Self {
            inner: Mutex::new(ContainerInner {
                displays,
                layers: LayerList::default(),
                conductor: Some(conductor),
                is_shut_down: false,
            }),
        }
    }

    fn make_display_name(name: &[u8]) -> DisplayName {
        let mut display_name = [0u8; 0x40];
        let len = name.len().min(0x40);
        display_name[..len].copy_from_slice(&name[..len]);
        display_name
    }

    pub fn on_terminate(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.is_shut_down = true;
        // In upstream, this destroys all layers and removes all displays
        // from the surface flinger.
    }

    pub fn open_display(&self, display_name: &DisplayName) -> Result<u64, ResultCode> {
        let inner = self.inner.lock().unwrap();
        match inner.displays.get_display_by_name(display_name) {
            Some(display) => Ok(display.get_id()),
            None => Err(vi_results::RESULT_NOT_FOUND),
        }
    }

    pub fn close_display(&self, _display_id: u64) -> Result<(), ResultCode> {
        Ok(())
    }

    pub fn create_managed_layer(
        &self,
        display_id: u64,
        owner_aruid: u64,
    ) -> Result<u64, ResultCode> {
        let mut inner = self.inner.lock().unwrap();
        Self::create_layer_locked(&mut inner, display_id, owner_aruid)
    }

    pub fn destroy_managed_layer(&self, layer_id: u64) -> Result<(), ResultCode> {
        let mut inner = self.inner.lock().unwrap();
        // Try to close, if open, but don't fail if not.
        let _ = Self::close_layer_locked(&mut inner, layer_id);
        Self::destroy_layer_locked(&mut inner, layer_id)
    }

    pub fn open_layer(&self, layer_id: u64, aruid: u64) -> Result<i32, ResultCode> {
        let mut inner = self.inner.lock().unwrap();
        Self::open_layer_locked(&mut inner, layer_id, aruid)
    }

    pub fn close_layer(&self, layer_id: u64) -> Result<(), ResultCode> {
        let mut inner = self.inner.lock().unwrap();
        Self::close_layer_locked(&mut inner, layer_id)
    }

    pub fn create_stray_layer(&self, display_id: u64) -> Result<(i32, u64), ResultCode> {
        let mut inner = self.inner.lock().unwrap();
        let layer_id = Self::create_layer_locked(&mut inner, display_id, 0)?;
        let producer_binder_id = Self::open_layer_locked(&mut inner, layer_id, 0)?;
        Ok((producer_binder_id, layer_id))
    }

    pub fn destroy_stray_layer(&self, layer_id: u64) -> Result<(), ResultCode> {
        let mut inner = self.inner.lock().unwrap();
        Self::close_layer_locked(&mut inner, layer_id)?;
        Self::destroy_layer_locked(&mut inner, layer_id)
    }

    pub fn set_layer_visibility(&self, layer_id: u64, _visible: bool) -> Result<(), ResultCode> {
        let inner = self.inner.lock().unwrap();
        if inner.layers.get_layer_by_id(layer_id).is_none() {
            return Err(vi_results::RESULT_NOT_FOUND);
        }
        // In upstream, delegates to surface_flinger.SetLayerVisibility().
        Ok(())
    }

    pub fn set_layer_blending(&self, layer_id: u64, _enabled: bool) -> Result<(), ResultCode> {
        let inner = self.inner.lock().unwrap();
        if inner.layers.get_layer_by_id(layer_id).is_none() {
            return Err(vi_results::RESULT_NOT_FOUND);
        }
        // In upstream, delegates to surface_flinger.SetLayerBlending().
        Ok(())
    }

    pub fn link_vsync_event(&self, display_id: u64, event_handle: u64) {
        let mut inner = self.inner.lock().unwrap();
        if let Some(ref mut conductor) = inner.conductor {
            conductor.link_vsync_event(display_id, event_handle);
        }
    }

    pub fn unlink_vsync_event(&self, display_id: u64, event_handle: u64) {
        let mut inner = self.inner.lock().unwrap();
        if let Some(ref mut conductor) = inner.conductor {
            conductor.unlink_vsync_event(display_id, event_handle);
        }
    }

    fn create_layer_locked(
        inner: &mut ContainerInner,
        display_id: u64,
        owner_aruid: u64,
    ) -> Result<u64, ResultCode> {
        if inner.displays.get_display_by_id(display_id).is_none() {
            return Err(vi_results::RESULT_NOT_FOUND);
        }

        // In upstream, creates buffer queues via surface_flinger, then creates a layer.
        // For now, use placeholder binder IDs since full buffer queue creation
        // depends on the SurfaceFlinger infrastructure.
        let consumer_binder_id = 0;
        let producer_binder_id = 0;

        let layer = inner
            .layers
            .create_layer(owner_aruid, display_id, consumer_binder_id, producer_binder_id)
            .ok_or(vi_results::RESULT_NOT_FOUND)?;

        Ok(layer.get_id())
    }

    fn destroy_layer_locked(
        inner: &mut ContainerInner,
        layer_id: u64,
    ) -> Result<(), ResultCode> {
        if inner.layers.get_layer_by_id(layer_id).is_none() {
            return Err(vi_results::RESULT_NOT_FOUND);
        }
        inner.layers.destroy_layer(layer_id);
        Ok(())
    }

    fn open_layer_locked(
        inner: &mut ContainerInner,
        layer_id: u64,
        _aruid: u64,
    ) -> Result<i32, ResultCode> {
        if inner.is_shut_down {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }

        let layer = inner
            .layers
            .get_layer_by_id_mut(layer_id)
            .ok_or(vi_results::RESULT_NOT_FOUND)?;

        if layer.is_open() {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }

        let producer_binder_id = layer.get_producer_binder_id();
        layer.open();
        Ok(producer_binder_id)
    }

    fn close_layer_locked(
        inner: &mut ContainerInner,
        layer_id: u64,
    ) -> Result<(), ResultCode> {
        let layer = inner
            .layers
            .get_layer_by_id_mut(layer_id)
            .ok_or(vi_results::RESULT_NOT_FOUND)?;

        if !layer.is_open() {
            return Err(vi_results::RESULT_OPERATION_FAILED);
        }

        layer.close();
        Ok(())
    }
}

impl Drop for Container {
    fn drop(&mut self) {
        self.on_terminate();
    }
}

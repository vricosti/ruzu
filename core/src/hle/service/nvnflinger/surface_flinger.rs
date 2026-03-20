// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/surface_flinger.h
//! Port of zuyu/src/core/hle/service/nvnflinger/surface_flinger.cpp
//!
//! SurfaceFlinger manages displays, layers, and buffer queues.
//! It is the compositor that collects layers and presents them.
//! Full composition logic depends on the hardware composer and GPU infrastructure.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use super::buffer_queue_core::BufferQueueCore;
use super::buffer_queue_consumer::BufferQueueConsumer;
use super::buffer_queue_producer::BufferQueueProducer;
use super::display::{Display, Layer, LayerStack};
use super::hos_binder_driver_server::HosBinderDriverServer;
use super::hwc_layer::LayerBlending;

/// SurfaceFlinger manages the composition of layers onto displays.
pub struct SurfaceFlinger {
    inner: Mutex<SurfaceFlingerInner>,
    server: Arc<HosBinderDriverServer>,
}

struct SurfaceFlingerInner {
    displays: HashMap<u64, Display>,
}

impl SurfaceFlinger {
    pub fn new(server: Arc<HosBinderDriverServer>) -> Arc<Self> {
        Arc::new(Self {
            inner: Mutex::new(SurfaceFlingerInner {
                displays: HashMap::new(),
            }),
            server,
        })
    }

    pub fn add_display(&self, display_id: u64) {
        let mut inner = self.inner.lock().unwrap();
        inner.displays.insert(display_id, Display::new(display_id));
    }

    pub fn remove_display(&self, display_id: u64) {
        let mut inner = self.inner.lock().unwrap();
        inner.displays.remove(&display_id);
    }

    /// Create a buffer queue pair and register both in the binder driver server.
    /// Returns (consumer_binder_id, producer_binder_id).
    pub fn create_buffer_queue(&self) -> (i32, i32) {
        let core = BufferQueueCore::new();
        let _consumer = Arc::new(BufferQueueConsumer::new(Arc::clone(&core)));
        let _producer = Arc::new(BufferQueueProducer::new(Arc::clone(&core)));

        // Register stub binders in the server. In a full port, BufferQueueProducer
        // and BufferQueueConsumer would implement IBinder directly. For now we
        // register stub binders that handle TransactParcel calls gracefully.
        let consumer_binder: Arc<dyn super::binder::IBinder> = Arc::new(StubBinder::new("consumer"));
        let producer_binder: Arc<dyn super::binder::IBinder> = Arc::new(StubBinder::new("producer"));

        let consumer_id = self.server.register_binder(consumer_binder);
        let producer_id = self.server.register_binder(producer_binder);

        log::info!(
            "SurfaceFlinger::create_buffer_queue consumer_id={}, producer_id={}",
            consumer_id, producer_id
        );

        (consumer_id, producer_id)
    }

    pub fn destroy_buffer_queue(&self, _consumer_id: i32, _producer_id: i32) {
        self.server.unregister_binder(_consumer_id);
        self.server.unregister_binder(_producer_id);
    }

    pub fn create_layer(&self, _consumer_binder_id: i32) {
        // In upstream, creates a Layer from the consumer's BufferItemConsumer
        // and adds it to the display. Full implementation depends on
        // BufferItemConsumer creation infrastructure.
        log::debug!("SurfaceFlinger::create_layer consumer_id={}", _consumer_binder_id);
    }

    pub fn destroy_layer(&self, consumer_binder_id: i32) {
        let mut inner = self.inner.lock().unwrap();
        for (_, display) in inner.displays.iter_mut() {
            display.stack.layers.retain(|l| l.consumer_id != consumer_binder_id);
        }
    }

    pub fn add_layer_to_display_stack(&self, display_id: u64, consumer_binder_id: i32) {
        log::debug!(
            "SurfaceFlinger::add_layer_to_display_stack display={} consumer={}",
            display_id,
            consumer_binder_id
        );
    }

    pub fn remove_layer_from_display_stack(&self, display_id: u64, consumer_binder_id: i32) {
        let mut inner = self.inner.lock().unwrap();
        if let Some(display) = inner.displays.get_mut(&display_id) {
            display
                .stack
                .layers
                .retain(|l| l.consumer_id != consumer_binder_id);
        }
    }

    pub fn set_layer_visibility(&self, consumer_binder_id: i32, visible: bool) {
        let inner = self.inner.lock().unwrap();
        for (_, display) in inner.displays.iter() {
            if let Some(layer) = display.stack.find_layer(consumer_binder_id) {
                // Layer visibility is stored in the Layer struct but requires
                // interior mutability. In a full port, Layer would use Mutex.
                log::debug!(
                    "SurfaceFlinger: set visibility consumer={} visible={}",
                    consumer_binder_id,
                    visible
                );
            }
        }
    }

    pub fn set_layer_blending(&self, consumer_binder_id: i32, blending: LayerBlending) {
        log::debug!(
            "SurfaceFlinger: set blending consumer={} blending={:?}",
            consumer_binder_id,
            blending
        );
    }

    /// Compose all layers on a display. Returns true if composition occurred.
    pub fn compose_display(
        &self,
        out_swap_interval: &mut i32,
        out_compose_speed_scale: &mut f32,
        display_id: u64,
    ) -> bool {
        let inner = self.inner.lock().unwrap();
        let _display = match inner.displays.get(&display_id) {
            Some(d) => d,
            None => return false,
        };

        // Composition logic: acquire buffers from each layer, compose via hardware
        // composer, and present. Full implementation depends on GPU infrastructure.
        *out_swap_interval = 1;
        *out_compose_speed_scale = 1.0;

        true
    }
}

/// Stub IBinder implementation for buffer queue endpoints.
/// Returns empty parcel responses for all transactions. This allows the game
/// to proceed through binder setup without crashing, even though no real
/// buffer exchange occurs yet.
struct StubBinder {
    name: String,
}

impl StubBinder {
    fn new(name: &str) -> Self {
        Self { name: name.to_string() }
    }
}

impl super::binder::IBinder for StubBinder {
    fn transact(&self, code: u32, _parcel_data: &[u8], parcel_reply: &mut [u8], _flags: u32) {
        log::warn!("StubBinder({}): transact code={} (STUBBED)", self.name, code);
        // Write a minimal valid empty parcel response so the caller doesn't read garbage.
        // Parcel format: header (16 bytes) = data_size, data_offset, objects_size, objects_offset
        if parcel_reply.len() >= 16 {
            // data_size = 4 (one u32 status = 0), data_offset = 16, objects_size = 0, objects_offset = 20
            let data_size: u32 = 4;
            let data_offset: u32 = 16;
            let objects_size: u32 = 0;
            let objects_offset: u32 = data_offset + data_size;

            parcel_reply[0..4].copy_from_slice(&data_size.to_le_bytes());
            parcel_reply[4..8].copy_from_slice(&data_offset.to_le_bytes());
            parcel_reply[8..12].copy_from_slice(&objects_size.to_le_bytes());
            parcel_reply[12..16].copy_from_slice(&objects_offset.to_le_bytes());

            // Write a status word = 0 (success)
            if parcel_reply.len() >= 20 {
                parcel_reply[16..20].copy_from_slice(&0u32.to_le_bytes());
            }
        }
    }

    fn get_native_handle(&self, _type_id: u32) -> Option<u32> {
        log::warn!("StubBinder({}): get_native_handle (STUBBED)", self.name);
        None
    }
}

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

use super::buffer_item_consumer::BufferItemConsumer;
use super::buffer_queue_consumer::BufferQueueConsumer;
use super::buffer_queue_core::BufferQueueCore;
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
    layers: LayerStack,
    consumers: HashMap<i32, Arc<BufferQueueConsumer>>,
}

impl SurfaceFlinger {
    pub fn new(server: Arc<HosBinderDriverServer>) -> Arc<Self> {
        Arc::new(Self {
            inner: Mutex::new(SurfaceFlingerInner {
                displays: HashMap::new(),
                layers: LayerStack::new(),
                consumers: HashMap::new(),
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
        let consumer = Arc::new(BufferQueueConsumer::new(Arc::clone(&core)));
        let consumer_binder: Arc<dyn super::binder::IBinder> = consumer.clone();
        let producer_binder: Arc<dyn super::binder::IBinder> =
            Arc::new(BufferQueueProducer::new(Arc::clone(&core)));

        let consumer_id = self.server.register_binder(consumer_binder);
        let producer_id = self.server.register_binder(producer_binder);
        self.inner
            .lock()
            .unwrap()
            .consumers
            .insert(consumer_id, consumer);

        log::info!(
            "SurfaceFlinger::create_buffer_queue consumer_id={}, producer_id={}",
            consumer_id,
            producer_id
        );

        (consumer_id, producer_id)
    }

    pub fn destroy_buffer_queue(&self, _consumer_id: i32, _producer_id: i32) {
        self.inner.lock().unwrap().consumers.remove(&_consumer_id);
        self.server.unregister_binder(_consumer_id);
        self.server.unregister_binder(_producer_id);
    }

    pub fn create_layer(&self, consumer_binder_id: i32) {
        let consumer = {
            self.inner
                .lock()
                .unwrap()
                .consumers
                .get(&consumer_binder_id)
                .cloned()
        };
        let Some(consumer) = consumer else {
            return;
        };

        let buffer_item_consumer = Arc::new(BufferItemConsumer::new(consumer));
        let _ = buffer_item_consumer.connect(false);
        self.inner
            .lock()
            .unwrap()
            .layers
            .layers
            .push(Arc::new(Layer::new(
                buffer_item_consumer,
                consumer_binder_id,
            )));
    }

    pub fn destroy_layer(&self, consumer_binder_id: i32) {
        let mut inner = self.inner.lock().unwrap();
        inner
            .layers
            .layers
            .retain(|l| l.consumer_id != consumer_binder_id);
        for (_, display) in inner.displays.iter_mut() {
            display
                .stack
                .layers
                .retain(|l| l.consumer_id != consumer_binder_id);
        }
    }

    pub fn add_layer_to_display_stack(&self, display_id: u64, consumer_binder_id: i32) {
        let mut inner = self.inner.lock().unwrap();
        let layer = inner.layers.find_layer(consumer_binder_id);
        let Some(display) = inner.displays.get_mut(&display_id) else {
            return;
        };
        let Some(layer) = layer else {
            return;
        };
        display.stack.layers.push(layer);
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

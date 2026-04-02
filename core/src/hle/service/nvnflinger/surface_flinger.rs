// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/surface_flinger.h
//! Port of zuyu/src/core/hle/service/nvnflinger/surface_flinger.cpp

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::nvdrv::Module;
use crate::hle::service::nvdrv::nvdrv_interface::NvdrvService;
use crate::hle::service::sm::sm::ServiceManager;

use super::binder::IBinder;
use super::buffer_item_consumer::BufferItemConsumer;
use super::buffer_queue_consumer::BufferQueueConsumer;
use super::buffer_queue_core::BufferQueueCore;
use super::buffer_queue_producer::BufferQueueProducer;
use super::display::{Display, Layer, LayerStack};
use super::hardware_composer::HardwareComposer;
use super::hos_binder_driver_server::HosBinderDriverServer;
use super::hwc_layer::LayerBlending;

pub struct SurfaceFlinger {
    system: SystemRef,
    server: Arc<HosBinderDriverServer>,
    inner: Mutex<SurfaceFlingerInner>,
    nvdrv: Arc<Module>,
    disp_fd: i32,
    composer: Mutex<HardwareComposer>,
}

struct SurfaceFlingerInner {
    displays: Vec<Display>,
    layers: LayerStack,
    consumers: HashMap<i32, Arc<BufferQueueConsumer>>,
}

impl SurfaceFlinger {
    pub fn new(system: SystemRef, server: Arc<HosBinderDriverServer>) -> Arc<Self> {
        let service_manager = system
            .get()
            .service_manager()
            .expect("SurfaceFlinger requires ServiceManager");
        let nvdrv_service = ServiceManager::get_service_blocking(&service_manager, "nvdrv:s");
        let nvdrv = nvdrv_service
            .as_any()
            .downcast_ref::<NvdrvService>()
            .expect("nvdrv:s handler must be NvdrvService")
            .get_module();
        let disp_fd = nvdrv.open("/dev/nvdisp_disp0", SessionId { id: 0 });

        Arc::new(Self {
            system,
            server,
            inner: Mutex::new(SurfaceFlingerInner {
                displays: Vec::new(),
                layers: LayerStack::new(),
                consumers: HashMap::new(),
            }),
            nvdrv,
            disp_fd,
            composer: Mutex::new(HardwareComposer::new()),
        })
    }

    pub fn add_display(&self, display_id: u64) {
        self.inner.lock().unwrap().displays.push(Display::new(display_id));
    }

    pub fn remove_display(&self, display_id: u64) {
        self.inner
            .lock()
            .unwrap()
            .displays
            .retain(|display| display.id != display_id);
    }

    pub fn compose_display(
        &self,
        out_swap_interval: &mut i32,
        out_compose_speed_scale: &mut f32,
        display_id: u64,
    ) -> bool {
        let inner = self.inner.lock().unwrap();
        let Some(display) = Self::find_display(&inner.displays, display_id) else {
            return false;
        };
        if !display.stack.has_layers() {
            return false;
        }

        let device = self
            .nvdrv
            .get_disp_device(self.disp_fd)
            .expect("nvdisp_disp0 device must be open while SurfaceFlinger lives");
        *out_swap_interval = self
            .composer
            .lock()
            .unwrap()
            .compose_locked(out_compose_speed_scale, display, &device) as i32;
        true
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
            .push(Arc::new(Mutex::new(Layer::new(
                buffer_item_consumer,
                consumer_binder_id,
            ))));
    }

    pub fn destroy_layer(&self, consumer_binder_id: i32) {
        self.inner
            .lock()
            .unwrap()
            .layers
            .layers
            .retain(|layer| layer.lock().unwrap().consumer_id != consumer_binder_id);
    }

    pub fn add_layer_to_display_stack(&self, display_id: u64, consumer_binder_id: i32) {
        let mut inner = self.inner.lock().unwrap();
        let layer = Self::find_layer(&inner.layers, consumer_binder_id);
        let Some(display) = Self::find_display_mut(&mut inner.displays, display_id) else {
            return;
        };
        let Some(layer) = layer else {
            return;
        };
        display.stack.layers.push(layer);
    }

    pub fn remove_layer_from_display_stack(&self, display_id: u64, consumer_binder_id: i32) {
        let mut inner = self.inner.lock().unwrap();
        let Some(display) = Self::find_display_mut(&mut inner.displays, display_id) else {
            return;
        };

        self.composer
            .lock()
            .unwrap()
            .remove_layer_locked(display, consumer_binder_id);
        display
            .stack
            .layers
            .retain(|layer| layer.lock().unwrap().consumer_id != consumer_binder_id);
    }

    pub fn set_layer_visibility(&self, consumer_binder_id: i32, visible: bool) {
        if let Some(layer) = Self::find_layer(&self.inner.lock().unwrap().layers, consumer_binder_id) {
            layer.lock().unwrap().visible = visible;
        }
    }

    pub fn set_layer_blending(&self, consumer_binder_id: i32, blending: LayerBlending) {
        if let Some(layer) = Self::find_layer(&self.inner.lock().unwrap().layers, consumer_binder_id) {
            layer.lock().unwrap().blending = blending;
        }
    }

    pub fn create_buffer_queue(&self) -> (i32, i32) {
        let core = BufferQueueCore::new();
        let consumer = Arc::new(BufferQueueConsumer::new(Arc::clone(&core)));
        let producer_binder: Arc<dyn IBinder> = Arc::new(BufferQueueProducer::new(Arc::clone(&core)));
        let consumer_binder: Arc<dyn IBinder> = consumer.clone();

        let consumer_binder_id = self.server.register_binder(consumer_binder);
        let producer_binder_id = self.server.register_binder(producer_binder);
        self.inner
            .lock()
            .unwrap()
            .consumers
            .insert(consumer_binder_id, consumer);
        (consumer_binder_id, producer_binder_id)
    }

    pub fn destroy_buffer_queue(&self, consumer_binder_id: i32, producer_binder_id: i32) {
        self.inner
            .lock()
            .unwrap()
            .consumers
            .remove(&consumer_binder_id);
        self.server.unregister_binder(producer_binder_id);
        self.server.unregister_binder(consumer_binder_id);
    }

    fn find_display(displays: &[Display], display_id: u64) -> Option<&Display> {
        displays.iter().find(|display| display.id == display_id)
    }

    fn find_display_mut(displays: &mut [Display], display_id: u64) -> Option<&mut Display> {
        displays.iter_mut().find(|display| display.id == display_id)
    }

    fn find_layer(layers: &LayerStack, consumer_binder_id: i32) -> Option<Arc<Mutex<Layer>>> {
        layers.find_layer(consumer_binder_id)
    }
}

impl Drop for SurfaceFlinger {
    fn drop(&mut self) {
        let _ = self.nvdrv.close(self.disp_fd);
        let _ = self.system;
    }
}

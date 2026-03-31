// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/display.h

use std::sync::Arc;

use super::buffer_item_consumer::BufferItemConsumer;
use super::hwc_layer::LayerBlending;

/// A single layer within the display stack.
pub struct Layer {
    pub buffer_item_consumer: Arc<BufferItemConsumer>,
    pub consumer_id: i32,
    pub blending: LayerBlending,
    pub visible: bool,
}

impl Layer {
    pub fn new(buffer_item_consumer: Arc<BufferItemConsumer>, consumer_id: i32) -> Self {
        Self {
            buffer_item_consumer,
            consumer_id,
            blending: LayerBlending::None,
            visible: true,
        }
    }
}

impl Drop for Layer {
    fn drop(&mut self) {
        self.buffer_item_consumer.abandon();
    }
}

/// A stack of layers, searchable by consumer_id.
pub struct LayerStack {
    pub layers: Vec<Arc<Layer>>,
}

impl LayerStack {
    pub fn new() -> Self {
        Self { layers: Vec::new() }
    }

    pub fn find_layer(&self, consumer_id: i32) -> Option<Arc<Layer>> {
        for layer in &self.layers {
            if layer.consumer_id == consumer_id {
                return Some(Arc::clone(layer));
            }
        }
        None
    }

    pub fn has_layers(&self) -> bool {
        !self.layers.is_empty()
    }
}

impl Default for LayerStack {
    fn default() -> Self {
        Self::new()
    }
}

/// A display with an ID and a layer stack.
pub struct Display {
    pub id: u64,
    pub stack: LayerStack,
}

impl Display {
    pub fn new(id: u64) -> Self {
        Self {
            id,
            stack: LayerStack::new(),
        }
    }
}

// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/layer_list.h

use super::layer::Layer;

pub struct LayerList {
    layers: [Layer; 8],
    next_id: u64,
}

impl Default for LayerList {
    fn default() -> Self {
        Self {
            layers: Default::default(),
            next_id: 0,
        }
    }
}

impl LayerList {
    pub fn create_layer(
        &mut self,
        owner_aruid: u64,
        display_id: u64,
        consumer_binder_id: i32,
        producer_binder_id: i32,
    ) -> Option<&mut Layer> {
        let layer = Self::get_free_layer(&mut self.layers)?;
        self.next_id += 1;
        layer.initialize(
            self.next_id,
            owner_aruid,
            display_id,
            consumer_binder_id,
            producer_binder_id,
        );
        Some(layer)
    }

    pub fn destroy_layer(&mut self, layer_id: u64) -> bool {
        if let Some(layer) = self.get_layer_by_id_mut(layer_id) {
            layer.finalize();
            true
        } else {
            false
        }
    }

    pub fn get_layer_by_id(&self, layer_id: u64) -> Option<&Layer> {
        self.layers
            .iter()
            .find(|l| l.is_initialized() && l.get_id() == layer_id)
    }

    pub fn get_layer_by_id_mut(&mut self, layer_id: u64) -> Option<&mut Layer> {
        self.layers
            .iter_mut()
            .find(|l| l.is_initialized() && l.get_id() == layer_id)
    }

    pub fn for_each_layer<F: FnMut(&Layer)>(&self, mut cb: F) {
        for layer in &self.layers {
            if layer.is_initialized() {
                cb(layer);
            }
        }
    }

    fn get_free_layer(layers: &mut [Layer; 8]) -> Option<&mut Layer> {
        layers.iter_mut().find(|layer| !layer.is_initialized())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_layer_uses_first_free_slot_without_advancing_id_when_full() {
        let mut layers = LayerList::default();
        for index in 0..8 {
            let layer = layers
                .create_layer(index, 10, index as i32, index as i32 + 20)
                .unwrap();
            assert_eq!(layer.get_id(), index + 1);
        }

        assert!(layers.create_layer(8, 10, 8, 28).is_none());
        assert!(layers.destroy_layer(4));
        let replacement = layers.create_layer(9, 10, 9, 29).unwrap();
        assert_eq!(replacement.get_id(), 9);
        assert_eq!(replacement.get_owner_aruid(), 9);
    }
}

// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/hardware_composer.h
//! Port of zuyu/src/core/hle/service/nvnflinger/hardware_composer.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use super::buffer_item::BufferItem;
use super::display::{Display, Layer};
use super::hwc_layer::HwcLayer;
use super::ui::fence::Fence;
use crate::hle::service::nvdrv::devices::nvdisp_disp0::NvDispDisp0;

type ConsumerId = i32;
type ReleaseFrameNumber = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CacheStatus {
    NoBufferAvailable,
    BufferAcquired,
    CachedBufferReused,
}

#[derive(Default)]
struct Framebuffer {
    item: BufferItem,
    release_frame_number: ReleaseFrameNumber,
    is_acquired: bool,
}

fn normalize_swap_interval(mut out_speed_scale: Option<&mut f32>, mut swap_interval: i32) -> i32 {
    if swap_interval <= 0 {
        if let Some(out_speed_scale) = out_speed_scale.as_deref_mut() {
            *out_speed_scale = 2.0 * (1 - swap_interval) as f32;
        }
        swap_interval = 1;
    }

    if swap_interval >= 5 {
        if let Some(out_speed_scale) = out_speed_scale.as_deref_mut() {
            *out_speed_scale = swap_interval as f32 / 100.0;
        }
        swap_interval = 1;
    }

    swap_interval
}

pub struct HardwareComposer {
    frame_number: u64,
    framebuffers: BTreeMap<ConsumerId, Framebuffer>,
}

impl HardwareComposer {
    pub fn new() -> Self {
        Self {
            frame_number: 0,
            framebuffers: BTreeMap::new(),
        }
    }

    pub fn compose_locked(
        &mut self,
        out_speed_scale: &mut f32,
        display: &Display,
        nvdisp: &NvDispDisp0,
    ) -> u32 {
        let mut composition_stack = Vec::with_capacity(display.stack.layers.len());
        *out_speed_scale = 1.0;

        let mut swap_interval: Option<i32> = None;
        let mut has_acquired_buffer = false;

        for layer in &display.stack.layers {
            let consumer_id = layer.lock().unwrap().consumer_id;
            let result = self.cache_framebuffer_locked(layer, consumer_id);

            if result == CacheStatus::NoBufferAvailable {
                continue;
            }

            if result == CacheStatus::BufferAcquired {
                has_acquired_buffer = true;
            }

            let Some(framebuffer) = self.framebuffers.get(&consumer_id) else {
                continue;
            };
            let item = &framebuffer.item;
            let Some(graphic_buffer) = item.graphic_buffer.as_ref() else {
                continue;
            };

            let layer_guard = layer.lock().unwrap();
            if layer_guard.visible {
                composition_stack.push(HwcLayer {
                    buffer_handle: graphic_buffer.get_buffer_id(),
                    offset: graphic_buffer.get_offset(),
                    format: graphic_buffer.get_external_format(),
                    width: graphic_buffer.get_width(),
                    height: graphic_buffer.get_height(),
                    stride: graphic_buffer.get_stride(),
                    z_index: 0,
                    blending: layer_guard.blending,
                    transform:
                        super::buffer_transform_flags::BufferTransformFlags::from_bits_retain(
                            item.transform.bits(),
                        ),
                    crop_rect: item.crop,
                    acquire_fence: item.fence,
                });
            }

            let item_swap_interval =
                normalize_swap_interval(Some(out_speed_scale), item.swap_interval);
            swap_interval = Some(match swap_interval {
                Some(current) => current.min(item_swap_interval),
                None => item_swap_interval,
            });
        }

        if has_acquired_buffer {
            composition_stack.sort_by_key(|layer| layer.z_index);
            nvdisp.composite(&composition_stack);
        }

        let frame_advance = swap_interval.unwrap_or(1) as u32;
        self.frame_number += frame_advance as u64;

        for (layer_id, framebuffer) in &mut self.framebuffers {
            if framebuffer.release_frame_number > self.frame_number || !framebuffer.is_acquired {
                continue;
            }

            let Some(layer) = display.stack.find_layer(*layer_id) else {
                continue;
            };

            layer
                .lock()
                .unwrap()
                .buffer_item_consumer
                .release_buffer(&framebuffer.item, &Fence::no_fence());
            framebuffer.is_acquired = false;
        }

        frame_advance
    }

    pub fn remove_layer_locked(&mut self, display: &Display, consumer_id: ConsumerId) {
        let Some(framebuffer) = self.framebuffers.remove(&consumer_id) else {
            return;
        };

        if !framebuffer.is_acquired {
            return;
        }

        if let Some(layer) = display.stack.find_layer(consumer_id) {
            layer
                .lock()
                .unwrap()
                .buffer_item_consumer
                .release_buffer(&framebuffer.item, &Fence::no_fence());
        }
    }

    fn try_acquire_framebuffer_locked(
        layer: &Arc<Mutex<Layer>>,
        framebuffer: &mut Framebuffer,
    ) -> bool {
        let status = layer.lock().unwrap().buffer_item_consumer.acquire_buffer(
            &mut framebuffer.item,
            0,
            false,
        );
        if status != super::status::Status::NoError {
            return false;
        }

        framebuffer.release_frame_number =
            normalize_swap_interval(None, framebuffer.item.swap_interval) as u64;
        framebuffer.is_acquired = true;
        true
    }

    fn cache_framebuffer_locked(
        &mut self,
        layer: &Arc<Mutex<Layer>>,
        consumer_id: ConsumerId,
    ) -> CacheStatus {
        if let Some(framebuffer) = self.framebuffers.get_mut(&consumer_id) {
            if framebuffer.is_acquired {
                return CacheStatus::CachedBufferReused;
            }

            if Self::try_acquire_framebuffer_locked(layer, framebuffer) {
                return CacheStatus::BufferAcquired;
            }

            return CacheStatus::CachedBufferReused;
        }

        let mut framebuffer = Framebuffer::default();
        if Self::try_acquire_framebuffer_locked(layer, &mut framebuffer) {
            self.framebuffers.insert(consumer_id, framebuffer);
            return CacheStatus::BufferAcquired;
        }

        CacheStatus::NoBufferAvailable
    }
}

impl Default for HardwareComposer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::normalize_swap_interval;

    #[test]
    fn normalize_swap_interval_turns_nonpositive_into_speed_scale() {
        let mut speed_scale = 1.0;
        let interval = normalize_swap_interval(Some(&mut speed_scale), 0);

        assert_eq!(interval, 1);
        assert_eq!(speed_scale, 2.0);
    }

    #[test]
    fn normalize_swap_interval_turns_large_interval_into_precise_speed_scale() {
        let mut speed_scale = 1.0;
        let interval = normalize_swap_interval(Some(&mut speed_scale), 50);

        assert_eq!(interval, 1);
        assert_eq!(speed_scale, 0.5);
    }
}

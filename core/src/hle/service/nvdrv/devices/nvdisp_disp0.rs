// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvdisp_disp0.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvdisp_disp0.cpp

use crate::core::SystemRef;
use crate::gpu_core::{
    BlendMode as GpuBlendMode, BufferTransformFlags as GpuBufferTransformFlags,
    FramebufferConfig as GpuFramebufferConfig, RectI as GpuRectI,
};
use crate::hle::service::nvdrv::core::nvmap::NvMap;
use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::nvdata::{DeviceFD, Ioctl, NvResult};
use crate::hle::service::nvnflinger::hwc_layer::{HwcLayer, LayerBlending};

/// nvdisp_disp0 device: display compositor.
pub struct NvDispDisp0 {
    system: SystemRef,
    nvmap: *const NvMap,
}

unsafe impl Send for NvDispDisp0 {}
unsafe impl Sync for NvDispDisp0 {}

impl NvDispDisp0 {
    pub fn new(system: SystemRef, nvmap: &NvMap) -> Self {
        Self {
            system,
            nvmap: nvmap as *const _,
        }
    }

    fn nvmap(&self) -> &NvMap {
        unsafe { &*self.nvmap }
    }

    fn convert_blending(blending: LayerBlending) -> GpuBlendMode {
        match blending {
            LayerBlending::None => GpuBlendMode::Opaque,
            LayerBlending::Premultiplied => GpuBlendMode::Premultiplied,
            LayerBlending::Coverage => GpuBlendMode::Coverage,
        }
    }

    /// Performs a screen flip, compositing each buffer.
    pub fn composite(&self, sorted_layers: &[HwcLayer]) {
        let mut output_layers = Vec::with_capacity(sorted_layers.len());
        let mut output_fences = Vec::new();

        for layer in sorted_layers {
            output_layers.push(GpuFramebufferConfig {
                address: self.nvmap().get_handle_address(layer.buffer_handle),
                offset: layer.offset,
                width: layer.width,
                height: layer.height,
                stride: layer.stride,
                pixel_format: layer.format as u32,
                transform_flags: GpuBufferTransformFlags(layer.transform.bits()),
                crop_rect: GpuRectI {
                    left: layer.crop_rect.left,
                    top: layer.crop_rect.top,
                    right: layer.crop_rect.right,
                    bottom: layer.crop_rect.bottom,
                },
                blending: Self::convert_blending(layer.blending),
            });

            output_fences.extend(
                layer.acquire_fence.fences[..layer.acquire_fence.num_fences as usize]
                    .iter()
                    .copied(),
            );
        }

        let system = self.system.get();
        system
            .gpu_core()
            .expect("GPU core must exist before nvdisp composite")
            .request_composite(output_layers, output_fences);

        if let Some(stats) = system.get_perf_stats() {
            stats.end_system_frame();
            stats.begin_system_frame();
        }
    }
}

impl NvDevice for NvDispDisp0 {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn ioctl1(&self, _fd: DeviceFD, command: Ioctl, _input: &[u8], _output: &mut [u8]) -> NvResult {
        log::error!("Unimplemented ioctl={:08X}", command.raw);
        NvResult::NotImplemented
    }

    fn ioctl2(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        _input: &[u8],
        _inline_input: &[u8],
        _output: &mut [u8],
    ) -> NvResult {
        log::error!("Unimplemented ioctl={:08X}", command.raw);
        NvResult::NotImplemented
    }

    fn ioctl3(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        _input: &[u8],
        _output: &mut [u8],
        _inline_output: &mut [u8],
    ) -> NvResult {
        log::error!("Unimplemented ioctl={:08X}", command.raw);
        NvResult::NotImplemented
    }

    fn on_open(&self, _session_id: SessionId, _fd: DeviceFD) {}
    fn on_close(&self, _fd: DeviceFD) {}

    fn query_event(
        &self,
        event_id: u32,
    ) -> Option<
        std::sync::Arc<std::sync::Mutex<crate::hle::kernel::k_readable_event::KReadableEvent>>,
    > {
        log::error!("Unknown DISP Event {}", event_id);
        None
    }
}

#[cfg(test)]
mod tests {
    use super::NvDispDisp0;
    use crate::gpu_core::BlendMode;
    use crate::hle::service::nvnflinger::hwc_layer::LayerBlending;

    #[test]
    fn convert_blending_matches_upstream_values() {
        assert_eq!(
            NvDispDisp0::convert_blending(LayerBlending::None),
            BlendMode::Opaque
        );
        assert_eq!(
            NvDispDisp0::convert_blending(LayerBlending::Premultiplied),
            BlendMode::Premultiplied
        );
        assert_eq!(
            NvDispDisp0::convert_blending(LayerBlending::Coverage),
            BlendMode::Coverage
        );
    }
}

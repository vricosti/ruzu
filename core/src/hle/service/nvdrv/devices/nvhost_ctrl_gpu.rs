// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_ctrl_gpu.h
//! Port of zuyu/src/core/hle/service/nvdrv/devices/nvhost_ctrl_gpu.cpp

use std::sync::{Arc, Mutex};

use crate::hle::kernel::k_readable_event::KReadableEvent;
use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::devices::nvdevice::NvDevice;
use crate::hle::service::nvdrv::devices::nvmap::{read_struct, write_struct};
use crate::hle::service::nvdrv::nvdrv::EventInterface;
use crate::hle::service::nvdrv::nvdata::{DeviceFD, Ioctl, NvResult};

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlGpuCharacteristics {
    pub arch: u32,
    pub r#impl: u32,
    pub rev: u32,
    pub num_gpc: u32,
    pub l2_cache_size: u64,
    pub on_board_video_memory_size: u64,
    pub num_tpc_per_gpc: u32,
    pub bus_type: u32,
    pub big_page_size: u32,
    pub compression_page_size: u32,
    pub pde_coverage_bit_count: u32,
    pub available_big_page_sizes: u32,
    pub gpc_mask: u32,
    pub sm_arch_sm_version: u32,
    pub sm_arch_spa_version: u32,
    pub sm_arch_warp_count: u32,
    pub gpu_va_bit_count: u32,
    pub reserved: u32,
    pub flags: u64,
    pub twod_class: u32,
    pub threed_class: u32,
    pub compute_class: u32,
    pub gpfifo_class: u32,
    pub inline_to_memory_class: u32,
    pub dma_copy_class: u32,
    pub max_fbps_count: u32,
    pub fbp_en_mask: u32,
    pub max_ltc_per_fbp: u32,
    pub max_lts_per_ltc: u32,
    pub max_tex_per_tpc: u32,
    pub max_gpc_count: u32,
    pub rop_l2_en_mask_0: u32,
    pub rop_l2_en_mask_1: u32,
    pub chipname: u64,
    pub gr_compbit_store_base_hw: u64,
}
const _: () = assert!(std::mem::size_of::<IoctlGpuCharacteristics>() == 160);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlCharacteristics {
    pub gpu_characteristics_buf_size: u64,
    pub gpu_characteristics_buf_addr: u64,
    pub gc: IoctlGpuCharacteristics,
}
const _: () = assert!(
    std::mem::size_of::<IoctlCharacteristics>()
        == 16 + std::mem::size_of::<IoctlGpuCharacteristics>()
);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlGpuGetTpcMasksArgs {
    pub mask_buffer_size: u32,
    pub _pad0: u32,
    pub mask_buffer_address: u64,
    pub tcp_mask: u32,
    pub _pad1: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlGpuGetTpcMasksArgs>() == 24);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlActiveSlotMask {
    pub slot: u32,
    pub mask: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlActiveSlotMask>() == 8);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlZcullGetCtxSize {
    pub size: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlNvgpuGpuZcullGetInfoArgs {
    pub width_align_pixels: u32,
    pub height_align_pixels: u32,
    pub pixel_squares_by_aliquots: u32,
    pub aliquot_total: u32,
    pub region_byte_multiplier: u32,
    pub region_header_size: u32,
    pub subregion_header_size: u32,
    pub subregion_width_align_pixels: u32,
    pub subregion_height_align_pixels: u32,
    pub subregion_count: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlNvgpuGpuZcullGetInfoArgs>() == 40);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlZbcSetTable {
    pub color_ds: [u32; 4],
    pub color_l2: [u32; 4],
    pub depth: u32,
    pub format: u32,
    pub r#type: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlZbcSetTable>() == 44);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlZbcQueryTable {
    pub color_ds: [u32; 4],
    pub color_l2: [u32; 4],
    pub depth: u32,
    pub ref_cnt: u32,
    pub format: u32,
    pub r#type: u32,
    pub index_size: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlZbcQueryTable>() == 52);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlFlushL2 {
    pub flush: u32,
    pub reserved: u32,
}
const _: () = assert!(std::mem::size_of::<IoctlFlushL2>() == 8);

#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct IoctlGetGpuTime {
    pub gpu_time: u64,
    pub _pad: [u32; 2],
}
const _: () = assert!(std::mem::size_of::<IoctlGetGpuTime>() == 0x10);

/// nvhost_ctrl_gpu device.
pub struct NvHostCtrlGpu {
    error_notifier_event: Arc<Mutex<KReadableEvent>>,
    unknown_event: Arc<Mutex<KReadableEvent>>,
}

impl NvHostCtrlGpu {
    pub fn new(events_interface: Arc<EventInterface>) -> Self {
        Self {
            error_notifier_event: events_interface.create_event("CtrlGpuErrorNotifier"),
            unknown_event: events_interface.create_event("CtrlGpuUnknownEvent"),
        }
    }

    fn fill_gpu_characteristics(gc: &mut IoctlGpuCharacteristics) {
        gc.arch = 0x120;
        gc.r#impl = 0xb;
        gc.rev = 0xa1;
        gc.num_gpc = 0x1;
        gc.l2_cache_size = 0x40000;
        gc.on_board_video_memory_size = 0x0;
        gc.num_tpc_per_gpc = 0x2;
        gc.bus_type = 0x20;
        gc.big_page_size = 0x20000;
        gc.compression_page_size = 0x20000;
        gc.pde_coverage_bit_count = 0x1B;
        gc.available_big_page_sizes = 0x30000;
        gc.gpc_mask = 0x1;
        gc.sm_arch_sm_version = 0x503;
        gc.sm_arch_spa_version = 0x503;
        gc.sm_arch_warp_count = 0x80;
        gc.gpu_va_bit_count = 0x28;
        gc.reserved = 0x0;
        gc.flags = 0x55;
        gc.twod_class = 0x902D;
        gc.threed_class = 0xB197;
        gc.compute_class = 0xB1C0;
        gc.gpfifo_class = 0xB06F;
        gc.inline_to_memory_class = 0xA140;
        gc.dma_copy_class = 0xB0B5;
        gc.max_fbps_count = 0x1;
        gc.fbp_en_mask = 0x0;
        gc.max_ltc_per_fbp = 0x2;
        gc.max_lts_per_ltc = 0x1;
        gc.max_tex_per_tpc = 0x0;
        gc.max_gpc_count = 0x1;
        gc.rop_l2_en_mask_0 = 0x21D70;
        gc.rop_l2_en_mask_1 = 0x0;
        gc.chipname = 0x6230326D67;
        gc.gr_compbit_store_base_hw = 0x0;
    }

    pub fn get_characteristics(&self, params: &mut IoctlCharacteristics) -> NvResult {
        log::debug!("nvhost_ctrl_gpu::GetCharacteristics called");
        Self::fill_gpu_characteristics(&mut params.gc);
        params.gpu_characteristics_buf_size = 0xA0;
        params.gpu_characteristics_buf_addr = 0xdeadbeef;
        NvResult::Success
    }

    pub fn get_tpc_masks(&self, params: &mut IoctlGpuGetTpcMasksArgs) -> NvResult {
        log::debug!(
            "nvhost_ctrl_gpu::GetTPCMasks called, mask_buffer_size=0x{:X}",
            params.mask_buffer_size
        );
        if params.mask_buffer_size != 0 {
            params.tcp_mask = 3;
        }
        NvResult::Success
    }

    pub fn get_active_slot_mask(&self, params: &mut IoctlActiveSlotMask) -> NvResult {
        log::debug!("nvhost_ctrl_gpu::GetActiveSlotMask called");
        params.slot = 0x07;
        params.mask = 0x01;
        NvResult::Success
    }

    pub fn zcull_get_ctx_size(&self, params: &mut IoctlZcullGetCtxSize) -> NvResult {
        log::debug!("nvhost_ctrl_gpu::ZCullGetCtxSize called");
        params.size = 0x1;
        NvResult::Success
    }

    pub fn zcull_get_info(&self, params: &mut IoctlNvgpuGpuZcullGetInfoArgs) -> NvResult {
        log::debug!("nvhost_ctrl_gpu::ZCullGetInfo called");
        params.width_align_pixels = 0x20;
        params.height_align_pixels = 0x20;
        params.pixel_squares_by_aliquots = 0x400;
        params.aliquot_total = 0x800;
        params.region_byte_multiplier = 0x20;
        params.region_header_size = 0x20;
        params.subregion_header_size = 0xc0;
        params.subregion_width_align_pixels = 0x20;
        params.subregion_height_align_pixels = 0x40;
        params.subregion_count = 0x10;
        NvResult::Success
    }

    pub fn zbc_set_table(&self, _params: &mut IoctlZbcSetTable) -> NvResult {
        log::warn!("nvhost_ctrl_gpu::ZBCSetTable (STUBBED) called");
        NvResult::Success
    }

    pub fn zbc_query_table(&self, _params: &mut IoctlZbcQueryTable) -> NvResult {
        log::warn!("nvhost_ctrl_gpu::ZBCQueryTable (STUBBED) called");
        NvResult::Success
    }

    pub fn flush_l2(&self, _params: &mut IoctlFlushL2) -> NvResult {
        log::warn!("nvhost_ctrl_gpu::FlushL2 (STUBBED) called");
        NvResult::Success
    }

    pub fn get_gpu_time(&self, params: &mut IoctlGetGpuTime) -> NvResult {
        log::debug!("nvhost_ctrl_gpu::GetGpuTime called");
        // Stubbed: would read from CoreTiming
        params.gpu_time = 0;
        NvResult::Success
    }
}

impl NvDevice for NvHostCtrlGpu {
    fn ioctl1(
        &self,
        _fd: DeviceFD,
        command: Ioctl,
        input: &[u8],
        output: &mut [u8],
    ) -> NvResult {
        match command.group() {
            b'G' => match command.cmd() {
                0x1 => {
                    let mut params: IoctlZcullGetCtxSize = read_struct(input);
                    let r = self.zcull_get_ctx_size(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x2 => {
                    let mut params: IoctlNvgpuGpuZcullGetInfoArgs = read_struct(input);
                    let r = self.zcull_get_info(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x3 => {
                    let mut params: IoctlZbcSetTable = read_struct(input);
                    let r = self.zbc_set_table(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x4 => {
                    let mut params: IoctlZbcQueryTable = read_struct(input);
                    let r = self.zbc_query_table(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x5 => {
                    let mut params: IoctlCharacteristics = read_struct(input);
                    let r = self.get_characteristics(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x6 => {
                    let mut params: IoctlGpuGetTpcMasksArgs = read_struct(input);
                    let r = self.get_tpc_masks(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x7 => {
                    let mut params: IoctlFlushL2 = read_struct(input);
                    let r = self.flush_l2(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x14 => {
                    let mut params: IoctlActiveSlotMask = read_struct(input);
                    let r = self.get_active_slot_mask(&mut params);
                    write_struct(output, &params);
                    r
                }
                0x1c => {
                    let mut params: IoctlGetGpuTime = read_struct(input);
                    let r = self.get_gpu_time(&mut params);
                    write_struct(output, &params);
                    r
                }
                _ => {
                    log::error!("Unimplemented ioctl={:08X}", command.raw);
                    NvResult::NotImplemented
                }
            },
            _ => {
                log::error!("Unimplemented ioctl={:08X}", command.raw);
                NvResult::NotImplemented
            }
        }
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
        input: &[u8],
        output: &mut [u8],
        inline_output: &mut [u8],
    ) -> NvResult {
        match command.group() {
            b'G' => match command.cmd() {
                0x5 => {
                    let mut params: IoctlCharacteristics = read_struct(input);
                    let r = self.get_characteristics(&mut params);
                    write_struct(output, &params);
                    // Write inline output (gpu characteristics)
                    if inline_output.len() >= std::mem::size_of::<IoctlGpuCharacteristics>() {
                        write_struct(inline_output, &params.gc);
                    }
                    r
                }
                0x6 => {
                    let mut params: IoctlGpuGetTpcMasksArgs = read_struct(input);
                    let r = self.get_tpc_masks(&mut params);
                    write_struct(output, &params);
                    if inline_output.len() >= 4 {
                        write_struct(inline_output, &params.tcp_mask);
                    }
                    r
                }
                _ => {
                    log::error!("Unimplemented ioctl={:08X}", command.raw);
                    NvResult::NotImplemented
                }
            },
            _ => {
                log::error!("Unimplemented ioctl={:08X}", command.raw);
                NvResult::NotImplemented
            }
        }
    }

    fn on_open(&self, _session_id: SessionId, _fd: DeviceFD) {}
    fn on_close(&self, _fd: DeviceFD) {}

    fn query_event(&self, event_id: u32) -> Option<Arc<Mutex<KReadableEvent>>> {
        match event_id {
            1 => Some(Arc::clone(&self.error_notifier_event)),
            2 => Some(Arc::clone(&self.unknown_event)),
            _ => {
                log::error!("Unknown Ctrl GPU Event {}", event_id);
                None
            }
        }
    }
}

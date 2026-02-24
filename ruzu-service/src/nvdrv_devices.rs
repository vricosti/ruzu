// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! NVIDIA driver device implementations for `/dev/nvmap`, `/dev/nvhost-as-gpu`,
//! `/dev/nvhost-gpu`, and `/dev/nvhost-ctrl`.

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use ruzu_gpu::command_processor::GpEntry;
use ruzu_gpu::gpu_context::GpuContext;

/// Trait for an NVIDIA device that handles ioctl commands.
pub trait NvDevice: Send + Sync {
    fn name(&self) -> &str;
    fn ioctl(&mut self, cmd: u32, input: &[u8], output: &mut [u8]) -> u32;
}

// ── Helper functions ────────────────────────────────────────────────────────

fn read_u32(buf: &[u8], offset: usize) -> u32 {
    if offset + 4 <= buf.len() {
        u32::from_le_bytes(buf[offset..offset + 4].try_into().unwrap())
    } else {
        0
    }
}

fn read_u64(buf: &[u8], offset: usize) -> u64 {
    if offset + 8 <= buf.len() {
        u64::from_le_bytes(buf[offset..offset + 8].try_into().unwrap())
    } else {
        0
    }
}

fn write_u32(buf: &mut [u8], offset: usize, val: u32) {
    if offset + 4 <= buf.len() {
        buf[offset..offset + 4].copy_from_slice(&val.to_le_bytes());
    }
}

fn write_u64(buf: &mut [u8], offset: usize, val: u64) {
    if offset + 8 <= buf.len() {
        buf[offset..offset + 8].copy_from_slice(&val.to_le_bytes());
    }
}

// ── NvMap device (/dev/nvmap) ───────────────────────────────────────────────

/// Internal handle tracking for nvmap.
struct NvMapHandle {
    id: u32,
    size: u32,
    align: u32,
    address: u64,
    allocated: bool,
}

/// `/dev/nvmap` — Memory handle allocation device.
///
/// Registers allocated handles in the shared `NvMapRegistry` so that other
/// services (NvHostAsGpu, VI) can resolve handle → guest address.
pub struct NvMap {
    gpu: Arc<GpuContext>,
    handles: HashMap<u32, NvMapHandle>,
    next_handle: u32,
    next_id: u32,
}

impl NvMap {
    pub fn new(gpu: Arc<GpuContext>) -> Self {
        Self {
            gpu,
            handles: HashMap::new(),
            next_handle: 1,
            next_id: 1,
        }
    }
}

impl NvDevice for NvMap {
    fn name(&self) -> &str {
        "/dev/nvmap"
    }

    fn ioctl(&mut self, cmd: u32, input: &[u8], output: &mut [u8]) -> u32 {
        let ioctl_nr = cmd & 0xFF;

        match ioctl_nr {
            // Create: input {size: u32}, output {handle: u32}
            1 => {
                let size = read_u32(input, 0);
                let handle = self.next_handle;
                self.next_handle += 1;
                let id = self.next_id;
                self.next_id += 1;

                self.handles.insert(
                    handle,
                    NvMapHandle {
                        id,
                        size,
                        align: 0x1000,
                        address: 0,
                        allocated: false,
                    },
                );

                log::debug!("nvmap: Create size=0x{:X} -> handle={}", size, handle);
                write_u32(output, 0, handle);
                0
            }

            // FromId: input {id: u32}, output {handle: u32}
            3 => {
                let id = read_u32(input, 0);
                let handle = self
                    .handles
                    .iter()
                    .find(|(_, h)| h.id == id)
                    .map(|(&k, _)| k);
                if let Some(handle) = handle {
                    log::debug!("nvmap: FromId id={} -> handle={}", id, handle);
                    write_u32(output, 0, handle);
                } else {
                    log::warn!("nvmap: FromId id={} not found", id);
                    write_u32(output, 0, 0);
                }
                0
            }

            // Alloc: input {handle, heap_mask, flags, align, kind, pad, addr}
            4 => {
                let handle = read_u32(input, 0);
                let align = read_u32(input, 12);
                let addr = read_u64(input, 20);

                if let Some(h) = self.handles.get_mut(&handle) {
                    if align > 0 {
                        h.align = align;
                    }
                    h.address = addr;
                    h.allocated = true;
                    // Publish to the shared registry so other services can resolve
                    // this handle to a guest address.
                    self.gpu.nvmap_registry.register(handle, addr, h.size);
                    log::debug!(
                        "nvmap: Alloc handle={}, align=0x{:X}, addr=0x{:X}",
                        handle,
                        h.align,
                        addr
                    );
                } else {
                    log::warn!("nvmap: Alloc handle={} not found", handle);
                }
                0
            }

            // Param: input {handle, param_type}, output {value}
            9 => {
                let handle = read_u32(input, 0);
                let param_type = read_u32(input, 4);

                let value = if let Some(h) = self.handles.get(&handle) {
                    match param_type {
                        1 => h.size,  // SIZE
                        2 => h.align, // ALIGNMENT
                        3 => 0x40,    // BASE (placeholder)
                        4 => {
                            // HEAP
                            if h.allocated {
                                0x40000000
                            } else {
                                0
                            }
                        }
                        5 => 0, // KIND
                        6 => 0, // COMPS
                        _ => 0,
                    }
                } else {
                    0
                };

                log::debug!(
                    "nvmap: Param handle={}, type={} -> {}",
                    handle,
                    param_type,
                    value
                );
                write_u32(output, 0, value);
                0
            }

            // GetId: input {handle}, output {id}
            14 => {
                // Input layout: {pad: u32, handle: u32} — id goes in first word of output
                let handle = read_u32(input, 4);
                let id = self.handles.get(&handle).map(|h| h.id).unwrap_or(0);
                log::debug!("nvmap: GetId handle={} -> id={}", handle, id);
                write_u32(output, 0, id);
                write_u32(output, 4, handle);
                0
            }

            _ => {
                log::warn!("nvmap: unknown ioctl_nr={}", ioctl_nr);
                0
            }
        }
    }
}

// ── NvHostAsGpu device (/dev/nvhost-as-gpu) ─────────────────────────────────

/// `/dev/nvhost-as-gpu` — GPU address space management.
///
/// Routes MapBufferEx to the GPU memory manager for real VA mapping.
pub struct NvHostAsGpu {
    gpu: Arc<GpuContext>,
}

impl NvHostAsGpu {
    pub fn new(gpu: Arc<GpuContext>) -> Self {
        Self { gpu }
    }
}

impl NvDevice for NvHostAsGpu {
    fn name(&self) -> &str {
        "/dev/nvhost-as-gpu"
    }

    fn ioctl(&mut self, cmd: u32, input: &[u8], output: &mut [u8]) -> u32 {
        let ioctl_nr = cmd & 0xFF;

        match ioctl_nr {
            // AllocAsEx (9): Initialize GPU VA space.
            9 => {
                log::debug!("nvhost-as-gpu: AllocAsEx");
                0
            }

            // MapBufferEx (6): Map nvmap handle to GPU VA.
            // Input: {flags: u32, kind: u32, nvmap_handle: u32, page_size: u32,
            //         buffer_offset: u64, mapping_size: u64, offset: u64}
            6 => {
                let flags = read_u32(input, 0);
                let nvmap_handle = read_u32(input, 8);
                let buffer_offset = read_u64(input, 16);
                let mapping_size = read_u64(input, 24);
                let fixed_offset = read_u64(input, 32);

                let is_fixed = (flags & 1) != 0;
                let size = if mapping_size > 0 { mapping_size } else { 0x10000 };

                // Look up the nvmap handle's guest address from the registry.
                // Fall back to buffer_offset if the handle isn't registered.
                let cpu_addr = self
                    .gpu
                    .nvmap_registry
                    .get_address(nvmap_handle)
                    .unwrap_or(buffer_offset);

                let mut mm = self.gpu.memory_manager.write();
                let gpu_va = if is_fixed && fixed_offset != 0 {
                    mm.alloc_fixed(fixed_offset, cpu_addr, size);
                    fixed_offset
                } else {
                    mm.alloc_any(cpu_addr, size)
                };

                log::debug!(
                    "nvhost-as-gpu: MapBufferEx handle={} size=0x{:X} -> gpu_va=0x{:X}",
                    nvmap_handle,
                    size,
                    gpu_va
                );
                write_u64(output, 0, gpu_va);
                0
            }

            // UnmapBuffer (5)
            5 => {
                let offset = read_u64(input, 0);
                log::debug!("nvhost-as-gpu: UnmapBuffer offset=0x{:X}", offset);
                // Unmap a page — real implementation would track the mapping size.
                let mut mm = self.gpu.memory_manager.write();
                mm.unmap(offset, 0x10000);
                0
            }

            // BindChannel (1)
            1 => {
                log::debug!("nvhost-as-gpu: BindChannel");
                0
            }

            // GetVARegions (8)
            8 => {
                log::debug!("nvhost-as-gpu: GetVARegions");
                // Return two VA regions: small page + big page.
                write_u64(output, 0, 0x0400_0000);  // offset
                write_u32(output, 8, 0x1000);         // page_size (4 KB)
                write_u32(output, 12, 0x3800);        // pages
                write_u64(output, 16, 0x0400_0000);  // offset2
                write_u32(output, 24, 0x10000);       // page_size (64 KB)
                write_u32(output, 28, 0x1);           // pages
                0
            }

            _ => {
                log::warn!("nvhost-as-gpu: unknown ioctl_nr={}", ioctl_nr);
                0
            }
        }
    }
}

// ── NvHostGpu device (/dev/nvhost-gpu) ──────────────────────────────────────

/// `/dev/nvhost-gpu` — GPU channel management.
///
/// Routes SubmitGpfifo to the GpuContext GPFIFO queue.
pub struct NvHostGpu {
    gpu: Arc<GpuContext>,
}

impl NvHostGpu {
    pub fn new(gpu: Arc<GpuContext>) -> Self {
        Self { gpu }
    }
}

impl NvDevice for NvHostGpu {
    fn name(&self) -> &str {
        "/dev/nvhost-gpu"
    }

    fn ioctl(&mut self, cmd: u32, input: &[u8], output: &mut [u8]) -> u32 {
        let ioctl_nr = cmd & 0xFF;

        match ioctl_nr {
            // SetNVMAPfd (1)
            1 => {
                log::debug!("nvhost-gpu: SetNVMAPfd");
                0
            }

            // SubmitGpfifo (8)
            // Input: {gpfifo_addr: u64, num_entries: u32, flags: u32, fence: {id, value}}
            // Each GPFIFO entry is 8 bytes (entry0: u32, entry1: u32).
            8 => {
                let num_entries = read_u32(input, 8) as usize;
                let fence_id = read_u32(input, 16);

                log::debug!(
                    "nvhost-gpu: SubmitGpfifo entries={}, fence_id={}",
                    num_entries,
                    fence_id
                );

                // GPFIFO entries follow after the header (offset 24).
                let entries_offset = 24;
                let mut entries = Vec::with_capacity(num_entries);
                for i in 0..num_entries {
                    let off = entries_offset + i * 8;
                    let entry0 = read_u32(input, off);
                    let entry1 = read_u32(input, off + 4);
                    entries.push(GpEntry { entry0, entry1 });
                }

                self.gpu.submit_gpfifo(entries);

                // Increment the fence syncpoint and return the new value.
                let new_value = if (fence_id as usize) < ruzu_gpu::syncpoint::NUM_SYNCPOINTS {
                    self.gpu.syncpoints.increment(fence_id)
                } else {
                    0
                };

                // Output: fence {id, value}
                write_u32(output, 0, fence_id);
                write_u32(output, 4, new_value);
                0
            }

            // AllocGPFIFOEx2 (0x1A)
            0x1A => {
                log::debug!("nvhost-gpu: AllocGPFIFOEx2");
                // Return dummy fence: id=0, value=0
                write_u32(output, 0, 0);
                write_u32(output, 4, 0);
                0
            }

            // SetErrorNotifier (0x1B)
            0x1B => {
                log::debug!("nvhost-gpu: SetErrorNotifier");
                0
            }

            // SetPriority (0x20)
            0x20 => {
                log::debug!("nvhost-gpu: SetPriority");
                0
            }

            _ => {
                log::warn!("nvhost-gpu: unknown ioctl_nr={}", ioctl_nr);
                0
            }
        }
    }
}

// ── NvHostCtrl device (/dev/nvhost-ctrl) ────────────────────────────────────

/// `/dev/nvhost-ctrl` — Syncpoint management.
///
/// Routes syncpoint queries and waits to the SyncpointManager.
pub struct NvHostCtrl {
    gpu: Arc<GpuContext>,
}

impl NvHostCtrl {
    pub fn new(gpu: Arc<GpuContext>) -> Self {
        Self { gpu }
    }
}

impl NvDevice for NvHostCtrl {
    fn name(&self) -> &str {
        "/dev/nvhost-ctrl"
    }

    fn ioctl(&mut self, cmd: u32, input: &[u8], output: &mut [u8]) -> u32 {
        let ioctl_nr = cmd & 0xFF;

        match ioctl_nr {
            // SyncpointRead (1): input {id: u32}, output {value: u32}
            1 => {
                let id = read_u32(input, 0);
                let value = self.gpu.syncpoints.get_value(id);
                log::debug!("nvhost-ctrl: SyncpointRead id={} -> value={}", id, value);
                write_u32(output, 0, value);
                0
            }

            // SyncpointIncr (2): input {id: u32}
            2 => {
                let id = read_u32(input, 0);
                let new_val = self.gpu.syncpoints.increment(id);
                log::debug!("nvhost-ctrl: SyncpointIncr id={} -> value={}", id, new_val);
                0
            }

            // SyncpointWait (3): input {id: u32, threshold: u32, timeout_ms: i32}
            3 => {
                let id = read_u32(input, 0);
                let threshold = read_u32(input, 4);
                let timeout_ms = read_u32(input, 8) as i32;

                let timeout = if timeout_ms < 0 {
                    Duration::from_secs(3600) // "infinite" timeout
                } else {
                    Duration::from_millis(timeout_ms as u64)
                };

                log::debug!(
                    "nvhost-ctrl: SyncpointWait id={}, threshold={}, timeout={}ms",
                    id,
                    threshold,
                    timeout_ms
                );

                let reached = self.gpu.syncpoints.wait(id, threshold, timeout);
                if reached {
                    let value = self.gpu.syncpoints.get_value(id);
                    write_u32(output, 0, value);
                    0
                } else {
                    // Timeout — return EAGAIN-like error code.
                    write_u32(output, 0, self.gpu.syncpoints.get_value(id));
                    5 // NvError_Timeout
                }
            }

            // EventWait (4) / EventWaitAsync (5) — simplified: just check syncpoint
            4 | 5 => {
                let id = read_u32(input, 0);
                let threshold = read_u32(input, 4);
                let value = self.gpu.syncpoints.get_value(id);

                log::debug!(
                    "nvhost-ctrl: EventWait id={}, threshold={}, current={}",
                    id,
                    threshold,
                    value
                );

                write_u32(output, 0, value);
                0
            }

            _ => {
                log::debug!("nvhost-ctrl: ioctl_nr={}", ioctl_nr);
                0
            }
        }
    }
}

// ── Factory function ────────────────────────────────────────────────────────

/// Create an NvDevice from a device path name.
///
/// Devices that need GPU access receive a shared reference to the GpuContext.
pub fn create_device(path: &str, gpu: Arc<GpuContext>) -> Option<Box<dyn NvDevice>> {
    match path {
        "/dev/nvmap" => Some(Box::new(NvMap::new(gpu.clone()))),
        "/dev/nvhost-as-gpu" => Some(Box::new(NvHostAsGpu::new(gpu))),
        "/dev/nvhost-gpu" => Some(Box::new(NvHostGpu::new(gpu.clone()))),
        "/dev/nvhost-ctrl" | "/dev/nvhost-ctrl-gpu" => Some(Box::new(NvHostCtrl::new(gpu))),
        _ => {
            log::warn!("nvdrv: unknown device path \"{}\"", path);
            None
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn test_gpu() -> Arc<GpuContext> {
        Arc::new(GpuContext::new())
    }

    #[test]
    fn test_nvmap_create_and_param() {
        let mut dev = NvMap::new(test_gpu());

        // Create a handle with size 0x1000.
        let mut input = [0u8; 32];
        input[0..4].copy_from_slice(&0x1000u32.to_le_bytes());
        let mut output = [0u8; 32];
        let rc = dev.ioctl(1, &input, &mut output);
        assert_eq!(rc, 0);
        let handle = u32::from_le_bytes(output[0..4].try_into().unwrap());
        assert_ne!(handle, 0);

        // Query the size (param_type=1).
        let mut param_in = [0u8; 8];
        param_in[0..4].copy_from_slice(&handle.to_le_bytes());
        param_in[4..8].copy_from_slice(&1u32.to_le_bytes());
        let mut param_out = [0u8; 4];
        let rc = dev.ioctl(9, &param_in, &mut param_out);
        assert_eq!(rc, 0);
        let size = u32::from_le_bytes(param_out[0..4].try_into().unwrap());
        assert_eq!(size, 0x1000);
    }

    #[test]
    fn test_nvmap_alloc() {
        let mut dev = NvMap::new(test_gpu());

        // Create
        let mut input = [0u8; 32];
        input[0..4].copy_from_slice(&0x2000u32.to_le_bytes());
        let mut output = [0u8; 32];
        dev.ioctl(1, &input, &mut output);
        let handle = u32::from_le_bytes(output[0..4].try_into().unwrap());

        // Alloc: {handle, heap_mask, flags, align, kind, pad, addr}
        let mut alloc_in = [0u8; 32];
        alloc_in[0..4].copy_from_slice(&handle.to_le_bytes());
        alloc_in[12..16].copy_from_slice(&0x2000u32.to_le_bytes()); // align
        alloc_in[20..28].copy_from_slice(&0xDEAD_0000u64.to_le_bytes()); // addr
        let mut alloc_out = [0u8; 32];
        let rc = dev.ioctl(4, &alloc_in, &mut alloc_out);
        assert_eq!(rc, 0);

        // Check handle is now allocated.
        assert!(dev.handles.get(&handle).unwrap().allocated);
    }

    #[test]
    fn test_nvmap_get_id() {
        let mut dev = NvMap::new(test_gpu());

        // Create
        let mut input = [0u8; 32];
        input[0..4].copy_from_slice(&0x1000u32.to_le_bytes());
        let mut output = [0u8; 32];
        dev.ioctl(1, &input, &mut output);
        let handle = u32::from_le_bytes(output[0..4].try_into().unwrap());

        // GetId: input {pad: u32, handle: u32}
        let mut id_in = [0u8; 8];
        id_in[4..8].copy_from_slice(&handle.to_le_bytes());
        let mut id_out = [0u8; 8];
        let rc = dev.ioctl(14, &id_in, &mut id_out);
        assert_eq!(rc, 0);
        let id = u32::from_le_bytes(id_out[0..4].try_into().unwrap());
        assert_ne!(id, 0);
    }

    #[test]
    fn test_nvhost_as_gpu_map() {
        let gpu = test_gpu();
        let mut dev = NvHostAsGpu::new(gpu);
        let input = [0u8; 64];
        let mut output = [0u8; 32];

        // MapBufferEx should return a monotonically increasing offset.
        let rc = dev.ioctl(6, &input, &mut output);
        assert_eq!(rc, 0);
        let off1 = u64::from_le_bytes(output[0..8].try_into().unwrap());

        let rc = dev.ioctl(6, &input, &mut output);
        assert_eq!(rc, 0);
        let off2 = u64::from_le_bytes(output[0..8].try_into().unwrap());

        assert!(off2 > off1);
    }

    #[test]
    fn test_create_device_known_paths() {
        let gpu = test_gpu();
        assert!(create_device("/dev/nvmap", gpu.clone()).is_some());
        assert!(create_device("/dev/nvhost-as-gpu", gpu.clone()).is_some());
        assert!(create_device("/dev/nvhost-gpu", gpu.clone()).is_some());
        assert!(create_device("/dev/nvhost-ctrl", gpu.clone()).is_some());
        assert!(create_device("/dev/nvhost-ctrl-gpu", gpu.clone()).is_some());
        assert!(create_device("/dev/unknown", gpu).is_none());
    }

    #[test]
    fn test_nvhost_gpu_submit_gpfifo() {
        let gpu = test_gpu();
        let mut dev = NvHostGpu::new(gpu.clone());

        // Build SubmitGpfifo input: {gpfifo_addr: u64, num_entries: u32, flags: u32,
        //                            fence: {id: u32, value: u32}, entries...}
        let mut input = [0u8; 64];
        write_u64(&mut input, 0, 0); // gpfifo_addr (unused, entries inline)
        write_u32(&mut input, 8, 1); // num_entries = 1
        write_u32(&mut input, 12, 0); // flags
        write_u32(&mut input, 16, 0); // fence id = 0
        write_u32(&mut input, 20, 0); // fence value
        // Entry at offset 24
        write_u32(&mut input, 24, 0x1000); // entry0
        write_u32(&mut input, 28, 0); // entry1

        let mut output = [0u8; 16];
        let rc = dev.ioctl(8, &input, &mut output);
        assert_eq!(rc, 0);

        // Fence syncpoint should have been incremented.
        assert_eq!(gpu.syncpoints.get_value(0), 1);
    }

    #[test]
    fn test_nvhost_ctrl_syncpoint_read() {
        let gpu = test_gpu();
        gpu.syncpoints.increment(5);
        gpu.syncpoints.increment(5);

        let mut dev = NvHostCtrl::new(gpu);
        let mut input = [0u8; 4];
        write_u32(&mut input, 0, 5); // syncpoint id = 5
        let mut output = [0u8; 4];

        let rc = dev.ioctl(1, &input, &mut output);
        assert_eq!(rc, 0);
        let value = u32::from_le_bytes(output[0..4].try_into().unwrap());
        assert_eq!(value, 2);
    }

    #[test]
    fn test_nvhost_ctrl_syncpoint_wait_already_reached() {
        let gpu = test_gpu();
        gpu.syncpoints.increment(3);
        gpu.syncpoints.increment(3);

        let mut dev = NvHostCtrl::new(gpu);
        let mut input = [0u8; 12];
        write_u32(&mut input, 0, 3); // id
        write_u32(&mut input, 4, 1); // threshold
        write_u32(&mut input, 8, 100); // timeout_ms
        let mut output = [0u8; 4];

        let rc = dev.ioctl(3, &input, &mut output);
        assert_eq!(rc, 0); // Should succeed immediately.
    }

    #[test]
    fn test_nvmap_alloc_registers_in_registry() {
        let gpu = test_gpu();
        let mut dev = NvMap::new(gpu.clone());

        // Create
        let mut input = [0u8; 32];
        input[0..4].copy_from_slice(&0x2000u32.to_le_bytes());
        let mut output = [0u8; 32];
        dev.ioctl(1, &input, &mut output);
        let handle = u32::from_le_bytes(output[0..4].try_into().unwrap());

        // Alloc with addr=0xBEEF_0000
        let mut alloc_in = [0u8; 32];
        alloc_in[0..4].copy_from_slice(&handle.to_le_bytes());
        alloc_in[12..16].copy_from_slice(&0x1000u32.to_le_bytes()); // align
        alloc_in[20..28].copy_from_slice(&0xBEEF_0000u64.to_le_bytes()); // addr
        let mut alloc_out = [0u8; 32];
        dev.ioctl(4, &alloc_in, &mut alloc_out);

        // Verify the registry has the handle.
        assert_eq!(gpu.nvmap_registry.get_address(handle), Some(0xBEEF_0000));
        assert_eq!(gpu.nvmap_registry.get_size(handle), Some(0x2000));
    }

    #[test]
    fn test_nvhost_as_gpu_map_uses_registry() {
        let gpu = test_gpu();

        // Pre-register an nvmap handle in the registry.
        gpu.nvmap_registry.register(7, 0xCAFE_0000, 0x10000);

        let mut dev = NvHostAsGpu::new(gpu);

        // MapBufferEx with nvmap_handle=7 (at input offset 8).
        let mut input = [0u8; 64];
        write_u32(&mut input, 0, 0);   // flags: not fixed
        write_u32(&mut input, 8, 7);   // nvmap_handle = 7
        write_u64(&mut input, 24, 0x10000); // mapping_size
        let mut output = [0u8; 32];

        let rc = dev.ioctl(6, &input, &mut output);
        assert_eq!(rc, 0);

        // The GPU VA should have been mapped. Verify it translates to 0xCAFE_0000.
        let gpu_va = u64::from_le_bytes(output[0..8].try_into().unwrap());
        let mm = dev.gpu.memory_manager.read();
        assert_eq!(mm.translate(gpu_va), Some(0xCAFE_0000));
    }
}

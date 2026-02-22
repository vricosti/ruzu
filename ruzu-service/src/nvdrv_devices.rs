// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! NVIDIA driver device implementations for `/dev/nvmap`, `/dev/nvhost-as-gpu`,
//! `/dev/nvhost-gpu`, and `/dev/nvhost-ctrl`.

use std::collections::HashMap;

/// Trait for an NVIDIA device that handles ioctl commands.
pub trait NvDevice: Send + Sync {
    fn name(&self) -> &str;
    fn ioctl(&mut self, cmd: u32, input: &[u8], output: &mut [u8]) -> u32;
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
pub struct NvMap {
    handles: HashMap<u32, NvMapHandle>,
    next_handle: u32,
    next_id: u32,
}

impl NvMap {
    pub fn new() -> Self {
        Self {
            handles: HashMap::new(),
            next_handle: 1,
            next_id: 1,
        }
    }

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
}

impl Default for NvMap {
    fn default() -> Self {
        Self::new()
    }
}

impl NvDevice for NvMap {
    fn name(&self) -> &str {
        "/dev/nvmap"
    }

    fn ioctl(&mut self, cmd: u32, input: &[u8], output: &mut [u8]) -> u32 {
        // Extract ioctl number from the command (bits [7:0] of the 3rd byte).
        let ioctl_nr = cmd & 0xFF;

        match ioctl_nr {
            // Create: input {size: u32}, output {handle: u32}
            1 => {
                let size = Self::read_u32(input, 0);
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
                Self::write_u32(output, 0, handle);
                0
            }

            // FromId: input {id: u32}, output {handle: u32}
            3 => {
                let id = Self::read_u32(input, 0);
                let handle = self
                    .handles
                    .iter()
                    .find(|(_, h)| h.id == id)
                    .map(|(&k, _)| k);
                if let Some(handle) = handle {
                    log::debug!("nvmap: FromId id={} -> handle={}", id, handle);
                    Self::write_u32(output, 0, handle);
                } else {
                    log::warn!("nvmap: FromId id={} not found", id);
                    Self::write_u32(output, 0, 0);
                }
                0
            }

            // Alloc: input {handle, heap_mask, flags, align, kind, pad, addr}
            4 => {
                let handle = Self::read_u32(input, 0);
                let align = Self::read_u32(input, 12);
                let addr = Self::read_u64(input, 20);

                if let Some(h) = self.handles.get_mut(&handle) {
                    if align > 0 {
                        h.align = align;
                    }
                    h.address = addr;
                    h.allocated = true;
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
                let handle = Self::read_u32(input, 0);
                let param_type = Self::read_u32(input, 4);

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
                Self::write_u32(output, 0, value);
                0
            }

            // GetId: input {handle}, output {id}
            14 => {
                // Input layout: {pad: u32, handle: u32} — id goes in first word of output
                let handle = Self::read_u32(input, 4);
                let id = self.handles.get(&handle).map(|h| h.id).unwrap_or(0);
                log::debug!("nvmap: GetId handle={} -> id={}", handle, id);
                Self::write_u32(output, 0, id);
                Self::write_u32(output, 4, handle);
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

/// `/dev/nvhost-as-gpu` — GPU address space management (stubs).
pub struct NvHostAsGpu {
    next_offset: u64,
}

impl NvHostAsGpu {
    pub fn new() -> Self {
        Self {
            next_offset: 0x0400_0000, // Start GPU VA allocations at 64 MB
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
}

impl Default for NvHostAsGpu {
    fn default() -> Self {
        Self::new()
    }
}

impl NvDevice for NvHostAsGpu {
    fn name(&self) -> &str {
        "/dev/nvhost-as-gpu"
    }

    fn ioctl(&mut self, cmd: u32, _input: &[u8], output: &mut [u8]) -> u32 {
        let ioctl_nr = cmd & 0xFF;

        match ioctl_nr {
            // AllocAsEx (9): Initialize GPU VA space.
            9 => {
                log::debug!("nvhost-as-gpu: AllocAsEx");
                0
            }

            // MapBufferEx (6): Map nvmap handle to GPU VA.
            6 => {
                let offset = self.next_offset;
                self.next_offset += 0x10000; // Increment by 64 KB
                log::debug!(
                    "nvhost-as-gpu: MapBufferEx -> offset=0x{:X}",
                    offset
                );
                Self::write_u64(output, 0, offset);
                0
            }

            // UnmapBuffer (5)
            5 => {
                log::debug!("nvhost-as-gpu: UnmapBuffer");
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
                // Return two VA regions: small page + big page
                Self::write_u64(output, 0, 0x0400_0000); // offset
                Self::write_u32(output, 8, 0x1000);       // page_size (4 KB)
                Self::write_u32(output, 12, 0x3800);      // pages
                Self::write_u64(output, 16, 0x0400_0000); // offset2
                Self::write_u32(output, 24, 0x10000);     // page_size (64 KB)
                Self::write_u32(output, 28, 0x1);         // pages
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

/// `/dev/nvhost-gpu` — GPU channel management (stubs).
pub struct NvHostGpu;

impl NvHostGpu {
    pub fn new() -> Self {
        Self
    }

    fn write_u32(buf: &mut [u8], offset: usize, val: u32) {
        if offset + 4 <= buf.len() {
            buf[offset..offset + 4].copy_from_slice(&val.to_le_bytes());
        }
    }
}

impl Default for NvHostGpu {
    fn default() -> Self {
        Self::new()
    }
}

impl NvDevice for NvHostGpu {
    fn name(&self) -> &str {
        "/dev/nvhost-gpu"
    }

    fn ioctl(&mut self, cmd: u32, _input: &[u8], output: &mut [u8]) -> u32 {
        let ioctl_nr = cmd & 0xFF;

        match ioctl_nr {
            // SetNVMAPfd (1)
            1 => {
                log::debug!("nvhost-gpu: SetNVMAPfd");
                0
            }

            // AllocGPFIFOEx2 (0x1A)
            0x1A => {
                log::debug!("nvhost-gpu: AllocGPFIFOEx2");
                // Return dummy fence: id=0, value=0
                Self::write_u32(output, 0, 0); // fence id
                Self::write_u32(output, 4, 0); // fence value
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

            // GetCharacteristics (0x1)
            // Note: Some homebrew may call this with a different cmd encoding
            _ => {
                log::warn!("nvhost-gpu: unknown ioctl_nr={}", ioctl_nr);
                0
            }
        }
    }
}

// ── NvHostCtrl device (/dev/nvhost-ctrl) ────────────────────────────────────

/// `/dev/nvhost-ctrl` — Syncpoint management (stubs).
pub struct NvHostCtrl;

impl NvHostCtrl {
    pub fn new() -> Self {
        Self
    }
}

impl Default for NvHostCtrl {
    fn default() -> Self {
        Self::new()
    }
}

impl NvDevice for NvHostCtrl {
    fn name(&self) -> &str {
        "/dev/nvhost-ctrl"
    }

    fn ioctl(&mut self, cmd: u32, _input: &[u8], _output: &mut [u8]) -> u32 {
        let ioctl_nr = cmd & 0xFF;
        log::debug!("nvhost-ctrl: ioctl_nr={}", ioctl_nr);
        0
    }
}

// ── Factory function ────────────────────────────────────────────────────────

/// Create an NvDevice from a device path name.
pub fn create_device(path: &str) -> Option<Box<dyn NvDevice>> {
    match path {
        "/dev/nvmap" => Some(Box::new(NvMap::new())),
        "/dev/nvhost-as-gpu" => Some(Box::new(NvHostAsGpu::new())),
        "/dev/nvhost-gpu" => Some(Box::new(NvHostGpu::new())),
        "/dev/nvhost-ctrl" => Some(Box::new(NvHostCtrl::new())),
        "/dev/nvhost-ctrl-gpu" => Some(Box::new(NvHostCtrl::new())),
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

    #[test]
    fn test_nvmap_create_and_param() {
        let mut dev = NvMap::new();

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
        let mut dev = NvMap::new();

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
        let mut dev = NvMap::new();

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
        let mut dev = NvHostAsGpu::new();
        let input = [0u8; 32];
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
        assert!(create_device("/dev/nvmap").is_some());
        assert!(create_device("/dev/nvhost-as-gpu").is_some());
        assert!(create_device("/dev/nvhost-gpu").is_some());
        assert!(create_device("/dev/nvhost-ctrl").is_some());
        assert!(create_device("/dev/nvhost-ctrl-gpu").is_some());
        assert!(create_device("/dev/unknown").is_none());
    }
}

// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

use std::sync::OnceLock;

#[derive(Debug, Clone, Copy, Default)]
pub struct MemoryInfo {
    pub total_physical_memory: u64,
    pub total_swap_memory: u64,
}

/// Detects the RAM and swap sizes of the host system.
fn detect() -> MemoryInfo {
    let mut mem_info = MemoryInfo::default();

    #[cfg(target_os = "linux")]
    {
        // Safety: sysinfo is a simple syscall that fills a well-defined struct.
        unsafe {
            let mut info: libc::sysinfo = std::mem::zeroed();
            if libc::sysinfo(&mut info) == 0 {
                mem_info.total_physical_memory = info.totalram as u64 * info.mem_unit as u64;
                mem_info.total_swap_memory = info.totalswap as u64 * info.mem_unit as u64;
            }
        }
    }

    #[cfg(target_os = "macos")]
    {
        use std::mem;

        unsafe {
            // Physical memory
            let mut ramsize: u64 = 0;
            let mut size = mem::size_of::<u64>();
            let name = b"hw.memsize\0";
            libc::sysctlbyname(
                name.as_ptr() as *const libc::c_char,
                &mut ramsize as *mut u64 as *mut libc::c_void,
                &mut size,
                std::ptr::null_mut(),
                0,
            );
            mem_info.total_physical_memory = ramsize;

            // Swap - on macOS we read vm.swapusage via sysctl
            // For simplicity, we set swap to 0 as xsw_usage requires
            // platform-specific struct definitions.
            mem_info.total_swap_memory = 0;
        }
    }

    #[cfg(target_os = "windows")]
    {
        use std::mem;

        // On Windows, use GetPhysicallyInstalledSystemMemory or GlobalMemoryStatusEx
        unsafe {
            #[repr(C)]
            struct MemoryStatusEx {
                dw_length: u32,
                dw_memory_load: u32,
                ull_total_phys: u64,
                ull_avail_phys: u64,
                ull_total_page_file: u64,
                ull_avail_page_file: u64,
                ull_total_virtual: u64,
                ull_avail_virtual: u64,
                ull_avail_extended_virtual: u64,
            }

            extern "system" {
                fn GlobalMemoryStatusEx(lpBuffer: *mut MemoryStatusEx) -> i32;
            }

            let mut status: MemoryStatusEx = mem::zeroed();
            status.dw_length = mem::size_of::<MemoryStatusEx>() as u32;
            if GlobalMemoryStatusEx(&mut status) != 0 {
                mem_info.total_physical_memory = status.ull_total_phys;
                mem_info.total_swap_memory = status
                    .ull_total_page_file
                    .saturating_sub(status.ull_total_phys);
            }
        }
    }

    mem_info
}

static MEM_INFO: OnceLock<MemoryInfo> = OnceLock::new();

/// Gets the memory info of the host system.
/// The result is cached after the first call (matching the upstream `static` local).
pub fn get_mem_info() -> &'static MemoryInfo {
    MEM_INFO.get_or_init(detect)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_mem_info() {
        let info = get_mem_info();
        // On any real system, physical memory should be > 0
        assert!(info.total_physical_memory > 0);
    }

    #[test]
    fn test_get_mem_info_cached() {
        let info1 = get_mem_info();
        let info2 = get_mem_info();
        assert_eq!(info1.total_physical_memory, info2.total_physical_memory);
        assert_eq!(info1.total_swap_memory, info2.total_swap_memory);
    }
}

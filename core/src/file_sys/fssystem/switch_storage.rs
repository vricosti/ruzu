// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_switch_storage.h

use crate::file_sys::vfs::vfs_types::VirtualFile;

#[derive(Debug, Clone, Copy, Default)]
pub struct Region {
    pub offset: i64,
    pub size: i64,
}

pub struct RegionSwitchStorage {
    inside_region_storage: VirtualFile,
    outside_region_storage: VirtualFile,
    region: Region,
}

impl RegionSwitchStorage {
    pub fn new(inside: VirtualFile, outside: VirtualFile, region: Region) -> Self {
        Self { inside_region_storage: inside, outside_region_storage: outside, region }
    }

    pub fn get_size(&self) -> usize { self.inside_region_storage.get_size() }

    pub fn read(&self, buffer: &mut [u8], size: usize, offset: usize) -> usize {
        let mut processed = 0usize;
        while processed < size {
            let mut cur_size: i64 = 0;
            let cur_offset = offset + processed;
            if self.check_regions(&mut cur_size, cur_offset as i64, (size - processed) as i64) {
                self.inside_region_storage.read(&mut buffer[processed..processed + cur_size as usize], cur_size as usize, cur_offset);
            } else {
                self.outside_region_storage.read(&mut buffer[processed..processed + cur_size as usize], cur_size as usize, cur_offset);
            }
            processed += cur_size as usize;
        }
        size
    }

    fn check_regions(&self, out_current_size: &mut i64, offset: i64, size: i64) -> bool {
        let region_end = self.region.offset + self.region.size;
        if self.region.offset <= offset {
            if offset < region_end {
                *out_current_size = if region_end <= offset + size { region_end - offset } else { size };
                true
            } else {
                *out_current_size = size;
                false
            }
        } else {
            *out_current_size = if self.region.offset <= offset + size { self.region.offset - offset } else { size };
            false
        }
    }
}

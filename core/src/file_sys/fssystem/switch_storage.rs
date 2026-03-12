// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_switch_storage.h
// Status: COMPLETE (structural parity)
//
// Region-based storage switch: reads from inside_region_storage when the
// access range falls within the configured region, otherwise reads from
// outside_region_storage. For accesses that span the region boundary,
// the read is split into multiple sub-reads.

use crate::file_sys::vfs::vfs_types::VirtualFile;

/// A region defined by an offset and size.
///
/// Corresponds to upstream `RegionSwitchStorage::Region`.
#[derive(Debug, Clone, Copy, Default)]
pub struct Region {
    pub offset: i64,
    pub size: i64,
}

/// Region-based storage switch.
///
/// Routes reads to either the inside or outside storage depending on
/// whether the current access range falls within the configured region.
///
/// Corresponds to upstream `RegionSwitchStorage`.
pub struct RegionSwitchStorage {
    inside_region_storage: VirtualFile,
    outside_region_storage: VirtualFile,
    region: Region,
}

impl RegionSwitchStorage {
    /// Create a new RegionSwitchStorage.
    ///
    /// Corresponds to upstream `RegionSwitchStorage::RegionSwitchStorage`.
    pub fn new(inside: VirtualFile, outside: VirtualFile, region: Region) -> Self {
        Self {
            inside_region_storage: inside,
            outside_region_storage: outside,
            region,
        }
    }

    /// Get the size of the storage.
    ///
    /// Corresponds to upstream `RegionSwitchStorage::GetSize`.
    pub fn get_size(&self) -> usize {
        self.inside_region_storage.get_size()
    }

    /// Read data from the storage, routing to inside/outside as appropriate.
    ///
    /// For accesses that span the region boundary, this splits the read into
    /// multiple sub-reads, each routed to the correct storage.
    ///
    /// Corresponds to upstream `RegionSwitchStorage::Read`.
    pub fn read(&self, buffer: &mut [u8], size: usize, offset: usize) -> usize {
        // Process until we're done.
        let mut processed = 0usize;
        while processed < size {
            // Process on the appropriate storage.
            let mut cur_size: i64 = 0;
            let cur_offset = offset + processed;
            if self.check_regions(&mut cur_size, cur_offset as i64, (size - processed) as i64) {
                self.inside_region_storage.read(
                    &mut buffer[processed..processed + cur_size as usize],
                    cur_size as usize,
                    cur_offset,
                );
            } else {
                self.outside_region_storage.read(
                    &mut buffer[processed..processed + cur_size as usize],
                    cur_size as usize,
                    cur_offset,
                );
            }

            // Advance.
            processed += cur_size as usize;
        }

        size
    }

    /// Check if the access at [offset, offset+size) falls within the region.
    ///
    /// Sets `out_current_size` to the number of bytes that should be read
    /// from the current storage (inside or outside).
    ///
    /// Returns true if the current chunk is inside the region.
    ///
    /// Corresponds to upstream `RegionSwitchStorage::CheckRegions`.
    fn check_regions(&self, out_current_size: &mut i64, offset: i64, size: i64) -> bool {
        let region_end = self.region.offset + self.region.size;
        if self.region.offset <= offset {
            if offset < region_end {
                // Inside the region.
                if region_end <= offset + size {
                    *out_current_size = region_end - offset;
                } else {
                    *out_current_size = size;
                }
                true
            } else {
                // Past the region.
                *out_current_size = size;
                false
            }
        } else {
            // Before the region.
            if self.region.offset <= offset + size {
                *out_current_size = self.region.offset - offset;
            } else {
                *out_current_size = size;
            }
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_regions_inside() {
        let storage = RegionSwitchStorage {
            inside_region_storage: std::sync::Arc::new(
                crate::file_sys::vfs::vfs_vector::VectorVfsFile::new(
                    vec![0u8; 100],
                    "inside".to_string(),
                    None,
                ),
            ),
            outside_region_storage: std::sync::Arc::new(
                crate::file_sys::vfs::vfs_vector::VectorVfsFile::new(
                    vec![0u8; 100],
                    "outside".to_string(),
                    None,
                ),
            ),
            region: Region {
                offset: 10,
                size: 20,
            },
        };

        let mut cur_size: i64 = 0;
        // Fully inside the region [10, 30)
        assert!(storage.check_regions(&mut cur_size, 15, 5));
        assert_eq!(cur_size, 5);
    }

    #[test]
    fn test_check_regions_outside_after() {
        let storage = RegionSwitchStorage {
            inside_region_storage: std::sync::Arc::new(
                crate::file_sys::vfs::vfs_vector::VectorVfsFile::new(
                    vec![0u8; 100],
                    "inside".to_string(),
                    None,
                ),
            ),
            outside_region_storage: std::sync::Arc::new(
                crate::file_sys::vfs::vfs_vector::VectorVfsFile::new(
                    vec![0u8; 100],
                    "outside".to_string(),
                    None,
                ),
            ),
            region: Region {
                offset: 10,
                size: 20,
            },
        };

        let mut cur_size: i64 = 0;
        // After the region
        assert!(!storage.check_regions(&mut cur_size, 35, 10));
        assert_eq!(cur_size, 10);
    }

    #[test]
    fn test_check_regions_spanning_end() {
        let storage = RegionSwitchStorage {
            inside_region_storage: std::sync::Arc::new(
                crate::file_sys::vfs::vfs_vector::VectorVfsFile::new(
                    vec![0u8; 100],
                    "inside".to_string(),
                    None,
                ),
            ),
            outside_region_storage: std::sync::Arc::new(
                crate::file_sys::vfs::vfs_vector::VectorVfsFile::new(
                    vec![0u8; 100],
                    "outside".to_string(),
                    None,
                ),
            ),
            region: Region {
                offset: 10,
                size: 20,
            },
        };

        let mut cur_size: i64 = 0;
        // Starts inside region [10, 30), reads past end
        assert!(storage.check_regions(&mut cur_size, 25, 10));
        // Should only read up to region end
        assert_eq!(cur_size, 5);
    }

    #[test]
    fn test_check_regions_before() {
        let storage = RegionSwitchStorage {
            inside_region_storage: std::sync::Arc::new(
                crate::file_sys::vfs::vfs_vector::VectorVfsFile::new(
                    vec![0u8; 100],
                    "inside".to_string(),
                    None,
                ),
            ),
            outside_region_storage: std::sync::Arc::new(
                crate::file_sys::vfs::vfs_vector::VectorVfsFile::new(
                    vec![0u8; 100],
                    "outside".to_string(),
                    None,
                ),
            ),
            region: Region {
                offset: 10,
                size: 20,
            },
        };

        let mut cur_size: i64 = 0;
        // Before the region, spanning into it
        assert!(!storage.check_regions(&mut cur_size, 5, 10));
        // Should only read up to region start
        assert_eq!(cur_size, 5);
    }
}

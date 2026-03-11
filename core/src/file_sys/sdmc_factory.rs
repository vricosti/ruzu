// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/sdmc_factory.h / .cpp

use super::registered_cache::{PlaceholderCache, RegisteredCache};
use super::vfs::vfs_types::VirtualDir;

/// File system interface to the SD Card archive.
/// Corresponds to upstream `SDMCFactory`.
pub struct SdmcFactory {
    sd_dir: VirtualDir,
    sd_mod_dir: VirtualDir,
    contents: Option<Box<RegisteredCache>>,
    placeholder: Option<Box<PlaceholderCache>>,
}

impl SdmcFactory {
    pub fn new(sd_dir: VirtualDir, sd_mod_dir: VirtualDir) -> Self {
        // TODO: initialize registered cache from sd_dir contents.
        Self {
            sd_dir,
            sd_mod_dir,
            contents: None,
            placeholder: None,
        }
    }

    pub fn open(&self) -> VirtualDir {
        self.sd_dir.clone()
    }

    pub fn get_sdmc_modification_load_root(&self, _title_id: u64) -> Option<VirtualDir> {
        // TODO: open sd_mod_dir/<title_id_hex>
        None
    }

    pub fn get_sdmc_content_directory(&self) -> Option<VirtualDir> {
        // TODO: open Contents directory on SD.
        None
    }

    pub fn get_sdmc_contents(&self) -> Option<&RegisteredCache> {
        self.contents.as_deref()
    }

    pub fn get_sdmc_placeholder(&self) -> Option<&PlaceholderCache> {
        self.placeholder.as_deref()
    }

    pub fn get_image_directory(&self) -> Option<VirtualDir> {
        None
    }

    pub fn get_sdmc_free_space(&self) -> u64 {
        0
    }

    pub fn get_sdmc_total_space(&self) -> u64 {
        0
    }
}

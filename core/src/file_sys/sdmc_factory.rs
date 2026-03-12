// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/sdmc_factory.h / .cpp
// Status: COMPLETE (structural parity; NAX decryption callback stubbed)
//
// File system interface to the SD Card archive. Provides access to SD card
// content directories, registered caches, placeholder caches, and image
// directories matching the upstream SDMCFactory behavior.

use super::registered_cache::{PlaceholderCache, RegisteredCache};
use super::vfs::vfs::get_or_create_directory_relative;
use super::vfs::vfs_types::VirtualDir;

/// Total size reported for the SD card: 1 TiB.
///
/// Corresponds to upstream `SDMC_TOTAL_SIZE`.
const SDMC_TOTAL_SIZE: u64 = 0x10000000000;

/// File system interface to the SD Card archive.
/// Corresponds to upstream `SDMCFactory`.
pub struct SdmcFactory {
    sd_dir: VirtualDir,
    sd_mod_dir: VirtualDir,
    contents: Option<Box<RegisteredCache>>,
    placeholder: Option<Box<PlaceholderCache>>,
}

impl SdmcFactory {
    /// Construct a new SDMCFactory.
    ///
    /// Corresponds to upstream `SDMCFactory::SDMCFactory`.
    /// In upstream, the constructor initializes `contents` from
    /// `/Nintendo/Contents/registered` with a NAX decryption callback,
    /// and `placeholder` from `/Nintendo/Contents/placehld`.
    pub fn new(sd_dir: VirtualDir, sd_mod_dir: VirtualDir) -> Self {
        // Initialize registered cache from sd_dir contents.
        let contents_dir =
            get_or_create_directory_relative(sd_dir.as_ref(), "/Nintendo/Contents/registered");
        let contents = contents_dir.map(|dir| {
            // In upstream, a NAX decryption callback is passed here:
            //   [](const VirtualFile& file, const NcaID& id) { return NAX{file, id}.GetDecrypted(); }
            // For now, create without the callback.
            Box::new(RegisteredCache::new(dir))
        });

        let placeholder_dir =
            get_or_create_directory_relative(sd_dir.as_ref(), "/Nintendo/Contents/placehld");
        let placeholder = placeholder_dir.map(|dir| Box::new(PlaceholderCache::new(dir)));

        Self {
            sd_dir,
            sd_mod_dir,
            contents,
            placeholder,
        }
    }

    /// Open the root SD directory.
    ///
    /// Corresponds to upstream `SDMCFactory::Open`.
    pub fn open(&self) -> VirtualDir {
        self.sd_dir.clone()
    }

    /// Get the modification load root for a given title.
    ///
    /// LayeredFS doesn't work on updates (title_id & 0xFFF == 0x800) or
    /// title id-less homebrew (title_id == 0).
    ///
    /// Corresponds to upstream `SDMCFactory::GetSDMCModificationLoadRoot`.
    pub fn get_sdmc_modification_load_root(&self, title_id: u64) -> Option<VirtualDir> {
        // LayeredFS doesn't work on updates and title id-less homebrew.
        if title_id == 0 || (title_id & 0xFFF) == 0x800 {
            return None;
        }
        get_or_create_directory_relative(
            self.sd_mod_dir.as_ref(),
            &format!("/{:016X}", title_id),
        )
    }

    /// Get the SD card content directory.
    ///
    /// Corresponds to upstream `SDMCFactory::GetSDMCContentDirectory`.
    pub fn get_sdmc_content_directory(&self) -> Option<VirtualDir> {
        get_or_create_directory_relative(self.sd_dir.as_ref(), "/Nintendo/Contents")
    }

    /// Get the registered cache for SD card contents.
    ///
    /// Corresponds to upstream `SDMCFactory::GetSDMCContents`.
    pub fn get_sdmc_contents(&self) -> Option<&RegisteredCache> {
        self.contents.as_deref()
    }

    /// Get the placeholder cache for SD card contents.
    ///
    /// Corresponds to upstream `SDMCFactory::GetSDMCPlaceholder`.
    pub fn get_sdmc_placeholder(&self) -> Option<&PlaceholderCache> {
        self.placeholder.as_deref()
    }

    /// Get the image (album) directory on the SD card.
    ///
    /// Corresponds to upstream `SDMCFactory::GetImageDirectory`.
    pub fn get_image_directory(&self) -> Option<VirtualDir> {
        get_or_create_directory_relative(self.sd_dir.as_ref(), "/Nintendo/Album")
    }

    /// Get the free space on the SD card.
    ///
    /// Corresponds to upstream `SDMCFactory::GetSDMCFreeSpace`.
    pub fn get_sdmc_free_space(&self) -> u64 {
        self.get_sdmc_total_space() - self.sd_dir.get_size() as u64
    }

    /// Get the total space on the SD card.
    ///
    /// Corresponds to upstream `SDMCFactory::GetSDMCTotalSpace`.
    pub fn get_sdmc_total_space(&self) -> u64 {
        SDMC_TOTAL_SIZE
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file_sys::vfs::vfs_vector::VectorVfsDirectory;
    use std::sync::Arc;

    fn make_empty_dir(name: &str) -> VirtualDir {
        Arc::new(VectorVfsDirectory::new(
            vec![],
            vec![],
            name.to_string(),
            None,
        ))
    }

    #[test]
    fn test_sdmc_total_size() {
        assert_eq!(SDMC_TOTAL_SIZE, 0x10000000000); // 1 TiB
    }

    #[test]
    fn test_new() {
        let sd = make_empty_dir("sd");
        let sd_mod = make_empty_dir("sd_mod");
        let factory = SdmcFactory::new(sd, sd_mod);
        assert_eq!(factory.get_sdmc_total_space(), SDMC_TOTAL_SIZE);
    }

    #[test]
    fn test_open() {
        let sd = make_empty_dir("sd");
        let sd_mod = make_empty_dir("sd_mod");
        let factory = SdmcFactory::new(sd, sd_mod);
        let opened = factory.open();
        assert_eq!(opened.get_name(), "sd");
    }

    #[test]
    fn test_modification_load_root_filters() {
        let sd = make_empty_dir("sd");
        let sd_mod = make_empty_dir("sd_mod");
        let factory = SdmcFactory::new(sd, sd_mod);

        // title_id == 0 should return None.
        assert!(factory.get_sdmc_modification_load_root(0).is_none());
        // Update title IDs (& 0xFFF == 0x800) should return None.
        assert!(factory
            .get_sdmc_modification_load_root(0x0100000000000800)
            .is_none());
    }

    #[test]
    fn test_free_space() {
        let sd = make_empty_dir("sd");
        let sd_mod = make_empty_dir("sd_mod");
        let factory = SdmcFactory::new(sd, sd_mod);
        // Empty dir should have near-total free space.
        assert_eq!(factory.get_sdmc_free_space(), SDMC_TOTAL_SIZE);
    }
}

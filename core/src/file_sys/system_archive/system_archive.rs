// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/file_sys/system_archive/system_archive.h/.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-12
//!
//! Synthesizes system archives from built-in data for known title IDs.

use crate::file_sys::vfs::vfs_types::{VirtualDir, VirtualFile};

use super::mii_model;
use super::ng_word;
use super::shared_font;
use super::system_version;
use super::time_zone_binary;

/// Base title ID for system archives.
const SYSTEM_ARCHIVE_BASE_TITLE_ID: u64 = 0x0100000000000800;

/// Number of known system archive entries.
const SYSTEM_ARCHIVE_COUNT: usize = 0x28;

/// Type alias for system archive supplier functions.
type SystemArchiveSupplier = fn() -> Option<VirtualDir>;

/// Descriptor for a system archive entry.
struct SystemArchiveDescriptor {
    title_id: u64,
    name: &'static str,
    supplier: Option<SystemArchiveSupplier>,
}

/// System archive descriptor table.
/// Corresponds to upstream `SYSTEM_ARCHIVES` array.
const SYSTEM_ARCHIVES: [SystemArchiveDescriptor; SYSTEM_ARCHIVE_COUNT] = [
    SystemArchiveDescriptor {
        title_id: 0x0100000000000800,
        name: "CertStore",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000801,
        name: "ErrorMessage",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000802,
        name: "MiiModel",
        supplier: Some(mii_model::mii_model),
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000803,
        name: "BrowserDll",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000804,
        name: "Help",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000805,
        name: "SharedFont",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000806,
        name: "NgWord",
        supplier: Some(ng_word::ng_word_1),
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000807,
        name: "SsidList",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000808,
        name: "Dictionary",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000809,
        name: "SystemVersion",
        supplier: Some(system_version::system_version),
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000080A,
        name: "AvatarImage",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000080B,
        name: "LocalNews",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000080C,
        name: "Eula",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000080D,
        name: "UrlBlackList",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000080E,
        name: "TimeZoneBinary",
        supplier: Some(time_zone_binary::time_zone_binary),
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000080F,
        name: "CertStoreCruiser",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000810,
        name: "FontNintendoExtension",
        supplier: Some(shared_font::font_nintendo_extension),
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000811,
        name: "FontStandard",
        supplier: Some(shared_font::font_standard),
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000812,
        name: "FontKorean",
        supplier: Some(shared_font::font_korean),
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000813,
        name: "FontChineseTraditional",
        supplier: Some(shared_font::font_chinese_traditional),
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000814,
        name: "FontChineseSimple",
        supplier: Some(shared_font::font_chinese_simple),
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000815,
        name: "FontBfcpx",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000816,
        name: "SystemUpdate",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000817,
        name: "0100000000000817",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000818,
        name: "FirmwareDebugSettings",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000819,
        name: "BootImagePackage",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000081A,
        name: "BootImagePackageSafe",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000081B,
        name: "BootImagePackageExFat",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000081C,
        name: "BootImagePackageExFatSafe",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000081D,
        name: "FatalMessage",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000081E,
        name: "ControllerIcon",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x010000000000081F,
        name: "PlatformConfigIcosa",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000820,
        name: "PlatformConfigCopper",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000821,
        name: "PlatformConfigHoag",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000822,
        name: "ControllerFirmware",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000823,
        name: "NgWord2",
        supplier: Some(ng_word::ng_word_2),
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000824,
        name: "PlatformConfigIcosaMariko",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000825,
        name: "ApplicationBlackList",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000826,
        name: "RebootlessSystemUpdateVersion",
        supplier: None,
    },
    SystemArchiveDescriptor {
        title_id: 0x0100000000000827,
        name: "ContentActionTable",
        supplier: None,
    },
];

/// Synthesize a system archive for the given title ID.
///
/// Returns the archive as a RomFS VirtualFile, or None if the title ID
/// is unknown or the archive cannot be synthesized.
///
/// Corresponds to upstream `FileSys::SystemArchive::SynthesizeSystemArchive`.
pub fn synthesize_system_archive(title_id: u64) -> Option<VirtualFile> {
    if title_id < SYSTEM_ARCHIVES[0].title_id
        || title_id > SYSTEM_ARCHIVES[SYSTEM_ARCHIVE_COUNT - 1].title_id
    {
        return None;
    }

    let index = (title_id - SYSTEM_ARCHIVE_BASE_TITLE_ID) as usize;
    if index >= SYSTEM_ARCHIVE_COUNT {
        return None;
    }

    let desc = &SYSTEM_ARCHIVES[index];

    log::info!(
        "Synthesizing system archive '{}' (0x{:016X}).",
        desc.name,
        desc.title_id
    );

    let supplier = desc.supplier?;
    let dir = supplier()?;

    // Convert the VirtualDir to a RomFS binary image.
    // Matches upstream: CreateRomFS(dir)
    let romfs = crate::file_sys::romfs::create_romfs(Some(dir), None);
    if romfs.is_none() {
        log::error!("    - System archive RomFS creation failed!");
        return None;
    }

    log::info!("    - System archive generation successful!");
    romfs
}

// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/caps/caps_types.h

/// nn::album::ImageOrientation
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AlbumImageOrientation {
    #[default]
    None = 0,
    Rotate90 = 1,
    Rotate180 = 2,
    Rotate270 = 3,
}

/// nn::album::AlbumReportOption
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlbumReportOption {
    Disable = 0,
    Enable = 1,
    Unknown2 = 2,
    Unknown3 = 3,
}

/// Content type.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum ContentType {
    #[default]
    Screenshot = 0,
    Movie = 1,
    ExtraMovie = 3,
}

/// Album storage location.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum AlbumStorage {
    #[default]
    Nand = 0,
    Sd = 1,
}

/// Screenshot decoder flags.
#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ScreenShotDecoderFlag {
    #[default]
    None = 0,
    EnableFancyUpsampling = 1 << 0,
    EnableBlockSmoothing = 1 << 1,
}

/// nn::capsrv::ShimLibraryVersion
#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShimLibraryVersion {
    Version1 = 1,
}

/// nn::capsrv::AlbumFileDateTime
#[repr(C)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct AlbumFileDateTime {
    pub year: i16,
    pub month: i8,
    pub day: i8,
    pub hour: i8,
    pub minute: i8,
    pub second: i8,
    pub unique_id: i8,
}
const _: () = assert!(core::mem::size_of::<AlbumFileDateTime>() == 0x8);

impl PartialOrd for AlbumFileDateTime {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AlbumFileDateTime {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.year.cmp(&other.year)
            .then(self.month.cmp(&other.month))
            .then(self.day.cmp(&other.day))
            .then(self.hour.cmp(&other.hour))
            .then(self.minute.cmp(&other.minute))
            .then(self.second.cmp(&other.second))
    }
}

/// nn::album::AlbumEntry (AlbumFileEntry)
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct AlbumFileEntry {
    pub size: u64,
    pub hash: u64,
    pub datetime: AlbumFileDateTime,
    pub storage: AlbumStorage,
    pub content: ContentType,
    pub _padding: [u8; 5],
    pub unknown: u8,
}
const _: () = assert!(core::mem::size_of::<AlbumFileEntry>() == 0x20);

/// Album file identifier.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct AlbumFileId {
    pub application_id: u64,
    pub date: AlbumFileDateTime,
    pub storage: AlbumStorage,
    pub content_type: ContentType,
    pub _padding: [u8; 5],
    pub unknown: u8,
}
const _: () = assert!(core::mem::size_of::<AlbumFileId>() == 0x18);

/// nn::capsrv::AlbumEntry
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct AlbumEntry {
    pub entry_size: u64,
    pub file_id: AlbumFileId,
}
const _: () = assert!(core::mem::size_of::<AlbumEntry>() == 0x20);

/// nn::capsrv::ApplicationAlbumEntry
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ApplicationAlbumEntry {
    pub size: u64,
    pub hash: u64,
    pub datetime: AlbumFileDateTime,
    pub storage: AlbumStorage,
    pub content: ContentType,
    pub _padding: [u8; 5],
    pub unknown: u8,
}
const _: () = assert!(core::mem::size_of::<ApplicationAlbumEntry>() == 0x20);

/// nn::capsrv::ApplicationAlbumFileEntry
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ApplicationAlbumFileEntry {
    pub entry: ApplicationAlbumEntry,
    pub datetime: AlbumFileDateTime,
    pub unknown: u64,
}
const _: () = assert!(core::mem::size_of::<ApplicationAlbumFileEntry>() == 0x30);

/// Application data buffer.
#[repr(C)]
#[derive(Clone)]
pub struct ApplicationData {
    pub data: [u8; 0x400],
    pub data_size: u32,
}
const _: () = assert!(core::mem::size_of::<ApplicationData>() == 0x404);

/// Screenshot attribute.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ScreenShotAttribute {
    pub unknown_0: u32,
    pub orientation: AlbumImageOrientation,
    pub unknown_1: u32,
    pub unknown_2: u32,
    pub _padding: [u8; 0x30],
}
const _: () = assert!(core::mem::size_of::<ScreenShotAttribute>() == 0x40);

impl Default for ScreenShotAttribute {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// Screenshot decode option.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct ScreenShotDecodeOption {
    pub flags: ScreenShotDecoderFlag,
    pub _padding: [u8; 0x18],
}
const _: () = assert!(core::mem::size_of::<ScreenShotDecodeOption>() == 0x20);

/// Output structure for LoadAlbumScreenShotImage.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct LoadAlbumScreenShotImageOutput {
    pub width: i64,
    pub height: i64,
    pub attribute: ScreenShotAttribute,
    pub _padding: [u8; 0x400],
}
const _: () = assert!(core::mem::size_of::<LoadAlbumScreenShotImageOutput>() == 0x450);

impl Default for LoadAlbumScreenShotImageOutput {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fs_save_data_types.h

use std::cmp::Ordering;
use std::fmt;

pub type SaveDataId = u64;
pub type SystemSaveDataId = u64;
pub type SystemBcatSaveDataId = SystemSaveDataId;
pub type ProgramId = u64;

/// Save data space identifier.
/// Corresponds to C++ `SaveDataSpaceId` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SaveDataSpaceId {
    System = 0,
    User = 1,
    SdSystem = 2,
    Temporary = 3,
    SdUser = 4,

    ProperSystem = 100,
    SafeMode = 101,
}

/// Save data type.
/// Corresponds to C++ `SaveDataType` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SaveDataType {
    System = 0,
    Account = 1,
    Bcat = 2,
    Device = 3,
    Temporary = 4,
    Cache = 5,
    SystemBcat = 6,
}

/// Save data rank.
/// Corresponds to C++ `SaveDataRank` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SaveDataRank {
    Primary = 0,
    Secondary = 1,
}

/// Save data size pair (normal + journal).
/// Corresponds to C++ `SaveDataSize` struct.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(C)]
pub struct SaveDataSize {
    pub normal: u64,
    pub journal: u64,
}

const _: () = assert!(
    std::mem::size_of::<SaveDataSize>() == 0x10,
    "SaveDataSize has invalid size"
);

/// User ID (128-bit), represented as two u64s to match C++ alignment (8-byte).
/// In C++, `u128` is typically `__int128` or `unsigned __int128` with 8-byte alignment,
/// whereas Rust's `u128` has 16-byte alignment which breaks struct layout parity.
/// Index [0] is the low 64 bits, index [1] is the high 64 bits.
pub type UserId = [u64; 2];

pub const INVALID_SYSTEM_SAVE_DATA_ID: SystemSaveDataId = 0;
pub const INVALID_USER_ID: UserId = [0, 0];

/// Save data flags.
/// Corresponds to C++ `SaveDataFlags` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum SaveDataFlags {
    None = 0,
    KeepAfterResettingSystemSaveData = 1 << 0,
    KeepAfterRefurbishment = 1 << 1,
    KeepAfterResettingSystemSaveDataWithoutUserSaveData = 1 << 2,
    NeedsSecureDelete = 1 << 3,
}

/// Save data meta type.
/// Corresponds to C++ `SaveDataMetaType` enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SaveDataMetaType {
    None = 0,
    Thumbnail = 1,
    ExtensionContext = 2,
}

/// Save data meta info.
/// Corresponds to C++ `SaveDataMetaInfo` struct.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SaveDataMetaInfo {
    pub size: u32,
    pub meta_type: SaveDataMetaType,
    pub _padding: [u8; 0xB],
}

const _: () = assert!(
    std::mem::size_of::<SaveDataMetaInfo>() == 0x10,
    "SaveDataMetaInfo has invalid size"
);

/// Save data creation info.
/// Corresponds to C++ `SaveDataCreationInfo` struct.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SaveDataCreationInfo {
    pub size: i64,
    pub journal_size: i64,
    pub block_size: i64,
    pub owner_id: u64,
    pub flags: u32,
    pub space_id: SaveDataSpaceId,
    pub pseudo: bool,
    pub _padding: [u8; 0x1A],
}

const _: () = assert!(
    std::mem::size_of::<SaveDataCreationInfo>() == 0x40,
    "SaveDataCreationInfo has invalid size"
);

/// Save data attribute key used to identify a save data entry.
/// Corresponds to C++ `SaveDataAttribute` struct.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SaveDataAttribute {
    pub program_id: ProgramId,
    pub user_id: UserId,
    pub system_save_data_id: SystemSaveDataId,
    pub save_type: SaveDataType,
    pub rank: SaveDataRank,
    pub index: u16,
    pub _padding: [u8; 0x1C],
}

const _: () = assert!(
    std::mem::size_of::<SaveDataAttribute>() == 0x40,
    "SaveDataAttribute has invalid size"
);

impl SaveDataAttribute {
    /// Construct a SaveDataAttribute with all parameters.
    pub const fn make(
        program_id: ProgramId,
        save_type: SaveDataType,
        user_id: UserId,
        system_save_data_id: SystemSaveDataId,
        index: u16,
        rank: SaveDataRank,
    ) -> Self {
        Self {
            program_id,
            user_id,
            system_save_data_id,
            save_type,
            rank,
            index,
            _padding: [0u8; 0x1C],
        }
    }

    /// Construct with default rank (Primary) and given index.
    pub const fn make_with_index(
        program_id: ProgramId,
        save_type: SaveDataType,
        user_id: UserId,
        system_save_data_id: SystemSaveDataId,
        index: u16,
    ) -> Self {
        Self::make(
            program_id,
            save_type,
            user_id,
            system_save_data_id,
            index,
            SaveDataRank::Primary,
        )
    }

    /// Construct with default rank (Primary) and index 0.
    pub const fn make_default(
        program_id: ProgramId,
        save_type: SaveDataType,
        user_id: UserId,
        system_save_data_id: SystemSaveDataId,
    ) -> Self {
        Self::make(
            program_id,
            save_type,
            user_id,
            system_save_data_id,
            0,
            SaveDataRank::Primary,
        )
    }

    /// Debug info string matching upstream format.
    pub fn debug_info(&self) -> String {
        format!(
            "[title_id={:016X}, user_id={:016X}{:016X}, save_id={:016X}, type={:02X}, rank={}, index={}]",
            self.program_id,
            self.user_id[1],
            self.user_id[0],
            self.system_save_data_id,
            self.save_type as u8,
            self.rank as u8,
            self.index,
        )
    }
}

impl PartialEq for SaveDataAttribute {
    fn eq(&self, other: &Self) -> bool {
        self.program_id == other.program_id
            && self.user_id == other.user_id
            && self.system_save_data_id == other.system_save_data_id
            && self.save_type as u8 == other.save_type as u8
            && self.rank as u8 == other.rank as u8
            && self.index == other.index
    }
}

impl Eq for SaveDataAttribute {}

impl PartialOrd for SaveDataAttribute {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SaveDataAttribute {
    fn cmp(&self, other: &Self) -> Ordering {
        self.program_id
            .cmp(&other.program_id)
            .then_with(|| self.user_id.cmp(&other.user_id))
            .then_with(|| self.system_save_data_id.cmp(&other.system_save_data_id))
            .then_with(|| self.index.cmp(&other.index))
            .then_with(|| (self.rank as u8).cmp(&(other.rank as u8)))
    }
}

impl fmt::Display for SaveDataAttribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.debug_info())
    }
}

/// Save data extra data.
/// Corresponds to C++ `SaveDataExtraData` struct.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SaveDataExtraData {
    pub attr: SaveDataAttribute,
    pub owner_id: u64,
    pub timestamp: i64,
    pub flags: u32,
    pub _padding0: [u8; 4],
    pub available_size: i64,
    pub journal_size: i64,
    pub commit_id: i64,
    pub _padding1: [u8; 0x190],
}

const _: () = assert!(
    std::mem::size_of::<SaveDataExtraData>() == 0x200,
    "SaveDataExtraData has invalid size"
);

/// Save data filter for querying save data entries.
/// Corresponds to C++ `SaveDataFilter` struct.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SaveDataFilter {
    pub use_program_id: bool,
    pub use_save_data_type: bool,
    pub use_user_id: bool,
    pub use_save_data_id: bool,
    pub use_index: bool,
    pub rank: SaveDataRank,
    pub attribute: SaveDataAttribute,
}

const _: () = assert!(
    std::mem::size_of::<SaveDataFilter>() == 0x48,
    "SaveDataFilter has invalid size"
);

/// Hash salt value.
/// Corresponds to C++ `HashSalt` struct.
pub const HASH_SALT_SIZE: usize = 32;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct HashSalt {
    pub value: [u8; HASH_SALT_SIZE],
}

const _: () = assert!(
    std::mem::size_of::<HashSalt>() == HASH_SALT_SIZE,
    "HashSalt has invalid size"
);

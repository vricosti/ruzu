// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fs_directory.h

/// Maximum length of a directory entry name.
pub const ENTRY_NAME_LENGTH_MAX: usize = 0x300;

/// A directory entry as laid out in HOS IPC.
/// Binary layout must match C++ `DirectoryEntry` exactly (0x310 bytes).
#[derive(Clone)]
#[repr(C)]
pub struct DirectoryEntry {
    pub name: [u8; ENTRY_NAME_LENGTH_MAX + 1],
    pub _padding0: [u8; 3],
    pub entry_type: i8,
    pub _padding1: [u8; 3],
    pub file_size: i64,
}

impl DirectoryEntry {
    /// Create a new `DirectoryEntry` from a name string, type, and size.
    /// Matches the C++ constructor behavior: copies up to `ENTRY_NAME_LENGTH_MAX` bytes
    /// from `view` and null-terminates.
    pub fn new(view: &str, entry_type: i8, entry_size: u64) -> Self {
        let mut entry = Self {
            name: [0u8; ENTRY_NAME_LENGTH_MAX + 1],
            _padding0: [0u8; 3],
            entry_type,
            _padding1: [0u8; 3],
            file_size: entry_size as i64,
        };

        let bytes = view.as_bytes();
        let copy_len = bytes.len().min(ENTRY_NAME_LENGTH_MAX);
        entry.name[..copy_len].copy_from_slice(&bytes[..copy_len]);
        entry.name[copy_len] = 0;

        entry
    }

    /// Get the name as a string slice (up to the first null byte).
    pub fn name_str(&self) -> &str {
        let end = self
            .name
            .iter()
            .position(|&b| b == 0)
            .unwrap_or(self.name.len());
        // Safety: the name was created from valid UTF-8 or ASCII
        std::str::from_utf8(&self.name[..end]).unwrap_or("")
    }
}

const _: () = assert!(
    std::mem::size_of::<DirectoryEntry>() == 0x310,
    "DirectoryEntry struct isn't exactly 0x310 bytes long"
);

// Verify field offsets match upstream static_asserts
const _: () = {
    // offsetof(DirectoryEntry, entry_type) == 0x304
    assert!(std::mem::offset_of!(DirectoryEntry, entry_type) == 0x304);
    // offsetof(DirectoryEntry, file_size) == 0x308
    assert!(std::mem::offset_of!(DirectoryEntry, file_size) == 0x308);
};

/// Opaque directory handle. Corresponds to C++ `DirectoryHandle`.
#[derive(Debug)]
pub struct DirectoryHandle {
    pub handle: *mut std::ffi::c_void,
}

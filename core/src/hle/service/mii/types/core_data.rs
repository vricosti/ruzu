// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mii/types/core_data.h
//! Port of zuyu/src/core/hle/service/mii/types/core_data.cpp
//!
//! CoreData: the core Mii data structure used internally.
//! Size: 0x30 bytes in upstream (packed bit-fields).

use crate::hle::service::mii::mii_types::Nickname;

/// CoreData stores the essential Mii parameters as bit-packed fields.
///
/// Upstream uses C++ bit-fields extensively. Here we store the raw bytes
/// and will add accessor methods matching the upstream getters/setters.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct CoreData {
    /// Raw data storage. Upstream uses bit-fields packed into this space.
    pub data: [u8; 0x30],
}

impl CoreData {
    pub fn new() -> Self {
        Self { data: [0u8; 0x30] }
    }
}

impl Default for CoreData {
    fn default() -> Self {
        Self::new()
    }
}

const _: () = assert!(core::mem::size_of::<CoreData>() == 0x30);

// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/ecommerce_interface.h
//! Port of zuyu/src/core/hle/service/ns/ecommerce_interface.cpp
//!
//! IECommerceInterface — e-commerce operations for NS.

/// IPC command table for IECommerceInterface.
///
/// Corresponds to the function table in upstream ecommerce_interface.cpp.
pub mod commands {
    pub const REQUEST_LINK_DEVICE: u32 = 0;
    pub const REQUEST_CLEANUP_ALL_PRE_INSTALLED_APPLICATIONS: u32 = 1;
    pub const REQUEST_CLEANUP_PRE_INSTALLED_APPLICATION: u32 = 2;
    pub const REQUEST_SYNC_RIGHTS: u32 = 3;
    pub const REQUEST_UNLINK_DEVICE: u32 = 4;
    pub const REQUEST_REVOKE_ALL_E_LICENSE: u32 = 5;
    pub const REQUEST_SYNC_RIGHTS_BASED_ON_ASSIGNED_E_LICENSES: u32 = 6;
}

/// IECommerceInterface.
///
/// Corresponds to `IECommerceInterface` in upstream.
pub struct IECommerceInterface;

impl IECommerceInterface {
    pub fn new() -> Self {
        Self
    }
}

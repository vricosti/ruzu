// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/service_creator.h
//! Port of zuyu/src/core/hle/service/vi/service_creator.cpp

use crate::hle::result::ResultCode;

use super::vi_results;
use super::vi_types::{Permission, Policy};

/// Validate that the given permission/policy combination is allowed.
fn is_valid_service_access(permission: Permission, policy: Policy) -> bool {
    match permission {
        Permission::User => policy == Policy::User,
        Permission::System | Permission::Manager => {
            policy == Policy::User || policy == Policy::Compositor
        }
    }
}

/// Create an IApplicationDisplayService if the permission/policy is valid.
///
/// In upstream C++, this returns a shared_ptr<IApplicationDisplayService>.
/// Here we just validate access and return Ok/Err. The actual service
/// creation depends on the ServiceFramework infrastructure.
pub fn get_application_display_service(
    permission: Permission,
    policy: Policy,
) -> Result<(), ResultCode> {
    if !is_valid_service_access(permission, policy) {
        log::error!("Permission denied for policy {:?}", policy);
        return Err(vi_results::RESULT_PERMISSION_DENIED);
    }

    Ok(())
}

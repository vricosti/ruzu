//! Port of zuyu/src/core/hle/kernel/svc/svc_resource_limit.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for resource limit operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Creates a new resource limit.
pub fn create_resource_limit(out_handle: &mut Handle) -> ResultCode {
    log::debug!("svc::CreateResourceLimit called");

    // TODO: KResourceLimit::Create, Initialize, Register, add to handle table
    *out_handle = 0;
    log::warn!("svc::CreateResourceLimit: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Gets the limit value for a resource.
pub fn get_resource_limit_limit_value(
    out_limit_value: &mut i64,
    resource_limit_handle: Handle,
    which: LimitableResource,
) -> ResultCode {
    log::debug!(
        "svc::GetResourceLimitLimitValue called, handle={:08X}, which={:?}",
        resource_limit_handle, which
    );

    if !is_valid_resource_type(which) {
        return RESULT_INVALID_ENUM_VALUE;
    }

    // TODO: Get resource limit from handle, get limit value
    *out_limit_value = 0;
    log::warn!("svc::GetResourceLimitLimitValue: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Gets the current value for a resource.
pub fn get_resource_limit_current_value(
    out_current_value: &mut i64,
    resource_limit_handle: Handle,
    which: LimitableResource,
) -> ResultCode {
    log::debug!(
        "svc::GetResourceLimitCurrentValue called, handle={:08X}, which={:?}",
        resource_limit_handle, which
    );

    if !is_valid_resource_type(which) {
        return RESULT_INVALID_ENUM_VALUE;
    }

    // TODO: Get resource limit from handle, get current value
    *out_current_value = 0;
    log::warn!("svc::GetResourceLimitCurrentValue: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Sets the limit value for a resource.
pub fn set_resource_limit_limit_value(
    resource_limit_handle: Handle,
    which: LimitableResource,
    limit_value: i64,
) -> ResultCode {
    log::debug!(
        "svc::SetResourceLimitLimitValue called, handle={:08X}, which={:?}, value={}",
        resource_limit_handle, which, limit_value
    );

    if !is_valid_resource_type(which) {
        return RESULT_INVALID_ENUM_VALUE;
    }

    // TODO: Get resource limit from handle, set limit value
    log::warn!("svc::SetResourceLimitLimitValue: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Gets the peak value for a resource. (Unimplemented upstream.)
pub fn get_resource_limit_peak_value(
    _out_peak_value: &mut i64,
    _resource_limit_handle: Handle,
    _which: LimitableResource,
) -> ResultCode {
    log::warn!("svc::GetResourceLimitPeakValue: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

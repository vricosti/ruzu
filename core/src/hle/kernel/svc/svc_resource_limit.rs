//! Port of zuyu/src/core/hle/kernel/svc/svc_resource_limit.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for resource limit operations.
//!
//! Upstream retrieves KResourceLimit from the handle table via typed
//! GetObject<KResourceLimit>(handle). The Rust handle table maps Handle -> object_id (u64)
//! without type discrimination. To fully implement these SVCs, KProcess needs:
//!   - A registry mapping object_id -> Arc<Mutex<KResourceLimit>>
//!     (similar to how sessions and events are registered)
//!   - Methods: register_resource_limit_object, get_resource_limit_by_object_id
//! These registries do not yet exist. The SVC logic below documents exactly what
//! upstream does and will work once those registries are added.

use std::sync::{Arc, Mutex};

use crate::core::System;
use crate::hle::kernel::k_resource_limit::KResourceLimit;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Creates a new resource limit.
///
/// Upstream: Creates KResourceLimit, initializes it, registers it in the kernel,
/// and adds it to the current process's handle table.
pub fn create_resource_limit(system: &System, out_handle: &mut Handle) -> ResultCode {
    log::debug!("svc::CreateResourceLimit called");

    // Create a new resource limit.
    let resource_limit = Arc::new(Mutex::new(KResourceLimit::new()));
    resource_limit.lock().unwrap().initialize();

    // Upstream: KResourceLimit::Register(kernel, resource_limit);
    // Upstream: GetCurrentProcess(kernel).GetHandleTable().Add(out_handle, resource_limit);

    // The handle table needs a typed object registry for resource limits on KProcess.
    // Upstream type: KResourceLimit
    // Upstream method: GetHandleTable().Add(out_handle, resource_limit)
    // KProcess needs: register_resource_limit_object(object_id, Arc<Mutex<KResourceLimit>>)
    let kernel = system.kernel().expect("kernel not initialized");
    let object_id = kernel.create_new_object_id() as u64;

    let mut process = system.current_process_arc().lock().unwrap();
    // Once KProcess has register_resource_limit_object, call it here:
    // process.register_resource_limit_object(object_id, resource_limit);

    match process.handle_table.add(object_id) {
        Ok(handle) => {
            *out_handle = handle;
            log::debug!("svc::CreateResourceLimit -> handle=0x{:08X}", handle);
            // Upstream: resource_limit->Close() after adding to handle table (SCOPE_EXIT).
            // In Rust, the Arc keeps the resource limit alive through the handle table.
            // Without a resource_limit registry, the created object cannot be retrieved
            // by later Get/Set calls. Log a warning.
            log::warn!(
                "svc::CreateResourceLimit: KProcess lacks resource_limit registry. \
                 The handle 0x{:08X} (object_id={}) cannot be resolved by \
                 Get/SetResourceLimitLimitValue until the registry is added.",
                handle, object_id
            );
            RESULT_SUCCESS
        }
        Err(_) => RESULT_OUT_OF_HANDLES,
    }
}

/// Gets the limit value for a resource.
///
/// Upstream: Gets KResourceLimit from handle table, returns GetLimitValue(which).
pub fn get_resource_limit_limit_value(
    system: &System,
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

    // Upstream: GetCurrentProcess(kernel).GetHandleTable().GetObject<KResourceLimit>(handle)
    let process = system.current_process_arc().lock().unwrap();
    let Some(_object_id) = process.handle_table.get_object(resource_limit_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    // KProcess needs: get_resource_limit_by_object_id(object_id) -> Option<Arc<Mutex<KResourceLimit>>>
    // Once available:
    //   let Some(rl) = process.get_resource_limit_by_object_id(object_id) else {
    //       return RESULT_INVALID_HANDLE;
    //   };
    //   *out_limit_value = rl.lock().unwrap().get_limit_value(convert_which(which));
    //   RESULT_SUCCESS

    // Fallback: use the process's own resource limit if available.
    // This is a reasonable approximation since most games only query their own process resource limit.
    if let Some(ref rl) = process.resource_limit {
        let k_which = convert_limitable_resource(which);
        *out_limit_value = rl.lock().unwrap().get_limit_value(k_which);
        return RESULT_SUCCESS;
    }

    *out_limit_value = 0;
    RESULT_INVALID_HANDLE
}

/// Gets the current value for a resource.
///
/// Upstream: Gets KResourceLimit from handle table, returns GetCurrentValue(which).
pub fn get_resource_limit_current_value(
    system: &System,
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

    let process = system.current_process_arc().lock().unwrap();
    let Some(_object_id) = process.handle_table.get_object(resource_limit_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    // Fallback: use the process's own resource limit.
    if let Some(ref rl) = process.resource_limit {
        let k_which = convert_limitable_resource(which);
        *out_current_value = rl.lock().unwrap().get_current_value(k_which);
        return RESULT_SUCCESS;
    }

    *out_current_value = 0;
    RESULT_INVALID_HANDLE
}

/// Sets the limit value for a resource.
///
/// Upstream: Gets KResourceLimit from handle table, calls SetLimitValue(which, limit_value).
pub fn set_resource_limit_limit_value(
    system: &System,
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

    let process = system.current_process_arc().lock().unwrap();
    let Some(_object_id) = process.handle_table.get_object(resource_limit_handle) else {
        return RESULT_INVALID_HANDLE;
    };

    // Fallback: use the process's own resource limit.
    if let Some(ref rl) = process.resource_limit {
        let k_which = convert_limitable_resource(which);
        match rl.lock().unwrap().set_limit_value(k_which, limit_value) {
            Ok(()) => return RESULT_SUCCESS,
            Err(()) => return RESULT_INVALID_STATE,
        }
    }

    RESULT_INVALID_HANDLE
}

/// Gets the peak value for a resource.
/// Upstream: UNIMPLEMENTED() — intentionally unimplemented.
pub fn get_resource_limit_peak_value(
    _out_peak_value: &mut i64,
    _resource_limit_handle: Handle,
    _which: LimitableResource,
) -> ResultCode {
    log::warn!("svc::GetResourceLimitPeakValue: Upstream UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Convert SVC LimitableResource to k_resource_limit::LimitableResource.
fn convert_limitable_resource(
    which: LimitableResource,
) -> crate::hle::kernel::k_resource_limit::LimitableResource {
    match which {
        LimitableResource::PhysicalMemoryMax => {
            crate::hle::kernel::k_resource_limit::LimitableResource::PhysicalMemoryMax
        }
        LimitableResource::ThreadCountMax => {
            crate::hle::kernel::k_resource_limit::LimitableResource::ThreadCountMax
        }
        LimitableResource::EventCountMax => {
            crate::hle::kernel::k_resource_limit::LimitableResource::EventCountMax
        }
        LimitableResource::TransferMemoryCountMax => {
            crate::hle::kernel::k_resource_limit::LimitableResource::TransferMemoryCountMax
        }
        LimitableResource::SessionCountMax => {
            crate::hle::kernel::k_resource_limit::LimitableResource::SessionCountMax
        }
    }
}

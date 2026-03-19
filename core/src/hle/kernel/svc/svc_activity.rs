//! Port of zuyu/src/core/hle/kernel/svc/svc_activity.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for thread and process activity.

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Sets the thread activity.
///
/// Validates the activity, gets the thread from its handle,
/// checks that it belongs to the current process and is not the current thread,
/// then sets the activity.
pub fn set_thread_activity(
    system: &System,
    thread_handle: Handle,
    thread_activity: ThreadActivity,
) -> ResultCode {
    log::debug!(
        "svc::SetThreadActivity called, handle=0x{:08X}, activity={:?}",
        thread_handle,
        thread_activity
    );

    // Validate the activity.
    let is_valid = matches!(
        thread_activity,
        ThreadActivity::Runnable | ThreadActivity::Paused
    );
    if !is_valid {
        return RESULT_INVALID_ENUM_VALUE;
    }

    let Some(current_thread_id) = system.current_thread_id() else {
        return RESULT_INVALID_HANDLE;
    };
    let process = system.current_process_arc().lock().unwrap();
    let Some(object_id) = process.handle_table.get_object(thread_handle) else {
        return RESULT_INVALID_HANDLE;
    };
    let Some(thread) = process.get_thread_by_object_id(object_id) else {
        return RESULT_INVALID_HANDLE;
    };
    drop(process);

    if thread.lock().unwrap().get_thread_id() == current_thread_id {
        return RESULT_BUSY;
    }

    let activity = match thread_activity {
        ThreadActivity::Runnable => 0,
        ThreadActivity::Paused => 1,
    };
    let result = ResultCode::new(thread.lock().unwrap().set_activity(activity));
    result
}

/// Sets the process activity. (Unimplemented upstream.)
pub fn set_process_activity(
    _process_handle: Handle,
    _process_activity: ProcessActivity,
) -> ResultCode {
    log::warn!("svc::SetProcessActivity: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use super::*;
    use crate::core::System;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_thread::{KThread, SuspendType};
    use crate::hle::kernel::svc::svc_thread;

    fn test_system() -> System {
        let mut system = System::new_for_test();

        let mut process = KProcess::new();
        process.capabilities.core_mask = 0xF;
        process.capabilities.priority_mask = u64::MAX;
        process.flags = 0;
        process.allocate_code_memory(0x200000, 0x20000);
        process.initialize_handle_table();
        process.initialize_thread_local_region_base(0x240000);

        let process = Arc::new(Mutex::new(process));
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        let scheduler = Arc::new(Mutex::new(crate::hle::kernel::k_scheduler::KScheduler::new(0)));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.initialize_main_thread(0x200000, 0x250000, 0, 0x23f000, &process, 1, 1, false);
            thread.set_priority(44);
            thread.set_base_priority(44);
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(current_thread.clone());
        scheduler.lock().unwrap().initialize(1, 0, 0);

        let shared_memory = process.lock().unwrap().get_shared_memory();

        system.set_current_process_arc(process);
        system.set_scheduler_arc(scheduler);
        system.set_shared_process_memory(shared_memory);
        system.set_runtime_program_id(1);
        system.set_runtime_64bit(false);
        system
    }

    #[test]
    fn set_thread_activity_pauses_and_resumes_non_current_thread() {
        let system = test_system();
        let mut handle = 0;
        assert_eq!(
            svc_thread::create_thread(&system, &mut handle, 0x201000, 0x1234, 0x260000, 44, 0),
            RESULT_SUCCESS
        );
        assert_eq!(svc_thread::start_thread(&system, handle), RESULT_SUCCESS);

        assert_eq!(
            set_thread_activity(&system, handle, ThreadActivity::Paused),
            RESULT_SUCCESS
        );

        let process = system.current_process_arc().lock().unwrap();
        let object_id = process.handle_table.get_object(handle).unwrap();
        let thread = process.get_thread_by_object_id(object_id).unwrap();
        drop(process);

        assert!(thread.lock().unwrap().is_suspend_requested_type(SuspendType::Thread));

        assert_eq!(
            set_thread_activity(&system, handle, ThreadActivity::Runnable),
            RESULT_SUCCESS
        );
        assert!(!thread.lock().unwrap().is_suspend_requested_type(SuspendType::Thread));
    }
}

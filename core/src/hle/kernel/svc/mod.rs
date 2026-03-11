//! Port of zuyu/src/core/hle/kernel/svc/
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! Supervisor Call (SVC) handler implementations.
//! Each submodule corresponds to a C++ file in the upstream svc/ directory.

pub mod svc_results;
pub mod svc_types;

pub mod svc_activity;
pub mod svc_address_arbiter;
pub mod svc_address_translation;
pub mod svc_cache;
pub mod svc_code_memory;
pub mod svc_condition_variable;
pub mod svc_debug;
pub mod svc_debug_string;
pub mod svc_device_address_space;
pub mod svc_event;
pub mod svc_exception;
pub mod svc_info;
pub mod svc_insecure_memory;
pub mod svc_interrupt_event;
pub mod svc_io_pool;
pub mod svc_ipc;
pub mod svc_kernel_debug;
pub mod svc_light_ipc;
pub mod svc_lock;
pub mod svc_memory;
pub mod svc_physical_memory;
pub mod svc_port;
pub mod svc_power_management;
pub mod svc_process;
pub mod svc_process_memory;
pub mod svc_processor;
pub mod svc_query_memory;
pub mod svc_register;
pub mod svc_resource_limit;
pub mod svc_secure_monitor_call;
pub mod svc_session;
pub mod svc_shared_memory;
pub mod svc_synchronization;
pub mod svc_thread;
pub mod svc_thread_profiler;
pub mod svc_tick;
pub mod svc_transfer_memory;

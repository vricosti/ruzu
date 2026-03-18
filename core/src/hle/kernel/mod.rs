//! Port of zuyu/src/core/hle/kernel/
//! Status: In progress
//! Derniere synchro: 2026-03-11
//!
//! Kernel subsystem modules.

// Type identification
pub mod k_class_token;
pub mod k_typed_address;

// Base kernel object hierarchy
pub mod k_auto_object;
pub mod k_synchronization_object;

// Slab allocators
pub mod k_slab_heap;
pub mod k_dynamic_slab_heap;
pub mod slab_helpers;

// Locks and synchronization primitives
pub mod k_spin_lock;
pub mod k_scoped_lock;
pub mod k_light_lock;
pub mod k_light_condition_variable;
pub mod k_scheduler_lock;
pub mod k_scoped_scheduler_lock_and_sleep;

// Scheduling and thread management support
pub mod k_affinity_mask;
pub mod k_priority_queue;
pub mod k_timer_task;

// Worker tasks
pub mod k_worker_task;
pub mod k_worker_task_manager;

// Memory types
pub mod memory_types;

// Memory management infrastructure
pub mod k_address_space_info;
pub mod k_dynamic_page_manager;
pub mod k_dynamic_resource_manager;
pub mod k_memory_block;
pub mod k_memory_block_manager;
pub mod k_memory_layout;
pub mod k_memory_manager;
pub mod k_memory_region;
pub mod k_memory_region_type;
pub mod k_page_bitmap;
pub mod k_page_buffer;
pub mod k_page_group;
pub mod k_page_heap;
pub mod k_page_table;
pub mod k_page_table_base;
pub mod k_page_table_manager;
pub mod k_page_table_slab_heap;
pub mod k_resource_limit;
pub mod k_system_resource;
pub mod physical_memory;

// SVC (Supervisor Call) types and results
pub mod svc_common;
pub mod svc_types;
pub mod svc_results;
pub mod svc_version;

// SVC handler implementations
pub mod svc;

// SVC dispatch table (port of svc.h/svc.cpp)
pub mod svc_dispatch;

// Board-selected KSystemControl re-export (port of k_system_control.h)
pub mod k_system_control;

// Previously ported kernel modules
pub mod k_capabilities;
pub mod k_client_port;
pub mod k_client_session;
pub mod k_event;
pub mod k_handle_table;
pub mod k_light_client_session;
pub mod k_light_server_session;
pub mod k_light_session;
pub mod k_port;
pub mod k_process;
pub mod k_process_page_table;
pub mod k_readable_event;
pub mod k_scheduler;
pub mod k_server_port;
pub mod k_server_session;
pub mod k_session;
pub mod k_session_request;
pub mod k_thread;
pub mod k_thread_local_page;
pub mod k_thread_queue;

// Board-specific kernel modules (board/nintendo/nx/).
pub mod board;

// Init-time kernel modules.
pub mod init;

// Kernel top-level modules.
pub mod code_set;
pub mod global_scheduler_context;
pub mod initial_process;
pub mod k_address_arbiter;
pub mod k_auto_object_container;
pub mod k_code_memory;
pub mod k_condition_variable;
pub mod k_debug;
pub mod k_device_address_space;
pub mod k_event_info;
pub mod k_hardware_timer;
pub mod k_hardware_timer_base;
pub mod k_interrupt_manager;
pub mod k_object_name;
pub mod k_scoped_resource_reservation;
pub mod k_shared_memory;
pub mod k_shared_memory_info;
pub mod k_trace;
pub mod k_transfer_memory;
pub mod kernel;
pub mod message_buffer;
pub mod physical_core;
#[cfg(feature = "debug-logs")]
pub mod physical_core_log;

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

// Utility types
pub mod alignment;
pub mod bit_field;
pub mod bit_util;
pub mod div_ceil;
pub mod error;
pub mod fixed_point;
pub mod math_util;
pub mod overflow;
pub mod swap;
pub mod typed_address;
pub mod types;
pub mod uint128;

// Settings
pub mod param_package;
pub mod settings;
pub mod settings_common;
pub mod settings_enums;
pub mod settings_input;

// Container types
pub mod bounded_threadsafe_queue;
pub mod intrusive_list;
pub mod lru_cache;
pub mod range_map;
pub mod ring_buffer;
pub mod scratch_buffer;
pub mod slot_vector;
pub mod thread_queue_list;
pub mod threadsafe_queue;

// Memory subsystem
pub mod address_space;
pub mod free_region_manager;
pub mod host_memory;
pub mod multi_level_page_table;
pub mod page_table;
pub mod virtual_buffer;

// Threading subsystem
pub mod fiber;
pub mod spin_lock;
pub mod thread;

// Compression
pub mod lz4_compression;
pub mod zstd_compression;

// System utilities
pub mod cityhash;
pub mod hex_util;
pub mod logging;
pub mod steady_clock;
pub mod string_util;
pub mod tiny_mt;
pub mod uuid;
pub mod wall_clock;

pub use error::ResultCode;
pub use types::*;

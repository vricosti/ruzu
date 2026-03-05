// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

pub mod address_space_info;
pub mod capabilities;
pub mod code_set;
pub mod device_memory_manager;
pub mod guest_memory;
pub mod handle_table;
pub mod kernel;
pub mod memory;
pub mod memory_block;
pub mod memory_layout;
pub mod memory_manager;
pub mod objects;
pub mod page_table_kernel;
pub mod process;
pub mod scheduler;
pub mod svc;
pub mod thread;

pub use device_memory_manager::DeviceMemoryManager as DeviceMemMgr;
pub use kernel::{IpcHandler, IpcHandlerResult, KernelCore};
pub use memory::Memory;
pub use memory_manager::MemoryManager;
pub use process::KProcess;
pub use thread::KThread;

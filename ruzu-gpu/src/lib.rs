// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU subsystem for ruzu.
//!
//! Provides GPU memory management, GPFIFO command processing, engine dispatch,
//! syncpoint synchronization, and rendering backend abstraction.

pub mod backend;
pub mod command_processor;
pub mod engines;
pub mod gpu_context;
pub mod macro_interpreter;
pub mod memory_manager;
pub mod renderer;
pub mod surface;
pub mod swapchain;
pub mod syncpoint;

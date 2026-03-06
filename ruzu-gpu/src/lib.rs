// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GPU subsystem for ruzu.
//!
//! Provides GPU memory management, GPFIFO command processing, engine dispatch,
//! syncpoint synchronization, and rendering backend abstraction.

pub mod command_processor;
pub mod descriptor_table;
pub mod engines;
pub mod gpu_context;
pub mod macro_interpreter;
pub mod memory_manager;
pub mod rasterizer;
pub mod rasterizer_interface;
pub mod renderer_null;
pub mod renderer_opengl;
pub mod renderer_vulkan;
pub mod shader;
pub mod shader_recompiler;
pub mod surface;
pub mod swapchain;
pub mod swizzle;
pub mod syncpoint;

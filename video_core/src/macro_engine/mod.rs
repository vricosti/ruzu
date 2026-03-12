// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/macro/` — Maxwell Macro Execution (MME) subsystem.
//!
//! The directory is named `macro_engine` because `macro` is a reserved keyword
//! in Rust. Upstream C++ namespace: `Tegra::Macro`.

pub mod macro_engine;
pub mod macro_hle;
pub mod macro_interpreter;
pub mod macro_jit_x64;

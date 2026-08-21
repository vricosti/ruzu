// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of video_core/engines/sw_blitter/
//!
//! Software blitting engine — provides a CPU-side fallback for 2D surface
//! copies when the hardware rasterizer cannot handle the operation.

pub mod blitter;
pub mod converter;

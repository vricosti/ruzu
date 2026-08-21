// SPDX-FileCopyrightText: 2013 Jorge Jimenez (jorge@iryoku.com)
// SPDX-FileCopyrightText: 2013 Jose I. Echevarria (joseignacioechevarria@gmail.com)
// SPDX-FileCopyrightText: 2013 Belen Masia (bmasia@unizar.es)
// SPDX-FileCopyrightText: 2013 Fernando Navarro (fernandn@microsoft.com)
// SPDX-FileCopyrightText: 2013 Diego Gutierrez (diegog@unizar.es)
// SPDX-License-Identifier: MIT

//! Port of video_core/smaa_area_tex.h
//!
//! SMAA area texture data, stored in R8G8 format.
//!
//! Load it in the following format:
//!  - DX9:  D3DFMT_A8L8
//!  - DX10: DXGI_FORMAT_R8G8_UNORM

pub const AREATEX_WIDTH: u32 = 160;
pub const AREATEX_HEIGHT: u32 = 560;
pub const AREATEX_PITCH: u32 = AREATEX_WIDTH * 2;
pub const AREATEX_SIZE: u32 = AREATEX_HEIGHT * AREATEX_PITCH;

/// The raw area texture bytes (179200 bytes = 160 * 560 * 2).
pub static AREA_TEX_BYTES: &[u8; 179200] = include_bytes!("smaa_area_tex.bin");

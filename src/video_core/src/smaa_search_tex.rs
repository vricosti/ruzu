// SPDX-FileCopyrightText: 2013 Jorge Jimenez (jorge@iryoku.com)
// SPDX-FileCopyrightText: 2013 Jose I. Echevarria (joseignacioechevarria@gmail.com)
// SPDX-FileCopyrightText: 2013 Belen Masia (bmasia@unizar.es)
// SPDX-FileCopyrightText: 2013 Fernando Navarro (fernandn@microsoft.com)
// SPDX-FileCopyrightText: 2013 Diego Gutierrez (diegog@unizar.es)
// SPDX-License-Identifier: MIT

//! Port of video_core/smaa_search_tex.h
//!
//! SMAA search texture data, stored in R8 format.
//!
//! Load it in the following format:
//!  - DX9:  D3DFMT_L8
//!  - DX10: DXGI_FORMAT_R8_UNORM

pub const SEARCHTEX_WIDTH: u32 = 64;
pub const SEARCHTEX_HEIGHT: u32 = 16;
pub const SEARCHTEX_PITCH: u32 = SEARCHTEX_WIDTH;
pub const SEARCHTEX_SIZE: u32 = SEARCHTEX_HEIGHT * SEARCHTEX_PITCH;

/// The raw search texture bytes (1024 bytes = 64 * 16).
pub static SEARCH_TEX_BYTES: &[u8; 1024] = include_bytes!("smaa_search_tex.bin");

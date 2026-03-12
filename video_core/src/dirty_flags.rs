// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/dirty_flags.h and video_core/dirty_flags.cpp
//!
//! Dirty flag indices and setup for Maxwell3D state tracking.

/// Dirty flag indices matching upstream `VideoCommon::Dirty` enum.
pub mod flags {
    pub const NULL_ENTRY: u8 = 0;
    pub const DESCRIPTORS: u8 = 1;
    pub const RENDER_TARGETS: u8 = 2;
    pub const RENDER_TARGET_CONTROL: u8 = 3;
    pub const COLOR_BUFFER0: u8 = 4;
    pub const COLOR_BUFFER1: u8 = 5;
    pub const COLOR_BUFFER2: u8 = 6;
    pub const COLOR_BUFFER3: u8 = 7;
    pub const COLOR_BUFFER4: u8 = 8;
    pub const COLOR_BUFFER5: u8 = 9;
    pub const COLOR_BUFFER6: u8 = 10;
    pub const COLOR_BUFFER7: u8 = 11;
    pub const ZETA_BUFFER: u8 = 12;
    pub const RESCALE_VIEWPORTS: u8 = 13;
    pub const RESCALE_SCISSORS: u8 = 14;
    pub const VERTEX_BUFFERS: u8 = 15;
    pub const VERTEX_BUFFER0: u8 = 16;
    // VertexBuffer31 = VertexBuffer0 + 31 = 47
    pub const VERTEX_BUFFER31: u8 = VERTEX_BUFFER0 + 31;
    pub const INDEX_BUFFER: u8 = 48;
    pub const SHADERS: u8 = 49;
    // Special entries
    pub const DEPTH_BIAS_GLOBAL: u8 = 50;
    pub const LAST_COMMON_ENTRY: u8 = 51;
}

/// A dirty state table: an array of u8 flag indices indexed by register offset.
pub type DirtyTable = Vec<u8>;

/// Two-table set used by Maxwell3D dirty state tracking.
/// tables[0] = per-entry flag, tables[1] = group flag.
pub type DirtyTables = [DirtyTable; 2];

/// Fill a block of entries in a single table with a given dirty index.
pub fn fill_block(table: &mut DirtyTable, begin: usize, num: usize, dirty_index: u8) {
    for i in begin..begin + num {
        if i < table.len() {
            table[i] = dirty_index;
        }
    }
}

/// Fill a block of entries in both tables with respective dirty indices.
pub fn fill_block_both(
    tables: &mut DirtyTables,
    begin: usize,
    num: usize,
    index_a: u8,
    index_b: u8,
) {
    fill_block(&mut tables[0], begin, num, index_a);
    fill_block(&mut tables[1], begin, num, index_b);
}

/// Sets up dirty flags for all Maxwell3D register ranges.
///
/// This mirrors the upstream `SetupDirtyFlags` function. The actual register offsets
/// depend on the Maxwell3D register layout which is defined in the engines module.
/// This is a stub that will be filled in once the engine register definitions are ported.
pub fn setup_dirty_flags(tables: &mut DirtyTables) {
    // TODO: Implement once Maxwell3D register layout (Regs) is ported.
    // Upstream calls:
    //   SetupDirtyVertexBuffers(tables)
    //   SetupIndexBuffer(tables)
    //   SetupDirtyDescriptors(tables)
    //   SetupDirtyRenderTargets(tables)
    //   SetupDirtyShaders(tables)
    let _ = tables;
    todo!("setup_dirty_flags requires Maxwell3D register layout");
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/dirty_flags.h and video_core/dirty_flags.cpp
//!
//! Dirty flag indices and setup for Maxwell3D state tracking.

use crate::engines::maxwell_3d::{
    PIPELINE_BASE, RT_BASE, RT_CONTROL, RT_STRIDE, SURFACE_CLIP_BASE, TEX_HEADER_POOL_BASE,
    TEX_SAMPLER_POOL_BASE, VERTEX_STREAM_BASE, VERTEX_STREAM_LIMIT_BASE, VERTEX_STREAM_STRIDE,
    ZETA_BASE, ZETA_ENABLE, ZETA_SIZE_BASE,
};

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
pub fn setup_dirty_flags(tables: &mut DirtyTables) {
    setup_dirty_vertex_buffers(tables);
    setup_index_buffer(tables);
    setup_dirty_descriptors(tables);
    setup_dirty_render_targets(tables);
    setup_dirty_shaders(tables);
}

fn setup_dirty_vertex_buffers(tables: &mut DirtyTables) {
    const NUM_VERTEX_ARRAYS: usize = 32;
    const NUM_VERTEX_STREAM_WORDS: usize = 4;
    const NUM_VERTEX_STREAM_LIMIT_WORDS: usize = 2;
    const NUM_ARRAY_WORDS_DIRTY: usize = 3;

    for i in 0..NUM_VERTEX_ARRAYS {
        let array_offset = VERTEX_STREAM_BASE as usize
            + i * NUM_VERTEX_STREAM_WORDS * VERTEX_STREAM_STRIDE as usize / 4;
        let limit_offset = VERTEX_STREAM_LIMIT_BASE as usize + i * NUM_VERTEX_STREAM_LIMIT_WORDS;
        let per_buffer = flags::VERTEX_BUFFER0 + i as u8;
        fill_block_both(
            tables,
            array_offset,
            NUM_ARRAY_WORDS_DIRTY,
            per_buffer,
            flags::VERTEX_BUFFERS,
        );
        fill_block_both(
            tables,
            limit_offset,
            NUM_VERTEX_STREAM_LIMIT_WORDS,
            per_buffer,
            flags::VERTEX_BUFFERS,
        );
    }
}

fn setup_index_buffer(tables: &mut DirtyTables) {
    const INDEX_BUFFER_WORDS: usize = 7;
    fill_block(
        &mut tables[0],
        crate::engines::maxwell_3d::IB_BASE as usize,
        INDEX_BUFFER_WORDS,
        flags::INDEX_BUFFER,
    );
}

fn setup_dirty_descriptors(tables: &mut DirtyTables) {
    const DESCRIPTOR_POOL_WORDS: usize = 3;
    fill_block(
        &mut tables[0],
        TEX_HEADER_POOL_BASE as usize,
        DESCRIPTOR_POOL_WORDS,
        flags::DESCRIPTORS,
    );
    fill_block(
        &mut tables[0],
        TEX_SAMPLER_POOL_BASE as usize,
        DESCRIPTOR_POOL_WORDS,
        flags::DESCRIPTORS,
    );
}

fn setup_dirty_render_targets(tables: &mut DirtyTables) {
    const NUM_RENDER_TARGETS: usize = 8;
    const RENDER_TARGET_WORDS: usize = 0x40 / 4;
    const SURFACE_CLIP_WORDS: usize = 0x8 / 4;
    const ZETA_WORDS: usize = 0x14 / 4;
    const ZETA_SIZE_WORDS: usize = 0xC / 4;

    let rt_begin = RT_BASE as usize;
    let rt_num = RENDER_TARGET_WORDS * NUM_RENDER_TARGETS;
    for rt in 0..NUM_RENDER_TARGETS {
        fill_block(
            &mut tables[0],
            rt_begin + rt * RT_STRIDE as usize,
            RENDER_TARGET_WORDS,
            flags::COLOR_BUFFER0 + rt as u8,
        );
    }
    fill_block(&mut tables[1], rt_begin, rt_num, flags::RENDER_TARGETS);
    fill_block(
        &mut tables[0],
        SURFACE_CLIP_BASE as usize,
        SURFACE_CLIP_WORDS,
        flags::RENDER_TARGETS,
    );

    tables[0][RT_CONTROL as usize] = flags::RENDER_TARGETS;
    tables[1][RT_CONTROL as usize] = flags::RENDER_TARGET_CONTROL;

    tables[0][ZETA_ENABLE as usize] = flags::ZETA_BUFFER;
    tables[1][ZETA_ENABLE as usize] = flags::RENDER_TARGETS;
    fill_block(
        &mut tables[0],
        ZETA_SIZE_BASE as usize,
        ZETA_SIZE_WORDS,
        flags::ZETA_BUFFER,
    );
    fill_block(
        &mut tables[1],
        ZETA_SIZE_BASE as usize,
        ZETA_SIZE_WORDS,
        flags::RENDER_TARGETS,
    );
    fill_block(
        &mut tables[0],
        ZETA_BASE as usize,
        ZETA_WORDS,
        flags::ZETA_BUFFER,
    );
    fill_block(
        &mut tables[1],
        ZETA_BASE as usize,
        ZETA_WORDS,
        flags::RENDER_TARGETS,
    );
}

fn setup_dirty_shaders(tables: &mut DirtyTables) {
    const MAX_SHADER_PROGRAMS: usize = 6;
    const PIPELINE_WORDS: usize = 0x40 / 4;
    fill_block(
        &mut tables[0],
        PIPELINE_BASE as usize,
        PIPELINE_WORDS * MAX_SHADER_PROGRAMS,
        flags::SHADERS,
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn setup_dirty_flags_marks_core_upstream_ranges() {
        let mut tables = [
            vec![flags::NULL_ENTRY; 0xE00],
            vec![flags::NULL_ENTRY; 0xE00],
        ];
        setup_dirty_flags(&mut tables);

        assert_eq!(
            tables[0][crate::engines::maxwell_3d::IB_BASE as usize],
            flags::INDEX_BUFFER
        );
        assert_eq!(tables[0][TEX_HEADER_POOL_BASE as usize], flags::DESCRIPTORS);
        assert_eq!(tables[0][RT_BASE as usize], flags::COLOR_BUFFER0);
        assert_eq!(tables[1][RT_BASE as usize], flags::RENDER_TARGETS);
        assert_eq!(tables[0][RT_CONTROL as usize], flags::RENDER_TARGETS);
        assert_eq!(tables[1][RT_CONTROL as usize], flags::RENDER_TARGET_CONTROL);
        assert_eq!(tables[0][ZETA_BASE as usize], flags::ZETA_BUFFER);
        assert_eq!(tables[1][ZETA_BASE as usize], flags::RENDER_TARGETS);
        assert_eq!(tables[0][PIPELINE_BASE as usize], flags::SHADERS);
    }
}

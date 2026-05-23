// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `gl_state_tracker.h` / `gl_state_tracker.cpp`.
//!
//! Tracks OpenGL state changes to avoid redundant GL calls.
//! Uses dirty flags indexed by the `Dirty` enum constants to selectively
//! re-apply state per draw call.

use std::ffi::c_void;
use std::sync::OnceLock;

use gl::types::{GLenum, GLuint};

use crate::dirty_flags::{fill_block, setup_dirty_flags, DirtyTables};
use crate::engines::maxwell_3d::{
    ALPHA_TEST_ENABLED, ALPHA_TEST_FUNC, ALPHA_TEST_REF, ANTI_ALIAS_ALPHA_CONTROL, BLEND_BASE,
    BLEND_COLOR_BASE, BLEND_PER_TARGET_BASE, BLEND_PER_TARGET_ENABLED, BLEND_PER_TARGET_STRIDE,
    COLOR_MASK_BASE, COLOR_MASK_COMMON, CULL_FACE, CULL_TEST_ENABLE, DEPTH_BIAS,
    DEPTH_BIAS_CLAMP, DEPTH_MODE, DEPTH_TEST_ENABLE, DEPTH_TEST_FUNC, DEPTH_WRITE_ENABLE,
    FILL_VIA_TRIANGLE_MODE, FRAG_COLOR_CLAMP, FRAMEBUFFER_SRGB, FRONT_FACE, LINE_ANTI_ALIAS_ENABLE,
    LINE_WIDTH_ALIASED, LINE_WIDTH_SMOOTH, LOGIC_OP, LOGIC_OP_WORDS, NUM_VERTEX_ATTRIBS, POINT_SIZE,
    POINT_SIZE_ATTRIBUTE, POINT_SPRITE_ENABLE, POLYGON_MODE_BACK, POLYGON_MODE_FRONT,
    POLYGON_OFFSET_FILL_ENABLE, POLYGON_OFFSET_LINE_ENABLE, POLYGON_OFFSET_POINT_ENABLE,
    PRIMITIVE_RESTART_BASE, PRIMITIVE_RESTART_WORDS, RASTERIZE_ENABLE, SCISSOR_BASE,
    SCISSOR_STRIDE, SLOPE_SCALE_DEPTH_BIAS, STENCIL_BACK_FUNC_MASK, STENCIL_BACK_MASK,
    STENCIL_BACK_OP_BASE, STENCIL_BACK_REF, STENCIL_ENABLE, STENCIL_FRONT_FUNC_MASK,
    STENCIL_FRONT_MASK, STENCIL_FRONT_OP_BASE, STENCIL_FRONT_REF, STENCIL_TWO_SIDE_ENABLE,
    USER_CLIP_ENABLE, VERTEX_ATTRIB_BASE, VERTEX_STREAM_BASE, VERTEX_STREAM_INSTANCE_BASE,
    VERTEX_STREAM_STRIDE, VIEWPORT_BASE, VIEWPORT_CLIP_CONTROL, VIEWPORT_SCALE_OFFSET_ENABLED,
    VIEWPORT_STRIDE, VP_TRANSFORM_BASE, VP_TRANSFORM_STRIDE, WINDOW_ORIGIN,
};

type GlMaterialfv = unsafe extern "system" fn(GLenum, GLenum, *const f32);

const GL_AMBIENT: GLenum = 0x1200;

static GL_MATERIALFV: OnceLock<Option<GlMaterialfv>> = OnceLock::new();

fn set_table(table: &mut [u8], offset: u32, dirty_index: u8) {
    if let Some(entry) = table.get_mut(offset as usize) {
        *entry = dirty_index;
    }
}

fn setup_dirty_color_masks(tables: &mut DirtyTables) {
    const NUM_RENDER_TARGETS: usize = 8;
    set_table(&mut tables[0], COLOR_MASK_COMMON, dirty::COLOR_MASK_COMMON);
    for rt in 0..NUM_RENDER_TARGETS {
        fill_block(
            &mut tables[0],
            COLOR_MASK_BASE as usize + rt,
            1,
            dirty::COLOR_MASK_0 + rt as u8,
        );
    }
    fill_block(
        &mut tables[1],
        COLOR_MASK_BASE as usize,
        NUM_RENDER_TARGETS,
        dirty::COLOR_MASKS,
    );
}

fn setup_dirty_vertex_instances(tables: &mut DirtyTables) {
    const NUM_VERTEX_ARRAYS: usize = 32;
    const INSTANCE_BASE_OFFSET: usize = 3;
    for index in 0..NUM_VERTEX_ARRAYS {
        let array_offset =
            VERTEX_STREAM_BASE as usize + index * VERTEX_STREAM_STRIDE as usize;
        let instance_array_offset = array_offset + INSTANCE_BASE_OFFSET;
        let dirty_index = dirty::VERTEX_INSTANCE_0 + index as u8;
        set_table(&mut tables[0], instance_array_offset as u32, dirty_index);
        set_table(&mut tables[1], instance_array_offset as u32, dirty::VERTEX_INSTANCES);

        let instance_offset = VERTEX_STREAM_INSTANCE_BASE as usize + index;
        set_table(&mut tables[0], instance_offset as u32, dirty_index);
        set_table(&mut tables[1], instance_offset as u32, dirty::VERTEX_INSTANCES);
    }
}

fn setup_dirty_vertex_format(tables: &mut DirtyTables) {
    for index in 0..NUM_VERTEX_ATTRIBS as usize {
        fill_block(
            &mut tables[0],
            VERTEX_ATTRIB_BASE as usize + index,
            1,
            dirty::VERTEX_FORMAT_0 + index as u8,
        );
    }
    fill_block(
        &mut tables[1],
        VERTEX_ATTRIB_BASE as usize,
        NUM_VERTEX_ATTRIBS as usize,
        dirty::VERTEX_FORMATS,
    );
}

fn setup_dirty_viewports(tables: &mut DirtyTables) {
    const NUM_VIEWPORTS: usize = 16;
    for index in 0..NUM_VIEWPORTS {
        let transform_offset = VP_TRANSFORM_BASE as usize + index * VP_TRANSFORM_STRIDE as usize;
        let viewport_offset = VIEWPORT_BASE as usize + index * VIEWPORT_STRIDE as usize;
        fill_block(
            &mut tables[0],
            transform_offset,
            VP_TRANSFORM_STRIDE as usize,
            dirty::VIEWPORT_0 + index as u8,
        );
        fill_block(
            &mut tables[0],
            viewport_offset,
            VIEWPORT_STRIDE as usize,
            dirty::VIEWPORT_0 + index as u8,
        );
    }
    fill_block(
        &mut tables[1],
        VP_TRANSFORM_BASE as usize,
        NUM_VIEWPORTS * VP_TRANSFORM_STRIDE as usize,
        dirty::VIEWPORTS,
    );
    fill_block(
        &mut tables[1],
        VIEWPORT_BASE as usize,
        NUM_VIEWPORTS * VIEWPORT_STRIDE as usize,
        dirty::VIEWPORTS,
    );
    set_table(
        &mut tables[0],
        VIEWPORT_SCALE_OFFSET_ENABLED,
        dirty::VIEWPORT_TRANSFORM,
    );
    set_table(
        &mut tables[1],
        VIEWPORT_SCALE_OFFSET_ENABLED,
        dirty::VIEWPORTS,
    );
}

fn setup_dirty_scissors(tables: &mut DirtyTables) {
    const NUM_VIEWPORTS: usize = 16;
    for index in 0..NUM_VIEWPORTS {
        let offset = SCISSOR_BASE as usize + index * SCISSOR_STRIDE as usize;
        fill_block(
            &mut tables[0],
            offset,
            SCISSOR_STRIDE as usize,
            dirty::SCISSOR_0 + index as u8,
        );
    }
    fill_block(
        &mut tables[1],
        SCISSOR_BASE as usize,
        NUM_VIEWPORTS * SCISSOR_STRIDE as usize,
        dirty::SCISSORS,
    );
}

fn setup_dirty_polygon_modes(tables: &mut DirtyTables) {
    set_table(&mut tables[0], POLYGON_MODE_FRONT, dirty::POLYGON_MODE_FRONT);
    set_table(&mut tables[0], POLYGON_MODE_BACK, dirty::POLYGON_MODE_BACK);
    set_table(&mut tables[1], POLYGON_MODE_FRONT, dirty::POLYGON_MODES);
    set_table(&mut tables[1], POLYGON_MODE_BACK, dirty::POLYGON_MODES);
    set_table(&mut tables[0], FILL_VIA_TRIANGLE_MODE, dirty::POLYGON_MODES);
}

fn setup_dirty_depth_test(tables: &mut DirtyTables) {
    set_table(&mut tables[0], DEPTH_TEST_ENABLE, dirty::DEPTH_TEST);
    set_table(&mut tables[0], DEPTH_WRITE_ENABLE, dirty::DEPTH_MASK);
    set_table(&mut tables[0], DEPTH_TEST_FUNC, dirty::DEPTH_TEST);
}

fn setup_dirty_stencil_test(tables: &mut DirtyTables) {
    const STENCIL_OP_WORDS: usize = 4;
    let offsets = [
        STENCIL_ENABLE,
        STENCIL_FRONT_OP_BASE,
        STENCIL_FRONT_OP_BASE + 1,
        STENCIL_FRONT_OP_BASE + 2,
        STENCIL_FRONT_OP_BASE + 3,
        STENCIL_FRONT_REF,
        STENCIL_FRONT_FUNC_MASK,
        STENCIL_FRONT_MASK,
        STENCIL_TWO_SIDE_ENABLE,
        STENCIL_BACK_OP_BASE,
        STENCIL_BACK_OP_BASE + 1,
        STENCIL_BACK_OP_BASE + 2,
        STENCIL_BACK_OP_BASE + 3,
        STENCIL_BACK_REF,
        STENCIL_BACK_FUNC_MASK,
        STENCIL_BACK_MASK,
    ];
    debug_assert_eq!(STENCIL_OP_WORDS, 4);
    for offset in offsets {
        set_table(&mut tables[0], offset, dirty::STENCIL_TEST);
    }
}

fn setup_dirty_alpha_test(tables: &mut DirtyTables) {
    set_table(&mut tables[0], ALPHA_TEST_REF, dirty::ALPHA_TEST);
    set_table(&mut tables[0], ALPHA_TEST_FUNC, dirty::ALPHA_TEST);
    set_table(&mut tables[0], ALPHA_TEST_ENABLED, dirty::ALPHA_TEST);
}

fn setup_dirty_blend(tables: &mut DirtyTables) {
    const NUM_RENDER_TARGETS: usize = 8;
    const BLEND_COLOR_WORDS: usize = 4;
    const BLEND_WORDS: usize = 16;
    fill_block(
        &mut tables[0],
        BLEND_COLOR_BASE as usize,
        BLEND_COLOR_WORDS,
        dirty::BLEND_COLOR,
    );
    set_table(
        &mut tables[0],
        BLEND_PER_TARGET_ENABLED,
        dirty::BLEND_INDEPENDENT_ENABLED,
    );
    for index in 0..NUM_RENDER_TARGETS {
        let offset = BLEND_PER_TARGET_BASE as usize + index * BLEND_PER_TARGET_STRIDE as usize;
        fill_block(
            &mut tables[0],
            offset,
            BLEND_PER_TARGET_STRIDE as usize,
            dirty::BLEND_STATE_0 + index as u8,
        );
        set_table(
            &mut tables[0],
            BLEND_BASE + 9 + index as u32,
            dirty::BLEND_STATE_0 + index as u8,
        );
    }
    fill_block(
        &mut tables[1],
        BLEND_PER_TARGET_BASE as usize,
        NUM_RENDER_TARGETS * BLEND_PER_TARGET_STRIDE as usize,
        dirty::BLEND_STATES,
    );
    fill_block(
        &mut tables[1],
        BLEND_BASE as usize,
        BLEND_WORDS,
        dirty::BLEND_STATES,
    );
}

fn setup_dirty_primitive_restart(tables: &mut DirtyTables) {
    fill_block(
        &mut tables[0],
        PRIMITIVE_RESTART_BASE as usize,
        PRIMITIVE_RESTART_WORDS as usize,
        dirty::PRIMITIVE_RESTART,
    );
}

fn setup_dirty_polygon_offset(tables: &mut DirtyTables) {
    set_table(
        &mut tables[0],
        POLYGON_OFFSET_FILL_ENABLE,
        dirty::POLYGON_OFFSET,
    );
    set_table(
        &mut tables[0],
        POLYGON_OFFSET_LINE_ENABLE,
        dirty::POLYGON_OFFSET,
    );
    set_table(
        &mut tables[0],
        POLYGON_OFFSET_POINT_ENABLE,
        dirty::POLYGON_OFFSET,
    );
    set_table(&mut tables[0], SLOPE_SCALE_DEPTH_BIAS, dirty::POLYGON_OFFSET);
    set_table(&mut tables[0], DEPTH_BIAS, dirty::POLYGON_OFFSET);
    set_table(&mut tables[0], DEPTH_BIAS_CLAMP, dirty::POLYGON_OFFSET);
}

fn setup_dirty_multisample_control(tables: &mut DirtyTables) {
    const ANTI_ALIAS_ALPHA_CONTROL_WORDS: usize = 1;
    fill_block(
        &mut tables[0],
        ANTI_ALIAS_ALPHA_CONTROL as usize,
        ANTI_ALIAS_ALPHA_CONTROL_WORDS,
        dirty::MULTISAMPLE_CONTROL,
    );
}

fn setup_dirty_rasterize_enable(tables: &mut DirtyTables) {
    set_table(&mut tables[0], RASTERIZE_ENABLE, dirty::RASTERIZE_ENABLE);
}

fn setup_dirty_framebuffer_srgb(tables: &mut DirtyTables) {
    set_table(&mut tables[0], FRAMEBUFFER_SRGB, dirty::FRAMEBUFFER_SRGB);
}

fn setup_dirty_logic_op(tables: &mut DirtyTables) {
    fill_block(
        &mut tables[0],
        LOGIC_OP as usize,
        LOGIC_OP_WORDS as usize,
        dirty::LOGIC_OP,
    );
}

fn setup_dirty_fragment_clamp_color(tables: &mut DirtyTables) {
    set_table(&mut tables[0], FRAG_COLOR_CLAMP, dirty::FRAGMENT_CLAMP_COLOR);
}

fn setup_dirty_point_size(tables: &mut DirtyTables) {
    set_table(&mut tables[0], POINT_SIZE_ATTRIBUTE, dirty::POINT_SIZE);
    set_table(&mut tables[0], POINT_SIZE, dirty::POINT_SIZE);
    set_table(&mut tables[0], POINT_SPRITE_ENABLE, dirty::POINT_SIZE);
}

fn setup_dirty_line_width(tables: &mut DirtyTables) {
    set_table(&mut tables[0], LINE_WIDTH_SMOOTH, dirty::LINE_WIDTH);
    set_table(&mut tables[0], LINE_WIDTH_ALIASED, dirty::LINE_WIDTH);
    set_table(&mut tables[0], LINE_ANTI_ALIAS_ENABLE, dirty::LINE_WIDTH);
}

fn setup_dirty_clip_control(tables: &mut DirtyTables) {
    set_table(&mut tables[0], WINDOW_ORIGIN, dirty::CLIP_CONTROL);
    set_table(&mut tables[0], DEPTH_MODE, dirty::CLIP_CONTROL);
}

fn setup_dirty_depth_clamp_enabled(tables: &mut DirtyTables) {
    set_table(&mut tables[0], VIEWPORT_CLIP_CONTROL, dirty::DEPTH_CLAMP_ENABLED);
}

fn setup_dirty_misc(tables: &mut DirtyTables) {
    set_table(&mut tables[0], USER_CLIP_ENABLE, dirty::CLIP_DISTANCES);
    set_table(&mut tables[0], FRONT_FACE, dirty::FRONT_FACE);
    set_table(&mut tables[0], CULL_TEST_ENABLE, dirty::CULL_TEST);
    set_table(&mut tables[0], CULL_FACE, dirty::CULL_TEST);
}

// ---------------------------------------------------------------------------
// Dirty flag indices — port of OpenGL::Dirty enum
// ---------------------------------------------------------------------------

/// Dirty flag indices matching upstream `OpenGL::Dirty::` enum.
///
/// Common entries occupy indices 0..LAST_COMMON_ENTRY, then OpenGL-specific
/// flags continue from `First`.
pub mod dirty {
    // Common entries (from VideoCommon::Dirty, shared with Vulkan)
    pub const RENDER_TARGETS: u8 = crate::dirty_flags::flags::RENDER_TARGETS;
    pub const VERTEX_BUFFERS: u8 = crate::dirty_flags::flags::VERTEX_BUFFERS;
    pub const VERTEX_BUFFER_0: u8 = crate::dirty_flags::flags::VERTEX_BUFFER0;
    pub const RESCALE_VIEWPORTS: u8 = crate::dirty_flags::flags::RESCALE_VIEWPORTS;
    pub const RESCALE_SCISSORS: u8 = crate::dirty_flags::flags::RESCALE_SCISSORS;
    pub const DEPTH_BIAS_GLOBAL: u8 = crate::dirty_flags::flags::DEPTH_BIAS_GLOBAL;
    pub const LAST_COMMON_ENTRY: u8 = crate::dirty_flags::flags::LAST_COMMON_ENTRY;

    // OpenGL-specific entries
    pub const FIRST: u8 = LAST_COMMON_ENTRY;

    pub const VERTEX_FORMATS: u8 = FIRST;
    pub const VERTEX_FORMAT_0: u8 = VERTEX_FORMATS + 1;
    pub const VERTEX_FORMAT_31: u8 = VERTEX_FORMAT_0 + 31;

    pub const VERTEX_INSTANCES: u8 = VERTEX_FORMAT_31 + 1;
    pub const VERTEX_INSTANCE_0: u8 = VERTEX_INSTANCES + 1;
    pub const VERTEX_INSTANCE_31: u8 = VERTEX_INSTANCE_0 + 31;

    pub const VIEWPORT_TRANSFORM: u8 = VERTEX_INSTANCE_31 + 1;
    pub const VIEWPORTS: u8 = VIEWPORT_TRANSFORM + 1;
    pub const VIEWPORT_0: u8 = VIEWPORTS + 1;
    pub const VIEWPORT_15: u8 = VIEWPORT_0 + 15;

    pub const SCISSORS: u8 = VIEWPORT_15 + 1;
    pub const SCISSOR_0: u8 = SCISSORS + 1;
    pub const SCISSOR_15: u8 = SCISSOR_0 + 15;

    pub const COLOR_MASK_COMMON: u8 = SCISSOR_15 + 1;
    pub const COLOR_MASKS: u8 = COLOR_MASK_COMMON + 1;
    pub const COLOR_MASK_0: u8 = COLOR_MASKS + 1;
    pub const COLOR_MASK_7: u8 = COLOR_MASK_0 + 7;

    pub const BLEND_COLOR: u8 = COLOR_MASK_7 + 1;
    pub const BLEND_INDEPENDENT_ENABLED: u8 = BLEND_COLOR + 1;
    pub const BLEND_STATES: u8 = BLEND_INDEPENDENT_ENABLED + 1;
    pub const BLEND_STATE_0: u8 = BLEND_STATES + 1;
    pub const BLEND_STATE_7: u8 = BLEND_STATE_0 + 7;

    pub const CLIP_DISTANCES: u8 = BLEND_STATE_7 + 1;

    pub const POLYGON_MODES: u8 = CLIP_DISTANCES + 1;
    pub const POLYGON_MODE_FRONT: u8 = POLYGON_MODES + 1;
    pub const POLYGON_MODE_BACK: u8 = POLYGON_MODE_FRONT + 1;

    pub const COLOR_MASK: u8 = POLYGON_MODE_BACK + 1;
    pub const FRONT_FACE: u8 = COLOR_MASK + 1;
    pub const CULL_TEST: u8 = FRONT_FACE + 1;
    pub const DEPTH_MASK: u8 = CULL_TEST + 1;
    pub const DEPTH_TEST: u8 = DEPTH_MASK + 1;
    pub const STENCIL_TEST: u8 = DEPTH_TEST + 1;
    pub const ALPHA_TEST: u8 = STENCIL_TEST + 1;
    pub const PRIMITIVE_RESTART: u8 = ALPHA_TEST + 1;
    pub const POLYGON_OFFSET: u8 = PRIMITIVE_RESTART + 1;
    pub const MULTISAMPLE_CONTROL: u8 = POLYGON_OFFSET + 1;
    pub const RASTERIZE_ENABLE: u8 = MULTISAMPLE_CONTROL + 1;
    pub const FRAMEBUFFER_SRGB: u8 = RASTERIZE_ENABLE + 1;
    pub const LOGIC_OP: u8 = FRAMEBUFFER_SRGB + 1;
    pub const FRAGMENT_CLAMP_COLOR: u8 = LOGIC_OP + 1;
    pub const POINT_SIZE: u8 = FRAGMENT_CLAMP_COLOR + 1;
    pub const LINE_WIDTH: u8 = POINT_SIZE + 1;
    pub const CLIP_CONTROL: u8 = LINE_WIDTH + 1;
    pub const DEPTH_CLAMP_ENABLED: u8 = CLIP_CONTROL + 1;

    pub const LAST: u8 = DEPTH_CLAMP_ENABLED + 1;
}

/// Backing store for dirty flags — a simple boolean array.
pub type DirtyFlags = [bool; 256];

// ---------------------------------------------------------------------------
// StateTracker
// ---------------------------------------------------------------------------

/// Tracks OpenGL state to minimize redundant API calls.
///
/// Port of `OpenGL::StateTracker` class.
///
/// Caches the currently bound index buffer, framebuffer, clip control state,
/// and Y-negate flag to skip redundant GL calls when state hasn't changed.
pub struct StateTracker {
    flags: DirtyFlags,
    default_flags: DirtyFlags,
    framebuffer: GLuint,
    index_buffer: GLuint,
    origin: GLenum,
    depth: GLenum,
    y_negate: bool,
}

impl StateTracker {
    /// Port of `StateTracker::StateTracker`.
    pub fn new() -> Self {
        let mut flags = [false; 256];
        // Start with all flags dirty
        for i in 0..(dirty::LAST as usize) {
            flags[i] = true;
        }
        Self {
            flags,
            default_flags: [false; 256],
            framebuffer: 0,
            index_buffer: 0,
            origin: gl::LOWER_LEFT,
            depth: gl::NEGATIVE_ONE_TO_ONE,
            y_negate: false,
        }
    }

    /// Load compatibility-profile fixed-function entry points that the generated
    /// `gl` bindings omit. Upstream keeps GLSL Y direction dynamic through
    /// `glMaterialfv(GL_FRONT, GL_AMBIENT, ...)`.
    pub fn load_compat_functions<F>(mut load_fn: F)
    where
        F: FnMut(&'static str) -> *const c_void,
    {
        let ptr = load_fn("glMaterialfv");
        let func = if ptr.is_null() {
            None
        } else {
            Some(unsafe { std::mem::transmute::<*const c_void, GlMaterialfv>(ptr) })
        };
        let _ = GL_MATERIALFV.set(func);
    }

    /// Port of `StateTracker::SetupTables`.
    pub fn setup_tables(tables: &mut DirtyTables) {
        setup_dirty_flags(tables);
        setup_dirty_color_masks(tables);
        setup_dirty_viewports(tables);
        setup_dirty_scissors(tables);
        setup_dirty_vertex_instances(tables);
        setup_dirty_vertex_format(tables);
        setup_dirty_polygon_modes(tables);
        setup_dirty_depth_test(tables);
        setup_dirty_stencil_test(tables);
        setup_dirty_alpha_test(tables);
        setup_dirty_blend(tables);
        setup_dirty_primitive_restart(tables);
        setup_dirty_polygon_offset(tables);
        setup_dirty_multisample_control(tables);
        setup_dirty_rasterize_enable(tables);
        setup_dirty_framebuffer_srgb(tables);
        setup_dirty_logic_op(tables);
        setup_dirty_fragment_clamp_color(tables);
        setup_dirty_point_size(tables);
        setup_dirty_line_width(tables);
        setup_dirty_clip_control(tables);
        setup_dirty_depth_clamp_enabled(tables);
        setup_dirty_misc(tables);
    }

    /// Port of `StateTracker::BindIndexBuffer`.
    pub fn bind_index_buffer(&mut self, new_index_buffer: GLuint) {
        if self.index_buffer == new_index_buffer {
            return;
        }
        self.index_buffer = new_index_buffer;
        unsafe {
            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, new_index_buffer);
        }
    }

    /// Port of `StateTracker::BindFramebuffer`.
    pub fn bind_framebuffer(&mut self, new_framebuffer: GLuint) {
        if self.framebuffer == new_framebuffer {
            return;
        }
        self.framebuffer = new_framebuffer;
        unsafe {
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, self.framebuffer);
        }
    }

    /// Port of `StateTracker::ClipControl`.
    pub fn clip_control(&mut self, new_origin: GLenum, new_depth: GLenum) {
        if new_origin == self.origin && new_depth == self.depth {
            return;
        }
        self.origin = new_origin;
        self.depth = new_depth;
        unsafe {
            gl::ClipControl(self.origin, self.depth);
        }
    }

    /// Port of `StateTracker::SetYNegate`.
    ///
    /// Upstream maps Y_NEGATE to `gl_FrontMaterial.ambient.a` via `glMaterialfv`.
    pub fn set_y_negate(&mut self, new_y_negate: bool) {
        if new_y_negate == self.y_negate {
            return;
        }
        self.y_negate = new_y_negate;
        let ambient = [0.0f32, 0.0, 0.0, if self.y_negate { -1.0 } else { 1.0 }];
        if let Some(Some(materialfv)) = GL_MATERIALFV.get() {
            unsafe {
                materialfv(gl::FRONT, GL_AMBIENT, ambient.as_ptr());
            }
        }
    }

    /// Returns the current Y-negate state.
    pub fn y_negate(&self) -> bool {
        self.y_negate
    }

    /// Port of `StateTracker::NotifyScreenDrawVertexArray`.
    pub fn notify_screen_draw_vertex_array(&mut self) {
        self.flags[dirty::VERTEX_FORMATS as usize] = true;
        self.flags[(dirty::VERTEX_FORMAT_0) as usize] = true;
        self.flags[(dirty::VERTEX_FORMAT_0 + 1) as usize] = true;

        self.flags[dirty::VERTEX_BUFFERS as usize] = true;
        self.flags[dirty::VERTEX_BUFFER_0 as usize] = true;

        self.flags[dirty::VERTEX_INSTANCES as usize] = true;
        self.flags[(dirty::VERTEX_INSTANCE_0) as usize] = true;
        self.flags[(dirty::VERTEX_INSTANCE_0 + 1) as usize] = true;
    }

    /// Port of `StateTracker::NotifyPolygonModes`.
    pub fn notify_polygon_modes(&mut self) {
        self.flags[dirty::POLYGON_MODES as usize] = true;
        self.flags[dirty::POLYGON_MODE_FRONT as usize] = true;
        self.flags[dirty::POLYGON_MODE_BACK as usize] = true;
    }

    /// Port of `StateTracker::NotifyViewport0`.
    pub fn notify_viewport0(&mut self) {
        self.flags[dirty::VIEWPORTS as usize] = true;
        self.flags[dirty::VIEWPORT_0 as usize] = true;
    }

    /// Port of `StateTracker::NotifyScissor0`.
    pub fn notify_scissor0(&mut self) {
        self.flags[dirty::SCISSORS as usize] = true;
        self.flags[dirty::SCISSOR_0 as usize] = true;
    }

    /// Port of `StateTracker::NotifyColorMask`.
    pub fn notify_color_mask(&mut self, index: usize) {
        self.flags[dirty::COLOR_MASKS as usize] = true;
        self.flags[(dirty::COLOR_MASK_0 as usize) + index] = true;
    }

    /// Port of `StateTracker::NotifyBlend0`.
    pub fn notify_blend0(&mut self) {
        self.flags[dirty::BLEND_STATES as usize] = true;
        self.flags[dirty::BLEND_STATE_0 as usize] = true;
    }

    /// Port of `StateTracker::NotifyFramebuffer`.
    pub fn notify_framebuffer(&mut self) {
        self.flags[dirty::RENDER_TARGETS as usize] = true;
        self.framebuffer = GLuint::MAX;
    }

    /// Port of `StateTracker::NotifyFrontFace`.
    pub fn notify_front_face(&mut self) {
        self.flags[dirty::FRONT_FACE as usize] = true;
    }

    /// Port of `StateTracker::NotifyCullTest`.
    pub fn notify_cull_test(&mut self) {
        self.flags[dirty::CULL_TEST as usize] = true;
    }

    /// Port of `StateTracker::NotifyDepthMask`.
    pub fn notify_depth_mask(&mut self) {
        self.flags[dirty::DEPTH_MASK as usize] = true;
    }

    /// Port of `StateTracker::NotifyDepthTest`.
    pub fn notify_depth_test(&mut self) {
        self.flags[dirty::DEPTH_TEST as usize] = true;
    }

    /// Port of `StateTracker::NotifyStencilTest`.
    pub fn notify_stencil_test(&mut self) {
        self.flags[dirty::STENCIL_TEST as usize] = true;
    }

    /// Port of `StateTracker::NotifyPolygonOffset`.
    pub fn notify_polygon_offset(&mut self) {
        self.flags[dirty::POLYGON_OFFSET as usize] = true;
    }

    /// Port of `StateTracker::NotifyRasterizeEnable`.
    pub fn notify_rasterize_enable(&mut self) {
        self.flags[dirty::RASTERIZE_ENABLE as usize] = true;
    }

    /// Port of `StateTracker::NotifyFramebufferSRGB`.
    pub fn notify_framebuffer_srgb(&mut self) {
        self.flags[dirty::FRAMEBUFFER_SRGB as usize] = true;
    }

    /// Port of `StateTracker::NotifyLogicOp`.
    pub fn notify_logic_op(&mut self) {
        self.flags[dirty::LOGIC_OP as usize] = true;
    }

    /// Port of `StateTracker::NotifyClipControl`.
    pub fn notify_clip_control(&mut self) {
        self.flags[dirty::CLIP_CONTROL as usize] = true;
    }

    /// Port of `StateTracker::NotifyAlphaTest`.
    pub fn notify_alpha_test(&mut self) {
        self.flags[dirty::ALPHA_TEST as usize] = true;
    }

    /// Port of `StateTracker::NotifyRange`.
    pub fn notify_range(&mut self, start: u8, end: u8) {
        for flag in start..=end {
            self.flags[flag as usize] = true;
        }
    }

    /// Port of `StateTracker::InvalidateState`.
    pub fn invalidate_state(&mut self) {
        for flag in self.flags.iter_mut() {
            *flag = true;
        }
    }

    /// Access the dirty flags array directly.
    pub fn flags_mut(&mut self) -> &mut DirtyFlags {
        &mut self.flags
    }

    /// Read-only access to dirty flags.
    pub fn flags(&self) -> &DirtyFlags {
        &self.flags
    }

    /// Check if a flag is dirty.
    pub fn is_dirty(&self, flag: u8) -> bool {
        self.flags[flag as usize]
    }

    /// Check if a flag is dirty, and clear it. Returns true if it was dirty.
    pub fn exchange(&mut self, flag: u8) -> bool {
        let is_dirty = self.flags[flag as usize];
        self.flags[flag as usize] = false;
        is_dirty
    }
}

impl Default for StateTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_all_dirty() {
        let tracker = StateTracker::new();
        assert!(tracker.is_dirty(dirty::VIEWPORTS));
        assert!(tracker.is_dirty(dirty::SCISSORS));
        assert!(tracker.is_dirty(dirty::FRONT_FACE));
        assert!(tracker.is_dirty(dirty::DEPTH_TEST));
    }

    #[test]
    fn test_exchange_clears_flag() {
        let mut tracker = StateTracker::new();
        assert!(tracker.exchange(dirty::VIEWPORTS));
        assert!(!tracker.exchange(dirty::VIEWPORTS));
    }

    #[test]
    fn test_notify_methods() {
        let mut tracker = StateTracker::new();
        // Clear everything first
        for flag in tracker.flags.iter_mut() {
            *flag = false;
        }
        // Now test notify methods set flags
        tracker.notify_front_face();
        assert!(tracker.is_dirty(dirty::FRONT_FACE));
        tracker.notify_cull_test();
        assert!(tracker.is_dirty(dirty::CULL_TEST));
        tracker.notify_depth_test();
        assert!(tracker.is_dirty(dirty::DEPTH_TEST));
    }

    #[test]
    fn test_notify_range() {
        let mut tracker = StateTracker::new();
        for flag in tracker.flags.iter_mut() {
            *flag = false;
        }
        tracker.notify_range(dirty::VIEWPORT_0, dirty::VIEWPORT_15);
        for i in dirty::VIEWPORT_0..=dirty::VIEWPORT_15 {
            assert!(tracker.is_dirty(i));
        }
    }

    #[test]
    fn set_y_negate_updates_cached_value_without_loaded_compat_function() {
        let mut tracker = StateTracker::new();
        assert!(!tracker.y_negate());
        tracker.set_y_negate(true);
        assert!(tracker.y_negate());
        tracker.set_y_negate(false);
        assert!(!tracker.y_negate());
    }

    #[test]
    fn setup_tables_marks_upstream_opengl_dirty_ranges() {
        let mut tables = [
            vec![crate::dirty_flags::flags::NULL_ENTRY; crate::engines::ENGINE_REG_COUNT],
            vec![crate::dirty_flags::flags::NULL_ENTRY; crate::engines::ENGINE_REG_COUNT],
        ];

        StateTracker::setup_tables(&mut tables);

        assert_eq!(dirty::FIRST, crate::dirty_flags::flags::LAST_COMMON_ENTRY);
        assert_eq!(tables[0][COLOR_MASK_COMMON as usize], dirty::COLOR_MASK_COMMON);
        assert_eq!(tables[0][COLOR_MASK_BASE as usize], dirty::COLOR_MASK_0);
        assert_eq!(tables[1][COLOR_MASK_BASE as usize], dirty::COLOR_MASKS);
        assert_eq!(
            tables[0][VERTEX_STREAM_BASE as usize + 3],
            dirty::VERTEX_INSTANCE_0
        );
        assert_eq!(
            tables[0][VERTEX_STREAM_INSTANCE_BASE as usize],
            dirty::VERTEX_INSTANCE_0
        );
        assert_eq!(tables[0][VERTEX_ATTRIB_BASE as usize], dirty::VERTEX_FORMAT_0);
        assert_eq!(tables[1][VERTEX_ATTRIB_BASE as usize], dirty::VERTEX_FORMATS);
        assert_eq!(tables[0][VP_TRANSFORM_BASE as usize], dirty::VIEWPORT_0);
        assert_eq!(tables[1][VP_TRANSFORM_BASE as usize], dirty::VIEWPORTS);
        assert_eq!(tables[0][VIEWPORT_BASE as usize], dirty::VIEWPORT_0);
        assert_eq!(tables[1][VIEWPORT_BASE as usize], dirty::VIEWPORTS);
        assert_eq!(tables[0][SCISSOR_BASE as usize], dirty::SCISSOR_0);
        assert_eq!(tables[1][SCISSOR_BASE as usize], dirty::SCISSORS);
        assert_eq!(tables[0][POLYGON_MODE_FRONT as usize], dirty::POLYGON_MODE_FRONT);
        assert_eq!(tables[1][POLYGON_MODE_BACK as usize], dirty::POLYGON_MODES);
        assert_eq!(tables[0][DEPTH_TEST_ENABLE as usize], dirty::DEPTH_TEST);
        assert_eq!(tables[0][DEPTH_WRITE_ENABLE as usize], dirty::DEPTH_MASK);
        assert_eq!(tables[0][STENCIL_ENABLE as usize], dirty::STENCIL_TEST);
        assert_eq!(tables[0][ALPHA_TEST_ENABLED as usize], dirty::ALPHA_TEST);
        assert_eq!(tables[0][BLEND_COLOR_BASE as usize], dirty::BLEND_COLOR);
        assert_eq!(
            tables[0][BLEND_PER_TARGET_ENABLED as usize],
            dirty::BLEND_INDEPENDENT_ENABLED
        );
        assert_eq!(tables[0][BLEND_PER_TARGET_BASE as usize], dirty::BLEND_STATE_0);
        assert_eq!(tables[1][BLEND_PER_TARGET_BASE as usize], dirty::BLEND_STATES);
        assert_eq!(tables[0][PRIMITIVE_RESTART_BASE as usize], dirty::PRIMITIVE_RESTART);
        assert_eq!(
            tables[0][POLYGON_OFFSET_FILL_ENABLE as usize],
            dirty::POLYGON_OFFSET
        );
        assert_eq!(
            tables[0][POLYGON_OFFSET_LINE_ENABLE as usize],
            dirty::POLYGON_OFFSET
        );
        assert_eq!(
            tables[0][POLYGON_OFFSET_POINT_ENABLE as usize],
            dirty::POLYGON_OFFSET
        );
        assert_eq!(tables[0][SLOPE_SCALE_DEPTH_BIAS as usize], dirty::POLYGON_OFFSET);
        assert_eq!(tables[0][ANTI_ALIAS_ALPHA_CONTROL as usize], dirty::MULTISAMPLE_CONTROL);
        assert_eq!(tables[0][RASTERIZE_ENABLE as usize], dirty::RASTERIZE_ENABLE);
        assert_eq!(tables[0][FRAMEBUFFER_SRGB as usize], dirty::FRAMEBUFFER_SRGB);
        assert_eq!(tables[0][LOGIC_OP as usize], dirty::LOGIC_OP);
        assert_eq!(tables[0][LOGIC_OP as usize + 1], dirty::LOGIC_OP);
        assert_eq!(tables[0][FRAG_COLOR_CLAMP as usize], dirty::FRAGMENT_CLAMP_COLOR);
        assert_eq!(tables[0][POINT_SIZE as usize], dirty::POINT_SIZE);
        assert_eq!(tables[0][LINE_WIDTH_SMOOTH as usize], dirty::LINE_WIDTH);
        assert_eq!(tables[0][WINDOW_ORIGIN as usize], dirty::CLIP_CONTROL);
        assert_eq!(tables[0][DEPTH_MODE as usize], dirty::CLIP_CONTROL);
        assert_eq!(tables[0][VIEWPORT_CLIP_CONTROL as usize], dirty::DEPTH_CLAMP_ENABLED);
        assert_eq!(tables[0][USER_CLIP_ENABLE as usize], dirty::CLIP_DISTANCES);
        assert_eq!(tables[0][FRONT_FACE as usize], dirty::FRONT_FACE);
        assert_eq!(tables[0][CULL_TEST_ENABLE as usize], dirty::CULL_TEST);
    }
}

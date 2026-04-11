// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_blit_screen.h and gl_blit_screen.cpp
//!
//! Handles compositing framebuffers to the screen using Layer + WindowAdaptPass.
//! This is the final stage of the OpenGL rendering pipeline.

use crate::framebuffer_config::FramebufferConfig;
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;

use super::gl_state_tracker::StateTracker;
use super::present::filters::{self, ScalingFilter};
use super::present::layer::Layer;
use super::present::window_adapt_pass::WindowAdaptPass;

/// BlitScreen handles the final frame composition to the window.
///
/// Corresponds to zuyu's `BlitScreen` class.
pub struct BlitScreen {
    current_window_adapt: Option<ScalingFilter>,
    window_adapt: Option<WindowAdaptPass>,
    layers: Vec<Layer>,
}

impl BlitScreen {
    /// Create a new BlitScreen.
    ///
    /// Port of `BlitScreen::BlitScreen()`.
    pub fn new() -> Result<Self, String> {
        log::info!("BlitScreen: OpenGL blit pipeline created");
        Ok(Self {
            current_window_adapt: None,
            window_adapt: None,
            layers: Vec::new(),
        })
    }

    /// Draw emulated screens to the emulator window.
    ///
    /// Port of `BlitScreen::DrawScreen()`.
    pub fn draw_screen(
        &mut self,
        framebuffers: &[FramebufferConfig],
        layout: &FramebufferLayout,
        state_tracker: &mut StateTracker,
        invert_y: bool,
        device_memory: Option<&crate::renderer_base::DeviceMemoryReader>,
    ) {
        // Notify state tracker about state changes we're about to make.
        // Port of the state_tracker.Notify*() calls in upstream DrawScreen.
        state_tracker.notify_screen_draw_vertex_array();
        state_tracker.notify_polygon_modes();
        state_tracker.notify_viewport0();
        state_tracker.notify_scissor0();
        state_tracker.notify_color_mask(0);
        state_tracker.notify_blend0();
        state_tracker.notify_framebuffer();
        state_tracker.notify_front_face();
        state_tracker.notify_cull_test();
        state_tracker.notify_depth_test();
        state_tracker.notify_stencil_test();
        state_tracker.notify_polygon_offset();
        state_tracker.notify_rasterize_enable();
        state_tracker.notify_framebuffer_srgb();
        state_tracker.notify_logic_op();
        state_tracker.notify_clip_control();
        state_tracker.notify_alpha_test();
        state_tracker.clip_control(gl::LOWER_LEFT, gl::ZERO_TO_ONE);

        unsafe {
            gl::Enable(gl::CULL_FACE);
            gl::Disable(gl::COLOR_LOGIC_OP);
            gl::Disable(gl::DEPTH_TEST);
            gl::Disable(gl::STENCIL_TEST);
            gl::Disable(gl::POLYGON_OFFSET_FILL);
            gl::Disable(gl::RASTERIZER_DISCARD);
            // GL_ALPHA_TEST not available in core profile via gl crate constant,
            // but upstream calls glDisable(GL_ALPHA_TEST). Safe to skip in core profile.
            gl::Disablei(gl::BLEND, 0);
            gl::PolygonMode(gl::FRONT_AND_BACK, gl::FILL);
            gl::CullFace(gl::BACK);
            gl::FrontFace(gl::CW);
            gl::ColorMaski(0, gl::TRUE, gl::TRUE, gl::TRUE, gl::TRUE);
            gl::DepthRangeIndexed(0, 0.0, 0.0);
        }

        // Ensure we have enough Layer instances.
        while self.layers.len() < framebuffers.len() {
            self.layers.push(Layer::new());
        }

        self.create_window_adapt();
        if let Some(ref window_adapt) = self.window_adapt {
            window_adapt.draw_to_framebuffer(
                &mut self.layers,
                framebuffers,
                layout,
                invert_y,
                device_memory,
            );
        }
    }

    /// Create or recreate the window adapt pass if the scaling filter changed.
    ///
    /// Port of `BlitScreen::CreateWindowAdapt()`.
    fn create_window_adapt(&mut self) {
        // For now, default to Bilinear. When Settings is fully ported,
        // this should read from settings: filters.get_scaling_filter().
        let desired = ScalingFilter::Bilinear;

        if self.window_adapt.is_some() && self.current_window_adapt == Some(desired) {
            return;
        }

        self.current_window_adapt = Some(desired);
        self.window_adapt = Some(filters::make_filter(desired));
    }
}

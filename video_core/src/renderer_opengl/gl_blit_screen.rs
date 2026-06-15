// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_blit_screen.h and gl_blit_screen.cpp
//!
//! Handles compositing framebuffers to the screen using Layer + WindowAdaptPass.
//! This is the final stage of the OpenGL rendering pipeline.

use crate::framebuffer_config::FramebufferConfig;
use crate::present::{PresentFilters, PRESENT_FILTERS_FOR_DISPLAY};
use ruzu_core::frontend::framebuffer_layout::FramebufferLayout;

use super::gl_shader_manager::ProgramManagerHandle;
use super::gl_state_tracker::StateTracker;
use super::present::filters::{self, ScalingFilter};
use super::present::layer::Layer;
use super::present::window_adapt_pass::WindowAdaptPass;
use super::{Device, RasterizerOpenGL};
use crate::renderer_base::DeviceMemoryReader;

const GL_ALPHA_TEST: u32 = 0x0BC0;

fn to_opengl_scaling_filter(filter: crate::present::ScalingFilter) -> ScalingFilter {
    match filter {
        crate::present::ScalingFilter::NearestNeighbor => ScalingFilter::NearestNeighbor,
        crate::present::ScalingFilter::Bilinear => ScalingFilter::Bilinear,
        crate::present::ScalingFilter::Bicubic => ScalingFilter::Bicubic,
        crate::present::ScalingFilter::Gaussian => ScalingFilter::Gaussian,
        crate::present::ScalingFilter::ScaleForce => ScalingFilter::ScaleForce,
        crate::present::ScalingFilter::Fsr => ScalingFilter::Fsr,
    }
}

/// BlitScreen handles the final frame composition to the window.
///
/// Corresponds to zuyu's `BlitScreen` class.
pub struct BlitScreen {
    filters: &'static PresentFilters,
    program_manager: ProgramManagerHandle,
    /// Upstream stores `RasterizerOpenGL& rasterizer` on `BlitScreen`.
    rasterizer: *mut RasterizerOpenGL,
    /// Upstream stores `StateTracker& state_tracker` on `BlitScreen`.
    state_tracker: *mut StateTracker,
    /// Upstream stores `Device& device` on `BlitScreen`.
    device: *const Device,
    device_memory: DeviceMemoryReader,
    current_window_adapt: Option<ScalingFilter>,
    window_adapt: Option<WindowAdaptPass>,
    layers: Vec<Layer>,
}

// Safety: `BlitScreen` is owned by `RendererOpenGL`. The raw rasterizer pointer
// and the raw state/device pointers are non-owning references to boxed members
// stored in that same renderer, and present work uses them on the renderer thread.
unsafe impl Send for BlitScreen {}

impl BlitScreen {
    /// Create a new BlitScreen.
    ///
    /// Port of `BlitScreen::BlitScreen()`.
    pub fn new(
        program_manager: ProgramManagerHandle,
        rasterizer: *mut RasterizerOpenGL,
        state_tracker: *mut StateTracker,
        device: *const Device,
        device_memory: DeviceMemoryReader,
    ) -> Result<Self, String> {
        log::info!("BlitScreen: OpenGL blit pipeline created");
        Ok(Self {
            filters: &PRESENT_FILTERS_FOR_DISPLAY,
            program_manager,
            rasterizer,
            state_tracker,
            device,
            device_memory,
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
        invert_y: bool,
    ) {
        // Notify state tracker about state changes we're about to make.
        // Port of the state_tracker.Notify*() calls in upstream DrawScreen.
        unsafe {
            let state_tracker = self
                .state_tracker
                .as_mut()
                .expect("OpenGL BlitScreen state tracker pointer must remain valid");
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
        }

        unsafe {
            gl::Enable(gl::CULL_FACE);
            gl::Disable(gl::COLOR_LOGIC_OP);
            gl::Disable(gl::DEPTH_TEST);
            gl::Disable(gl::STENCIL_TEST);
            gl::Disable(gl::POLYGON_OFFSET_FILL);
            gl::Disable(gl::RASTERIZER_DISCARD);
            // GL_ALPHA_TEST is a compatibility enum omitted by the generated `gl`
            // crate, but upstream still disables it before present draws.
            gl::Disable(GL_ALPHA_TEST);
            gl::Disablei(gl::BLEND, 0);
            gl::PolygonMode(gl::FRONT_AND_BACK, gl::FILL);
            gl::CullFace(gl::BACK);
            gl::FrontFace(gl::CW);
            gl::ColorMaski(0, gl::TRUE, gl::TRUE, gl::TRUE, gl::TRUE);
            gl::DepthRangeIndexed(0, 0.0, 0.0);
        }

        // Ensure we have enough Layer instances.
        while self.layers.len() < framebuffers.len() {
            self.layers.push(Layer::new(
                self.rasterizer,
                self.device_memory.clone(),
                self.filters,
            ));
        }

        self.create_window_adapt();
        if let Some(ref window_adapt) = self.window_adapt {
            window_adapt.draw_to_framebuffer(
                &mut self.layers,
                framebuffers,
                layout,
                invert_y,
                &self.program_manager,
            );
        }
    }

    /// Create or recreate the window adapt pass if the scaling filter changed.
    ///
    /// Port of `BlitScreen::CreateWindowAdapt()`.
    fn create_window_adapt(&mut self) {
        let desired = to_opengl_scaling_filter((self.filters.get_scaling_filter)());

        if self.window_adapt.is_some() && self.current_window_adapt == Some(desired) {
            return;
        }

        self.current_window_adapt = Some(desired);
        self.window_adapt = Some(filters::make_filter(desired, self.device));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn opengl_scaling_filter_maps_present_filters() {
        assert_eq!(
            to_opengl_scaling_filter(crate::present::ScalingFilter::NearestNeighbor),
            ScalingFilter::NearestNeighbor
        );
        assert_eq!(
            to_opengl_scaling_filter(crate::present::ScalingFilter::Bilinear),
            ScalingFilter::Bilinear
        );
        assert_eq!(
            to_opengl_scaling_filter(crate::present::ScalingFilter::Bicubic),
            ScalingFilter::Bicubic
        );
        assert_eq!(
            to_opengl_scaling_filter(crate::present::ScalingFilter::Gaussian),
            ScalingFilter::Gaussian
        );
        assert_eq!(
            to_opengl_scaling_filter(crate::present::ScalingFilter::ScaleForce),
            ScalingFilter::ScaleForce
        );
        assert_eq!(
            to_opengl_scaling_filter(crate::present::ScalingFilter::Fsr),
            ScalingFilter::Fsr
        );
    }

    #[test]
    fn alpha_test_enum_matches_upstream_compatibility_constant() {
        assert_eq!(GL_ALPHA_TEST, 0x0BC0);
    }
}

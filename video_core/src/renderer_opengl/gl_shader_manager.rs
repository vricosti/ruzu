// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of Eden `video_core/renderer_opengl/gl_shader_manager.{h,cpp}`.
//!
//! OpenGL program manager — manages binding of shader programs and assembly programs.

use super::gl_resource_manager::{OGLAssemblyProgram, OGLPipeline, OGLProgram};
use super::gl_shader_util::{bind_assembly_program, create_program_from_source};
use crate::host_shaders::compute_shaders::OPENGL_LMEM_WARMUP_COMP;
use std::sync::Arc;

pub type ProgramManagerHandle = Arc<parking_lot::Mutex<ProgramManager>>;

/// Number of shader stages.
const NUM_STAGES: usize = 5;

/// Assembly program stage enums (NV extensions).
const ASSEMBLY_PROGRAM_ENUMS: [u32; NUM_STAGES] = [
    0x8620, // GL_VERTEX_PROGRAM_NV
    0x891E, // GL_TESS_CONTROL_PROGRAM_NV
    0x891F, // GL_TESS_EVALUATION_PROGRAM_NV
    0x8C26, // GL_GEOMETRY_PROGRAM_NV
    0x8870, // GL_FRAGMENT_PROGRAM_NV
];

/// OpenGL program manager.
///
/// Corresponds to `OpenGL::ProgramManager`.
pub struct ProgramManager {
    // Rust drops fields in declaration order; Eden destroys the last member
    // (`lmem_warmup_program`) before the first owning member (`pipeline`).
    lmem_warmup_program: OGLProgram,
    pipeline: OGLPipeline,
    is_pipeline_bound: bool,
    is_compute_bound: bool,
    current_stage_mask: u32,
    current_programs: [u32; NUM_STAGES],
    current_assembly_compute_program: u32,
}

impl ProgramManager {
    /// Create a new program manager.
    ///
    /// Corresponds to `ProgramManager::ProgramManager()`.
    pub fn new(_device: &super::gl_device::Device) -> Self {
        Self::new_with_caps(_device.use_assembly_shaders(), _device.has_lmem_perf_bug())
    }

    fn new_with_caps(use_assembly_shaders: bool, has_lmem_perf_bug: bool) -> Self {
        let mut pipeline = OGLPipeline::new();
        unsafe {
            gl::CreateProgramPipelines(1, &mut pipeline.handle);
        }
        if use_assembly_shaders {
            const GL_COMPUTE_PROGRAM_NV: u32 = 0x90FB;
            unsafe {
                gl::Enable(GL_COMPUTE_PROGRAM_NV);
            }
        }

        let mut lmem_warmup_program = OGLProgram::new();
        if has_lmem_perf_bug {
            lmem_warmup_program =
                create_program_from_source(OPENGL_LMEM_WARMUP_COMP, gl::COMPUTE_SHADER);
        }

        Self {
            lmem_warmup_program,
            pipeline,
            is_pipeline_bound: false,
            is_compute_bound: false,
            current_stage_mask: 0,
            current_programs: [0; NUM_STAGES],
            current_assembly_compute_program: 0,
        }
    }

    pub(crate) fn new_shared(device: &super::gl_device::Device) -> ProgramManagerHandle {
        Arc::new(parking_lot::Mutex::new(Self::new(device)))
    }

    #[cfg(test)]
    pub(crate) fn new_shared_for_test() -> ProgramManagerHandle {
        Arc::new(parking_lot::Mutex::new(Self {
            lmem_warmup_program: OGLProgram::new(),
            pipeline: OGLPipeline::new(),
            is_pipeline_bound: false,
            is_compute_bound: false,
            current_stage_mask: 0,
            current_programs: [0; NUM_STAGES],
            current_assembly_compute_program: 0,
        }))
    }

    /// Bind a compute program (GLSL/SPIR-V).
    ///
    /// Corresponds to `ProgramManager::BindComputeProgram()`.
    pub fn bind_compute_program(&mut self, program: u32) {
        unsafe {
            gl::UseProgram(program);
        }
        self.is_compute_bound = true;
    }

    /// Bind a compute assembly program (GLASM).
    ///
    /// Corresponds to `ProgramManager::BindComputeAssemblyProgram()`.
    pub fn bind_compute_assembly_program(&mut self, program: u32) {
        if self.current_assembly_compute_program != program {
            self.current_assembly_compute_program = program;
            const GL_COMPUTE_PROGRAM_NV: u32 = 0x90FB;
            bind_assembly_program(GL_COMPUTE_PROGRAM_NV, program);
        }
        self.unbind_pipeline();
    }

    /// Bind source programs for all stages.
    ///
    /// Corresponds to `ProgramManager::BindSourcePrograms()`.
    pub fn bind_source_programs(&mut self, programs: &[OGLProgram; NUM_STAGES]) {
        const STAGE_ENUMS: [u32; NUM_STAGES] = [
            gl::VERTEX_SHADER_BIT,
            gl::TESS_CONTROL_SHADER_BIT,
            gl::TESS_EVALUATION_SHADER_BIT,
            gl::GEOMETRY_SHADER_BIT,
            gl::FRAGMENT_SHADER_BIT,
        ];
        for stage in 0..NUM_STAGES {
            if self.current_programs[stage] != programs[stage].handle {
                self.current_programs[stage] = programs[stage].handle;
                unsafe {
                    gl::UseProgramStages(
                        self.pipeline.handle,
                        STAGE_ENUMS[stage],
                        programs[stage].handle,
                    );
                }
            }
        }
        self.bind_pipeline();
    }

    /// Bind vertex and fragment programs for presentation.
    ///
    /// Corresponds to `ProgramManager::BindPresentPrograms()`.
    pub fn bind_present_programs(&mut self, vertex: u32, fragment: u32) {
        if self.current_programs[0] != vertex {
            self.current_programs[0] = vertex;
            unsafe {
                gl::UseProgramStages(self.pipeline.handle, gl::VERTEX_SHADER_BIT, vertex);
            }
        }
        if self.current_programs[4] != fragment {
            self.current_programs[4] = fragment;
            unsafe {
                gl::UseProgramStages(self.pipeline.handle, gl::FRAGMENT_SHADER_BIT, fragment);
            }
        }
        unsafe {
            gl::UseProgramStages(
                self.pipeline.handle,
                gl::TESS_CONTROL_SHADER_BIT
                    | gl::TESS_EVALUATION_SHADER_BIT
                    | gl::GEOMETRY_SHADER_BIT,
                0,
            );
        }
        self.current_programs[1] = 0;
        self.current_programs[2] = 0;
        self.current_programs[3] = 0;

        if self.current_stage_mask != 0 {
            self.current_stage_mask = 0;
            for program_type in ASSEMBLY_PROGRAM_ENUMS {
                unsafe {
                    gl::Disable(program_type);
                }
            }
        }
        self.bind_pipeline();
    }

    /// Bind assembly programs for all stages.
    ///
    /// Corresponds to `ProgramManager::BindAssemblyPrograms()`.
    pub fn bind_assembly_programs(
        &mut self,
        programs: &[OGLAssemblyProgram; NUM_STAGES],
        stage_mask: u32,
    ) {
        let changed_mask = self.current_stage_mask ^ stage_mask;
        self.current_stage_mask = stage_mask;

        if changed_mask != 0 {
            for stage in 0..NUM_STAGES {
                if ((changed_mask >> stage) & 1) != 0 {
                    unsafe {
                        if ((stage_mask >> stage) & 1) != 0 {
                            gl::Enable(ASSEMBLY_PROGRAM_ENUMS[stage]);
                        } else {
                            gl::Disable(ASSEMBLY_PROGRAM_ENUMS[stage]);
                        }
                    }
                }
            }
        }
        for stage in 0..NUM_STAGES {
            if self.current_programs[stage] != programs[stage].handle {
                self.current_programs[stage] = programs[stage].handle;
                bind_assembly_program(ASSEMBLY_PROGRAM_ENUMS[stage], programs[stage].handle);
            }
        }
        self.unbind_pipeline();
    }

    /// Restore guest compute state.
    pub fn restore_guest_compute(&mut self) {
        // No-op in upstream
    }

    /// Warm up local memory with a compute dispatch.
    pub fn local_memory_warmup(&mut self) {
        if self.lmem_warmup_program.handle != 0 {
            self.bind_compute_program(self.lmem_warmup_program.handle);
            unsafe {
                gl::DispatchCompute(1, 1, 1);
            }
        }
    }

    fn bind_pipeline(&mut self) {
        if !self.is_pipeline_bound {
            self.is_pipeline_bound = true;
            unsafe {
                gl::BindProgramPipeline(self.pipeline.handle);
            }
        }
        self.unbind_compute();
    }

    fn unbind_pipeline(&mut self) {
        if self.is_pipeline_bound {
            self.is_pipeline_bound = false;
            unsafe {
                gl::BindProgramPipeline(0);
            }
        }
        self.unbind_compute();
    }

    fn unbind_compute(&mut self) {
        if self.is_compute_bound {
            self.is_compute_bound = false;
            unsafe {
                gl::UseProgram(0);
            }
        }
    }
}

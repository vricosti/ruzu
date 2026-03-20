// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_shader_manager.h and gl_shader_manager.cpp
//!
//! OpenGL program manager — manages binding of shader programs and assembly programs.

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

/// Stage shader bits for glUseProgramStages.
const STAGE_ENUMS: [u32; NUM_STAGES] = [
    gl::VERTEX_SHADER_BIT,
    gl::TESS_CONTROL_SHADER_BIT,
    gl::TESS_EVALUATION_SHADER_BIT,
    gl::GEOMETRY_SHADER_BIT,
    gl::FRAGMENT_SHADER_BIT,
];

/// OpenGL program manager.
///
/// Corresponds to `OpenGL::ProgramManager`.
pub struct ProgramManager {
    pipeline: u32,
    is_pipeline_bound: bool,
    is_compute_bound: bool,
    current_stage_mask: u32,
    current_programs: [u32; NUM_STAGES],
    current_assembly_compute_program: u32,
    lmem_warmup_program: u32,
}

impl ProgramManager {
    /// Create a new program manager.
    ///
    /// Corresponds to `ProgramManager::ProgramManager()`.
    pub fn new(_device: &super::gl_device::Device) -> Self {
        let mut pipeline: u32 = 0;
        unsafe {
            gl::CreateProgramPipelines(1, &mut pipeline);
        }
        // Upstream: if (device.UseAssemblyShaders()) { glEnable(GL_COMPUTE_PROGRAM_NV); }
        // GL_COMPUTE_PROGRAM_NV (0x90FB) is an NV extension enum not exposed by the gl crate.
        // Assembly shader support requires NV-specific function pointers (see gl_shader_util.rs).
        // Until those are loaded via runtime GetProcAddress, this is a no-op.
        if _device.use_assembly_shaders() {
            const GL_COMPUTE_PROGRAM_NV: u32 = 0x90FB;
            unsafe {
                gl::Enable(GL_COMPUTE_PROGRAM_NV);
            }
        }

        // Upstream: if (device.HasLmemPerfBug()) { lmem_warmup_program = CreateProgram(...); }
        // HasLmemPerfBug is not yet ported to gl_device::Device. When it is added, this
        // should create the warmup program from OPENGL_LMEM_WARMUP_COMP. For now, the
        // lmem_warmup_program stays 0 (no warmup needed).
        let lmem_warmup_program = 0;

        Self {
            pipeline,
            is_pipeline_bound: false,
            is_compute_bound: false,
            current_stage_mask: 0,
            current_programs: [0; NUM_STAGES],
            current_assembly_compute_program: 0,
            lmem_warmup_program,
        }
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
            // glBindProgramARB(GL_COMPUTE_PROGRAM_NV, program) — NV extension
        }
        self.unbind_pipeline();
    }

    /// Bind source programs for all stages.
    ///
    /// Corresponds to `ProgramManager::BindSourcePrograms()`.
    pub fn bind_source_programs(&mut self, programs: &[u32; NUM_STAGES]) {
        for stage in 0..NUM_STAGES {
            if self.current_programs[stage] != programs[stage] {
                self.current_programs[stage] = programs[stage];
                unsafe {
                    gl::UseProgramStages(self.pipeline, STAGE_ENUMS[stage], programs[stage]);
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
                gl::UseProgramStages(self.pipeline, gl::VERTEX_SHADER_BIT, vertex);
            }
        }
        if self.current_programs[4] != fragment {
            self.current_programs[4] = fragment;
            unsafe {
                gl::UseProgramStages(self.pipeline, gl::FRAGMENT_SHADER_BIT, fragment);
            }
        }
        unsafe {
            gl::UseProgramStages(
                self.pipeline,
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
            // Disable all assembly stages
        }
        self.bind_pipeline();
    }

    /// Bind assembly programs for all stages.
    ///
    /// Corresponds to `ProgramManager::BindAssemblyPrograms()`.
    pub fn bind_assembly_programs(&mut self, programs: &[u32; NUM_STAGES], stage_mask: u32) {
        let changed_mask = self.current_stage_mask ^ stage_mask;
        self.current_stage_mask = stage_mask;

        if changed_mask != 0 {
            for stage in 0..NUM_STAGES {
                if ((changed_mask >> stage) & 1) != 0 {
                    // Enable/disable assembly stage via NV extension
                    let _ = ASSEMBLY_PROGRAM_ENUMS[stage];
                }
            }
        }
        for stage in 0..NUM_STAGES {
            if self.current_programs[stage] != programs[stage] {
                self.current_programs[stage] = programs[stage];
                // glBindProgramARB(ASSEMBLY_PROGRAM_ENUMS[stage], programs[stage])
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
        if self.lmem_warmup_program != 0 {
            self.bind_compute_program(self.lmem_warmup_program);
            unsafe {
                gl::DispatchCompute(1, 1, 1);
            }
        }
    }

    fn bind_pipeline(&mut self) {
        if !self.is_pipeline_bound {
            self.is_pipeline_bound = true;
            unsafe {
                gl::BindProgramPipeline(self.pipeline);
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

impl Drop for ProgramManager {
    fn drop(&mut self) {
        if self.pipeline != 0 {
            unsafe {
                gl::DeleteProgramPipelines(1, &self.pipeline);
            }
        }
    }
}

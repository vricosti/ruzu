// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell shader recompiler: Maxwell GPU bytecode → IR → SPIR-V.
//!
//! This module implements a shader recompiler matching zuyu's architecture:
//!
//! ```text
//! Maxwell binary
//!   → CFG analysis (control_flow.rs)
//!   → Maxwell decoder (maxwell_opcodes.rs)
//!   → TranslatorVisitor (translate/*.rs) → IR::Program
//!   → Optimization passes (ir_opt/)
//!   → SPIR-V backend (backend/) → Vec<u32>
//!   → VkShaderModule → Vulkan Pipeline → GPU execution
//! ```
//!
//! # Public API
//!
//! - [`compile_shader`] — Compile a single Maxwell shader to SPIR-V.
//! - [`PipelineCache`] — Cache compiled shaders by Maxwell binary hash.
//! - [`Profile`] — GPU/driver capability profile for SPIR-V emission.
//!
//! # Modules
//!
//! - `ir` — Core Intermediate Representation (opcodes, values, instructions, blocks, program)
//! - `frontend` — Maxwell decoder, CFG builder, structured CF, translator
//! - `backend` — SPIR-V emission via rspirv
//! - `ir_opt` — Optimization passes (constant propagation, DCE, etc.)
//! - `pipeline_cache` — Shader compilation and caching

pub mod ir;
pub mod frontend;
pub mod backend;
pub mod ir_opt;
pub mod pipeline_cache;

// Re-export public API
pub use backend::Profile;
pub use pipeline_cache::{compile_shader, CompiledShader, PipelineCache, ShaderKey};
pub use ir::types::ShaderStage;

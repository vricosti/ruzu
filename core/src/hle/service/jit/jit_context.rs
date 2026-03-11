// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/jit/jit_context.h
//! Port of zuyu/src/core/hle/service/jit/jit_context.cpp
//!
//! JitContext — manages JIT plugin loading and code generation callbacks.

/// JitContext manages the JIT plugin and its code generation callbacks.
///
/// Corresponds to `JitContext` in upstream jit_context.h / jit_context.cpp.
pub struct JitContext {
    // TODO: callbacks, plugin handle, etc.
}

impl JitContext {
    pub fn new() -> Self {
        Self {}
    }

    /// GenerateCode — invokes the JIT plugin to generate code.
    pub fn generate_code(&mut self) {
        log::warn!("JitContext::generate_code (STUBBED) called");
        // TODO: implement JIT code generation
    }

    /// Control — sends a control command to the JIT plugin.
    pub fn control(&mut self) {
        log::warn!("JitContext::control (STUBBED) called");
        // TODO: implement JIT control
    }

    /// LoadPlugin — loads a JIT plugin NRO.
    pub fn load_plugin(&mut self) {
        log::warn!("JitContext::load_plugin (STUBBED) called");
        // TODO: implement JIT plugin loading
    }
}

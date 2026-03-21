// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/ectx.h
//! Port of zuyu/src/core/hle/service/glue/ectx.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command IDs for ECTX_AW
pub mod ectx_aw_commands {
    pub const CREATE_CONTEXT_REGISTRAR: u32 = 0;
    pub const COMMIT_CONTEXT: u32 = 1;
}

/// IPC command IDs for IContextRegistrar
pub mod context_registrar_commands {
    pub const COMPLETE: u32 = 0;
}

/// ECTX_AW service ("ectx:aw").
///
/// Corresponds to `ECTX_AW` in upstream `ectx.h`.
pub struct EctxAW;

impl EctxAW {
    pub fn new() -> Self { Self }

    /// Creates and returns an IContextRegistrar instance.
    ///
    /// Upstream creates an IContextRegistrar via PushIpcInterface.
    pub fn create_context_registrar(&self) -> (ResultCode, IContextRegistrar) {
        log::debug!("ECTX_AW::create_context_registrar called");
        (RESULT_SUCCESS, IContextRegistrar::new())
    }
}

/// IContextRegistrar: nn::err::context::IContextRegistrar.
///
/// Defined in upstream `ectx.cpp`.
pub struct IContextRegistrar;

impl IContextRegistrar {
    pub fn new() -> Self { Self }

    pub fn complete(&self, _unk: u32, _value: &[u8]) -> (ResultCode, u32) {
        (RESULT_SUCCESS, 0)
    }
}

// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! ARM Native Code Execution (NCE) backend.
//! Port of zuyu/src/core/arm/nce/

pub mod arm_nce;
pub mod arm_nce_asm_definitions;
pub mod guest_context;
pub mod instructions;
pub mod interpreter_visitor;
pub mod patcher;
pub mod visitor_base;

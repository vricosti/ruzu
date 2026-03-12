// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/kernel/k_system_control.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-12
//!
//! This is a thin re-export layer that selects the board-specific
//! KSystemControl implementation. Upstream uses a preprocessor define
//! `BOARD_NINTENDO_NX` to select the implementation. In Rust we
//! unconditionally re-export the Nintendo NX board implementation.

// Upstream: #define BOARD_NINTENDO_NX
// Upstream: #include "core/hle/kernel/board/nintendo/nx/k_system_control.h"
// Upstream: using Kernel::Board::Nintendo::Nx::KSystemControl;

pub use super::board::k_system_control::*;

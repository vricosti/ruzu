// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/ovln/sender_service.h
//! Port of zuyu/src/core/hle/service/psc/ovln/sender_service.cpp
//!
//! ISenderService: "ovln:snd" service.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command table for ISenderService.
///
/// | Cmd | Handler    | Name       |
/// |-----|-----------|------------|
/// | 0   | OpenSender | OpenSender |
pub struct ISenderService;

impl ISenderService {
    pub fn new() -> Self {
        ISenderService
    }

    /// Cmd 0: OpenSender
    ///
    /// Creates a new ISender instance with the given sender_id and data.
    /// Upstream creates an ISender (separate class).
    pub fn open_sender(&self, sender_id: u32, data: [u64; 2]) -> ResultCode {
        log::warn!(
            "(STUBBED) ISenderService::open_sender called, sender_id={}, data={:016X} {:016X}",
            sender_id,
            data[0],
            data[1]
        );
        // TODO: return ISender once ported
        RESULT_SUCCESS
    }
}

// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/ovln/receiver_service.h
//! Port of zuyu/src/core/hle/service/psc/ovln/receiver_service.cpp
//!
//! IReceiverService: "ovln:rcv" service.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command table for IReceiverService.
///
/// | Cmd | Handler      | Name         |
/// |-----|-------------|--------------|
/// | 0   | OpenReceiver | OpenReceiver |
pub struct IReceiverService;

impl IReceiverService {
    pub fn new() -> Self {
        IReceiverService
    }

    /// Cmd 0: OpenReceiver
    ///
    /// Creates a new IReceiver instance. Upstream creates an IReceiver
    /// (separate class); here we return success since IReceiver is a thin wrapper.
    pub fn open_receiver(&self) -> ResultCode {
        log::debug!("IReceiverService::open_receiver called");
        // TODO: return IReceiver once ported
        RESULT_SUCCESS
    }
}

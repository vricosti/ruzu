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
    /// Creates a new IReceiver instance. Upstream creates an IReceiver (separate class).
    /// The IReceiver commands (AddSource, RemoveSource, GetReceiveEventHandle, Receive,
    /// ReceiveWithTick) are defined in receiver.rs but are all unimplemented (nullptr)
    /// in upstream, so returning success here matches upstream behavior.
    pub fn open_receiver(&self) -> ResultCode {
        log::debug!("IReceiverService::open_receiver called");
        // IReceiver is defined in receiver.rs with its command table. All upstream
        // handlers are nullptr, so creating the interface has no observable effect
        // beyond registering the empty command table.
        RESULT_SUCCESS
    }
}

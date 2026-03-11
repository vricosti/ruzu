// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/event_observer.h
//! Port of zuyu/src/core/hle/service/am/event_observer.cpp

/// Tag for multi-wait user data.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UserDataTag {
    WakeupEvent = 0,
    AppletProcess = 1,
}

/// Port of EventObserver
///
/// Observes applet process lifecycle events and triggers WindowSystem updates.
/// Stubbed: requires kernel multi-wait, threading, and process holder integration.
pub struct EventObserver {
    // TODO: System reference, ServiceContext, WindowSystem reference
    // TODO: wakeup event, multi-wait holders
    // TODO: process holder list, processing thread
}

impl EventObserver {
    pub fn new() -> Self {
        Self {}
    }

    pub fn request_update(&self) {
        // TODO: signal wakeup event
    }
}

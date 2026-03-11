// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/library_applet_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/library_applet_accessor.cpp

/// IPC command table for ILibraryAppletAccessor:
/// - 0: GetAppletStateChangedEvent
/// - 1: IsCompleted
/// - 10: Start
/// - 20: RequestExit
/// - 25: Terminate
/// - 30: GetResult
/// - 60: PresetLibraryAppletGpuTimeSliceZero
/// - 100: PushInData
/// - 101: PopOutData
/// - 103: PushInteractiveInData
/// - 104: PopInteractiveOutData
/// - 105: GetPopOutDataEvent
/// - 106: GetPopInteractiveOutDataEvent
/// - 160: GetIndirectLayerConsumerHandle
pub struct ILibraryAppletAccessor {
    // TODO: AppletDataBroker, Applet references
}

impl ILibraryAppletAccessor {
    pub fn new() -> Self {
        Self {}
    }
}

// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/lifecycle_manager.h
//! Port of zuyu/src/core/hle/service/am/lifecycle_manager.cpp

use std::collections::VecDeque;

use super::am_types::{AppletMessage, FocusState};

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ActivityState {
    #[default]
    ForegroundVisible = 0,
    ForegroundObscured = 1,
    BackgroundVisible = 2,
    BackgroundObscured = 3,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FocusHandlingMode {
    AlwaysSuspend = 0,
    #[default]
    SuspendHomeSleep = 1,
    NoSuspend = 2,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SuspendMode {
    #[default]
    NoOverride = 0,
    ForceResume = 1,
    ForceSuspend = 2,
}

pub struct LifecycleManager {
    // Events - stubbed as booleans until kernel event types are wired
    system_event_signaled: bool,
    operation_mode_changed_system_event_signaled: bool,

    unordered_messages: VecDeque<AppletMessage>,

    is_application: bool,
    focus_state_changed_notification_enabled: bool,
    operation_mode_changed_notification_enabled: bool,
    performance_mode_changed_notification_enabled: bool,
    resume_notification_enabled: bool,

    requested_request_to_display_state: bool,
    acknowledged_request_to_display_state: bool,
    has_resume: bool,
    has_focus_state_changed: bool,
    has_album_recording_saved: bool,
    has_album_screen_shot_taken: bool,
    has_auto_power_down: bool,
    has_sleep_required_by_low_battery: bool,
    has_sleep_required_by_high_temperature: bool,
    has_sd_card_removed: bool,
    has_performance_mode_changed: bool,
    has_operation_mode_changed: bool,
    has_requested_request_to_prepare_sleep: bool,
    has_acknowledged_request_to_prepare_sleep: bool,
    has_requested_exit: bool,
    has_acknowledged_exit: bool,
    applet_message_available: bool,

    forced_suspend: bool,
    focus_handling_mode: FocusHandlingMode,
    activity_state: ActivityState,
    suspend_mode: SuspendMode,
    requested_focus_state: FocusState,
    acknowledged_focus_state: FocusState,
}

impl LifecycleManager {
    pub fn new(is_application: bool) -> Self {
        Self {
            system_event_signaled: false,
            operation_mode_changed_system_event_signaled: false,
            unordered_messages: VecDeque::new(),
            is_application,
            focus_state_changed_notification_enabled: true,
            operation_mode_changed_notification_enabled: true,
            performance_mode_changed_notification_enabled: true,
            resume_notification_enabled: false,
            requested_request_to_display_state: false,
            acknowledged_request_to_display_state: false,
            has_resume: false,
            has_focus_state_changed: true,
            has_album_recording_saved: false,
            has_album_screen_shot_taken: false,
            has_auto_power_down: false,
            has_sleep_required_by_low_battery: false,
            has_sleep_required_by_high_temperature: false,
            has_sd_card_removed: false,
            has_performance_mode_changed: false,
            has_operation_mode_changed: false,
            has_requested_request_to_prepare_sleep: false,
            has_acknowledged_request_to_prepare_sleep: false,
            has_requested_exit: false,
            has_acknowledged_exit: false,
            applet_message_available: false,
            forced_suspend: false,
            focus_handling_mode: FocusHandlingMode::SuspendHomeSleep,
            activity_state: ActivityState::ForegroundVisible,
            suspend_mode: SuspendMode::NoOverride,
            requested_focus_state: FocusState::default(),
            acknowledged_focus_state: FocusState::default(),
        }
    }

    pub fn is_application(&self) -> bool {
        self.is_application
    }

    pub fn get_forced_suspend(&self) -> bool {
        self.forced_suspend
    }

    pub fn get_exit_requested(&self) -> bool {
        self.has_requested_exit
    }

    pub fn get_activity_state(&self) -> ActivityState {
        self.activity_state
    }

    pub fn get_and_clear_focus_state(&mut self) -> FocusState {
        self.acknowledged_focus_state = self.requested_focus_state;
        self.acknowledged_focus_state
    }

    pub fn set_focus_state(&mut self, state: FocusState) {
        if self.requested_focus_state != state {
            self.has_focus_state_changed = true;
        }
        self.requested_focus_state = state;
        self.signal_system_event_if_needed();
    }

    pub fn request_exit(&mut self) {
        self.has_requested_exit = true;
        self.signal_system_event_if_needed();
    }

    pub fn request_resume_notification(&mut self) {
        if self.resume_notification_enabled {
            self.has_resume = true;
        }
    }

    pub fn on_operation_and_performance_mode_changed(&mut self) {
        if self.operation_mode_changed_notification_enabled {
            self.has_operation_mode_changed = true;
        }
        if self.performance_mode_changed_notification_enabled {
            self.has_performance_mode_changed = true;
        }
        self.operation_mode_changed_system_event_signaled = true;
        self.signal_system_event_if_needed();
    }

    pub fn set_focus_state_changed_notification_enabled(&mut self, enabled: bool) {
        self.focus_state_changed_notification_enabled = enabled;
        self.signal_system_event_if_needed();
    }

    pub fn set_operation_mode_changed_notification_enabled(&mut self, enabled: bool) {
        self.operation_mode_changed_notification_enabled = enabled;
        self.signal_system_event_if_needed();
    }

    pub fn set_performance_mode_changed_notification_enabled(&mut self, enabled: bool) {
        self.performance_mode_changed_notification_enabled = enabled;
        self.signal_system_event_if_needed();
    }

    pub fn set_resume_notification_enabled(&mut self, enabled: bool) {
        self.resume_notification_enabled = enabled;
    }

    pub fn set_activity_state(&mut self, state: ActivityState) {
        self.activity_state = state;
    }

    pub fn set_suspend_mode(&mut self, mode: SuspendMode) {
        self.suspend_mode = mode;
    }

    pub fn set_forced_suspend(&mut self, enabled: bool) {
        self.forced_suspend = enabled;
    }

    pub fn set_focus_handling_mode(&mut self, suspend: bool) {
        match self.focus_handling_mode {
            FocusHandlingMode::AlwaysSuspend | FocusHandlingMode::SuspendHomeSleep => {
                if !suspend {
                    self.focus_handling_mode = FocusHandlingMode::NoSuspend;
                }
            }
            FocusHandlingMode::NoSuspend => {
                if suspend {
                    self.focus_handling_mode = FocusHandlingMode::SuspendHomeSleep;
                }
            }
        }
    }

    pub fn set_out_of_focus_suspending_enabled(&mut self, enabled: bool) {
        match self.focus_handling_mode {
            FocusHandlingMode::AlwaysSuspend => {
                if !enabled {
                    self.focus_handling_mode = FocusHandlingMode::SuspendHomeSleep;
                }
            }
            FocusHandlingMode::SuspendHomeSleep | FocusHandlingMode::NoSuspend => {
                if enabled {
                    self.focus_handling_mode = FocusHandlingMode::AlwaysSuspend;
                }
            }
        }
    }

    pub fn remove_force_resume_if_possible(&mut self) {
        if self.suspend_mode != SuspendMode::ForceResume {
            return;
        }

        match self.activity_state {
            ActivityState::ForegroundVisible | ActivityState::ForegroundObscured => {
                self.suspend_mode = SuspendMode::NoOverride;
                return;
            }
            _ => {}
        }

        match self.focus_handling_mode {
            FocusHandlingMode::AlwaysSuspend | FocusHandlingMode::SuspendHomeSleep => {
                self.suspend_mode = SuspendMode::NoOverride;
            }
            FocusHandlingMode::NoSuspend => {
                if !self.is_application {
                    self.suspend_mode = SuspendMode::NoOverride;
                }
            }
        }
    }

    pub fn is_runnable(&self) -> bool {
        if self.forced_suspend {
            return false;
        }

        match self.suspend_mode {
            SuspendMode::NoOverride => {}
            SuspendMode::ForceResume => return self.has_requested_exit,
            SuspendMode::ForceSuspend => return false,
        }

        if self.has_requested_exit {
            return true;
        }

        if self.activity_state == ActivityState::ForegroundVisible {
            return true;
        }

        if self.activity_state == ActivityState::ForegroundObscured {
            return match self.focus_handling_mode {
                FocusHandlingMode::AlwaysSuspend => false,
                FocusHandlingMode::SuspendHomeSleep => true,
                FocusHandlingMode::NoSuspend => true,
            };
        }

        self.focus_handling_mode == FocusHandlingMode::NoSuspend
    }

    pub fn update_requested_focus_state(&mut self) -> bool {
        let new_state = if self.suspend_mode == SuspendMode::NoOverride {
            match self.activity_state {
                ActivityState::ForegroundVisible => FocusState::InFocus,
                ActivityState::ForegroundObscured => {
                    self.get_focus_state_while_foreground_obscured()
                }
                ActivityState::BackgroundVisible => self.get_focus_state_while_background(false),
                ActivityState::BackgroundObscured => self.get_focus_state_while_background(true),
            }
        } else {
            self.get_focus_state_while_background(false)
        };

        if new_state != self.requested_focus_state {
            self.requested_focus_state = new_state;
            true
        } else {
            false
        }
    }

    pub fn signal_system_event_if_needed(&mut self) {
        let applet_message_available = self.applet_message_available;

        if applet_message_available != self.should_signal_system_event() {
            if !applet_message_available {
                self.system_event_signaled = true;
                self.applet_message_available = true;
            } else {
                self.system_event_signaled = false;
                self.applet_message_available = false;
            }
        }
    }

    pub fn push_unordered_message(&mut self, message: AppletMessage) {
        self.unordered_messages.push_back(message);
        self.signal_system_event_if_needed();
    }

    pub fn pop_message(&mut self, out_message: &mut AppletMessage) -> bool {
        let message = self.pop_message_in_order_of_priority();
        self.signal_system_event_if_needed();

        *out_message = message;
        message != AppletMessage::None
    }

    fn get_focus_state_while_foreground_obscured(&self) -> FocusState {
        match self.focus_handling_mode {
            FocusHandlingMode::AlwaysSuspend => FocusState::InFocus,
            FocusHandlingMode::SuspendHomeSleep => FocusState::NotInFocus,
            FocusHandlingMode::NoSuspend => FocusState::NotInFocus,
        }
    }

    fn get_focus_state_while_background(&self, is_obscured: bool) -> FocusState {
        match self.focus_handling_mode {
            FocusHandlingMode::AlwaysSuspend => FocusState::InFocus,
            FocusHandlingMode::SuspendHomeSleep => {
                if is_obscured {
                    FocusState::NotInFocus
                } else {
                    FocusState::InFocus
                }
            }
            FocusHandlingMode::NoSuspend => {
                if self.is_application {
                    FocusState::Background
                } else {
                    FocusState::NotInFocus
                }
            }
        }
    }

    fn pop_message_in_order_of_priority(&mut self) -> AppletMessage {
        if self.has_resume {
            self.has_resume = false;
            return AppletMessage::Resume;
        }

        if self.has_acknowledged_exit != self.has_requested_exit {
            self.has_acknowledged_exit = self.has_requested_exit;
            return AppletMessage::Exit;
        }

        if self.focus_state_changed_notification_enabled {
            if !self.is_application {
                if self.requested_focus_state != self.acknowledged_focus_state {
                    self.acknowledged_focus_state = self.requested_focus_state;
                    return match self.requested_focus_state {
                        FocusState::InFocus => AppletMessage::ChangeIntoForeground,
                        FocusState::NotInFocus => AppletMessage::ChangeIntoBackground,
                        _ => {
                            log::error!("Unexpected focus state in pop_message_in_order_of_priority");
                            AppletMessage::None
                        }
                    };
                }
            } else if self.has_focus_state_changed {
                self.has_focus_state_changed = false;
                return AppletMessage::FocusStateChanged;
            }
        }

        if self.has_requested_request_to_prepare_sleep
            != self.has_acknowledged_request_to_prepare_sleep
        {
            self.has_acknowledged_request_to_prepare_sleep = true;
            return AppletMessage::RequestToPrepareSleep;
        }

        if self.requested_request_to_display_state != self.acknowledged_request_to_display_state {
            self.acknowledged_request_to_display_state = self.requested_request_to_display_state;
            return AppletMessage::RequestToDisplay;
        }

        if self.has_operation_mode_changed {
            self.has_operation_mode_changed = false;
            return AppletMessage::OperationModeChanged;
        }

        if self.has_performance_mode_changed {
            self.has_performance_mode_changed = false;
            return AppletMessage::PerformanceModeChanged;
        }

        if self.has_sd_card_removed {
            self.has_sd_card_removed = false;
            return AppletMessage::SdCardRemoved;
        }

        if self.has_sleep_required_by_high_temperature {
            self.has_sleep_required_by_high_temperature = false;
            return AppletMessage::SleepRequiredByHighTemperature;
        }

        if self.has_sleep_required_by_low_battery {
            self.has_sleep_required_by_low_battery = false;
            return AppletMessage::SleepRequiredByLowBattery;
        }

        if self.has_auto_power_down {
            self.has_auto_power_down = false;
            return AppletMessage::AutoPowerDown;
        }

        if self.has_album_screen_shot_taken {
            self.has_album_screen_shot_taken = false;
            return AppletMessage::AlbumScreenShotTaken;
        }

        if self.has_album_recording_saved {
            self.has_album_recording_saved = false;
            return AppletMessage::AlbumRecordingSaved;
        }

        if let Some(message) = self.unordered_messages.pop_front() {
            return message;
        }

        AppletMessage::None
    }

    fn should_signal_system_event(&self) -> bool {
        if self.focus_state_changed_notification_enabled {
            if !self.is_application {
                if self.requested_focus_state != self.acknowledged_focus_state {
                    return true;
                }
            } else if self.has_focus_state_changed {
                return true;
            }
        }

        !self.unordered_messages.is_empty()
            || self.has_resume
            || (self.has_requested_exit != self.has_acknowledged_exit)
            || (self.has_requested_request_to_prepare_sleep
                != self.has_acknowledged_request_to_prepare_sleep)
            || self.has_operation_mode_changed
            || self.has_performance_mode_changed
            || self.has_sd_card_removed
            || self.has_sleep_required_by_high_temperature
            || self.has_sleep_required_by_low_battery
            || self.has_auto_power_down
            || (self.requested_request_to_display_state
                != self.acknowledged_request_to_display_state)
            || self.has_album_screen_shot_taken
            || self.has_album_recording_saved
    }
}

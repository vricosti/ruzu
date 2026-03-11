// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/applet.h
//! Port of zuyu/src/core/hle/service/am/applet.cpp

use super::am_types::*;
use super::lifecycle_manager::LifecycleManager;

/// Stub for the Applet struct.
/// Many fields reference external crate types (Process, Event, HidRegistration, etc.)
/// that are not yet wired. These are represented as placeholders.
pub struct Applet {
    // Lifecycle manager
    pub lifecycle_manager: LifecycleManager,

    // Process state (stubbed)
    pub is_process_running: bool,

    // Creation state
    pub applet_id: AppletId,
    pub aruid: AppletResourceUserId,
    pub launch_reason: AppletProcessLaunchReason,
    pub applet_type: AppletType,
    pub program_id: ProgramId,
    pub library_applet_mode: LibraryAppletMode,
    pub previous_program_index: i32,
    pub previous_screenshot_permission: ScreenshotPermission,

    pub screen_shot_identity: AppletIdentityInfo,

    // Applet common functions
    pub terminate_result: u32,
    pub display_logical_width: i32,
    pub display_logical_height: i32,
    pub home_button_double_click_enabled: bool,
    pub home_button_short_pressed_blocked: bool,
    pub home_button_long_pressed_blocked: bool,
    pub vr_mode_curtain_required: bool,
    pub sleep_required_by_high_temperature: bool,
    pub sleep_required_by_low_battery: bool,
    pub cpu_boost_request_priority: i32,
    pub handling_capture_button_short_pressed_message_enabled_for_applet: bool,
    pub handling_capture_button_long_pressed_message_enabled_for_applet: bool,
    pub application_core_usage_mode: u32,

    // Application functions
    pub game_play_recording_supported: bool,
    pub game_play_recording_state: GamePlayRecordingState,
    pub jit_service_launched: bool,
    pub application_crash_report_enabled: bool,

    // Common state
    pub sleep_lock_enabled: bool,
    pub vr_mode_enabled: bool,
    pub lcd_backlight_off_enabled: bool,
    pub request_exit_to_library_applet_at_execute_next_program_enabled: bool,

    // Channels
    pub user_channel_launch_parameter: std::collections::VecDeque<Vec<u8>>,
    pub preselected_user_launch_parameter: std::collections::VecDeque<Vec<u8>>,

    // Caller applet (stubbed - would be Weak<Applet> in full impl)
    pub is_completed: bool,

    // Self state
    pub exit_locked: bool,
    pub fatal_section_count: i32,
    pub handles_request_to_display: bool,
    pub screenshot_permission: ScreenshotPermission,
    pub idle_time_detection_extension: IdleTimeDetectionExtension,
    pub auto_sleep_disabled: bool,
    pub suspended_ticks: u64,
    pub album_image_taken_notification_enabled: bool,
    pub record_volume_muted: bool,
    pub is_activity_runnable: bool,
    pub is_interactible: bool,
    pub window_visible: bool,
}

impl Applet {
    pub fn new(is_application: bool) -> Self {
        Self {
            lifecycle_manager: LifecycleManager::new(is_application),
            is_process_running: false,
            applet_id: AppletId::default(),
            aruid: AppletResourceUserId::default(),
            launch_reason: AppletProcessLaunchReason::default(),
            applet_type: AppletType::default(),
            program_id: 0,
            library_applet_mode: LibraryAppletMode::default(),
            previous_program_index: -1,
            previous_screenshot_permission: ScreenshotPermission::Enable,
            screen_shot_identity: AppletIdentityInfo::default(),
            terminate_result: 0,
            display_logical_width: 0,
            display_logical_height: 0,
            home_button_double_click_enabled: false,
            home_button_short_pressed_blocked: false,
            home_button_long_pressed_blocked: false,
            vr_mode_curtain_required: false,
            sleep_required_by_high_temperature: false,
            sleep_required_by_low_battery: false,
            cpu_boost_request_priority: -1,
            handling_capture_button_short_pressed_message_enabled_for_applet: false,
            handling_capture_button_long_pressed_message_enabled_for_applet: false,
            application_core_usage_mode: 0,
            game_play_recording_supported: false,
            game_play_recording_state: GamePlayRecordingState::Disabled,
            jit_service_launched: false,
            application_crash_report_enabled: false,
            sleep_lock_enabled: false,
            vr_mode_enabled: false,
            lcd_backlight_off_enabled: false,
            request_exit_to_library_applet_at_execute_next_program_enabled: false,
            user_channel_launch_parameter: std::collections::VecDeque::new(),
            preselected_user_launch_parameter: std::collections::VecDeque::new(),
            is_completed: false,
            exit_locked: false,
            fatal_section_count: 0,
            handles_request_to_display: false,
            screenshot_permission: ScreenshotPermission::default(),
            idle_time_detection_extension: IdleTimeDetectionExtension::default(),
            auto_sleep_disabled: false,
            suspended_ticks: 0,
            album_image_taken_notification_enabled: false,
            record_volume_muted: false,
            is_activity_runnable: false,
            is_interactible: true,
            window_visible: true,
        }
    }

    /// Port of Applet::UpdateSuspensionStateLocked
    pub fn update_suspension_state_locked(&mut self, force_message: bool) {
        self.lifecycle_manager.remove_force_resume_if_possible();

        let curr_activity_runnable = self.lifecycle_manager.is_runnable();
        let prev_activity_runnable = self.is_activity_runnable;
        let was_changed = curr_activity_runnable != prev_activity_runnable;

        if was_changed {
            if !curr_activity_runnable {
                self.lifecycle_manager.request_resume_notification();
            }
            self.is_activity_runnable = curr_activity_runnable;
        }

        if self.lifecycle_manager.get_forced_suspend() {
            return;
        }

        if self.lifecycle_manager.update_requested_focus_state() || was_changed || force_message {
            self.lifecycle_manager.signal_system_event_if_needed();
        }
    }

    /// Port of Applet::SetInteractibleLocked
    pub fn set_interactible_locked(&mut self, interactible: bool) {
        if self.is_interactible == interactible {
            return;
        }
        self.is_interactible = interactible;
        // TODO: hid_registration.enable_applet_to_get_input(interactible && !lifecycle_manager.get_exit_requested())
    }

    /// Port of Applet::OnProcessTerminatedLocked
    pub fn on_process_terminated_locked(&mut self) {
        self.is_completed = true;
        // TODO: state_changed_event.signal()
    }
}

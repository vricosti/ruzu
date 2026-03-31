// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/glue/notif.h
//! Port of zuyu/src/core/hle/service/glue/notif.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// nn::notification::AlarmSettingId
pub type AlarmSettingId = u16;

pub type ApplicationParameter = [u8; 0x400];

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct DailyAlarmSetting {
    pub hour: i8,
    pub minute: i8,
}

const _: () = assert!(core::mem::size_of::<DailyAlarmSetting>() == 0x2);

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct WeeklyScheduleAlarmSetting {
    pub _padding: [u8; 0xA],
    pub day_of_week: [DailyAlarmSetting; 0x7],
}

const _: () = assert!(core::mem::size_of::<WeeklyScheduleAlarmSetting>() == 0x18);

impl Default for WeeklyScheduleAlarmSetting {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// nn::notification::AlarmSetting
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct AlarmSetting {
    pub alarm_setting_id: AlarmSettingId,
    pub kind: u8,
    pub muted: u8,
    pub _padding1: [u8; 0x4],
    pub account_id: [u8; 16],
    pub application_id: u64,
    pub _padding2: [u8; 0x8],
    pub schedule: WeeklyScheduleAlarmSetting,
}

const _: () = assert!(core::mem::size_of::<AlarmSetting>() == 0x40);

impl Default for AlarmSetting {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NotificationChannel {
    Unknown0 = 0,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct NotificationPresentationSetting {
    pub _padding: [u8; 0x10],
}

const _: () = assert!(core::mem::size_of::<NotificationPresentationSetting>() == 0x10);

const MAX_ALARMS: usize = 8;

/// NotificationServiceImpl: shared alarm management logic.
///
/// Corresponds to `NotificationServiceImpl` in upstream `notif.cpp`.
pub struct NotificationServiceImpl {
    pub alarms: Vec<AlarmSetting>,
    pub last_alarm_setting_id: AlarmSettingId,
}

impl NotificationServiceImpl {
    pub fn new() -> Self {
        Self {
            alarms: Vec::new(),
            last_alarm_setting_id: 0,
        }
    }

    pub fn register_alarm_setting(
        &mut self,
        alarm_setting: &AlarmSetting,
        _application_parameter: &[u8],
    ) -> (ResultCode, AlarmSettingId) {
        if self.alarms.len() > MAX_ALARMS {
            log::error!("Alarm limit reached");
            return (ResultCode::new(u32::MAX), 0);
        }

        let mut new_alarm = *alarm_setting;
        new_alarm.alarm_setting_id = self.last_alarm_setting_id;
        self.last_alarm_setting_id += 1;
        self.alarms.push(new_alarm);

        log::warn!(
            "(STUBBED) register_alarm_setting called, setting_id={}, kind={}, muted={}",
            new_alarm.alarm_setting_id,
            new_alarm.kind,
            new_alarm.muted,
        );

        (RESULT_SUCCESS, new_alarm.alarm_setting_id)
    }

    pub fn update_alarm_setting(
        &mut self,
        alarm_setting: &AlarmSetting,
        _application_parameter: &[u8],
    ) -> ResultCode {
        if let Some(alarm) = self
            .alarms
            .iter_mut()
            .find(|a| a.alarm_setting_id == alarm_setting.alarm_setting_id)
        {
            *alarm = *alarm_setting;
        }
        log::warn!("(STUBBED) update_alarm_setting called");
        RESULT_SUCCESS
    }

    pub fn list_alarm_settings(&self, out_alarms: &mut [AlarmSetting]) -> (ResultCode, i32) {
        let count = std::cmp::min(out_alarms.len(), self.alarms.len());
        for i in 0..count {
            out_alarms[i] = self.alarms[i];
        }
        log::info!(
            "list_alarm_settings called, alarm_count={}",
            self.alarms.len()
        );
        (RESULT_SUCCESS, count as i32)
    }

    pub fn delete_alarm_setting(&mut self, alarm_setting_id: AlarmSettingId) -> ResultCode {
        self.alarms
            .retain(|a| a.alarm_setting_id != alarm_setting_id);
        log::info!(
            "delete_alarm_setting called, alarm_setting_id={}",
            alarm_setting_id
        );
        RESULT_SUCCESS
    }

    pub fn initialize(&mut self, _aruid: u64) -> ResultCode {
        log::warn!("(STUBBED) initialize called");
        RESULT_SUCCESS
    }
}

/// INotificationServicesForApplication ("notif:a")
pub struct INotificationServicesForApplication {
    pub impl_: NotificationServiceImpl,
}

impl INotificationServicesForApplication {
    pub fn new() -> Self {
        Self {
            impl_: NotificationServiceImpl::new(),
        }
    }
}

/// INotificationServices ("notif:s")
pub struct INotificationServices {
    pub impl_: NotificationServiceImpl,
}

impl INotificationServices {
    pub fn new() -> Self {
        Self {
            impl_: NotificationServiceImpl::new(),
        }
    }
}

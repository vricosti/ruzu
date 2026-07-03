// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/frontend/applet_mii_edit.h
//! Port of zuyu/src/core/hle/service/am/frontend/applet_mii_edit.cpp

use std::sync::Arc;
use std::sync::Mutex;

use crate::core::SystemRef;
use crate::hle::service::am::am_types::LibraryAppletMode;
use crate::hle::service::am::applet_data_broker::AppletDataBroker;
use crate::hle::service::mii::mii::{IStaticService, SERVICE_NAME_E};
use crate::hle::service::mii::mii_manager::MiiManager;
use crate::hle::service::mii::mii_types::{Age, DatabaseSessionMetadata, Gender, Nickname, Race};
use crate::hle::service::mii::types::char_info::CharInfo;
use crate::hle::service::mii::types::store_data::StoreData;
use crate::hle::service::sm::sm::ServiceManager;

use super::applet_mii_edit_types::{
    MiiEditAppletInputCommon, MiiEditAppletInputV3, MiiEditAppletInputV4, MiiEditAppletMode,
    MiiEditAppletOutput, MiiEditAppletOutputForCharInfoEditing, MiiEditResult,
};
use super::applets::FrontendApplet;

fn copy_from_prefix<T: Copy>(data: &[u8]) -> Option<T> {
    if data.len() < std::mem::size_of::<T>() {
        return None;
    }

    let mut value = std::mem::MaybeUninit::<T>::uninit();
    unsafe {
        std::ptr::copy_nonoverlapping(
            data.as_ptr(),
            value.as_mut_ptr().cast::<u8>(),
            std::mem::size_of::<T>(),
        );
        Some(value.assume_init())
    }
}

fn struct_to_vec<T>(value: &T) -> Vec<u8> {
    unsafe {
        std::slice::from_raw_parts((value as *const T).cast::<u8>(), std::mem::size_of::<T>())
            .to_vec()
    }
}

/// Port of `Service::AM::Frontend::MiiEdit`.
pub struct MiiEdit {
    #[allow(dead_code)]
    system: SystemRef,
    broker: Arc<AppletDataBroker>,
    applet_mode: LibraryAppletMode,
    initialized: bool,
    is_complete: bool,
    applet_input_common: MiiEditAppletInputCommon,
    applet_input_v3: MiiEditAppletInputV3,
    applet_input_v4: MiiEditAppletInputV4,
    manager: Option<Arc<Mutex<MiiManager>>>,
    metadata: DatabaseSessionMetadata,
}

impl MiiEdit {
    pub fn new(
        system: SystemRef,
        broker: Arc<AppletDataBroker>,
        applet_mode: LibraryAppletMode,
    ) -> Self {
        Self {
            system,
            broker,
            applet_mode,
            initialized: false,
            is_complete: false,
            applet_input_common: MiiEditAppletInputCommon::default(),
            applet_input_v3: MiiEditAppletInputV3::default(),
            applet_input_v4: MiiEditAppletInputV4::default(),
            manager: None,
            metadata: DatabaseSessionMetadata::default(),
        }
    }

    fn get_mii_manager(&self) -> Arc<Mutex<MiiManager>> {
        let Some(service_manager) = self.system.get().service_manager() else {
            return Arc::new(Mutex::new(MiiManager::new()));
        };

        let handler =
            ServiceManager::get_service_blocking(&service_manager, self.system, SERVICE_NAME_E);
        if let Some(service) = handler.as_any().downcast_ref::<IStaticService>() {
            return service.get_mii_manager();
        }

        log::error!("mii:e is not an IStaticService");
        Arc::new(Mutex::new(MiiManager::new()))
    }

    fn mii_edit_output(&mut self, result: MiiEditResult, index: i32) {
        let mut output = MiiEditAppletOutput::default();
        output.result = result as u32;
        output.index = index;

        log::info!(
            "MiiEdit::MiiEditOutput called, result={:?}, index={}",
            result,
            index
        );

        self.is_complete = true;
        self.broker.get_out_data().push(struct_to_vec(&output));
    }

    fn mii_edit_output_for_char_info_editing(
        &mut self,
        result: MiiEditResult,
        char_info: &CharInfo,
    ) {
        let mut output = MiiEditAppletOutputForCharInfoEditing::default();
        output.result = result as u32;
        let bytes = unsafe {
            std::slice::from_raw_parts(
                (char_info as *const CharInfo).cast::<u8>(),
                std::mem::size_of::<CharInfo>(),
            )
        };
        output.char_info.mii_info.copy_from_slice(bytes);

        self.is_complete = true;
        self.broker.get_out_data().push(struct_to_vec(&output));
    }

    fn mode(&self) -> Option<MiiEditAppletMode> {
        match self.applet_input_common.applet_mode {
            0 => Some(MiiEditAppletMode::ShowMiiEdit),
            1 => Some(MiiEditAppletMode::AppendMii),
            2 => Some(MiiEditAppletMode::AppendMiiImage),
            3 => Some(MiiEditAppletMode::UpdateMiiImage),
            4 => Some(MiiEditAppletMode::CreateMii),
            5 => Some(MiiEditAppletMode::EditMii),
            _ => None,
        }
    }

    pub fn is_complete(&self) -> bool {
        self.is_complete
    }
}

impl FrontendApplet for MiiEdit {
    fn initialize(&mut self) {
        let Ok(applet_input_data) = self.broker.get_in_data().pop() else {
            log::error!("MiiEdit::Initialize missing input storage");
            return;
        };

        let Some(common) = copy_from_prefix::<MiiEditAppletInputCommon>(&applet_input_data) else {
            log::error!(
                "MiiEdit::Initialize input too small: {}",
                applet_input_data.len()
            );
            return;
        };
        self.applet_input_common = common;

        log::info!(
            "Initializing MiiEdit Applet with MiiEditAppletVersion={} and MiiEditAppletMode={}",
            self.applet_input_common.version,
            self.applet_input_common.applet_mode
        );

        let payload = &applet_input_data[std::mem::size_of::<MiiEditAppletInputCommon>()..];
        match self.applet_input_common.version {
            3 => {
                if let Some(input) = copy_from_prefix::<MiiEditAppletInputV3>(payload) {
                    self.applet_input_v3 = input;
                }
            }
            4 => {
                if let Some(input) = copy_from_prefix::<MiiEditAppletInputV4>(payload) {
                    self.applet_input_v4 = input;
                }
            }
            _ => {
                log::error!(
                    "Unknown MiiEditAppletVersion={} with size={}",
                    self.applet_input_common.version,
                    applet_input_data.len()
                );
                if let Some(input) = copy_from_prefix::<MiiEditAppletInputV4>(payload) {
                    self.applet_input_v4 = input;
                }
            }
        }

        let manager = self.get_mii_manager();
        {
            let mut manager = manager.lock().unwrap();
            let _ = manager.initialize(&mut self.metadata);
        }
        self.manager = Some(manager);
        self.initialized = true;
    }

    fn execute_interactive(&mut self) {
        panic!("Attempted to call interactive execution on non-interactive applet.");
    }

    fn execute(&mut self) {
        if self.is_complete {
            return;
        }

        match self.mode() {
            Some(MiiEditAppletMode::ShowMiiEdit)
            | Some(MiiEditAppletMode::AppendMiiImage)
            | Some(MiiEditAppletMode::UpdateMiiImage) => {
                self.mii_edit_output(MiiEditResult::Success, 0);
            }
            Some(MiiEditAppletMode::AppendMii) => {
                let mut store_data = StoreData::new();
                store_data.build_random(Age::All, Gender::All, Race::All);
                store_data.set_nickname(Nickname {
                    data: [
                        b'y' as u16,
                        b'u' as u16,
                        b'z' as u16,
                        b'u' as u16,
                        0,
                        0,
                        0,
                        0,
                        0,
                        0,
                    ],
                });
                store_data.set_checksum();

                let Some(manager) = self.manager.as_ref().map(Arc::clone) else {
                    self.mii_edit_output(MiiEditResult::Cancel, 0);
                    return;
                };
                let result = manager
                    .lock()
                    .unwrap()
                    .add_or_replace(&mut self.metadata, store_data);
                if result.is_error() {
                    self.mii_edit_output(MiiEditResult::Cancel, 0);
                    return;
                }

                let index = manager
                    .lock()
                    .unwrap()
                    .find_index(store_data.get_create_id(), false);
                if index == -1 {
                    self.mii_edit_output(MiiEditResult::Cancel, 0);
                    return;
                }
                self.mii_edit_output(MiiEditResult::Success, index);
            }
            Some(MiiEditAppletMode::CreateMii) => {
                let Some(manager) = self.manager.as_ref() else {
                    self.mii_edit_output(MiiEditResult::Cancel, 0);
                    return;
                };
                let char_info =
                    manager
                        .lock()
                        .unwrap()
                        .build_random(Age::All, Gender::All, Race::All);
                self.mii_edit_output_for_char_info_editing(MiiEditResult::Success, &char_info);
            }
            Some(MiiEditAppletMode::EditMii) => {
                let mut char_info = CharInfo::default();
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        self.applet_input_v4.char_info.as_ptr(),
                        (&mut char_info as *mut CharInfo).cast::<u8>(),
                        std::mem::size_of::<CharInfo>(),
                    );
                }
                self.mii_edit_output_for_char_info_editing(MiiEditResult::Success, &char_info);
            }
            None => {
                log::error!(
                    "Unknown MiiEditAppletMode={}",
                    self.applet_input_common.applet_mode
                );
                self.mii_edit_output(MiiEditResult::Success, 0);
            }
        }
    }

    fn request_exit(&mut self) {}

    fn get_library_applet_mode(&self) -> LibraryAppletMode {
        self.applet_mode
    }

    fn is_initialized(&self) -> bool {
        self.initialized
    }
}

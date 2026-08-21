// SPDX-FileCopyrightText: Copyright 2019 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/backend/backend.h
//! Port of zuyu/src/core/hle/service/bcat/backend/backend.cpp
//!
//! ProgressServiceBackend: manages download progress signaling.
//! BcatBackend trait: abstract backend for BCAT functionality.
//! NullBcatBackend: no-op backend implementation.

use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::bcat::bcat_types::*;
use crate::hle::service::kernel_helpers::ServiceContext;
use crate::hle::service::os::event::Event;

/// ProgressServiceBackend manages progress signaling for BCAT downloads.
///
/// Corresponds to `ProgressServiceBackend` in upstream `backend.h`.
pub struct ProgressServiceBackend {
    pub impl_data: DeliveryCacheProgressImpl,
    pub event_name: String,
    service_context: ServiceContext,
    update_event_handle: u32,
}

impl ProgressServiceBackend {
    pub fn new(event_name: &str) -> Self {
        let mut service_context = ServiceContext::new("ProgressServiceBackend".to_string());
        let update_event_handle = service_context
            .create_event(format!("ProgressServiceBackend:UpdateEvent:{}", event_name));
        Self {
            impl_data: DeliveryCacheProgressImpl::default(),
            event_name: event_name.to_string(),
            service_context,
            update_event_handle,
        }
    }

    /// Get the update event (readable event).
    /// Corresponds to upstream `ProgressServiceBackend::GetEvent()`.
    pub fn get_event(&self) -> Arc<Event> {
        self.service_context
            .get_event(self.update_event_handle)
            .expect("update_event must exist")
    }

    pub fn get_impl(&self) -> &DeliveryCacheProgressImpl {
        &self.impl_data
    }

    pub fn set_total_size(&mut self, size: u64) {
        self.impl_data.total_bytes = size as i64;
        self.signal_update();
    }

    pub fn start_connecting(&mut self) {
        self.impl_data.status = DeliveryCacheProgressStatus::Connecting;
        self.signal_update();
    }

    pub fn start_processing_data_list(&mut self) {
        self.impl_data.status = DeliveryCacheProgressStatus::ProcessingDataList;
        self.signal_update();
    }

    pub fn start_downloading_file(&mut self, dir_name: &str, file_name: &str, file_size: u64) {
        self.impl_data.status = DeliveryCacheProgressStatus::Downloading;
        self.impl_data.current_downloaded_bytes = 0;
        self.impl_data.current_total_bytes = file_size as i64;

        let dir_bytes = dir_name.as_bytes();
        let file_bytes = file_name.as_bytes();
        let dir_copy_len = std::cmp::min(dir_bytes.len(), 0x20);
        let file_copy_len = std::cmp::min(file_bytes.len(), 0x20);
        self.impl_data.current_directory[..dir_copy_len]
            .copy_from_slice(&dir_bytes[..dir_copy_len]);
        self.impl_data.current_file[..file_copy_len].copy_from_slice(&file_bytes[..file_copy_len]);

        self.signal_update();
    }

    pub fn update_file_progress(&mut self, downloaded: u64) {
        self.impl_data.current_downloaded_bytes = downloaded as i64;
        self.signal_update();
    }

    pub fn finish_downloading_file(&mut self) {
        self.impl_data.total_downloaded_bytes += self.impl_data.current_total_bytes;
        self.signal_update();
    }

    pub fn commit_directory(&mut self, dir_name: &str) {
        self.impl_data.status = DeliveryCacheProgressStatus::Committing;
        self.impl_data.current_file = [0u8; 0x20];
        self.impl_data.current_downloaded_bytes = 0;
        self.impl_data.current_total_bytes = 0;

        let dir_bytes = dir_name.as_bytes();
        let copy_len = std::cmp::min(dir_bytes.len(), 0x20);
        self.impl_data.current_directory = [0u8; 0x20];
        self.impl_data.current_directory[..copy_len].copy_from_slice(&dir_bytes[..copy_len]);

        self.signal_update();
    }

    pub fn finish_download(&mut self, result: ResultCode) {
        self.impl_data.total_downloaded_bytes = self.impl_data.total_bytes;
        self.impl_data.status = DeliveryCacheProgressStatus::Done;
        self.impl_data.result = result;
        self.signal_update();
    }

    fn signal_update(&self) {
        if let Some(event) = self.service_context.get_event(self.update_event_handle) {
            event.signal();
        }
    }
}

impl Drop for ProgressServiceBackend {
    fn drop(&mut self) {
        self.service_context.close_event(self.update_event_handle);
    }
}

/// Abstract backend trait for BCAT functionality.
///
/// Corresponds to `BcatBackend` in upstream `backend.h`.
pub trait BcatBackend {
    fn synchronize(&mut self, title: TitleIdVersion, progress: &mut ProgressServiceBackend)
        -> bool;
    fn synchronize_directory(
        &mut self,
        title: TitleIdVersion,
        name: String,
        progress: &mut ProgressServiceBackend,
    ) -> bool;
    fn clear(&mut self, title_id: u64) -> bool;
    fn set_passphrase(&mut self, title_id: u64, passphrase: &Passphrase);
    fn get_launch_parameter(&self, title: TitleIdVersion) -> Option<Vec<u8>>;
}

/// NullBcatBackend: no-op backend.
///
/// Corresponds to `NullBcatBackend` in upstream `backend.h`.
pub struct NullBcatBackend;

impl NullBcatBackend {
    pub fn new() -> Self {
        Self
    }
}

impl BcatBackend for NullBcatBackend {
    fn synchronize(
        &mut self,
        title: TitleIdVersion,
        progress: &mut ProgressServiceBackend,
    ) -> bool {
        log::debug!(
            "NullBcatBackend::synchronize called, title_id={:016X}, build_id={:016X}",
            title.title_id,
            title.build_id
        );
        progress.finish_download(RESULT_SUCCESS);
        true
    }

    fn synchronize_directory(
        &mut self,
        title: TitleIdVersion,
        name: String,
        progress: &mut ProgressServiceBackend,
    ) -> bool {
        log::debug!(
            "NullBcatBackend::synchronize_directory called, title_id={:016X}, build_id={:016X}, name={}",
            title.title_id,
            title.build_id,
            name
        );
        progress.finish_download(RESULT_SUCCESS);
        true
    }

    fn clear(&mut self, title_id: u64) -> bool {
        log::debug!("NullBcatBackend::clear called, title_id={:016X}", title_id);
        true
    }

    fn set_passphrase(&mut self, title_id: u64, _passphrase: &Passphrase) {
        log::debug!(
            "NullBcatBackend::set_passphrase called, title_id={:016X}",
            title_id
        );
    }

    fn get_launch_parameter(&self, title: TitleIdVersion) -> Option<Vec<u8>> {
        log::debug!(
            "NullBcatBackend::get_launch_parameter called, title_id={:016X}, build_id={:016X}",
            title.title_id,
            title.build_id
        );
        None
    }
}

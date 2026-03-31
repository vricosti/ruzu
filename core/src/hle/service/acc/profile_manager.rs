// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/profile_manager.h
//! Port of zuyu/src/core/hle/service/acc/profile_manager.cpp

use common::fs::path_util::{get_ruzu_path, RuzuPath};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

pub const MAX_USERS: usize = 8;
pub const PROFILE_USERNAME_SIZE: usize = 32;

pub type ProfileUsername = [u8; PROFILE_USERNAME_SIZE];
pub type UserIdArray = [u128; MAX_USERS];

/// Contains extra data related to a user.
///
/// Corresponds to `UserData` in upstream `profile_manager.h`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct UserData {
    pub _padding1: u32,
    pub icon_id: u32,
    pub bg_color_id: u8,
    pub _padding2: [u8; 0x7],
    pub _padding3: [u8; 0x10],
    pub _padding4: [u8; 0x60],
}

const _: () = assert!(core::mem::size_of::<UserData>() == 0x80);

impl Default for UserData {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

/// General information about a user's profile.
///
/// Corresponds to `ProfileInfo` in upstream `profile_manager.h`.
#[derive(Debug, Clone)]
pub struct ProfileInfo {
    pub user_uuid: u128,
    pub username: ProfileUsername,
    pub creation_time: u64,
    pub data: UserData,
    pub is_open: bool,
}

impl Default for ProfileInfo {
    fn default() -> Self {
        Self {
            user_uuid: 0,
            username: [0u8; PROFILE_USERNAME_SIZE],
            creation_time: 0,
            data: UserData::default(),
            is_open: false,
        }
    }
}

/// Profile base data sent over IPC.
///
/// Corresponds to `ProfileBase` in upstream `profile_manager.h`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ProfileBase {
    pub user_uuid: [u8; 16],
    pub timestamp: u64,
    pub username: ProfileUsername,
}

const _: () = assert!(core::mem::size_of::<ProfileBase>() == 0x38);

impl Default for ProfileBase {
    fn default() -> Self {
        unsafe { core::mem::zeroed() }
    }
}

impl ProfileBase {
    /// Zero out all fields to make the profile slot considered "Empty".
    pub fn invalidate(&mut self) {
        self.user_uuid = [0u8; 16];
        self.timestamp = 0;
        self.username.fill(0);
    }
}

/// The profile manager handles multiple user profiles.
///
/// Corresponds to `ProfileManager` in upstream `profile_manager.h`.
pub struct ProfileManager {
    is_save_needed: bool,
    profiles: [ProfileInfo; MAX_USERS],
    stored_opened_profiles: [ProfileInfo; MAX_USERS],
    user_count: usize,
    last_opened_user: u128,
}

impl ProfileManager {
    pub fn new() -> Self {
        let mut manager = Self {
            is_save_needed: false,
            profiles: Default::default(),
            stored_opened_profiles: Default::default(),
            user_count: 0,
            last_opened_user: 0,
        };
        manager.parse_user_save_file();
        manager
    }

    pub fn add_user(&mut self, user: &ProfileInfo) -> ResultCode {
        if self.user_count >= MAX_USERS {
            return super::errors::RESULT_INVALID_ARRAY_LENGTH;
        }
        if self.add_to_profiles(user).is_none() {
            return super::errors::RESULT_ACCOUNT_UPDATE_FAILED;
        }
        RESULT_SUCCESS
    }

    pub fn create_new_user(&mut self, uuid: u128, username: &ProfileUsername) -> ResultCode {
        if uuid == 0 {
            return super::errors::RESULT_INVALID_USER_ID;
        }
        if self.user_exists(uuid) {
            return super::errors::RESULT_ACCOUNT_UPDATE_FAILED;
        }
        let info = ProfileInfo {
            user_uuid: uuid,
            username: *username,
            creation_time: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            data: UserData::default(),
            is_open: false,
        };
        self.add_user(&info)
    }

    pub fn get_user(&self, index: usize) -> Option<u128> {
        if index >= self.user_count {
            return None;
        }
        Some(self.profiles[index].user_uuid)
    }

    pub fn get_user_index(&self, uuid: u128) -> Option<usize> {
        self.profiles[..self.user_count]
            .iter()
            .position(|p| p.user_uuid == uuid)
    }

    pub fn get_profile_base(&self, index: Option<usize>) -> Option<ProfileBase> {
        let idx = index?;
        if idx >= self.user_count {
            return None;
        }
        let profile = &self.profiles[idx];
        Some(ProfileBase {
            user_uuid: profile.user_uuid.to_le_bytes(),
            timestamp: profile.creation_time,
            username: profile.username,
        })
    }

    pub fn get_profile_base_by_uuid(&self, uuid: u128) -> Option<ProfileBase> {
        let index = self.get_user_index(uuid);
        self.get_profile_base(index)
    }

    pub fn get_profile_base_and_data(
        &self,
        index: Option<usize>,
    ) -> Option<(ProfileBase, UserData)> {
        let idx = index?;
        if idx >= self.user_count {
            return None;
        }
        let profile = &self.profiles[idx];
        Some((
            ProfileBase {
                user_uuid: profile.user_uuid.to_le_bytes(),
                timestamp: profile.creation_time,
                username: profile.username,
            },
            profile.data,
        ))
    }

    pub fn get_user_count(&self) -> usize {
        self.user_count
    }

    pub fn get_open_user_count(&self) -> usize {
        self.profiles[..self.user_count]
            .iter()
            .filter(|p| p.is_open)
            .count()
    }

    pub fn user_exists(&self, uuid: u128) -> bool {
        self.get_user_index(uuid).is_some()
    }

    pub fn user_exists_index(&self, index: usize) -> bool {
        index < self.user_count
    }

    pub fn open_user(&mut self, uuid: u128) {
        if let Some(idx) = self.get_user_index(uuid) {
            self.profiles[idx].is_open = true;
            self.last_opened_user = uuid;
        }
    }

    pub fn close_user(&mut self, uuid: u128) {
        if let Some(idx) = self.get_user_index(uuid) {
            self.profiles[idx].is_open = false;
        }
    }

    pub fn get_open_users(&self) -> UserIdArray {
        let mut users = [0u128; MAX_USERS];
        let mut idx = 0;
        for profile in &self.profiles[..self.user_count] {
            if profile.is_open {
                users[idx] = profile.user_uuid;
                idx += 1;
            }
        }
        users
    }

    pub fn get_all_users(&self) -> UserIdArray {
        let mut users = [0u128; MAX_USERS];
        for (i, profile) in self.profiles[..self.user_count].iter().enumerate() {
            users[i] = profile.user_uuid;
        }
        users
    }

    pub fn get_last_opened_user(&self) -> u128 {
        self.last_opened_user
    }

    pub fn get_stored_opened_users(&self) -> UserIdArray {
        let mut users = [0u128; MAX_USERS];
        for (i, profile) in self.stored_opened_profiles.iter().enumerate() {
            if profile.user_uuid != 0 {
                users[i] = profile.user_uuid;
            }
        }
        users
    }

    pub fn store_opened_users(&mut self) {
        self.stored_opened_profiles = self.profiles.clone();
    }

    pub fn can_system_register_user(&self) -> bool {
        self.user_count < MAX_USERS
    }

    pub fn remove_user(&mut self, uuid: u128) -> bool {
        if let Some(idx) = self.get_user_index(uuid) {
            self.remove_profile_at_index(idx)
        } else {
            false
        }
    }

    pub fn set_profile_base(&mut self, uuid: u128, profile_new: &ProfileBase) -> bool {
        if let Some(idx) = self.get_user_index(uuid) {
            self.profiles[idx].username = profile_new.username;
            self.profiles[idx].creation_time = profile_new.timestamp;
            self.is_save_needed = true;
            true
        } else {
            false
        }
    }

    pub fn set_profile_base_and_data(
        &mut self,
        uuid: u128,
        profile_new: &ProfileBase,
        data_new: &UserData,
    ) -> bool {
        if let Some(idx) = self.get_user_index(uuid) {
            self.profiles[idx].username = profile_new.username;
            self.profiles[idx].creation_time = profile_new.timestamp;
            self.profiles[idx].data = *data_new;
            self.is_save_needed = true;
            true
        } else {
            false
        }
    }

    /// Write user profiles to save file.
    /// Port of upstream `ProfileManager::WriteUserSaveFile`.
    pub fn write_user_save_file(&mut self) {
        if !self.is_save_needed {
            return;
        }

        let save_dir =
            get_ruzu_path(RuzuPath::NANDDir).join("system/save/8000000000000010/su/avators");
        if let Err(e) = std::fs::create_dir_all(&save_dir) {
            log::warn!("Failed to create profile save directory: {}", e);
            return;
        }

        let save_path = save_dir.join("profiles.dat");

        // Serialize profiles: for each user, write uuid(16) + uuid2(16) + timestamp(8) + username(32) + extra_data(128) = 200 bytes per user
        let mut data = Vec::with_capacity(MAX_USERS * 200);
        for i in 0..MAX_USERS {
            let profile = &self.profiles[i];
            data.extend_from_slice(&profile.user_uuid.to_le_bytes()); // uuid
            data.extend_from_slice(&profile.user_uuid.to_le_bytes()); // uuid2 (same as uuid)
            data.extend_from_slice(&profile.creation_time.to_le_bytes()); // timestamp
            data.extend_from_slice(&profile.username); // username
                                                       // Write extra_data as raw bytes (UserData is #[repr(C)], 0x80 bytes)
            let user_data_bytes: &[u8] = unsafe {
                std::slice::from_raw_parts(
                    &profile.data as *const UserData as *const u8,
                    std::mem::size_of::<UserData>(),
                )
            };
            data.extend_from_slice(user_data_bytes);
        }

        match std::fs::write(&save_path, &data) {
            Ok(()) => {
                self.is_save_needed = false;
                log::debug!("Wrote profile save data to {}", save_path.display());
            }
            Err(e) => {
                log::warn!("Failed to write profile save data: {}", e);
            }
        }
    }

    /// Parse user profiles from save file.
    /// Port of upstream `ProfileManager::ParseUserSaveFile`.
    fn parse_user_save_file(&mut self) {
        let save_path = get_ruzu_path(RuzuPath::NANDDir)
            .join("system/save/8000000000000010/su/avators/profiles.dat");

        let data = match std::fs::read(&save_path) {
            Ok(d) => d,
            Err(_) => {
                log::warn!(
                    "Failed to load profile data from save... \
                     Generating new user 'yuzu' with random UUID."
                );
                return;
            }
        };

        // Each user record is 200 bytes: uuid(16) + uuid2(16) + timestamp(8) + username(32) + extra_data(128)
        let record_size = 200;
        if data.len() < MAX_USERS * record_size {
            log::warn!(
                "profiles.dat is smaller than expected ({} < {})... \
                 Generating new user 'yuzu' with random UUID.",
                data.len(),
                MAX_USERS * record_size,
            );
            return;
        }

        for i in 0..MAX_USERS {
            let offset = i * record_size;
            let uuid = u128::from_le_bytes(data[offset..offset + 16].try_into().unwrap());
            if uuid == 0 {
                continue;
            }
            let timestamp = u64::from_le_bytes(data[offset + 32..offset + 40].try_into().unwrap());
            let mut username = [0u8; PROFILE_USERNAME_SIZE];
            username.copy_from_slice(&data[offset + 40..offset + 40 + PROFILE_USERNAME_SIZE]);

            self.add_user(&ProfileInfo {
                user_uuid: uuid,
                username,
                creation_time: timestamp,
                data: UserData::default(),
                is_open: false,
            });
        }

        // Sort valid profiles first (matching upstream stable_partition).
        let count = self.user_count;
        self.profiles[..count].sort_by(|a, b| {
            let a_valid = a.user_uuid != 0;
            let b_valid = b.user_uuid != 0;
            b_valid.cmp(&a_valid)
        });
    }

    fn add_to_profiles(&mut self, profile: &ProfileInfo) -> Option<usize> {
        if self.user_count >= MAX_USERS {
            return None;
        }
        let idx = self.user_count;
        self.profiles[idx] = profile.clone();
        self.user_count += 1;
        self.is_save_needed = true;
        Some(idx)
    }

    fn remove_profile_at_index(&mut self, index: usize) -> bool {
        if index >= self.user_count {
            return false;
        }
        // Shift profiles down
        for i in index..self.user_count - 1 {
            self.profiles[i] = self.profiles[i + 1].clone();
        }
        self.profiles[self.user_count - 1] = ProfileInfo::default();
        self.user_count -= 1;
        self.is_save_needed = true;
        true
    }
}

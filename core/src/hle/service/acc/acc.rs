// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc.h
//! Port of zuyu/src/core/hle/service/acc/acc.cpp
//!
//! Account module and Interface base class.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use super::profile_manager::{ProfileManager, ProfileBase, UserData, UserIdArray, MAX_USERS};

/// Generate a random UUID. Upstream uses `Common::UUID::MakeRandom()`.
fn generate_random_uuid() -> u128 {
    use std::collections::hash_map::RandomState;
    use std::hash::{BuildHasher, Hasher};
    let s = RandomState::new();
    let mut h = s.build_hasher();
    h.write_u64(std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64);
    let lo = h.finish() as u128;
    let s2 = RandomState::new();
    let mut h2 = s2.build_hasher();
    h2.write_u64(lo as u64 ^ 0xDEADBEEFCAFEBABE);
    let hi = h2.finish() as u128;
    (hi << 64) | lo
}

/// ApplicationType enum from upstream `acc.h`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ApplicationType {
    GameCard = 0,
    Digital = 1,
    Unknown = 3,
}

/// ApplicationInfo from upstream `acc.h` (Module::Interface private).
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ApplicationInfo {
    pub title_id: u64,
    pub application_type: ApplicationType,
}

impl Default for ApplicationInfo {
    fn default() -> Self {
        Self {
            title_id: 0,
            application_type: ApplicationType::Unknown,
        }
    }
}

/// Module corresponds to upstream `Module` in `acc.h`.
pub struct Module;

/// Module::Interface base methods shared across ACC_U0, ACC_U1, ACC_SU, ACC_AA.
///
/// Corresponds to `Module::Interface` in upstream `acc.h`.
pub struct Interface {
    pub system: crate::core::SystemRef,
    pub module: Arc<Module>,
    pub profile_manager: Arc<Mutex<ProfileManager>>,
    pub service_name: String,
    pub application_info: ApplicationInfo,
}

impl Interface {
    /// Matches upstream `Module::Interface(shared_ptr<Module>, shared_ptr<ProfileManager>, System&, const char*)`.
    pub fn new(
        module: Arc<Module>,
        profile_manager: Arc<Mutex<ProfileManager>>,
        system: crate::core::SystemRef,
        name: &str,
    ) -> Self {
        Self {
            system,
            module,
            profile_manager,
            service_name: name.to_string(),
            application_info: ApplicationInfo::default(),
        }
    }

    pub fn get_user_count(&self, profile_manager: &ProfileManager) -> (ResultCode, u32) {
        log::debug!("Account::GetUserCount called");
        (RESULT_SUCCESS, profile_manager.get_user_count() as u32)
    }

    pub fn get_user_existence(&self, profile_manager: &ProfileManager, uuid: u128) -> (ResultCode, bool) {
        log::debug!("Account::GetUserExistence called");
        (RESULT_SUCCESS, profile_manager.user_exists(uuid))
    }

    pub fn list_all_users(&self, profile_manager: &ProfileManager) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListAllUsers called");
        (RESULT_SUCCESS, profile_manager.get_all_users())
    }

    pub fn list_open_users(&self, profile_manager: &ProfileManager) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListOpenUsers called");
        (RESULT_SUCCESS, profile_manager.get_open_users())
    }

    pub fn get_last_opened_user(&self, profile_manager: &ProfileManager) -> (ResultCode, u128) {
        log::debug!("Account::GetLastOpenedUser called");
        (RESULT_SUCCESS, profile_manager.get_last_opened_user())
    }

    pub fn get_profile(&self, uuid: u128) -> (ResultCode, SessionRequestHandlerPtr) {
        log::debug!("Account::GetProfile called, user_id=0x{:032x}", uuid);
        let iprofile = new_iprofile(self.profile_manager.clone(), uuid);
        (RESULT_SUCCESS, iprofile)
    }

    pub fn is_user_registration_request_permitted(&self) -> (ResultCode, bool) {
        log::debug!("Account::IsUserRegistrationRequestPermitted called");
        (RESULT_SUCCESS, true)
    }

    pub fn try_select_user_without_interaction(&self, profile_manager: &ProfileManager) -> (ResultCode, u128) {
        log::debug!("Account::TrySelectUserWithoutInteraction called");
        if profile_manager.get_user_count() != 1 {
            return (RESULT_SUCCESS, 0);
        }
        (RESULT_SUCCESS, profile_manager.get_last_opened_user())
    }

    pub fn is_user_account_switch_locked(&self) -> (ResultCode, bool) {
        log::debug!("Account::IsUserAccountSwitchLocked called");
        (RESULT_SUCCESS, false)
    }

    pub fn list_open_context_stored_users(&self, profile_manager: &ProfileManager) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListOpenContextStoredUsers called");
        (RESULT_SUCCESS, profile_manager.get_stored_opened_users())
    }

    pub fn list_qualified_users(&self, profile_manager: &ProfileManager) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListQualifiedUsers called");
        (RESULT_SUCCESS, profile_manager.get_all_users())
    }

    pub fn begin_user_registration(&self, profile_manager: &mut ProfileManager) -> (ResultCode, u128) {
        // Upstream: generates a random UUID and creates a new user with name "yuzu"
        let uuid = generate_random_uuid();
        let mut username = [0u8; super::profile_manager::PROFILE_USERNAME_SIZE];
        let name = b"yuzu";
        username[..name.len()].copy_from_slice(name);
        profile_manager.create_new_user(uuid, &username);
        log::info!("Account::BeginUserRegistration called, uuid=0x{:032x}", uuid);
        (RESULT_SUCCESS, uuid)
    }

    pub fn complete_user_registration(&self, _profile_manager: &mut ProfileManager, _uuid: u128) -> ResultCode {
        log::debug!("Account::CompleteUserRegistration called");
        RESULT_SUCCESS
    }

    pub fn get_profile_editor(&self, uuid: u128) -> (ResultCode, SessionRequestHandlerPtr) {
        log::debug!("Account::GetProfileEditor called, user_id=0x{:032x}", uuid);
        let editor = new_iprofile_editor(self.profile_manager.clone(), uuid);
        (RESULT_SUCCESS, editor)
    }

    pub fn initialize_application_info(&mut self) -> ResultCode {
        log::debug!("Account::InitializeApplicationInfo called");
        // Upstream: calls InitializeApplicationInfoBase() which checks ARP manager
        // for launch property and sets application_type based on storage_id.
        // For now, default to Digital since we don't have full ARP integration.
        self.application_info.application_type = ApplicationType::Digital;
        RESULT_SUCCESS
    }

    pub fn initialize_application_info_restricted(&mut self) -> ResultCode {
        log::debug!("Account::InitializeApplicationInfoRestricted called");
        RESULT_SUCCESS
    }

    pub fn initialize_application_info_v2(&mut self) -> ResultCode {
        log::debug!("Account::InitializeApplicationInfoV2 called");
        RESULT_SUCCESS
    }
}

// ---------------------------------------------------------------------------
// IProfileCommon / IProfile / IProfileEditor
// ---------------------------------------------------------------------------
// Upstream: acc.cpp defines IProfileCommon as a base, IProfile (editor=false),
// IProfileEditor (editor=true).

/// IProfileCommon implements Get, GetBase, GetImageSize, LoadImage.
/// When `editor=true` it also registers Store and StoreWithImage.
///
/// Corresponds to `IProfileCommon` in upstream `acc.cpp`.
pub struct IProfileCommon {
    profile_manager: Arc<Mutex<ProfileManager>>,
    user_id: u128,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    service_name: &'static str,
}

impl IProfileCommon {
    fn new(
        profile_manager: Arc<Mutex<ProfileManager>>,
        user_id: u128,
        name: &'static str,
        editor_commands: bool,
    ) -> Self {
        let mut entries: Vec<(u32, Option<fn(&dyn ServiceFramework, &mut HLERequestContext)>, &str)> = vec![
            (0, Some(IProfileCommon::get_handler), "Get"),
            (1, Some(IProfileCommon::get_base_handler), "GetBase"),
            (10, Some(IProfileCommon::get_image_size_handler), "GetImageSize"),
            (11, Some(IProfileCommon::load_image_handler), "LoadImage"),
        ];
        if editor_commands {
            entries.push((100, Some(IProfileCommon::store_handler), "Store"));
            entries.push((101, Some(IProfileCommon::store_with_image_handler), "StoreWithImage"));
        }
        let refs: Vec<(u32, Option<fn(&dyn ServiceFramework, &mut HLERequestContext)>, &str)> = entries;
        let handlers = build_handler_map(&refs);
        Self {
            profile_manager,
            user_id,
            handlers,
            handlers_tipc: BTreeMap::new(),
            service_name: name,
        }
    }

    fn as_self(this: &dyn ServiceFramework) -> &IProfileCommon {
        unsafe { &*(this as *const dyn ServiceFramework as *const IProfileCommon) }
    }

    /// Upstream: IProfileCommon::Get (cmd 0)
    fn get_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let pm = svc.profile_manager.lock().unwrap();
        log::debug!("IProfileCommon::Get called, user_id=0x{:032x}", svc.user_id);

        let index = pm.get_user_index(svc.user_id);
        if let Some((base, data)) = pm.get_profile_base_and_data(index) {
            // Write UserData to output buffer
            let data_bytes = unsafe {
                std::slice::from_raw_parts(
                    &data as *const UserData as *const u8,
                    std::mem::size_of::<UserData>(),
                )
            };
            ctx.write_buffer(data_bytes, 0);
            // rb size 16 = 2 header + 14 for ProfileBase (0x38 bytes = 14 u32s)
            let mut rb = ResponseBuilder::new(ctx, 16, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_raw(&base);
        } else {
            log::error!(
                "IProfileCommon::Get: Failed to get profile base and data for user=0x{:032x}",
                svc.user_id
            );
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(ResultCode::new(1)); // ResultUnknown
        }
    }

    /// Upstream: IProfileCommon::GetBase (cmd 1)
    fn get_base_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let pm = svc.profile_manager.lock().unwrap();
        log::debug!("IProfileCommon::GetBase called, user_id=0x{:032x}", svc.user_id);

        if let Some(base) = pm.get_profile_base_by_uuid(svc.user_id) {
            let mut rb = ResponseBuilder::new(ctx, 16, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_raw(&base);
        } else {
            log::error!(
                "IProfileCommon::GetBase: Failed to get profile base for user=0x{:032x}",
                svc.user_id
            );
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(ResultCode::new(1)); // ResultUnknown
        }
    }

    /// Upstream: IProfileCommon::GetImageSize (cmd 10)
    fn get_image_size_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("(STUBBED) IProfileCommon::GetImageSize called");
        // Return backup JPEG size. Upstream loads from file or falls back to backup.
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // image size = 0 (no image available)
    }

    /// Upstream: IProfileCommon::LoadImage (cmd 11)
    fn load_image_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        log::debug!("(STUBBED) IProfileCommon::LoadImage called");
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0); // bytes written = 0
    }

    /// Upstream: IProfileCommon::Store (cmd 100, editor only)
    fn store_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut pm = svc.profile_manager.lock().unwrap();
        let mut rp = RequestParser::new(ctx);
        let base = rp.pop_raw::<ProfileBase>();

        let user_data_buf = ctx.read_buffer(0);
        log::debug!("IProfileCommon::Store called");

        if user_data_buf.len() < std::mem::size_of::<UserData>() {
            log::error!("IProfileCommon::Store: UserData buffer too small!");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(super::errors::RESULT_INVALID_ARRAY_LENGTH);
            return;
        }

        let mut data = UserData::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                user_data_buf.as_ptr(),
                &mut data as *mut UserData as *mut u8,
                std::mem::size_of::<UserData>(),
            );
        }

        if !pm.set_profile_base_and_data(svc.user_id, &base, &data) {
            log::error!("IProfileCommon::Store: Failed to update user data and base!");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(super::errors::RESULT_ACCOUNT_UPDATE_FAILED);
            return;
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Upstream: IProfileCommon::StoreWithImage (cmd 101, editor only)
    fn store_with_image_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut pm = svc.profile_manager.lock().unwrap();
        let mut rp = RequestParser::new(ctx);
        let base = rp.pop_raw::<ProfileBase>();

        let user_data_buf = ctx.read_buffer(0);
        log::debug!("IProfileCommon::StoreWithImage called");

        if user_data_buf.len() < std::mem::size_of::<UserData>() {
            log::error!("IProfileCommon::StoreWithImage: UserData buffer too small!");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(super::errors::RESULT_INVALID_ARRAY_LENGTH);
            return;
        }

        let mut data = UserData::default();
        unsafe {
            std::ptr::copy_nonoverlapping(
                user_data_buf.as_ptr(),
                &mut data as *mut UserData as *mut u8,
                std::mem::size_of::<UserData>(),
            );
        }

        if !pm.set_profile_base_and_data(svc.user_id, &base, &data) {
            log::error!("IProfileCommon::StoreWithImage: Failed to update profile data!");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(super::errors::RESULT_ACCOUNT_UPDATE_FAILED);
            return;
        }

        // Upstream also writes image data to file; we skip that for now
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IProfileCommon {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        self.service_name
    }
}

impl ServiceFramework for IProfileCommon {
    fn get_service_name(&self) -> &str {
        self.service_name
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

/// IProfile (read-only profile interface).
/// Corresponds to `IProfile` in upstream `acc.cpp`.
pub fn new_iprofile(
    profile_manager: Arc<Mutex<ProfileManager>>,
    user_id: u128,
) -> SessionRequestHandlerPtr {
    Arc::new(IProfileCommon::new(profile_manager, user_id, "IProfile", false))
}

/// IProfileEditor (read-write profile interface with Store/StoreWithImage).
/// Corresponds to `IProfileEditor` in upstream `acc.cpp`.
pub fn new_iprofile_editor(
    profile_manager: Arc<Mutex<ProfileManager>>,
    user_id: u128,
) -> SessionRequestHandlerPtr {
    Arc::new(IProfileCommon::new(profile_manager, user_id, "IProfileEditor", true))
}

/// Registers account services with the server manager.
///
/// Corresponds to `LoopProcess` in upstream `acc.cpp`.
/// Services registered: acc:u0, acc:u1, acc:su, acc:aa
pub fn loop_process() {
    use std::sync::Arc;
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    log::debug!("Account::LoopProcess - registering acc:u0, acc:u1, acc:su, acc:aa");

    let module = Arc::new(Module);
    let profile_manager = Arc::new(std::sync::Mutex::new(
        super::profile_manager::ProfileManager::new(),
    ));

    let mut server_manager = ServerManager::new(crate::core::SystemRef::null());

    // acc:aa -> ACC_AA
    {
        let m = module.clone();
        let pm = profile_manager.clone();
        server_manager.register_named_service(
            "acc:aa",
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(super::acc_aa::AccAA::new(m.clone(), pm.clone(), crate::core::SystemRef::null()))
            }),
            64,
        );
    }

    // acc:su -> ACC_SU
    {
        let m = module.clone();
        let pm = profile_manager.clone();
        server_manager.register_named_service(
            "acc:su",
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(super::acc_su::AccSU::new(m.clone(), pm.clone(), crate::core::SystemRef::null()))
            }),
            64,
        );
    }

    // acc:u0 -> ACC_U0
    {
        let m = module.clone();
        let pm = profile_manager.clone();
        server_manager.register_named_service(
            "acc:u0",
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(super::acc_u0::AccU0::new(m.clone(), pm.clone(), crate::core::SystemRef::null()))
            }),
            64,
        );
    }

    // acc:u1 -> ACC_U1
    {
        let m = module.clone();
        let pm = profile_manager.clone();
        server_manager.register_named_service(
            "acc:u1",
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(super::acc_u1::AccU1::new(m.clone(), pm.clone(), crate::core::SystemRef::null()))
            }),
            64,
        );
    }

    ServerManager::run_server(server_manager);
}

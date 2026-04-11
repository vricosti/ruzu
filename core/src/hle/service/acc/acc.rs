// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc.h
//! Port of zuyu/src/core/hle/service/acc/acc.cpp
//!
//! Account module and Interface base class.

use super::profile_manager::{ProfileBase, ProfileManager, UserData, UserIdArray, MAX_USERS};
use crate::file_sys::romfs_factory::StorageId;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::acc::errors::{
    RESULT_APPLICATION_INFO_ALREADY_INITIALIZED, RESULT_INVALID_APPLICATION,
};
use crate::hle::service::glue::glue_manager::ApplicationLaunchProperty;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

/// Generate a random UUID. Upstream uses `Common::UUID::MakeRandom()`.
fn generate_random_uuid() -> u128 {
    use std::collections::hash_map::RandomState;
    use std::hash::{BuildHasher, Hasher};
    let s = RandomState::new();
    let mut h = s.build_hasher();
    h.write_u64(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as u64,
    );
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
    pub launch_property: ApplicationLaunchProperty,
    pub application_type: ApplicationType,
}

impl Default for ApplicationInfo {
    fn default() -> Self {
        Self {
            launch_property: ApplicationLaunchProperty::default(),
            application_type: ApplicationType::Unknown,
        }
    }
}

impl ApplicationInfo {
    fn is_initialized(&self) -> bool {
        self.launch_property.title_id != 0
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

    pub fn get_user_existence(
        &self,
        profile_manager: &ProfileManager,
        uuid: u128,
    ) -> (ResultCode, bool) {
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

    pub fn try_select_user_without_interaction(
        &self,
        profile_manager: &ProfileManager,
    ) -> (ResultCode, u128) {
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

    pub fn list_open_context_stored_users(
        &self,
        profile_manager: &ProfileManager,
    ) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListOpenContextStoredUsers called");
        (RESULT_SUCCESS, profile_manager.get_stored_opened_users())
    }

    pub fn list_qualified_users(
        &self,
        profile_manager: &ProfileManager,
    ) -> (ResultCode, UserIdArray) {
        log::debug!("Account::ListQualifiedUsers called");
        (RESULT_SUCCESS, profile_manager.get_all_users())
    }

    pub fn begin_user_registration(
        &self,
        profile_manager: &mut ProfileManager,
    ) -> (ResultCode, u128) {
        // Upstream: generates a random UUID and creates a new user with name "yuzu"
        let uuid = generate_random_uuid();
        let mut username = [0u8; super::profile_manager::PROFILE_USERNAME_SIZE];
        let name = b"yuzu";
        username[..name.len()].copy_from_slice(name);
        profile_manager.create_new_user(uuid, &username);
        log::info!(
            "Account::BeginUserRegistration called, uuid=0x{:032x}",
            uuid
        );
        (RESULT_SUCCESS, uuid)
    }

    pub fn complete_user_registration(
        &self,
        _profile_manager: &mut ProfileManager,
        _uuid: u128,
    ) -> ResultCode {
        log::debug!("Account::CompleteUserRegistration called");
        RESULT_SUCCESS
    }

    pub fn get_profile_editor(&self, uuid: u128) -> (ResultCode, SessionRequestHandlerPtr) {
        log::debug!("Account::GetProfileEditor called, user_id=0x{:032x}", uuid);
        let editor = new_iprofile_editor(self.profile_manager.clone(), uuid);
        (RESULT_SUCCESS, editor)
    }

    fn initialize_application_info_base(&mut self) -> ResultCode {
        if self.application_info.is_initialized() {
            log::error!("Account::InitializeApplicationInfoBase: already initialized");
            return RESULT_APPLICATION_INFO_ALREADY_INITIALIZED;
        }

        let system = self.system.get();
        let title_id = system
            .current_process_arc
            .as_ref()
            .map(|process| process.lock().unwrap().get_program_id())
            .unwrap_or_else(|| system.runtime_program_id());
        let (result, launch_property) = system
            .arp_manager()
            .lock()
            .unwrap()
            .get_launch_property(title_id);

        if result != RESULT_SUCCESS {
            log::error!(
                "Account::InitializeApplicationInfoBase: failed to get launch property for {:016X}",
                title_id
            );
            return RESULT_INVALID_APPLICATION;
        }

        let Some(launch_property) = launch_property else {
            log::error!("Account::InitializeApplicationInfoBase: ARP returned no launch property");
            return RESULT_INVALID_APPLICATION;
        };

        let application_type = match launch_property.base_game_storage_id {
            x if x == StorageId::GameCard as u8 => ApplicationType::GameCard,
            x if x == StorageId::Host as u8
                || x == StorageId::NandUser as u8
                || x == StorageId::SdCard as u8
                || x == StorageId::None as u8 =>
            {
                ApplicationType::Digital
            }
            storage_id => {
                log::error!(
                    "Account::InitializeApplicationInfoBase: invalid game storage id {}",
                    storage_id
                );
                return RESULT_INVALID_APPLICATION;
            }
        };

        self.application_info.launch_property = launch_property;
        self.application_info.application_type = application_type;

        log::warn!("Account::InitializeApplicationInfoBase: ApplicationInfo init required");
        RESULT_SUCCESS
    }

    pub fn initialize_application_info(&mut self) -> ResultCode {
        log::debug!("Account::InitializeApplicationInfo called");
        self.initialize_application_info_base()
    }

    pub fn initialize_application_info_restricted(&mut self) -> ResultCode {
        log::debug!("Account::InitializeApplicationInfoRestricted called");
        self.initialize_application_info_base()
    }

    pub fn initialize_application_info_v2(&mut self) -> ResultCode {
        log::debug!("Account::InitializeApplicationInfoV2 called");
        RESULT_SUCCESS
    }

    pub fn get_baas_account_manager_for_application(
        &self,
    ) -> (ResultCode, SessionRequestHandlerPtr) {
        log::debug!("Account::GetBaasAccountManagerForApplication called");
        (
            RESULT_SUCCESS,
            Arc::new(IManagerForApplication::new(
                self.system,
                self.profile_manager.clone(),
            )),
        )
    }
}

fn push_interface_response(ctx: &mut HLERequestContext, object: SessionRequestHandlerPtr) {
    let is_domain = ctx
        .get_manager()
        .map_or(false, |manager| manager.lock().unwrap().is_domain());
    let move_handle = if is_domain {
        0
    } else {
        ctx.create_session_for_service(object.clone()).unwrap_or(0)
    };
    let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
    rb.push_result(RESULT_SUCCESS);
    if is_domain {
        ctx.add_domain_object(object);
    } else {
        rb.push_move_objects(move_handle);
    }
}

struct EnsureTokenIdCacheAsyncInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl EnsureTokenIdCacheAsyncInterface {
    fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::unknown0_handler), "Unknown0"),
            (
                1,
                Some(Self::load_id_token_cache_handler),
                "LoadIdTokenCache",
            ),
            (2, Some(Self::unknown2_handler), "Unknown2"),
            (3, Some(Self::unknown3_handler), "Unknown3"),
            (100, Some(Self::cancel_handler), "Cancel"),
            (101, Some(Self::get_result_handler), "GetResult"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn unknown0_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn load_id_token_cache_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe {
            &*(this as *const dyn ServiceFramework as *const EnsureTokenIdCacheAsyncInterface)
        };
        svc.load_id_token_cache(ctx);
    }

    fn unknown2_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn unknown3_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn cancel_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn get_result_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn load_id_token_cache(&self, ctx: &mut HLERequestContext) {
        ctx.write_buffer(&[], 0);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(0);
    }
}

impl SessionRequestHandler for EnsureTokenIdCacheAsyncInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for EnsureTokenIdCacheAsyncInterface {
    fn get_service_name(&self) -> &str {
        "EnsureTokenIdCacheAsyncInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

struct IManagerForApplication {
    profile_manager: Arc<Mutex<ProfileManager>>,
    ensure_token_id: Arc<EnsureTokenIdCacheAsyncInterface>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IManagerForApplication {
    fn new(_system: crate::core::SystemRef, profile_manager: Arc<Mutex<ProfileManager>>) -> Self {
        let handlers = build_handler_map(&[
            (
                0,
                Some(Self::check_availability_handler),
                "CheckAvailability",
            ),
            (1, Some(Self::get_account_id_handler), "GetAccountId"),
            (
                2,
                Some(Self::ensure_id_token_cache_async_handler),
                "EnsureIdTokenCacheAsync",
            ),
            (
                3,
                Some(Self::load_id_token_cache_handler),
                "LoadIdTokenCache",
            ),
            (
                130,
                Some(Self::get_nintendo_account_user_resource_cache_for_application_handler),
                "GetNintendoAccountUserResourceCacheForApplication",
            ),
            (150, None, "CreateAuthorizationRequest"),
            (
                160,
                Some(Self::store_open_context_handler),
                "StoreOpenContext",
            ),
            (170, None, "LoadNetworkServiceLicenseKindAsync"),
        ]);
        Self {
            profile_manager,
            ensure_token_id: Arc::new(EnsureTokenIdCacheAsyncInterface::new()),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn check_availability_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(false);
    }

    fn get_account_id_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc =
            unsafe { &*(this as *const dyn ServiceFramework as *const IManagerForApplication) };
        let pm = svc.profile_manager.lock().unwrap();
        let hash =
            common::uuid::UUID::from_bytes(pm.get_last_opened_user().to_le_bytes()).hash_value();
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(hash);
    }

    fn ensure_id_token_cache_async_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc =
            unsafe { &*(this as *const dyn ServiceFramework as *const IManagerForApplication) };
        push_interface_response(ctx, svc.ensure_token_id.clone());
    }

    fn load_id_token_cache_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc =
            unsafe { &*(this as *const dyn ServiceFramework as *const IManagerForApplication) };
        svc.ensure_token_id.load_id_token_cache(ctx);
    }

    fn get_nintendo_account_user_resource_cache_for_application_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let svc =
            unsafe { &*(this as *const dyn ServiceFramework as *const IManagerForApplication) };
        ctx.write_buffer(&vec![0u8; 0x68], 0);
        if ctx.can_write_buffer(1) {
            ctx.write_buffer(&vec![0u8; ctx.get_write_buffer_size(1)], 1);
        }

        let pm = svc.profile_manager.lock().unwrap();
        let hash =
            common::uuid::UUID::from_bytes(pm.get_last_opened_user().to_le_bytes()).hash_value();
        let mut rb = ResponseBuilder::new(ctx, 4, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u64(hash);
    }

    fn store_open_context_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc =
            unsafe { &*(this as *const dyn ServiceFramework as *const IManagerForApplication) };
        svc.profile_manager.lock().unwrap().store_opened_users();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for IManagerForApplication {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for IManagerForApplication {
    fn get_service_name(&self) -> &str {
        "IManagerForApplication"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
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
        let mut entries: Vec<(
            u32,
            Option<fn(&dyn ServiceFramework, &mut HLERequestContext)>,
            &str,
        )> = vec![
            (0, Some(IProfileCommon::get_handler), "Get"),
            (1, Some(IProfileCommon::get_base_handler), "GetBase"),
            (
                10,
                Some(IProfileCommon::get_image_size_handler),
                "GetImageSize",
            ),
            (11, Some(IProfileCommon::load_image_handler), "LoadImage"),
        ];
        if editor_commands {
            entries.push((100, Some(IProfileCommon::store_handler), "Store"));
            entries.push((
                101,
                Some(IProfileCommon::store_with_image_handler),
                "StoreWithImage",
            ));
        }
        let refs: Vec<(
            u32,
            Option<fn(&dyn ServiceFramework, &mut HLERequestContext)>,
            &str,
        )> = entries;
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
        log::debug!(
            "IProfileCommon::GetBase called, user_id=0x{:032x}",
            svc.user_id
        );

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
    Arc::new(IProfileCommon::new(
        profile_manager,
        user_id,
        "IProfile",
        false,
    ))
}

/// IProfileEditor (read-write profile interface with Store/StoreWithImage).
/// Corresponds to `IProfileEditor` in upstream `acc.cpp`.
pub fn new_iprofile_editor(
    profile_manager: Arc<Mutex<ProfileManager>>,
    user_id: u128,
) -> SessionRequestHandlerPtr {
    Arc::new(IProfileCommon::new(
        profile_manager,
        user_id,
        "IProfileEditor",
        true,
    ))
}

/// Registers account services with the server manager.
///
/// Corresponds to `LoopProcess` in upstream `acc.cpp`.
/// Services registered: acc:u0, acc:u1, acc:su, acc:aa
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
    use crate::hle::service::server_manager::ServerManager;
    use std::sync::Arc;

    log::debug!("Account::LoopProcess - registering acc:u0, acc:u1, acc:su, acc:aa");

    let module = Arc::new(Module);
    let profile_manager = Arc::new(std::sync::Mutex::new(
        super::profile_manager::ProfileManager::new(),
    ));

    let mut server_manager = ServerManager::new(system);

    // acc:aa -> ACC_AA
    {
        let m = module.clone();
        let pm = profile_manager.clone();
        server_manager.register_named_service(
            "acc:aa",
            Box::new(move || -> SessionRequestHandlerPtr {
                Arc::new(super::acc_aa::AccAA::new(m.clone(), pm.clone(), system))
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
                Arc::new(super::acc_su::AccSU::new(m.clone(), pm.clone(), system))
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
                Arc::new(super::acc_u0::AccU0::new(m.clone(), pm.clone(), system))
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
                Arc::new(super::acc_u1::AccU1::new(m.clone(), pm.clone(), system))
            }),
            64,
        );
    }

    ServerManager::run_server(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::service::hle_ipc::HLERequestContext;

    fn make_interface(system: crate::core::SystemRef) -> Interface {
        Interface::new(
            Arc::new(Module),
            Arc::new(Mutex::new(ProfileManager::new())),
            system,
            "acc:u0",
        )
    }

    #[test]
    fn initialize_application_info_base_uses_arp_launch_property() {
        let mut system = crate::core::System::new();
        let program_id = 0x0100_1520_0002_2000;
        system.set_runtime_program_id(program_id);
        system.arp_manager().lock().unwrap().register(
            program_id,
            ApplicationLaunchProperty {
                title_id: program_id,
                version: 0,
                base_game_storage_id: StorageId::Host as u8,
                update_storage_id: StorageId::None as u8,
                program_index: 0,
                reserved: 0,
            },
            vec![0; 0x4000],
        );

        let mut interface = make_interface(crate::core::SystemRef::from_ref(&system));
        let result = interface.initialize_application_info();

        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(
            interface.application_info.launch_property.title_id,
            program_id
        );
        assert_eq!(
            interface.application_info.application_type,
            ApplicationType::Digital
        );
    }

    #[test]
    fn initialize_application_info_base_rejects_second_initialization() {
        let mut system = crate::core::System::new();
        let program_id = 0x0100_1520_0002_2000;
        system.set_runtime_program_id(program_id);
        system.arp_manager().lock().unwrap().register(
            program_id,
            ApplicationLaunchProperty {
                title_id: program_id,
                version: 0,
                base_game_storage_id: StorageId::Host as u8,
                update_storage_id: StorageId::None as u8,
                program_index: 0,
                reserved: 0,
            },
            vec![0; 0x4000],
        );

        let mut interface = make_interface(crate::core::SystemRef::from_ref(&system));
        assert_eq!(interface.initialize_application_info(), RESULT_SUCCESS);
        assert_eq!(
            interface.initialize_application_info(),
            RESULT_APPLICATION_INFO_ALREADY_INITIALIZED
        );
    }

    #[test]
    fn ensure_token_id_load_id_token_cache_uses_upstream_response_shape() {
        let mut ctx = HLERequestContext::new();

        EnsureTokenIdCacheAsyncInterface::new().load_id_token_cache(&mut ctx);

        assert_eq!(ctx.write_size, 3);
    }
}

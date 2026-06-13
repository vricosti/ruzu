// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/acc/acc.h
//! Port of zuyu/src/core/hle/service/acc/acc.cpp
//!
//! Account module and Interface base class.

use super::profile_manager::{ProfileBase, ProfileManager, UserData, UserIdArray};
use crate::constants::ACCOUNT_BACKUP_JPEG;
use crate::file_sys::romfs_factory::StorageId;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::acc::errors::{
    RESULT_APPLICATION_INFO_ALREADY_INITIALIZED, RESULT_INVALID_APPLICATION,
    RESULT_INVALID_ARRAY_LENGTH, RESULT_INVALID_USER_ID,
};
use crate::hle::service::glue::glue_manager::ApplicationLaunchProperty;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::os::event::Event;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};
use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

const MAX_JPEG_IMAGE_SIZE: usize = 0x20000;
const PROFILE_IMAGE_DIMENSIONS: u32 = 256;
// Upstream hard-codes save-data thumbnails to this exact size.
const THUMBNAIL_SIZE: usize = 0x24000;

/// Port of upstream `GetImagePath`.
fn get_image_path(user_id: u128) -> std::path::PathBuf {
    let uuid = common::uuid::UUID::from_bytes(user_id.to_le_bytes());
    common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::NANDDir).join(format!(
        "system/save/8000000000000010/su/avators/{}.jpg",
        uuid.formatted_string()
    ))
}

/// Port of upstream `JPEGImageSize` policy.
fn jpeg_image_size(size: usize) -> usize {
    size.min(MAX_JPEG_IMAGE_SIZE)
}

/// Port of upstream `SanitizeJPEGImageSize`.
fn sanitize_jpeg_image_size(image: &mut Vec<u8>) {
    match image::load_from_memory(image) {
        Ok(decoded) => {
            let rgb = decoded.to_rgb8();
            if rgb.width() != PROFILE_IMAGE_DIMENSIONS || rgb.height() != PROFILE_IMAGE_DIMENSIONS {
                let resized = image::imageops::resize(
                    &rgb,
                    PROFILE_IMAGE_DIMENSIONS,
                    PROFILE_IMAGE_DIMENSIONS,
                    image::imageops::FilterType::Triangle,
                );
                let mut encoded = Vec::new();
                let mut encoder =
                    image::codecs::jpeg::JpegEncoder::new_with_quality(&mut encoded, 90);
                if let Err(err) = encoder.encode_image(&resized) {
                    log::error!("Failed to resize the user provided image: {err}");
                } else {
                    *image = encoded;
                }
            }
        }
        Err(err) => {
            log::error!("Failed to decode the user provided image: {err}");
        }
    }
    image.truncate(jpeg_image_size(image.len()));
}

fn load_profile_image_or_backup(user_id: u128) -> Vec<u8> {
    let path = get_image_path(user_id);
    match std::fs::read(&path) {
        Ok(mut image) => {
            sanitize_jpeg_image_size(&mut image);
            image
        }
        Err(_) => {
            log::warn!(
                "IProfileCommon: failed to load profile image at {}; falling back to backup JPEG",
                path.display()
            );
            ACCOUNT_BACKUP_JPEG.to_vec()
        }
    }
}

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

    fn store_save_data_thumbnail_result(
        &self,
        uuid: u128,
        title_id: u64,
        thumbnail_size: usize,
    ) -> ResultCode {
        if title_id == 0 {
            log::error!("Account::StoreSaveDataThumbnail: TitleID is not valid");
            return RESULT_INVALID_APPLICATION;
        }

        if uuid == 0 {
            log::error!("Account::StoreSaveDataThumbnail: User ID is not valid");
            return RESULT_INVALID_USER_ID;
        }

        if thumbnail_size != THUMBNAIL_SIZE {
            log::error!(
                "Account::StoreSaveDataThumbnail: invalid buffer size {thumbnail_size:#X}, expecting {THUMBNAIL_SIZE:#X}"
            );
            return RESULT_INVALID_ARRAY_LENGTH;
        }

        // Upstream currently stubs thumbnail persistence after validation.
        RESULT_SUCCESS
    }

    /// Port of upstream `Module::Interface::StoreSaveDataThumbnailApplication`.
    pub fn store_save_data_thumbnail_application(&self, ctx: &mut HLERequestContext, uuid: u128) {
        log::warn!(
            "Account::StoreSaveDataThumbnailApplication: STUBBED called, uuid=0x{:032x}",
            uuid
        );

        // Upstream uses a temporary non-zero TID because it cannot reliably confirm
        // the application id in this path.
        let title_id = 1;
        let result =
            self.store_save_data_thumbnail_result(uuid, title_id, ctx.get_read_buffer_size(0));

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }
}

fn push_interface_response(ctx: &mut HLERequestContext, object: SessionRequestHandlerPtr) {
    let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
    rb.push_result(RESULT_SUCCESS);
    rb.push_ipc_interface(object);
}

struct EnsureTokenIdCacheAsyncInterface {
    completion_event: Arc<Event>,
    is_complete: AtomicBool,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl EnsureTokenIdCacheAsyncInterface {
    fn new() -> Self {
        let handlers = build_handler_map(&[
            (0, Some(Self::get_system_event_handler), "GetSystemEvent"),
            (1, Some(Self::cancel_handler), "Cancel"),
            (2, Some(Self::has_done_handler), "HasDone"),
            (3, Some(Self::get_result_handler), "GetResult"),
        ]);
        let completion_event = Arc::new(Event::new());
        completion_event.signal();
        Self {
            completion_event,
            is_complete: AtomicBool::new(true),
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    fn get_system_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe {
            &*(this as *const dyn ServiceFramework as *const EnsureTokenIdCacheAsyncInterface)
        };
        let object_id = svc.completion_event.copy_object_id(ctx).unwrap_or(0);
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_object_id(object_id);
    }

    fn cancel_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe {
            &*(this as *const dyn ServiceFramework as *const EnsureTokenIdCacheAsyncInterface)
        };
        svc.is_complete.store(true, Ordering::Release);
        svc.completion_event.signal();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn has_done_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe {
            &*(this as *const dyn ServiceFramework as *const EnsureTokenIdCacheAsyncInterface)
        };
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(svc.is_complete.load(Ordering::Acquire));
    }

    fn get_result_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = unsafe {
            &*(this as *const dyn ServiceFramework as *const EnsureTokenIdCacheAsyncInterface)
        };
        svc.is_complete.store(true, Ordering::Release);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn load_id_token_cache(&self, ctx: &mut HLERequestContext) {
        self.is_complete.store(true, Ordering::Release);
        self.completion_event.signal();
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

    fn trace_profile(
        stage: u64,
        result: ResultCode,
        base: Option<&ProfileBase>,
        data: Option<&UserData>,
    ) {
        if !common::trace::is_enabled(common::trace::cat::ACC_PROFILE) {
            return;
        }
        let (user_lo, user_hi, timestamp, name0, name1) = if let Some(base) = base {
            let user_lo = u64::from_le_bytes(base.user_uuid[0..8].try_into().unwrap());
            let user_hi = u64::from_le_bytes(base.user_uuid[8..16].try_into().unwrap());
            let mut name0 = [0u8; 8];
            let mut name1 = [0u8; 8];
            name0.copy_from_slice(&base.username[0..8]);
            name1.copy_from_slice(&base.username[8..16]);
            (
                user_lo,
                user_hi,
                base.timestamp,
                u64::from_le_bytes(name0),
                u64::from_le_bytes(name1),
            )
        } else {
            (0, 0, 0, 0, 0)
        };
        let (data0, data1) = if let Some(data) = data {
            let bytes = unsafe {
                std::slice::from_raw_parts(
                    data as *const UserData as *const u8,
                    std::mem::size_of::<UserData>(),
                )
            };
            (
                u64::from_le_bytes(bytes[0..8].try_into().unwrap()),
                u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
            )
        } else {
            (0, 0)
        };
        common::trace::emit_raw(
            common::trace::cat::ACC_PROFILE,
            &[
                stage,
                result.get_inner_value() as u64,
                user_lo,
                user_hi,
                timestamp,
                name0,
                name1,
                data0,
                data1,
            ],
        );
    }

    /// Upstream: IProfileCommon::Get (cmd 0)
    fn get_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let pm = svc.profile_manager.lock().unwrap();
        log::debug!("IProfileCommon::Get called, user_id=0x{:032x}", svc.user_id);

        let index = pm.get_user_index(svc.user_id);
        if let Some((base, data)) = pm.get_profile_base_and_data(index) {
            Self::trace_profile(1, RESULT_SUCCESS, Some(&base), Some(&data));
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
            Self::trace_profile(1, ResultCode::new(1), None, None);
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
            Self::trace_profile(2, RESULT_SUCCESS, Some(&base), None);
            let mut rb = ResponseBuilder::new(ctx, 16, 0, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_raw(&base);
        } else {
            Self::trace_profile(2, ResultCode::new(1), None, None);
            log::error!(
                "IProfileCommon::GetBase: Failed to get profile base for user=0x{:032x}",
                svc.user_id
            );
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(ResultCode::new(1)); // ResultUnknown
        }
    }

    /// Upstream: IProfileCommon::GetImageSize (cmd 10)
    fn get_image_size_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IProfileCommon::GetImageSize called");
        let image = load_profile_image_or_backup(svc.user_id);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(image.len() as u32);
    }

    /// Upstream: IProfileCommon::LoadImage (cmd 11)
    fn load_image_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        log::debug!("IProfileCommon::LoadImage called");
        let image = load_profile_image_or_backup(svc.user_id);
        let written = ctx.write_buffer(&image, 0);
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_u32(written as u32);
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

        let image_data = ctx.read_buffer_a(0);
        let user_data_buf = ctx.read_buffer_x(0);
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

        let image_path = get_image_path(svc.user_id);
        let write_image = image_path
            .parent()
            .is_some_and(|parent| std::fs::create_dir_all(parent).is_ok())
            && std::fs::write(&image_path, &image_data).is_ok();

        if !write_image || !pm.set_profile_base_and_data(svc.user_id, &base, &data) {
            log::error!("IProfileCommon::StoreWithImage: Failed to update profile data and image!");
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(super::errors::RESULT_ACCOUNT_UPDATE_FAILED);
            return;
        }

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

    let trace_boot = std::env::var_os("RUZU_APPLET_BOOT_TRACE")
        .is_some_and(|value| value != std::ffi::OsStr::new("0"));
    log::debug!("Account::LoopProcess - registering acc:u0, acc:u1, acc:su, acc:aa");

    let module = Arc::new(Module);
    let profile_manager = Arc::new(std::sync::Mutex::new(
        super::profile_manager::ProfileManager::new(),
    ));
    if trace_boot {
        log::info!("ACC::loop_process: profile manager ready");
    }

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();

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
        if trace_boot {
            log::info!("ACC::loop_process: registered acc:aa");
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
        if trace_boot {
            log::info!("ACC::loop_process: registered acc:su");
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
        if trace_boot {
            log::info!("ACC::loop_process: registered acc:u0");
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
    }
    if trace_boot {
        log::info!("ACC::loop_process: registered acc:u1");
    }

    ServerManager::run_server_shared(server_manager);
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

    #[test]
    fn store_save_data_thumbnail_result_matches_upstream_validation() {
        let system = crate::core::System::new();
        let interface = make_interface(crate::core::SystemRef::from_ref(&system));

        assert_eq!(
            interface.store_save_data_thumbnail_result(1, 1, THUMBNAIL_SIZE),
            RESULT_SUCCESS
        );
        assert_eq!(
            interface.store_save_data_thumbnail_result(1, 0, THUMBNAIL_SIZE),
            RESULT_INVALID_APPLICATION
        );
        assert_eq!(
            interface.store_save_data_thumbnail_result(0, 1, THUMBNAIL_SIZE),
            RESULT_INVALID_USER_ID
        );
        assert_eq!(
            interface.store_save_data_thumbnail_result(1, 1, THUMBNAIL_SIZE - 1),
            RESULT_INVALID_ARRAY_LENGTH
        );
    }
}

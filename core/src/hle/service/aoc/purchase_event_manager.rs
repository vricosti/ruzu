// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/aoc/purchase_event_manager.h
//! Port of zuyu/src/core/hle/service/aoc/purchase_event_manager.cpp
//!
//! IPurchaseEventManager service.

use std::collections::BTreeMap;

use crate::hle::result::{ErrorModule, ResultCode};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// Error: no purchased product info available.
/// Upstream: `ResultNoPurchasedProductInfoAvailable{ErrorModule::NIMShop, 400}`
const RESULT_NO_PURCHASED_PRODUCT_INFO_AVAILABLE: ResultCode =
    ResultCode::from_module_description(ErrorModule::NIMShop, 400);

/// IPC command IDs for IPurchaseEventManager
pub mod commands {
    pub const SET_DEFAULT_DELIVERY_TARGET: u32 = 0;
    pub const SET_DELIVERY_TARGET: u32 = 1;
    pub const GET_PURCHASED_EVENT: u32 = 2;
    pub const POP_PURCHASED_PRODUCT_INFO: u32 = 3;
    pub const POP_PURCHASED_PRODUCT_INFO_WITH_UID: u32 = 4;
}

/// IPurchaseEventManager service.
///
/// Corresponds to `IPurchaseEventManager` in upstream `purchase_event_manager.h`.
pub struct IPurchaseEventManager {
    service_context: crate::hle::service::kernel_helpers::ServiceContext,
    purchased_event_handle: u32,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IPurchaseEventManager {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (
                commands::POP_PURCHASED_PRODUCT_INFO,
                Some(Self::pop_purchased_product_info_handler),
                "PopPurchasedProductInfo",
            ),
            (
                commands::POP_PURCHASED_PRODUCT_INFO_WITH_UID,
                Some(Self::pop_purchased_product_info_with_uid_handler),
                "PopPurchasedProductInfoWithUid",
            ),
            (
                commands::GET_PURCHASED_EVENT,
                Some(Self::get_purchased_event_handler),
                "GetPurchasedEvent",
            ),
        ]);
        let mut service_context = crate::hle::service::kernel_helpers::ServiceContext::new(
            "IPurchaseEventManager".to_string(),
        );
        let purchased_event_handle = service_context.create_event("purchased_event".to_string());
        Self {
            service_context,
            purchased_event_handle,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Stubbed: SetDefaultDeliveryTarget (cmd 0)
    pub fn set_default_delivery_target(&self, process_id: u64, _in_buffer: &[u8]) {
        log::warn!(
            "(STUBBED) IPurchaseEventManager::set_default_delivery_target called, process_id={}",
            process_id
        );
    }

    /// Stubbed: SetDeliveryTarget (cmd 1)
    pub fn set_delivery_target(&self, unknown: u64, _in_buffer: &[u8]) {
        log::warn!(
            "(STUBBED) IPurchaseEventManager::set_delivery_target called, unknown={}",
            unknown
        );
    }

    /// GetPurchasedEvent (cmd 2) — returns the purchased event readable handle.
    pub fn get_purchased_event(&self) {
        log::warn!("IPurchaseEventManager::get_purchased_event called");
    }

    /// Stubbed: PopPurchasedProductInfo (cmd 3)
    pub fn pop_purchased_product_info(&self) -> Result<(), ResultCode> {
        log::debug!("(STUBBED) IPurchaseEventManager::pop_purchased_product_info called");
        Err(RESULT_NO_PURCHASED_PRODUCT_INFO_AVAILABLE)
    }

    /// Stubbed: PopPurchasedProductInfoWithUid (cmd 4)
    pub fn pop_purchased_product_info_with_uid(&self) -> Result<(), ResultCode> {
        log::debug!("(STUBBED) IPurchaseEventManager::pop_purchased_product_info_with_uid called");
        Err(RESULT_NO_PURCHASED_PRODUCT_INFO_AVAILABLE)
    }

    fn get_purchased_event_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IPurchaseEventManager) };
        service.get_purchased_event();
        if let Some(handle) = ctx.create_readable_event_handle(false) {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(crate::hle::result::RESULT_SUCCESS);
            rb.push_copy_objects(handle);
        } else {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(crate::hle::result::RESULT_SUCCESS);
            rb.push_copy_objects(0);
        }
    }

    fn pop_purchased_product_info_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IPurchaseEventManager) };
        let result = service
            .pop_purchased_product_info()
            .err()
            .unwrap_or(crate::hle::result::RESULT_SUCCESS);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    fn pop_purchased_product_info_with_uid_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let service =
            unsafe { &*(this as *const dyn ServiceFramework as *const IPurchaseEventManager) };
        let result = service
            .pop_purchased_product_info_with_uid()
            .err()
            .unwrap_or(crate::hle::result::RESULT_SUCCESS);
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }
}

impl SessionRequestHandler for IPurchaseEventManager {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }
}

impl ServiceFramework for IPurchaseEventManager {
    fn get_service_name(&self) -> &str {
        "aoc::IPurchaseEventManager"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

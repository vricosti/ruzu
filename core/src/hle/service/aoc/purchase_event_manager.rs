// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/aoc/purchase_event_manager.h
//! Port of zuyu/src/core/hle/service/aoc/purchase_event_manager.cpp
//!
//! IPurchaseEventManager service.

use crate::hle::result::{ErrorModule, ResultCode};

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
    // TODO: service_context, purchased_event
}

impl IPurchaseEventManager {
    pub fn new() -> Self {
        Self {}
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

    /// Stubbed: GetPurchasedEvent (cmd 2)
    pub fn get_purchased_event(&self) {
        log::warn!("IPurchaseEventManager::get_purchased_event called");
        // TODO: return event handle
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
}

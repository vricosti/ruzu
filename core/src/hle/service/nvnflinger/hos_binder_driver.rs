// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvnflinger/hos_binder_driver.h
//! Port of zuyu/src/core/hle/service/nvnflinger/hos_binder_driver.cpp
//!
//! IHOSBinderDriver is the service interface for binder transactions.
//! It delegates to HosBinderDriverServer for actual binder management.

use std::collections::BTreeMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use crate::hle::result::{ResultCode, RESULT_SUCCESS, RESULT_UNKNOWN};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::hos_binder_driver_server::HosBinderDriverServer;
use super::surface_flinger::SurfaceFlinger;

static TRACE_BINDER_TXN_COUNT: AtomicUsize = AtomicUsize::new(0);

fn trace_binder_txn(args: std::fmt::Arguments<'_>) {
    if std::env::var_os("RUZU_TRACE_BINDER_TXN").is_none() {
        return;
    }
    let idx = TRACE_BINDER_TXN_COUNT.fetch_add(1, Ordering::Relaxed);
    if idx < 192 {
        log::info!("{}", args);
    }
}

/// The IHOSBinderDriver service provides the display driver binder interface.
///
/// Upstream command table:
///   0: TransactParcel(id, transaction_id, parcel_data, parcel_reply, flags)
///   1: AdjustRefcount(id, addval, type)
///   2: GetNativeHandle(id, type_id) -> copy_handle
///   3: TransactParcelAuto(id, transaction_id, parcel_data, parcel_reply, flags)
pub struct IHosBinderDriver {
    server: Arc<HosBinderDriverServer>,
    surface_flinger: Arc<SurfaceFlinger>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IHosBinderDriver {
    pub fn new(server: Arc<HosBinderDriverServer>, surface_flinger: Arc<SurfaceFlinger>) -> Self {
        Self {
            server,
            surface_flinger,
            handlers: build_handler_map(&[
                (0, Some(Self::transact_parcel_handler), "TransactParcel"),
                (1, Some(Self::adjust_refcount_handler), "AdjustRefcount"),
                (2, Some(Self::get_native_handle_handler), "GetNativeHandle"),
                (
                    3,
                    Some(Self::transact_parcel_auto_handler),
                    "TransactParcelAuto",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn get_server(&self) -> &Arc<HosBinderDriverServer> {
        &self.server
    }

    pub fn get_surface_flinger(&self) -> Arc<SurfaceFlinger> {
        Arc::clone(&self.surface_flinger)
    }

    fn as_self(this: &dyn ServiceFramework) -> &Self {
        unsafe { &*(this as *const dyn ServiceFramework as *const Self) }
    }

    /// Shared implementation for TransactParcel and TransactParcelAuto.
    fn transact_parcel_impl(svc: &IHosBinderDriver, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let id = rp.pop_i32();
        let transaction_id = rp.pop_u32();
        let flags = rp.pop_u32();

        log::debug!(
            "IHOSBinderDriver::TransactParcel id={}, transaction={}, flags={}",
            id,
            transaction_id,
            flags
        );
        {
            use std::collections::HashMap;
            use std::sync::{Mutex, OnceLock};
            static COMBO_COUNTS: OnceLock<Mutex<HashMap<(i32, u32), u64>>> = OnceLock::new();
            let counts = COMBO_COUNTS.get_or_init(|| Mutex::new(HashMap::new()));
            let mut map = counts.lock().unwrap();
            let n = map.entry((id, transaction_id)).or_insert(0);
            let current = *n;
            *n += 1;
            drop(map);
            if current == 0 || current.is_power_of_two() {
                log::info!(
                    "[BINDER_TXN] id={} txn={} n={}",
                    id,
                    transaction_id,
                    current
                );
            }
        }

        let parcel_data = ctx.read_buffer(0);
        let write_size = ctx.get_write_buffer_size(0);
        let mut parcel_reply = vec![0u8; write_size];

        trace_binder_txn(format_args!(
            "IHOSBinderDriver::TransactParcel id={} txn={} flags={} in_len={} out_len={}",
            id,
            transaction_id,
            flags,
            parcel_data.len(),
            write_size
        ));

        if let Some(binder) = svc.server.try_get_binder(id) {
            binder.transact(transaction_id, &parcel_data, &mut parcel_reply, flags);
            // For txn=1 (RequestBuffer) dump up to 400 bytes; the GraphicBuffer
            // payload is ~380 bytes and we want to byte-diff against zuyu.
            let dump_len = if transaction_id == 1 { 400 } else { 32 };
            trace_binder_txn(format_args!(
                "IHOSBinderDriver::TransactParcel reply id={} txn={} reply_len={} first_bytes=[{:02x?}]",
                id,
                transaction_id,
                parcel_reply.len(),
                &parcel_reply[..parcel_reply.len().min(dump_len)]
            ));
        } else {
            log::warn!("TransactParcel: binder id={} not found", id);
        }
        // Upstream: R_SUCCEED_IF(binder == nullptr) — silently succeeds if not found

        if ctx.can_write_buffer(0) {
            ctx.write_buffer(&parcel_reply, 0);
        }

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 0: TransactParcel (uses HipcMapAlias buffers)
    fn transact_parcel_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        Self::transact_parcel_impl(svc, ctx);
    }

    /// cmd 1: AdjustRefcount (STUBBED)
    fn adjust_refcount_handler(_this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let mut rp = RequestParser::new(ctx);
        let id = rp.pop_i32();
        let addval = rp.pop_i32();
        let type_val = rp.pop_i32();
        log::warn!(
            "IHOSBinderDriver::AdjustRefcount (STUBBED) id={}, addval={}, type={}",
            id,
            addval,
            type_val
        );
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// cmd 2: GetNativeHandle
    fn get_native_handle_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        let mut rp = RequestParser::new(ctx);
        let id = rp.pop_i32();
        let type_id = rp.pop_u32();
        log::debug!(
            "IHOSBinderDriver::GetNativeHandle id={}, type_id={}",
            id,
            type_id
        );
        let Some(binder) = svc.server.try_get_binder(id) else {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(RESULT_UNKNOWN);
            return;
        };

        if let Some(thread) = ctx.get_thread() {
            let thread_guard = thread.lock().unwrap();
            if let (Some(parent), Some(scheduler)) = (
                thread_guard
                    .parent
                    .as_ref()
                    .and_then(|parent| parent.upgrade()),
                thread_guard
                    .scheduler
                    .as_ref()
                    .and_then(|scheduler| scheduler.upgrade()),
            ) {
                binder.register_native_handle_owner(parent, scheduler);
            }
        }

        let Some(readable_event) = binder.get_native_handle(type_id) else {
            let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
            rb.push_result(RESULT_SUCCESS);
            rb.push_copy_objects(0);
            return;
        };

        if std::env::var_os("RUZU_TRACE_BINDER_HANDLE").is_some() {
            log::info!(
                "IHOSBinderDriver::GetNativeHandle result id={} type_id={} object_id={}",
                id,
                type_id,
                readable_event.lock().unwrap().object_id
            );
        }

        let object_id = ctx
            .register_readable_event_object(readable_event)
            .unwrap_or(0);
        if std::env::var_os("RUZU_TRACE_BINDER_HANDLE").is_some() {
            log::info!(
                "IHOSBinderDriver::GetNativeHandle copy id={} type_id={} object_id={:#x}",
                id,
                type_id,
                object_id
            );
        }
        let mut rb = ResponseBuilder::new(ctx, 2, 1, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_copy_object_id(object_id);
    }

    /// cmd 3: TransactParcelAuto (same as TransactParcel but with AutoSelect buffers)
    fn transact_parcel_auto_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let svc = Self::as_self(this);
        Self::transact_parcel_impl(svc, ctx);
    }
}

impl SessionRequestHandler for IHosBinderDriver {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
    fn service_name(&self) -> &str {
        ServiceFramework::get_service_name(self)
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl ServiceFramework for IHosBinderDriver {
    fn get_service_name(&self) -> &str {
        "IHOSBinderDriver"
    }
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

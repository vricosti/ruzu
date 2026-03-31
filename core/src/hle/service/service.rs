// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/service.h and service.cpp
//! Status: Structural port
//!
//! Contains:
//! - ServerSessionCountMax constant
//! - ServiceFrameworkBase: non-generic base for service dispatch
//! - ServiceFramework trait: CRTP-like pattern for registering handlers
//!
//! The C++ CRTP pattern (ServiceFramework<Self>) is represented here as a trait
//! with handler registration and dispatch methods. The type-erasure pattern is
//! handled through function pointer dispatch.

use std::collections::BTreeMap;
use std::sync::Mutex;

use crate::hle::ipc;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers;

/// Default number of maximum connections to a server session.
pub const SERVER_SESSION_COUNT_MAX: u32 = 0x40;

const _: () = assert!(SERVER_SESSION_COUNT_MAX == 0x40);

/// Information about a single IPC handler function.
#[derive(Clone)]
pub struct FunctionInfo {
    pub expected_header: u32,
    pub handler_callback: Option<fn(&dyn ServiceFramework, &mut HLERequestContext)>,
    pub name: &'static str,
}

/// Trait that corresponds to upstream `ServiceFramework<Self>`.
///
/// Services implement this trait to register CMIF/TIPC handlers and dispatch IPC requests.
/// The upstream C++ CRTP (Curiously Recurring Template Pattern) is replaced by a trait.
///
/// Upstream stores `Core::System& system` in `ServiceFrameworkBase`, giving every service
/// access to `system.ServiceManager()`. The Rust port routes that global owner through
/// `HLERequestContext`, so control requests remain system-owned instead of service-owned.
pub trait ServiceFramework: SessionRequestHandler {
    /// Returns the service name.
    fn get_service_name(&self) -> &str;

    /// Returns the maximum number of concurrent sessions.
    fn get_max_sessions(&self) -> u32 {
        SERVER_SESSION_COUNT_MAX
    }

    /// Returns a reference to the CMIF handler map.
    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo>;

    /// Returns a reference to the TIPC handler map.
    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo>;

    /// Deprecated compatibility hook kept for older services.
    fn service_manager(
        &self,
    ) -> Option<std::sync::Arc<std::sync::Mutex<crate::hle::service::sm::sm::ServiceManager>>> {
        None
    }

    /// Invokes a service request routine using the HIPC protocol.
    fn invoke_request(&self, ctx: &mut HLERequestContext)
    where
        Self: Sized,
    {
        let cmd = ctx.get_command();
        let info = self.handlers().get(&cmd);

        match info {
            Some(fi) if fi.handler_callback.is_some() => {
                log::trace!("Service::{}: {}", self.get_service_name(), fi.name);
                if let Some(callback) = fi.handler_callback {
                    callback(self, ctx);
                }
            }
            _ => {
                self.report_unimplemented_function(ctx, info);
            }
        }
    }

    /// Invokes a service request routine using the TIPC protocol.
    fn invoke_request_tipc(&self, ctx: &mut HLERequestContext)
    where
        Self: Sized,
    {
        let cmd = ctx.get_command();
        let info = self.handlers_tipc().get(&cmd);

        match info {
            Some(fi) if fi.handler_callback.is_some() => {
                log::trace!("Service::{}: {}", self.get_service_name(), fi.name);
                if let Some(callback) = fi.handler_callback {
                    callback(self, ctx);
                }
            }
            _ => {
                self.report_unimplemented_function(ctx, info);
            }
        }
    }

    /// Reports an unimplemented function and writes a stub success response.
    fn report_unimplemented_function(
        &self,
        ctx: &mut HLERequestContext,
        info: Option<&FunctionInfo>,
    ) {
        let function_name = match info {
            Some(fi) => fi.name.to_string(),
            None => format!("{}", ctx.get_command()),
        };

        let cmd_buf = ctx.command_buffer();
        let mut buf = format!(
            "function '{}': port='{}' cmd_buf={{[0]=0x{:X}",
            function_name,
            self.get_service_name(),
            cmd_buf[0]
        );
        for i in 1..=8 {
            buf.push_str(&format!(", [{}]=0x{:X}", i, cmd_buf[i]));
        }
        buf.push('}');

        log::warn!("Unknown / unimplemented {}", buf);

        // Auto-stub: write a success response so the game doesn't read garbage
        // from the TLS buffer. Matches upstream behavior when use_auto_stub is enabled.
        // Without this, the request data stays in the buffer and the game misparses
        // it as a response, leading to state corruption and crashes.
        let mut rb = ipc_helpers::ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    /// Handles a synchronization request for the service.
    ///
    /// Corresponds to upstream `ServiceFrameworkBase::HandleSyncRequest`.
    fn handle_sync_request_impl(&self, ctx: &mut HLERequestContext) -> ResultCode
    where
        Self: Sized,
    {
        let mut result = RESULT_SUCCESS;

        match ctx.get_command_type() {
            ipc::CommandType::Close | ipc::CommandType::TipcClose => {
                let mut rb = ipc_helpers::ResponseBuilder::new(ctx, 2, 0, 0);
                rb.push_result(RESULT_SUCCESS);
                result = ipc_helpers::RESULT_SESSION_CLOSED;
            }
            ipc::CommandType::ControlWithContext | ipc::CommandType::Control => {
                // Matches upstream: system.ServiceManager().InvokeControlRequest(ctx)
                log::debug!(
                    "Control cmd={} on service '{}'",
                    ctx.get_command(),
                    self.get_service_name()
                );
                if let Some(sm) = ctx
                    .get_service_manager()
                    .cloned()
                    .or_else(|| self.service_manager())
                {
                    let controller = sm.lock().unwrap().controller_interface();
                    controller.invoke_request(ctx);
                } else {
                    log::warn!(
                        "Control request but no ServiceManager available for service '{}'",
                        self.get_service_name()
                    );
                }
            }
            ipc::CommandType::RequestWithContext | ipc::CommandType::Request => {
                self.invoke_request(ctx);
            }
            _ => {
                if ctx.is_tipc() {
                    self.invoke_request_tipc(ctx);
                } else {
                    log::warn!("Unimplemented command_type={:?}", ctx.get_command_type());
                    let mut rb = ipc_helpers::ResponseBuilder::new(ctx, 2, 0, 0);
                    rb.push_result(RESULT_SUCCESS);
                }
            }
        }

        // Write response back.
        ctx.write_to_outgoing_command_buffer();

        result
    }
}

/// Helper to build a handler map from a slice of (id, callback, name) tuples.
pub fn build_handler_map(
    functions: &[(
        u32,
        Option<fn(&dyn ServiceFramework, &mut HLERequestContext)>,
        &'static str,
    )],
) -> BTreeMap<u32, FunctionInfo> {
    let mut map = BTreeMap::new();
    for &(id, callback, name) in functions {
        map.insert(
            id,
            FunctionInfo {
                expected_header: id,
                handler_callback: callback,
                name,
            },
        );
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_server_session_count_max() {
        assert_eq!(SERVER_SESSION_COUNT_MAX, 0x40);
    }

    #[test]
    fn test_build_handler_map() {
        let map = build_handler_map(&[(0, None, "Initialize"), (1, None, "GetService")]);
        assert_eq!(map.len(), 2);
        assert!(map.contains_key(&0));
        assert!(map.contains_key(&1));
        assert_eq!(map[&0].name, "Initialize");
        assert_eq!(map[&1].name, "GetService");
    }
}

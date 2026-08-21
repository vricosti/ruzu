// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/csrng.h
//! Port of zuyu/src/core/hle/service/spl/csrng.cpp
//!
//! CSRNG service — cryptographic secure random number generator ("csrng").
//!
//! This is a Module::Interface variant with only GenerateRandomBytes (cmd 0).

use std::collections::BTreeMap;
use std::sync::Mutex;

use super::mt19937::Mt19937;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for CSRNG (IRandomInterface).
///
/// Corresponds to the function table in upstream csrng.cpp.
pub mod commands {
    pub const GENERATE_RANDOM_BYTES: u32 = 0;
}

/// CSRNG — IRandomInterface service.
///
/// Corresponds to `CSRNG` in upstream csrng.h / csrng.cpp. This is a
/// `Module::Interface` with only the `GenerateRandomBytes` handler.
/// Upstream inherits the `std::mt19937 rng` member from `Module::Interface`;
/// we mirror that with a persistent `Mutex<Mt19937>` per-instance.
pub struct Csrng {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    rng: Mutex<Mt19937>,
}

impl Csrng {
    pub fn new(rng_seed: Option<u32>) -> Self {
        // Default construction of upstream's `std::mt19937` uses seed 5489.
        let seed = rng_seed.unwrap_or(5489);

        let handlers = build_handler_map(&[(
            commands::GENERATE_RANDOM_BYTES,
            Some(Self::generate_random_bytes_handler),
            "GenerateRandomBytes",
        )]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            rng: Mutex::new(Mt19937::new(seed)),
        }
    }

    /// GenerateRandomBytes (cmd 0).
    ///
    /// Corresponds to `Module::Interface::GenerateRandomBytes` in upstream.
    pub fn generate_random_bytes(&self, buf: &mut [u8]) {
        log::debug!("CSRNG::generate_random_bytes called, size={}", buf.len());
        let mut rng = self.rng.lock().unwrap();
        for byte in buf.iter_mut() {
            // `uniform_int_distribution<u16>(0, 255)` reduces a full-range
            // 32-bit generator to 256 values using the high eight bits.
            *byte = (rng.next_u32() >> 24) as u8;
        }
    }

    fn generate_random_bytes_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut data = vec![0; ctx.get_write_buffer_size(0)];
        service.generate_random_bytes(&mut data);
        ctx.write_buffer(&data, 0);

        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl SessionRequestHandler for Csrng {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "csrng"
    }
}

impl ServiceFramework for Csrng {
    fn get_service_name(&self) -> &str {
        "csrng"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_random_bytes_handler_is_registered() {
        let service = Csrng::new(Some(1));
        let handler = service
            .handlers()
            .get(&commands::GENERATE_RANDOM_BYTES)
            .expect("GenerateRandomBytes command must exist");
        assert!(handler.handler_callback.is_some());
    }

    #[test]
    fn random_state_advances_between_calls() {
        let service = Csrng::new(Some(1));
        let mut first = [0; 32];
        let mut second = [0; 32];
        service.generate_random_bytes(&mut first);
        service.generate_random_bytes(&mut second);
        assert_ne!(first, second);
    }

    #[test]
    fn default_seed_matches_std_mt19937_uniform_u8_sequence() {
        let service = Csrng::new(None);
        let mut bytes = [0; 8];
        service.generate_random_bytes(&mut bytes);
        assert_eq!(bytes, [0xd0, 0x22, 0xe7, 0xd5, 0x20, 0xf8, 0xe9, 0x38]);
    }
}

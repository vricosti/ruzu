// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/csrng.h
//! Port of zuyu/src/core/hle/service/spl/csrng.cpp
//!
//! CSRNG service — cryptographic secure random number generator ("csrng").
//!
//! This is a Module::Interface variant with only GenerateRandomBytes (cmd 0).

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for CSRNG (IRandomInterface).
///
/// Corresponds to the function table in upstream csrng.cpp.
pub mod commands {
    pub const GENERATE_RANDOM_BYTES: u32 = 0;
}

/// CSRNG — IRandomInterface service.
///
/// Corresponds to `CSRNG` in upstream csrng.h / csrng.cpp.
/// This is a Module::Interface with only the GenerateRandomBytes handler.
pub struct Csrng {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    rng_seed: u32,
}

impl Csrng {
    pub fn new(rng_seed: Option<u32>) -> Self {
        let seed = rng_seed.unwrap_or_else(|| {
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs() as u32)
                .unwrap_or(0)
        });

        let handlers = build_handler_map(&[(0, None, "GenerateRandomBytes")]);

        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
            rng_seed: seed,
        }
    }

    /// GenerateRandomBytes (cmd 0).
    ///
    /// Corresponds to `Module::Interface::GenerateRandomBytes` in upstream.
    pub fn generate_random_bytes(&self, buf: &mut [u8]) {
        log::debug!("CSRNG::generate_random_bytes called, size={}", buf.len());
        // Use a simple PRNG. For proper emulation, this should use a
        // cryptographic RNG. Upstream uses std::mt19937 with
        // uniform_int_distribution<u16>.
        let mut state = self.rng_seed as u64;
        for byte in buf.iter_mut() {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            *byte = (state & 0xFF) as u8;
        }
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

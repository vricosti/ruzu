// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/services.h and services.cpp
//! Status: Structural stub
//!
//! Contains:
//! - Services: owns shared objects across service implementations, launches service processes.
//!
//! The upstream implementation creates and launches all HLE services (audio, filesystem,
//! SM, account, AM, etc.) on either host-core or guest-core processes. This is stubbed
//! until the individual service crates and kernel process infrastructure are available.
//! The service list matches upstream for reference.

/// The purpose of this struct is to own any objects that need to be shared across the other service
/// implementations. Will be torn down when the global system instance is shutdown.
///
/// Corresponds to upstream `Service::Services`.
pub struct Services {
    // In the full implementation, this launches all service processes.
    // Each service gets its own ServerManager and runs in its own kernel process/thread.
}

impl Services {
    /// Creates and launches all HLE services.
    ///
    /// Corresponds to upstream `Services::Services(sm, system, token)`.
    ///
    /// The full upstream implementation launches services on host-core and guest-core processes:
    ///
    /// Host-core processes (run on host threads):
    /// - audio, FS, jit, ldn, Loader, nvservices, bsdsocket, vi
    ///
    /// Guest-core processes (run on guest emulated cores):
    /// - sm, account, am, aoc, apm, bcat, bpc, btdrv, btm, capsrv, erpt, es, eupld, fatal,
    ///   fgm, friends, settings, psc, glue, grc, hid, lbl, LogManager.Prod, mig, mii, mm,
    ///   mnpp, nvnflinger, NCM, nfc, nfp, ngc, nifm, nim, npns, ns, olsc, omm, pcie, pctl,
    ///   pcv, prepo, ProcessManager, ptm, ro, spl, ssl, usb
    pub fn new() -> Self {
        // TODO: launch service processes when infrastructure is ready
        log::info!("Services::new - service launch stubbed");
        Self {}
    }
}

impl Drop for Services {
    fn drop(&mut self) {
        // Upstream: Services::~Services() = default;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_services_creation() {
        let _services = Services::new();
    }
}

// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/ns.h
//! Port of zuyu/src/core/hle/service/ns/ns.cpp
//!
//! NS LoopProcess registers the following named services:
//!   ns:am2, ns:ec, ns:rid, ns:rt, ns:web, ns:ro -> IServiceGetterInterface
//!   ns:dev                                        -> IDevelopInterface
//!   ns:su                                         -> ISystemUpdateInterface
//!   ns:vm                                         -> IVulnerabilityManagerInterface
//!   pdm:qry                                       -> IQueryService
//!   pl:s, pl:u                                    -> IPlatformServiceManager

/// Service names registered by NS LoopProcess.
///
/// Corresponds to the registrations in upstream ns.cpp `LoopProcess`.
pub const NS_SERVICE_GETTER_NAMES: &[&str] =
    &["ns:am2", "ns:ec", "ns:rid", "ns:rt", "ns:web", "ns:ro"];

/// LoopProcess — registers all NS services.
///
/// Corresponds to `Service::NS::LoopProcess` in upstream ns.cpp.
pub fn loop_process() {
    log::debug!("NS::LoopProcess called");
    // TODO: Register:
    //   ns:am2, ns:ec, ns:rid, ns:rt, ns:web, ns:ro -> IServiceGetterInterface
    //   ns:dev -> IDevelopInterface
    //   ns:su  -> ISystemUpdateInterface
    //   ns:vm  -> IVulnerabilityManagerInterface
    //   pdm:qry -> IQueryService
    //   pl:s, pl:u -> IPlatformServiceManager
}

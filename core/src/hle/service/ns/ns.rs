// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/ns.cpp/.h
//!
//! LoopProcess registers the following named services:
//!   ns:am2, ns:ec, ns:rid, ns:rt, ns:web, ns:ro  -> IServiceGetterInterface
//!   ns:dev                                         -> IDevelopInterface
//!   ns:su                                          -> ISystemUpdateInterface
//!   ns:vm                                          -> IVulnerabilityManagerInterface
//!   pdm:qry                                        -> IQueryService
//!   pl:s, pl:u                                     -> IPlatformServiceManager

/// Service names registered by NS LoopProcess.
pub const NS_SERVICE_NAMES: &[&str] = &[
    "ns:am2", "ns:ec", "ns:rid", "ns:rt", "ns:web", "ns:ro",
    "ns:dev", "ns:su", "ns:vm", "pdm:qry", "pl:s", "pl:u",
];

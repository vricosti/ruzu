// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/spl.h and spl.cpp
//!
//! SPL service variants and LoopProcess registration.
//!
//! Upstream defines: SPL, SPL_MIG, SPL_FS, SPL_SSL, SPL_ES, SPL_MANU
//! Each is a Module::Interface with a different IPC function table.

use std::sync::Arc;
use super::spl_module::ModuleInterface;

/// IPC function tables for each SPL service variant.
///
/// Corresponds to the function tables in upstream spl.cpp.
pub mod spl_functions {
    /// "spl:" -- basic SPL interface.
    pub const SPL_COMMANDS: &[(u32, &str)] = &[
        (0, "GetConfig"),
        (1, "ModularExponentiate"),
        (5, "SetConfig"),
        (7, "GenerateRandomBytes"),
        (11, "IsDevelopment"),
        (24, "SetBootReason"),
        (25, "GetBootReason"),
    ];

    /// "spl:mig" -- migration SPL interface.
    pub const SPL_MIG_COMMANDS: &[(u32, &str)] = &[
        (0, "GetConfig"),
        (1, "ModularExponentiate"),
        (2, "GenerateAesKek"),
        (3, "LoadAesKey"),
        (4, "GenerateAesKey"),
        (5, "SetConfig"),
        (7, "GenerateRandomBytes"),
        (11, "IsDevelopment"),
        (14, "DecryptAesKey"),
        (15, "CryptAesCtr"),
        (16, "ComputeCmac"),
        (21, "AllocateAesKeyslot"),
        (22, "DeallocateAesKeySlot"),
        (23, "GetAesKeyslotAvailableEvent"),
        (24, "SetBootReason"),
        (25, "GetBootReason"),
    ];

    /// "spl:fs" -- filesystem SPL interface.
    pub const SPL_FS_COMMANDS: &[(u32, &str)] = &[
        (0, "GetConfig"),
        (1, "ModularExponentiate"),
        (2, "GenerateAesKek"),
        (3, "LoadAesKey"),
        (4, "GenerateAesKey"),
        (5, "SetConfig"),
        (7, "GenerateRandomBytes"),
        (9, "ImportLotusKey"),
        (10, "DecryptLotusMessage"),
        (11, "IsDevelopment"),
        (12, "GenerateSpecificAesKey"),
        (14, "DecryptAesKey"),
        (15, "CryptAesCtr"),
        (16, "ComputeCmac"),
        (19, "LoadTitleKey"),
        (21, "AllocateAesKeyslot"),
        (22, "DeallocateAesKeySlot"),
        (23, "GetAesKeyslotAvailableEvent"),
        (24, "SetBootReason"),
        (25, "GetBootReason"),
        (31, "GetPackage2Hash"),
    ];

    /// "spl:ssl" -- SSL SPL interface.
    pub const SPL_SSL_COMMANDS: &[(u32, &str)] = &[
        (0, "GetConfig"),
        (1, "ModularExponentiate"),
        (2, "GenerateAesKek"),
        (3, "LoadAesKey"),
        (4, "GenerateAesKey"),
        (5, "SetConfig"),
        (7, "GenerateRandomBytes"),
        (11, "IsDevelopment"),
        (13, "DecryptDeviceUniqueData"),
        (14, "DecryptAesKey"),
        (15, "CryptAesCtr"),
        (16, "ComputeCmac"),
        (21, "AllocateAesKeyslot"),
        (22, "DeallocateAesKeySlot"),
        (23, "GetAesKeyslotAvailableEvent"),
        (24, "SetBootReason"),
        (25, "GetBootReason"),
        (26, "DecryptAndStoreSslClientCertKey"),
        (27, "ModularExponentiateWithSslClientCertKey"),
    ];

    /// "spl:es" -- ES SPL interface.
    pub const SPL_ES_COMMANDS: &[(u32, &str)] = &[
        (0, "GetConfig"),
        (1, "ModularExponentiate"),
        (2, "GenerateAesKek"),
        (3, "LoadAesKey"),
        (4, "GenerateAesKey"),
        (5, "SetConfig"),
        (7, "GenerateRandomBytes"),
        (11, "IsDevelopment"),
        (13, "DecryptDeviceUniqueData"),
        (14, "DecryptAesKey"),
        (15, "CryptAesCtr"),
        (16, "ComputeCmac"),
        (17, "ImportEsKey"),
        (18, "UnwrapTitleKey"),
        (20, "PrepareEsCommonKey"),
        (21, "AllocateAesKeyslot"),
        (22, "DeallocateAesKeySlot"),
        (23, "GetAesKeyslotAvailableEvent"),
        (24, "SetBootReason"),
        (25, "GetBootReason"),
        (28, "DecryptAndStoreDrmDeviceCertKey"),
        (29, "ModularExponentiateWithDrmDeviceCertKey"),
        (31, "PrepareEsArchiveKey"),
        (32, "LoadPreparedAesKey"),
    ];

    /// "spl:manu" -- manufacturing SPL interface.
    pub const SPL_MANU_COMMANDS: &[(u32, &str)] = &[
        (0, "GetConfig"),
        (1, "ModularExponentiate"),
        (2, "GenerateAesKek"),
        (3, "LoadAesKey"),
        (4, "GenerateAesKey"),
        (5, "SetConfig"),
        (7, "GenerateRandomBytes"),
        (11, "IsDevelopment"),
        (13, "DecryptDeviceUniqueData"),
        (14, "DecryptAesKey"),
        (15, "CryptAesCtr"),
        (16, "ComputeCmac"),
        (21, "AllocateAesKeyslot"),
        (22, "DeallocateAesKeySlot"),
        (23, "GetAesKeyslotAvailableEvent"),
        (24, "SetBootReason"),
        (25, "GetBootReason"),
        (30, "ReencryptDeviceUniqueData"),
    ];
}

/// SPL service ("spl:").
///
/// Corresponds to `SPL` in upstream spl.h / spl.cpp.
pub struct Spl {
    pub module: Arc<ModuleInterface>,
}

impl Spl {
    pub fn new(module: Arc<ModuleInterface>) -> Self {
        Self { module }
    }
}

/// SPL_MIG service ("spl:mig").
///
/// Corresponds to `SPL_MIG` in upstream spl.h.
pub struct SplMig {
    pub module: Arc<ModuleInterface>,
}

impl SplMig {
    pub fn new(module: Arc<ModuleInterface>) -> Self {
        Self { module }
    }
}

/// SPL_FS service ("spl:fs").
///
/// Corresponds to `SPL_FS` in upstream spl.h.
pub struct SplFs {
    pub module: Arc<ModuleInterface>,
}

impl SplFs {
    pub fn new(module: Arc<ModuleInterface>) -> Self {
        Self { module }
    }
}

/// SPL_SSL service ("spl:ssl").
///
/// Corresponds to `SPL_SSL` in upstream spl.h.
pub struct SplSsl {
    pub module: Arc<ModuleInterface>,
}

impl SplSsl {
    pub fn new(module: Arc<ModuleInterface>) -> Self {
        Self { module }
    }
}

/// SPL_ES service ("spl:es").
///
/// Corresponds to `SPL_ES` in upstream spl.h.
pub struct SplEs {
    pub module: Arc<ModuleInterface>,
}

impl SplEs {
    pub fn new(module: Arc<ModuleInterface>) -> Self {
        Self { module }
    }
}

/// SPL_MANU service ("spl:manu").
///
/// Corresponds to `SPL_MANU` in upstream spl.h.
pub struct SplManu {
    pub module: Arc<ModuleInterface>,
}

impl SplManu {
    pub fn new(module: Arc<ModuleInterface>) -> Self {
        Self { module }
    }
}

/// LoopProcess -- registers "csrng", "spl:", "spl:mig", "spl:fs", "spl:ssl",
/// "spl:es", "spl:manu" services.
///
/// Corresponds to `Service::SPL::LoopProcess` in upstream spl_module.cpp.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;
    use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;

    let mut server_manager = ServerManager::new(system);

    let stub_names = &[
        "csrng", "spl:", "spl:mig", "spl:fs", "spl:ssl", "spl:es", "spl:manu",
    ];
    for &name in stub_names {
        let svc_name = name.to_string();
        server_manager.register_named_service(
            name,
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(crate::hle::service::services::GenericStubService::new(&svc_name))
            }),
            16,
        );
    }

    ServerManager::run_server(server_manager);
}

// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/spl/spl.h
//! Port of zuyu/src/core/hle/service/spl/spl.cpp
//!
//! SPL service variants and LoopProcess registration.
//!
//! Upstream defines: SPL, SPL_MIG, SPL_FS, SPL_SSL, SPL_ES, SPL_MANU
//! Each is a Module::Interface with a different IPC function table.

/// IPC function tables for each SPL service variant.
///
/// Corresponds to the function tables in upstream spl.cpp.
pub mod spl_functions {
    /// "spl:" — basic SPL interface.
    pub const SPL_COMMANDS: &[(u32, &str)] = &[
        (0, "GetConfig"),
        (1, "ModularExponentiate"),
        (5, "SetConfig"),
        (7, "GenerateRandomBytes"),
        (11, "IsDevelopment"),
        (24, "SetBootReason"),
        (25, "GetBootReason"),
    ];

    /// "spl:mig" — migration SPL interface.
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

    /// "spl:fs" — filesystem SPL interface.
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

    /// "spl:ssl" — SSL SPL interface.
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

    /// "spl:es" — ES SPL interface.
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

    /// "spl:manu" — manufacturing SPL interface.
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

/// LoopProcess — registers "csrng", "spl:", "spl:mig", "spl:fs", "spl:ssl",
/// "spl:es", "spl:manu" services.
///
/// Corresponds to `Service::SPL::LoopProcess` in upstream spl_module.cpp.
pub fn loop_process() {
    log::debug!("SPL::LoopProcess called");
    // TODO: Create shared Module and register all SPL service variants
    // with ServerManager.
}

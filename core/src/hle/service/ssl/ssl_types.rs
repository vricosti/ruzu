// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ssl/ssl_types.h

#[repr(transparent)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CaCertificateId(i32);

#[allow(non_upper_case_globals)]
impl CaCertificateId {
    pub const All: Self = Self(-1);
    pub const NintendoCAG3: Self = Self(1);
    pub const NintendoClass2CAG3: Self = Self(2);
    pub const NintendoRootCAG4: Self = Self(3);
    pub const AmazonRootCA1: Self = Self(1000);
    pub const StarfieldServicesRootCertificateAuthorityG2: Self = Self(1001);
    pub const AddTrustExternalCARoot: Self = Self(1002);
    pub const COMODOCertificationAuthority: Self = Self(1003);
    pub const UTNDATACorpSGC: Self = Self(1004);
    pub const UTNUSERFirstHardware: Self = Self(1005);
    pub const BaltimoreCyberTrustRoot: Self = Self(1006);
    pub const CybertrustGlobalRoot: Self = Self(1007);
    pub const VerizonGlobalRootCA: Self = Self(1008);
    pub const DigiCertAssuredIDRootCA: Self = Self(1009);
    pub const DigiCertAssuredIDRootG2: Self = Self(1010);
    pub const DigiCertGlobalRootCA: Self = Self(1011);
    pub const DigiCertGlobalRootG2: Self = Self(1012);
    pub const DigiCertHighAssuranceEVRootCA: Self = Self(1013);
    pub const EntrustnetCertificationAuthority2048: Self = Self(1014);
    pub const EntrustRootCertificationAuthority: Self = Self(1015);
    pub const EntrustRootCertificationAuthorityG2: Self = Self(1016);
    pub const GeoTrustGlobalCA2: Self = Self(1017);
    pub const GeoTrustGlobalCA: Self = Self(1018);
    pub const GeoTrustPrimaryCertificationAuthorityG3: Self = Self(1019);
    pub const GeoTrustPrimaryCertificationAuthority: Self = Self(1020);
    pub const GlobalSignRootCA: Self = Self(1021);
    pub const GlobalSignRootCAR2: Self = Self(1022);
    pub const GlobalSignRootCAR3: Self = Self(1023);
    pub const GoDaddyClass2CertificationAuthority: Self = Self(1024);
    pub const GoDaddyRootCertificateAuthorityG2: Self = Self(1025);
    pub const StarfieldClass2CertificationAuthority: Self = Self(1026);
    pub const StarfieldRootCertificateAuthorityG2: Self = Self(1027);
    pub const ThawtePrimaryRootCAG3: Self = Self(1028);
    pub const ThawtePrimaryRootCA: Self = Self(1029);
    pub const VeriSignClass3PublicPrimaryCertificationAuthorityG3: Self = Self(1030);
    pub const VeriSignClass3PublicPrimaryCertificationAuthorityG5: Self = Self(1031);
    pub const VeriSignUniversalRootCertificationAuthority: Self = Self(1032);
    pub const DSTRootCAX3: Self = Self(1033);
    pub const USERTrustRsaCertificationAuthority: Self = Self(1034);
    pub const ISRGRootX10: Self = Self(1035);
    pub const USERTrustEccCertificationAuthority: Self = Self(1036);
    pub const COMODORsaCertificationAuthority: Self = Self(1037);
    pub const COMODOEccCertificationAuthority: Self = Self(1038);
    pub const AmazonRootCA2: Self = Self(1039);
    pub const AmazonRootCA3: Self = Self(1040);
    pub const AmazonRootCA4: Self = Self(1041);
    pub const DigiCertAssuredIDRootG3: Self = Self(1042);
    pub const DigiCertGlobalRootG3: Self = Self(1043);
    pub const DigiCertTrustedRootG4: Self = Self(1044);
    pub const EntrustRootCertificationAuthorityEC1: Self = Self(1045);
    pub const EntrustRootCertificationAuthorityG4: Self = Self(1046);
    pub const GlobalSignECCRootCAR4: Self = Self(1047);
    pub const GlobalSignECCRootCAR5: Self = Self(1048);
    pub const GlobalSignECCRootCAR6: Self = Self(1049);
    pub const GTSRootR1: Self = Self(1050);
    pub const GTSRootR2: Self = Self(1051);
    pub const GTSRootR3: Self = Self(1052);
    pub const GTSRootR4: Self = Self(1053);
    pub const SecurityCommunicationRootCA: Self = Self(1054);
    pub const GlobalSignRootE4: Self = Self(1055);
    pub const GlobalSignRootR4: Self = Self(1056);
    pub const TTeleSecGlobalRootClass2: Self = Self(1057);
    pub const DigiCertTLSECCP384RootG5: Self = Self(1058);
    pub const DigiCertTLSRSA4096RootG5: Self = Self(1059);

    pub const fn from_raw(raw: i32) -> Self {
        Self(raw)
    }

    pub const fn raw(self) -> i32 {
        self.0
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TrustedCertStatus(i32);

#[allow(non_upper_case_globals)]
impl TrustedCertStatus {
    pub const Invalid: Self = Self(-1);
    pub const Removed: Self = Self(0);
    pub const EnabledTrusted: Self = Self(1);
    pub const EnabledNotTrusted: Self = Self(2);
    pub const Revoked: Self = Self(3);

    pub const fn from_raw(raw: i32) -> Self {
        Self(raw)
    }

    pub const fn raw(self) -> i32 {
        self.0
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct BuiltInCertificateInfo {
    pub cert_id: CaCertificateId,
    pub status: TrustedCertStatus,
    pub der_size: u64,
    pub der_offset: u64,
}
const _: () = assert!(std::mem::size_of::<BuiltInCertificateInfo>() == 0x18);

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CertStoreHeader {
    pub magic: u32,
    pub num_entries: u32,
}
const _: () = assert!(std::mem::size_of::<CertStoreHeader>() == 0x8);

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CertStoreEntry {
    pub certificate_id: CaCertificateId,
    pub certificate_status: TrustedCertStatus,
    pub der_size: u32,
    pub der_offset: u32,
}
const _: () = assert!(std::mem::size_of::<CertStoreEntry>() == 0x10);

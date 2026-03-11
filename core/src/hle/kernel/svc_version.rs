//! Port of zuyu/src/core/hle/kernel/svc_version.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! SVC kernel version encoding/decoding and version constants.
//! There is no svc_version.cpp in upstream (header-only).

/// Convert SDK major version to SVC major version.
pub const fn convert_to_svc_major_version(sdk: u32) -> u32 {
    sdk + 4
}

/// Convert SVC major version to SDK major version.
pub const fn convert_to_sdk_major_version(svc: u32) -> u32 {
    svc - 4
}

/// Convert SDK minor version to SVC minor version (identity).
pub const fn convert_to_svc_minor_version(sdk: u32) -> u32 {
    sdk
}

/// Convert SVC minor version to SDK minor version (identity).
pub const fn convert_to_sdk_minor_version(svc: u32) -> u32 {
    svc
}

/// Bit positions and widths for KernelVersion encoding.
/// Layout: bits [0, 4) = minor_version, bits [4, 17) = major_version.
const MINOR_VERSION_BITS: u32 = 4;
const MINOR_VERSION_SHIFT: u32 = 0;
const MAJOR_VERSION_BITS: u32 = 13;
const MAJOR_VERSION_SHIFT: u32 = 4;

/// Encode a kernel version from major and minor components.
/// Mirrors upstream `EncodeKernelVersion(u32 major, u32 minor)`.
pub const fn encode_kernel_version(major: u32, minor: u32) -> u32 {
    let minor_mask = ((1u32 << MINOR_VERSION_BITS) - 1) << MINOR_VERSION_SHIFT;
    let major_mask = ((1u32 << MAJOR_VERSION_BITS) - 1) << MAJOR_VERSION_SHIFT;
    ((minor << MINOR_VERSION_SHIFT) & minor_mask) | ((major << MAJOR_VERSION_SHIFT) & major_mask)
}

/// Extract the major version from an encoded kernel version.
pub const fn get_kernel_major_version(encoded: u32) -> u32 {
    (encoded >> MAJOR_VERSION_SHIFT) & ((1u32 << MAJOR_VERSION_BITS) - 1)
}

/// Extract the minor version from an encoded kernel version.
pub const fn get_kernel_minor_version(encoded: u32) -> u32 {
    (encoded >> MINOR_VERSION_SHIFT) & ((1u32 << MINOR_VERSION_BITS) - 1)
}

/// Minimum required kernel major version.
/// Nintendo doesn't support programs targeting SVC versions < 3.0.
pub const REQUIRED_KERNEL_MAJOR_VERSION: u32 = 3;
/// Minimum required kernel minor version.
pub const REQUIRED_KERNEL_MINOR_VERSION: u32 = 0;
/// Minimum required kernel version (encoded).
pub const REQUIRED_KERNEL_VERSION: u32 =
    encode_kernel_version(REQUIRED_KERNEL_MAJOR_VERSION, REQUIRED_KERNEL_MINOR_VERSION);

/// Highest supported SVC major version.
/// Official kernel versions have SVC major = SDK major + 4, SVC minor = SDK minor.
pub const SUPPORTED_KERNEL_MAJOR_VERSION: u32 = convert_to_svc_major_version(15);
/// Highest supported SVC minor version.
pub const SUPPORTED_KERNEL_MINOR_VERSION: u32 = convert_to_svc_minor_version(3);
/// Highest supported kernel version (encoded).
pub const SUPPORTED_KERNEL_VERSION: u32 =
    encode_kernel_version(SUPPORTED_KERNEL_MAJOR_VERSION, SUPPORTED_KERNEL_MINOR_VERSION);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_conversion() {
        assert_eq!(convert_to_svc_major_version(15), 19);
        assert_eq!(convert_to_sdk_major_version(19), 15);
        assert_eq!(convert_to_svc_minor_version(3), 3);
        assert_eq!(convert_to_sdk_minor_version(3), 3);
    }

    #[test]
    fn test_encode_decode_kernel_version() {
        let encoded = encode_kernel_version(19, 3);
        assert_eq!(get_kernel_major_version(encoded), 19);
        assert_eq!(get_kernel_minor_version(encoded), 3);
    }

    #[test]
    fn test_required_version() {
        assert_eq!(get_kernel_major_version(REQUIRED_KERNEL_VERSION), 3);
        assert_eq!(get_kernel_minor_version(REQUIRED_KERNEL_VERSION), 0);
    }

    #[test]
    fn test_supported_version() {
        assert_eq!(SUPPORTED_KERNEL_MAJOR_VERSION, 19);
        assert_eq!(SUPPORTED_KERNEL_MINOR_VERSION, 3);
        assert_eq!(get_kernel_major_version(SUPPORTED_KERNEL_VERSION), 19);
        assert_eq!(get_kernel_minor_version(SUPPORTED_KERNEL_VERSION), 3);
    }
}

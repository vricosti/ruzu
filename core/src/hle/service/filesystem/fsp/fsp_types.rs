//! Port of zuyu/src/core/hle/service/filesystem/fsp/fsp_types.h
//!
//! Types used by the FSP service.

/// Port of Service::FileSystem::FileSystemProxyType
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum FileSystemProxyType {
    Code = 0,
    Rom = 1,
    Logo = 2,
    Control = 3,
    Manual = 4,
    Meta = 5,
    Data = 6,
    Package = 7,
    RegisteredUpdate = 8,
}

/// Port of Service::FileSystem::SizeGetter
///
/// Provides free and total size queries for a given storage.
pub struct SizeGetter {
    pub get_free_size: Box<dyn Fn() -> u64 + Send + Sync>,
    pub get_total_size: Box<dyn Fn() -> u64 + Send + Sync>,
}

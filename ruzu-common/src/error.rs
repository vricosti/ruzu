// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

/// HOS result code: module (9 bits) + description (13 bits).
/// Layout: bits [8:0] = module, bits [21:9] = description.
/// Value 0 = success.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ResultCode(pub u32);

impl ResultCode {
    pub const SUCCESS: Self = Self(0);

    /// Create a result code from module and description.
    #[inline]
    pub const fn new(module: u32, description: u32) -> Self {
        Self((module & 0x1FF) | ((description & 0x1FFF) << 9))
    }

    /// Extract the module number.
    #[inline]
    pub const fn module(self) -> u32 {
        self.0 & 0x1FF
    }

    /// Extract the description number.
    #[inline]
    pub const fn description(self) -> u32 {
        (self.0 >> 9) & 0x1FFF
    }

    /// Check if this is a success result.
    #[inline]
    pub const fn is_success(self) -> bool {
        self.0 == 0
    }

    /// Check if this is an error result.
    #[inline]
    pub const fn is_error(self) -> bool {
        self.0 != 0
    }

    /// Raw u32 value.
    #[inline]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

impl std::fmt::Debug for ResultCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_success() {
            write!(f, "ResultCode::SUCCESS")
        } else {
            write!(
                f,
                "ResultCode(module={}, desc={}, raw=0x{:08X})",
                self.module(),
                self.description(),
                self.0
            )
        }
    }
}

impl std::error::Error for ResultCode {}

impl std::fmt::Display for ResultCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_success() {
            write!(f, "Success")
        } else {
            write!(
                f,
                "Error(module={}, desc={})",
                self.module(),
                self.description()
            )
        }
    }
}

/// Common HOS error modules.
pub mod module {
    pub const KERNEL: u32 = 1;
    pub const FS: u32 = 2;
    pub const OS: u32 = 3;
    pub const HTCS: u32 = 4;
    pub const NCM: u32 = 5;
    pub const LR: u32 = 8;
    pub const LOADER: u32 = 9;
    pub const SM: u32 = 21;
    pub const RO: u32 = 22;
    pub const PM: u32 = 15;
    pub const NS: u32 = 16;
    pub const HID: u32 = 202;
    pub const AM: u32 = 128;
    pub const VI: u32 = 114;
    pub const NV: u32 = 408;
    pub const SET: u32 = 105;
}

// Common kernel error codes (flat at module level for easy import).
pub const INVALID_SIZE: ResultCode = ResultCode::new(module::KERNEL, 101);
pub const INVALID_ADDRESS: ResultCode = ResultCode::new(module::KERNEL, 102);
pub const OUT_OF_RESOURCE: ResultCode = ResultCode::new(module::KERNEL, 103);
pub const OUT_OF_MEMORY: ResultCode = ResultCode::new(module::KERNEL, 104);
pub const HANDLE_TABLE_FULL: ResultCode = ResultCode::new(module::KERNEL, 105);
pub const INVALID_MEMORY_STATE: ResultCode = ResultCode::new(module::KERNEL, 106);
pub const INVALID_MEMORY_PERMISSIONS: ResultCode = ResultCode::new(module::KERNEL, 108);
pub const INVALID_MEMORY_RANGE: ResultCode = ResultCode::new(module::KERNEL, 110);
pub const INVALID_PRIORITY: ResultCode = ResultCode::new(module::KERNEL, 112);
pub const INVALID_CORE_ID: ResultCode = ResultCode::new(module::KERNEL, 113);
pub const INVALID_HANDLE: ResultCode = ResultCode::new(module::KERNEL, 114);
pub const INVALID_POINTER: ResultCode = ResultCode::new(module::KERNEL, 115);
pub const INVALID_COMBINATION: ResultCode = ResultCode::new(module::KERNEL, 116);
pub const TIMEOUT: ResultCode = ResultCode::new(module::KERNEL, 117);
pub const CANCELLED: ResultCode = ResultCode::new(module::KERNEL, 118);
pub const OUT_OF_RANGE: ResultCode = ResultCode::new(module::KERNEL, 119);
pub const INVALID_ENUM_VALUE: ResultCode = ResultCode::new(module::KERNEL, 120);
pub const NOT_FOUND: ResultCode = ResultCode::new(module::KERNEL, 121);
pub const BUSY: ResultCode = ResultCode::new(module::KERNEL, 122);
pub const SESSION_CLOSED: ResultCode = ResultCode::new(module::KERNEL, 123);
pub const INVALID_STATE: ResultCode = ResultCode::new(module::KERNEL, 124);
pub const PORT_REMOTE_CLOSED: ResultCode = ResultCode::new(module::KERNEL, 301);

/// sm: service not registered.
pub const SM_NOT_REGISTERED: ResultCode = ResultCode::new(module::SM, 7);
pub const SM_ALREADY_REGISTERED: ResultCode = ResultCode::new(module::SM, 4);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_result_code() {
        let rc = ResultCode::new(1, 101);
        assert_eq!(rc.module(), 1);
        assert_eq!(rc.description(), 101);
        assert!(rc.is_error());
        assert!(!rc.is_success());
    }

    #[test]
    fn test_success() {
        assert!(ResultCode::SUCCESS.is_success());
        assert_eq!(ResultCode::SUCCESS.module(), 0);
        assert_eq!(ResultCode::SUCCESS.description(), 0);
    }
}

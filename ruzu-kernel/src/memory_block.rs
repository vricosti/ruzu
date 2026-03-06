//! Port of zuyu/src/core/hle/kernel/k_memory_block.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Kernel memory state, permission, and attribute types.
//! These are the "full" kernel-internal types (with flag bits), as opposed to
//! the SVC-visible types. The SVC types are a strict subset (lower 8 bits).

use bitflags::bitflags;

/// Page size used by the kernel (4 KiB).
pub const PAGE_SIZE: usize = 0x1000;
/// Page size as u64.
pub const PAGE_SIZE_U64: u64 = PAGE_SIZE as u64;

// ---------------------------------------------------------------------------
// SVC-visible memory state (lower 8 bits)
// ---------------------------------------------------------------------------

/// SVC-visible memory state values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SvcMemoryState {
    Free = 0x00,
    Io = 0x01,
    Static = 0x02,
    Code = 0x03,
    CodeData = 0x04,
    Normal = 0x05,
    Shared = 0x06,
    Alias = 0x07,
    AliasCode = 0x08,
    AliasCodeData = 0x09,
    Ipc = 0x0A,
    Stack = 0x0B,
    ThreadLocal = 0x0C,
    Transferred = 0x0D,
    SharedTransferred = 0x0E,
    SharedCode = 0x0F,
    Inaccessible = 0x10,
    NonSecureIpc = 0x11,
    NonDeviceIpc = 0x12,
    Kernel = 0x13,
    GeneratedCode = 0x14,
    CodeOut = 0x15,
    Coverage = 0x16,
    Insecure = 0x17,
}

// ---------------------------------------------------------------------------
// Kernel memory state (full, with flag bits)
// ---------------------------------------------------------------------------

bitflags! {
    /// Full kernel memory state: SVC state in bits [0..7], flags in bits [8..27].
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct KMemoryState: u32 {
        const NONE = 0;
        const MASK = 0xFF;

        // Flag bits
        const FLAG_CAN_REPROTECT           = 1 << 8;
        const FLAG_CAN_DEBUG               = 1 << 9;
        const FLAG_CAN_USE_IPC             = 1 << 10;
        const FLAG_CAN_USE_NON_DEVICE_IPC  = 1 << 11;
        const FLAG_CAN_USE_NON_SECURE_IPC  = 1 << 12;
        const FLAG_MAPPED                  = 1 << 13;
        const FLAG_CODE                    = 1 << 14;
        const FLAG_CAN_ALIAS               = 1 << 15;
        const FLAG_CAN_CODE_ALIAS          = 1 << 16;
        const FLAG_CAN_TRANSFER            = 1 << 17;
        const FLAG_CAN_QUERY_PHYSICAL      = 1 << 18;
        const FLAG_CAN_DEVICE_MAP          = 1 << 19;
        const FLAG_CAN_ALIGNED_DEVICE_MAP  = 1 << 20;
        const FLAG_CAN_IPC_USER_BUFFER     = 1 << 21;
        const FLAG_REFERENCE_COUNTED       = 1 << 22;
        const FLAG_CAN_MAP_PROCESS         = 1 << 23;
        const FLAG_CAN_CHANGE_ATTRIBUTE    = 1 << 24;
        const FLAG_CAN_CODE_MEMORY         = 1 << 25;
        const FLAG_LINEAR_MAPPED           = 1 << 26;
        const FLAG_CAN_PERMISSION_LOCK     = 1 << 27;

        // Composite flags
        const FLAGS_DATA = Self::FLAG_CAN_REPROTECT.bits()
            | Self::FLAG_CAN_USE_IPC.bits() | Self::FLAG_CAN_USE_NON_DEVICE_IPC.bits()
            | Self::FLAG_CAN_USE_NON_SECURE_IPC.bits() | Self::FLAG_MAPPED.bits()
            | Self::FLAG_CAN_ALIAS.bits() | Self::FLAG_CAN_TRANSFER.bits()
            | Self::FLAG_CAN_QUERY_PHYSICAL.bits() | Self::FLAG_CAN_DEVICE_MAP.bits()
            | Self::FLAG_CAN_ALIGNED_DEVICE_MAP.bits() | Self::FLAG_CAN_IPC_USER_BUFFER.bits()
            | Self::FLAG_REFERENCE_COUNTED.bits() | Self::FLAG_CAN_CHANGE_ATTRIBUTE.bits()
            | Self::FLAG_LINEAR_MAPPED.bits();

        const FLAGS_CODE = Self::FLAG_CAN_DEBUG.bits()
            | Self::FLAG_CAN_USE_IPC.bits() | Self::FLAG_CAN_USE_NON_DEVICE_IPC.bits()
            | Self::FLAG_CAN_USE_NON_SECURE_IPC.bits() | Self::FLAG_MAPPED.bits()
            | Self::FLAG_CODE.bits() | Self::FLAG_CAN_QUERY_PHYSICAL.bits()
            | Self::FLAG_CAN_DEVICE_MAP.bits() | Self::FLAG_CAN_ALIGNED_DEVICE_MAP.bits()
            | Self::FLAG_REFERENCE_COUNTED.bits() | Self::FLAG_LINEAR_MAPPED.bits();

        const FLAGS_MISC = Self::FLAG_MAPPED.bits() | Self::FLAG_REFERENCE_COUNTED.bits()
            | Self::FLAG_CAN_QUERY_PHYSICAL.bits() | Self::FLAG_CAN_DEVICE_MAP.bits()
            | Self::FLAG_LINEAR_MAPPED.bits();

        // Concrete states (matching zuyu's static_asserts)
        const FREE              = 0x0000_0000;
        const IO_MEMORY         = 0x0018_2001;
        const IO_REGISTER       = 0x0018_0001;
        const STATIC            = 0x0004_2002;
        const CODE              = 0x04DC_7E03;
        const CODE_DATA         = 0x0FFE_BD04;
        const NORMAL            = 0x077E_BD05;
        const SHARED            = 0x0440_2006;
        const ALIAS_CODE        = 0x04DD_7E08;
        const ALIAS_CODE_DATA   = 0x0FFF_BD09;
        const IPC               = 0x045C_3C0A;
        const STACK             = 0x045C_3C0B;
        const THREAD_LOCAL      = 0x0400_000C;
        const TRANSFERRED       = 0x055C_3C0D;
        const SHARED_TRANSFERRED = 0x045C_380E;
        const SHARED_CODE       = 0x0440_380F;
        const INACCESSIBLE      = 0x0000_0010;
        const NON_SECURE_IPC    = 0x045C_3811;
        const NON_DEVICE_IPC    = 0x044C_2812;
        const KERNEL            = 0x0000_0013;
        const GENERATED_CODE    = 0x0440_2214;
        const CODE_OUT          = 0x0440_2015;
        const COVERAGE          = 0x0000_2016;
        const INSECURE          = 0x055C_3817;
    }
}

impl KMemoryState {
    /// Get the SVC-visible state (lower 8 bits).
    pub fn svc_state(self) -> u32 {
        self.bits() & 0xFF
    }
}

// ---------------------------------------------------------------------------
// SVC memory permission
// ---------------------------------------------------------------------------

bitflags! {
    /// SVC-visible memory permission flags.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct SvcMemoryPermission: u32 {
        const NONE    = 0;
        const READ    = 1 << 0;
        const WRITE   = 1 << 1;
        const EXECUTE = 1 << 2;
    }
}

// ---------------------------------------------------------------------------
// Kernel memory permission
// ---------------------------------------------------------------------------

bitflags! {
    /// Full kernel memory permission flags.
    ///
    /// User permissions are in bits [0..2], kernel permissions are shifted
    /// by 3 bits into bits [3..5]. Bit 6 means "not mapped".
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct KMemoryPermission: u8 {
        const NONE = 0;

        // User permissions
        const USER_READ    = 1 << 0;
        const USER_WRITE   = 1 << 1;
        const USER_EXECUTE = 1 << 2;

        // Kernel permissions (shifted by 3)
        const KERNEL_READ    = 1 << 3;
        const KERNEL_WRITE   = 1 << 4;
        const KERNEL_EXECUTE = 1 << 5;

        const NOT_MAPPED = 1 << 6;

        // Combined
        const KERNEL_READ_WRITE    = Self::KERNEL_READ.bits() | Self::KERNEL_WRITE.bits();
        const KERNEL_READ_EXECUTE  = Self::KERNEL_READ.bits() | Self::KERNEL_EXECUTE.bits();

        const USER_READ_WRITE    = Self::USER_READ.bits() | Self::USER_WRITE.bits()
                                   | Self::KERNEL_READ.bits() | Self::KERNEL_WRITE.bits();
        const USER_READ_EXECUTE  = Self::USER_READ.bits() | Self::USER_EXECUTE.bits()
                                   | Self::KERNEL_READ.bits();

        const USER_MASK = Self::USER_READ.bits() | Self::USER_WRITE.bits() | Self::USER_EXECUTE.bits();
        const IPC_LOCK_CHANGE_MASK = Self::NOT_MAPPED.bits() | Self::USER_READ_WRITE.bits();
    }
}

/// Convert SVC permission to kernel permission.
pub fn convert_to_k_memory_permission(perm: SvcMemoryPermission) -> KMemoryPermission {
    let mut result = KMemoryPermission::empty();

    if perm.contains(SvcMemoryPermission::READ) {
        result |= KMemoryPermission::USER_READ | KMemoryPermission::KERNEL_READ;
    }
    if perm.contains(SvcMemoryPermission::WRITE) {
        result |= KMemoryPermission::USER_WRITE | KMemoryPermission::KERNEL_WRITE;
    }
    if perm.contains(SvcMemoryPermission::EXECUTE) {
        result |= KMemoryPermission::USER_EXECUTE;
    }

    if perm == SvcMemoryPermission::NONE {
        result |= KMemoryPermission::NOT_MAPPED;
    }

    result
}

// ---------------------------------------------------------------------------
// Kernel memory attribute
// ---------------------------------------------------------------------------

bitflags! {
    /// Kernel memory attribute flags.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct KMemoryAttribute: u8 {
        const NONE              = 0;
        const LOCKED            = 1 << 0;
        const IPC_LOCKED        = 1 << 1;
        const DEVICE_SHARED     = 1 << 2;
        const UNCACHED          = 1 << 3;
        const PERMISSION_LOCKED = 1 << 4;

        const SET_MASK          = Self::UNCACHED.bits() | Self::PERMISSION_LOCKED.bits();
    }
}

// ---------------------------------------------------------------------------
// KMemoryInfo (result of QueryMemory)
// ---------------------------------------------------------------------------

/// Information about a memory block, used by QueryMemory SVC.
#[derive(Debug, Clone)]
pub struct KMemoryInfo {
    /// Base address of the block.
    pub address: u64,
    /// Size in bytes.
    pub size: u64,
    /// Kernel memory state.
    pub state: KMemoryState,
    /// Kernel memory permission.
    pub permission: KMemoryPermission,
    /// Kernel memory attribute.
    pub attribute: KMemoryAttribute,
    /// IPC lock count.
    pub ipc_lock_count: u16,
    /// Device use count.
    pub device_use_count: u16,
}

impl KMemoryInfo {
    /// Get the end address (exclusive).
    pub fn end_address(&self) -> u64 {
        self.address + self.size
    }

    /// Get the SVC-visible memory state.
    pub fn svc_state(&self) -> u32 {
        self.state.svc_state()
    }

    /// Get the SVC-visible permission.
    pub fn svc_permission(&self) -> u32 {
        self.permission.bits() as u32 & KMemoryPermission::USER_MASK.bits() as u32
    }

    /// Get the SVC-visible attribute.
    pub fn svc_attribute(&self) -> u32 {
        self.attribute.bits() as u32
    }
}

// ---------------------------------------------------------------------------
// KMemoryBlock
// ---------------------------------------------------------------------------

/// A single block in the memory block manager's list.
///
/// Represents a contiguous region of virtual memory with uniform state,
/// permission, and attribute.
#[derive(Debug, Clone)]
pub struct KMemoryBlock {
    pub address: u64,
    pub num_pages: usize,
    pub state: KMemoryState,
    pub permission: KMemoryPermission,
    pub attribute: KMemoryAttribute,
    pub original_permission: KMemoryPermission,
    pub ipc_lock_count: u16,
    pub device_use_count: u16,
}

impl KMemoryBlock {
    /// Create a new memory block.
    pub fn new(
        address: u64,
        num_pages: usize,
        state: KMemoryState,
        permission: KMemoryPermission,
        attribute: KMemoryAttribute,
    ) -> Self {
        Self {
            address,
            num_pages,
            state,
            permission,
            attribute,
            original_permission: KMemoryPermission::NONE,
            ipc_lock_count: 0,
            device_use_count: 0,
        }
    }

    /// Get the size in bytes.
    pub fn size(&self) -> u64 {
        (self.num_pages as u64) * PAGE_SIZE_U64
    }

    /// Get the end address (exclusive).
    pub fn end_address(&self) -> u64 {
        self.address + self.size()
    }

    /// Get info for QueryMemory.
    pub fn get_memory_info(&self) -> KMemoryInfo {
        KMemoryInfo {
            address: self.address,
            size: self.size(),
            state: self.state,
            permission: self.permission,
            attribute: self.attribute,
            ipc_lock_count: self.ipc_lock_count,
            device_use_count: self.device_use_count,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_state_values() {
        // Verify key state values match zuyu's static_asserts.
        assert_eq!(KMemoryState::FREE.bits(), 0x0000_0000);
        assert_eq!(KMemoryState::CODE.bits(), 0x04DC_7E03);
        assert_eq!(KMemoryState::CODE_DATA.bits(), 0x0FFE_BD04);
        assert_eq!(KMemoryState::NORMAL.bits(), 0x077E_BD05);
        assert_eq!(KMemoryState::STACK.bits(), 0x045C_3C0B);
        assert_eq!(KMemoryState::THREAD_LOCAL.bits(), 0x0400_000C);
        assert_eq!(KMemoryState::INACCESSIBLE.bits(), 0x0000_0010);
    }

    #[test]
    fn test_svc_state_extraction() {
        assert_eq!(KMemoryState::CODE.svc_state(), 0x03);
        assert_eq!(KMemoryState::STACK.svc_state(), 0x0B);
        assert_eq!(KMemoryState::FREE.svc_state(), 0x00);
    }

    #[test]
    fn test_memory_block() {
        let block = KMemoryBlock::new(
            0x8000_0000,
            4,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ_EXECUTE,
            KMemoryAttribute::NONE,
        );
        assert_eq!(block.size(), 4 * PAGE_SIZE_U64);
        assert_eq!(block.end_address(), 0x8000_0000 + 4 * PAGE_SIZE_U64);
    }

    #[test]
    fn test_convert_permission() {
        let perm =
            convert_to_k_memory_permission(SvcMemoryPermission::READ | SvcMemoryPermission::WRITE);
        assert!(perm.contains(KMemoryPermission::USER_READ));
        assert!(perm.contains(KMemoryPermission::USER_WRITE));
        assert!(perm.contains(KMemoryPermission::KERNEL_READ));
        assert!(perm.contains(KMemoryPermission::KERNEL_WRITE));
        assert!(!perm.contains(KMemoryPermission::NOT_MAPPED));
    }

    #[test]
    fn test_convert_permission_none() {
        let perm = convert_to_k_memory_permission(SvcMemoryPermission::NONE);
        assert!(perm.contains(KMemoryPermission::NOT_MAPPED));
    }
}

//! Port of zuyu/src/core/hle/kernel/k_capabilities.h and k_capabilities.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Process capability parsing (from NPDM/KIP).
//! Parses the capability descriptors that define what SVCs, interrupts,
//! cores, priorities, and other resources a process is allowed to use.

use common::ResultCode;

/// Number of SVC IDs (0..0xFF).
const SVC_COUNT: usize = 0x100;

/// Number of interrupt IDs.
const INTERRUPT_ID_COUNT: usize = 0x400;

/// Padding interrupt ID (means "no interrupt" in a pair).
const PADDING_INTERRUPT_ID: u32 = 0x3FF;

/// Supported kernel major version (SVC major = SDK major + 4; SDK 15 -> SVC 19).
pub const SUPPORTED_KERNEL_MAJOR_VERSION: u32 = 19;

/// Supported kernel minor version (SDK 3 -> SVC 3).
pub const SUPPORTED_KERNEL_MINOR_VERSION: u32 = 3;

/// Number of virtual cores.
const NUM_VIRTUAL_CORES: u32 = 64;

/// Virtual core mask (all 64 bits set).
const VIRTUAL_CORE_MASK: u64 = u64::MAX;

/// Converts a virtual core mask to a physical core mask.
/// Virtual cores 0-3 map to physical cores 0-3, all others map to core 0
/// (except virtual core 63 which maps to physical core 3).
fn convert_virtual_core_mask_to_physical(mut v_core_mask: u64) -> u64 {
    // Simplified version matching hardware_properties.rs
    let map: [u32; 64] = {
        let mut m = [0u32; 64];
        m[1] = 1;
        m[2] = 2;
        m[3] = 3;
        m[63] = 3;
        m
    };
    let mut p_core_mask: u64 = 0;
    while v_core_mask != 0 {
        let next = v_core_mask.trailing_zeros() as usize;
        v_core_mask &= !(1u64 << next);
        if next < 64 {
            p_core_mask |= 1u64 << map[next];
        }
    }
    p_core_mask
}

/// Capability type determined by the lowest set bit pattern.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
enum CapabilityType {
    CorePriority  = (1 << 3) - 1,   // 0x07
    SyscallMask   = (1 << 4) - 1,   // 0x0F
    MapRange      = (1 << 6) - 1,   // 0x3F
    MapIoPage     = (1 << 7) - 1,   // 0x7F
    MapRegion     = (1 << 10) - 1,  // 0x3FF
    InterruptPair = (1 << 11) - 1,  // 0x7FF
    ProgramType   = (1 << 13) - 1,  // 0x1FFF
    KernelVersion = (1 << 14) - 1,  // 0x3FFF
    HandleTable   = (1 << 15) - 1,  // 0x7FFF
    DebugFlags    = (1 << 16) - 1,  // 0xFFFF

    Invalid       = 0,
    Padding       = 0xFFFF_FFFF,
}

/// Determine the capability type from a raw value.
fn get_capability_type(value: u32) -> CapabilityType {
    let type_val = (!value & value.wrapping_add(1)).wrapping_sub(1);
    match type_val {
        v if v == CapabilityType::CorePriority as u32 => CapabilityType::CorePriority,
        v if v == CapabilityType::SyscallMask as u32 => CapabilityType::SyscallMask,
        v if v == CapabilityType::MapRange as u32 => CapabilityType::MapRange,
        v if v == CapabilityType::MapIoPage as u32 => CapabilityType::MapIoPage,
        v if v == CapabilityType::MapRegion as u32 => CapabilityType::MapRegion,
        v if v == CapabilityType::InterruptPair as u32 => CapabilityType::InterruptPair,
        v if v == CapabilityType::ProgramType as u32 => CapabilityType::ProgramType,
        v if v == CapabilityType::KernelVersion as u32 => CapabilityType::KernelVersion,
        v if v == CapabilityType::HandleTable as u32 => CapabilityType::HandleTable,
        v if v == CapabilityType::DebugFlags as u32 => CapabilityType::DebugFlags,
        0xFFFF_FFFF => CapabilityType::Padding,
        0 => CapabilityType::Invalid,
        _ => CapabilityType::Invalid,
    }
}

fn get_capability_flag(cap_type: CapabilityType) -> u32 {
    (cap_type as u32).wrapping_add(1)
}

/// Flags for capabilities that can only be set once.
const INITIALIZE_ONCE_FLAGS: u32 =
    get_capability_flag_const(CapabilityType::CorePriority)
    | get_capability_flag_const(CapabilityType::ProgramType)
    | get_capability_flag_const(CapabilityType::KernelVersion)
    | get_capability_flag_const(CapabilityType::HandleTable)
    | get_capability_flag_const(CapabilityType::DebugFlags);

const fn get_capability_flag_const(cap_type: CapabilityType) -> u32 {
    (cap_type as u32).wrapping_add(1)
}

/// Extract bits [lo..lo+count) from a u32 value.
fn extract_bits(value: u32, lo: u32, count: u32) -> u32 {
    (value >> lo) & ((1u32 << count) - 1)
}

/// Parsed process capabilities.
///
/// Holds the SVC permission bitmap, interrupt permissions, core/priority masks,
/// handle table size, kernel version, and debug flags.
#[derive(Debug)]
pub struct KCapabilities {
    /// SVC access flags: svc_access_flags[id] == true means SVC `id` is permitted.
    svc_access_flags: [bool; SVC_COUNT],
    /// Interrupt access flags.
    irq_access_flags: [bool; INTERRUPT_ID_COUNT],
    /// Bitmask of allowed virtual cores.
    core_mask: u64,
    /// Bitmask of allowed physical cores.
    phys_core_mask: u64,
    /// Bitmask of allowed thread priorities.
    priority_mask: u64,
    /// Debug capabilities raw value.
    debug_capabilities: u32,
    /// Handle table size.
    handle_table_size: i32,
    /// Intended kernel version (encoded).
    intended_kernel_version: u32,
    /// Program type.
    program_type: u32,
}

impl Default for KCapabilities {
    fn default() -> Self {
        Self {
            svc_access_flags: [false; SVC_COUNT],
            irq_access_flags: [false; INTERRUPT_ID_COUNT],
            core_mask: 0,
            phys_core_mask: 0,
            priority_mask: 0,
            debug_capabilities: 0,
            handle_table_size: 0,
            intended_kernel_version: 0,
            program_type: 0,
        }
    }
}

impl KCapabilities {
    /// Create a new empty capabilities set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Initialize capabilities for a KIP (initial process).
    ///
    /// KIPs can run on all cores and use any user priority by default.
    /// The kernel version is set to the highest supported version.
    pub fn initialize_for_kip(&mut self, kern_caps: &[u32]) -> Result<(), ResultCode> {
        self.svc_access_flags = [false; SVC_COUNT];
        self.irq_access_flags = [false; INTERRUPT_ID_COUNT];
        self.debug_capabilities = 0;
        self.handle_table_size = 0;
        self.intended_kernel_version = 0;
        self.program_type = 0;

        // Initial processes may run on all cores.
        self.core_mask = VIRTUAL_CORE_MASK;
        self.phys_core_mask = convert_virtual_core_mask_to_physical(VIRTUAL_CORE_MASK);

        // Initial processes may use any user priority (exclude kernel priorities 0-3).
        self.priority_mask = !0xFu64;

        // Set kernel version to highest supported.
        self.intended_kernel_version = encode_kernel_version(
            SUPPORTED_KERNEL_MAJOR_VERSION,
            SUPPORTED_KERNEL_MINOR_VERSION,
        );

        self.set_capabilities(kern_caps)
    }

    /// Initialize capabilities for a user process.
    ///
    /// User processes must specify cores/priorities via capability descriptors.
    pub fn initialize_for_user(&mut self, user_caps: &[u32]) -> Result<(), ResultCode> {
        self.svc_access_flags = [false; SVC_COUNT];
        self.irq_access_flags = [false; INTERRUPT_ID_COUNT];
        self.debug_capabilities = 0;
        self.handle_table_size = 0;
        self.intended_kernel_version = 0;
        self.program_type = 0;

        self.core_mask = 0;
        self.priority_mask = 0;

        self.set_capabilities(user_caps)
    }

    // -- Accessors --

    pub fn core_mask(&self) -> u64 {
        self.core_mask
    }

    pub fn phys_core_mask(&self) -> u64 {
        self.phys_core_mask
    }

    pub fn priority_mask(&self) -> u64 {
        self.priority_mask
    }

    pub fn handle_table_size(&self) -> i32 {
        self.handle_table_size
    }

    pub fn svc_access_flags(&self) -> &[bool; SVC_COUNT] {
        &self.svc_access_flags
    }

    pub fn is_permitted_svc(&self, id: u32) -> bool {
        (id as usize) < SVC_COUNT && self.svc_access_flags[id as usize]
    }

    pub fn is_permitted_interrupt(&self, id: u32) -> bool {
        (id as usize) < INTERRUPT_ID_COUNT && self.irq_access_flags[id as usize]
    }

    pub fn is_permitted_debug(&self) -> bool {
        // allow_debug is bit 17
        (self.debug_capabilities >> 17) & 1 != 0
    }

    pub fn can_force_debug(&self) -> bool {
        // force_debug is bit 18
        (self.debug_capabilities >> 18) & 1 != 0
    }

    pub fn intended_kernel_major_version(&self) -> u32 {
        get_kernel_major_version(self.intended_kernel_version)
    }

    pub fn intended_kernel_minor_version(&self) -> u32 {
        get_kernel_minor_version(self.intended_kernel_version)
    }

    pub fn program_type(&self) -> u32 {
        self.program_type
    }

    // -- Private capability parsing --

    fn set_core_priority_capability(&mut self, cap: u32) -> Result<(), ResultCode> {
        // Can't set core/priority if already set.
        if self.core_mask != 0 || self.priority_mask != 0 {
            return Err(ResultCode(0xCA01)); // ResultInvalidArgument
        }

        let lowest_thread_priority = extract_bits(cap, 4, 6);
        let highest_thread_priority = extract_bits(cap, 10, 6);
        let minimum_core_id = extract_bits(cap, 16, 8);
        let maximum_core_id = extract_bits(cap, 24, 8);

        if minimum_core_id > maximum_core_id {
            return Err(ResultCode(0xE401)); // ResultInvalidCombination
        }
        if highest_thread_priority > lowest_thread_priority {
            return Err(ResultCode(0xE401));
        }
        if maximum_core_id >= NUM_VIRTUAL_CORES {
            return Err(ResultCode(0x8C01)); // ResultInvalidCoreId
        }

        // Set core mask.
        for core_id in minimum_core_id..=maximum_core_id {
            self.core_mask |= 1u64 << core_id;
        }

        // Set physical core mask.
        self.phys_core_mask = convert_virtual_core_mask_to_physical(self.core_mask);

        // Set priority mask.
        for prio in highest_thread_priority..=lowest_thread_priority {
            self.priority_mask |= 1u64 << prio;
        }

        if self.core_mask == 0 || self.priority_mask == 0 {
            return Err(ResultCode(0xCA01));
        }

        // Processes must not have access to kernel thread priorities (0-3).
        if (self.priority_mask & 0xF) != 0 {
            return Err(ResultCode(0xCA01));
        }

        Ok(())
    }

    fn set_syscall_mask_capability(&mut self, cap: u32, set_svc: &mut u32) -> Result<(), ResultCode> {
        let mask = extract_bits(cap, 5, 24);
        let index = extract_bits(cap, 29, 3);

        let index_flag = 1u32 << index;
        if (*set_svc & index_flag) != 0 {
            return Err(ResultCode(0xE401)); // ResultInvalidCombination
        }
        *set_svc |= index_flag;

        // Set SVCs.
        for i in 0..24u32 {
            let svc_id = 24 * index + i;
            if (mask & (1u32 << i)) != 0 {
                if (svc_id as usize) < SVC_COUNT {
                    self.svc_access_flags[svc_id as usize] = true;
                } else {
                    return Err(ResultCode(0xD001)); // ResultOutOfRange
                }
            }
        }

        Ok(())
    }

    fn set_interrupt_pair_capability(&mut self, cap: u32) -> Result<(), ResultCode> {
        let id0 = extract_bits(cap, 12, 10);
        let id1 = extract_bits(cap, 22, 10);

        for &id in &[id0, id1] {
            if id != PADDING_INTERRUPT_ID {
                if (id as usize) < INTERRUPT_ID_COUNT {
                    self.irq_access_flags[id as usize] = true;
                } else {
                    return Err(ResultCode(0xD001)); // ResultOutOfRange
                }
            }
        }

        Ok(())
    }

    fn set_program_type_capability(&mut self, cap: u32) -> Result<(), ResultCode> {
        let program_type = extract_bits(cap, 14, 3);
        let reserved = extract_bits(cap, 17, 15);
        if reserved != 0 {
            return Err(ResultCode(0xEE01)); // ResultReservedUsed
        }
        self.program_type = program_type;
        Ok(())
    }

    fn set_kernel_version_capability(&mut self, cap: u32) -> Result<(), ResultCode> {
        // Ensure we haven't set version before.
        if get_kernel_major_version(self.intended_kernel_version) != 0 {
            return Err(ResultCode(0xCA01));
        }

        self.intended_kernel_version = cap;

        if get_kernel_major_version(self.intended_kernel_version) == 0 {
            return Err(ResultCode(0xCA01));
        }

        Ok(())
    }

    fn set_handle_table_capability(&mut self, cap: u32) -> Result<(), ResultCode> {
        let size = extract_bits(cap, 16, 10);
        let reserved = extract_bits(cap, 26, 6);
        if reserved != 0 {
            return Err(ResultCode(0xEE01));
        }
        self.handle_table_size = size as i32;
        Ok(())
    }

    fn set_debug_flags_capability(&mut self, cap: u32) -> Result<(), ResultCode> {
        let allow_debug = extract_bits(cap, 17, 1);
        let force_debug = extract_bits(cap, 18, 1);
        let reserved = extract_bits(cap, 19, 13);
        if reserved != 0 {
            return Err(ResultCode(0xEE01));
        }

        // Build the debug capabilities value with the same bit layout.
        self.debug_capabilities = (allow_debug << 17) | (force_debug << 18);
        Ok(())
    }

    fn set_capability(&mut self, cap: u32, set_flags: &mut u32, set_svc: &mut u32) -> Result<(), ResultCode> {
        let cap_type = get_capability_type(cap);

        if cap_type == CapabilityType::Invalid {
            return Err(ResultCode(0xCA01));
        }

        if cap_type == CapabilityType::Padding {
            return Ok(());
        }

        // Check that we haven't already processed initialize-once capabilities.
        let flag = get_capability_flag(cap_type);
        if (*set_flags & INITIALIZE_ONCE_FLAGS) & flag != 0 {
            return Err(ResultCode(0xE401));
        }
        *set_flags |= flag;

        match cap_type {
            CapabilityType::CorePriority => self.set_core_priority_capability(cap),
            CapabilityType::SyscallMask => self.set_syscall_mask_capability(cap, set_svc),
            CapabilityType::MapRange => {
                // MapRange is handled specially in set_capabilities (paired entries).
                // If we get here alone, just skip (the pairing code handles it).
                Ok(())
            }
            CapabilityType::MapIoPage => {
                // We don't emulate physical I/O mapping, skip.
                log::debug!("KCapabilities: ignoring MapIoPage capability");
                Ok(())
            }
            CapabilityType::MapRegion => {
                // We don't emulate kernel memory region mapping, skip.
                log::debug!("KCapabilities: ignoring MapRegion capability");
                Ok(())
            }
            CapabilityType::InterruptPair => self.set_interrupt_pair_capability(cap),
            CapabilityType::ProgramType => self.set_program_type_capability(cap),
            CapabilityType::KernelVersion => self.set_kernel_version_capability(cap),
            CapabilityType::HandleTable => self.set_handle_table_capability(cap),
            CapabilityType::DebugFlags => self.set_debug_flags_capability(cap),
            _ => Err(ResultCode(0xCA01)),
        }
    }

    fn set_capabilities(&mut self, caps: &[u32]) -> Result<(), ResultCode> {
        let mut set_flags: u32 = 0;
        let mut set_svc: u32 = 0;

        let mut i = 0;
        while i < caps.len() {
            let cap = caps[i];

            if get_capability_type(cap) == CapabilityType::MapRange {
                // MapRange comes in pairs: the range descriptor and the size descriptor.
                i += 1;
                if i >= caps.len() {
                    return Err(ResultCode(0xE401));
                }
                let size_cap = caps[i];
                if get_capability_type(size_cap) != CapabilityType::MapRange {
                    return Err(ResultCode(0xE401));
                }
                // We don't emulate physical memory mapping, skip the pair.
                log::debug!("KCapabilities: ignoring MapRange pair");
            } else {
                self.set_capability(cap, &mut set_flags, &mut set_svc)?;
            }

            i += 1;
        }

        Ok(())
    }
}

// -- Kernel version encoding/decoding helpers --

/// Encode a kernel version from major and minor components.
pub fn encode_kernel_version(major: u32, minor: u32) -> u32 {
    (minor & 0xF) | ((major & 0x1FFF) << 4)
}

/// Extract the major version from an encoded kernel version.
pub fn get_kernel_major_version(encoded: u32) -> u32 {
    (encoded >> 4) & 0x1FFF
}

/// Extract the minor version from an encoded kernel version.
pub fn get_kernel_minor_version(encoded: u32) -> u32 {
    encoded & 0xF
}

/// Convert SDK major version to SVC major version.
pub fn convert_to_svc_major_version(sdk: u32) -> u32 {
    sdk + 4
}

/// Convert SVC major version to SDK major version.
pub fn convert_to_sdk_major_version(svc: u32) -> u32 {
    svc - 4
}

/// Required minimum kernel version (3.0).
pub const REQUIRED_KERNEL_MAJOR_VERSION: u32 = 3;
pub const REQUIRED_KERNEL_MINOR_VERSION: u32 = 0;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kernel_version_encoding() {
        let encoded = encode_kernel_version(19, 3);
        assert_eq!(get_kernel_major_version(encoded), 19);
        assert_eq!(get_kernel_minor_version(encoded), 3);
    }

    #[test]
    fn test_capability_type_detection() {
        // Padding: all bits set
        assert_eq!(get_capability_type(0xFFFF_FFFF), CapabilityType::Padding);

        // CorePriority: lowest 4 bits are 0b0111 (type mask 0x07, id flag 0x08)
        // Value with bits [0..3] = 0b0111 means type = CorePriority
        // (~0b0111 & (0b0111 + 1)) - 1 = (0xFFFFFFF8 & 0x8) - 1 = 8 - 1 = 7 = CorePriority
        let core_prio_cap: u32 = 0b0000_0000_0000_0000_0000_0000_0000_0111;
        // Hmm, the pattern is: (!v & (v+1)) - 1
        // For CorePriority (type = 7): the raw value must have lowest bits ...0111
        // (!0b0111 & 0b1000) - 1 = (0b1000) - 1 = 0b0111 = 7
        assert_eq!(get_capability_type(core_prio_cap), CapabilityType::CorePriority);
    }

    #[test]
    fn test_initialize_for_kip() {
        let mut caps = KCapabilities::new();
        caps.initialize_for_kip(&[]).unwrap();
        assert_eq!(caps.core_mask, VIRTUAL_CORE_MASK);
        assert_ne!(caps.priority_mask, 0);
        // Kernel priorities (0-3) should be excluded.
        assert_eq!(caps.priority_mask & 0xF, 0);
        assert_eq!(caps.intended_kernel_major_version(), SUPPORTED_KERNEL_MAJOR_VERSION);
    }

    #[test]
    fn test_initialize_for_user_empty() {
        let mut caps = KCapabilities::new();
        caps.initialize_for_user(&[]).unwrap();
        assert_eq!(caps.core_mask, 0);
        assert_eq!(caps.priority_mask, 0);
        assert_eq!(caps.handle_table_size, 0);
    }

    #[test]
    fn test_syscall_mask() {
        let mut caps = KCapabilities::new();
        // Build a syscall mask capability:
        // Type = SyscallMask (0x0F), id bits [0..4] = 0b01111
        // mask bits [5..28] = set bit 0 (SVC 0 in group 0)
        // index bits [29..31] = 0
        // So: 0b00000_000000000000000000000001_01111 = 0x2F
        let svc_cap = (1u32 << 5) | 0x0F; // mask=1, index=0, type=SyscallMask
        caps.initialize_for_kip(&[svc_cap]).unwrap();
        // SVC 0 should now be permitted.
        assert!(caps.is_permitted_svc(0));
        assert!(!caps.is_permitted_svc(1));
    }
}

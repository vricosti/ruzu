//! Port of zuyu/src/core/hle/kernel/k_capabilities.h / k_capabilities.cpp
//! Status: Partial (structural port, parsing methods stubbed)
//! Derniere synchro: 2026-03-11
//!
//! KCapabilities: parses and stores process capability descriptors
//! (core mask, priority mask, SVC permissions, interrupts, etc.)

/// Number of possible interrupt IDs.
pub const INTERRUPT_ID_COUNT: usize = 0x400;

/// Number of SVC access flags (matching Svc::SvcAccessFlagSet size).
pub const SVC_ACCESS_FLAG_COUNT: usize = 0xC0;

/// Capability type discriminants.
/// Matches upstream `KCapabilities::CapabilityType` (k_capabilities.h).
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapabilityType {
    CorePriority = (1 << 3) - 1,  // 0x07
    SyscallMask = (1 << 4) - 1,   // 0x0F
    MapRange = (1 << 6) - 1,      // 0x3F
    MapIoPage = (1 << 7) - 1,     // 0x7F
    MapRegion = (1 << 10) - 1,    // 0x3FF
    InterruptPair = (1 << 11) - 1, // 0x7FF
    ProgramType = (1 << 13) - 1,  // 0x1FFF
    KernelVersion = (1 << 14) - 1, // 0x3FFF
    HandleTable = (1 << 15) - 1,  // 0x7FFF
    DebugFlags = (1 << 16) - 1,   // 0xFFFF
    Invalid = 0,
    Padding = 0xFFFF_FFFF,
}

/// Region type for MapRegion capability.
/// Matches upstream `KCapabilities::RegionType` (k_capabilities.h).
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionType {
    NoMapping = 0,
    KernelTraceBuffer = 1,
    OnMemoryBootImage = 2,
    Dtb = 3,
}

/// Padding interrupt ID (0x3FF).
pub const PADDING_INTERRUPT_ID: u32 = 0x3FF;

/// Physical map allowed mask.
pub const PHYSICAL_MAP_ALLOWED_MASK: u64 = (1u64 << 36) - 1;

/// Initialize-once flag set.
pub const INITIALIZE_ONCE_FLAGS: u32 = (1 << 3) | (1 << 13) | (1 << 14) | (1 << 15) | (1 << 16);

/// Determine the capability type from a raw value.
/// Matches upstream `GetCapabilityType`.
pub fn get_capability_type(value: u32) -> CapabilityType {
    let flag = (!value) & (value.wrapping_add(1));
    let type_val = flag.wrapping_sub(1);
    // Match known types
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
        _ => CapabilityType::Invalid,
    }
}

// ---------------------------------------------------------------------------
// KCapabilities
// ---------------------------------------------------------------------------

/// Process capabilities.
/// Matches upstream `KCapabilities` class (k_capabilities.h).
pub struct KCapabilities {
    pub svc_access_flags: [bool; SVC_ACCESS_FLAG_COUNT],
    pub irq_access_flags: [bool; INTERRUPT_ID_COUNT],
    pub core_mask: u64,
    pub phys_core_mask: u64,
    pub priority_mask: u64,
    pub debug_capabilities: u32,
    pub handle_table_size: i32,
    pub intended_kernel_version: u32,
    pub program_type: u32,
}

impl KCapabilities {
    pub fn new() -> Self {
        Self {
            svc_access_flags: [false; SVC_ACCESS_FLAG_COUNT],
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

    pub fn get_core_mask(&self) -> u64 {
        self.core_mask
    }

    pub fn get_physical_core_mask(&self) -> u64 {
        self.phys_core_mask
    }

    pub fn get_priority_mask(&self) -> u64 {
        self.priority_mask
    }

    pub fn get_handle_table_size(&self) -> i32 {
        self.handle_table_size
    }

    pub fn is_permitted_svc(&self, id: u32) -> bool {
        (id as usize) < SVC_ACCESS_FLAG_COUNT && self.svc_access_flags[id as usize]
    }

    pub fn is_permitted_interrupt(&self, id: u32) -> bool {
        (id as usize) < INTERRUPT_ID_COUNT && self.irq_access_flags[id as usize]
    }

    pub fn is_permitted_debug(&self) -> bool {
        // DebugFlags.allow_debug is bit 17
        (self.debug_capabilities >> 17) & 1 != 0
    }

    pub fn can_force_debug(&self) -> bool {
        // DebugFlags.force_debug is bit 18
        (self.debug_capabilities >> 18) & 1 != 0
    }

    pub fn get_intended_kernel_major_version(&self) -> u32 {
        // KernelVersion.major_version is bits [19..31]
        (self.intended_kernel_version >> 19) & 0x1FFF
    }

    pub fn get_intended_kernel_minor_version(&self) -> u32 {
        // KernelVersion.minor_version is bits [15..18]
        (self.intended_kernel_version >> 15) & 0xF
    }

    /// Initialize capabilities for a KIP (kernel initial process).
    /// TODO: Port from k_capabilities.cpp.
    pub fn initialize_for_kip(&mut self, _kern_caps: &[u32]) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Initialize capabilities for a user process.
    /// Matches upstream `KCapabilities::InitializeForUser`.
    pub fn initialize_for_user(&mut self, user_caps: &[u32]) -> u32 {
        self.core_mask = 0;
        self.phys_core_mask = 0;
        self.priority_mask = 0;
        self.intended_kernel_version = 0;
        self.program_type = 0;

        self.set_capabilities(user_caps)
    }

    /// Parse a capabilities array.
    /// Matches upstream `KCapabilities::SetCapabilities`.
    fn set_capabilities(&mut self, caps: &[u32]) -> u32 {
        let mut set_flags = 0u32;
        let mut set_svc = 0u32;
        let mut i = 0;

        while i < caps.len() {
            let cap = caps[i];
            let cap_type = get_capability_type(cap);

            if cap_type == CapabilityType::MapRange {
                // MapRange is a pair — skip both entries.
                i += 2;
                continue;
            }

            if cap_type == CapabilityType::Padding {
                i += 1;
                continue;
            }

            if cap_type == CapabilityType::Invalid {
                log::warn!("KCapabilities: invalid capability {:#x} at index {}", cap, i);
                i += 1;
                continue;
            }

            // Process the capability.
            match cap_type {
                CapabilityType::CorePriority => {
                    self.set_core_priority_capability(cap);
                }
                CapabilityType::SyscallMask => {
                    self.set_syscall_mask_capability(cap, &mut set_svc);
                }
                CapabilityType::HandleTable => {
                    // HandleTable: bits [26..16] = size
                    let size = ((cap >> 16) & 0x3FF) as i32;
                    self.handle_table_size = size;
                }
                CapabilityType::ProgramType => {
                    // ProgramType: bits [17..14] = type
                    let ptype = (cap >> 14) & 0x7;
                    self.program_type = ptype;
                }
                CapabilityType::KernelVersion => {
                    self.intended_kernel_version = cap;
                }
                CapabilityType::DebugFlags => {
                    self.debug_capabilities = cap;
                }
                _ => {
                    // MapIoPage, MapRegion, InterruptPair — skip for now
                }
            }

            i += 1;
        }

        0 // success
    }

    /// Parse CorePriority capability.
    /// Matches upstream `KCapabilities::SetCorePriorityCapability`.
    fn set_core_priority_capability(&mut self, cap: u32) {
        // CorePriority pack layout (upstream BitField in k_capabilities.h):
        // bits [3..0]   = type id (CorePriority = 0x7)
        // bits [9..4]   = lowest_thread_priority (6 bits, max numerical value)
        // bits [15..10] = highest_thread_priority (6 bits, min numerical value)
        // bits [23..16] = minimum_core_id (8 bits)
        // bits [31..24] = maximum_core_id (8 bits)
        let max_prio = ((cap >> 4) & 0x3F) as u32;  // lowest_thread_priority
        let min_prio = ((cap >> 10) & 0x3F) as u32; // highest_thread_priority
        let min_core = ((cap >> 16) & 0xFF) as u32;
        let max_core = ((cap >> 24) & 0xFF) as u32;

        log::debug!(
            "KCapabilities::SetCorePriorityCapability: cores=[{}..{}], priority=[{}..{}]",
            min_core, max_core, min_prio, max_prio
        );

        if min_core > max_core || min_prio > max_prio {
            log::warn!("KCapabilities: invalid CorePriority cap {:#x}", cap);
            return;
        }

        // Set core mask.
        for core_id in min_core..=max_core {
            self.core_mask |= 1u64 << core_id;
        }
        // Physical core mask = same as virtual for now.
        self.phys_core_mask = self.core_mask;

        // Set priority mask.
        for prio in min_prio..=max_prio {
            if prio < 64 {
                self.priority_mask |= 1u64 << prio;
            }
        }
    }

    /// Parse SyscallMask capability.
    /// Matches upstream `KCapabilities::SetSyscallMaskCapability`.
    fn set_syscall_mask_capability(&mut self, cap: u32, set_svc: &mut u32) {
        // SyscallMask layout:
        // bits [4..0]   = type (0xF -> SyscallMask after decode)
        // bits [28..5]  = mask (24 bits)
        // bits [31..29] = index (which group of 24 SVCs)
        let mask = (cap >> 5) & 0x00FF_FFFF;
        let index = (cap >> 29) & 0x7;

        for bit in 0..24u32 {
            if mask & (1 << bit) != 0 {
                let svc_id = index * 24 + bit;
                self.set_svc_allowed(svc_id);
            }
        }
    }

    /// Set an SVC as allowed.
    fn set_svc_allowed(&mut self, id: u32) -> bool {
        if (id as usize) < SVC_ACCESS_FLAG_COUNT {
            self.svc_access_flags[id as usize] = true;
            true
        } else {
            false
        }
    }

    /// Set an interrupt as permitted.
    fn set_interrupt_permitted(&mut self, id: u32) -> bool {
        if (id as usize) < INTERRUPT_ID_COUNT {
            self.irq_access_flags[id as usize] = true;
            true
        } else {
            false
        }
    }
}

impl Default for KCapabilities {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_capability_type_values() {
        assert_eq!(CapabilityType::CorePriority as u32, 0x07);
        assert_eq!(CapabilityType::SyscallMask as u32, 0x0F);
        assert_eq!(CapabilityType::HandleTable as u32, 0x7FFF);
        assert_eq!(CapabilityType::DebugFlags as u32, 0xFFFF);
    }

    #[test]
    fn test_get_capability_type() {
        // CorePriority: flag bit 3, so value with lowest 3 bits set + flag bit
        // A value like 0b...XXX0111 -> type bits = 0b0111 = 7
        assert_eq!(
            get_capability_type(0xFFFF_FFFF),
            CapabilityType::Padding
        );
    }
}

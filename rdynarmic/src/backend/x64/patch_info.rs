use crate::ir::location::LocationDescriptor;
use std::collections::HashMap;

/// A64 patch slot sizes (matching upstream dynarmic `a64_emit_x64.cpp`).
pub const A64_PATCH_JG_SIZE: usize = 23;
pub const A64_PATCH_JZ_SIZE: usize = 23;
pub const A64_PATCH_JMP_SIZE: usize = 22;

/// A32 patch slot sizes (matching upstream dynarmic `a32_emit_x64.cpp`).
pub const A32_PATCH_JG_SIZE: usize = 14;
pub const A32_PATCH_JZ_SIZE: usize = 14;
pub const A32_PATCH_JMP_SIZE: usize = 13;

/// Type of patch at a link site.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatchType {
    /// Conditional jump on cycles > 0 (jg rel32).
    Jg,
    /// Conditional jump on halt == 0 (jz rel32).
    Jz,
    /// Unconditional jump (jmp rel32).
    Jmp,
    /// MOV RCX, imm64 for RSB code pointer.
    MovRcx,
}

/// A single patch entry recorded during block emission.
#[derive(Debug, Clone)]
pub struct PatchEntry {
    /// Target location this patch links to.
    pub target: LocationDescriptor,
    /// Type of patch (determines slot size).
    pub patch_type: PatchType,
    /// Code buffer offset where the patch slot begins.
    pub code_offset: usize,
}

/// Collected patch slots for a single target location.
#[derive(Debug, Clone, Default)]
pub struct PatchInformation {
    /// Code buffer offsets of jg patch slots.
    pub jg: Vec<usize>,
    /// Code buffer offsets of jz patch slots.
    pub jz: Vec<usize>,
    /// Code buffer offsets of jmp patch slots.
    pub jmp: Vec<usize>,
    /// Code buffer offsets of mov rcx patch slots (RSB).
    pub mov_rcx: Vec<usize>,
}

/// Mapping from target LocationDescriptor to all patch slots pointing at it.
pub type PatchTable = HashMap<LocationDescriptor, PatchInformation>;

impl PatchType {
    /// A64 slot size in bytes for this patch type.
    pub fn a64_slot_size(self) -> usize {
        match self {
            PatchType::Jg => A64_PATCH_JG_SIZE,
            PatchType::Jz => A64_PATCH_JZ_SIZE,
            PatchType::Jmp => A64_PATCH_JMP_SIZE,
            PatchType::MovRcx => 10, // mov rcx, imm64
        }
    }

    /// A32 slot size in bytes for this patch type.
    pub fn a32_slot_size(self) -> usize {
        match self {
            PatchType::Jg => A32_PATCH_JG_SIZE,
            PatchType::Jz => A32_PATCH_JZ_SIZE,
            PatchType::Jmp => A32_PATCH_JMP_SIZE,
            PatchType::MovRcx => 10, // mov rcx, imm64
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_patch_type_slot_sizes() {
        assert_eq!(PatchType::Jg.a64_slot_size(), 23);
        assert_eq!(PatchType::Jz.a64_slot_size(), 23);
        assert_eq!(PatchType::Jmp.a64_slot_size(), 22);
        assert_eq!(PatchType::Jg.a32_slot_size(), 14);
        assert_eq!(PatchType::Jz.a32_slot_size(), 14);
        assert_eq!(PatchType::Jmp.a32_slot_size(), 13);
        assert_eq!(PatchType::MovRcx.a64_slot_size(), 10);
        assert_eq!(PatchType::MovRcx.a32_slot_size(), 10);
    }

    #[test]
    fn test_patch_information_default() {
        let info = PatchInformation::default();
        assert!(info.jg.is_empty());
        assert!(info.jz.is_empty());
        assert!(info.jmp.is_empty());
        assert!(info.mov_rcx.is_empty());
    }

    #[test]
    fn test_patch_table() {
        let mut table: PatchTable = HashMap::new();
        let loc = LocationDescriptor::new(0x1000);
        let entry = table.entry(loc).or_default();
        entry.jmp.push(100);
        entry.jmp.push(200);
        assert_eq!(table[&loc].jmp.len(), 2);
    }
}

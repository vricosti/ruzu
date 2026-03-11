// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/nce/patcher.h and patcher.cpp
//! NCE instruction patcher for intercepting SVCs, MRS/MSR, and exclusive
//! memory operations in guest code.
//!
//! The patcher scans guest .text segments for instructions that need
//! interception (SVC, MRS/MSR of system registers, exclusive ops) and
//! generates patch code that saves/restores context and branches to/from
//! the host.
//!
//! NOTE: The upstream implementation uses the oaknut library (ARM64 code
//! generator) to emit patch instructions. This Rust port provides the
//! structural skeleton but the actual code generation is stubbed, as it
//! requires an ARM64 assembler library.

use std::collections::HashMap;

use super::instructions::{Svc, Mrs, Msr, Exclusive, SystemRegister};

/// Maximum relative branch distance for ARM64 B instruction (128 MiB).
const MAX_RELATIVE_BRANCH: usize = 128 * 1024 * 1024;

/// Index of the first patchable instruction in the module text.
/// Corresponds to upstream `ModuleCodeIndex` (0x24 / sizeof(u32)).
const MODULE_CODE_INDEX: u32 = 0x24 / 4;

/// Patch mode determining where the patch section is placed.
///
/// Corresponds to upstream `Core::NCE::PatchMode`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PatchMode {
    None,
    /// Patch section is inserted before .text.
    PreText,
    /// Patch section is inserted after .data.
    PostData,
}

impl Default for PatchMode {
    fn default() -> Self {
        Self::None
    }
}

/// Module text address (offset within the module).
pub type ModuleTextAddress = u64;

/// Patch text address (offset within the patch section).
pub type PatchTextAddress = u64;

/// Map from module text addresses to patch trampoline addresses.
///
/// Corresponds to upstream `Core::NCE::EntryTrampolines`.
pub type EntryTrampolines = HashMap<ModuleTextAddress, PatchTextAddress>;

/// Relocation entry linking patch and module offsets.
///
/// Corresponds to upstream `Core::NCE::Patcher::Relocation`.
#[derive(Debug, Clone)]
struct Relocation {
    /// Offset in bytes from the start of the patch section.
    patch_offset: usize,
    /// Offset in bytes from the start of the text section.
    module_offset: usize,
}

/// Trampoline entry for post-SVC return.
///
/// Corresponds to upstream `Core::NCE::Patcher::Trampoline`.
#[derive(Debug, Clone)]
struct Trampoline {
    patch_offset: usize,
    module_offset: usize,
}

/// Per-module patch data.
///
/// Corresponds to upstream `Core::NCE::Patcher::ModulePatch`.
#[derive(Debug, Clone, Default)]
struct ModulePatch {
    trampolines: Vec<Trampoline>,
    branch_to_patch_relocations: Vec<Relocation>,
    branch_to_module_relocations: Vec<Relocation>,
    write_module_pc_relocations: Vec<Relocation>,
    exclusives: Vec<ModuleTextAddress>,
}

/// NCE instruction patcher.
///
/// Corresponds to upstream `Core::NCE::Patcher`.
///
/// Scans guest ARM64 code for instructions that need interception and
/// generates trampoline/patch code for each.
pub struct Patcher {
    /// Generated patch instructions.
    patch_instructions: Vec<u32>,

    /// Patch mode (where to place the patch section).
    mode: PatchMode,

    /// Total size of all program images processed.
    total_program_size: usize,

    /// Index of the next module to relocate.
    relocate_module_index: usize,

    /// Per-module patch data.
    modules: Vec<ModulePatch>,
}

impl Patcher {
    /// Create a new patcher.
    ///
    /// Corresponds to upstream `Patcher::Patcher()`.
    /// Upstream emits the initial branch instruction and save/load context helpers.
    pub fn new() -> Self {
        let mut patcher = Self {
            patch_instructions: Vec::new(),
            mode: PatchMode::None,
            total_program_size: 0,
            relocate_module_index: 0,
            modules: Vec::new(),
        };

        // The first word of the patch section is always a branch to the first
        // instruction of the module (placeholder, filled in during relocation).
        patcher.patch_instructions.push(0);

        // TODO: Emit save_context and load_context helper functions.
        // Upstream uses oaknut to generate ARM64 assembly for:
        //   - WriteSaveContext(): saves all guest registers to GuestContext
        //   - WriteLoadContext(): restores all guest registers from GuestContext
        // These are called by the SVC trampoline.

        patcher
    }

    /// Scan and patch a text segment.
    ///
    /// Returns `false` if the module cannot be reached from existing patches
    /// (would need its own patcher).
    ///
    /// Corresponds to upstream `Patcher::PatchText(PhysicalMemory&, CodeSet::Segment&)`.
    pub fn patch_text(&mut self, program_image: &[u8], code_offset: usize, code_size: usize) -> bool {
        let image_size = program_image.len();
        if self.total_program_size + image_size > MAX_RELATIVE_BRANCH && self.total_program_size > 0 {
            return false;
        }

        // Add a new module patch.
        let mut module_patch = ModulePatch::default();

        // The first word relocates to the module start.
        module_patch.branch_to_module_relocations.push(Relocation {
            patch_offset: 0,
            module_offset: 0,
        });

        // Read text segment as u32 words.
        let text = &program_image[code_offset..code_offset + code_size];
        let text_words: Vec<u32> = text
            .chunks_exact(4)
            .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]]))
            .collect();

        // Scan instructions.
        for i in (MODULE_CODE_INDEX as usize)..text_words.len() {
            let inst = text_words[i];
            let this_offset = i * 4;
            let _next_offset = this_offset + 4;

            // SVC
            let svc = Svc::new(inst);
            if svc.verify() {
                module_patch.branch_to_patch_relocations.push(Relocation {
                    patch_offset: self.patch_instructions.len() * 4,
                    module_offset: this_offset,
                });
                // TODO: self.write_svc_trampoline(next_offset, svc.get_value());
                log::trace!("Patcher: SVC #{} at offset {:#x}", svc.get_value(), this_offset);
                continue;
            }

            // MRS Xn, TPIDR_EL0 / TPIDRRO_EL0
            let mrs = Mrs::new(inst);
            if mrs.verify()
                && (mrs.get_system_reg() == SystemRegister::TpidrroEl0 as u32
                    || mrs.get_system_reg() == SystemRegister::TpidrEl0 as u32)
            {
                module_patch.branch_to_patch_relocations.push(Relocation {
                    patch_offset: self.patch_instructions.len() * 4,
                    module_offset: this_offset,
                });
                // TODO: self.write_mrs_handler(next_offset, dest_reg, src_reg);
                log::trace!("Patcher: MRS TPIDR at offset {:#x}", this_offset);
                continue;
            }

            // MRS Xn, CNTPCT_EL0
            if mrs.verify() && mrs.get_system_reg() == SystemRegister::CntpctEl0 as u32 {
                module_patch.branch_to_patch_relocations.push(Relocation {
                    patch_offset: self.patch_instructions.len() * 4,
                    module_offset: this_offset,
                });
                // TODO: self.write_cntpct_handler(next_offset, dest_reg);
                log::trace!("Patcher: MRS CNTPCT at offset {:#x}", this_offset);
                continue;
            }

            // MRS Xn, CNTFRQ_EL0
            if mrs.verify() && mrs.get_system_reg() == SystemRegister::CntfrqEl0 as u32 {
                panic!("CNTFRQ_EL0 access not supported");
            }

            // MSR TPIDR_EL0, Xn
            let msr = Msr::new(inst);
            if msr.verify() && msr.get_system_reg() == SystemRegister::TpidrEl0 as u32 {
                module_patch.branch_to_patch_relocations.push(Relocation {
                    patch_offset: self.patch_instructions.len() * 4,
                    module_offset: this_offset,
                });
                // TODO: self.write_msr_handler(next_offset, src_reg);
                log::trace!("Patcher: MSR TPIDR at offset {:#x}", this_offset);
                continue;
            }

            // Exclusive load/store
            let exclusive = Exclusive::new(inst);
            if exclusive.verify() {
                module_patch.exclusives.push(i as u64);
            }
        }

        // Determine patching mode.
        self.total_program_size += image_size;
        self.mode = if image_size > MAX_RELATIVE_BRANCH {
            PatchMode::PreText
        } else {
            PatchMode::PostData
        };

        self.modules.push(module_patch);
        true
    }

    /// Get the size of the patch section (page-aligned).
    ///
    /// Corresponds to upstream `Patcher::GetSectionSize()`.
    pub fn get_section_size(&self) -> usize {
        let raw_size = self.patch_instructions.len() * 4;
        // Align to page size (4096).
        (raw_size + 0xFFF) & !0xFFF
    }

    /// Get the current patch mode.
    ///
    /// Corresponds to upstream `Patcher::GetPatchMode()`.
    pub fn get_patch_mode(&self) -> PatchMode {
        self.mode
    }

    /// Relocate and copy patch instructions into the program image.
    ///
    /// Corresponds to upstream `Patcher::RelocateAndCopy(...)`.
    ///
    /// NOTE: This is a structural stub. The actual relocation requires
    /// generating ARM64 branch instructions with correct offsets.
    pub fn relocate_and_copy(
        &mut self,
        _load_base: u64,
        _code_offset: usize,
        _code_size: usize,
        program_image: &mut Vec<u8>,
        _out_trampolines: &mut EntryTrampolines,
    ) -> bool {
        if self.relocate_module_index >= self.modules.len() {
            return false;
        }

        let patch_size = self.get_section_size();
        let image_size = program_image.len();
        let module_idx = self.relocate_module_index;
        self.relocate_module_index += 1;

        // TODO: Apply relocations (branch instructions between module and patch).
        // This requires writing ARM64 branch instructions at the relocation offsets.

        // Convert exclusive instructions to ordered variants.
        let text_start = _code_offset;
        let text_end = _code_offset + _code_size;
        if text_end <= program_image.len() {
            for &excl_idx in &self.modules[module_idx].exclusives {
                let byte_offset = text_start + (excl_idx as usize) * 4;
                if byte_offset + 4 <= program_image.len() {
                    let inst = u32::from_le_bytes([
                        program_image[byte_offset],
                        program_image[byte_offset + 1],
                        program_image[byte_offset + 2],
                        program_image[byte_offset + 3],
                    ]);
                    let ordered = Exclusive::new(inst).as_ordered();
                    let bytes = ordered.to_le_bytes();
                    program_image[byte_offset..byte_offset + 4].copy_from_slice(&bytes);
                }
            }
        }

        self.total_program_size -= image_size;

        // Copy patch instructions into the program image (last module only).
        if self.relocate_module_index == self.modules.len() {
            if self.mode == PatchMode::PreText {
                // Prepend (upstream copies to start of image)
                // This is handled by the caller in practice.
            } else {
                // Append patch section after data.
                program_image.resize(image_size + patch_size, 0);
                let patch_bytes: Vec<u8> = self
                    .patch_instructions
                    .iter()
                    .flat_map(|w| w.to_le_bytes())
                    .collect();
                let copy_len = patch_bytes.len().min(patch_size);
                program_image[image_size..image_size + copy_len]
                    .copy_from_slice(&patch_bytes[..copy_len]);
            }
            return true;
        }

        false
    }
}

impl Default for Patcher {
    fn default() -> Self {
        Self::new()
    }
}

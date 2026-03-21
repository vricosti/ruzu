// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/memory/cheat_engine.h and cheat_engine.cpp
//! Cheat engine for applying game cheats via the dmnt cheat VM.

use super::dmnt_cheat_types::{
    CheatDefinition, CheatEntry, CheatProcessMetadata, MemoryRegionExtents,
};
use super::dmnt_cheat_vm::{DmntCheatVm, VmCallbacks};
use super::memory::Memory;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

/// Cheat engine callback frequency: ~12 Hz (1000000000 / 12 ns).
const CHEAT_ENGINE_NS: Duration = Duration::from_nanos(1_000_000_000 / 12);

// ---- Helper: Extract name from cheat text ----

fn extract_name(data: &str, start_index: usize, match_char: char) -> Option<(usize, &str)> {
    let remaining = &data[start_index..];
    if let Some(end_offset) = remaining.find(match_char) {
        let name_size = end_offset;
        // Clamp to CheatDefinition::readable_name size (0x40)
        let clamped = name_size.min(0x40);
        Some((name_size, &remaining[..clamped]))
    } else {
        None
    }
}

// ---- StandardVmCallbacks ----

/// Standard VM callbacks that interact with the emulator's memory system.
/// Port of StandardVmCallbacks from cheat_engine.h/cpp.
pub struct StandardVmCallbacks {
    metadata: CheatProcessMetadata,
    /// Reference to the application's memory.
    /// Upstream: `Core::System& m_system` → `system.ApplicationMemory()`.
    memory: Option<Arc<Mutex<Memory>>>,
}

impl StandardVmCallbacks {
    pub fn new(metadata: CheatProcessMetadata) -> Self {
        Self {
            metadata,
            memory: None,
        }
    }

    pub fn with_memory(metadata: CheatProcessMetadata, memory: Arc<Mutex<Memory>>) -> Self {
        Self {
            metadata,
            memory: Some(memory),
        }
    }

    fn is_address_in_range(&self, address: u64) -> bool {
        let in_main = address >= self.metadata.main_nso_extents.base
            && address < self.metadata.main_nso_extents.base + self.metadata.main_nso_extents.size;
        let in_heap = address >= self.metadata.heap_extents.base
            && address < self.metadata.heap_extents.base + self.metadata.heap_extents.size;
        let in_alias = address >= self.metadata.alias_extents.base
            && address < self.metadata.alias_extents.base + self.metadata.alias_extents.size;
        let in_aslr = address >= self.metadata.aslr_extents.base
            && address < self.metadata.aslr_extents.base + self.metadata.aslr_extents.size;

        if !in_main && !in_heap && !in_alias && !in_aslr {
            log::debug!(
                "Cheat attempting to access memory at invalid address={:016X}",
                address
            );
            return false;
        }

        true
    }
}

impl VmCallbacks for StandardVmCallbacks {
    fn memory_read_unsafe(&self, address: u64, data: &mut [u8]) {
        if !self.is_address_in_range(address) {
            data.fill(0);
            return;
        }
        if let Some(ref memory) = self.memory {
            let mem = memory.lock().unwrap();
            mem.read_block(address, data);
        } else {
            data.fill(0);
        }
    }

    fn memory_write_unsafe(&self, address: u64, data: &[u8]) {
        if !self.is_address_in_range(address) {
            return;
        }
        if let Some(ref memory) = self.memory {
            let mem = memory.lock().unwrap();
            mem.write_block(address, data);
        }
    }

    fn hid_keys_down(&self) -> u64 {
        // Upstream: queries HID service via system.ServiceManager().
        // HID key state requires the input subsystem to be wired.
        0
    }

    fn pause_process(&self) {
        // Upstream: system.ApplicationProcess()->SetActivity(Paused).
        // Process activity control requires System reference.
        log::debug!("CheatEngine: pause_process requested");
    }

    fn resume_process(&self) {
        // Upstream: system.ApplicationProcess()->SetActivity(Runnable).
        log::debug!("CheatEngine: resume_process requested");
    }

    fn debug_log(&self, id: u8, value: u64) {
        log::info!(
            "Cheat triggered DebugLog: ID '{:01X}' Value '{:016X}'",
            id,
            value
        );
    }

    fn command_log(&self, data: &str) {
        let trimmed = data.trim_end_matches('\n');
        log::debug!("[DmntCheatVm]: {}", trimmed);
    }
}

// ---- CheatParser ----

/// Trait for parsing cheat text into CheatEntry lists.
/// Port of CheatParser from cheat_engine.h.
pub trait CheatParser {
    fn parse(&self, data: &str) -> Vec<CheatEntry>;
}

/// Text-based cheat parser. Port of TextCheatParser.
pub struct TextCheatParser;

impl CheatParser for TextCheatParser {
    fn parse(&self, data: &str) -> Vec<CheatEntry> {
        let mut out = vec![CheatEntry::default()];
        let mut current_entry: Option<usize> = None;

        let chars: Vec<char> = data.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            if chars[i].is_whitespace() {
                i += 1;
                continue;
            }

            if chars[i] == '{' {
                current_entry = Some(0);

                if out[0].definition.num_opcodes > 0 {
                    return vec![];
                }

                let start = i + 1;
                if let Some((name_size, name)) = extract_name(data, start, '}') {
                    let name_bytes = name.as_bytes();
                    let copy_len =
                        name_bytes.len().min(out[0].definition.readable_name.len() - 1);
                    out[0].definition.readable_name[..copy_len]
                        .copy_from_slice(&name_bytes[..copy_len]);
                    let last = out[0].definition.readable_name.len() - 1;
                    out[0].definition.readable_name[last] = 0;
                    i += name_size + 1;
                } else {
                    return vec![];
                }

                i += 1;
            } else if chars[i] == '[' {
                let idx = out.len();
                current_entry = Some(idx);
                out.push(CheatEntry::default());

                let start = i + 1;
                if let Some((name_size, name)) = extract_name(data, start, ']') {
                    let name_bytes = name.as_bytes();
                    let copy_len =
                        name_bytes.len().min(out[idx].definition.readable_name.len() - 1);
                    out[idx].definition.readable_name[..copy_len]
                        .copy_from_slice(&name_bytes[..copy_len]);
                    let last = out[idx].definition.readable_name.len() - 1;
                    out[idx].definition.readable_name[last] = 0;
                    i += name_size + 1;
                } else {
                    return vec![];
                }

                i += 1;
            } else if chars[i].is_ascii_hexdigit() {
                let entry_idx = match current_entry {
                    Some(idx) => idx,
                    None => return vec![],
                };

                if out[entry_idx].definition.num_opcodes as usize
                    >= out[entry_idx].definition.opcodes.len()
                {
                    return vec![];
                }

                // Read 8 hex characters
                if i + 8 > chars.len() {
                    return vec![];
                }
                let hex_str: String = chars[i..i + 8].iter().collect();
                if !hex_str.chars().all(|c| c.is_ascii_hexdigit()) {
                    return vec![];
                }

                let value = u32::from_str_radix(&hex_str, 16).unwrap_or(0);
                let num = out[entry_idx].definition.num_opcodes as usize;
                out[entry_idx].definition.opcodes[num] = value;
                out[entry_idx].definition.num_opcodes += 1;

                i += 8;
            } else {
                return vec![];
            }
        }

        out[0].enabled = out[0].definition.num_opcodes > 0;
        out[0].cheat_id = 0;

        for idx in 1..out.len() {
            out[idx].enabled = out[idx].definition.num_opcodes > 0;
            out[idx].cheat_id = idx as u32;
        }

        out
    }
}

// ---- CheatEngine ----

/// Encapsulates a CheatList and manages its interaction with the cheat VM.
/// Port of CheatEngine from cheat_engine.h/cpp.
pub struct CheatEngine {
    vm: DmntCheatVm,
    metadata: CheatProcessMetadata,
    cheats: Vec<CheatEntry>,
    is_pending_reload: AtomicBool,
    // Upstream: Core::Timing event + CoreTiming reference for periodic execution.
    // In our model, the caller triggers execute_cheat_list periodically.
}

impl CheatEngine {
    pub fn new(cheats: Vec<CheatEntry>, build_id: &[u8; 0x20]) -> Self {
        let mut metadata = CheatProcessMetadata::default();
        metadata.main_nso_build_id = *build_id;

        let callbacks = Box::new(StandardVmCallbacks::new(metadata.clone()));
        let vm = DmntCheatVm::new(callbacks);

        Self {
            vm,
            metadata,
            cheats,
            is_pending_reload: AtomicBool::new(false),
        }
    }

    pub fn initialize(&mut self) {
        // Upstream: schedules a CoreTiming event at CHEAT_ENGINE_NS interval
        // and reads process metadata (process_id, title_id, heap/aslr/alias extents)
        // from system.ApplicationProcess(). The caller should set metadata via
        // set_main_memory_parameters before calling initialize.
        self.is_pending_reload.store(true, Ordering::Release);
    }

    pub fn set_main_memory_parameters(&mut self, main_region_begin: u64, main_region_size: u64) {
        self.metadata.main_nso_extents = MemoryRegionExtents {
            base: main_region_begin,
            size: main_region_size,
        };
    }

    pub fn reload(&mut self, reload_cheats: Vec<CheatEntry>) {
        self.cheats = reload_cheats;
        self.is_pending_reload.store(true, Ordering::Release);
    }

    /// Called each frame (at CHEAT_ENGINE_NS intervals).
    pub fn frame_callback(&mut self) {
        if self.is_pending_reload.swap(false, Ordering::AcqRel) {
            self.vm.load_program(&self.cheats);
        }

        if self.vm.get_program_size() == 0 {
            return;
        }

        self.vm.execute(&self.metadata);
    }
}

// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/symbols.h and symbols.cpp
//! Symbol loading and resolution from ELF dynamic sections.

use std::collections::BTreeMap;

use crate::memory::memory::Memory;

/// Type alias for virtual addresses
pub type VAddr = u64;

/// Symbol table: name -> (start_address, size)
pub type Symbols = BTreeMap<String, (VAddr, usize)>;

/// ELF dynamic tag constants
const ELF_DT_NULL: u64 = 0;
const ELF_DT_STRTAB: u64 = 5;
const ELF_DT_SYMTAB: u64 = 6;
const ELF_DT_SYMENT: u64 = 11;

/// MOD0 magic bytes
const MOD0_MAGIC: u32 = u32::from_le_bytes([b'M', b'O', b'D', b'0']);

/// Get symbols from memory at a given base address.
///
/// Corresponds to upstream `Core::Symbols::GetSymbols(VAddr, Memory&, bool)`.
///
/// Upstream creates a `ReadBytes` lambda that calls `memory.ReadBlock(base + offset, ptr, size)`
/// to read from guest memory, then passes it to the templated `GetSymbols<Word, ELFSymbol>`.
pub fn get_symbols_from_memory(
    base: VAddr,
    memory: &Memory,
    is_64: bool,
) -> Symbols {
    let read_bytes = |offset: usize, size: usize| -> Vec<u8> {
        let mut buf = vec![0u8; size];
        memory.read_block(base + offset as u64, &mut buf);
        buf
    };

    if is_64 {
        get_symbols_impl::<u64>(&read_bytes)
    } else {
        get_symbols_impl::<u32>(&read_bytes)
    }
}

/// Get symbols from a byte slice.
///
/// Corresponds to upstream `Core::Symbols::GetSymbols(span<const u8>, bool)`.
pub fn get_symbols_from_data(data: &[u8], is_64: bool) -> Symbols {
    let read_bytes = |offset: usize, size: usize| -> Vec<u8> {
        if offset + size <= data.len() {
            data[offset..offset + size].to_vec()
        } else {
            vec![0; size]
        }
    };

    if is_64 {
        get_symbols_impl::<u64>(&read_bytes)
    } else {
        get_symbols_impl::<u32>(&read_bytes)
    }
}

/// Internal symbol parsing, generic over word size.
///
/// Corresponds to upstream template `GetSymbols<Word, ELFSymbol, ByteReader>`.
fn get_symbols_impl<W: Word>(read_bytes: &dyn Fn(usize, usize) -> Vec<u8>) -> Symbols {
    let read8 = |index: usize| -> u8 {
        let data = read_bytes(index, 1);
        data[0]
    };

    let read32 = |index: usize| -> u32 {
        let data = read_bytes(index, 4);
        u32::from_le_bytes([data[0], data[1], data[2], data[3]])
    };

    let read_word = |index: usize| -> u64 { W::read_word(read_bytes, index) };

    let mod_offset = read32(4) as usize;

    if read32(mod_offset) != MOD0_MAGIC {
        return Symbols::new();
    }

    let mut string_table_offset: u64 = 0;
    let mut symbol_table_offset: u64 = 0;
    let mut symbol_entry_size: u64 = 0;

    let dynamic_offset = read32(mod_offset + 4) as usize + mod_offset;

    let mut dynamic_index = dynamic_offset;
    loop {
        let tag = read_word(dynamic_index);
        let value = read_word(dynamic_index + W::SIZE);
        dynamic_index += 2 * W::SIZE;

        if tag == ELF_DT_NULL {
            break;
        }

        if tag == ELF_DT_STRTAB {
            string_table_offset = value;
        } else if tag == ELF_DT_SYMTAB {
            symbol_table_offset = value;
        } else if tag == ELF_DT_SYMENT {
            symbol_entry_size = value;
        }
    }

    if string_table_offset == 0 || symbol_table_offset == 0 || symbol_entry_size == 0 {
        return Symbols::new();
    }

    let mut out = Symbols::new();

    let mut symbol_index = symbol_table_offset;
    while symbol_index < string_table_offset {
        let (st_name, st_value, st_size) = W::read_symbol(read_bytes, symbol_index as usize);

        let mut string_offset = (string_table_offset + st_name as u64) as usize;
        let mut name = String::new();
        loop {
            let c = read8(string_offset);
            if c == 0 {
                break;
            }
            name.push(c as char);
            string_offset += 1;
        }

        symbol_index += symbol_entry_size;
        out.insert(name, (st_value, st_size));
    }

    out
}

/// Look up a symbol name by address.
///
/// Corresponds to upstream `Core::Symbols::GetSymbolName`.
pub fn get_symbol_name(symbols: &Symbols, addr: VAddr) -> Option<&str> {
    for (name, (start_address, size)) in symbols.iter() {
        let end_address = start_address + *size as u64;
        if addr >= *start_address && addr < end_address {
            return Some(name.as_str());
        }
    }
    None
}

/// Trait abstracting over 32-bit and 64-bit ELF word sizes.
trait Word {
    const SIZE: usize;
    fn read_word(read_bytes: &dyn Fn(usize, usize) -> Vec<u8>, index: usize) -> u64;
    /// Returns (st_name, st_value, st_size)
    fn read_symbol(
        read_bytes: &dyn Fn(usize, usize) -> Vec<u8>,
        index: usize,
    ) -> (u32, u64, usize);
}

impl Word for u32 {
    const SIZE: usize = 4;

    fn read_word(read_bytes: &dyn Fn(usize, usize) -> Vec<u8>, index: usize) -> u64 {
        let data = read_bytes(index, 4);
        u32::from_le_bytes([data[0], data[1], data[2], data[3]]) as u64
    }

    /// Elf32_Sym: st_name(4), st_value(4), st_size(4), st_info(1), st_other(1), st_shndx(2)
    fn read_symbol(
        read_bytes: &dyn Fn(usize, usize) -> Vec<u8>,
        index: usize,
    ) -> (u32, u64, usize) {
        let data = read_bytes(index, 16);
        let st_name = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
        let st_value = u32::from_le_bytes([data[4], data[5], data[6], data[7]]) as u64;
        let st_size = u32::from_le_bytes([data[8], data[9], data[10], data[11]]) as usize;
        (st_name, st_value, st_size)
    }
}

impl Word for u64 {
    const SIZE: usize = 8;

    fn read_word(read_bytes: &dyn Fn(usize, usize) -> Vec<u8>, index: usize) -> u64 {
        let data = read_bytes(index, 8);
        u64::from_le_bytes([
            data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
        ])
    }

    /// Elf64_Sym: st_name(4), st_info(1), st_other(1), st_shndx(2), st_value(8), st_size(8)
    fn read_symbol(
        read_bytes: &dyn Fn(usize, usize) -> Vec<u8>,
        index: usize,
    ) -> (u32, u64, usize) {
        let data = read_bytes(index, 24);
        let st_name = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
        let st_value = u64::from_le_bytes([
            data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15],
        ]);
        let st_size = u64::from_le_bytes([
            data[16], data[17], data[18], data[19], data[20], data[21], data[22], data[23],
        ]) as usize;
        (st_name, st_value, st_size)
    }
}

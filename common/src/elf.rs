// SPDX-FileCopyrightText: 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/common/elf.h
//!
//! ELF binary format constants and types.

// ── Type aliases ──
// These mirror the C++ type aliases for ELF types.

/// Type for a 16-bit quantity.
pub type Elf32Half = u16;
pub type Elf64Half = u16;

/// Types for signed and unsigned 32-bit quantities.
pub type Elf32Word = u32;
pub type Elf32Sword = i32;
pub type Elf64Word = u32;
pub type Elf64Sword = i32;

/// Types for signed and unsigned 64-bit quantities.
pub type Elf32Xword = u64;
pub type Elf32Sxword = i64;
pub type Elf64Xword = u64;
pub type Elf64Sxword = i64;

/// Type of addresses.
pub type Elf32Addr = u32;
pub type Elf64Addr = u64;

/// Type of file offsets.
pub type Elf32Off = u32;
pub type Elf64Off = u64;

/// Type for section indices, which are 16-bit quantities.
pub type Elf32Section = u16;
pub type Elf64Section = u16;

/// Type for version symbol information.
pub type Elf32Versym = Elf32Half;
pub type Elf64Versym = Elf64Half;

pub const ELF_IDENT_SIZE: usize = 16;

// ── ELF file header ──

/// The ELF file header (32-bit). This appears at the start of every ELF file.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf32Ehdr {
    /// Magic number and other info
    pub e_ident: [u8; ELF_IDENT_SIZE],
    /// Object file type
    pub e_type: Elf32Half,
    /// Architecture
    pub e_machine: Elf32Half,
    /// Object file version
    pub e_version: Elf32Word,
    /// Entry point virtual address
    pub e_entry: Elf32Addr,
    /// Program header table file offset
    pub e_phoff: Elf32Off,
    /// Section header table file offset
    pub e_shoff: Elf32Off,
    /// Processor-specific flags
    pub e_flags: Elf32Word,
    /// ELF header size in bytes
    pub e_ehsize: Elf32Half,
    /// Program header table entry size
    pub e_phentsize: Elf32Half,
    /// Program header table entry count
    pub e_phnum: Elf32Half,
    /// Section header table entry size
    pub e_shentsize: Elf32Half,
    /// Section header table entry count
    pub e_shnum: Elf32Half,
    /// Section header string table index
    pub e_shstrndx: Elf32Half,
}

/// The ELF file header (64-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf64Ehdr {
    /// Magic number and other info
    pub e_ident: [u8; ELF_IDENT_SIZE],
    /// Object file type
    pub e_type: Elf64Half,
    /// Architecture
    pub e_machine: Elf64Half,
    /// Object file version
    pub e_version: Elf64Word,
    /// Entry point virtual address
    pub e_entry: Elf64Addr,
    /// Program header table file offset
    pub e_phoff: Elf64Off,
    /// Section header table file offset
    pub e_shoff: Elf64Off,
    /// Processor-specific flags
    pub e_flags: Elf64Word,
    /// ELF header size in bytes
    pub e_ehsize: Elf64Half,
    /// Program header table entry size
    pub e_phentsize: Elf64Half,
    /// Program header table entry count
    pub e_phnum: Elf64Half,
    /// Section header table entry size
    pub e_shentsize: Elf64Half,
    /// Section header table entry count
    pub e_shnum: Elf64Half,
    /// Section header string table index
    pub e_shstrndx: Elf64Half,
}

// ── ELF identification constants ──

/// 32-bit objects
pub const ELF_CLASS_32: u8 = 1;
/// 64-bit objects
pub const ELF_CLASS_64: u8 = 2;
/// 2's complement, little endian
pub const ELF_DATA_2_LSB: u8 = 1;
/// EV_CURRENT
pub const ELF_VERSION_CURRENT: u8 = 1;
/// System V ABI
pub const ELF_OS_ABI_NONE: u8 = 0;

/// No file type
pub const ELF_TYPE_NONE: u16 = 0;
/// Relocatable file
pub const ELF_TYPE_REL: u16 = 0;
/// Executable file
pub const ELF_TYPE_EXEC: u16 = 0;
/// Shared object file
pub const ELF_TYPE_DYN: u16 = 0;

/// ARM
pub const ELF_MACHINE_ARM: u16 = 40;
/// ARM AARCH64
pub const ELF_MACHINE_AARCH64: u16 = 183;

/// 32-bit ELF identification bytes.
pub const ELF32_IDENT: [u8; ELF_IDENT_SIZE] = [
    0x7f,
    b'E',
    b'L',
    b'F',
    ELF_CLASS_32,
    ELF_DATA_2_LSB,
    ELF_VERSION_CURRENT,
    ELF_OS_ABI_NONE,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
];

/// 64-bit ELF identification bytes.
pub const ELF64_IDENT: [u8; ELF_IDENT_SIZE] = [
    0x7f,
    b'E',
    b'L',
    b'F',
    ELF_CLASS_64,
    ELF_DATA_2_LSB,
    ELF_VERSION_CURRENT,
    ELF_OS_ABI_NONE,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
];

// ── Section header ──

/// Section header (32-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf32Shdr {
    /// Section name (string tbl index)
    pub sh_name: Elf32Word,
    /// Section type
    pub sh_type: Elf32Word,
    /// Section flags
    pub sh_flags: Elf32Word,
    /// Section virtual addr at execution
    pub sh_addr: Elf32Addr,
    /// Section file offset
    pub sh_offset: Elf32Off,
    /// Section size in bytes
    pub sh_size: Elf32Word,
    /// Link to another section
    pub sh_link: Elf32Word,
    /// Additional section information
    pub sh_info: Elf32Word,
    /// Section alignment
    pub sh_addralign: Elf32Word,
    /// Entry size if section holds table
    pub sh_entsize: Elf32Word,
}

/// Section header (64-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf64Shdr {
    /// Section name (string tbl index)
    pub sh_name: Elf64Word,
    /// Section type
    pub sh_type: Elf64Word,
    /// Section flags
    pub sh_flags: Elf64Xword,
    /// Section virtual addr at execution
    pub sh_addr: Elf64Addr,
    /// Section file offset
    pub sh_offset: Elf64Off,
    /// Section size in bytes
    pub sh_size: Elf64Xword,
    /// Link to another section
    pub sh_link: Elf64Word,
    /// Additional section information
    pub sh_info: Elf64Word,
    /// Section alignment
    pub sh_addralign: Elf64Xword,
    /// Entry size if section holds table
    pub sh_entsize: Elf64Xword,
}

// ── Section header constants ──

/// Undefined section
pub const ELF_SHN_UNDEF: u32 = 0;

/// Section header table entry unused
pub const ELF_SHT_NULL: u32 = 0;
/// Program data
pub const ELF_SHT_PROGBITS: u32 = 1;
/// Symbol table
pub const ELF_SHT_SYMTAB: u32 = 2;
/// String table
pub const ELF_SHT_STRTAB: u32 = 3;
/// Relocation entries with addends
pub const ELF_SHT_RELA: u32 = 4;
/// Dynamic linking information
pub const ELF_SHT_DYNAMIC: u32 = 6;
/// Program space with no data (bss)
pub const ELF_SHT_NOBITS: u32 = 7;
/// Relocation entries, no addends
pub const ELF_SHT_REL: u32 = 9;
/// Dynamic linker symbol table
pub const ELF_SHT_DYNSYM: u32 = 11;

// ── Symbol table entry ──

/// Symbol table entry (32-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf32Sym {
    /// Symbol name (string tbl index)
    pub st_name: Elf32Word,
    /// Symbol value
    pub st_value: Elf32Addr,
    /// Symbol size
    pub st_size: Elf32Word,
    /// Symbol type and binding
    pub st_info: u8,
    /// Symbol visibility
    pub st_other: u8,
    /// Section index
    pub st_shndx: Elf32Section,
}

/// Symbol table entry (64-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf64Sym {
    /// Symbol name (string tbl index)
    pub st_name: Elf64Word,
    /// Symbol type and binding
    pub st_info: u8,
    /// Symbol visibility
    pub st_other: u8,
    /// Section index
    pub st_shndx: Elf64Section,
    /// Symbol value
    pub st_value: Elf64Addr,
    /// Symbol size
    pub st_size: Elf64Xword,
}

// ── Symbol table helpers ──

/// Extract binding from st_info.
#[inline]
pub fn elf_st_bind(st_info: u8) -> u8 {
    st_info >> 4
}

/// Extract type from st_info.
#[inline]
pub fn elf_st_type(st_info: u8) -> u8 {
    st_info & 0xf
}

/// Construct st_info from binding and type.
#[inline]
pub fn elf_st_info(st_bind: u8, st_type: u8) -> u8 {
    (st_bind << 4) + (st_type & 0xf)
}

/// Local symbol
pub const ELF_BIND_LOCAL: u8 = 0;
/// Global symbol
pub const ELF_BIND_GLOBAL: u8 = 1;
/// Weak symbol
pub const ELF_BIND_WEAK: u8 = 2;

/// Symbol type is unspecified
pub const ELF_TYPE_UNSPEC: u8 = 0;
/// Symbol is a data object
pub const ELF_TYPE_OBJECT: u8 = 1;
/// Symbol is a code object
pub const ELF_TYPE_FUNC: u8 = 2;

/// Extract visibility from st_other.
#[inline]
pub fn elf_st_visibility(st_other: u8) -> u8 {
    st_other & 0x3
}

/// Default symbol visibility rules
pub const ELF_VISIBILITY_DEFAULT: u8 = 0;
/// Processor specific hidden class
pub const ELF_VISIBILITY_INTERNAL: u8 = 1;
/// Sym unavailable in other modules
pub const ELF_VISIBILITY_HIDDEN: u8 = 2;
/// Not preemptible, not exported
pub const ELF_VISIBILITY_PROTECTED: u8 = 3;

// ── Relocation entries ──

/// Relocation table entry without addend (32-bit, ShtRel).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf32Rel {
    /// Address
    pub r_offset: Elf32Addr,
    /// Relocation type and symbol index
    pub r_info: Elf32Word,
}

/// Relocation table entry with addend (32-bit, ShtRela).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf32Rela {
    /// Address
    pub r_offset: Elf32Addr,
    /// Relocation type and symbol index
    pub r_info: Elf32Word,
    /// Addend
    pub r_addend: Elf32Sword,
}

/// Relocation table entry with addend (64-bit, ShtRela).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf64Rela {
    /// Address
    pub r_offset: Elf64Addr,
    /// Relocation type and symbol index
    pub r_info: Elf64Xword,
    /// Addend
    pub r_addend: Elf64Sxword,
}

// ── RELR relocation table entry ──

pub type Elf32Relr = Elf32Word;
pub type Elf64Relr = Elf64Xword;

// ── Relocation info helpers ──

/// Extract symbol index from 32-bit r_info.
#[inline]
pub fn elf32_rel_sym_index(r_info: Elf32Word) -> u32 {
    r_info >> 8
}

/// Extract relocation type from 32-bit r_info.
#[inline]
pub fn elf32_rel_type(r_info: Elf32Word) -> u8 {
    (r_info & 0xff) as u8
}

/// Construct 32-bit r_info from symbol index and type.
#[inline]
pub fn elf32_rel_info(sym_index: u32, rel_type: u8) -> Elf32Word {
    (sym_index << 8) + rel_type as u32
}

/// Extract symbol index from 64-bit r_info.
#[inline]
pub fn elf64_rel_sym_index(r_info: Elf64Xword) -> u32 {
    (r_info >> 32) as u32
}

/// Extract relocation type from 64-bit r_info.
#[inline]
pub fn elf64_rel_type(r_info: Elf64Xword) -> u32 {
    (r_info & 0xffffffff) as u32
}

/// Construct 64-bit r_info from symbol index and type.
#[inline]
pub fn elf64_rel_info(sym_index: u32, rel_type: u32) -> Elf64Xword {
    ((sym_index as Elf64Xword) << 32) + rel_type as Elf64Xword
}

// ── ARM relocation types ──

/// Copy symbol at runtime (ARM)
pub const ELF_ARM_COPY: u32 = 20;
/// Create GOT entry (ARM)
pub const ELF_ARM_GLOB_DAT: u32 = 21;
/// Create PLT entry (ARM)
pub const ELF_ARM_JUMP_SLOT: u32 = 22;
/// Adjust by program base (ARM)
pub const ELF_ARM_RELATIVE: u32 = 23;

/// Copy symbol at runtime (AArch64)
pub const ELF_AARCH64_COPY: u32 = 1024;
/// Create GOT entry (AArch64)
pub const ELF_AARCH64_GLOB_DAT: u32 = 1025;
/// Create PLT entry (AArch64)
pub const ELF_AARCH64_JUMP_SLOT: u32 = 1026;
/// Adjust by program base (AArch64)
pub const ELF_AARCH64_RELATIVE: u32 = 1027;

// ── Program segment header ──

/// Program segment header (32-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf32Phdr {
    /// Segment type
    pub p_type: Elf32Word,
    /// Segment file offset
    pub p_offset: Elf32Off,
    /// Segment virtual address
    pub p_vaddr: Elf32Addr,
    /// Segment physical address
    pub p_paddr: Elf32Addr,
    /// Segment size in file
    pub p_filesz: Elf32Word,
    /// Segment size in memory
    pub p_memsz: Elf32Word,
    /// Segment flags
    pub p_flags: Elf32Word,
    /// Segment alignment
    pub p_align: Elf32Word,
}

/// Program segment header (64-bit).
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Elf64Phdr {
    /// Segment type
    pub p_type: Elf64Word,
    /// Segment flags
    pub p_flags: Elf64Word,
    /// Segment file offset
    pub p_offset: Elf64Off,
    /// Segment virtual address
    pub p_vaddr: Elf64Addr,
    /// Segment physical address
    pub p_paddr: Elf64Addr,
    /// Segment size in file
    pub p_filesz: Elf64Xword,
    /// Segment size in memory
    pub p_memsz: Elf64Xword,
    /// Segment alignment
    pub p_align: Elf64Xword,
}

// ── Program header type constants ──

/// Program header table entry unused
pub const ELF_PT_NULL: u32 = 0;
/// Loadable program segment
pub const ELF_PT_LOAD: u32 = 1;
/// Dynamic linking information
pub const ELF_PT_DYNAMIC: u32 = 2;
/// Program interpreter
pub const ELF_PT_INTERP: u32 = 3;
/// Auxiliary information
pub const ELF_PT_NOTE: u32 = 4;
/// Entry for header table itself
pub const ELF_PT_PHDR: u32 = 6;
/// Thread-local storage segment
pub const ELF_PT_TLS: u32 = 7;

// ── Program header flag constants ──

/// Segment is executable
pub const ELF_PF_EXEC: u32 = 0;
/// Segment is writable
pub const ELF_PF_WRITE: u32 = 1;
/// Segment is readable
pub const ELF_PF_READ: u32 = 2;

// ── Dynamic section entry ──

/// Dynamic section entry (32-bit).
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Elf32Dyn {
    /// Dynamic entry type
    pub d_tag: Elf32Sword,
    /// Integer value / Address value (union in C++)
    pub d_un: Elf32Word,
}

/// Dynamic section entry (64-bit).
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Elf64Dyn {
    /// Dynamic entry type
    pub d_tag: Elf64Sxword,
    /// Integer value / Address value (union in C++)
    pub d_un: Elf64Xword,
}

// In C++, d_un is a union of d_val and d_ptr (both the same size).
// In Rust, we use a single field since both union members have the same type.
impl Elf32Dyn {
    #[inline]
    pub fn d_val(&self) -> Elf32Word {
        self.d_un
    }
    #[inline]
    pub fn d_ptr(&self) -> Elf32Addr {
        self.d_un
    }
}

impl Elf64Dyn {
    #[inline]
    pub fn d_val(&self) -> Elf64Xword {
        self.d_un
    }
    #[inline]
    pub fn d_ptr(&self) -> Elf64Addr {
        self.d_un
    }
}

// ── Dynamic entry type constants ──

/// Marks end of dynamic section
pub const ELF_DT_NULL: u32 = 0;
/// Name of needed library
pub const ELF_DT_NEEDED: u32 = 1;
/// Size in bytes of PLT relocs
pub const ELF_DT_PLTRELSZ: u32 = 2;
/// Processor defined value
pub const ELF_DT_PLTGOT: u32 = 3;
/// Address of symbol hash table
pub const ELF_DT_HASH: u32 = 4;
/// Address of string table
pub const ELF_DT_STRTAB: u32 = 5;
/// Address of symbol table
pub const ELF_DT_SYMTAB: u32 = 6;
/// Address of Rela relocs
pub const ELF_DT_RELA: u32 = 7;
/// Total size of Rela relocs
pub const ELF_DT_RELASZ: u32 = 8;
/// Size of one Rela reloc
pub const ELF_DT_RELAENT: u32 = 9;
/// Size of string table
pub const ELF_DT_STRSZ: u32 = 10;
/// Size of one symbol table entry
pub const ELF_DT_SYMENT: u32 = 11;
/// Address of init function
pub const ELF_DT_INIT: u32 = 12;
/// Address of termination function
pub const ELF_DT_FINI: u32 = 13;
/// Address of Rel relocs
pub const ELF_DT_REL: u32 = 17;
/// Total size of Rel relocs
pub const ELF_DT_RELSZ: u32 = 18;
/// Size of one Rel reloc
pub const ELF_DT_RELENT: u32 = 19;
/// Type of reloc in PLT
pub const ELF_DT_PLTREL: u32 = 20;
/// Reloc might modify .text
pub const ELF_DT_TEXTREL: u32 = 22;
/// Address of PLT relocs
pub const ELF_DT_JMPREL: u32 = 23;
/// Process relocations of object
pub const ELF_DT_BIND_NOW: u32 = 24;
/// Array with addresses of init fct
pub const ELF_DT_INIT_ARRAY: u32 = 25;
/// Array with addresses of fini fct
pub const ELF_DT_FINI_ARRAY: u32 = 26;
/// Size in bytes of DT_INIT_ARRAY
pub const ELF_DT_INIT_ARRAYSZ: u32 = 27;
/// Size in bytes of DT_FINI_ARRAY
pub const ELF_DT_FINI_ARRAYSZ: u32 = 28;
/// Address of SYMTAB_SHNDX section
pub const ELF_DT_SYMTAB_SHNDX: u32 = 34;
/// Size of RELR relative relocations
pub const ELF_DT_RELRSZ: u32 = 35;
/// Address of RELR relative relocations
pub const ELF_DT_RELR: u32 = 36;
/// Size of one RELR relative relocation
pub const ELF_DT_RELRENT: u32 = 37;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_elf_st_info_roundtrip() {
        let info = elf_st_info(ELF_BIND_GLOBAL, ELF_TYPE_FUNC);
        assert_eq!(elf_st_bind(info), ELF_BIND_GLOBAL);
        assert_eq!(elf_st_type(info), ELF_TYPE_FUNC);
    }

    #[test]
    fn test_elf32_rel_info_roundtrip() {
        let info = elf32_rel_info(42, 7);
        assert_eq!(elf32_rel_sym_index(info), 42);
        assert_eq!(elf32_rel_type(info), 7);
    }

    #[test]
    fn test_elf64_rel_info_roundtrip() {
        let info = elf64_rel_info(0x1234, 0xABCD);
        assert_eq!(elf64_rel_sym_index(info), 0x1234);
        assert_eq!(elf64_rel_type(info), 0xABCD);
    }

    #[test]
    fn test_elf_ident() {
        assert_eq!(ELF32_IDENT[0], 0x7f);
        assert_eq!(ELF32_IDENT[1], b'E');
        assert_eq!(ELF32_IDENT[2], b'L');
        assert_eq!(ELF32_IDENT[3], b'F');
        assert_eq!(ELF32_IDENT[4], ELF_CLASS_32);

        assert_eq!(ELF64_IDENT[4], ELF_CLASS_64);
    }

    #[test]
    fn test_elf_st_visibility() {
        assert_eq!(elf_st_visibility(0), ELF_VISIBILITY_DEFAULT);
        assert_eq!(elf_st_visibility(1), ELF_VISIBILITY_INTERNAL);
        assert_eq!(elf_st_visibility(2), ELF_VISIBILITY_HIDDEN);
        assert_eq!(elf_st_visibility(3), ELF_VISIBILITY_PROTECTED);
        // Upper bits should be masked off
        assert_eq!(elf_st_visibility(0xFF), 3);
    }

    #[test]
    fn test_struct_sizes() {
        // Verify struct sizes match expected C layout sizes
        assert_eq!(std::mem::size_of::<Elf32Ehdr>(), 52);
        assert_eq!(std::mem::size_of::<Elf64Ehdr>(), 64);
    }
}

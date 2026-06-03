"""Shared NSO format parsing helpers — used by all scripts in this directory.

NSO0 header layout (offset in bytes):
  0x00: magic 'NSO0'
  0x04: version u32
  0x0C: flags u32 (bits: text_compressed, rodata_compressed, data_compressed, text_check, rodata_check, data_check)
  0x10: text  segment header  (file_off u32, mem_off u32, decomp_size u32)
  0x20: rodata segment header (file_off u32, mem_off u32, decomp_size u32)
  0x30: data  segment header  (file_off u32, mem_off u32, decomp_size u32)
  0x3C: bss size u32
  0x40: module ID (32 bytes)
  0x60: text   compressed size u32
  0x64: rodata compressed size u32
  0x68: data   compressed size u32
  0x88: dynstr  (mem_off_in_rodata u32, size u32)
  0x98: dynsym  (mem_off_in_rodata u32, size u32)

After loading, MOD0 header is referenced by the u32 stored at .text+4.
MOD0 header layout (relative to MOD0 start):
  0x00: 'MOD0'
  0x04: dynamic_offset_rel (signed offset from MOD0 start, typically into .rwdata)
  0x08: bss_start, bss_end, unwind_start, unwind_end, module_object (each u32)

Symbol entry (Elf32_Sym, 16 bytes):
  st_name  u32   — offset into dynstr
  st_value u32
  st_size  u32
  st_info  u8    — high 4 bits: bind; low 4 bits: type
  st_other u8
  st_shndx u16

ARM32 relocation entry (Elf32_Rel, 8 bytes):
  r_offset u32   — NSO mem-offset (NOT runtime addr) of the GOT slot to patch
  r_info   u32   — high 24 bits: symbol index; low 8 bits: relocation type
                   For JUMP_SLOT entries, type = 22 (R_ARM_JUMP_SLOT)
"""

import struct
import sys

try:
    import lz4.block
except ImportError:
    sys.stderr.write("ERROR: python lz4 module missing. Install: pip install --user --break-system-packages lz4\n")
    sys.exit(1)


# Dynamic tag names (ELF spec subset relevant to NSO)
DT_NAMES = {
    0: 'NULL', 1: 'NEEDED', 2: 'PLTRELSZ', 3: 'PLTGOT', 4: 'HASH', 5: 'STRTAB',
    6: 'SYMTAB', 7: 'RELA', 8: 'RELASZ', 9: 'RELAENT', 10: 'STRSZ', 11: 'SYMENT',
    17: 'REL', 18: 'RELSZ', 19: 'RELENT', 20: 'PLTREL', 23: 'JMPREL', 24: 'BIND_NOW',
    25: 'INIT_ARRAY', 26: 'FINI_ARRAY', 27: 'INIT_ARRAYSZ', 28: 'FINI_ARRAYSZ',
    0x6FFFFFF0: 'VERSYM', 0x6FFFFFFE: 'VERNEED', 0x6FFFFFFF: 'VERNEEDNUM',
    0x6FFFFEF5: 'GNU_HASH',
}


class Nso:
    """Loaded + decompressed NSO with section accessors and dynamic-section parsing."""

    def __init__(self, path):
        self.path = path
        with open(path, 'rb') as f:
            self.raw = f.read()
        if self.raw[:4] != b'NSO0':
            raise ValueError(f"not an NSO file: {path!r} (magic={self.raw[:4]!r})")

        # Headers
        self.text_file_off,   self.text_mem_off,   self.text_size   = struct.unpack('<III', self.raw[0x10:0x1C])
        self.rodata_file_off, self.rodata_mem_off, self.rodata_size = struct.unpack('<III', self.raw[0x20:0x2C])
        self.data_file_off,   self.data_mem_off,   self.data_size   = struct.unpack('<III', self.raw[0x30:0x3C])
        self.bss_size = struct.unpack('<I', self.raw[0x3C:0x40])[0]
        self.text_csz, self.rodata_csz, self.data_csz = struct.unpack('<III', self.raw[0x60:0x6C])
        self.dynstr_off_in_rodata, self.dynstr_size = struct.unpack('<II', self.raw[0x90:0x98])
        self.dynsym_off_in_rodata, self.dynsym_size = struct.unpack('<II', self.raw[0x98:0xA0])

        # Decompress (LZ4 if flag set, otherwise raw bytes)
        flags = struct.unpack('<I', self.raw[0x0C:0x10])[0]
        self.text   = self._decompress_section(self.text_file_off,   self.text_csz,   self.text_size,   flags & 1)
        self.rodata = self._decompress_section(self.rodata_file_off, self.rodata_csz, self.rodata_size, flags & 2)
        self.rwdata = self._decompress_section(self.data_file_off,   self.data_csz,   self.data_size,   flags & 4)

        # NSO virtual layout: text starts at 0, then rodata at text_size_aligned, etc.
        # The mem_off fields tell us exact mem offsets.
        self._sections = [
            ('text',   self.text_mem_off,   self.text_mem_off   + self.text_size,   self.text),
            ('rodata', self.rodata_mem_off, self.rodata_mem_off + self.rodata_size, self.rodata),
            ('rwdata', self.data_mem_off,   self.data_mem_off   + self.data_size,   self.rwdata),
        ]

        # Lazy-parsed dynamic section info
        self._dyn_info = None
        self._mod0_off = None

    def _decompress_section(self, file_off, csz, dsz, is_compressed):
        raw = self.raw[file_off:file_off + csz]
        if is_compressed and csz < dsz:
            return lz4.block.decompress(raw, uncompressed_size=dsz)
        return raw

    def mem_read(self, mem_addr, size):
        """Read `size` bytes from the NSO at memory offset `mem_addr` (NSO-relative)."""
        for name, start, end, buf in self._sections:
            if start <= mem_addr < end:
                rel = mem_addr - start
                return buf[rel:rel + size]
        return None

    @property
    def mod0_off(self):
        """MOD0 header mem offset (referenced by .text+4)."""
        if self._mod0_off is None:
            self._mod0_off = struct.unpack('<I', self.text[4:8])[0]
        return self._mod0_off

    @property
    def dynamic_mem_off(self):
        """NSO mem offset of the DYNAMIC section."""
        mod0 = self.text[self.mod0_off:self.mod0_off + 32]
        if mod0[:4] != b'MOD0':
            raise ValueError(f"MOD0 magic missing at .text+{self.mod0_off:#x}")
        dyn_off_rel = struct.unpack('<I', mod0[4:8])[0]
        return self.mod0_off + dyn_off_rel

    def parse_dynamic(self):
        """Parse the DYNAMIC section into a dict of {tag_name: value}.

        For repeated tags (NEEDED), the values are collected as a list under 'NEEDED'.
        """
        if self._dyn_info is not None:
            return self._dyn_info
        info = {}
        for i in range(2000):
            chunk = self.mem_read(self.dynamic_mem_off + i * 8, 8)
            if chunk is None or len(chunk) < 8:
                break
            tag, val = struct.unpack('<II', chunk)
            name = DT_NAMES.get(tag, f"0x{tag:X}")
            if name == 'NEEDED':
                info.setdefault('NEEDED', []).append(val)
            elif name == 'NULL':
                if i > 10:  # ignore the first few NULL terminators (multiple DT_NULL allowed)
                    break
            else:
                info[name] = val
        self._dyn_info = info
        return info

    def dynstr_bytes(self):
        dyn = self.parse_dynamic()
        # STRTAB is NSO-mem-offset
        return self.mem_read(dyn['STRTAB'], dyn['STRSZ'])

    def get_string(self, str_off):
        """Read a NUL-terminated string from dynstr at offset `str_off`."""
        d = self.dynstr_bytes()
        if str_off >= len(d):
            return ''
        end = d.find(b'\x00', str_off)
        return d[str_off:end].decode('utf-8', errors='replace') if end > 0 else d[str_off:].decode('utf-8', errors='replace')

    def get_symbol(self, idx):
        """Read symbol entry by index. Returns dict or None."""
        dyn = self.parse_dynamic()
        sym_addr = dyn['SYMTAB'] + idx * 16
        chunk = self.mem_read(sym_addr, 16)
        if chunk is None or len(chunk) < 16:
            return None
        st_name, st_value, st_size, st_info, st_other, st_shndx = struct.unpack('<IIIBBH', chunk)
        return {
            'i':       idx,
            'name':    self.get_string(st_name),
            'value':   st_value,
            'size':    st_size,
            'type':    st_info & 0xF,
            'bind':    st_info >> 4,
            'shndx':   st_shndx,
        }

    def n_symbols(self):
        dyn = self.parse_dynamic()
        # SYMENT is always 16 on ARM32. Total count is bounded by JMPREL etc.
        # Compute via the dynsym header values from the NSO header (preferred — exact).
        return self.dynsym_size // 16

    def iter_symbols(self):
        for i in range(self.n_symbols()):
            yield self.get_symbol(i)

    def jmprel_entries(self):
        """Iterate over JMPREL relocations as (index, r_offset, sym_idx, rel_type) tuples.

        r_offset is the NSO mem offset (NOT runtime addr) of the GOT slot to patch.
        """
        dyn = self.parse_dynamic()
        jmprel = dyn['JMPREL']
        size = dyn['PLTRELSZ']
        n = size // 8
        for i in range(n):
            chunk = self.mem_read(jmprel + i * 8, 8)
            r_offset, r_info = struct.unpack('<II', chunk)
            sym_idx = r_info >> 8
            rel_type = r_info & 0xFF
            yield i, r_offset, sym_idx, rel_type


def find_base_by_pattern(nso, runtime_addr, pattern_bytes):
    """Return the base address such that pattern_bytes is at NSO mem-offset (runtime_addr - base).

    Searches all sections. Returns None if pattern not found.
    """
    for name, start, end, buf in nso._sections:
        pos = buf.find(pattern_bytes)
        if pos < 0:
            continue
        # The runtime address corresponds to NSO mem offset (start + pos).
        base = runtime_addr - (start + pos)
        return base, name, start + pos
    return None

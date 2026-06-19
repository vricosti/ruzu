#!/usr/bin/env python3
"""Find which symbol a runtime address resolves to.

Usage:
  find_symbol.py <nso_path> --pc <runtime_pc_hex> --base <load_base_hex>
  find_symbol.py <nso_path> --pc <runtime_pc_hex> --base <load_base_hex> --arch aarch64
  find_symbol.py <nso_path> --pc <runtime_pc_hex> --base <load_base_hex> --arch aarch64 --nearest
  find_symbol.py <nso_path> --pc <runtime_pc_hex> --registry bases.json --game-key "MK8D ..."

Two ways to use it:
  1) The PC is a `bl <plt_addr>` target — for ARM32 we disassemble the
     3-instruction PLT stub in-place and compute its `[ip, #N]!` GOT slot.
     For AArch64 we disassemble the 4-instruction `adrp/ldr/add/br` PLT
     stub and compute the GOT slot from the ADRP page plus the LDR offset.
  2) The PC is already the GOT slot address — pass --got instead of --pc.
  3) The PC is inside normal text — pass --nearest to resolve the nearest
     dynsym function <= PC.

This avoids the ARM PC+8 trap and the "PLT[0] is 4 vs 20 bytes" trap.
"""

import argparse
import json
import struct
import sys

from _nso_common import Nso

try:
    import capstone
except ImportError:
    sys.exit("ERROR: python capstone module missing. Install: pip install --user --break-system-packages capstone")


def resolve_plt_to_got_offset(nso, plt_pc_nso_off):
    """Disassemble the PLT stub at NSO mem offset `plt_pc_nso_off` and return the GOT slot it accesses (NSO mem offset).

    Standard ARM PLT entry layout for ELF dynamic linking (3 insns, 12 bytes):
      add ip, pc, #IMM1          ← pc reads as current_pc + 8
      add ip, ip, #IMM2
      ldr pc, [ip, #IMM3]!       ← reads from [ip + IMM3]; writeback ip = ip + IMM3

    Returns the GOT NSO-mem-offset = (pc_of_first_add + 8) + IMM1 + IMM2 + IMM3.
    """
    chunk = nso.mem_read(plt_pc_nso_off, 12)
    if chunk is None or len(chunk) < 12:
        raise ValueError(f"can't read 12 bytes at NSO offset 0x{plt_pc_nso_off:X}")

    md = capstone.Cs(capstone.CS_ARCH_ARM, capstone.CS_MODE_ARM)
    insns = list(md.disasm(chunk, plt_pc_nso_off))
    if len(insns) < 3:
        raise ValueError(f"PLT stub disassembly failed at 0x{plt_pc_nso_off:X}; bytes={chunk.hex()}")

    # Manually decode the three instructions to get reliable immediates
    # (capstone op_str format varies — parse the encoded words instead)
    words = struct.unpack('<III', chunk)

    def decode_arm_immediate(word):
        """Decode a 12-bit ARM immediate field: 8-bit value rotated right by 2 * 4-bit rotation."""
        imm12 = word & 0xFFF
        rot = (imm12 >> 8) & 0xF
        val = imm12 & 0xFF
        return (val >> (rot * 2)) | (val << (32 - rot * 2)) & 0xFFFFFFFF if rot else val

    # Instruction 0: add ip, pc, #IMM1
    imm1 = decode_arm_immediate(words[0])
    # Instruction 1: add ip, ip, #IMM2
    imm2 = decode_arm_immediate(words[1])
    # Instruction 2: ldr pc, [ip, #IMM3]!  → bits[11:0] is the unsigned 12-bit offset
    imm3 = words[2] & 0xFFF

    # ARM ARM: PC reads as instruction_addr + 8 (pipeline)
    pc_at_first_add = plt_pc_nso_off + 8
    got_off = pc_at_first_add + imm1 + imm2 + imm3
    return got_off, (imm1, imm2, imm3)


def resolve_plt_to_got_offset_aarch64(nso, plt_pc_nso_off):
    """Resolve an AArch64 PLT stub to its GOT slot NSO offset.

    Standard NSO AArch64 PLT entries look like:
      adrp x16, #page
      ldr  x17, [x16, #off]
      add  x16, x16, #off
      br   x17

    Capstone reports the ADRP target as an absolute address in the disassembly
    address space. We disassemble at NSO offsets, so the reported immediate is
    the NSO-relative page containing the GOT slot.
    """
    chunk = nso.mem_read(plt_pc_nso_off, 16)
    if chunk is None or len(chunk) < 16:
        raise ValueError(f"can't read 16 bytes at NSO offset 0x{plt_pc_nso_off:X}")

    md = capstone.Cs(capstone.CS_ARCH_ARM64, capstone.CS_MODE_ARM)
    md.detail = True
    insns = list(md.disasm(chunk, plt_pc_nso_off))
    if len(insns) < 4:
        raise ValueError(f"AArch64 PLT stub disassembly failed at 0x{plt_pc_nso_off:X}; bytes={chunk.hex()}")

    if insns[0].mnemonic != 'adrp' or insns[1].mnemonic != 'ldr' or insns[2].mnemonic != 'add' or insns[3].mnemonic != 'br':
        raise ValueError(
            "not an AArch64 adrp/ldr/add/br PLT stub at "
            f"0x{plt_pc_nso_off:X}: " + " ; ".join(f"{i.mnemonic} {i.op_str}" for i in insns)
        )

    page = insns[0].operands[1].imm
    mem = insns[1].operands[1].mem
    got_off = page + mem.disp
    return got_off, (page, mem.disp)


def lookup_jmprel_by_got_offset(nso, got_nso_off):
    for i, r_offset, sym_idx, rel_type in nso.jmprel_entries():
        if r_offset == got_nso_off:
            return i, sym_idx, rel_type
    return None


def parse_dynamic_aarch64(nso):
    info = {}
    for i in range(2000):
        chunk = nso.mem_read(nso.dynamic_mem_off + i * 16, 16)
        if chunk is None or len(chunk) < 16:
            break
        tag, val = struct.unpack('<QQ', chunk)
        name = {
            0: 'NULL', 1: 'NEEDED', 2: 'PLTRELSZ', 3: 'PLTGOT', 4: 'HASH', 5: 'STRTAB',
            6: 'SYMTAB', 7: 'RELA', 8: 'RELASZ', 9: 'RELAENT', 10: 'STRSZ', 11: 'SYMENT',
            17: 'REL', 18: 'RELSZ', 19: 'RELENT', 20: 'PLTREL', 23: 'JMPREL',
            24: 'BIND_NOW', 25: 'INIT_ARRAY', 27: 'INIT_ARRAYSZ', 0x6FFFFEF5: 'GNU_HASH',
        }.get(tag, f"0x{tag:X}")
        if name == 'NEEDED':
            info.setdefault('NEEDED', []).append(val)
        elif name == 'NULL':
            if i > 10:
                break
        else:
            info[name] = val
    return info


def dynstr_aarch64(nso, dyn):
    return nso.mem_read(dyn['STRTAB'], dyn['STRSZ'])


def get_string_aarch64(nso, dyn, str_off):
    d = dynstr_aarch64(nso, dyn)
    if d is None or str_off >= len(d):
        return ''
    end = d.find(b'\x00', str_off)
    if end < 0:
        end = len(d)
    return d[str_off:end].decode('utf-8', errors='replace')


def get_symbol_aarch64(nso, dyn, idx):
    syment = dyn.get('SYMENT', 24)
    chunk = nso.mem_read(dyn['SYMTAB'] + idx * syment, syment)
    if chunk is None or len(chunk) < 24:
        return None
    st_name, st_info, st_other, st_shndx, st_value, st_size = struct.unpack('<IBBHQQ', chunk[:24])
    return {
        'i': idx,
        'name': get_string_aarch64(nso, dyn, st_name),
        'value': st_value,
        'size': st_size,
        'type': st_info & 0xF,
        'bind': st_info >> 4,
        'shndx': st_shndx,
    }


def iter_jmprel_entries_aarch64(nso, dyn):
    jmprel = dyn.get('JMPREL')
    size = dyn.get('PLTRELSZ', 0)
    if not jmprel or size == 0:
        return
    relaent = dyn.get('RELAENT', 24)
    for i in range(size // relaent):
        chunk = nso.mem_read(jmprel + i * relaent, relaent)
        if chunk is None or len(chunk) < 24:
            continue
        r_offset, r_info, r_addend = struct.unpack('<QQq', chunk[:24])
        sym_idx = r_info >> 32
        rel_type = r_info & 0xFFFF_FFFF
        yield i, r_offset, sym_idx, rel_type, r_addend


def lookup_jmprel_by_got_offset_aarch64(nso, dyn, got_nso_off):
    for i, r_offset, sym_idx, rel_type, r_addend in iter_jmprel_entries_aarch64(nso, dyn):
        if r_offset == got_nso_off:
            return i, sym_idx, rel_type, r_addend
    return None


def lookup_base(registry_path, game_key):
    with open(registry_path) as f:
        reg = json.load(f)
    if game_key not in reg:
        sys.exit(f"game key {game_key!r} not in {registry_path}; available: {list(reg.keys())}")
    if 'main' not in reg[game_key]:
        sys.exit(f"no 'main' base in registry for {game_key!r}")
    val = reg[game_key]['main']
    return int(val, 16) if isinstance(val, str) else val


def dynstr_from_nso_header(nso):
    return nso.mem_read(nso.rodata_mem_off + nso.dynstr_off_in_rodata, nso.dynstr_size)


def get_header_string(nso, str_off):
    d = dynstr_from_nso_header(nso)
    if d is None or str_off >= len(d):
        return ''
    end = d.find(b'\x00', str_off)
    if end < 0:
        end = len(d)
    return d[str_off:end].decode('utf-8', errors='replace')


def iter_header_symbols(nso, arch):
    """Iterate dynsym entries using NSO header offsets rather than DYNAMIC."""
    symtab = nso.rodata_mem_off + nso.dynsym_off_in_rodata
    if arch == 'aarch64':
        syment = 24
        for i in range(nso.dynsym_size // syment):
            chunk = nso.mem_read(symtab + i * syment, syment)
            if chunk is None or len(chunk) < syment:
                continue
            st_name, st_info, st_other, st_shndx, st_value, st_size = struct.unpack(
                '<IBBHQQ', chunk
            )
            yield {
                'i': i,
                'name': get_header_string(nso, st_name),
                'value': st_value,
                'size': st_size,
                'type': st_info & 0xF,
                'bind': st_info >> 4,
                'shndx': st_shndx,
            }
    else:
        syment = 16
        for i in range(nso.dynsym_size // syment):
            chunk = nso.mem_read(symtab + i * syment, syment)
            if chunk is None or len(chunk) < syment:
                continue
            st_name, st_value, st_size, st_info, st_other, st_shndx = struct.unpack(
                '<IIIBBH', chunk
            )
            yield {
                'i': i,
                'name': get_header_string(nso, st_name),
                'value': st_value,
                'size': st_size,
                'type': st_info & 0xF,
                'bind': st_info >> 4,
                'shndx': st_shndx,
            }


def run_nearest(nso, pc_runtime, base, arch):
    pc_nso_off = pc_runtime - base
    best = None
    for sym in iter_header_symbols(nso, arch):
        value = sym['value']
        if not sym['name'] or value == 0:
            continue
        if value <= pc_nso_off and (best is None or value > best['value']):
            best = sym

    print(f"PC runtime 0x{pc_runtime:X} → NSO offset 0x{pc_nso_off:X}")
    if best is None:
        print("\nNOT FOUND in dynsym nearest-symbol lookup")
        raise SystemExit(1)

    delta = pc_nso_off - best['value']
    in_range = best['size'] == 0 or delta < best['size']
    print(f"\n*** NEAREST SYMBOL ({arch}) ***")
    print(f"sym[{best['i']}]:")
    print(f"  name: {best['name']}")
    print(f"  value: 0x{best['value']:X}  size: 0x{best['size']:X}")
    print(f"  offset: +0x{delta:X}  in_range: {str(in_range).lower()}")
    print(f"  type: {best['type']}  bind: {best['bind']}  shndx: {best['shndx']}")


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('nso_path')
    p.add_argument('--pc',   help='runtime PC of the PLT stub (hex, e.g. 0xc6064c)')
    p.add_argument('--got',  help='runtime GOT slot address (alt. to --pc, skips PLT disasm)')
    p.add_argument('--base', help='NSO load base (hex). Use find_base.py first if unknown')
    p.add_argument('--registry', help='path to bases.json registry')
    p.add_argument('--game-key', help='key within registry')
    p.add_argument('--arch', choices=['auto', 'arm32', 'aarch64'], default='auto',
                   help='PLT/dynamic format to use (default: auto)')
    p.add_argument('--nearest', action='store_true',
                   help='Resolve --pc to nearest dynsym symbol instead of treating it as a PLT stub')
    args = p.parse_args()

    if args.base is None:
        if not (args.registry and args.game_key):
            sys.exit("--base required (or --registry + --game-key)")
        base = lookup_base(args.registry, args.game_key)
    else:
        base = int(args.base, 16)

    nso = Nso(args.nso_path)

    if args.nearest:
        if not args.pc:
            sys.exit("--nearest requires --pc")
        arch = args.arch
        if arch == 'auto':
            arch = 'aarch64'
        run_nearest(nso, int(args.pc, 16), base, arch)
        return

    def run_arm32():
        if args.got:
            got_runtime = int(args.got, 16)
            got_nso_off = got_runtime - base
            print(f"GOT runtime 0x{got_runtime:X} → NSO offset 0x{got_nso_off:X}")
        else:
            if not args.pc:
                sys.exit("--pc or --got required")
            pc_runtime = int(args.pc, 16)
            pc_nso_off = pc_runtime - base
            got_nso_off, (imm1, imm2, imm3) = resolve_plt_to_got_offset(nso, pc_nso_off)
            print(f"PLT call:  runtime 0x{pc_runtime:X} → NSO offset 0x{pc_nso_off:X}")
            print(f"  ARM32 PLT stub: add ip, pc, #{imm1:#x} ; add ip, ip, #{imm2:#x} ; ldr pc, [ip, #{imm3:#x}]!")
            print(f"  GOT slot: NSO offset 0x{got_nso_off:X}  (runtime 0x{got_nso_off + base:X})")

        hit = lookup_jmprel_by_got_offset(nso, got_nso_off)
        if hit is None:
            print(f"\nNOT FOUND in ARM32 JMPREL. Nearby entries:")
            for i, r_offset, sym_idx, rel_type in nso.jmprel_entries():
                if abs(r_offset - got_nso_off) < 0x20:
                    sym = nso.get_symbol(sym_idx)
                    print(f"  JMPREL[{i}] r_offset=0x{r_offset:X} sym[{sym_idx}] = {sym['name'][:100]}")
            raise SystemExit(1)

        i, sym_idx, rel_type = hit
        sym = nso.get_symbol(sym_idx)
        print(f"\n*** RESOLVED (ARM32) ***")
        print(f"JMPREL[{i}] → sym[{sym_idx}]:")
        print(f"  name: {sym['name']}")
        print(f"  type: {sym['type']}  bind: {sym['bind']}  shndx: {sym['shndx']}")
        print(f"  value: 0x{sym['value']:X}  size: 0x{sym['size']:X}")

    def run_aarch64():
        if args.got:
            got_runtime = int(args.got, 16)
            got_nso_off = got_runtime - base
            print(f"GOT runtime 0x{got_runtime:X} → NSO offset 0x{got_nso_off:X}")
        else:
            if not args.pc:
                sys.exit("--pc or --got required")
            pc_runtime = int(args.pc, 16)
            pc_nso_off = pc_runtime - base
            got_nso_off, (page, disp) = resolve_plt_to_got_offset_aarch64(nso, pc_nso_off)
            print(f"PLT call:  runtime 0x{pc_runtime:X} → NSO offset 0x{pc_nso_off:X}")
            print(f"  AArch64 PLT stub: adrp page={page:#x}; ldr/add disp={disp:#x}")
            print(f"  GOT slot: NSO offset 0x{got_nso_off:X}  (runtime 0x{got_nso_off + base:X})")

        dyn = parse_dynamic_aarch64(nso)
        hit = lookup_jmprel_by_got_offset_aarch64(nso, dyn, got_nso_off)
        if hit is None:
            print(f"\nNOT FOUND in AArch64 JMPREL. Nearby entries:")
            for i, r_offset, sym_idx, rel_type, r_addend in iter_jmprel_entries_aarch64(nso, dyn):
                if abs(r_offset - got_nso_off) < 0x20:
                    sym = get_symbol_aarch64(nso, dyn, sym_idx)
                    print(f"  JMPREL[{i}] r_offset=0x{r_offset:X} sym[{sym_idx}] = {sym['name'][:100]}")
            raise SystemExit(1)

        i, sym_idx, rel_type, r_addend = hit
        sym = get_symbol_aarch64(nso, dyn, sym_idx)
        print(f"\n*** RESOLVED (AArch64) ***")
        print(f"JMPREL[{i}] → sym[{sym_idx}]:")
        print(f"  name: {sym['name']}")
        print(f"  type: {sym['type']}  bind: {sym['bind']}  shndx: {sym['shndx']}")
        print(f"  value: 0x{sym['value']:X}  size: 0x{sym['size']:X}")
        print(f"  rel_type: {rel_type}  addend: {r_addend}")

    if args.arch == 'arm32':
        run_arm32()
    elif args.arch == 'aarch64':
        run_aarch64()
    else:
        try:
            run_arm32()
        except Exception as arm32_error:
            print(f"[auto] ARM32 resolver failed: {arm32_error}", file=sys.stderr)
            run_aarch64()


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""Find which symbol a PLT-call runtime address resolves to.

Usage:
  find_symbol.py <nso_path> --pc <runtime_pc_hex> --base <load_base_hex>
  find_symbol.py <nso_path> --pc <runtime_pc_hex> --registry bases.json --game-key "MK8D ..."

Two ways to use it:
  1) The PC is a `bl <plt_addr>` target — we disassemble the 3-instruction PLT stub
     in-place, compute its `[ip, #N]!` GOT slot, then look up which JMPREL entry
     points at that slot. NO off-by-N math from PLT[0] size assumptions.
  2) The PC is already the GOT slot address — pass --got instead of --pc.

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


def lookup_jmprel_by_got_offset(nso, got_nso_off):
    for i, r_offset, sym_idx, rel_type in nso.jmprel_entries():
        if r_offset == got_nso_off:
            return i, sym_idx, rel_type
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


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('nso_path')
    p.add_argument('--pc',   help='runtime PC of the PLT stub (hex, e.g. 0xc6064c)')
    p.add_argument('--got',  help='runtime GOT slot address (alt. to --pc, skips PLT disasm)')
    p.add_argument('--base', help='NSO load base (hex). Use find_base.py first if unknown')
    p.add_argument('--registry', help='path to bases.json registry')
    p.add_argument('--game-key', help='key within registry')
    args = p.parse_args()

    if args.base is None:
        if not (args.registry and args.game_key):
            sys.exit("--base required (or --registry + --game-key)")
        base = lookup_base(args.registry, args.game_key)
    else:
        base = int(args.base, 16)

    nso = Nso(args.nso_path)

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
        print(f"  PLT stub: add ip, pc, #{imm1:#x} ; add ip, ip, #{imm2:#x} ; ldr pc, [ip, #{imm3:#x}]!")
        print(f"  GOT slot: NSO offset 0x{got_nso_off:X}  (runtime 0x{got_nso_off + base:X})")

    # Look up JMPREL
    hit = lookup_jmprel_by_got_offset(nso, got_nso_off)
    if hit is None:
        # Show nearby entries for diagnostic context
        print(f"\nNOT FOUND in JMPREL. Nearby entries:")
        for i, r_offset, sym_idx, rel_type in nso.jmprel_entries():
            if abs(r_offset - got_nso_off) < 0x20:
                sym = nso.get_symbol(sym_idx)
                print(f"  JMPREL[{i}] r_offset=0x{r_offset:X} sym[{sym_idx}] = {sym['name'][:100]}")
        sys.exit(1)

    i, sym_idx, rel_type = hit
    sym = nso.get_symbol(sym_idx)
    print(f"\n*** RESOLVED ***")
    print(f"JMPREL[{i}] → sym[{sym_idx}]:")
    print(f"  name: {sym['name']}")
    print(f"  type: {sym['type']}  bind: {sym['bind']}  shndx: {sym['shndx']}")
    print(f"  value: 0x{sym['value']:X}  size: 0x{sym['size']:X}")


if __name__ == '__main__':
    main()

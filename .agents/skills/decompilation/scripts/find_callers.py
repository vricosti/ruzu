#!/usr/bin/env python3
"""Find all `bl <target>` instructions in an NSO's .text that target a given address.

Usage:
  find_callers.py <nso_path> --target <runtime_addr_hex> --base <base_hex>

ARM A1 BL encoding:
  cond(4) 1011(4) imm24(24) = 0xCBxxxxxx where C is the condition (0xE = AL).
  insn = 0xEB000000 | (imm24 & 0xFFFFFF)
  target = (insn_pc + 8) + sign_extend(imm24) * 4

We scan .text 4 bytes at a time, decode any BL, compute its absolute target, and
flag those that match. Output is in runtime addresses (NSO offset + base).
"""

import argparse
import struct
import sys

from _nso_common import Nso


def sign_extend_24(x):
    if x & 0x800000:
        return x - 0x1000000
    return x


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('nso_path')
    p.add_argument('--target', required=True, help='runtime address to find callers of (hex)')
    p.add_argument('--base', required=True, help='NSO load base (hex)')
    p.add_argument('--limit', type=int, default=50, help='max results (default 50)')
    args = p.parse_args()

    base = int(args.base, 16)
    target_runtime = int(args.target, 16)
    target_nso = target_runtime - base

    nso = Nso(args.nso_path)
    text = nso.text
    text_mem_off = nso.text_mem_off  # usually 0

    print(f"Scanning .text ({len(text)} bytes) for bl 0x{target_runtime:X} (NSO offset 0x{target_nso:X})")

    hits = []
    for off in range(0, len(text) - 3, 4):
        word = struct.unpack_from('<I', text, off)[0]
        # check cond=AL (0xE) + opcode 1011 (BL) → top byte 0xEB
        if (word >> 24) != 0xEB:
            continue
        imm24 = word & 0xFFFFFF
        imm = sign_extend_24(imm24) * 4
        insn_pc_nso = text_mem_off + off
        bl_target_nso = insn_pc_nso + 8 + imm
        if bl_target_nso == target_nso:
            hits.append(insn_pc_nso)
            if len(hits) >= args.limit:
                break

    print(f"\n{len(hits)} callers found:")
    for nso_off in hits:
        runtime = nso_off + base
        print(f"  caller PC: runtime 0x{runtime:08X}  (NSO offset 0x{nso_off:X})")


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""Capstone-disassemble ARM32 bytes at a given VMA.

Usage: disasm.py <bytes_hex> <vma_hex>

Example:
  disasm.py "00009fe701a080e00a00a0e1f50215eb" 0x71EE40
"""

import sys

try:
    import capstone
except ImportError:
    sys.exit("ERROR: python capstone module missing. Install: pip install --user --break-system-packages capstone")


def main():
    if len(sys.argv) != 3:
        print(__doc__, file=sys.stderr)
        sys.exit(2)

    hex_str = sys.argv[1].lower().replace(' ', '').replace('0x', '')
    vma = int(sys.argv[2], 16)
    data = bytes.fromhex(hex_str)

    md = capstone.Cs(capstone.CS_ARCH_ARM, capstone.CS_MODE_ARM)
    for insn in md.disasm(data, vma):
        print(f"  0x{insn.address:08X}: {insn.mnemonic:10s} {insn.op_str}")


if __name__ == '__main__':
    main()

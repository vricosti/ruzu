#!/usr/bin/env python3
"""Verify NSO load base address by byte-pattern match.

Usage: find_base.py <nso_path> <runtime_addr_hex> <runtime_bytes_hex>

Example:
  find_base.py /tmp/mk8d_extract/exefs/main 0x71EE40 00009fe701a080e00a00a0e1f50215eb
  → base = 0x00206000  (pattern found in .text at NSO offset 0x51EE40)

This is the SAFE way to determine the runtime load base of an NSO. Never
guess "0x200000" — different builds, ASLR, kernel rng all shift the base.
"""

import sys
from _nso_common import Nso, find_base_by_pattern


def main():
    if len(sys.argv) != 4:
        print(__doc__, file=sys.stderr)
        sys.exit(2)

    nso_path = sys.argv[1]
    runtime_addr = int(sys.argv[2], 16)
    pattern_hex = sys.argv[3].lower().replace(' ', '').replace('0x', '')
    try:
        pattern = bytes.fromhex(pattern_hex)
    except ValueError as e:
        sys.exit(f"error parsing hex bytes: {e}")

    nso = Nso(nso_path)
    result = find_base_by_pattern(nso, runtime_addr, pattern)
    if result is None:
        sys.exit(f"NOT FOUND — pattern {pattern.hex()} not present in any section of {nso_path}")

    base, section, found_off = result
    print(f"base = 0x{base:08X}")
    print(f"  pattern matched in .{section} at NSO mem offset 0x{found_off:X}")
    print(f"  runtime PC 0x{runtime_addr:X} - base 0x{base:X} = NSO offset 0x{runtime_addr - base:X}")


if __name__ == '__main__':
    main()

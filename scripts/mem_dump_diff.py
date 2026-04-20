#!/usr/bin/env python3
"""Diff two memory snapshots produced by RUZU_DUMP_AT_SVC / ZUYU_DUMP_AT_SVC."""

import struct
import sys
from collections import OrderedDict

PAGE = 4096
HDR = 24


def parse(path):
    """Yield (addr, size, state, perm, data) tuples from a snapshot file."""
    blocks = OrderedDict()
    with open(path, "rb") as f:
        while True:
            hdr = f.read(HDR)
            if len(hdr) < HDR:
                break
            addr, size, state, perm = struct.unpack("<QQII", hdr)
            data = f.read(size)
            if len(data) < size:
                break
            blocks[(addr, size)] = (state, perm, data)
    return blocks


def diff_block(addr, ruzu_data, zuyu_data, max_diffs=8):
    """Yield (offset, ruzu_bytes, zuyu_bytes) tuples for differing pages."""
    diffs = []
    n = min(len(ruzu_data), len(zuyu_data))
    for off in range(0, n, PAGE):
        end = min(off + PAGE, n)
        if ruzu_data[off:end] != zuyu_data[off:end]:
            # Find first differing byte in this page
            for i in range(off, end):
                if ruzu_data[i] != zuyu_data[i]:
                    rd = ruzu_data[i:i + 16]
                    zd = zuyu_data[i:i + 16]
                    diffs.append((addr + i, rd.hex(), zd.hex()))
                    break
            if len(diffs) >= max_diffs:
                break
    return diffs


def main():
    if len(sys.argv) < 3:
        print("usage: mem_dump_diff.py <ruzu.bin> <zuyu.bin> [--addr=0xN] [--all]")
        sys.exit(1)
    ruzu_path = sys.argv[1]
    zuyu_path = sys.argv[2]
    show_all = "--all" in sys.argv
    addr_filter = None
    for a in sys.argv[3:]:
        if a.startswith("--addr="):
            addr_filter = int(a.split("=", 1)[1], 0)

    print(f"Loading {ruzu_path}...")
    ruzu = parse(ruzu_path)
    print(f"  {len(ruzu)} blocks")
    print(f"Loading {zuyu_path}...")
    zuyu = parse(zuyu_path)
    print(f"  {len(zuyu)} blocks")

    ruzu_keys = set(ruzu.keys())
    zuyu_keys = set(zuyu.keys())
    common = ruzu_keys & zuyu_keys
    only_ruzu = ruzu_keys - zuyu_keys
    only_zuyu = zuyu_keys - ruzu_keys

    print(f"\n== Block layout ==")
    print(f"common: {len(common)} blocks")
    print(f"only ruzu: {len(only_ruzu)}")
    for a, s in sorted(only_ruzu):
        print(f"  RUZU-ONLY 0x{a:X} size=0x{s:X}")
    print(f"only zuyu: {len(only_zuyu)}")
    for a, s in sorted(only_zuyu):
        print(f"  ZUYU-ONLY 0x{a:X} size=0x{s:X}")

    print(f"\n== Content diff (first {8 if not show_all else 'all'} differing blocks) ==")
    diff_count = 0
    for (addr, size) in sorted(common):
        if addr_filter is not None and addr != addr_filter:
            continue
        rs, rp, rd = ruzu[(addr, size)]
        zs, zp, zd = zuyu[(addr, size)]
        if rs != zs or rp != zp:
            print(f"BLOCK 0x{addr:X} size=0x{size:X} STATE/PERM differ: ruzu state=0x{rs:X} perm=0x{rp:X}, zuyu state=0x{zs:X} perm=0x{zp:X}")
        if rd == zd:
            continue
        diffs = diff_block(addr, rd, zd, max_diffs=4)
        print(f"BLOCK 0x{addr:X} size=0x{size:X} state=0x{rs:X} ({len(diffs)} diffs in first {min(8, size//PAGE)} pages):")
        for da, rb, zb in diffs:
            print(f"  @0x{da:X}: ruzu={rb} zuyu={zb}")
        diff_count += 1
        if not show_all and diff_count >= 8:
            print("  ... (use --all to see more)")
            break


if __name__ == "__main__":
    main()

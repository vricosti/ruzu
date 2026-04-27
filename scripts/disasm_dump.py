#!/usr/bin/env python3
"""Disassemble [CODE_DUMP] blocks emitted by RUZU_DUMP_CODE.

Usage:
  scripts/disasm_dump.py ruzu_predicate.log [--mode thumb|arm|auto]
                                            [--mark 0xADDR[,0xADDR...]]
                                            [--filter 0xADDR]

Each [CODE_DUMP] line in the log carries `addr=`, `len=`, `bytes=HEX`. We
disassemble the block in Thumb-2 (default — nnSdk and most Switch game code
is Thumb-2), or ARM, or `auto` which picks per-block by counting valid
instructions in each mode.

`--mark` flags addresses with a `>>>` prefix so call/return sites pop out.
`--filter` restricts output to one starting address.
"""

import argparse
import re
import sys
from typing import Iterable, List, Tuple

try:
    import capstone
except ImportError:
    sys.exit("capstone not installed; pip install capstone")


CODE_DUMP_RE = re.compile(
    r"\[CODE_DUMP\]\s+addr=0x([0-9A-Fa-f]+)\s+len=(\d+)\s+bytes=([0-9A-Fa-f]+)"
)


def parse_marks(raw: str) -> List[int]:
    if not raw:
        return []
    out = []
    for tok in raw.split(","):
        tok = tok.strip()
        if not tok:
            continue
        out.append(int(tok, 16) if tok.lower().startswith("0x") else int(tok))
    return out


def parse_log(path: str) -> Iterable[Tuple[int, int, bytes]]:
    with open(path, "r", errors="replace") as f:
        for line in f:
            m = CODE_DUMP_RE.search(line)
            if not m:
                continue
            addr = int(m.group(1), 16)
            length = int(m.group(2))
            blob = bytes.fromhex(m.group(3))
            if len(blob) != length:
                print(
                    f"warning: addr=0x{addr:08X} expected {length} bytes, got {len(blob)}",
                    file=sys.stderr,
                )
            yield addr, length, blob


def make_engine(mode: str) -> capstone.Cs:
    arch = capstone.CS_ARCH_ARM
    flag = capstone.CS_MODE_THUMB if mode == "thumb" else capstone.CS_MODE_ARM
    cs = capstone.Cs(arch, flag)
    cs.detail = False
    return cs


def count_valid(cs: capstone.Cs, addr: int, blob: bytes) -> int:
    return sum(1 for _ in cs.disasm(blob, addr))


def pick_mode(addr: int, blob: bytes, requested: str) -> str:
    if requested != "auto":
        return requested
    cs_t = make_engine("thumb")
    cs_a = make_engine("arm")
    return "thumb" if count_valid(cs_t, addr, blob) >= count_valid(cs_a, addr, blob) else "arm"


def disasm_block(addr: int, blob: bytes, mode: str, marks: List[int]) -> None:
    actual_mode = pick_mode(addr, blob, mode)
    cs = make_engine(actual_mode)
    print(f"\n=== block addr=0x{addr:08X} len={len(blob)} mode={actual_mode} ===")
    last_end = addr
    for ins in cs.disasm(blob, addr):
        marker = ">>>" if ins.address in marks else "   "
        raw = " ".join(f"{b:02x}" for b in ins.bytes)
        print(
            f"{marker} 0x{ins.address:08X}: {raw:<11} {ins.mnemonic:<8} {ins.op_str}"
        )
        last_end = ins.address + ins.size
    end = addr + len(blob)
    if last_end < end:
        skipped = blob[last_end - addr :]
        print(
            f"    ... {len(skipped)} undecoded bytes at 0x{last_end:08X}: "
            + skipped[:16].hex()
            + ("..." if len(skipped) > 16 else "")
        )


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("log", help="path to the ruzu log file")
    ap.add_argument(
        "--mode",
        choices=("thumb", "arm", "auto"),
        default="thumb",
        help="instruction set (default: thumb)",
    )
    ap.add_argument(
        "--mark",
        default="",
        help="comma-separated hex addresses to flag with >>>",
    )
    ap.add_argument(
        "--filter",
        default=None,
        help="only print the block starting at this hex address",
    )
    args = ap.parse_args()

    marks = parse_marks(args.mark)
    filt = (
        int(args.filter, 16) if args.filter and args.filter.lower().startswith("0x")
        else int(args.filter) if args.filter
        else None
    )

    found = 0
    for addr, length, blob in parse_log(args.log):
        if filt is not None and addr != filt:
            continue
        disasm_block(addr, blob, args.mode, marks)
        found += 1

    if found == 0:
        sys.exit("no [CODE_DUMP] blocks matched")


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Byte-diff two IPC capture dumps produced by RUZU_TRACE_IPC_DIFF.

Each input is a JSONL file written by `record_ipc_diff` in svc_ipc.rs:
  {"seq":N,"handle":"0xXXXX","tid":T,"req_len":L,"rsp_len":L,
   "req":"hex","rsp":"hex","result":"0xR"}

We align entries by sequence number (same boot phase, same handle sequence),
then for each pair report:
  - identity mismatch (handle differs at same seq)
  - request bytes differ (guest sent different data)
  - response bytes differ (handler produced different data)

Stops at the first divergence by default. `--all` reports every divergence.

Usage:
    scripts/ipc_byte_diff.py inline.jsonl host_thread.jsonl
    scripts/ipc_byte_diff.py inline.jsonl host_thread.jsonl --all
    scripts/ipc_byte_diff.py inline.jsonl host_thread.jsonl --limit 100

Exit code 0 if no divergence in the compared range, 1 otherwise.
"""

import argparse
import json
import sys
from typing import Iterator


def load(path: str) -> list[dict]:
    out = []
    with open(path) as f:
        for line_no, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            try:
                out.append(json.loads(line))
            except json.JSONDecodeError as e:
                print(f"{path}:{line_no}: parse error: {e}", file=sys.stderr)
    return out


def first_diff(a: str, b: str) -> int:
    """Byte offset (in original byte stream, not hex chars) of first difference, or -1."""
    n = min(len(a), len(b))
    for i in range(0, n, 2):
        if a[i:i + 2] != b[i:i + 2]:
            return i // 2
    if len(a) != len(b):
        return n // 2
    return -1


def hex_window(s: str, offset: int, width: int = 16) -> str:
    """Render a hex window of `width` bytes around `offset` from a hex-string."""
    char_off = offset * 2
    start_char = max(0, char_off - width)
    end_char = min(len(s), char_off + width + 2)
    pre = s[start_char:char_off]
    cursor = s[char_off:char_off + 2] or "??"
    post = s[char_off + 2:end_char]
    return f"...{pre}[{cursor}]{post}..."


def cmd_from_req(req_hex: str) -> int:
    """Extract IPC command id from the request bytes.

    Layout of switch IPC TLS:
      0x00-0x03 : type (cmd type + has_special_header)
      0x04-0x07 : (num_buffers, recv_list)
      0x08-0x0F : magic / interface name
      0x10-0x13 : "SFCI" magic
      0x14-0x17 : version
      0x18-0x1B : command id
    """
    if len(req_hex) < 0x1C * 2:
        return -1
    try:
        return int.from_bytes(bytes.fromhex(req_hex[0x18 * 2:0x1C * 2]), "little")
    except ValueError:
        return -1


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("a", help="reference dump (typically inline)")
    ap.add_argument("b", help="comparand dump (typically host-thread)")
    ap.add_argument("--all", action="store_true", help="report every divergence, not just the first")
    ap.add_argument("--limit", type=int, default=None, help="only compare first N IPCs")
    ap.add_argument("--from-seq", type=int, default=0, help="skip IPCs with seq below this")
    ap.add_argument("--quiet", action="store_true", help="suppress matching-prefix summary")
    args = ap.parse_args()

    a_entries = load(args.a)
    b_entries = load(args.b)

    if not args.quiet:
        print(f"A: {args.a}  ({len(a_entries)} IPCs)", file=sys.stderr)
        print(f"B: {args.b}  ({len(b_entries)} IPCs)", file=sys.stderr)

    n = min(len(a_entries), len(b_entries))
    if args.limit is not None:
        n = min(n, args.limit)

    divergences = 0
    matched_in_a_row = 0
    last_match_seq = -1

    for i in range(n):
        a = a_entries[i]
        b = b_entries[i]

        if a.get("seq") < args.from_seq:
            continue

        seq = a["seq"]
        identity_diff = a["handle"] != b["handle"]
        req_off = first_diff(a["req"], b["req"])
        rsp_off = first_diff(a["rsp"], b["rsp"])
        result_diff = a["result"] != b["result"]

        any_diff = identity_diff or req_off >= 0 or rsp_off >= 0 or result_diff

        if not any_diff:
            matched_in_a_row += 1
            last_match_seq = seq
            continue

        if matched_in_a_row > 0 and not args.quiet:
            print(f"[seq 0..{last_match_seq}] {matched_in_a_row} consecutive matching IPCs",
                  file=sys.stderr)
            matched_in_a_row = 0

        divergences += 1
        cmd = cmd_from_req(a["req"])
        print(f"\n=== DIVERGENCE at seq={seq} (handle_A={a['handle']} cmd_A={cmd}) ===")
        if identity_diff:
            print(f"  HANDLE mismatch: A={a['handle']}  B={b['handle']}")
            print(f"  (different IPCs at same seq — alignment broken)")
        if req_off >= 0:
            print(f"  REQ bytes differ at offset 0x{req_off:X}:")
            print(f"    A: {hex_window(a['req'], req_off)}")
            print(f"    B: {hex_window(b['req'], req_off)}")
        if rsp_off >= 0:
            print(f"  RSP bytes differ at offset 0x{rsp_off:X}:")
            print(f"    A: {hex_window(a['rsp'], rsp_off)}")
            print(f"    B: {hex_window(b['rsp'], rsp_off)}")
        if result_diff:
            print(f"  RESULT differs: A={a['result']}  B={b['result']}")
        print(f"  tid: A={a['tid']}  B={b['tid']}")

        if not args.all:
            break

    if divergences == 0:
        if not args.quiet:
            print(f"\nNo divergence in {n} compared IPCs.", file=sys.stderr)
        return 0

    if len(a_entries) != len(b_entries):
        print(f"\nNote: dumps have different totals: A={len(a_entries)}, B={len(b_entries)}",
              file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""hw_watch_wedge.py — hardware-watchpoint debugger for the STK heap-shifted-pointer wedge.

The wedge corrupts memfd[0x10B603F8] (= guest vaddr 0x814903F8 in mstate). All
software instrumentation in ruzu-cmd has failed to identify the corrupting write;
this script uses a *hardware* watchpoint via gdb (4 DR registers on x86-64) to
catch writes at the CPU level — no instrumentation can hide a write from this.

USAGE
-----
    # Terminal 1: launch ruzu-cmd with STK normally (no env vars needed).
    cargo run --release --bin ruzu-cmd -- -r vulkan -g <STK.nro>

    # Terminal 2: as soon as ruzu-cmd logs `fastmem_pointer=Some(...)`,
    # invoke this script with the ruzu-cmd PID.
    scripts/hw_watch_wedge.py <PID>

    # Optional: --vaddr 0xVADDR to watch a different guest vaddr.
    # Optional: --hits N to break only after N writes (skip early init writes).

The script:
  1. Reads /proc/PID/cmdline + /proc/PID/maps to find the fastmem arena base.
  2. Computes host VA = arena_base + guest_vaddr.
  3. gdb-attaches, sets `awatch *(uint64_t*)host_va`, prints bt + reg state on hit.
  4. After N hits (default 0 = first hit), continues; subsequent hits also logged.

Because ruzu-cmd may not include the arena base in its symbol table, we recover
it heuristically by scanning /proc/PID/maps for the largest anonymous mapping
that's MAP_PRIVATE-shaped (matches HostMemoryImpl::new's virtual reservation of
~512 GB).
"""

import argparse
import os
import re
import subprocess
import sys
from pathlib import Path


# 39-bit address space → ~512 GB virtual reservation. Match anything close.
ARENA_SIZE_HINT_GB = 512
PAGE = 4096


def find_fastmem_arena(pid: int) -> int:
    """Scan /proc/<pid>/maps for the fastmem arena base.

    HostMemoryImpl::new mmaps a 512 GB+huge_page anonymous private region as the
    fastmem arena, then overlays parts with MAP_FIXED|MAP_SHARED memfd mappings.
    The largest contiguous anonymous-private region is the arena base.

    Returns the host virtual address of the arena base.
    """
    maps_path = Path(f"/proc/{pid}/maps")
    if not maps_path.exists():
        sys.exit(f"error: /proc/{pid}/maps not found (pid alive?)")

    candidates: list[tuple[int, int]] = []  # (size, start)
    for line in maps_path.read_text().splitlines():
        # Format: addr-addr perm offset dev inode pathname
        m = re.match(
            r"^([0-9a-f]+)-([0-9a-f]+)\s+([rwxp\-]+)\s+\S+\s+\S+\s+\S+(.*)$",
            line,
        )
        if not m:
            continue
        start, end, perm, rest = m.groups()
        start_i = int(start, 16)
        end_i = int(end, 16)
        size = end_i - start_i
        # Want huge anon mapping (~512 GB) with rw permissions
        if size > 100 * (1 << 30) and "rw" in perm:
            candidates.append((size, start_i))

    if not candidates:
        sys.exit("error: no candidate fastmem arena found in /proc/PID/maps")
    candidates.sort(reverse=True)
    base = candidates[0][1]
    print(f"[hw_watch] detected fastmem arena base: 0x{base:016x} (size {candidates[0][0] >> 30} GB)")
    return base


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("pid", type=int, help="ruzu-cmd PID")
    parser.add_argument(
        "--vaddr",
        type=lambda s: int(s, 0),
        default=0x814903F8,
        help="guest vaddr to watch (default: 0x814903F8 = mstate sentinel that wedges STK)",
    )
    parser.add_argument(
        "--width",
        type=int,
        default=8,
        choices=[1, 2, 4, 8],
        help="watchpoint width in bytes (1/2/4/8; gdb hardware watch limit)",
    )
    parser.add_argument(
        "--hits",
        type=int,
        default=0,
        help="number of writes to ignore before stopping (default: 0 = stop on first hit)",
    )
    parser.add_argument(
        "--continue-after",
        type=int,
        default=10,
        help="continue execution after each hit (logs N hits then detaches)",
    )
    parser.add_argument(
        "--gdb",
        default="gdb",
        help="gdb binary (default: gdb)",
    )
    args = parser.parse_args()

    arena = find_fastmem_arena(args.pid)
    target = arena + args.vaddr
    print(f"[hw_watch] guest vaddr 0x{args.vaddr:016x} → host VA 0x{target:016x}")

    # gdb command file. `awatch` traps on read OR write; `watch` is write-only.
    # We use `watch` to focus on writes (the corruption is a write).
    width_to_type = {1: "uint8_t", 2: "uint16_t", 4: "uint32_t", 8: "uint64_t"}
    watch_expr = f"*({width_to_type[args.width]}*)0x{target:016x}"

    # Note: `set non-stop` and `set scheduler-locking` must be set BEFORE
    # attach (or as gdb -ex args), not in a sourced command file after
    # attach (gdb errors with "Cannot change this setting while the
    # inferior is running"). Default gdb is all-stop mode which is fine.
    cmd_lines = [
        f"set pagination off",
        f"set print pretty on",
        # ruzu installs SIGSEGV handler for fastmem-fallback. SIGUSR1 is the
        # thread-state dump trigger. Pass these through to the inferior
        # transparently — otherwise gdb stops execution on every fastmem
        # out-of-arena access (which happens billions of times).
        f"handle SIGSEGV nostop noprint pass",
        f"handle SIGUSR1 nostop noprint pass",
        f"handle SIGILL nostop noprint pass",
        f"handle SIGSTOP nostop noprint pass",
        f"handle SIGCHLD nostop noprint pass",
        f"watch {watch_expr}",
    ]
    if args.hits > 0:
        cmd_lines += [
            f"ignore 1 {args.hits}",
        ]
    cmd_lines += [
        f"commands 1",
        f"  printf \"\\n[HW_WATCH HIT] write to 0x{target:016x}\\n\"",
        f"  printf \"  guest vaddr was 0x{args.vaddr:016x}\\n\"",
        f"  bt 30",
        f"  info registers rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 rip rsp rbp",
        f"  printf \"\\n--- continuing ---\\n\"",
        f"  continue",
        f"end",
        f"continue",
    ]

    cmd_file = Path(f"/tmp/hw_watch_wedge_{args.pid}.gdb")
    cmd_file.write_text("\n".join(cmd_lines) + "\n")
    print(f"[hw_watch] gdb command file: {cmd_file}")
    print(f"[hw_watch] attaching gdb to pid {args.pid}…")

    try:
        result = subprocess.run(
            [
                args.gdb,
                "--batch",
                "-x",
                str(cmd_file),
                "-p",
                str(args.pid),
            ],
            check=False,
        )
        return result.returncode
    except FileNotFoundError:
        sys.exit(f"error: gdb binary '{args.gdb}' not found in PATH")


if __name__ == "__main__":
    sys.exit(main())

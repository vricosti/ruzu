#!/usr/bin/env python3
"""
Diff CreateThread / StartThread sequences between ruzu and zuyu logs.

This is narrower than `scripts/svc_diff.py`: it focuses only on the owner-local
`svc_thread` log lines so missing worker-thread creation can be localized
without diffing the whole SVC stream.

Supported inputs:
  - ruzu `svc::CreateThread ...` / `svc::StartThread ...` logs
  - zuyu `svc_thread.cpp:CreateThread...` / `svc_thread.cpp:StartThread...` logs

Usage:
  python3 scripts/thread_svc_diff.py <ruzu.log> <zuyu.log> [--show N] [--mode full|normalized]
"""

from __future__ import annotations

import argparse
import difflib
import re
import sys
from dataclasses import dataclass
from pathlib import Path


RUZU_CREATE_RE = re.compile(
    r"svc::CreateThread called entry=0x([0-9A-Fa-f]+), arg=0x([0-9A-Fa-f]+), "
    r"stack=0x([0-9A-Fa-f]+), prio=([-0-9]+), core=([-0-9]+)"
)
RUZU_RESOLVED_RE = re.compile(
    r"svc::CreateThread resolved entry=0x([0-9A-Fa-f]+) prio=([-0-9]+) "
    r"requested_core=([-0-9]+) effective_core=([-0-9]+)"
)
RUZU_START_CALL_RE = re.compile(r"svc::StartThread called thread=0x([0-9A-Fa-f]+)")
RUZU_START_RESULT_RE = re.compile(
    r"svc::StartThread result handle=0x([0-9A-Fa-f]+) tid=([0-9]+) .*? affinity=0x([0-9A-Fa-f]+)"
)

ZUYU_CREATE_RE = re.compile(
    r"svc_thread\.cpp:CreateThread:\d+: called entry_point=0x([0-9A-Fa-f]+), "
    r"arg=0x([0-9A-Fa-f]+), stack_bottom=0x([0-9A-Fa-f]+), "
    r"priority=0x([0-9A-Fa-f]+), core_id=0x([0-9A-Fa-f\-]+)"
)
ZUYU_START_CALL_RE = re.compile(
    r"svc_thread\.cpp:StartThread:\d+: called thread=0x([0-9A-Fa-f]+)"
)
ZUYU_START_RESULT_RE = re.compile(
    r"StartThread result handle=0x([0-9A-Fa-f]+) tid=([0-9]+) core=([-0-9]+) "
    r"current_core=([-0-9]+) affinity=0x([0-9A-Fa-f]+) prio=([-0-9]+)"
)


@dataclass
class CreateEvent:
    source: str
    line_no: int
    entry: int
    arg: int
    stack: int
    priority: int
    requested_core: int
    effective_core: int | None = None

    def key(self) -> tuple[int, int, int, int, int]:
        return (self.entry, self.arg, self.stack, self.priority, self.requested_core)

    def render(self) -> str:
        eff = (
            f" effective_core={self.effective_core}"
            if self.effective_core is not None
            else ""
        )
        return (
            f"entry=0x{self.entry:08X} arg=0x{self.arg:08X} stack=0x{self.stack:08X} "
            f"prio={self.priority} requested_core={self.requested_core}{eff}"
        )


@dataclass
class StartEvent:
    source: str
    line_no: int
    handle: int
    tid: int | None = None
    affinity: int | None = None

    def key(self) -> tuple[int]:
        return (self.handle,)

    def render(self) -> str:
        parts = [f"handle=0x{self.handle:08X}"]
        if self.tid is not None:
            parts.append(f"tid={self.tid}")
        if self.affinity is not None:
            parts.append(f"affinity=0x{self.affinity:X}")
        return " ".join(parts)


@dataclass
class ParsedLog:
    creates: list[CreateEvent]
    starts_called: list[StartEvent]
    starts_result: list[StartEvent]


def parse_signed_hex(text: str) -> int:
    if text.startswith("-"):
        return -int(text[1:], 16)
    return int(text, 16)


def parse_log(path: Path) -> ParsedLog:
    creates: list[CreateEvent] = []
    starts_called: list[StartEvent] = []
    starts_result: list[StartEvent] = []
    last_unresolved_create: CreateEvent | None = None

    with path.open("r", errors="replace") as f:
        for line_no, line in enumerate(f, start=1):
            if m := RUZU_CREATE_RE.search(line):
                event = CreateEvent(
                    source="ruzu",
                    line_no=line_no,
                    entry=int(m.group(1), 16),
                    arg=int(m.group(2), 16),
                    stack=int(m.group(3), 16),
                    priority=int(m.group(4), 10),
                    requested_core=int(m.group(5), 10),
                )
                creates.append(event)
                last_unresolved_create = event
                continue

            if m := RUZU_RESOLVED_RE.search(line):
                if last_unresolved_create is not None:
                    last_unresolved_create.effective_core = int(m.group(4), 10)
                continue

            if m := RUZU_START_CALL_RE.search(line):
                starts_called.append(
                    StartEvent(
                        source="ruzu",
                        line_no=line_no,
                        handle=int(m.group(1), 16),
                    )
                )
                continue

            if m := RUZU_START_RESULT_RE.search(line):
                starts_result.append(
                    StartEvent(
                        source="ruzu",
                        line_no=line_no,
                        handle=int(m.group(1), 16),
                        tid=int(m.group(2), 10),
                        affinity=int(m.group(3), 16),
                    )
                )
                continue

            if m := ZUYU_CREATE_RE.search(line):
                creates.append(
                    CreateEvent(
                        source="zuyu",
                        line_no=line_no,
                        entry=int(m.group(1), 16),
                        arg=int(m.group(2), 16),
                        stack=int(m.group(3), 16),
                        priority=parse_signed_hex(m.group(4)),
                        requested_core=parse_signed_hex(m.group(5)),
                    )
                )
                last_unresolved_create = None
                continue

            if m := ZUYU_START_CALL_RE.search(line):
                starts_called.append(
                    StartEvent(
                        source="zuyu",
                        line_no=line_no,
                        handle=int(m.group(1), 16),
                    )
                )
                continue

            if m := ZUYU_START_RESULT_RE.search(line):
                starts_result.append(
                    StartEvent(
                        source="zuyu",
                        line_no=line_no,
                        handle=int(m.group(1), 16),
                        tid=int(m.group(2), 10),
                        affinity=int(m.group(5), 16),
                    )
                )

    return ParsedLog(creates=creates, starts_called=starts_called, starts_result=starts_result)


def key_full(evt: CreateEvent) -> tuple[int, int, int, int, int]:
    return evt.key()


def key_normalized(evt: CreateEvent) -> tuple[int, int, int]:
    return (evt.entry, evt.priority, evt.requested_core)


def render_normalized(evt: CreateEvent) -> str:
    return (
        f"entry=0x{evt.entry:08X} prio={evt.priority} requested_core={evt.requested_core}"
    )


def shared_prefix_len(lhs, rhs, key_fn) -> int:
    limit = min(len(lhs), len(rhs))
    i = 0
    while i < limit and key_fn(lhs[i]) == key_fn(rhs[i]):
        i += 1
    return i


def show_positional_diff(
    name: str,
    lhs,
    rhs,
    key_fn,
    render_fn,
) -> None:
    limit = min(len(lhs), len(rhs))
    for i in range(limit):
        if key_fn(lhs[i]) != key_fn(rhs[i]):
            print(f"\nFirst positional {name} divergence at index {i}:")
            print(f"  ruzu: {render_fn(lhs[i])} (line {lhs[i].line_no})")
            print(f"  zuyu: {render_fn(rhs[i])} (line {rhs[i].line_no})")
            return
    if len(lhs) != len(rhs):
        side = "ruzu" if len(lhs) > len(rhs) else "zuyu"
        extra = lhs[limit] if len(lhs) > len(rhs) else rhs[limit]
        print(f"\n{name} counts differ after shared prefix of {limit}.")
        print(f"  extra {side} event: {render_fn(extra)} (line {extra.line_no})")


def show_sequence_diff(lhs, rhs, show: int, key_fn, render_fn) -> None:
    lhs_keys = [repr(key_fn(evt)) for evt in lhs]
    rhs_keys = [repr(key_fn(evt)) for evt in rhs]
    matcher = difflib.SequenceMatcher(a=lhs_keys, b=rhs_keys)
    shown = 0

    print("\nCreateThread sequence diff:")
    for tag, i1, i2, j1, j2 in matcher.get_opcodes():
        if tag == "equal":
            continue
        print(f"  {tag}: ruzu[{i1}:{i2}] vs zuyu[{j1}:{j2}]")
        if tag in ("replace", "delete"):
            for evt in lhs[i1:i2][:show]:
                print(f"    ruzu line {evt.line_no}: {render_fn(evt)}")
        if tag in ("replace", "insert"):
            for evt in rhs[j1:j2][:show]:
                print(f"    zuyu line {evt.line_no}: {render_fn(evt)}")
        shown += 1
        if shown >= show:
            break
    if shown == 0:
        print("  no structural diff detected")


def show_tail_summary(
    name: str,
    lhs,
    rhs,
    key_fn,
    render_fn,
    show: int,
) -> None:
    prefix = shared_prefix_len(lhs, rhs, key_fn)
    print(f"\nShared {name} prefix length: {prefix}")
    if prefix == len(lhs) and prefix == len(rhs):
        print("  sequences are identical")
        return

    print("  ruzu tail:")
    for evt in lhs[prefix : prefix + show]:
        print(f"    line {evt.line_no}: {render_fn(evt)}")
    print("  zuyu tail:")
    for evt in rhs[prefix : prefix + show]:
        print(f"    line {evt.line_no}: {render_fn(evt)}")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("ruzu_log")
    parser.add_argument("zuyu_log")
    parser.add_argument("--show", type=int, default=4, help="max events to print per diff block")
    parser.add_argument(
        "--mode",
        choices=("full", "normalized"),
        default="normalized",
        help="comparison key for CreateThread events",
    )
    args = parser.parse_args()

    ruzu = parse_log(Path(args.ruzu_log))
    zuyu = parse_log(Path(args.zuyu_log))

    print(f"ruzu creates={len(ruzu.creates)} start_calls={len(ruzu.starts_called)} start_results={len(ruzu.starts_result)}")
    print(f"zuyu creates={len(zuyu.creates)} start_calls={len(zuyu.starts_called)} start_results={len(zuyu.starts_result)}")

    if args.mode == "full":
        key_fn = key_full
        render_fn = CreateEvent.render
    else:
        key_fn = key_normalized
        render_fn = render_normalized

    show_positional_diff("CreateThread", ruzu.creates, zuyu.creates, key_fn, render_fn)
    show_tail_summary("CreateThread", ruzu.creates, zuyu.creates, key_fn, render_fn, args.show)
    show_sequence_diff(ruzu.creates, zuyu.creates, args.show, key_fn, render_fn)

    print("\nStartThread call counts:")
    print(f"  ruzu: {len(ruzu.starts_called)}")
    print(f"  zuyu: {len(zuyu.starts_called)}")
    show_positional_diff(
        "StartThread call",
        ruzu.starts_called,
        zuyu.starts_called,
        StartEvent.key,
        StartEvent.render,
    )
    show_tail_summary(
        "StartThread call",
        ruzu.starts_called,
        zuyu.starts_called,
        StartEvent.key,
        StartEvent.render,
        args.show,
    )
    print("StartThread result counts:")
    print(f"  ruzu: {len(ruzu.starts_result)}")
    print(f"  zuyu: {len(zuyu.starts_result)}")

    return 0


if __name__ == "__main__":
    sys.exit(main())

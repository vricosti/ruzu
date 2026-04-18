#!/usr/bin/env python3
"""Align ruzu vs zuyu IPC_REPLY logs and report first payload divergences.

Both logs share the format:
  IPC_REPLY seq=N service=<name> cmd=<id> words=<n> payload=<hex...>

We key each reply by (service, cmd, rank) where rank is the per-key occurrence
index within the run — this skips past differing session IDs / seq numbers.
"""
import re
import sys
from collections import defaultdict

PAT = re.compile(
    r"IPC_REPLY seq=(?P<seq>\d+) service=(?P<service>[^ ]+) cmd=(?P<cmd>\d+)"
    r"(?:\s+ioctl=0x(?P<ioctl>[0-9a-fA-F]+))?"
    r"\s+words=(?P<words>\d+) payload=(?P<payload>[0-9a-fA-F ]+)"
)


# Normalize Rust-qualified service names ("core::hle::service::...::IStorage")
# down to the tail class name so they align with zuyu's short names.
def _normalize_service(name):
    name = name.rstrip(":")
    if "::" in name:
        name = name.split("::")[-1]
    return name


def parse(path):
    records = []
    per_key = defaultdict(int)
    with open(path, encoding="utf-8", errors="replace") as fh:
        for line in fh:
            m = PAT.search(line)
            if not m:
                continue
            service = _normalize_service(m.group("service"))
            cmd = int(m.group("cmd"))
            ioctl = int(m.group("ioctl"), 16) if m.group("ioctl") else 0
            payload = m.group("payload").strip().split()
            # Key on ioctl for nvdrv so distinct ioctls don't collapse onto one rank.
            key = (service, cmd, ioctl)
            rank = per_key[key]
            per_key[key] += 1
            records.append(((service, cmd, ioctl, rank), payload, int(m.group("seq"))))
    return records


def main():
    if len(sys.argv) != 3:
        print("usage: ipc_diff.py <ruzu.log> <zuyu.log>", file=sys.stderr)
        sys.exit(2)
    ruzu = parse(sys.argv[1])
    zuyu = parse(sys.argv[2])

    ruzu_idx = {k: (payload, seq) for k, payload, seq in ruzu}
    zuyu_idx = {k: (payload, seq) for k, payload, seq in zuyu}

    # Keys present in both, sorted by earliest zuyu seq so we report in time order.
    shared = sorted(set(ruzu_idx) & set(zuyu_idx), key=lambda k: zuyu_idx[k][1])

    def trim_to_sfco(words):
        # Align at SFCO magic so header descriptor diffs (handle ids) drop out.
        for i, w in enumerate(words):
            if w.lower() == "4f434653":
                return words[i:]
        return words

    diffs = 0
    for key in shared:
        r_payload, r_seq = ruzu_idx[key]
        z_payload, z_seq = zuyu_idx[key]
        r_trim = trim_to_sfco(r_payload)
        z_trim = trim_to_sfco(z_payload)
        if r_trim == z_trim:
            continue
        diffs += 1
        service, cmd, ioctl, rank = key
        ioctl_s = f" ioctl=0x{ioctl:08X}" if ioctl else ""
        print(
            f"DIFF service={service} cmd={cmd}{ioctl_s} rank={rank} "
            f"ruzu_seq={r_seq} zuyu_seq={z_seq}"
        )
        print(f"  ruzu(sfco+): {' '.join(r_trim)}")
        print(f"  zuyu(sfco+): {' '.join(z_trim)}")
        # Per-word diff summary (indices from SFCO)
        diff_words = []
        for i, (rw, zw) in enumerate(zip(r_trim, z_trim)):
            if rw != zw:
                diff_words.append(f"[{i}] {rw} != {zw}")
        if len(r_trim) != len(z_trim):
            diff_words.append(f"len ruzu={len(r_trim)} zuyu={len(z_trim)}")
        print(f"  words_differ: {', '.join(diff_words)}")
        if diffs >= 30:
            print("... (stopping after 30 diffs)")
            break

    # Also report keys present only on one side
    only_ruzu = sorted(set(ruzu_idx) - set(zuyu_idx))
    only_zuyu = sorted(set(zuyu_idx) - set(ruzu_idx))
    print(f"\n=== summary ===")
    print(f"shared_keys={len(shared)} diffs={diffs}")
    print(f"only_in_ruzu={len(only_ruzu)}, only_in_zuyu={len(only_zuyu)}")
    for k in only_ruzu[:10]:
        print(f"  only_ruzu: {k}")
    for k in only_zuyu[:10]:
        print(f"  only_zuyu: {k}")


if __name__ == "__main__":
    main()

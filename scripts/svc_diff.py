#!/usr/bin/env python3
"""
SVC-level diff between ruzu (RUZU_SVC_TRACE=1) and zuyu (zuyu_full_5s_trace.log).

Extracts, for each main-thread SVC:
  - imm, args (matches both engines)
  - TLS_REQ bytes (cmif request envelope)
  - TLS_RSP_BUF bytes (ruzu: HLERequestContext command buffer pre-writeback)
  - TLS_RSP_TLS bytes (ruzu: guest TLS after writeback, matches zuyu's TLS_RSP)
  - TLS_RSP bytes (zuyu: single post-writeback view; treated as the reference)
  - OUT_BUF bytes (ioctl output buffer parsed from X-descriptor)

Compares ruzu tid=73 vs zuyu tid=75 and prints the first point where a field differs.

Usage:
  python3 scripts/svc_diff.py /tmp/mk8d-buf2.log traces/zuyu_full_5s_trace.log \
      [--limit N] [--rsp {tls,buf}] [--no-rsp-gate] [--semantic-only]

--rsp selects which ruzu response view is compared against zuyu's TLS_RSP:
  tls (default): compare ruzu TLS_RSP_TLS (guest memory after writeback)
  buf:           compare ruzu TLS_RSP_BUF (command buffer pre-writeback)

--no-rsp-gate disables the "effective write_size" gate that limits response
comparison to the range the handler actually writes. Without the gate, tail
bytes beyond write_size produce false-positive diffs because zuyu's reference
trace captures 16 words of TLS regardless of how much the handler wrote.

--semantic-only ignores raw SVC arg differences and only stops on:
  - imm mismatch
  - TLS_REQ mismatch
  - TLS_RSP mismatch (selected response view)
  - OUT_BUF mismatch
This is useful once allocator / VA drift makes arg tuples noisy but you still
want the first guest-visible IPC divergence.

Zuyu's trace currently only has TLS_REQ/TLS_RSP (not OUT_BUF). To diff OUT_BUFs,
add matching instrumentation to upstream `src/core/hle/service/hle_ipc.cpp` that
writes the X-descriptor output bytes after each SendSyncRequest returns.
"""

import re
import sys
from collections import OrderedDict


ARG_RE = re.compile(r"args=\[([^\]]+)\]")
IMM_RE = re.compile(r"imm=0x([0-9a-fA-F]+)")
TID_RE = re.compile(r"tid=(\d+)")
TLS_REQ_RE = re.compile(r"TLS_REQ\s+\[0x([0-9a-fA-F]+)\]:\s*(.*)")
TLS_RSP_BUF_RE = re.compile(r"TLS_RSP_BUF\s+\[0x([0-9a-fA-F]+)\]:\s*(.*)")
TLS_RSP_TLS_RE = re.compile(r"TLS_RSP_TLS\s+\[0x([0-9a-fA-F]+)\]:\s*(.*)")
# Legacy single-label TLS_RSP (zuyu still uses this; also matches older ruzu logs).
TLS_RSP_RE = re.compile(r"TLS_RSP(?![_A-Z])\s+\[0x([0-9a-fA-F]+)\]:\s*(.*)")
OUT_BUF_RE = re.compile(r"OUT_BUF\s+\[0x([0-9a-fA-F]+)\]\s+size=0x([0-9a-fA-F]+):\s*(.*)")


def _display_word_to_u32(display_word):
    """Both parsers emit words as 8 hex chars in LE byte order (byte stream).
    Convert back to the numeric u32 value."""
    if len(display_word) != 8:
        return None
    b0, b1, b2, b3 = display_word[0:2], display_word[2:4], display_word[4:6], display_word[6:8]
    try:
        return int(b3 + b2 + b1 + b0, 16)
    except ValueError:
        return None


def request_compare_mask(request_display_words):
    """Return the set of word indices the guest actually writes for a CMIF
    request: CommandHeader + optional HandleDescriptor + descriptors +
    data segment. The alignment pad between descriptors and the data segment
    is *not* written by the guest — differences there are stale residue.

    Returns None when the header shape is unsupported (TIPC, missing bytes,
    unrecognized); caller falls back to comparing all captured words.
    """
    if not request_display_words or len(request_display_words) < 2:
        return None
    raw_low = _display_word_to_u32(request_display_words[0])
    raw_high = _display_word_to_u32(request_display_words[1])
    if raw_low is None or raw_high is None:
        return None
    cmd_type = raw_low & 0xffff
    if cmd_type >= 16:
        return None  # TIPC — skip gating
    num_x = (raw_low >> 16) & 0xf
    num_a = (raw_low >> 20) & 0xf
    num_b = (raw_low >> 24) & 0xf
    num_w = (raw_low >> 28) & 0xf
    data_size = raw_high & 0x3ff
    enable_hd = (raw_high >> 31) & 1
    if data_size == 0:
        return None

    index = 2  # CommandHeader
    if enable_hd:
        if len(request_display_words) < 3:
            return None
        hdh = _display_word_to_u32(request_display_words[2])
        if hdh is None:
            return None
        num_copy = (hdh >> 1) & 0xf
        num_move = (hdh >> 5) & 0xf
        index += 1 + num_copy + num_move
    index += 2 * num_x + 3 * num_a + 3 * num_b + 3 * num_w
    pre_align_end = index  # guest-written up to here (exclusive)
    # AlignWithPadding — guest does NOT write these pad words.
    rem = index & 3
    if rem:
        index += 4 - rem
    data_start = index
    data_end = index + data_size
    # Build the comparison mask: [0, pre_align_end) ∪ [data_start, data_end).
    capture_cap = len(request_display_words)
    mask = set()
    for j in range(min(pre_align_end, capture_cap)):
        mask.add(j)
    for j in range(data_start, min(data_end, capture_cap)):
        mask.add(j)
    return mask


def response_compare_mask(response_display_words):
    """Return the set of word indices the handler actually writes for a CMIF
    response. The ResponseBuilder zeros the alignment pad via `Skip(..., true)`
    so unlike requests, response pad is handler-written; include it in the mask.
    The tail beyond `write_size` is stale residue and excluded.

    Returns None when the header shape is unsupported; caller falls back.
    """
    if not response_display_words or len(response_display_words) < 2:
        return None
    raw_low = _display_word_to_u32(response_display_words[0])
    raw_high = _display_word_to_u32(response_display_words[1])
    if raw_low is None or raw_high is None:
        return None
    cmd_type = raw_low & 0xffff
    if cmd_type >= 16:
        return None
    data_size = raw_high & 0x3ff
    enable_hd = (raw_high >> 31) & 1
    if data_size < 6:
        return None

    index = 2  # CommandHeader
    if enable_hd:
        if len(response_display_words) < 3:
            return None
        hdh = _display_word_to_u32(response_display_words[2])
        if hdh is None:
            return None
        num_copy = (hdh >> 1) & 0xf
        num_move = (hdh >> 5) & 0xf
        index += 1 + num_copy + num_move
    rem = index & 3
    if rem:
        index += 4 - rem
    # index now = data_payload_offset (SFCO is written, so pad is included)
    index += 2  # DataPayloadHeader (SFCO + pad word)
    n = (data_size - 6) // 2  # normal_params_size (non-domain non-TIPC)
    write_words = index + n
    capture_cap = len(response_display_words)
    return set(range(min(write_words, capture_cap)))


def parse_ruzu(path, target_tid=73):
    """Parse ruzu log with RUZU_TRACE_SVC=1."""
    svcs = []
    cur = None
    with open(path) as f:
        for line in f:
            if "SVC_IN  imm=" in line:
                m = IMM_RE.search(line)
                tm = TID_RE.search(line)
                am = ARG_RE.search(line)
                if not m or not tm:
                    continue
                tid = int(tm.group(1))
                if tid != target_tid:
                    cur = None
                    continue
                cur = {
                    "imm": int(m.group(1), 16),
                    "args": am.group(1) if am else "",
                    "tls_req": None,
                    "tls_rsp_buf": None,
                    "tls_rsp_tls": None,
                    "out_buf": None,
                }
                svcs.append(cur)
            elif cur is None:
                continue
            elif "TLS_REQ" in line:
                m = TLS_REQ_RE.search(line)
                if m:
                    cur["tls_req"] = (int(m.group(1), 16), m.group(2).strip())
            elif "TLS_RSP_BUF" in line:
                m = TLS_RSP_BUF_RE.search(line)
                if m:
                    cur["tls_rsp_buf"] = (int(m.group(1), 16), m.group(2).strip())
            elif "TLS_RSP_TLS" in line:
                m = TLS_RSP_TLS_RE.search(line)
                if m:
                    cur["tls_rsp_tls"] = (int(m.group(1), 16), m.group(2).strip())
            elif "TLS_RSP" in line:
                # Legacy unlabeled TLS_RSP from older ruzu logs — fall back to TLS view.
                m = TLS_RSP_RE.search(line)
                if m and cur["tls_rsp_tls"] is None:
                    cur["tls_rsp_tls"] = (int(m.group(1), 16), m.group(2).strip())
            elif "OUT_BUF" in line:
                # Skip OUT_BUF_XDESC lines and the WriteToOutgoingCommandBuffer
                # data dump that follows them — both read from
                # `buffer_x_descriptors[0]` which on zuyu's existing
                # instrumentation path holds the PREVIOUS request's
                # X-descriptor (stale across IPCs), not the current one.
                # The clean ground-truth OUT_BUF for diffing comes from
                # the per-WriteBufferB/C trace mirror added in this
                # investigation pass; that's the only one we keep.
                if "OUT_BUF_XDESC" in line:
                    continue
                if "WriteToOutgoingCommandBuffer" in line:
                    continue
                m = OUT_BUF_RE.search(line)
                if m:
                    cur["out_buf"] = (
                        int(m.group(1), 16),
                        int(m.group(2), 16),
                        m.group(3).strip(),
                    )
    return svcs


def parse_zuyu(path, target_tid=75):
    """Parse zuyu_full_5s_trace.log."""
    svcs = []
    cur = None
    with open(path) as f:
        for line in f:
            if "SVC_IN  imm=" in line and f"tid={target_tid}" in line:
                m = IMM_RE.search(line)
                am = ARG_RE.search(line)
                if not m:
                    continue
                cur = {
                    "imm": int(m.group(1), 16),
                    "args": am.group(1) if am else "",
                    "tls_req": None,
                    "tls_rsp": None,
                    "out_buf": None,  # populated from OUT_BUF lines (ZUYU_TRACE_OUT_BUF)
                }
                svcs.append(cur)
            elif cur is None:
                continue
            elif "TLS_REQ" in line:
                m = TLS_REQ_RE.search(line)
                if m:
                    # Zuyu's TLS uses u32-word format; normalize to byte hex.
                    words = m.group(2).strip().split()
                    bytes_hex = "".join(
                        f"{int(w, 16):08x}"[6:8]
                        + f"{int(w, 16):08x}"[4:6]
                        + f"{int(w, 16):08x}"[2:4]
                        + f"{int(w, 16):08x}"[0:2]
                        for w in words
                    )
                    # Normalize back to space-separated 4-byte groups
                    grouped = " ".join(bytes_hex[i : i + 8] for i in range(0, len(bytes_hex), 8))
                    cur["tls_req"] = (int(m.group(1), 16), grouped)
            elif "TLS_RSP" in line:
                m = TLS_RSP_RE.search(line)
                if m:
                    words = m.group(2).strip().split()
                    bytes_hex = "".join(
                        f"{int(w, 16):08x}"[6:8]
                        + f"{int(w, 16):08x}"[4:6]
                        + f"{int(w, 16):08x}"[2:4]
                        + f"{int(w, 16):08x}"[0:2]
                        for w in words
                    )
                    grouped = " ".join(bytes_hex[i : i + 8] for i in range(0, len(bytes_hex), 8))
                    cur["tls_rsp"] = (int(m.group(1), 16), grouped)
            elif "OUT_BUF" in line:
                # Skip OUT_BUF_XDESC lines and the WriteToOutgoingCommandBuffer
                # data dump that follows them — same reason as parse_ruzu.
                # The clean ground-truth OUT_BUF for diffing comes from
                # the per-WriteBufferB/C trace mirror added in this
                # investigation pass; that's the only one we keep.
                if "OUT_BUF_XDESC" in line:
                    continue
                if "WriteToOutgoingCommandBuffer" in line:
                    continue
                m = OUT_BUF_RE.search(line)
                if m:
                    cur["out_buf"] = (
                        int(m.group(1), 16),
                        int(m.group(2), 16),
                        m.group(3).strip(),
                    )
    return svcs


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)
    ruzu_path, zuyu_path = sys.argv[1], sys.argv[2]
    limit = 3000
    rsp_view = "tls"
    rsp_gate = True
    semantic_only = False
    args_iter = iter(range(3, len(sys.argv)))
    for idx in args_iter:
        arg = sys.argv[idx]
        if arg == "--limit":
            limit = int(sys.argv[idx + 1])
            next(args_iter, None)
        elif arg == "--rsp":
            rsp_view = sys.argv[idx + 1]
            if rsp_view not in ("tls", "buf"):
                print(f"--rsp must be 'tls' or 'buf', got {rsp_view!r}")
                sys.exit(1)
            next(args_iter, None)
        elif arg == "--no-rsp-gate":
            rsp_gate = False
        elif arg == "--semantic-only":
            semantic_only = True
    rsp_key = f"tls_rsp_{rsp_view}"
    rsp_label = f"TLS_RSP_{rsp_view.upper()}"

    ruzu = parse_ruzu(ruzu_path)[:limit]
    zuyu = parse_zuyu(zuyu_path)[:limit]
    gate_note = "effective write_size gate ON" if rsp_gate else "gate OFF (--no-rsp-gate)"
    print(
        f"ruzu tid=73: {len(ruzu)} svcs, zuyu tid=75: {len(zuyu)} svcs "
        f"(response view: ruzu {rsp_label} vs zuyu TLS_RSP; {gate_note}; "
        f"{'semantic-only' if semantic_only else 'args+tls'})"
    )
    n = min(len(ruzu), len(zuyu))
    suppressed_tail = 0
    for i in range(n):
        r, z = ruzu[i], zuyu[i]
        if r["imm"] != z["imm"]:
            print(f"\n[SVC #{i+1}] IMM DIFFERS: ruzu=0x{r['imm']:02x} zuyu=0x{z['imm']:02x}")
            print(f"  ruzu args: {r['args']}")
            print(f"  zuyu args: {z['args']}")
            return
        if r["args"] != z["args"]:
            if semantic_only:
                continue
            # Skip scratch-register-only differences in arg 7
            r_args = r["args"].split(",")
            z_args = z["args"].split(",")
            if len(r_args) >= 7 and r_args[:7] == z_args[:7]:
                continue  # only last arg differs (likely scratch)
            print(f"\n[SVC #{i+1}] ARGS DIFFER (imm=0x{r['imm']:02x}):")
            print(f"  ruzu: {r['args']}")
            print(f"  zuyu: {z['args']}")
            return
        # Compare TLS_REQ and the selected response view when available on both
        # sides. The mask-based gate suppresses stale-residue differences in
        # words the guest (or handler) did not actually write.
        def compare_with_mask(label, r_payload, z_payload, mask_fn):
            nonlocal suppressed_tail
            r_words = r_payload.split()
            z_words = z_payload.split()
            capture = min(len(r_words), len(z_words))
            mask = mask_fn(r_words) if rsp_gate else None
            if mask is None:
                mask = set(range(capture))  # fall back to full comparison
            else:
                mask = {j for j in mask if j < capture}
            diff_idx = None
            for j in sorted(mask):
                if r_words[j] != z_words[j]:
                    diff_idx = j
                    break
            if diff_idx is not None:
                # Show the whole captured window so context is visible.
                r_shown = " ".join(r_words[:capture])
                z_shown = " ".join(z_words[:capture])
                print(
                    f"\n[SVC #{i+1}] {label} DIFFERS at word[{diff_idx}] "
                    f"(imm=0x{r['imm']:02x}, gate mask has {len(mask)} words):"
                )
                print(f"  ruzu: {r_shown}")
                print(f"  zuyu: {z_shown}")
                return True
            # Count suppressed residue diffs (outside the mask) for the summary.
            if rsp_gate:
                for j in range(capture):
                    if j not in mask and r_words[j] != z_words[j]:
                        suppressed_tail += 1
                        break
            return False

        if r["tls_req"] and z["tls_req"]:
            if compare_with_mask("TLS_REQ", r["tls_req"][1], z["tls_req"][1], request_compare_mask):
                return
        r_rsp = r.get(rsp_key)
        if r_rsp and z["tls_rsp"]:
            if compare_with_mask(rsp_label, r_rsp[1], z["tls_rsp"][1], response_compare_mask):
                return
        r_ob = r.get("out_buf")
        z_ob = z.get("out_buf")
        if r_ob and z_ob:
            r_addr, r_size, r_hex = r_ob
            z_addr, z_size, z_hex = z_ob
            # Addresses may differ across runs due to allocator ordering;
            # only compare content bytes.
            #
            # Size mismatch alone is treated as benign IF the shared prefix
            # bytes match: one side may zero-pad the entire client buffer
            # while the other writes only the meaningful parcel — both are
            # legal as long as the cmif response size word agrees, and the
            # game only reads up to that size. The diff cares about
            # content divergence, not handler buffer-fill style.
            r_bytes = r_hex.replace(" ", "")
            z_bytes = z_hex.replace(" ", "")
            common_chars = min(len(r_bytes), len(z_bytes))
            if r_bytes[:common_chars] != z_bytes[:common_chars]:
                print(f"\n[SVC #{i+1}] OUT_BUF DIFFERS "
                      f"(imm=0x{r['imm']:02x}, ruzu_size=0x{r_size:x}, zuyu_size=0x{z_size:x}):")
                print(f"  ruzu @0x{r_addr:x}: {r_hex}")
                print(f"  zuyu @0x{z_addr:x}: {z_hex}")
                return
            # Tail-only difference (one side zero-padded further). Note &
            # continue.
            if r_size != z_size:
                tail_zero_only = (
                    r_bytes[common_chars:].replace("0", "") == ""
                    and z_bytes[common_chars:].replace("0", "") == ""
                )
                if not tail_zero_only:
                    print(f"\n[SVC #{i+1}] OUT_BUF TAIL DIFFERS (non-zero) "
                          f"(imm=0x{r['imm']:02x}, ruzu_size=0x{r_size:x}, zuyu_size=0x{z_size:x}):")
                    print(f"  ruzu @0x{r_addr:x}: {r_hex}")
                    print(f"  zuyu @0x{z_addr:x}: {z_hex}")
                    return
    print(f"\nNo divergence found in first {n} SVCs (at args/tls level).")
    if rsp_gate and suppressed_tail:
        print(
            f"({suppressed_tail} post-write_size tail diffs suppressed by the gate — "
            "pass --no-rsp-gate to include them.)"
        )
    print("If ruzu ran past zuyu's capture window, OUT_BUF bytes are needed for deeper diff.")


if __name__ == "__main__":
    main()

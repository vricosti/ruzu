# Build warning audit — 2026-08-21

## Scope

Command under review:

```text
./build.sh
```

The release build completed successfully on Ubuntu 24.04.4 LTS with Rust 1.92.0.
The final compiler inventory contains 997 warnings. The 360 Rust naming warnings
(`non_camel_case_types` and `non_snake_case`) are intentionally excluded from this
audit, leaving 637 warnings. Every retained diagnostic is recorded in
`docs/build-warning-audit-2026-08-21.tsv` with its lint, source location,
classification, matching Eden files, normalized symbol occurrence counts, and a
short conclusion.

The comparison source is `/home/vricosti/Dev/emulators/eden/src`. `rdynarmic`
warnings are compared with Eden's embedded Dynarmic tree under
`src/dynarmic/src/dynarmic` where a direct counterpart exists.

## Results

| Classification | Count | Conclusion |
| --- | ---: | --- |
| `RUST_HYGIENE` | 201 | Unused imports, unnecessary `mut`/`unsafe`, documentation or syntax cleanup. These do not by themselves represent Eden behavior. |
| `CONFIRMED_PARITY_DEBT` | 163 | The matching symbol is used by Eden but is not consumed by Ruzu. Treat it as missing wiring, not dead code. |
| `PORT_DEBT_OR_RENAMED` | 128 | A matching upstream file exists, but direct symbol matching cannot prove equivalence. Strict-mode default: retain and review as parity debt. |
| `BACKEND_PORT_DEBT_OR_FALLBACK` | 53 | `rdynarmic`/`rxbyak` backend or fallback code is not selected by the current host dispatch. It must be audited against embedded Dynarmic before deletion. |
| `PLATFORM_DEFERRED` | 33 | Android or AArch64/NCE code is inactive on this Linux x86_64 build but is live in Eden on its target platform. |
| `FRONTEND_DEAD_OR_UNWIRED` | 14 | Ruzu/ruzu-cmd frontend code has no active caller. It is either removable local code or unfinished frontend wiring. |
| `CONFIRMED_DEAD_DUPLICATE` | 15 | Fourteen warnings come from the abandoned intrusive-tree half of `common/heap_tracker.rs`; the other is an unused input callback wrapper bypassed by the active direct callback. |
| `UPSTREAM_DORMANT_STATE` | 9 | Eden also only declares the matching state. Keep it for structural parity unless upstream removes it. |
| `OWNERSHIP_ANCHOR` | 8 | An `Arc`, `Box`, `ServiceContext`, or wrapper is retained for destruction/lifetime semantics. A compiler read is not required. |
| `CONFIRMED_DEAD_OBSOLETE` | 7 | Abandoned duplicate logger state, a telemetry port removed from current Eden, and one redundant shader decoder wrapper. These can be removed in a dedicated cleanup. |
| `REPRESENTATION_DIFFERENCE` | 3 | Two exhaustive Rust enums make Eden's defensive C++ `default` branch unreachable; FFmpeg combines receive and hardware transfer in its C shim. |
| `PLATFORM_ALIAS` | 1 | Linux defines `EWOULDBLOCK == EAGAIN`; Eden handles `EAGAIN`. |
| `MIXED_OWNERSHIP_AND_PARITY` | 1 | The Vulkan rasterizer warning combines real lifetime owners with redundant or unwired cached state. |
| `LAYOUT_ANCHOR` | 1 | The aligned tuple payload is storage passed by address to `MONITORX`; removing it would change behavior. |

## Never-read fields

There are 112 field-warning records covering 170 individual fields.

- 116 field names have multiple occurrences in the corresponding Eden files.
  They are generally parity debt: controller state, applet/service state, kernel
  bookkeeping, VFS eviction state, renderer capabilities, query state, and cache
  state are read by Eden but not by Ruzu.
- 14 fields occur only at their Eden declaration. They are upstream-dormant state,
  not Ruzu-only dead code.
- The remaining fields either use different Rust ownership/naming, belong to a
  Ruzu-specific frontend, or have no direct current Eden file. Their individual
  classification is retained in the TSV rather than being collapsed into
  `dead_code`.

Representative confirmed debt includes the GameCube adapter/Joy-Con state,
`HeapTracker`'s unused intrusive implementation, application/window-system state,
LAN discovery state, real-VFS open-file eviction, NCE patch bookkeeping,
transform-feedback query configuration, Vulkan feature/capability state, and
the dedicated-room token which is decoded but, unlike Eden, never published to
the settings consumed by the announcement session.

The old `TelemetryJson` credentials are different: current Eden removed that
whole component and Ruzu has no production caller, so they are classified as
obsolete local code rather than parity debt.

Representative non-debt fields include `ServiceContext` values whose `Drop`
implementation releases kernel resources, Vulkan/renderer `Arc` and `Box` owners
that preserve pointer validity and destruction order, and the cache-line-aligned
`MONITORX` storage.

## Parity bugs fixed during the audit

### NCA LZ4 decompression

`core/file_sys/fssystem/compression_configuration.rs` guarded decompression with a
nonexistent Cargo feature named `lz4`. Since `lz4_flex` is an unconditional core
dependency, the active build always selected the failure branch. The guard was
removed and the size validation now matches Eden's `DecompressLz4` contract.

### Polish and Thai application languages

`core/hle/service/ns/language.rs` and
`core/hle/service/set/settings_types.rs` lacked Eden's Polish and Thai enum values,
language-code conversions, and priority lists. They are now ported, including the
zero-filled aggregate tail behavior of Eden's partially initialized C++ priority
arrays.

These corrections remove two `unexpected_cfgs` warnings and one unreachable
language-match warning from the original build inventory.

## Validation

- `./build.sh --skip-deps` completes in release mode and produces both `ruzu`
  and `ruzu-cmd`.
- The focused LZ4 and language tests pass; `cargo fmt -p core -- --check` also
  passes.
- The broader `cargo test -p core --lib` run does not complete: it reports
  unrelated failures in Dynarmic invalid-fetch, crypto key-generation and
  kernel process/condition-variable tests, then aborts in
  `k_server_session::cleanup_map_succeeds_without_resolved_processes` on an
  invalid `slice::from_raw_parts` precondition. `build.sh` itself does not run
  this suite, so these failures did not block the requested release build.

## Policy

Do not remove a `never read` field solely to silence rustc. Remove it only when the
TSV classifies it as confirmed local dead code and a fresh Eden comparison confirms
that it is not an ownership, layout, platform, or pending parity contract. The
`CONFIRMED_PARITY_DEBT`, `PORT_DEBT_OR_RENAMED`, backend, and platform groups should
be resolved subsystem by subsystem rather than suppressed globally.

# Porting State

## 2026-08-05 — Controls Motion / Touch configuration

- Status: completed and verified.
- Interrupted slice: `ruzu/src/configuration/configure_motion_touch.rs`, the GTK
  counterpart of `yuzu/configuration/configure_motion_touch.{h,cpp,ui}`.
- Confirmed behavior: the button currently only logs a message. Its upstream
  dialog owns Cemuhook UDP server management, communication testing, touchpad
  calibration, and touch-from-button map selection.
- Missing prerequisite: `input_common/src/drivers/udp_client.rs` exposes
  `reload_sockets`, `CalibrationConfigurationJob`, and `test_communication`,
  but all three are non-functional stubs. In addition,
  `input_common/src/helpers/udp_protocol.rs` validates responses but cannot
  serialize requests or decode response payloads for a socket owner.
- Prerequisite result: the UDP socket lifecycle, request serialization,
  response decoding, communication test, and calibration job are implemented
  in their upstream-owned `input_common` files.
- Resumed result: both Controls entry points open `ConfigureMotionTouch`; UDP
  server management, testing, calibration, and touch-from-button profile and
  binding configuration are functional. Closing a capture or calibration
  releases its poller/thread.

## 2026-07-31 — Windows in-process game boot

- Status: interrupted while implementing the missing prerequisite.
- Interrupted slice: `ruzu/src/main_window.rs::boot_game`, the GTK counterpart
  of `GMainWindow::BootGame`.
- Confirmed behavior: double-click activation reaches `boot_game`, but the
  non-macOS/non-Linux implementation is an explicit logging stub.
- Missing prerequisite: Windows has no Rust counterpart for upstream
  `bootmanager.cpp`'s native `RenderWidget`. The Vulkan renderer already accepts
  `WindowSystemType::Windows`, but the GTK frontend must first create and own a
  child `HWND`, expose it as `WindowSystemInfo::render_surface`, and preserve
  upstream show/hide/resize/destroy ordering.
- Resume condition: implement and verify the Win32 render-window owner, then
  replace the Windows boot stub with the existing in-process boot pipeline.
- Render prerequisite result: the child `HWND` is created and the Vulkan
  swapchain initializes successfully on the AMD Radeon RX 5700 XT.
- New missing prerequisite discovered during the resumed real-title test:
  guest execution terminates with Windows exception `0x80000001`
  (`EXCEPTION_GUARD_PAGE`) in `VCRUNTIME140.dll` immediately after the CPU/GPU
  threads start. The Dynarmic Windows fastmem exception path must be verified
  against its upstream owner before the frontend slice can be called complete.
- Exception prerequisite result: the Windows unwind registration now describes
  the real dispatcher frame and follows upstream unwind-code ordering. A
  ProcDump minidump then identified the remaining access violation in the first
  A32 host callback; removing the non-upstream nested callback stack frame and
  restoring the MSVC hidden-return-pointer order fixed that crash.
- New missing prerequisite discovered after guest execution resumed:
  `Unknown SVC 0x2499F94 in 32-bit mode`. The A32 and A64 x64 SVC emitters write
  the immediate directly to `RSI`, which is only the System V second parameter.
  Upstream routes the immediate through `ArgCallback`'s ABI-selected parameter
  list; on Windows the fixed callback context occupies `RCX` and the SVC
  immediate must be written to `RDX`.
- Resume condition: restore upstream callback-owned argument selection for both
  x64 SVC emitters, verify the emitted Windows register choice with focused
  tests, then resume the MK8D boot validation.

## 2026-07-31 — Windows game-list population

- Status: completed and verified.
- Interrupted slice: `ruzu/src/game_list.rs` directory selection, recursive
  scan, and metadata population.
- Confirmed behavior: recursive enumeration finds all nine `.xci` / `.nsp`
  candidates below the configured directory, but loader validation classifies
  every candidate as `FileType::Error`.
- Missing prerequisite: `core/src/crypto/key_manager.rs::resolve_keys_dir`
  claims to search legacy yuzu locations but does not include yuzu's actual
  Windows `%APPDATA%\yuzu\keys` directory. The available `prod.keys` and
  `title.keys` are therefore not loaded.
- Prerequisite result: the resolver now checks `%APPDATA%\yuzu\keys` and
  `%APPDATA%\suyu\keys` before the existing Unix-style fallbacks. The focused
  key-directory regression test passes and `DIFF.md` contains the required
  upstream comparison.
- Resumed work: make the directory toolbar select the newly added or sole
  directory so `Scan Subfolders` cannot silently remain disabled.
- Final verification: the rebuilt Windows GUI loaded the persisted recursive
  directory, remained responsive, and reported 7 games. The other 2 discovered
  NSP files are update-only packages and were skipped by the upstream
  `FileType::Error` rule.
## 2026-07-31 — interrupted Windows x64 callback validation

- Interrupted slice: full `cargo test -p rdynarmic --release` validation after
  aligning callback, unwind, SVC and FPSCR emission with upstream.
- Exact missing prerequisite:
  `rdynarmic/src/backend/x64/emit_exclusive_memory.rs` assumes every
  128-bit exclusive-read callback returns a pair in `RAX:RDX`. That is the
  SysV return contract; MSVC uses a hidden return pointer, as represented by
  upstream `Callback::EmitCallWithReturnPointer`.
- Reproduction:
  `cargo test -p rdynarmic --release
  test_a64_ldxp_uses_exclusive_read_128_pair_return` exits with Windows
  `STATUS_ACCESS_VIOLATION`.
- Required prerequisite work: re-read the upstream exclusive-read emitter,
  implement the MSVC hidden-return path in the matching Rust x64 emitter,
  add a focused Windows execution regression, re-read/compare upstream and
  update `DIFF.md`.
- Resume condition: the isolated LDXP test and the full rdynarmic suite no
  longer terminate with an access violation.
- Prerequisite result: implemented the MSVC stack-buffer paths for ordinary
  and exclusive 128-bit reads in their x64 emitter owners. The focused LDXP
  and `LDR Q` generated-code regressions pass; full-suite validation resumed.
- Remaining full-suite issue outside this prerequisite:
  `test_a64_fmov_fmul_fmadd_fmla_sequence_preserves_lanes` fails identically
  when run alone (`(0, 0)` instead of the expected low lane). The serial suite
  subsequently terminates abnormally at that test on Windows. This separate
  SIMD-emission slice predates the callback ABI work and requires its own
  upstream comparison; the focused callback, unwind, SVC, FPSCR, LDXP and
  `LDR Q` regressions all pass.

## 2026-07-31 — interrupted MK8D boot after 128-bit callback repair

- Interrupted slice: MK8D runtime validation after rebuilding the Windows
  release with callback, SVC, FPSCR, SEH and 128-bit return fixes.
- Reproduction: the base NSP reaches GPU submission, HID, account and save-data
  initialization, then terminates in `RtlVirtualUnwind2` while unwinding an
  access violation from an x64 JIT run.
- Exact missing prerequisite found by the upstream comparison:
  A32 and A64 ordinary fastmem fallback emitters still hard-coded SysV
  `RSI`/`RDX` for callback address/value arguments. Upstream uses
  `ABI_PARAM2`/`ABI_PARAM3`, which are `RDX`/`R8` on Windows.
- Prerequisite result: both owners now select `ABI_PARAMS[1]` and
  `ABI_PARAMS[2]`; a generated A64 read/write fallback test passes on Windows.
  A separate native `RtlVirtualUnwind` test confirms that the registered
  dispatcher unwind table itself restores caller `RSP` and `RIP` correctly.
- Resume condition: rebuild Ruzu and launch the exact MK8D base NSP under
  ProcDump to determine whether the corrected fastmem ABI removes the runtime
  failure.

## 2026-07-31 — MK8D Windows boot reached rendering

- Status: the startup-crash prerequisite is completed for the tested base
  title.
- Validation title:
  `Mario Kart 8 Deluxe [0100152000022000][v0].nsp` (base application, not the
  update NSP).
- Result: the Windows release passed loader, CPU/GPU initialization and guest
  execution, displayed MK8D, and remained alive, responsive and rendering for
  continued observation. The launched process is PID 21620 and was deliberately
  left running for the user.
- Runtime corrections verified on this path: MSVC callback argument placement,
  dispatcher `StackLayout`/shadow-space addressing, dynamic SEH unwind metadata,
  ordinary and exclusive 128-bit callback returns, A32/A64 SVC emission, A32
  FPSCR lifecycle, fastmem fallback parameters, vector fallback frames, table
  lookup frame placement, and native upstream `VectorTranspose` emission.
- Remaining scope: longer gameplay compatibility is not established by this
  startup validation.

## 2026-07-31 — remaining rdynarmic validation debt

- Focused status: callback, dispatcher-prologue, real `RtlVirtualUnwind`,
  A32 FPSCR/SVC, A64 `LDR Q`, A64 `LDXP`, fastmem fallback and all-size
  `TRN1`/`TRN2` regressions pass.
- Build status: `cargo test -p rdynarmic --release --no-run` passes.
- Full-suite status: not green. A run that skipped the already known
  `test_a64_fmov_fmul_fmadd_fmla_sequence_preserves_lanes` test completed with
  709 passed, 12 failed, 4 ignored and 1 filtered. The failures include the
  independently reproducible pre-existing
  `test_a64_rev32_8h_reverses_halfwords_within_words` mismatch and several A32
  fuzz/oracle tests that report the external oracle unavailable or mismatched.
- Required future slice: audit the REV32 emitter and each independently
  reproducible SIMD failure against its upstream operation owner; separately
  serialize or isolate A32 oracle-backed tests before treating their parallel
  failures as implementation mismatches.

## 2026-08-18 — interrupted HardwareComposer pacing parity

- Interrupted slice: align `HardwareComposer::ComposeLocked` with Eden after a
  runtime profile showed the guest CPU cores, GPU thread, and Vulkan worker all
  mostly idle while presentation varied around 30 FPS.
- Exact missing prerequisites: the Rust `Layer` omitted upstream `z_index` and
  `is_overlay`; `Gpu`/`NvDispDisp0` omitted `WaitForComposite` and synchronously
  waited every `RequestComposite` instead of carrying the pending fence to the
  next composition tick.
- Required prerequisite work: port those fields and their owner-local setters,
  then port the deferred composite fence lifecycle before resuming the HWC
  acquire/release ordering change.
- Resume condition: `HardwareComposer` can reproduce Eden's wait, release,
  interval-gated acquire, z-order, overlay, and frame-number lifecycle without
  placeholder values.
- Status: completed. The prerequisite fields and setters are ported, the GPU
  carries the pending composite fence to the next HWC tick, and the composer
  now follows Eden's wait/release/acquire/compose/frame-advance order.
- Runtime result: a release run remained alive and presented at a stable
  52--55 FPS in the reached scene. This does not establish course performance;
  the remaining lower and variable course framerate requires a scene-matched
  profile rather than further HWC approximation.

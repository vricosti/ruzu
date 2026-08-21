# Porting State

## 2026-08-22 — Joy-Con HID warning slice

- Status: interrupted pending the SDL3 HID owner prerequisite.
- Interrupted slice: restore Eden's live `JoyconDriver` arrays, scan thread, output methods, and
  protocol callbacks instead of retaining private no-op methods solely to resemble the header.
- Exact missing prerequisite: Ruzu has no `JoyconHandle` owner around `SDL_hid_device`, and its
  `CommonProtocol` explicitly has no HID handle. Consequently `JoyconDriver` cannot request device
  access, read reports, write subcommands, or construct the protocol objects used by Eden.
- Required prerequisite work: port `JoyconHandle` in `joycon_types.rs`, wire it into
  `CommonProtocol` and every protocol owner, then port `JoyconDriver::RequestDeviceAccess` and its
  stoppable input thread before resuming `Joycons::{Setup,ScanThread}` and the controller arrays.
- Resume condition: `input_common/joycon-hid` can be enabled with SDL3 HID enumeration and report
  I/O working, after which the mapping-only compatibility engine can regain Eden's hardware state
  and output behavior without placeholder methods.

## 2026-08-22 — GameCube Adapter warning slice

- Status: interrupted pending the USB transport prerequisite.
- Interrupted slice: consume the retained `GCAdapter` endpoint, payload, controller-origin,
  vibration, and retry state instead of suppressing its `dead_code` warnings.
- Exact missing prerequisite: Ruzu has no libusb dependency or equivalents of Eden's
  `LibUSBContext` and `LibUSBDeviceHandle`; `InputSubsystemImpl` also never constructs or
  registers `GCAdapter`. The retained parsing methods therefore have no producer thread and the
  adapter cannot function.
- Required prerequisite work: add the cross-platform libusb owner, port device discovery,
  interface claiming, endpoint discovery, interrupt input/output and stoppable scan/input thread
  lifetimes, then register the engine under the same feature boundary in `main_common.rs`.
- Resume condition: enabling the `input_common/libusb` feature produces a live registered adapter
  on Eden's supported desktop targets, while the default build excludes the unfinished driver in
  the same way Eden excludes `gc_adapter.cpp` when `ENABLE_LIBUSB` is off.

## 2026-08-22 — abstracted-pad warning slice

- Status: completed and verified for the warning-producing ownership slice.
- Interrupted slice: consume the retained holder and handler state in
  `hid_core/resources/abstracted_pad` instead of suppressing its `dead_code` warnings.
- Exact missing prerequisite: Ruzu's `NpadAbstractedPadHolder` retains only copied assignment
  metadata, while Eden retains live `IAbstractedPad*` objects and wires the holder plus the applet
  resource into every handler through `AbstractPad::SetExternals`. Ruzu therefore cannot port the
  holder queries, MCU selection, property updates, or button/six-axis shared-memory writes without
  first restoring that ownership and external-resource graph.
- Required prerequisite work: represent live abstract-pad ownership with stable Rust shared
  owners, restore `AbstractPad::set_externals` and each upstream-owned handler setter, then port
  `NpadAbstractedPadHolder::{RemoveAbstractPadByAssignmentStyle,GetAbstractedPads}` before
  resuming the warning-producing handlers.
- Resume condition: holder mutations preserve live pad identity, focused holder tests cover Eden's
  registration/removal ordering, and the handlers can query the holder without copying stale pad
  state.
- Prerequisite result: the holder now retains stable `Arc<Mutex<IAbstractedPad>>` owners, ports
  assignment-style removal and pad enumeration, and preserves mutations made after registration.
  `AbstractPad` shares that holder with its properties handler, while the MCU handler shares the
  properties owner and selects the same live rail/six-axis pads as Eden.
- Resumed result: property and MCU queries are active; button and six-axis handlers traverse the
  applet resource in Eden's ARUID and helper-call order; battery state is selected from live pads
  and published to Npad shared memory. `NPad::on_update` defers the shared-owner callback until its
  applet lock is released, avoiding recursive locking. `cargo check -p hid_core` reports no
  `hid_core` warning and all 70 crate tests pass.

## 2026-08-21 — SMAA creation-helper warning slice

- Status: completed and verified.
- Interrupted slice: restore `SMAA::CreateImages` through `CreatePipelines` so the retained
  `m_image_count` state is consumed by its upstream-owned methods instead of flattened constructor
  locals.
- Exact missing prerequisite: Eden's `SMAA::UploadImages` calls
  `present/util.cpp::UploadImage`, but Ruzu has no counterpart. `smaa.rs` instead owns private
  upload-buffer and command helpers, keeps two upload buffers for the lifetime of `Smaa`, and uses
  tightly packed Vulkan copy-region dimensions instead of Eden's explicit texture dimensions.
- Required prerequisite work: port `UploadImage` to
  `src/video_core/src/renderer_vulkan/present/util.rs`, preserve its staging-buffer lifetime through
  `Scheduler::finish`, verify it against `util.h/.cpp`, then remove the misplaced SMAA helpers and
  resume the class-owned creation methods.
- Resume condition: the utility compiles with focused presentation tests, its upstream comparison
  is recorded in `DIFF.md`, and SMAA can call it through its retained allocator owner.
- Prerequisite result: `present/util.rs::upload_image` now owns staging allocation, mapped copy and
  flush, explicit row dimensions, render-pass exit, layout transitions, copy recording, and the
  synchronous finish in Eden's order. `cargo check -p video_core` passes and the utility has a
  focused upstream audit entry; the SMAA slice is resumed.
- Resumed result: all nine SMAA creation methods again own their matching logic, `m_image_count`
  drives dynamic-image and descriptor-pool creation, the misplaced upload helpers and permanent
  staging buffers are removed, and first-use upload follows Eden through `util::upload_image`.

## 2026-08-21 — A32 scalar saturation warning slice

- Status: completed and verified.
- Interrupted slice: replace the incomplete `frontend/a32/translate/saturated.rs` SSAT/USAT
  translation that ignored `sat_imm`, then remove the corresponding warning.
- Exact missing prerequisite: Eden's translator calls `IREmitter::{SignedSaturation,
  UnsignedSaturation,SignedSaturatedAddWithFlag,SignedSaturatedSubWithFlag}` and consumes their
  `ResultAndOverflow`. Ruzu has the base saturation opcodes, but exposes none of those scalar IR
  helpers and entirely lacks the two `WithFlag32` opcodes. Its x64 scalar saturation emitters also
  write A64 FPSR.QC directly instead of defining Eden's associated overflow pseudo-operation, so
  they cannot correctly update A32 CPSR.Q.
- Required prerequisite work: port the scalar saturation IR API and the two WithFlag32 opcodes in
  their upstream-owned IR files, then port their x64/arm64 backend handling before resuming
  `saturated.rs`.
- Resume condition: focused IR/backend tests verify saturated results and overflow values, both
  host backends compile, and the prerequisite files have fresh upstream comparisons in `DIFF.md`.
- Prerequisite result: the four scalar saturation helpers now produce Eden-compatible associated
  overflow pseudo-operations. The x64 emitter no longer writes A64 FPSR.QC for these result/flag
  operations, and the ARM64 emitter now owns the corresponding scalar saturation implementations.
  Native x64 checks/tests and AArch64 checks plus QEMU routing/encoding tests pass.
- Newly discovered prerequisite: `SSAT16` and `USAT16` call Eden's
  `translate/impl/common.h::{MostSignificantHalf,Pack2x16To1x32}`. Their Rust owner is
  `frontend/a32/translate/helpers.rs`, but neither helper exists there yet.
- New resume condition: port and verify those two common helpers against `common.h`, then resume
  the eight ARM saturation translations without duplicating the helper logic in `saturated.rs`.
- Common-helper result: `pack_2x16_to_1x32` and `most_significant_half` now live in
  `frontend/a32/translate/helpers.rs`, preserve Eden's exact IR operation order, and pass their
  focused regression test.
- Resumed result: ARM `SSAT`, `SSAT16`, `USAT`, `USAT16`, `QADD`, `QSUB`, `QDADD`, and `QDSUB`
  now preserve Eden's validation, operand, saturation, register-write, and sticky-Q ordering. The
  two unused `sat_imm` warnings are gone. Focused translation tests pass, and the same end-to-end
  result/Q-flag regression passes on native x64 and the AArch64 backend under QEMU.

## 2026-08-21 — Windows GNU 128-bit callback ABI cleanup

- Status: completed and verified.
- Interrupted slice: remove platform-specific unused imports from
  `backend/x64/{block_of_code,emit_memory}.rs` while preserving Eden's ABI.
- Exact missing prerequisite: the reviewed emitter correctly showed that Eden
  selects its indirect 128-bit memory-read path with `_WIN32`, but Ruzu's
  ordinary/exclusive-read trampolines and fastmem fallback owners still selected
  that contract only for `target_env = "msvc"`. The ordinary and exclusive
  128-bit write paths also forwarded their lanes in hard-coded System V
  registers, which overwrote the Windows context/address parameters.
- Required prerequisite work: make each explicit Rust trampoline/fallback owner
  select by `target_os = "windows"`, pass 128-bit writes through a Windows pointer
  payload, verify both native and `x86_64-pc-windows-gnu` builds, then resume the
  warning cleanup slice. Keep `callback.rs`'s MSVC-vs-MinGW hidden-return ordering:
  Eden makes that distinction explicitly in `callback.cpp`.
- Resume condition: targeted ordinary/exclusive 128-bit tests pass, MinGW
  `cargo check` reports no warning in the touched files, and each owner has a
  fresh upstream comparison in `DIFF.md`.
- Prerequisite result: ordinary and exclusive A64 reads use an explicit output
  pointer on every Windows toolchain; writes use an explicit 16-byte input
  payload instead of System V lane registers. Native `LDR Q`, `STR Q`, `LDXP`
  and `STXP` execution regressions pass, both native and MinGW checks pass, and
  the touched files emit no warning in either check.

## 2026-08-21 — NCM content-service parity

- Status: completed and verified.
- Interrupted slice: port `IContentStorage`, `IContentMetaDatabase`, and the
  `NCM::OpenContent*` handlers from `core/hle/service/ncm/ncm.cpp`.
- Prerequisite result: storage selection, mutable NAND/SDMC caches,
  placeholder registration, registered-content deletion and raw NCA install
  now live in their upstream-owned filesystem modules. Game-card registered
  and placeholder caches are also constructed and selected as upstream does.
- Resumed result: all eight NCM interfaces expose the same command tables as
  Eden and the same 12 commands have concrete handlers. Placeholder mutation,
  metadata staging/commit and child-interface creation use the real caches.
- Follow-up audit result: `RegisteredCache::InstallEntry`,
  `IterateAllMetadata`, `PlaceholderCache::GetRightsID`, and the SDMC NAX
  parsing callback are ported and covered by focused cache tests.

## 2026-08-05 — Controls Motion / Touch configuration

- Status: completed and verified.
- Interrupted slice: `src/ruzu/src/configuration/configure_motion_touch.rs`, the GTK
  counterpart of `yuzu/configuration/configure_motion_touch.{h,cpp,ui}`.
- Confirmed behavior: the button currently only logs a message. Its upstream
  dialog owns Cemuhook UDP server management, communication testing, touchpad
  calibration, and touch-from-button map selection.
- Missing prerequisite: `src/input_common/src/drivers/udp_client.rs` exposes
  `reload_sockets`, `CalibrationConfigurationJob`, and `test_communication`,
  but all three are non-functional stubs. In addition,
  `src/input_common/src/helpers/udp_protocol.rs` validates responses but cannot
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
- Interrupted slice: `src/ruzu/src/main_window.rs::boot_game`, the GTK counterpart
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

## 2026-07-31 — Windows game-list population

- Status: completed and verified.
- Interrupted slice: `src/ruzu/src/game_list.rs` directory selection, recursive
  scan, and metadata population.
- Confirmed behavior: recursive enumeration finds all nine `.xci` / `.nsp`
  candidates below the configured directory, but loader validation classifies
  every candidate as `FileType::Error`.
- Missing prerequisite: `src/core/src/crypto/key_manager.rs::resolve_keys_dir`
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
  `src/rdynarmic/src/backend/x64/emit_exclusive_memory.rs` assumes every
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

## 2026-08-20 — interrupted Mii applet startup after fastmem isolation

- Interrupted slice: resume the Mii LLE applet after restricting fastmem to the
  application process, matching Eden's `KProcess::Initialize` policy.
- Reproduction: the applet `rtld` calls `QueryMemory`, receives an error, then
  branches to its fatal self-loop at guest PC `0x80000788`. The SVC attempts to
  write its 40-byte result at guest address zero.
- Exact missing prerequisite: Rust has no counterpart for
  `k_thread.cpp::GetCurrentProcessPointer` / `GetCurrentMemory`. SVC dispatch
  and handlers therefore use `System::current_process_arc`, which is the
  frontend application process even while an applet thread is running. This
  replaces the applet's SVC registers with the application's registers and
  routes all SVC memory access to the wrong address space.
- Resume condition: resolve the current process and memory from the current
  thread's owner, use that process for SVC register save/load and handlers, add
  focused ownership regressions, then rebuild and rerun the applet.
- Status: completed. Current-process/current-memory lookup now follows the
  current thread owner, and SVC register save/load uses that same process.

## 2026-08-20 — interrupted Mii applet retest after current-process repair

- Interrupted slice: retest the Mii LLE applet after restoring upstream
  current-thread process ownership for SVC dispatch.
- Reproduction: after selecting a Mii, host `CPUCore_3` remains at 100% in
  `ServerManager::WaitSignaled` / `MultiWait::TimedWaitImpl`; profiling shows
  repeated wait-object resolution without service dispatch.
- Exact missing prerequisite: Eden `MultiWait::TimedWaitImpl` passes the native
  `KSynchronizationObject*` values owned by each `MultiWaitHolder` directly to
  `KSynchronizationObject::Wait`. Ruzu instead reduces them to numeric ids and
  resolves those ids through the current process object maps. A guest service
  process does not necessarily own registrations for every port/session held
  by its `ServerManager`, so resolution returns `ResultInvalidHandle`
  immediately and the service fiber busy-loops.
- Resume condition: preserve the holder's native synchronization-object owner
  through `MultiWait`, keep numeric process-table resolution only at SVC handle
  boundaries, add a focused direct-native-object regression, reread the three
  upstream owners, then rebuild and rerun the applet.

## 2026-08-20 — interrupted Mii LLE applet retest after native MultiWait repair

- Interrupted slice: retest the Mii LLE applet after restoring native
  synchronization-object ownership in `MultiWait`.
- Reproduction: the applet reaches `OpenDataStorageByCurrentProcess`, but FSP
  reports that no RomFS is available; the applet then enters its fatal path and
  its caller remains waiting for an applet result that can no longer arrive.
- Exact missing prerequisite: Eden `AppLoader_NCA::Load` registers every NCA
  process with a `RomFSFactory` built from that loader, the content provider,
  and the filesystem controller. Rust `loader/nca.rs` registers the process
  with `romfs_factory: None`; the later application-only replacement in
  `System::load` never runs for LLE library applets created by
  `AM::CreateProcess`.
- Resume condition: construct and register the upstream-owned `RomFSFactory`
  in `loader/nca.rs`, add a focused registration/controller regression, reread
  the upstream NCA loader and RomFS factory, then rebuild and rerun the applet.
- Status: prerequisite completed. `AppLoaderNca::load` now registers its
  process with the same loader-owned `RomFSFactory` inputs as Eden, and the
  controller regression confirms that `OpenProcess` exposes the registered
  RomFS to FSP.
- Runtime validation pending: rebuild and rerun the LLE applet to confirm it
  now completes instead of entering the fatal path.

## 2026-08-20 — interrupted Mii LLE applet retest after RomFS registration

- Interrupted slice: resume the Mii LLE applet after registering its NCA-owned
  RomFS factory.
- Reproduction: the GPU thread receives a command list whose uniform-buffer
  address belongs to the previously active application channel, but the Vulkan
  buffer cache is bound to the Mii applet channel and panics when translating
  the address in the applet memory manager.
- Exact missing prerequisite: `VideoGpuChannelHandle::init_channel` and
  `bind_memory_manager` directly call `RasterizerInterface::bind_channel`.
  Eden's `GPU::Impl::InitChannel` only initializes the channel and binds the
  rasterizer interface to its engines; `nvhost_as_gpu::BindChannel` only stores
  the memory manager. Only `Scheduler::Push` calls `GPU::BindChannel` before
  dispatching commands. The extra Rust calls change the rasterizer owner
  without updating `Gpu::bound_channel`, so the scheduler can incorrectly skip
  the next required bind.
- Resume condition: remove both out-of-order rasterizer binds, retain channel
  binding exclusively in `Gpu::bind_channel`, add focused lifecycle
  regressions, rebuild, and rerun the applet selection.
- Status: implementation and focused regressions completed. The Rust lifecycle
  now matches Eden: initialization creates the per-channel rasterizer state,
  while `Scheduler::push` is the sole command path that changes the active
  rasterizer channel through `Gpu::bind_channel`. Runtime validation pending.

## 2026-08-20 — interrupted Mii LLE applet teardown after GPU-channel repair

- Interrupted slice: resume the application after the Mii applet has rendered
  and returned the selected profile.
- Reproduction: the applet now progresses past the former uniform-buffer
  failure, then `HLE:nvservices` panics in
  `TextureCache::unmap_gpu_memory` because an address-space GPU page table still
  contains an `ImageId` whose `SlotVector` entry has already been erased.
- Exact missing prerequisite under investigation: preserve the owning GPU
  address-space table when unregistering an image during deferred
  `MemoryManager` rasterizer notifications. The current Rust implementation
  selects the table through the rasterizer's currently bound channel, while the
  notification itself identifies the memory manager being modified.
- Resume condition: prove the table-owner mismatch at unregister time, restore
  Eden-equivalent cleanup ordering/ownership, add a multi-address-space
  regression, reread the upstream texture-cache and memory-manager owners, then
  rebuild and rerun the applet teardown.
- Status: implementation and tests completed. Runtime tracing proved that
  application images registered in dense table 0 were being unregistered while
  the applet's dense table 2 was current. Each registered image now retains its
  dense GPU page-table owner until `UnregisterImage`, including the paired
  sparse table. Registration/unregistration flag, LRU, dense-table, map-view,
  and sparse-table ordering was also restored to Eden's order. All 1,465
  `video_core` tests pass (one ignored); runtime validation pending.

## 2026-08-20 — interrupted Mii LLE applet return after texture cleanup repair

- Interrupted slice: resume the application after creating a Mii and closing
  the LLE Mii editor.
- Reproduction: no panic occurs and guest processes remain alive, but the
  application waits indefinitely for its library-applet state-changed event
  after the Mii editor process terminates.
- Exact missing prerequisite: Eden's
  `WindowSystem::PruneTerminatedAppletsLocked` calls
  `Applet::OnProcessTerminatedLocked`, which both sets `is_completed` and
  signals `state_changed_event`. Ruzu's prune path only set `is_completed`, so
  a caller already waiting on `GetAppletStateChangedEvent` was never woken.
- Resume condition: restore the termination callback in the upstream-owned
  `applet.rs`, invoke it from `window_system.rs`, add a focused event regression,
  reread both upstream files, rebuild, and rerun the Mii return path.
- Status: implementation and upstream re-verification completed. The focused
  AM tests pass, including the new completion/event regression. The full
  `core` suite still has four independently reproducible pre-existing
  `k_process` failures; the unrelated parallel `k_server_session` abort passes
  when run alone. Runtime validation of the Mii return path is pending.

## 2026-08-20 — interrupted Mii output retrieval after process completion

- Interrupted slice: return the Mii editor's output to its caller after the
  applet process has completed and its state-changed event has fired.
- Reproduction: the applet reaches `PushOutData` and
  `ExitProcessAndReturn`; the observer removes the terminated process, but the
  caller remains in its applet transition while continuing to submit frames.
- Exact missing prerequisite: Eden's
  `ILibraryAppletAccessor::PopOutData` directly signals the caller lifecycle
  system event, requests its resume notification, clears that event, and
  updates the requested focus state before popping output. Rust omitted the
  complete sequence. Porting it requires the upstream-owned
  `LifecycleManager::GetSystemEvent` counterpart, which is also missing.
- Resume condition: add `LifecycleManager::get_system_event` in
  `lifecycle_manager.rs`, verify it against the upstream header/implementation,
  then port the exact `PopOutData` ordering in `library_applet_accessor.rs`,
  add a focused lifecycle regression, rebuild, and rerun the return path.
- Status: prerequisite and interrupted slice completed. The getter and the
  `PopOutData` ordering now match Eden, the focused regression passes, and the
  runtime trace confirms the sequence executes and returns 2008 bytes to the
  Mii editor. Runtime validation exposed the next independent lifecycle issue
  below.

## 2026-08-20 — interrupted teardown of a completed processless HLE applet

- Interrupted slice: prune the completed HLE software-keyboard child so the
  terminated LLE Mii editor can itself be finalized and return to the game.
- Reproduction: the keyboard completes, its caller retrieves its output, and
  `is_completed` is true; the keyboard was deliberately created with an
  uninitialized `Process`, however. When the Mii process terminates,
  `WindowSystem::PruneTerminatedAppletsLocked` sees one child, calls the
  no-op `Process::Terminate`, and waits forever because a processless applet
  can never satisfy `Process::IsTerminated`.
- Exact missing prerequisite: the C++ frontend path also creates a processless
  applet and `FrontendApplet::Exit` only marks it completed, so this is a
  confirmed upstream lifecycle hole rather than a missing guest-process port.
  Ruzu's explicit `FrontendApplet::is_complete` adaptation provides the
  completion state needed to close that hole without inventing a synthetic
  kernel process.
- Resume condition: let the upstream-owned prune path finalize an applet when
  either its real process is terminated or it has no process and is already
  completed; add a regression proving the processless applet is unlinked,
  reread the upstream window/process/frontend owners, rebuild, and rerun.

## 2026-08-20 — interrupted Mii database insertion after applet return

- Interrupted slice: resume the application after the LLE Mii editor creates
  and returns a new Mii.
- Reproduction: `AddOrReplace` is called, but the following database-only
  `Get` returns zero entries. The application then dereferences the absent Mii
  and performs an indirect call through a null vtable slot at guest PC zero.
- Exact missing prerequisite: Eden's `GetMiiAuthorId` replaces an invalid
  stored UUID with `Common::UUID::MakeDefault()` and marks the settings save as
  needed. Ruzu returned the all-zero UUID unchanged, while `MiiUtil` validated
  the resulting device checksum against a different default UUID. The LLE
  editor therefore produced a `StoreData` that the Mii database rejected.
  Ruzu's `UUID::make_default` also still contained the old yuzu value instead
  of Eden's upstream-owned `"Eden Default UID"` constant.
- Resume condition: restore Eden's default UUID bytes in `common/uuid.rs`,
  restore the invalid-ID initialization and save-needed ordering in
  `set/system_settings_server.rs`, add focused regressions, then rebuild and
  verify that `AddOrReplace` persists one entry and the caller resumes.

## 2026-08-20 — interrupted library-applet display-layer teardown

- Interrupted slice: resume the caller after a completed LLE library applet
  has returned its output.
- Reproduction: the caller receives the output and the window system prunes
  the terminated applet, but SurfaceFlinger continues composing two layers.
- Exact missing prerequisite: Eden's `KProcess::FinalizeHandleTable` closes
  every client-session handle. That releases `ISelfController`, whose
  destructor calls `DisplayLayerManager::Finalize` and destroys the applet's
  shared layer. Ruzu's `KHandleTable::finalize` only cleared numeric object
  identifiers; the process-owned Rust `Arc` session owners remained alive
  until whole-system shutdown.
- Resume condition: restore `KProcess` ownership of handle-table finalization,
  close process client sessions in upstream order, defer dropping their Rust
  owners until the process lock is released, add a focused lifecycle
  regression, then rebuild and rerun the applet return path.
- Status: implementation, focused lifecycle regression, upstream reread, and
  release build completed. Runtime validation of the Mii return path remains.

## 2026-08-20 — interrupted reply after applet client-session closure

- Interrupted slice: complete the active `ExitProcessAndReturn` IPC after the
  applet process closes its client-session handles.
- Reproduction: `KProcess::FinalizeHandleTable` now closes the applet's active
  `appletOE` session, but `ServerManager` panics because `SendReplyHLE` returns
  `ResultInvalidState` (`0xFA01`) instead of `ResultSessionClosed`.
- Exact missing prerequisite: Eden `KServerSession::OnClientClosed` preserves
  `m_current_request` while marking its terminating client thread unavailable;
  the subsequent `SendReplyHLE` consumes that request and returns
  `ResultSessionClosed`. Ruzu called `cleanup_requests`, which removed and
  finalized the active request before the reply path could consume it.
- Resume condition: port Eden's active-request preservation and pending-request
  ordering in `k_server_session.rs`, add a focused close-during-dispatch
  regression, rebuild, and rerun the applet return path.
- Status: implementation, focused regression, upstream reread, UI tests, and
  release build completed. Runtime validation remains.

## 2026-08-20 — interrupted ServerManager handling of the closed applet session

- Interrupted slice: let `appletOE` complete its active dispatch after
  `KServerSession::SendReplyHLE` correctly reports the closed client endpoint.
- Reproduction: the reply now returns kernel `ResultSessionClosed` (`0xF601`),
  but `ServerManager(appletOE)` still asserts because it compares the reply to
  IPC `ResultSessionClosed` instead of the kernel result.
- Exact missing prerequisite: Eden `CompleteSyncRequest` compares the
  `SendReplyHLE` result with `Kernel::ResultSessionClosed` and independently
  compares the service result with `IPC::ResultSessionClosed`. Both Ruzu event
  paths used the IPC constant for both values.
- Resume condition: use the kernel result for receive/reply results in both
  ServerManager paths, retain the IPC result for service dispatch, add the
  close-during-shared-dispatch regression, rebuild, and rerun.
- Status: both event paths now distinguish kernel and IPC session-closed
  results like Eden; the focused shared-dispatch regression and release build
  pass. Runtime validation remains.

## 2026-08-20 — interrupted ServerManager session-holder destruction

- Interrupted slice: resume the application after the completed Mii editor's
  display layer and client sessions are closed.
- Reproduction: after `SF_REMOVE_LAYER` removes the applet layer, the process
  exits with `SIGSEGV`. GDB shows the fault in
  `MultiWaitHolder::native_waitable_object` on the `HLE:audio` host thread.
- Exact missing prerequisite: Eden's `DestroySession` is reached only after
  `WaitSignaled` has unlinked the selected holder. Ruzu's additional
  `pending_session_closures` path can call `destroy_session` for a holder that
  is still linked to either `m_multi_wait` or `m_deferred_list`; dropping its
  `Box<MultiWaitHolder>` then leaves a dangling raw pointer in the wait list.
- Resume condition: make the Rust session destruction boundary unlink its
  holder before freeing the session, add regressions for destruction from both
  wait lists, reread Eden's `WaitSignaled`/`DestroySession` and MultiWait holder
  ownership, rebuild, and rerun the Mii return path.
- Status: the destruction boundary now restores Eden's already-unlinked
  invariant for both the ordinary and queued-close paths. Both wait-list
  regressions and the prior closed-reply regression pass, post-implementation
  upstream re-verification is complete, `ARCHI_CHOICES.md` documents the Rust
  adaptation, and the release build succeeds. Runtime validation remains.

## 2026-08-21 — interrupted real-VFS file-reference parity

- Interrupted slice: port Eden's retained `IOFile` references, LRU eviction, trait-level open/create
  operations, and directory-root checks in `core/file_sys/vfs/vfs_real.rs`.
- Exact missing prerequisite: Eden's `Common::FS::SanitizePath` resolves `.` and `..` components
  before `RealVfsDirectory::IsWithinRoot` runs. Ruzu's counterpart only normalized separators, so a
  focused root-escape regression still opened `root/../outside.bin`.
- Resume condition: port `SanitizePath` component resolution in its owning common module, verify it
  against Eden and update `DIFF.md`, then rerun the suspended VFS tests and finish its audit.
- Status: prerequisite implemented and verified; the VFS slice resumed and both retained-handle
  and root-escape regressions pass. Full `core` validation is currently red on three unrelated,
  independently reproducible `k_process` tests; the VFS-focused tests remain green.

## 2026-08-21 — interrupted rdynarmic warning cleanup on scalar FCMEQ parity

- Interrupted slice: classify and remove unused x64 vector fallback warnings after checking each
  emitter against Eden's Dynarmic implementation.
- Exact missing prerequisite: the unused `Eq` variants exposed that Eden's scalar A64
  `FCMEQ_reg_2` and `FCMEQ_zero_2` translators were present in Ruzu's decoder table but absent from
  both their upstream-owned translation files and `TranslatorVisitor::dispatch`; decoded
  single/double scalar equality instructions therefore fell through to interpretation.
- Resume condition: port both methods in their matching scalar translation files, restore their
  dispatch entries, add focused 32/64-bit translation regressions, reread the two upstream C++
  files and declarations, update `DIFF.md`, then resume the x64 fallback audit.
- Status: prerequisite ported and re-verified against both upstream implementation files and
  `impl.h`; focused single/double register and zero-comparison regressions pass. The x64 fallback
  warning audit can resume.

## 2026-08-21 — rdynarmic vector-arrangement validation limitation

- Validated slice: Eden-parity x64 narrow, sign-extension, and zero-extension emitters, including
  the corrected `VectorSignExtend64` high-lane sign mask and SSE2 host-feature fallbacks.
- Focused emitter tests and `cargo check -p rdynarmic --lib` pass.
- Full `cargo test -p rdynarmic --lib` is not currently a clean validation gate: the unchanged
  `a32_write_memory32_after_shift_and_add_emits_without_losing_address_value` test panics before
  reaching this slice because its A32 fastmem configuration has no fallback table. Several fuzz
  tests also continued past 60 seconds, so the run was stopped after recording the independent
  failure rather than left blocking indefinitely.

## 2026-08-21 — rdynarmic vector-basic slice interrupted by AVX immediate-shift prerequisite

- Interrupted slice: Eden parity for `VectorCountLeadingZeros8/16/32`,
  `VectorPopulationCount`, and `VectorReverseBits` in
  `src/rdynarmic/src/backend/x64/emit_vector_basic.rs`.
- Exact missing prerequisite: local `rxbyak` exposes legacy `psll*`/`psrl*` immediate forms and AVX
  register-count forms, but not the three-operand AVX immediate forms used by Eden's AVX CLZ16
  path (`vpsrlw dst, src, imm` and `vpsllw dst, src, imm`).
- Required next action: add and byte-test the missing encoders in `externals/rxbyak`, verify their
  VEX encodings, then resume the interrupted vector-basic slice. No temporary SSE-only shortcut is
  being retained.
- Status: prerequisite implemented in rxbyak commit `b0a6181`; XMM, YMM, and extended-register
  encodings match NASM byte-for-byte and the rxbyak all-target check passes. The vector-basic slice
  can now resume.

## 2026-08-21 — rdynarmic widening paired-add interrupted by AVX-512 prerequisite

- Status: prerequisite completed and verified; the interrupted emitter slice can resume.
- Interrupted slice: replace all six callback or non-upstream implementations of
  `VectorPairedAdd{Signed,Unsigned}Widen{8,16,32}` with Eden's native x64 instruction sequences.
- Exact missing prerequisite: Eden's signed 32-bit widening emitter selects an
  `AVX512_Ortho` path containing `vpsraq xmm, xmm, 32`, `vpsllq xmm, xmm, 32`, and
  `vpaddq xmm, xmm, xmm`. The local `rxbyak` generator already exposes `vpaddq` and the
  register-count form of `vpsllq`, but does not expose the immediate `vpsllq` overload or the
  EVEX-only immediate `vpsraq` form.
- Required prerequisite work: add the two immediate assembler operations in `externals/rxbyak`, verify
  their bytes against an independent assembler for ordinary and extended registers, run the
  rxbyak test suite, and record the encoder comparison in `DIFF.md`.
- Resume condition: the missing encoders pass byte-level tests and the nested rxbyak commit is
  recorded by the parent repository. No SSE-only replacement is permitted because it would drop
  Eden's host-feature branch.
- Prerequisite result: `vpsllq_imm` and `vpsraq_imm` now preserve Xbyak 7.35.2's exact encoding
  flags. XMM/YMM/ZMM and extended-register forms match NASM byte-for-byte, and the complete
  rxbyak test suite plus its all-target check pass.
- Resumed result: all six signed/unsigned 8/16/32-bit widening paired-add emitters now use Eden's
  native instruction sequences, including its AVX-512 and baseline SSE2 branches for signed
  32-bit lanes. The all-width JIT regression passes and `rdynarmic` reports only the intentionally
  ignored opcode naming warnings.

## 2026-08-21 — buffer-cache word manager interrupted by batched page-tracking prerequisite

- Interrupted slice: audit and remove the `word_manager.rs` unused-local warnings while restoring
  current Eden parity for `IterateWords`, `ChangeRegionState`, `ForEachModifiedRange`, and
  `FlushCachedWrites`.
- Exact missing prerequisite: current Eden coalesces changed page ranges and calls
  `DeviceMemoryManager::UpdatePagesCachedBatch`; Ruzu's `DeviceTracker` trait and
  `MaxwellDeviceMemoryManager` only expose the single-range update, so faithfully porting the word
  manager would otherwise require retaining the older notification behavior.
- Required next action: split `update_pages_cached_count` into the upstream-owned no-lock helper,
  port sorted/coalesced `update_pages_cached_batch` under one range lock, expose it through
  `DeviceTracker`, add focused coalescing and transition regressions, re-verify against Eden, then
  resume `word_manager.rs`.
- Status: prerequisite completed and re-verified. `update_pages_cached_count_no_lock` now owns the
  counter transitions, both public paths acquire the matching lock scope, and focused empty plus
  sorted/overlapping/adjacent batch regressions pass. The resumed word-manager slice now batches
  `ChangeRegionState`, `ForEachModifiedRange`, and `FlushCachedWrites`, uses Eden's helper
  boundaries, and passes focused cross-word coalescing regressions plus the full `video_core` test
  suite.

## 2026-08-22 — controller preview battery rendering interrupted by raw-value accessor prerequisite

- Interrupted slice: port Eden's player LED and battery rendering into
  `src/ruzu/src/configuration/controller_preview.rs`.
- Exact missing prerequisite: `ControllerStatus` already owns Eden's two raw `battery_values`, but
  `EmulatedController` does not expose the matching `GetBatteryValues()` accessor. Its existing
  `get_battery()` returns the converted HID-service `BatteryLevelState` instead and is not the
  frontend API used by Eden's preview.
- Required next action: add `get_battery_values()` to the matching hid-core frontend owner, add a
  focused snapshot regression, reread Eden's declaration and implementation, update `DIFF.md`,
  then resume the controller-preview slice.
- Status: prerequisite ported and re-verified against Eden's header and implementation. The raw
  two-device array is returned unchanged from the controller-owned status, and the focused
  left/right snapshot regression passes. The preview slice can resume.

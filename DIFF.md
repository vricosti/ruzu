## 2026-03-23 — core/src/hle/service/vi/application_display_service.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/vi/application_display_service.cpp

### Intentional differences
- CMIF/template-based upstream signatures are represented through `RequestParser`/`ResponseBuilder`: mechanical Rust porting adaptation while keeping method ownership in `application_display_service.rs`.
- Event lifetime cleanup from the upstream destructor is still modeled differently: Rust currently relies on process/session teardown rather than a full destructor-equivalent cleanup path for VI display-vsync events and open layers.

### Unintentional differences (to fix)
- `GetRelayService` and `GetIndirectDisplayTransactionService` still use the AM `push_interface_response` helper instead of a VI-local helper matching upstream ownership more directly.
- `ListDisplays`, `SetLayerScalingMode`, `GetIndirectLayerImageMap`, and several other commands remain simplified/stubbed relative to upstream behavior.
- `GetDisplayVsyncEvent` still creates an immediately signaled ad hoc event instead of matching upstream `ServiceContext` ownership and duplicate-fetch semantics.

### Missing items
- Upstream destructor parity for unlinking vsync events and closing/destroying all tracked layers.
- Upstream `GetIndirectLayerImageCropMap` and `GetDisplayVsyncEventForDebug`.
- Remaining behavioral parity for indirect layer and display management commands that are still stubbed in the Rust file.

### Binary layout verification
- PASS: this file does not define raw-serialized structs whose layout differs from upstream ownership; existing VI data structs remain in `vi_types.rs`.

## 2026-03-23 — core/src/hle/service/vi/vi.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/vi/vi.cpp

### Intentional differences
- Rust service registration uses closure-based `ServerManager::register_named_service` in place of upstream templated service registration: mechanical adaptation while preserving root service ownership.
- `vi.rs` now exposes a shared-container accessor for AM to reach the same VI container instance: Rust-only plumbing added because the current service framework does not yet provide an upstream-equivalent typed `ServiceManager().GetService<...>()` path.

### Unintentional differences (to fix)
- `vi.rs` is still a registration coordinator only; upstream also has additional surrounding service-loop details that may still differ in lifecycle/logging behavior.
- The new shared-container accessor is broader than upstream ownership and should be replaced once AM can fetch typed VI services through the service manager like upstream.

### Missing items
- Line-by-line parity audit for any remaining service-loop behavior outside named service registration.

### Binary layout verification
- PASS: registration-only file with no raw-serialized layout concerns.

## 2026-03-23 — core/src/hle/service/am/service/application_proxy_service.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/application_proxy_service.cpp

### Intentional differences
- Rust derives the caller PID from `HLERequestContext` and the current thread when the raw CMIF helper path does not surface the upstream typed `ClientProcessId`/`InCopyHandle<Kernel::KProcess>` pair directly: mechanical IPC adaptation while preserving method ownership in `application_proxy_service.rs`.

### Unintentional differences (to fix)
- Rust still does not pass the real `KProcess` object into `IApplicationProxy`/`ISelfController`; it only reuses the tracked `Applet` and caller PID, so full upstream process ownership parity is not restored yet.

### Missing items
- Upstream-equivalent propagation of the process handle from `OpenApplicationProxy` into the constructed `IApplicationProxy`.

### Binary layout verification
- PASS: service-dispatch file with no raw-serialized structs introduced by this change.

## 2026-03-23 — core/src/hle/service/am/service/self_controller.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/self_controller.cpp

### Intentional differences
- Rust uses `Drop` to mirror the upstream destructor and initialize/finalize `DisplayLayerManager` around the service object's lifetime.

### Unintentional differences (to fix)
- `ISelfController` still lacks the upstream `Kernel::KProcess* m_process` ownership and therefore cannot forward the exact process object into display-layer operations.
- Many upstream commands in this file remain absent or stubbed; this change only restores the `DisplayLayerManager` lifecycle and `CreateManagedDisplayLayer` ownership.

### Missing items
- Upstream process ownership in the `ISelfController` constructor.
- Remaining unported/self-stubbed commands, including the full shared-buffer path.

### Binary layout verification
- PASS: no raw-serialized structs are defined in this file.

## 2026-03-23 — core/src/hle/service/am/display_layer_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/display_layer_manager.cpp

### Intentional differences
- Rust currently stores the shared VI `Container` directly instead of upstream `IApplicationDisplayService` and `IManagerDisplayService` objects: temporary plumbing to preserve behavior until typed VI service acquisition is available.
- `CreateManagedDisplayLayer` uses the tracked applet ARUID directly rather than an upstream `KProcess*` call to `GetProcessId()`: equivalent bit-pattern for the currently used managed-layer creation path.

### Unintentional differences (to fix)
- `IsSystemBufferSharingEnabled` and `GetSystemSharedLayerHandle` remain simplified and do not create shared-layer sessions like upstream.
- `Finalize` cannot destroy shared-layer sessions because the upstream manager-display-service session object is still missing.
- `WriteAppletCaptureBuffer` is still unported.

### Missing items
- Upstream acquisition of `IApplicationDisplayService` and `IManagerDisplayService` during `Initialize`.
- Shared-layer session creation/destruction and applet capture buffer support.

### Binary layout verification
- PASS: this file manages runtime state only and does not define raw-serialized structs.

## 2026-03-23 — common/src/fiber.rs vs /Users/vricosti/Dev/emulators/zuyu/src/common/fiber.cpp

### Intentional differences
- Linux still uses `ucontext` instead of upstream `boost::context::detail::fcontext_t`: existing Rust backend retained, while method ownership and guard/order semantics stay in `fiber.rs`.
- macOS uses `corosensei` as the stack-switching backend because macOS ARM64 does not provide `getcontext`/`makecontext`/`swapcontext`: this preserves same-thread stack switching without introducing thread-per-fiber scheduling.
- The macOS backend uses a dispatcher inside `Fiber::yield_to` to route `YieldTo` requests through `corosensei`'s parent-yield model: this is a mechanical adaptation to recover upstream arbitrary fiber-to-fiber transfer semantics while keeping `Start`, `Rewind`, `YieldTo`, and `Exit` owned by `fiber.rs`.
- On macOS, the pre-rewind coroutine remains stored in `context` while the active post-rewind coroutine is stored in `rewind_context` and selected first on dispatch: upstream discards the old `fcontext_t` immediately, but the retained suspended coroutine is not resumed again and exists only as backend bookkeeping.

### Unintentional differences (to fix)
- `FiberImpl` field layout on macOS is backend-oriented (`context`/`rewind_context` coroutines plus dispatcher state) instead of mirroring upstream `stack_limit`/`rewind_stack_limit` more directly.
- Full crate validation is still incomplete because `cargo test -p common` currently reaches an unrelated `elf` test `SIGBUS` after the fiber tests pass, so this change has only been validated with the focused fiber tests.

### Missing items
- Re-audit whether the retained suspended pre-rewind coroutine on macOS can be safely dropped/reset earlier without violating upstream lifecycle semantics.
- Full `cargo test -p common` green run once the unrelated `elf` test crash is resolved.

### Binary layout verification
- PASS: `fiber.rs` defines runtime control-flow state only and no raw-serialized structs.

## 2026-03-23 — yuzu_cmd/src/emu_window/emu_window_sdl2.rs vs /Users/vricosti/Dev/emulators/zuyu/src/yuzu_cmd/emu_window/emu_window_sdl2.cpp

### Intentional differences
- Temporary SDL startup diagnostics were added around `SDL_Init` to log available video drivers and post-init driver/display information while investigating the macOS Cocoa startup failure. This is debug-only instrumentation in the Rust counterpart file and does not change ownership.
- Rust still uses a simplified fixed title string in `update_title_bar` instead of the upstream perf-stats title formatting because the surrounding frontend/perf wiring is not yet ported.
- `input_subsystem->Initialize()` and the destructor shutdown path remain stubbed/unported in Rust because the corresponding input subsystem ownership is not yet present in this frontend file.

### Unintentional differences (to fix)
- The macOS startup failure currently reproduces before SDL video initialization completes even though SDL reports the `cocoa` driver as available. Upstream does not hit this failure on the same frontend path, so additional platform-specific parity work is still required.
- Rust does not yet port upstream methods and behaviors including `SDLButtonToMouseButton`, `MouseToTouchPos`, `OnMouseButton`, `ShowCursor`, `Fullscreen`, `SetWindowIcon`, and the perf-stat title update path.
- The Rust file does not yet own the upstream `input_subsystem` and `system` state, so event handling and destructor behavior are materially incomplete relative to upstream.

### Missing items
- Port the remaining upstream helper and lifecycle methods listed above into `emu_window_sdl2.rs`.
- Revisit whether the temporary SDL diagnostics should be kept behind debug logging only or removed once the macOS display-init issue is resolved.

### Binary layout verification
- PASS: frontend window runtime state only; no raw-serialized structs are defined here.

## 2026-03-23 — core/src/hle/service/am/applet_manager.rs vs /Users/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/applet_manager.cpp

### Intentional differences
- Rust stores the `WindowSystem` as `Arc<Mutex<WindowSystem>>` and the pending process as `Arc<Mutex<KProcess>>` instead of upstream raw/shared ownership types. This is a mechanical ownership adaptation for the current Rust service architecture.
- Rust does not yet port the upstream `Core::System& m_system`-driven frontend applet launch-parameter setup in this file, so only the core applet creation/foreground handoff path is mirrored here.

### Unintentional differences (to fix)
- Rust still does not port the upstream frontend input-data setup for `QLaunch`, `Cabinet`, `MiiEdit`, `PhotoViewer`, `SoftwareKeyboard`, and `Controller` before tracking the new applet.
- `launch_type == ApplicationInitiated` handling is still missing; upstream swaps the user-channel launch parameter from `System`.
- Rust still does not mirror the upstream `applet->process->Run()` ownership in `SetWindowSystem`; process startup remains outside this file in the Rust core path.
- `RequestExit` and `OperationModeChanged` still call through an `Arc<Mutex<WindowSystem>>` clone while holding the AppletManager lock, rather than following the upstream raw-pointer/reference model exactly.

### Missing items
- Port the remaining upstream launch-parameter helpers and `ApplicationInitiated` user-channel transfer into `applet_manager.rs`.
- Reconcile process-run ownership so `SetWindowSystem` matches upstream more closely.

### Binary layout verification
- PASS: runtime state only; no raw-serialized structs are defined in this file.

## 2026-03-23 — core/src/hle/kernel/k_process.rs vs /Users/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_process.cpp

### Intentional differences
- Rust still uses `Arc<Mutex<...>>` ownership and a separate `GlobalSchedulerContext` thread list instead of upstream intrusive/object-lifetime registration. The registration order in `run()` was adjusted so the new main thread is visible to the Rust scheduler structures before it is published runnable.

### Unintentional differences (to fix)
- The Rust `run()` path still directly sets `highest_priority_thread_id` on the per-core scheduler instead of relying on the full upstream `OnThreadStateChanged` / scheduler-update flow.
- Some process startup responsibilities remain split across Rust files (`AppletManager`, `System`, `KProcess`) more than upstream, even though scheduler visibility now lives in `k_process.rs`.

### Missing items
- Reconcile the remaining scheduler-update path with upstream `KScheduler::OnThreadStateChanged` ownership.

### Binary layout verification
- PASS: runtime/process state only; no raw-serialized structs are defined in this slice.

## 2026-03-23 — core/src/core.rs vs /Users/vricosti/Dev/emulators/zuyu/src/core/core.cpp

### Intentional differences
- `register_application_thread()` is a Rust-only helper used after `AppletManager::set_window_system()` calls `KProcess::run()`. It no longer owns scheduler registration; it only stores the application thread pointer in `KernelCore` and wakes core 0 so the idle loop can reschedule.

### Unintentional differences (to fix)
- Upstream does not need a separate `register_application_thread()` hook at all; the remaining helper exists because Rust process startup is still not fully collapsed into the same ownership boundaries as upstream.

### Missing items
- Remove or further reduce the Rust-only helper if kernel/application-thread bookkeeping can be fully absorbed into the upstream-equivalent startup owner.

### Binary layout verification
- PASS: orchestration/runtime state only; no raw-serialized structs are defined in this slice.

## 2026-03-23 — core/src/arm/dynarmic/arm_dynarmic_32.rs vs /Users/vricosti/Dev/emulators/zuyu/src/core/arm/dynarmic/arm_dynarmic_32.cpp

### Intentional differences
- Rust uses the local `rdynarmic` callback surface and `SystemRef` plumbing instead of upstream Dynarmic C++ classes and direct `Core::System&` references. This is a mechanical ownership/backend adaptation.

### Unintentional differences (to fix)
- Several upstream debugger/watchpoint branches are still simplified or absent in Rust.
- `exclusive_clear` remains a no-op until the exclusive monitor wiring is finished.

### Missing items
- Remaining parity work for debugger-enabled exception/watchpoint behavior and fully wired exclusive monitor semantics.

### Binary layout verification
- PASS: runtime/JIT callback state only; no raw-serialized structs are defined here.

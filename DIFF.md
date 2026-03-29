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

## 2026-03-23 — core/src/hle/service/vi/application_root_service.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/vi/application_root_service.cpp

### Intentional differences
- Rust uses `RequestParser`/`ResponseBuilder` and trait-object session creation instead of upstream CMIF templates and `SharedPointer`: mechanical IPC adaptation while preserving ownership in `application_root_service.rs`.
- Invalid raw policy values now return `ResultPermissionDenied` instead of relying on an upstream typed-enum deserialization path. This is a defensive Rust adaptation to avoid invalid enum construction.

### Unintentional differences (to fix)
- `GetDisplayServiceWithProxyNameExchange` remains unported/stubbed, matching the current upstream null handler registration but still missing behavior.
- The constructed `IApplicationDisplayService` still depends on the broader Rust VI/session plumbing, which may differ from upstream lifecycle semantics.

### Missing items
- Full parity for command 1 (`GetDisplayServiceWithProxyNameExchange`).

### Binary layout verification
- PASS: service-dispatch file with no raw-serialized struct definitions.

## 2026-03-23 — core/src/hle/service/vi/system_root_service.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/vi/system_root_service.cpp

### Intentional differences
- Rust uses `RequestParser`/`ResponseBuilder` and trait-object session creation instead of upstream CMIF templates and `SharedPointer`: mechanical IPC adaptation while preserving ownership in `system_root_service.rs`.
- Invalid raw policy values now return `ResultPermissionDenied` instead of relying on an upstream typed-enum deserialization path. This is a defensive Rust adaptation to avoid invalid enum construction.

### Unintentional differences (to fix)
- `GetDisplayServiceWithProxyNameExchange` remains unported/stubbed, matching the current upstream null handler registration but still missing behavior.
- The returned display-service session still depends on the broader Rust VI/session plumbing, which may differ from upstream lifecycle semantics.

### Missing items
- Full parity for command 3 (`GetDisplayServiceWithProxyNameExchange`).

### Binary layout verification
- PASS: service-dispatch file with no raw-serialized struct definitions.

## 2026-03-23 — core/src/hle/service/vi/manager_root_service.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/vi/manager_root_service.cpp

### Intentional differences
- Rust uses `RequestParser`/`ResponseBuilder` and trait-object session creation instead of upstream CMIF templates and `SharedPointer`: mechanical IPC adaptation while preserving ownership in `manager_root_service.rs`.
- Invalid raw policy values now return `ResultPermissionDenied` instead of relying on an upstream typed-enum deserialization path. This is a defensive Rust adaptation to avoid invalid enum construction.

### Unintentional differences (to fix)
- `GetDisplayServiceWithProxyNameExchange` remains unported/stubbed, matching the current upstream null handler registration but still missing behavior.
- The returned display-service session still depends on the broader Rust VI/session plumbing, which may differ from upstream lifecycle semantics.

### Missing items
- Full parity for command 3 (`GetDisplayServiceWithProxyNameExchange`).

### Binary layout verification
- PASS: service-dispatch file with no raw-serialized struct definitions.

## 2026-03-23 — core/src/hle/service/vi/vi_types.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/vi/vi_types.h

### Intentional differences
- Rust adds `Policy::from_raw(u32)` as a narrow parsing helper so IPC owners can decode the raw request word without unsafe enum transmute. This keeps the policy constants in the upstream owner file.

### Unintentional differences (to fix)
- `vi_types.rs` still contains only the subset of upstream VI type definitions needed by the current port; a full parity audit of every upstream type in `vi_types.h` remains incomplete.

### Missing items
- Remaining upstream VI type definitions not yet represented in the Rust counterpart file.

### Binary layout verification
- PASS: existing `repr(C)` layout assertions remain intact; `Policy::from_raw` does not affect binary layout.

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

## 2026-03-28 — core/src/hle/kernel/kernel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.h and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_hardware_timer.cpp

### Intentional differences
- Rust adds `get_current_hardware_tick()` as a narrow helper over the existing global `KERNEL_PTR` so lower kernel owners can query the current hardware tick without pushing timeout conversion logic back into SVC files. This is mechanical plumbing to preserve upstream timeout ownership in `KThread` and `KConditionVariable`.

### Unintentional differences (to fix)
- `KernelCore` still exposes global-pointer based access patterns that do not line up one-for-one with upstream method ownership. The helper only narrows an existing divergence; it does not remove it.

### Missing items
- Full parity audit of the remaining `KernelCore` global access helpers versus upstream call sites.

### Binary layout verification
- PASS: helper-only change; no raw-serialized structs affected.

## 2026-03-28 — core/src/hle/kernel/k_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.h

### Intentional differences
- Positive timeout ticks are translated to host `Instant` deadlines through a small local helper because the current Rust scheduler models wakeups with `Instant` rather than upstream `KHardwareTimer` tasks at every call site. This preserves upstream ownership of timeout consumption in `k_thread.rs`.
- When no kernel/hardware timer is initialized (unit tests / bootstrap-only paths), the helper falls back to treating the positive timeout as a relative duration. This is a Rust test-environment adaptation until every focused test path has a wired kernel timer.

### Unintentional differences (to fix)
- `wait_park_mutex` / `wait_park_cv` remain in the struct even though `BeginWait` now matches upstream and no longer parks the host thread. They are leftover Rust-only state.
- `BeginWait`/`EndWait`/`CancelWait` still rely on the current Rust scheduler and queue plumbing, which remains only behaviorally close to upstream rather than line-for-line equivalent.

### Missing items
- Remove obsolete host-thread parking fields once no remaining callers/tests depend on them.
- Full parity audit of all wait/cancel paths that still use Rust-specific queue helpers.

### Binary layout verification
- PASS: `KThread` is not serialized by raw memory copy here; this change only affects runtime wait semantics.

## 2026-03-28 — core/src/hle/kernel/k_condition_variable.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.h

### Intentional differences
- Positive timeout ticks are translated to host `Instant` deadlines through a local helper because the Rust scheduler currently wakes sleepers from `Instant` deadlines instead of driving every condition-variable wait through upstream `KHardwareTimer` ownership.
- When no kernel/hardware timer is initialized, the helper falls back to relative-duration behavior for test-only paths.

### Unintentional differences (to fix)
- `wait_locked` and `begin_wait_condition_variable` still model timeout wakeup through `sleep_deadline` polling rather than upstream timer-task registration on the wait queue.
- The Rust implementation still carries lock-transfer and process-lookup adaptations that do not yet map one-for-one to upstream raw pointer ownership.

### Missing items
- Full upstream-equivalent timer-task ownership for condition-variable waits.
- Re-audit of every cancel/end-wait path after the `BeginWait` ownership fix in `k_thread.rs`.

### Binary layout verification
- PASS: no raw-serialized structs changed.

## 2026-03-28 — core/src/hle/kernel/svc/svc_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/svc/svc_thread.cpp

### Intentional differences
- Rust factors the positive-sleep timeout conversion into `sleep_timeout_tick_from_ns(current_tick, ns)` instead of spelling it inline in `sleep_thread()`: this is a mechanical extraction within the same owner file to make the upstream absolute-tick formula directly testable.

### Unintentional differences (to fix)
- `sleep_thread()` still falls back to `i64::MAX` when the kernel or hardware timer is unavailable. Upstream always has a live `KernelCore`/`KHardwareTimer` on this path.
- The non-positive yield paths still rely on the current Rust scheduler helpers and are not yet a full line-by-line parity port of upstream `KScheduler::Yield*`.

### Missing items
- Full line-by-line parity audit for the remaining SVC thread commands in this file, especially the scheduler/yield helpers and resource-limit reservation paths.

### Binary layout verification
- PASS: service handler file only; no raw-serialized structs changed.

### Binary layout verification
- PASS: `fiber.rs` defines runtime control-flow state only and no raw-serialized structs.

## 2026-03-23 — yuzu_cmd/src/emu_window/emu_window_sdl2.rs vs /Users/vricosti/Dev/emulators/zuyu/src/yuzu_cmd/emu_window/emu_window_sdl2.cpp

### Intentional differences
- Temporary SDL startup diagnostics were added around `SDL_Init` to log available video drivers and post-init driver/display information while investigating the macOS Cocoa startup failure. This is debug-only instrumentation in the Rust counterpart file and does not change ownership.
- Rust still uses a simplified fixed title string in `update_title_bar` instead of the upstream perf-stats title formatting because the surrounding frontend/perf wiring is not yet ported.
- `input_subsystem->Initialize()` and the destructor shutdown path remain stubbed/unported in Rust because the corresponding input subsystem ownership is not yet present in this frontend file.
- Rust now skips the title-bar refresh once a close request has marked the window not open. This is a defensive divergence to avoid SDL/X11 calls on a window that the window manager may already have started destroying.

### Unintentional differences (to fix)
- The macOS startup failure currently reproduces before SDL video initialization completes even though SDL reports the `cocoa` driver as available. Upstream does not hit this failure on the same frontend path, so additional platform-specific parity work is still required.
- Rust does not yet port upstream methods and behaviors including `SDLButtonToMouseButton`, `MouseToTouchPos`, `OnMouseButton`, `ShowCursor`, `Fullscreen`, `SetWindowIcon`, and the perf-stat title update path.
- The Rust file does not yet own the upstream `input_subsystem` and `system` state, so event handling and destructor behavior are materially incomplete relative to upstream.

### Missing items
- Port the remaining upstream helper and lifecycle methods listed above into `emu_window_sdl2.rs`.
- Revisit whether the temporary SDL diagnostics should be kept behind debug logging only or removed once the macOS display-init issue is resolved.
- Revisit whether the post-close title-bar guard can be removed once the remaining SDL/X11 close-path behavior is fully matched to upstream.

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

## 2026-03-23 — core/src/arm/dynarmic/arm_dynarmic_32.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/arm/dynarmic/arm_dynarmic_32.cpp

### Intentional differences
- Rust passes the process-owned `DynarmicExclusiveMonitor` into `DynarmicCallbacks32` as a raw pointer so the `CLREX` callback can clear per-core exclusive state in the same file that owns the Dynarmic callback implementation. This preserves upstream ownership while adapting to Rust trait-object callbacks.
- Rust adds env-gated unmapped-access PC logging (`RUZU_LOG_UNMAPPED_ACCESS_PC`), optional A32 GPR dumps (`RUZU_LOG_UNMAPPED_ACCESS_REGS`), and an optional one-shot guest code window dump (`RUZU_DUMP_UNMAPPED_CODE_PATH`) in `DynarmicCallbacks32` using the JIT-owned register state already exposed by rdynarmic. This is temporary diagnostic instrumentation in the upstream owner file for the MK8D AArch32 crash path.

### Unintentional differences (to fix)
- Fixed in this pass: `ArmDynarmic32::get_context()` now mirrors upstream by copying all A32 GPRs `r0..r15` into `ctx.r[0..15]` and by setting `ctx.fp` from `r11`. The previous Rust port incorrectly stopped at `r14` and left `ctx.fp` stale, which could corrupt any save/restore path that consumed the captured thread context.
- Other debugger/watchpoint branches in this file remain simplified relative to upstream.
- The callback still uses Rust-side shared-memory plumbing instead of upstream direct `Core::System`/`KProcess` references.

### Missing items
- Remaining upstream debugger/watchpoint and exception-path parity not covered by this `CLREX` fix.

### Binary layout verification
- PASS: runtime/JIT callback state only; no raw-serialized structs are defined here.

## 2026-03-23 — core/src/arm/dynarmic/arm_dynarmic_64.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/arm/dynarmic/arm_dynarmic_64.cpp

### Intentional differences
- Rust passes the process-owned `DynarmicExclusiveMonitor` into `DynarmicCallbacks64` as a raw pointer so the `CLREX` callback can clear per-core exclusive state in the same file that owns the Dynarmic callback implementation. This preserves upstream ownership while adapting to Rust trait-object callbacks.

### Unintentional differences (to fix)
- Other debugger/watchpoint branches in this file remain simplified relative to upstream.
- The callback still uses Rust-side shared-memory plumbing instead of upstream direct `Core::System`/`KProcess` references.

### Missing items
- Remaining upstream debugger/watchpoint and exception-path parity not covered by this `CLREX` fix.

### Binary layout verification
- PASS: runtime/JIT callback state only; no raw-serialized structs are defined here.

## 2026-03-23 — yuzu_cmd/src/main.rs vs /Users/vricosti/Dev/emulators/zuyu/src/yuzu_cmd/yuzu.cpp

### Intentional differences
- Rust still uses `clap` and a temporary `--renderer` CLI override because the upstream `getopt_long`/settings path is not fully ported. This is frontend plumbing outside the exit/shutdown ownership slice.

### Unintentional differences (to fix)
- Disk shader cache loading and debugger attach/detach still do not match the surrounding upstream `yuzu.cpp` flow.

### Missing items
- Remaining `yuzu.cpp` parity outside the shutdown/exit path, including the unported debugger and disk-cache branches.

### Binary layout verification
- PASS: frontend runtime/orchestration only; no raw-serialized structs are defined here.

## 2026-03-23 — core/src/cpu_manager.rs vs /Users/vricosti/Dev/emulators/zuyu/src/core/cpu_manager.cpp

### Intentional differences
- Rust still does not create upstream-style per-core shutdown threads in `KernelCore`; instead, the CPU loops detect `System::is_shutting_down()` and explicitly yield the current guest fiber back to the host fiber via `CpuManager::shutdown_thread(...)`. This preserves the same shutdown destination while using the current Rust fiber ownership.

### Unintentional differences (to fix)
- The shutdown trigger still originates from `System::is_shutting_down()` polling inside the CPU loops rather than from upstream kernel-owned shutdown-thread scheduling.

### Missing items
- Port upstream kernel-owned shutdown-thread creation/scheduling so `CpuManager` no longer needs to poll `System::is_shutting_down()`.

### Binary layout verification
- PASS: runtime/thread orchestration only; no raw-serialized structs are defined here.

## 2026-03-23 — core/src/hle/kernel/kernel.rs vs /Users/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.cpp

### Intentional differences
- Rust now exposes `suspend_emulation()` and `shutdown_cores()` in the upstream ownership location (`kernel.rs`), but these currently operate only on `System::current_process_arc` rather than the upstream full kernel process list. This matches the current Rust runtime, which only tracks the frontend-loaded application process.

### Unintentional differences (to fix)
- `SuspendEmulation` does not yet implement the upstream wait-until-no-process-thread-is-running loop.
- `ShutdownCores` does not yet create or run upstream-style per-core shutdown threads under a scheduler lock.
- `CloseServices` remains a no-op because server-manager tracking is not fully wired into `KernelCore`.

### Missing items
- Port the remaining upstream process-list ownership and shutdown-thread scheduling into `kernel.rs`.

### Binary layout verification
- PASS: runtime/kernel orchestration only; no raw-serialized structs are defined here.

## 2026-03-23 — core/src/core.rs vs /Users/vricosti/Dev/emulators/zuyu/src/core/core.cpp

### Intentional differences
- Rust now calls `kernel.suspend_emulation(...)`, `kernel.close_services()`, and `kernel.shutdown_cores()` from `System::run()`, `System::pause()`, and `System::shutdown_main_process()` in the upstream ownership order. The remaining divergence is inherited from the still-incomplete kernel implementation documented above.

### Unintentional differences (to fix)
- `shutdown_main_process()` still omits several upstream subsystems (`gpu_core->NotifyShutdown()`, socket cancellation/restart, cheat engine, debugger) because those shutdown hooks are not yet fully wired in the Rust port.

### Missing items
- Complete the remaining upstream teardown calls once the corresponding subsystems exist with matching ownership.

### Binary layout verification
- PASS: orchestration/runtime state only; no raw-serialized structs are defined in this slice.
## 2026-03-23 — core/src/hle/kernel/physical_core.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/physical_core.cpp

### Intentional differences
- Rust adds env-gated step tracing (`RUZU_LOG_STEP_INTERVAL`) in `run_loop()` to inspect guest PC/instruction progress after enabling `RUZU_STEP_AFTER_SVC`, and env-gated post-SVC logging (`RUZU_LOG_AFTER_SVC`) in `dispatch_supervisor_call()` to inspect the guest context immediately after handoff from kernel SVC handling: temporary diagnostic instrumentation in the upstream owner file.

### Unintentional differences (to fix)
- Upstream does not include this diagnostic path; remove or gate it more tightly once the CPU-side stall is understood.

### Missing items
- None for functional parity; this entry only documents temporary diagnostic instrumentation.

### Binary layout verification
- PASS: runtime control-flow file; no raw-serialized structs affected.

## 2026-03-23 — core/src/hle/kernel/svc/svc_ipc.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/svc/svc_ipc.cpp

### Intentional differences
- Rust adds temporary diagnostic logging of the resolved session handler name in `SendSyncRequest` so the failing MK8D IPC path can be mapped back to its owning service file. The instrumentation stays in the upstream owner file.

### Unintentional differences (to fix)
- Upstream does not emit this extra handler-name log line; remove or tighten the instrumentation once the offending IPC owner is identified.

### Missing items
- None for functional parity; this entry only documents temporary diagnostic instrumentation.

### Binary layout verification
- PASS: SVC/IPC dispatch file only; no raw-serialized structs are defined here.

## 2026-03-23 — core/src/hle/service/hle_ipc.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hle_ipc.cpp

### Intentional differences
- Rust still carries temporary MK8D diagnostic instrumentation elsewhere (`svc_ipc.rs`, `physical_core.rs`, `arm_dynarmic_32.rs`), but this file now matches the upstream `WriteToOutgoingCommandBuffer` write length by copying only `write_size` words back into TLS.

### Unintentional differences (to fix)
- Fixed in this pass: `write_to_outgoing_command_buffer()` previously wrote the entire `COMMAND_BUFFER_LENGTH` back to TLS instead of the upstream `write_size * sizeof(u32)` range, which could clobber adjacent TLS state after IPC replies.
- Remaining domain/session helper ownership and object-lifecycle details in this file still need broader parity review against upstream.

### Missing items
- Full parity audit of the remaining helper paths outside this TLS write-length fix.

### Binary layout verification
- PASS: command-buffer/TLS serialization only; no new raw struct layouts introduced.

## 2026-03-23 — core/src/arm/arm_interface.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/arm/arm_interface.h

### Intentional differences
- Rust keeps `ThreadContext` in the ARM owner module instead of importing the kernel header directly, to avoid a Rust module cycle between the ARM interface trait and `KThread`.

### Unintentional differences (to fix)
- Fixed in this pass: the Rust `ThreadContext` mirror had drifted from upstream `Kernel::Svc::ThreadContext` by using `r[31]` and omitting the explicit padding word after `pstate`. It now matches the upstream kernel layout again with `r[29]`, `fp`, `lr`, `sp`, `pc`, `pstate`, `padding`, vectors, FP status, and `tpidr`.

### Missing items
- None in this slice beyond the ARM backend parity gaps already tracked elsewhere.

### Binary layout verification
- PASS: `ThreadContext` now matches upstream `Kernel::Svc::ThreadContext` field order and size expectations.

## 2026-03-23 — core/src/hle/kernel/k_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.h

### Intentional differences
- Rust still keeps a local `ThreadContext` mirror in the owning kernel file for dependency reasons, but the mirror is now structurally aligned with upstream and remains `#[repr(C)]` for the existing kernel/ARM handoff code.

### Unintentional differences (to fix)
- Fixed in this pass: `KThread::thread_context` had the same structural drift as the ARM copy (`r[31]` plus missing explicit padding), which made the documented “same layout as ArmThreadContext” claim false relative to upstream and unsafe for the context handoff cast paths.

### Missing items
- The broader `KThread` behavioral gaps already documented elsewhere remain unchanged.

### Binary layout verification
- PASS: `thread_context` now matches upstream `Svc::ThreadContext` field layout in the owner file.

## 2026-03-23 — core/src/arm/debug.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/arm/debug.cpp

### Intentional differences
- Symbolication is still partial in Rust because module enumeration is not fully wired; this file still falls back to raw addresses when symbol data is unavailable.

### Unintentional differences (to fix)
- Fixed in this pass: `get_backtrace()` now copies the explicit `padding` word when reconstructing an ARM `ThreadContext` view from `KThread::thread_context`, so the temporary debug context matches the corrected upstream layout.

### Missing items
- Module enumeration and symbol lookup remain partial.

### Binary layout verification
- PASS: this file only reconstructs the already-corrected `ThreadContext` layout; it defines no new raw payloads.

## 2026-03-23 — core/src/hle/kernel/k_process.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_process.cpp

### Intentional differences
- Rust still uses the existing `map_pages_find_free` / cooperative thread bootstrap path in `run()` because the full upstream `MapPages`/cleanup scaffolding is not ported yet, but the resource-limit ownership and release points now live in the correct upstream owner file and follow the same lifecycle.

### Unintentional differences (to fix)
- Fixed in this pass: `KProcess::FinishTermination()` was releasing the `PhysicalMemoryMax` hint with `code_size + main_thread_stack_size` instead of upstream `GetUsedNonSystemUserPhysicalMemorySize()`, which broke the shutdown resource accounting.
- Fixed in this pass: `KProcess::Finalize()` was also releasing only `code_size + main_thread_stack_size` instead of the upstream non-system user physical memory total, so its release path no longer matched the hint path.
- Fixed in this pass: `KProcess::Run()` was missing the upstream `KScopedResourceReservation` for the main-thread stack (`PhysicalMemoryMax, stack_size`), which left resource-limit accounting short by the stack reservation and caused the close/shutdown panic path.
- Fixed in this pass: `KProcess::Run()` was also missing the upstream tentative `ThreadCountMax` reservation for the main thread before thread creation.

### Missing items
- `KProcess::Run()` still does not mirror the upstream cleanup scaffolding (`ON_RESULT_FAILURE` unmap path and adjacent ownership details) one-for-one.
- The broader process lifecycle gaps already tracked elsewhere remain unchanged.

### Binary layout verification
- PASS: no raw serialized structs changed in this pass; the fix is lifecycle/resource accounting only.

## 2026-03-23 — core/src/hle/kernel/k_page_table_base.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_page_table_base.cpp

### Intentional differences
- Rust still uses the existing `KScopedResourceReservation` port and page-table helpers already present in this file rather than the exact upstream helper types/macros, but the heap/resource-limit ownership remains in the upstream owner file and follows the same lifecycle ordering.

### Unintentional differences (to fix)
- Fixed in this pass: `get_normal_memory_size()` previously returned only `GetSizeByState(KMemoryState::Normal)` instead of the upstream `(m_current_heap_end - m_heap_region_start) + m_mapped_physical_memory_size`, which made process memory accounting and resource-limit releases too small.
- Fixed in this pass: `set_heap_size()` was missing the upstream `PhysicalMemoryMax` reservation on heap growth, so growing the process heap consumed physical memory without matching the resource-limit accounting.
- Fixed in this pass: `set_heap_size()` was also missing the upstream `PhysicalMemoryMax` release on heap shrink, so heap teardown could later release more memory than had ever been hinted.

### Missing items
- The surrounding `SetHeapSize` cleanup/error-path details still need a broader parity audit against upstream.
- Other `KPageTableBase` behavioral gaps outside this heap/resource slice remain unchanged.

### Binary layout verification
- PASS: no raw serialized structs changed in this pass; the fix is page-table/resource accounting only.

## 2026-03-23 — core/src/arm/dynarmic/arm_dynarmic_32.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/arm/dynarmic/arm_dynarmic_32.cpp

### Intentional differences
- Rust now defaults A32 JIT optimizations to `0x3E` instead of the upstream "all safe optimizations" set, leaving `BLOCK_LINKING` disabled by default. This is a temporary backend workaround in the upstream owner file because the current `rdynarmic` A32 block-linking path miscompiles the MK8D `vi:m -> GetDisplayService` return sequence and immediately triggers guest null-pointer writes.
- The env overrides `RUZU_A32_OPTIMIZATION_MASK` and `RUZU_A32_NO_OPTIMIZATIONS` remain available for targeted backend validation.

### Unintentional differences (to fix)
- Upstream enables `BLOCK_LINKING`; Rust cannot do that yet without reproducing the MK8D AArch32 crash path. The root issue is in the current `rdynarmic` A32 backend, not in the upstream `ArmDynarmic32` owner logic.
- Other debugger/watchpoint branches in this file remain simplified relative to upstream.

### Missing items
- Fix `rdynarmic` A32 block linking so this owner file can return to the upstream default optimization set.
- Remaining upstream debugger/watchpoint and exception-path parity not covered by this workaround.

### Binary layout verification
- PASS: runtime/JIT callback state only; no raw-serialized structs changed in this pass.
## 2026-03-24 — core/src/arm/dynarmic/arm_dynarmic_32.rs vs zuyu/src/core/arm/dynarmic/arm_dynarmic_32.cpp

### Intentional differences
- Added `RUZU_A32_TRACE_RANGE_START` / `RUZU_A32_TRACE_RANGE_END` / `RUZU_A32_TRACE_LIMIT` / `RUZU_A32_TRACE_SEARCH_LIMIT` gated tracing in `run_thread()`: temporary debug-only instrumentation to capture post-IPC A32 register flow around the MK8D fault window without affecting normal execution when unset.

### Unintentional differences (to fix)
- None in this instrumentation-only slice.

### Missing items
- Root-cause fix for the MK8D post-`vi:m::GetDisplayService` bad guest state remains unresolved.

### Binary layout verification
- PASS: no shared serialized struct layout changed in this slice.

## 2026-03-24 — core/src/hle/service/nvnflinger/hos_binder_driver_server.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/hos_binder_driver_server.cpp

### Intentional differences
- Rust keeps the binder registry under `Mutex<HashMap<...>>` instead of upstream `std::unordered_map` plus `std::mutex`: mechanical ownership adaptation in the same owner file.

### Unintentional differences (to fix)
- Fixed in this pass: binder IDs now start at `1`, matching upstream `last_id++` semantics instead of incorrectly returning `0` for the first binder registration.

### Missing items
- None in this slice beyond the broader binder infrastructure gaps already tracked elsewhere.

### Binary layout verification
- PASS: runtime map only; no raw-serialized structs are defined here.

## 2026-03-24 — core/src/hle/service/nvnflinger/hos_binder_driver.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/hos_binder_driver.cpp

### Intentional differences
- Rust exposes `as_any()` on the owner service so service-to-service lookups can recover the typed `IHosBinderDriver` instance, mirroring upstream typed `shared_ptr<IHOSBinderDriver>` retrieval through a Rust trait object.

### Unintentional differences (to fix)
- `GetNativeHandle` remains stubbed and still returns a synthetic readable event instead of the binder-owned native handle object.

### Missing items
- Full parity for `GetNativeHandle`.

### Binary layout verification
- PASS: service/runtime file only; no raw-serialized structs are defined here.

## 2026-03-24 — core/src/hle/service/nvnflinger/nvnflinger.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/nvnflinger.cpp

### Intentional differences
- Rust keeps the `Nvnflinger` helper owner struct and now adds `loop_process(system)` beside it, so the upstream owner file both constructs the shared binder stack and registers `"dispdrv"` through `ServerManager`.

### Unintentional differences (to fix)
- Fixed in this pass: `"dispdrv"` is no longer registered as a generic stub in `services.rs`; the real `IHosBinderDriver` owner is now registered from the nvnflinger owner file.

### Missing items
- None for this constructor/registration slice.

### Binary layout verification
- PASS: service-registration/runtime file only; no raw-serialized structs are defined here.

## 2026-03-24 — core/src/hle/service/vi/container.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/vi/container.cpp

### Intentional differences
- Rust still omits the full `SharedBufferManager` initialization because the supporting nvdrv/shared-buffer plumbing is not yet complete; this remains a tracked subsystem gap in the same owner file.
- Rust uses `ServiceManager::get_service_blocking(..., Duration::from_secs(5))` instead of upstream's unbounded blocking service fetch. This is a defensive host-side timeout for the current sequential service launcher.

### Unintentional differences (to fix)
- Fixed in this pass: `Container::new()` now obtains the shared `"dispdrv"` binder driver from the global `ServiceManager` and reuses its `HosBinderDriverServer` / `SurfaceFlinger`, instead of incorrectly constructing a private binder stack disconnected from the upstream owner service.
- `on_terminate()` still only marks shutdown and does not yet remove all layers/displays from `SurfaceFlinger` as upstream does.

### Missing items
- Full `SharedBufferManager` parity.
- Upstream `OnTerminate()` display/layer teardown ordering.
- `GetLayerProducerHandle`.

### Binary layout verification
- PASS: container/service owner state only; no raw-serialized structs are defined here.

## 2026-03-24 — core/src/hle/service/services.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/services.cpp

### Intentional differences
- Rust still executes service loops sequentially instead of spawning detached host/guest service threads. To preserve the upstream `VI -> dispdrv` dependency under that sequential fallback, `nvnflinger` is launched before `vi` in this file even though upstream starts `vi` on a detached host-core process and `nvnflinger` later on a guest-core process.

### Unintentional differences (to fix)
- Fixed in this pass: the nvnflinger slice no longer registers `"dispdrv"` as a stub; it now launches the real `nvnflinger::loop_process(system)` owner.

### Missing items
- Replace this sequencing workaround once the service launcher mirrors upstream detached process/thread behavior.

### Binary layout verification
- PASS: orchestration file only; no raw-serialized structs are defined here.

## 2026-03-24 — ../rdynarmic/src/frontend/a32/translate/data_processing.rs vs /home/vricosti/Dev/emulators/zuyu/externals/dynarmic/src/dynarmic/frontend/A32/translate/impl/data_processing.cpp

### Intentional differences
- Rust keeps the opcode-family dispatch in a shared `arm_dp_common(...)` helper instead of one upstream visitor method per encoding. This remains a mechanical ownership compromise already present in the file.

### Unintentional differences (to fix)
- Fixed in this pass: arithmetic/test flag updates produced by `get_nzcv_from_op(...)` were being written through `set_cpsr_nzcv_raw(...)`, incorrectly treating dynarmic's internal x64-format NZCV as raw ARM-format NZCV. Upstream uses `SetCpsrNZCV(ir.NZCVFrom(result))`; the Rust port now matches that semantic contract.

### Missing items
- The file still does not have one Rust function per upstream visitor method, so line-by-line ownership parity remains weaker than upstream.

### Binary layout verification
- PASS: translation file only; no raw-serialized structs are defined here.

## 2026-03-24 — ../rdynarmic/src/frontend/a32/translate/thumb16.rs vs /home/vricosti/Dev/emulators/zuyu/externals/dynarmic/src/dynarmic/frontend/A32/translate/impl/thumb16.cpp

### Intentional differences
- Rust groups several Thumb16 translator routines in one owner file rather than mirroring every upstream visitor method as a separate C++ member function. This is an existing structural compromise.

### Unintentional differences (to fix)
- Fixed in this pass: Thumb16 arithmetic/compare translators were also routing `NZCVFrom(result)` through `set_cpsr_nzcv_raw(...)` instead of `set_cpsr_nzcv(...)`, corrupting the internal condition-flag format used by conditional branches.

### Missing items
- Full one-method-per-upstream-visitor structural parity remains incomplete.

### Binary layout verification
- PASS: translation file only; no raw-serialized structs are defined here.

## 2026-03-24 — ../rdynarmic/src/frontend/a32/translate/thumb32.rs vs /home/vricosti/Dev/emulators/zuyu/externals/dynarmic/src/dynarmic/frontend/A32/translate/impl/thumb32_data_processing_modified_immediate.cpp and /home/vricosti/Dev/emulators/zuyu/externals/dynarmic/src/dynarmic/frontend/A32/translate/impl/thumb32_data_processing_shifted_register.cpp

### Intentional differences
- Rust still merges the relevant Thumb32 data-processing visitor families into one file-level dispatcher instead of separate upstream translation units.

### Unintentional differences (to fix)
- Fixed in this pass: Thumb32 arithmetic/test flag updates now store `NZCVFrom(result)` through `set_cpsr_nzcv(...)`, matching upstream's internal-flag contract instead of mis-converting it as raw ARM NZCV.

### Missing items
- Structural split parity with the upstream Thumb32 translation units remains incomplete.

### Binary layout verification
- PASS: translation file only; no raw-serialized structs are defined here.
## 2026-03-24 — ../rdynarmic/src/backend/x64/block_of_code.rs vs upstream dynarmic/src/dynarmic/backend/x64/block_of_code.cpp

### Intentional differences
- Rust tests construct `RunCodeCallbacks` inline: mechanical adaptation of upstream C++ test scaffolding.

### Unintentional differences (to fix)
- None after this change.

### Missing items
- No new runtime items in this pass.

### Binary layout verification
- PASS: test-only callback initialization; no serialized layout affected.

## 2026-03-24 — ../rdynarmic/src/backend/x64/a64_emit_x64.rs vs upstream dynarmic/src/dynarmic/backend/x64/a64_emit_x64.cpp

### Intentional differences
- Rust unit tests use a local `make_test_callbacks()` helper rather than upstream's C++ fixture style.

### Unintentional differences (to fix)
- None after this change.

### Missing items
- No new runtime items in this pass.

### Binary layout verification
- PASS: test-only callback initialization; no serialized layout affected.

## 2026-03-24 — ../rdynarmic/src/tests_a32_fuzz.rs vs upstream dynarmic tests

### Intentional differences
- Rust keeps the fuzz harness in a native test module instead of upstream's exact C++ test harness layout.

### Unintentional differences (to fix)
- The `JitConfig` initializer lagged behind the new `fastmem_pointer` field and no longer matched the runtime config shape.

### Missing items
- No new runtime items in this pass.

### Binary layout verification
- PASS: test-only config initialization; no serialized layout affected.
## 2026-03-24 — ../rdynarmic/src/backend/x64/a32_emit_x64.rs vs upstream dynarmic/src/dynarmic/backend/x64/a32_emit_x64.cpp

### Intentional differences
- Rust keeps the same A32 translation/emission ownership as upstream; this pass removes temporary local instrumentation, mirrors upstream's local `gpr_order` construction by starting from the generic GPR list and removing `R13` only when `fastmem_pointer` is active, and temporarily logs the full IR block if `emit_block` panics so the remaining regalloc mismatch can be diagnosed.

### Unintentional differences (to fix)
- None after this change.

### Missing items
- A remaining regalloc mismatch still exists when `BLOCK_LINKING` is re-enabled; the temporary panic logging is there to isolate the exact failing block before removing it again.

### Binary layout verification
- PASS: logging removal only; no layout impact.

## 2026-03-24 — core/src/hle/kernel/svc_dispatch.rs vs core/hle/kernel/svc.cpp

### Intentional differences
- Rust SVC dispatch remains centralized in one file as part of the existing port shape; this pass only removes temporary high-volume tracing.

### Unintentional differences (to fix)
- None after this change.

### Missing items
- No new SVC parity items addressed in this pass.

### Binary layout verification
- PASS: logging removal only; no layout impact.

## 2026-03-24 — ../rdynarmic/src/backend/x64/emit_data_processing.rs vs upstream dynarmic/src/dynarmic/backend/x64/emit_x64_data_processing.cpp

### Intentional differences
- Rust keeps the shared data-processing emitters in one Rust module instead of several C++ methods on `EmitX64`, but method ownership remains aligned by opcode responsibility.
- Rust unit tests use native `#[test]` helpers and dummy callbacks instead of upstream's C++ fixture style.

### Unintentional differences (to fix)
- `UnsignedDiv32`, `UnsignedDiv64`, `SignedDiv32`, and `SignedDiv64` previously forced the dividend into `RAX` too early instead of matching upstream's `ScratchGpr(RAX/RDX)` then arbitrary-reg dividend/divisor flow. This caused the A32 `0x3f` MK8D regalloc panic under block-linking pressure. Fixed in this pass.

### Missing items
- No new division-emission parity gaps identified in this pass.

### Binary layout verification
- PASS: emitter-only control-flow change; no serialized layout affected.

## 2026-03-24 — ../rdynarmic/src/backend/x64/a32_emit_x64.rs vs upstream dynarmic/src/dynarmic/backend/x64/a32_emit_x64.cpp

### Intentional differences
- Rust keeps the upstream-local `gpr_order` construction and removes `R13` only when `fastmem_pointer` is active, matching the upstream ownership of A32 fastmem register reservation.

### Unintentional differences (to fix)
- Temporary panic logging used to isolate the remaining A32 regalloc mismatch has been removed after the `UnsignedDiv32` parity fix; no known divergence remains from that diagnostic pass.

### Missing items
- No new `a32_emit_x64` ownership gaps identified in this pass.

### Binary layout verification
- PASS: logging cleanup only; no layout impact.

## 2026-03-28 — core/src/hle/service/ns/platform_service_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/ns/platform_service_manager.h and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/ns/platform_service_manager.cpp

### Intentional differences
- Rust keeps the upstream file ownership in `platform_service_manager.rs`, but loads baked font byte arrays from `file_sys::system_archive::data` instead of traversing NAND NCAs / synthesized RomFS files at runtime. This preserves the service-local ownership while diverging from the literal upstream content-loading path.
- Rust lazily creates and registers a process-visible `KSharedMemory` handle on first `GetSharedMemoryNativeHandle` call, instead of copying into `kernel.GetFontSharedMem()` and returning that kernel-owned object. This is a temporary Rust adaptation to the current kernel/shared-memory ownership shape.

### Unintentional differences (to fix)
- Upstream defines `SHARED_FONTS` with 7 source files, including `nintendo_ext2_003.bfttf`. Rust currently builds only 6 regions and has no counterpart for the second extension font source.
- Upstream stores decrypted shared-font blobs with an 8-byte header and returns regions as `offset + 8` / `size - 8`. Rust currently copies prebuilt font bytes directly into shared memory and returns offsets/sizes for that direct layout, so the backing blob layout is not byte-for-byte upstream-identical.

### Missing items
- No Rust counterpart yet for the upstream helper functions `DecryptSharedFontToTTF`, `EncryptSharedFont`, `DecryptSharedFont`, and `GetU32Swapped`; the current port bypasses those helpers because it does not reconstruct the upstream encrypted BFTTF layout.
- No parity test yet proving that `GetSharedMemoryNativeHandle` maps a guest-visible region whose bytes and reported offsets/sizes match upstream expectations.

### Binary layout verification
- FAIL: `FontRegion` itself matches the simple upstream `{u32 offset; u32 size;}` shape, but the shared-font backing blob layout does not yet match the upstream encrypted-header layout byte-for-byte.

## 2026-03-28 — core/src/hle/service/ns/ns.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/ns/ns.cpp

### Intentional differences
- Rust uses `ServerManager::new(system)` plus closure-based registration because `register_named_service` expects factories, whereas upstream directly passes `std::shared_ptr` instances.

### Unintentional differences (to fix)
- `pdm:qry` is still registered as `GenericStubService`, while upstream registers `IQueryService`.

### Missing items
- `IServiceGetterInterface` instances are still constructed without the `(system, service_name)` ownership shape used upstream.

### Binary layout verification
- PASS: service registration file only; no serialized structs are defined here.

## 2026-03-28 — core/src/arm/dynarmic/arm_dynarmic_32.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/arm/dynarmic/arm_dynarmic_32.cpp

### Intentional differences
- Rust still routes the callback through the existing `ArmDynarmic32` owner struct instead of storing upstream's exact `m_debugger_enabled` boolean in `DynarmicCallbacks32`; debugger-disabled behavior is currently hard-wired because debugger parity is still incomplete.

### Unintentional differences (to fix)
- Fixed in this pass: the default `ExceptionRaised` path was halting execution on `BKPT`/other non-`NoExecuteFault` exceptions. Upstream only halts when the debugger is enabled; otherwise it logs the backtrace and critical message, then returns to execution.

### Missing items
- The exact upstream debugger-enabled path still is not wired: Rust does not yet save `breakpoint_context` and return `InstructionBreakpoint` conditionally based on a real debugger-enabled flag.

### Binary layout verification
- PASS: control-flow change only; no serialized layout affected.

## 2026-03-28 — core/src/core.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/core.h and /home/vricosti/Dev/emulators/zuyu/src/core/core.cpp

### Intentional differences
- Rust still flattens upstream `System::Impl` into one `System` struct. The `run_server()` method added in this pass restores the same ownership chain (`ServerManager -> System -> KernelCore`) even though the Pimpl boundary is not represented literally.

### Unintentional differences (to fix)
- None newly identified in the `System::RunServer` slice after this pass.

### Missing items
- No broader `System::Impl` parity audit was completed in this pass beyond the `RunServer` ownership path.

### Binary layout verification
- PASS: ownership/control-flow change only; no serialized layout affected.

## 2026-03-28 — core/src/hle/kernel/kernel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.cpp

### Intentional differences
- Rust tracks active service servers as `Mutex<Vec<Arc<Mutex<ServerManager>>>>` instead of upstream's `std::vector<std::unique_ptr<Service::ServerManager>>`. This preserves kernel ownership while adapting to Rust shared ownership for shutdown signaling.

### Unintentional differences (to fix)
- Fixed in this pass: `KernelCore` previously had no counterpart for upstream `RunServer()` ownership. `ServerManager::run_server()` spawned an unrelated host thread directly instead of transferring ownership into the kernel.
- Fixed in this pass: `KernelCore::run_server()` incorrectly called `run_on_guest_core_process()` again, creating a second guest thread and letting the original service thread return immediately. Upstream pushes the manager into `server_managers` and calls `manager->LoopProcess()` inline on the current guest service thread.
- `close_services()` previously did nothing, while upstream destroys tracked server managers during kernel shutdown. Rust now requests stop on all tracked managers, but still does not wait for them to finish as strictly as upstream destruction does.

### Missing items
- Rust still lacks the exact upstream shutdown semantics where destroying each `ServerManager` waits for `LoopProcess()` to stop before the owner is released.

### Binary layout verification
- PASS: kernel ownership/control-flow change only; no serialized layout affected.

## 2026-03-28 — core/src/hle/service/server_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/server_manager.h and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/server_manager.cpp

### Intentional differences
- Rust keeps session/port tracking in Rust collections and helper wrappers instead of upstream intrusive lists / `MultiWaitHolder` inheritance. This is an existing structural adaptation, not introduced by this pass.
- Rust still lacks a full counterpart to upstream `MultiWait::WaitAny(m_system.Kernel())`, so this pass adds a guest-kernel readable wakeup event to block the current service thread instead of leaving it permanently runnable. This preserves the upstream ownership slice in `server_manager.rs` while remaining a temporary adaptation.
- Rust also has to trigger a scheduler fiber yield immediately after entering this wait, because unlike the upstream `WaitAny()` path there is no surrounding SVC/interrupt exit to reschedule automatically.

### Unintentional differences (to fix)
- Fixed in this pass: `ServerManager::run_server()` no longer owns its own host-thread spawn. It now follows the upstream ownership chain through `System::run_server()` and `KernelCore::run_server()`.
- Fixed in this pass: the old no-event path kept guest service threads runnable via `rotate_scheduled_queue + schedule_raw_if_needed`, which does not match upstream `WaitSignaled()` behavior and produced permanent runnable churn on MK8D service cores. Rust now sleeps the guest service thread on a kernel-readable wakeup event and signals that event from the same `register_session` / deferred-link / stop paths that wake the service-layer event loop.
- `wait_and_process_impl()` still scans sessions/events in Rust order instead of delegating selection to upstream `WaitSignaled()` with `m_multi_wait.WaitAny(m_system.Kernel())`.
- `OnPortEvent` / `Process` / `WaitSignaled` ownership is still not ported line-for-line; the event loop remains an approximation around the same responsibilities.

### Missing items
- No Rust counterpart yet for the full upstream `WaitSignaled()` / `Process(MultiWaitHolder*)` split with `UserDataTag`-driven dispatch.
- No Rust counterpart yet for upstream `Port` as a real `KServerPort` waitable inside `m_multi_wait`; server-session and port wait integration is still partial.
- `StartAdditionalHostThreads()` remains a stub and does not mirror upstream behavior.

### Binary layout verification
- PASS: service manager control-flow change only; no serialized layout affected.
## 2026-03-28 — core/src/hle/kernel/k_thread.rs vs zuyu/src/core/hle/kernel/k_thread.h / k_thread.cpp

### Intentional differences
- `LockWithPriorityInheritanceInfo` stores waiter ordering in a `BTreeSet` and owner identity as `thread_id`: Rust ownership replaces upstream intrusive containers and raw pointers while keeping the same per-thread ownership boundary.

### Unintentional differences (to fix)
- `RestorePriority` is still simplified: upstream walks the full owner chain and reorders waiters around both held locks and condvar trees, while Rust still uses `restore_priority_simplified()`.

### Missing items
- Full upstream-equivalent `RestorePriority(KernelCore&, KThread*)`.
- Full intrusive-tree parity for lock waiter ordering updates during priority changes.

### Binary layout verification
- PASS: this change did not modify raw serialized/thread-context payload layout.

## 2026-03-28 — core/src/hle/kernel/k_condition_variable.rs vs zuyu/src/core/hle/kernel/k_condition_variable.cpp

### Intentional differences
- `ThreadTree` is represented with `BTreeSet` + `HashMap` instead of the upstream intrusive red-black tree; ordering still follows `(cv_key, priority, thread_id)`.
- `UpdateLockAtomic` still uses process-memory serialization instead of the upstream exclusive-monitor CAS loop. This remains an existing documented deviation.

### Unintentional differences (to fix)
- `Signal`/`Wait` behavior still needs end-to-end confirmation against MK8D's `SignalProcessWideKey -> Break(0,0,0)` path; the current parity fixes were necessary but not sufficient.

### Missing items
- Real exclusive-monitor-based `UpdateLockAtomic`.
- End-to-end validation that condvar wake/owner transfer semantics fully match upstream on the MK8D bootstrap path.

### Binary layout verification
- PASS: no raw byte layout or IPC payload struct changed in this slice.

## 2026-03-28 — core/src/hle/kernel/kernel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.cpp

### Intentional differences
- Rust still creates the guest-core service `KProcess` with the existing lightweight `KProcess::new()` path instead of a full counterpart to upstream `process->Initialize(CreateProcessParameter{}, GetSystemResourceLimit(), false)`. This preserves file ownership in `kernel.rs` but the service-process initialization slice is still incomplete.

### Unintentional differences (to fix)
- Fixed in this pass: `KernelCore::run_on_guest_core_process()` wrapped the service closure and forced `ThreadState::TERMINATED` directly after `func()` returned. Upstream does not own exit semantics in `kernel.cpp`; `KThread::InitializeServiceThread()` owns the `OnThreadStart() -> func() -> ExitThread()` lambda in `k_thread.cpp`.
- Fixed in this pass: `KernelCore::run_on_guest_core_process()` made the service thread runnable before `register_thread_object()`. Upstream orders this as `InitializeServiceThread -> KThread::Register -> Run`, so Rust could previously dispatch a service thread before self-reference/process registration existed.
- Fixed in this pass: `KernelCore::run_on_guest_core_process()` created the guest service `KProcess` in a local `Arc` but did not retain kernel ownership after setup. Upstream calls `KProcess::Register(*this, process)`, so the Rust `Weak<KProcess>` stored by service threads could otherwise dangle as soon as the setup function returned.
- Rust still does not reserve `ThreadCountMax` before service-thread initialization the way upstream `KScopedResourceReservation thread_reservation(process, LimitableResource::ThreadCountMax)` does.

### Missing items
- Full counterpart to upstream service-process initialization and `KScopedResourceReservation` commit flow in `RunOnGuestCoreProcess`.
- Rust still does not maintain a real upstream-like registered process table; `service_processes` is a targeted ownership retention vector, not a full `KProcess::Register` parity implementation.

### Binary layout verification
- PASS: ordering/ownership change only; no serialized layout affected.

## 2026-03-28 — core/src/hle/kernel/k_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.cpp

### Intentional differences
- Rust passes `&Arc<Mutex<KThread>>` into `initialize_service_thread()` so the method can own the upstream `GlobalSchedulerContext::AddThread(thread)` step from the matching Rust file. This preserves method ownership in `k_thread.rs` while adapting pointer semantics to `Arc<Mutex<_>>`.

### Unintentional differences (to fix)
- Fixed in this pass: the upstream `GlobalSchedulerContext().AddThread(thread)` step lived in `kernel.rs` instead of `k_thread.rs`, so service-thread global-scheduler registration was owned by the wrong file.
- `InitializeServiceThread` still does not mirror upstream line-for-line around `InitializeThread(...)` because the Rust service-thread path is layered over the existing host-fiber constructor rather than a unified `InitializeThread` helper.

### Missing items
- Full line-for-line parity for the shared `InitializeThread(...)` helper path used by `InitializeUserThread`, `InitializeHighPriorityThread`, and `InitializeServiceThread` upstream.

### Binary layout verification
- PASS: service-thread ownership/control-flow change only; no raw layout changed.
## 2026-03-28 — `core/src/hle/kernel/k_synchronization_object.rs` vs `src/core/hle/kernel/k_synchronization_object.cpp`

### Intentional differences
- Rust keeps `SynchronizationWaitSet` / process object-ID indirection instead of upstream raw `KSynchronizationObject**`: mechanical adaptation to the current object registry layout.

### Unintentional differences (to fix)
- Prior to this pass, `Wait(...)` returned `RESULT_SUCCESS` immediately after `BeginWait` instead of resuming only after the wait completed. Fixed.
- Prior to this pass, the Rust callers kept the `KProcess` mutex locked across the whole blocked wait path, preventing wake/signal paths from reacquiring process state. Fixed by moving process locking inside the preparation slice only.

### Missing items
- Full upstream `KScopedSchedulerLockAndSleep` structure is still not ported literally; the Rust path still uses existing scheduler/timer helpers.

### Binary layout verification
- PASS: no raw byte layout affected in this pass.

## 2026-03-28 — `core/src/hle/kernel/k_scheduler.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_scheduler.cpp`

### Intentional differences
- Rust still stores scheduler thread references as `Arc<Mutex<KThread>>` / `Weak<Mutex<KThread>>` and uses host `Fiber` wrappers instead of upstream raw `KThread*` and `Common::Fiber` references. This is the existing ownership adaptation for the Rust scheduler port.

### Unintentional differences (to fix)
- Fixed in this pass: `schedule_impl_fiber()` returned early when `highest_priority_thread_id == None`. Upstream `KScheduler::ScheduleImpl()` still arms the switch fiber in that case and lets `ScheduleImplFiber()` fall back to `m_idle_thread`; the Rust early return skipped the idle-thread handoff entirely.

### Missing items
- Full line-for-line parity for `ScheduleImpl()` / `ScheduleImplFiber()` is still incomplete, especially around interrupt-task handling and the exact `DisableDispatch`/`EnableDispatch` sequencing.

### Binary layout verification
- PASS: control-flow only; no serialized or raw struct layout changed.

## 2026-03-28 — `core/src/hle/kernel/svc/svc_synchronization.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/svc/svc_synchronization.cpp`

### Intentional differences
- Rust still resolves handles through the existing process object-ID registries instead of upstream `GetMultipleObjects<KSynchronizationObject>()`. This is the current ownership adaptation of the process object tables.

### Unintentional differences (to fix)
- Fixed in this pass: `WaitSynchronization` forwarded positive `timeout_ns` values directly to `k_synchronization_object::wait()`. Upstream converts positive nanosecond timeouts to absolute hardware ticks as `current_tick + timeout_ns + 2` before calling `KSynchronizationObject::Wait(...)`.

### Missing items
- Full upstream-equivalent `GetMultipleObjects<KSynchronizationObject>()` handle resolution path instead of rebuilding object IDs in Rust.

### Binary layout verification
- PASS: timeout conversion/control-flow only; no raw layout changed.

## 2026-03-28 — `core/src/hle/kernel/k_condition_variable.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.cpp`

### Intentional differences
- Rust still uses `Arc<Mutex<KProcess>>` / `Arc<Mutex<KThread>>` and a `BTreeSet`-backed tree instead of upstream raw pointers and intrusive RB-tree nodes. This is the existing ownership adaptation for the kernel thread/process registry.
- `UpdateLockAtomic` still uses serialized process-memory access instead of the upstream exclusive-monitor CAS loop. This remains documented debt until the exclusive monitor is wired into this owner file.

### Unintentional differences (to fix)
- Fixed in this pass: `WaitForAddress(...)` returned `RESULT_SUCCESS` immediately after `BeginWait(...)`. Upstream returns only after the wait completes and then propagates `cur_thread->GetWaitResult()`.
- Fixed in this pass: positive-timeout `Wait(...)` pre-seeded `wait_result = ResultSuccess` before the wait completed. Upstream does not set the wait result speculatively before `BeginWait(...)`.

### Missing items
- Full literal counterpart to upstream `KScopedSchedulerLockAndSleep` / `wait_queue.SetHardwareTimer(timer)` ownership inside this file. The Rust port still relies on the existing scheduler/timer helpers layered around the owner methods.
- Exclusive-monitor-backed `UpdateLockAtomic`.

### Binary layout verification
- PASS: no serialized layout or raw payload struct changed in this slice.

## 2026-03-28 — `core/src/hle/kernel/k_process.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_process.cpp`

### Intentional differences
- Rust `KProcess::wait_condition_variable(...)` still wraps the owner `KConditionVariable` through `std::mem::take(&mut self.cond_var)` to satisfy Rust borrowing rules while keeping condition-variable ownership in `k_condition_variable.rs`.

### Unintentional differences (to fix)
- Fixed in this pass: `KProcess::wait_condition_variable(...)` returned the immediate setup result from `KConditionVariable::wait_locked(...)` instead of scheduling away and returning only after the current thread's wait completed, unlike the upstream `KConditionVariable::Wait(...)` call chain.

### Missing items
- Full upstream-equivalent scheduler-lock RAII around the `KConditionVariable::Wait(...)` call chain remains incomplete; the Rust wrapper still uses the existing scheduler request + wait loop adaptation.

### Binary layout verification
- PASS: control-flow only; no raw or serialized layout changed.

## 2026-03-28 — `core/src/hle/kernel/k_synchronization_object.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_synchronization_object.cpp`

### Intentional differences
- Rust still represents waited objects through process object IDs and `SynchronizationWaitSet` instead of upstream raw `KSynchronizationObject**`. This remains the current ownership adaptation for the Rust handle/object registry.

### Unintentional differences (to fix)
- Fixed in this pass: on the guest-thread path, `wait(...)` called `KScheduler::schedule_raw_if_needed(...)` once and then returned immediately even if the current thread was still `WAITING`. Upstream `KSynchronizationObject::Wait(...)` only returns after the waiting thread is resumed and `thread->GetWaitResult()` is finalized.

### Missing items
- Full literal `KScopedSchedulerLockAndSleep` parity is still not present; the Rust path still uses the existing scheduler/timer plumbing instead of the upstream RAII helper object.
- The older stack-local `KSynchronizationObject::wait(_kernel, out_index, objects, timeout)` stub higher in this file is still dead compatibility code and should eventually be removed once no callers remain.

### Binary layout verification
- PASS: wait-control-flow change only; no raw payload or serialized layout changed.

## 2026-03-28 — `/home/vricosti/Dev/emulators/rdynarmic/src/frontend/a32/translate/thumb16.rs` vs `/home/vricosti/Dev/emulators/zuyu/externals/dynarmic/src/dynarmic/frontend/A32/translate/impl/thumb16.cpp`

### Intentional differences
- Rust keeps the Thumb16 translators in a single `thumb16.rs` owner file instead of the upstream monolithic `thumb16.cpp`, but the owner boundary still matches the upstream Thumb16 translation unit.

### Unintentional differences (to fix)
- Fixed in this pass: `thumb16_blx_reg()` wrote `LR` before `BXWritePC(target)`. Upstream orders these as `PushRSB`, `UpdateUpperLocationDescriptor`, `BXWritePC(GetRegister(m))`, then `SetRegister(LR, return_addr)`. The old Rust order was observably wrong when `m == LR`, because the branch target consumed the newly written return address instead of the original `LR`.

### Missing items
- No new missing item identified in this slice beyond the broader Thumb16 translation parity still tracked elsewhere.

### Binary layout verification
- PASS: IR/control-flow only; no raw payload or serialized layout changed.

## 2026-03-28 — `/home/vricosti/Dev/emulators/rdynarmic/src/frontend/a32/translate/synchronization.rs` vs `/home/vricosti/Dev/emulators/zuyu/externals/dynarmic/src/dynarmic/frontend/A32/translate/impl/synchronization.cpp`

### Intentional differences
- Rust keeps the ARM synchronization translators in one `synchronization.rs` owner file instead of the upstream `synchronization.cpp`, but the owner boundary still matches the upstream translation unit.
- Rust uses a local `unpredictable_instruction(...)` helper inside this file instead of the upstream shared `TranslatorVisitor::UnpredictableInstruction()` method. The helper now mirrors the upstream sequence for this owner slice: `UpdateUpperLocationDescriptor`, advance `PC`, `ExceptionRaised`, then `CheckHalt(ReturnToDispatch)`.

### Unintentional differences (to fix)
- Fixed in this pass: `LDREX*` and `STREX*` used `AccType::Ordered` in the Rust port. Upstream uses `IR::AccType::ATOMIC` for the ARM exclusive load/store family.
- Fixed in this pass: `LDREX*`, `STREX*`, `SWP`, and `SWPB` were missing upstream `UnpredictableInstruction()` guards on `PC` and overlapping register combinations. In particular, `STREX/STREXB/STREXH` with `Rd == PC` were incorrectly accepted and lowered into `set_register(R15, ...)`, which emitted `A32BXWritePC` mid-block instead of raising an unpredictable-instruction exception like upstream.
- Fixed in this pass: the earlier local unpredictable path only emitted `ExceptionRaised` and returned `true`. Upstream stops translation for that instruction and sets a halt-check terminal after updating `PC`.

### Missing items
- Full line-for-line parity for the acquire/release instructions (`LDA*`, `STL*`, `STLEX*`) present in upstream `synchronization.cpp` is still missing from the Rust owner file.
- This slice still needs a backend-level regression once the remaining `A32ExceptionRaised`/regalloc panic is fixed; current regression coverage stops at frontend/IR generation.

### Binary layout verification
- PASS: IR/control-flow only; no raw payload or serialized layout changed.

## 2026-03-28 — `core/src/hle/service/hle_ipc.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hle_ipc.cpp`

### Intentional differences
- Rust still keeps the IPC command buffer parsing inside `HLERequestContext` methods instead of the upstream C++ class split between header/implementation. Ownership remains in the matching owner file.

### Unintentional differences (to fix)
- Fixed in this pass: C receive-list descriptor parsing used `CommandHeader::buf_c_descriptor_flags()` too early, which collapses raw flag values `3..15` to `Disabled`. Upstream consumes the raw 4-bit field and interprets values greater than `InlineDescriptor` as multi-descriptor C receive-lists. The Rust path now reads the raw field directly and preserves the upstream `flags - 2` descriptor count rule.

### Missing items
- No new missing item identified in this slice beyond the remaining broader HIPC parity still tracked elsewhere.

### Binary layout verification
- PASS: command-buffer layout unchanged; fixed only the interpretation of the raw header bits.

## 2026-03-28 — `core/src/hle/service/nvdrv/core/container.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/core/container.cpp`

### Intentional differences
- Rust still omits upstream Host1x SMMU registration, ASID ownership, and heap preallocation because those owners are not fully wired in this port yet. Session ownership remains in the matching owner file.

### Unintentional differences (to fix)
- Fixed in this pass: `open_session()` always created a fresh session instead of matching the upstream reuse-by-process behavior for active sessions.
- Fixed in this pass: session state did not retain the owning process, which prevented downstream `nvmap` from recovering `GetSession(...)->process` like upstream.

### Missing items
- Upstream `RegisterProcess`, `UnregisterProcess`, and heap preallocation remain unported in this owner file.

### Binary layout verification
- PASS: no raw serialized payload affected; session owner fields are internal Rust state only.

## 2026-03-28 — `core/src/hle/service/nvdrv/nvdrv_interface.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/nvdrv_interface.cpp`

### Intentional differences
- Rust still resolves handles through the current process handle table and `Arc<Mutex<KProcess>>` instead of upstream raw `KProcess*` object accessors. This is the existing object-ownership adaptation.

### Unintentional differences (to fix)
- Fixed in this pass: `Initialize` ignored the copied process handle and opened an anonymous session. Upstream resolves the process object from copy handle 0 and opens the nvdrv session against that process.

### Missing items
- `MapSharedMem`, `SetAruidForTest`, and `InitializeDevtools` are still unimplemented, matching the already documented gaps in this service owner.

### Binary layout verification
- PASS: IPC payload shape unchanged; fixed only handle/process resolution behavior.

## 2026-03-28 — `core/src/hle/service/nvdrv/devices/nvmap.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvmap.cpp`

### Intentional differences
- Rust still omits the full Host1x-backed SMMU/GMMU integration present upstream. The owner file remains responsible for nvmap device ioctl behavior.

### Unintentional differences (to fix)
- Fixed in this pass: `IocAlloc` stopped after `Handle::alloc(...)` and never mirrored the upstream `process->GetPageTable().LockForMapDeviceAddressSpace(...)` step.
- Fixed in this pass: `NvMapDevice` had no access to the owning `Container`, so it could not recover the active session's process owner like upstream `container.GetSession(...)->process`.

### Missing items
- Full upstream success path still expects Host1x-backed handle pinning and address-space mapping in `NvCore::NvMap`.

### Binary layout verification
- PASS: `IocAllocParams` layout unchanged; fixed only post-allocation lifecycle behavior.

## 2026-03-28 — `core/src/hle/service/nvdrv/devices/nvhost_as_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_as_gpu.cpp`

### Intentional differences
- Rust still does not instantiate the upstream GPU memory manager / GMMU object. The owner file keeps a lighter internal mapping model until Host1x memory-manager parity is completed.

### Unintentional differences (to fix)
- Fixed in this pass: `AllocAsEx` did not initialize the upstream small-page and big-page allocators, leaving subsequent address-space allocations without the required VA allocator state.
- Fixed in this pass: `AllocateSpace(flags without Fixed)` never assigned `params.offset`, while upstream allocates GPU VA and returns it to the guest.
- Fixed in this pass: `MapBufferEx(flags without Fixed)` returned success without allocating or recording any GPU VA mapping, while upstream allocates space and writes the resulting `offset` back to the request struct.
- Fixed in this pass: `FreeSpace` did not free the corresponding small/big-page allocator range.

### Missing items
- Full upstream `gmmu->Map`, sparse mapping, and `nvmap.PinHandle(...)` device-address translation are still incomplete in this owner file.
- The per-allocation `mappings` list from upstream `Allocation` is still not modeled literally in Rust.

### Binary layout verification
- PASS: ioctl struct layouts unchanged; added allocator-backed state and return-value behavior only.

## 2026-03-28 — `core/src/hle/kernel/k_page_table_base.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_page_table_base.cpp`

### Intentional differences
- Rust still does not model upstream allocator RAII helpers such as `KMemoryBlockManagerUpdateAllocator`; it uses the existing `update_lock(...)` API in the matching owner file instead.

### Unintentional differences (to fix)
- Fixed in this pass: `LockForMapDeviceAddressSpace` incorrectly delegated to the generic `lock_memory_and_open(...)` helper. Upstream performs its own state/attribute check for the device-share path and then updates blocks via `KMemoryBlock::ShareToDevice`.
- Fixed in this pass: `UnlockForDeviceAddressSpace` similarly used the generic unlock helper instead of the upstream device-share-specific contiguous-state check plus `KMemoryBlock::UnshareToDevice`.
- The old Rust path incorrectly rejected a valid `NORMAL` region used by `nvmap::IocAlloc`, returning `RESULT_INVALID_CURRENT_MEMORY` where upstream asserts success.

### Missing items
- `LockForUnmapDeviceAddressSpace` / `UnlockForDeviceAddressSpacePartialMap` are still not ported literally alongside this slice.

### Binary layout verification
- PASS: no raw struct layout affected; fixed kernel memory-state transition behavior only.

## 2026-03-28 — `core/src/hle/service/nvdrv/core/syncpoint_manager.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/core/syncpoint_manager.cpp`

### Intentional differences
- Rust still does not own the upstream `Host1x` object inside this file, so `update_min()` cannot sample hardware syncpoint values yet.
- Rust adds a local `signal_syncpoint(id)` helper in this owner file. This is a temporary adaptation for immediately-completing nvdrv stubs until the upstream Host1x-backed completion path exists.

### Unintentional differences (to fix)
- Fixed in this pass: the Rust port had no owner-local way to bring `counter_min` up to `counter_max` after synchronous stubbed GPU work, so fences returned by the partial `nvhost_gpu` path never became signalled.

### Missing items
- Full upstream `Host1x` wiring for `UpdateMin()` remains missing.
- Upstream fence expiry still depends on real Host1x progress rather than the temporary synchronous helper used here.

### Binary layout verification
- PASS: internal manager state only; no serialized payload layout changed.

## 2026-03-28 — `core/src/hle/service/nvdrv/devices/nvhost_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_gpu.cpp`

### Intentional differences
- Rust still does not own upstream GPU channel state, puller command submission, or event-backed notifier objects in this owner file because the `video_core` ownership boundary is not yet ported literally.
- Rust uses the owner-local `SyncpointManager::signal_syncpoint()` adaptation to complete fences immediately for the stubbed submission path.

### Unintentional differences (to fix)
- Fixed in this pass: `AllocGPFIFOEx2` returned a default fence instead of allocating/returning the per-channel syncpoint fence owned by this device upstream.
- Fixed in this pass: `SubmitGPFIFOBase1` did not propagate fence increment semantics into the returned fence state. The Rust port now updates the channel syncpoint max value and returns the matching fence from the owner file.
- Fixed in this pass: `nvhost_gpu` was constructed without the container-owned syncpoint manager it needs for upstream per-channel fence ownership.

### Missing items
- `InitChannel`, `PushGPUEntries`, wait/increment command-list generation, and event notifier behaviour from upstream `nvhost_gpu.cpp` remain unported.
- The Rust owner file still does not bind real `video_core::ChannelState` / Host1x submission state.

### Binary layout verification
- PASS: ioctl struct layouts unchanged; fixed fence/syncpoint ownership and lifecycle only.

## 2026-03-28 — `core/src/hle/service/nvdrv/nvdrv.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/nvdrv.cpp`

### Intentional differences
- Rust still constructs devices directly in the `match` inside `Module::open()` instead of mirroring the upstream builder map verbatim. Device ownership nevertheless remains in the matching owner file.
- Rust still uses the lightweight `EventInterface` placeholder rather than upstream `KEvent*` service-context objects.

### Unintentional differences (to fix)
- Fixed in this pass: `/dev/nvhost-gpu` was not receiving the shared `Container` owner needed for syncpoint parity.
- Fixed in this pass: `/dev/nvhost-as-gpu` was not receiving the shared `Container` owner needed to recover `NvMap` and session-owned state like upstream `nvhost_as_gpu(system, *this, container)`.

### Missing items
- Full upstream builder ownership, service-context events, and device constructors that depend on `video_core` owners remain incomplete.

### Binary layout verification
- PASS: no raw payload layout affected; fixed only owner wiring during device creation.

## 2026-03-28 — `core/src/hle/service/nvdrv/devices/nvhost_as_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_as_gpu.cpp`

### Intentional differences
- Rust still does not construct the upstream `Tegra::MemoryManager` / GMMU owner in this file, so the owner-local model remains bookkeeping-only for VA state.
- `BindChannel` remains documented-stubbed in this owner file because directly assigning `channel_state->memory_manager = gmmu` would introduce a `core -> video_core` dependency cycle in the current Rust tree.
- `Remap` is still stubbed in Rust; the upstream `gmmu->Map(...)` semantics are not yet representable without the missing memory-manager owner.

### Unintentional differences (to fix)
- Fixed in this pass: the Rust owner file did not keep upstream `Container` / `NvMap` ownership, so `MapBufferEx` could not look up or pin nvmap handles locally.
- Fixed in this pass: `MapBufferEx` created mappings with `ptr = 0` instead of the pinned device address returned by `NvMap::PinHandle(...)`.
- Fixed in this pass: `MapBufferEx` defaulted the mapping size from `page_size`, while upstream falls back to the nvmap handle's original size.
- Fixed in this pass: fixed mappings were not recorded against their parent allocation, so `FreeSpace` could not mirror upstream per-allocation mapping cleanup and unpin behaviour.
- Fixed in this pass: `UnmapBuffer` removed bookkeeping state only; it now also mirrors the owner-local unpin/free behaviour for mapped handles.

### Missing items
- Full upstream `gmmu->Map`, sparse remap handling, and allocation-backed `Mapping` ownership via shared objects remain incomplete.
- `GetVARegionsImpl` still synthesizes the two regions from stored VM bounds rather than the upstream allocator getters.
- The current Rust `Allocation::mappings` uses offset bookkeeping rather than the upstream `std::list<std::shared_ptr<Mapping>>`.

### Binary layout verification
- PASS: ioctl struct layouts unchanged; fixed only owner-local mapping bookkeeping and pin/unpin lifecycle.

## 2026-03-28 — `core/src/arm/debug.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/arm/debug.cpp`

### Intentional differences
- Rust still has partial symbolication and module discovery in this owner file; `find_modules()` remains a placeholder because page-table based module walking is not fully wired yet.

### Unintentional differences (to fix)
- Fixed in this pass: `get_backtrace_from_context()` did not match upstream guard conditions before dereferencing frame records.
- Fixed in this pass: the Rust port skipped the upstream `pc` entry insertion and walked the frame chain by reading `fp` blindly, without checking zero, alignment, or `IsValidVirtualAddressRange`.
- The old Rust behavior could segfault the host while logging an invited exception, masking the real guest fault path.

### Missing items
- Full upstream module enumeration and symbolication remain incomplete in this file.

### Binary layout verification
- PASS: no serialized layout changed; fixed only backtrace walking guards and entry ordering.

## 2026-03-28 — `core/src/arm/debug.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/arm/debug.cpp`

### Intentional differences
- Rust still has partial symbolication and module discovery in this owner file; `find_modules()` remains a placeholder because page-table based module walking is not fully wired yet.
- `symbolicate_backtrace()` now takes the real Rust `KProcess` directly instead of round-tripping through the local opaque forward declaration. This is a Rust-only safety adaptation that preserves owner/file boundaries.

### Unintentional differences (to fix)
- Fixed in this pass: the AArch32 backtrace path read `ctx.r[11]` instead of the upstream-owned `ctx.fp` field.
- Fixed in this pass: frame-record validation used `ProcessMemoryData::is_valid_range()`, which treated sparse address-space holes as valid and was looser than upstream `Memory::IsValidVirtualAddressRange`.
- The Rust backtrace path still does not share the exact upstream memory-validity implementation because the Rust memory subsystem is split across `ProcessMemoryData` and `KProcessPageTable`.

### Missing items
- Full upstream module enumeration and symbolication remain incomplete in this file.
- Re-audit `find_modules()` / `get_module_end()` once page-table based module walking is fully ported.

### Binary layout verification
- PASS: no serialized layout changed; fixed only frame-pointer source selection, mapping validation, and local symbolication plumbing.

## 2026-03-28 — `core/src/arm/dynarmic/arm_dynarmic_32.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/arm/dynarmic/arm_dynarmic_32.cpp`

### Intentional differences
- Rust still stores the last exception address on the parent and halts with `EXCEPTION_RAISED` on `NoExecuteFault`, because the full upstream `ReturnException(...)` path is not yet ported in this owner file.

### Unintentional differences (to fix)
- Fixed in this pass: the Rust `ExceptionRaised` path added an extra `check_memory_access(pc, 4)` guard before reading the faulting instruction, unlike upstream which calls `m_memory.Read32(pc)` directly.
- The extra guard could recursively re-enter the same exception path while logging, producing an infinite backtrace loop at a stable `pc` and hiding the real exception log line.

### Missing items
- Full upstream debugger-enabled `ReturnException(pc, InstructionBreakpoint)` behavior is still not implemented in this owner file.
- Re-audit the remaining `ExceptionRaised` / `CallSVC` ordering against upstream once the current MK8D bring-up bug is fully resolved.

### Binary layout verification
- PASS: no serialized layout changed; fixed only exception logging control flow in the owner callback.

## 2026-03-28 — `core/src/hle/service/nvdrv/devices/nvhost_as_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_as_gpu.cpp`

### Intentional differences
- Rust still does not construct the upstream `Tegra::MemoryManager` / `GPU().InitAddressSpace(*gmmu)` owner in `AllocAsEx`, because the current Rust tree does not yet carry that `video_core` integration in this owner file.

### Unintentional differences (to fix)
- Fixed in this pass: `GetVARegions` synthesized the returned ranges from `vm.va_range_*` instead of the upstream allocator-owned `GetVAStart()` / `GetVALimit()` values.
- That divergence made the owner file less traceable to upstream and risked drifting if allocator state stopped matching the initial VM ranges.

### Missing items
- Full upstream `gmmu` construction and `BindChannel` ownership transfer into `channel_state->memory_manager` remain incomplete.
- `GetVARegions3` and the rest of the address-space lifecycle still need re-audit once the missing `gmmu` owner is ported.

### Binary layout verification
- PASS: `VaRegion` / ioctl struct layouts unchanged; fixed only region source selection and ownership parity.

## 2026-03-28 — `core/src/hle/service/nvdrv/devices/nvhost_as_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_as_gpu.cpp`

### Intentional differences
- Rust still does not construct the upstream `Tegra::MemoryManager` / `GPU().InitAddressSpace(*gmmu)` owner in `AllocAsEx`, so `Remap` preserves the upstream allocation validation and `NvMap::PinHandle(...)` lifecycle without yet calling the missing `gmmu->Map(...)` / `MapSparse(...)` backend.

### Unintentional differences (to fix)
- Fixed in this pass: `Remap` was a stub that returned `Success` without performing the upstream allocation-range validation, sparse-allocation check, or `NvMap::PinHandle(...)` ownership step.
- The stub made this owner file structurally correct but behaviorally too weak, allowing invalid remaps and skipping persistent nvmap pinning that upstream performs for sparse remap entries.

### Missing items
- Full upstream `gmmu->Map(...)` and `gmmu->MapSparse(...)` calls in `Remap` remain unported in this owner file.
- `BindChannel` still does not transfer the upstream `gmmu` owner into `nvhost_gpu::channel_state->memory_manager`.

### Binary layout verification
- PASS: `IoctlRemapEntry` layout unchanged; fixed only owner-local remap validation and nvmap pinning behavior.

## 2026-03-28 — `core/src/hle/service/nvdrv/devices/nvhost_ctrl.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_ctrl.cpp`

### Intentional differences
- Rust still uses the local `core::syncpoint_manager::SyncpointManager` owner rather than the upstream Host1x syncpoint manager, so `IocCtrlEventWait` cannot yet register or deregister real host actions in this owner file.
- Because the upstream host-action handle owner is missing, the Rust port preserves the slot allocation, `fails`, and `SyncpointEventValue` encoding semantics locally but still returns `Timeout` without wiring the asynchronous signal callback.

### Unintentional differences (to fix)
- Fixed in this pass: `IocCtrlEventWait` had drifted from upstream `fails` handling for the non-allocation path and did not clear the reused slot state after a non-allocation wait request.
- Fixed in this pass: freeing an NV event did not reset all owner-local fields (`fails`, `assigned_value`) even though upstream resets the slot back to an available state.

### Missing items
- Full upstream `wait_handle` ownership and Host1x callback registration/deregistration remain unported in this file.
- Re-audit `IocCtrlClearEventWait` once the upstream host-action path exists in Rust.

### Binary layout verification
- PASS: `SyncpointEventValue`, `IocCtrlEventWaitParams`, and related ioctl structs remain unchanged; only event-slot lifecycle logic changed.

## 2026-03-28 — `core/src/hle/service/nvdrv/devices/nvhost_as_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_as_gpu.cpp`

### Intentional differences
- Rust still does not own a real upstream `gmmu` / `Tegra::MemoryManager` in this file, so `Remap` and `MapBufferEx(REMAP)` preserve the upstream validation and address calculations but still stop short of a real `gmmu->Map(...)` call.
- `BindChannel` still cannot write through to upstream `nvhost_gpu::channel_state->memory_manager` because that owner lives across the current `core` / `video_core` boundary; Rust stores the bound address-space token in the matching owner file instead.

### Unintentional differences (to fix)
- Fixed in this pass: `MapBufferEx(REMAP)` rejected `mapping_size == 0` and negative signed offsets even though upstream only checks the mapped-region size and uses signed address addition.
- Fixed in this pass: `MapBufferEx(REMAP)` did not mirror the upstream signed `offset + buffer_offset` / `ptr + buffer_offset` address calculation.
- Fixed in this pass: `Remap(handle == 0)` had no explicit sparse-path behavior in the owner file; Rust now preserves the upstream branch structure and logging even though the final `MapSparse(...)` backend call is still missing.

### Missing items
- Full upstream `gmmu->Map(...)`, `gmmu->MapSparse(...)`, and `GPU().InitAddressSpace(*gmmu)` ownership are still missing.
- `BindChannel` still needs a real memory-manager transfer once the upstream `channel_state` owner is reachable without violating crate ownership.

### Binary layout verification
- PASS: `IoctlMapBufferEx`, `IoctlRemapEntry`, and `IoctlBindChannel` layouts remain unchanged; only control flow and owner-local state updates changed.

## 2026-03-28 — `core/src/hle/service/nvdrv/devices/nvhost_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_gpu.cpp`

### Intentional differences
- Rust still does not expose the upstream `channel_state` / `memory_manager` owner in this file, so the temporary `bound_address_space_token` field stands in for the missing direct `memory_manager` assignment.

### Unintentional differences (to fix)
- Fixed in this pass: the owner file had no place to receive the `BindChannel` result from `nvhost_as_gpu`, so the address-space binding state was silently discarded.

### Missing items
- Replace `bound_address_space_token` with the real upstream `channel_state->memory_manager` ownership when the `video_core` owner can be ported without violating crate boundaries.

### Binary layout verification
- PASS: no ioctl payload layout changed; fixed only owner-local channel binding state.

## 2026-03-28 — `core/src/hle/service/nvdrv/nvdrv.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/nvdrv.cpp`

### Intentional differences
- Rust still uses explicit per-device construction in `Module::open()` rather than the exact upstream factory structure.

### Unintentional differences (to fix)
- Fixed in this pass: `Module` did not retain a typed `nvhost_gpu` owner path, so `nvhost_as_gpu::BindChannel` could not recover the target device like upstream `module.GetDevice<nvhost_gpu>(fd)`.

### Missing items
- Full upstream device-factory structure and service-context lifetime still need re-audit once the remaining nvdrv owners are ported.

### Binary layout verification
- PASS: no serialized layout changed; fixed only module-side owner lookup for `BindChannel`.

## 2026-03-29 — `core/src/gpu_core.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/core.cpp`, `/home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.cpp`, and `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_as_gpu.cpp`

### Intentional differences
- Rust adds this bridge-only owner file because the split `core` / `video_core` crate graph cannot name `Tegra::GPU`, `Tegra::MemoryManager`, or `ChannelState` directly across crates the way the monolithic upstream tree can.
- The traits preserve upstream ownership boundaries (`GPU::AllocateChannel()`, `std::make_shared<Tegra::MemoryManager>(...)`, `channel_state->memory_manager = gmmu`) without introducing a reverse `core -> video_core` dependency.

### Unintentional differences (to fix)
- None in this bridge file for the current slice; it exists specifically to preserve upstream ownership under the Rust crate split.

### Missing items
- The bridge now exposes `Map(...)`, `MapSparse(...)`, and `Unmap(...)` because `nvhost_as_gpu.cpp` owns those calls upstream too.
- It still does not expose the full upstream address-space initialization path (`system.GPU().InitAddressSpace(*gmmu)` behavior remains owner-local to `video_core::gpu`).

### Binary layout verification
- PASS: no serialized layout; this file only carries opaque trait ownership.

## 2026-03-29 — `core/src/core.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/core.h` and `/home/vricosti/Dev/emulators/zuyu/src/core/core.cpp`

### Intentional differences
- `System::gpu_core` now stores `Box<dyn GpuCoreInterface>` instead of `Box<dyn Any + Send>` so Rust owners can reach the upstream-equivalent GPU channel / memory-manager lifecycle without violating crate boundaries.
- The type-erased frontend handoff remains because Rust still constructs `video_core::gpu::Gpu` outside `core`, unlike upstream `System::Impl`.

### Unintentional differences (to fix)
- None fixed in this pass beyond narrowing the type-erased owner to a GPU-specific interface.

### Missing items
- `System` still does not construct the concrete GPU in the exact upstream place; the frontend bootstrap path remains a documented Rust split-crate difference.

### Binary layout verification
- PASS: no serialized layout changed; only subsystem owner typing changed.

## 2026-03-29 — `video_core/src/gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.cpp`

### Intentional differences
- Rust adds opaque bridge implementations (`GpuChannelHandle`, `GpuMemoryManagerHandle`) in this owner file because upstream can hand around `shared_ptr<ChannelState>` and `shared_ptr<MemoryManager>` directly, while Rust must hide those concrete types from `core`.
- The bridge still constructs the memory-manager object through `GpuCoreInterface` because `core` and `video_core` remain split crates, unlike the monolithic upstream tree.

### Unintentional differences (to fix)
- Fixed in this pass: `BindChannel` no longer receives a synthetic `usize` token. The owner file now transfers an opaque memory-manager object so `channel_state.memory_manager` points at the same GPU-owned object allocated by `nvhost_as_gpu`, matching upstream ownership much more closely.
- Fixed in this pass: `VideoGpuMemoryManagerHandle` now wraps `video_core::memory_manager::MemoryManager` instead of the temporary local placeholder that had been declared in `control/channel_state.rs`.

### Missing items
- The concrete `Tegra::MemoryManager` behavior (`Map`, `MapSparse`, `Unmap`, rasterizer binding, address translation with system memory backing) is still only partially ported behind `VideoGpuMemoryManagerHandle`.
- `GPU::InitAddressSpace(*gmmu)` still has no direct Rust counterpart in this owner file.

### Binary layout verification
- PASS: no ioctl or serialized layout changed; fixed only GPU-side owner transfer semantics.

## 2026-03-29 — `video_core/src/memory_manager.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/memory_manager.h` and `/home/vricosti/Dev/emulators/zuyu/src/video_core/memory_manager.cpp`

### Intentional differences
- Rust still retains the older simplified `GpuMemoryManager` backend internally because the full upstream `Core::System`, `MaxwellDeviceMemoryManager`, rasterizer invalidation, and page-table helpers are not fully wired into this crate yet.
- A thin `MemoryManager` wrapper was added in this file to restore the upstream owner and type name without moving the backend into an unrelated module.

### Unintentional differences (to fix)
- Fixed in this pass: the concrete memory-manager owner no longer lived as a placeholder type inside `control/channel_state.rs`; it now resides in the matching upstream owner file.
- Fixed in this pass: owner-local `Map(...)`, `MapSparse(...)`, and `Unmap(...)` entrypoints now exist in this file instead of being silently dropped in `nvhost_as_gpu`.

### Missing items
- Constructor parity is incomplete: upstream takes `Core::System&`, optional `MaxwellDeviceMemoryManager&`, address-space bits, split address, big-page bits, and page bits. Rust currently exposes only `MemoryManager::new(id)`.
- `BindRasterizer`, `Map`, `MapSparse`, `Unmap`, `GetPageKind`, `GetSubmappedRange`, dirty tracking, invalidation accumulator behavior, and host memory pointer fast paths remain unported.
- `MemoryManager` still delegates to the simplified `GpuMemoryManager` instead of the upstream multi-level page table plus host1x device memory manager.

### Binary layout verification
- PASS: no raw serialized struct layout introduced in this pass; owner relocation only.

## 2026-03-29 — `video_core/src/control/channel_state.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/control/channel_state.h` and `/home/vricosti/Dev/emulators/zuyu/src/video_core/control/channel_state.cpp`

### Intentional differences
- Engine ownership is still placeholder-only in Rust because `Maxwell3D`, `Fermi2D`, `KeplerCompute`, `MaxwellDMA`, and `KeplerMemory` are not fully constructed here yet.

### Unintentional differences (to fix)
- Fixed in this pass: `ChannelState` no longer owns a fake local `MemoryManager` type. Its `memory_manager` field now points at the real owner type from `video_core::memory_manager`, matching upstream ownership boundaries.

### Missing items
- `Init` still does not instantiate the upstream engines with `system` and `*memory_manager`.
- `BindRasterizer` still does not forward to the full engine set or the real memory-manager rasterizer binding path.

### Binary layout verification
- PASS: no serialized layout changes; only type ownership changed.

## 2026-03-29 — `core/src/hle/service/nvdrv/devices/nvhost_as_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_as_gpu.cpp`

### Intentional differences
- Rust still lacks the full upstream `Tegra::MemoryManager` backend, so the new `gmmu` field stores an opaque bridge handle instead of a concrete `shared_ptr<Tegra::MemoryManager>`.
- `AllocAsEx` now allocates the upstream-equivalent owner object, and the bridge now forwards `MapBufferEx` / `Remap` / sparse allocation teardown into `gmmu->Map(...)` / `MapSparse(...)` / `Unmap(...)`.

### Unintentional differences (to fix)
- Fixed in this pass: `BindChannel` no longer forwards a fake address-space token. It now transfers the `gmmu` owner allocated in `AllocAsEx`, matching the upstream ownership path `gpu_channel_device->channel_state->memory_manager = gmmu`.
- Fixed in this pass: the Rust constructor now carries `SystemRef`, matching the upstream owner set (`system`, `module`, `container`) instead of synthesizing the GPU memory-manager transfer indirectly.
- Fixed in this pass: `AllocateSpace`, `FreeMappingLocked`, `FreeSpace`, `Remap`, `MapBufferEx`, and `UnmapBuffer` no longer stop at allocator bookkeeping; they now call the `gmmu` bridge in the same owner file where upstream performs `gmmu->Map(...)`, `MapSparse(...)`, and `Unmap(...)`.
- Fixed in this pass: `AllocAsEx` now performs the upstream two-step lifecycle `allocate_memory_manager_handle(...)` then `GPU::InitAddressSpace(...)`, instead of constructing the owner object without the explicit GPU-side address-space initialization step.

### Missing items
- The `gmmu` handle still wraps a simplified `video_core::memory_manager::MemoryManager` backend, so page kinds, big-page semantics, sparse-vs-reserved distinction, rasterizer invalidation, and host1x device-memory parity remain incomplete.

### Binary layout verification
- PASS: `IoctlAllocAsEx`, `IoctlMapBufferEx`, `IoctlRemapEntry`, and `IoctlBindChannel` layouts unchanged; fixed only owner lifecycle and transfer semantics.

## 2026-03-29 — `core/src/hle/service/nvdrv/devices/nvhost_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_gpu.cpp`

### Intentional differences
- Rust still allocates `channel_syncpoint` lazily in `AllocGPFIFOEx2` instead of the upstream constructor; this owner ordering divergence remains to be fixed.
- Event destruction still uses the local no-op `EventInterface::free_event(...)` placeholder because the full upstream service-context close path is not present yet.

### Unintentional differences (to fix)
- Fixed in this pass: `bind_address_space()` no longer stores or propagates a synthetic token. The owner now receives and forwards an opaque GPU memory-manager object, so `channel_state.memory_manager` is set from the actual `nvhost_as_gpu` owner object rather than a fabricated identifier.

### Missing items
- Re-audit constructor/destructor ordering against upstream (`AllocateSyncpoint`, `FreeEvent`, `FreeSyncpoint`) once the current nvdrv runtime bug is fully resolved.
- Full upstream `SubmitGPFIFOImpl` / pushbuffer submission remains unported in this file.

### Binary layout verification
- PASS: no ioctl payload layout changed; fixed only memory-manager ownership transfer.

## 2026-03-29 — `core/src/hle/service/nvdrv/nvdrv.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/nvdrv.cpp`

### Intentional differences
- Rust still uses explicit per-device construction in `Module::open()` rather than the exact upstream factory structure.

### Unintentional differences (to fix)
- Fixed in this pass: `/dev/nvhost-as-gpu` now receives the same upstream owner set (`system`, `module`, `container`) instead of constructing the device without `system`.

### Missing items
- Full upstream device-factory structure and service-context lifetime still need re-audit once the remaining nvdrv owners are ported.

### Binary layout verification
- PASS: no serialized layout changed; fixed only constructor ownership parity.

## 2026-03-29 — `video_core/src/memory_manager.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/memory_manager.{h,cpp}`

### Intentional differences
- Rust still uses a simplified owner-local page-table backend instead of the upstream `Common::MultiLevelPageTable<u32>`, `RangeMap<GPUVAddr, PTEKind>`, `VirtualBuffer<u32> big_page_table_dev`, and `MaxwellDeviceMemoryManager`.
- `PTEKind` is still stored as raw `u32` values inside page entries (`kind_raw`) instead of the upstream `kind_map` owner object.
- Rasterizer-backed behavior (`BindRasterizer`, `ModifyGPUMemory`, `FlushRegion`, `InvalidateRegion`, `UnmapMemory`, `InnerInvalidation`, `InvalidationAccumulator`) remains stubbed or absent in Rust.
- The Rust API still exposes a convenience wrapper `GpuMemoryManager` alongside the upstream-owner `MemoryManager`; this is temporary compatibility debt for current callsites.

### Unintentional differences (to fix)
- Fixed in this pass: sparse mappings no longer alias to plain unmap behavior. Reserved GPU ranges are now tracked explicitly, matching the upstream `EntryType::Reserved` semantics more closely.
- Fixed in this pass: `MaxContinuousRange` no longer treats any mapped pages as continuous. It now stops on CPU-address discontinuities, matching upstream’s address-continuity check.
- Fixed in this pass: `GetMemoryLayoutSize` now supports an explicit upper bound and no longer implicitly assumes “unbounded only”.
- Fixed in this pass: `GetSubmappedRange` now exists in the owner file and splits output on unmapped pages and CPU-address discontinuities, rather than forcing downstream owners to infer this from `translate()`.
- Fixed in this pass: the owner now records the upstream `BindRasterizer` lifecycle edge via `MemoryManager::bind_rasterizer()`, so `GPU::InitAddressSpace()` can initialize the address-space owner in the right file instead of silently skipping that step.

### Missing items
- Big-page vs small-page dual-table behavior is still not implemented with the upstream split (`entries`, `big_entries`, `big_page_table_dev`, `big_page_continuous`).
- `MemoryOperation`, `ReadBlockImpl`, `WriteBlockImpl`, `WriteBlockCached`, `CopyBlock`, `GetSpan`, `FlushCaching`, `IsMemoryDirty`, and `GetSubmappedRangeImpl<false>` are still missing or simplified relative to upstream.
- `GpuToCpuAddress(gpu_addr, size)` is not yet ported as a size-aware scan overload.
- `GetPageKind` still lacks the upstream `RangeMap` ownership and typed `PTEKind` return path.
- `Map`, `MapSparse`, and `Unmap` still do not call rasterizer invalidation/update hooks in upstream order.
- No full `cargo test -p video_core` parity claim yet; only focused tests were run for this slice.

### Binary layout verification
- PASS: no raw serialized structs were changed in this slice; behavior changes are confined to owner-local page-table state and helper methods.

## 2026-03-29 — `core/src/gpu_core.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.h`

### Intentional differences
- This Rust file is an explicit cross-crate bridge with no single upstream file equivalent. It exists because `core` cannot name `video_core` concrete types directly, while upstream keeps all owners in one C++ target.

### Unintentional differences (to fix)
- Fixed in this pass: the bridge now exposes `init_address_space(...)`, matching the upstream `GPU::InitAddressSpace(Tegra::MemoryManager&)` lifecycle edge instead of silently folding or skipping it.

### Missing items
- The bridge still does not expose the full upstream `GPU` surface; it only carries the owner interactions currently required by `nvdrv`.

### Binary layout verification
- PASS: bridge traits only; no serialized layout involved.

## 2026-03-29 — `video_core/src/gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.{h,cpp}`

### Intentional differences
- Rust still keeps a simplified `Gpu` implementation and does not yet wire the full upstream renderer/host1x/rasterizer stack.

### Unintentional differences (to fix)
- Fixed in this pass: `GpuCoreInterface` now exposes and implements the upstream owner method `InitAddressSpace`, and the Rust owner calls `MemoryManager::bind_rasterizer()` from this file instead of leaving address-space initialization implicit or absent.

### Missing items
- `BindRenderer` still does not perform the full upstream `host1x.MemoryManager().BindInterface(rasterizer)` and `host1x.GMMU().BindRasterizer(rasterizer)` lifecycle.
- `InitChannel` and `BindChannel` still lag upstream in rasterizer/channel integration depth.

### Binary layout verification
- PASS: no ioctl or raw payload layout changed; owner lifecycle only.

## 2026-03-29 — `core/src/hle/service/nvdrv/devices/nvhost_gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_gpu.{h,cpp}`

### Intentional differences
- Temporary investigation-only logging was added around `SubmitGPFIFOBase1`, `SubmitGPFIFOBase2`, and `SubmitGPFIFOImpl` to compare the runtime ioctl/fence sequence against upstream `zuyu`. This is not upstream behavior and must be removed after the current nvdrv parity bug is fixed.

### Unintentional differences (to fix)
- No new semantic ownership or behavior difference identified in this pass. The current slice was re-read against upstream before adding bounded logging.

### Missing items
- Full runtime parity of the `SubmitGPFIFO*` submission sequence against upstream is still unverified; current logging exists specifically to compare the first healthy `fd=6` submission flow.
- `channel_mutex` ownership from upstream is still missing in Rust and remains to be re-audited once the current submission/syncpoint bug is isolated.

### Binary layout verification
- PASS: no ioctl payload struct layout changed in this pass; only logging was added.

## 2026-03-29 — `core/src/hle/service/nvdrv/nvdrv_interface.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/nvdrv_interface.cpp`

### Intentional differences
- Temporary investigation-only logging was added in `ioctl1_handler`, `ioctl2_handler`, and `ioctl3_handler` to capture raw ioctl numbers and buffer sizes before and after dispatch. This diverges from upstream logging volume and must be removed once the current nvdrv parity bug is isolated.
- Temporary investigation-only logging was expanded in this pass to dump bounded input/output payload prefixes for the specific `nvmap` and `nvhost_as_gpu` ioctls immediately preceding the MK8D guest abort. This is still debug-only and not upstream behavior.
- Temporary investigation-only logging was further expanded to include `nvhost_as_gpu::AllocateSpace` (`0xC0184102`) and `nvhost_as_gpu::MapBufferEx` (`0xC0284106`), because the current MK8D abort occurs after `GetVARegions1` and now needs exact guest-visible payload comparison on those later ioctls.
- Temporary investigation-only logging was further expanded again to include `nvmap::IocCreate` (`0xC0080101`) and `nvmap::IocAlloc` (`0xC0200104`), because the current MK8D abort occurs after a second `nvmap` create/alloc pair and before any later GPU ioctls become visible.

### Unintentional differences (to fix)
- No new semantic ownership or behavior difference identified in this pass. The file was re-read against upstream before adding bounded logging.

### Missing items
- The current investigation still needs the exact raw ioctl sequence for the repeated `nvdrv parsed_cmd=1` loop leading into the guest abort at `0x01D1DD20`.

### Binary layout verification
- PASS: no response or ioctl payload layout changed; only logging was added.

## 2026-03-29 — `core/src/hle/kernel/svc/svc_ipc.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/svc/svc_ipc.cpp`

### Intentional differences
- Temporary investigation-only logging was expanded in this pass so the Rust port also dumps the flushed TLS response words for `service=nvdrv` `cmd=1` after `write_to_outgoing_command_buffer()`. This is diagnostic-only and must be removed once the current nvdrv parity bug is isolated.

### Unintentional differences (to fix)
- No new semantic difference identified in this pass. The file was re-read against upstream before adding bounded logging.

### Missing items
- The current investigation still needs confirmation that the raw TLS response words seen by the guest for the second `nvmap::IocAlloc` exactly match upstream before the later `QueryMemory`/`SetMemoryAttribute` sequence.

### Binary layout verification
- PASS: no command-buffer or payload layout changed; only logging was added.

## 2026-03-29 — `core/src/arm/dynarmic/arm_dynarmic_32.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/arm/dynarmic/arm_dynarmic_32.cpp`

### Intentional differences
- Temporary investigation-only logging was expanded in this pass to dump the saved frame-chain words at `[fp]`, `[fp+4]`, `[fp+8]`, and `[fp+12]` when the current MK8D `UDF` path triggers. This is diagnostic-only and must be removed once the caller of the abort/reporting helper is identified.

### Unintentional differences (to fix)
- No new semantic CPU/JIT difference identified in this pass. The file was re-read against upstream before adding bounded logging.

### Missing items
- The current investigation still needs the caller return address of the abort/reporting helper so the first failing runtime branch can be compared against upstream.

### Binary layout verification
- PASS: no guest-visible binary layout changed; only diagnostic logging was added.

## 2026-03-29 — `core/src/arm/dynarmic/arm_dynarmic_32.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/arm/dynarmic/arm_dynarmic_32.cpp`

### Intentional differences
- Temporary investigation-only logging was expanded again in this pass to dump one more level of saved frame-chain words (`caller_fp` and `caller2_fp`) when the current MK8D `UDF` path triggers. This remains diagnostic-only and must be removed once the true caller of `nn::nlibsdk::heap::CentralHeap::Free` is identified.

### Unintentional differences (to fix)
- No new semantic CPU/JIT difference identified in this pass. The file was re-read against upstream before extending the bounded diagnostics.

### Missing items
- The investigation still needs the higher-level caller address above `CentralHeap::Free` so the original double-free site can be compared against upstream behavior.

### Binary layout verification
- PASS: no guest-visible binary layout changed; only diagnostic logging was added.

## 2026-03-29 — `core/src/hle/kernel/k_page_table_base.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_page_table_base.{h,cpp}`

### Intentional differences
- Rust still lacks the full upstream `QueryInfoImpl` locking and page-table internals; this pass only realigns the public `QueryInfo` out-of-range contract.

### Unintentional differences (to fix)
- Fixed in this pass: `query_info()` now matches upstream `KPageTableBase::QueryInfo` for out-of-range addresses by synthesizing the terminal `Inaccessible` block at `m_address_space_end` instead of returning `None`.

### Missing items
- `QueryInfoImpl` is still not ported as a dedicated owner method; the in-range lookup still delegates straight to `KMemoryBlockManager`.
- `out_page_info->flags = 0` parity still lives in the SVC caller path rather than a full `QueryInfo` owner implementation.

### Binary layout verification
- PASS: `KMemoryInfo` layout unchanged; only the returned synthesized values changed.

## 2026-03-29 — `core/src/hle/kernel/svc/svc_query_memory.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/svc/svc_query_memory.cpp`

### Intentional differences
- Rust still resolves non-pseudo process handles through the simplified current-process-only handle-table model. This existing limitation is unchanged by this pass.

### Unintentional differences (to fix)
- Fixed in this pass: `query_process_memory()` no longer fabricates a fake `Free`/size-zero `MemoryInfo` when the page-table lookup misses. It now relies on `KPageTableBase::query_info()` to provide the upstream terminal `Inaccessible` block semantics.

### Missing items
- Full multi-process handle-table object resolution remains incomplete compared to upstream.

### Binary layout verification
- PASS: `MemoryInfo` serialization format is unchanged; only the source values now follow upstream for out-of-range queries.

## 2026-03-29 — `core/src/hle/kernel/k_page_table_base.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_page_table_base.{h,cpp}`

### Intentional differences
- Rust still lacks the full upstream lock/updater allocator scaffolding around `SetMemoryAttribute`; this pass only restores the same state-validation contract inside the existing owner method.

### Unintentional differences (to fix)
- Fixed in this pass: `set_memory_attribute()` now includes `KMemoryState::FLAG_CAN_PERMISSION_LOCK` in `state_test_mask` when `mask` includes `KMemoryAttribute::PERMISSION_LOCKED`, matching upstream `KPageTableBase::SetMemoryAttribute`.

### Missing items
- The full upstream `KScopedLightLock`, `KMemoryBlockManagerUpdateAllocator`, and `KScopedPageTableUpdater` ownership/lifecycle around `SetMemoryAttribute` are still not mirrored exactly in Rust.

### Binary layout verification
- PASS: no guest-visible struct layout changed; only the state-validation path now matches upstream for `PermissionLocked`.

## 2026-03-29 — `video_core/src/gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.{h,cpp}`

### Intentional differences
- Rust now carries a temporary `guest_memory_reader` callback bridge so `video_core::DmaPusher` can fetch GPU command words through the frontend-owned core memory while `video_core::GPU` still lacks the upstream `Core::System&` ownership. This is a Rust-only adaptation that should disappear once `GPU` owns the same memory backing path as upstream.

### Unintentional differences (to fix)
- `GPU` still does not own upstream `Core::System&`, so several owner-local methods remain simplified around memory and sync integration.

### Missing items
- Full upstream `GPU(Core::System&, ...)` ownership and direct system-backed memory access remain unported.

### Binary layout verification
- PASS: no guest-visible binary layout changed; only owner-local callback plumbing was added.

## 2026-03-29 — `video_core/src/dma_pusher.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/dma_pusher.{h,cpp}`

## 2026-03-29 — `video_core/src/gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.{h,cpp}`

### Intentional differences
- Rust still keeps a temporary `guest_memory_reader` callback bridge because `video_core::Gpu` does not yet own upstream `Core::System&` directly.
- Rust calls `RasterizerInterface::initialize_channel(channel_id)` with the upstream channel ID instead of passing a whole `ChannelState&`, because the current rasterizer trait still exposes the reduced owner-local signature.

### Unintentional differences (to fix)
- Fixed in this pass: `VideoGpuChannelHandle::init_channel()` now mirrors upstream `GPU::Impl::InitChannel()` lifecycle ordering by calling `ChannelState::bind_rasterizer(...)` and then `rasterizer->InitializeChannel(...)` immediately after `ChannelState::init(...)`.
- Fixed in this pass: `Gpu` no longer truncates the rasterizer trait object to `AtomicPtr<()>`. It now preserves the full trait-object pointer so the upstream `InitChannel`/`InitAddressSpace` rasterizer edges can execute.

### Missing items
- `GPU` still does not own upstream `Core::System&`, so memory/sync integration methods in this owner remain incomplete.
- The rasterizer trait still uses a reduced `initialize_channel(channel_id)` API instead of the upstream `InitializeChannel(ChannelState&)`.

### Binary layout verification
- PASS: no guest-visible layout changed; only owner-local rasterizer lifecycle storage/calls were corrected.

### Intentional differences
- Rust now uses the temporary `GPU::guest_memory_reader` bridge to resolve GPU-resident command lists before `ProcessCommands`, because `video_core` still lacks the upstream direct `Core::System&`/guest-memory ownership path.

### Unintentional differences (to fix)
- Fixed in this pass: `step()` and `step_with_engine()` no longer silently skip GPU-resident command lists. They now fetch command headers through `MemoryManager::read_block_unsafe()` and process them, matching upstream control flow more closely.

### Missing items
- The full upstream `puller`/subchannel engine binding ownership is still incomplete in Rust.
- The full upstream safe-vs-unsafe read selection based on GPU accuracy and macro/compute special cases is still simplified.

### Binary layout verification
- PASS: `CommandHeader`/`CommandListHeader` layout unchanged; only command-fetch behavior changed.

## 2026-03-29 — `video_core/src/control/channel_state.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/control/channel_state.{h,cpp}`

### Intentional differences
- Rust constructors still omit upstream `Core::System&` arguments because `video_core::Gpu` does not yet own that upstream dependency. The owner file now instantiates the real engine types that already exist in Rust, but with simplified constructor shapes.

### Unintentional differences (to fix)
- Fixed in this pass: `ChannelState::init()` now instantiates the real engine owners (`Maxwell3D`, `Fermi2D`, `KeplerCompute`, `MaxwellDMA`, `KeplerMemory`) instead of placeholder structs, and passes `GPU` + `memory_manager` + `ChannelState` ownership into `DmaPusher`, matching upstream ownership much more closely.
- Fixed in this pass: `BindRasterizer()` now forwards the lifecycle edge to `dma_pusher`, `memory_manager`, and `kepler_memory` instead of leaving the whole owner path disconnected.
- Fixed in this pass: `BindRasterizer()` now also forwards to `maxwell_3d`, `fermi_2d`, `kepler_compute`, and `maxwell_dma`, matching the full upstream owner list in `channel_state.cpp`.

### Missing items
- Engine constructors still do not receive the full upstream `system`/memory dependencies.
- `fermi_2d`, `kepler_compute`, and `maxwell_dma` still keep simplified local rasterizer state behind those owner entry points; their full upstream rasterizer-backed execution remains incomplete in their own files.

### Binary layout verification
- PASS: `ChannelState` guest-invisible layout unchanged; only owner-local constructor wiring changed.

## 2026-03-29 — `video_core/src/engines/fermi_2d.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/engines/fermi_2d.{h,cpp}`

### Intentional differences
- Rust still uses a simplified software blit path in this owner file instead of the upstream `SoftwareBlitEngine` + accelerated copy flow.

### Unintentional differences (to fix)
- Fixed in this pass: the owner file now has `bind_rasterizer()` and stores the rasterizer edge locally, matching upstream method ownership in `fermi_2d.cpp`.

### Missing items
- `Blit()` still does not consult the bound rasterizer for `AccelerateSurfaceCopy(...)` before falling back to software, unlike upstream.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — `video_core/src/engines/kepler_compute.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/engines/kepler_compute.{h,cpp}`

### Intentional differences
- Rust still records dispatch calls into a local queue instead of directly calling the upstream rasterizer dispatch path from `ProcessLaunch()`.

### Unintentional differences (to fix)
- Fixed in this pass: the owner file now has `bind_rasterizer()` and stores the rasterizer edge locally, matching upstream method ownership in `kepler_compute.cpp`.

### Missing items
- Upstream `upload_state.BindRasterizer(rasterizer)` is still missing because the full `upload_state` owner path is not ported in this file yet.
- `ProcessLaunch()` still does not call the bound rasterizer directly.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — `video_core/src/engines/maxwell_3d.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/engines/maxwell_3d.{h,cpp}`

### Intentional differences
- Rust still stores the rasterizer edge as an erased fat-pointer payload instead of the upstream raw pointer field, because ownership crosses crate boundaries.
- Rust still keeps several upload and query paths simplified in this owner file.

### Unintentional differences (to fix)
- Fixed in this pass: `bind_rasterizer()` now exists in the owner file and `process_method_call()` forwards the upstream rasterizer-backed hooks for `WaitForIdle`, cache barriers, syncpoints, counter reset, and constant-buffer bind operations.

### Missing items
- Upstream `upload_state.BindRasterizer(rasterizer)` is still missing because the `upload_state` owner path is not ported in Rust yet.
- `process_counter_reset()` still uses a temporary query-type mapping that should be aligned with the upstream enums.
- Additional rasterizer-backed paths in this owner file remain simplified.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — `video_core/src/engines/maxwell_dma.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/engines/maxwell_dma.{h,cpp}`

### Intentional differences
- Rust still uses a simplified DMA-copy backend and does not expose the upstream accelerated DMA access path through `RasterizerInterface`.

### Unintentional differences (to fix)
- Fixed in this pass: the owner file now has `bind_rasterizer()` and stores the rasterizer edge locally, matching upstream method ownership in `maxwell_dma.cpp`.

### Missing items
- The launch path still does not use the bound rasterizer for accelerated DMA or semaphore release behavior like upstream.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — `video_core/src/dma_pusher.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/dma_pusher.{h,cpp}`

### Intentional differences
- Rust still uses the temporary `GPU::guest_memory_reader` bridge for guest-memory reads, because `video_core::Gpu` still lacks the upstream direct `Core::System&` ownership.
- The upstream safe-vs-unsafe fetch policy and rasterizer flush integration are still simplified.

### Unintentional differences (to fix)
- Fixed in this pass: `DmaPusher` now owns a real `Puller` and routes decoded methods through it instead of logging and returning on all puller methods `< 0x40`.
- Fixed in this pass: `DispatchCalls()` now drives the same owner path that can bind subchannels and dispatch engine methods, rather than a standalone no-op method stream.

### Missing items
- `DmaPusher` still lacks the full upstream subchannel object table and `BindSubchannel` owner API.
- `DispatchCalls()` still does not implement the upstream `system.IsPoweredOn()` loop or rasterizer flush behavior exactly.
- The upstream safe/unsafe guest-memory fetch split based on accuracy and macro/compute cases remains simplified.

### Binary layout verification
- PASS: `CommandHeader` and `CommandListHeader` layout unchanged.

## 2026-03-29 — `video_core/src/engines/puller.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/engines/puller.{h,cpp}`

### Intentional differences
- Engine dispatch is only wired for the engine owners that are already callable through the current Rust engine interfaces (`Maxwell3D`, `KeplerMemory`).
- Rust still does not have the full upstream `GPU` write-back path for semaphore/query payload writes, so `Query(...)` callbacks in this file still use a placeholder no-op writer.

### Unintentional differences (to fix)
- Fixed in this pass: `Puller` now owns the upstream-equivalent `ChannelState` link and no longer treats `BindObject` as a dead-end warning.
- Fixed in this pass: `CallEngineMethod` and `CallEngineMultiMethod` now dispatch into the bound `ChannelState` engines for the wired classes, instead of always warning and returning.
- Fixed in this pass: `Puller` now owns the upstream rasterizer + memory-manager edges closely enough to execute `RefCnt`, `WaitForIdle`, `MemOpB`, `SyncpointOperation`, and semaphore acquire loops instead of leaving them as trace-only stubs.

### Missing items
- `Fermi2D`, `KeplerCompute`, and `MaxwellDMA` dispatch are not yet wired through `Puller`.
- `SemaphoreRelease` and `SemaphoreOperation(WriteLong)` still lack the full upstream GPU-memory writeback path for query payloads.
- `dma_pusher.BindSubchannel(...)` as a separate owner API is still absent.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — `video_core/src/renderer_null/null_rasterizer.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_null/null_rasterizer.{h,cpp}`

### Intentional differences
- Rust injects `Arc<host1x::SyncpointManager>` into `RasterizerNull` instead of storing the upstream `Tegra::GPU&`. This is a temporary owner-local adaptation until the rasterizer constructors receive the same `GPU` owner path as upstream.

### Unintentional differences (to fix)
- Fixed in this pass: `SignalSyncPoint()` no longer increments a disconnected renderer-local syncpoint manager. It now increments both guest and host counters on the shared Host1x syncpoint manager, matching the upstream effect.

### Missing items
- `RasterizerNull` still does not own upstream `Tegra::GPU&`, so `Query()` still cannot source real GPU ticks.

### Binary layout verification
- PASS: no guest-visible layout in this owner file.

## 2026-03-29 — `video_core/src/renderer_null/renderer_null.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_null/renderer_null.{h,cpp}`

### Intentional differences
- Rust still exposes a reduced renderer constructor that receives the shared Host1x syncpoint manager directly, instead of the full upstream `GPU&` owner chain.

### Unintentional differences (to fix)
- Fixed in this pass: `RendererNull::new()` now takes the shared Host1x syncpoint manager instead of creating/receiving a disconnected renderer-only syncpoint owner.

### Missing items
- The full upstream `GPU&` constructor ownership is still missing.

### Binary layout verification
- PASS: no guest-visible layout in this owner file.

## 2026-03-29 — `video_core/src/renderer_opengl/gl_rasterizer.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.{h,cpp}`

### Intentional differences
- Rust still does not own the upstream `FenceManager` in this file; `signal_sync_point()` remains a simplified direct syncpoint update until the full fence-manager owner path is ported.

### Unintentional differences (to fix)
- Fixed in this pass: the simplified `signal_sync_point()` path no longer updates a disconnected renderer-local syncpoint manager. It now updates the shared Host1x guest and host syncpoints, which restores the upstream observable effect for `nvhost_ctrl` waits.

### Missing items
- `FenceManager::SignalSyncPoint()` ownership and the rest of the fence-manager path are still not ported in this owner.

### Binary layout verification
- PASS: no guest-visible layout in this owner file.

## 2026-03-29 — `yuzu_cmd/src/main.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/yuzu_cmd/yuzu.cpp`

### Intentional differences
- Rust still creates `Host1x` and GPU through the local subsystem factory instead of directly inside upstream `Core::System::SetupForApplicationProcess()`, because of the existing crate split.

### Unintentional differences (to fix)
- Fixed in this pass: the frontend no longer creates a disconnected `video_core::syncpoint::SyncpointManager` for renderers. It now clones the real `Host1x::syncpoint_manager()` before transferring `Host1x` into `System`.

### Missing items
- The subsystem factory split from upstream `core.cpp` is still present and documented debt.

### Binary layout verification
- PASS: frontend wiring only; no binary layout involved.

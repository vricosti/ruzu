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
- Rust uses `corosensei` instead of upstream `boost::context::detail::fcontext_t`: platform/backend replacement documented here, while keeping `Start`, `Rewind`, `YieldTo`, and `Exit` owned by `fiber.rs`.
- The Rust backend still keeps a dispatcher inside `Fiber::yield_to` for thread fibers because `corosensei` resumes coroutines from the host thread rather than exposing a raw `jump_fcontext` entrypoint. This is a backend adaptation, not an ownership change.
- The Rust backend still models rewind with a second coroutine object (`rewind_context`) instead of upstream `rewind_stack_limit`/`fcontext_t` swapping. This remains backend bookkeeping inside the same owner file.

### Unintentional differences (to fix)
- `FiberImpl` field layout remains backend-oriented (`context`/`rewind_context` coroutines plus dispatcher state) instead of mirroring upstream `stack_limit`/`rewind_stack_limit` more directly.
- The Rust stack size remains `2 MiB` instead of upstream `512 KiB`; this was kept because earlier Rust debug/service paths overflowed the smaller stack, but it is still a structural divergence from upstream.
- Full crate validation is still incomplete because `cargo test -p common` beyond the focused fiber tests still hits unrelated crate issues.

### Missing items
- Re-audit whether the retained suspended pre-rewind coroutine can be dropped/reset earlier without violating upstream lifecycle semantics.
- Re-audit cross-host-thread exchange behavior against the upstream scheduler once more runtime evidence is gathered on the clean restart branch.
- Full `cargo test -p common` green run once unrelated crate issues are resolved.

### Binary layout verification
- PASS: runtime-only fiber backend; no raw-serialized structs are defined here.

## 2026-03-28 — core/src/hle/kernel/kernel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.h and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_hardware_timer.cpp

### Intentional differences
- Rust adds `get_current_hardware_tick()` as a narrow helper over the existing global `KERNEL_PTR` so lower kernel owners can query the current hardware tick without pushing timeout conversion logic back into SVC files. This is mechanical plumbing to preserve upstream timeout ownership in `KThread` and `KConditionVariable`.

### Unintentional differences (to fix)
- `KernelCore` still exposes global-pointer based access patterns that do not line up one-for-one with upstream method ownership. The helper only narrows an existing divergence; it does not remove it.

### Missing items
- Full parity audit of the remaining `KernelCore` global access helpers versus upstream call sites.

### Binary layout verification
- PASS: helper-only change; no raw-serialized structs affected.

## 2026-03-29 — video_core/src/renderer_opengl/gl_rasterizer.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.cpp

### Intentional differences
- Rust still models `RasterizerOpenGL` with the current lightweight `FenceManager<Fence>` and without the full upstream cache objects (`texture_cache`, `buffer_cache`, `query_cache`). `signal_reference()` now preserves the upstream ownership and ordering semantics within that reduced backend.

### Unintentional differences (to fix)
- `signal_sync_point()` is still implemented directly in `gl_rasterizer.rs` instead of delegating through `FenceManager::SignalSyncPoint(...)` because the generic Rust fence manager does not yet own the upstream `GPU`/`Host1x` references.
- `query()` still writes `ticks=0` for timeout queries instead of the upstream timestamp path.
- The rest of the OpenGL rasterizer remains substantially simplified relative to upstream cache and rendering behavior.

### Missing items
- Upstream-equivalent `FenceManager` ownership of syncpoint signaling.
- Remaining cache-backed rendering/query behavior from `gl_rasterizer.cpp`.

### Binary layout verification
- PASS: runtime-only file; no raw-serialized structs are defined here.

## 2026-03-29 — video_core/src/fence_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/fence_manager.h

### Intentional differences
- Rust keeps a generic `FenceManager<F>` without the upstream cache/renderer template parameters. The new `#[cfg(test)]` accessors are test-only inspection helpers and do not affect runtime ownership.

### Unintentional differences (to fix)
- `FenceManager` still does not own the upstream `GPU`/`Host1x` references, so `SignalSyncPoint(...)` is not yet hosted directly in this file.
- Async flush/cache ownership remains simplified relative to upstream.

### Missing items
- Full upstream `SignalSyncPoint` ownership in `fence_manager.rs`.
- Remaining cache-backed `ShouldFlush`/`CommitAsyncFlushes` behavior.

### Binary layout verification
- PASS: runtime-only generic manager; no raw-serialized structs are defined here.

## 2026-03-30 — core/src/hle/kernel/k_condition_variable.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.h

### Intentional differences
- Rust still wraps ownership through `Arc<Mutex<KProcess>>` and a local `wait_for_current_thread(...)` helper instead of upstream `KScopedSchedulerLockAndSleep`: this is a mechanical adaptation to preserve the same ownership in `k_condition_variable.rs` while integrating with the current Rust scheduler.

### Unintentional differences (to fix)
- `wait_for_current_thread(...)` still relies on the current Rust scheduler/request-schedule path rather than a line-for-line equivalent of upstream `KScopedSchedulerLockAndSleep` plus timer task ownership.
- `KProcess::wait_condition_variable(...)` still extracts/reinserts `cond_var` with `std::mem::take` to satisfy Rust borrowing rules; ownership stays correct but the lifecycle is not yet structurally identical to upstream member-call syntax.

### Missing items
- Full parity audit of the timeout/hardware-timer wake path used by `WaitProcessWideKeyAtomic`.
- Remaining line-by-line comparison of priority-update hooks and signal ordering against upstream `SignalImpl`/tree iteration.

### Binary layout verification
- PASS: runtime-only synchronization owner; no raw-serialized struct layout changed in this slice.

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

### Unintentional differences (to fix)
- Fixed in this pass: `Container::new()` now obtains the shared `"dispdrv"` binder driver from the global `ServiceManager` and reuses its `HosBinderDriverServer` / `SurfaceFlinger`, instead of incorrectly constructing a private binder stack disconnected from the upstream owner service.
- Fixed in this pass: `Container::new()` no longer uses a Rust-only 5-second timeout while waiting for `"dispdrv"`. It now blocks indefinitely like upstream `GetService<T>("dispdrv", true)`.
- `on_terminate()` still only marks shutdown and does not yet remove all layers/displays from `SurfaceFlinger` as upstream does.

### Missing items
- Full `SharedBufferManager` parity.
- Upstream `OnTerminate()` display/layer teardown ordering.
- `GetLayerProducerHandle`.

### Binary layout verification
- PASS: container/service owner state only; no raw-serialized structs are defined here.

## 2026-03-31 — core/src/hle/service/sm/sm.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/sm/sm.cpp

### Intentional differences
- Rust still stores whole `Arc<Mutex<KPort>>` owners in `service_ports` instead of upstream raw `KClientPort*`, because kernel-object lifetime is carried through `Arc<Mutex<...>>` in the Rust port.

### Unintentional differences (to fix)
- Fixed in this pass: `ServiceManager::get_service_blocking(...)` no longer has a Rust-only timeout. It now matches upstream host-side behavior and polls until the requested service is registered.
- `ServiceManager::register_service(...)` still does not expose the upstream `KServerPort** out_server_port` ownership handoff, because the current Rust service-registration path keeps the full `KPort` inside `ServiceManager`.

### Missing items
- Full parity for the upstream `RegisterService(KServerPort**, ...)` output ownership.
- Removal of the Rust-only direct `"sm:"` service registration fallback once all internal HLE lookups go through the named-port path.

### Binary layout verification
- PASS: service-registration/state file only; no raw-serialized structs are defined here.

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
- Rust still uses `Arc<Mutex<...>>` plus a Rust-only weak owner record per queried NV event, because upstream `KEvent*` signaling does not need an explicit process/scheduler owner. This adaptation is required so asynchronous host-action callbacks can wake the same process handle waiters after `QueryEvent` copies the readable event into the caller handle table.

### Unintentional differences (to fix)
- Fixed in this pass: `IocCtrlEventWait` had drifted from upstream `fails` handling for the non-allocation path and did not clear the reused slot state after a non-allocation wait request.
- Fixed in this pass: freeing an NV event did not reset all owner-local fields (`fails`, `assigned_value`) even though upstream resets the slot back to an available state.
- Fixed in this pass: async `IocCtrlEventWait` callbacks signaled the readable event through `current_process`, which can be a different owner than the process that queried/copied the event handle. The owner now follows the queried event slot, matching the upstream `KEvent*` ownership intent more closely.

### Missing items
- Full upstream `ServiceContext::CreateEvent` / `KEvent` ownership is still represented as a Rust-only `KReadableEvent` plus copied handle owner, not a literal `KEvent*`.
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
- Fixed in this pass: `QueryEvent` copied the readable event handle into the caller without telling the device owner which process/scheduler should later receive the asynchronous wakeup. The interface now records that owner on the queried device before copying the handle.

### Missing items
- The current investigation still needs the exact raw ioctl sequence for the repeated `nvdrv parsed_cmd=1` loop leading into the guest abort at `0x01D1DD20`.

## 2026-03-31 — `core/src/hle/service/nvdrv/devices/nvdevice.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvdevice.h`

### Intentional differences
- Rust adds a default `register_query_event_owner(...)` adapter on the trait so persistent queried events can remember the guest owner that copied the handle. Upstream does not need this hook because it returns `KEvent*` directly and kernel ownership stays attached to the event object.

### Unintentional differences (to fix)
- No new owner mismatch identified beyond the required Rust-only adapter.

### Missing items
- None in this owner file beyond keeping the adapter narrow and only used by queried persistent events.

### Binary layout verification
- PASS: trait-only change; no guest-visible payloads or raw structs changed.

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

## 2026-03-30 — video_core/src/renderer_opengl/gl_rasterizer.rs vs video_core/renderer_opengl/gl_rasterizer.cpp

### Intentional differences
- Rust uses an injected callback to reach the owning `GPU::InvalidateGPUCache()` because `RasterizerOpenGL` is stored separately from `Gpu` and does not hold a direct `GPU&` field yet.

### Unintentional differences (to fix)
- Fixed in this pass: `invalidate_gpu_cache()` no longer returns early as a stub; it now forwards to the owner callback, matching upstream `gpu.InvalidateGPUCache()` behaviorally.

### Missing items
- direct owner field parity (`GPU& gpu`) in the OpenGL rasterizer constructor

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-30 — core/src/memory/memory.rs vs core/memory.cpp

### Intentional differences
- Rust stores `gpu_dirty_managers` as `Vec<Arc<Mutex<...>>>` instead of `std::span<...>` because the managers are owned by `System` and shared across owners without C++-style reference stability.

### Unintentional differences (to fix)
- CPU-write collection still uses a simplified core-index selection (`first()` manager) instead of the exact upstream per-host-thread slot selection and `sys_core` guard logic.
- `HandleRasterizerDownload` / `InvalidateGPUMemory` are not yet ported literally in this owner file.

### Missing items
- exact upstream `HandleRasterizerDownload`
- exact upstream `InvalidateGPUMemory`
- exact per-core dirty area tracking state

### Binary layout verification
- PASS: no guest-visible binary layout; this slice restores owner-local lifecycle and dirty-memory collection wiring.

## 2026-03-30 — core/src/core.rs vs core/core.cpp

### Intentional differences
- Rust stores `gpu_dirty_memory_managers` in `Vec<Arc<Mutex<_>>>` instead of a fixed native array to preserve cross-owner shared mutability safely.

### Unintentional differences (to fix)
- none in the `GetGPUDirtyMemoryManager` / `GatherGPUDirtyMemory` ownership slice after this change.

### Missing items
- none in this file's current dirty-memory slice

### Binary layout verification
- PASS: not layout-sensitive; ownership/method placement matched.

## 2026-03-30 — video_core/src/gpu.rs vs video_core/gpu.cpp

### Intentional differences
- Rust stores the upstream `Core::System&` as `SystemRef` behind a mutex because `Gpu` is constructed before it is finally moved into `System`.

### Unintentional differences (to fix)
- `InvalidateGPUCache()` now matches the upstream owner and behavior slice by calling `system.GatherGPUDirtyMemory(...)` and forwarding ranges to `rasterizer.on_cache_invalidation(...)`.
- `OnCPUWrite()` now delegates to `rasterizer->OnCPUWrite(...)` instead of remaining a stub.

### Missing items
- full upstream `RequestFlush` behavior
- remaining direct host1x/system integrations still owned elsewhere

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-30 — video_core/src/engines/maxwell_3d.rs vs video_core/engines/maxwell_3d.cpp

### Intentional differences
- Upstream writes report query fallback results through `memory_manager` owned directly by `Maxwell3D`; Rust preserves the same owner and ordering but routes the final CPU write through the injected `guest_memory_writer` callback because `video_core` does not own the full `Core::System` object.

### Unintentional differences (to fix)
- Fixed in this pass: `process_query_get()` no longer passes a no-op writer callback to `rasterizer.query(...)` when a rasterizer is bound. This previously allowed report semaphore queries to "succeed" without writing their payload/timestamp back to guest-visible memory, diverging from upstream `Rasterizer*::QueryFallback(...)`.

### Missing items
- Full ownership split cleanup for the Rust-only `guest_memory_reader` / `guest_memory_writer` bridges.

### Binary layout verification
- PASS: this slice changes callback wiring/order only; no guest-visible struct layout changed.

## 2026-03-30 — video_core/src/macro_engine/macro_hle.rs vs video_core/macro/macro_hle.cpp

### Intentional differences
- Upstream constructs `HLEMacro(Maxwell3D&)` once in the owner constructor. The Rust port now stores an optional raw `Maxwell3D` owner pointer and refreshes it from `MacroEngine` before execution because `Maxwell3D` contains `MacroEngine` by value and cannot hand out `&mut self` during struct construction without extra self-referential machinery.

### Unintentional differences (to fix)
- Fixed in this pass: `HLE_ClearConstBuffer<base_size>` now follows upstream behavior instead of warning-only stubs, including `RefreshParameters()`, `const_buffer` register setup, zero-fill through `ProcessCBMultiData`, and offset reset.
- Fixed in this pass: `HLE_ClearMemory` now follows upstream behavior instead of warning-only stubs, including upload register setup, `launch_dma` trigger, and zeroed `inline_data` upload.
- The other HLE macro implementations in this owner file are still stubs and must be ported as their hashes become relevant on the runtime path.

### Missing items
- owner-faithful implementations for `HLE_C713C83D8F63CCF3`, `HLE_D7333D26E0A93EDE`, `HLE_BindShader`, `HLE_SetRasterBoundingBox`, `HLE_TransformFeedbackSetup`, and the draw-indirect helpers

### Binary layout verification
- PASS: no guest-visible raw struct layout in this owner file; parity concern is owner/lifecycle and method behavior.

## 2026-03-30 — video_core/src/macro_engine/macro_engine.rs vs video_core/macro/macro.cpp

### Intentional differences
- Rust still injects the backend compiler as a closure into `execute()` instead of storing a polymorphic upstream subclass instance directly.
- `MacroEngine` now forwards a raw `Maxwell3D` owner pointer into `HleMacro` immediately before lookup/execution instead of receiving it in the constructor; this is the narrowest self-reference adaptation that preserves file ownership.

### Unintentional differences (to fix)
- Fixed in this pass: HLE macro lookup no longer operates ownerless on the runtime path; it now receives the owning `Maxwell3D` before cached HLE execution.

### Missing items
- upstream-equivalent backend ownership instead of closure injection
- dump/settings integration from upstream `Dump(...)`

### Binary layout verification
- PASS: `Opcode`/`MethodAddress` raw-bit layout unchanged by this slice.

## 2026-03-30 — video_core/src/engines/maxwell_3d.rs vs video_core/engines/maxwell_3d.cpp

### Intentional differences
- Rust exposes narrow `pub(crate)` helper methods `hle_clear_const_buffer()` and `hle_clear_memory()` in the matching owner file so `macro_hle.rs` can invoke the same owner-local behavior without breaking module boundaries. Upstream performs the body directly from the HLE macro classes through the owning `Maxwell3D&`.

### Unintentional differences (to fix)
- Fixed in this pass: the HLE runtime path now refreshes `MacroEngine` with the current `Maxwell3D` owner before both `call_macro_method()` and `flush_macro()`, so HLE macros can mutate the real engine state instead of remaining ownerless.
- Fixed in this pass: `HLE_ClearConstBuffer` and `HLE_ClearMemory` now execute through the real `Maxwell3D` owner in this file instead of stubbing out at the macro layer.
- Fixed in this pass: `ProcessCBMultiData()` now performs the upstream `memory_manager.WriteBlockCached(...)` write instead of acting as a logging-only offset bump. This makes the HLE clear-const-buffer path functionally meaningful again.

### Missing items
- upstream-faithful owner helpers for the remaining HLE macro classes
- cleanup of temporary macro trace logging once the runtime blocker is resolved

### Binary layout verification
- PASS: no struct layout changed; parity concern is owner routing and register/write ordering.

## 2026-03-30 — video_core/src/memory_manager.rs vs video_core/memory_manager.cpp

### Intentional differences
- The outer Rust `MemoryManager` wrapper still exists to preserve crate/lifetime boundaries around `Arc<Mutex<_>>`; upstream exposes the concrete owner directly.

### Unintentional differences (to fix)
- Fixed in this pass: the outer wrapper now exposes `write_block_cached(...)`, matching the upstream API surface needed by `Maxwell3D::ProcessCBMultiData()`.

### Missing items
- remaining behavioral gaps in the backend noted earlier (`PTEKind`, fuller big-page semantics, and other incomplete owner slices)

### Binary layout verification
- PASS: no guest-visible struct layout changed in this wrapper slice.

## 2026-03-30 — video_core/src/engines/maxwell_3d.rs vs video_core/engines/maxwell_3d.cpp

### Intentional differences
- Rust still stores the register file as a flat `Box<[u32; ENGINE_REG_COUNT]>` instead of a typed `Regs` union. This slice only corrects the constant placement/routing for the `const_buffer.buffer` method range within that flat owner.

### Unintentional differences (to fix)
- Fixed in this pass: `CB_DATA_BASE`/`CB_DATA_END` were off by 3 header registers. Rust was incorrectly routing writes to `const_buffer.address_high`, `const_buffer.address_low`, and `const_buffer.offset` through `ProcessCBData/ProcessCBMultiData`, whereas upstream only treats `const_buffer.buffer[0..15]` as CB inline data.
- `process_cb_multi_data()` still logs instead of performing the full upstream `memory_manager.WriteBlockCached(...)` side effect.
- `maxwell_3d.rs` still keeps some behavior that upstream owns in auxiliary engine files or richer typed register structs.

### Missing items
- full typed-owner parity for `Regs::ConstantBuffer`
- full `WriteBlockCached` side effect parity in `ProcessCBMultiData`

### Binary layout verification
- PASS: this slice corrects method index placement against upstream `Regs::ConstantBuffer`; no new binary payload layout was introduced.

## 2026-03-30 — common/src/container_hash.rs vs common/container_hash.h

### Intentional differences
- Rust keeps only the unsigned/container hashing helpers actually used by the current port, rather than every C++ template overload. This slice only corrects the arithmetic in the shared unsigned-value path.

### Unintentional differences (to fix)
- Fixed in this pass: `hash_value_unsigned()` previously performed `seed ^= value; seed += ...` as two separate steps, which does not match upstream `seed ^= value + (seed << 6) + (seed >> 2)`. That produced non-upstream `Common::HashValue` results for vectors/slices, including macro-code hashes.
- The Rust file still does not expose every upstream overload shape, only the ones currently needed by the port.

### Missing items
- any additional upstream `HashValue` overloads if/when a later owner needs them

### Binary layout verification
- PASS: not a binary-layout slice; this is pure hash-function parity.

## 2026-03-29 — video_core/src/rasterizer_interface.rs vs video_core/rasterizer_interface.h

### Intentional differences
- Rust carries an explicit guest-memory write callback in `RasterizerInterface::query(...)` because `GPUVAddr` writes are not yet owned directly by each backend as in upstream.

### Unintentional differences (to fix)
- none in this query-callback ownership slice

### Missing items
- full direct backend ownership of GPU memory writes without the Rust callback adaptation

### Binary layout verification
- PASS: interface-only file; no raw serialized layout

## 2026-03-29 — video_core/src/renderer_null/null_rasterizer.rs vs video_core/renderer_null/null_rasterizer.cpp

### Intentional differences
- Rust uses the injected guest-memory callback instead of upstream direct `gpu_memory->Write<...>()`.

### Unintentional differences (to fix)
- null backend still writes a placeholder `ticks=0` instead of upstream `m_gpu.GetTicks()`.

### Missing items
- GPU tick ownership parity for timestamped query writes

### Binary layout verification
- PASS: no guest-visible struct layout; query write ordering covered by targeted tests

## 2026-03-29 — video_core/src/renderer_opengl/gl_rasterizer.rs vs video_core/renderer_opengl/gl_rasterizer.cpp

### Intentional differences
- Rust still routes query fallback writes through an injected callback instead of upstream direct `gpu_memory` access.

### Unintentional differences (to fix)
- Fixed in this pass: `Query(..., IsAFence, ...)` now defers the guest write behind `signal_fence(...)` instead of writing immediately and signaling an empty fence.
- timestamped fallback queries still write `ticks=0` instead of upstream `gpu.GetTicks()`.

### Missing items
- full `QueryCache` / `QueryFallback` split
- upstream GPU tick sourcing in query fallback writes

### Binary layout verification
- PASS: no raw struct layout; fence ordering covered by `query_fence_defers_guest_write_until_release`

## 2026-03-29 — video_core/src/renderer_vulkan/mod.rs vs video_core/renderer_vulkan/vk_rasterizer.cpp

### Intentional differences
- Rust Vulkan query path is still a simplified fallback and uses the injected guest-memory callback rather than upstream `query_cache.CounterReport(...)`.

### Unintentional differences (to fix)
- owner signature updated in this pass to keep callback ownership compatible with deferred fence/query execution elsewhere.

### Missing items
- full `CounterReport` / `QueryCache` parity

### Binary layout verification
- PASS: no raw serialized layout in this owner file

## 2026-03-29 — video_core/src/engines/puller.rs vs video_core/engines/puller.cpp

### Intentional differences
- Rust semaphore/query paths still bridge GPU memory writes through `MemoryManager::write_block_unsafe(...)` plus the `Gpu::write_guest_memory(...)` callback because direct backend ownership remains split across crates.

### Unintentional differences (to fix)
- Fixed in this pass: the guest write callback passed to `rasterizer.query(...)` is now owned (`Arc<dyn Fn...>`) so `SemaphoreRelease` can be deferred behind a fence like upstream.

### Missing items
- direct backend ownership of query/semaphore writes without the Rust callback bridge

### Binary layout verification
- PASS: not layout-sensitive; this slice changes callback lifetime/ordering only

## 2026-03-29 — video_core/src/engines/maxwell_3d.rs vs video_core/engines/maxwell_3d.cpp

### Intentional differences
- Rust still falls back to a no-op guest write callback when `Maxwell3D` emits report semaphore queries without a backend-owned memory writer.

### Unintentional differences (to fix)
- owner signature updated in this pass to keep query callbacks compatible with deferred execution semantics.

### Missing items
- direct backend ownership of report-semaphore writeback destination

### Binary layout verification
- PASS: query register ownership unchanged; this slice only updates callback lifetime semantics

## 2026-03-29 — video_core/src/fence_manager.rs vs video_core/fence_manager.h

### Intentional differences
- Rust stores queued callbacks as `Box<dyn FnOnce() + Send>` and uses `Arc<Mutex<...>>` fence owners instead of the upstream template/`shared_ptr` shape, but the ownership still remains in the same owner file.

### Unintentional differences (to fix)
- `tick_frame()` and fence release ordering are now owner-local and match the upstream lifecycle for queued fences/operations in this slice.

### Missing items
- none in this generic fence lifecycle slice

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — video_core/src/renderer_opengl/gl_fence_manager.rs vs video_core/renderer_opengl/gl_fence_manager.cpp

### Intentional differences
- Rust implements `FenceBase` for `Arc<Mutex<GLInnerFence>>` instead of using upstream inheritance and `std::shared_ptr`.

### Unintentional differences (to fix)
- none in the `GLInnerFence` queue/is_signaled/wait slice after this change

### Missing items
- none in this owner-local backend slice

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — video_core/src/renderer_opengl/gl_rasterizer.rs vs video_core/renderer_opengl/gl_rasterizer.cpp

### Intentional differences
- Rust keeps `FenceManagerOpenGL` and the generic `FenceManager<Fence>` as explicit fields rather than upstream nested ownership, but both stay in the same owner file/module boundary.

### Unintentional differences (to fix)
- `signal_fence`, `sync_operation`, `signal_sync_point`, `signal_reference`, `release_fences`, `wait_for_idle`, `flush_commands`, and `tick_frame` now route through the fence manager with the same owner responsibilities as upstream.

### Missing items
- Full rasterizer behavior outside the fence/sync slice remains elsewhere in this owner file.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — video_core/src/engines/maxwell_3d.rs vs video_core/engines/maxwell_3d.cpp

### Intentional differences
- Upstream owns `Core::System&` and `MemoryManager&` directly in `Maxwell3D`; the Rust port still threads guest-memory reads through an injected callback and stores the `MemoryManager` behind `Arc<Mutex<_>>` to preserve crate boundaries.
- Upstream reads `Regs::ReportSemaphore::Compare` directly into the C++ struct; Rust currently decodes the same 24-byte block manually in this owner file.

### Unintentional differences (to fix)
- Fixed in this pass: `RenderEnable::Override` now matches upstream enum semantics (`UseRenderEnable = 0`, `AlwaysRender = 1`, `NeverRender = 2`) instead of the previous shifted mapping.
- Fixed in this pass: `ProcessQueryGet()` now uses the upstream register owner and query semantics, including `HasTimeout`/`IsAFence` flag derivation and `rasterizer->Query(...)` as the primary path.
- Fixed in this pass: `REPORT_SEMAPHORE_QUERY` now matches `MAXWELL3D_REG_INDEX(report_semaphore.query)` instead of a stale raw byte offset that incorrectly routed the write through macro processing.
- Fixed in this pass: the local high-register regression test now verifies the upstream word-indexed register owner instead of indexing the raw byte offset.

### Missing items
- fuller `Core::System` ownership parity in the constructor
- direct `memory_manager.ReadBlock(...)` owner wiring instead of the current callback bridge

### Binary layout verification
- PASS: register indices now use upstream word-indexed placement for the corrected `report_semaphore.query` owner.

## 2026-03-29 — `video_core/src/engines/puller.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/engines/puller.{h,cpp}`

### Intentional differences
- Rust still stores `GPU`, `MemoryManager`, `DmaPusher`, and `ChannelState` through raw pointers / `Arc<Mutex<_>>` bridges instead of direct C++ references, to preserve crate boundaries without changing owner placement.

### Unintentional differences (to fix)
- Fixed in this pass: `ProcessSemaphoreTriggerMethod()` now forwards the rasterizer query writeback into GPU memory, matching the upstream `rasterizer->Query(...)` side effect instead of discarding the write with a no-op callback.
- Fixed in this pass: the puller register file now matches upstream `Regs::NUM_REGS = 0x800` instead of truncating to `0x40`.

### Missing items
- Upstream `Regs` still exposes the named acquire-state registers directly in the owner struct; the Rust port still accesses them through helper methods over the backing array.

### Binary layout verification
- PASS: no guest-visible binary layout; owner-local register file size and method side effects now match upstream more closely.

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
- Rust still keeps several query and draw-manager-adjacent paths simplified in this owner file.

### Unintentional differences (to fix)
- Fixed in this pass: `bind_rasterizer()` now exists in the owner file and `process_method_call()` forwards the upstream rasterizer-backed hooks for `WaitForIdle`, cache barriers, syncpoints, counter reset, and constant-buffer bind operations.
- Fixed in this pass: `LAUNCH_DMA` and `INLINE_DATA` in `process_method_call()` / `call_multi_method()` now execute through the owner-local `upload_state`, matching the upstream `upload_state.ProcessExec(...)` and `upload_state.ProcessData(...)` dispatch points.

### Missing items
- Upstream `upload_state` still owns `MemoryManager&` and `RasterizerInterface*` directly, while Rust still supplies them through temporary callbacks/context to preserve current crate boundaries.
- `process_counter_reset()` still uses a temporary query-type mapping that should be aligned with the upstream enums.
- Additional rasterizer-backed paths in this owner file remain simplified.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — `video_core/src/engines/engine_upload.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/engines/engine_upload.{h,cpp}`

### Intentional differences
- Upstream `Upload::State` stores `MemoryManager&`, `Registers&`, and `RasterizerInterface*` directly as owner fields. Rust still uses an owner-local `FlushContext` callback bundle at flush time to avoid cross-crate lifetime/self-reference issues.
- Upstream block-linear flush reads existing GPU memory through `GpuGuestMemoryScoped<... SafeReadCachedWrite>`. Rust still zero-fills unread portions because this owner file does not yet receive a direct guest-memory read callback for the block-linear path.

### Unintentional differences (to fix)
- Fixed in this pass: the owner file now talks to the outer `video_core::memory_manager::MemoryManager` API with `read_block(...)` and `write_block(...)`, matching the upstream owner boundary instead of calling nonexistent internal-style `read(...)` / `write(...)` methods.

### Missing items
- Direct upstream-style ownership of `MemoryManager&` and `RasterizerInterface*`.
- Upstream-equivalent safe cached read behavior for block-linear uploads.

### Binary layout verification
- PASS: `Registers` / `DestRegisters` remain owned in the matching file and preserve the upstream field order used by the upload logic.

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
- Fixed in this pass: `Step()` now mirrors the upstream `current_dirty = memory_manager.IsMemoryDirty(...)` pre-check for the macro-heavy `Maxwell3D` path before fetching command headers.

### Missing items
- `DmaPusher` still lacks the full upstream subchannel object table and `BindSubchannel` owner API.
- `DispatchCalls()` still does not implement the upstream `system.IsPoweredOn()` loop or rasterizer flush behavior exactly.
- The upstream safe/unsafe guest-memory fetch split based on accuracy and macro/compute cases remains simplified.
- The compute-side `current_dirty` equivalent is still absent because `kepler_compute.rs` does not yet expose the corresponding owner-local state/hooks.

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

## 2026-03-30 — `video_core/src/renderer_opengl/gl_rasterizer.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.cpp`

### Intentional differences
- Rust still keeps the simplified fallback query path and does not yet own the upstream OpenGL query cache / `GPU&` integration needed for `gpu.GetTicks()` and typed query-cache backends.

### Unintentional differences (to fix)
- Fixed in this pass: fallback `Query()` now matches upstream `QueryFallback()` for non-payload query types by forcing the written payload to `1` instead of echoing the caller-supplied payload.
- Fixed in this pass: `signal_fence()` now triggers `invalidate_gpu_cache()` after queueing/flushing the fence, matching the upstream `FenceManager::SignalFence()` lifecycle that ends with `rasterizer.InvalidateGPUCache()`.

### Missing items
- Real `gpu.GetTicks()` sourcing in the fallback path is still missing.
- The typed `MaxwellToVideoCoreQuery()` / `query_cache.Query()` owner path is still not ported.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-30 — `video_core/src/renderer_null/null_rasterizer.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_null/null_rasterizer.cpp`

### Intentional differences
- Rust still injects the Host1x syncpoint owner directly instead of storing the upstream `GPU&`.

### Unintentional differences (to fix)
- Fixed in this pass: `Query()` now matches upstream `RasterizerNull::Query()` fallback behavior for non-payload query types by writing `1` instead of the raw payload.

### Missing items
- Real GPU tick sourcing is still missing in the timeout path because this owner still does not have upstream `GPU&`.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

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

## 2026-03-29 — `video_core/src/memory_manager.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/memory_manager.{h,cpp}`

### Intentional differences
- Rust still keeps guest-memory access behind callback-based read/write closures instead of owning upstream `Core::System` and `MaxwellDeviceMemoryManager` directly.
- The invalidation accumulator and full safe/unsafe cache-type split are still simplified.

### Unintentional differences (to fix)
- Fixed in this pass: `BindRasterizer()` no longer records only a boolean. The owner file now stores the rasterizer edge and uses it for upstream-visible `ModifyGPUMemory`, `UnmapMemory`, `FlushRegion`, `InvalidateRegion`, and `MustFlushRegion` callbacks.
- Fixed in this pass: the earlier bounded Rust `GetMemoryLayoutSize(gpu_addr, max_size)` change was unwound. The method now matches upstream again and ignores `max_size`.

### Missing items
- The full upstream `Core::System` / `MaxwellDeviceMemoryManager` ownership and pointer-continuity check for big pages are still adapted in Rust.
- `FlushCaching()` and the invalidation accumulator still do not match the upstream implementation.
- Safe vs unsafe read/write cache behavior remains simplified.

### Binary layout verification
- PASS: page-table entry packing and public wrapper layout remain unchanged; only owner-local rasterizer behavior changed.

## 2026-03-29 — `video_core/src/gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.{h,cpp}`

### Intentional differences
- Rust still uses the `GpuCoreInterface` bridge because `core` and `video_core` are split crates, unlike upstream.

### Unintentional differences (to fix)
- Fixed in this pass: `Gpu::bind_channel()` no longer only records `bound_channel`. It now also performs the upstream-visible rasterizer side effect `BindChannel(current_channel)`.
- Fixed in this pass: `init_address_space()` now passes the actual rasterizer object into `MemoryManager::bind_rasterizer(...)` instead of toggling a placeholder boolean.

### Missing items
- The bridge layer (`GpuCoreInterface` / handles) is still a structural divergence from upstream and should be unwound if the crate boundary changes.
- Additional upstream GPU lifecycle methods still remain simplified elsewhere in this file.

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
## 2026-03-29 — video_core/src/dma_pusher.rs vs video_core/dma_pusher.cpp

### Intentional differences
- `EngineInterface*` / `RasterizerInterface*` are stored as Rust fat-pointer payloads (`[usize; 2]`) instead of raw C++ pointers: required to preserve trait-object vtable metadata.

### Unintentional differences (to fix)
- `system.IsPoweredOn()` loop guard is still not owned by `DmaPusher`: Rust still loops until `step()` returns false.
- safe/unsafe GPU memory fetch split is still simplified compared to upstream.

### Missing items
- full `system` ownership in constructor and `gpu.FlushCommands()` / `gpu.OnCommandListEnd()` parity on all dispatch paths

### Binary layout verification
- PASS: `CommandListHeader` and `CommandHeader` remain raw-bit packed wrappers.

## 2026-03-29 — video_core/src/engines/puller.rs vs video_core/engines/puller.cpp

### Intentional differences
- `DmaPusher&` is represented as a late-bound raw pointer set after `DmaPusher` construction to work around Rust self-referential construction.

### Unintentional differences (to fix)
- `bound_engines` still uses `Option<EngineID>` instead of a fully initialized fixed array value model.

### Missing items
- none in the `ProcessBindMethod -> BindSubchannel` ownership slice

### Binary layout verification
- PASS: puller register file remains word-indexed at upstream offsets.

## 2026-03-29 — video_core/src/engines/kepler_compute.rs vs video_core/engines/kepler_compute.cpp

### Intentional differences
- Rust still keeps the higher-level recorded-dispatch representation (`DispatchCall` / `QueueMetaData`) alongside the upstream-like `EngineInterfaceState`.

### Unintentional differences (to fix)
- upload handling (`upload_state`, `uploads`, `indirect_compute`) is still incomplete relative to upstream.

### Missing items
- `ConsumeSinkImpl` exact upstream body for upload-related state
- `GetIndirectComputeAddress`
- `GetTICEntry`
- `GetTSCEntry`

### Binary layout verification
- PASS: launch-related register indices kept at upstream word offsets.

## 2026-03-29 — video_core/src/engines/kepler_memory.rs vs video_core/engines/kepler_memory.cpp

### Intentional differences
- Rust still uses the owner-local `engine_upload::State` abstraction instead of storing upstream references directly.

### Unintentional differences (to fix)
- none in the `EngineInterface` ownership slice

### Missing items
- full flush-context wiring for upload completion

### Binary layout verification
- PASS: exec/data register ownership and execution mask placement match upstream.

## 2026-03-29 — video_core/src/engines/fermi_2d.rs vs video_core/engines/fermi_2d.cpp

### Intentional differences
- simplified software blit backend remains compared to upstream accelerated paths.

### Unintentional differences (to fix)
- `Fermi2D` backend behavior is still heavily simplified beyond the `EngineInterface` ownership slice.

### Missing items
- full blitter/rasterizer acceleration parity

### Binary layout verification
- PASS: trigger method remains executable through the upstream-owned `execution_mask`.

## 2026-03-29 — video_core/src/engines/maxwell_dma.rs vs video_core/engines/maxwell_dma.cpp

### Intentional differences
- Rust keeps a simplified DMA execution backend while preserving the upstream `launch_dma` trigger ownership.

### Unintentional differences (to fix)
- full launch semantics, remap behavior, and acceleration paths remain simplified.

### Missing items
- full DMA launch behavior parity

### Binary layout verification
- PASS: `launch_dma` remains the executable register in the engine-local `execution_mask`.
## 2026-03-29 — video_core/src/control/scheduler.rs vs video_core/control/scheduler.cpp

### Intentional differences
- `shared_ptr<ChannelState>` is represented as `Arc<Mutex<ChannelState>>`, so the Rust port must shorten lock scope where upstream reads fields lock-free.

### Unintentional differences (to fix)
- none in the `Push()` lock-order slice

### Missing items
- none in this file's current port slice

### Binary layout verification
- PASS: not layout-sensitive; ownership/order checked against upstream

## 2026-03-29 — video_core/src/dma_pusher.rs vs video_core/dma_pusher.cpp

### Intentional differences
- Rust still stores engine trait objects through a fat-pointer representation because `std::array<EngineInterface*>` has no direct safe Rust equivalent.

### Unintentional differences (to fix)
- The first Rust port installed `Puller`'s back-pointer to `DmaPusher` before the final `Box` move, so `ProcessBindMethod()` wrote subchannel bindings into stale storage instead of the live `DmaPusher`.

### Missing items
- none in the stable self-reference ownership slice

### Binary layout verification
- PASS: `CommandHeader`/`CommandListHeader` bitfield extraction remains unchanged; this slice only fixes object lifetime/order.

## 2026-03-29 — video_core/src/control/channel_state.rs vs video_core/control/channel_state.cpp

### Intentional differences
- Rust must perform one extra explicit step after `Box<DmaPusher>` construction to install the stable self-reference used by `Puller`.

### Unintentional differences (to fix)
- none in the `DmaPusher` construction ordering slice after this change

### Missing items
- none in this ownership-order slice

### Binary layout verification
- PASS: not layout-sensitive; parity concern is initialization order only.

## 2026-03-29 — video_core/src/engines/maxwell_3d.rs vs video_core/engines/maxwell_3d.cpp

### Intentional differences
- Upstream owns `Core::System&` and `MemoryManager&` directly in `Maxwell3D`; the Rust port currently threads guest-memory reads through an injected callback and stores the `MemoryManager` behind `Arc<Mutex<_>>` to preserve crate boundaries.
- Upstream owns most draw-side helpers in `video_core/engines/draw_manager.cpp`; the Rust port still has `process_draw_method_call()` absorbed into `maxwell_3d.rs`. This slice only restores the missing upstream behaviors for `DrawIndexSmall`, `VertexArrayInstanced`, and `DrawTexture` inside the current owner.

### Unintentional differences (to fix)
- Fixed in this pass: the Rust-only compatibility wrapper `Engine::write_reg()` now delegates to `EngineInterface::call_method(...)` instead of bypassing side effects through `process_method()`, so runtime command processing and tests observe the same lifecycle as upstream `CallMethod`.
- `refresh_parameters_impl()` is now wired in the correct owner, but still uses a Rust callback bridge instead of the direct upstream `memory_manager.ReadBlock(...)` call path.
- Full structural ownership parity with `draw_manager.rs` is still missing; `DrawManager::ProcessMethodCall()` remains split between files instead of being restored to the dedicated owner.

### Missing items
- fuller `Core::System` ownership parity in the constructor
- full file-ownership parity for `DrawManager` logic

### Binary layout verification
- PASS: not layout-sensitive; this slice is lifecycle/ownership parity for macro parameter refresh.

## 2026-03-29 — video_core/src/macro_engine/macro_engine.rs vs video_core/macro/macro.cpp

### Intentional differences
- Rust replaces the virtual `Compile` method with a closure parameter on `execute()` because `MacroEngine` is stored as a concrete owner and the backend is injected at call time.

### Unintentional differences (to fix)
- macro execution backend selection still depends on a Rust closure instead of a concrete upstream subclass instance held by the engine owner.
- dump/settings integration from upstream `Dump(...)` and `Settings::values.*` is still absent.

### Missing items
- upstream-equivalent backend factory ownership (`MacroInterpreter`/JIT owner held directly)
- macro dump/settings path wiring

### Binary layout verification
- PASS: `Opcode` and `MethodAddress` raw-bit layout remain represented in the same owner file.

## 2026-03-29 — `video_core/src/dma_pusher.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/dma_pusher.{h,cpp}`

### Intentional differences
- Rust still uses callback-based guest-memory fetch and does not yet mirror the full upstream safe/unsafe read policy or `system.IsPoweredOn()` dependency exactly.

### Unintentional differences (to fix)
- Fixed in this pass: `dispatch_calls()` now finishes with the same owner-local lifecycle as upstream by calling `gpu.flush_commands()` and `gpu.on_command_list_end()` after draining the pushbuffer.

### Missing items
- Full upstream `system.IsPoweredOn()` loop condition and remaining safe/unsafe fetch parity.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-29 — `video_core/src/gpu.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.{h,cpp}`

### Intentional differences
- Rust still routes ownership through mutexes and callback bridges where upstream stores direct references/pointers in `GPU::Impl`.

### Unintentional differences (to fix)
- Fixed in this pass: `flush_commands()` now delegates to `rasterizer->FlushCommands()`.
- Fixed in this pass: `on_command_list_end()` now delegates to `rasterizer->ReleaseFences(false)` like upstream, instead of returning early as a stub.

### Missing items
- Upstream `Settings::UpdateGPUAccuracy()` side effect in `OnCommandListEnd()`.
- Full `host1x.MemoryManager().BindInterface(...)` and `host1x.GMMU().BindRasterizer(...)` parity remains elsewhere in this owner file.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.
## 2026-03-30 — `video_core/src/fence_manager.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/fence_manager.h`

### Intentional differences
- Rust keeps the upstream CRTP fence manager as a generic struct with callback parameters instead of C++ template inheritance, while preserving owner placement and method boundaries in this file.

### Unintentional differences (to fix)
- Fixed in this pass: `signal_fence()` no longer defers every callback unconditionally. It now matches upstream `delay_fence = Settings::IsGPULevelHigh()`, executing callbacks immediately outside GPU-high mode and only deferring them when GPU-high accuracy is enabled.

### Missing items
- Async fence-thread parity for the `can_async_check` path is still simplified compared to upstream OpenGL/Vulkan implementations.
- Upstream `CommitAsyncFlushes`, `ShouldFlush`, and `PopAsyncFlushes` helpers are still collapsed into caller-side behavior instead of being fully ported into this owner.

### Binary layout verification
- PASS: no raw serialized structs in this file.

## 2026-03-30 — `video_core/src/renderer_opengl/gl_rasterizer.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.cpp`

### Intentional differences
- Rust still stores the upstream `GPU` cache invalidation edge as an `invalidate_gpu_cache_callback` instead of calling back through the full `GPU` owner directly, because the current `RasterizerOpenGL` port remains split from the full upstream object graph.

### Unintentional differences (to fix)
- Fixed in this pass: `signal_reference()` no longer degrades to ordering-only fence release. It now queues a real no-op fence through `signal_fence()`, matching upstream `FenceManager::SignalReference() -> SignalFence(do_nothing)`.

### Missing items
- Large portions of OpenGL rendering state/cache logic remain unported in this owner file.

### Binary layout verification
- PASS: no raw serialized structs in this file.
## 2026-03-30 — video_core/src/engines/maxwell_3d.rs vs video_core/engines/maxwell_3d.cpp

### Intentional differences
- Rust keeps the register file as flat `u32` arrays instead of the upstream typed `Regs` union. This preserves owner placement in the same file but not the upstream binary struct layout.

### Unintentional differences (to fix)
- `Maxwell3D::new()` previously skipped upstream `InitializeRegisterDefaults()`: fixed. The Rust constructor now applies the upstream boot-time defaults for blend, stencil, color masks, vertex-attribute constant bits, rasterization enables, and line widths before copying them into `shadow_state`.
- Viewport and typed register-field defaults are still only partially represented through flat register writes, not through the full upstream typed `Regs` substructures.

### Missing items
- Full typed parity for the remaining `InitializeRegisterDefaults()` fields, especially the typed viewport/viewports defaults and any still-unmodeled boot values.

### Binary layout verification
- FAIL: the Rust engine still uses flat register arrays, not the upstream binary `Regs` layout.
## 2026-03-30 — video_core/src/renderer_opengl/gl_rasterizer.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.cpp

### Intentional differences
- OpenGL backend ownership stays in `gl_rasterizer.rs`, but Rust uses trait-object plumbing and test-only hooks that do not exist in the upstream C++ file.

### Unintentional differences (to fix)
- `RasterizerOpenGL::signal_reference()` had drifted to `signal_fence(do_nothing)`: fixed. It now matches upstream and calls `FenceManager::SignalOrdering()` instead of queueing a reference fence.

### Missing items
- `FenceManager::signal_ordering()` still lacks the full upstream cache-accumulation side effects because the current generic Rust fence manager does not directly own texture/buffer/query caches.

### Binary layout verification
- PASS: no raw shared binary layout in this file.
## 2026-03-30 — video_core/src/fence_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/fence_manager.h

### Intentional differences
- Rust garde un `FenceManager<F>` générique et injecte les dépendances rasterizer/cache par callbacks d’appel plutôt que par ownership direct `rasterizer/gpu/texture_cache/buffer_cache/query_cache`. C’est une adaptation mécanique à la structure actuelle des owners Rust, tout en restaurant l’ordre upstream dans `signal_fence`, `signal_sync_point`, `signal_ordering` et `wait_pending_fences`.

### Unintentional differences (to fix)
- `HAS_ASYNC_CHECK` n’existe toujours pas comme axe structurel réel en Rust; le manager ne possède pas encore le vrai thread de release upstream.
- Le manager ne possède pas encore directement les caches/rasterizer comme l’upstream `FenceManager<Traits>`, donc `ShouldWait`, `ShouldFlush`, `CommitAsyncFlushes`, `PopAsyncFlushes` et `AccumulateFlushes` restent délégués au callsite.

### Missing items
- Parité ownership complète du template upstream avec caches/rasterizer en membres directs.
- Release thread upstream pour le mode async-check.

### Binary layout verification
- PASS: fichier runtime-only, sans struct sérialisée en raw bytes.

## 2026-03-30 — video_core/src/renderer_opengl/gl_rasterizer.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.cpp

### Intentional differences
- `RasterizerOpenGL` Rust reste encore très allégé et n’own pas les vrais `TextureCache`, `BufferCache`, `QueryCache` OpenGL upstream. Les callbacks passés à `FenceManager` sont donc actuellement `false`/no-op pour les chemins cache.

### Unintentional differences (to fix)
- `SignalFence`, `SignalSyncPoint`, `SignalReference` et `ReleaseFences` suivent maintenant l’ordre upstream via `FenceManager`, mais sans les vrais caches OpenGL derrière; la visibilité complète des flushes async n’est donc pas rétablie.
- `FlushCommands`/`InvalidateGPUCache` restent appelés côté rasterizer après `FenceManager::signal_fence(...)` au lieu d’être owned directement par le manager comme upstream.

### Missing items
- Vrais owners `texture_cache`, `buffer_cache`, `query_cache` OpenGL dans `RasterizerOpenGL`.
- Passage des vrais `ShouldWaitAsyncFlushes`/`HasUncommittedFlushes`/`CommitAsyncFlushes`/`PopAsyncFlushes`/`AccumulateFlushes` à `FenceManager`.

### Binary layout verification
- PASS: fichier runtime-only, sans layout binaire partagé.

## 2026-03-30 — `core/src/hle/kernel/k_condition_variable.rs` vs `/home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.cpp`

### Intentional differences
- Rust garde une boucle de polling/yield dans `wait_for_current_thread()` au lieu d’un vrai `KScopedSchedulerLockAndSleep` kernel-side. C’est une adaptation mécanique à l’absence de blocage host-thread identique au C++, mais le point d’observation reste dans le bon owner file.

### Unintentional differences (to fix)
- Fixed in this pass: the guest-thread branch of `wait_for_current_thread()` only called `KScheduler::schedule_raw_if_needed()` once, then returned even if the thread was still `WAITING`. Upstream does not return from `WaitForAddress` / `Wait` before the thread is actually resumed. The Rust path now loops until the thread leaves `WAITING`, matching the upstream wait lifecycle.

### Missing items
- Full upstream `KScopedSchedulerLockAndSleep` ownership and timer integration still remain unported in this owner.
- `UpdateLockAtomic` still uses serialized process-memory read/modify/write instead of the upstream exclusive-monitor loop.

### Binary layout verification
- PASS: runtime-only file, no raw shared binary struct layout.
## 2026-03-30 — core/src/hle/kernel/k_address_arbiter.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_address_arbiter.cpp

### Intentional differences
- `KAddressArbiter` stores the owning process memory directly instead of `Core::System&` / `KernelCore&`: this preserves the real upstream owner (`KProcess::m_address_arbiter`) without introducing a new global kernel `Arc` in Rust.
- wait/signal internals still use `Condvar` + per-address waiter counts instead of upstream `KConditionVariable::ThreadTree` + scheduler lock objects: temporary behavioral simplification already present before this slice.

### Unintentional differences (to fix)
- `WaitIfLessThan` / `WaitIfEqual` still do not build the full upstream `ThreadQueueImplForKAddressArbiter` + `KScopedSchedulerLockAndSleep` path, so scheduling / cancellation parity is incomplete.
- atomic guest-memory updates still use `RwLock` writes instead of the upstream exclusive monitor loop.

### Missing items
- full `ThreadTree` ownership matching upstream
- proper timer-cancel / thread-cancel integration during address-arbiter waits
- exact exclusive monitor semantics

### Binary layout verification
- PASS: no raw shared struct layout involved in this slice

## 2026-03-30 — video_core/src/renderer_null/null_rasterizer.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_null/null_rasterizer.cpp

### Intentional differences
- Rust still stores `SyncpointManager` directly instead of the upstream `GPU& m_gpu`, because the current `RasterizerInterface` adaptation passes narrower owners than the C++ class graph.

### Unintentional differences (to fix)
- fixed in this pass: `Query()` no longer overwrites non-payload queries with `1`; it now writes the provided payload, matching upstream.
- `InitializeChannel` / `BindChannel` / `ReleaseChannel` are still no-op stubs, while upstream routes them through `ChannelSetupCaches<ChannelInfo>`.
- timeout queries still write `ticks=0` because `Gpu::get_ticks()` remains a placeholder in the Rust owner.

### Missing items
- route channel lifecycle through the upstream-equivalent `ChannelSetupCaches<ChannelInfo>`
- real GPU tick source for timeout query writes

### Binary layout verification
- PASS: runtime-only file, no raw shared struct layout.

## 2026-03-30 — core/src/hle/kernel/kernel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.cpp

### Intentional differences
- Rust keeps `Arc<Mutex<KThread>>` in thread-local storage for `CURRENT_THREAD`/dummy-host-thread state instead of upstream raw `KThread*`. This preserves the same owner/lifecycle boundary while fitting Rust ownership.

### Unintentional differences (to fix)
- fixed in this pass: `KernelCore::register_host_thread()` no longer only allocated a host-thread ID. It now also installs either the provided existing thread or a lazily created dummy host thread as the current emulation thread, matching upstream `RegisterHostThread(existing_thread)` and `GetHostDummyThread(...)`.
- fixed in this pass: `run_on_host_core_process()` no longer spawned a bare host OS thread. It now creates a dummy `KThread`, associates it with a host process owner, and calls `register_host_thread_with_existing(...)` inside the spawned thread, matching upstream `RunHostThreadFunc(...)`.

### Missing items
- `RunOnHostCoreThread` still has no direct Rust counterpart file/method.
- host-thread dummy thread registration still does not register the dummy thread in the global kernel object list the way upstream `KThread::Register(kernel, thread)` does for explicit host-core process threads.

### Binary layout verification
- PASS: lifecycle/owner file only; no raw shared struct layout changed.

## 2026-03-30 — core/src/hle/kernel/k_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.cpp

### Intentional differences
- Rust models dummy-thread ownership with `Option<&Arc<Mutex<KProcess>>>` instead of upstream raw `KProcess*`.

### Unintentional differences (to fix)
- fixed in this pass: `initialize_dummy_thread(...)` now exists in the owner file and sets dummy-thread priority/core/type/disable-count like upstream `KThread::InitializeDummyThread`.

### Missing items
- the broader `Initialize(...)` / `InitializeThread(...)` helper layering still remains structurally reduced versus upstream.

### Binary layout verification
- PASS: no guest-visible raw struct layout changed in this slice.

## 2026-03-30 — core/src/core.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/core.cpp

### Intentional differences
- none in this pass

### Unintentional differences (to fix)
- fixed in this pass: `System::register_core_thread()` and `System::register_host_thread()` no longer remain stubs; they now delegate to `KernelCore` like upstream.

### Missing items
- none in this slice

### Binary layout verification
- PASS: owner/delegation file only; no raw shared struct layout changed.

## 2026-03-30 — video_core/src/renderer_opengl/gl_rasterizer.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.cpp

### Intentional differences
- `RasterizerOpenGL` Rust still uses a heavily reduced owner set and delegates actual rendering to the software rasterizer, unlike the full upstream cache/pipeline stack.

### Unintentional differences (to fix)
- fixed in this pass: `query()` no longer overwrites non-payload queries with `1`; it now preserves the payload like upstream.
- timeout queries still write `ticks=0` because the Rust GPU tick path is not yet wired to `CoreTiming`.

### Missing items
- real GPU tick source for timeout query writes
- full OpenGL cache and pipeline ownership matching upstream

### Binary layout verification
- PASS: runtime-only file, no raw shared struct layout.

## 2026-03-30 — core/src/hle/kernel/k_process.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_process.h

### Intentional differences
- the Rust owner wires `m_address_arbiter` with `SharedProcessMemory` directly at `KProcess::new()`, because the current Rust kernel still stores `KernelCore` by value instead of behind a shared owning pointer.

### Unintentional differences (to fix)
- `KProcess::WaitAddressArbiter` / `SignalAddressArbiter` now exist in the correct owner, but they still delegate to the simplified Rust `KAddressArbiter`, not the full upstream implementation.

### Missing items
- full upstream `KAddressArbiter` behavior behind these wrappers

### Binary layout verification
- PASS: owner-only method slice, no serialized layout change

## 2026-03-30 — core/src/hle/kernel/svc/svc_address_arbiter.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/svc/svc_address_arbiter.cpp

### Intentional differences
- none

### Unintentional differences (to fix)
- fixed: `WaitForAddress` / `SignalToAddress` no longer instantiate a throwaway local `KAddressArbiter`; they now delegate to the upstream owner `KProcess::m_address_arbiter`.
- timeout conversion and enum validation still need broader runtime validation against real games.

### Missing items
- none for owner placement in this file

### Binary layout verification
- PASS: no raw shared struct layout involved in this slice

## 2026-03-30 — video_core/src/gpu.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.{h,cpp}

### Intentional differences
- Rust stores the upstream `Core::System&` as `SystemRef` behind a mutex because the crate split prevents the exact `GPU::Impl` object graph.

### Unintentional differences (to fix)
- fixed in this pass: `Gpu::get_ticks()` no longer returns a stubbed zero. It now reads `system.CoreTiming().GetGPUTicks()` and applies `Settings::values.use_fast_gpu_time` scaling like upstream.

### Missing items
- `renderer_frame_end_notify()`, CPU-context ownership, and remaining renderer/host1x lifecycle details are still structurally reduced versus upstream `GPU::Impl`.
- `BindRenderer()` still does not directly perform the full upstream `host1x.MemoryManager().BindInterface(...)` and `host1x.GMMU().BindRasterizer(...)` wiring in this owner file.

### Binary layout verification
- PASS: no guest-visible binary layout in this owner file.

## 2026-03-30 — video_core/src/rasterizer_interface.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/rasterizer_interface.h

### Intentional differences
- Rust temporarily threads `gpu_ticks` through `RasterizerInterface::query(...)` because the current crate split does not let each rasterizer own an upstream-equivalent `GPU&`. This is a Rust-only adaptation to preserve the upstream query timestamp behavior.

### Unintentional differences (to fix)
- none in this pass

### Missing items
- restore direct owner access to `GPU::GetTicks()` from rasterizer owners once the crate graph can represent the upstream object graph without an extra parameter.

### Binary layout verification
- PASS: trait/interface file, no raw shared struct layout.

## 2026-03-30 — video_core/src/control/channel_state.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/control/channel_state.cpp

### Intentional differences
- Rust still injects owner-local callbacks into `Maxwell3D` because it cannot hand the exact upstream `Core::System&` / `GPU&` graph through constructors.

### Unintentional differences (to fix)
- fixed in this pass: `ChannelState::init()` now wires a `gpu_ticks_getter` bridge into `Maxwell3D`, so semaphore/query timestamp writes can use real GPU ticks instead of a stubbed zero.

### Missing items
- remaining constructor/lifecycle parity for other owner-local bridges in `ChannelState::Init(...)`.

### Binary layout verification
- PASS: owner-only lifecycle file, no raw shared struct layout.

## 2026-03-30 — video_core/src/engines/maxwell_3d.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/engines/maxwell_3d.cpp

### Intentional differences
- Rust uses an owner-local callback for `system.GPU().GetTicks()` because this engine still lacks the exact upstream `Core::System&` ownership chain.

### Unintentional differences (to fix)
- fixed in this pass: long query/semaphore report writes no longer force a zero timestamp when a rasterizer is present; they now source GPU ticks through the owner-local bridge.

### Missing items
- full upstream `Core::System&` ownership instead of callback bridges.

### Binary layout verification
- PASS: no raw serialized struct layout changed in this slice.

## 2026-03-30 — video_core/src/renderer_null/null_rasterizer.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_null/null_rasterizer.cpp

### Intentional differences
- Rust still stores `SyncpointManager` directly instead of the upstream `GPU& m_gpu`, because the current `RasterizerInterface` adaptation passes narrower owners than the C++ class graph.

### Unintentional differences (to fix)
- fixed in this pass: timeout `Query()` writes no longer force `ticks=0`; they now consume the propagated GPU tick value and write the same timestamp/payload layout as upstream.
- `InitializeChannel` / `BindChannel` / `ReleaseChannel` are still no-op stubs, while upstream routes them through `ChannelSetupCaches<ChannelInfo>`.

### Missing items
- route channel lifecycle through the upstream-equivalent `ChannelSetupCaches<ChannelInfo>`

### Binary layout verification
- PASS: runtime-only file, no raw shared struct layout.

## 2026-03-30 — video_core/src/renderer_opengl/gl_rasterizer.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.cpp

### Intentional differences
- `RasterizerOpenGL` Rust still uses a heavily reduced owner set and delegates actual rendering to the software rasterizer, unlike the full upstream cache/pipeline stack.

### Unintentional differences (to fix)
- fixed in this pass: timeout `query()` writes no longer force `ticks=0`; they now preserve the payload and consume propagated GPU ticks like upstream.

### Missing items
- full OpenGL cache and pipeline ownership matching upstream

### Binary layout verification
- PASS: runtime-only file, no raw shared struct layout.

## 2026-03-30 — core/src/hle/kernel/k_scheduler.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_scheduler.cpp

### Intentional differences
- The Rust port currently skips the upstream idle-core migration pass inside `UpdateHighestPriorityThreadsImpl`. This is a temporary containment for a Rust-only backend limitation: `common/src/fiber.rs` does not yet faithfully support the upstream cross-host-thread fiber exchange semantics, so migrating a runnable thread between core schedulers can resume its host fiber on the wrong host core and corrupt runtime thread-local state.
- `set_scheduler_current_thread(...)` is a Rust-only helper to keep `current_thread_id`, `current_thread`, and thread-local `CurrentThread` bookkeeping coherent in the absence of the upstream raw `SetCurrentThread(m_kernel, next_thread)` global path.

### Unintentional differences (to fix)
- Full upstream idle-core migration remains unported until `common/src/fiber.rs` reaches behavioral parity for cross-thread fiber exchange.
- The file still contains more logging and investigation scaffolding than upstream in some scheduling paths.

### Missing items
- Restore the upstream migration loop once `common/src/fiber.rs` can safely exchange runnable guest fibers across host core threads.
- Re-audit remaining scheduler logging/debug scaffolding and remove it after the runtime bug is closed.

### Binary layout verification
- PASS: scheduler runtime logic only; no raw-serialized structs affected.

## 2026-03-30 — core/src/hle/kernel/physical_core.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/physical_core.cpp

### Intentional differences
- Rust keeps `initialize_guest_runtime(...)` and `handoff_after_svc(...)` owner-local helpers because the current callback-based `run_loop(...)` does not mirror the upstream `RunThread(...)` monolith one-for-one. This preserves `PhysicalCore` ownership while adapting to the Rust run-loop structure.

### Unintentional differences (to fix)
- The current Rust file still contains additional post-SVC context-guard handoff logic not present in upstream `physical_core.cpp`. This was added while debugging cross-core resume bugs and should be re-audited once the scheduler/fiber migration issue is resolved.
- The file still contains investigation logging gated by environment variables in the post-SVC path.

### Missing items
- Reconcile `handoff_after_svc(...)` with the upstream `Svc::Call(...)` return path once the scheduler/current-thread ownership is stable.

### Binary layout verification
- PASS: runtime control-flow file; no raw-serialized structs affected.

## 2026-03-30 — core/src/cpu_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/cpu_manager.cpp

### Intentional differences
- Temporary env-gated runtime sampling was added in this pass via `thread_pc_sample` logging (`RUZU_SAMPLE_THREAD_IDS`, `RUZU_SAMPLE_INTERVAL_MS`) to identify which guest thread keeps running after the `0x01D31B18` worker-thread bootstrap. This is investigation scaffolding only and should be removed once the scheduler/runtime stall is fixed.

### Unintentional differences (to fix)
- fixed in this pass: the multicore guest path no longer carries its own full inlined JIT halt decoder. It now uses the owner-local `physical_core.run_thread(...)` event boundary and returns after each SVC, matching the upstream `PhysicalCore::RunThread()` return shape more closely instead of continuing through an extra Rust-only post-SVC execution loop.
- The Rust file still contains more logging and investigation scaffolding than upstream in the multicore guest loop.

### Missing items
- Remove temporary PC-sampling diagnostics after the current scheduler/runtime stall is understood.

### Binary layout verification
- PASS: runtime control-flow file; no raw-serialized structs affected.

## 2026-03-31 — core/src/hle/service/pctl/pctl.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/pctl/pctl.cpp

### Intentional differences
- Rust still threads the unused `service_manager` parameter through `loop_process(...)` because the surrounding service bootstrap API has not yet been narrowed to the upstream `Core::System&`-only shape.

### Unintentional differences (to fix)
- Fixed in this pass: `register_named_service(...)` now passes the real `SystemRef` into `IParentalControlServiceFactory`, matching upstream ownership where the factory is constructed with `Core::System& system`.

### Missing items
- Remove the unused Rust-only `service_manager` parameter once the service bootstrap owner API matches upstream more closely.

### Binary layout verification
- PASS: service registration file only; no raw-serialized structs changed.

## 2026-03-31 — core/src/hle/service/pctl/parental_control_service_factory.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/pctl/parental_control_service_factory.cpp

### Intentional differences
- Rust stores `SystemRef` by value in the factory instead of upstream's reference member because the port uses a lightweight copyable system handle type.

### Unintentional differences (to fix)
- Fixed in this pass: `CreateService` and `CreateServiceWithoutInitialize` previously instantiated `IParentalControlService` with `SystemRef::null()`, which dropped the upstream service ownership on the floor and prevented later commands from seeing the application process/program context.

### Missing items
- Re-audit whether `CreateServiceWithoutInitialize` should diverge behaviorally from `CreateService` once the upstream distinction grows beyond "construct the same service".

### Binary layout verification
- PASS: factory/service owner file only; no guest-visible raw structs changed.

## 2026-03-30 — core/src/hle/kernel/k_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.cpp

### Intentional differences
- `capture_guest_context()` / `restore_guest_context()` are Rust-only helpers used by the temporary post-SVC runtime handoff path in `physical_core.rs`; upstream does not have these helpers because it keeps the context switch flow inside `PhysicalCore::RunThread`.

### Unintentional differences (to fix)
- fixed in this pass: the Rust-only guest-context helpers failed to keep the A32 aliases `r[11]/r[13]/r[14]/r[15]` coherent with `fp/sp/lr/pc`. This caused resumed A32 threads to reload stale entry-point registers (`PC=0x200000`) after SVC/scheduler handoff even though the explicit `pc/sp` fields had advanced.
- fixed in this pass: `reset_thread_context32()` now also initializes the explicit `pc/sp` aliases so the Rust-only helper path starts from a coherent baseline.
- fixed in this pass: `on_timer()` previously called `self.cancel_wait(...)`, which reacquired `KScopedSchedulerLock` even though upstream `KHardwareTimer::DoTask()` already holds the scheduler lock before calling `KThread::OnTimer()`. The timer wake path now directly delegates to the existing wait queue, matching upstream ownership and avoiding the recursive scheduler-lock deadlock.

### Missing items
- remove the Rust-only helper path once `physical_core.rs` is brought closer to the upstream `RunThread()` lifecycle

### Binary layout verification
- PASS: field layout unchanged; only alias/value synchronization logic changed.

## 2026-03-30 — core/src/hle/kernel/kernel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.cpp

### Intentional differences
- `KScheduler` still stores cloned `Arc<PhysicalCore>` handles because the Rust scheduler does not yet own the upstream `KernelCore& m_kernel` reference directly.

### Unintentional differences (to fix)
- fixed in this pass: `initialize_physical_cores()` created `PhysicalCore` instances but never wired them into the per-core schedulers. As a result, `KScheduler::unload()` could not call `PhysicalCore::save_context()`, so guest thread contexts were never saved on switches and A32 threads reloaded stale entry-point state.

### Missing items
- replace this Rust-only `physical_cores` vector bridge if/when `KScheduler` regains a closer upstream owner reference to `KernelCore`

### Binary layout verification
- PASS: runtime ownership wiring only; no raw shared struct layout changed.

## 2026-03-30 — core/src/hle/service/nvnflinger/binder.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/binder.h

### Intentional differences
- Rust adds `register_native_handle_owner(...)` as a narrow adapter so binder-owned readable events can learn the current process/scheduler after `GetNativeHandle` copies the object into a process handle table. Upstream does not need this because `KReadableEvent` signaling stays inside `KEvent`/`ServiceContext` ownership.

### Unintentional differences (to fix)
- None in this pass for the `GetNativeHandle` return type/ownership slice.

### Missing items
- None for the `IBinder::GetNativeHandle` ownership contract itself.

### Binary layout verification
- PASS: trait/interface file; no raw-serialized structs affected.

## 2026-03-30 — core/src/hle/service/nvnflinger/buffer_queue_producer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_queue_producer.cpp

### Intentional differences
- Rust stores the binder-owned readable event directly as `Arc<Mutex<KReadableEvent>>` instead of an upstream `KEvent*` plus `GetReadableEvent()` because the current kernel/service split does not yet expose the full `ServiceContext::CreateEvent` owner path.
- Rust caches weak `KProcess`/`KScheduler` owner references after `GetNativeHandle` so later `buffer_wait_event` signaling can wake `WaitSynchronization` waiters through the real `KReadableEvent`.

### Unintentional differences (to fix)
- `buffer_wait_event` creation still bypasses upstream `ServiceContext::CreateEvent("BufferQueue:WaitEvent")` and manually allocates a `KReadableEvent` object id.
- fixed in this pass: `RequestBuffer` now rejects slots not owned by the producer (`Dequeued`) like upstream instead of accepting any in-range slot.
- fixed in this pass: `SetBufferCount` now calls `WaitWhileAllocatingLocked()`, rejects existing dequeued buffers, keeps the `buffer_count == 0` early return, enforces the minimum buffer count, only frees all buffers when no preallocated buffers exist, and calls `OnBuffersReleased()` after dropping the lock.
- fixed in this pass: `DequeueBuffer` now performs the upstream size validation, default-format/default-size handling, consumer-usage OR, `WaitForFreeSlotThenRelock()` free-slot selection, second-dequeue validation, min-undequeued validation, and `BufferNeedsReallocation` / `ReleaseAllBuffers` flag generation.
- Rust still lacks the full upstream `QueueBuffer` callback ordering (`OnFrameAvailable`/`OnFrameReplaced`) and allocation-service/NvMap-backed `GraphicBuffer` lifecycle.

### Missing items
- Constructor/destructor parity for the full upstream `ServiceContext` event lifetime.
- Full `DetachBuffer`, `DetachNextBuffer`, `AttachBuffer`, and `AllocateBuffers` parity.
- Full callback ordering/lifecycle parity with upstream `QueueBuffer` producer and consumer listeners.
- Upstream `Status` is a true bitflag enum; Rust still uses `i32` locally in `DequeueBuffer::transact` to serialize combined status bits because `enum Status` cannot represent combined discriminants directly.

### Binary layout verification
- PASS: binder reply serialization for `DequeueBuffer` now preserves combined raw status bits explicitly; no raw-serialized struct layout changed.

## 2026-03-30 — core/src/hle/service/nvnflinger/buffer_queue_consumer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_queue_consumer.cpp

### Intentional differences
- None in this pass.

### Unintentional differences (to fix)
- The Rust file still logs and returns `None` instead of asserting on `GetNativeHandle` like upstream.

### Missing items
- Bring the consumer-side `GetNativeHandle` failure semantics closer to the upstream assert path if a dedicated assertion/logging policy is chosen for Rust.

### Binary layout verification
- PASS: no raw-serialized struct layout changed.

## 2026-03-30 — core/src/hle/service/nvnflinger/hos_binder_driver.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/hos_binder_driver.cpp

### Intentional differences
- Rust uses `ctx.copy_handle_for_readable_event(...)` to translate the binder-owned `KReadableEvent` into a process handle, matching the upstream `OutCopyHandle<KReadableEvent>` contract through existing IPC helpers.

### Unintentional differences (to fix)
- The file still logs `GetNativeHandle` as `(STUBBED)` even though it now returns the real binder-owned readable event object instead of manufacturing a one-off pre-signaled handle.

### Missing items
- Re-audit command 2 logging and error code choices once more `nvnflinger` binder commands are non-stubbed.

### Binary layout verification
- PASS: IPC response layout unchanged; only object ownership changed.

## 2026-03-30 — core/src/hle/service/nvnflinger/hos_binder_driver_server.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/hos_binder_driver_server.cpp

### Intentional differences
- None in this pass.

### Unintentional differences (to fix)
- None in this pass for the `GetNativeHandle` binder-object lookup slice.

### Missing items
- None specific to this file for the native-handle lookup path.

### Binary layout verification
- PASS: registry/lookup file; no raw-serialized structs affected.

## 2026-03-31 — core/src/hle/kernel/k_condition_variable.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.cpp

### Intentional differences
- Rust still uses explicit `Arc<Mutex<KThread>>`/process lookups and process-owned helper wrappers where upstream uses raw `KThread*` plus `KScopedSchedulerLock` ownership. This preserves owner boundaries but not the exact pointer model.

### Unintentional differences (to fix)
- fixed in this pass: several wakeup paths (`SignalToAddress`, `Wait`, `SignalImpl`) called `end_wait()` and then manually pushed the same thread back into the priority queue. Upstream only calls `EndWait()`, and `KThreadQueue::EndWait()` already performs the `WAITING -> RUNNABLE` transition that re-enters the scheduler path. The Rust extra push could duplicate runnable membership and leave stale front entries in the PQ.

### Missing items
- re-audit whether the remaining explicit `remove_from_priority_queue(...)` calls before `BeginWait(...)` are still needed, because upstream relies on `BeginWait()`/`SetState(Waiting)` for the runnable-to-waiting transition.

### Binary layout verification
- PASS: scheduler/runtime behavior only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/service/nvnflinger/buffer_queue_producer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_queue_producer.cpp

### Intentional differences
- Rust still does not thread the upstream `ServiceContext&` and `NvMap&` through the `BufferQueueProducer` constructor. The file now owns an explicit `KEvent` + `KReadableEvent` pair locally instead of asking `ServiceContext::CreateEvent("BufferQueue:WaitEvent")` for the kernel object.
- `register_native_handle_owner(...)` remains a Rust-only adapter on `IBinder` so the binder-owned event can learn the current `KProcess`/`KScheduler` after `GetNativeHandle` copies the readable end into a process handle table.

### Unintentional differences (to fix)
- fixed in this pass: `buffer_wait_event` no longer stores only a standalone `KReadableEvent`. The Rust file now owns a real `KEvent` + `KReadableEvent` pair, and `signal_buffer_wait_event()` routes through `KEvent::signal(...)` once the owner process/scheduler has been registered.
- fixed in this pass: `GetNativeHandle` now returns the persistent readable end of that owned event pair, matching the upstream `buffer_wait_event->GetReadableEvent()` ownership more closely.
- `BufferQueueProducer` still lacks the exact upstream constructor/destructor lifecycle through `ServiceContext::CreateEvent/CloseEvent`.

### Missing items
- Constructor/destructor parity with upstream `ServiceContext` ownership.
- Full `NvMap&` constructor dependency and the remaining producer methods still stubbed elsewhere in this file.

### Binary layout verification
- PASS: no raw-serialized structs changed in this pass; only kernel event ownership/wakeup routing changed.

## 2026-03-31 — core/src/hle/service/nvnflinger/hos_binder_driver.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/hos_binder_driver.cpp

### Intentional differences
- Rust still converts the binder-owned readable event into a process copy handle via `ctx.copy_handle_for_readable_event(...)`, which is the local equivalent of the upstream `OutCopyHandle<KReadableEvent>` serialization path.

### Unintentional differences (to fix)
- fixed in this pass: the Rust file no longer logs `GetNativeHandle` as `(STUBBED)` even though it now returns the real persistent binder event.

### Missing items
- Re-audit this file once more `IHOSBinderDriver` commands stop being stubbed and upstream logging parity matters less than result/handle parity.

### Binary layout verification
- PASS: IPC response layout unchanged; only logging text changed.

## 2026-03-31 — video_core/src/renderer_opengl/gl_rasterizer.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_opengl/gl_rasterizer.cpp

### Intentional differences
- Rust still lacks the full upstream `RasterizerOpenGL` owner graph (`gpu`, `state_tracker`, `program_manager`, concrete OpenGL `TextureCache`/`QueryCache` runtimes, etc.) in this file. This pass restores only the cache owners that already exist as reusable Rust modules (`buffer_cache`, `texture_cache`, `shader_cache`, `query_cache`) and wires the cache-management methods through them.
- `query_cache` currently uses `video_core/src/query_cache_top.rs` (`QueryCacheLegacy`) rather than the exact upstream OpenGL-specific query cache type because `gl_query_cache.rs` still lacks the overlapping-region invalidation/flush owner methods.

### Unintentional differences (to fix)
- fixed in this pass: `FragmentBarrier()` now matches upstream ordering more closely by issuing `glTextureBarrier()` plus `GL_FRAMEBUFFER_BARRIER_BIT | GL_TEXTURE_FETCH_BARRIER_BIT`, instead of only `GL_FRAMEBUFFER_BARRIER_BIT`.
- fixed in this pass: `TiledCacheBarrier()` now matches upstream `glTextureBarrier()` instead of using the wrong framebuffer memory barrier.
- fixed in this pass: `FlushCommands()` now preserves the upstream ordering for `num_queued_commands` and `has_written_global_memory`, issuing `GL_BUFFER_UPDATE_BARRIER_BIT` before `glFlush()` when needed.
- fixed in this pass: `TickFrame()` now resets `num_queued_commands` before ticking the fence manager, matching the upstream “swap implies flush” bookkeeping.
- fixed in this pass: `FlushAndInvalidateRegion()` now follows the upstream `IsGPULevelExtreme() -> FlushRegion() -> InvalidateRegion()` ordering.
- fixed in this pass: `FlushRegion()`, `InvalidateRegion()`, `OnCacheInvalidation()`, `OnCPUWrite()`, `UnmapMemory()`, and `TickFrame()` now delegate through restored Rust cache owners in the same file, matching the upstream ownership much more closely.
- `ModifyGPUMemory()` remains incomplete because Rust `TextureCacheBase` still lacks the upstream `UnmapGPUMemory(as_id, addr, size)` owner method.
- `MustFlushRegion()` and `GetFlushArea()` still do not match the upstream `TextureCache`/`BufferCache` area selection because the exact OpenGL cache/runtime owners are not yet present here.

### Missing items
- Port the remaining upstream `RasterizerOpenGL` owner state and methods beyond the current cache-management slice.
- Restore the exact OpenGL query/texture runtime owners so `MustFlushRegion()`, `GetFlushArea()`, `ModifyGPUMemory()`, and channel setup methods can be ported literally.

### Binary layout verification
- PASS: runtime ordering only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/k_priority_queue.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_priority_queue.h

### Intentional differences
- Rust still stores queue membership in internal `HashMap<u64, QueueEntry>` state plus cached `ThreadProps`, whereas upstream uses intrusive per-thread queue entries. This is the local ownership adaptation required by the lack of raw intrusive nodes in Rust.

### Unintentional differences (to fix)
- fixed in this pass: `push_back()` / `push_front()` previously allowed the same thread id to be inserted multiple times into the internal lists. Upstream cannot express this bug because the intrusive `Member` node has unique membership. The Rust duplicate insertion could leave stale scheduled-front entries after a single `remove()`. The queue now removes any existing membership for that thread id before reinserting it, restoring the upstream uniqueness invariant.
- still to re-audit: because the queue remains non-intrusive, every path that updates `active_core` / `affinity` still depends on the cached `ThreadProps` staying synchronized with `KThread`.

### Missing items
- Re-audit the remaining `change_core` / `change_affinity_mask` / migration paths against upstream once the scheduler sleep-loop bug is fully resolved.

### Binary layout verification
- PASS: scheduler data-structure behavior only; no raw serialized structs changed.

## 2026-03-31 — rdynarmic/src/frontend/a32/translate/thumb32.rs vs dynarmic frontend/A32 Thumb-2 SVC translation

### Intentional differences
- The Rust file still lives inside `rdynarmic` rather than the upstream C++ tree under `zuyu/src/`; this entry documents the JIT parity slice because it directly affects `ruzu` runtime behavior.

### Unintentional differences (to fix)
- fixed in this pass: `thumb32_svc()` previously emitted only `A32CallSupervisor` plus `CheckHalt(ReturnToDispatch)`. Unlike `arm_svc()` and `thumb16_svc()`, it did not advance PC, update the upper location descriptor, or push/pop the RSB hint. The function now advances to the post-SVC PC and uses `PopRSBHint`, matching the established ARM/Thumb SVC contract in the same frontend.

### Missing items
- Re-audit the remaining Thumb-2 exception/hint paths against upstream once the current sleep-loop investigation is complete.

### Binary layout verification
- PASS: IR/control-flow behavior only; no raw serialized structs changed.

## 2026-03-31 — core/src/cpu_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/cpu_manager.cpp

### Intentional differences
- Rust still splits part of the upstream `CpuManager` execution loop across [`physical_core.rs`](/home/vricosti/Dev/emulators/ruzu/core/src/hle/kernel/physical_core.rs) and [`cpu_manager.rs`](/home/vricosti/Dev/emulators/ruzu/core/src/cpu_manager.rs) because the fiber/JIT bridge is not structured as a literal class port. This is a structural adaptation, but the SVC handoff semantics must still match upstream.

### Unintentional differences (to fix)
- fixed in this pass: the multicore guest path `run_guest_thread_once()` handled `SupervisorCall` by dispatching the SVC and then returning directly to the same guest loop. That bypassed the local `PhysicalCore::handoff_after_svc(...)` scheduling boundary, unlike the upstream `PhysicalCore` run loop which re-enters scheduling after SVC handling. In practice, `SleepThread` could mark the current thread `WAITING` and yet continue executing guest code, causing repeated `WAITING -> WAITING` transitions instead of an actual handoff. The multicore path now invokes `handoff_after_svc(...)` after updating the JIT SVC arguments.
- fixed in this pass: even after the first handoff fix, the Rust guest loop still continued running the same host fiber after an SVC if the current thread became non-runnable or the scheduler selected a different thread. Upstream `SleepThread` blocks inside the SVC path and only resumes after scheduling. The Rust path now immediately re-enters `schedule_raw_if_needed()` when post-SVC state shows the current thread is no longer runnable or no longer selected, restoring the expected block-before-resume behavior.

### Missing items
- Re-audit the remaining multicore `Halted(...)` path against upstream once the `SleepThread` loop is rechecked at runtime.
- Re-audit whether the single-core guest loop should share more of the `PhysicalCore::run_loop(...)` ownership instead of open-coding part of it here.

### Binary layout verification
- PASS: scheduling/control-flow only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/k_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.h

### Intentional differences
- Rust still splits part of the upstream sleep/timer lifecycle across [`k_thread.rs`](/home/vricosti/Dev/emulators/ruzu/core/src/hle/kernel/k_thread.rs), [`k_thread_queue.rs`](/home/vricosti/Dev/emulators/ruzu/core/src/hle/kernel/k_thread_queue.rs), and cooperative scheduler helpers, because `KScopedSchedulerLockAndSleep` is not yet a literal one-to-one port of the C++ blocking model.

### Unintentional differences (to fix)
- fixed in this pass: `initialize_main_thread_with_func()` previously inherited only the owner process pointer and scheduler. Unlike the other thread initialization paths, it forgot to inherit `global_scheduler_context` and `process_schedule_count` from the owning process. That meant main-thread `RUNNABLE <-> WAITING` transitions could bypass `GlobalSchedulerContext::on_thread_state_changed(...)`, leaving the global priority queue stale after `SleepThread` or similar waits. The main-thread initialization path now inherits those owners just like the user/service-thread paths.
- fixed in this pass: the Rust port now exposes an explicit `inherit_process_scheduler_state()` helper because, unlike upstream, `KThread` caches process-owned scheduler/GSC links locally. This helper is the owner-local backfill point used when a process acquires or republishes scheduler state after thread creation.
- fixed in this pass: `KThread::sleep()` previously only recorded a local `sleep_deadline` and transitioned to `WAITING`. Unlike upstream `KScopedSchedulerLockAndSleep`, it did not arm `KHardwareTimer`, so once all runnable threads slept the core could remain idle forever with no timer interrupt to wake the sleeping thread. The Rust path now registers the absolute timeout with the shared hardware timer after entering the wait state.

### Missing items
- Re-audit `KThread::Sleep()` against the upstream `ThreadQueueImplForKThreadSleep` + `KScopedSchedulerLockAndSleep` path once the current sleep-loop runtime behavior is rechecked.
- Re-audit the remaining main-thread priority/lifecycle details against the upstream `InitializeThread(..., ThreadType::Main, ...)` path after the scheduler stall is fully resolved.

### Binary layout verification
- PASS: owner wiring only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/kernel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.h

### Intentional differences
- Rust exposes small free-function accessors like `get_current_hardware_tick()` / `get_hardware_timer_arc()` because many owners are split across modules and cannot hold a literal `KernelCore&` the way upstream C++ methods do.

### Unintentional differences (to fix)
- fixed in this pass: the Rust runtime had no shared accessor for the live `KHardwareTimer` object outside direct `KernelCore` ownership, which made `KThread::sleep()` fall back to a local wall-clock deadline instead of the upstream timer-task path. The new helper only exposes the existing owner; it does not move timer behavior out of [`k_hardware_timer.rs`](/home/vricosti/Dev/emulators/ruzu/core/src/hle/kernel/k_hardware_timer.rs).

### Missing items
- Re-audit the remaining callers that still infer wall-clock deadlines directly once the sleep/wakeup path is fully validated.

### Binary layout verification
- PASS: helper accessors only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/k_process.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_process.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_process.h

### Intentional differences
- Rust caches `scheduler`, `global_scheduler_context`, and `process_schedule_count` on each [`KThread`](/home/vricosti/Dev/emulators/ruzu/core/src/hle/kernel/k_thread.rs), whereas upstream resolves scheduler ownership through kernel/process owners directly. This cache is a Rust adaptation required by the split ownership model around `Arc<Mutex<...>>`.

### Unintentional differences (to fix)
- fixed in this pass: `attach_scheduler()` only updated the process-owned weak pointers. Threads that were already registered kept stale `None` scheduler/GSC caches forever, so later `KThread::notify_state_transition()` calls could skip `GlobalSchedulerContext::on_thread_state_changed(...)`.
- fixed in this pass: `register_thread_object()` only bound the thread self-reference. It did not backfill the process-owned scheduler/GSC/schedule-count links onto the thread being registered, unlike the effective upstream ownership where a registered thread can always reach the global scheduler state through its owners.
- still to re-audit: several bootstrap paths still assign `process.global_scheduler_context` directly before/after `attach_scheduler()`. The new `set_global_scheduler_context()` backfills registered threads, but the remaining callers should continue to be audited so direct field writes do not reintroduce stale thread caches.

### Missing items
- Re-audit all process bootstrap paths against upstream `KProcess::Run()` / `RegisterThread()` once the current `SleepThread` stall is rechecked at runtime.

### Binary layout verification
- PASS: owner wiring only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/k_thread_queue.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread_queue.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread_queue.h

### Intentional differences
- Rust still uses an owned `Option<Arc<Mutex<KHardwareTimer>>>` inside `KThreadQueue` instead of the upstream raw `KHardwareTimer*`, because queue instances are copied by value in several Rust call sites and need shared ownership semantics across those copies.

### Unintentional differences (to fix)
- fixed in this pass: the Rust queue stored only a boolean `hardware_timer_set`, so `EndWait()` / `CancelWait()` could not actually delegate to `KHardwareTimer::CancelTask(...)` like upstream. The queue now retains the real timer owner and forwards cancellation to it.
- still to re-audit: Rust `KThreadQueue` remains `Clone` and value-owned, unlike the upstream polymorphic queue object lifetime. Each call site must still be audited so queue cloning does not hide ownership bugs.

### Missing items
- Re-audit all wait-queue call sites that still instantiate `KThreadQueue::default()` against upstream-specific derived queues.

### Binary layout verification
- PASS: queue runtime behavior only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/k_hardware_timer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_hardware_timer.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_hardware_timer.h

### Intentional differences
- Rust resolves timer tasks by `thread_id`, preferring `GlobalSchedulerContext` lookup and only then falling back to the local raw-pointer cache, because `KThread` does not literally inherit `KTimerTask` as an intrusive node in Rust.
- Rust no longer acquires `GlobalSchedulerContext::m_scheduler_lock` inside `KHardwareTimer::DoTask()`. Upstream does, but the Rust `CoreTiming` callback runs on a host timing thread whose TLS `current_thread` is not the target core's current guest thread; using `KAbstractSchedulerLock` there mis-attributes ownership and deadlocks the wakeup path. Rust relies on the existing `KThread`/`GlobalSchedulerContext` owners for state transitions on this callback thread instead.

### Unintentional differences (to fix)
- fixed in this pass: `RegisterAbsoluteTask()` previously did not mirror the upstream `KTimerTask::SetTime(...)` effect onto the thread, so later cancellation and wake bookkeeping had stale `timer_task_time`.
- fixed in this pass: the timer callback was created but never wired from system startup. [`core.rs`](/home/vricosti/Dev/emulators/ruzu/core/src/core.rs) now calls `KernelCore::wire_hardware_timer(...)` once `CoreTiming` exists.
- fixed in this pass: `DoTask()` previously bypassed the upstream `KThreadQueue`/`SetState` wake path by manually pushing woken threads back into the PQ a second time. The Rust path now relies on the thread wait/cancel transition to perform scheduler-visible state changes.
- fixed in this pass: timer register/cancel paths previously relocked the current `KThread` through `get_current_thread_pointer().lock()` just to mutate `disable_dispatch_count`, which can deadlock while the caller already holds that thread mutex during `KThread::sleep()`. The file now uses the thread-local fast current-thread accessor in the same conceptual owner.
- fixed in this pass: timer delivery previously resolved the sleeping thread through `Weak<Mutex<KThread>>` and relocked it inside `DoTask()`. That deadlocked when the sleeping thread's mutex was still held by the caller-side `SleepThread` path. The timer owner now stores a raw `KThread*`-equivalent pointer, matching the upstream `KTimerTask*` ownership more closely for scheduler-locked delivery.
- fixed in this pass: `DoTask()` could fire `task_id=17` without ever reaching `KThread::on_timer()` because Rust timer delivery relied only on the local pointer cache. The owner now re-resolves the task via `GlobalSchedulerContext` before falling back to the pointer cache, and runtime validation now shows `do_task -> on_timer -> cancel_wait -> RUNNABLE` for `tid=17`.

### Missing items
- Re-audit the next post-wakeup blocker after `tid=17` returns to `RUNNABLE`; the timer wake path itself is now validated.

### Binary layout verification
- PASS: timer runtime behavior only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/k_hardware_timer_base.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_hardware_timer_base.h

### Intentional differences
- Rust uses a `BTreeMap<i64, Vec<TimerTaskId>>` and explicit collection helpers instead of the upstream intrusive RB tree of `KTimerTask`, because Rust does not model the inheritance/intrusive-node layout literally here.

### Unintentional differences (to fix)
- fixed in this pass: `KHardwareTimer` previously had to process expired tasks inside a closure passed to `do_interrupt_task_impl()`, which forced an aliasing borrow of the outer `KHardwareTimer` and blocked the validated wake path from compiling. `collect_expired_tasks()` now removes elapsed tasks first, then lets the owner `KHardwareTimer` deliver them afterward without changing behavioral ordering.

### Missing items
- Re-audit whether more of the upstream `KHardwareTimerBase` helper surface should be split out as the rest of timer-task parity is ported.

### Binary layout verification
- PASS: runtime tree behavior only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/k_scoped_scheduler_lock_and_sleep.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_scoped_scheduler_lock_and_sleep.h

### Intentional differences
- Rust keeps `Arc<Mutex<KThread>>` / `Arc<Mutex<KHardwareTimer>>` owners in the guard instead of raw pointers, matching the crate-wide ownership adaptation.

### Unintentional differences (to fix)
- fixed in this pass: `KThread::sleep()` now routes through `KScopedSchedulerLockAndSleep` instead of open-coding timer registration after the wait transition.
- fixed in this pass: the guard no longer carries `Arc<Mutex<KThread>>` into timer registration. It now forwards the raw `KThread*`-equivalent pointer like upstream, avoiding a relock of the sleeping thread during timer delivery.
- still to fix: end-to-end wakeup/resume still needs runtime revalidation after the raw-thread timer registration adaptation.

### Missing items
- Validate end-to-end `SleepThread -> KScopedSchedulerLockAndSleep -> KHardwareTimer` wakeup once the scheduler lock path is corrected.

### Binary layout verification
- PASS: RAII/scheduling behavior only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/k_scheduler_lock.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_scheduler_lock.h

### Intentional differences
- Rust still implements the upstream template lock as a concrete `KAbstractSchedulerLock` with callback pointers, because there is only one scheduler type in practice.

### Unintentional differences (to fix)
- fixed in this pass: `is_locked_by_current_thread()` previously relocked the current `KThread` mutex just to read `thread_id`, unlike the upstream thread-local raw-pointer comparison. This could deadlock on paths like `KThread::sleep(&mut self)` that already hold the thread mutex. The owner check now uses a thread-local cached current-thread id.
- fixed in this pass: the default scheduler-lock callbacks no longer relock the current `KThread` mutex to mutate `disable_dispatch_count`; they use the thread-local fast current-thread accessor instead.
- still to fix: even after removing the obvious self-deadlock on current-thread inspection, runtime tracing shows the sleep path still stalls inside scheduler-lock acquisition, so another lock-ordering mismatch remains.

### Missing items
- Re-audit the remaining scheduler-lock owner interactions against upstream once the blocking `SleepThread` path is fully cleared.

### Binary layout verification
- PASS: scheduler-lock behavior only; no raw serialized structs changed.

## 2026-03-31 — core/src/core.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/core.cpp

### Intentional differences
- Rust system startup is flattened into [`core.rs`](/home/vricosti/Dev/emulators/ruzu/core/src/core.rs) instead of the upstream `System::Impl`, but initialization ordering must still match.

### Unintentional differences (to fix)
- fixed in this pass: `KernelCore::wire_hardware_timer(...)` existed but was never called during startup, leaving the kernel timer without a `CoreTiming` callback. System initialization now wires the hardware timer immediately after publishing `CoreTiming` to the kernel.

### Missing items
- Re-audit startup ordering against upstream once the timer wakeup path has been validated at runtime.

### Binary layout verification
- PASS: initialization ordering only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/kernel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.h

### Intentional differences
- Rust still uses thread-local caches (`CURRENT_THREAD`, `CURRENT_THREAD_ID`, `CURRENT_THREAD_PTR`) to model the upstream thread-local `KThread*` state across `Arc<Mutex<KThread>>` owners.

### Unintentional differences (to fix)
- fixed in this pass: the scheduler-lock callbacks previously relocked the current `KThread` mutex via `get_current_thread_pointer()`, which can deadlock on paths already holding that mutex. The kernel now publishes fast thread-local current-thread id/pointer accessors for scheduler-lock ownership checks and dispatch-disable mutations.
- fixed in this pass: `real_enable_scheduling(...)` previously only marked `needs_scheduling` on target cores and never mirrored the upstream `KScheduler::EnableScheduling(...)` behavior on the current core. It now delegates to `KScheduler::enable_scheduling_with_scheduler(...)`, which reschedules other cores and immediately reschedules the current core when the dispatch-disable count drops to the upstream threshold.
- fixed in this pass: `real_enable_scheduling(...)` previously returned immediately on host callback threads with no `CURRENT_THREAD`, silently dropping `cores_needing_scheduling`. It now still executes the upstream-equivalent static `RescheduleCores(...)` path in that case.
- fixed in this pass: `real_enable_scheduling(...)` previously deadlocked on `SleepThread` because it reacquired the current thread's `Mutex<KThread>` while that mutex was already held by the SVC owner (`&mut self`). It now uses only the fast thread-local current-thread accessors on this path.
- still to fix: the end-to-end wakeup/resume path after timer delivery still needs runtime revalidation after restoring the host-thread `EnableScheduling` path without relocking the current thread.

### Missing items
- Re-audit the remaining scheduler-lock callback semantics once the `SleepThread` deadlock is resolved.

### Binary layout verification
- PASS: thread-local owner access only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/k_scheduler.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_scheduler.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_scheduler.h

### Intentional differences
- Rust still keeps idle-core migration disabled in `UpdateHighestPriorityThreadsImpl()` until `common::fiber` reaches upstream cross-host-thread handoff semantics. This preserves correctness of the current runtime at the cost of one upstream optimization path.

### Unintentional differences (to fix)
- fixed in this pass: the static `KScheduler::reschedule_cores(...)` path was still a stub that only logged, so scheduler-lock wakeups originating on non-core host threads could compute a core mask and then never send the corresponding IPIs. It now interrupts the target `PhysicalCore`s through `KernelCore`, matching upstream ownership and behavior.
- fixed in this pass: Rust `wait_for_next_thread(...)` was mutating `current_thread`, clearing `needs_scheduling`, and publishing the next emulated thread directly from the SVC return path. Upstream `k_scheduler.cpp` only changes `m_current_thread` inside `SwitchThread(...)`. The helper is now side-effect free so the real fiber switch remains owned by `ScheduleImplFiber()`/`SwitchThread()`.
- fixed in this pass: the Rust `EnableScheduling(...)` path could merely decrement `disable_dispatch_count` and return even when the current thread had already transitioned to `WAITING` inside `KScopedSchedulerLockAndSleep`. That let a non-runnable thread continue in the same fiber until some later owner happened to reschedule. The Rust adaptation now immediately `RescheduleCurrentCore()` in that case, matching the upstream effect that a sleeping current thread does not continue executing past `EnableScheduling(...)`.
- fixed in this pass: the Rust `EnableScheduling(...)` implementation relocked the current `KThread` through `Arc<Mutex<...>>`, which deadlocked while `SleepThread` still held `&mut self`. The path now uses fast thread-local current-thread accessors instead of the thread mutex.
- fixed in this pass: the Rust `EnableScheduling(...)` direct-reschedule branch attempted to call `RescheduleCurrentCore()` while `SleepThread` still held the current thread mutex, which deadlocked before `ScheduleImplFiber()` could switch to the next runnable thread. Rust now defers that immediate reschedule to the outer `CpuManager` owner when the current thread is already non-runnable, preserving the upstream effect (`tid=17 -> tid=19`) without reentering thread locking under the SVC owner.
- fixed in this pass: `SwitchThread(...)` in upstream receives a direct `KThread*` and can always fall back to `m_idle_thread` when no runnable thread exists. The Rust `switch_thread_impl(thread_id)` was re-resolving the next thread only through `GlobalSchedulerContext`, which does not own kernel main/idle threads, so the idle fallback could not be materialized after `highest=None`. Rust now resolves `idle_thread_id` directly through the scheduler-owned idle-thread reference before consulting `GlobalSchedulerContext`.

### Missing items
- Re-audit the remaining `ScheduleOnInterrupt` / `RescheduleCurrentHLEThread` split against upstream once the post-`tid=17 -> tid=19` runtime path is fully validated.

### Binary layout verification
- PASS: scheduler control-flow only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/physical_core.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/physical_core.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/physical_core.h

### Intentional differences
- Rust still has a helper `handoff_after_svc(...)` because `CpuManager` models the upstream `RunThread()`/event loop split differently across Rust fibers and callback owners.
- `log_backtrace()` reconstructs the current `ThreadContext` through the trait-object `ArmInterface::get_context()` path, then delegates to `ArmInterfaceBase::log_backtrace(...)`. This is a mechanical Rust adaptation because `ArmInterface` does not currently expose a virtual `log_backtrace()` method like the C++ class hierarchy.

### Unintentional differences (to fix)
- fixed in this pass: `handoff_after_svc(...)` was performing a full next-thread selection and context restore after every SVC. Upstream `PhysicalCore::RunThread()` never switches threads in the SVC handler path; it returns to the outer scheduler logic after `Svc::Call(system, interface->GetSvcNumber())`. Rust `handoff_after_svc(...)` now only captures the current JIT context into the current `KThread`, leaving thread selection and `m_current_thread` mutation to `KScheduler::ScheduleImplFiber()` / `SwitchThread()`.
- fixed in this pass: `PhysicalCore::log_backtrace()` was still a stub that only logged the core index. It now reads the active thread/JIT context and emits the upstream-style symbolicated backtrace.

### Missing items
- Re-audit whether `handoff_after_svc(...)` can be removed entirely once the Rust `CpuManager` loop more literally matches upstream `PhysicalCore::RunThread()`.

### Binary layout verification
- PASS: control-flow/context-capture only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/kernel/svc/svc_exception.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/svc/svc_exception.cpp

### Intentional differences
- Reporter/debugger ownership remains incomplete: Rust still cannot call the full upstream `Reporter::SaveSvcBreakReport(...)` and debugger stop-notification path because those owners are not yet fully ported.

### Unintentional differences (to fix)
- fixed in this pass: `Break()` was not calling `CurrentPhysicalCore().LogBacktrace()` on non-notification breaks. Rust now mirrors the upstream backtrace emission path.

### Missing items
- Full upstream reporter integration for `SaveSvcBreakReport(...)`.
- Full upstream debugger notification / suspend path after `Break()`.

### Binary layout verification
- PASS: service-control file only; no raw serialized structs defined here.

## 2026-03-31 — core/src/cpu_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/cpu_manager.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/cpu_manager.h

### Intentional differences
- Rust still routes guest execution through an explicit `PhysicalCoreExecutionEvent` loop instead of the upstream direct `PhysicalCore::RunThread()` ownership, because guest fibers and host callbacks are split across Rust owners.

### Unintentional differences (to fix)
- fixed in this pass: after a supervisor call, the Rust loop only rescheduled conditionally based on a heuristic snapshot of the current thread state and scheduler current-thread id. Upstream always returns from `RunThread()` after `Svc::Call(...)`, leaving scheduling to the outer owner unconditionally. Rust now always calls the raw current-core reschedule path after every SVC event, which keeps the scheduling decision in the same owner as upstream and avoids missing the `RUNNABLE -> WAITING` handoff when the snapshot races with timer callbacks.
- fixed in this pass: the unconditional post-SVC reschedule path can now complete the `SleepThread` handoff in practice because `EnableScheduling(...)` no longer deadlocks under the current thread mutex. Runtime revalidation now shows the expected `schedule_impl_fiber: target=19` and `switch_thread_impl: cur=Some(17) next=19` after `tid=17` goes to sleep.

### Missing items
- Re-audit the remaining guest-thread loop ownership against upstream once the `SleepThread` handoff to `tid=19` is revalidated at runtime.

### Binary layout verification
- PASS: CPU-thread control-flow only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/service/nvnflinger/buffer_queue_consumer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_queue_consumer.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_queue_consumer.h

### Intentional differences
- Rust still uses `Arc<dyn IConsumerListener>` instead of upstream `std::shared_ptr<IConsumerListener>`.

### Unintentional differences (to fix)
- fixed in this pass: `connect()` previously ignored the upstream `consumer_listener` owner entirely and only toggled `consumer_controlled_by_app`, leaving `core.consumer_listener` permanently `None`.
- fixed in this pass: `disconnect()` previously skipped the upstream validation/error result when no consumer was connected.

### Missing items
- Re-audit the remaining binder-side transaction coverage once display progression is revalidated at runtime.

### Binary layout verification
- PASS: control-flow/listener ownership only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/service/nvnflinger/consumer_base.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/consumer_base.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/consumer_base.h

### Intentional differences
- Rust passes the listener owner explicitly as an `Arc<dyn IConsumerListener>` instead of using upstream `shared_from_this()`, because `ConsumerBase` is not itself reference-counted.

### Unintentional differences (to fix)
- fixed in this pass: `connect()` previously delegated to `BufferQueueConsumer::connect()` without any listener owner, making the upstream callback chain impossible.

### Missing items
- Re-audit whether `ConsumerBase` should itself become the trait owner once more of the upstream inheritance shape is restored.

### Binary layout verification
- PASS: listener ownership only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/service/nvnflinger/buffer_item_consumer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_item_consumer.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_item_consumer.h

### Intentional differences
- Rust `BufferItemConsumer::connect()` takes `self: &Arc<Self>` so it can hand an owned listener object to `BufferQueueConsumer`, mirroring upstream `shared_from_this()` semantics.

### Unintentional differences (to fix)
- fixed in this pass: `BufferItemConsumer` previously did not implement `IConsumerListener`, so it could not be registered as the upstream callback owner.

### Missing items
- Re-audit whether more `ConsumerBase` methods should move onto `BufferItemConsumer` as the port approaches exact inheritance parity.

### Binary layout verification
- PASS: trait/listener wiring only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/service/nvnflinger/buffer_queue_producer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_queue_producer.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_queue_producer.h

### Intentional differences
- Rust models the upstream callback mutex/condition with `Mutex<i32>` counters plus a `Condvar`, preserving callback ordering while adapting to Rust ownership.

### Unintentional differences (to fix)
- fixed in this pass: `queue_buffer()` previously only appended to the queue and never invoked the upstream `OnFrameAvailable` / `OnFrameReplaced` consumer callbacks.
- fixed in this pass: `queue_buffer()` previously always appended and never followed the upstream droppable-front replacement path.
- fixed in this pass: the callback ticket fields existed but were never used, so callback sequencing did not match upstream.

### Missing items
- Re-audit `queue_buffer()` validation and `BufferItem` field population against upstream once display progression is revalidated at runtime.

### Binary layout verification
- PASS: callback/control-flow only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/service/nvnflinger/surface_flinger.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/surface_flinger.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/surface_flinger.h

### Intentional differences
- Rust keeps a concrete `consumers` map alongside binder IDs so `create_layer()` can recover the typed `BufferQueueConsumer` without a trait-object downcast helper.

### Unintentional differences (to fix)
- fixed in this pass: `create_layer()` was still a stub and never created the upstream `BufferItemConsumer` owner or called `Connect(false)`.
- fixed in this pass: Rust did not keep an upstream-like top-level layer registry, so `add_layer_to_display_stack()` had nothing to attach.

### Missing items
- `set_layer_visibility()` and `set_layer_blending()` still lack mutable layer-state ownership parity.
- Re-audit the remaining composition/layer ownership paths once runtime display progression moves forward.

### Binary layout verification
- PASS: layer/listener ownership only; no raw serialized structs changed.

## 2026-03-31 — hid_core/src/resources/applet_resource.rs vs /home/vricosti/Dev/emulators/zuyu/src/hid_core/resources/applet_resource.cpp and /home/vricosti/Dev/emulators/zuyu/src/hid_core/resources/applet_resource.h

### Intentional differences
- Rust `get_shared_memory_handle(aruid)` returns the validated applet-resource slot index instead of a `Kernel::KSharedMemory*`, because `hid_core` cannot depend on `core` without a crate cycle. The real kernel object is created in the matching `core` service owner.

### Unintentional differences (to fix)
- fixed in this pass: the upstream owner method `AppletResource::GetSharedMemoryHandle(...)` was missing entirely from the Rust file, so `hid::IAppletResource` had no upstream-like delegation path at all.

### Missing items
- `SharedMemoryHolder` still does not own a real kernel `KSharedMemory` object in `hid_core`; only the service layer mirrors the shared-memory payload into kernel memory today.

### Binary layout verification
- PASS: no field layout changed in `SharedMemoryFormat`; this pass only restored owner methods and validation flow.

## 2026-03-31 — hid_core/src/resource_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/hid_core/resource_manager.cpp and /home/vricosti/Dev/emulators/zuyu/src/hid_core/resource_manager.h

### Intentional differences
- Rust `ResourceManager::get_shared_memory_handle(aruid)` returns the validated applet-resource slot index for the same crate-cycle reason as `AppletResource::get_shared_memory_handle(...)`.

### Unintentional differences (to fix)
- fixed in this pass: the upstream owner method `ResourceManager::GetSharedMemoryHandle(...)` was missing entirely, leaving `hid::IAppletResource` forced to stub around the real ownership chain.

### Missing items
- The `hid_core` crate still lacks direct `Core::System` ownership, so the real `KSharedMemory` allocation continues to live in the `core` HID service layer.

### Binary layout verification
- PASS: owner/delegation only; no raw serialized structs changed here.

## 2026-03-31 — core/src/hle/service/hid/hid.rs and core/src/hle/service/hid/hid_server.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hid/hid.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hid/hid_server.cpp/.h

### Intentional differences
- Rust uses `SystemRef` instead of upstream `Core::System&`, preserving owner identity while adapting to the Rust service factory pattern.

### Unintentional differences (to fix)
- fixed in this pass: `IHidServer` was instantiated without the upstream `System` owner, which prevented downstream `IAppletResource` from creating the real kernel shared-memory object.

### Missing items
- Re-audit other HID child services that still return null copy handles and also likely need the same `SystemRef` propagation.

### Binary layout verification
- PASS: service-owner wiring only; no raw serialized structs changed.

## 2026-03-31 — core/src/hle/service/hid/applet_resource.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hid/applet_resource.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hid/applet_resource.h

### Intentional differences
- Rust caches `(object_id, Arc<KSharedMemory>)` instead of upstream `shared_ptr<ResourceManager>` plus a raw kernel object pointer because handle-table ownership is explicit and reference-counted in the Rust port.
- The real `KSharedMemory` is mirrored from `hid_core::SharedMemoryFormat` on first `GetSharedMemoryHandle()` call. This preserves a non-null kernel object and correct IPC copy-handle ownership, but ongoing HID writes still target the `hid_core` heap owner rather than the mirrored kernel pages.

### Unintentional differences (to fix)
- fixed in this pass: `GetSharedMemoryHandle()` was still a stub that returned success with zero copy handles, directly causing downstream `svc::MapSharedMemory(handle=0)`.
- fixed in this pass: the upstream destructor behavior `resource_manager->FreeAppletResourceId(aruid)` was missing; Rust `Drop` now performs the same owner cleanup.

### Missing items
- Reconnect `hid_core::SharedMemoryHolder` to the real kernel backing so HID updates write directly into the same shared pages that `MapSharedMemory` exposes to the guest.
- Re-audit whether repeated `GetSharedMemoryHandle()` calls should duplicate the same kernel object into different process handle tables exactly like upstream close/reopen semantics.

### Binary layout verification
- PASS: the mirrored allocation size is `size_of::<SharedMemoryFormat>() == 0x40000`, matching upstream `SharedMemoryHolder::Initialize(...)`.

## 2026-04-01 — core/src/hle/service/am/applet.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/applet.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/applet.h

### Intentional differences
- Rust still models the upstream `std::unique_ptr<Process>` as an owned `Process` value and stores events as `Option<Arc<Mutex<KReadableEvent>>>`, which preserves lifecycle semantics while adapting to Rust ownership.

### Unintentional differences (to fix)
- fixed in this pass: `Applet::new(...)` previously constructed `HidRegistration` from a dummy `Process::new()` and without a `System` owner. It now takes `SystemRef` and constructs `HidRegistration(system, &process)` in the matching owner, like upstream `Applet::Applet(Core::System&, std::unique_ptr<Process>, ...)`.

### Missing items
- The event ownership still needs a full re-audit against upstream `Event`/`ReadableEvent` pairs once the AM service slice is more complete.

### Binary layout verification
- PASS: constructor/lifecycle only; no raw serialized structs changed.

## 2026-04-01 — core/src/hle/service/am/applet_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/applet_manager.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/applet_manager.h

### Intentional differences
- Rust still builds `Applet` in two steps (`Applet::new(...)`, then `Process::with_process(...)`) because `Process` is a value member rather than the upstream moved `std::unique_ptr<Process>`.

### Unintentional differences (to fix)
- fixed in this pass: `AppletManager` previously called `Applet::new(...)` without a `System` owner and had to backfill `hid_registration` afterward. It now constructs the applet with `self.system`, matching the upstream owner flow more closely.

### Missing items
- The remaining two-step `Process` assignment should be revisited if `Applet` is later refactored closer to upstream move-construction.

### Binary layout verification
- PASS: applet construction ordering only; no raw serialized structs changed.

## 2026-04-01 — core/src/hle/service/am/hid_registration.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/hid_registration.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/hid_registration.h

### Intentional differences
- Rust stores an optional `Arc<parking_lot::Mutex<ResourceManager>>` rather than upstream `std::shared_ptr<IHidServer>`, because the resource manager is the actual owner used on all call paths and avoids a trait-object cycle through the service layer.
- Rust caches a raw `*const Process` only for Drop-time `is_initialized()` parity, mirroring the upstream reference member while adapting to Rust move semantics.

### Unintentional differences (to fix)
- fixed in this pass: `HidRegistration::new(...)` was using a non-blocking HID service lookup, unlike upstream `GetService<HID::IHidServer>("hid", true)`, so early applet construction could miss HID registration entirely. Rust now blocks until `hid` is registered before resolving the resource manager.
- fixed in this pass: construction/destruction paths were not gated on `Process::is_initialized()`, unlike upstream `if (m_process.IsInitialized())`. Rust now matches that guard on register/unregister/input-enable paths.

### Missing items
- Re-audit whether this owner should cache `IHidServer` directly once more service downcast ownership is restored.

### Binary layout verification
- PASS: lifecycle/registration only; no raw serialized structs changed.

## 2026-04-01 — core/src/hle/service/am/service/all_system_applet_proxies_service.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/all_system_applet_proxies_service.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/all_system_applet_proxies_service.h

### Intentional differences
- Rust still extracts `pid` from `HLERequestContext`/thread parent manually because CMIF typed argument wrappers like upstream `ClientProcessId` / `InCopyHandle<KProcess>` are not yet fully modeled in this owner.

### Unintentional differences (to fix)
- fixed in this pass: the Rust file previously invented a new `Applet` inside `open_*_proxy`, which has no upstream counterpart and bypassed the tracked applet/HID registration state. It now looks up the existing applet from `WindowSystem::get_by_applet_resource_user_id(pid)`, matching upstream `GetAppletFromProcessId(...)`.

### Missing items
- `OpenSystemAppletProxy` / `OpenLibraryAppletProxy` still do not model the upstream `process_handle` ownership because the proxy constructors remain simplified.

### Binary layout verification
- PASS: proxy lookup/control-flow only; no raw serialized structs changed.

## 2026-04-02 — core/src/hle/kernel/svc/svc_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/svc/svc_thread.cpp

### Intentional differences
- Rust now maps a fallback 1 MiB `Stack` region under the guest-supplied `stack_bottom` when `CreateThread` receives a stack pointer that still lives in a `Free` page-table region. Upstream does not do this; it trusts user mode to provide already-mapped stack memory. This is a bounded compatibility stopgap to recover MK8D worker-thread stack visibility while the earlier user-mode stack allocation parity gap remains unresolved.
- The fallback mapping preserves any preexisting bytes already present in the underlying guest memory backing. This intentionally avoids clearing startup metadata that user mode may already have written near `stack_top` before the page-table mapping became visible.

### Unintentional differences (to fix)
- `CreateThread` still lacks the upstream resource-limit reservation path (`KScopedResourceReservation` waiting up to 100ms) before allocating the thread object.
- The fallback stack mapping above indicates some earlier process/user-mode stack allocation path is still structurally wrong; once that owner is fixed, this eager mapping should be removed.

### Missing items
- Re-audit the full `CreateThread` owner for literal parity with `KThread::InitializeUserThread`, `KThread::Register`, and resource-limit accounting after the worker-stack issue is resolved.

### Binary layout verification
- PASS: no serialized structs changed; this slice only changes runtime stack mapping behavior in the matching owner.

## 2026-04-01 — core/src/hle/service/am/service/application_functions.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/application_functions.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/application_functions.h

### Intentional differences
- `PopLaunchParameter` converts the raw `u32` CMIF argument to `LaunchParameterKind` with an explicit Rust `match` rather than upstream typed deserialization.

### Unintentional differences (to fix)
- fixed in this pass: `PopLaunchParameter` was still stubbed and always returned `ResultNoDataInChannel`, which directly caused MK8D to call `SetTerminateResult(0x2A2)` and abort.
- fixed in this pass: the Rust file now pops from the matching `Applet` channel (`user_channel_launch_parameter` or `preselected_user_launch_parameter`), removes the last entry like upstream, and returns a real `am::IStorage` session when data exists.
- fixed in this pass: `PopLaunchParameter` now constructs `IStorage` with the real `SystemRef` owner instead of `SystemRef::null()`, matching the upstream `std::make_shared<IStorage>(system, data)` ownership path.

### Missing items
- Re-audit the remaining stubbed methods in this owner against upstream now that boot proceeds past `PopLaunchParameter`.

### Binary layout verification
- PASS: this slice only moves existing `Vec<u8>` launch-parameter payloads into `IStorage`; no new raw struct serialization was introduced.

## 2026-04-01 — core/src/hle/service/nvnflinger/buffer_queue_producer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/buffer_queue_producer.cpp

### Intentional differences
- Rust still represents the upstream `slots[slot] = {}` assignment with `BufferSlot::default()` plus explicit field writes in the same owner, because Rust does not support C++ aggregate reset syntax.

### Unintentional differences (to fix)
- fixed in this pass: `SetPreallocatedBuffer(...)` previously reused `free_buffer_locked(slot)` instead of literally resetting the slot state like upstream. That could preserve non-upstream slot fields and fence state across repeated preallocation calls.
- fixed in this pass: the slot fence now resets to `Fence::no_fence()` exactly on this path, matching upstream `slots[slot].fence = Fence::NoFence()`. The previous Rust code left the default zeroed fence shape instead.
- fixed in this pass: width/height are now assigned unconditionally from the supplied preallocated buffer, matching upstream's literal assignment order.

### Missing items
- `GraphicBuffer::from_nv_buffer(...)` still needs a focused re-audit against the upstream `std::make_shared<GraphicBuffer>(nvmap, buffer)` constructor path if more BufferQueue parity bugs remain.

### Binary layout verification
- PASS: no new serialized layout was introduced; this slice only changes slot lifecycle/reset ordering in the matching owner.

## 2026-04-01 — core/src/hle/service/am/service/application_proxy_service.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/application_proxy_service.cpp

### Intentional differences
- Rust still reconstructs the caller `KProcess` from `HLERequestContext` instead of using the upstream typed `InCopyHandle<KProcess>` parameter directly, because CMIF typed handle wrappers are not yet modeled in this owner.

### Unintentional differences (to fix)
- fixed in this pass: `IApplicationProxyService` was not storing the upstream `Core::System&` owner, so downstream `IApplicationProxy` instances could not propagate the real `System` into child AM services.

### Missing items
- Re-audit this owner once typed CMIF `InCopyHandle<KProcess>` support exists so the proxy uses the copied process handle literally like upstream.

### Binary layout verification
- PASS: service-owner wiring only; no raw serialized structs changed.

## 2026-04-01 — core/src/hle/service/am/service/application_proxy.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/application_proxy.cpp

### Intentional differences
- Rust still omits the upstream raw `KProcess* m_process` from some child-service constructor signatures that do not use it yet, but `SystemRef` and applet/window owners now live in the matching owner file.

### Unintentional differences (to fix)
- fixed in this pass: `GetLibraryAppletCreator` and `GetApplicationFunctions` were constructing child services without the upstream `Core::System&` owner, leaving `IApplicationFunctions` on `SystemRef::null()` and forcing later AM behavior to diverge.

### Missing items
- Re-audit the other child-service constructors in this owner for the same `SystemRef` parity once their Rust counterparts accept the owner explicitly.

### Binary layout verification
- PASS: owner wiring only; no raw serialized structs changed.

## 2026-04-01 — core/src/hle/service/am/service/all_system_applet_proxies_service.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/all_system_applet_proxies_service.cpp

### Intentional differences
- Rust still does not forward the upstream `InCopyHandle<KProcess>` literally into `ISystemAppletProxy` / `ILibraryAppletProxy`; it still re-derives the process from the request thread where needed.

### Unintentional differences (to fix)
- fixed in this pass: `appletAE` was not storing the upstream `Core::System&` owner, so both library/system applet proxies lost parity immediately when constructing `ILibraryAppletCreator`.

### Missing items
- Re-audit `OpenSystemAppletProxy` / `OpenLibraryAppletProxy` once typed process-handle ownership is available in CMIF wrappers.

### Binary layout verification
- PASS: proxy factory wiring only; no raw serialized structs changed.

## 2026-04-01 — core/src/hle/service/am/service/library_applet_proxy.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/library_applet_proxy.cpp

### Intentional differences
- Rust still does not model the upstream `m_process` field in this owner, because the process-handle path is still simplified at the service factory boundary.

### Unintentional differences (to fix)
- fixed in this pass: `GetLibraryAppletCreator` was constructing `ILibraryAppletCreator` without the upstream `Core::System&` owner.

### Missing items
- Re-audit the remaining child-service constructors in this owner for full `SystemRef` parity.

### Binary layout verification
- PASS: owner wiring only; no raw serialized structs changed.

## 2026-04-01 — core/src/hle/service/am/service/system_applet_proxy.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/system_applet_proxy.cpp

### Intentional differences
- Rust still simplifies the upstream `m_process` ownership similarly to `library_applet_proxy.rs`.

### Unintentional differences (to fix)
- fixed in this pass: `GetLibraryAppletCreator` was constructing `ILibraryAppletCreator` without the upstream `Core::System&` owner.

### Missing items
- Re-audit the other child-service constructors in this owner for full `SystemRef` parity.

### Binary layout verification
- PASS: owner wiring only; no raw serialized structs changed.

## 2026-04-01 — core/src/hle/service/am/service/library_applet_creator.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/library_applet_creator.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/library_applet_creator.h

### Intentional differences
- Rust still keeps `CreateTransferMemoryStorage` and `CreateHandleStorage` stubbed because this owner does not yet have a faithful `KTransferMemory handle -> LibraryAppletStorage` bridge. This is documented debt, not an accepted end state.
- Rust returns a real `am::IStorage` via the existing service-session path instead of upstream typed CMIF `Out<SharedPointer<IStorage>>`, preserving service ownership while adapting to the Rust IPC layer.

### Unintentional differences (to fix)
- fixed in this pass: `CreateStorage` was stubbed and returned success without an `IStorage` object, diverging directly from the upstream owner behavior.
- fixed in this pass: `ILibraryAppletCreator` itself was missing the upstream `Core::System&` owner.

### Missing items
- Port `CreateLibraryApplet`.
- Port `CreateTransferMemoryStorage`.
- Port `CreateHandleStorage`.

### Binary layout verification
- PASS: `CreateStorage` only allocates a zero-initialized `Vec<u8>` of the requested size, matching the upstream `std::vector<u8>(size)` payload semantics.

## 2026-04-01 — core/src/hle/service/am/service/storage.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/storage.cpp

### Intentional differences
- Rust still stores the upstream `Core::System&` as a `SystemRef`, and the field is not yet consumed by the accessor constructors because those owners have not been re-audited for full parity.

### Unintentional differences (to fix)
- fixed in this pass: `IStorage` could only be constructed without a `System` owner, which blocked faithful construction from `ILibraryAppletCreator::CreateStorage`.

### Missing items
- Re-audit `IStorageAccessor` / `ITransferStorageAccessor` constructors to propagate the upstream `System` owner explicitly if later AM slices require it.

### Binary layout verification
- PASS: no serialized storage payload layout changed; this pass only adds owner wiring.

## 2026-04-01 — core/src/hle/service/am/service/storage_accessor.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/storage_accessor.cpp

### Intentional differences
- `Core::System&` is not yet stored on `IStorageAccessor`/`ITransferStorageAccessor`: current Rust `ServiceFramework` wiring does not require per-service system ownership for this file's currently ported behavior.

### Unintentional differences (to fix)
- fixed in this pass: `IStorageAccessor::Read` previously read from `offset` to the end of storage and then relied on `HLERequestContext::write_buffer()` clamping. Upstream reads exactly `out_buffer.size()` bytes and fails if that range exceeds the backing storage.
- `ITransferStorageAccessor::GetHandle` remains unimplemented: upstream returns both the storage size and a copied `KTransferMemory` handle.

### Missing items
- Port `ITransferStorageAccessor::GetHandle`.

### Binary layout verification
- PASS: no raw serialized structs in this file.

## 2026-04-01 — core/src/hle/service/acc/profile_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/acc/profile_manager.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/acc/profile_manager.h

### Intentional differences
- `Common::UUID` remains represented internally as `u128` in this Rust file: this is an older structural divergence, but raw IPC/savefile byte order is still preserved through `to_le_bytes()` / `from_le_bytes()` on all current paths touched in this pass.

### Unintentional differences (to fix)
- fixed in this pass: `ProfileManager::new()` previously only parsed the save file and stopped. Upstream also creates a default `"yuzu"` user when the profile store is empty, persists it, clamps `Settings::values.current_user`, and opens the current user immediately.
- fixed in this pass: `ParseUserSaveFile()` / `WriteUserSaveFile()` previously treated `profiles.dat` as a raw array of 0xC8-byte user records starting at byte 0. Upstream serializes `ProfileDataRaw` with a leading 0x10-byte padding block, then 8 `UserRaw` records.
- fixed in this pass: parsed `UserData` was previously discarded and replaced with `UserData::default()`. Upstream preserves the raw 0x80-byte payload from the savefile.
- fixed in this pass: `GetOpenUsers()` / `GetStoredOpenedUsers()` previously compacted only the active prefix manually; upstream transforms the full fixed-size array then stable-partitions valid UUIDs to the front.
- fixed in this pass: `RemoveUser()` previously shifted the prefix manually. Upstream invalidates the slot and stable-partitions valid profiles to the front.

### Missing items
- Re-audit `ProfileInfo.user_uuid`/`UserIDArray` against upstream `Common::UUID` ownership and placement.

### Binary layout verification
- PASS: `ProfileBase` remains `0x38` and `UserData` remains `0x80`.

## 2026-04-01 — core/src/hle/service/acc/acc.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/acc/acc.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/acc/acc.h

### Intentional differences
- `Common::UUID` is still represented through `u128` in the shared `Interface` methods: the new `IManagerForApplication` computes the upstream account hash through `UUID::from_bytes(uuid.to_le_bytes())` to preserve the same bit pattern.

### Unintentional differences (to fix)
- fixed in this pass: `GetBaasAccountManagerForApplication` was missing entirely on the shared interface side, so `acc:u0` command 101 had no owner-local object to return.
- `EnsureTokenIdCacheAsyncInterface` is only minimally ported: upstream has a dedicated async interface type with real command semantics; current Rust implementation returns success and an empty ID-token cache payload.
- `IManagerForApplication::CheckAvailability` and `GetNintendoAccountUserResourceCacheForApplication` remain stub-like, matching upstream's current placeholder behavior but still lacking deeper backing state.

### Missing items
- Re-audit `EnsureTokenIdCacheAsyncInterface` against the full upstream async contract.
- Port `CreateAuthorizationRequest` and `LoadNetworkServiceLicenseKindAsync`.

### Binary layout verification
- PASS: no raw serialized structs introduced in this pass.

## 2026-04-01 — core/src/hle/service/acc/acc_u0.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/acc/acc_u0.cpp

### Intentional differences
- none beyond existing Rust service-framework adaptation.

### Unintentional differences (to fix)
- fixed in this pass: command 101 `GetBaasAccountManagerForApplication` was registered as `None`, while upstream returns an `IManagerForApplication` object.

### Missing items
- `AuthenticateApplicationAsync`
- `CheckNetworkServiceAvailabilityAsync`
- `StoreSaveDataThumbnail`
- `ClearSaveDataThumbnail`
- `CreateGuestLoginRequest`
- `LoadOpenContext`

### Binary layout verification
- PASS: command table wiring only; no raw layout changes.

## 2026-04-01 — core/src/hle/service/filesystem/fsp/fs_i_storage.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/filesystem/fsp/fs_i_storage.cpp

### Intentional differences
- Rust still stages reads through an owned `Vec<u8>` before `ctx.write_buffer(...)`: upstream writes directly into the CMIF out-buffer span, but the owner and ordering remain the same.

### Unintentional differences (to fix)
- fixed in this pass: `IStorage::Read` previously returned `Success` for negative `offset`/`length`, while upstream returns `ResultInvalidOffset` / `ResultInvalidSize`.
- fixed in this pass: `IStorage::Read` previously clamped the requested `length` to the HIPC write-buffer size before reading. Upstream reads the requested `length` from the backend and leaves CMIF buffer sizing to the IPC contract.
- backend read short-counts are still silently accepted: upstream virtual file backends typically fill the requested span, while the Rust VFS still exposes byte-counting reads.

### Missing items
- Re-audit whether `VirtualFile::read()` short reads should be surfaced as an FS error on this IPC path.
- `Write`, `Flush`, `SetSize`, and `OperateRange` remain unimplemented like upstream `nullptr` entries, but still return stub success in the generic Rust fallback.

### Binary layout verification
- PASS: command handler only; no raw serialized struct changes.

## 2026-04-01 — core/src/core.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/core.cpp

### Intentional differences
- The current Rust load path still bypasses `am/process_creation.rs`, so ARP launch-property registration is temporarily performed inside `System::load()` before service startup. This is a temporary ownership divergence to match the current active control flow until `CreateApplicationProcess` is the real owner path.

### Unintentional differences (to fix)
- fixed in this pass: `System` had no persistent `ARPManager` owner, so ACC could not query the same launch-property database that upstream exposes through `system.GetARPManager()`.
- fixed in this pass: frontend-launched applications were not registering any `ApplicationLaunchProperty`, so later ACC initialization could not derive the application type from ARP.
- the registered launch property is still simplified compared to upstream `CreateApplicationProcess`: `version` is forced to `0`, `base_game_storage_id` is forced to `Host`, and `update_storage_id` is forced to `None`.

### Missing items
- Move ARP registration ownership back into `core/src/hle/service/am/process_creation.rs` once that file becomes the active application-process creation path.
- Fill `ApplicationLaunchProperty.version` and storage IDs from real content-provider / patch-manager state like upstream.

### Binary layout verification
- PASS: `ApplicationLaunchProperty` remains `0x10`; this pass only wires persistent ownership and registration.

## 2026-04-01 — core/src/hle/service/acc/acc.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/acc/acc.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/acc/acc.h

### Intentional differences
- `Common::UUID` is still represented through `u128` in the shared `Interface` methods: the new `IManagerForApplication` computes the upstream account hash through `UUID::from_bytes(uuid.to_le_bytes())` to preserve the same bit pattern.
- `InitializeApplicationInfoV2` remains a stub success path, matching the upstream owner file.

### Unintentional differences (to fix)
- fixed in this pass: `ApplicationInfo` stored only a raw `title_id`, while upstream stores the full `Glue::ApplicationLaunchProperty` plus the derived `ApplicationType`.
- fixed in this pass: `InitializeApplicationInfo` returned unconditional success and forced `ApplicationType::Digital`, instead of querying ARP with the current application-process program ID and validating `base_game_storage_id`.
- fixed in this pass: `InitializeApplicationInfoRestricted` returned unconditional success instead of delegating to `InitializeApplicationInfoBase()` like upstream.
- `InitializeApplicationInfoBase()` still derives storage validity from Rust `romfs_factory::StorageId` values because there is not yet a dedicated upstream-aligned `FileSys::StorageId` owner file in the target tree.
- `EnsureTokenIdCacheAsyncInterface` is only minimally ported: upstream has a dedicated async interface type with real command semantics; current Rust implementation returns success and an empty ID-token cache payload.
- `IManagerForApplication::CheckAvailability` and `GetNintendoAccountUserResourceCacheForApplication` remain stub-like, matching upstream's current placeholder behavior but still lacking deeper backing state.

### Missing items
- Re-audit `EnsureTokenIdCacheAsyncInterface` against the full upstream async contract.
- Port `CreateAuthorizationRequest` and `LoadNetworkServiceLicenseKindAsync`.

### Binary layout verification
- PASS: `ApplicationInfo` now carries the full `ApplicationLaunchProperty` owner payload; `ApplicationLaunchProperty` remains `0x10`.

## 2026-04-01 — core/src/hle/service/filesystem/fsp/fsp_srv.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/filesystem/fsp/fsp_srv.cpp and /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/filesystem/fsp/fsp_srv.h

### Intentional differences
- Error propagation still uses the existing Rust `push_error_with_null_interface(...)` helper to preserve CMIF/domain response shape. Upstream expresses the same contract through `OutInterface<T>` serialization rather than an explicit helper.

### Unintentional differences (to fix)
- fixed in this pass: `OpenDataStorageByCurrentProcess` returned an empty `VectorVfsFile` backend instead of delegating to `RomFsController::OpenRomFSCurrentProcess()` and caching the resulting `romfs`.
- fixed in this pass: `FspSrv` had no `romfs` owner field, so it could not preserve the upstream cached `FileSys::VirtualFile romfs` lifecycle across repeated opens.
- `OpenPatchDataStorageByCurrentProcess` still follows the current upstream stub-like `ResultTargetNotFound` behavior and remains intentionally unimplemented.
- `OpenDataStorageByDataId` and `OpenDataStorageWithProgramIndex` are not yet re-audited against the full upstream patch/base-NCA flow, including `PatchManager` usage and `OpenBaseNca`.

### Missing items
- Port `RomFsController::OpenBaseNca` and then re-audit `OpenDataStorageByDataId`.
- Re-audit `OpenDataStorageWithProgramIndex` against the exact upstream `OpenPatchedRomFSWithProgramIndex(...)` path and error handling.

### Binary layout verification
- PASS: owner/lifecycle only; no raw serialized struct layout changed in this pass.

## 2026-04-01 — core/src/hle/service/nvnflinger/nvnflinger.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/nvnflinger.cpp

### Intentional differences
- Rust still stores `SurfaceFlinger` behind `Arc<Mutex<_>>` instead of the upstream direct member object. This preserves the same owner while adapting to the service thread model.

### Unintentional differences (to fix)
- fixed in this pass: `Nvnflinger` no longer constructs `SurfaceFlinger` with `SystemRef::null()`. Upstream passes the real `Core::System&`.

### Missing items
- Re-audit `Nvnflinger` service-thread wakeup/loop behavior against upstream once the presentation path is fully unblocked.

### Binary layout verification
- PASS: owner wiring only; no raw serialized structs affected.

## 2026-04-01 — core/src/hle/service/nvnflinger/surface_flinger.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/surface_flinger.cpp

### Intentional differences
- Rust still keeps a local `consumers: HashMap<i32, Arc<BufferQueueConsumer>>` cache. Upstream reacquires the typed consumer through `HosBinderDriverServer::TryGetBinder(...)`, but the current Rust `Arc<dyn IBinder>` bridge cannot yet recover `Arc<BufferQueueConsumer>` cleanly.
- `Display` ownership still uses a `Vec<Display>` and helper lookups instead of the exact C++ member layout. Ownership and lookup behavior remain equivalent.

### Unintentional differences (to fix)
- fixed in this pass: `SurfaceFlinger` now owns the real `SystemRef`, fetches the real `nvdrv:s` module, opens `/dev/nvdisp_disp0`, and keeps that FD for the same owner-local lifecycle as upstream.
- fixed in this pass: `compose_display()` now routes through `HardwareComposer::compose_locked(...)` and the real `nvdisp_disp0` device owner instead of stopping at an internal layer collection stub.
- fixed in this pass: `remove_layer_from_display_stack()` now delegates the release path through `HardwareComposer::remove_layer_locked(...)` before erasing the layer, matching upstream ordering.
- `set_layer_visibility()` and `set_layer_blending()` still only log instead of mutating the `Layer`, because `Layer` still lacks the interior mutability needed to mirror the upstream direct field writes.

### Missing items
- Replace the local consumer cache once binder downcasting can recover typed `BufferQueueConsumer` owners from `HosBinderDriverServer`.
- Port real `Layer` mutability so `SetLayerVisibility` and `SetLayerBlending` match upstream behavior literally.

### Binary layout verification
- PASS: owner/lifecycle only; no raw serialized structs introduced in this pass.

## 2026-04-01 — core/src/hle/service/nvnflinger/hardware_composer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/hardware_composer.cpp

### Intentional differences
- `BTreeMap` replaces upstream `flat_map` for framebuffer slots. This is a Rust container adaptation; owner placement and keyed lifetime remain the same.

### Unintentional differences (to fix)
- fixed in this pass: `HardwareComposer` now owns `frame_number`, per-consumer cached framebuffers, `NormalizeSwapInterval`, `ComposeLocked`, `RemoveLayerLocked`, `TryAcquireFramebufferLocked`, and `CacheFramebufferLocked` in the matching owner file instead of a broad presentation stub.
- fixed in this pass: `compose_locked()` now releases acquired buffers only after advancing `frame_number`, preserving upstream ordering.
- the `transform` type still crosses crates as raw bits and is rebuilt into `video_core::framebuffer_config::BufferTransformFlags` later in the GPU bridge; upstream uses the concrete shared enum type end-to-end.

### Missing items
- Re-audit whether `BufferItemConsumer::AcquireBuffer` timeout/default arguments match upstream exactly on all presentation paths.
- Re-audit `MicroProfileFlip()` parity once the profiling subsystem is revisited.

### Binary layout verification
- PASS: no raw serialized structs owned here; targeted tests cover the swap-interval contract.

## 2026-04-02 — core/src/hle/service/nvnflinger/display.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/display.h

### Intentional differences
- Rust uses `Arc<Mutex<Layer>>` where upstream uses `std::shared_ptr<Layer>`. This is the minimal Rust adaptation needed to preserve shared mutable layer ownership across `SurfaceFlinger` and `HardwareComposer`.

### Unintentional differences (to fix)
- fixed in this pass: `LayerStack` no longer stores immutable `Arc<Layer>`. Shared layers are now mutable again, so upstream-style field updates on `visible` and `blending` are possible at the matching owners.

### Missing items
- Re-audit whether any remaining display-layer owners still assume immutable `Layer` handles.

### Binary layout verification
- PASS: owner/lifetime change only; no raw serialized structs affected.

## 2026-04-02 — core/src/hle/service/nvnflinger/surface_flinger.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/surface_flinger.cpp

### Intentional differences
- Rust still keeps a local `consumers: HashMap<i32, Arc<BufferQueueConsumer>>` cache. Upstream reacquires the typed consumer through `HosBinderDriverServer::TryGetBinder(...)`, but the current Rust `Arc<dyn IBinder>` bridge cannot yet recover `Arc<BufferQueueConsumer>` cleanly.
- `Display` ownership still uses a `Vec<Display>` and helper lookups instead of the exact C++ member layout. Ownership and lookup behavior remain equivalent.

### Unintentional differences (to fix)
- fixed in this pass: `set_layer_visibility()` now mutates the matching `Layer.visible` owner field instead of logging and returning.
- fixed in this pass: `set_layer_blending()` now mutates the matching `Layer.blending` owner field instead of logging and returning.

### Missing items
- Replace the local consumer cache once binder downcasting can recover typed `BufferQueueConsumer` owners from `HosBinderDriverServer`.

### Binary layout verification
- PASS: owner/lifecycle only; no raw serialized structs introduced in this pass.

## 2026-04-02 — core/src/hle/service/nvnflinger/hardware_composer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvnflinger/hardware_composer.cpp

### Intentional differences
- `BTreeMap` replaces upstream `flat_map` for framebuffer slots. This is a Rust container adaptation; owner placement and keyed lifetime remain the same.

### Unintentional differences (to fix)
- fixed in this pass: `HardwareComposer` now reads `visible`, `blending`, and `buffer_item_consumer` through the shared mutable `Layer` owner, matching the upstream assumption that `SurfaceFlinger` layer state updates are visible during composition/release.
- the `transform` type still crosses crates as raw bits and is rebuilt into `video_core::framebuffer_config::BufferTransformFlags` later in the GPU bridge; upstream uses the concrete shared enum type end-to-end.

### Missing items
- Re-audit whether `BufferItemConsumer::AcquireBuffer` timeout/default arguments match upstream exactly on all presentation paths.
- Re-audit `MicroProfileFlip()` parity once the profiling subsystem is revisited.

### Binary layout verification
- PASS: no raw serialized structs owned here; targeted tests cover the swap-interval contract.

## 2026-04-02 — video_core/src/rasterizer_interface.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/rasterizer_interface.h

### Intentional differences
- Rust still models the rasterizer as a trait instead of a C++ virtual base class. This is the minimal language adaptation and preserves owner boundaries.

### Unintentional differences (to fix)
- fixed in this pass: `initialize_channel` and `bind_channel` now take `&ChannelState`, matching the upstream owner/signature shape instead of passing only `bind_id`.

### Missing items
- Re-audit all backend rasterizers so they consume `ChannelState` directly where upstream does.

### Binary layout verification
- PASS: interface-only change; no serialized structs affected.

## 2026-04-02 — video_core/src/gpu.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.cpp and /home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.h

### Intentional differences
- Rust still passes `&ChannelState` through a trait object instead of a direct virtual call on a concrete C++ interface. Owner placement and call ordering remain aligned.

### Unintentional differences (to fix)
- fixed in this pass: `VideoGpuChannelHandle::init_channel()` now calls `RasterizerInterface::initialize_channel(&ChannelState)` instead of passing only `bind_id`.
- fixed in this pass: `Gpu::bind_channel()` now calls `RasterizerInterface::bind_channel(&ChannelState)` instead of passing only `bind_id`.

### Missing items
- Re-audit `release_channel` call sites against upstream channel teardown ordering.

### Binary layout verification
- PASS: owner/signature change only; no serialized structs affected.

## 2026-04-02 — video_core/src/renderer_null/null_rasterizer.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_null/null_rasterizer.cpp and /home/vricosti/Dev/emulators/zuyu/src/video_core/renderer_null/null_rasterizer.h

### Intentional differences
- Rust stores `ChannelSetupCaches<ChannelInfo>` as a normal field instead of protected C++ inheritance. This preserves the same owner and helper boundary without relying on inheritance.

### Unintentional differences (to fix)
- fixed in this pass: `RasterizerNull` no longer leaves `InitializeChannel`, `BindChannel`, and `ReleaseChannel` stubbed. These now route through the matching `ChannelSetupCaches<ChannelInfo>` owner helpers: `create_channel`, `bind_to_channel`, and `erase_channel`.

### Missing items
- Re-audit whether the null backend also needs upstream `MemoryManager`-visible cache hooks beyond channel cache registration once the runtime stall is narrowed further.

### Binary layout verification
- PASS: owner/cache wiring only; no serialized structs affected.

## 2026-04-01 — core/src/gpu_core.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.h and /home/vricosti/Dev/emulators/zuyu/src/video_core/framebuffer_config.h

### Intentional differences
- `gpu_core.rs` remains a Rust-only bridge file because `core` cannot name `video_core` concrete types directly. The added framebuffer and blend-mode bridge types exist only to preserve upstream owner calls across the crate split.

### Unintentional differences (to fix)
- fixed in this pass: the bridge had no owner-local equivalent for upstream `GPU::RequestComposite(layers, fences)`, so `nvdisp_disp0` could not forward composition requests at all.
- the bridge still forwards fences only as opaque payload into the trait boundary; the current `video_core::Gpu` implementation ignores them, while upstream waits on them before compositing.

### Missing items
- Port real fence gating in `video_core::Gpu::request_composite(...)`.

### Binary layout verification
- PASS: bridge structs are plain value carriers; no raw guest-facing binary copy path uses them.

## 2026-04-01 — core/src/hle/service/nvdrv/devices/nvdisp_disp0.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvdisp_disp0.cpp

### Intentional differences
- Rust stores `SystemRef` and `*const NvMap` directly in the device, mirroring upstream owner access without depending on the full C++ object graph. This is the same adaptation pattern already used by `NvMapDevice`.
- `SpeedLimiter::DoSpeedLimiting(...)` is still not called here because `System` currently exposes only shared access on this path. Upstream performs speed limiting in this owner after `RequestComposite`.

### Unintentional differences (to fix)
- fixed in this pass: `NvDispDisp0` now owns the real `SystemRef` and `NvMap` access path instead of staying stateless/stubbed.
- fixed in this pass: `composite()` now builds real framebuffer configs from `HwcLayer`, collects active acquire fences, forwards them to `system.GPU().RequestComposite(...)`, and rotates `PerfStats` system-frame markers like upstream.
- `QueryEvent()` remains stubbed like upstream for unknown DISP events.

### Missing items
- Reintroduce upstream `SpeedLimiter::DoSpeedLimiting(...)` in this owner once a safe mutable `System` path exists.
- Re-audit exact `PerfStats` ordering against upstream when the full frame-presentation path is stable.

### Binary layout verification
- PASS: no raw guest payload structs changed; the added conversion only repacks existing owner-local values.

## 2026-04-01 — core/src/hle/service/nvdrv/nvdrv.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/nvdrv.cpp

### Intentional differences
- The Rust port still tracks typed open-device maps (`gpu_files`, `disp_files`) in parallel with the generic FD map. This is a temporary bridge to recover typed owners without C++ `GetDevice<T>(fd)` templates.

### Unintentional differences (to fix)
- fixed in this pass: `/dev/nvdisp_disp0` is now constructed with the real `SystemRef` and `NvMap` owner path required for composition, instead of a stateless stub device.

### Missing items
- Replace the typed side maps once a more upstream-like typed device lookup exists in the Rust owner.

### Binary layout verification
- PASS: FD/device wiring only; no binary layout impact.

## 2026-04-01 — video_core/src/gpu.rs vs /home/vricosti/Dev/emulators/zuyu/src/video_core/gpu.cpp and /home/vricosti/Dev/emulators/zuyu/src/video_core/framebuffer_config.h

### Intentional differences
- `video_core::Gpu` still reconstructs framebuffer configs from the Rust bridge types defined in `core/src/gpu_core.rs`. Upstream passes the concrete `Tegra::FramebufferConfig` type directly.

### Unintentional differences (to fix)
- fixed in this pass: `GpuCoreInterface` now forwards upstream `RequestComposite(layers, fences)` ownership into the real `video_core::Gpu` owner instead of leaving `nvdisp_disp0` with no path to the renderer.
- the current `request_composite(...)` implementation still ignores the incoming `NvFence` array; upstream waits for the fences before compositing.

### Missing items
- Implement fence gating in `Gpu::request_composite(...)` to match upstream `RequestComposite(layers, fences)` more literally.

### Binary layout verification
- PASS: owner-local conversion only; no raw guest-facing struct copy path changed.

## 2026-04-01 — core/src/hle/service/filesystem/fsp/fsp_srv.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/filesystem/fsp/fsp_srv.cpp

### Intentional differences
- `OpenSaveDataFileSystem` now threads the real opened `VirtualDir` into the matching Rust `IFileSystem` owner, but `SizeGetter::FromStorageId(...)` is still not ported. The Rust owner currently uses a placeholder zero-sized getter until `FileSystemController` exposes the upstream storage-size queries.
- `OpenSdCardFileSystem` still returns an empty placeholder filesystem because the upstream `fsc.OpenSDMC(...)` owner path is not ported yet.

### Unintentional differences (to fix)
- fixed in this pass: command `51` (`OpenSaveDataFileSystem`) was missing from the owner handler table, so the root `fsp-srv` session returned stub success with no interface object. The guest then reused the missing domain object and hit `Session handler is invalid`.
- fixed in this pass: `OpenSaveDataFileSystem` now parses `SaveDataSpaceId` and `SaveDataAttribute` in the matching owner and returns a real `IFileSystem` domain/session object on success.
- the request parser for this command still uses an owner-local padding skip (`u8` + one word) instead of a shared CMIF typed serializer helper; re-audit this against upstream serialization when the broader FSP command surface is ported.

### Missing items
- Port `SizeGetter::FromStorageId(...)` / real storage-size ownership so `OpenSaveDataFileSystem` and `OpenSdCardFileSystem` can match upstream sizes.
- Port the adjacent FSP filesystem-opening commands (`52`, `53`, and other interface-returning owners) that still rely on stubs.

### Binary layout verification
- PASS: `SaveDataAttribute` remains `repr(C)` size `0x40`; this pass copies the full payload bytes explicitly before field use.

## 2026-04-01 — core/src/hle/service/filesystem/fsp/fs_i_filesystem.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/filesystem/fsp/fs_i_filesystem.cpp

### Intentional differences
- The Rust owner now holds the upstream conceptual owners `FileSys::Fsa::IFileSystem` and `SizeGetter`, but `SizeGetter` is still a Rust closure pair instead of the upstream helper factory object.
- Temporary probe logging was added in this pass to inspect HIPC `X` and `A` descriptors on `OpenFile`/`OpenDirectory`; remove it once the pointer-read bug is fixed.

### Unintentional differences (to fix)
- fixed in this pass: `IFileSystem` no longer returned stub success for commands `8` (`OpenFile`) and `9` (`OpenDirectory`). It now creates real `IFile` / `IDirectory` session or domain objects in the matching owner file.
- fixed in this pass: `GetEntryType` now goes through the real `FileSys::Fsa::IFileSystem` backend instead of staying unimplemented.
- still wrong: path payload reads for `InLargeData<FileSys::Sf::Path, BufferAttr_HipcPointer>` hit `Unmapped ReadBlock` on real game traffic. Upstream reads these through `ReadBufferX`, but the Rust owner currently reaches addresses that the memory bridge cannot resolve during IPC handling.

### Missing items
- Port the remaining `IFileSystem` commands in this owner (`CreateFile`, `DeleteFile`, `CreateDirectory`, `DeleteDirectory`, `DeleteDirectoryRecursively`, `RenameFile`, `CleanDirectoryRecursively`, `GetFileTimeStampRaw`, `GetFileSystemAttribute`) to full upstream behavior.
- Remove the temporary descriptor probe once the `ReadBufferX` path is fixed.

### Binary layout verification
- PASS: `FileSys::Sf::Path` remains `repr(C)` and the Rust owner copies exactly `size_of::<Path>()` bytes before decoding.

## 2026-04-01 — core/src/hle/service/hle_ipc.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hle_ipc.cpp

### Intentional differences
- Rust now prefers `SharedProcessMemory` for guest buffer payload reads and mirrors guest buffer writes into both `SharedProcessMemory` and the `Memory` bridge. Upstream has a single coherent `Memory` owner; this dual-path logic remains a temporary Rust adaptation while the two owners are not yet fully unified.
- TLS command-buffer reads and writes still prefer the `Memory` bridge first, then fall back to `SharedProcessMemory`, because the Rust TLS IPC path is not yet fully coherent through process memory alone.

### Unintentional differences (to fix)
- fixed in this pass: `HLERequestContext::new_with_thread(...)` now binds the memory owner from `thread->GetOwnerProcess()` instead of later overwriting it from a global current-process accessor.
- fixed in this pass: IPC guest-buffer reads no longer depend on the `Memory` bridge being able to resolve every user-buffer page-table mapping. `ReadBufferX` for `FileSys::Sf::Path` now succeeds on the MK8D boot path by reading through owner process memory.
- still wrong: the Rust owner graph still needs both `Memory` and `SharedProcessMemory` to stay coherent, whereas upstream has only one memory owner.

### Missing items
- Unify the Rust memory owners so the `SharedProcessMemory` preference can be removed and `Memory` regains strict upstream ownership for both TLS and guest buffers.
- Remove the temporary `read_guest_memory fail` probe once the broader IPC memory-owner unification is complete.

### Binary layout verification
- PASS: no IPC descriptor layout changed in this pass; only the fallback read path changed.

## 2026-04-02 — core/src/hle/service/nvdrv/devices/nvhost_ctrl_gpu.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/devices/nvhost_ctrl_gpu.cpp

### Intentional differences
- The Rust owner stores `SystemRef` explicitly because there is no shared C++ `nvdevice{system_}` base object carrying `Core::System&`; this preserves the same owner access path in Rust.
- The Rust owner still does not model the C++ destructor path that frees persistent events explicitly; event lifetime is currently handled by `Arc` and `EventInterface`.

### Unintentional differences (to fix)
- fixed in this pass: `GetGpuTime` no longer returns a stubbed zero value. It now reads `system.core_timing().get_global_time_ns()` in the matching owner file, like upstream `system.CoreTiming().GetGlobalTimeNs().count()`.
- fixed in this pass: `Module::open("/dev/nvhost-ctrl-gpu")` now passes the real `SystemRef` through to the matching device owner instead of constructing the device without system access.

### Missing items
- Re-audit `Ioctl3` in this owner against upstream `WrapFixedInlOut(...)` once the later `nvdrv` path is stable, especially the exact inline-output semantics for commands `0x5` and `0x6`.
- Re-audit persistent event destruction/lifetime against upstream once the wider `EventInterface` owner path is cleaned up.

### Binary layout verification
- PASS: `IoctlGetGpuTime` remains `repr(C)` size `0x10`; this pass only changes the value source, not the layout.

## 2026-04-02 — hid_core/src/resources/touch_screen/touch_screen_resource.rs vs /home/vricosti/Dev/emulators/zuyu/src/hid_core/resources/touch_screen/touch_screen_resource.cpp

### Intentional differences
- Rust does not store `Core::System&`, `KEvent*`, or `CoreTiming::EventType` directly in this owner because `hid_core` cannot depend on `core`. The owner keeps the upstream behavioral methods, while timing/event wiring is bridged from the `core` HID service layer.

## 2026-04-02 — core/src/hle/service/nvdrv/nvdrv_interface.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/nvdrv/nvdrv_interface.cpp

### Intentional differences
- Rust still resolves handles through the current process handle table and `Arc<Mutex<KProcess>>` instead of upstream raw `KProcess*` object accessors. This is the existing object-ownership adaptation.

### Unintentional differences (to fix)
- fixed in this pass: `NVDRV::Open` contained temporary descriptor probes (`buffer_descriptor_a/x`, `read_buffer_a`, `read_buffer_x`, manual memory probe) that do not exist upstream. Those extra reads could trigger spurious `BufferDescriptorX invalid buffer_index` errors on valid A-only traffic.

### Missing items
- `MapSharedMem`, `SetAruidForTest`, and `InitializeDevtools` are still unimplemented, matching the already documented gaps in this service owner.

### Binary layout verification
- PASS: IPC payload shape unchanged; this pass only removes non-upstream debug reads from `Open`.

## 2026-04-02 — core/src/hle/service/filesystem/fsp/fs_i_filesystem.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/filesystem/fsp/fs_i_filesystem.cpp

### Intentional differences
- The Rust owner now holds the upstream conceptual owners `FileSys::Fsa::IFileSystem` and `SizeGetter`, but `SizeGetter` is still a Rust closure pair instead of the upstream helper factory object.

### Unintentional differences (to fix)
- fixed in this pass: `read_path_from_buffer()` no longer forces `ReadBufferX(0)` first. Upstream `InLargeData<..., BufferAttr_HipcPointer>` is deserialized generically through CMIF buffer selection; the Rust owner now matches that by using `ctx.read_buffer(0)` directly.
- fixed in this pass: temporary descriptor probe logging for `A`/`X` path payloads was removed from the owner after the IPC buffer-owner issue was narrowed elsewhere.

### Missing items
- Port the remaining `IFileSystem` commands in this owner (`CreateFile`, `DeleteFile`, `CreateDirectory`, `DeleteDirectory`, `DeleteDirectoryRecursively`, `RenameFile`, `CleanDirectoryRecursively`, `GetFileTimeStampRaw`, `GetFileSystemAttribute`) to full upstream behavior.

### Binary layout verification
- PASS: `FileSys::Sf::Path` remains `repr(C)` and the Rust owner still copies exactly `size_of::<Path>()` bytes before decoding.

### Unintentional differences (to fix)
- fixed in this pass: `ActivateTouch(aruid)` and `ActivateGesture(aruid, basic_gesture_id)` now initialize the per-ARUID touch/gesture LIFOs in the matching owner instead of staying stubbed.
- fixed in this pass: `SetTouchScreenResolution`, `SetTouchScreenConfiguration`, and `GetTouchScreenConfiguration` now update/read the per-ARUID touch owner data instead of returning placeholder success.
- fixed in this pass: `OnTouchUpdate(timestamp)` now walks assigned ARUIDs and writes gesture/touch LIFO entries into shared memory instead of leaving shared-memory publication unwired.
- still wrong: upstream signals `input_event` on touch changes and unschedules `timer_event` in `Finalize()`. The Rust owner still lacks those concrete kernel/timing owners and therefore does not yet reproduce that exact event lifecycle.

### Missing items
- Port `SetInputEvent(...)` / `SetTimerEvent(...)` ownership literally once `hid_core` can receive the required kernel/timing bridge objects without breaking crate layering.
- Re-audit `ReadTouchInput()`, `RequestNextTouchInput()`, and `RequestNextDummyInput()` against upstream when real input-driven signaling is wired.

### Binary layout verification
- PASS: no guest-facing struct layout changed in this pass; the owner writes through the existing shared-memory structs and lifos.

## 2026-04-02 — hid_core/src/resource_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/hid_core/resource_manager.cpp

### Intentional differences
- Upstream owns `Core::System&`, `ServiceContext`, `KEvent* input_event`, and concrete `CoreTiming::EventType` handles in this file. Rust still cannot name those `core` owners directly inside `hid_core`, so periodic callback scheduling is bridged from `core/src/hle/service/hid/hid.rs`.

### Unintentional differences (to fix)
- fixed in this pass: `CreateAppletResource(aruid)` now activates `touch_screen` and `gesture` by default like upstream, instead of leaving those resources inactive for homebrew/application paths.
- fixed in this pass: `InitializeTouchScreenSampler()` now wires `TouchResource` to `AppletResource` and `HandheldConfig` so the touch owner can populate shared memory per ARUID.
- fixed in this pass: the matching owner now exposes `update_touch_screen(timestamp)` so the upstream `touch_update_event` callback behavior exists again in the Rust owner.
- still wrong: `input_event` creation, `ServiceContext`, and event ownership remain outside this file.

### Missing items
- Port literal `ServiceContext::CreateEvent("ResourceManager:InputEvent")` ownership into this file via a reviewed bridge.
- Move the `CoreTiming` event creation back into this owner once crate boundaries permit the upstream ownership model.

### Binary layout verification
- PASS: owner-only lifecycle changes; no binary layout changes.

## 2026-04-02 — core/src/hle/service/hid/hid_server.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hid/hid_server.cpp

### Intentional differences
- Rust keeps `ResourceManager` behind `Arc<Mutex<_>>` instead of `std::shared_ptr<ResourceManager>`, preserving shared lifecycle with Rust synchronization.

### Unintentional differences (to fix)
- fixed in this pass: `ActivateTouchScreen` now performs the upstream two-step activation (`Activate()` then `Activate(aruid)`) against the real touch owners.
- fixed in this pass: `ActivateGesture` now performs the upstream two-step activation against the real gesture/touch owners.
- fixed in this pass: `SetTouchScreenConfiguration` and `SetTouchScreenResolution` now forward to the real touch owner instead of staying fake success no-ops.

### Missing items
- Re-audit all other HID command owners still returning placeholder success on the MK8D boot path, especially if a later blocker remains in HID shared-memory visibility.

### Binary layout verification
- PASS: IPC payload structs were not reshaped; only owner dispatch behavior changed.

## 2026-04-02 — core/src/hle/service/hid/hid.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hid/hid.cpp

### Intentional differences
- Rust now creates the HID `CoreTiming` events in this file, whereas upstream creates them indirectly inside `hid_core::ResourceManager(system, firmware_settings)`. This divergence is temporary and exists only because `hid_core` cannot depend on `core::CoreTiming` directly.

### Unintentional differences (to fix)
- fixed in this pass: the HID service loop now schedules periodic callbacks for `update_npad`, `update_controllers`, `update_mouse_keyboard`, `update_motion`, and `update_touch_screen`, restoring the upstream periodic HID update behavior that was previously missing entirely.

### Missing items
- Push the event creation/ownership back into `hid_core::ResourceManager` once a reviewed timing bridge exists.
- Add explicit shutdown unscheduling parity for these events if later lifecycle bugs show up.

### Binary layout verification
- PASS: service wiring only; no guest binary layout impact.

## 2026-04-02 — core/src/hle/service/hle_ipc.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/hle_ipc.cpp

### Intentional differences
- Rust still stores `SharedProcessMemory` alongside `Memory` because `ruzu` has not fully collapsed onto a single upstream-style `Core::Memory::Memory&` owner yet. This remains a temporary bridge for subsystems that still read guest data through `ProcessMemoryData`.

### Unintentional differences (to fix)
- fixed in this pass: `HLERequestContext::new_with_thread(...)` no longer trusts the caller-provided `SharedProcessMemory` owner. It now derives both `Memory` and `SharedProcessMemory` from `thread->parent` so the request context uses one coherent owner process, matching upstream's `thread->GetOwnerProcess()->GetMemory()`.
- still wrong: `ReadBufferX` / `ReadBufferA` still bounce through Rust-owned copies instead of the upstream `CpuGuestMemory` span helpers, so guest-buffer visibility bugs can still come from the fallback bridge rather than the sole `Memory` owner.

### Missing items
- Re-audit `read_guest_memory` / `write_guest_memory` until `SharedProcessMemory` fallback can be removed from the steady-state HLE IPC path.
- Re-audit `ServerManager::complete_sync_request()` so its synthetic context construction cannot reintroduce cross-process memory ownership drift on deferred/session-manager paths.

### Binary layout verification
- PASS: no IPC descriptor or payload struct layout changed in this pass; only request-context ownership was corrected.

## 2026-04-02 — core/src/hle/kernel/k_process.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_process.cpp

### Intentional differences
- Rust uses `Arc<Mutex<KProcess>>` and raw-pointer reborrows inside this owner where upstream uses direct object ownership under kernel scheduler locks. This is limited to borrow-checker adaptation; the process remains the sole logical owner of `KConditionVariable`.

### Unintentional differences (to fix)
- fixed in this pass: `KProcess::wait_condition_variable()` no longer moves `cond_var` out of the process with `mem::take()`. Upstream always keeps the condition-variable tree process-owned while other threads signal it. The old Rust path made concurrent `SignalProcessWideKey` observe an empty tree and lose wakeups.
- fixed in this pass: `signal_condition_variable()`, `before_update_condition_variable_priority()`, `after_update_condition_variable_priority()`, and `remove_condition_variable_waiter()` now operate on the in-place process-owned condvar instead of swapping in a temporary default owner.
- still wrong: `cargo test -p core` remains blocked by unrelated pre-existing test compile failures in other owners (`nvhost_ctrl_gpu.rs`, `hle_ipc.rs` tests), so this slice is only validated with focused tests/build plus runtime.

### Missing items
- Re-audit `KProcess::WaitConditionVariable` and `SignalConditionVariable` again once the remaining AM/nvdrv boot blocker is fixed, to confirm no further wake ordering differences remain.

### Binary layout verification
- PASS: owner/lifecycle change only; no guest-facing structs or IPC payloads changed.

## 2026-04-02 — core/src/hle/kernel/k_condition_variable.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.cpp

### Intentional differences
- Rust exposes `wait_for_current_thread()` as `pub(crate)` so the matching process owner can keep the condvar in-place while still delegating the post-wait scheduler loop. Upstream expresses this by direct same-translation-unit access.

### Unintentional differences (to fix)
- fixed in this pass: added a regression test covering the real ownership bug where one thread waits through `KProcess::wait_condition_variable()` and another signals through the process owner while the waiter is still asleep.
- still wrong: timeout registration is still expressed through Rust `sleep_deadline`/timer bridges instead of the exact upstream `KScopedSchedulerLockAndSleep` + `KThreadQueue` timer ownership.

### Missing items
- Re-audit `Wait()` timer registration and queue hardware-timer ownership against upstream after the remaining runtime blocker is solved.

### Binary layout verification
- PASS: no binary layout changes; tests and internal visibility only.

## 2026-04-02 — core/src/hle/service/am/lifecycle_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/lifecycle_manager.cpp

### Intentional differences
- Rust stores weak references to the owning `KProcess`/`KScheduler` after `ensure_*_event(ctx)` so this owner file can drive `KReadableEvent::signal()`/`clear()` later. Upstream embeds `Event` objects directly in `LifecycleManager` through `ServiceContext`, so no separate owner capture is needed there.

### Unintentional differences (to fix)
- fixed in this pass: `SignalSystemEventIfNeeded()` no longer only flips cached booleans. It now performs the real `signal()`/`clear()` transitions on the underlying `KReadableEvent`, matching upstream `m_system_event.Signal()` / `m_system_event.Clear()`.
- fixed in this pass: `OnOperationAndPerformanceModeChanged()` now signals the real operation-mode event object in addition to updating cached flags, matching upstream `m_operation_mode_changed_system_event.Signal()`.
- still wrong: `cargo test -p core` remains blocked by unrelated pre-existing test compile failures in other owners, so this slice is validated by line-by-line audit, build, and runtime only.

### Missing items
- Add a focused regression test once the broader `core` test target compiles again: consuming the only applet message through `ReceiveMessage` must clear the readable system event and stop the infinite AM poll loop.

### Binary layout verification
- PASS: owner/event lifecycle change only; no guest-visible struct layout changed.

## 2026-04-02 — ../rdynarmic/src/frontend/a32/decoder.rs vs /home/vricosti/Dev/emulators/zuyu/externals/dynarmic/src/dynarmic/frontend/A32/decoder/asimd.inc

### Intentional differences
- Rust keeps the ASIMD unconditional decode table inside one handwritten decoder file instead of upstream's generated `.inc` expansion. This is a generator/porting difference only; ownership remains in the A32 decoder.

### Unintentional differences (to fix)
- fixed in this pass: `ASIMD_VMAX_float` / `ASIMD_VMIN_float` were missing from the unconditional decode table, so opcodes like `0xF2600F01` still reached `Unknown` before translation.

### Missing items
- Re-audit adjacent ASIMD floating-point patterns (`VRECPS`, `VRSQRTS`, `VPMAX_float`, `VPMIN_float`) against the same upstream table; this file still does not cover the full unconditional ASIMD surface.

### Binary layout verification
- PASS: decoder-only change; no guest-visible binary layout involved.

## 2026-04-02 — ../rdynarmic/src/frontend/a32/translate/asimd.rs vs /home/vricosti/Dev/emulators/zuyu/externals/dynarmic/src/dynarmic/frontend/A32/translate/impl/asimd_three_regs.cpp

### Intentional differences
- Rust uses a small file-local operand decoder helper to keep the upstream `FloatingPointInstruction(...)` field extraction readable in this file. Ownership remains local to the matching ASIMD translator owner.

### Unintentional differences (to fix)
- fixed in this pass: `asimd_floating_point_instruction(...)` read the `z/sz` bit from bit 21. Upstream's pattern `111100100D1znnnndddd1111NQM0mmmm` places `z/sz` at bit 20. This made the real MK8D opcode `0xF2600F01` look like `sz==1` and raise `UndefinedInstruction` even though capstone and upstream both treat it as `vmin.f32 d16, d0, d1`.

### Missing items
- Add fuller ASIMD translation coverage tests once more three-register floating-point operations are ported in this owner.

### Binary layout verification
- PASS: translator-only change; no guest-visible binary layout involved.

## 2026-04-02 — ../rdynarmic/src/backend/x64/emit_vector_helpers.rs vs /home/vricosti/Dev/emulators/zuyu/externals/dynarmic/src/dynarmic/backend/x64/emit_x64_vector.cpp

### Intentional differences
- Rust factors the vector-emission helpers into a standalone helper file rather than keeping them as local templates inside one giant backend `.cpp`. Ownership still matches the x64 vector emitter backend.

### Unintentional differences (to fix)
- fixed in this pass: the stack-based vector fallback helpers reserved `result` with `scratch_xmm()` after `host_call(...)`. Upstream reserves the result register before `EndOfAllocScope()` and before the host-call setup. The old Rust order could leave all XMM candidates locked and panic with `All candidate registers have already been allocated` during MK8D block compilation.

### Missing items
- Re-audit the remaining non-vector host-call helpers for the same ordering pattern (`result` allocation vs `HostCall`/`EndOfAllocScope`) so this backend bug does not recur in other owners.

### Binary layout verification
- PASS: backend emitter ordering only; no guest-visible binary layout involved.

## 2026-04-02 — core/src/hle/service/am/service/application_functions.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/am/service/application_functions.cpp

### Intentional differences
- Rust computes `GetDesiredLanguage` directly inside `IApplicationFunctions` by using the matching Rust owners `PatchManager` and `IReadOnlyApplicationControlDataInterface` helpers, instead of routing through `ns:am2 -> IServiceGetterInterface -> IApplicationManagerInterface`. Those NS owner files are not fully wired yet, so this keeps behavior in the correct AM owner while preserving the upstream control flow at the subsystem level.

### Unintentional differences (to fix)
- fixed in this pass: `GetDesiredLanguage` was a hardcoded stub returning `"en"`. It now reads `supported_languages` from control metadata, falls back to the update title like upstream, selects the desired application language, and converts it to a real language code.
- still wrong: the Rust `IReadOnlyApplicationControlDataInterface::get_application_desired_language()` helper still uses a simplified selection policy compared with upstream `ns/language.cpp`, so exact priority resolution can still diverge for some non-English language combinations.

### Missing items
- Wire the true upstream NS path (`ns:am2` / `IServiceGetterInterface` / `IApplicationManagerInterface`) so `IApplicationFunctions::GetDesiredLanguage` can delegate exactly like C++ instead of using the temporary direct helper call.
- Add a focused regression test covering `supported_languages == 0` and a title that supports a subset of languages.

### Binary layout verification
- PASS: no guest-facing struct layout changed; IPC response remains `Result + u64 language_code`.

## 2026-04-02 — core/src/hle/kernel/k_condition_variable.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.cpp

### Intentional differences
- Rust still uses a helper `wait_for_current_thread(process, current_thread)` after the owner-local wait setup, because guest waits are multiplexed onto host fibers instead of blocking the host thread exactly like upstream. Ownership remains in `k_condition_variable.rs`.

### Unintentional differences (to fix)
- fixed in this pass: `Wait()`/`WaitLocked()` now actually use `KScopedSchedulerLockAndSleep` and propagate the returned hardware timer into the condition-variable wait queue, matching upstream `wait_queue.SetHardwareTimer(timer)` before `BeginWait(...)`.
- fixed in this pass: `begin_wait_condition_variable(...)` no longer hardcodes a fresh queue with no timer ownership. The caller now passes the configured queue, preserving upstream timer cancellation ownership in `KThreadQueue`.
- still wrong: `wait_for_current_thread(...)` remains a Rust-only host scheduling shim. Upstream simply returns `cur_thread->GetWaitResult()` after the wait path completes.

### Missing items
- Re-audit `WaitForAddress()` against the same scheduler-lock/timer ownership pattern. That owner still uses a Rust-local `wait_for_current_thread(...)` flow too.

### Binary layout verification
- PASS: synchronization/lifecycle only; no guest-visible struct layout changed.

## 2026-04-02 — core/src/hle/kernel/k_process.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_process.h

### Intentional differences
- Rust calls the process-owned condition variable through an `unsafe` raw pointer while holding the `KProcess` mutex so the owner stays in place. This is the Rust adaptation of upstream inline forwarding to `m_cond_var`.

### Unintentional differences (to fix)
- fixed in this pass: `KProcess::wait_condition_variable(...)` no longer bypasses the owner-local `KConditionVariable::wait(...)` path by calling `wait_locked(...)` directly. It now forwards through the real owner method like upstream `return m_cond_var.Wait(...)`.

### Missing items
- None in this owner for this slice.

### Binary layout verification
- PASS: owner-forwarding change only; no binary layout involved.

## 2026-04-02 — core/src/hle/kernel/k_scheduler.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_scheduler.cpp

### Intentional differences
- Rust exposes a raw helper `reschedule_current_core_raw(...)` so wait owners can trigger the upstream `RescheduleCurrentCore()` handoff without holding the per-core scheduler mutex across a fiber yield. Upstream does not need this helper because it does not wrap `KScheduler` in a `Mutex`.
- Rust still keeps a local `scan_runnable_threads(...)` fallback because some wakeup owners are not fully parity-complete yet. Upstream has no equivalent global scan because runnable-thread ownership stays entirely inside the priority queue.

### Unintentional differences (to fix)
- fixed in this pass: wait owners no longer rely only on `schedule_raw_if_needed(...)`, which could leave a just-blocked current thread spinning if the scheduling flag was not observed on that path. They now force the raw equivalent of upstream `RescheduleCurrentCore()` after transitioning the thread to `WAITING`.
- fixed in this pass: the Rust-only fallback `scan_runnable_threads(...)` no longer steals a runnable thread from another core when the local PQ is empty. It now filters on `active_core == self.core_id` and physical affinity containing the scheduler core, matching the ownership assumptions upstream gets from `GetScheduledFront(core)` instead of a global scan.
- fixed in this pass: `reschedule_current_core_raw(...)` no longer unconditionally calls `EnableDispatch()` on the current thread. That panic path was invalid for server/synchronization waits that enter the raw helper with `disable_dispatch_count == 0`.
- fixed in this pass: `EnableScheduling(...)` now keeps `needs_scheduling` set when the current thread became non-runnable but the Rust port must defer the actual switch until after the owner releases the thread mutex. Upstream reschedules immediately from this point; the Rust adaptation now preserves the same pending-handoff state instead of silently clearing it.

### Missing items
- Re-audit other wait owners still using ad hoc host-side reschedule loops and convert them to the same raw helper only where they truly model upstream `RescheduleCurrentCore()`.
- Remove the Rust-only `scan_runnable_threads(...)` fallback entirely once all wakeup paths always repopulate the PQ correctly.
- Re-audit the `RUNNABLE -> WAITING` removal path in the global priority queue. The latest runtime still shows some sleeping worker threads reappearing as the per-core highest-priority thread immediately after switching to idle, which suggests a remaining PQ/state parity bug outside `EnableScheduling(...)` itself.

### Binary layout verification
- PASS: scheduler control-flow only; no binary layout involved.

## 2026-04-04 — core/src/hle/kernel/k_scheduler.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_scheduler.cpp

### Intentional differences
- Rust still cannot perform the upstream immediate fiber switch from `EnableScheduling(...)` while the current owner path holds `&mut KThread` (for example `KThread::sleep()`). The port now defers that handoff uniformly for non-runnable current threads, including nested dispatch-disable cases, and leaves `needs_scheduling` set so the outer `CpuManager` loop performs the switch immediately after the SVC returns.

### Unintentional differences (to fix)
- `EnableScheduling(...)` still depends on the outer Rust execution loop to consume the deferred handoff instead of matching upstream's in-place reschedule literally.
- The broader scheduler/fiber interaction still differs structurally from upstream because the Rust port wraps schedulers in `Mutex` and uses the current `corosensei` backend.

### Missing items
- Re-audit the remaining `EnableScheduling(...)` callers once the current MK8D blocker moves past the first `SleepThread(5ms)` handoff.

### Binary layout verification
- PASS: scheduler control-flow only; no binary layout involved.

## 2026-04-02 — core/src/hle/kernel/k_synchronization_object.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_synchronization_object.cpp

### Intentional differences
- Rust still keeps the host-side `wait_for_current_thread` loop because waits are multiplexed onto fibers instead of blocking the host thread directly.

### Unintentional differences (to fix)
- fixed in this pass: `KSynchronizationObject::Wait(...)` now uses the same raw `RescheduleCurrentCore()` helper as the condition-variable path, instead of only `schedule_raw_if_needed(...)`.

### Missing items
- None in this owner for this specific handoff slice.

### Binary layout verification
- PASS: wait/scheduler handoff only; no guest-visible struct layout changed.

## 2026-04-02 — core/src/hle/kernel/k_page_table_base.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_page_table_base.cpp

### Intentional differences
- Rust still does not model upstream `KScopedLightLock`, `KMemoryBlockManagerUpdateAllocator`, or `KScopedPageTableUpdater` as separate RAII owner types. The equivalent ordering remains inside the owner methods in `k_page_table_base.rs`.

### Unintentional differences (to fix)
- fixed in this pass: `MapMemory` no longer assumes `phys_addr = DRAM_BASE + src_address`. It now reconstructs the true source page backing through a Rust `make_page_group(...)` helper and remaps the destination through `map_page_group_impl(...)`, matching upstream `MakePageGroup(pg, src_address, num_pages)` plus `MapPageGroupImpl(...)`.
- still wrong: `make_page_group(...)` currently walks the Rust `PageTable` one page at a time through `get_physical_address(...)` instead of using upstream traversal helpers and block-info manager ownership. Behavior is closer to upstream, but the internal structure is still simplified.

### Missing items
- Re-audit `UnmapMemory` against upstream allocator/update ordering. This pass targeted the source-page selection bug in `MapMemory`.

### Binary layout verification
- PASS: page-table remap behavior only; no guest-visible struct layout changed.

## 2026-04-02 — core/src/cpu_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/cpu_manager.cpp

### Intentional differences
- Rust still wraps scheduler/core owners in `Arc<Mutex<...>>` and splits some helper logic across `cpu_manager.rs` and `physical_core.rs`, while upstream keeps these as direct object references. Ownership for the guest-thread loop remains in `cpu_manager.rs`.

### Unintentional differences (to fix)
- fixed in this pass: the multicore guest loop no longer forces `reschedule_current_core_raw(...)` after every SVC return. Upstream `PhysicalCore::RunThread()` returns immediately after `Svc::Call(...)`, and `CpuManager::MultiCoreRunGuestThread()` simply re-enters the loop with the scheduler-visible current thread/core state. Rust now only performs the handoff when the SVC left the current thread non-runnable after owner locks were released.
- fixed in this pass: after `svc_dispatch`, the multicore guest loop now checks both the scheduler's pending handoff bit and the current thread's base state before re-entering guest execution. This lets deferred wait owners model upstream immediate reschedule semantics without running the same guest SVC block again first.
- still wrong: the Rust guest loop still uses extra host-side reschedule helpers on some interrupt/idle paths because fiber handoff is not yet fully parity-identical to upstream's direct scheduler/fiber integration.

### Missing items
- Re-audit the remaining `reschedule_current_core_raw(...)` call sites in `cpu_manager.rs` against upstream `MultiCoreRunGuestThread()` / `MultiCoreRunIdleThread()` to remove any other over-eager host-side reschedule that does not correspond to an upstream scheduler transition.

### Binary layout verification
- PASS: control-flow/scheduler behavior only; no guest-visible struct layout changed.

## 2026-04-02 — core/src/hle/kernel/svc/svc_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/svc/svc_thread.cpp

### Intentional differences
- Rust still carries targeted bootstrap diagnostics in this owner while the thread-start/sleep parity work is in flight. The owner remains correct: thread SVC behavior stays in `svc_thread.rs`.

### Unintentional differences (to fix)
- fixed in this pass: `SleepThread(ns > 0)` no longer only sets a scheduler request bit and returns to the guest loop. Upstream `GetCurrentThread(kernel).Sleep(timeout)` blocks immediately through `KScopedSchedulerLockAndSleep`; the Rust port now arms the wait there and leaves the immediate core handoff to the post-SVC CPU owner once the thread lock is released.
- fixed in this pass: `SetThreadCoreMask(...)` now validates `affinity_mask` against the process core mask before delegating to `KThread::set_core_mask(...)`, matching upstream `svc_thread.cpp`.
- still wrong: `CreateThread` still contains Rust-only fallback stack probing/mapping (`ensure_user_stack_mapping(...)`) that does not exist upstream and should be removed once the real stack mapping bug is fully understood and fixed in the correct owner.

### Missing items
- Re-audit `StartThread`/`Run()` with the latest worker-bootstrap comparison once the sleep handoff is validated; the bootstrap still carries extra diagnostics and may still differ in handle/thread-lifecycle details.

### Binary layout verification
- PASS: SVC control-flow only; no guest-visible struct layout changed.

## 2026-04-02 — core/src/hle/kernel/k_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.cpp

### Intentional differences
- Rust still omits the full pinned-waiter retry loop inside `SetCoreMask(...)`; the current port keeps ownership in `k_thread.rs` but only implements the affinity/core-state updates that are exercised by the MK8D worker bootstrap.

### Unintentional differences (to fix)
- fixed in this pass: `set_core_mask(...)` previously treated negative core ids as a no-op on `active_core/current_core` and copied the virtual mask directly into the physical mask. Upstream preserves `IdealCoreNoUpdate`, translates the virtual mask through `VirtualToPhysicalCoreMap`, and rehomes `active_core` when the old core is no longer allowed. Rust now matches that behavior for the active affinity update path.
- still wrong: the post-affinity-change pinned waiter handling from upstream `ThreadQueueImplForKThreadSetProperty` is not yet ported, so running pinned threads still do not retry this update path literally.

### Missing items
- `GetPhysicalCoreMask`
- full pinned waiter retry/update loop in `SetCoreMask(...)`

### Binary layout verification
- PASS: thread scheduler/control-flow behavior only; no raw guest-visible struct layout changed.

## 2026-04-02 — core/src/hle/kernel/k_hardware_timer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_hardware_timer.cpp

### Intentional differences
- Rust still resolves timer tasks through `Arc<Mutex<KThread>>` or a cached raw pointer instead of upstream inheritance from `KTimerTask`. Ownership remains in `k_hardware_timer.rs`, and the callback still targets the owning `KThread`.

### Unintentional differences (to fix)
- fixed in this pass: `KHardwareTimer::DoTask()` previously ran without taking the scheduler lock. Upstream executes `DoTask()` under `KScopedSchedulerLock` and the timer's own lock before waking timed-out threads. Rust now takes the owner scheduler lock and the timer base lock before collecting expired tasks and calling `thread.on_timer()`.
- still simplified: the Rust timer lock is a `Mutex<()>` from `KHardwareTimerBase` instead of upstream `KScopedSpinLock`, and the current callback does not yet model interrupt clearing beyond the existing event unschedule/rearm behavior.

### Missing items
- Re-audit `RegisterAbsoluteTask` / `CancelTask` ordering against upstream `KScopedDisableDispatch` + timer-lock interplay once the runtime wake path is validated.

### Binary layout verification
- PASS: timer/scheduler behavior only; no guest-visible struct layout changed.

## 2026-04-03 — core/src/hle/service/friend/friend_interface.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/friend/friend_interface.cpp

### Intentional differences
- Rust stores `Core::System` and `Module` explicitly on `Friend`, while upstream inherits the shared module owner through `Module::Interface`. Ownership still remains in `friend_interface.rs`.

### Unintentional differences (to fix)
- fixed in this pass: `CreateFriendService` and `CreateNotificationService` were registered with `None` callbacks, so the service always fell through to generic unimplemented-success handling instead of constructing the upstream sub-services.
- fixed in this pass: cmd `0`/`1` now push a real `IFriendService` / `INotificationService` object using the same domain vs non-domain response pattern as other upstream-like factory services.
- still simplified: cmd `2` (`CreateDaemonSuspendSessionService`) remains unimplemented, matching the existing null upstream handler registration.

### Missing items
- No daemon suspend session service owner yet.

### Binary layout verification
- PASS: IPC control-flow only; no raw guest-visible struct layout changed.

## 2026-04-03 — core/src/hle/service/friend/friend.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/friend/friend.cpp

### Intentional differences
- Rust currently keeps only the exercised command subset from upstream in the handler table rather than registering every null entry. The implemented methods still live in the correct owner file.
- `INotificationService::Pop` still returns success without serializing the notification payload bytes; state mutation and event ownership are now correct, but the payload layout is still missing.

### Unintentional differences (to fix)
- fixed in this pass: `IFriendService` and `INotificationService` existed only as plain structs and were not `SessionRequestHandler`s, so any pushed sub-service would still have been invalid at IPC dispatch time.
- fixed in this pass: the exercised upstream command handlers now build real IPC responses in this owner file, including copy-event returns for `GetCompletionEvent` and `GetEvent`.
- still simplified: UUID parsing uses raw `u128` words instead of an upstream `Common::UUID` owner type.

### Missing items
- Full upstream null-command table coverage for `IFriendService`.
- `INotificationService::Pop` output payload serialization.
- Destructor-parity cleanup for friend service events if later required beyond handle-table lifetime ownership.

### Binary layout verification
- PASS: no raw struct copy was introduced on an IPC payload boundary in this pass; event/object ownership only.

## 2026-04-03 — core/src/hle/service/audio/audio_renderer_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/audio/audio_renderer_manager.cpp

### Intentional differences
- Rust still stubs most renderer/backend construction details and does not yet pass the upstream parameter block, transfer-memory handle, process handle, or ARUID into a real `IAudioRenderer` backend. Ownership remains in `audio_renderer_manager.rs`.

### Unintentional differences (to fix)
- fixed in this pass: `OpenAudioRenderer`, `GetAudioDeviceService`, and `GetAudioDeviceServiceWithRevisionInfo` were constructing the IPC response with `num_objects_to_move = 0` while still pushing an IPC interface. That produced a response header with no outgoing object slots even though the command returns a sub-service upstream. These handlers now reserve one moved object in the response header, matching upstream `Out<SharedPointer<...>>`.

### Missing items
- Full upstream request parsing and validation for `OpenAudioRenderer`.
- Real `AudioCore::Renderer::Manager` ownership and session-count enforcement.
- Upstream ARUID/revision/process-handle propagation into `IAudioDevice` and `IAudioRenderer`.

### Binary layout verification
- PASS: this slice changes IPC response header/object counts only; no raw guest-visible struct layout changed.

## 2026-04-03 — core/src/hle/service/audio/audio_renderer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/audio/audio_renderer.cpp

### Intentional differences
- Rust still uses a stubbed `IAudioRenderer` without the upstream `AudioCore::Renderer::Renderer` backend, transfer-memory ownership, process-handle ownership, or event lifecycle/destructor parity. Ownership remains in `audio_renderer.rs`.

### Unintentional differences (to fix)
- fixed in this pass: `RequestUpdate` / `RequestUpdateAuto` previously reported success while leaving output buffers untouched (`WriteBuffer` with an empty slice). Upstream always writes renderer output through the provided out-buffers. The Rust stub now explicitly zero-initializes every writable output buffer so callers do not consume stale guest memory after a successful result.

### Missing items
- Full upstream renderer initialization and `impl->RequestUpdate(...)` behavior.
- Destructor parity (`Finalize`, `CloseEvent`, `process_handle->Close()`).
- Upstream state/sample-rate/sample-count values sourced from the real renderer backend instead of fixed stub values.

### Binary layout verification
- PASS: this slice only changes guest buffer initialization and IPC behavior; no raw struct definition layout changed.

## 2026-04-04 — core/src/hle/kernel/k_thread.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread.cpp

### Intentional differences
- Rust still stores `m_host_context` as `Arc<Fiber>` and thread ownership behind `Arc<Mutex<KThread>>` rather than upstream raw pointers and `std::shared_ptr`. Ownership remains in `k_thread.rs`.

### Unintentional differences (to fix)
- fixed in this pass: Rust had introduced a non-upstream lazy `pending_host_context_init` path that deferred `m_host_context` allocation until first schedule. Upstream constructs `m_host_context` eagerly in `InitializeThread(...)`; Rust now creates the fiber immediately in the matching initialization owners.

### Missing items
- Re-audit `common/src/fiber.rs` against upstream `common/fiber.cpp`/`.h`; `KThread` now matches the owner/lifecycle timing, but the underlying Rust fiber backend is still not a literal port.

### Binary layout verification
- PASS: host fiber lifecycle only; no guest-visible binary layout changed.

## 2026-04-04 — core/src/hle/service/psc/time/system_clock.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/psc/time/system_clock.cpp

### Intentional differences
- Rust keeps clock state in a local `Mutex<SystemClockState>` instead of holding upstream `SystemClockCore&` and `OperationEvent`. Ownership remains in `system_clock.rs`, and the exercised IPC behavior now lives in the correct owner file.

### Unintentional differences (to fix)
- fixed in this pass: exercised commands were registered with `None`, so `ISystemClock` fell through to auto-stub success instead of executing `GetCurrentTime` / `GetSystemClockContext` and related handlers.
- still simplified: the state is still local Rust state, not yet wired to the full upstream PSC clock-core graph.

### Missing items
- Reconnect this owner to `SystemClockCore` / `OperationEvent` parity.

### Binary layout verification
- PASS: `SystemClockContext` stays `repr(C)` and is serialized raw; this pass changed handler routing and local state ownership only.

## 2026-04-04 — core/src/hle/service/glue/time/time_zone.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/glue/time/time_zone.cpp

### Intentional differences
- Rust still omits upstream `FileTimestampWorker`, `set:sys` persistence, and operation-event fanout. Ownership remains in `glue/time/time_zone.rs`, and exercised read-only delegation stays in this owner.

### Unintentional differences (to fix)
- fixed in this pass: exercised read-only commands, especially `ToCalendarTimeWithMyRule`, were registered with `None`, so `time:u` returned auto-stub success and MK8D later aborted.
- still simplified: `ToCalendarTime` currently reconstructs the input rule from the first read buffer using existing IPC helpers instead of a dedicated `InLargeData<Tz::Rule, BufferAttr_HipcMapAlias>` abstraction.

### Missing items
- Full upstream callback coverage for every registered command.
- `SetDeviceLocationNameWithTimeZoneRule`, `ParseTimeZoneBinary`, and operation-event lifecycle parity.
- `FileTimestampWorker` and `set:sys` persistence side effects.

### Binary layout verification
- PASS: `CalendarTime`, `CalendarAdditionalInfo`, `LocationName`, and `RuleVersion` remain raw-serialized via their existing `repr(C)` layouts; this pass changed handler routing only.

## 2026-04-04 — core/src/core.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/core.cpp

### Intentional differences
- Rust still cannot own `AudioCore::AudioCore` directly inside `System` because `audio_core` depends on `core`. This pass narrows the existing type-erasure to a dedicated `AudioCoreInterface` bridge instead of a fully opaque `Any`, preserving the upstream owner boundary (`System` owns audio core) for exercised service calls.

### Unintentional differences (to fix)
- fixed in this pass: `System::audio_core` was stored as `Box<dyn Any + Send>`, so audio services could not call even the exercised upstream `AudioCore::Renderer::Manager::GetWorkBufferSize` path.

### Missing items
- Reconnect the rest of the exercised audio service surface (`OpenAudioRenderer`, real renderer/session ownership) through the same owner bridge or a stricter parity alternative.

### Binary layout verification
- PASS: owner bridge only; no guest-visible struct layout changed.

## 2026-04-04 — core/src/hle/service/audio/audio.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/audio/audio.cpp

### Intentional differences
- `audctl` remains a stubbed named service in Rust because `IAudioController` is still unported as a real session handler.

### Unintentional differences (to fix)
- fixed in this pass: `audren:u` was registered without passing `SystemRef` into `IAudioRendererManager`, which diverged from upstream `std::make_shared<IAudioRendererManager>(system)` and blocked access to the real audio-core owner.

### Missing items
- Full system-owned constructors for the remaining audio service managers.

### Binary layout verification
- PASS: service registration ownership only; no guest-visible struct layout changed.

## 2026-04-04 — core/src/hle/service/audio/audio_renderer_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/audio/audio_renderer_manager.cpp

### Intentional differences
- Rust still does not construct a full upstream `IAudioRenderer` backend in `OpenAudioRenderer`; that owner remains simplified and separate from this `GetWorkBufferSize` parity slice.

### Unintentional differences (to fix)
- fixed in this pass: `GetWorkBufferSize` returned a hardcoded `0x4000` and ignored the upstream `AudioRendererParameterInternal` request payload entirely.
- fixed in this pass: the manager had no `System` owner, so it could not delegate to the audio-core backend as upstream does.

### Missing items
- Full upstream request parsing/validation for `OpenAudioRenderer`.
- Real backend/session construction for `IAudioRenderer`.
- Real `IAudioDevice` revision/ARUID propagation.

### Binary layout verification
- PASS: the request payload is now forwarded as a raw `[u8; 0x34]` blob to the backend bridge without reinterpretation in the wrong owner; response remains a raw `u64`.

## 2026-04-04 — audio_core/src/audio_core.rs vs /home/vricosti/Dev/emulators/zuyu/src/audio_core/audio_core.cpp

### Intentional differences
- Rust implements the exercised `GetWorkBufferSize` owner bridge directly on `AudioCore` because `core` cannot name concrete `audio_core` types across the crate cycle.

### Unintentional differences (to fix)
- fixed in this pass: there was no way for `core` services to call the already-ported audio-core work-buffer calculation, so `audren:u` fell back to a fake constant.

### Missing items
- Reconnect `OpenAudioRenderer` and live renderer/session ownership through the same `AudioCore` owner.
- Re-audit frontend construction: `yuzu_cmd` still instantiates `AudioCore` with a detached Rust `System`, unlike upstream `AudioCore(system)`.

### Binary layout verification
- PASS: the bridge consumes the exact 0x34-byte `AudioRendererParameterInternal` payload and reinterprets it with unaligned raw-copy semantics matching the upstream binary layout.

## 2026-04-04 — core/src/hle/service/ipc_helpers.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/ipc_helpers.h

### Intentional differences
- Rust keeps request parsing as a manual helper API instead of the upstream C++ template-based CMIF serializer. This pass narrows that gap by adding an explicit natural-alignment helper for raw CMIF data, without changing file ownership.

### Unintentional differences (to fix)
- fixed in this pass: `RequestParser` had no way to reproduce upstream CMIF raw-data natural alignment, so handlers that manually parsed a non-8-byte-sized blob followed by `u64` read the next argument one word too early.

### Missing items
- Broader audit of manual request parsers that still depend on implicit packed layout assumptions.
- Full parity with upstream `cmif_serialization.h` argument-layout generation.

### Binary layout verification
- PASS: regression test covers a `0x34`-byte blob followed by two naturally aligned `u64` values and verifies the parser lands on the same words as upstream CMIF layout rules.

## 2026-04-04 — core/src/hle/service/audio/audio_renderer_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/audio/audio_renderer_manager.cpp

### Intentional differences
- Rust still routes backend construction through the crate-bridge `AudioCoreInterface` instead of instantiating the concrete upstream `IAudioRenderer` type directly in this owner.

### Unintentional differences (to fix)
- fixed in this pass: `OpenAudioRenderer` manually parsed `AudioRendererParameterInternal` and then read `u64` fields without restoring the upstream 8-byte alignment after the `0x34`-byte blob, shifting `tmem_size` left by 32 bits and causing a huge audio workbuffer allocation.

### Missing items
- Pass the real transfer-memory/process owners all the way to the concrete audio renderer service/backend, like upstream `KTransferMemory*` and `KProcess*`.
- Re-audit `GetAudioDeviceService*` ARUID/revision propagation against upstream.

### Binary layout verification
- PASS: `AudioRendererParameterInternal` remains a raw 0x34-byte payload; this pass only restores the upstream-aligned position of the following `u64` arguments.

## 2026-04-04 — core/src/hle/service/audio/audio_renderer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/audio/audio_renderer.cpp

### Intentional differences
- Rust still lazily creates only the readable end of the audio system event instead of owning a full upstream `KEvent` via `ServiceContext`.

### Unintentional differences (to fix)
- fixed in this pass: `QuerySystemEvent` created the event in a signaled state, diverging from the upstream service-context event lifecycle and making the returned event immediately ready.

### Missing items
- Port the full upstream `ServiceContext`/`KEvent` ownership for `rendered_event`.
- Wire actual render completion to the returned event instead of keeping audio-render completion as a separate `AtomicBool`.
- Open/close and retain the real process handle like upstream.

### Binary layout verification
- PASS: response shape remains one copied readable-event handle; this pass changes only initial signal state and owner notes.

## 2026-04-04 — core/src/hle/service/audio/audio_renderer_manager.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/service/audio/audio_renderer_manager.cpp

### Intentional differences
- Rust still creates the service-owned audio event manually in this file instead of through upstream `KernelHelpers::ServiceContext`, because the Rust `ServiceContext` owner is not yet ported to real `KEvent` ownership.

### Unintentional differences (to fix)
- fixed in this pass: `OpenAudioRenderer` created the backend session first and only then created a separate service-local event path, so the backend `audio_core` renderer never received the same render-completion event owner as upstream.

### Missing items
- Port the full upstream `ServiceContext` owner instead of manual event-object registration.
- Re-audit `OpenAudioRenderer` request/handle ownership against the upstream `KTransferMemory*` path.

### Binary layout verification
- PASS: no guest-visible struct layout changed; the response still returns one IPC object and no raw payload changes.

## 2026-04-04 — audio_core/src/audio_core.rs vs /home/vricosti/Dev/emulators/zuyu/src/audio_core/audio_core.cpp

### Intentional differences
- Rust still bridges `core` to `audio_core` with traits because the crates cannot name each other’s concrete owners directly.

### Unintentional differences (to fix)
- fixed in this pass: `open_audio_renderer()` could not receive or preserve the upstream service-owned rendered event, so the backend `Renderer/System` had no way to signal the same kernel object returned by `QuerySystemEvent`.

### Missing items
- Re-audit the remaining `AudioCore` construction path against upstream `AudioCore(system)`, especially the detached Rust `System` ownership noted earlier.

### Binary layout verification
- PASS: the audio parameter blob remains a raw 0x34-byte reinterpretation; this slice only adds owner propagation for the event object.

## 2026-04-04 — audio_core/src/renderer/audio_renderer.rs vs /home/vricosti/Dev/emulators/zuyu/src/audio_core/renderer/audio_renderer.cpp

### Intentional differences
- Rust still stores the upstream `System` owner behind `Arc<Mutex<...>>` because the manager/session bridge is shared across crates.

### Unintentional differences (to fix)
- fixed in this pass: `Renderer::new()`/`System::new()` did not accept the upstream rendered-event owner, so `Renderer` diverged structurally from `Renderer(system, manager, Kernel::KEvent*)`.

### Missing items
- Re-audit `Initialize/Finalize` ordering against the upstream `Renderer` once the remaining audio boot stall is fixed.

### Binary layout verification
- PASS: no raw payload layout is affected by this owner-only constructor change.

## 2026-04-04 — audio_core/src/renderer/system.rs vs /home/vricosti/Dev/emulators/zuyu/src/audio_core/renderer/system.cpp

### Intentional differences
- Rust still uses `ProcessHandle` as an opaque raw pointer wrapper instead of upstream `KProcess*` directly because the ADSP command-buffer bridge keeps process ownership erased at that boundary.

### Unintentional differences (to fix)
- fixed in this pass: `System` used a private `AtomicBool rendered_event` instead of the upstream `Kernel::KEvent* adsp_rendered_event`, so `Update()` and `SendCommandToDsp()` never cleared/signaled the same kernel object exported by `IAudioRenderer::QuerySystemEvent`.

### Missing items
- Re-audit the exact `Start/QuerySystemEvent/RequestUpdate` boot sequence against upstream, because MK8D still stalls before repeated `RequestUpdate`.
- Port any remaining `CoreTiming`/thread-priority details from upstream `SystemManager::ThreadFunc` if they prove relevant.

### Binary layout verification
- PASS: all guest-facing audio buffers remain unchanged; only kernel-event ownership and signaling behavior changed.

## 2026-04-04 — core/src/hle/kernel/k_condition_variable.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_condition_variable.cpp

### Intentional differences
- Rust still has to thread `Arc<Mutex<KProcess>>` through the wait path because the upstream owner has no process-wide mutex; the implementation now mirrors the upstream lock order more closely by acquiring the scheduler sleep guard before re-entering the process owner.

### Unintentional differences (to fix)
- fixed in this pass: `KProcess::wait_condition_variable()` / `KConditionVariable::wait()` held the process mutex while trying to acquire the global scheduler lock, creating a Rust-only AB/BA deadlock that upstream cannot hit because it has no outer process mutex.

### Missing items
- Port the upstream `Signal()` scheduler-lock ownership literally; the current Rust process-owner wrapper still bypasses that owner boundary.
- Re-audit `WaitForAddress()` against upstream, which still lacks a literal `KScopedSchedulerLock` wrapper in the Rust port.

### Binary layout verification
- PASS: no guest-visible payload changed; only lock ordering and owner access changed.

## 2026-04-04 — core/src/hle/kernel/k_process.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_process.cpp

### Intentional differences
- Rust still routes condition-variable waits through `Arc<Mutex<KProcess>>` rather than a raw process owner pointer.

### Unintentional differences (to fix)
- fixed in this pass: `WaitConditionVariable()` reacquired the process mutex before the scheduler sleep guard, which inverted upstream lock order and contributed to deadlock on multithreaded boot.

### Missing items
- Re-audit the remaining process-owned condvar/address-arbiter wrappers for the same mutex-vs-scheduler ordering issue.

### Binary layout verification
- PASS: owner-only control flow change; no raw struct layout changed.

## 2026-04-04 — core/src/hle/kernel/k_hardware_timer.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_hardware_timer.h/.cpp

### Intentional differences
- Rust stores timer state in an internal `Mutex<KHardwareTimerState>` instead of relying on upstream `KScopedSpinLock` + raw owner object. This preserves the upstream ownership shape (`Arc<KHardwareTimer>` with internal synchronization) while avoiding the previous outer-object mutex divergence.
- Thread resolution still uses `thread_id -> raw ptr` / `GSC` lookup because Rust does not model `KThread : KTimerTask` inheritance literally.

### Unintentional differences (to fix)
- fixed in this pass: `KHardwareTimer` was previously wrapped in an outer `Arc<Mutex<KHardwareTimer>>`, which created a Rust-only lock-order deadlock with `KScopedSchedulerLockAndSleep` and the CoreTiming callback.
- fixed in this pass: callback wiring used the outer mutex owner instead of an upstream-like direct timer object.

### Missing items
- Re-audit `KHardwareTimerBase` against upstream intrusive-tree behavior; it still uses `BTreeMap` and thread IDs rather than literal `KTimerTask*` ordering semantics.
- Remove temporary trace logging once the current MK8D blocker is fully isolated.

### Binary layout verification
- PASS: kernel-owner/lifecycle change only; no guest-visible payload layout changed.

## 2026-04-04 — core/src/hle/kernel/kernel.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/kernel.cpp

### Intentional differences
- Rust still stores the kernel timer as `Option<Arc<KHardwareTimer>>` rather than an in-place member object, because multiple Rust owners need shared access to the timer.

### Unintentional differences (to fix)
- fixed in this pass: `KernelCore` exposed the hardware timer as `Arc<Mutex<KHardwareTimer>>`, propagating the outer-object mutex mismatch through all wait paths.

### Missing items
- Re-audit shutdown/finalize ordering for other shared kernel owners now that the timer no longer uses an outer mutex wrapper.

### Binary layout verification
- PASS: owner-only change.

## 2026-04-04 — core/src/hle/kernel/k_thread_queue.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_thread_queue.h/.cpp

### Intentional differences
- Rust still stores an optional `Arc<KHardwareTimer>` on wait queues because it cannot rely on upstream raw pointer/member inheritance.

### Unintentional differences (to fix)
- fixed in this pass: wait queues carried `Arc<Mutex<KHardwareTimer>>`, which forced cancellation paths through the same outer-object timer mutex that did not exist upstream.

### Missing items
- Re-audit all wait-queue derived owners for exact parity on timer-cancel ordering once tracing is removed.

### Binary layout verification
- PASS: queue-owner change only.

## 2026-04-04 — core/src/hle/kernel/k_scoped_scheduler_lock_and_sleep.rs vs /home/vricosti/Dev/emulators/zuyu/src/core/hle/kernel/k_scoped_scheduler_lock_and_sleep.h

### Intentional differences
- Rust still returns the timer owner as `Option<Arc<KHardwareTimer>>` from `new()` because downstream wait queues need shared ownership rather than a raw pointer.

### Unintentional differences (to fix)
- fixed in this pass: `Drop` had temporarily diverged from upstream by unlocking before timer registration to avoid the deadlock caused by the old outer `Mutex<KHardwareTimer>`.
- fixed in this pass: exact upstream order is restored now that the timer owner no longer sits behind an outer object mutex.

### Missing items
- Remove temporary caller/debug tracing once the current MK8D scheduler/runtime blocker is fully isolated.

### Binary layout verification
- PASS: RAII/lifecycle change only; no guest-visible binary layout involved.

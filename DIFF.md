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

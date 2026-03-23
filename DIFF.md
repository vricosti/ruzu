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

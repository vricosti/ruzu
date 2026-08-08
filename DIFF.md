# Current upstream parity debt

This file contains only active differences confirmed in the current source tree against
`~/Dev/emulators/zuyu`. Implementation history, diagnostics, commands, runtime logs, and audit
procedures are intentionally omitted.

## Kernel

### Unintentional differences (to fix)

- `core/src/hle/kernel/k_device_address_space.rs` does not own a `KDevicePageTable`.
  `initialize_static` and `finalize` are empty, while upstream initializes and finalizes the
  device page table and routes mapping through it.
- `core/src/hle/kernel/k_thread.rs` omits upstream's `m_activity_pause_lock` and still uses
  `restore_priority_simplified` on paths that do not have access to the complete process thread
  graph. Upstream always performs the full priority-inheritance owner-chain walk.
- `core/src/hle/kernel/k_process.rs` still represents thread-local pages, thread ownership, and
  shared-memory ownership with Rust side vectors instead of upstream's intrusive kernel-object
  structures.
- `core/src/hle/kernel/k_worker_task_manager.rs` has the asynchronous queue but not upstream's
  `KernelCore` ownership and lifecycle.
- Light ports are not supported by `svc_port.rs` and `sm.rs`; affected requests are rejected
  instead of using upstream's light-session path.
- The exception SVC path does not notify upstream's reporter or debugger because those owners are
  not connected to `svc_exception.rs`.

## HLE services

### Missing items

- The following handlers are registered with `None` even though the matching upstream command
  table connects a real implementation:
  - `ssl/ssl.rs`: `GetCertificates`, `GetCertificateBufSize`.
  - `nvdrv/nvmemp.rs`: `Open`, `GetAruid`.
  - `ldn/user_local_communication_service.rs`: `GetState`, `GetNetworkInfo`, `GetIpv4Address`,
    `GetDisconnectReason`, `GetSecurityParameter`, `GetNetworkConfig`, `AttachStateChangeEvent`,
    `GetNetworkInfoLatestUpdate`, `Scan`, `ScanPrivate`, `SetWirelessControllerRestriction`,
    `OpenAccessPoint`, `CloseAccessPoint`, `CreateNetwork`, `CreateNetworkPrivate`,
    `DestroyNetwork`, `SetAdvertiseData`, `SetStationAcceptPolicy`, `AddAcceptFilterEntry`,
    `OpenStation`, `CloseStation`, `Connect`, `Disconnect`, `Initialize`, `Finalize`, and
    `Initialize2`.
  - `nfc/nfc.rs`: `Initialize`, `Finalize`.
  - `btm/btm_system.rs` and `btm/btm_user.rs`: `GetCore`.
  - `btm/btm_user_core.rs`: the four BLE event-acquisition commands.
  - `btm/btm_system_core.rs`: gamepad pairing, radio control/event, connected/paired audio-device
    queries, and audio-device connection-rejection commands.
  - `caps/caps_ss.rs`: `SaveScreenShotEx0`, `SaveEditedScreenShotEx1`.
  - `caps/caps_su.rs`: `SetShimLibraryVersion`, `SaveScreenShotEx0`, `SaveScreenShotEx1`.
  - `caps/caps_u.rs`: `SetShimLibraryVersion`, `GetAlbumFileList0AafeAruidDeprecated`,
    `GetAlbumFileList3AaeAruid`.
  - `caps/caps_c.rs`: `SetShimLibraryVersion`.
  - `acc/async_context.rs`: `GetSystemEvent`, `Cancel`, `HasDone`, `GetResult`.
  - `acc/acc_su.rs` and `acc/acc_u1.rs`: `GetBaasAccountManagerForSystemService`,
    `StoreSaveDataThumbnail`.
  - `spl/csrng.rs`: `GenerateRandomBytes`.
  - `am/service/application_creator.rs`: `CreateApplication`.
  - `am/service/application_accessor.rs`: `GetAppletStateChangedEvent`, `GetResult`,
    `RequestForApplicationToGetForeground`, `GetCurrentLibraryApplet`, `PushLaunchParameter`,
    `GetApplicationControlProperty`, and `SetUsers`.
- `hid/hid_server.rs` returns placeholder success/zero results for GameCube ERM and N64 boolean
  vibration commands instead of routing them through upstream's vibration-device objects.
- `frontend/applets/profile_select.rs` always returns the zero UUID because the upstream
  `ProfileManager` lookup is not ported.

## Network and web services

### Missing items

- `network` does not implement the ENet transport. Room creation/join, peer lifecycle, packet
  delivery, chat, moderation, and announcement loops remain local stubs.
- `web_service/src/web_backend.rs` has no HTTP client; generic web requests return a local error.
- Web telemetry submission is disabled in `core/src/telemetry_session.rs`.
- The LDN service has helper methods for a small subset of commands, but its IPC table remains
  disconnected as listed above and it does not own upstream's event and network lifecycle.

## Input and frontends

### Missing items

- `ruzu_cmd/src/sdl_config.rs` can read the currently bridged settings but does not implement the
  upstream reload/save and INI write paths for SDL, players, debug controls, and HIDBus values.
- `ruzu/src/configuration/configure_hotkeys.rs` displays default bindings, but bindings are not
  editable or persisted because `HotkeyRegistry` is absent; Clear All and Restore Defaults only
  log requests.
- Several advanced input configuration actions in
  `ruzu/src/configuration/configure_input_advanced.rs` remain informational placeholders.
- The Android Oboe audio backend is represented by a no-op stub in
  `audio_core/src/sink/oboe_sink.rs`.

## Video core

### Unintentional differences (to fix)

- `renderer_opengl/gl_rasterizer.rs` drops indirect byte-count draws and indirect-count-buffer
  draws. Upstream emits the corresponding `glMultiDraw*Indirect` and
  `glMultiDraw*IndirectCount` calls.
- OpenGL asynchronous shader compilation falls back to synchronous compilation because the
  upstream `ShaderWorker` owner is absent. `renderer_opengl/gl_shader_context.rs` also has empty
  object pools and no shared frontend graphics context, unlike upstream's per-worker context.
- Vulkan texture blits still reject multisampled depth/stencil helper blits and
  non-MSAA-to-MSAA copies that upstream routes through its runtime copy helpers.
- Vulkan turbo mode owns a simplified keep-alive thread rather than upstream's complete control
  loop.

### Missing items

- `host1x/codecs/vp8.rs` and `vp9.rs` return empty frame bitstreams; their upstream frame
  composition and memory-manager integration are missing.

## Shader recompiler and JIT

### Unintentional differences (to fix)

- `shader_recompiler/src/backend/glasm/glasm_emit_context.rs` omits upstream GLASM image
  descriptor binding construction.
- rdynarmic's ARM64 backend rejects FP-to-fixed combinations with nonzero fractional bits or
  `ToOdd` rounding that upstream Dynarmic emits, and its vector backend has the same gap.
- rdynarmic's ARM64 backend still has a catch-all error for unported IR opcodes and rejects large
  cycle-count immediates; upstream emits both cases.
- rdynarmic's x64 backend does not implement dynamic `ExtractRegister32` and
  `ExtractRegister64`, although upstream emits them.
- rdynarmic's x64 exclusive-inline fastmem helpers remain `unimplemented!()` while upstream
  generates exclusive read/write fastmem paths.

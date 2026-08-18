# Current upstream parity debt

This file contains only active differences confirmed in the current source tree against
`~/Dev/emulators/zuyu`. Implementation history, diagnostics, commands, runtime logs, and audit
procedures are intentionally omitted.

## Kernel

### Unintentional differences (to fix)

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
  - `ldn/user_local_communication_service.rs`: `GetState`, `GetNetworkInfo`, `GetIpv4Address`,
    `GetDisconnectReason`, `GetSecurityParameter`, `GetNetworkConfig`, `AttachStateChangeEvent`,
    `GetNetworkInfoLatestUpdate`, `Scan`, `ScanPrivate`, `SetWirelessControllerRestriction`,
    `OpenAccessPoint`, `CloseAccessPoint`, `CreateNetwork`, `CreateNetworkPrivate`,
    `DestroyNetwork`, `SetAdvertiseData`, `SetStationAcceptPolicy`, `AddAcceptFilterEntry`,
    `OpenStation`, `CloseStation`, `Connect`, `Disconnect`, `Initialize`, `Finalize`, and
    `Initialize2`.
  - `btm/btm_system_core.rs`: gamepad pairing, radio control/event, connected/paired audio-device
    queries, and audio-device connection-rejection commands.
  - `acc/acc_su.rs` and `acc/acc_u1.rs`: `GetBaasAccountManagerForSystemService`; its upstream
    `IManagerForSystemService` prerequisite is not ported.
  - `am/service/application_creator.rs`: `CreateApplication`.
  - `am/service/application_accessor.rs`: `GetAppletStateChangedEvent`, `GetResult`,
    `RequestForApplicationToGetForeground`, `GetCurrentLibraryApplet`, `PushLaunchParameter`,
    `GetApplicationControlProperty`, and `SetUsers`.
- `hid/hid_server.rs` returns placeholder success/zero results for GameCube ERM and N64 boolean
  vibration commands instead of routing them through upstream's vibration-device objects.

## Network and web services

### Missing items

- `network` does not implement the ENet transport. Room creation/join, peer lifecycle, packet
  delivery, chat, moderation, and announcement loops remain local stubs.
- `web_service/src/web_backend.rs` has no HTTP client; generic web requests return a local error.
- Web telemetry submission is disabled in `core/src/telemetry_session.rs`.
- The LDN service has helper methods for a small subset of commands, but its IPC table remains
  disconnected as listed above and it does not own upstream's event and network lifecycle.

## Input and frontends

## 2026-08-09 — `ruzu/{src/game_list.rs,src/uisettings.rs,src/configuration/qt_config.rs,src/main.rs,i18n/catalogs.json}` vs `src/yuzu/{game_list.cpp,game_list_p.h,uisettings.h,configuration/qt_config.cpp}` and `dist/languages/*.ts` (`GameListFavorites`, `ToggleFavorite`, `AddFavorite`, `RemoveFavorite`, and `AddFavoritesPopup`)

### Intentional differences

- Qt represents Favorites with a `GameListFavorites` `QStandardItem` subclass and hides its row
  through `QTreeView::setRowHidden`. GTK has no hidden-row API for `TreeListModel`, so ruzu gives
  `GameEntry` an explicit Favorites kind and removes/reinserts that root at position zero. Its child
  store remains alive, preserving the same visible behavior and ordering. Synthetic collapse
  notifications emitted while that root is absent are ignored, and inserting the first favorite
  explicitly expands the new GTK row to reproduce Qt revealing its still-expanded hidden row.
- The upstream colorful-theme `folder.png` and `star.png` assets are embedded into ruzu rather than
  resolved from the host GTK icon theme. This preserves the upstream 48 px artwork while keeping
  ruzu independent of both the desktop theme and the zuyu source tree at runtime.
- Upstream incrementally clones or removes one `QStandardItem` row. Ruzu rebuilds the small Favorites
  child store from already-scanned immutable `GameEntry` metadata after each toggle; no directory is
  rescanned, and first-match/configured-id ordering remains identical.

The `favorites_expanded` setting is loaded, applied to the GTK tree row, updated on expansion changes,
and persisted under upstream's `UIGameList\\favorites_expanded` key.

### Binary layout verification

- Not applicable. The changed state is GTK frontend model data only.

## 2026-08-09 — `ruzu/src/game_list.rs` vs `src/yuzu/game_list.cpp` (`GameList::PopupContextMenu` and `AddGamePopup`)

### Intentional differences

- Upstream fully configures each `QAction`, including the checkable Favorite state, before
  `QMenu::exec` materializes and displays the menu. GTK resolves stateful `GMenu` rows through an
  action group, so ruzu installs that group and parents/styles the empty `GtkPopoverMenu` before
  assigning its menu model. This preserves upstream's single layout pass and avoids initially
  rendering Favorite as a stateless row before rebuilding it as a checkbox.

### Binary layout verification

- Not applicable. This only changes GTK context-menu construction order.

## 2026-08-09 — `ruzu/src/main_window.rs` vs `src/yuzu/main.{h,cpp}` (`GMainWindow::OnRestartGame`)

### Intentional differences

- Upstream calls `ShutdownGame()` and immediately continues to `BootGame()` after its Qt shutdown
  synchronization. The GTK frontend requests the same confirmed shutdown non-blockingly, retains a
  copy of `current_game_path`, and calls `boot_game` only after `LoadingEvent::StopComplete` has
  joined the emulation thread and released the native render target.
- A pending restart is discarded when teardown reports a failure or the application window is
  closing, preventing a shutdown callback from launching a new session behind an error or close.

### Binary layout verification

- Not applicable. This changes frontend action wiring and lifecycle state only. A focused regression
  test verifies that the retained restart path survives only a successful non-closing shutdown.

## 2026-08-09 — `ruzu/src/configuration/qt_config.rs`, `configure_dialog.rs`, and `main.rs` vs `src/frontend_common/config.cpp` and `src/yuzu/configuration/qt_config.cpp`

### Intentional differences

- Rust keeps generic settings, Qt-compatible controls, and GTK UI values in separate writers over
  the same INI file. They execute in upstream order: generic `ReadValues`/`SaveValues` first, then
  frontend-owned controls and UI values.

### Binary layout verification

- Not applicable. A focused regression test verifies that the global `[Renderer]` category is read
  and that `backend=0` selects OpenGL instead of retaining the Vulkan default.

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

## 2026-08-09 — `ruzu/src/boot.rs`, `main_window.rs`, and `render_window_x11.rs` vs `src/video_core/video_core.cpp` and `src/yuzu/bootmanager.{h,cpp}`

### Intentional differences

- GTK4 does not expose a native child render widget, so the Linux frontend creates an X11 child
  with a GLX-compatible visual and retains an `Arc`-owned root share group. Renderer and shader
  worker contexts share that root, matching upstream `OpenGLSharedContext` ownership and thread
  behavior without Qt objects.
### Missing items

- The GTK frontend's shared OpenGL context bridge currently exists only for Linux/X11. The macOS
  and Windows GTK render-window adapters still provide Vulkan surfaces only.

### Unintentional differences (to fix)

- Renderer-construction failures still terminate through the existing CLI-style hard-error path.
  Upstream propagates renderer creation failure through `CreateGPU`, allowing the frontend to show
  an error without terminating the process; Rust's current `System::subsystem_factory` callback
  cannot return a `Result` yet.

### Binary layout verification

- Not applicable. This slice changes frontend native-context ownership and renderer dispatch; no
  guest-visible structure or raw payload layout changes.

## 2026-08-09 — `video_core/src/renderer_vulkan/turbo_mode.rs`, `renderer_vulkan/texture_cache.rs`, and `host_shaders/vulkan_turbo_mode.comp` vs `src/video_core/renderer_vulkan/vk_turbo_mode.{h,cpp}`, `vk_texture_cache.cpp`, and `host_shaders/vulkan_turbo_mode.comp`

### Intentional differences

- `TurboMode` moves a separately owned `TurboResources` bundle into its worker thread and exposes
  an `Arc` callback to `Scheduler`; upstream captures the containing object from a `std::jthread`.
  The device, workload, 100 ms idle predicate, queue-submit notification, and destruction ordering
  are unchanged.
- `TextureCacheRuntime` receives `cant_blit_msaa` during construction instead of retaining the full
  Vulkan `Device` wrapper. It uses the same predicate as upstream `Image::NeedsScaleHelper` and the
  same color or combined depth/stencil helper blits.

### Binary layout verification

- The turbo compute shader is byte-for-byte identical to upstream. This slice introduces no
  guest-visible raw-memory structure.

## 2026-08-09 — `video_core/src/host1x/codecs/vp8.rs`, `vp9.rs`, and `vp9_types.rs` vs `src/video_core/host1x/codecs/vp8.{h,cpp}`, `vp9.{h,cpp}`, and `vp9_types.h`

### Intentional differences

- Decoder methods receive the current `NvdecRegisters` explicitly through the existing Rust
  `DecoderImpl` trait; upstream retains the register owner in the decoder base class.
- Rust `Vec<u8>` values replace upstream `ScratchBuffer` and `Stream` owners without changing the
  emitted VP8/VP9 byte order or frame buffering lifecycle.

### Binary layout verification

- `Vp8PictureInfo` is `0xc0` bytes. `PictureInfo`, `EntropyProbs`, and `Vp9EntropyProbs` are
  respectively `0x100`, `0xea0`, and `0x7b4` bytes; compile-time offset assertions cover the fields
  read from NVDEC memory. Focused tests verify VP8 frame tags and VP9 range/bitstream encoder bytes.

## 2026-08-09 — `common/src/thread_worker.rs`, `video_core/src/rasterizer_interface.rs`, and renderer disk-cache loaders vs `src/common/thread_worker.h`, `src/video_core/rasterizer_interface.h`, and renderer shader caches

### Intentional differences

- Rust passes an `Arc<AtomicBool>` through `RasterizerInterface::load_disk_resources` instead of a
  copied `std::stop_token`. `StatefulThreadWorker::wait_for_requests_or_stop` polls that state while
  blocked because `std::sync::Condvar` has no stop-callback integration; observing cancellation
  permanently stops every worker and abandons queued work, matching upstream `request_stop()`
  semantics.
- The command-line frontend supplies a never-signaled cancellation owner because it has no loading
  dialog. The GTK frontend forwards the same stop state that owns its launch lifecycle.

### Binary layout verification

- Not applicable: this slice changes synchronization and owner propagation only.

## 2026-08-09 — `video_core/src/renderer_opengl/gl_state_tracker.rs` and `gl_rasterizer.rs` vs `src/video_core/renderer_opengl/gl_state_tracker.{h,cpp}` and `gl_rasterizer.cpp`

### Intentional differences

- `StateTracker` stores the active channel dirty flags as `NonNull<[bool; 256]>` and clears that
  borrowed pointer in `release_channel`; upstream stores a raw C++ pointer whose lifetime follows
  the channel owner implicitly.
- The scoped lock over the buffer and texture caches uses the existing retrying dual-lock helper
  because `parking_lot::ReentrantMutex` has no direct `std::scoped_lock` equivalent.

### Binary layout verification

- Not applicable: this slice changes owner references and lifecycle ordering only; no guest-visible
  structure is serialized or copied as raw bytes.

## 2026-08-09 — `video_core/src/texture_cache/texture_cache_base.rs` vs `src/video_core/texture_cache/texture_cache_base.h` and `control/channel_state_cache.inc`

### Intentional differences

- `channel_gpu_memory` is a Rust shared-owner mirror of upstream's live
  `channel_state->gpu_memory` reference. It is resynchronized after channel erasure so releasing an
  inactive channel preserves the active memory owner and releasing the active channel clears it.

### Binary layout verification

- Not applicable: this slice only updates channel ownership state.

## 2026-08-09 — `video_core/src/renderer_opengl/` vs `src/video_core/renderer_opengl/`

### Intentional differences

- Every upstream OpenGL header/implementation basename has a matching Rust module. Rust-only
  `mod.rs` files provide module declarations and do not own upstream behavior.
- `RendererOpenGL` boxes the single `StateTracker`, while `RasterizerOpenGL`, the texture runtime,
  and blit helpers hold stable non-owning pointers to it. This preserves upstream's single shared
  owner graph without creating movable Rust self-references.
- `QueryCache` receives `RasterizerOpenGL::any_command_queued()` immediately before the four query
  synchronization entry points instead of storing a back-reference to its containing rasterizer.
  The observable predicate and call ordering match upstream while avoiding another self-reference.
- Render-target and descriptor helpers receive register projections created from the production
  `Maxwell3DDrawView::Live` owner. Upstream dereferences `maxwell3d` directly inside the cache; the
  Rust projection avoids overlapping mutable borrows while reading the same live registers at the
  operation boundary.
- Backend `Image` state is stored separately from generic `ImageBase` state. Methods such as
  scaling therefore receive the paired base image explicitly instead of using C++ inheritance.

### Binary layout verification

- `ComputePipelineKey` is 24 bytes, `GraphicsPipelineKey` is 624 bytes, the GLASM bindless SSBO
  payload is 16 bytes, and `ScreenRectVertex` is four contiguous `GLfloat` values. Focused tests
  verify these raw-byte contracts.

## Shader recompiler and JIT

## 2026-08-09 — `rdynarmic/src/backend/arm64/emit_arm64_floating_point.rs`, `emit_arm64_vector_floating_point.rs`, and x64 exclusive-memory emitters vs Dynarmic `backend/arm64/emit_arm64_{floating_point,vector_floating_point}.cpp` and `backend/x64/emit_x64_memory.cpp.inc`

### Intentional differences

- ARM64 instruction words are emitted through rdynarmic's local encoder instead of Oaknut. The
  scalar half/fixed-16 conversions, reciprocal operations, `FMULX`, and vector half conversions
  preserve upstream register widths, FPCR/FPSR handling, and instruction ordering.
- x64 fastmem fallback addresses are offsets in rdynarmic's generated fallback table rather than
  Xbyak function pointers. Exclusive monitor locking, reservation invalidation, `cmpxchg` widths,
  and the `0` success / `1` failure status follow upstream.
- The x64 exception layer exposes upstream's `SupportsFastmem` capability as a compile-target
  predicate. A32 and A64 emitters disable direct fastmem when no native exception handler exists,
  while Linux/x86-64 and Windows/x86-64 retain fault redirection.
- The 128-bit exclusive-write split uses runtime SSE4.1 detection. Its fallback reproduces
  upstream's `movaps`/`movq`/`punpckhqdq` sequence on hosts without `pextrq`.
- A64 exclusive accesses emit upstream `EmitCheckMemoryAbort`; exclusive reads record the resume
  address immediately after the faulting load and only emit an explicit bounds-abort block when
  `EmitFastmemVAddr` requests one. Exclusive writes retain upstream's unconditional deferred fault
  stub and post-callback resume point.

### Binary layout verification

- No guest payload layout changes. Focused x64 tests cover 8/16/32/64/128-bit fallback generation,
  successful `LDAXR`/`STLXR` and `LDXP` return paths, host exception-handler capability, and a
  fault redirected to the raw exclusive callback.
- The ARM64 scalar/vector FP routing and half/fixed-16 conversion tests compile for
  `aarch64-unknown-linux-gnu` and pass under QEMU. This also verifies that the former cross-target
  exception-handler build failure is no longer present.

### Unintentional differences (to fix)

- rdynarmic's ARM64 backend still has a catch-all error for unported IR opcodes. Implemented
  upstream families still absent from the Rust dispatcher include packed arithmetic, scalar and
  vector saturation, AES/SHA/CRC/SM4 cryptography, and selected integer vector reductions,
  min/max, halving, rounding, and broadcast operations. Upstream's 16-bit FP specializations that
  themselves terminate with `ASSERT_FALSE("Unimplemented")` are not counted as port debt.

## 2026-08-09 — `frontend_common/src/play_time_manager.rs` vs Eden `src/frontend_common/play_time_manager.{h,cpp}`

### Intentional differences

- Rust uses a channel and `JoinHandle` in place of `std::jthread` and its stop token. Stop still
  wakes and joins the worker, accounts the final whole-second interval, then persists the database.
- A mutex protects the database because GTK can read it while the timestamp worker updates it.

### Binary layout verification

- PASS: each entry is two consecutive little-endian `u64` values and occupies 16 bytes, matching
  Eden's raw `PlayTimeElement` array in `playtime.bin`.

## 2026-08-09 — `ruzu/src/game_list.rs` vs Eden `src/yuzu/game/game_list.{h,cpp}` and `src/qt_common/game_list/{model,worker}.{h,cpp}`

### Intentional differences

- GTK `ColumnView` factories replace Qt `QStandardItem` subclasses while preserving Eden's Name,
  File type, Size, Play time, and Add-ons column order, values, and visibility settings.
- Eden transfers worker results with Qt signals. Ruzu transfers plain scan results over a channel
  and materializes GTK objects on the main context. A generation counter provides Eden's stale-work
  cancellation guarantee when a newer refresh supersedes an older scan.
- The metadata worker builds a filesystem controller and provider union because ruzu has no
  persistent frontend `Core::System`; NAND, SDMC, and game-directory manual content are mounted
  before `PatchManager` is queried.
- The internal action identifier remains `properties`, while its visible label is Eden's
  `Configure Game`.

### Binary layout verification

- Not applicable to the GTK model; the shared play-time file layout is verified separately.

## 2026-08-09 — `ruzu/src/{boot,main_window}.rs` vs Eden `src/yuzu/main_window.{h,cpp}`

### Intentional differences

- Eden starts play-time accounting directly in `OnStartGame`. Ruzu's boot thread emits a lossless
  `Started { program_id }` event so GTK performs the equivalent transition. Pause, resume, stop,
  restart, and guest-driven exit retain Eden's ordering.

### Binary layout verification

- Not applicable: this changes frontend lifecycle events only.

## 2026-08-09 — `ruzu/src/configuration/configure_per_game_addons.rs` vs Eden `src/yuzu/configuration/configure_per_game_addons.{h,cpp,ui}`

### Intentional differences

- Eden reuses its persistent frontend `Core::System`. Ruzu rebuilds NAND, SDMC, and configured game
  directory providers while Configure Game is open, then queries the same `PatchManager` data.
- GTK uses a `gio::ListStore` rather than `QStandardItemModel`; patch name, version, enabled state,
  sorting, and disabled-addon persistence retain their upstream roles.

### Binary layout verification

- Not applicable: this is host frontend state.

## 2026-08-09 — `common/src/settings.rs` vs Eden `src/common/settings.h`

### Intentional differences

- `ext_content_from_game_dirs` participates in ruzu's generic category visitor instead of Eden's
  C++ settings linkage, preserving the same default and persisted value.
- `gpu_fence_behavior` uses ruzu's generic switchable-setting visitor and GTK combo-row frontend
  instead of Eden's C++ linkage and Qt widget. The five enum values, persisted key, default, range,
  per-game switchability, and helper predicates match Eden.

### Binary layout verification

- Not applicable: this setting is not guest-visible.

## 2026-08-09 — `core/src/file_sys/registered_cache.rs` vs Eden `src/core/file_sys/registered_cache.{h,cpp}`

### Intentional differences

- `ExternalUpdateEntry::files` uses seven `Option<VirtualFile>` elements in place of nullable C++
  handles. The raw `ContentRecordType` index and seven-entry contract are unchanged.
- `open_container_as_nsp` probes NSP and then XCI directly, preserving Eden's final parser fallback
  without introducing a reverse dependency from `file_sys` to the loader dispatcher.

### Binary layout verification

- Not applicable: manual-provider entries are host-only. Focused tests cover highest-version
  selection, descending update order, and clearing versioned entries.

## 2026-08-09 — `video_core/src/engines/maxwell_3d.rs` and `video_core/src/buffer_cache/buffer_cache.rs` vs Eden `src/video_core/engines/maxwell_3d.h` and `src/video_core/buffer_cache/buffer_cache.h`

### Intentional differences

- Rust reads transform-feedback registers through `transform_feedback_buffer_info` rather than
  exposing the packed register union. `size` and `start_offset` remain signed `s32` values, and the
  buffer cache preserves their raw two's-complement bit patterns when forming GPU addresses and
  sizes.
- `PrimitivesSucceededStreamer` owns the same dependency on the transform-feedback byte counter,
  but the Rust query owner retains the dependent host report directly instead of storing an index
  into Eden's generic `SimpleStreamer` pool. Topology conversion, tessellation-output remapping,
  patch-vertex handling, per-stream stride selection, reset forwarding, and zero-stride handling
  remain identical.
- The external recursive buffer-cache mutex is held in an `Arc`. This keeps the mutex owned by the
  cache while allowing Vulkan query operations to clone the lock before mutating the cache, instead
  of creating an aliased raw pointer to a field of the active mutable reference.

### Binary layout verification

- PASS: focused register tests verify that `0xffff_fff0` and `0xffff_ffe0` are exposed as `-16`
  and `-32`; consumers cast back to unsigned values without clamping or normalization.

## 2026-08-09 — `video_core/src/renderer_vulkan/query_cache.rs`, `scheduler.rs`, `vk_rasterizer.rs`, `renderer_vulkan.rs`, and `video_core/src/vulkan_common/vulkan_device.rs` vs Eden `src/video_core/renderer_vulkan/vk_query_cache.{h,cpp}`, `vk_scheduler.{h,cpp}`, `vk_rasterizer.{h,cpp}`, `renderer_vulkan.{h,cpp}`, and `vk_device.{h,cpp}`

### Intentional differences

- Query banks use Rust leases and shared state handles instead of Eden's `BankPool` and raw
  `QueryCache*`. Slot reuse, render-pass close ordering, query reset ordering, and final-value
  synchronization follow the upstream lifecycle.
- Transform-feedback query banks retain a non-owning allocator pointer because the renderer owns
  the allocator for longer than the rasterizer and query cache. Readback uses a mapped mirror while
  preserving Eden's begin/end/copy ordering and four-stream contract.
- Dynamic vertex input is rebuilt from the complete Maxwell description through Vulkan dynamic
  state. Attribute and binding limits, constant-attribute filtering, divisors, and dirty-state
  clearing follow `RasterizerVulkan::UpdateVertexInput`.
- `report_device_loss` is a module helper so query-bank owners that retain an `ash::Device` rather
  than the complete `Device` can execute Eden's same error-and-delay behavior.
- Vulkan buffers retained by the allocator are represented by raw `vk::Buffer` handles rather than
  Eden's RAII wrappers. Their lifetime remains bounded by the renderer-owned allocator, which
  outlives the boxed query cache and its compute passes.
- Channel-bound guest-address translation uses a boxed adapter because the generic Rust query cache
  stores trait-object pointers. Conditional rendering is stopped before that adapter is released.
- Multi-slot occlusion reports feed Eden's exact prefix-scan shaders and push constants directly
  into the tracked common buffer-cache destination. The Rust query owner retains cumulative query
  leases instead of reproducing Eden's `HostSyncValues` staging vectors; reset and accumulation
  boundaries produce the same prefix value. Resolve and intermediary buffers use Eden's lazy
  power-of-two size classes with the same 2048-slot minimum and are reused for the renderer
  lifetime.
- The direct guest-buffer path copies Eden's complete 8-byte query value. A producer-specific
  barrier orders either query-pool transfer writes or prefix-scan compute writes before the final
  transfer read.
- Host conditional rendering uses the same direct-buffer and compute-resolve paths, extension
  commands, driver fallbacks, comparison inversion, and transfer/host barriers. Rust stores the
  active Vulkan setup in scheduler-shared state so render-pass transitions can pause it without a
  raw `QueryCacheRuntime*`.

### Binary layout verification

- PASS: the compute push-constant structs are `repr(C)` and verified as 4 bytes for conditional
  rendering and 16 bytes for prefix scan. The three GLSL sources are byte-identical to Eden.
  Focused tests cover slot ordering, cumulative ZPass reports, primitive topology conversion,
  unsynchronized fence rejection, empty ZPass reports, scan size classes and producer barriers,
  TFB stream mapping, query payload/timestamp writes, and draw preparation ordering.

## 2026-08-09 — `video_core/src/renderer_vulkan/compute_pass.rs`, `descriptor_pool.rs`, and `update_descriptor.rs` vs Eden `src/video_core/renderer_vulkan/vk_compute_pass.{h,cpp}`, `vk_descriptor_pool.{h,cpp}`, and `vk_update_descriptor.{h,cpp}`

### Intentional differences

- `DescriptorAllocator` clones share allocator state through `Arc<Mutex<_>>` so Rust's `Send +
  'static` scheduler closures can perform Eden's descriptor-set commit on the worker. The resource
  pool, bank, layout and tick-based reuse remain shared by the same compute-pass owner.
- Raw descriptor payload pointers are wrapped in a `Send` newtype. The queue owns one fixed
  allocation for the renderer lifetime, and its frame ring waits for the worker before recycling a
  slice, matching Eden's recorded `const DescriptorUpdateEntry*` lifetime.

### Binary layout verification

- PASS: compute descriptor templates use `size_of::<DescriptorUpdateEntry>()` as Eden does. Unit
  tests verify the union size/alignment and the two- and three-buffer template strides.

## 2026-08-09 — `core/src/core.rs` and `core/src/hle/kernel/kernel.rs` vs Eden `src/core/hle/kernel/kernel.cpp`

### Intentional differences

- Ruzu still owns one shared `KMemoryBlockSlabManager` instead of Eden's separate application and
  system managers. Its runtime capacity is now the exact sum of Eden's 20000-entry application and
  10000-entry system heaps, so the adaptation no longer lowers the available resource limit.

### Missing items

- Separate application and system `KSystemResource` ownership remains to be ported before the two
  memory-block slab managers can be represented independently.

### Binary layout verification

- PASS: no guest-visible binary layout is changed; the regression test verifies both upstream
  capacities and their combined runtime value.

## 2026-08-09 — `core/src/hle/kernel/k_shared_memory.rs` vs Eden `src/core/hle/kernel/k_shared_memory.{h,cpp}`

### Unintentional differences (fixed)

- Allocation failure now returns `Kernel::ResultOutOfMemory` (`0xD001`) as Eden does; the previous
  raw `0xCE01` encoded `Kernel::ResultOutOfResource`.

### Binary layout verification

- PASS: no structure layout changed.

## 2026-08-18 — workspace SDL manifests vs Eden `src/audio_core/CMakeLists.txt`, `src/input_common/CMakeLists.txt`, and `src/yuzu_cmd/CMakeLists.txt`

### Intentional differences

- Eden links `SDL3::SDL3` supplied by CMake. Ruzu pins `sdl3` 0.18.4 and
  `sdl3-sys` 0.6.8 (SDL 3.4.14) in the workspace and builds the static SDL3
  library from source. This keeps the same SDL generation and one resolved
  runtime across Linux, macOS, Windows, and BSD hosts without requiring a
  platform package, pkg-config, or vcpkg SDL installation.
- `input_common` uses the raw `sdl3-sys` API because its port mirrors the C API;
  `audio_core` and `ruzu_cmd` use the higher-level `sdl3` crate while still
  resolving the same `sdl3-sys` package and native SDL library.

### Unintentional differences (to fix)

- None found in the desktop SDL3 dependency ownership or generation.

### Missing items

- Cross-target dependency resolution was verified for Windows MSVC, macOS
  aarch64, and FreeBSD. Native linking and runtime execution still require CI
  or hardware for each target.

### Binary layout verification

- N/A: this change affects native dependency selection only. `audio_core` and
  `input_common` unit tests pass with the resolved SDL 3.4.14 build.

## 2026-08-18 — `ruzu/Info.plist` and `scripts/build-macos-app.sh` vs Eden `src/yuzu/Info.plist` and `src/yuzu/CMakeLists.txt`

### Intentional differences

- Eden uses CMake's `MACOSX_BUNDLE` target property; ruzu's Cargo workspace uses a dedicated
  packaging script after `cargo build --release --bin ruzu`. Both produce the same macOS bundle
  ownership and directory layout.
- Eden copies prebuilt `eden.icns` and `Assets.car` resources. Ruzu generates `ruzu.icns` from the
  frontend-owned rusty-lemon PNG because it does not have an Apple asset catalog.
- The local developer bundle receives an ad-hoc signature after MoltenVK is copied. Distribution
  identity signing and notarization remain release-pipeline responsibilities.

### Unintentional differences (to fix)

- None found in the application bundle layout or MoltenVK lookup path.

### Missing items

- Ruzu has no liquid-glass `Assets.car` equivalent to Eden's asset catalog.

### Binary layout verification

- PASS: the generated bundle contains an arm64 `Contents/MacOS/ruzu`, a valid `Info.plist`,
  `Contents/Resources/ruzu.icns`, and an arm64
  `Contents/Frameworks/libMoltenVK.dylib`. `codesign --verify --deep --strict` passes, and a
  LaunchServices smoke test starts the bundled executable.

## 2026-08-18 — `video_core/src/vulkan_common/vulkan_library.rs` vs Eden `src/video_core/vulkan_common/vulkan_library.cpp`

### Intentional differences

- Both implementations retain `LIBVULKAN_PATH` as the first explicit lookup and prefer the
  application bundle next. For an unbundled development `ruzu-cmd`, Rust additionally searches the
  sibling Eden build so performance and rendering comparisons use Eden's exact bundled MoltenVK.
- `scripts/build-macos-app.sh` likewise copies Eden's bundled MoltenVK when available, after an
  explicit `MOLTENVK_LIBRARY` override and before the Homebrew fallback.

### Unintentional differences (to fix)

- None found in lookup priority. The previous development fallback selected a different emulator's
  MoltenVK 1.4.2 while the current Eden build embeds MoltenVK 1.4.1.

### Missing items

- Distribution builds still need a release-owned MoltenVK artifact rather than relying on a sibling
  development checkout.

### Binary layout verification

- N/A: the Vulkan loader ABI is unchanged; this only selects the dynamic library implementation.

## 2026-08-18 — `ruzu_cmd/src/emu_window/emu_window_sdl3_vk.rs` vs Eden `src/yuzu_cmd/emu_window/emu_window_sdl3_vk.cpp`

### Intentional differences

- Ruzu stores the `CAMetalLayer` returned by `SDL_Metal_GetLayer` as the render surface and retains
  the `SDL_MetalView` separately for its lifetime. Eden stores the opaque Metal view directly while
  its Vulkan surface path consumes it as a `CAMetalLayer`; the Rust split keeps the consumed native
  object explicit without changing the Cocoa ownership boundary.

### Unintentional differences (to fix)

- None. The SDL3 migration had left `WindowSystemInfo::type_` at `Headless` on macOS; it now assigns
  `Cocoa` before publishing the Metal layer, matching Eden's constructor ordering.

### Missing items

- None for macOS window-system selection.

### Binary layout verification

- N/A: no serialized or guest-visible structure is changed.

## 2026-08-18 — `video_core/src/vulkan_common/vulkan_device.rs` vs Eden `src/video_core/vulkan_common/vulkan_device.cpp`

### Intentional differences

- None in the format-property probe list.

### Unintentional differences (to fix)

- None. The ten ETC2/EAC formats at the end of Eden's `GetFormatProperties` format list are now
  queried by ruzu as well. Previously they missed the cache and `is_format_supported` conservatively
  returned true after logging `Unimplemented format query`, which also prevented device-aware
  storage, blit, and texel-buffer capability checks from using the real driver properties.
- Eden explicitly disables `robustBufferAccess2` and `robustImageAccess2` while retaining
  `nullDescriptor`. Ruzu now applies the same feature mutation before passing the queried feature
  chain to `vkCreateDevice`; previously all robustness2 features advertised by MoltenVK remained
  enabled.

### Missing items

- None for the format-property probe list or robustness2 feature selection.

### Binary layout verification

- N/A: the change only extends physical-device capability discovery.

## 2026-08-18 — `video_core/src/renderer_vulkan/query_cache.rs` vs Eden `src/video_core/renderer_vulkan/vk_query_cache.cpp`

### Intentional differences

- Rust query reports share their measured slots and synchronized result through `Arc` rather than
  Eden's query IDs and `HostQueryBase::IsFinalValueSynced` flag. The report remains unavailable to
  the guest writeback callback until the matching async-flush set has been popped.

### Unintentional differences (to fix)

- None in the host occlusion-query flush lifecycle. The Vulkan `SamplesStreamer` now participates
  in `HasUnsyncedQueries`, `PushUnsyncedQueries`, `ShouldWaitAsyncFlushes`, and
  `PopUnsyncedQueries`. Previously it bypassed that lifecycle and called
  `vkGetQueryPoolResults` before the corresponding fence, producing `VK_NOT_READY` and thousands
  of unsynchronized-query errors.
- `pending_flush_sets` is protected across the GPU and GPU-fencing threads, matching Eden's
  `flush_guard`. The initial Rust adaptation omitted this synchronization.

### Missing items

- None for host occlusion-query fence synchronization. The existing Rust lease-based bank owner
  remains an intentional structural adaptation documented in the 2026-08-09 query-cache entry.

### Binary layout verification

- N/A: no guest-visible structure changed. All 17 focused Vulkan query-cache tests pass. A
  90-second title run produced zero `Query report value not synchronized` and zero
  `vkGetQueryPoolResults ... NOT_READY` messages; the previous implementation produced roughly
  8,000 such messages in the same startup/title interval.

## 2026-08-18 — `core/src/gpu_core.rs` and `video_core/src/gpu.rs` vs Eden `src/video_core/gpu.{h,cpp}`

### Intentional differences

- The cross-crate `GpuCoreInterface` exposes Eden's concrete `GPU` methods to `core`; its test
  doubles in `memory.rs`, `nvhost_as_gpu.rs`, and `nvhost_gpu.rs` implement `wait_for_composite`
  as a no-op because they have no GPU thread or renderer.
- Rust stores the pending composite fence in `AtomicU64` because the split interface is callable
  through shared references. Eden stores the same single pending fence as a plain `u64` under its
  HWC/GPU-thread lifecycle.

### Unintentional differences (to fix)

- None. `RequestComposite` now records the pending sync-operation fence and returns after
  `TickGPU`; it no longer waits synchronously. `WaitForComposite` consumes and waits that fence at
  the next HWC tick, including Eden's zero-fence and shutdown exits.

### Missing items

- None for the composite request/wait lifecycle.

### Binary layout verification

- N/A: no guest-visible or serialized structure changed.

## 2026-08-18 — `core/src/hle/service/nvdrv/devices/nvdisp_disp0.rs` vs Eden `src/core/hle/service/nvdrv/devices/nvdisp_disp0.{h,cpp}`

### Intentional differences

- The Rust owner forwards through `GpuCoreInterface` because `core` cannot own the concrete
  `video_core::Gpu`; the call position and behavior match Eden's direct `system.GPU()` call.

### Unintentional differences (to fix)

- None. `wait_for_composite` now forwards Eden's HWC synchronization point to the GPU.

### Missing items

- None for composite waiting.

### Binary layout verification

- N/A: no ABI payload changed.

## 2026-08-18 — `core/src/hle/service/nvnflinger/display.rs` and `hardware_composer.rs` vs Eden `src/core/hle/service/nvnflinger/display.h` and `hardware_composer.{h,cpp}`

### Intentional differences

- Rust uses `BTreeMap` and `Arc<Mutex<Layer>>` in place of Eden's `flat_map` and shared pointers;
  keys, layer ownership and mutation boundaries are unchanged.

### Unintentional differences (to fix)

- None. `Layer` now owns Eden's `z_index` and `is_overlay` fields with the same defaults.
- `ComposeLocked` now waits for the previous composite, releases eligible buffers before
  acquisition, interval-gates non-overlay acquisition, excludes overlays from game cadence,
  stable-sorts real z indices, composites only after a new acquisition, advances exactly one HWC
  frame, and returns one.
- Framebuffer release numbers are absolute (`frame_number + interval`), `last_acquire_frame` is
  tracked, and overlays release independently, matching Eden's lifecycle and ordering.

### Missing items

- None in the framebuffer cadence and release lifecycle covered by this slice.

### Binary layout verification

- N/A: these are host-side service structures. The Layer default regression test passes.

## 2026-08-18 — `core/src/hle/service/nvnflinger/surface_flinger.rs` vs Eden `src/core/hle/service/nvnflinger/surface_flinger.{h,cpp}`

### Intentional differences

- Rust returns `Option<Arc<Mutex<Layer>>>` from `find_layer` instead of a nullable shared pointer.

### Unintentional differences (to fix)

- None. `find_layer` is again a public SurfaceFlinger-owned operation, and the overlay setter
  updates the matching layer where Eden owns that mutation. Z-index writes remain owned by
  `Container`, which uses this lookup exactly as Eden does.

### Missing items

- None for layer lookup, z-index, visibility, blending, and overlay state.

### Binary layout verification

- N/A: no guest-visible structure changed.

## 2026-08-18 — `core/src/hle/service/vi/container.rs`, `manager_display_service.rs`, and `system_display_service.rs` vs Eden `src/core/hle/service/vi/container.{h,cpp}`, `manager_display_service.{h,cpp}`, and `system_display_service.{h,cpp}`

### Intentional differences

- Rust returns `Result<T, ResultCode>` rather than writing C++ `Out<T>` parameters. The CMIF
  handlers retain Eden's wire ordering and signed-to-unsigned bit casts.

### Unintentional differences (to fix)

- None. Container now owns set/get z-index and overlay forwarding. ManagerDisplayService exposes
  its upstream z-index forwarding method.
- SystemDisplayService now wires `GetLayerZ`, parses `SetLayerZ` as `layer_id: u64` followed by
  `z_value: u64`, preserves the low signed 32-bit z pattern, and forwards visibility instead of
  returning success without changing the layer.

### Missing items

- None for the z-index and visibility methods covered by this slice.

### Binary layout verification

- PASS: SetLayerZ consumes two consecutive 64-bit request values in Eden's signature order;
  GetLayerZ returns the signed 32-bit z index sign-extended and reinterpreted as `u64`.

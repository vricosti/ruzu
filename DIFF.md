# Current upstream parity debt

This file contains only active differences confirmed in the current source tree against
`~/Dev/emulators/zuyu`. Implementation history, diagnostics, commands, runtime logs, and audit
procedures are intentionally omitted.

## Kernel

### Unintentional differences (to fix)

- `src/core/src/hle/kernel/k_process.rs` still represents thread-local pages, thread ownership, and
  shared-memory ownership with Rust side vectors instead of upstream's intrusive kernel-object
  structures.
- `src/core/src/hle/kernel/k_worker_task_manager.rs` has the asynchronous queue but not upstream's
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
- `src/web_service/src/web_backend.rs` has no HTTP client; generic web requests return a local error.
- Web telemetry submission is disabled in `src/core/src/telemetry_session.rs`.
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

## 2026-08-09 — `src/ruzu/src/game_list.rs` vs `src/yuzu/game_list.cpp` (`GameList::PopupContextMenu` and `AddGamePopup`)

### Intentional differences

- Upstream fully configures each `QAction`, including the checkable Favorite state, before
  `QMenu::exec` materializes and displays the menu. GTK resolves stateful `GMenu` rows through an
  action group, so ruzu installs that group and parents/styles the empty `GtkPopoverMenu` before
  assigning its menu model. This preserves upstream's single layout pass and avoids initially
  rendering Favorite as a stateless row before rebuilding it as a checkbox.

### Binary layout verification

- Not applicable. This only changes GTK context-menu construction order.

## 2026-08-09 — `src/ruzu/src/main_window.rs` vs `src/yuzu/main.{h,cpp}` (`GMainWindow::OnRestartGame`)

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

## 2026-08-09 — `src/ruzu/src/configuration/qt_config.rs`, `configure_dialog.rs`, and `main.rs` vs `src/frontend_common/config.cpp` and `src/yuzu/configuration/qt_config.cpp`

### Intentional differences

- Rust keeps generic settings, Qt-compatible controls, and GTK UI values in separate writers over
  the same INI file. They execute in upstream order: generic `ReadValues`/`SaveValues` first, then
  frontend-owned controls and UI values.

### Binary layout verification

- Not applicable. A focused regression test verifies that the global `[Renderer]` category is read
  and that `backend=0` selects OpenGL instead of retaining the Vulkan default.

### Missing items

- `src/ruzu_cmd/src/sdl_config.rs` can read the currently bridged settings but does not implement the
  upstream reload/save and INI write paths for SDL, players, debug controls, and HIDBus values.
- `src/ruzu/src/configuration/configure_hotkeys.rs` displays default bindings, but bindings are not
  editable or persisted because `HotkeyRegistry` is absent; Clear All and Restore Defaults only
  log requests.
- Several advanced input configuration actions in
  `src/ruzu/src/configuration/configure_input_advanced.rs` remain informational placeholders.
- The Android Oboe audio backend is represented by a no-op stub in
  `src/audio_core/src/sink/oboe_sink.rs`.

## Video core

## 2026-08-09 — `src/ruzu/src/boot.rs`, `main_window.rs`, and `render_window_x11.rs` vs `src/video_core/video_core.cpp` and `src/yuzu/bootmanager.{h,cpp}`

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

## 2026-08-09 — `src/video_core/src/renderer_vulkan/turbo_mode.rs`, `renderer_vulkan/texture_cache.rs`, and `host_shaders/vulkan_turbo_mode.comp` vs `src/video_core/renderer_vulkan/vk_turbo_mode.{h,cpp}`, `vk_texture_cache.cpp`, and `host_shaders/vulkan_turbo_mode.comp`

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

## 2026-08-09 — `src/video_core/src/host1x/codecs/vp8.rs`, `vp9.rs`, and `vp9_types.rs` vs `src/video_core/host1x/codecs/vp8.{h,cpp}`, `vp9.{h,cpp}`, and `vp9_types.h`

### Intentional differences

- Decoder methods receive the current `NvdecRegisters` explicitly through the existing Rust
  `DecoderImpl` trait; upstream retains the register owner in the decoder base class.
- Rust `Vec<u8>` values replace upstream `ScratchBuffer` and `Stream` owners without changing the
  emitted VP8/VP9 byte order or frame buffering lifecycle.

### Binary layout verification

- `Vp8PictureInfo` is `0xc0` bytes. `PictureInfo`, `EntropyProbs`, and `Vp9EntropyProbs` are
  respectively `0x100`, `0xea0`, and `0x7b4` bytes; compile-time offset assertions cover the fields
  read from NVDEC memory. Focused tests verify VP8 frame tags and VP9 range/bitstream encoder bytes.

## 2026-08-09 — `src/common/src/thread_worker.rs`, `src/video_core/src/rasterizer_interface.rs`, and renderer disk-cache loaders vs `src/common/thread_worker.h`, `src/video_core/rasterizer_interface.h`, and renderer shader caches

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

## 2026-08-09 — `src/video_core/src/renderer_opengl/gl_state_tracker.rs` and `gl_rasterizer.rs` vs `src/video_core/renderer_opengl/gl_state_tracker.{h,cpp}` and `gl_rasterizer.cpp`

### Intentional differences

- `StateTracker` stores the active channel dirty flags as `NonNull<[bool; 256]>` and clears that
  borrowed pointer in `release_channel`; upstream stores a raw C++ pointer whose lifetime follows
  the channel owner implicitly.
- The scoped lock over the buffer and texture caches uses the existing retrying dual-lock helper
  because `parking_lot::ReentrantMutex` has no direct `std::scoped_lock` equivalent.

### Binary layout verification

- Not applicable: this slice changes owner references and lifecycle ordering only; no guest-visible
  structure is serialized or copied as raw bytes.

## 2026-08-09 — `src/video_core/src/texture_cache/texture_cache_base.rs` vs `src/video_core/texture_cache/texture_cache_base.h` and `control/channel_state_cache.inc`

### Intentional differences

- `channel_gpu_memory` is a Rust shared-owner mirror of upstream's live
  `channel_state->gpu_memory` reference. It is resynchronized after channel erasure so releasing an
  inactive channel preserves the active memory owner and releasing the active channel clears it.

### Binary layout verification

- Not applicable: this slice only updates channel ownership state.

## 2026-08-09 — `src/video_core/src/renderer_opengl/` vs `src/video_core/renderer_opengl/`

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

## 2026-08-09 — `src/rdynarmic/src/backend/arm64/emit_arm64_floating_point.rs`, `emit_arm64_vector_floating_point.rs`, and x64 exclusive-memory emitters vs Dynarmic `backend/arm64/emit_arm64_{floating_point,vector_floating_point}.cpp` and `backend/x64/emit_x64_memory.cpp.inc`

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

## 2026-08-09 — `src/frontend_common/src/play_time_manager.rs` vs Eden `src/frontend_common/play_time_manager.{h,cpp}`

### Intentional differences

- Rust uses a channel and `JoinHandle` in place of `std::jthread` and its stop token. Stop still
  wakes and joins the worker, accounts the final whole-second interval, then persists the database.
- A mutex protects the database because GTK can read it while the timestamp worker updates it.

### Binary layout verification

- PASS: each entry is two consecutive little-endian `u64` values and occupies 16 bytes, matching
  Eden's raw `PlayTimeElement` array in `playtime.bin`.

## 2026-08-09 — `src/ruzu/src/game_list.rs` vs Eden `src/yuzu/game/game_list.{h,cpp}` and `src/qt_common/game_list/{model,worker}.{h,cpp}`

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

## 2026-08-09 — `src/ruzu/src/{boot,main_window}.rs` vs Eden `src/yuzu/main_window.{h,cpp}`

### Intentional differences

- Eden starts play-time accounting directly in `OnStartGame`. Ruzu's boot thread emits a lossless
  `Started { program_id }` event so GTK performs the equivalent transition. Pause, resume, stop,
  restart, and guest-driven exit retain Eden's ordering.

### Binary layout verification

- Not applicable: this changes frontend lifecycle events only.

## 2026-08-09 — `src/ruzu/src/configuration/configure_per_game_addons.rs` vs Eden `src/yuzu/configuration/configure_per_game_addons.{h,cpp,ui}`

### Intentional differences

- Eden reuses its persistent frontend `Core::System`. Ruzu rebuilds NAND, SDMC, and configured game
  directory providers while Configure Game is open, then queries the same `PatchManager` data.
- GTK uses a `gio::ListStore` rather than `QStandardItemModel`; patch name, version, enabled state,
  sorting, and disabled-addon persistence retain their upstream roles.

### Binary layout verification

- Not applicable: this is host frontend state.

## 2026-08-09 — `src/common/src/settings.rs` vs Eden `src/common/settings.h`

### Intentional differences

- `ext_content_from_game_dirs` participates in ruzu's generic category visitor instead of Eden's
  C++ settings linkage, preserving the same default and persisted value.
- `gpu_fence_behavior` uses ruzu's generic switchable-setting visitor and GTK combo-row frontend
  instead of Eden's C++ linkage and Qt widget. The five enum values, persisted key, default, range,
  per-game switchability, and helper predicates match Eden.

### Binary layout verification

- Not applicable: this setting is not guest-visible.

## 2026-08-09 — `src/core/src/file_sys/registered_cache.rs` vs Eden `src/core/file_sys/registered_cache.{h,cpp}`

### Intentional differences

- `ExternalUpdateEntry::files` uses seven `Option<VirtualFile>` elements in place of nullable C++
  handles. The raw `ContentRecordType` index and seven-entry contract are unchanged.
- `open_container_as_nsp` probes NSP and then XCI directly, preserving Eden's final parser fallback
  without introducing a reverse dependency from `file_sys` to the loader dispatcher.

### Binary layout verification

- Not applicable: manual-provider entries are host-only. Focused tests cover highest-version
  selection, descending update order, and clearing versioned entries.

## 2026-08-09 — `src/video_core/src/engines/maxwell_3d.rs` and `src/video_core/src/buffer_cache/buffer_cache.rs` vs Eden `src/video_core/engines/maxwell_3d.h` and `src/video_core/buffer_cache/buffer_cache.h`

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

## 2026-08-09 — `src/video_core/src/renderer_vulkan/query_cache.rs`, `scheduler.rs`, `vk_rasterizer.rs`, `renderer_vulkan.rs`, and `src/video_core/src/vulkan_common/vulkan_device.rs` vs Eden `src/video_core/renderer_vulkan/vk_query_cache.{h,cpp}`, `vk_scheduler.{h,cpp}`, `vk_rasterizer.{h,cpp}`, `renderer_vulkan.{h,cpp}`, and `vk_device.{h,cpp}`

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

## 2026-08-09 — `src/video_core/src/renderer_vulkan/compute_pass.rs`, `descriptor_pool.rs`, and `update_descriptor.rs` vs Eden `src/video_core/renderer_vulkan/vk_compute_pass.{h,cpp}`, `vk_descriptor_pool.{h,cpp}`, and `vk_update_descriptor.{h,cpp}`

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

## 2026-08-09 — `src/core/src/core.rs` and `src/core/src/hle/kernel/kernel.rs` vs Eden `src/core/hle/kernel/kernel.cpp`

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

## 2026-08-09 — `src/core/src/hle/kernel/k_shared_memory.rs` vs Eden `src/core/hle/kernel/k_shared_memory.{h,cpp}`

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

## 2026-08-18 — `src/ruzu/Info.plist` and `scripts/build-macos-app.sh` vs Eden `src/yuzu/Info.plist` and `src/yuzu/CMakeLists.txt`

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

## 2026-08-18 — `src/video_core/src/vulkan_common/vulkan_library.rs` vs Eden `src/video_core/vulkan_common/vulkan_library.cpp`

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

## 2026-08-18 — `src/ruzu_cmd/src/emu_window/emu_window_sdl3_vk.rs` vs Eden `src/yuzu_cmd/emu_window/emu_window_sdl3_vk.cpp`

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

## 2026-08-18 — `src/video_core/src/vulkan_common/vulkan_device.rs` vs Eden `src/video_core/vulkan_common/vulkan_device.cpp`

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

## 2026-08-18 — `src/video_core/src/renderer_vulkan/query_cache.rs` vs Eden `src/video_core/renderer_vulkan/vk_query_cache.cpp`

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

## 2026-08-18 — `src/core/src/gpu_core.rs` and `src/video_core/src/gpu.rs` vs Eden `src/video_core/gpu.{h,cpp}`

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

## 2026-08-18 — `src/core/src/hle/service/nvdrv/devices/nvdisp_disp0.rs` vs Eden `src/core/hle/service/nvdrv/devices/nvdisp_disp0.{h,cpp}`

### Intentional differences

- The Rust owner forwards through `GpuCoreInterface` because `core` cannot own the concrete
  `video_core::Gpu`; the call position and behavior match Eden's direct `system.GPU()` call.

### Unintentional differences (to fix)

- None. `wait_for_composite` now forwards Eden's HWC synchronization point to the GPU.

### Missing items

- None for composite waiting.

### Binary layout verification

- N/A: no ABI payload changed.

## 2026-08-18 — `src/core/src/hle/service/nvnflinger/display.rs` and `hardware_composer.rs` vs Eden `src/core/hle/service/nvnflinger/display.h` and `hardware_composer.{h,cpp}`

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

## 2026-08-18 — `src/core/src/hle/service/nvnflinger/surface_flinger.rs` vs Eden `src/core/hle/service/nvnflinger/surface_flinger.{h,cpp}`

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

## 2026-08-18 — `src/core/src/hle/service/vi/container.rs`, `manager_display_service.rs`, and `system_display_service.rs` vs Eden `src/core/hle/service/vi/container.{h,cpp}`, `manager_display_service.{h,cpp}`, and `system_display_service.{h,cpp}`

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

## 2026-08-20 — `src/video_core/src/query_cache/bank_base.rs` vs Eden `src/video_core/query_cache/bank_base.h`

### Intentional differences

- `BankPool::can_recycle_front` exposes the exact predicate used by `ReserveBank` so the Vulkan
  caller can construct fallible resources before entering Rust's infallible builder closure.
- The file was normalized from CRLF to LF while formatting the new implementation and tests.

### Unintentional differences (to fix)

- None. Reserve, close, reference counting, reset, dead-bank selection and queue rotation retain
  Eden's ordering and conditions.

### Missing items

- None for `BankBase` and `BankPool`.

### Binary layout verification

- N/A: these are host-only bookkeeping types.

## 2026-08-20 — `src/video_core/src/renderer_vulkan/query_cache.rs` vs Eden `src/video_core/renderer_vulkan/vk_query_cache.{h,cpp}`

### Intentional differences

- Samples banks live in `Arc` and hold `BankBase` behind a mutex so fence-thread reports can own
  their banks safely; Eden stores banks by value in `std::deque`.
- Reports materialize bank spans instead of following `next_bank`. They retain independent bank
  references, remain cumulative until reset, and merge min/max ranges per bank across each flush
  set before host readback.
- The CPU and GPU halves of recycled pool reset are split because `BankLike::reset` cannot receive
  `&mut Scheduler`; the GPU reset is still recorded before the first reused slot.
- Scheduler-facing accessors return the three independently locked state handles needed by the
  safe cross-owner adaptation.

### Unintentional differences (to fix)

- None in samples report ownership, async-flush gating, bank-wide host readback, or the scheduler
  bridge covered by this correction.

### Missing items

- Existing parity debt outside this correction remains in the full Eden samples accumulation
  state machine (`amend_value`, `accumulation_value`, checkpoints and the complete
  `PresyncWrites`/`SyncWrites` lifecycle).
- A real Vulkan occlusion-query title run is still required; unit tests do not execute a device
  query pool.

### Binary layout verification

- N/A: no guest-visible raw-memory structure changed.

## 2026-08-20 — `src/video_core/src/renderer_vulkan/scheduler.rs` vs Eden `src/video_core/renderer_vulkan/vk_scheduler.{h,cpp}`

### Intentional differences

- Rust stores shared handles to `SamplesQueryState`, `TfbCounterState` and `QueryRuntimeState`
  instead of Eden's non-owning `QueryCache*`. This avoids aliased `&mut` references while keeping
  `EndPendingOperations` and `EndRenderPass` call ordering identical.
- `clear_query_cache_state` releases those handles before the rasterizer's Vulkan resources are
  destroyed; Eden relies on C++ member lifetime and its raw pointer is not dereferenced afterward.

### Unintentional differences (to fix)

- None in the reviewed counter-reset, counter-close and conditional-rendering ordering.

### Missing items

- None for this scheduler/query-cache interaction slice.

### Binary layout verification

- N/A: scheduler state is host-only.

## 2026-08-20 — `src/video_core/src/renderer_vulkan/vk_rasterizer.rs` vs Eden `src/video_core/renderer_vulkan/vk_rasterizer.{h,cpp}`

### Intentional differences

- The Rust constructor installs safe query-state handles only after every fallible resource
  creation succeeds, rather than storing Eden's direct `QueryCache*`. This prevents failed
  construction from leaving a dangling scheduler registration.
- The destructor explicitly clears those handles after `finish` and before destroying the query
  cache's Vulkan resource owners.

### Unintentional differences (to fix)

- None in construction registration, async query flush forwarding, or teardown ordering.

### Missing items

- None for the reviewed scheduler/query-cache ownership slice.

### Binary layout verification

- N/A: no guest ABI or serialized payload changed.

## 2026-08-20 — `src/core/src/hle/service/am/service/library_applet_creator.rs` vs Eden `src/core/hle/service/am/service/library_applet_creator.{h,cpp}`

### Intentional differences

- Rust manually parses CMIF arguments and resolves the transfer-memory handle through the current
  process, replacing Eden's typed `InCopyHandle<KTransferMemory>` deserializer.
- Rust returns service objects through `ResponseBuilder` rather than C++ `Out<SharedPointer<T>>`.

### Unintentional differences (to fix)

- None. `CreateTransferMemoryStorage` now naturally aligns the `s64` following the `bool`, and
  both transfer-memory creation commands validate `size` before resolving the handle, matching
  Eden's argument layout and validation order.

### Missing items

- None for the storage creation handlers reviewed in this slice.

### Binary layout verification

- PASS: `RequestParser::align_for::<i64>()` advances the raw CMIF cursor to the same 8-byte
  boundary used by Eden's typed serialization.

## 2026-08-20 — `src/ruzu/src/applets/software_keyboard.rs` vs Eden `src/yuzu/applets/qt_software_keyboard.{h,cpp}`

### Intentional differences

- GTK widgets, CSS and a main-loop channel replace Qt Designer widgets, Qt queued signals and the
  dedicated `InputInterpreter` thread; the frontend remains owned by the GUI module.
- Inline hide destroys the GTK dialog and recreates it on the next show while retaining guest text
  state; Eden hides and reuses its Qt dialog. This avoids retaining a hidden modal GTK window.
- The GTK frontend uses a single-line `Entry` for every draw type and does not reproduce Eden's
  framebuffer-relative geometry, controller artwork or DPI-specific Qt layout.

### Unintentional differences (to fix)

- None in the reviewed applet contract. Normal submissions now retain the active dialog through
  `Failure`/`Confirm` text checks, and only `ExitKeyboard` tears it down.
- Controller callbacks no longer re-enter `active: RefCell` while it is borrowed, and the input
  edge which opened the keyboard is discarded instead of immediately activating X/Cancel.
- Inline appear parameters, guest text/cursor updates, `ChangedString`, `MovedCursor`, key-disable
  flags, optional number-pad symbols, Shift/Caps Lock transitions and wrapped grid navigation now
  follow Eden's corresponding paths.

### Missing items

- Eden's held-button autorepeat and rich multi-line `SwkbdTextDrawType::Box` presentation remain UI
  features of the excluded Qt frontend; they are not part of this GTK crash/lifecycle correction.

### Binary layout verification

- N/A: this is host UI state. Guest-visible string lengths and cursor positions are explicitly
  converted to UTF-16 code-unit counts, with a focused regression test.

## 2026-08-20 — `src/ruzu/src/applets/mod.rs`, `src/ruzu/src/boot.rs`, and `src/ruzu/src/main_window.rs` vs Eden `src/yuzu/main_window.{h,cpp}` software-keyboard ownership

### Intentional differences

- `GMainWindow` creates the persistent GTK channel frontend and passes its trait object through
  `boot_game`; Eden owns a persistent `QtSoftwareKeyboard` signal bridge and allocates the dialog
  from its main-window slots.
- The module and boot wiring have no direct file counterpart because Eden's Qt frontend directory
  is excluded and ruzu owns its GTK frontend under `src/ruzu/src/applets`.


## 2026-08-20 — `src/core/src/hle/service/am/frontend/applet_software_keyboard.rs` vs Eden `src/core/hle/service/am/frontend/applet_software_keyboard.{h,cpp}`

### Intentional differences

- Eden's frontend callbacks invoke `SubmitTextNormal` and `SubmitTextInline` directly on the
  owning C++ object. Rust queues callback arguments to avoid aliasing the applet through a GUI
  callback, then resumes the owning frontend applet through its weak `Applet` reference.
- `frontend_executing` distinguishes synchronous frontend callbacks from delayed GUI callbacks;
  queued work is drained before an active call returns, while delayed work reacquires the applet

### Binary layout verification

- PASS: the foreground result remains a zero-initialized `sizeof(SwkbdResult) +
  STRING_BUFFER_SIZE` buffer, with the result followed by UTF-8 or UTF-16 text exactly as before.

## 2026-08-20 — `src/core/src/hle/kernel/k_process.rs` vs Eden `src/core/hle/kernel/k_process.{h,cpp}` termination caller selection

### Intentional differences

- Rust represents Eden's `KThread* thread_to_not_terminate` as an `Option<u64>` thread id while
  preserving the same identity comparison in `terminate_children`.
- `exit_with_current_thread` performs Eden's final `GetCurrentThread(kernel).Exit(kernel)` after
  releasing the process guard because Rust cannot re-enter the thread lifecycle while borrowing
  `KProcess` through its owning cell.

## 2026-08-20 — `src/ruzu/src/overlay_dialog.rs` and `src/ruzu/src/main_window.rs` vs Eden `src/yuzu/util/overlay_dialog.{h,cpp,ui}` and `src/yuzu/main_window.{h,cpp}`

### Intentional differences

- The GTK shutdown-only counterpart is an undecorated transient window sized to Eden's visible
  780-by-300 regular-text panel proportions. Eden uses a parent-sized translucent Qt dialog whose
  internal grid draws that panel; a GTK top-level is required to remain above ruzu's native render
  child window.
- The GTK module implements only the non-interactive regular-text configuration used by
  `OnShutdownBeginDialog`; controller navigation and rich text belong to Eden's other overlay uses.

### Unintentional differences (to fix)

- None in the Stop/Restart lifecycle: the panel is created only after a successful asynchronous
  stop request and is closed when `StopComplete` reaches `on_emulation_stopped`.

### Missing items

- Generic interactive and rich-text overlay modes are outside this shutdown-dialog slice.

### Binary layout verification

- N/A: the overlay contains host UI state only.

## 2026-08-20 — `src/ruzu/src/game_list.rs` and `src/ruzu/src/main_window.rs` vs Eden `src/yuzu/game/game_list.{h,cpp}` and `src/yuzu/main_window.{h,cpp}` shortcut dispatch

### Intentional differences

- A Rust callback replaces Eden's Qt `GameList::CreateShortcut` signal while retaining the same
  `(program_id, game_path, target)` payload and `GMainWindow` ownership of argument construction.
- GTK `gio::SimpleAction` objects replace the two `QAction` objects. Both remain hidden on macOS,
  matching Eden's compile-time guard.

### Unintentional differences (to fix)

- None. Both context-menu actions now reach `on_game_list_create_shortcut`; the former
  unavailable-action placeholders were removed.

### Missing items

- None for per-game shortcut dispatch.

### Binary layout verification

- N/A: this is host frontend dispatch.

## 2026-08-20 — `src/ruzu/src/util/game.rs` vs Eden `src/qt_common/util/game.{h,cpp}` shortcut creation

### Intentional differences

- GTK message dialogs replace `QtCommon::Frontend` dialogs, and GLib's XDG directory resolvers
  replace `QStandardPaths` on Linux.
- Linux icons and comments use the ruzu name (`ruzu-*.png`, `Ruzu Emulator`) instead of Eden's
  branding while preserving Eden's icon directory and title-id naming scheme.
- Windows creates the equivalent `.lnk` through the installed PowerShell `WScript.Shell` COM
  bridge and standard user-profile paths rather than directly owning `IShellLinkW`; this avoids a
  second Windows COM binding while preserving target, arguments, description and icon fields.

### Unintentional differences (to fix)

- None in the Linux shortcut slice. Target validation, patched control metadata precedence,
  loader fallbacks, illegal-character removal, icon creation, one-time AppImage warning,
  fullscreen argument ordering and result messages follow Eden's order.

### Missing items

- `CreateHomeMenuShortcut` and the unrelated content-removal helpers from `qt_common/util/game.cpp`
  are outside this per-game shortcut slice.
- Eden's multi-resolution Windows ICO encoder is not yet ported; Windows currently stores the
  decoded icon as PNG before assigning it to the `.lnk`.

### Binary layout verification

- N/A on Linux. The `.desktop` field order and optional-field rules are covered by a focused test.

## 2026-08-20 — `src/ruzu/src/game_list.rs` vs Eden `src/yuzu/game/game_list.cpp` context-menu submenu presentation

### Intentional differences

- GTK `PopoverMenuFlags::NESTED` supplies the traditional child-popover behavior provided by
  Eden's `QMenu`; the toolkit-specific construction differs while retaining hover, click and
  keyboard access to each submenu.

### Unintentional differences (to fix)

- None. `Remove`, `Dump RomFS`, and `Create Shortcut` no longer use GTK's click-only sliding-page
  presentation and now open as nested menus on pointer hover like Eden.

### Missing items

- None for game-list submenu presentation.

### Binary layout verification

- N/A: this is host UI behavior only.

## 2026-08-20 — `src/ruzu/src/overlay_dialog.rs` vs Eden `src/yuzu/util/overlay_dialog.cpp` and `src/yuzu/main_window.cpp` shutdown-dialog destruction

### Intentional differences

- GTK exposes window-manager closure and programmatic `Window::close` through the same
  `close-request` signal. Ruzu retains the signal id so it can remove the user-close guard before
  performing Eden's `OnEmulationStopped`-owned destruction.

### Unintentional differences (to fix)

- None. The initial port incorrectly returned `Stop` for the programmatic close request too, which
  left `Closing software...` visible after `StopComplete`; the guard is now disconnected first.

### Missing items

- None for shutdown-dialog destruction.

### Binary layout verification

- N/A: this is host UI lifecycle state only.

## 2026-08-20 — `src/ruzu/src/main_window.rs` and `src/ruzu/src/game_list.rs` vs Eden `src/yuzu/main_window.{h,cpp}` and `src/qt_common/game_list/model.{h,cpp}` refresh ownership

### Intentional differences

- Per explicit project UI direction, Ruzu keeps Refresh beside Add Game Directory in the upper
  game-list toolbar instead of Eden's bottom status bar. The widget forwards its action to
  `GMainWindow::OnGameListRefresh`, and its handle is disabled and enabled across the same
  emulation lifecycle as Eden's button.
- Ruzu's game-directory worker clears and rebuilds the frontend manual content provider in the
  same scan that rebuilds the visible rows. `refresh_external_content` therefore records that the
  already-started directory refresh covers external content instead of starting a second racing
  Rust worker; Eden can safely run two sequential `Repopulate()` calls because destroying its
  current worker waits for completion.

### Unintentional differences (to fix)

- None for the manual refresh behavior. The upper-toolbar button clears cached metadata before
  scanning, refreshes the directory/provider data, and is disabled from boot until emulation
  stops.

### Missing items

- Eden's independent filesystem watchers for `Settings::values.external_content_dirs` are not
  present in Ruzu; configured game directories are refreshed explicitly by this button.
- `SetFirmwareVersion()` has no Ruzu status-label counterpart to update after refresh.

### Binary layout verification

- N/A: this is host frontend state and worker dispatch.

## 2026-08-20 — `src/ruzu/src/util/game.rs` and `src/ruzu/src/uisettings.rs` vs Eden `src/qt_common/util/game.{h,cpp}` and `src/yuzu/uisettings.h` metadata reset

### Intentional differences

- Rust reports recursive-removal errors through `std::io::Error` and GTK message dialogs; Eden
  uses `Common::FS::RemoveDirRecursively` and `QtCommon::Frontend` dialogs.
- The reload-pending flag is a module-level `AtomicBool` next to the frontend settings because
  Ruzu's cloneable `UISettings::Values` cannot directly contain an atomic member.

### Unintentional differences (to fix)

- None. `ResetMetadata` now removes the complete Ruzu `cache/game_list` directory, including the
  stale `<title-id>.pv.txt` Add-ons cache, and marks the game-list reload pending after success.

### Missing items

- None for metadata-cache removal and reload-pending signaling.

### Binary layout verification

- N/A: cache entries are host files; the focused test verifies complete directory removal.

## 2026-08-20 — `src/ruzu/src/configuration/configure_filesystem.rs` vs Eden `src/yuzu/configuration/configure_filesystem.{h,cpp}` metadata-reset action

### Intentional differences

- The GTK button resolves its transient parent from the live widget root before calling the shared
  utility; Eden passes its `ConfigureFilesystem` widget through the global frontend dialog owner.

### Unintentional differences (to fix)

- None. The button now calls the shared metadata reset instead of logging an unavailable-action
  placeholder, and the main-window apply callback consumes the resulting reload-pending flag.

### Missing items

- None for `ConfigureFilesystem::ResetMetadata`.

### Binary layout verification

- N/A: this is host UI dispatch.

## 2026-08-20 — `src/hid_core/src/resources/ring_lifo.rs` vs Eden `src/hid_core/resources/ring_lifo.h`

### Intentional differences

- Rust uses the `LifoState` trait to express the C++ template requirement that every state expose
  `sampling_number`; this avoids an untyped raw-layout cast and does not change LIFO ownership.
- Rust bounds a corrupt `buffer_tail` to the backing array instead of reproducing C++ undefined
  behavior; the existing diagnostic remains available through `RUZU_TRACE_LIFO_CORRUPTION`.

### Unintentional differences (to fix)

- None. `write_next_entry` now publishes `new_state.sampling_number << 1` exactly like Eden. The
  previous `previous_atomic_marker + 1` calculation could publish an odd marker, which newer
  Nintendo SDK readers treat as an in-progress write and retry indefinitely.

### Missing items

- None for `AtomicStorage` and `Lifo` behavior.

### Binary layout verification

- PASS: `AtomicStorage` and `Lifo` remain `repr(C)` with unchanged fields; the full HID shared
  memory layout test passes, and focused tests verify the even marker and source sample contract.

## 2026-08-20 — `src/hid_core/src/resources/shared_memory_format.rs` vs Eden `src/hid_core/resources/shared_memory_format.h`

### Intentional differences

- The concrete shared-memory state types implement Rust's `LifoState` trait at their LIFO
  instantiation owner; Eden's C++ template accesses the same `sampling_number` members directly.

### Unintentional differences (to fix)

- None introduced by the atomic-publication correction.

### Missing items

- None for the LIFO state sampling accessors.

### Binary layout verification

- PASS: trait implementations add no fields or vtables to the state values, and
  `shared_memory_layout_matches_upstream` passes.

## 2026-08-20 — `src/hid_core/src/resources/six_axis/seven_six_axis.rs` vs Eden `src/hid_core/resources/six_axis/seven_six_axis.{h,cpp}`

### Intentional differences

- `SevenSixAxisState` converts its unsigned sampling number to `i64` for the common Rust
  `LifoState` interface; `as` preserves the underlying two's-complement bit pattern.

### Unintentional differences (to fix)

- None introduced by the LIFO marker correction.

### Missing items

- The pre-existing incomplete `SevenSixAxis::on_update` integration remains outside this fix.

### Binary layout verification

- PASS: the state remains `repr(C)` and its existing `0x48` size assertion is unchanged.

## 2026-08-20 — `src/hid_core/src/resources/npad/npad.rs` vs Eden `src/hid_core/resources/npad/npad.{h,cpp}` prefill regression

### Intentional differences

- Rust regression tests observe the shared-memory result directly after activation; Eden has no
  matching C++ unit test in the ported source tree.

### Unintentional differences (to fix)

- None. The prefill expectation now reflects Eden's exact recurrence: each empty state derives
  from the preceding atomic marker and the marker is twice the state sample.

### Missing items

- None for `NPad::WriteEmptyEntry` in this verification slice.

### Binary layout verification

- PASS: no Npad production struct changed; the full HID layout test and all Npad tests pass.

## 2026-08-20 — `src/core/src/hle/service/aoc/addon_content_manager.rs` vs Eden `src/core/hle/service/aoc/addon_content_manager.{h,cpp}`

### Intentional differences

- Rust serializes the returned `u32` add-on IDs explicitly with `to_le_bytes`; Eden copies the
  native little-endian `u32` vector into the HIPC map-alias output buffer with `std::memcpy`.
- The Rust service obtains `ClientProcessId` from `HLERequestContext::get_pid`; Eden's CMIF
  serializer supplies the same request PID through its typed `ClientProcessId` argument.

### Unintentional differences (to fix)

- The pre-existing Rust constructor still initializes `add_on_content` as an empty vector instead
  of calling Eden's `AccumulateAOCTitleIDs` over the content provider. Restoring that requires the
  content-provider enumeration integration and is separate from the missing command dispatch that
  produced the invalid CMIF response.
- The pre-existing `GetAddOnContentBaseId` implementation always takes Eden's no-control-metadata
  fallback because the required `PatchManager` integration is not wired at the system level.

### Missing items

- None for command 3 dispatch: `ListAddOnContent` now parses `offset` and `count`, forwards the
  client PID, writes the returned IDs to output buffer 0, and returns the output count.

### Binary layout verification

- PASS: add-on IDs are emitted as packed four-byte little-endian values, matching Eden's raw
  `u32` buffer copy; no shared structs changed.

## 2026-08-20 — `src/shader_recompiler/src/frontend/control_flow.rs` vs Eden `src/shader_recompiler/frontend/maxwell/control_flow.{h,cpp}`

### Intentional differences

- Rust represents upstream `Shader::Exception` subclasses as typed panic payloads at the CFG
  boundary. The Vulkan and OpenGL pipeline-cache owners catch those exact payload types at the
  same `catch (const Shader::Exception&)` boundaries used by Eden.
- Rust stores CFG links as stable vector indices instead of pointers allocated by `ObjectPool`;
  method ownership and branch/link ordering remain in the matching control-flow module.
- `to_cfg_blocks` converts the upstream-shaped flow graph into the existing Rust translation
  consumer's index-based `CfgBlock` representation. The older slice-based `build_cfg` entry point
  remains for callers that already own decoded instruction words.

### Unintentional differences (to fix)

- None in the corrected exception/lifecycle slice. `PRET`, constant-buffer branches, unsupported
  indirect branches, invalid stack pops, invalid split addresses, and unsupported `EXIT` forms now
  raise the same shader exception categories as Eden instead of killing the GPU worker with an
  untyped panic or silently continuing.

### Missing items

- `PRET` flow analysis itself remains unimplemented, matching Eden. The pipeline cache now rejects
  that shader without terminating the GPU thread.

### Binary layout verification

- N/A: CFG nodes are host-only analysis structures and are not copied to guest or GPU memory.

## 2026-08-20 — `src/common/src/scm_rev.rs` and `src/common/build.rs` vs Eden `src/common/scm_rev.{h,cpp.in}` and `CMakeModules/GenerateSCMRev.cmake`

### Intentional differences

- Cargo runs a Rust build script instead of CMake `configure_file`; both publish the full revision,
  branch, ten-character revision-plus-branch build version, build name, and detected native C++
  compiler identity as build-time constants.
- Source archives without Git metadata fall back to `unknown-detached`; CI/package builds can
  provide `GIT_REV` and `GIT_BRANCH` explicitly. Eden obtains equivalent overrides through its
  CMake SCM module.
- Ruzu currently exposes only the SCM/compiler constants consumed by its frontend. Eden's update
  feed, nightly-build, build-date, and custom title-format constants remain outside this slice.

### Unintentional differences (to fix)

- None in the development-build identity slice. The generated values on this host are
  `08b3fb5169-main` and `GNU 13.3.0`; the compiler string is detected, not hard-coded.

### Missing items

- Stable/nightly release tag formatting and auto-update endpoint constants are not used by Ruzu.

### Binary layout verification

- N/A: Rust string constants replace generated C++ character arrays and are not guest-visible.

## 2026-08-20 — `src/ruzu/src/boot.rs` vs Eden `src/yuzu/main_window.cpp` `MainWindow::BootGame`

### Intentional differences

- The boot thread sends a typed `TitleChanged` event to GTK's main thread because GTK widgets may
  only be changed by their owning thread; Eden computes the same values on its Qt GUI thread.

### Unintentional differences (to fix)

- None in the running-title metadata slice. Ruzu reads the loader title, lets
  `PatchManager::GetControlMetadata` replace it with the selected add-on NACP title/version,
  applies Eden's filename fallback and translated 64/32-bit suffix, obtains the renderer vendor,
  logs the boot identity, and publishes it before disk-cache construction.

### Missing items

- None for the default running-title fields.

### Binary layout verification

- N/A: title metadata is host UI text.

## 2026-08-20 — `src/ruzu/src/main_window.rs` vs Eden `src/yuzu/main_window.{h,cpp}` `UpdateWindowTitle`

### Intentional differences

- Ruzu formats the default title directly instead of supporting Eden's optional
  `TITLE_BAR_FORMAT_IDLE` override, which has no Ruzu configuration owner.
- The same handler exists in each platform-specific GTK launch loop because those loops own their
  native render surfaces; all three consume the identical `TitleChanged` event.

### Unintentional differences (to fix)

- None. Idle, versioned-running, versionless-running, and shutdown-reset title ordering matches
  Eden: `Ruzu | build-version | compiler | game | optional-version | GPU vendor`.

### Missing items

- User-defined idle title-bar format overrides are not ported.

### Binary layout verification

- N/A: window titles are host UI strings.

## 2026-08-20 — `src/ruzu/src/game_list.rs` vs Eden `src/qt_common/game_list/worker.cpp` and `src/core/file_sys/program_metadata.{h,cpp}`

### Intentional differences

- Ruzu adds a frontend-only Architecture column immediately after File type; Eden has no matching
  column. Application architecture comes from the selected/patched ExeFS `main.npdm` bit, KIP
  architecture comes from its header, and standalone NRO/NSO uses Eden's 64-bit default program
  metadata.
- Architecture is cached independently as `<title-id>.arch.txt`. This leaves Eden's
  `<title-id>.pv.txt` add-on cache byte-compatible and lets warm scans read only the small cached
  label. A manual refresh removes the complete cache directory, including both files.
- The frontend renders the architecture names as lowercase `aarch64`/`aarch32`; cached labels
  written by earlier Ruzu builds are normalized while loading, without changing the cache format.

### Unintentional differences (to fix)

- None in the `pv.txt` format: enabled/disabled names, version parentheses, packed-update file
  type substitution, update filtering, UTF-8 encoding, and newline joining match Eden.

### Missing items

- Eden has no architecture-column behavior to port. Files whose executable metadata cannot be
  recovered display `Unknown`.

### Binary layout verification

- PASS: `ProgramMetadata::is_64_bit_program` reads the existing NPDM bit; no guest or container
  binary structure was changed.

## 2026-08-21 — `src/shader_recompiler/src/backend/spirv/emit_spirv_special.rs` vs Eden `src/shader_recompiler/backend/spirv/emit_spirv_special.cpp`

### Intentional differences

- Ruzu uses `rspirv::dr::Builder` result IDs and Rust `match` expressions in place of Sirit's
  `EmitContext` helpers and the C++ `switch`; the emitted ordered floating-point comparisons,
  selection merge, conditional branch, and `OpKill` ordering are the same.
- Ruzu checks host-side SPIR-V IDs against zero and treats a missing position output as a no-op;
  Eden uses `Sirit::ValidId` for fragment colors and assumes its declared vertex outputs are valid.
- Ruzu derives the clip-distance-written mask once from `Program::info.stores` and keeps it in the
  per-compilation SPIR-V context; Eden uses a header-level `std::bitset<8>`. The emitted prologue
  still initializes exactly the clip-distance components not written by the shader, while the Rust
  ownership prevents state leaking between concurrent shader compilations.
- Unsupported geometry streams panic in Rust where Eden throws `NotImplementedException`.

### Unintentional differences (to fix)

- None in the reviewed prologue/epilogue slice: dual-source fragment outputs, component-aware
  generic varyings, unwritten clip distances, and the fragment alpha test follow Eden's ordering.

### Missing items

- None in the reviewed prologue/epilogue slice.

### Binary layout verification

- N/A: this change emits SPIR-V instructions and does not alter a serialized host structure.

## 2026-08-21 — `src/shader_recompiler/src/runtime_info.rs` vs Eden `src/shader_recompiler/runtime_info.h`

### Intentional differences

- Rust stores active transform-feedback entries in a `Vec`; Eden uses a fixed 256-entry array.
  `xfb_count` remains the authoritative bound in both implementations.

### Unintentional differences (to fix)

- None in the reviewed runtime-state slice: `TransformFeedbackVarying::stream` and
  `RuntimeInfo::dual_source_blend` now have the same owners and defaults as Eden.

### Missing items

- None in the reviewed runtime-state slice.

### Binary layout verification

- N/A: `RuntimeInfo` is host-side compiler state and is not copied as a guest binary payload.

## 2026-08-21 — `src/video_core/src/transform_feedback.rs` vs Eden `src/video_core/transform_feedback.{h,cpp}`

### Intentional differences

- Invalid attribute indices are ignored safely in Rust; Eden indexes its fixed array directly.

### Unintentional differences (to fix)

- None: generated varyings preserve `layout.stream`, and the complete Eden vector table through
  `gl_TexCoord[7]` is present.

### Missing items

- None in `MakeTransformFeedbackVaryings`.

### Binary layout verification

- PASS: `TransformFeedbackLayout` remains `repr(C)` with Eden's `stream`, `varying_count`, and
  `stride` field order; generated varying descriptors are host-side values.

## 2026-08-21 — `src/shader_recompiler/src/backend/spirv/spirv_emit_context.rs` vs Eden `src/shader_recompiler/backend/spirv/spirv_emit_context.{h,cpp}`

### Intentional differences

- SPIR-V construction uses `rspirv::dr::Builder` instead of Sirit.

### Unintentional differences (to fix)

- None in `DefineGenericOutput`: split component outputs and nonzero geometry transform-feedback
  stream decorations now match Eden.

### Missing items

- None in the reviewed generic-output slice.

### Binary layout verification

- N/A: this slice emits SPIR-V declarations and decorations.

## 2026-08-21 — renderer runtime-info propagation

Compared `src/video_core/src/renderer_vulkan/graphics_pipeline.rs` with Eden
`src/video_core/renderer_vulkan/vk_pipeline_cache.cpp`, and
`src/video_core/src/renderer_opengl/gl_shader_cache.rs` with Eden
`src/video_core/renderer_opengl/gl_shader_cache.cpp`.

### Intentional differences

- Rust maps the fixed pipeline key into owned `RuntimeInfo` values; Eden copies into fixed arrays.

### Unintentional differences (to fix)

- None in the reviewed fields: Vulkan propagates `attachment0_dual_source_blend`, and both Vulkan
  and OpenGL propagate transform-feedback `stream`.

### Missing items

- None in the reviewed runtime-info propagation slice.

### Binary layout verification

- N/A: these are host-side compiler inputs.

## 2026-08-21 — `src/shader_recompiler/src/pipeline_cache.rs` runtime identity vs Eden runtime shader state

### Intentional differences

- Ruzu hashes runtime compiler inputs for its Rust pipeline cache; Eden keys the equivalent state
  through its fixed pipeline cache key.

### Unintentional differences (to fix)

- None: `dual_source_blend` and transform-feedback `stream` now participate in Ruzu's runtime hash.

### Missing items

- None in the reviewed runtime-hash slice.

### Binary layout verification

- N/A: the value is a host-side cache identity hash.

## 2026-08-21 — `src/shader_recompiler/src/frontend/translate/load_store_attribute.rs` vs Eden `src/shader_recompiler/frontend/maxwell/translate/impl/load_store_attribute.cpp`

### Intentional differences

- Rust decodes instruction bit fields into integers and represents Eden's translation exceptions
  as panics.
- The Rust visitor stores the program header in an `Option`; generic `IPA` now requires it to be
  present, matching Eden's unconditional `env.SPH()` access.

### Unintentional differences (to fix)

- None in `IPA`: legacy interpolation, whole-vector effective `PixelImap` selection, the
  perspective fallback for an unused vector, `Sc` handling, multiplier ordering, and the
  saturated `FrontFace` rejection now match Eden.

### Missing items

- None in the reviewed `IPA` slice.

### Binary layout verification

- N/A: the instruction is decoded from the same bit positions, but no host struct is copied as a
  guest payload.

## 2026-08-21 — `src/shader_recompiler/src/ir/value.rs` vs Eden `src/shader_recompiler/frontend/ir/attribute.h`

### Intentional differences

- The active Rust IR represents an attribute as a checked numeric newtype instead of a C++ enum;
  the numeric values and range predicates remain upstream-owned contracts.

### Unintentional differences (to fix)

- The crate still contains a second, enum-based `Attribute` in `ir/attribute.rs`. Consolidating
  those pre-existing parallel IR representations is a structural refactor outside this runtime
  correction; `IsLegacyAttribute` was added to the active translation type so current users share
  one predicate.

### Missing items

- None in the reviewed generic/legacy classification slice.

### Binary layout verification

- N/A: attributes are host-side IR identifiers and are not raw-copied guest payloads.

## 2026-08-21 — `src/shader_recompiler/src/frontend/translate_program.rs` vs Eden `src/shader_recompiler/frontend/maxwell/translate_program.cpp`

### Intentional differences

- Rust invokes the active attribute newtype's `is_legacy` method; Eden imports
  `IR::IsLegacyAttribute` from `attribute.h`.

### Unintentional differences (to fix)

- None in the reviewed legacy-varying classification call sites; the duplicate private predicate
  was removed.

### Missing items

- None in the reviewed call-site slice.

### Binary layout verification

- N/A: this pass rewrites host-side IR instructions.
## 2026-08-20 — `src/core/src/hle/service/filesystem/filesystem.rs` vs Eden `src/core/hle/service/filesystem/filesystem.{h,cpp}`

### Intentional differences

- Ruzu adds an optional frontend-owned `sdmc_open_override`. `OpenSDMC` returns it when installed,
  while every content-cache, modification-root, size, and normal launch path remains owned by the
  upstream-equivalent `SDMCFactory`.
- `set_sdmc_open_override` is a narrow Ruzu extension used only for standalone NRO launches. An
  overwriting `create_factories` call clears it together with the upstream factories so a view
  cannot leak into a later launch.

### Unintentional differences (to fix)

- None in this slice. With no override installed, `open_sdmc` retains Eden's factory/null-device
  behavior.

### Missing items

- None for the per-launch SDMC override.

### Binary layout verification

- N/A: the added host-side `VirtualDir` does not alter a guest-visible or serialized structure.

## 2026-08-20 — `src/ruzu/src/homebrew_vfs.rs` vs Eden `src/core/file_sys/vfs/vfs_layered.{h,cpp}`

### Intentional differences

- Eden's `LayeredVfsDirectory` is read-only, so it remains unchanged. Ruzu's GTK frontend owns a
  separate writable two-layer view for homebrew: the standalone NRO's containing directory has
  first priority and the configured SDMC is the fallback.
- Creates and missing parent directories are routed to the homebrew layer. Existing fallback-only
  entries retain normal SDMC behavior. Directory enumeration recursively merges both layers and
  hides lower-priority entries shadowed by either a file or directory in the homebrew layer.
- Entry enumeration uses ordered Rust maps/sets for deterministic results; Eden's layered VFS uses
  an unordered set. The guest-visible set and priority are unchanged.
- Activation checks the NRO header through `AppLoaderNro::IdentifyType`, rather than trusting the
  filename extension. No symbolic-link, junction-point, or platform-specific filesystem API is
  required.

### Unintentional differences (to fix)

- None. The writable semantics are deliberately frontend-specific because changing Eden's
  read-only layered VFS would violate its contract.

### Missing items

- None for exposing sibling homebrew assets and writable nested paths.

### Binary layout verification

- N/A: the view contains host VFS handles and path components only.

## 2026-08-20 — `src/ruzu/src/boot.rs` and `src/ruzu/src/main.rs` vs Eden `src/yuzu/main_window.cpp` `MainWindow::BootGame` and `src/yuzu/main.cpp`

### Intentional differences

- After the upstream-equivalent filesystem factories are created and before `System::Load`, Ruzu
  detects a standalone NRO and installs its per-launch homebrew SDMC view. Eden has no equivalent
  boot hook and relies on files already being present in its configured SDMC.
- The GTK entry point declares `homebrew_vfs` as a private frontend module; Eden's excluded Qt
  frontend has no corresponding source file.

### Unintentional differences (to fix)

- None. Non-NRO boot ordering and filesystem behavior are unchanged.

### Missing items

- None for this boot integration.

### Binary layout verification

- N/A: this changes host-side launch wiring only.
## 2026-08-21 — workspace source layout vs Eden repository source layout

### Intentional differences

- Rust keeps each crate's conventional inner `src/` directory, so Eden's
  `src/video_core/foo.cpp` maps to Ruzu's `src/video_core/src/foo.rs`.
- Cargo manifests remain inside their crates, while the root `Cargo.toml` coordinates the
  workspace.
- The GTK frontend test for the quick-start action reaches the repository-level documentation
  through `../../../docs/quickstart.md`; Eden's excluded Qt frontend has different test ownership.

### Unintentional differences (to fix)

- None: all source crates now live under the top-level `src/` directory; scripts,
  documentation, externals, tools, and agent configuration remain at the repository root.

### Missing items

- None for the workspace layout migration.

### Binary layout verification

- N/A: this is a path-only structural migration and changes no guest-visible layout.

## 2026-08-21 — `src/ruzu/src/homebrew_vfs.rs` vs Eden `src/core/file_sys/vfs/vfs_layered.{h,cpp}` and `src/core/hle/service/filesystem/filesystem.cpp`

### Intentional differences

- Ruzu's frontend-owned writable SDMC view now treats an NRO directly inside a directory named
  `switch` as a conventional SD-card archive: the directory above `switch` becomes the writable
  upper layer. This exposes asset directories shipped beside `switch` without host links or a
  manual copy into the configured SDMC. Eden has no automatic host-package mount and continues to
  open only its configured `SDMCFactory` root.
- NROs in flat or per-application layouts retain the previous containing-directory root, and the
  configured SDMC remains the fallback layer in both cases.

### Unintentional differences (to fix)

- None in the reviewed package-root selection slice.

### Missing items

- None for conventional `<package>/switch/application.nro` asset visibility.

### Binary layout verification

- N/A: the change selects a host `VirtualDir` root and does not alter serialized or guest ABI
  structures.

## 2026-08-21 — `src/video_core/src/gpu.rs` and `src/video_core/src/gpu_thread.rs` vs Eden `src/video_core/gpu.{h,cpp}` and `src/video_core/gpu_thread.{h,cpp}`

### Intentional differences

- Ruzu exposes an idempotent `ThreadManager::shutdown` helper because Rust field destruction runs
  in declaration order. `Gpu::drop` invokes it explicitly to reproduce the relevant C++ reverse
  member destruction contract: `GPU::Impl::gpu_thread` is stopped and joined while `renderer` is
  still alive. Ruzu also stops the thread before freeing its boxed scheduler; Eden's scheduler is
  stored in-place and has a trivial destructor, so its storage remains within `GPU::Impl` while
  `gpu_thread` is destroyed.

### Unintentional differences (to fix)

- None in the reviewed GPU-thread lifetime slice. Previously, Rust could destroy renderer-owned
  state before requesting GPU-thread stop, causing a shutdown join hang, `SlotVector` panic, or
  allocator corruption.

### Missing items

- None for GPU-thread shutdown ordering.

### Binary layout verification

- N/A: the change affects host-thread lifecycle only.

## 2026-08-21 — `src/core/src/core.rs` vs Eden `src/core/core.{h,cpp}` (`System::Impl::ShutdownMainProcess`)

### Intentional differences

- Eden destroys `audio_core` before `gpu_core` and `CpuManager::Shutdown`. Ruzu retains
  `audio_core` until after `finalize_terminated_processes_after_cpu_shutdown`, because Rust kernel
  sessions can keep `IAudioRenderer` alive in the terminated-process table. Its finalizer waits
  for a signal from `AudioRenderSystemManager`; destroying `audio_core` at Eden's earlier point
  stops that worker first and deadlocks shutdown.

### Unintentional differences (to fix)

- None in the reviewed shutdown slice.

### Missing items

- None for GPU shutdown and delayed Rust session finalization ordering.

### Binary layout verification

- N/A: the change affects host subsystem lifetime only.

## 2026-08-21 — `src/common/src/settings.rs` vs Eden `src/common/settings.h` (`dd12266c`)

### Intentional differences

- Rust uses `cfg!(target_os = "windows")` for the setting's persistence flag instead of Eden's
  `_WIN32` preprocessor branch. The resulting platform behavior is identical.
- `enable_raw_input` was added to Ruzu's category visitor alongside the new setting. Its existing
  Rust declaration had incorrectly disabled persistence on every platform, while Eden persists it
  on Windows through the same settings linkage used by `disable_wgi_xinput`.

### Unintentional differences (to fix)

- None in the reviewed WGI/XInput settings slice.

### Missing items

- None for the `disable_wgi_xinput` setting introduced by Eden commit `dd12266c`.

### Binary layout verification

- N/A: these are host configuration values and are not copied into a guest-visible binary payload.

## 2026-08-21 — `src/input_common/src/drivers/sdl_driver.rs` vs Eden `src/input_common/drivers/sdl_driver.cpp` (`dd12266c`)

### Intentional differences

- Rust constructs temporary `CString` values before calling the SDL3 C API; Eden passes the SDL
  hint macros directly. Both set `SDL_JOYSTICK_RAWINPUT_CORRELATE_XINPUT` and `SDL_JOYSTICK_WGI`
  to `0` with `SDL_HINT_OVERRIDE`, only on Windows and only when the setting is enabled.

### Unintentional differences (to fix)

- None in the reviewed WGI/XInput SDL hint slice.

### Missing items

- None for the SDL behavior introduced by Eden commit `dd12266c`.

### Binary layout verification

- N/A: SDL hints alter host input-backend selection and serialize no guest data.

## 2026-08-21 — `src/ruzu/src/configuration/configure_input_advanced.rs` vs Eden `src/yuzu/configuration/configure_input_advanced.{cpp,ui}` (`dd12266c`)

### Intentional differences

- The excluded Qt frontend's `QCheckBox` is represented by Ruzu's GTK `CheckButton`; the label,
  tooltip, initial setting value, apply behavior, and Windows-only visibility match Eden.

### Unintentional differences (to fix)

- None in the reviewed WGI/XInput configuration-widget slice.

### Missing items

- None for the advanced-input control introduced by Eden commit `dd12266c`.

### Binary layout verification

- N/A: this is host GUI state only.
## 2026-08-21 — `src/core/src/hle/kernel/svc/svc_synchronization.rs` vs Eden `src/core/hle/kernel/svc/svc_synchronization.cpp` (`7731b5bc`)

### Intentional differences

- None in the `ResetSignal` logging-level slice.

### Unintentional differences (to fix)

- None. `ResetSignal` now logs routine calls at trace level, matching Eden's demotion from debug.

### Missing items

- None for this upstream commit.

### Binary layout verification

- N/A: the change affects host logging only.
## 2026-08-21 — `src/core/src/hle/service/acc/acc.rs` vs Eden `src/core/hle/service/acc/acc.cpp`

### Intentional differences

- Eden commit `a41a98028a` moved `acc:aa`, `acc:su`, `acc:u0`, and `acc:u1` into `acc.cpp` and
  deleted their dedicated source/header pairs. Ruzu now mirrors that ownership: the corresponding
  Rust implementations live in `acc.rs`, while `acc_aa.rs`, `acc_su.rs`, `acc_u0.rs`, and
  `acc_u1.rs` and their declarations in `acc/mod.rs` are removed.
- Rust uses a local macro only for the repeated, data-only service-framework plumbing. Each
  service name and command table remains declared in `acc.rs` beside its Eden counterpart.
- `Arc<Mutex<_>>`, `ResultCode`, and Rust enums replace C++ shared pointers, exceptions/results,
  and enum classes without changing service ownership or command behavior.

### Unintentional differences (to fix)

- None in the reviewed `a41a98028a` ACC consolidation and new `acc:e`, `acc:e:u1`, `acc:e:u2`, and
  `dauth:0` service-table slice.

### Missing items

- None introduced by this port. Pre-existing unimplemented ACC commands remain registered as
  stubs exactly where the Rust service framework represents Eden's null handlers.

### Binary layout verification

- PASS: user IDs, pin-code lengths, IPC scalar widths, and existing raw profile payload types are
  unchanged; this slice adds no new raw-copied structure.

## 2026-08-21 — `src/core/src/hle/service/{apm,audio,bpc,caps,es,friend,glue,grc,hid,lm,mnpp,ncm,ngc,nim,ns,nvdrv,olsc,pcie,pcv,psc,ptm,ro,tma,usb,wlan}` vs Eden commit `a41a98028a` service files

### Intentional differences

- `apm/apm_controller.rs`, `apm/apm_interface.rs`, and
  `am/service/common_state_getter.rs` retain Eden's APM ownership and update ordering. Transparent
  raw wrappers preserve unknown `PerformanceMode`, `PerformanceConfiguration`, and `CpuBoostMode`
  bit patterns instead of rejecting or normalizing values during Rust conversion.
- `audio/audio.rs` and `audio/audio_renderer_manager.rs` mirror Eden's applet/debug service tables
  and invalid-process-handle behavior. The unusual upstream registration of `audren:d` through
  `IAudioInManager` is preserved literally.
- `bpc/bpc.rs`, `caps/caps.rs`, `caps/caps_manager.rs`, `es/es.rs`, `friend/friend.rs`,
  `friend/friend_interface.rs`, `glue/ectx.rs`, `glue/glue.rs`, `grc/grc.rs`, `hid/hid.rs`,
  `hid/hid_system_server.rs`, `lm/lm.rs`, `ncm/ncm.rs`, `ngc/ngc.rs`, `nim/nim.rs`, `ns/ns.rs`,
  `ns/query_service.rs`, `nvdrv/mod.rs`, `nvdrv/nvdrv.rs`, `olsc/olsc.rs`, `pcie/pcie.rs`,
  `pcv/pcv.rs`, `psc/psc.rs`, `psc/time/service_manager.rs`, `ptm/ptm.rs`, `ro/ro.rs`, and
  `usb/usb.rs` keep the new service names, command IDs, command labels, and registration order from
  their same-named Eden `.cpp` owners.
- Eden's `mnpp_app` rename/split is mirrored by deleting `mnpp/mnpp_app.rs`, adding
  `mnpp/mnpp.rs`, and updating `mnpp/mod.rs`. The new Eden-owned modules are mirrored by
  `tma/mod.rs`, `tma/tma.rs`, `wlan/mod.rs`, and `wlan/wlan.rs`; `hle/service/mod.rs` only declares
  those modules.
- Firmware-gated service registration reads the installed firmware through Ruzu's existing
  `set::system_settings_server::get_firmware_version_impl` owner. Eden obtains the same major
  version through `FrontendCommon::FirmwareManager`, which is unavailable in the excluded Qt
  frontend and would violate crate ownership if copied into these service modules.
- `services.rs` preserves Eden's service-thread ownership while adapting `unique_ptr` and thread
  launch to Rust's existing server-manager lifecycle.

### Unintentional differences (to fix)

- None in the reviewed command-table and registration slice. Eden's changes to `mii.cpp` and
  `glue/notif.cpp` are formatting-only, and its `spl.cpp` change only relocates explicit default
  destructor definitions; Rust requires no corresponding behavioral change.

### Missing items

- None introduced by this port. Commands represented by null handlers upstream remain named Rust
  stubs and deliberately return the service framework's unimplemented result.

### Binary layout verification

- PASS: the port adds service dispatch tables and scalar IPC replies only. Existing `repr(C)`
  payload declarations are unchanged, and empty CAPS/PSC responses return Eden's error before any
  payload serialization.

## 2026-08-21 — `src/core/src/hle/service/hle_ipc.rs` and `src/core/src/hle/service/sockets/{bsd,sfdnsres,sockets}.rs` vs Eden `src/core/hle/service/hle_ipc.h` and `src/core/hle/service/sockets/*.{h,cpp}`

### Intentional differences

- `hle_ipc.rs` represents Eden's missing copy/move handle as numeric handle `0`; checked slice
  access replaces C++ pointer/index access and prevents the same out-of-range crash.
- `sockets/bsd.rs` retains Eden's `Bsd` ownership for `is_user`, `SocketExempt`, and BSD command
  dispatch. Rust wrapper service types for `bsd:nu` and the additional socket services delegate to
  the same owner rather than duplicating BSD state.
- `sockets/sfdnsres.rs` and `sockets/sockets.rs` preserve Eden's service names, command IDs, and
  user/system split. Rust `Arc<Mutex<_>>` replaces C++ shared ownership for the shared network
  interface.

### Unintentional differences (to fix)

- None in the reviewed `a41a98028a` handle-safety and socket-service slice.

### Missing items

- None introduced by this port; null upstream socket handlers remain explicit stubs.

### Binary layout verification

- PASS: BSD request/reply integer widths and handle values are unchanged; no socket ABI structure
  was added or reordered.

## 2026-08-21 — `src/hid_core/src/resources/npad/{npad,npad_resource}.rs` vs Eden `src/hid_core/resources/npad/{npad,npad_resource}.cpp`

### Intentional differences

- `NPadResource::get_index_from_aruid` returns `Option<usize>` instead of Eden's sentinel
  `AruidIndexMax`. Invalid unregister requests now return before clearing state, preserving Eden's
  new guard exactly.
- `NPad::activate` returns success after logging an invalid ARUID because the following upstream
  null-data check also returns before the fallback index is consumed. `NPad::unregister` uses
  index zero only for the temporary controller cleanup, then calls the guarded resource owner,
  matching Eden's fallback and lifecycle ordering.

### Unintentional differences (to fix)

- None in the reviewed NPad ARUID guard slice.

### Missing items

- Eden also adds a null `shared_memory_format` guard to
  `abstracted_pad/abstract_battery_handler.cpp`. Ruzu's pre-existing abstract battery handler does
  not yet own or dereference an applet resource at all, so that crash path is already absent and
  this commit requires no executable Rust change there. Full abstract-battery integration remains
  pre-existing parity work, not a shortcut added by this port.

### Binary layout verification

- PASS: controller state and shared-memory payload declarations are unchanged. The added regression
  test verifies that unregistering an unknown ARUID cannot clear the first registered resource.

## 2026-08-21 — corrective audit of Eden `a41a98028a` homebrew-service prerequisites

### Intentional differences

- `src/core/src/file_sys/registered_cache.rs` stores Eden's
  `ContentProviderParsingFunction` and `VfsCopyFunction` as Rust `Fn` trait objects. The install
  callback accepts non-`'static` captures, preserving the flexibility of upstream `std::function`.
- The pre-existing Rust cache indexes remain `BTreeMap`s instead of Eden's
  `ankerl::unordered_dense::map`s. This makes enumeration deterministic but does not alter lookup,
  filtering or ownership; changing the container is outside this corrective method slice.
- `RegisteredCache::install_entry_xci` returns `ErrorMetaFailed` if an XCI has no secure-partition
  NSP. Eden dereferences the returned pointer without a null check; the valid-XCI path and install
  ordering are otherwise identical.
- `src/core/src/hle/service/filesystem/filesystem.rs` retains Ruzu's separate frontend-owned
  `FrontendManual` provider for content discovered inside ordinary game directories, alongside
  the newly ported engine-owned `ExternalContentProvider` for explicitly configured external
  update/DLC directories.
- `FileSystemController` stores Ruzu's concrete `Arc<RealVfsFilesystem>` rather than Eden's
  abstract VFS reference. BIS partition-storage behavior and result ordering remain the same.
- `src/core/src/core.rs::get_game_file_from_path` uses Rust host-path detection for extracted
  directories and the existing Rust VFS concatenation owner. It otherwise preserves Eden's
  `00` through `0F` scan order, early stop, directory name and `/main` fallback.
- If game-file opening fails, Rust logs the failure and does not call `set_game_card`; Eden passes
  the null VFS handle into `SetGameCard`. Valid current-game and configured-image paths preserve
  Eden's branch ordering, including applying the empty-path check only to the configured path.

### Unintentional differences (to fix)

- None in the re-reviewed NCM command/handler slice, registered-cache install/iteration/rights-ID
  slice, SDMC parsing path, filesystem-controller wrappers, or game-file opening path.
- This entry supersedes the broader “none” claim in the earlier `a41a98028a` service entry:
  subsequent line-by-line review found and fixed missing NCM prerequisites, NAX parsing,
  metadata filtering, registered installation and game-card setup.

### Missing items

- None among the 31 reviewed `PlaceholderCache`/`RegisteredCache` methods: `GetRightsID`, all four
  `InstallEntry` overloads and `IterateAllMetadata` are now present.
- None among the eight reviewed NCM interfaces: the same 12 commands have concrete handlers in
  Eden and Ruzu, and all remaining commands are registered stubs on both sides.
- `FileSystemController::GetExternalContentProvider`, BIS partition access, the standalone
  save-data controller, image-directory access and placeholder wrappers are now present in their
  upstream-owned controller file.

### Binary layout verification

- PASS: `ContentMetaKey` remains `repr(C)` and 0x10 bytes; padding is ignored when matching keys,
  as upstream does.
- PASS: `CNMTHeader`, `OptionalHeader` and `ContentRecord` remain deterministically initialized
  `repr(C)` payloads of 0x20, 0x10 and 0x38 bytes respectively. The new install path serializes the
  same fields and hashes the same first 1 MiB as Eden.
- N/A: filesystem controller accessors and `GetGameFileFromPath` add no guest-visible raw payload.

## 2026-08-21 — explicit service declarations vs Eden `a41a98028a` service owners

### Intentional differences

- Rust service-framework trait boilerplate remains implemented with the existing mechanical
  `impl_service_framework!` helper. It does not declare commands, own behavior or combine upstream
  files.

### Unintentional differences (to fix)

- None in the reviewed stub-service ownership slice. The port-local `define_stub_service!` macro
  has been removed from `audio`, `nvdrv`, `usb`, `psc`, `sockets`, `ptm`, `glue/ectx`, `wlan` and
  `bpc`; each upstream service type and command table is now explicit in its corresponding Rust
  owner.

### Missing items

- None introduced by expanding these declarations. Null Eden handlers remain explicit Rust
  unimplemented handlers with the same command IDs and labels.

### Binary layout verification

- N/A: this ownership correction changes declarations only and adds no raw-copied structure.
## 2026-08-21 — `src/core/src/hle/service/nim/nim.rs` vs Eden `src/core/hle/service/nim/nim.{h,cpp}` at `5c54abf353`

### Intentional differences

- Eden's `std::jthread` plus stop token is represented by a `JoinHandle` paired with a shared
  `AtomicBool`. `cancel_impl` requests stop before joining, and `Drop` performs cancellation before
  closing the completion event, preserving the upstream lifecycle order.
- Eden stores service state directly because its IPC service object is mutable through C++ object
  ownership. Rust uses `Mutex` for the worker and response bytes and `AtomicU32` for the error code,
  allowing the same service object to satisfy `SessionRequestHandler: Send + Sync` without moving
  ownership to another module.
- `ServiceContext` owns the completion `Event`; its copy-handle bridge supplies the readable event
  returned beside the new async IPC interface. This is Ruzu's existing equivalent of Eden's
  `KEvent*`/`KReadableEvent*` ownership.
- `Prepare` logs invalid UTF-8 paths lossily because Rust logging requires text. The original bytes
  are otherwise unused, just as Eden ignores the POST buffer and does not execute a real request.
- `nim:eca`, `IShopServiceAccessServer`, and `IShopServiceAccessor` were pre-existing direct-method
  stubs on `main` but were not registered as dispatchable service frameworks. They are wired in
  their existing `nim.rs` owner so Eden's new async implementation is reachable through the same
  interface chain; unrelated `nim`, `nim:shp`, and `ntc` parity remains outside this commit.

### Unintentional differences (to fix)

- None in the reviewed async-shop slice: command IDs, cancellation/join ordering, buffer locking,
  offset clamping, error-code updates, dummy `{}` response, event clearing/signaling, and returned
  copy/move objects match Eden.

### Missing items

- `Request` remains deliberately stubbed to a two-byte JSON object, exactly as in Eden commit
  `5c54abf353`; no network download is performed.

### Binary layout verification

- PASS: IPC outputs retain Eden's `u64` size/read count and `u32` error code widths. Download data
  is copied as bytes into the caller-provided output buffer; no host struct is raw-copied.

## 2026-08-21 — `src/core/src/hle/service/acc/{profile_manager.rs,acc.rs}` vs Eden `src/core/hle/service/acc/{profile_manager.cpp,acc.cpp}`

### Intentional differences

- Eden creates the automatic first user and the `BeginUserRegistration` user with the branded
  name `Eden`; Ruzu uses the direct product-name adaptation `ruzu`. Existing saved profiles are
  parsed without renaming, so a user-selected or migrated name is never overwritten.

### Unintentional differences (to fix)

- None in the reviewed default-profile creation paths: both paths generate a random non-null UUID,
  construct a fixed-size zero-padded profile name, create the user, and preserve upstream ordering.

### Missing items

- None for default profile naming or `BeginUserRegistration` naming.

### Binary layout verification

- PASS: `ProfileUsername` remains 32 bytes. The four ASCII bytes `ruzu` are followed by 28
  deterministic zero bytes, matching the upstream fixed-size payload contract.

## 2026-08-21 — `dist` Windows packaging vs Eden `dist/{installer.nsi,yuzu.manifest,eden.ico}`

### Intentional differences

- Ruzu stages its Rust executables and the dynamic `x64-windows-ruzu` vcpkg GTK/GLib runtime;
  Eden's installer consumes an already binplaced Qt directory. `package-windows.ps1` owns that
  extra staging step because Ruzu has no CMake/binplace packaging stage.
- The application installs to `%LOCALAPPDATA%\Programs\Ruzu`, keeping executable files separate
  from Ruzu's `%APPDATA%\ruzu` user data. Uninstalling therefore removes the program directory but
  deliberately preserves keys, firmware, saves, configuration and caches.
- File types are registered through the `Ruzu.SwitchFile` OpenWith ProgID instead of taking over
  each extension's default handler. Uninstall removes only Ruzu's own registry values.
- The Ruzu manifest fixes the malformed long-path namespace in the source manifest and embeds it,
  together with the Ruzu icon, through the Rust crate's Windows resource build step.

### Unintentional differences (to fix)

- None found by static validation of the adapted installer, manifest and resource definition.

### Missing items

- The installer has not yet been executed on a native Windows host; MSVC resource compilation,
  vcpkg runtime staging, NSIS generation, install, launch and uninstall still require that test.

### Binary layout verification

- PASS: `dist/ruzu.ico` has a Windows ICO header and seven image sizes from 16 through 256 pixels.
- PASS: the XML manifest parses successfully and uses resource ID 1/type 24, the standard
  `CREATEPROCESS_MANIFEST_RESOURCE_ID` application-manifest slot.

## 2026-08-21 — `src/ruzu/src/{main.rs,main_window.rs,gui_settings.rs,uisettings.rs,configuration/qt_config.rs}` vs Eden `src/{yuzu/main.cpp,yuzu/main_window.cpp,qt_common/gui_settings.cpp,qt_common/config/uisettings.h}`

### Intentional differences

- GTK dialogs are asynchronous, so Ruzu chains migration, the missing-key question, key
  installation and the Wayland check through completion callbacks. Eden obtains the same ordering
  from blocking `QMessageBox::exec()` and its synchronous file chooser.
- `GDK_BACKEND=x11` is Ruzu's GTK equivalent of Eden's `QT_QPA_PLATFORM=xcb`. Both are set before
  constructing the toolkit application, only when the persisted preference is enabled and no
  explicit backend environment override is present.
- `gui_settings.rs` lives in the Ruzu frontend crate because Ruzu has no `qt_common` crate. It keeps
  the upstream filename (`gui_config.ini`), key (`gui_force_x11`) and method ownership together.
- The restart text substitutes the Ruzu product name for Eden. The Wayland warning text, choices,
  default X11 action and “Don't show again” behavior otherwise match upstream.

### Unintentional differences (to fix)

- None in the reviewed missing-key and Linux-backend startup slice.

### Missing items

- None for detection, warning suppression, X11 preference persistence or early backend selection.

### Binary layout verification

- N/A: these frontend settings are textual INI booleans and no raw structure is serialized.

## 2026-08-21 — `src/ruzu/src/user_data_migration.rs` vs Eden `src/yuzu/user_data_migration.{h,cpp}`

### Intentional differences

- Ruzu's migration policy remains the previously documented non-destructive, selective GTK flow.
  The first page now exposes `No migration` as a method instead of a separate `Start Fresh`
  response, clears and disables Firmware/Keys for that method, and has a single `Next` action.
- Completing `No migration` records the explicit one-time prompt marker and resumes the normal
  startup prerequisite chain, which presents Eden's missing-key question when appropriate.

### Unintentional differences (to fix)

- None in the requested first-page interaction.

### Missing items

- Per-game migration remains hidden as documented by the existing implementation.

### Binary layout verification

- N/A: the changes affect GTK state and the existing text marker only.

## 2026-08-21 — external-content settings and `FileSystemController` vs Eden

### Intentional differences

- `src/ruzu/src/configuration/configure_general.rs` uses a GTK `ListBox` and asynchronous native
  folder chooser in place of Qt's `QListWidget` and blocking `QFileDialog`; list order, duplicate
  rejection, native trailing separator and apply-time comparison are preserved.
- `RealVfsFilesystem::arc_open_directory` currently constructs a VFS directory even for a missing
  host path. `FileSystemController::create_factories` therefore performs `Path::is_dir` before the
  VFS call to reproduce Eden's null result for an invalid configured directory.
- Ruzu requests its existing game-list worker rebuild directly after applying a changed directory
  list. This is the GTK equivalent of Eden's `ExternalContentDirsChanged` signal followed by
  `OnGameListRefresh`.

### Unintentional differences (to fix)

- None in the implemented persistence, explicit refresh or engine registration path.

### Missing items

- Eden installs a `QFileSystemWatcher` on external-content roots so later host filesystem changes
  trigger a metadata reset automatically. Ruzu currently detects those changes on the toolbar
  refresh or the next game-list rebuild; it has no directory watcher yet.

### Binary layout verification

- N/A: external paths use a textual QSettings-compatible array. The provider registration adds no
  guest-visible raw structure.

## 2026-08-21 — `src/ruzu/src/util/content.rs` and firmware menu vs Eden `src/qt_common/util/content.{h,cpp}` / `src/yuzu/main_window.cpp`

### Intentional differences

- GTK file selection is asynchronous. Once a source is returned, both paths converge on the same
  synchronous copy and firmware-only integrity verification routine, preserving Eden's ordering.
- Ruzu uses the Rust `zip` crate instead of `JlCompress`; `ZipFile::enclosed_name` additionally
  rejects entries that escape the fixed `ruzu/firmware` temporary root.
- The success message reports the number of verified NCA files. Eden reports the installed
  firmware display version, whose frontend lookup still depends on Ruzu's not-yet-faithful
  installed SystemVersion reader.

### Unintentional differences (to fix)

- None in source selection, direct-versus-recursive NCA discovery, extraction cleanup, copy order
  or firmware-only integrity verification.

### Missing items

- Displaying the installed firmware version requires replacing Ruzu's hardcoded
  `get_firmware_version_impl` with Eden's SystemVersion archive lookup; that prerequisite is
  outside this frontend menu slice.

### Binary layout verification

- N/A: ZIP extraction and firmware copying operate on files, not raw-copied payload structures.

## 2026-08-21 — `UISettings::enable_gamemode` ownership vs Eden `src/qt_common/config/uisettings.h`

### Intentional differences

- None. The obsolete standalone Rust `configure_linux_tab.rs` owner was removed because current
  Eden exposes Gamemode and X11 as `UiGeneral` settings in `ConfigureGeneral`.

### Unintentional differences (to fix)

- None in ownership, platform default or row ordering: the MSVC default is false, other targets
  default true, and Gamemode follows the profile prompt.

### Missing items

- Ruzu does not yet have Eden's `qt_common/gamemode.cpp` DBus activation owner; this pre-existing
  runtime integration gap is separate from the corrected setting ownership and UI placement.

### Binary layout verification

- N/A: the value is a textual frontend boolean.

## 2026-08-21 — `src/core/src/file_sys/fssystem/compression_configuration.rs` vs Eden `src/core/file_sys/fssystem/fssystem_compression_configuration.{h,cpp}`

### Intentional differences

- Ruzu calls the safe Rust `lz4_flex::decompress_into` API in place of Eden's
  `Common::Compression::DecompressDataLZ4`; both require the decompressed byte count to equal the
  requested destination size.

### Unintentional differences (to fix)

- None. The invalid `cfg(feature = "lz4")` gate was removed: `lz4_flex` is an unconditional core
  dependency and NCA LZ4 decompression is now active in every build, matching Eden.

### Missing items

- None for the NCA decompressor selection or destination-size validation path.

### Binary layout verification

- N/A: compressed bytes are decoded into caller-owned byte slices; no Rust structure is copied as
  a guest payload.

## 2026-08-21 — `src/core/src/hle/service/ns/language.rs` and `src/core/src/hle/service/set/settings_types.rs` vs Eden `src/core/hle/service/ns/language.{h,cpp}` and `src/core/hle/service/set/settings_types.h`

### Intentional differences

- Eden's partially initialized fixed-size Thai and Polish priority arrays zero-initialize their
  remaining enum slots. Rust arrays require every element explicitly, so Ruzu spells those zero
  values as trailing `ApplicationLanguage::AmericanEnglish` entries.

### Unintentional differences (to fix)

- None. Polish and Thai enum values, language codes, conversions, priority-list selection, and
  Eden's exact aggregate-initialization result are now present.

### Missing items

- None in the reviewed language enum/conversion/priority-list slice.

### Binary layout verification

- PASS: `ApplicationLanguage` remains `repr(u8)` with Polish 16, Thai 17 and Count 18.
- PASS: `LanguageCode` remains `repr(u64)` with Eden's exact little-endian `pl` and `th` values.

## 2026-08-21 — `src/common/src/logging/backend.rs` vs Eden `src/common/logging.{h,cpp}`

### Intentional differences

- Ruzu sends entries through a background Rust channel, while current Eden writes synchronously to
  each backend. This existing threading difference is outside this dead-code cleanup slice.
- Ruzu shares the active color-console flag with the logging thread through `Arc<AtomicBool>`;
  Eden stores the equivalent atomic flag directly in `ColorConsoleBackend`.

### Unintentional differences (to fix)

- None in the reviewed state ownership. The abandoned Rust `LoggerImpl`, duplicate
  `ColorConsoleBackend`, unused stacktrace hook, and redundant `LoggerState::color_console_enabled`
  were removed. The live file backend and `LoggerState` remain the only active owners.

### Missing items

- Eden's platform-specific Windows debugger and Android logcat backends remain platform-deferred.
- Eden's `log_flush_line`, `extended_logging`, and username-censoring behavior is not part of this
  cleanup and still requires a dedicated parity pass.

### Binary layout verification

- N/A: logging state is host-only and is not serialized or exposed to guest memory.

## 2026-08-21 — removal of `src/web_service/src/telemetry_json.rs` vs current Eden `src/web_service/`

### Intentional differences

- None. This is a structural correction to the current Eden source tree rather than a Rust
  adaptation.

### Unintentional differences (to fix)

- None. Ruzu's `telemetry_json.rs` was an incomplete port from an older source tree: current Eden
  has no `telemetry_json.{h,cpp}`, Ruzu had no production caller, and both HTTP submission methods
  were explicit stubs. The module and its public export were removed.

### Missing items

- None relative to current Eden's `web_service` file list.

### Binary layout verification

- N/A: the removed component was host-only JSON state.

## 2026-08-21 — dead-code cleanup in `src/common/src/heap_tracker.rs` vs Eden `src/common/heap_tracker.{h,cpp}`

### Intentional differences

- The active Ruzu implementation currently uses two safe `BTreeMap` indexes where Eden owns two
  intrusive red-black trees over each `SeparateHeapMap`. This is an existing structural and
  performance divergence and remains explicit parity debt.

### Unintentional differences (to fix)

- None introduced by this cleanup. The removed `SeparateHeapMap`, `AddrNode`, `TickNode`,
  `HeapTrackerInner`, comparators, and partial `addr_tree` helpers formed a separate abandoned
  implementation that was never constructed or referenced by the active `HeapTracker`.

### Missing items

- A future parity slice must replace the active `BTreeMap` representation with the same dual-tree
  ownership model as Eden; retaining an unused partial tree beside it did not provide that parity.

### Binary layout verification

- N/A for the removed host-only structures. The active mapping records are not copied to guest
  memory or serialized.

## 2026-08-21 — `src/dedicated_room/src/main.rs` announcement credentials vs Eden `src/dedicated_room/yuzu_room.cpp`

### Intentional differences

- Ruzu retains the historical setting field names `yuzu_username` and `yuzu_token`; they are the
  existing Rust equivalents consumed by `AnnounceMultiplayerSession`.

### Unintentional differences (to fix)

- None in this slice. Before constructing the verification backend and announcement session, Ruzu
  now writes `web_api_url`, username, and token to global settings in the same branches and order as
  Eden. Display tokens publish the decoded token directly instead of assigning it to an otherwise
  unread local variable.

### Missing items

- None for dedicated-room announcement credential propagation.

### Binary layout verification

- N/A: credentials are host strings and are not raw guest payloads.

## 2026-08-21 — current program ID in `src/common/src/settings.rs` / `src/core/src/core.rs` vs Eden `src/common/settings.{h,cpp}` / `src/core/core.cpp`

### Intentional differences

- Ruzu stores the process-global ID in `AtomicU64` because Rust global mutable state must be
  synchronized. Eden uses a plain file-local `u64`; relaxed atomic operations preserve the same
  value semantics without adding ordering to unrelated emulator state.

### Unintentional differences (to fix)

- None. `set_current_program_id` and `get_current_program_id` now belong to `settings.rs`, and
  `System::load` publishes the loaded process ID immediately after updating its runtime ID, at the
  corresponding point in Eden's application-load flow.

### Missing items

- None for this settings prerequisite.

### Binary layout verification

- N/A: this is host-global scalar state and is not serialized or copied to guest memory.

## 2026-08-21 — macro dumping in `src/video_core/src/macro_engine/macro_engine.rs` vs Eden `src/video_core/macro.{h,cpp}`

### Intentional differences

- `dump_to_directory` isolates the mechanical file write so the filename and payload can be tested
  without mutating Ruzu's process-global dump path. It remains private in the upstream-owned macro
  module and does not change method ownership.
- Rust uses `bytemuck::cast_slice` for the same native `u32` byte representation that Eden writes
  through `reinterpret_cast<const char*>`.

### Unintentional differences (to fix)

- None. Newly compiled macros now read `CacheInfo::hash` after execution and dump when
  `dump_macros` is enabled, using Eden's exact program-ID/hash/variant filename and payload.

### Missing items

- None in the reviewed macro dump path.

### Binary layout verification

- PASS: the regression test verifies that the `.macro` payload is the contiguous native-byte view
  of the original `u32` instruction span, matching Eden's `code.size_bytes()` write.

## 2026-08-21 — `src/shader_recompiler/src/pipeline_cache.rs` vs Eden Maxwell decode/translate ownership

### Intentional differences

- None for this cleanup.

### Unintentional differences (to fix)

- None. The unused Ruzu-only `maxwell_opcode_is_unknown` wrapper was removed. Opcode decoding
  remains owned by the control-flow and translation modules that consume it, matching Eden's
  direct `Decode` use rather than making the unrelated pipeline cache an extra owner.

### Missing items

- None introduced by this removal; the broader structured-control-flow parity work remains a
  separate implementation slice.

### Binary layout verification

- N/A: no guest-visible or serialized data changed.

## 2026-08-21 — `src/input_common/src/main_common.rs` vs Eden `src/input_common/main.{h,cpp}` mapping callback ownership

### Intentional differences

- Rust's callback captures the shared `Arc<Mutex<MappingFactory>>` rather than a raw `this`
  pointer. Consequently, the private `InputSubsystemImpl` methods receive that shared factory
  explicitly; their ownership and call chain still mirror Eden's `Impl` methods.

### Unintentional differences (to fix)

- None. `mapping_callback`, `register_engine`, and `register_input` now belong to
  `InputSubsystemImpl`, and every engine callback routes through `register_input` as Eden's
  `RegisterEngine` lambda does.

### Missing items

- `GCAdapter` and Android registration remain the already documented platform-specific gaps in
  this subsystem; they are not introduced by this callback correction.

### Binary layout verification

- N/A: this changes host callback ownership only.

## 2026-08-21 — `src/hid_core/src/frontend/emulated_console.rs` vs Eden `src/hid_core/frontend/emulated_console.{h,cpp}` motion path

### Intentional differences

- Input callbacks capture `Arc<Mutex<ConsoleStatus>>`, the configuration flag, and the immutable
  sensitivity instead of Eden's raw `this` pointer. This preserves callback-thread safety without
  moving console behavior out of its upstream-owned file.
- Ruzu's input factory always returns an `InputDevice` (a null device for an unavailable backend),
  so the explicit null-device branches in Eden are represented by normal callback installation.
- `ConsoleMotion::quaternion` uses this module's existing Rust `MotionInput::Quaternion`; it carries
  the same four scalar components and is host state rather than a raw guest payload.
- The private `motion_state` helper mechanically shares Eden's identical field projection between
  reload and callback paths while remaining in the same upstream-owned module.

### Unintentional differences (to fix)

- None in the reviewed file. Ruzu now owns both motion parameter slots and devices, restores the
  first player's configured motion source, adds the virtual-gamepad source, updates raw and
  emulated motion in callback order, resets rotations/quaternion on reload, and applies
  `motion_sensitivity` to `is_at_rest` exactly where Eden does.
- Callback keys now increment before insertion and therefore start at 1, matching Eden.
- Deleting an unknown callback now asserts instead of only logging, matching Eden's
  `ASSERT_MSG` contract.

### Missing items

- The downstream `ConsoleSixAxis` and `SevenSixAxis` resources do not yet consume this newly live
  console state in their update paths. That wiring belongs to those corresponding files and is a
  separate prerequisite-sensitive slice.

### Binary layout verification

- N/A: `ConsoleMotion` and `ConsoleMotionInfo` are synchronized host-side frontend state and are
  not copied raw into guest memory.

## 2026-08-21 — console six-axis ownership in `src/hid_core/src/resources/six_axis/console_six_axis.rs` / `resource_manager.rs` vs Eden counterparts

### Intentional differences

- Ruzu's `ControllerActivation` stores shared `Arc<Mutex<...>>` references in place of Eden's
  `ControllerBase` raw/reference members. `ConsoleSixAxis::new` receives the HID core and the
  resource manager supplies the applet resource during sampler initialization.
- The private `update_shared_memory` helper is a mechanical extraction of Eden's four assignments
  so their projection can be regression-tested without fabricating kernel-backed applet memory.

### Unintentional differences (to fix)

- None. `ConsoleSixAxis::on_update` now owns active-ARUID validation, activation validation,
  `EmulatedConsole::get_motion`, and the shared-memory projection. `ResourceManager::update_motion`
  only schedules the call, matching Eden's ownership boundary.
- The obsolete Ruzu-only `ConsoleMotionStatus` duplicate and the default status constructed by the
  resource manager were removed.
- Sampler initialization no longer assigns an applet resource to `SevenSixAxis`, matching Eden,
  which only assigns one to `ConsoleSixAxis`.

### Missing items

- `SevenSixAxis::on_update` still needs the `Core::System` timing/application-memory dependency
  owned by its Eden constructor. It remains a separate structural prerequisite and was not
  approximated in this slice.

### Binary layout verification

- PASS: the existing compile-time assertion still verifies
  `ConsoleSixAxisSensorSharedMemoryFormat` is `0x20` bytes; the focused test verifies the exact
  fields projected by Eden's update.

## 2026-08-21 — `src/core/src/file_sys/fs_path_utility.rs` vs Eden `src/core/file_sys/fs_path_utility.h` bounded backslash replacement

### Intentional differences

- Rust uses a zero-initialized `Vec<u8>` plus a bounded slice copy for Eden's temporary allocation
  and `Strlcpy`; both reserve the caller-provided remaining buffer length and terminate the copied
  source within that bound.

### Unintentional differences (to fix)

- None. The Windows-path backslash replacement now computes `replaced_src_len` from the supplied
  `path_len` minus the consumed source prefix, rather than ignoring `path_len` and sizing from
  `strlen(src)`. This matches Eden when the caller's source-buffer bound truncates the visible
  string.
- The Rust-only outer `relative_len` temporary was removed; `rlen` still advances `cur_pos` at the
  exact point where Eden consumes `relative_len`.

### Missing items

- None in the reviewed `PathFormatter::Normalize` backslash-replacement branch.

### Binary layout verification

- N/A: the regression test verifies bounded byte-copy and normalized output behavior; no struct
  layout changed.

## 2026-08-21 — `src/hid_core/src/frontend/input_converter.rs` vs Eden `src/hid_core/frontend/input_converter.{h,cpp}` analog conversion

### Intentional differences

- None beyond Rust's direct return value and `log` facade.

### Unintentional differences (to fix)

- None. `transform_to_analog` now accepts only `InputType::Analog`, copies properties and raw
  value, sanitizes without clamping, then applies Eden's second inversion step in the same order.

### Missing items

- None for `TransformToAnalog`; it unblocks the upstream-owned mouse-wheel path in
  `EmulatedDevices`.

### Binary layout verification

- N/A: `AnalogStatus` is host-side callback state. Tests cover the non-clamped range, deadzone,
  copied properties, and Eden's deliberately preserved inversion ordering.

## 2026-08-21 — `src/hid_core/src/frontend/emulated_devices.rs` vs Eden `src/hid_core/frontend/emulated_devices.{h,cpp}`

### Intentional differences

- Device callbacks capture `Arc<Mutex<DeviceStatus>>`, the atomic configuration flag, and the
  callback map rather than Eden's raw `this` pointer. State, callback, and method ownership remain
  in `EmulatedDevices`.
- Ruzu's input factory returns a null-object `InputDevice` when a backend is unavailable, so every
  array slot contains `Some(device)` after reload instead of requiring Eden's pointer-null checks.
- The private `assign_bit` helper mechanically represents Eden's `BitField::Assign` operations for
  keyboard modifiers and mouse buttons.

### Unintentional differences (to fix)

- None. Reload/unload now owns all mouse buttons, position, wheel axes, keyboard keys, and keyboard
  modifiers with Eden's exact parameter packages and callback order.
- Button toggle/lock transitions, configuration-mode suppression, modifier bit mapping, mouse
  projection, raw-value getters, notifications, and callback-key lifecycle now match upstream.

### Missing items

- None in the reviewed `EmulatedDevices` file.

### Binary layout verification

- PASS: the existing compile-time assertions retain `KeyboardKey` at `0x20`,
  `KeyboardModifier`/`MouseButton` at `0x4`, and `AnalogStickState` at `0x8`; focused tests verify
  the corresponding bit and numeric projections.

## 2026-08-21 — `src/common/src/random.rs` vs Eden `src/common/random.{h,cpp}`

### Intentional differences

- Rust represents `std::mt19937` with the local `Mt19937` type implementing the standard engine's
  state transition and tempering exactly.
- `fastrand` supplies the process-global host entropy in place of C++ `std::random_device`; both
  are cross-platform, nondeterministic host random sources and the upstream seed parameters remain
  intentionally ignored.

### Unintentional differences (to fix)

- None. `random32`, `random64`, and `get_mt19937` retain Eden's ownership and behavior, including
  the 32-bit `random_device::result_type` widened to `u64` by `random64`.

### Missing items

- None in the reviewed files.

### Binary layout verification

- N/A: no payload struct is serialized. A focused test verifies the MT19937 reference sequence and
  another verifies that `random64` preserves Eden's zero upper 32 bits.

## 2026-08-21 — `src/core/src/hle/kernel/k_process.rs` vs Eden `src/core/hle/kernel/k_process.{h,cpp}` ASLR load offset

### Intentional differences

- Ruzu retains its `is_hbl` argument and assignment because this frontend state is currently owned
  by `KProcess`; it follows the upstream parameters without changing their order.
- The Rust `match` returns the selected address directly instead of declaring a zero-valued local
  and assigning it in every switch arm. Flag mutation and address selection remain in the upstream
  order.

### Unintentional differences (to fix)

- None in the reviewed address-selection path. Every address-space base now includes
  `aslr_space_offset`, then adds `aslr_space_start` when constructing the process parameters.

### Missing items

- `load_from_metadata` still uses pool-size constants because its Rust signature does not yet carry
  Eden's `KernelCore` reference.
- Eden calls `InitializeInterfaces` before returning; Ruzu still creates the ARM interfaces later
  from `System::load`.

### Binary layout verification

- N/A: no serialized layout changed. The focused regression initializes the kernel slab allocator,
  loads a synthetic homebrew process with a nonzero page-aligned offset, and verifies its exact
  entrypoint.

## 2026-08-21 — `src/core/src/loader/deconstructed_rom_directory.rs` vs Eden `src/core/loader/deconstructed_rom_directory.{h,cpp}` ASLR load offset

### Intentional differences

- The additional Ruzu `is_hbl` state is forwarded after Eden's five load parameters; it does not
  alter the upstream ASLR calculation.

### Unintentional differences (to fix)

- None in the reviewed ASLR calculation. The selected seed is shifted by 12, masked with
  `0xfff000`, and passed to `KProcess` after the fast-memory base exactly as in Eden.

### Missing items

- Eden's NCE patch collection, patch-section size, and direct-mapped fast-memory base are not yet
  integrated, so the corresponding argument remains zero on Ruzu's current backends.

### Binary layout verification

- N/A: this slice passes scalar addresses only.

## 2026-08-21 — `src/core/src/loader/kip.rs` vs Eden `src/core/loader/kip.{h,cpp}` ASLR load offset

### Intentional differences

- Rust keeps the loader's virtual file because the `AppLoader` trait has no C++-style base-class
  file member; loader ownership is otherwise unchanged.
- Ruzu's internal `is_hbl = false` argument follows Eden's load parameters.

### Unintentional differences (to fix)

- None in the reviewed ASLR path. Seed selection, shift, mask, zero fast-memory base, and argument
  ordering now match Eden.

### Missing items

- None in the reviewed ASLR path.

### Binary layout verification

- N/A: this slice passes scalar addresses only.

## 2026-08-21 — `src/core/src/loader/nro.rs` vs Eden `src/core/loader/nro.{h,cpp}` ASLR load offset

### Intentional differences

- Ruzu's internal `is_hbl = false` argument follows Eden's load parameters.

### Unintentional differences (to fix)

- None in the reviewed ASLR calculation. The offset is generated after determining `image_size`
  and before process setup, with Eden's exact shift and mask.

### Missing items

- Eden's NCE patching, patch relocation, and direct-mapped fast-memory base remain unintegrated; the
  fast-memory argument therefore remains zero.

### Binary layout verification

- PASS: the existing compile-time assertions still verify the affected NRO, MOD, and asset header
  sizes; this scalar ASLR change does not alter them.

## 2026-08-21 — `src/common/src/intrusive_red_black_tree.rs` vs Eden `src/common/intrusive_red_black_tree.h` bidirectional iteration

### Intentional differences

- Pointer-based C++ iterator positions are represented by arena indices. Rust's immutable and
  mutable double-ended iterators therefore retain explicit front and back indices so mixed forward
  and reverse traversal cannot yield a node twice.
- `IntrusiveRedBlackTreeBaseNode` locates `self` in the arena before following its embedded node
  links; this linear lookup replaces the parent-pointer cast that Rust's arena representation
  cannot express safely.

### Unintentional differences (to fix)

- None. Immutable and mutable iterators now support reverse traversal, and base-node predecessor
  and successor accessors now follow the tree links instead of always returning `NONE`.

### Missing items

- None in the reviewed bidirectional iterator and base-node neighbor methods.

### Binary layout verification

- N/A: Ruzu deliberately uses indices rather than serializing Eden's host pointers. Focused tests
  cover forward, reverse, mixed, mutable, predecessor, and successor traversal without duplicates.

## 2026-08-21 — `src/audio_core/src/sink/cubeb_sink.rs` vs Eden `src/audio_core/sink/cubeb_sink.{h,cpp}` stream metadata ownership

### Intentional differences

- Rust keeps the Cubeb backend object beside a shared `SinkStreamHandle`; this replaces Eden's
  `unique_ptr<CubebSinkStream>` ownership while keeping the stream metadata on `SinkStream`.

### Unintentional differences (to fix)

- None in the reviewed ownership slice. The duplicate `name` and `stream_type` fields were removed
  from the Rust-only Cubeb wrapper; their canonical values remain on `SinkStream`, matching Eden's
  `CubebSinkStream` inheritance from `SinkStream`.

### Missing items

- None in the reviewed ownership slice.

### Binary layout verification

- N/A: the Rust wrapper is host-only state and is neither serialized nor copied to guest memory.

## 2026-08-21 — `src/core/src/hle/service/filesystem/filesystem.rs` vs Eden `src/core/hle/service/filesystem/filesystem.{h,cpp}` provider ownership

### Intentional differences

- Ruzu registers providers through its shared `ContentProviderUnion` rather than Eden's
  `Core::System::RegisterContentProvider`; both unions retain non-owning provider pointers.
- Rust `Box<T>` replaces each upstream `std::unique_ptr<T>` and provides the same stable heap
  address while `FileSystemController` itself is moved.

### Unintentional differences (to fix)

- None in the reviewed ownership slice. BIS, SDMC, external-content, game-card, registered-cache,
  and placeholder-cache objects now have the stable allocation required by Eden's ownership model.
  This prevents union slots from retaining dangling pointers after a controller move.

### Missing items

- None in the reviewed provider and game-card ownership slice.

### Binary layout verification

- N/A: these are host-side ownership objects. A focused regression moves a fully initialized
  controller and verifies that all four union-provider addresses remain unchanged.

## 2026-08-21 — `src/ruzu/src/{main_window,gtk_compat}.rs` vs Eden `src/yuzu/main_window.{h,cpp}` stop confirmation lifecycle

### Intentional differences

- Eden's `ConfirmShutdownGame` uses a blocking `QMessageBox`, while GTK4 confirmation is
  asynchronous. Ruzu therefore retains a one-shot callback and explicit pending state until the
  user responds or the dialog is dismissed.
- Ruzu rejects overlapping Stop/Restart and window-close confirmations. This reproduces the
  exclusivity that Eden receives automatically from its blocking modal dialog.

### Unintentional differences (to fix)

- None in the reviewed confirmation slice. Dismissing or destroying a GTK question now completes
  it as a rejection, so `stop_confirmation_pending` and `close_confirmation_pending` cannot remain
  latched after the dialog disappears.

### Missing items

- None in the reviewed `ConfirmShutdownGame` / `OnStopGame` confirmation lifecycle.

### Binary layout verification

- N/A: the change contains frontend-only callback and modal state.

## 2026-08-21 — `src/audio_core/src/adsp/apps/audio_renderer/audio_renderer.rs` vs Eden `src/audio_core/adsp/apps/audio_renderer/audio_renderer.{h,cpp}`

### Intentional differences

- Rust stores command buffers, processors, and stream handles in
  `Arc<parking_lot::Mutex<RendererShared>>`; this preserves Eden's single owning
  `AudioRenderer` while allowing its host and DSP threads to access the same state safely.
- Ruzu's mailbox and stream waits accept an atomic stop request, and `Stop` drains the response
  before resetting the mailbox. This is the Rust counterpart of `std::jthread` stop-token
  cancellation and prevents teardown from waiting forever after the DSP worker exits.
- `wait_with_stop`, `wait_with_timeout`, and startup-abort cleanup are Rust lifecycle adapters used
  by the threaded system manager; Eden expresses those ownership paths through `std::jthread` and
  blocking mailbox calls.
- Environment-gated event tracing remains available through `RUZU_TRACE_ADSP_AUDIO`. The removed
  `RUZU_PROFILE_ADSP` per-step timer had no Eden equivalent and imposed `Instant::now()` calls and
  an extra stream lock on the real-time render path even though it was only investigation tooling.
- Ruzu handles the map/unmap protocol messages declared by Eden's `Message` enum inline; Eden's
  current `Main` still leaves the separate map/unmap worker as a TODO.
- `CommandListProcessor::process` returns elapsed processing time in both implementations. Ruzu
  stores that duration directly; Eden's current `Process(index) - start_time` subtracts a global
  timestamp from that duration and is inconsistent with the method's implementation and contract.
- Fallible Rust initialization and optional stream handles reject an invalid session safely where
  Eden relies on initialized raw pointers.

### Unintentional differences (to fix)

- None in the reviewed renderer lifecycle and render-loop slice. The 200 ms shutdown delay and the
  `SetProcessTimeMax` → `WaitFreeSpace` → `Process` ordering now match Eden.

### Missing items

- None from the upstream `AudioRenderer` public/private method set or message constants.

### Binary layout verification

- N/A: `AudioRenderer` and `RendererShared` are host-side synchronization and ownership objects;
  guest command-buffer layouts remain owned by `command_buffer.rs`.

## 2026-08-21 — `src/audio_core/src/adsp/apps/opus/opus_decoder.rs` vs Eden `src/audio_core/adsp/apps/opus/opus_decoder.{h,cpp}`

### Intentional differences

- Focused Rust tests exercise the mailbox protocol and decoder lifecycle directly. Their success
  assertions now use the upstream Opus-domain constant `OPUS_OK`, rather than the numerically equal
  but unrelated HLE-service `ResultCode::SUCCESS`.

### Unintentional differences (to fix)

- None introduced by this warning-cleanup slice; runtime decoder behavior is unchanged.

### Missing items

- None discovered while tracing the unused `ResultCode` import through the upstream return-value
  assignments.

### Binary layout verification

- N/A: this slice only changes test assertions and removes an unused production import.

## 2026-08-21 — `src/common/src/dynamic_library.rs` vs Eden `src/common/dynamic_library.{h,cpp}`

### Intentional differences

- Rust's owned `DynamicLibrary` value is non-copyable by default and transfers ownership through
  ordinary moves; `Drop` implements Eden's destructor cleanup.
- `Option<i32>` represents Eden's `-1` major/minor sentinel in
  `get_versioned_filename`.
- Rust converts symbol and file names to `CString` and rejects embedded NUL bytes before calling
  the platform loader; Eden receives pre-existing C strings.
- `get_symbol<T>` returns `Option<T>` instead of assigning through an output pointer and returning
  `bool`.

### Unintentional differences (to fix)

- None. `open` now matches Eden's ordering and replaces the stored handle without first calling
  `close`; the previous pre-emptive cleanup was a Rust-only lifecycle change.

### Missing items

- None from the upstream `DynamicLibrary` interface.

### Binary layout verification

- N/A: the platform loader handle is host-only state and is never serialized.

## 2026-08-21 — `src/common/src/time_zone.rs` vs Eden `src/common/time_zone.{h,cpp}`

### Intentional differences

- Rust uses a `LazyLock<HashMap<...>>` for Eden's immutable `std::map`; the key/value contents and
  lookup behavior are identical.
- Windows uses the thread-safe CRT functions `localtime_s` and `gmtime_s` to obtain owned `tm`
  values. Eden immediately copies the results of `std::localtime` and `std::gmtime`, so subsequent
  calculations see the same state without retaining their static buffers.
- Targets that are neither Unix nor Windows retain a GMT fallback because Eden does not define a
  separate platform implementation for them.

### Unintentional differences (to fix)

- None. Windows now calculates the local/GMT offset and DST state like Eden instead of always
  returning zero and selecting GMT.

### Missing items

- None from the upstream `Common::TimeZone` interface or offset table.

### Binary layout verification

- N/A: timezone values are host-side strings and scalar calculations, not serialized structures.

## 2026-08-21 — `src/common/src/tree.rs` vs Eden `src/common/tree.h`

### Intentional differences

- Rust stores links as indices into a caller-owned slice and uses `usize::MAX` as the null
  sentinel, instead of retaining raw `T*` links. Every upstream rotation, color repair, lookup,
  insertion, removal, and traversal helper remains owned by this file with the same ordering.
- `HasRBEntry` replaces Eden's `CheckRBEntry`, `IsRBEntry`, and `HasRBEntry` C++ concepts.
- Rust naming follows snake_case, and a returned index replaces each returned pointer.

### Unintentional differences (to fix)

- None. `RB_REMOVE`'s `child` is assigned exactly once on each control-flow path as in Eden; its
  unnecessary Rust `mut` qualifier was removed without changing the algorithm.

### Missing items

- None from the upstream red-black-tree type and function set.

### Binary layout verification

- N/A: the index-based `RBEntry` is an internal safe-Rust representation and is not copied to or
  from Eden's packed, pointer-based host structure.

## 2026-08-21 — removed `src/common/src/x64/cpu_wait.rs` vs Eden `src/common/thread.{h,cpp}`

### Intentional differences

- None for the removed module: Eden has no `common/x64/cpu_wait.*` counterpart and Ruzu had no
  production caller for its public `micro_sleep` function.

### Unintentional differences (to fix)

- Ruzu's separate helper monitored the address of a temporary aligned zero rather than the
  `Event::is_set` state used by Eden. Consequently it could only expire by timer and could not be
  awakened by `Event::set`; retaining or moving it would not provide upstream behavior.

### Missing items

- Ruzu's `common/thread.rs` still uses the condition-variable `Event::wait_for` path on Windows and
  does not yet port Eden's x86-64 Windows `MONITORX`/`WAITPKG` branches. This is a separate,
  platform-specific implementation slice rather than a prerequisite for removing the unreachable
  helper.

### Binary layout verification

- N/A: the removed cache-line-aligned tuple was host-only temporary storage passed to inline
  assembly and was never serialized.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/exception_handler.rs` vs Eden `src/dynarmic/src/dynarmic/backend/exception_handler.{h,posix.cpp}`

### Intentional differences

- Rust's `Option<FakeCall>` callback can decline a fault. Eden's callback returns `FakeCall`
  directly for a matched code range.
- The Windows SEH implementation remains in the same Rust file under `cfg(windows)` because the
  crate currently exposes one x64 exception-handler module rather than Eden's per-platform C++
  translation units.
- Ruzu additionally installs an owned alternate stack on each Linux CPU thread because POSIX
  alternate stacks are thread-local. Eden's singleton owns only the stack installed on the thread
  that constructs it. The Rust thread-local owner disables and unmaps its stack at thread exit.
- Eden's POSIX source installs `SIGBUS` only when `__APPLE__` is defined. Ruzu's macOS path is the
  separately documented non-fastmem Mach stub, so Linux correctly installs only `SIGSEGV`.

### Unintentional differences (to fix)

- None identified for the Linux x86-64 handler lifecycle after the 2026-08-21 parity pass.

### Missing items

- None for the Linux x86-64 handler lifecycle. macOS fastmem remains disabled as documented at the
  top of the Rust module and is outside this POSIX/Linux slice.

### Binary layout verification

- N/A: `SigHandlerState` is host-only Rust state. Platform context and SEH
  layouts are verified by the existing platform-specific tests in this module.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/a64_emit_x64.rs` vs Eden `src/dynarmic/src/dynarmic/backend/x64/{emit_x64,a64_emit_x64}.{h,cpp}`

### Intentional differences

- Rust has no shared C++ `EmitX64` base object, so the A64 emitter directly owns its
  `ExceptionHandler`. It is declared before the owned code buffer and callback table so Rust's
  field drop order removes the registration first.

### Unintentional differences (to fix)

- None identified in exception-handler registration, support probing, callback publication, or
  destruction ordering.

### Missing items

- None for this exception-handler ownership slice.

### Binary layout verification

- N/A: the new owner contains host pointers/ranges and is not copied to guest memory.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/a32_emit_x64.rs` vs Eden `src/dynarmic/src/dynarmic/backend/x64/{emit_x64,a32_emit_x64}.{h,cpp}`

### Intentional differences

- Rust has no shared C++ `EmitX64` base object, so the A32 emitter directly owns its
  `ExceptionHandler`. It is declared before the owned code buffer and callback table so cleanup
  follows Eden's emitter-before-code lifetime.

### Unintentional differences (to fix)

- None identified in exception-handler registration, support probing, callback publication, or
  destruction ordering.

### Missing items

- None for this exception-handler ownership slice.

### Binary layout verification

- N/A: the new owner contains host pointers/ranges and is not copied to guest memory.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/block_of_code.rs` vs Eden `src/dynarmic/src/dynarmic/backend/x64/block_of_code.{h,cpp}`

### Intentional differences

- On Windows, Ruzu still places and registers SEH unwind metadata during `prelude_complete`; its
  Windows-only `Drop` remains a fallback for standalone code-buffer tests. Production cleanup is
  now first performed by the emitter-owned `ExceptionHandler`.

### Unintentional differences (to fix)

- None identified in Linux code-block cleanup: the non-upstream unconditional `BlockOfCode::drop`
  registration removal has been deleted.

### Missing items

- None for this exception-handler ownership slice.

### Binary layout verification

- N/A on Linux. Existing Windows tests verify the in-buffer unwind layouts.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/block_of_code.rs` vs Eden `src/dynarmic/src/dynarmic/backend/x64/block_of_code.{h,cpp}`

### Intentional differences

- Ruzu emits x64 through `rxbyak::CodeAssembler` and stores byte offsets into its owned code buffer;
  Eden derives `BlockOfCode` from C++ Xbyak and stores native code pointers.
- Rust uses `cfg(target_os = "windows")` for Eden's `_WIN32` callee-saved XMM6–XMM15 path.

### Unintentional differences (to fix)

- None in the reviewed ABI import/save-restore slice. The `xmmword_ptr` operand and
  `xmm_save_base` helper are now compiled only on Windows, matching the only path that consumes
  them. The native constant-pool regression test now verifies both deduplicated operands.

### Missing items

- None introduced or discovered in the Windows callee-save operand slice.

### Binary layout verification

- PASS: the existing stack-frame and Windows unwind-code tests verify the offsets consumed by the
  XMM save/restore instructions; no serialized guest structure is changed.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/emit_memory.rs` vs Eden `src/dynarmic/src/dynarmic/backend/x64/{a64_emit_x64_memory.cpp,emit_x64_memory.cpp.inc}`

### Intentional differences

- Rust keeps the scalar callback emitters in this shared x64 module and represents the 128-bit
  callback return with an explicit stack buffer on Windows.
- Rust also passes 128-bit callback writes through an explicit pointer on Windows; Eden's C++ ABI
  passes its `Vector` aggregate indirectly there. System V continues to use two integer lanes.
- `rxbyak` memory-operand constructors replace C++ Xbyak's `ptr`/`xword` address frames.

### Unintentional differences (to fix)

- None in the reviewed callback ABI slice. The indirect return buffer now applies to every Windows
  toolchain, matching Eden's `_WIN32`, instead of only MSVC. Ordinary 128-bit writes likewise use
  a Windows pointer payload, and 32/64-bit XMM-backed scalar writes select their third argument
  through `ABI_PARAMS` rather than hard-coding System V's `RDX`.

### Missing items

- The fastmem/page-table 128-bit paths are owned by
  `backend/x64/a64_emit_x64_memory.rs`; this file intentionally remains the callback-only owner
  selected by the current dispatcher for `A64ReadMemory128`/`A64WriteMemory128`.

### Binary layout verification

- PASS: Windows read/write buffers are exactly 16 bytes after ABI shadow space, and the
  non-Windows path still transfers two 64-bit lanes through ABI-selected registers.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/a64_emit_x64_memory.rs` vs Eden `src/dynarmic/src/dynarmic/backend/x64/a64_emit_x64_memory.cpp`

### Intentional differences

- Ruzu stores fallback entry offsets in Rust hash maps and calls explicit Rust trampolines; Eden
  stores native function pointers and devirtualizes C++ `UserCallbacks`.
- The Rust Windows read trampoline accepts an explicit output pointer after the fixed context and
  address arguments. This preserves the same stack-buffer transfer without relying on C++'s
  compiler-specific hidden-return ordering.

### Unintentional differences (to fix)

- None in the reviewed 128-bit read-fallback ABI slice. Both MSVC and MinGW now use the Windows
  stack buffer, matching upstream `_WIN32`; System V no longer reserves the unused 16-byte local.
- Removed one unused register binding from Ruzu-only fastmem diagnostic emission; emitted code is
  unchanged.

### Missing items

- Ruzu's current dispatcher routes ordinary 128-bit accesses through callback-only
  `emit_memory.rs`; it does not yet select Eden's fastmem/page-table 128-bit read/write fallback
  path. This is pre-existing behavioral debt outside the ABI prerequisite fixed here.

### Binary layout verification

- PASS for the reviewed fallback payload: the Windows local is 16 bytes and is loaded with
  `movups`; System V reconstructs the vector from the two 64-bit return registers.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/emit_exclusive_memory.rs` vs Eden `src/dynarmic/src/dynarmic/backend/x64/emit_x64_memory.cpp.inc`

### Intentional differences

- Ruzu owns architecture-specific exclusive emission in this Rust file, while Eden instantiates
  the shared C++ template include from its A64 emitter.
- Rust's Windows trampolines take explicit pointer payloads for 128-bit values instead of exposing
  the host compiler's aggregate ABI directly to generated code.

### Unintentional differences (to fix)

- None in the reviewed callback-only 128-bit read/write slice. All Windows toolchains use the
  stack-buffer read path, and exclusive writes use a pointer payload rather than System V lane
  registers that overwrite Win64 arguments.

### Missing items

- No new missing item found in the callback-only exclusive slice; inline fastmem ownership was not
  re-audited as part of this prerequisite.

### Binary layout verification

- PASS: each Windows exclusive payload occupies 16 bytes after the 32-byte shadow space; System V
  continues to pass or return two 64-bit lanes.

## 2026-08-21 — `src/rdynarmic/src/jit.rs` vs Eden `src/dynarmic/src/dynarmic/interface/A64/config.h` and x64 memory callback call sites

### Intentional differences

- Rust uses free `extern "C"` trampolines to recover `JitInner`; Eden invokes virtual
  `UserCallbacks` through `ArgCallback`/`Devirtualize`.
- On Windows, Rust gives the read and write trampolines explicit `Pair128` pointers. Eden obtains
  the equivalent indirect aggregate transfer from its C++ ABI and generated accessor stubs.

### Unintentional differences (to fix)

- None in the reviewed A64 128-bit trampoline slice. The ordinary and exclusive read/write
  signatures now agree with the emitter on both Windows toolchains.

### Missing items

- None introduced in the A64 trampoline slice. A32 trampolines have separate emitter ownership and
  were not changed or claimed by this comparison.

### Binary layout verification

- PASS: `Pair128` is `repr(C)`, compile-time asserted to size 16/alignment 8, and every field is
  initialized before it crosses the trampoline boundary.

## 2026-08-21 — `src/rdynarmic/src/ir/opcode.rs` vs Eden `src/dynarmic/src/dynarmic/ir/opcodes.inc`

### Intentional differences

- Rust represents Eden's generated opcode table as an enum plus an explicit `OpcodeInfo` match.

### Unintentional differences (to fix)

- None in the scalar result-and-overflow saturation opcode slice: both `WithFlag32` operations
  have the same U32 inputs/result, while signed and unsigned saturation keep their U8 width input.

### Missing items

- None for the four scalar saturation opcodes reviewed in this slice.

### Binary layout verification

- N/A: these are internal IR opcode/type declarations and are not serialized guest payloads.

## 2026-08-21 — `src/rdynarmic/src/ir/emitter.rs` vs Eden `src/dynarmic/src/dynarmic/ir/ir_emitter.h`

### Intentional differences

- Rust's `ResultAndOverflow` stores the untyped `Value` enum instead of Eden's templated result
  type; opcode metadata enforces that every helper in this slice returns U32 plus U1.

### Unintentional differences (to fix)

- None in `signed_saturated_add_with_flag`, `signed_saturated_sub_with_flag`,
  `signed_saturation`, or `unsigned_saturation`: validation, opcode arguments, and associated
  overflow pseudo-operation ordering match Eden.

### Missing items

- None for the scalar saturation IR API reviewed in this slice.

### Binary layout verification

- N/A: `ResultAndOverflow` is an internal SSA builder result and is never copied to guest memory.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/emit_saturation.rs` vs Eden `src/dynarmic/src/dynarmic/backend/x64/emit_x64_saturation.cpp`

### Intentional differences

- Rust passes the presence of Eden's `has_overflow_inst` template parameter explicitly and uses
  `Option<InstRef>` for the associated pseudo-operation.
- `rxbyak` register-width conversions replace C++ Xbyak's `changeBit` views.

### Unintentional differences (to fix)

- None in the signed saturated add/sub, signed scalar saturation, or unsigned scalar saturation
  methods reviewed here. In particular, `WithFlag32` exposes overflow without touching FPSR.QC,
  ordinary signed saturated add/sub ORs the generated overflow byte into QC, and the 8-bit CMOV
  uses a 32-bit operand exactly as Eden does.

### Missing items

- None for the scalar saturation prerequisite methods reviewed in this slice; unrelated methods
  in the same pre-existing file were not claimed as re-audited.

### Binary layout verification

- N/A: emitted host instructions operate on internal SSA values and JIT state fields.

## 2026-08-21 — `src/rdynarmic/src/backend/arm64/emit_arm64_saturation.rs` vs Eden `src/dynarmic/src/dynarmic/backend/arm64/emit_arm64_saturation.cpp`

### Intentional differences

- Ruzu's local ARM64 encoder has no EOR-immediate helper, so Eden's single
  `EOR Wscratch0, Wscratch0, 0x80000000` is emitted as a MOVZ/MOVK into `Wscratch1` followed by
  register EOR. The result and flags are identical.
- Eden's explicit `UNREACHABLE` specializations for generic scalar/vector saturation opcodes fall
  through Ruzu's common unsupported-opcode error if they survive required IR lowering; the four
  reachable scalar result-and-overflow operations remain owned by this matching file.

### Unintentional differences (to fix)

- None in `SignedSaturatedAddWithFlag32`, `SignedSaturatedSubWithFlag32`, `SignedSaturation`, or
  `UnsignedSaturation`: register realization, flag spilling, clamp ordering, and overflow creation
  match Eden.

### Missing items

- None for the four reachable scalar saturation operations reviewed in this slice.

### Binary layout verification

- N/A: the emitted AArch64 instruction stream does not serialize a guest-visible structure.

## 2026-08-21 — `src/rdynarmic/src/backend/arm64/inst.rs` vs Oaknut instructions used by Eden `emit_arm64_saturation.cpp`

### Intentional differences

- Ruzu encodes AArch64 instructions directly as `u32` words rather than calling Oaknut.

### Unintentional differences (to fix)

- None for the newly required `CMP Wn, Wm` encoding; its known machine word is covered by the
  AArch64 encoding regression test.

### Missing items

- None for the instruction-encoding prerequisite in this slice.

### Binary layout verification

- PASS: `cmp w16, w17` encodes as `0x6b11021f`, verified under the AArch64 test target.

## 2026-08-21 — `src/rdynarmic/src/backend/{x64/emit.rs,arm64/emit_arm64.rs,arm64/mod.rs}` vs Eden backend saturation emitter registration

### Intentional differences

- Rust dispatches opcodes through explicit `match` arms and declares the ARM64 source module in
  `mod.rs`; Eden registers template specializations through its C++ emitter headers and build.

### Unintentional differences (to fix)

- None in this routing slice: all four scalar result-and-overflow saturation opcodes reach their
  architecture-specific owner on x64 and ARM64.

### Missing items

- None introduced by the routing change.

### Binary layout verification

- N/A: routing declarations do not define a serialized layout.

## 2026-08-21 — `src/rdynarmic/src/frontend/a32/translate/helpers.rs` vs Eden `src/dynarmic/src/dynarmic/frontend/A32/translate/impl/common.h`

### Intentional differences

- Rust returns the untyped internal `Value` enum where Eden's helper signatures distinguish U16
  and U32 at compile time; the emitted opcode metadata retains those types.

### Unintentional differences (to fix)

- None in `pack_2x16_to_1x32` or `most_significant_half`: masks, shift amounts, carry input, and
  operation ordering match Eden exactly.

### Missing items

- None for the two common helpers required by the scalar saturation translator slice. Other
  pre-existing helpers in `common.h` were not re-audited or claimed by this prerequisite.

### Binary layout verification

- N/A: these helpers construct internal SSA operations and serialize no guest-visible payload.

## 2026-08-21 — `src/rdynarmic/src/frontend/a32/translate/saturated.rs` vs Eden `src/dynarmic/src/dynarmic/frontend/A32/translate/impl/{saturated.cpp,a32_translate_impl.h}`

### Intentional differences

- Ruzu decodes fields from `DecodedArm::raw` inside each Rust method, while Eden's generated
  decoder passes typed immediates, booleans, and registers as method arguments.
- ARM condition state is emitted once at the Rust block-translation boundary; the method bodies
  therefore begin with Eden's pre-condition register validation and then emit the instruction
  body. Invalid PC operands still raise Unpredictable before any register read.

### Unintentional differences (to fix)

- None in `arm_ssat`, `arm_ssat16`, `arm_usat`, `arm_usat16`, `arm_qadd`, `arm_qsub`,
  `arm_qdadd`, or `arm_qdsub`. Saturation widths, immediate-shift carry input, signed halfword
  extension, result packing, and every sticky-Q update match Eden's order.

### Missing items

- Eden's `arm_QASX`, `arm_QSAX`, `arm_UQASX`, and `arm_UQSAX` remain absent because Ruzu's ARM
  decoder does not yet expose those instruction IDs. They are pre-existing parallel-instruction
  debt outside this scalar warning slice.

### Binary layout verification

- N/A: these translators construct internal SSA and no raw guest payload.

## 2026-08-21 — `src/rdynarmic/src/frontend/a32/translate/mod.rs` vs Eden ARM decoder/visitor dispatch for scalar saturation

### Intentional differences

- Rust uses an explicit `ArmInstId` match after block-level condition setup; Eden invokes visitor
  methods through generated decoder callbacks.

### Unintentional differences (to fix)

- None in this routing slice: all eight decoded ARM scalar saturation instructions now call their
  owner in `saturated.rs`; the former successful no-op stubs were removed.

### Missing items

- The four parallel saturation IDs named in the `saturated.rs` audit remain absent from the Rust
  decoder and consequently from this dispatcher.

### Binary layout verification

- N/A: dispatcher routing defines no serialized layout.

## 2026-08-21 — `src/rdynarmic/src/jit.rs` scalar saturation regression vs Eden `frontend/A32/translate/impl/saturated.cpp`

### Intentional differences

- The Rust-native regression executes a compact ARM instruction stream through each available
  host backend; Eden's C++ source defines the expected semantics but does not own this Rust test.

### Unintentional differences (to fix)

- None in the covered behavior: signed/unsigned scalar and halfword clamps produce the expected
  registers, saturated addition clamps to INT32_MAX, and CPSR.Q remains set.

### Missing items

- This focused regression does not claim exhaustive immediate widths or every QDADD/QDSUB input;
  their IR ordering is covered by module tests.

### Binary layout verification

- N/A: the test executes guest instructions but changes no serialized guest structure.

## 2026-08-21 — `src/rdynarmic/src/frontend/a32/translate/{data_processing.rs,mod.rs}` vs Eden `src/dynarmic/src/dynarmic/frontend/A32/translate/impl/{data_processing.cpp,a32_translate_impl.h}`

### Intentional differences

- Ruzu extracts instruction fields from `DecodedArm` and performs ARM condition handling at the
  block-translation boundary; Eden's generated decoder passes typed fields to individual visitor
  methods, each of which calls `ArmConditionPassed`.
- Rust inserts an identity `Or32` before `GetNZFromOp` when MOV/MVN yields a non-instruction
  `Value`; Eden's typed IR can attach `NZFrom` directly. This preserves the associated-pseudo-op
  contract used by both Rust backends.
- `translate/mod.rs` imports only `decode_thumb32`; Eden's visitor declarations do not require a
  Rust instruction-ID type import, and the removed `Thumb32InstId` import had no behavior.

### Unintentional differences (to fix)

- The pre-existing `classify`/`dp_emit` dispatcher consolidates Eden's 48 separately owned
  immediate, immediate-shift, and register-shift methods. The audited paths now preserve Eden's
  helper choice, carry reads, PC validation, state-update ordering, and BIC `AndNot` operation,
  but the method-boundary flattening still needs to be unwound for strict structural parity.

### Missing items

- No decoded ARM data-processing operation is missing from this file. Exact one-method-per-Eden-
  visitor structure remains the structural work identified above.

### Binary layout verification

- N/A: these translators construct internal SSA operations and serialize no guest-visible
  payload.

## 2026-08-21 — `src/rdynarmic/src/backend/x64/{a32_emit_a32.rs,emit_a64.rs,emit_vector_multiply.rs}` warning-only cleanup vs Eden x64 emitter owners

### Intentional differences

- The Rust A32 emitter keeps the uniform `EmitContext` argument required by opcode dispatch but
  names it `_ctx`; Eden's `EmitA32ClearExclusive` likewise retains and leaves its
  `A32EmitContext&` parameter unnamed.
- Rust-native emitter regressions have no direct Eden test-file counterpart. Removing one unused
  synthetic `Inst` and three unnecessary `unsafe` call sites changes neither the emitted code nor
  the paired-min/max fallback calculations rechecked against Eden's `emit_x64_vector.cpp`.

### Unintentional differences (to fix)

- None introduced or found in this warning-only slice. Production vector-emitter parity outside
  the three existing lower-paired regressions was not re-audited here.

### Missing items

- None for this warning-only slice.

### Binary layout verification

- N/A: parameter naming and Rust test call-site cleanup define no serialized layout.

## 2026-08-21 — `src/rdynarmic/src/frontend/a32/translate/thumb16.rs` PUSH/POP vs Eden `src/dynarmic/src/dynarmic/frontend/A32/translate/impl/{thumb16.cpp,a32_translate_impl.h}`

### Intentional differences

- Ruzu extracts `M`/`P` and the low register list from `DecodedThumb16`; Eden's generated decoder
  passes those fields as typed visitor arguments. Both construct the same 16-bit register mask.
- Rust uses `Reg::R13` for Eden's `Reg::SP` spelling and `Value::ImmU1` carry operands for the
  equivalent `ir.Add`/`ir.Sub` operations.

### Unintentional differences (to fix)

- None in the re-audited PUSH/POP slice: empty lists are rejected before reading SP, stack
  accesses are `Atomic`, registers are visited in ascending order, and POP writes the incremented
  address to SP at Eden's exact point before `PopRSBHint`.

### Missing items

- None in `thumb16_PUSH` or `thumb16_POP`. Other methods in the shared Rust file were not claimed
  by this warning-driven audit.

### Binary layout verification

- N/A: the methods emit guest memory operations but define no serialized structure.

## 2026-08-21 — `src/rdynarmic/src/frontend/a64/translate/mod.rs` vs Eden `src/dynarmic/src/dynarmic/frontend/A64/translate/{a64_translate.cpp,a64_translate.h}`

### Intentional differences

- Rust returns its newly allocated `Block`; Eden appends into a caller-owned block. Location
  advancement, cycle accounting, single-step linking, terminal validation, and end-location
  assignment otherwise retain the same ownership and order.
- Rust leaves `should_continue` uninitialized until the first mandatory loop iteration, avoiding
  an overwritten-value warning. Eden initializes it for C++ `do`/`while` syntax; both assign it
  on every path before reading it.

### Unintentional differences (to fix)

- Eden raises `UnallocatedEncoding` whenever its decoder has no match. Ruzu currently raises it
  only for the reserved low encoding range and sends other unmatched instructions to
  `interpret_this_instruction` so its incomplete decoder can fall back to the interpreter. This
  compatibility path must disappear when decoder parity is complete; changing it in this
  warning-only slice would turn still-supported instructions into exceptions.

### Missing items

- The public equivalent of Eden's `TranslateSingleInstruction` is absent. Module-local test
  helpers with a similar name do not implement that API.

### Binary layout verification

- N/A: block translation control flow defines no serialized guest structure.

## 2026-08-21 — `src/rdynarmic/src/ir/opt/a32_get_set_elimination.rs` pending-C forwarding vs Eden `src/dynarmic/src/dynarmic/ir/{opt_passes.cpp,opt_passes.h}`

### Intentional differences

- Eden inserts `GetCFlagFromNZCV`, breaks from the switch, and lets reverse-iterator movement
  revisit the shifted `A32SetCpsrNZCV`. Rust's indexed arena inserts before the set, adjusts the
  pending use and set indices, and completes the same set handling in the current iteration. The
  resulting instruction order and optimizer state are identical.
- The Rust pass is split into its own ownership file instead of remaining a static section of
  Eden's large `opt_passes.cpp`; this is an existing Rust module boundary for a named upstream
  pass, and its comments now point to the actual current Eden owner.

### Unintentional differences (to fix)

- None in the re-audited pending-C/`A32SetCpsrNZCV` path. The removed boolean assignment was
  overwritten by the complete `FlagInfo::set_not_required()` state before any read.

### Missing items

- None in this warning-driven path. The rest of `FlagsPass` and `RegisterPass` was not newly
  claimed by this focused audit.

### Binary layout verification

- N/A: this optimizer rewrites internal SSA and defines no serialized structure.

## 2026-08-21 — `src/core/src/file_sys/content_archive.rs` vs Eden `src/core/file_sys/{content_archive.h,content_archive.cpp}`

### Intentional differences

- Rust receives a non-nullable `VirtualFile`; Eden's initial `file == nullptr` branch therefore has
  no Rust representation. An empty, non-null file is passed to `NcaReader::Initialize` and reported
  as `ErrorBadNCAHeader`, as Eden does for the same object.
- Rust stores the reader in an `Option<Arc<NcaReader>>` because initialization can fail before an
  initialized reader is available. The getters retain their existing safe defaults when called on
  a failed object; Eden relies on callers checking `GetStatus()` before using those getters.
- The unused `encrypted` member was removed. Eden declares and default-initializes the member but
  never reads or writes it anywhere; retaining it in Rust only produced dead-state warning noise.
- `Arc<Mutex<KeyManager>>` replaces Eden's reference to the singleton key manager while preserving
  key lookup ownership and constructor ordering.

### Unintentional differences (to fix)

- `get_type` maps an invalid raw content-type byte (or a missing reader after failed construction)
  to `Program`; Eden directly casts the byte to `NCAContentType`. Preserving an invalid discriminant
  safely requires changing the Rust public type instead of constructing an invalid enum value.

### Missing items

- None in the constructor paths audited here: reader initialization, key-area validation, title-key
  setup, filesystem classification, update detection, and final status now follow Eden's ordering.

### Binary layout verification

- PASS: `NCA` itself is not serialized. The regression fixture writes the existing `repr(C)`
  `NcaHeader`, whose compile-time size assertion remains `0x400`; it introduces no new payload type.

## 2026-08-21 — `src/core/src/file_sys/fssystem/aes_xts_storage.rs` vs Eden `src/core/file_sys/fssystem/{fssystem_aes_xts_storage.h,fssystem_aes_xts_storage.cpp}`

### Intentional differences

- Rust constructs an `AesCipher` from the retained key for each locked read; Eden constructs and
  retains an optional cipher in the object. This preserves serialized access and the exact tweak
  sequence, with only cipher-context reuse differing.
- Eden's bounded `boost::container::static_vector` is represented by a zero-initialized `Vec` after
  enforcing the same `NcaHeader::XtsBlockSize` maximum. Its bytes and lifetime are equivalent, but
  Rust currently allocates this uncommon partial-sector buffer on the heap.
- The `VfsFile` implementation supplies the Rust VFS naming, parent, readability, and write-reject
  methods around Eden's `IReadOnlyStorage` interface.

### Unintentional differences (to fix)

- None in `MakeAesXtsIv`, construction, `Read`, or `GetSize` after this audit. In particular, reads
  now seed the counter from the supplied IV and preserve XTS block-tweak position for an offset in
  the middle of a storage block.

### Missing items

- None for this storage layer.

### Binary layout verification

- N/A: `AesXtsStorage` is an in-memory polymorphic storage object and is not serialized. Key and IV
  arrays retain Eden's exact `0x20` and `0x10` byte sizes.

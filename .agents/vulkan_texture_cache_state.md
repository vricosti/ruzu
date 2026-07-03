# Vulkan texture cache interrupted slice

## 2026-07-02 validation update

External workload:

- `/Users/vricosti/Dev/emulators/deko3d-join-min/deko3d-join-min.nro`
- Source lives outside the ruzu repo at `/Users/vricosti/Dev/emulators/deko3d-join-min`.
- The workload now creates a two-level RGBA8 deko3d image per swapchain slot, renders first to
  the 640x360 mip1 view, then binds a sampled descriptor for the full 1280x720 image and renders
  mip0. It uses a zero-filled sampler descriptor to avoid calling `dkSamplerDescriptorInitialize`,
  because that guest library path currently hits an unported A64 JIT instruction
  (`FP to 32-bit fixed with fbits=8`).

Validation command:

```bash
timeout 10s env \
  RUST_LOG=info \
  RUZU_TRACE_RT=1 \
  RUZU_TRACE_VK_JOIN_COPY=1 \
  RUZU_TRACE_TEXTURE_CACHE_ADDRS=0x500970000,0x500D30000,0x500E20000,0x5011E0000 \
  RUZU_DUMP_PRESENT_FRAME=/tmp/ruzu_deko3d_join_sampler.ppm \
  /Users/vricosti/Dev/emulators/ruzu/target/release/ruzu-cmd \
  -g /Users/vricosti/Dev/emulators/deko3d-join-min/deko3d-join-min.nro
```

Observed result:

- The first render-target-only pass still creates one-level RT images:
  - mip1 RT: `gpu=0x500D30000 cpu=0xBC6000 640x360 size=0xF0000`
  - mip0 RT: `gpu=0x500970000 cpu=0x806000 1280x720 size=0x3C0000`
- The sampled texture/TIC path then creates the real multi-level parent:
  `join_begin gpu=0x500970000 cpu=0x806000 size=0x4B0000 overlaps=[3, 1]`.
- `finish_pending_join_copies` drains the queued tail and reaches the intended upstream runtime
  copy path:
  - `[VK_JOIN_COPY] join_tail new=2 copies=2 left_alias=0 right_alias=0 bad_overlap=0`
  - `[VK_JOIN_COPY] runtime.CopyImage dst=2 src=1 copies=1`
  - `[VK_JOIN_COPY] runtime.CopyImage dst=2 src=3 copies=1`
- Slot 1 repeats the same path for `new=7`, also with two runtime `CopyImage` calls.
- A frame is presented and dumped to `/tmp/ruzu_deko3d_join_sampler.ppm`.

Implication for the next slice:

- This workload is now the correct lightweight validation gate for
  `FinishPendingJoinCopies -> TextureCacheRuntime::CopyImage`. Pinball remains a non-regression
  gate only; it does not exercise join-copy.
- RT-only workloads cannot validate this path because
  `ImageInfo::from_render_target_info` has `resources.levels = 1`, so the parent RT region ends
  exactly where mip1 starts and `collect_images_in_region` sees no overlap. A sampled texture/TIC
  bind is required to create the multi-level parent image.
- The next strict backend step can use this workload after each `TextureCacheRuntime` operation
  port to verify that queued join copies still execute through scheduler-recorded runtime methods.

## 2026-07-01

Requested slice: full upstream-faithful backend `Vulkan::Image`, `Vulkan::ImageView`, and
`Vulkan::Framebuffer` port from `video_core/renderer_vulkan/vk_texture_cache.h/cpp`.

Implemented in `video_core/src/renderer_vulkan/texture_cache.rs`:

- Upstream-shaped `TextureCacheRuntime` owner for Vulkan device/instance/physical-device resource
  creation and destruction.
- Scheduler-tick based delayed destruction queue for detached backend Vulkan framebuffers, image
  views, and images.
- Vulkan drains `TextureCacheBase::pending_backend_deletions` and evicts dependent backend
  framebuffers/image views/images without immediate `vkDestroy*`.
- Vulkan sets `backend_completes_join_images` and completes pending backend insertions enough to
  preserve base registration/allocation ordering before backend image/view materialization.
- Dedicated backend `Image`, `ImageView`, and `Framebuffer` owners.
- `ImageId`-keyed backend image ownership.
- `ImageViewId`-keyed backend image-view ownership.
- `RenderTargets`-keyed backend framebuffer ownership.
- Resource creation/destruction helpers moved behind `TextureCacheRuntime`:
  `create_image_from_info`, image-view creation, auxiliary views, framebuffer creation,
  attachment creation, sampled-image creation, and resource teardown.
- `MemoryAllocator::create_owned_image` and `AllocatedImage` now provide a Rust owner equivalent
  for the upstream `vk::Image` wrapper returned by `MemoryAllocator::CreateImage`.
- Backend `ImageId` images now allocate through
  `TextureCacheRuntime::memory_allocator().create_owned_image(MakeImageCreateInfo(...))` and
  store the returned `AllocatedImage` owner. `TextureCacheRuntime::destroy_image` now destroys
  image views only; image memory is released by the allocator-backed owner when the backend image
  is dropped or sentenced.
- The legacy `CachedTexture` sampled-image cache has been removed. Sampled texture materialization
  now resolves through the backend `Image`/`ImageView` owners: pending backend insertion, pending
  joins, CPU-modified refresh, `ensure_image`, `ensure_image_view`, then `image_view_handle`.
  Uploads therefore go through `Image::upload_memory` and scheduler-recorded barriers instead of
  writing directly into the active command buffer.
- Dead texture-cache attachment helpers that created/destroyed raw `VkImage`/`VkDeviceMemory`
  pairs have been removed from `TextureCacheRuntime` and `TextureCache`. Remaining Vulkan image
  allocation inside the texture cache goes through backend `Image`/`AllocatedImage`.
- Upstream-shaped helpers: image type conversion, sample count conversion, image usage flags,
  `make_image_create_info`, image/view aspect masks, subresource range conversion, texture/view
  type conversion, and typed storage image-format conversion.
- Lazy depth, stencil, color, and storage view creation methods on the backend texture cache.
- Render target path reconnected through these owners.
- Partial upstream-shaped upload path for backend images:
  `Image::upload_memory` now requests an outside-render-pass context and records the
  buffer-to-image copy/barriers through `Scheduler::record`, matching upstream's ownership/order
  more closely than direct command-buffer writes. Sampled-image materialization can complete the
  targeted pending backend insertion by reading guest memory, unswizzling it, uploading to the
  `ImageId` backend image, clearing `CPU_MODIFIED`, and registering the completed backend image.
- Runtime upload/service shims:
  `TextureCacheRuntime::upload_staging_buffer` wraps the staging-pool request, and
  `insert_upload_memory_barrier` exists as the upstream-faithful Vulkan no-op.
- `TextureCacheRuntime::copy_image` has been ported as a scheduler-recorded operation with
  upstream pre/post barriers, `MakeImageCopy`, and `RangedBarrierRange`.
- `TextureCacheRuntime` now also stores upstream-shaped references to `BlitImageHelper`,
  `DescriptorPool`, and `ComputePassDescriptorQueue`. `RasterizerVulkan` owns those services next
  to the guest descriptor queue/render-pass cache and passes stable references into the runtime,
  matching the upstream service ownership direction.
- `MSAACopyPass` is wired as an optional runtime service when the physical device reports
  `shaderStorageImageMultisample`. The host shader build now generates both
  `convert_non_msaa_to_msaa.comp` and `convert_msaa_to_non_msaa.comp` SPIR-V blobs.
- `TextureCacheRuntime::copy_image_msaa` now records descriptor-backed compute dispatches through
  the scheduler for sample-count-mismatch join copies, and pending join completion no longer
  requeues sample-count mismatch solely because `CopyImageMSAA` was missing.
- `TextureCacheRuntime` now stores stable references to `Scheduler`, `MemoryAllocator`,
  `StagingBufferPool`, and `RenderPassCache`, matching upstream's service-reference ownership for
  those services. `RasterizerVulkan` boxes scheduler/staging/render-pass-cache so their addresses
  remain stable for the runtime, while `RendererVulkan` boxes `MemoryAllocator` and passes it
  through `RasterizerVulkan` just like upstream.
- `RendererVulkan` declares `rasterizer` before `memory_allocator` so Rust drops the rasterizer
  before the allocator. This is required because Rust drops fields in declaration order, while
  upstream C++ destroys fields in reverse declaration order and therefore also destroys
  `RasterizerVulkan` before `MemoryAllocator`.
- Sampled-image materialization/render-target framebuffer lookup no longer pass scheduler,
  staging, or render-pass-cache through their public APIs.
- Vulkan now drains `TextureCacheBase::pending_join_copies` from the sampled-image materialization
  path when reader/staging/scheduler are available. The non-rescaled, non-MSAA path follows the
  upstream join tail ordering: sibling-rescale gate, refresh new image, apply alias relations,
  preflight copy support, scheduler-recorded alias/GPU-modified `CopyImage`, delete overlap image,
  and register the completed new image.
- `TextureCacheRuntime` now owns the current `ResolutionScalingInfo`, creates allocator-backed
  scaled images, and records upstream-shaped `BlitScale` commands with transfer barriers.
- Backend `scale_up_image` / `scale_down_image` now mirror upstream `Image::ScaleUp` /
  `Image::ScaleDown` for the direct `vkCmdBlitImage` path: allocate scaled image lazily, switch
  `current_image`, toggle `RESCALED`, and invalidate both backend/base scale state.
- `TextureCacheRuntime` now checks the upstream `NeedsScaleHelper` format-feature gate before
  direct scaling: if the format lacks optimal `BLIT_SRC|BLIT_DST`, Rust requeues the join/rescale
  instead of incorrectly issuing the direct `vkCmdBlitImage` path. The full helper draw path is
  still not ported.
- `BlitImageHelper` now owns stable references to `Scheduler` and `DescriptorPool`, matching the
  upstream service direction for helper blits. The color blit path is no longer a no-op: it creates
  a real fullscreen-triangle graphics pipeline, allocates/updates a combined-image-sampler
  descriptor set, requests the destination render pass, binds viewport/scissor/push constants, and
  records `Draw(3, 1, 0, 0)`.
- `TextureCache` now has a color `BlitScaleHelper` path: backend `Image` stores cached
  scale/normal helper views and framebuffers, creates them lazily when `NeedsScaleHelper` requires
  helper-backed scaling, and calls `BlitImageHelper::blit_color` instead of requeueing color
  helper scales.
- `BlitImageHelper::blit_depth_stencil` now records a real upstream-shaped draw path when
  `VK_EXT_shader_stencil_export` is available: it creates/caches the depth/stencil helper
  pipeline, allocates/updates two combined-image-sampler descriptors, requests the render pass,
  binds viewport/scissor/push constants, and draws the fullscreen triangle.
- `TextureCache` now wires depth/stencil `BlitScaleHelper` for images whose aspect is exactly
  depth|stencil. It lazily creates combined render-target, depth-only, and stencil-only helper
  views plus a depth-only helper framebuffer, then calls `BlitImageHelper::blit_depth_stencil`.
- Pending join completion no longer requeues purely because siblings/new image need
  `ScaleUp`/`ScaleDown`. It now follows upstream `JoinImages` ordering for the direct path:
  scale siblings first, refresh the new image, scale the new image, then copy with
  `up_scale/down_shift` applied to `MakeShrinkImageCopies` when the join is rescaled.
- Join-overlap deletion removes tracked/registered base state, evicts backend framebuffers for
  removed views, sentences backend image views/images, then calls
  `delete_image_after_backend_cleanup`, preserving the base/backend lifecycle split.

Stopped prerequisite:

- Full `Image::DownloadMemory`, `BlitImage`, `ConvertImage`, ASTC GPU decode, and helper-backed
  scale paths are still incomplete. The runtime now has the required scheduler, memory allocator,
  staging pool, render-pass cache, blit helper, descriptor pool/queue, resolution state, and MSAA
  copy service wiring, so the next slices should port the remaining upstream runtime operations
  instead of adding local fallbacks.
- `MemoryAllocator` is now used for backend `ImageId` image creation and sampled-image
  materialization no longer has a separate raw-image cache. The texture-cache-local raw attachment
  helpers are gone; raw offscreen framebuffer attachments remain in `renderer_vulkan/mod.rs`
  outside `TextureCacheRuntime`.
- The current Rust `TextureCacheRuntime` is therefore a real Vulkan resource owner plus partial
  runtime service owner, but not yet the complete upstream runtime services object. Adding no-op
  copy/rescale methods would create a false owner and violate upstream lifecycle ordering.
- Backend insertion completion is now split deliberately:
  - no-reader paths complete only images that do not need a CPU upload and defer `CPU_MODIFIED`
    images;
  - sampled-image materialization completes only the requested `ImageId` when a reader/staging
    buffer/command buffer is available.
  A local attempt to drain all pending insertions with the sampled reader regressed Pinball to a
  black frame because it also completed render targets without the full upstream scheduler/barrier
  context. Do not reintroduce a global reader drain until `FinishPendingJoinCopies` and
  scheduler-recorded runtime ops are ported.
- Pending join completion is intentionally conservative for still-unsupported backend
  prerequisites: if the join needs helper-backed scaling or another unported runtime operation, the
  whole join is requeued intact rather than partially completing with incorrect copy coordinates or
  silently dropping contents. Sample-count mismatch now uses `CopyImageMSAA` when the pass is
  available, and formats requiring helper-backed scale are detected before the direct blit path.
- Present still has a temporary CPU-address compatibility fallback (`render_target_cpu_map`) when
  the common CPU-range lookup misses a newly completed backend render target. The owning backend
  identity remains `ImageId`/`ImageViewId`.

Next prerequisite slice:

- A lightweight deko3d depth workload now exists outside the repo at
  `/Users/vricosti/Dev/emulators/deko3d-depth-min/deko3d-depth-min.nro`. It is based on
  devkitPro `deko_basic`, but binds a real `Z24S8` depth/stencil image, clears depth/stencil,
  enables depth test/write, and draws two overlapping triangles at different depths.
- Reference command:
  `DYLD_LIBRARY_PATH=/Users/vricosti/Dev/emulators/zuyu/build/bin/yuzu.app/Contents/Frameworks:/Users/vricosti/Dev/emulators/zuyu/build/bin/yuzu.app/Contents/MacOS VK_ICD_FILENAMES=/Users/vricosti/Dev/emulators/zuyu/build/bin/yuzu.app/Contents/Resources/vulkan/icd.d/MoltenVK_icd.json /opt/homebrew/bin/timeout 20s /Users/vricosti/Dev/emulators/zuyu/build/bin/yuzu-cmd -g /Users/vricosti/Dev/emulators/deko3d-depth-min/deko3d-depth-min.nro`
- ruzu validation command:
  `RUST_LOG=info RUZU_DUMP_PRESENT_FRAME=/tmp/ruzu_deko3d_depth_min.ppm RUZU_DUMP_PRESENT_FRAME_AT=30 /opt/homebrew/bin/timeout 35s /Users/vricosti/Dev/emulators/ruzu/target/release/ruzu-cmd -g /Users/vricosti/Dev/emulators/deko3d-depth-min/deko3d-depth-min.nro`
- Current result: yuzu-cmd runs with the `yuzu.app` Vulkan/MoltenVK environment. ruzu reaches
  submits/draw frames and dumps `/tmp/ruzu_deko3d_depth_min.ppm`, but the dumped frame is entirely
  black and the run later hits MoltenVK `VK_ERROR_OUT_OF_DEVICE_MEMORY` / device lost. The log
  shows `D24_UNORM_S8_UINT` falling back to `D32_SFLOAT_S8_UINT`, then repeated primitive-restart
  warnings. This makes the next debugging slice clear: add targeted ruzu Vulkan instrumentation
  around framebuffer/render-target binding, clear, pipeline creation, and draw state for this
  deko3d depth workload.
- Scheduler renderpass barrier parity was added after comparing against upstream
  `vk_scheduler.cpp::RequestRenderpass` and `EndRenderPass`: draw/clear render passes now pass
  backend framebuffer images and ranges into `Scheduler::request_renderpass`, and
  `request_outside_renderpass` emits the upstream post-render-pass GENERAL->GENERAL image memory
  barriers after `cmdEndRenderPass`. This is documented in `DIFF.md`.
- Helper blit framebuffers no longer pass empty image/range slices to the scheduler. The reduced
  `BlitFramebufferInfo` bridge now carries the destination image, subresource range, and
  `num_images`, and `BlitImageHelper::blit_color` / `blit_depth_stencil` pass those to
  `Scheduler::request_renderpass`. This removes the scheduler-critical gap where helper
  framebuffers were not represented as upstream `Framebuffer::Images()/ImageRanges()`.
- Pinball non-regression after the helper framebuffer metadata change:
  `/tmp/ruzu_pinball_blitfbinfo.ppm` at frame 30 is complete (`2560x1440`, bbox
  `x[250..2282] y[0..1439]`, guest approx `x[188..1712] y[0..1079]`).
- `TextureCacheRuntime::blit_image` is now ported as the Vulkan runtime owner for
  upstream `TextureCacheRuntime::BlitImage`: color non-MSAA delegates to
  `BlitImageHelper::blit_color`, depth|stencil can delegate to
  `BlitImageHelper::blit_depth_stencil` when direct depth/stencil blit support is absent, and the
  direct blit/resolve path records upstream-shaped transfer barriers and `vkCmdBlitImage` /
  `vkCmdResolveImage`. Helper equivalents of `MakeSubresourceLayers`, `MakeImageBlit`, and
  `MakeImageResolve` live next to the Vulkan runtime helpers.
- Pinball non-regression after adding runtime `blit_image`:
  `/tmp/ruzu_pinball_runtime_blitimage.ppm` at frame 30 is complete (`2560x1440`, bbox
  `x[250..2282] y[0..1439]`, guest approx `x[188..1712] y[0..1079]`).
- Do not wire Fermi2D `AccelerateSurfaceCopy` to runtime blit until backend `PrepareImage` is
  ported. Upstream `TextureCache<P>::BlitImage` calls `PrepareImage(src_id, false, false)` and
  `PrepareImage(dst_id, true, false)` before any view/framebuffer/runtime blit. Rust base
  `prepare_image` still panics because `RefreshContents`, `SynchronizeAliases`,
  `MarkModification`, and LRU touch ordering require the backend runtime.
- Vulkan backend `PrepareImage` is now ported as `TextureCache::prepare_image_with_reader`:
  invalidation clears `CPU_MODIFIED|GPU_MODIFIED` and tracks untracked images; normal preparation
  runs `refresh_contents_with_reader`, `synchronize_aliases`, optional `mark_modification_by_id`,
  then `touch_image`, matching upstream ordering. `synchronize_aliases` now follows upstream alias
  selection by newer `modification_tick`, propagates latest tick and `GPU_MODIFIED`, sorts aliases
  by tick, applies rescale state, and copies alias contents through the existing Vulkan
  `copy_join_image` runtime path.
- The base `TextureCacheBase::prepare_image` remains an intentional panic guard. Call the Vulkan
  backend method from future Vulkan wrapper code; do not route through the common base.
- The wrapper-level `TextureCache<P>::BlitImage` port for Vulkan is now wired:
  `TextureCache::blit_image` translates Fermi2D source/destination addresses, finds or inserts
  the two images with backend completion, finishes pending joins, calls
  `prepare_image_with_reader(src,false,false)` and `prepare_image_with_reader(dst,true,false)`,
  handles rescale mismatch/resolve behavior, creates exact render-target `ImageViewId`s through
  `ImageViewInfo::for_render_target(ImageViewType::E2D, format, range)`, assembles a destination
  `BlitFramebufferInfo`, and calls `TextureCacheRuntime::blit_image`.
- The active Vulkan renderer (`renderer_vulkan/mod.rs`) now wires
  `RasterizerVulkan::accelerate_surface_copy` to the Vulkan texture-cache wrapper using the
  bound channel memory manager for GPU->CPU translation and guest reads.
- Pinball non-regression after wiring the Vulkan wrapper/Fermi2D path:
  `/tmp/ruzu_pinball_vulkan_blit_wrapper_final.ppm` at frame 30 is complete (`2560x1440`, bbox
  `x[250..2282] y[0..1439]`, guest approx `x[188..1712] y[0..1079]`).
- Remaining Vulkan blit gaps are now the runtime fallback branches, not the wrapper call edge:
  `ConvertImage`, `ReinterpretImage`, non-MSAA-to-MSAA, and unsupported Fermi2D operations still
  need upstream-faithful runtime implementations before the blit subsystem is complete.
- `TextureCacheRuntime::ReinterpretImage` is now ported with upstream `ShouldReinterpret` and
  `GetTemporaryBuffer`: reinterpret copies use a cached device-local temporary buffer, transformed
  input/output `VkBufferImageCopy` lists, and the same scheduler-recorded barrier/copy sequence as
  `vk_texture_cache.cpp`. The Vulkan `copy_join_image` path now mirrors upstream
  `TextureCache<P>::CopyImage` for the covered branches: rescale copies first, use direct
  `CopyImage` for same `GetFormatType`, and use `ReinterpretImage` for permitted 2D cross-type
  copies.
- Pinball non-regression after `ReinterpretImage`:
  `/tmp/ruzu_pinball_reinterpret.ppm` at frame 30 is visually complete. The window resized before
  the dump (`3024x1834`), so the bbox is not directly comparable to the earlier `2560x1440`
  captures; measured bbox is `x[295..2696] y[66..1766]`.
- Remaining Vulkan copy fallback gap is now the post-`ShouldReinterpret` conversion branch:
  upstream creates layer-local 2D views/framebuffers and calls `TextureCacheRuntime::ConvertImage`.
  Do not add a placeholder conversion path; port the real `BlitImageHelper::Convert*` pipelines
  first.
- `TextureCacheRuntime::ConvertImage` and the post-`ShouldReinterpret` conversion branch are now
  ported. `copy_join_image` creates per-copy 2D source/destination views, resolves a destination
  framebuffer, builds a `ConversionImageView` bridge with color/depth/stencil handles, verifies the
  upstream expected extent, and records the conversion through `BlitImageHelper::Convert*`.
  `BlitImageHelper` now creates real conversion graphics pipelines and records scheduler
  render-pass draws for `Convert` and `ConvertDepthStencil`.
- Remaining conversion fidelity gaps: no Rust equivalent of upstream `scheduler.InvalidateState()`
  is called after helper conversion draws, and `ConversionImageView` is still a reduced bridge
  rather than a full backend `ImageView&`. Pinball does not exercise these conversion paths.
- deko3d depth validation after `ConvertImage` still fails before any useful visual output:
  `/tmp/ruzu_deko3d_depth_after_convert.ppm` at frame 30 is `2560x1440` and fully black
  (`nonblack=0`), then the run later loses the device with MoltenVK
  `VK_ERROR_OUT_OF_DEVICE_MEMORY` / `ERROR_DEVICE_LOST`. The log still reports
  `VK_FORMAT_D24_UNORM_S8_UINT` fallback to `VK_FORMAT_D32_SFLOAT_S8_UINT` and repeated
  primitive-restart warnings. This confirms the conversion branch did not regress Pinball but also
  did not fix the depth workload.
- The scheduler barrier patch is verified to build, but it does not fix deko3d: frame 30 remains
  all black and the run still later reaches MoltenVK out-of-device-memory/device-lost. Do not keep
  iterating on framebuffer extent/depth-size hypotheses for this workload without new evidence.
- The deko3d black-frame root cause found after this was common-cache image lifecycle visibility:
  with `backend_completes_join_images` enabled, Rust delayed `RegisterImage(new_image_id)` and
  `image_allocs_table` registration behind backend completion. That diverged from upstream
  `TextureCache<P>::JoinImages` / `InsertImage`, where the image is registered immediately after
  the synchronous join. As a result, repeated render-target lookup for the same `(gpu_addr,
  cpu_addr, info)` missed the just-created image and created a new `ImageId` every draw, eventually
  causing view/image churn and black output.
- Current fix: `TextureCacheBase::join_images` now calls `register_image(new_image_id)` immediately
  when no backend copy/delete tail is queued, even if Vulkan will materialize the `VkImage` lazily,
  and `find_or_insert_image_from_info_with_options_result` always registers the image allocation
  after `JoinImages`. Deko3d trace is stable now: `0x500F10000/0x806000` remains `image=1`,
  `0x5012D0000/0xBC6000` remains `image=2`, and `/tmp/ruzu_deko3d_registerfix.ppm` at frame 3 is
  non-black over the full `2560x1440` frame (`nonblack=3686400`).
- Pinball non-regression after the lifecycle fix: `/tmp/ruzu_pinball_registerfix.ppm` at frame 30
  is complete (`2560x1440`, bbox `x[250..2282] y[0..1439]`).
- `Image::UploadMemory` rescale ordering is now aligned with upstream. The Rust Vulkan wrapper
  runs `scale_down_image(image_id, true)` before uploading CPU-modified contents into the original
  image and `scale_up_image(image_id, false)` after `Image::upload_memory` plus
  `insert_upload_memory_barrier`, mirroring upstream `ScaleDown(true) -> CopyBufferToImage ->
  ScaleUp()`. This lives in `refresh_contents_with_reader` because Rust's split backend keeps
  scale helpers on the texture-cache wrapper rather than on backend `Image`.
- Validation after the upload/rescale ordering fix:
  - `cargo fmt --check` and `git diff --check` pass.
  - `cargo build --release --bin ruzu-cmd` passes with the usual pre-existing warnings.
  - `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture` passes.
  - deko3d depth-min remains full-frame non-black:
    `/tmp/ruzu_deko3d_upload_rescale.ppm` is `2560x1440`, bbox `x[0..2559] y[0..1439]`,
    `nonblack=3686400`.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_upload_rescale.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    `nonblack=2041193`.
- Pending join completion is stricter and closer to upstream `TextureCache<P>::JoinImages`:
  - Removed the whole-tail `pending_join_copy_tail_supported` preflight/requeue path.
  - Alias and GPU-modified overlap copies now check `copy_join_image(...)` and panic before
    deleting overlaps if the backend copy path cannot be performed.
  - GPU-modified overlap copy now panics if `new_image.TryFindBase(overlap.gpu_addr)` fails,
    matching upstream's `.value()`/assert-style fail-fast behavior instead of silently skipping.
  - The remaining structural divergence is the delayed Rust tail itself: upstream performs refresh,
    rescale, alias relations, copies, deletes, and register synchronously inside `JoinImages`; Rust
    still performs that sequence when the Vulkan wrapper has the GPU reader/backend context.
- Validation after strict pending-join copy handling:
  - `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`, and
    `cargo build --release --bin ruzu-cmd` pass.
  - `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture` passes.
  - deko3d depth-min remains full-frame non-black:
    `/tmp/ruzu_deko3d_join_strict.ppm` is `2560x1440`, bbox `x[0..2559] y[0..1439]`,
    `nonblack=3686400`, with no `unsupported pending` panic.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_join_strict.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    `nonblack=2041193`, with no `unsupported pending` panic.
- Render-target update now performs the upstream `PrepareImageView(..., true, invalidate)` edge
  before framebuffer assembly. `TextureCache::update_render_targets_and_get_rt0_framebuffer`
  finishes pending join copies with the available GPU reader, prepares all bound color and
  depth/stencil views through `prepare_image_with_reader`, and computes the full-clear invalidate
  condition from the clear scissor tuple passed by `renderer_vulkan/mod.rs`. This matches
  upstream `TextureCache<P>::UpdateRenderTargets` more closely than the previous path, where RT
  images could be materialized without first running `RefreshContents`/alias synchronization or
  clear invalidation.
- Validation after the render-target prepare edge:
  - `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo build --release --bin ruzu-cmd`, and
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`
    pass.
  - deko3d depth-min remains full-frame non-black:
    `/tmp/ruzu_deko3d_rt_prepare.ppm` is `2560x1440`, bbox `x[0..2559] y[0..1439]`,
    `nonblack=3686400`.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_rt_prepare.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    `nonblack=2041193`.
- Remaining render-target fidelity gaps after this edge:
  - Queued pending join tails still complete when the Vulkan wrapper has a reader, while upstream
    completes the same sequence synchronously inside `JoinImages`.
  - `IsFullClear` is represented by a reduced scissor tuple in the wrapper instead of direct
    Maxwell register ownership inside the common cache.
  - `RescaleRenderTargets` viewport/scissor dirty propagation still needs a separate upstream
    audit.
- Pending join tail retry behavior was tightened again after upstream comparison:
  - Removed the residual requeue path for sibling `ScaleUp`/`ScaleDown`,
    `RefreshContents(new_image)`, and new-image `ScaleUp`/`ScaleDown` failures.
  - These cases now panic with the affected `ImageId`, because upstream `JoinImages` performs the
    sequence synchronously and has no recoverable retry path at these points.
  - This does not remove the larger split-backend divergence: Rust still completes queued tails
    later when the Vulkan wrapper has backend services and a GPU reader.
- Validation after removing pending-join requeue:
  - `cargo fmt`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete with no pending-join panic:
    `/tmp/ruzu_pinball_join_no_requeue.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    `nonblack=2041193`.
  - deko3d depth-min remains full-frame non-black with no pending-join panic:
    `/tmp/ruzu_deko3d_join_no_requeue.ppm` is `2560x1440`, bbox `x[0..2559] y[0..1439]`,
    `nonblack=3686400`. Known MoltenVK D24 substitution/copy and primitive-restart warnings
    remain.
- Render-target/zeta insertion now completes backend join tails immediately at the Vulkan
  RT/zeta call edge:
  - `renderer_vulkan::TextureCache::update_render_targets_from_snapshot_with_dirty_flags_and_finish`
    mirrors the common render-target snapshot logic but calls
    `find_or_insert_image_from_info_with_options_and_finish` for each color RT and zeta image
    before creating/binding the image view.
  - `TextureCacheBase::bind_color_render_target` and `bind_depth_render_target` were exposed as
    `pub(crate)` so the Vulkan wrapper can keep common-cache binding/preemptive-download behavior
    while owning backend completion.
  - This narrows the previous delay where render-target `JoinImages` tails were completed only
    after the whole base update returned.
- Validation after immediate RT/zeta backend completion:
  - `cargo fmt`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_rt_immediate_join.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    `nonblack=2041193`.
  - deko3d depth-min remains full-frame non-black:
    `/tmp/ruzu_deko3d_rt_immediate_join.ppm` is `2560x1440`, bbox `x[0..2559] y[0..1439]`,
    `nonblack=3686400`. Known MoltenVK D24 substitution and primitive-restart warnings remain.
- Remaining RT-path fidelity gaps:
  - Port/audit `RescaleRenderTargets` and its `Dirty::RescaleViewports` /
    `Dirty::RescaleScissors` side effects.
- Upstream `FindRenderTargetView` retry behavior is now ported for Vulkan RT/zeta lookups:
  - `find_or_insert_render_target_image_with_retry` saves `has_deleted_images`, clears it for
    each RT/zeta insertion attempt, calls the backend-completing find/insert helper, ORs deletion
    state into the saved state, repeats while deletion occurred, then restores the accumulated
    state before view creation.
  - This covers color render targets and zeta in
    `update_render_targets_from_snapshot_with_dirty_flags_and_finish`.
- Validation after RT retry loop:
  - `cargo fmt --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_rt_retry.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    `nonblack=2041193`.
  - deko3d depth-min remains full-frame non-black:
    `/tmp/ruzu_deko3d_rt_retry.ppm` is `2560x1440`, bbox `x[0..2559] y[0..1439]`,
    `nonblack=3686400`. Known MoltenVK D24 substitution and primitive-restart warnings remain.
- Vulkan render-target rescale behavior is now ported at the RT/zeta wrapper edge:
  - `update_render_targets_from_snapshot_with_dirty_flags_and_finish` takes a mutable dirty flag
    snapshot, clears `RenderTargets`/`RenderTargetControl`/per-RT/zeta flags, and sets
    `RescaleViewports`, `RescaleScissors`, and `DepthBiasGlobal` like upstream
    `TextureCache<P>::UpdateRenderTargets`.
  - `rescale_current_render_targets` mirrors upstream `TextureCache<P>::RescaleRenderTargets`:
    it checks each bound color RT and zeta view, computes `can_rescale`, `any_rescaled`, and
    `scale_rating`, scales RT images up/down, loops while `has_deleted_images`, and updates
    `scale_rating`/`scale_tick`.
  - Remaining structural gap: mutations apply to per-draw/per-clear snapshots rather than the
    live Maxwell dirty table. This is currently hidden by conservative Vulkan state invalidation,
    but should be revisited when dirty tracking becomes less conservative.
  - Validation after render-target rescale parity:
    `cargo fmt --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    `cargo build --release --bin ruzu-cmd`, and `git diff --check` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_rescale_rt.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    `nonblack=2041193`. Known MoltenVK primitive-restart warnings remain.
  - deko3d depth-min remains full-frame non-black:
    `/tmp/ruzu_deko3d_rescale_rt.ppm` is `2560x1440`, bbox `x[0..2559] y[0..1439]`,
    `nonblack=3686400`. Known MoltenVK D24 substitution and primitive-restart warnings remain.
- Upload ownership narrowed:
  - `Image::upload_memory` now records `CopyBufferToImage` against `original_image.handle()`,
    matching upstream `Vulkan::Image::UploadMemory`'s `const VkImage vk_image = *original_image`.
  - The rescaled upload sequence is now backend `Image` owned: `Image::upload_memory` checks
    `RESCALED`, performs upload-local `ScaleDown(true)`, records the upload, then performs
    `ScaleUp()` using backend-owned scale views/framebuffers when a scale helper is needed.
  - `refresh_contents_with_reader` no longer orchestrates scale around the upload; it temporarily
    removes the backend `Image` from the map to satisfy Rust borrowing, calls
    `Image::upload_memory(&mut runtime, ...)`, then synchronizes backend flags/`has_scaled` back
    to the common slot image.
  - Remaining ownership cleanup: non-upload scale call sites still use wrapper methods
    (`scale_up_image` / `scale_down_image`) for join/render-target/alias paths. Those should move
    to one backend `Image::ScaleUp` / `ScaleDown` owner in a later slice.
  - Validation after the upload target correction:
    `cargo fmt --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    `cargo build --release --bin ruzu-cmd`, and `git diff --check` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_image_upload_owner.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    `nonblack=2041193`. Known MoltenVK primitive-restart warnings remain.
- Backend scale ownership narrowed:
  - `Image::scale_up` / `Image::scale_down` now own the upstream `Vulkan::Image::ScaleUp` /
    `ScaleDown` behavior for all scale call sites, including lazy scaled-image allocation,
    `current_image` switching, `RESCALED` flag updates, and `BlitScaleHelper` dispatch.
  - `TextureCache::scale_up_image` / `scale_down_image` are now thin common/backend bridges that
    ensure the backend image exists, temporarily remove it from the `images` map for Rust borrow
    safety, call the backend `Image` method, then synchronize flags/`has_scaled` back to the
    common `slot_images` entry.
  - The transient black-frame regression from stale backend/common `ImageBase` state was fixed by
    refreshing `image.base` from `TextureCacheBase::slot_images[image_id]` before backend upload
    and scale calls.
  - Validation after backend scale ownership:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`, and
    `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_image_scale_owner_sync.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[187..1711] y[0..1079]`,
    `nonblack=2041193`.
- Remaining scale fidelity gap:
  - Fixed: upstream `TextureCache<P>::ScaleUp/ScaleDown` `InvalidateScale(image)` is now mirrored
    by `TextureCacheBase::invalidate_scale` plus Vulkan backend view/framebuffer retirement at the
    wrapper scale call edge.
  - `invalidate_scale` updates `scale_tick`, clears render-target refs, removes image-view refs
    and framebuffer keys, sentences common image views, clears image view lists/infos, invalidates
    active channel image-view tables, and marks `has_deleted_images`, matching upstream ordering.
  - The Vulkan wrapper retires backend `ImageView` and dependent backend `Framebuffer` owners for
    those view IDs without deleting the backend `Image`, preserving the upstream distinction
    between scale invalidation and image deletion.
  - Validation after scale invalidation:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo build --release --bin ruzu-cmd`,
    `cargo test -p video_core texture_cache::texture_cache::tests::invalidate_scale_removes_views_framebuffers_and_marks_deleted -- --nocapture`,
    and
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`
    pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_invalidate_scale.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[187..1711] y[0..1079]`,
    `nonblack=2041193`.
- Scale memory accounting now mirrors upstream `TextureCache<P>::ScaleUp`:
  - `TextureCache::scale_up_image` captures `had_scaled_copy` before calling backend
    `Image::scale_up`.
  - After a successful scale, `TextureCacheBase::account_scale_up_memory` adds
    `scaled_image_memory_size(image)` to `total_used_memory` only when no scaled copy existed
    before the scale, then scale invalidation runs as before.
  - `scaled_image_memory_size` is the Rust counterpart of upstream `GetScaledImageSizeBytes`:
    `max(guest_size_bytes, unswizzled_size_bytes) * up_scale^2 >> down_shift^2`, aligned to
    1024 bytes.
  - Validation after scale memory accounting:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo build --release --bin ruzu-cmd`,
    `cargo test -p video_core texture_cache::texture_cache::tests::account_scale_up_memory_adds_scaled_size_once -- --nocapture`,
    `cargo test -p video_core texture_cache::texture_cache::tests::invalidate_scale_removes_views_framebuffers_and_marks_deleted -- --nocapture`,
    and
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`
    pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_scale_memory.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[187..1711] y[0..1079]`,
    `nonblack=2041193`.
- Texture download wiring now mirrors the upstream Vulkan texture-cache owner:
  - `TextureCacheRuntime::download_staging_buffer` maps to upstream
    `TextureCacheRuntime::DownloadStagingBuffer(size, deferred)`, using the current
    download `StagingBufferPool` path. The `deferred` flag is intentionally ignored until
    `FreeDeferredStagingBuffer` exists in Rust.
  - `Image::download_memory` owns upstream `Vulkan::Image::DownloadMemory`: rescale down,
    scheduler outside-render-pass request, `GENERAL -> TRANSFER_SRC_OPTIMAL` read barrier,
    `cmd_copy_image_to_buffer`, memory/image write barriers back to `GENERAL`, then
    `ScaleUp(true)` when needed.
  - `TextureCache::download_memory` owns upstream `TextureCache<P>::DownloadMemory` for the
    texture cache: collect safe images, clear `GPU_MODIFIED`, sort by modification tick, allocate
    download staging, download full image copies, `runtime.finish()`, then swizzle the mapped
    staging bytes back to guest memory.
  - `RasterizerVulkan::flush_region` now calls the Vulkan texture-cache download path before
    query-cache flushing. Vulkan buffer-cache download is still missing and intentionally not
    faked in this texture-cache slice.
  - Validation after texture download wiring:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo build --release --bin ruzu-cmd`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and
    `cargo test -p video_core texture_cache::texture_cache::tests::account_scale_up_memory_adds_scaled_size_once -- --nocapture`
    pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_download.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    guest approx `x[187..1711] y[0..1079]`, `nonblack=2041193`.
- Immediate next fidelity gaps:
  - Complete exact scheduler-backed staging reuse (`scheduler.IsFree`, per-usage/log2 caches,
    `ReleaseCache`) now that the `FreeDeferredStagingBuffer` owner edge exists.
  - Port Vulkan buffer-cache `DownloadMemory` and route it from `RasterizerVulkan::flush_region`.
  - Port/audit exact `TextureCache<P>::GetFlushArea` minimization.
  - Add a workload that actually exercises GPU-modified texture download/readback; Pinball only
    proves non-regression of the live render path.
- Deferred staging ownership is now partially ported:
  - Vulkan `StagingBuffer` carries upstream `StagingBufferRef` identity fields: usage, log2 level,
    unique index, and deferred state.
  - `StagingBufferPool::request_download_buffer(size, deferred)` forwards the deferred flag, owns
    dedicated staging entries, and returns copyable handles instead of transferring ownership.
  - `StagingBufferPool::free_deferred` and
    `TextureCacheRuntime::free_deferred_staging_buffer` now provide the upstream
    `FreeDeferredStagingBuffer` call edge.
  - Remaining difference: the pool still lacks `Scheduler&`, so it cannot do upstream
    `scheduler.IsFree(entry.tick)`. To avoid unsafe reuse, only buffers explicitly released by
    `free_deferred` are currently marked reusable. Exact per-usage/log2 caches, iterate/delete
    indices, and `ReleaseCache` are still pending.
  - Validation after deferred staging ownership:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo build --release --bin ruzu-cmd`,
    `cargo test -p video_core renderer_vulkan::staging_buffer_pool::tests::log2_ceil_matches_staging_size_classes -- --nocapture`,
    and
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`
    pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_staging_deferred.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[187..1711] y[0..1079]`,
    `nonblack=2041193`.
- Updated next fidelity gaps:
  - Port exact per-usage/log2 staging cache arrays, `iterate_index`, `delete_index`, stream
    region sync tracking, and full `ReleaseCache` structure.
  - Wire the common `BufferCache<P>` to the new Vulkan `BufferCacheRuntime`, then route
    `RasterizerVulkan::flush_region` to buffer-cache `DownloadMemory`.
- Scheduler-backed staging reuse is now partially ported:
  - `StagingBufferPool` receives a stable pointer to `Scheduler`, matching upstream's
    scheduler-owned staging lifecycle.
  - Dedicated staging entries store a tick; normal allocations use `pending_tick()` as Rust's
    closest equivalent to upstream current command-buffer tick, while deferred allocations use
    `u64::MAX`.
  - `try_get_reserved_buffer` now reuses entries only when `!deferred` and
    `scheduler.is_free(entry.tick)`, replacing the temporary conservative `reusable` flag.
  - `new_frame` advances a delete level and releases up to 16 free dedicated entries from that
    level, matching upstream `ReleaseLevel` cadence at a simplified storage-layout level.
  - Remaining difference: Rust still stores all staging entries in one Vec filtered by usage/log2,
    and stream-buffer region tracking is still simplified.
  - Validation after scheduler-backed staging:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo build --release --bin ruzu-cmd`,
    `cargo test -p video_core renderer_vulkan::staging_buffer_pool::tests::log2_ceil_matches_staging_size_classes -- --nocapture`,
    and
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`
    pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_scheduler_pool.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    guest approx `x[187..1711] y[0..1079]`, `nonblack=2041193`.
- Vulkan `BufferCacheRuntime` service owner is now started:
  - `renderer_vulkan::buffer_cache.rs` now contains a `BufferCacheRuntime` implementing the
    common `buffer_cache_base::BufferCacheRuntime` trait in the upstream-owned Vulkan file.
  - Implemented services: backend buffer materialization, upload/download staging,
    `FreeDeferredStagingBuffer`, `Finish`, `PreCopyBarrier`, `PostCopyBarrier`, scheduler-recorded
    `CopyBuffer`, and scheduler-recorded `ClearBuffer`.
  - Because the common Rust `BufferBase` still stores `gpu_handle: u32`, the runtime uses an
    internal `u32 -> VkBuffer` table as the bridge until the backend buffer object is parameterized
    like upstream `TextureCache<P>` / `BufferCache<P>`.
  - Remaining difference: the Vulkan rasterizer still uses the legacy direct buffer cache helper;
    the new runtime is not yet wired into `RasterizerVulkan`. Descriptor/vertex/uniform/storage/
    texture/image/transform-feedback binding methods are still not ported in this runtime.
  - Validation after adding runtime services:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo build --release --bin ruzu-cmd`,
    `cargo test -p video_core renderer_vulkan::staging_buffer_pool::tests::log2_ceil_matches_staging_size_classes -- --nocapture`,
    and
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`
    pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_vk_buffer_runtime.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[187..1711] y[0..1079]`,
    `nonblack=2041193`.
- Vulkan common buffer-cache policy is now present:
  - `renderer_vulkan::buffer_cache::BufferCacheParams` matches upstream
    `Vulkan::BufferCacheParams` constants.
  - `VulkanCommonBufferCache = BufferCache<BufferCacheParams, VulkanDeviceTracker>` mirrors
    upstream `using BufferCache = VideoCommon::BufferCache<BufferCacheParams>`.
  - `VulkanDeviceTracker` is currently no-op, matching the current Rust OpenGL tracker level.
  - Validation after adding the policy/alias:
    `cargo fmt --check`, `cargo check -p video_core`,
    `cargo test -p video_core renderer_vulkan::buffer_cache::tests::buffer_cache_params_match_upstream_vulkan -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_vk_buffer_params.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    guest approx `x[187..1711] y[0..1079]`, `nonblack=2041193`.
- Immediate next buffer-cache step:
  - Add a second field in `RasterizerVulkan` for `VulkanCommonBufferCache`, install
    `BufferCacheRuntime`, and migrate one non-draw lifecycle edge first (`tick_frame` or
    `flush_region`) before replacing draw-time binding. Avoid replacing the legacy direct helper
    until Vulkan runtime descriptor/binding methods are ported.
- Vulkan common buffer-cache is now partially wired into `RasterizerVulkan`:
  - `RasterizerVulkan` owns `common_buffer_cache: VulkanCommonBufferCache` in parallel with the
    legacy direct helper.
  - Constructor installs a Vulkan `BufferCacheRuntime` on the common cache.
  - `flush_region` now calls common `download_memory` in upstream order: texture, buffer, query.
  - `tick_frame` now ticks the common cache.
  - Remaining difference: no channel/gpu/device memory adapters are wired yet, so common
    `download_memory` is currently a safe lifecycle/order edge rather than complete behavior.
    Draw-time bindings still use the direct helper.
  - Validation after common buffer-cache lifecycle wiring:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo build --release --bin ruzu-cmd`, and
    `cargo test -p video_core renderer_vulkan::buffer_cache::tests::buffer_cache_params_match_upstream_vulkan -- --nocapture`
    pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_common_buffer_wire.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[187..1711] y[0..1079]`,
    `nonblack=2041193`.
- Immediate next buffer-cache step:
  - Port Vulkan GPU/device memory adapters and channel binding into `common_buffer_cache`, then
    make `flush_region` behavior real rather than no-op. Only after that migrate draw-time
    binding methods off the legacy helper.
- Vulkan common buffer-cache memory access is now wired:
  - `renderer_vulkan/mod.rs` has private `GpuMemoryAccessAdapter` and `DeviceMemoryAccessAdapter`
    equivalents for the common buffer cache.
  - `DeviceMemoryAccessAdapter` wraps the constructor-owned `Arc<MaxwellDeviceMemoryManager>`,
    matching upstream `buffer_cache(device_memory, buffer_cache_runtime)` ownership.
  - `bind_channel` installs `GpuMemoryAccessAdapter` from the active channel's `MemoryManager` and
    initializes the common buffer-cache channel state.
  - Remaining difference: draw-time bindings still use the legacy direct helper, and the common
    cache still lacks engine-state adapters plus Vulkan descriptor/vertex/uniform/storage binding
    methods.
  - Validation after memory-adapter wiring:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo test -p video_core renderer_vulkan::buffer_cache::tests::buffer_cache_params_match_upstream_vulkan -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_common_buffer_memory.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[188..1712] y[0..1079]`,
    `nonblack=2041193`.
- Immediate next buffer-cache step:
  - Port Vulkan engine-state adapters into `common_buffer_cache` so `UpdateIndexBuffer`,
    `UpdateVertexBuffers`, cbuf/ssbo lookup, and indirect paths can read Maxwell3D/KeplerCompute
    state through the common cache.
  - Then port runtime binding methods from `vk_buffer_cache.cpp` and migrate one draw-time binding
    path at a time off the legacy direct helper.
- Next concrete suspects:
  - Primitive restart handling. Main graphics pipeline creation gates `primitiveRestartEnable`
    like upstream, but dynamic `cmd_set_primitive_restart_enable` currently forwards the guest
    value directly and utility/present/blit pipelines set primitive restart to false. MoltenVK
    repeatedly warns that Metal does not support disabling primitive restart.
  - Backend join-tail ordering is still not fully upstream-synchronous. Non-tail insertion is now
    registered immediately; queued copy/delete tails still need a complete upstream-ordered audit.
- Verify `Image::BlitScaleHelper` depth/stencil on the deko3d workload only after the basic
  color+depth draw renders correctly. Pinball still does not exercise this path.
- Finish `BlitImageHelper` parity beyond the current color/depth-stencil blits: conversion
  helpers, clear helpers, state-tracker invalidation, and exact descriptor allocator commit timing.
- Blit helper scheduler invalidation is now partially ported:
  - `Scheduler` can hold a stable `StateTracker` pointer, mirroring upstream
    `Scheduler(const Device&, StateTracker&)` ownership.
  - `RasterizerVulkan` boxes `StateTracker` and installs it into the scheduler before creating
    runtime/helper services.
  - `BlitImageHelper::blit_color`, `blit_depth_stencil`, `convert`, and
    `convert_depth_stencil` now call `scheduler.invalidate_state()` after recording their helper
    draw, matching upstream `blit_image.cpp`.
  - Remaining difference: Rust scheduler does not yet own upstream-local
    `state.graphics_pipeline` / `state.rescaling_defined`, so `invalidate_state` currently only
    invalidates `StateTracker` command-buffer state. Clear helpers are still reduced and still
    need their own upstream draw + invalidation paths.
  - Validation after helper scheduler invalidation:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_scheduler_invalidate.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[188..1712] y[0..1079]`,
    `nonblack=2041193`.
- Immediate next texture-cache/runtime step:
  - Port scheduler-local graphics-pipeline/rescaling state enough for
    `Scheduler::InvalidateState` to clear the same fields as upstream, then finish the reduced
    clear helper paths and their invalidation calls.
- Scheduler-local invalidation and clear helper draw paths are now ported:
  - `Scheduler` has a local `SchedulerState` carrying the upstream invalidation fields
    `graphics_pipeline`, `is_rescaling`, and `rescaling_defined`.
  - `Scheduler::invalidate_state` now clears `graphics_pipeline` and `rescaling_defined`, then
    invalidates `StateTracker`, matching upstream `Scheduler::InvalidateState` ordering.
  - `BlitImageHelper::clear_color` now mirrors upstream `ClearColor`: uses
    `Operation::BlendPremult`, requests the destination render pass, records pipeline bind, blend
    constants from `color_mask`, viewport/scissor, fragment push constants, draw, then scheduler
    invalidation.
  - `BlitImageHelper::clear_depth_stencil` now mirrors upstream `ClearDepthStencil`: builds the
    depth/stencil key, requests render pass, records blend constants zero, pipeline bind,
    viewport/scissor, fragment depth push constant, draw, then scheduler invalidation.
  - `FindOrEmplaceClearColorPipeline` and `FindOrEmplaceClearStencilPipeline` now create real
    Vulkan graphics pipelines with the upstream shader modules and blend/depth/stencil states
    instead of caching `VK_NULL_HANDLE`.
  - Rust uses a separate `bind_clear_state` helper because the existing Rust `bind_blit_state`
    pushes vertex-stage blit constants, while upstream clear paths only set viewport/scissor
    before fragment-stage clear push constants.
  - Remaining differences: `Scheduler::UpdateGraphicsPipeline` and `Scheduler::UpdateRescaling`
    are still not ported, so the producer side of `graphics_pipeline`/`is_rescaling` is missing;
    `BlitImageHelper` still reaches state invalidation through `Scheduler` rather than owning its
    own `StateTracker&` member; Pinball does not exercise depth/stencil clear.
  - Validation after clear helper port:
    `cargo fmt`, `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_clearhelpers.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    guest approx `x[188..1712] y[0..1079]`, `nonblack=2041193`.
- Immediate next texture-cache/runtime step:
  - Port upstream `Scheduler::UpdateGraphicsPipeline` / `UpdateRescaling` producer paths or, if
    staying inside texture-cache first, continue with the next `BlitImageHelper` parity gap that
    is still directly used by `TextureCacheRuntime` (descriptor allocator commit timing,
    ASTC/runtime ownership, then remaining upload/download/blit/convert/rescale completion).
- Port ASTC runtime ownership and the remaining descriptor-template/update-data parity.
- After that, port full upload/download/blit/convert/rescale completion as upstream
  `vk_texture_cache.cpp`.
- Upload barrier parity tightened:
  - `Image::upload_memory` still records through the scheduler and
    `TextureCacheRuntime::insert_upload_memory_barrier` remains intentionally empty like upstream
    Vulkan `InsertUploadMemoryBarrier`.
  - The recorded upload barriers now match upstream file-local `CopyBufferToImage`: pre-barrier
    source access is shader/color/depth-stencil writes, post-barrier destination access is
    write+read access, and the subresource range uses `VK_REMAINING_MIP_LEVELS` /
    `VK_REMAINING_ARRAY_LAYERS`.
  - Remaining difference: Rust still inlines `CopyBufferToImage` inside `Image::upload_memory`
    instead of a named file-local helper; behavior is aligned, ownership/auditability can still be
    improved.
  - Validation after upload-barrier parity:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_uploadbarrier.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    guest approx `x[188..1712] y[0..1079]`, `nonblack=2041193`.
- Upload helper ownership parity tightened:
  - `Image::upload_memory` now delegates the scheduler-recorded barrier/copy sequence to a
    file-local `copy_buffer_to_image` helper, mirroring upstream `CopyBufferToImage` in
    `vk_texture_cache.cpp`.
  - The helper owns the same write/read access masks, initialized/undefined layout choice,
    `TRANSFER_DST_OPTIMAL` copy layout, `VK_REMAINING_MIP_LEVELS` /
    `VK_REMAINING_ARRAY_LAYERS` range, and pre/post pipeline-barrier ordering as upstream.
  - Validation after helper extraction:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_copy_buffer_to_image.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[188..1712] y[0..1079]`,
    `nonblack=2041193`.
- Immediate next texture-cache/runtime step:
  - Continue with ASTC accelerated/asynchronous upload ownership in
    `TextureCacheRuntime` / `refresh_contents_with_reader`, or audit the next concrete drift in
    `finish_pending_join_copies` against upstream join/alias/rescale ordering. Pinball cannot
    validate depth/MRT or ASTC-specific behavior, so keep the deko3d depth workload as the
    runtime validation target for those paths.
- ASTC accelerated upload ownership is now partially ported:
  - `video_core/build.rs` now generates `ASTC_DECODER_COMP_SPV` from upstream
    `astc_decoder.comp`, and `spirv_shaders.rs` includes it in the generated-SPIR-V sanity test.
  - `TextureCacheRuntime` owns `Option<AstcDecoderPass>`, matching upstream
    `TextureCacheRuntime::astc_decoder_pass`.
  - `refresh_contents_with_reader` now handles `ImageFlagBits::ACCELERATED_UPLOAD` before the
    CPU unswizzle path: it reads raw guest bytes into a Vulkan upload staging buffer, builds
    `full_upload_swizzles`, and calls `TextureCacheRuntime::accelerate_image_upload`.
  - `TextureCacheRuntime::accelerate_image_upload` creates per-level ASTC storage views with
    `VK_FORMAT_A8B8G8R8_UNORM_PACK32`, exchanges image initialization, delegates to
    `AstcDecoderPass::assemble`, and leaves the image in `GENERAL`.
  - `AstcDecoderPass::assemble` now mirrors upstream barriers and dispatch ordering:
    outside-renderpass request, initial image barrier, compute pipeline bind, per-swizzle
    descriptor set update, push constants from `MakeBlockLinearSwizzle2DParams`, dispatch, final
    shader-write barrier, then `scheduler.finish()`.
  - Difference still tracked in `DIFF.md`: Rust updates descriptor sets directly instead of
    consuming `ComputePassDescriptorQueue::update_data()` through a descriptor update template.
    Descriptor contents/order match this pass, but descriptor-template ownership is not fully
    upstream-shaped.
  - Validation after ASTC accelerated upload ownership:
    `cargo fmt`, `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_astc_upload.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    guest approx `x[188..1712] y[0..1079]`, `nonblack=2041193`.
- Remaining upload gaps before claiming upstream-faithful upload:
  - Port `QueueAsyncDecode` / async decode completion lifecycle for
    `ImageFlagBits::ASYNCHRONOUS_DECODE`; Vulkan Rust still returns `false` for this flag.
  - Validate the accelerated ASTC path with an actual ASTC-using workload; Pinball only proves
    non-regression.
- Converted synchronous upload path is now ported:
  - `refresh_contents_with_reader` now mirrors upstream `UploadImageContents` for
    `ImageFlagBits::CONVERTED`: allocate an unswizzled temporary buffer, call `unswizzle_image`
    into it, call `convert_image` into the mapped upload-sized buffer, then upload using the
    converted `BufferImageCopy` list.
  - Non-converted uploads still unswizzle directly into the upload buffer, matching the upstream
    `else` branch.
  - Validation after converted upload wiring:
    `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_converted_upload.ppm` is `2560x1440`, bbox
    `x[250..2282] y[0..1439]`, guest approx `x[188..1712] y[0..1079]`,
    `nonblack=2041193`.
- Remaining upload gaps before claiming upstream-faithful upload:
  - Validate accelerated/converted ASTC or BCn paths with a real compressed-texture workload;
    Pinball only proves non-regression.
- Async decode queue/tick is now ported:
  - Vulkan `TextureCache` owns `ThreadWorker::new_named(1, "TextureDecoder")`.
  - `refresh_contents_with_reader` reads guest bytes for `ASYNCHRONOUS_DECODE`, calls
    `queue_async_decode`, consumes `CPU_MODIFIED`, tracks the image, and returns.
  - `queue_async_decode` mirrors upstream: requires `CONVERTED`, sets `IS_DECODING`, pushes an
    `AsyncDecodeContext`, unswizzles into temporary data, queues worker `convert_image`, stores
    decoded data/copies, and marks complete.
  - `tick_async_decode` is called from Vulkan `tick_frame` before runtime/base frame ticks. It
    uploads completed decoded data through backend `Image::upload_memory`, clears
    `IS_DECODING` on success, removes completed decode contexts, and calls
    `insert_upload_memory_barrier` if any uploads completed.
  - Difference still tracked in `DIFF.md`: Rust upload is fallible; if async upload fails, the
    decode context is removed and `IS_DECODING` may remain set. Upstream has no equivalent
    fallible path, so this needs a later Vulkan upload-error policy.
  - Validation after async decode queue/tick:
    `cargo fmt`, `cargo fmt --check`, `git diff --check`, `cargo check -p video_core`,
    `cargo test -p video_core texture_cache::texture_cache::tests::backend_completed_join_registers_common_cache_immediately_like_upstream -- --nocapture`,
    and `cargo build --release --bin ruzu-cmd` pass.
  - SpaceCadetPinball-NX remains complete:
    `/tmp/ruzu_pinball_async_decode.ppm` is `2560x1440`, bbox `x[250..2282] y[0..1439]`,
    guest approx `x[188..1712] y[0..1079]`, `nonblack=2041193`.
- Remaining upload validation gap:
  - Validate accelerated/converted/async compressed-texture paths with a real ASTC or BCn
    workload; Pinball only proves non-regression.
- Immediate next texture-cache/runtime step:
  - Resume the main objective at `finish_pending_join_copies`: audit the current Vulkan Rust
    implementation against upstream `TextureCache<P>::JoinImages` / pending join completion order,
    focusing on refresh contents, sibling/new image rescale, alias relations, and GPU-modified
    copies. Keep the deko3d depth workload for depth/MRT validation after the join path audit.
- Pending join tail audit against upstream `TextureCache<P>::JoinImages`:
  - Upstream tail order after overlap discovery is:
    can-rescale gate over copy siblings; scale siblings up/down; insert new image; delete ignored
    overlaps; `RefreshContents(new_image)`; scale new image up/down; sort copies by sibling
    `modification_tick`; apply right/left alias relations and bad-overlap relations; for each
    copy, copy alias contents when safe, copy GPU-modified overlaps with `MakeShrinkImageCopies`
    and MSAA/non-MSAA runtime routing, then untrack/unregister/delete non-alias overlaps; finally
    `RegisterImage(new_image_id)`.
  - Current Vulkan backend tail in `finish_pending_join_copies_with_reader` already mirrors this
    order for queued tails:
    `prepare_pending_join_sibling_rescale_gate`, sibling scale up/down,
    `refresh_contents_with_reader(new_image_id)`, new image scale up/down,
    `apply_join_relations`, alias copies through `copy_join_image`, GPU-modified copies through
    `make_shrink_image_copies` + `copy_join_image`, `delete_join_overlap_image`, then
    `register_completed_backend_image`.
  - Current common-cache split is intentional and matches the backend ownership constraint:
    `TextureCacheBase::join_images` queues `PendingJoinCopies` only when `join_copies_to_do` is
    non-empty. Non-tail insertions are completed through
    `finish_pending_backend_insertion_with_reader`, which calls `refresh_contents_with_reader`
    and then `register_completed_backend_image`.
  - No code change was made for this audit pass because the audited order is already aligned with
    upstream for the queued-tail path.
  - Remaining risks are validation, not obvious code-order drift:
    alias/GPU-modified join copies need a real compressed/depth/MRT workload; Pinball does not
    exercise these branches.
- Immediate next texture-cache/runtime step:
  - Move from audit to validation/instrumentation: run or create a small workload that triggers
    GPU-modified overlap joins and depth/MRT, then compare traces against upstream. The existing
    deko3d depth workload is the current candidate, but it previously rendered black/device-lost
    and needs targeted instrumentation around render-target binding, clear, pipeline creation,
    draw state, and join-copy activity.

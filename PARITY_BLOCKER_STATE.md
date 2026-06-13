# 2026-06-13 Texture Cache Sparse Parity Blocker

## Active blocker — Texture cache render-target rescale parity

Status: partially resolved; keep open for the remaining owner/dirty-flag parity.

- Suspended implementation: continuing MK8D `Press L + R` invalid texture and black/overexposed cinematic investigation after restoring `UpdateRenderTargets` `surface_clip`/`render_targets.size` parity.
- Implemented prerequisite: `video_core/src/renderer_opengl/gl_texture_cache.rs` now runs an upstream-shaped render-target rescale pass from the OpenGL snapshot bridge before framebuffer lookup. It re-runs render-target discovery while `has_deleted_images` is raised, materializes backend images before `ScaleUp`/`ScaleDown`, applies the upstream scale-rating decision, updates `render_targets.is_rescaled`/scaled size, and invalidates the current channel's graphics/compute image descriptor tables when scaled views are destroyed.
- Remaining prerequisite: upstream `TextureCache<P>::InvalidateScale` also mutates Maxwell3D dirty flags (`RenderTargets`, `ZetaBuffer`, color buffers, rescale viewport/scissor/depth-bias paths) and iterates all `active_channel_ids`. Rust still has a single inline channel and the OpenGL texture cache does not own Maxwell3D dirty flags, so this is still a structural owner gap rather than a completed port.
- Additional prerequisite found while tracing the `Press L + R` source image: upstream `TextureCache<P>::JoinImages` runs the sibling-image rescale decision before creating/copying the joined image and uses the rescale factor in `MakeShrinkImageCopies`. Rust now applies that OpenGL backend portion while draining `PendingJoinCopies`, reader-backed draw/render-target drains now call `RefreshContents(new_image)` before rescale/copy, and pending-copy joins now delay `RegisterImage(new_image_id)` until after the deferred tail. Reader-less drains and exposure of the unregistered intermediate image id are still not upstream-equivalent.
- Required before resuming deeper ad-hoc rendering fixes: continue removing the documented GPU/texture DIFF debt, especially the `JoinImages` `RefreshContents` ordering/registration split and the remaining `InvalidateScale` dirty-flag owner gap, before adding visual-specific fixes.
- Resume point: build after the pending-join rescale fix, run MK8D via `/tmp/ruzu-run.sh`, dump to `screenshots/tmp`, inspect the `Press L + R` rectangle and later cinematic frames, then kill the launched instance.

## Active blocker — OpenGL compressed/raw alias copy fallback

Status: resolved pending MK8D visual re-test.

- Suspended implementation: investigating MK8D invalid `Press L + R` prompt texture after the compressed-to-compressed fallback reduced but did not remove the title-screen corruption.
- Missing prerequisite: the remaining `GL_INVALID_VALUE` copies are byte-compatible aliases between compressed `BC3` mips and raw `R32G32B32A32_UINT` textures. Upstream emits these through the same `TextureCacheRuntime::CopyImage` direct path, but Mesa rejects the raw/compressed `glCopyImageSubData` rectangle.
- Required before resuming: keep upstream direct copy first, then implement a narrow backend-local fallback that copies exact bytes for raw/compressed pairs with equal block/texel byte size.
- Resume point: rebuild, run MK8D via `/tmp/ruzu-run.sh`, capture the `Press L + R` rectangle, and inspect whether `[COPY_IMAGE_INVALID]` now reports `fallback=raw_compressed_block` instead of `none`.
- Resolution: `video_core/src/renderer_opengl/gl_texture_cache.rs` now detects equal-size raw/compressed alias pairs such as `BC3` 16-byte blocks and `R32G32B32A32_UINT` 16-byte texels. On direct-copy failure it reads the source with the correct raw or compressed GL API and writes the destination with the matching API, expanding compressed rectangles from upstream alias block units back to compressed GL pixel dimensions.

## Active blocker — OpenGL compressed sub-block copy fallback

Status: resolved.

- Suspended implementation: investigating MK8D invalid `Press L + R` texture and black/overexposed cinematic after the MSAA copy path was wired.
- Missing prerequisite: Rust follows upstream `TextureCacheRuntime::CopyImage` by dispatching `glCopyImageSubData` directly, but the current Mesa path rejects small-mip compressed texture copies whose pixel rectangle is smaller than a compression block or reaches a partial mip edge.
- Required before resuming: preserve the upstream copy ownership in `gl_texture_cache.rs`, then add a backend-local compressed-block fallback that reads the source compressed block and writes it to the destination with edge-clamped compressed dimensions when `glCopyImageSubData` returns `GL_INVALID_VALUE`.
- Resume point: re-run MK8D via `/tmp/ruzu-run.sh` and verify whether `[COPY_IMAGE_INVALID]` disappears and whether the title/cinematic textures improve.
- Resolution: `video_core/src/renderer_opengl/gl_texture_cache.rs` now keeps the upstream direct `CopyImage` path first, detects `GL_INVALID_VALUE` for compatible compressed block-layout textures, reads the source compressed block with `glGetCompressedTextureSubImage`, and writes it back with destination-edge dimensions via `glCompressedTextureSubImage3D`. The compatibility guard accepts only matching block-layout families such as BC1 unorm/sRGB and ASTC same-size unorm/sRGB. Focused block-rectangle/layout tests and `cargo build --release --bin ruzu-cmd` pass.

Status: resolved.

## Active blocker — OpenGL JoinImages MSAA copy path

- Suspended implementation: investigating MK8D invalid `Press L + R` texture and black/overexposed cinematic after sparse texture-cache parity.
- Missing prerequisite: upstream `TextureCacheRuntime::CopyImageMSAA` calls `UtilShaders::CopyMSAA(dst_image, src_image, copies)`, but Rust `TextureCache::copy_image` returned early on sample-count mismatch and Rust `UtilShaders` left the MSAA compute programs uncompiled.
- Required before resuming: compile the embedded upstream MSAA conversion shaders and wire `TextureCache::copy_image` to dispatch the MSAA copy path over the exact `ImageCopy` list.
- Resume point: re-run MK8D via `/tmp/ruzu-run.sh` and inspect whether `glCopyImageSubData`/MSAA-copy warnings disappear and whether the title/cinematic textures improve.
- Resolution: `video_core/src/renderer_opengl/util_shaders.rs` now compiles the embedded upstream MSAA conversion shaders and dispatches per `ImageCopy`; `video_core/src/renderer_opengl/gl_texture_cache.rs` now owns `UtilShaders` and routes sample-count mismatches through `copy_image_msaa`.

The texture-cache parity pass was stopped because `TextureCacheBase` lacked the
upstream `gpu_memory->GetSubmappedRange` / `IsContinuousRange` owner needed by
`RegisterImage` and `JoinImages`.

State at stop:
- `TextureCacheChannelInfo` now owns GPU and sparse page tables.
- Dense image registration updates `gpu_page_table`.
- `JoinImages` checks sparse GPU overlaps.
- Tests added for dense GPU registration and sparse exact-overlap deletion.

Resolution:
- Wire the channel `MemoryManager` into `TextureCacheBase`.
- Use `MemoryManager::get_submapped_range` to populate sparse map views,
  sparse page table entries, and CPU page-table entries.
- Use `MemoryManager::is_continuous_range` in `JoinImages` to mark joined
  sparse images like upstream.
- Re-run texture-cache tests and update `DIFF.md` with no remaining sparse gap
  in this slice.

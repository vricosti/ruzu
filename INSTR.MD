THIS FILE SHOULD NOT ME MODIFIED BY AN IA - ONLY BY A HUMAN

Mot clés a rechercher plus tard:
closely enough


● Before diving in, let me scope this properly — porting the lifecycle has deep dependency chains (image upload/download against
  the GL backend), and I want to honestly assess what's reachable.
     
  Searched for 2 patterns, read 1 file (ctrl+o to expand)

● Honest scope assessment before I start writing code: this is a much bigger fish than it looks, and I want you to make an informed
   call.

  Upstream surface (texture_cache.h is 2547 lines of templated implementation):
  - 18 stub methods in texture_cache.rs covering image lifecycle (refresh_contents, prepare_image, unregister_image, delete_image,
  join_images, blit_image, fill_graphics_image_views, fill_compute_image_views, find/insert/render-target paths, etc.)
  
  Hard dependencies that don't exist yet in ruzu:
  - Runtime::UploadStagingBuffer / DownloadStagingBuffer — GL persistent-mapped staging pool
  - Image::UploadMemory — glTexSubImage* per subresource, plus format-specific conversions (ASTC, BCn, depth-stencil)
  - Image::DownloadMemory (partial — see task #158 marked complete, but only a slice)
  - SwizzleTexture / UnswizzleTexture — Tegra block-linear ↔ linear conversion
  - LRU cache (lru_cache.Touch, lru_cache.MakeLRU)
  - Async ASTC decode pipeline (QueueAsyncDecode, TickAsyncDecode)
  - MSAA upload path / image-layout transitions

  Realistic estimate: ~2000-3000 lines of port work, spanning ~5-7 files, with the heaviest piece being block-linear ↔ linear
  swizzle (the Tegra texture format is non-trivial). Multi-day at minimum, and verifying correctness requires actually rendering
  frames — without a working GPU pipeline end-to-end, bugs are silent and hard to attribute.
  ----
  ● Definitive end-of-session conclusion:

  The MK8D wedge bottom-line, derived from this session's deep dive:
  
  1. SMMU port (commit bdced9b) ✅ — architectural fix, GPU now sees correct device-physical addresses matching zuyu (dev=0x41000).
  2. Diagnostic env vars (commit 0eaf05f) ✅ — RUZU_TRACE_HEAP_TRACKER_MAP, RUZU_TRACE_MACRO_HASH for future cross-emulator
  comparison work.
  3. Root cause identified — same cv 0x7F2C0BCC lost-wakeup:
    - tid=73's wedge state: WaitProcessWideKeyAtomic args=[0x7F2C0BC8, 0x7F2C0BCC, 0x81FF, 0xFFFFFFFF]
    - This is the IDENTICAL cv key from project_mk8d_pthread_cond_lost_wakeup_2026_05_13.md
    - The user's kernel fixes moved MK8D past the FIRST wedge instance (into GPU pipeline init). This is the SECOND iteration of
  the same worker-pool wedge.
    - Per prior diagnosis: kernel KScheduler::reschedule_other_cores IPI fires during KScopedSchedulerLockAndSleep drop; on some
  races the other core's dispatch loop misses the wake.
  
  The path forward: continue the multi-day kernel-scheduler investigation from project_mk8d_pthread_cond_lost_wakeup_2026_05_13.md.
   The SMMU work removed an architectural blocker but the upstream timing race is unchanged.

  Branch state:
  - 2 new commits ahead of ee9abbf
  - ~85 files in working tree still uncommitted (kernel, audio, shader_recompiler — from prior sessions)
  - zuyu source has 6 diagnostic patches (env-gated, harmless)
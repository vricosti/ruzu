the audio renderer never got
   its shared-memory pool.

  Let me chase what object_id=333 actually is and whether MK8D is stuck waiting for an event nobody signals.

  Searched for 1 pattern (ctrl+o to expand)

● Audio renderer not getting shared memory is real and confirmed at startup. Let me look at the impact.

---
● The findings narrow the picture. Let me check what specific HLE service MK8D keeps polling during this stall.

  Searched for 1 pattern (ctrl+o to expand)
  ⎿  Referenced file ../../../../../tmp/ruzu_cv_0x553be2d4_1778940177.log
  ⎿  Read ../../../../../tmp/pix3.log (59 lines)
  ⎿  Referenced file video_core/src/renderer_opengl/gl_shader_cache.rs
  ⎿  Read core/src/hle/service/nvnflinger/surface_flinger.rs (319 lines)
  ⎿  Read core/src/hle/service/nvnflinger/hardware_composer.rs (348 lines)

● I've got strong signal now. Two things stand out:

  1. tid=73's last call is an INFINITE WaitSync on object_id=333 (a readable_event) — the trace ends right there. That's likely the
   actual blocker.
  2. In /tmp/pix3.log line 47: audio_core::open_audio_renderer: system.memory_shared() returned None — the audio renderer never got
   its shared-memory pool.

  Let me chase what object_id=333 actually is and whether MK8D is stuck waiting for an event nobody signals.

  Searched for 1 pattern (ctrl+o to expand)

● Audio renderer not getting shared memory is real and confirmed at startup. Let me look at the impact.

  Searched for 1 pattern, read 1 file (ctrl+o to expand)

● The memory_shared() returned None only disables audio wave-buffer decoding (decoder fails silently), and the renderer initializes
   fine — so that's almost certainly not the rendering blocker.

  Let me reconcile what I actually know and ask before chasing more threads. Strong evidence from the trace work:

  - Kernel CV / IPC / renderer pipeline = healthy: ~52 FPS, 1286 frames presented, cv signal→wake chain works (user verified).
  - All 3 swapchain slots cycle "loading screen content" — MK8D produces frames but they're the same loading frame, never advances
  state.
  - tid=73's last visible SVC is an infinite WaitSync on object_id=333 (readable_event) — that thread is parked on an event nobody
  signals.
  - tid=102 spins in 100ms timeout polling on object_id=388 (vsync-style event).
  - Object 333 / 331 / 315 are each waited on once each by tid=73 — different events per call.

  So the game is running its render/audio loops, but it never gets the event(s) that transitions it past the loading state. That's
  what's blocking visible progress
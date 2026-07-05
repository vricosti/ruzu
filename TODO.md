# MK8D First-Logo Delay Investigation

## Symptom

In upstream yuzu, Mario Kart 8 Deluxe shows the first logo for roughly 3-4
seconds, then transitions with animation to the next logo while the audio
changes and Mario says "Mario Kart eight".

In ruzu, the first MK8D logo remains visible far too long. Current Vulkan
frames are color-correct, audio continues, and the emulator is not frozen.

## Reproduction Command

```bash
SCRATCH=/tmp/ruzu_mk8d_$(date +%s)
mkdir -p "$SCRATCH/data/ruzu" "$SCRATCH/cache" "$SCRATCH/config" "$SCRATCH/data/ruzu/shader"
for d in keys nand sdmc load; do
  ln -s "$HOME/.local/share/yuzu/$d" "$SCRATCH/data/ruzu/$d"
done
env XDG_DATA_HOME="$SCRATCH/data" \
    XDG_CACHE_HOME="$SCRATCH/cache" \
    XDG_CONFIG_HOME="$SCRATCH/config" \
    RUST_LOG=warn \
    RUZU_TRACE_SLEEP_BT_TID=79 \
    RUZU_TRACE_SLEEP_BT_NS=1000000 \
    RUZU_TRACE_MII_WAIT_CHANGES=1 \
    RUZU_TRACE_MII_RESOURCE_CHANGES=1 \
    /Users/vricosti/Dev/emulators/ruzu/target/release/ruzu-cmd \
    -g "/Users/vricosti/Games/Emulators/Switch/roms/Mario Kart 8 Deluxe [NSP]/Mario Kart 8 Deluxe [0100152000022000][v0].nsp"
```

Avoid `RUST_LOG=info` for timing comparisons: current `info` logging floods
Maxwell/GPU/service logs and measurably perturbs the logo-delay window.


## Verified Facts

- The old black-and-white / color-loss rendering issue is fixed; MK8D is
  color-correct through the first-logo period, and the emulator is not frozen.
- A32 `CNTPCT` is correct (~19.2 MHz), so the delay is not a slow guest counter.
- BufferQueue/HWC is not deadlocked: after pipeline preload MK8D queues buffers
  and ruzu presents frames continuously (QueueBuffer / HWC-acquire counts climb).
- Audio continues during the long first-logo period.
- A CoreTiming looping-event reschedule parity bug was found and fixed (ruzu
  rescheduled from `max(evt.time, now)`; upstream reschedules from `evt.time`).
  This restored ~59 Hz VSync cadence but did not fix the transition.
- The packed-RomFS-update registration divergence was fixed (`System::load`
  calls `SetPackedUpdate` after `register_process`). Real parity fix, not the
  cause; the tested v0 NSP is base content anyway.
- ruzu's Vulkan disk shader cache is NOT yuzu-cache compatible; ruzu must keep
  `ShaderDir` under its own root and never read/delete yuzu's `shader/`.
- The main guest wait is tid 79 sleeping in the SDK `SleepThread` wrapper
  (`pc=0x01D5034C`), polling a loading state machine (`base=0x5564C690`, fields
  `+0x4308`/`+0x4314`) that advances slowly (e.g. `w4314` 7 -> 3 -> 0xA) rather
  than being stuck. Later phases use 5-10ms sleeps at callsite `0x009CF410`.
- The hot guest work in the window is the MK8D Yaz0 decompressor
  (`0x00B92D80..0x00B93098`, inner copy loop `0x00B92E04`). Block relinking is
  active (not dispatcher-bound); generated ARM64 does 2 page-table lookups per
  copied byte (fastmem unavailable on this 16K-page host — same for yuzu).

## Fixes Already Applied (parity; none of these fixed the delay)

- Vulkan uniform-buffer staging hot path (`bind_mapped_uniform_buffer`),
  matching upstream `BindHostGraphicsUniformBuffer` / `BindMappedUniformBuffer`.
- `PresentManager::present` records the threaded present push via
  `Scheduler::record` (upstream `vk_present_manager.cpp::Present`).
- `ShaderDir` kept under the ruzu root even with legacy yuzu NAND/SDMC/keys.
- CoreTiming looping-event reschedule from `evt.time`.
- Scheduler `YieldWithCoreMigration` / `YieldToAnyThread` real migration ported.
- Packed-RomFS-update wiring into `RomFSFactory`.
- A32 dynarmic parity: `silently_mirror_{fastmem,page_table}=true` and
  `check_halt_on_memory_access = debugger || (cpu_debug_mode && !ignore_aborts)`.

## Invalidated Hypotheses (with results)

- Color-loss / black-and-white rendering: not reproduced; current problem is
  timing/progress, not color. INVALIDATED.
- A32 guest timer frequency (CNTPCT 31x slow): measured ~19.2 MHz. INVALIDATED.
- BufferQueue/HWC *deadlock*: queue/acquire/present continue after preload;
  DequeueBuffer waits line up with 60 Hz pacing. INVALIDATED as a deadlock.
  (NB: the DequeueBuffer *wait* is nonetheless the root cause — it freezes the
  guest core thread, not as a deadlock. See the dated ROOT CAUSE entry below.)
- Uniform-buffer fresh allocation as primary bottleneck: staging hot path added;
  reduced allocation pressure but did not restore upstream speed. INVALIDATED.
- Fastmem absence as the yuzu/ruzu delta: both ruzu and local yuzu-cmd lack
  fastmem on this 16K-page host, so it is a shared limitation, not the delta.
  INVALIDATED as a differential.
- Disk pipeline-cache preload/failures as sole cause: isolated empty-cache runs
  still reproduce the delay. INVALIDATED as sole cause (still a parity item).
- Shader / vertex-input mismatch as primary cause: real backend parity bug, but
  isolated runs still stall; delay is gated by guest resource data. INVALIDATED
  as primary.
- Missing Miidatabase: NAND has it, ruzu logs successful load. INVALIDATED.
- Initial Mii resource wait as the *complete* explanation: Mii entries clear by
  ~9-12s while the logo persists much longer. INVALIDATED as complete.
- Scheduler migration/fairness as the direct cause: the hot workers are
  guest-pinned to single cores (`affinity=0x1/0x2/0x4`), so idle cores are
  expected and not a missed migration. INVALIDATED (yield port kept as parity).
- CoreTiming looping-event drift as the *complete* explanation: fixed as parity,
  but the logo still stalls afterward. INVALIDATED as complete.
- SleepThread / guest sleep timing: sleeps are accurate (measured ratio ~1.0);
  the loop sleeps because its resource predicates are still false. INVALIDATED.
- Heavy logging / blocking memory-watch probes as reliable measurements:
  `RUST_LOG=info`, `RUZU_WATCH_ADDR`, forced legacy callback paths perturb
  timing and miss the page-table writer. INVALIDATED as tools (use warn-level
  changed-state logs / non-blocking traces).
- A32 dispatcher/hash lookup as the main bottleneck: direct-linked block
  prologue counts dwarf dispatcher/get-or-emit; not dispatcher-bound.
  INVALIDATED.
- A32 GetSetElimination missing for the hot loop: GSE is active in post-opt IR;
  remaining writes are live-out state. INVALIDATED.
- ARM64 memory lowering `read_w`/`write_w` truncates 64-bit ops: they are X-reg
  aliases; width is chosen by the memory emitter. INVALIDATED.
- Missing `w4308 & 0x40000` bit blocks state 7: the guest intentionally gates it
  off (`b430D & 1` false) for state 7. INVALIDATED for that state.
- State 7 plateau is the whole delay: a later, longer plateau is state `0xA`
  (`w430C=0x200`, `w4308=0x00203FFC`, ~31s..71s in one run). INVALIDATED —
  the delay tracks the full loading state machine, not one state.

## Prior Active Hypotheses (now understood as symptoms of the root cause)

The dated ROOT CAUSE entry below (buffer-queue `dequeue_buffer` freezes the
guest core thread ~84% of the time) explains these: the loading workers are
starved because their core is frozen, not because of their own logic. Kept for
audit:

- A32 JIT throughput of the Yaz0 worker (`0x00B92E04` inner copy loop) is low:
  real absolute cost, but secondary — the worker is starved on the frozen core,
  not purely CPU-bound (host samples show cores 0/2/3 idle).
- Resource-queue worker / callback producer too slow (`resource+4` data pointer
  null too long): same starvation cause.
- Guest worker scheduling / SVC synchronization delaying the consumer: heavy
  `WaitProcessWideKeyAtomic` / `SignalProcessWideKey` / `ArbitrateLock` churn,
  but no HLE/SVC deadlock — the HLE blocker is the buffer-queue wait itself.
- HLE service work (binder / nvdrv / fsp) too slow: secondary; the plateau has a
  runnable worker rather than a service deadlock.

## 2026-07-05 — state `0xA` resource-slot sequence

The visible first-logo delay is not fully explained by the earlier
`w4314=7 -> 3` transition. A low-noise run with present-source dumps,
`/tmp/ruzu_mk8d_present_timeline_1783271073`, showed:

- `w4314=7 / w4308=0x000BC002` reached at about `6.97s`.
- `w4314=3` reached at about `16.00s`.
- The presented source was still first-logo-like through frame dumps
  `src_000300.ppm` through `src_002700.ppm`.
- At about `31.19s`, the same state object reached
  `w4314=0xA`, `w430C=0x200`, `w4308=0x00203FFC`.

Static mapping from MK8D `main` (base `0x00206000`) now identifies the state
`0xA` launcher:

- `0x005174D4` calls `0x009CE884`, a helper that clears/reset-flags across a
  large sequence of per-resource/per-slot structures.
- `0x00517574` calls `0x009CC73C` with an index in `r1`/`r5`.
- `0x009CC7B4` writes `base+0x4314 = 0xA`.
- `0x009CC840` clears `base+0x4308 = 0`.
- `0x009CC7E0..0x009CC83C` copies slot/resource data from
  `global_table->0x34[index]` into `base+0x4034..0x404C`.
- Waiters around `0x009CE200..0x009CE868` poll bits in `base+0x430A`
  (`w4308` bits 16..23), not just the low byte.

PC trace run `/tmp/ruzu_mk8d_pc_0a_only_1783271581` confirms the dynamic order:

- First `0x005174D4` / `0x009CE884` hit: about `31.914s`.
- First `0x00517574` hit: about `31.924s`.
- First `0x009CC7B4` (`w4314=0xA` write): about `31.924s`.
- First `0x009CC840` (`w4308=0` clear): about `31.925s`.
- By `50.098s`, the trace counted `24` state-0xA launches and `24`
  `w4308` clears. The argument distribution shows the slot index cycling
  (`r1` / `r6` values include `0..0xB`), with multiple passes over the slot
  set.

Interpretation:

- The visible first logo remains because MK8D is still sequencing resource
  slots after the state-7 Yaz0 phase. The later delay is a repeated
  `0x00517574 -> 0x009CC73C -> state 0xA` slot-launch path, not a single stuck
  wait.
- The repeated cadence is roughly frame/timer-like (about 0.5s between groups
  in this run), so the next question is whether ruzu is executing the same
  number/order of slot launches as yuzu but too slowly, or whether a guest
  condition makes it repeat extra passes before the second logo.

Hypotheses invalidated or weakened by this run:

- "The first-logo problem ends when `w4314=7` clears." Invalidated. The image
  remains first-logo-like after `w4314=3`; the later state-0xA sequence must
  be included.
- "The `w4314=0xA` plateau is a vague symptom." Weakened. It now maps to
  concrete guest code: `0x00517574 -> 0x009CC73C`.
- "The state object is missing writes." Still invalidated. The relevant state
  writes occur; the issue is timing/order/progress of the sequence.

Immediate next steps:

- Trace the caller conditions in `0x00517148..0x0051759C`, especially the
  counters at `[caller_object+0x594]`, `[caller_object+0x598]`,
  `[caller_object+0x5A0]`, and the per-slot fields around
  `resource+0x2B0..0x2C1`.
- Determine whether yuzu performs the same number of state-0xA launches before
  switching logo. If yuzu cannot be instrumented, infer expected behavior from
  the guest code and compare ruzu counters/frame cadence.
- Add a low-volume trace for the state-0xA launch path that logs timestamp,
  selected slot index, `base+0x4308/0x430A/0x4314`, and the relevant
  per-slot `0x2B0/0x2B4/0x2B8/0x2BC/0x2C1` fields.

Follow-up note:

- The counters near `+0x594/+0x598/+0x5A0` are **not** fields of
  `base=0x5564C690` from `0x009F5998`; they belong to the caller object kept
  in `r8` inside `0x00517148..0x0051759C` (observed around `0x555C0810` in
  `/tmp/ruzu_mk8d_pc_0a_only_1783271581`). A temporary HLE trace of
  `base+0x594/+0x598/+0x5A0` is therefore an invalidated probe and should not
  be used to interpret the state object.

### 2026-07-05 follow-up — caller object confirms frame-paced slot loop

Run `/tmp/ruzu_mk8d_caller_1783272143` used
`RUZU_TRACE_MK8D_LOGO_STATE=1` and
`RUZU_TRACE_MK8D_LOGO_CALLER=0x555C0810`.

Findings:

- The run reached `w4314=7`, `w4308=0x000BC002` at about `7.5s`.
- It left that phase (`w4314=3`) at about `17.2s`.
- Later low-bit progress appeared as `w4308=0x4`, `0xC`, and `0x1C`
  between about `22s` and `26s`.
- It entered the later slot sequence at about `32.8s`:
  `w4314=0xA`, `w430C=0x200`, `w4308=0x00203FFC`.
- On the caller object, `b059C` becomes `1`, `w0598` cycles through slot
  indexes, and `w05A0` counts `0..0x1D`.
- `w05A0` increments at roughly 60 Hz and gates each slot for 30 frames
  (about 0.5 seconds), exactly matching the guest code at
  `0x005174B4..0x005174C4`.
- The sequence launches repeated slots through
  `0x00517574 -> 0x009CC73C`; observed slot starts include slots `1..12`,
  then another pass.

Interpretation:

- The later first-logo delay is not a kernel deadlock and not a missing write:
  the guest is voluntarily pacing the slot-launch sequence with a 30-frame
  counter.
- The remaining question is whether yuzu executes the same number/order of
  slot launches much earlier, or whether ruzu causes extra passes/late entry by
  making earlier resource readiness predicates complete late.

Related profile run `/tmp/ruzu_mk8d_profiles_1783272302`:

- `IHOSBinderDriver cmd=0` remains the largest aggregate IPC bucket, but it is
  mostly presentation/dequeue pacing while the guest keeps showing the first
  logo.
- SIGUSR/PC samples are dominated by SDK SVC wrappers
  (`SleepThread`, `WaitProcessWideKeyAtomic`, `SendSyncRequest`), not a pure
  host deadlock or one infinite guest CPU loop.

Invalidated hypotheses:

- Invalidated: "state `0xA` is stuck." It is a repeated, frame-paced guest
  sequence.
- Invalidated: "timer conversion is still the visible-logo root cause." The
  `w05A0` counter advances at the expected frame cadence.
- Invalidated: "`RUZU_FASTMEM_TRAP_PAGE` can trace the state writers on this
  Apple Silicon run." The ARM64 backend has no active fastmem pointer here, so
  the trap path logs that fastmem is disabled and does not catch stores.
- Invalidated as a practical trace method: global `RUZU_NO_FASTMEM=1` reaches
  memory callbacks but slows boot too much to study the late slot sequence.

Next useful instrumentation:

- Use `RUZU_A32_INLINE_WATCH_ADDR` for exact guest writer attribution when
  needed; it is more targeted than global no-fastmem.
- Add or use a low-volume trace for the caller object plus state object around
  `0x00517148..0x0051759C`, especially:
  `caller+0x594`, `caller+0x598`, `caller+0x59C`, `caller+0x5A0`,
  `base+0x4308`, `base+0x430C`, and `base+0x4314`.
- Compare the number and cadence of `0x00517574 -> 0x009CC73C` launches
  before the second logo against yuzu behavior if a yuzu-side trace becomes
  available; otherwise infer the expected stop condition from the guest code.
  `/tmp/ruzu_mk8d_logo_counters_1783271849` therefore produced stable but
  mostly irrelevant values (`w0598=0x00E380B0`, `w05A0=0x40`, `b059C=0x34`
  for `base=0x5564C690`).
- `RUZU_A32_PC_TRACE_MEM` with `r0+0x594,...` crashed with `exit=139` in
  `/tmp/ruzu_mk8d_517574_mem_1783271735`; do not use that unsafe fastmem
  probe for this path until it validates mappings before host reads.

### 2026-07-05 follow-up — delayed callback dispatch before the slot loop

Additional PC and write traces moved the active root-cause target earlier than
the `0x00517574 -> 0x009CC73C` slot loop.

Key dynamic facts:

- `caller+0x59C` (`0x555C0DAC` in current runs) gates the slot counter at
  `0x00517494`.
- `0x009F51C0` writes `strh #0x100` to `caller+0x59C`, which initializes
  `b59C=0` and `b59D=1`; it does **not** open the gate.
- The gate-open write is `strb #1, [r0+0x59C]` at `0x005FB314`, reached via
  `lr=0x009F4B0C`.
- The function starting at `0x005FB084` contains that gate-open write and
  completes in less than 1 ms once entered.
- `0x005FB084` itself starts very late in ruzu traces (about `34-36s` in
  `/tmp/ruzu_mk8d_b59c_func_1783273488` and
  `/tmp/ruzu_mk8d_callback_path_1783273591`).
- The generic callback runner at `0x00A039B4` / `0x00A03AA4` runs much earlier
  for other objects, but the specific dispatch where `r1=0x005FB084` only
  occurs late. In that late dispatch, `0x005FB084` is loaded from the object's
  vtable slot at `+0x12C`.

Interpretation:

- The long first-logo delay is currently best localized to **late scheduling or
  late dispatch of the object whose vtable entry `+0x12C` is `0x005FB084`**.
- The callback body is not the bottleneck; it is a short initializer that opens
  `caller+0x59C`, allowing the 30-frame slot loop to begin.
- After this gate opens, the observed 30-frame `w05A0` cadence is expected
  frame pacing rather than the primary delay.

Invalidated hypotheses:

- Invalidated: "`0x005FB084` is internally slow or stuck." It enters late and
  finishes almost immediately.
- Invalidated: "`0x009F51C0` opens the slot-loop gate." It initializes
  `caller+0x59C` to `0x0100`, which leaves byte `+0x59C` at `0`.
- Invalidated: "The 30-frame slot counter itself is too slow." It advances at
  the expected frame cadence once enabled.
- Weakened: "The state-0xA loop is the first root cause." It is downstream of
  the late `0x005FB084` dispatch.

Next investigation target:

- Identify where the object/vtable containing `0x005FB084` at vtable `+0x12C`
  is created, queued, and finally dispatched by `0x00A039B4`.
- Search the extracted `main` NSO for the little-endian callback pointer
  `84 B0 5F 00`, map the containing table to runtime addresses using the
  verified MK8D base (`0x00206000`), then trace writes/references to that
  vtable/object.

### 2026-07-05 follow-up — object lifecycle queue, not callback body

The extracted MK8D `main` NSO was inspected with the verified base
`0x00206000`.

Static mapping:

- The callback pointer is stored in `.rwdata` as NSO-relative
  `0x003F5084`, not absolute `0x005FB084`.
- The relocation is at NSO `0x00C7AA4C`, runtime `0x00E80A4C`.
- Therefore the vtable base is runtime `0x00E80920`; entry `+0x12C` resolves
  to `0x005FB084`.
- Entry `+0x124` resolves to `0x005FAB34`.
- `0x005FAB34` is a constructor/initializer-like routine. It allocates a
  `0x64`-byte subobject, installs vtables, and registers child resources.

Dynamic trace `/tmp/ruzu_mk8d_ctor_dispatch_1783274113`:

- `0x005FAB34` for the delayed object is not called early. It first appears
  around `34.934s` with `r0=0x8A514150`, `lr=0x00A03638`.
- Immediately after, the generic runner reaches `0x00A03A98/0x00A03AA4` for
  the same object and calls `vtable+0x12C = 0x005FB084`.
- `0x005FB314` then writes `caller+0x59C = 1`, opening the downstream
  30-frame slot loop.

Static mapping of the caller:

- `0x00A0349C` is a generic lifecycle routine.
- It calls `vtable+0x120` at `0x00A035B4`.
- It calls `vtable+0x124` at `0x00A03634`; for the delayed object this is
  `0x005FAB34`.
- Its wrapper `0x00A0343C` calls `0x00A0349C` at `0x00A03464`, explaining the
  observed `lr=0x00A03468` on many lifecycle entries.

Dynamic trace `/tmp/ruzu_mk8d_lifecycle_entry_1783274209`:

- `0x00A0349C` runs already at about `3.52s` for many objects.
- The delayed logo object (`0x8A514150`) is not one of those early objects; it
  reaches this lifecycle path only much later.
- Therefore the remaining root-cause target is not the constructor body, not
  `0x005FB084`, and not the downstream slot loop. It is the scheduling/order
  of the object lifecycle queue that eventually presents object `0x8A514150`
  to `0x00A0349C`.

Invalidated hypotheses:

- Invalidated: "The object is constructed early but dispatched late." The
  constructor itself is late for the delayed object.
- Invalidated: "The delay is in `0x005FAB34`." Once called, it runs directly
  into the expected callback path.
- Invalidated: "The generic lifecycle runner starts late globally." It starts
  around `3.5s`; only this specific object arrives late.

Next investigation target:

- Trace cadence and inputs of `0x00A0349C`/`0x00A0343C` to determine why the
  delayed object reaches the lifecycle queue after ~35s.
- Identify the producer that enqueues or exposes object `0x8A514150` to this
  lifecycle path. The current strongest target is the wrapper/parent path
  around `0x00A0343C` and the object lists used by
  `0x00A03354..0x00A03408`.

### 2026-07-05 follow-up — lifecycle batch gap localized

Run `/tmp/ruzu_mk8d_lifecycle_cadence_1783274741` traced
`0x00A0349C` entries whose wrapper return is `0x00A0343C`.

Observed cadence:

- First lifecycle batch: `t=3.669s..3.734s`, mostly
  `r1=0x84AC0AD4`, `r11=4..0x11`.
- One extra early entry: `t=3.798s`, `r1=0x90480984`, `r11=3`.
- Large gap: `44.568s` with no matching lifecycle entries.
- Second lifecycle batch: `t=48.366s..48.519s`, `r1=0x875C0AE4`,
  `r11=0x13..0x5F`.
- The delayed first-logo object appears inside this second batch:
  `t=48.401s`, `r0=0x8A514150`, `r1=0x875C0AE4`, `r11=0x20`.

This narrows the visible first-logo delay to the gap before the second
lifecycle group (`r1=0x875C0AE4`) becomes active. Once that group starts, it
processes rapidly and the delayed object is just one item in the batch.

Invalidated hypotheses:

- Invalidated: "The lifecycle queue processes one object slowly per frame."
  The second batch processes dozens of entries in roughly `0.15s` once it
  starts.
- Invalidated: "`0x8A514150` itself is the slow object." It is item
  `r11=0x20` in the second batch; the delay happens before the batch starts.
- Weakened: "Runtime pipeline failures are the sole cause." The same run did
  show MoltenVK pipeline failures, but current evidence only proves
  correlation. The next step is to timestamp pipeline build/failure events
  against the lifecycle gap before assigning causality.

Next investigation target:

- Identify what releases or starts the lifecycle group `r1=0x875C0AE4`.
- Correlate runtime/disk pipeline build failures with the `44.568s` gap using
  ruzu timestamped logs, not raw MoltenVK stderr lines.

### 2026-07-05 follow-up — VI/HWC backpressure caused by swapchain-layout lock

New profiled runs:

- `/tmp/ruzu_mk8d_nvdrv_1783276589`
- `/tmp/ruzu_mk8d_binder_1783276709`
- `/tmp/ruzu_mk8d_hwc_1783276818`
- `/tmp/ruzu_mk8d_vsync_1783277084`
- `/tmp/ruzu_mk8d_vkcomp_1783277299`
- `/tmp/ruzu_mk8d_vkcomp2_1783277448`
- `/tmp/ruzu_mk8d_trylayout_1783277701`

Verified/invalidated:

- Invalidated: `nvdrv` ioctl/syncpoint was not the first-logo plateau cause.
  The largest ioctl totals were only milliseconds over the run, while the
  visible delay was tens of seconds.
- Invalidated: stale HWC cached framebuffer was not the cause. HWC acquired
  new buffers continuously; reuse was tiny (`0.2%` or less in the key runs).
- Invalidated: `DequeueBuffer` was not slow because binder itself was slow.
  Binder transaction `3` was spending almost all time inside BufferQueue wait,
  not parcel read/write/dispatch.
- Invalidated: missing `Common::Event::Set` notify was not the cause.
  Upstream `Common::Event::Set()` only notifies when the event was previously
  unset; ruzu's local VI `ThreadEvent` already matched this coalescing
  behavior.

Root-cause slice found:

- Before the fix, CoreTiming generated vsync callbacks at the expected cadence,
  but `VsyncThread` could not keep up:
  `callbacks=2093 already_set=1105 thread_wakes=988 process=988`.
- `process_vsync()` averaged about `19.4ms` and had ~`1s` spikes. This caused
  BufferQueue backpressure: `DequeueBuffer` averaged about `38-40ms` and
  accumulated about `29-30s` of wait time in a ~40s run.
- `RendererVulkan::Composite` profiling localized nearly all of the ~1s spikes
  to `current_framebuffer_layout_for_present()`.
- That function was taking `self.swapchain.lock()` every frame. On macOS/
  MoltenVK the present thread can hold the swapchain mutex while WSI waits for
  a drawable, so the VI/HWC thread blocked inside layout synchronization before
  doing any real composite work.
- Upstream `RendererVulkan::Composite` uses the frontend framebuffer layout and
  does not block on a per-frame swapchain query for layout.

Fix applied:

- `current_framebuffer_layout_for_present()` now uses `try_lock()` on the
  swapchain and falls back to the frontend-provided layout if the present thread
  owns the mutex.
- The extra swapchain-extent query used only for `RUZU_TRACE_PRESENT` logging
  was made conditional, so normal presentation does not take that lock.

Validation after fix (`/tmp/ruzu_mk8d_trylayout_1783277701`):

- `VSYNC_PROFILE`: `callbacks=2074 already_set=2 thread_wakes=2072 process=2072`
  and `process_vsync` average dropped to `259us`.
- `VK_COMPOSITE_PROFILE`: average composite dropped to about `195us`; max was
  about `52.8ms`, no recurring ~1s layout stalls.
- `BINDER_TXN_PROFILE`: `DequeueBuffer` average dropped from ~`39ms` to
  ~`15ms`, and max dropped from ~`1s` to ~`30ms`.
- HWC acquired `1840` buffers in the profiled window instead of roughly
  `740-760` before the fix.

Remaining uncertainty:

- Resolved: a normal visual dump was run after the fix, so this is no longer
  only counter-based.

Post-fix visual validation:

- `/tmp/ruzu_mk8d_visual_1783277878/frame600.png` still shows the simple MK8D
  logo at frame `600`.
- `/tmp/ruzu_mk8d_visual2_1783278001/frame1200.png` still shows the simple MK8D
  logo at frame `1200`.
- yuzu reference `screenshots/yuzu/MK8D/Capture d’écran 2026-07-03 à 11.31.20.png`
  is already on the Mario/Peach/Rainbow Road artwork at the comparable later
  stage.

Updated conclusion:

- The swapchain-layout lock fix is correct and necessary, but it is not the
  full first-logo fix.
- Invalidated: "the first-logo delay was only VI/HWC presentation
  backpressure." The visual plateau persists after HWC/vsync cadence is fixed.
- The lifecycle-batch hypothesis remains active and must be re-measured after
  the VI/HWC fix, because the previous timings were collected under heavy
  presentation backpressure.

Post-fix lifecycle re-measure:

- `RUZU_A32_PC_EXEC=0x00A0349C` is required for this target; the block-entry
  `RUZU_A32_PC_TRACE=0x00A0349C` can miss because the address is not
  necessarily a block entry.
- `/tmp/ruzu_mk8d_lifecycle_exec_1783278348` confirmed the hook works and saw
  the early batch around `6.51s..6.64s`.
- `/tmp/ruzu_mk8d_lifecycle_exec_long_1783278404` saw the early batch around
  `3.935s..4.043s`:
  `r1=0x84AC0AD4`, `r11=4..0x11`, followed by one `r1=0x90480984`,
  `r11=3`.
- No later `0x00A0349C` batch appeared by `60s` in that long run, so the
  missing transition is still upstream of the lifecycle runner. The older
  "second batch appears late" observation is now refined to "the second batch
  may not appear at all within the tested window after the VI/HWC fix".

Next investigation target after post-fix traces:

- Find what should release/start the next lifecycle group after the early
  `r1=0x84AC0AD4` group.
- Continue from the parent/list traversal around `0x00A0343C` and
  `0x00A03354..0x00A03408`, but trace the producer/writer that changes the
  list or state which makes the second group visible.
- Use guest memory probes on the owner/list pointers seen in the early batch
  rather than adding more renderer instrumentation.

### 2026-07-05 follow-up — late waiter is `0x009CF410`, not lifecycle dispatch

New traces after the VI/HWC fix refine the lifecycle hypothesis again.

Run `/tmp/ruzu_mk8d_lifecycle_multi_nomem_1783281342` traced:

- `0x006CA30C`
- `0x00A03354`
- `0x00A0343C`
- `0x00A0349C`
- `0x00876940`
- `0x00878364`
- `0x00878EA4`

Result:

- The lifecycle/runner callsites fired only in an early burst around
  `3.617s..3.741s`.
- No later second lifecycle group appeared up to `70s`.
- Therefore the late visible-logo delay is not explained by a missing second
  invocation of `0x00A0349C`; the remaining wait is further down the loading
  state-machine path.

Run `/tmp/ruzu_mk8d_statemachine_pc_1783281449` traced the state-machine
callsites:

- Late time (`~79s..93s`) is dominated by `0x009CF410`.
- At `0x009CF410`, registers are stable:
  `r0=3`, `r8=0x55650998` (`base+0x4308`),
  `r10=0x556509A4` (`base+0x4314`), `lr=0x00758964`.
- The earlier state-machine callsites (`0x009CCB14`, `0x009CD258`,
  `0x009CD4BC`, `0x009CD4C0`, `0x009CD5B4`, `0x009CD624`,
  `0x009CD668`, `0x009CF344`) only appeared early.

Static disassembly with verified MK8D `main` base `0x00206000`:

- `0x009CF344` resets `base+0x4308 = 0`, calls setup helper
  `0x009CC228(base, ..., 0xFFFF)`, then enters the wait path.
- `0x009CF410` loops until `[base+0x4314] == 0` or
  `([base+0x4308] & 0x04) != 0`.
- `0x009CC228` prepares/copies resource descriptor data and initializes nearby
  fields, but does not set the waited low bit itself.
- `0x009CD668` sets `base+0x4308 |= 0x40000`; it is not the setter for the
  low `0x04` bit waited by `0x009CF410`.

Safe SIGUSR1 memory dump:

- `/tmp/ruzu_mk8d_sigmem_1783281778/run.log` used the safer
  `RUZU_DUMP_MEM` path-table/process-memory read path, avoiding raw fastmem
  faults.
- At about `82s`, memory at `0x55650990` was:
  `[00000000, 00000000, 00000000, 0000000C, 00000001, 00000003,
  00000100, 00EBB7DC, 00000000, 00000000, 5E7A9F48, 5E7AA000]`.
- Interpreted against `base=0x5564C690`:
  `base+0x4308 = 0`, `base+0x430C = 0x0C`,
  `base+0x4310 = 1`, `base+0x4314 = 3`,
  `base+0x4318 = 0x100`.
- This proves the immediate late wait condition: the guest is in state `3`
  and is waiting for bit `0x04` in `base+0x4308`.

Thread dump from the same run:

- Main waiting thread: tid `79`, `SleepThread`, PC `0x01D5034C`,
  LR `0x00758964`, stack includes `0x009CF410`.
- Runnable worker/resource candidate: tid `84`, PC `0x009E7F40`,
  LR `0x00515930`, with registers including
  `r0/r4=0x55653008`, `r2/r6=0x972BF7A0`, `r8=0x973885B8`.
- Many other threads are condition-variable waiters, so the system is not a
  simple all-core Yaz0 burn at this late point.

Static disassembly of the tid `84` path:

- `0x009E7F40` selects among resource/job slots and checks per-slot readiness
  fields.
- `0x00515930` is just after an indirect/vtable call in a resource updater-like
  routine.
- This makes tid `84` the best next target: it may be the worker that should
  produce the resource/callback progress which eventually sets
  `base+0x4308 & 0x04`.

Instrumentation changes used for this:

- `RUZU_DUMP_MEM` now defaults to page-table/process-memory reads instead of
  raw fastmem reads. `RUZU_DUMP_MEM_USE_FASTMEM=1` keeps the old raw fastmem
  path for explicit experiments.
- A32 write-watch instrumentation was added with `RUZU_WATCH_WRITE=addr:len`
  for JIT memory write callbacks. A first run with fixed addresses did not
  capture writes, likely because the state base moved or because the inline
  watch changed the path; do not treat that negative result as proof that
  writes never happen.

Invalidated hypotheses:

- Invalidated: "the late delay is a missing second `0x00A0349C` lifecycle
  batch." The lifecycle callsites are early-only in the latest long trace.
- Invalidated as a complete explanation: "the late plateau is purely active
  Yaz0 CPU burn." Yaz0 dominates earlier resource production, but the `82s`
  dump shows a concrete waiter at `0x009CF410` plus runnable worker/resource
  threads rather than all cores spending time only in the decompressor.

Next investigation target:

- Trace tid `84`/resource-worker path around `0x009E7F40`, `0x0051592C`,
  `0x00515930`, and nearby indirect callsites without unsafe memory probes.
- On a later SIGUSR1 dump, inspect memory around
  `0x55653008`, `0x972BF7A0`, and `0x973885B8` to identify the worker object
  and slot state.
- Identify the callback or worker write that sets `base+0x4308 & 0x04`.

### 2026-07-05 follow-up — wake/handoff profile refines the synchronization hypothesis

Run `/tmp/ruzu_mk8d_wake_120s_1783284994` used:

- `RUZU_PROFILE_WAKE=1`
- `RUZU_PROFILE_WAKE_PER_TID=1`
- `RUZU_PROFILE_SVC_SUMMARY=1`
- `RUZU_PROFILE_SVC_PER_TID=1`
- `RUZU_PROFILE_GAP=1`
- `RUZU_PROFILE_VSYNC=1`
- `RUZU_PROFILE_BINDER_TXN=1`
- `RUZU_PROFILE_BQP_WAIT=1`

Result:

- SleepThread precision is not the problem in prior timing checks:
  `5ms -> ~5.07ms`, `1ms -> ~1.05ms`, ratio ~`1.0`.
- Kernel wake latency is real but concentrated:
  `WAKE_LATENCY count=49713 total=112247.81ms avg=2257.9us max=1009649.1us`.
- The dominant wake-latency owner is `tid=101`:
  `count=2580 total=104023.97ms avg=40319.4us max=1009649.1us`.
- `tid=101` has `2371` wake samples in the `8.4ms..16.8ms` bucket and `68`
  samples in the `536.9ms..1.1s` bucket.
- Most other high-count guest threads are cheap:
  - `tid=74`: `15088` wakes, `avg=114.8us`.
  - `tid=82`: `11021` wakes, `avg=44.0us`.
  - `tid=83`: `15023` wakes, `avg=26.4us`.
  - `tid=78`: `2584` wakes, `avg=21.2us`.
- `GAP_PROFILE` shows `tid=84` spends most wall time between SVCs:
  `count=1210 total=100333.03ms avg=82919.9us`, with long multi-second
  buckets. This matches the observation that `tid=84` is often in GPU/resource
  work or Yaz0/submit paths between SVCs, not simply stuck in a kernel wait.
- `tid=105` is also expensive between SVCs:
  `count=7109 total=81889.51ms avg=11519.1us`.
- VI/HWC backpressure remains visible in the same run:
  - `BINDER txn=3`/`DequeueBuffer`: `104092.30ms` total, `avg=40377us`,
    `max=1006890us`.
  - `BQP_WAIT`: `104059227us` total, `avg=13872us`, `max=1002185us`.
  - `VSYNC`: `callbacks=6876`, `already_set=4022`, `process avg=24387us`,
    `max=1004894us`.

Interpretation:

- The broad "many handoffs are expensive" hypothesis is partially confirmed,
  but the measured hot spot is not evenly distributed across ~500k handoffs.
  The current `RUZU_PROFILE_WAKE` data shows about `49.7k` consumed kernel
  wake samples, with most lost wall time concentrated on `tid=101` wake
  latency and `tid=84`/`tid=105` guest gaps.
- The first-logo delay should now be treated as a synchronization/producer
  pacing issue with one or two pathological participants, not a uniform
  fixed-cost penalty on every condvar handoff.
- `BQP/VSYNC` still shows ~1s tails in this run. This may be a symptom of the
  same pathological producer cadence, or another macOS presentation path that
  still occasionally blocks. It should not be collapsed back into the old
  `nvdrv`/SubmitGpfifo hypothesis: those profiles remain cheap.

Invalidated/refined hypotheses:

- Refined: "500k handoffs at ~0.15ms each explains the plateau." The shape is
  plausible but the existing wake profiler measured `49.7k` consumed kernel
  wakes, and the wall time is highly concentrated in `tid=101` and long guest
  gaps on `tid=84`/`tid=105`.
- Invalidated: "SleepThread/timer quantization is the cause." Sleep timing is
  accurate.
- Invalidated: "`nvdrv` ioctl or SubmitGpfifo handler cost is the cause."
  Current 120s runs show only tens of milliseconds in those handlers.

Next investigation target:

- Extend `RUZU_PROFILE_WAKE` or add a focused profile to record wake source,
  source tid/core, target tid/core/affinity, and whether the woken thread
  resumes on the same physical core or a different one.
- Focus first on `tid=101` wake paths and `tid=84`/`tid=105` long
  SVC-to-SVC gaps. These are now the measured outliers.
- Compare scheduler dispatch choice with upstream before changing behavior:
  the next code fix should be an upstream-faithful scheduling/affinity
  correction, not a heuristic "pin everything" workaround.

### 2026-07-05 follow-up — ROOT CAUSE: buffer-queue dequeue freezes the guest core thread

A host `sample` during the plateau plus the affinity/thread dump finally
localize the delay to a single concrete mechanism.

**Per-core busy/idle split (8s host sample at ~t60s):**

- `CPUCore_1`: 4731 guest samples / 9 idle — ~99% busy, but only **484 in
  `A32Jit::run`** (real guest code). The other ~4247 are in the kernel/HLE
  path.
- `CPUCore_0`: 3610 idle / 1092 guest (76% idle).
- `CPUCore_2`: 4542 idle / 167 guest (~96% idle).
- `CPUCore_3`: idle.

So it is **not** uniformly idle: core 1 is saturated, cores 0/2/3 idle.

**Where core 1's time actually goes (dominant stack, 3976/4731 = 84%):**

```
svc::send_sync_request (SVC 0x21)
  -> hle_ipc::complete_sync_request
  -> IHosBinderDriver::transact_parcel_impl
  -> BufferQueueProducer::dequeue_buffer
  -> wait_for_free_slot_then_relock
  -> BufferQueueCore::wait_for_dequeue_condition
  -> _pthread_cond_wait / __psynch_cvwait   <-- HOST core thread frozen
```

`BufferQueueCore::wait_for_dequeue_condition` does a plain
`self.dequeue_condition.wait_while(guard, ...)` (std condvar) that blocks the
**host CPUCore_1 thread** until a graphics buffer frees (~1 per frame ≈ 16ms).

**Why this stalls loading:** MK8D pins almost all of its loading worker
threads to core 1. Affinity dump during the plateau:

- core 0 (0x1): tid 79 (main, prio 45)
- core 1 (0x2): tid 78 (prio 35), 82 (44), 84 (52), 101 (43), 104 (32), 105 (45)
- core 2 (0x4): tid 83 (44)
- core 3: (system)

tid 78 (prio 35, the graphics producer doing `dequeueBuffer`) blocks the
core-1 host thread in `pthread_cond_wait`. Because ruzu multiplexes guest
threads as fibers over 4 host core threads, **freezing the core-1 host thread
freezes every guest fiber on core 1**, including all the loading workers.
They only advance in the ~16% of the time the producer is not blocked, so the
logo/loading crawls (~80s instead of ~3-4s).

**Why it looks like a "synchronization stall" and not CPU:** RUZU_PROFILE_WAKE
showed 49,713 wakes / ~112s cumulative, dominated by tid 101 (104s cumulative,
avg 40ms, 68 spikes of ~1s); GAP_PROFILE showed tid 84 ~100s and tid 105 ~81s
of SVC-to-SVC gap. Those outliers are the *symptom*: those workers are starved
on core 1 while it is frozen in `dequeue_buffer`. The wake latency measured
(end_wait -> next SVC) is exactly the dispatch delay caused by the frozen core.

**Cross-core wakeup path verified correct:** `KScopedSchedulerLock::unlock`
computes `cores_needing_scheduling` via `update_highest_priority_threads` and
`enable_scheduling_with_scheduler` -> `reschedule_other_cores` -> `core.interrupt()`.
`PhysicalCore::idle` waits on `m_on_interrupt` (no timeout). So a runnable
thread on an idle core IS interrupted promptly; the stall is not a missing IPI.
SleepThread wake accuracy is ~1.0 (5ms->5.07ms, 1ms->1.05ms), so it is not
timer/tick quantization either.

**Affinity is NOT a ruzu bug:** the workers get `affinity = 1 << ideal_core`
(single core) from `svcCreateThread(ideal_core=1)`, matching upstream
`KThread::Initialize`; the game issues zero `svcSetThreadCoreMask` calls to
widen the mask. So pinning to core 1 is the guest's design, not a ruzu default.
The fix is therefore not "spread threads across cores" — it is "stop freezing
the core in the buffer-queue wait".

**Options to fix (root cause = HLE blocking wait freezes the emulated core):**

1. Park the guest thread in the kernel and yield the fiber (preferred,
   upstream-faithful). Replace the host `condvar.wait_while` in
   `wait_for_dequeue_condition` (and the analogous acquire/queue waits) with a
   guest-kernel wait: put the calling guest thread into a kernel wait
   (BeginWait / event), register a wake when `dequeue_possible` becomes true
   (signalled from the consumer/vsync release path), and `reschedule` so the
   core runs its other fibers meanwhile. Effect: core 1 keeps running the
   loading workers while the producer waits; expect roughly a 6x loading
   speedup (the ~84% frozen fraction is reclaimed). Loading stays single-core
   (guest pins to core 1) but no longer frozen.
2. Route nvnflinger/binder IPC to a dedicated host thread (host-thread IPC
   routing already exists for other services). The blocking `pthread_cond_wait`
   would then run on that host thread, not on the guest core thread, so it no
   longer freezes core 1. Less invasive to the buffer-queue code, but must
   ensure the binder transaction still returns its reply to the guest correctly.
3. Audit every other inline HLE handler that can block the caller with a host
   condvar/lock (audio `wait_free_space_with_stop`, syncpoint waits, MultiWait
   `local_timed_wait`, etc.): the same fiber-freeze anti-pattern anywhere on a
   guest-core thread will stall co-located fibers. Option 1/2 for the buffer
   queue is the highest-value first target because it is measured at 84% of the
   saturated core during the logo.

**Next step:** confirm the producer thread id (expected tid 78) by tracing the
`dequeue_buffer` caller tid, then implement option 1 in
`BufferQueueCore::wait_for_dequeue_condition` (and mirror it in the acquire /
`wait_for_free_slot_then_relock` waits), remeasuring the core-1 frozen fraction
and the logo transition time.

## Success Criteria

- First logo transitions after roughly 3-4 seconds of visible time, like yuzu.
- Audio transition matches the visual transition, including the "Mario Kart
  eight" voice line.
- No long-lived first-logo period remains after pipeline preload completes.

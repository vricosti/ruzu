# MK8D First-Logo Delay Investigation

## 2026-07-10 - OPEN: attract scene saturates to white

After the KLightLock waiter-path port removed the host-condvar stall, the
longer attract-mode validation reached the 3D scene. The scene starts black,
briefly exposes fixed geometry with incorrect colors, then saturates almost
entirely to white.

Current localization:

- At present 6000, both the presented source and the independently downloaded
  HDR target `0x524C10000` contain the same overexposed geometry. The final
  nvnflinger/swapchain composition is therefore **invalidated as the source of
  the white output**; the corruption already exists in the guest HDR chain.
- At present 5200, the presented source is already nearly uniform pale pink
  (`mean=0.9504`, range `0.8667..1.0`).
- MK8D issues explicit black color clears for every observed bloom level,
  including the 960x540, 240x136, 120x68, 60x34 and 30x17 views rooted at
  `0x521410000`. **The hypothesis that the bloom target is never cleared is
  invalidated.**
- The texture cache copies between the full alias image and the separately
  materialized reduced views in both directions. This is consistent with the
  upstream alias model in principle, but the exact copy regions and
  modification-tick order still need comparison; it remains an active
  hypothesis rather than a proven bug.
- The first bloom dump looked uniformly red because the diagnostic path treated
  packed `B10G11R11_UFLOAT` bytes as RGBA8. That visual conclusion is
  **invalidated**. The dump path now decodes the 11/11/10 unsigned-float fields
  and applies a diagnostic Reinhard curve so the next run can inspect the real
  HDR contents.
- The corrected bloom dump is black while the final frame is white. **Bloom
  amplification/alias feedback is invalidated as the active cause.** The first
  bloom pass reads the 1920x1080 main HDR target at `0x520510000`; the bad
  frame is already produced before the bloom pyramid.
- A controlled present-4200 dump proves the same main HDR target and final
  output are color-correct on the title screen. The format, depth, clear and
  tone-map paths are therefore not globally broken.
- During the attract transition, captures show giant/stale geometry with the
  old title artwork still visible behind it, not merely excessive exposure.
  The active Vulkan draw path binds vertex/index data through the reduced
  GPU-VA keyed `DirectBufferCache`, but CPU/cache invalidation callbacks only
  notified `VulkanCommonBufferCache`. `DirectBufferCache::get_or_upload`
  consequently reused the first upload forever at an address rewritten by the
  guest. The callbacks now invalidate overlapping direct-cache entries too;
  attract-mode validation is pending.
- `RUZU_TRACE_COMPUTE=1` produced no dispatch or skipped-dispatch event through
  the white phase. **The missing compute-descriptor path is retained as a
  structural gap but invalidated as this symptom's active cause.**
- No `[DRAW_SKIP]` occurred in the traced interval. Progressive pipeline
  compilation remains observable, but skipped asynchronous draws are not yet
  supported as the cause of the white output.

Next gate: rebuild and repeat the long attract run with direct-cache
invalidation active. If geometry is corrected, continue the full upstream
geometry migration (`UpdateGraphicsBuffers(is_indexed)` +
`BindHostGeometryBuffers`) and retire the reduced direct cache rather than
leaving two buffer owners.

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

## 2026-07-10 — FIXED: disk pipeline key/cache rebuild divergence and invalid SPIR-V CFG

This was not the sole cause of the original first-logo delay (cold-cache runs
still reproduce that timing issue), but it made warm-cache visual and timing
runs unreliable and caused the later black-title/device-hang regressions.

Four upstream-parity defects were found and fixed as one dependency chain:

- `FixedPipelineState` derived full-struct equality while upstream compares
  only `GraphicsPipelineCacheKey::Size()` bytes. Fields intentionally omitted
  by dynamic state were zero after disk load, so equivalent live keys missed
  and approximately 100 duplicate entries were appended per boot. Equality
  and hashing now cover the same variable prefix as upstream; XFB correctly
  forces the complete state.
- SPIR-V Phi operands were resolved while their block was emitted. Upstream
  uses Sirit `DeferredOpPhi` and patches operands after all definitions exist.
  Rust now follows that lifecycle, removing the last
  `unresolved IR value reference` preload failure.
- Rust `TranslatePass::Visit` shared `current_block` across recursion and the
  child emitted its fallthrough block; the parent emitted it again. This made
  duplicate `OpLabel` definitions. `current_block` is now local to each Visit,
  like upstream.
- `rebuild_syntax_successors` invented `continue_block -> loop body`; upstream
  routes continue to `loop_header` or `merge`. The false edge generated Phi
  operands for non-predecessors and is removed.
- Most importantly, graphics disk-load passed `env.start_address()` to CFG
  translation after stripping the 0x50-byte Maxwell SPH. Upstream explicitly
  uses `env.StartAddress() + sizeof(ProgramHeader)`. Rust now uses the same
  instruction start for normal and dual-vertex graphics stages; compute keeps
  its headerless start.

Validation:

- Full portable cache: `97 built, 0 skipped`; no `PIPELINE_KEY_DIFF`, no cache
  growth (`vulkan.bin` remains 339416 bytes), and no Metal device loss.
- All 92 dumped preload modules pass `spirv-val --target-env vulkan1.3`.
- The critical fragment is bit-identical across paths after the offset fix:
  disk `stage4_base_010F80` and live `stage5_base_010F80` both hash to SPIR-V
  `4BD624006F4172DE`.
- Before the fixes, the same preload produced duplicate labels, a Phi with two
  incoming blocks for one real predecessor, then an immediate MoltenVK GPU
  hang on the first frames.

Next visual gate: rerun through the title and attract transition with the
stable full cache, then reassess the missing `Press L+R` prompt and black
cinematic independently of cache reconstruction.

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

### 2026-07-06 — FIXED: option 1 implemented (guest fiber park in dequeue wait)

`BufferQueueCore::wait_for_dequeue_condition` now parks the calling **guest**
thread in the kernel instead of blocking the host core thread:

- New `dequeue_parked_threads: Mutex<Vec<Weak<KThreadLock>>>` on
  `BufferQueueCore`. Guest-core callers register there, drop the queue mutex,
  `begin_wait_with_queue` under `KScopedSchedulerLockAndSleep` (10ms
  safety-net timer), and fiber-reschedule — the same proven park sequence as
  the host-thread IPC path in `svc_ipc.rs`. Host threads (env-gated
  host-thread IPC routing, unit tests) keep the upstream host-condvar wait.
- `signal_dequeue_condition` additionally drains the parked list and
  `end_wait`s each thread under the scheduler lock (same lock order as
  `KReadableEvent::signal`). When the signaler is itself a guest core fiber
  (it holds `core.mutex`), the current-core inline switch is deferred via a
  dispatch-disable bracket — otherwise the woken waiter would immediately
  re-lock `core.mutex` on the same host thread and deadlock it; cross-core
  IPIs still fire inside `enable_scheduling`.
- Lost-wakeup-free: the waker sets `dequeue_possible` before draining, the
  parker registers before re-checking the flag under the scheduler lock.
- Kill switch: `RUZU_BQ_DEQUEUE_HOST_WAIT=1` restores the old host wait.

**Measured (release, cold-ish run, disk pipeline cache present):** first logo
→ color splash within ~7s of presented frames (was ~80s); loading spinner
reached at ~+45s of presentation. No panics, no
`KTHREAD_QUEUE_WITHOUT_END_WAIT`, no `OWNER_FAIL`, no stuck-session logs in a
120s run. `cargo test -p core` shows no new failures (the 4
`buffer_queue_producer` unit-test failures and the parallel-suite SIGSEGV in
the `k_condition_variable` region reproduce identically at HEAD).

**Still open (option 3 audit):** other inline HLE handlers that block the
caller with a host condvar/lock on a guest core thread — audio
`wait_free_space_with_stop`, syncpoint waits, MultiWait `local_timed_wait`,
and the inline same-session busy retry loop in `svc_ipc.rs`
(`receive_queued_inline_request_hle` → 5µs host sleep) which spins the host
core without yielding the fiber if a second guest thread ever hits a session
whose current request is parked.

### 2026-07-06 — FIXED: disk pipeline-cache preload was serialized (79s black screen)

After the dequeue fix, the remaining startup pain was ~60-80s of black
window before the first logo. Measured with `RUST_LOG=info`: the
`load_disk_resources` preload ran from t=0.7 to t=80 (1946-2048 cached
pipelines, only 213 built, ~1730 rejected), and `system.run()` only starts
after it (upstream yuzu_cmd ordering), so the guest cannot even boot during
that window.

**Root cause:** `catch_shader_exception` in
`video_core/src/renderer_vulkan/graphics_pipeline.rs` swapped the
process-wide panic hook under a global `SHADER_EXCEPTION_HOOK_LOCK` held for
the entire pipeline build (shader translate + vkCreateGraphicsPipelines). A
host `sample` during preload showed the 9 `VkPipelineBuilder` workers ~96%
blocked on that mutex — the "parallel" preload built one pipeline at a time.

**Fix:** install once (`std::sync::Once`) a panic-hook wrapper that delegates
to the previous hook unless the current thread is inside
`catch_shader_exception` (thread-local flag). No lock held during builds.
Upstream parity note: upstream uses per-thread C++ `catch
(const Shader::Exception&)` with no global state; the panic-hook filter is
the Rust equivalent without the serialization.

**Measured:** preload 79.3s → **11.0s** (same results: built=213, same skip
reasons). First visible frame ≈ t+20-24s after launch (was t+60-77s); logo →
color splash ≈ t+28-33s.

**Still open (preload follow-ups):**
- ~~85% of cached pipelines are rejected~~ RESOLVED 2026-07-06 (see next
  entry): the FixedPipelineState attribute bits were corrupted (enum
  ordinals instead of raw Maxwell encodings). Remaining rejects are real
  shader-recompiler gaps: 73 `FFMA CC` (FFMA with condition-code write not
  implemented) + 3 `SPIR-V unresolved IR value reference` — fixing FFMA.CC
  in the recompiler would recover those pipelines at runtime too.
- No loading indication during the remaining ~11s (yuzu GUI shows a
  "Preparing shaders" screen; yuzu_cmd is also black here, so this is
  upstream-faithful for the cmd frontend).

### 2026-07-06 — FIXED: GPU-thread panic at ~t80s (dangling descriptor-queue pointers)

`RasterizerVulkan::new` handed `&mut` to stack locals (`descriptor_pool`,
`desc_queue`, `compute_pass_desc_queue`, `blit_image`) to sub-components that
store them as `NonNull` (BufferCacheRuntime, TextureCache, BlitImageHelper),
then moved those locals into `Self` — leaving the pointers dangling on the
old stack frame. Observed as `add_buffer` growing a stale cursor that
`acquire()` (which clamps the *real* moved instance) never reset →
`index out of bounds: the len is 1048576 but the index is 1048576` in
`update_descriptor.rs` after ~80s of MK8D. Fixed by boxing the four fields
(same pattern as `scheduler`/`staging_pool`/`render_pass_cache`); upstream
C++ constructs members in place so this is a Rust-only hazard, semantics
unchanged. Also restored upstream parity: `RasterizerVulkan::TickFrame`
rotates `guest_descriptor_queue`/`compute_pass_descriptor_queue`
(vk_rasterizer.cpp:765-766) — ruzu's `tick_frame` never ticked them, so the
per-frame payload ring never advanced. Validated: 150s MK8D run, 0 panics
(previously crashed at ~82s).

### 2026-07-06 — FIXED: FixedPipelineState attribute bits held enum ordinals, not raw Maxwell values

`FixedPipelineState::refresh` packed `attrib.attrib_type as u32` /
`attrib.size as u32` (Rust enum ordinals) while every reader decodes the
bits with `from_raw` (Maxwell hardware encodings): Float (ordinal 6) read
back as SScaled, R32G32B32A32 (ordinal 0) read back as Invalid, etc. Disk
cache rebuilds therefore compiled shaders with wrong input types and
silently dropped vertex attributes the SPIR-V declared — the ~400 MoltenVK
"Vertex attribute m_NN is missing from the vertex descriptor" / "Render
pipeline compile failed" errors per run, and ~85% of cache entries
unbuildable. Fixed with exact `to_raw()` inverses used in `refresh`
(upstream packs `input.type.Value()` raw), CACHE_VERSION bumped 11→12.
Measured: preload rejects 1835/2048 → 151/594 (built 213 → 443), 0 MoltenVK
errors, 0 DRAW_SKIP at runtime (was: pipelines failing at runtime too).

### 2026-07-06 — REGRESSION fixed by gating: common-buffer-cache uniform stream fed wrong data

After the NonNull/Box fix, `desc_queue.update_data()` (previously always
empty through the dangling pointer) started feeding
`common_uniform_buffer_infos` for real — and MK8D regressed to
grayscale/corrupted frames (with or without disk cache; bisected by cache
removal). Two known defects in that path: the stream interleaves
storage-buffer entries with uniform entries (upstream consumes both via one
descriptor update template in push order; ruzu's loop consumes uniforms
only, so entries shift across stages), and the common buffer cache's
upload/synchronization is not complete enough for cbuf contents. The
consumption is now gated behind `RUZU_BC_UNIFORM_STREAM=1` (default off →
the direct guest-memory fallback path, which renders correctly). Re-enable
once the consumption is upstream-shaped end-to-end (descriptor update
template equivalent).

### 2026-07-06 — DIAGNOSED: "Press L + R" is not a rendering bug — the title screen is reached at ~285s

Fine-grained present dumps (every 24 presents) show the splash art region is
byte-identical across the entire visible phase: what renders at ~15-100s is
the **static loading splash**, not the title screen. A 300s run maps the
sequence: static splash (#480-#4320) → black + loading spinner for ~170s of
presents (#4560-#8640) → white flash (#8880-#9120, title-screen intro) →
splash again (#9360+, the actual title screen) at **~285s wallclock**. The
user quits before that. On yuzu the same point is reached in ~15-20s.

Host `sample` during the spinner phase (t=140-150): CPUCore_1 = **97.6% in
`A32Jit::run`, all inside JIT-generated code** (fragmented unsymbolized
buckets; no runtime-helper hotspots), cores 0/2/3 ~99% idle (game pins its
loading workers to core 1), GPU/present threads idle. No blocking waits
remain — the fiber-park fix holds. The bottleneck is now purely **rdynarmic
A32 codegen throughput** on the loading/decompression workers (the Yaz0
loop `0x00B92E04` documented above: 2 inline page-table lookups per copied
byte, no fastmem on this 16K-page host). Previously classified secondary;
now the primary remaining cause (~15x slower than yuzu's C++ dynarmic on
the same fastmem-less host).

Also observed: `[DRAW_OFFSCREEN]` reaches #131072 during the spinner phase
(133k+ draws diverted to the internal offscreen framebuffer, rt0 fmt=0) —
worth auditing separately, but the spinner phase is CPU-bound, not
GPU-bound.

**Next steps (rdynarmic perf campaign):**
- Profile the emitted code for the Yaz0 inner loop (RUZU_DUMP_JIT_MAP +
  host-profile attribution landed in rdynarmic) and compare instruction
  count per iteration vs upstream dynarmic arm64 output.
- Candidate wins: cache the last translated page (single-entry TLB) in the
  inline memory path; tighten the page-table lookup sequence; check
  register allocation quality in the hot block; block linking on the copy
  loop back-edge (benchmark already added in rdynarmic ba49e4e).

### 2026-07-07 — rdynarmic perf campaign: per-block codegen is at upstream parity; bottleneck is elsewhere

Investigated the spinner-phase JIT throughput theory in depth. Every suspect
checked came back **upstream-faithful**:

- **FPCR/FPSR churn is upstream-identical.** The hot spinner block
  (`0x0028A718`, 24.2% of core-1 samples) emits 40× `msr fpcr` + 11×
  `msr fpsr` + 11× `mrs fpsr` for 24 FP ops because MK8D runs with FPSCR
  mode bits = 0 (verified at compile time via `RUZU_LOG_A32_FPSCR_MODES`
  over 150s — every block descriptor mode is 0 — and at runtime via
  context-restore logging: only NZCV/QC variations, never DN/FZ/RMode).
  With mode 0, `MaybeStandardFPSCRValue` must swap to the ASIMD standard
  value (0x03000000) around every vector-FP op — upstream does exactly the
  same. **Proof**: dumped upstream dynarmic's live emitted code for the
  same guest block out of a running yuzu process (lldb `memory find` on
  the terminal's `movz w16,#0xA7E8; movk w16,#0x28` constant): upstream
  emits **exactly 40× `msr fpcr`, 11× `msr fpsr`, 11× `mrs`**, same FP
  ops, ~671 instructions vs our 753 (windowing/deferred-stub noise).
  Also verified faithful: inline page-table lookup (instruction-for-
  instruction), FPSR spill sites in the memory emitters (upstream spills
  in the same four places), VMSR translation, `emit_a32_set_fpscr`,
  terminal shapes and link slots, `descriptor_to_fpcr`, prelude FPCR
  load/restore.
- **The hot block is the game's audio DSP** (nn::atk-style software
  reverb/filter: per-sample loop, 4 channels, vmla/vmls feedback chains,
  float→s32 conversion). `RUZU_BLOCK_PROLOGUE_COUNT_PC` measured
  **46,896 iterations/sec** during the spinner — exactly realtime 48kHz.
  No over-mixing; audio pacing is correct. yuzu pays the same per-sample
  msr-fpcr cost.
- **yuzu reference re-established on this machine**: yuzu-cmd (zuyu
  build) reaches the animated MK8D title screen in **≤24s from a cold
  shader cache** on this Mac (Vulkan via manual MoltenVK ICD manifest +
  brew vulkan-loader; see scratchpad yuzucap/). ruzu: ~285s. The ~12x gap
  is real on identical hardware.
- **Scheduling divergence found (new prime suspect)**: during yuzu's
  loading phase, sample shows CPUCore_0/1/2 all busy (~38/39/25%) — the
  game parallelizes loading across three cores. Under ruzu, cores 0/2/3
  are ~99% idle and ONLY core 1 works. Also notable: yuzu's total busy
  CPU during loading ≈ 1.0 core-equivalent, i.e. similar instantaneous
  CPU usage to ruzu — yet it finishes 10x sooner, so ruzu's core-1 grind
  may be partly non-productive (starved loaders / repeated work) rather
  than slow codegen.

**Campaign redirect**: stop optimizing per-block codegen (it's at parity).
Investigate instead (a) why the game's loader threads never run on cores
0/2 under ruzu (thread affinity / core assignment / ideal-core handling in
the ported kernel scheduler), and (b) whether the spinner-phase core-1
work is productive loading or a starvation/retry pattern (SVC-summary +
thread-dump snapshots during the spinner).

### 2026-07-07 — FIXED: ruzu clobbered yuzu's disk shader cache (yuzu.app crashed with bad_alloc)

User report: yuzu.app used to work, stopped working. Root cause: before
commit 40dc9ff (Jul 5 23:38) ruzu, when it selects the legacy yuzu data
root, wrote its own pipeline-cache files into
`~/.local/share/yuzu/shader/0100152000022000/` (files dated Jul 5 09:21).
yuzu then died deserializing the foreign `vulkan.bin`
(`VideoCommon::FileEnvironment::Deserialize` → `std::bad_alloc`, verified
via lldb backtrace). Fix applied: moved the directory aside to
`0100152000022000.corrupt`; yuzu rebuilt its cache and boots to the title
screen again. 40dc9ff already prevents recurrence (ruzu shader cache stays
under the ruzu root); the `.corrupt` dir can be deleted once yuzu is
confirmed healthy.

### 2026-07-08 — FIXED: host-thread-IPC wedge = switch-fiber same-thread-skip race (context_guard leak)

With `RUZU_SERVER_THREAD_IPC_ALL=1`, MK8D wedged 100% of runs at t≈30s:
SIGUSR1 dump showed the Main thread RUNNABLE in svc 0x21 (SendSyncRequest to
friend:a / prepo:a) at the front of core 0's priority queue while core 0 sat
"idle" with `needs_scheduling=false`, and lldb showed CPUCore_0's switch
fiber spinning at 100% in `schedule_impl_fiber_loop`'s upstream-faithful
`while (!context_guard.try_lock())` loop. New `ctx_guard` attribution in the
SIGUSR1 dump (RUZU_TRACE_CTX_GUARD) showed the wedged thread's guard locked
by the switch fiber itself and never unlocked.

**Root cause** (`k_scheduler.rs::switch_thread_impl`): the function compared
`next` against the **captured** `switch_cur_thread` instead of the
scheduler's own `current_thread`. In the switch fiber's retry loop (a
host-thread `end_wait` fires `needs_scheduling` mid-switch — common with
host-thread IPC because replies race the client's park), iteration 1
switches 75→idle (current=idle), iteration 2 picks the re-woken 75 and hits
`switch_thread_impl(cur=captured 75, next=75)` → same-thread fast return →
bookkeeping never updated (current stays idle) while the code still reloads
75 and yields to it. The scheduler now believes idle is current while 75
runs; 75's context_guard (locked at the reload) is never unloaded/unlocked,
and the next attempt to schedule 75 spins forever.

Upstream cannot hit this: `KScheduler::SwitchThread` reads
`GetCurrentThreadPointer(kernel)` — the per-core pointer SwitchThread itself
updates every call — so retry iterations always compare against the true
current. Fix: `switch_thread_impl` now prefers `self.current_thread`
(scheduler-owned, updated by every switch) over the captured
`switch_cur_thread`, with TLS resolution last.

Validation: 2× `RUZU_SERVER_THREAD_IPC_ALL=1` runs now progress through the
full boot timeline (logs to t≈98/100s and t≈117/120s, DRAW_OFFSCREEN spinner
phase reached, 0 leaked guards in dumps) vs 3/3 wedges at t≈30s before;
baseline (inline IPC) run unaffected (t≈97/100s); `cargo test -p core --lib
k_scheduler` 17/17. The same race exists in inline mode (any cross-core
`end_wait` can fire mid-switch) — this is a strong candidate for the OPEN
t≈11-12s intermittent cold-boot wedge below. Also added: per-thread
`context_guard` lock/unlock attribution (`RUZU_TRACE_CTX_GUARD`, shown in
the SIGUSR1 dump) and `[SCHED_PICK]` remains available via
`RUZU_TRACE_SCHED_PICK` + `RUZU_TRACE_SCHED_STATE=<tids>`.

Host-thread IPC (`RUZU_SERVER_THREAD_IPC_ALL=1`) is now viable for the
per-service promotion plan (DIFF.md): next step is validating service groups
(binder/vi first) and comparing boot timing vs inline.

### 2026-07-08 — AUDIT: KConditionVariable is exonerated; loading bottleneck root-caused to GPU-thread shader recompilation

**KConditionVariable/KAddressArbiter audit (line-by-line vs upstream)**:
`signal`/`signal_impl`/`wait`/`wait_for_address`/`signal_to_address` are
faithful — has-waiter-flag protocol (set on wait, clear on empty tree),
mutex-waiter requalification in SignalImpl (no thundering herd,
requeue_on_owner ≈ 100% as expected for signal-under-lock),
UpdateLockAtomic via the process exclusive monitor, cv tree ordered by
(cv_key, priority, tid), seq-cst fences in place. New always-on protocol
counters (`[CV_STATS]` in the SIGUSR1 dump) confirm healthy runtime
behavior: waits_timeout0=0 (no spurious timeouts), unlocks_empty ≈ 2%
(benign races), and the 69% empty-signal rate is **game behavior** — the
cv word reads 0 at signal time (`empty_hint0` ≈ 100% of empty signals),
i.e. MK8D's nn::os signals without consulting the has-waiters hint;
upstream receives the same syscalls. The condvar churn is a symptom of
pipeline pacing, not a kernel bug.

**The actual loading bottleneck, measured end-to-end** (fresh spinner
sample at t=80, 6s):
1. `[NVHOST_CTRL_WAIT]` tracing shows only **6 host stalls in 95s — but
   one lasted ~44s** (t=42.7→87.0: waited for syncpt 1 to reach 7762
   while it was at ~509; the GPU advanced ~165 increments/s). The game
   runs thousands of fences ahead of the GPU, its event waits time out
   3× (fails>2), and ruzu falls back to `wait_host_stalled` — which,
   because HLE runs inline, also freezes CPU core 0 (the Main thread)
   for the whole catch-up, starving the loader pipeline (hence the
   condvar churn and idle cores).
2. The GPU thread itself is saturated: 82% in
   `DmaPusher::process_commands`, and **~87% of draw time is inside
   `PipelineCache::current_graphics_pipeline_with_shared_cache →
   build_pipeline_keyed_from_environments → compile_stage_from_environment
   → shader_recompiler::compile_shader_from_env`** — dominated by
   `structured_control_flow::structure_cfg_detailed` /
   `find_last_goto_in_tree` (deep recursion + Vec grow churn) and
   `ssa_rewrite_pass`. At t=80+, long past boot, **shaders are being
   (re)compiled on the per-draw path** — the pipeline cache is not
   hitting during the spinner phase (relates to the 131k DRAW_OFFSCREEN
   note above). Secondary Vulkan costs on the same path:
   `vkCreateGraphicsPipelines` (MoltenVK MTLRenderPipelineState) and
   per-request `vkAllocateMemory` in `StagingBufferPool`.

**Next steps (redirected campaign)**:
- Instrument pipeline-cache hit/miss (and miss cause: new key vs
  invalidation vs unstable key fields) during the spinner; compare
  consecutive-draw keys for stability. Suspect: a non-stable field in
  the graphics-pipeline key or shader-hash invalidation churn.
- Fix the per-draw recompile (expected to collapse the 44s syncpoint
  stalls and most of the 285s load time).
- Optimize `structure_cfg_detailed`'s quadratic goto search + preallocate
  in `find_last_goto_in_tree`/`find_label_in_tree` (upstream uses
  intrusive lists; our Vec-based port regrows constantly).
- `StagingBufferPool::request_buffer` should reuse allocations (upstream
  caches staging buffers; per-request `vkAllocateMemory` shows in the
  profile).
- Longer term: `wait_host_stalled` should not block a guest core inline
  (host-thread IPC promotion for nvdrv, or upstream's event-based path
  should keep fails at 0 once the GPU keeps up).

### 2026-07-08 — MEASURED: GPU-thread budget during the spinner; pipeline keys are unstable across runs

Instrumented the graphics pipeline cache (`[PIPELINE_STATS]`, always-on)
and `vkCreateGraphicsPipelines` (`[VK_PIPELINE_CREATE]`), plus per-shader
translate timing (existing `RUZU_TRACE_SHADER_WORDS`). Findings on a
~110s MK8D run + a 20s GPU-thread sample mid-stall (t=45-65):

- **Intra-run cache health is fine**: fast_hits=161k, slow_hits=33k,
  misses=256 (99.87% hit rate). The earlier "recompile per draw" reading
  was a sampling-window artifact.
- **But keys are unstable across runs**: the disk pipeline cache has
  grown 443 → **9,325 entries** (each debug run appends ~250 "new"
  pipelines) while every fresh run STILL takes ~250 runtime misses after
  preloading all 9,325. The runtime-computed keys do not match the
  preloaded ones — ~250 keys/run differ in unique_hashes and/or
  fixed_state (miss classification: hash_only=46 fixed_only=38 both=171;
  fixed-state diffs concentrate in attributes/divisors_strides/attach).
- **Each miss is expensive and runs on the GPU thread**: shader translate
  p90=13ms but max=1.77s (`structure_cfg_detailed` /
  `find_last_goto_in_tree` quadratic recursion), plus synchronous MoltenVK
  `vkCreateGraphicsPipelines`. Mid-stall the GPU thread is 91% busy:
  ~28% compiling shaders, ~37% in `Gpu::composite_layers` →
  `RendererVulkan::composite_impl`, rest DMA dispatch. That undercuts the
  syncpoint rate to ~165/s → the game runs thousands of fences ahead →
  the 44s `wait_host_stalled` core-0 freezes documented above.

**Next steps**:
1. Root-cause the cross-run key instability (dump a few runtime-miss keys
   and diff against the disk entries with matching unique_hashes; suspect
   a fixed_state field affected by boot-order/nondeterminism, and/or
   shader unique_hash sensitivity to code-size probing). This single fix
   should make the preload actually eliminate runtime compiles AND stop
   the disk cache from growing unbounded.
2. Move runtime pipeline builds off the GPU thread (upstream uses
   VkPipelineBuilder workers + optional async shaders).
3. Optimize `structure_cfg_detailed` quadratic goto search.
4. Profile `composite_layers` (37% of the GPU thread during loading is
   suspicious for a phase that presents a mostly-static image).

### 2026-07-08 — FIXED: pipeline-key instability (dynamic state baked into keys) + cache convergence

Root cause of the disk-cache growth and perpetual runtime compiles:
`FixedPipelineState::refresh` filled `dynamic_state` unconditionally,
while upstream (fixed_pipeline_state.cpp:142-165) only refreshes each
group when the covering extension is missing. With EDS declared in the
pipeline's dynamic states, every per-draw variation of cull/depth/
stencil/logic-op state minted a distinct pipeline key for the same
logical pipeline. Fix: `refresh(draw, features)` now takes upstream's
`DynamicFeatures` parameter, assigns the extension flags first, and
gates the DynamicState::Refresh/Refresh2/Refresh3 + attachments groups
exactly like upstream. CACHE_VERSION 12→13 purges the bloated caches.

**INTENTIONAL DIVERGENCE (documented in refresh())**: `vertex_strides`
stays in the key on all devices. Upstream excludes it under
extended_dynamic_state (strides then come from vkCmdBindVertexBuffers2 +
VK_DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE), but that combination
renders black on MoltenVK here (A/B window captures). Revisit once the
dynamic-stride path is debugged; cost is a few extra variants per shader.

Validation: A/B window captures at t=30s show the MK8D splash identical
to baseline (bisection proved the black screen came from the strides
half, now excluded). `[PIPELINE_STATS]` miss causes: dynstate=0 (was the
dominant cause). Cache converges instead of exploding: cold=352 runtime
creates → warm1=551 (boot progresses further) → warm2=**99**, disk cache
829 entries / 6.4MB (was 9,325 entries growing ~250/run without ever
converging). fixed_pipeline_state tests 25/25 incl. new regression test.

Remaining from the GPU-thread budget entry above: move pipeline builds
off the GPU thread, structure_cfg quadratic goto search, composite_layers
37%, StagingBufferPool vkAllocateMemory reuse.

### 2026-07-08 — FIXED: GPU thread stalled 37% on the swapchain mutex inside Composite

The 20s mid-stall GPU-thread profile showed 37% of samples in
`composite_impl` — all of it blocked in `pthread_mutex_wait`, not
compositing. `composite_impl` locked `Mutex<Swapchain>` just to read
`get_image_count()`/`get_image_view_format()`, while the present thread
holds that same mutex across all of `copy_to_swapchain` — including
`acquire_next_image`, where MoltenVK blocks on the next drawable for up
to a vsync period per presented frame. Upstream `RendererVulkan::
Composite` reads those getters without any lock (renderer_vulkan.cpp:163).

Fix: `PresentThreadContext` caches both values in atomics (the
`image_count` atomic already existed), updated at swapchain
(re)creation; `composite_impl` reads them via new lock-free
`PresentManager::swapchain_image_count()/swapchain_image_view_format()`.

Validation: GPU-thread sample after the fix: mutexwait 4296→0 samples,
composite 37%→2%, cvwait ~1.5% (the thread now does useful work);
MK8D splash at t=30s identical to baseline.

### 2026-07-08 — FIXED: structurizer path-clone churn (~10x cheaper shader translation)

`find_last_goto_in_tree`/`find_label_in_tree` cloned the `Vec<usize>`
statement path once per visited node; with the goto pass re-scanning the
whole tree after every removed goto, big MK8D shaders spent most of their
translation time growing/freeing path vectors (GPU-thread profiles:
`RawVec::grow_one` under `structure_cfg_detailed`; worst shader 1.77s).
Both scans now use a backtracking path buffer (push/pop around recursion,
clone only when a match is recorded) — semantics unchanged.

Cold-boot measurement (RUZU_TRACE_SHADER_WORDS): 928 compiles in
2,350ms total, p90 2ms, max 347ms — vs 196 compiles in 4,207ms, max
1,767ms before (≈10x per-compile). Splash at t=30s intact. The remaining
347ms worst case is the goto pass's O(gotos × tree) rescan itself;
migrating to upstream's intrusive-list structure would shave it further
if it still matters after the pipeline-build-workers work. NOTE: 3
structured_control_flow unit tests (conditional_exit/unconditional_exit/
conditional_forward_branch) fail identically before and after this
change — pre-existing breakage, not introduced here.

### 2026-07-08 — Pipeline disk cache / async pipeline preload status

Objective file
`/Users/vricosti/.codex/attachments/34ce0e81-0796-4f62-a368-d641ee3f3e7c/pasted-text-1.txt`
requires the Vulkan disk pipeline cache path to match upstream:
`PipelineCache::LoadDiskResources`, async build ownership, disk-rebuildable
`GraphicsPipelineKey`, and MK8D verification.

Current status:
- `LoadDiskResources` parses
  `/Users/vricosti/.local/share/ruzu/shader/0100152000022000/vulkan.bin`
  through `VideoCommon::LoadPipelines`, queues rebuild work, inserts rebuilt
  compute/graphics pipelines into the Vulkan caches, and serializes
  `vulkan_pipelines.bin`.
- Runtime graphics already uses upstream-style `ThreadWorker` final pipeline
  creation for async shaders, and disk graphics rebuild uses `FixedPipelineState`
  instead of a live `DrawCall` for render pass, vertex input, raster/blend/depth
  state.
- 2026-07-08 change: compute pipeline construction now also has the upstream
  nullable-worker model (`ComputePipeline::new_with_worker`), and
  `PipelineCache::CreateComputePipeline` passes `&workers` when
  `use_asynchronous_shaders` is enabled. Disk preload remains build-job
  queued with synchronous inner construction, matching upstream
  `LoadDiskResources(... build_in_parallel=false)`.

Verification:
- `cargo check -p video_core` passes.
- `cargo test -p video_core pipeline_cache --lib` passes (15/15).
- `cargo build --release --bin ruzu-cmd` passes.
- MK8D short run
  `/tmp/ruzu_mk8d_pipeline_async_1783545423`: loaded `vulkan.bin` at t=0.67s,
  reported `Total Pipeline Count: 2932 (built=1055, skipped=1877)` at t=4.61s,
  and sample at t≈10s contained no `shader_recompiler` frames.

Remaining validation gap:
- This proves disk preload is active and removes shader-recompiler domination
  from the captured t≈10s sample, but it does not yet prove the visual/audio
  transition reaches parity with yuzu. A longer interactive MK8D run is still
  needed to confirm the first logo advances around frame ~360 / ~3-4s with
  the expected audio transition.

### 2026-07-08 — FIXED: depth-only draws were dropped (rt0 fmt=0 → offscreen fallback)

`update_render_targets_and_get_rt0_framebuffer` bailed out whenever
RT0 had format 0, throwing every depth-only draw (shadow maps, depth
pre-pass — no colour target bound at all) into the internal offscreen
fallback. MK8D issues 260k+ such draws during loading; upstream treats
format 0 as "colour target disabled" (the per-target binding already
did) and renders with the zeta attachment only. Fixed the early-out to
bail only when nothing is bound, skipped the RT0-by-address fallback for
disabled RT0, and made the framebuffer key/rt0_cpu fall back to the
depth image for colour-less framebuffers (render_pass_cache and
create_framebuffer_owner already supported 0 colour attachments).
Validation: [DRAW_OFFSCREEN] 262k+ → **0**, splash at t=30s intact, no
new MoltenVK errors.

### 2026-07-09 — FIXED: host-thread wakes never sent IPIs (the mid-spinner "freeze" below)

Root cause of the 2026-07-08 mid-spinner freeze:
`default_enable_scheduling` (k_scheduler_lock.rs) had a `_ => no-op`
branch for "no current thread": when a HOST thread (CoreTiming's
KHardwareTimer::do_task, ServerManager, GPU thread) released the
scheduler lock, the `cores_needing_scheduling` mask was silently
dropped — no IPI was ever sent. Upstream `KScheduler::EnableScheduling`
calls `RescheduleCores(kernel, cores_needing_scheduling)` in exactly
that case. Every timeout-paced wake (condvar/sleep timeouts delivered by
the timer thread) marked the waiter RUNNABLE and set needs_scheduling on
its core, but a sleeping core never woke — the frozen-instance dump
showed core 2 asleep with needs=true, is_interrupted=false,
highest=RUNNABLE-thread. During the loading spinner (cores mostly idle)
every such wake waited for an unrelated event, degrading the game to
~1% speed (window captures byte-frozen). The "leaked ctx_guard" threads
in that dump were TERMINATED loaders (ThreadState 3 = TERMINATED;
upstream intentionally skips their unlock) — red herring.

Fix: the no-current-thread branch now calls
`KScheduler::reschedule_cores(cores_needing_scheduling)` (upstream
semantics). Validated: spinner window captures keep changing through
t=305s (previously byte-identical from t≈142-203s), k_scheduler tests
17/17, [CV_STATS] unchanged-healthy. Likely also implicated in the
older intermittent wedges (t≈11-12s cold boots, "Sig-A" t≈210-240s).

**Post-fix profile (t=150, all of today's fixes in)**: GPU thread
saturated 99% doing real work — 78% DMA dispatch/draws (the depth-only
draws now actually execute), 10% in MoltenVK
MVKCmdBeginRenderPass::encode (render-pass fragmentation: one
MTLRenderCommandEncoder per pass), 7% composite; shader compiles ≈ 0.
CPU cores 70-99% idle waiting on the GPU (Main hits wait_host_stalled
~14%). The loading bottleneck is now genuinely GPU-thread draw
throughput (~2,500 draws/s vs yuzu's ~30k/s). Next targets: render-pass
batching (draws alternating framebuffers must not reopen a render pass
each time), then per-draw encode costs (descriptors/uploads).

### 2026-07-09 — FIXED: Maxwell sched-word grid was anchored at absolute offset 0; MK8D shaders anchor it at the code start

With the game now reaching the title screen, the user reported the
"Press L+R to start" texture missing and the attract demo rendering
black. The run log showed 645k+ "Unknown Maxwell opcode" warnings and
1,193 pipelines rejected with "FFMA CC" — 100% of the unknown words sat
at `abs % 32 == 16` and had sched-word bit patterns.

Memory dump of live shaders (RUZU_TRACE_SHADER_WORDS + new
[SHADER_HEAD] dump) proved the layout: SPH is 0x50 bytes, the first
word at `code = sph + 0x50` is ALWAYS a sched word, then every 4th
word (`code + 0x20k`). The sched grid is anchored at the START OF THE
CODE, not at absolute offset 0. MK8D ships two shader populations:
`code % 32 == 0` (absolute grid happens to match — these rendered
fine, e.g. the splash) and `code % 32 == 16` (absolute grid skips one
REAL instruction per bundle and decodes the sched words as
instructions — garbage translations, phantom "FFMA CC" rejects, the
black demo scene).

DOCUMENTED DIVERGENCE from upstream: upstream `Location` (location.h)
hardcodes the absolute `offset % 32` grid and works because the games
it sees keep shader code 32-byte aligned within the program region;
MK8D's `0x...E0` start addresses (+0x50 SPH = `0x...30` code starts)
break that assumption. Rust `Location` now carries a `phase`
(`code_start % 32`); `Location::new_code_start` anchors the CFG at the
code start, branch targets inherit the phase via `with_offset`, and
`is_sched_control_word` in translate is simply `word_index % 4 == 0`
(the slice starts at the code start). Phase 0 = exactly upstream
behaviour.

Validated (fresh shader cache): "Unknown Maxwell opcode" 678,221 → 0,
"FFMA CC" pipeline rejects 1,252 → 0. Splash still pixel-clean.

### 2026-07-09 — FIXED: submit-worker splash corruption = stream-buffer per-frame reset (pre-existing race); worker now default ON

The magenta splash region below was NOT a bug in the submit worker: it
was a pre-existing data race in `StagingBufferPool` that the worker's
deferral window made deterministic. `new_frame()` reset
`stream_offset = 0` every frame, so `bind_mapped_uniform_buffer`'s
mapped uniform writes recycled stream regions the GPU (or a
not-yet-submitted job) was still reading. Upstream never does this:
`GetStreamBuffer` (vk_staging_buffer_pool.cpp:105) splits a 128MiB
stream buffer into NUM_SYNCS=16 regions stamped with the submission
tick, reuses a region only once `KnownGpuTick()` passes its stamp, and
falls back to a dedicated staging buffer instead of waiting.

Ported that verbatim: `stream_iterator`/`stream_used_iterator`/
`stream_free_iterator` + `stream_sync_ticks[16]`,
`are_stream_regions_active` against `Scheduler::known_gpu_tick`,
stream capacity 1MiB→128MiB (upstream MAX_STREAM_BUFFER_SIZE), request
threshold = one region size like upstream `Request`. Also fixed a
latent texture_cache hole found on the way: `refresh_contents_with_
reader` skipped the guest upload when `CPU_MODIFIED` was clear even if
`ensure_image` had just RECREATED the backend image (blank contents) —
now a recreation forces the re-upload.

Validated: worker ON splash pixel-clean at t=30/45 (window captures,
6,920,713-byte clean signature), worker OFF unchanged, targeted
video_core tests pass. The submit worker is now **default ON** like
upstream (`RUZU_VK_SUBMIT_WORKER=0` forces synchronous submits, `=sync`
drains per push for bisecting).

### 2026-07-09 — superseded (fixed above): Vulkan submit worker ported, gated behind RUZU_VK_SUBMIT_WORKER=1 (rendering race to fix)

Ported the submit half of upstream's Scheduler::WorkerThread: flush()
hands the fully recorded command-buffer pair to a FIFO "VulkanWorker"
thread which performs vkQueueSubmit (where MoltenVK does ALL Metal
encoding). Recording + end_command_buffer stay on the GPU thread
(Vulkan requires external sync on the command pool for recording; the
timeline tick is still taken on the GPU thread so waiters see it
immediately — timeline semaphores allow wait-before-signal). Flushes
that signal BINARY semaphores (the present manager's render_ready)
drain the worker before returning, since binary semaphores forbid
wait-before-signal submission ordering.

Measured with the worker ON: GPU-thread vkQueueSubmit samples 1182→0,
MVKQueue::submit 2364→0 on the GPU thread (absorbed by VulkanWorker at
~26% load), GPU thread 87% useful work and sometimes waiting for
input, game nvhost stalls 14%→6%.

**BUT**: a deterministic magenta-corrupted texture region appears on
the MK8D splash (same spot, every run, persists at t=45). A/B with the
worker stashed (async-compute commit still in) renders clean → the race
is in the deferred submit. Suspect: some resource (image/buffer/
framebuffer) is destroyed or reused between flush() and the worker's
encode; that was safe before because the synchronous vkQueueSubmit had
already encoded. Staging pool and descriptor rings retire on the GPU
timeline tick (checked — safe); look for immediate vkDestroy*/reuse
paths gated on CPU/submitted tick (texture_cache
finish_pending_backend_deletions, evict_rt_framebuffers,
framebuffer/image-view recreation). Default OFF until fixed.

### 2026-07-09 — DIAGNOSED: GPU-thread draw throughput gap = inline Vulkan execution (upstream uses a scheduler worker thread)

With the GPU thread now saturated on real work, aggregated its 8s
profile: **~40% goes to MoltenVK encode/submit on the GPU thread itself**
(MVKQueue::submit 1816/4435 samples, vkQueueSubmit 908, flush_impl 921,
MVKCommandEncoder::encode 892 incl. beginMetalRenderPass 808). Render
pass churn measured with new `[RP_STATS]` counters: ~1,300 begins/s —
76% genuine framebuffer switches (the game alternates depth-only and
colour targets), 24% forced reentries; upstream emits the exact same
EndRenderPass barriers (vk_scheduler.cpp:291-326), so that part is
parity.

The structural difference: upstream `Scheduler::Flush` only queues the
recorded chunk for its **worker thread** (vk_scheduler.cpp:46
`worker_thread = std::jthread(WorkerThread)`), which replays commands,
encodes and submits off the hot path — yuzu's "VulkanWorker" thread was
visibly busy in its boot sample. ruzu's scheduler executes every chunk
INLINE on the GPU thread (`dispatch_work` runs the closures, flush_impl
calls vkQueueSubmit synchronously), so all MoltenVK encode/submit cost
lands on the DMA-pusher critical path. `queue_fence` (one flush per
guest syncpoint fence) makes this fire constantly during loading.

**Next**: port the upstream scheduler worker thread (chunks already are
boxed closures in ruzu — the recording side is ready; needs the
WorkerThread loop, AcquireNewChunk reserve, per-chunk submit handoff and
master-semaphore tick handshake). Expected to reclaim ~40% of the GPU
thread and materially raise the ~2,500 draws/s ceiling.

### 2026-07-08 — superseded: game freezes mid-spinner at t≈140-200s (see 2026-07-09 fix above)

End-to-end timing runs revealed the spinner IMAGE FREEZES (byte-identical
window captures) at t≈142s / t≈203s in consecutive runs — the boot never
reaches the title screen not because loading is slow but because the game
wedges. SIGUSR1 + host sample on a frozen instance (t=250):
- 8+ threads (incl. the Main thread, tid 74, and freshly spawned loaders
  113-119) have `ctx_guard locked=true` with `last_lock=switch_fiber` and
  no matching unload — permanently unschedulable.
- Unlike the RUZU_SERVER_THREAD_IPC_ALL wedge (fixed in d106a16), NO
  switch fiber is spinning: all cores sleep in PhysicalCore::idle while
  core 2 shows `needs_scheduling=true, highest=83, current=idle` — a
  lost reschedule on top of the leaked guards.
- This matches the old "Sig-A wedge" note (frame production collapses
  t≈210-240s, cores wedge one by one). d106a16 fixed one leak path
  (stale switch_cur comparison); at least one more remains, plus a lost
  IPI/needs_scheduling wake. Next: trace ctx_guard lock/unload pairing
  per thread (RUZU_TRACE_CTX_GUARD already records sites) around the
  freeze moment, and audit schedule_impl_fiber_loop exits that bypass
  both yield and unlock (e.g. resolve/host-context failure paths).

### 2026-07-06 — OPEN: intermittent early wedge at t≈11-12s (~2/8 cold runs)

Some runs stop presenting right at the preload→boot transition (0-3 present
dumps, log goes silent at t≈11-12, no panic). Matches the pre-existing
"cold MK8D boots are less stable" note. Not reproduced under a 6-attempt
loop with 35s windows; needs a SIGUSR1 thread dump captured on a wedged
instance to classify.

### 2026-07-10 — FIXED: warm portable pipeline cache rendered the MK8D title entirely black

The black window was reproducible only after a cold run had generated the
portable Vulkan pipeline cache. Controlled A/B at present frame 600:

- cold cache (`Total Pipeline Count: 0`): complete color title,
  `mean=0.475541`, 943105 colors;
- warm cache (`97 built, 0 skipped`): guest present source was all zero,
  `mean=0`, one color;
- cold and warm emitted the same 92 SPIR-V modules byte-for-byte after stage
  normalization, so shader translation and serialized environments were not
  the cause.

Root cause: `FixedPipelineState::refresh` built each `BlendingAttachment`
without copying `draw.color_masks`. If blending was disabled, the attachment
remained zero. The live pipeline path used `draw.color_masks` directly and
rendered correctly, while the disk path rebuilt its Vulkan pipeline from the
serialized fixed state and therefore set `VkPipelineColorBlendAttachmentState
::colorWriteMask = 0`. Every affected draw executed but wrote no color.

Upstream `FixedPipelineState::BlendingAttachment::Refresh` always copies the
four color-mask bits before its early return for disabled blending. Rust now
does the same. Cache version 14 was bumped to 15 because existing payloads
contain irreparably zeroed masks.

Validation with a newly generated v15 cache:

- cold source: `mean=0.475541`, 943105 colors;
- warm preload: `100 built, 0 skipped`;
- warm source: `mean=0.475541`, 943105 colors;
- cold/warm source files are byte-identical (`compare AE=0`);
- captured warm swapchain is colored (`mean=0.475889`, 1558960 colors).

Invalidated hypotheses retained for traceability:

- **INVALIDATED for this black-frame regression:** stale/incompatible SPIR-V
  from disk. All 92 cold/warm modules matched exactly.
- **INVALIDATED for this black-frame regression:** Vulkan driver cache. The
  cold run used an empty 48-byte driver cache and the warm failure was fully
  reproducible from the portable cache alone.
- **INVALIDATED for this black-frame regression:** submit-worker/present-thread
  ordering. Disabling `VulkanWorker` did not restore the guest present source;
  the source itself was black on the bad warm cache.

This fixes the newly introduced all-black warm-cache regression. It does not
resolve the separate OPEN issue where MK8D remains on the first logo much
longer than yuzu; continue that timing/scheduler investigation independently.

### 2026-07-10 - FIXED: MK8D title prompt used stale animated instance data

The yuzu captures in `~/Movies/Capture d'ecran 2026-07-10 a 10.42.*.png`
confirm that `Press L+R to start` keeps a fixed position and size. Only its
packed RGBA colors and lens-flare layers animate.

The MK8D vertex shader at code address `0xB00080` uses `I2F.U8` selectors to
unpack those RGBA bytes. Ruzu's `integer_floating_point_conversion.rs` ignores
the integer size and selector fields and converts the shifted 32-bit word
directly. Upstream first emits `BitFieldExtract(offset=selector*8, count=8)`,
which explains why ruzu saturates/fades edge glyphs and makes fixed geometry
look as if it moves or shrinks.

The IR/SPIR-V integer-to-float conversion matrix and 64-bit
select/packing dispatch were completed, then upstream `I2F` was ported
literally. This fixed the packed-byte interpretation but did not fully fix the
animation. The remaining movement/flicker was stale SSBO data:

- guest checksums for the two alternating instance buffers changed while
  `SynchronizeBuffer` reported one initial upload followed by cache hits;
- Vulkan's common buffer cache used a no-op `DeviceTracker`, so registering a
  buffer never changed the corresponding guest page to
  `RasterizerCachedMemory`;
- the ARM64 rdynarmic backend selected fastmem even though its default null
  exception handler reported `SupportsFastmem() == false`, bypassing the
  callback required for protected/cached pages;
- `Memory::handle_rasterizer_write` translated the CPU virtual address as one
  physical address instead of using upstream `ApplyOpOnPointer` to fan out the
  host pointer to its SMMU device-address aliases.

The Vulkan cache now uses `MaxwellDeviceMemoryManager` as its tracker, the
ARM64 backend requires `supports_fastmem()` like upstream, and the memory write
path uses the SMMU host-pointer fan-out with upstream's per-core
`last_address`/dirty-manager ordering. A 30-second validation observed 960
prompt SSBO synchronizations and 960 uploads; no checksum change was missed.
The text remains at a fixed position and transitions from dark/black to blue.

The faint moving blue glow behind the prompt is present in the yuzu reference
captures too and is the intended lens-flare layer. Side-by-side crops show no
second displaced text after the cache fix.

Long-run attract-mode validation (180 seconds, warm v15 pipeline cache): the
title prompt fix does **not** fix the attract cinematic. After the title phase,
the window becomes entirely black; the previously visible blue loading spinner
is also absent. This is not a guest/GPU freeze:

- buffer-queue traffic continues past frame 2048;
- render-pass begins continue from 122880 to 241664 during the black phase;
- 122 additional runtime graphics pipelines are created (1117 -> 1239);
- no panic, Vulkan device loss, unresolved IR value, unknown Maxwell opcode,
  or Rust-side graphics-pipeline creation failure is logged.

MoltenVK repeatedly reports `VK_ERROR_FEATURE_NOT_PRESENT: Metal does not
support disabling primitive restart`, including during runtime pipeline
creation. This is a confirmed backend/driver compatibility signal but is **not
yet proven causal**: `vkCreateGraphicsPipelines` still returns success and the
Rust pipeline-failure path is not entered. The next attract-mode investigation
must dump the guest present source and the principal render targets during the
black phase, then determine whether the scene is already black before
nvnflinger composition or is lost in the final copy/present path.

Follow-up investigation resolved that split:

- The black interval is the game's shader/loading phase. A present-source dump
  at 4608 contains the expected MK8D logo and cyan spinner, even when the
  foreground window made them difficult to see.
- The later attract scene was already wrong in the guest HDR render target
  `0x524C10000`, before nvnflinger and swapchain composition. At present 5000,
  the HDR/source/composited means were `0.816/0.869/0.854`, and all three
  showed the same fixed geometry becoming progressively white.
- **Active root divergence found:** the Vulkan framebuffer owner selected an
  arbitrary `ImageView` from a `HashMap` when constructing image barriers for
  a shared `ImageId`. This was wrong for MK8D's layered/mipmapped HDR and bloom
  targets, so barriers could synchronize a different mip/layer while stale
  values were repeatedly fed through additive passes. The owner now stores the
  exact bound view range and preserves upstream `rt_map`, view size, samples
  and layer count.
- A separate clear divergence was corrected: Rust silently skipped every
  color clear where `clear_surface.RT != 0`, while upstream clears the selected
  attachment. A 70-second `RUZU_TRACE_VK_CLEAR` run observed 46236 MK8D clears,
  all targeting RT0, so this is **confirmed parity work but invalidated as the
  active attract-mode root cause**.
- The extra RT0 transfer clear (`vkCmdClearColorImage`) was removed. Upstream
  uses the render-pass `vkCmdClearAttachments` path and does not force an
  outside-render-pass operation after every full RT0 clear.

Initial post-fix validation at present 5000 reached the loading phase sooner
(74s instead of 145s) and no longer accumulated white: HDR/source/composited
means were `0.250/0.535/0.577`. A later present-10000 validation is in progress
to verify the actual moving attract scene rather than the earlier loading
frame.

That longer validation is currently interrupted by a kernel prerequisite, not
by shader compilation. A macOS sample taken while presentation stopped showed:

- the GPU queue and all `VkPipelineBuilder` workers idle;
- CPU cores 2 and 3 idle, and core 1 waiting in the IPC path;
- CPU core 0 blocked in
  `NvMapDevice::ioc_alloc -> KPageTableBase::lock_for_map_device_address_space
  -> KLightLock::lock_slow_path -> pthread_cond_wait`.

Upstream `KLightLock::LockSlowPath` never waits on a host condition variable:
under `KScopedSchedulerLock` it adds the current `KThread` to the owner's
priority-inheritance waiter list and calls `BeginWait`, allowing another guest
fiber to run on the same core. Ruzu's `KLightLock` instead uses a host
`Condvar`, which freezes the whole physical-core host thread and prevents the
guest fiber that owns the lock from running. This is a confirmed architectural
divergence and the active prerequisite for continuing attract-mode validation.
The interrupted validation must resume after `KLightLock` uses the upstream
KThread waiter/ownership-transfer path.

Invalidated/refined hypotheses retained for traceability:

- **INVALIDATED:** VSync cadence caused the prompt movement. It did not alter
  the instance data consumed by the draw.
- **INVALIDATED:** the prompt needed draw 17 or draw 34 to be skipped. Skipping
  either only hid part of the upstream effect and was diagnostic, not a fix.
- **RETRACTED:** stale SSBO synchronization was previously marked invalidated
  after an incomplete experiment. End-to-end source-checksum/upload tracing
  later proved it was the main remaining defect.

## Success Criteria

- First logo transitions after roughly 3-4 seconds of visible time, like yuzu.
- Audio transition matches the visual transition, including the "Mario Kart
  eight" voice line.
- No long-lived first-logo period remains after pipeline preload completes.

---------
⏺ Pendant que le run de validation tourne (150s, au-delà du point de crash à ~82s), voici la réponse à ta
  question, changement par changement :

  Fidèle à upstream ou adaptation ?

  1. Park de fibre dans wait_for_dequeue_condition (nvnflinger) — adaptation, documentée
  - Upstream : DequeueBuffer tourne sur un thread hôte de service (ServerManager). Bloquer une condvar là est
  inoffensif : le thread guest appelant est déjà parqué dans le noyau le temps de l'IPC
  (KClientSession::SendSyncRequest → BeginWait).
  - Ruzu : les handlers HLE tournent (pour l'instant) inline sur la fibre du core guest appelant. Le même
  condvar.wait upstream y gèle le core émulé entier — c'est un artefact de l'architecture inline de ruzu, pas
  du code upstream.
  - Pourquoi nécessaire : l'adaptation reproduit l'effet upstream (l'appelant guest dort dans le noyau
  pendant que le service attend un buffer) avec les primitives noyau elles-mêmes portées d'upstream
  (begin_wait_with_queue, KScopedSchedulerLockAndSleep, end_wait). La voie 100 % upstream serait de router le
  binder vers le routing IPC host-thread — il existe (env-gated) mais a été instable historiquement (TODO.md
  option 2). Les threads hôtes gardent d'ailleurs le chemin condvar upstream inchangé.

  2. Panic-hook du preload — un retour vers upstream
  - Upstream : catch (const Shader::Exception&) C++, par thread, aucun état global.
  - L'ancien code ruzu (verrou global + swap du hook) était déjà une adaptation — c'est elle qui sérialisait
  les 9 workers. Mon fix supprime la sérialisation ; le hook filtrant par thread-local est simplement
  l'équivalent Rust du catch per-thread (en Rust, le panic hook est global au process, contrairement aux
  exceptions C++ — d'où le besoin d'un filtre pour ne pas spammer les backtraces des échecs shader attendus).

  3. Box sur descriptor_pool/desc_queue/compute_pass_desc_queue/blit_image — contrainte Rust, sémantique
  upstream inchangée
  - Upstream C++ : les membres sont construits in-place dans RasterizerVulkan, this ne bouge jamais → les
  pointeurs pris à la construction restent valides.
  - Rust : retourner Self déplace la struct ; les NonNull pris avant le move pendaient (c'est le crash de ton
  run : add_buffer incrémentait un curseur fantôme jamais clampé → overflow à 1M après ~80s). Le Box épingle
  l'objet sur le tas — pattern déjà utilisé dans ce même constructeur pour
  scheduler/staging_pool/render_pass_cache.

  4. desc_queue.tick_frame() dans tick_frame — correction de parité pure
  Upstream l'appelle en tête de RasterizerVulkan::TickFrame (vk_rasterizer.cpp:765-766) ; ruzu l'avait oublié
  (le ring de descripteurs ne tournait jamais). Ajouté à l'identique.

  Je te confirme le résultat du run de validation dès qu'il se termine.
## 2026-07-10 - OPEN prerequisite: retain KLightLock waiter thread ownership like upstream

The priority-inheritance port currently stores waiter IDs in `LockWithPriorityInheritanceInfo` and resolves them through `GlobalSchedulerContext` from `KLightLock::unlock_slow_path`. Upstream stores intrusive `KThread*` directly. This creates two Rust-only risks: taking the GSC mutex while the scheduler lock is held, and panicking if a waiter disappears from the GSC table before ownership transfer.

Do not replace the `expect` with a guessed unlock/skip fallback. The faithful correction is structural: make the lock-waiter tree retain stable thread ownership/references, transfer those references from `remove_waiter_by_key`, and remove `find_thread` from the light-lock slow path. Audit cancellation and thread-exit ordering in the same slice.

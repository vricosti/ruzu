# Investigation: Why MK8D Hangs After ~4.5s

## Branch: dev/claude-investigations
## Date: 2026-04-10

## Summary

After all kernel/cpu_manager/jit fixes, MK8D still hangs after ~4.5s with no
visible image and no audio output. Game thread (tid=73) keeps running ARM code
but never makes another SVC after t=4.342461s. Worker threads (tid=84-92) are
spawned, immediately call `WaitProcessWideKeyAtomic` with infinite timeout, and
**never get woken up**.

## Symptoms

- **No frames rendered**: 0 `txn=7` (QueueBuffer) calls in 60s
- **No audio output**: AudioRenderer's `RequestUpdate` is called repeatedly but
  the underlying audio_core renderer never produces output that the game then
  uses to signal "buffer ready"
- **Worker thread starvation**: Worker threads spawned around t=1.78-1.84s never
  make a second SVC after their initial wait

## Thread State at Hang

| Thread | Last activity | Status |
|--------|--------------|--------|
| tid=73 (game main) | 4.342461s — `SendSyncRequest(0x801f3)` | Running ARM code, no SVC |
| tid=83 (worker) | 4.586144s — `WaitProcessWideKeyAtomic` | Blocked on condvar (∞ timeout) |
| tid=82 (worker) | 4.339227s — `WaitProcessWideKeyAtomic` | Blocked on condvar (∞ timeout) |
| tid=86-92 (workers) | ~1.84s — `WaitProcessWideKeyAtomic` | Blocked since 1.84s, **never woken** |
| tid=94, 95, 96, 98, 100, 102 | ~3.3s | Blocked |

## Critical Finding: Worker Condvar Keys Never Signaled

Workers wait on keys in range `0x686a76e4` - `0x686a7f58` (heap-allocated
condvar slots). Game thread tid=73 signals keys in range `0x686ab100`,
`0x686ae100`, `0x686b7100` — completely different offsets within the same
heap region.

In zuyu, the equivalent worker tid=78 waits on `0x552880e8` and gets signaled
by tid=75 (game) on the **same** key 18ms later.

In ruzu, no signal is ever issued on the worker keys `0x686a76e4` etc.

## Comparison: SVC Throughput

| Metric | Zuyu (5s) | Ruzu (60s) |
|--------|-----------|------------|
| Total SVCs | 14,209 | 3,774 |
| Active threads at t=4 | 8 (tid=104 dominant with 3200 SVCs) | 1 (tid=83 with 719 SVCs) |
| MapPhysMemUnsafe (0x5f) | 3339 | 272 |
| SignalProcessWideKey (0x1d/0x1b) | ~5500 | 686 |
| QueueBuffer (game→display) | many | 0 |

## Root Cause Hypothesis

**Hypothesis A: Missing service implementation causes async event chain to break**

Some service call (likely audio renderer or display vsync) returns success
but doesn't trigger the async path that would later signal the worker condvars.
The game's worker threads are spinning waiting for "work to do" notifications
that should come from the service that didn't actually do its work.

Strongest candidate: `audren:u IAudioRenderer::Start` — Start is called once
at 4s but the audio thread that should signal "buffer ready" events isn't
running real audio output.

**Hypothesis B: KConditionVariable signaling is broken in Rust**

The signal goes to a different Process or KConditionVariable instance than
the wait. The signal_to_address path may be looking up the wrong process
or wrong wait list. Could explain why signals on 0x686a... addresses don't
wake waiters on neighboring 0x686a... addresses.

**Hypothesis C: ApplyConditionVariable returns wrong waiter**

In KConditionVariable::SignalImpl, the upstream code finds waiters by
arbitration key. If the Rust implementation uses the wrong key (e.g. mutex
addr vs condvar addr), waiters would never be found.

## ROOT CAUSE FOUND: Worker Thread Core Affinity Bug

The issue is that when the game creates worker threads with multi-core affinity,
**ruzu's scheduler always puts them on core=0**, on the same core as the game
main thread tid=73. This means all workers serialize through core=0.

### Evidence

In zuyu (working), worker threads are distributed across cores:
```
[  0.406835] SCHED core=1 from_tid=10 to_tid=78
[  0.413762] SCHED core=0 from_tid=75 to_tid=79
[  0.437694] SCHED core=1 from_tid=78 to_tid=80
[  0.444036] SCHED core=1 from_tid=10 to_tid=82
[  0.444128] SCHED core=2 from_tid=12 to_tid=83
```

In ruzu (broken), all worker threads queue on core=0:
```
[  1.844090] SCHED core=0 from_tid=73 to_tid=86
[  1.844226] SCHED core=0 from_tid=86 to_tid=87
[  1.844351] SCHED core=0 from_tid=87 to_tid=88
[  1.844475] SCHED core=0 from_tid=88 to_tid=89
[  1.844604] SCHED core=0 from_tid=89 to_tid=90
[  1.844728] SCHED core=0 from_tid=90 to_tid=91
[  1.844855] SCHED core=0 from_tid=91 to_tid=92
```

### Why This Causes the Hang

1. Game creates 7 worker threads in rapid succession
2. Each worker calls `WaitProcessWideKeyAtomic(infinite_timeout)` immediately
3. Game expects to signal them later as work becomes available
4. But because they're all on core=0, they only run when game thread yields
5. By the time they call wait, the game has already moved past the signal point
6. Workers wait forever on stale condvars

In zuyu, workers run on cores 1-3 IMMEDIATELY in parallel with the game on core 0.
The wait → signal → wake sequence happens within milliseconds.

## REAL ROOT CAUSE: Spinning host service threads (now partially fixed)

After deeper investigation with `ps -eLo pid,lwp,%cpu,comm`, the actual
problem was discovered: **HLE host service threads were spinning at 100%
CPU each**, consuming all host CPU and starving the actual emulation cores.

### Evidence (before fix)

```
Total process CPU: 864%
am:EventObserve  100%   (was busy-spinning)
HLE:Loader        97%
HLE:ldn           97%
HLE:audio         97%
HLE:nvservices    96%
HLE:FS            95%
HLE:vi            94%
HLE:bsdsocket     94%
HLE:jit           94%
CPUCore_0          4%   <-- the actual game emulation
```

8 HLE service threads × 95% CPU = 760% CPU wasted on spin loops.
Meanwhile CPUCore_0 (the JIT thread that actually runs the game)
got only 4% CPU due to host scheduler contention.

### The bugs

1. **`ServerManager::wait_and_process_impl`** at line 386 was calling
   `std::thread::yield_now()` when no events were ready. This is
   *not* a sleep — it returns immediately on most platforms, creating
   a tight 100% CPU spin loop. Fixed by using `wakeup_event.wait_timeout`
   with proper signal clearing.

2. **`EventObserver::wait_signaled`** at line 178 was calling
   `std::thread::sleep(Duration::from_micros(100))` between holder
   polls = 10,000 iterations/sec on a single thread. Fixed by using
   `wakeup_event.wait_timeout` with proper signal clearing.

3. **AM `set_window_system` was hijacking the EventObserver thread**:
   the previous fix had moved the `set_window_system` blocking wait
   onto the EventObserver thread via an `on_thread_start` callback.
   When `set_window_system → process.run` made the game's main thread
   runnable, fiber scheduling caused the EventObserver OS thread to
   end up running the game's JIT (fibers run on whatever OS thread
   calls the scheduler). Fixed by spawning a dedicated `am:SetWindowSystem`
   OS thread for that wait.

### Result of fixes

- Total process CPU: **864% → 123%** (7× reduction)
- HLE service threads now correctly idle at 0% CPU
- Game progresses further (last SVC moved from t=4.5s to t=6.2s)

### Remaining problem

`am:EventObserver` thread still spikes to 99.9% CPU **but it's no longer
spinning in event_observer.rs code**. Diagnostic logging confirmed
`wait_signaled` is only called ~3 times in 5 seconds, blocking properly
on the wakeup_event condvar.

The CPU is being consumed by **fiber-scheduled JIT execution running on
the EventObserver OS thread**. This is a deeper architectural issue:
ruzu's fibers run on whatever OS thread happens to call into the
scheduler, not on dedicated `CPUCore_X` threads. When the EventObserver
thread (or any AM service thread) calls into kernel code, it can end up
running a guest thread fiber, which makes that OS thread the de facto
JIT execution thread.

This explains why CPUCore_0 only shows 19% CPU — most of the JIT work
runs on the EventObserver thread instead. The thread name is misleading
because pthread names don't change with fiber context.

### Next investigation

The fiber/scheduler architecture needs review. Fibers should ideally
only run on dedicated CPUCore_X threads, not on service threads. This
likely requires:
- A real cross-thread scheduler that wakes up CPUCore_X threads when a
  KThread becomes runnable
- Service threads should NEVER call into the scheduler in a way that
  causes them to switch to a guest fiber
- Or alternatively, host service threads should never block on guest
  kernel APIs that require fiber switching

## VERIFIED ROOT CAUSE: JIT Throughput

After 5+ minute runs with thread_probe and PC sampling:
- **0 new SVCs** from tid=73 after t=3.3s in a 5-minute run
- **227 thread interrupts** in 30s for tid=73 (preemption working at ~7.5/sec)
- **134 distinct PCs visited** by tid=73 during 30s of probing
- **No deadlock**: thread is making forward progress through different
  functions, just not making any syscalls
- Hot PCs are in the dynamic linker / init code range (0x015D-0x0202)
- The game is in a CPU-bound init phase that takes >>5 minutes on this
  ruzu JIT speed

### Why JIT is slow

Likely candidates:
1. NEON instruction translation overhead
2. Many small JIT blocks instead of larger optimized chunks
3. Memory access checks per instruction
4. Lack of upstream optimizations (block linking, fastmem patches)

### Validation

This is consistent with the memory note "MK8D boots and enters main loop,
bottleneck is JIT NEON performance".

## REVISED ROOT CAUSE: NOT a deadlock — slow JIT execution

After deeper investigation with thread_probe and PC_probe, the actual root
cause is NOT a deadlock or missing service. Findings:

### What is actually happening at the "hang"

1. **Game thread tid=73 is alive and making progress** through the JIT
2. PC sample distribution shows it executes through many different functions
3. Most-sampled PC: `0x02003E58` (41 hits out of ~340 probes)
4. The instruction at 0x02003E58 is `BHI 0x02003E2C` — a backwards branch
   in a memory-init helper function
5. The LR varies across many call sites (0x42598EA8, 0x43A4C2C0, 0x53CF9250,
   0x5D859FF8, 0x60059FF8, 0x6224FC30, 0x646B629C, 0x66CE95E0, 0x68A70D84,
   0x76470950, etc.)
6. This means the game is calling the same memory-init helper from MANY
   different places (likely asset/world initialization)

### The hang is JIT throughput

After ~4.5s, the game transitions from SVC-heavy init (servicing requests)
to CPU-heavy init (asset/data structure setup). During this CPU-heavy phase:
- Few SVCs are issued (SVC count drops to ~0)
- The game thread runs ARM code at JIT-limited speed
- ruzu's JIT is significantly slower than upstream's, so this phase takes
  forever
- The game appears "hung" but is actually doing legitimate work

### Why no rendering / no audio

The game hasn't yet reached the point where it submits its first frame
because it's still in initialization. With faster JIT throughput, the game
would eventually progress to rendering — it just needs much more wall-clock
time than zuyu.

### Worker thread observations remain valid but not the root cause

The worker thread tid=86-92 condvar wait pattern is real, but not blocking
forward progress. tid=73 keeps running on core 0 doing CPU work. The workers
are just sitting idle waiting for work that hasn't been queued yet.

## Detailed Code Path Analysis

The thread creation/scheduling code paths in ruzu vs zuyu are functionally
equivalent for the **first** wave of worker threads (tid=77-84). Both create
the thread with `affinity=0b0001`, then SetThreadCoreMask updates the affinity,
and the thread starts on the right core.

### The actual divergence: second wave of worker threads

The game creates a SECOND wave of worker threads at ~1.82s in ruzu (tid=85-92,
seven threads in rapid succession). For these:
- **No SetThreadCoreMask is called** before they become RUNNABLE
- They all default to `core_id=0, affinity=0b0001`
- All seven queue up on core 0 sequentially behind tid=73

In zuyu, the same pattern of CreateThreads happens, but the threads end up
distributed across cores. The reason must be one of:
1. zuyu calls SetThreadCoreMask later after StartThread (ruzu doesn't capture
   this path because it uses the default mask)
2. zuyu's scheduler does **migration** of newly-created threads via the
   suggested_queue → scheduled_queue path (we have this code but it requires
   the thread to be in the suggested_queue, which only happens if affinity
   has multiple bits set)
3. The game's NPDM has `process.core_mask = 0b1111` and zuyu's CreateThread
   actually sets `affinity = process.core_mask` at creation, not just the
   single ideal core bit

### Hypothesis to verify

In upstream `svc::CreateThread`, after resolving `core_id` from
`USE_PROCESS_DEFAULT`, what affinity does the new thread end up with?

Need to trace upstream's `KThread::InitializeUserThread` to confirm whether
it really sets `m_physical_affinity_mask = 1ULL << virt_core` (single bit)
or if it sets it to the process's core mask (multi-bit).

If upstream uses single-bit, then the migration code MUST be the difference.
But ruzu's migration code only triggers when there's a thread in the
suggested_queue for an idle core, which requires multi-bit affinity.

### Most likely actual fix

The game expects that worker threads created with `core_id=USE_PROCESS_DEFAULT`
get the **process's full core mask** as their affinity, not just the single
ideal core bit. Both upstream and ruzu currently set single-bit, so this
isn't a "ruzu-only" bug — but somehow zuyu still distributes the work.

The remaining mystery: how does zuyu distribute identical-affinity
single-core-bit worker threads across cores? Either:
- zuyu calls SetCoreMask on these threads via a code path we haven't traced
- zuyu's scheduler has additional load-balancing logic we haven't found yet

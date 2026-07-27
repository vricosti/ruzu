# Scheduler preemption parity state

Interrupted slice: port `KernelCore::InitializePreemption` from
`zuyu/src/core/hle/kernel/kernel.cpp`.

Missing prerequisite found: `KScheduler::rotate_scheduled_queue` lacked both
upstream suggested-thread migration loops, and
`GlobalSchedulerContext::preempt_threads` bypassed it.

Attempted prerequisite: both migration loops were ported and exercised by
`GlobalSchedulerContext::preempt_threads`.

Runtime result: rejected. SuperTuxKart stalled after three icons with a thread
waiting on a userspace mutex that named itself as owner. An A/B run using the
previous rotation-only behavior displayed fifteen icons and retained distinct
thread contexts. The upstream migration loops assume that a running thread is
always the scheduled front for its core. The Rust fiber scheduler does not yet
maintain that invariant at every migration point, so the loops can migrate a
thread whose context is still active.

Current state: the migration loops and their focused test were removed again.
The existing rotation-only implementation remains active. The single
CoreTiming preemption callback and scheduler-lock ordering stay ported.

Missing prerequisite: make `KScheduler` maintain and test the upstream
invariant that each core's running non-idle thread is the scheduled front
before enabling `RotateScheduledQueue`'s suggested-thread migrations.

Resume point: port/verify that scheduler invariant, then restore the two
migration loops and repeat the same A/B runtime check.

## x64 FP-to-fixed prerequisite

Interrupted slice: identify why SuperTuxKart intermittently stops progressing
while loading its splash textures.

Runtime evidence: x64 block compilation at guest PC `0x801A072C` panics in
`RegAlloc::add_arg_reference` while emitting `FPSingleToFixedS32`. The emitter
calls `get_argument_info` once, then calls `emit_host_call_3`, which calls it a
second time for the same IR instruction.

Missing prerequisite: the fallback helper used by that path is also not an
upstream-equivalent `FPToFixed`; it omits FPCR input and FPSR exception
updates. Port `mantissa_util` and `FPToFixed` into their upstream-owned common
FP modules before resuming the runtime investigation.

Resume point: wire the x64 fallback through the ported operation with one
argument-registration pass, add a regression test for
`ToNearestTieAwayFromZero`, then rerun SuperTuxKart.

# Architecture — Rust vs C++ Differences

This document describes the current architecture of the thread/process lifecycle and CPU dispatch in `ruzu`, particularly the recent logic around cooperative termination.

It does not replace the upstream C++ as the source of truth. It describes:

- what upstream does conceptually
- how the Rust port approximates it today
- where ownership boundaries live in the Rust code
- which differences remain assumed or still temporary

## Scope

The main files involved are:

- [`core/src/hle/kernel/k_thread.rs`](core/src/hle/kernel/k_thread.rs)
- [`core/src/hle/kernel/k_process.rs`](core/src/hle/kernel/k_process.rs)
- [`core/src/hle/kernel/k_scheduler.rs`](core/src/hle/kernel/k_scheduler.rs)
- [`core/src/hle/kernel/k_worker_task_manager.rs`](core/src/hle/kernel/k_worker_task_manager.rs)
- [`core/src/hle/kernel/svc_dispatch.rs`](core/src/hle/kernel/svc_dispatch.rs)
- [`core/src/hle/kernel/svc/svc_process.rs`](core/src/hle/kernel/svc/svc_process.rs)
- [`core/src/hle/kernel/svc/svc_thread.rs`](core/src/hle/kernel/svc/svc_thread.rs)
- [`core/src/hle/kernel/physical_core.rs`](core/src/hle/kernel/physical_core.rs)

## Overview

The upstream model is not a Rust cooperative runtime. Thread or process termination is observed naturally through:

- SVC returns
- interrupt or timer tick returns
- scheduler lock release
- kernel wait/signal paths

In `ruzu`, the same semantics do not exist everywhere yet. The port therefore relies on three explicit boundaries where a requested termination can be drained centrally:

1. `Yield` return / scheduler selection
2. each SVC return in `svc_dispatch`
3. CPU quantum return in `physical_core`

The goal is to cover the three major families of kernel reentry:

- voluntary reentry via SVC
- cooperative reentry via scheduler
- implicit reentry via end of CPU quantum

## Upstream Model

### Thread

Upstream separates several stages:

- `RequestTerminate()`: marks the intent
- `Exit()`: initiates the exit path for the current thread
- `Terminate()`: forces or waits for another thread's termination
- `FinishTermination()`: finalizes state, signals the synchronization object, cleans up resources

The important point is the ordering:

- the termination request does not immediately destroy the thread
- the thread is observed at a scheduling or kernel return boundary
- complete termination is only visible after `FinishTermination()`

### Process

Upstream also follows several phases:

- `Exit()` or `Terminate()`
- `StartTermination()`
- `TerminateChildren(...)`
- `DoWorkerTaskImpl()`
- `FinishTermination()`

`TerminateChildren(...)` does two passes:

1. request termination of children
2. then terminate remaining children

`DoWorkerTaskImpl()` exists to leave the immediate call path and complete cleanup in the right context.

## Current Rust Adaptation

### Rust Ownership

The port uses:

- `Arc<Mutex<KThread>>`
- `Arc<Mutex<KProcess>>`
- `Arc<Mutex<KScheduler>>`

This form imposes a constraint absent from upstream C++: many operations must avoid reentrance while a `MutexGuard` is still active.

This is the main reason for several Rust-side helpers:

- `KProcess::exit_with_current_thread(...)`
- `KThread::terminate_thread(&Arc<Mutex<KThread>>)`
- the global `KWorkerTaskManager` queue

These helpers are not an attempt to redesign the architecture. They exist to preserve upstream ownership as much as possible despite Rust's locking constraints.

### Self References

`KThread` and `KProcess` now have a weak/upgradeable self-reference on the Rust side. This allows:

- `KThread::exit()` to enqueue itself as a worker task
- `KProcess::exit()` or `terminate()` to reschedule `do_worker_task_impl()`

Without this, the lifecycle remained too inline and diverged from the upstream model.

### KWorkerTaskManager

`KWorkerTaskManager` is now a real global asynchronous queue.

Its current role:

- execute termination completions outside the immediate call path
- bring `KThread::exit()` and `KProcess::exit()` closer to the upstream model
- provide `wait_for_idle()` and `wait_for_global_idle()` for tests

What this changes:

- end-of-life is no longer forced inline everywhere
- `do_worker_task_impl()` becomes a useful owner of process cleanup again

## Drain Boundaries

The term "drain" here means: observing that the current thread has `termination_requested` and triggering its complete `exit()` at a central kernel/runtime return boundary.

### 1. Scheduler Boundary

Owner file:

- [`core/src/hle/kernel/k_scheduler.rs`](core/src/hle/kernel/k_scheduler.rs)

The local helper is:

- `exit_thread_if_termination_requested(...)`

It is used today in:

- `yield_without_core_migration(...)`
- `wait_for_next_thread(...)`
- `select_next_thread_id(...)` for tests

This point covers cooperative paths where the current thread goes back through the scheduler.

### 2. SVC Boundary

Owner file:

- [`core/src/hle/kernel/svc_dispatch.rs`](core/src/hle/kernel/svc_dispatch.rs)

The local helper is:

- `drain_current_thread_termination(ctx)`

It is called once after `call32(...)` or `call64(...)`, in `call(...)`.

This is intentionally centralized:

- no duplication in each SVC handler
- ownership preserved in the true owner of the SVC return
- better conceptual parity with the point where upstream releases the scheduler lock after the handler

### 3. CPU Quantum Boundary

Owner file:

- [`core/src/hle/kernel/physical_core.rs`](core/src/hle/kernel/physical_core.rs)

The local helper is:

- `drain_current_thread_termination(...)`

It is called in the `PhysicalCoreExecutionEvent::Halted(...)` path, i.e., on return from CPU dispatch after a guest execution quantum without SVC.

In `ruzu`'s cooperative model, this is the closest equivalent to the upstream IRQ/timer tick return.

After the drain:

- `exit()` is triggered
- the scheduler receives `request_schedule()`
- `handoff_after_svc(...)` is reused as the switchover boundary to the next runnable

The handoff helper naming is not yet ideal, but the ownership is correct: the next CPU switchover remains in `physical_core`.

## Current Lifecycle

### KThread

Simplified path today:

1. `request_terminate()` sets the intent
2. a drain boundary observes this intent
3. `exit()` enqueues the worker task thread
4. the worker runs `finish_termination()`
5. the thread becomes `TERMINATED` and `signaled`

Important difference from upstream:

- not all paths go through a real upstream-compatible `KSynchronizationObject::Wait()` yet
- some cases remain guarded to avoid deadlocks under `MutexGuard`

### KProcess

Simplified path today:

1. `exit()` or `terminate()`
2. `start_termination(...)`
3. `terminate_children(...)`
4. `KWorkerTaskManager` runs `do_worker_task_impl()`
5. `finish_termination()`

`terminate_children(...)` now respects upstream ownership:

- first pass: `request_terminate()`
- second pass: effective termination of children when safe in the cooperative runtime

The second pass remains more conservative than upstream, because a Rust-side thread may still depend on a future dispatch boundary to self-drain.

## Why Rust Forces These Differences

The main divergences do not come from a redesign choice but from mechanical constraints.

### 1. MutexGuard and Reentrance

In C++, many transitions happen with more freedom over the actual call ordering.

In Rust, with `Arc<Mutex<_>>`, one must avoid:

- calling back a method that re-acquires the same mutex indirectly
- blocking in `wait()` while a critical guard is still alive
- doing deep cleanup from a context that already owns the parent object

Consequence:

- some upstream calls must become orchestration helpers
- some steps must be deferred to the worker task manager

### 2. Cooperative Runtime

The current runtime does not naturally interrupt a guest thread anywhere like upstream does via its scheduling and interrupt boundaries.

Consequence:

- explicit drain points must be introduced
- a thread in "pure CPU" mode can only be observed at the end of a quantum

### 3. Incomplete Synchronization Waits

Until `KSynchronizationObject::Wait()` and some scheduler/signal pieces are fully at upstream level, some operations that would block in C++ must remain cautious in Rust.

Consequence:

- `terminate_thread(...)` exists in addition to `terminate(&mut self)`
- `terminate_children(...)` does not yet aggressively block on all states

## What Is Faithful

The following points are now close to upstream intent:

- process cleanup ownership in `k_process.rs`
- SVC return ownership in `svc_dispatch.rs`
- CPU quantum return ownership in `physical_core.rs`
- separation between termination request and effective termination
- use of a worker task manager instead of fully inline cleanup

## What Remains Different

Known important differences still include:

- drain remains explicit and cooperative, not implicit everywhere like upstream
- `handoff_after_svc(...)` is also reused after the CPU boundary, which is functional but not yet perfectly named
- some waits/terminations are still more conservative than upstream to avoid Rust-side deadlocks
- `KThread::Terminate()` and some synchronization waits are not yet fully at C++ semantic level
- not all possible kernel return boundaries are wired yet

## Invariants For Future Changes

If you modify this area, maintain these invariants:

- drain must remain centralized per major boundary, not duplicated in each handler
- the owner file must remain the true upstream conceptual owner of the return point
- `request_terminate()` must not become an immediate `exit()`
- `finish_termination()` must remain the transition that makes termination visible
- the worker task manager must remain the deferral point for asynchronous cleanup
- do not reintroduce inline lifecycle that breaks `k_process.rs` or `k_thread.rs` ownership

## Remaining Target Boundaries

If further gaps need to be closed later, new hooks should only be sought at common reentry boundaries:

- exception return if distinct from the `Halted(...)` path
- other common CPU dispatch exits
- future wait/signal paths when `KSynchronizationObject::Wait()` is more complete

Do not:

- instrument each SVC individually
- scatter drain logic into generic helpers without a clear owner
- make memory or IPC subsystems carry behavior they do not own in dispatch

## Reference Tests

The tests that document the current behavior are:

- `call_drains_current_thread_termination_on_svc_return`
- `yield_exits_current_thread_when_termination_was_requested`
- `run_loop_drains_current_thread_termination_on_halt_boundary`

They cover respectively:

- the SVC boundary
- the cooperative scheduler boundary
- the CPU quantum boundary

## Summary

The Rust port does not yet reproduce all the implicit mechanics of the upstream kernel, but it now has three central drain points that cover the major returns:

- scheduler
- SVC
- end of CPU quantum

The essential difference with Rust is that these boundaries must be made explicit to remain safe under `Arc<Mutex<_>>` and a cooperative runtime, while keeping the ownership of behaviors in the same conceptual files as upstream.

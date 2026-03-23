 The bug is in the application-thread scheduling handoff, not in the A32 JIT.

  What happens:

  - Core 0 starts, calls GuestActivate(), finds no user thread yet, and drops into idle.
  - Later, KProcess::run() creates the application main thread and sets scheduler.state.highest_priority_thread_id =
    Some(thread_id) in core/src/hle/kernel/k_process.rs:2017.
  - But at that point the thread is not yet in GlobalSchedulerContext’s thread list. That happens later in core/src/core.rs:1091
    via register_application_thread().

  Why this is bad:

  - KScheduler::schedule_impl_fiber() resolves highest_priority_thread_id back to an Arc<KThread> through
    gsc.get_thread_by_thread_id(id) in core/src/hle/kernel/k_scheduler.rs:1099 and will get None if the thread has not been added
    to the GSC list yet.
  - So the scheduler can be told “thread X is highest priority” before it can actually look thread X up.
  - That means the reschedule can fail and the core stays idle, which matches the symptom: no [A32JIT] lookup_block logs at all.

  The concrete race:

  1. KProcess::run() sets highest_priority_thread_id and needs_scheduling.
  2. The thread is still absent from GlobalSchedulerContext::m_thread_list.
  3. Scheduler tries to pick the target thread by ID and gets None.
  4. Core 0 remains idle unless another wakeup happens after registration.

  Why I’m confident:

  - Your log reaches System: running but never shows any A32 JIT lookup/compile logs.
  - That means guest dispatch never actually reached the A32 dispatcher.
  - The scheduler/GSC mismatch above is enough to cause exactly that.

  Fix:

  - Add the new thread to GlobalSchedulerContext before publishing it as schedulable.
  - In practice, do the GSC registration inside KProcess::run() before:
      - push_back_to_priority_queue(thread_id)
      - highest_priority_thread_id = Some(thread_id)
      - any interrupt/reschedule request

  So the bug is: the thread becomes “selected” before it becomes “resolvable.”
# TODO

Some maxwell_3d tests hang (test_call_method_cb_bind, test_call_method_cb_data) — needs investigation. The test_call_method_sync_point and other simpler tests pass. The hang is not a stack overflow — it occurs even with a single test and large stack. Likely related to the EngineInterface trait dispatch or a subtle deadlock in the test setup.

------
 // Upstream emits save_context and load_context helper functions using oaknut
        // (ARM64 code generator):
        //   - WriteSaveContext(): saves all guest GPRs/vectors to GuestContext struct
        //   - WriteLoadContext(): restores all guest GPRs/vectors from GuestContext struct
        // These are called by the SVC trampoline to save/restore guest state when
        // transitioning to/from host code.
        // Requires: oaknut-equivalent ARM64 code generation library for Rust.
        // NCE is AArch64-host-only; on x86_64 hosts this entire module is unused.
------

 The AudioCore takes Arc<Values>. This is a case where we should change the API to not require an Arc<Values> since there's   
  now a global. But that's a bigger refactor. For now, let me update the call sites to use values() and clone where needed.    
                                                                                                                               
  Let me now do all the edits. For the AudioCore case in main.rs, since it requires Arc<Values>, I'll create an Arc from a     
  clone of the global values. 
  ----

  KScopedSchedulerLockAndSleep   
    /// Upstream wraps in KScopedSchedulerLockAndSleep for atomic lock + sleep.
    /// In our single-core cooperative model, the process lock provides
    /// equivalent protection. Wire KScopedSchedulerLockAndSleep when
    /// KConditionVariable gains a KernelCore reference.
  ----

  Requires rdynarmic A64 decoder integration                                                                                           
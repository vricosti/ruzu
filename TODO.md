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

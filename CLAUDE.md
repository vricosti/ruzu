# CLAUDE.md — Porting Philosophy And Execution Contract

STRICT MODE ON

## Purpose

This document defines how a ChatGPT/Codex-style agent must port C++ code from `/home/vricosti/shared/zuyu/src/` into `/home/vricosti/shared/ruzu/`.

It is written as a handoff contract for another instance.

The goal is not merely:

- "make it compile"
- "make tests pass"
- "approximate the behavior"

The goal is:

- strict file-structure parity where technically possible
- strict method ownership parity where technically possible
- strict constant placement parity where technically possible
- strict behavioral parity with the C++ source
- explicit, reviewable traceability from each Rust file/method/constant back to the upstream C++ file/method/constant

This document exists because earlier progress proved that a crate can be "substantial" while still drifting structurally from upstream in ways that make review and future porting harder.

## Source Of Truth

### Upstream reference

The C++ source of truth is:

- `/home/vricosti/shared/zuyu/src/`

That tree is read-only.

Never edit it.

Always treat it as authoritative for:

- file layout
- class/module boundaries
- constants
- method names and responsibilities
- state ownership
- control flow
- validation behavior
- data structure invariants

### Rust target

The Rust destination is:

- `/home/vricosti/shared/ruzu/`

The Rust port should be judged against the C++ tree, not against whether the Rust design looks idiomatic in isolation.

## Core Philosophy

### 1. Fidelity beats elegance

If there is tension between:

- a cleaner Rust architecture
- a more faithful C++ port

pick the faithful port.

Rust idioms are allowed only when they preserve the same ownership boundaries and behavior.

Examples of acceptable Rust adaptations:

- `Result` instead of exceptions
- `Arc<Mutex<T>>` instead of `shared_ptr` + mutex
- `enum` instead of tagged unions / `std::variant`
- `Drop` instead of destructors

Examples of unacceptable adaptations:

- merging several upstream files into one large Rust file "for convenience"
- moving constants away from their upstream module
- centralizing behavior in a generic dispatcher when upstream behavior belongs to per-file modules
- inventing a new architecture because the upstream one looks awkward in Rust

### 2. Auditability is a feature

A good port is easy to compare line-by-line against upstream.

When reviewing a Rust file, a maintainer should be able to answer:

- which C++ file does this map to?
- which upstream constants live here?
- which upstream methods live here?
- what is still missing?

If the answer is unclear, the port is not structured well enough.

### 3. Intermediate progress is allowed, but temporary shortcuts must be unwound

During the port, temporary shortcuts may be used to unblock progress.

Examples:

- stubbing out a complex platform-specific function
- using a simplified implementation while other crates depend on the interface
- keeping a placeholder module with `todo!()` bodies

But these shortcuts must be treated as debt, not design.

Before calling a subsystem "ported", unwind the shortcut and restore parity.

### 4. Tests are necessary but not sufficient

Passing tests only proves:

- current exercised behavior works

It does not prove:

- structural parity
- ownership parity
- exact data-structure layout parity
- exact lifecycle parity

Never conclude "finished" purely from green tests.

### 5. The correct default is suspicion

When something is "close enough", assume it is still wrong until verified against upstream.

Examples of common false confidence:

- same output in one test
- same field names but different file ownership
- logically equivalent control flow but different edge-case handling

For this port, "probably equivalent" is not good enough.

## Non-Negotiable Rules

### Rule A: Preserve file structure whenever technically possible

If upstream has a `.cpp` and `.h` pair, Rust should have a corresponding `.rs` file at the same relative path within its crate.

Do not hide several upstream files behind one Rust file unless there is a strong technical reason.

If you must diverge, document the reason in code comments and in the final summary.

Exceptions are listed per-folder at the end of this document. These omissions are intentional and should not be flagged as missing files during parity audits.

### Rule B: Preserve method ownership whenever technically possible

If an upstream method belongs to a specific `.cpp` file, then its Rust equivalent should live in the corresponding `.rs` file with Rust naming conventions (snake_case).

Do not leave such logic in:

- `lib.rs` or `mod.rs`
- some unrelated shared helper
- a "utils" catch-all

unless upstream itself centralizes it.

### Rule C: Preserve constant placement

If constants live in a specific upstream file, they belong in the corresponding Rust file.

Do not centralize them into unrelated utility modules unless the upstream does the same.

### Rule D: Preserve behavior before abstraction

Never extract a helper unless:

- the upstream has the same helper concept
- or the extraction is purely mechanical and does not change ownership semantics

Avoid "helpful" generic abstractions that make the port less traceable.

### Rule E: Preserve binary layout exactly where raw serialization exists

Anywhere structs are copied as raw bytes, all of these matter:

- field order
- alignment
- explicit padding
- implicit padding
- zero-initialization of reserved bytes

`repr(C)` is necessary but not sufficient.

If a payload is serialized by raw memory copy, initialize the entire payload deterministically before writing fields.

### Rule F: Preserve signed/unsigned bit patterns

Upstream often forwards raw values through integer casts.

Do not replace that with "friendlier" Rust logic like:

- clamping negatives to zero
- turning invalid values into `None`

unless upstream truly does that.

### Rule G: Preserve update and lifecycle ordering

Many bugs come from doing the right work in the wrong order.

When in doubt, port the ordering literally.

## What "Finished" Means

Do not call a folder finished until all of the following are true.

### Structural completion

- The Rust module/file tree mirrors the upstream C++ folder as closely as reasonably possible.
- Major missing files are identified and either ported or documented as exceptions.
- No upstream files are silently merged into unrelated Rust files.

### Ownership completion

- Methods and constants live in the same conceptual modules as upstream.
- No "catch-all" utility files that absorb logic from multiple upstream sources.

### Behavioral completion

- Critical subsystems follow upstream behavior closely enough that remaining differences are minor and identified.

### Binary completion

- Data structure layouts match upstream where shared across components.
- Serialized payloads are bit-accurate.

### Validation completion

- Tests exist for nontrivial parity-sensitive behavior.
- Full `cargo test -p <crate>` passes.
- No known major "still structurally wrong" caveat remains.

If major caveats remain, it is not finished.

## Practical Workflow For Each File

For every upstream file, use this process.

### Step 1: Identify the exact Rust counterpart

Before writing code, answer:

- what is the upstream file path?
- where is the Rust counterpart supposed to live?
- does that file already exist?
- if it exists, is it the right owner for this logic?

If the logic currently lives elsewhere, fix ownership first or during the change.

### Step 2: Read the upstream header and implementation

For each C++ file:

- inspect its header for interface and state
- inspect its `.cpp` for behavior

Capture:

- constants
- structs/classes
- public methods
- private helpers
- static helpers
- validation rules

### Step 3: Port names with Rust conventions, not new semantics

Do not rename at the semantic level. Convert to snake_case and Rust naming conventions only.

### Step 4: Keep upstream helper boundaries visible

If upstream uses several helpers inside a file, keep corresponding Rust helpers in the same file rather than flattening them into one large function.

### Step 5: Add focused regression tests

After each meaningful parity fix, add targeted tests for:

- the bug being fixed
- the upstream-specific edge case
- any binary-layout or ordering contract involved

### Step 6: Run the smallest useful test set first, then full crate tests

Use focused module tests while iterating, then `cargo test -p <crate>` before concluding the pass.

## Anti-Patterns To Avoid

These are all mistakes that already occurred or are easy to repeat.

### Anti-pattern 1: "Flatten now, split later" without actually splitting later

This leads to giant central files, hidden ownership, and difficult review. Only flatten temporarily if you are committed to unwinding it.

### Anti-pattern 2: Treating dispatcher files as owners

Central dispatch files (`mod.rs`, `lib.rs`, processor files) should coordinate, not own every module's real logic.

### Anti-pattern 3: Central constant dumps

If constants belong to a specific upstream file, keep them in the corresponding Rust file. Do not make one convenience constants file unless upstream does.

### Anti-pattern 4: Replacing upstream lifecycle with a Rust convenience lifecycle

Examples: eager initialization where upstream initializes lazily, preemptive cleanup that upstream performs later. If you diverge for temporary practicality, mark it and come back.

### Anti-pattern 5: Using "tests pass" to justify structural divergence

A structurally wrong port can still pass current tests.

### Anti-pattern 6: Replacing platform-specific code with pure-Rust alternatives without documenting it

If upstream uses platform-specific APIs and Rust uses a different approach, document the divergence.

## How To Treat Rust Idioms

Rust idioms are tools, not goals.

Use them when they preserve parity.

### Preferred Rust adaptations

- `enum` for tagged unions and mode flags
- `Drop` for cleanup matching destructor behavior
- `Option` for optional ownership
- `Arc`, `Weak`, `Mutex` for shared lifecycle
- module-level functions replacing C++ `static` helpers
- `bitflags` crate for flag enums that use `DECLARE_ENUM_FLAG_OPERATORS` in C++

### Rust adaptations to use carefully

- traits replacing inheritance
- generic helpers replacing duplicated code
- borrow-driven refactors that alter access patterns

### Rust adaptations to avoid in this port

- architecture redesigns
- replacing C++ data structures with different Rust ones that have different performance characteristics
- helper layers that erase file ownership

## Testing Philosophy

### Required testing style

Add focused tests for:

- parity-sensitive edge cases
- previous bugs
- binary-layout expectations
- hash function output matching
- data structure invariants

### Preferred test granularity

- unit tests near the module being ported
- regression tests for specific upstream contracts

### What to avoid

- only broad integration tests
- only "does not crash" tests
- tests that assert Rust-specific behavior not derived from upstream

## How To Decide What To Work On Next

When choosing the next task, prefer this order:

1. structural mismatches that make review harder
2. ownership mismatches that keep logic in the wrong file
3. missing files that other crates depend on
4. behavioral mismatches in critical infrastructure
5. test coverage for newly fixed parity-sensitive behavior

Do not spend time polishing low-value style details while major ownership or behavior mismatches remain.

## Completion Checklist For A Subtree

Before calling any subtree "ported", check all of these.

### File structure

- Do corresponding Rust files exist?
- Are there any remaining flattened upstream files?

### Ownership

- Are constants in the right file?
- Are methods/helpers in the right file?

### Behavior

- Does sequencing match upstream?
- Are sentinel values preserved?
- Are validation and failure paths aligned?

### Serialization

- Is binary layout correct where shared across components?

### Tests

- Is there a focused regression test for the nontrivial behavior?

## How To Communicate Progress

When reporting progress:

- state exactly what ownership/parity slice was improved
- name the affected files
- say what is still missing
- do not over-claim completion

Good progress statement:

- "Ported `fs/path_util.rs` from `fs/path_util.cpp`; all path resolution methods match upstream. `cargo test -p common` passes."

Bad progress statement:

- "Improved the crate significantly."

## Final Standard

If another instance uses this file correctly, it should keep asking:

- "Where does this logic belong upstream?"
- "Why is this method not in the matching file?"
- "Why is this constant not next to its upstream equivalent?"
- "Is this behavior literally the same, or merely plausible?"

That is the right mindset for finishing this port.

---

## Per-Folder Exceptions

Each section below lists C++ files that have no meaningful Rust counterpart for a specific upstream folder. These omissions are intentional and should not be flagged as missing files during parity audits.

### `audio_core` — `/home/vricosti/shared/zuyu/src/audio_core`

**Rust crate:** `audio_core` at `/home/vricosti/shared/ruzu/audio_core`

**Status:** Ported.

**Exceptions:**

- `precompiled_headers.h` — precompiled headers are a C++ build optimization; Rust has no equivalent concept.

### `common` — `/home/vricosti/shared/zuyu/src/common`

**Rust crate:** `common` at `/home/vricosti/shared/ruzu/common`

**Status:** Ported.

**Exceptions:**

- `precompiled_headers.h`, `common_precompiled_headers.h` — C++ build optimization; no Rust equivalent.
- `common_types.h` — defines `u8`, `u16`, `u32`, `u64`, `s8`, `s16`, `s32`, `s64`, `f32`, `f64`; Rust has these as built-in primitive types.
- `common_funcs.h` — C++ utility macros (`DECLARE_ENUM_FLAG_OPERATORS`, `ZUYU_NON_COPYABLE`, etc.); Rust handles these via `bitflags`, `Clone`/`Copy` derives, etc.
- `concepts.h` — C++ concepts; Rust uses trait bounds natively.
- `polyfill_ranges.h`, `polyfill_thread.h` — C++ standard library polyfills; Rust stdlib provides these features natively.
- `expected.h` — C++ `std::expected` polyfill; Rust has `Result` built in.
- `scope_exit.h` — Rust uses `Drop` for deterministic cleanup.
- `bit_cast.h` — Rust uses `bytemuck` or `transmute`.
- `make_unique_for_overwrite.h` — C++ memory allocation detail with no Rust equivalent.
- `parent_of_member.h` — C++ `container_of` macro pattern; Rust does not use this pattern.
- `microprofile.h`, `microprofile.cpp`, `microprofileui.h` — profiling framework integration; a separate concern.
- `stb.h`, `stb.cpp` — stb_image C library bindings; Rust would use an image crate instead.
- `scm_rev.h` — build-time source control revision; handled differently in Rust builds.
- `assert.h` — C++ macro-based assertion system; Rust has built-in `assert!`, `debug_assert!`, `panic!`.
- `atomic_helpers.h` — C++ TSAN annotation macros for atomic fences; Rust `std::sync::atomic` handles this natively.
- `atomic_ops.h` — MSVC/GCC specific compare-and-swap operations; Rust `std::sync::atomic` provides these.
- `demangle.h` — C++ symbol demangling; Rust has different name mangling and its own demangling support.
- `logging/formatter.h` — fmt library formatter specialization for enums; Rust `Display`/`Debug` traits handle this.
- `logging/log.h` — C++ fmt-based logging macros (`LOG_DEBUG`, `LOG_INFO`, etc.); Rust uses the `log` crate macros.
- `unique_function.h` — C++ move-only `std::function`; Rust has `Box<dyn FnOnce()>` and closures natively.
- `literals.h` — C++ user-defined literals (`_KiB`, `_MiB`, etc.); Rust uses `const` values or plain arithmetic.
- `android/android_common.*`, `android/applets/software_keyboard.*`, `android/id_cache.*` — Android JNI integration; platform-specific frontend code with no Rust JNI equivalent in this project.
- `fs/fs_android.*` — Android-specific filesystem via JNI; same as above.
- `linux/gamemode.*` — Linux Feral Interactive gamemode integration; thin platform glue.
- `signal_chain.*` — POSIX signal handler wrapping (Android-specific); platform-specific.
- `nvidia_flags.*` — Environment variable configuration for Nvidia driver quirks; thin platform glue.
- `windows/timer_resolution.*` — Windows-specific timer resolution API; platform-specific.
- `x64/xbyak_abi.h`, `x64/xbyak_util.h` — Xbyak JIT assembly library integration; JIT-specific, handled by `rdynarmic`.
- `reader_writer_queue.h` — third-party lock-free queue (`moodycamel::ReaderWriterQueue`); Rust would use a dedicated crate.
- `algorithm.h` — small generic iterator helpers (`BinaryFind`, `FoldRight`); Rust iterators provide this natively.
- `socket_types.h` — C++ network socket type aliases; Rust `std::net` handles this.

### `core` — `/home/vricosti/shared/zuyu/src/core`

**Rust crate:** `core` at `/home/vricosti/shared/ruzu/core`

**Status:** Ported.

**Exceptions:**

- `precompiled_headers.h` — C++ build optimization; no Rust equivalent.
- `memory.h/cpp` — ported as `guest_memory.rs` (upstream `Memory::Memory` class maps to `GuestMemory`); the top-level `memory.h` is a thin wrapper around page table access that is integrated into `guest_memory.rs`.
- `file_sys/fssystem_bucket_tree_template_impl.h`, `file_sys/fssystem_bucket_tree_utils.h` — C++ template implementation headers; logic folded into `fssystem/bucket_tree.rs`.
- `arm/nce/arm_nce.s` — ARM64 assembly source file; would need a separate `.s` file or inline assembly in Rust.
- `arm/dynarmic/` — JIT backend files are ported as stubs; full implementation depends on `rdynarmic` integration.
- `arm/nce/` — Native code execution backend is ported structurally; full implementation depends on platform-specific signal handling.

### `video_core` — `/home/vricosti/shared/zuyu/src/video_core`

**Rust crate:** `video_core` at `/home/vricosti/shared/ruzu/video_core`

**Status:** In progress.

**Exceptions:** *(to be filled as the port progresses)*

## Directories Excluded From The Port

The following upstream directories are **intentionally not ported** and should never be flagged as missing during parity audits.

### `yuzu` — `/home/vricosti/shared/zuyu/src/yuzu`

**Not ported.** This is the Qt-based GUI frontend application. The ruzu project will implement its own frontend using a different UI library (not Qt). This directory should not be ported.

### `tests` — `/home/vricosti/shared/zuyu/src/tests`

**Not ported.** Upstream C++ test suite. Rust tests are written natively alongside each crate using `#[cfg(test)]` modules and `cargo test`.

---

## Testing yuzu-cmd

### Manual test with Mario Kart 8 Deluxe (AArch32)

```bash
cargo run --bin yuzu-cmd -- -g "/home/vricosti/Games/Emulators/Switch/common/roms/Mario Kart 8 Deluxe [NSP]/Mario Kart 8 Deluxe [0100152000022000][v0].nsp"
```

With debug logging:

```bash
RUST_LOG=info cargo run --bin yuzu-cmd -- -g "/home/vricosti/Games/Emulators/Switch/common/roms/Mario Kart 8 Deluxe [NSP]/Mario Kart 8 Deluxe [0100152000022000][v0].nsp"
```

With isolated data directories (avoids polluting `~/.local/share/ruzu`, `~/.cache/ruzu`, `~/.config/ruzu`):

```bash
env XDG_DATA_HOME=/tmp/ruzu-data \
    XDG_CACHE_HOME=/tmp/ruzu-cache \
    XDG_CONFIG_HOME=/tmp/ruzu-config \
    RUST_LOG=info cargo run --bin yuzu-cmd -- -g "/home/vricosti/Games/Emulators/Switch/common/roms/Mario Kart 8 Deluxe [NSP]/Mario Kart 8 Deluxe [0100152000022000][v0].nsp"
```

**Note:** MK8D is an ARM32 (AArch32) game — title ID `0100152000022000`.

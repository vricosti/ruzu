# CHATGPT.md — Porting Philosophy And Execution Contract For `audio_core`

## Purpose

This document defines how a ChatGPT/Codex-style agent must port `/home/vricosti/shared/zuyu/src/audio_core` into `/home/vricosti/shared/ruzu/audio_core`.

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

- `/home/vricosti/shared/zuyu/src/audio_core`

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
- update sequencing
- command generation
- DSP-side execution semantics

### Rust target

The Rust destination is:

- `/home/vricosti/shared/ruzu/audio_core`

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
- centralizing command behavior in a generic dispatcher when upstream behavior belongs to per-command files
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

- centralizing command serialization in one file
- dispatching through a large `match`
- keeping behavior in `command_list_processor.rs` temporarily

But these shortcuts must be treated as debt, not design.

Before calling a subsystem "ported", unwind the shortcut and restore parity.

### 4. Tests are necessary but not sufficient

Passing tests only proves:

- current exercised behavior works

It does not prove:

- structural parity
- ownership parity
- exact command layout parity
- exact lifecycle parity

Never conclude "finished" purely from green tests.

### 5. The correct default is suspicion

When something is "close enough", assume it is still wrong until verified against upstream.

Examples of common false confidence:

- same output in one test
- same field names but different file ownership
- same command bytes except for padding
- logically equivalent control flow but different update ordering

For this port, "probably equivalent" is not good enough.

## Non-Negotiable Rules

### Rule A: Preserve file structure whenever technically possible

If upstream has:

- `renderer/command/effect/reverb.cpp`
- `renderer/command/effect/reverb.h`

Rust should have a corresponding file:

- `audio_core/src/renderer/command/effect/reverb.rs`

The same applies across the tree:

- `adsp/apps/opus/opus_decode_object.*`
- `renderer/command/data_source/*`
- `renderer/command/mix/*`
- `renderer/command/sink/*`
- `renderer/command/resample/*`
- etc.

Do not hide several upstream files behind one Rust file unless there is a strong technical reason.

If you must diverge, document the reason in code comments and in the final summary.

### Rule B: Preserve method ownership whenever technically possible

If upstream method `UpdateReverbEffectParameter` belongs to `reverb.cpp`, then its Rust equivalent:

- should exist
- should live in `reverb.rs`
- should use Rust naming (`update_reverb_effect_parameter`)
- should do the same job

Do not leave such logic in:

- `command_list_processor.rs`
- `commands.rs`
- some unrelated shared helper

unless upstream itself centralizes it.

### Rule C: Preserve constant placement

If constants live in `reverb.cpp` upstream, they belong in `reverb.rs` in Rust.

Do not centralize them into:

- `commands.rs`
- `common.rs`
- unrelated utility modules

unless the upstream does the same.

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

Upstream often forwards raw ids or sentinel values through integer casts.

Do not replace that with "friendlier" Rust logic like:

- clamping negatives to zero
- turning invalid values into `None`

unless upstream truly does that.

Raw bit-pattern preservation matters for:

- node ids
- session ids
- performance targets
- command payload fields

### Rule G: Preserve update and lifecycle ordering

Many renderer bugs come from doing the right work in the wrong order.

Examples:

- result-state init before/after output translation
- counter increments in `GenerateCommand` vs `SendCommandToDsp`
- pool use-state clearing timing
- `Start` / `Stop` / `Finalize` ordering
- effect and sink cleanup ordering

When in doubt, port the ordering literally.

## What "Finished" Means For `audio_core`

Do not call `audio_core` finished until all of the following are true.

### Structural completion

- The Rust module/file tree mirrors `zuyu/src/audio_core` as closely as reasonably possible.
- Major missing folders are gone.
- Temporary centralization files have been reduced to thin registries or removed.

### Ownership completion

- Methods and constants live in the same conceptual modules as upstream.
- `command_list_processor.rs` is mostly an iterator/dispatcher, not the owner of every command’s logic.
- `commands.rs` is mostly registry/serialization plumbing, not the owner of all runtime behavior.

### Behavioral completion

- `renderer/system`
- `renderer/behavior/info_updater`
- ADSP audio renderer app
- ADSP opus app
- device/session paths

all follow upstream behavior closely enough that remaining differences are minor and identified.

### Binary completion

- Command payload layout matches upstream where serialized
- reserved bytes are deterministic
- command ordering is correct
- address translation behavior is not secretly host-only where upstream expects translated memory

### Validation completion

- Tests exist for nontrivial parity-sensitive behavior
- Full `cargo test -p audio_core` passes
- No known major "still structurally wrong" caveat remains

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
- update order

### Step 3: Port names with Rust conventions, not new semantics

Examples:

- `UpdateReverbEffectParameter` -> `update_reverb_effect_parameter`
- `InitializeReverbEffect` -> `initialize_reverb_effect`
- `ApplyReverbEffectBypass` -> `apply_reverb_effect_bypass`

Do not rename at the semantic level.

### Step 4: Keep upstream helper boundaries visible

If upstream uses several helpers inside a file, keep corresponding Rust helpers in the same file rather than flattening them into one large function.

### Step 5: Add focused regression tests

After each meaningful parity fix, add targeted tests for:

- the bug being fixed
- the upstream-specific edge case
- any binary-layout or ordering contract involved

### Step 6: Run the smallest useful test set first, then full crate tests

Use:

- focused module tests while iterating
- `cargo test -p audio_core` before concluding the pass

## Anti-Patterns To Avoid

These are all mistakes that already occurred or are easy to repeat.

### Anti-pattern 1: "Flatten now, split later" without actually splitting later

This leads to:

- giant central files
- hidden ownership
- difficult review

Only flatten temporarily if you are committed to unwinding it.

### Anti-pattern 2: Treating dispatcher files as owners

Examples:

- `command_list_processor.rs`
- `commands.rs`

These files should coordinate, not own every command family’s real logic.

### Anti-pattern 3: Central constant dumps

If constants belong to reverb, keep them in reverb.

If they belong to delay, keep them in delay.

Do not make one convenience constants file unless upstream does.

### Anti-pattern 4: Replacing upstream lifecycle with a Rust convenience lifecycle

Examples:

- eager session startup where upstream starts lazily
- preemptive cleanup that upstream performs later
- auto-link behavior that changes ownership meaning

If you diverge for temporary practicality, mark it and come back.

### Anti-pattern 5: Using "tests pass" to justify structural divergence

A structurally wrong port can still pass current tests.

### Anti-pattern 6: Hiding protocol bytes behind "safe" abstractions

If the system is building a raw DSP command stream, the raw bytes matter.

Do not abstract away details that change byte layout or update timing.

## How To Treat Rust Idioms

Rust idioms are tools, not goals.

Use them when they preserve parity.

### Preferred Rust adaptations

- `enum` for command ids and modes
- `Drop` for cleanup matching destructor behavior
- `Option` for optional ownership
- `Arc`, `Weak`, `Mutex` for shared lifecycle
- module-level functions replacing C++ `static` helpers

### Rust adaptations to use carefully

- traits replacing inheritance
- generic helpers replacing duplicated code
- borrow-driven refactors that alter update ordering

### Rust adaptations to avoid in this port

- architecture redesigns
- state machines that no longer resemble upstream control flow
- helper layers that erase file ownership

## Command System Philosophy

### Goal

The command system should gradually converge toward:

- per-command-family ownership of payload layout
- per-command-family ownership of dump/verify/process behavior
- thin central registry only where unavoidable

### Desired end state

- `renderer/command/data_source/*` owns data-source behavior
- `renderer/command/effect/*` owns effect behavior
- `renderer/command/mix/*` owns mix behavior
- `renderer/command/resample/*` owns resample behavior
- `renderer/command/sink/*` owns sink behavior
- `renderer/command/performance/*` owns performance behavior

`commands.rs` should be mainly:

- `Command` enum
- serialization routing
- shared type glue only if truly necessary

`command_list_processor.rs` should be mainly:

- iteration over commands
- lifecycle of one processing pass
- dispatch into command-owned behavior

### Verification and dump ownership

If upstream `ICommand` methods conceptually belong to each command type, Rust should reflect that as closely as practical:

- payload methods
- file-local helpers
- family-level dispatch in the family module

not in one central processor file.

## ADSP Philosophy

### ADSP apps are not placeholders

The ADSP layer must not remain a superficial host-side stub if the goal is a finished port.

For:

- `adsp/apps/audio_renderer`
- `adsp/apps/opus`

the Rust code should preserve:

- mailbox protocol shape
- thread lifecycle
- decode/render object ownership
- per-buffer/per-session state
- shutdown semantics

### File parity matters here too

If upstream has:

- `opus_decode_object.*`
- `opus_multistream_decode_object.*`
- `opus_decoder.*`

Rust should preserve those file boundaries and keep object-local behavior in those object files.

## Renderer/System Philosophy

The most dangerous remaining mismatches are often not in algorithms but in orchestration.

Priority areas:

- `renderer/system.rs`
- `renderer/behavior/info_updater.rs`

For these files:

- preserve ordering literally
- preserve counters literally
- preserve lifecycle transitions literally
- preserve validation literally

Do not rewrite them into a cleaner state machine if that obscures parity.

## Testing Philosophy

### Required testing style

Add focused tests for:

- parity-sensitive edge cases
- previous bugs
- binary-layout expectations
- initialization/finalization contracts
- command behavior that depends on ordering

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
3. lifecycle/ordering mismatches in renderer and ADSP paths
4. binary-layout mismatches in serialized commands
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
- Is central dispatcher ownership reduced?

### Behavior

- Does sequencing match upstream?
- Are sentinel values preserved?
- Are validation and failure paths aligned?

### Serialization

- Is binary layout correct?
- Are reserved bytes deterministic?

### Tests

- Is there a focused regression test for the nontrivial behavior?

## How To Communicate Progress

When reporting progress:

- state exactly what ownership/parity slice was improved
- name the affected files
- say what is still missing
- do not over-claim completion

Good progress statement:

- "Moved mix-family dump/verify/process dispatch out of `commands.rs` into `renderer/command/mix/mod.rs`; `commands.rs` now delegates by family. Full `cargo test -p audio_core` passes."

Bad progress statement:

- "Improved audio_core significantly."

## Final Standard

If another instance uses this file correctly, it should keep asking:

- "Where does this logic belong upstream?"
- "Why is this method not in the matching file?"
- "Why is this constant not next to its upstream equivalent?"
- "Why is this dispatcher owning behavior that belongs to the command module?"
- "Is this behavior literally the same, or merely plausible?"

That is the right mindset for finishing this port.


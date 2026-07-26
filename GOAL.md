# Goal: MK8D in-race performance parity

## Objective

Identify and fix why MK8D runs substantially slower in a race under ruzu than
under yuzu on the same machine.

The investigation must compare equivalent in-race workloads. Boot and menu
timings are useful context, but they are not the performance target.

The final result must:

- identify the dominant measured divergence rather than apply speculative
  optimizations;
- implement the upstream-faithful correction in the module that owns the
  behavior;
- add a focused regression test where practical;
- quantify performance before and after the correction against yuzu;
- preserve rendering, audio, input, and game progression.

## Repository and runtime inputs

Set these paths for the machine running the investigation:

```bash
export RUZU_ROOT=/path/to/ruzu
export YUZU_ROOT=/path/to/zuyu
export MK8D_ROM=/path/to/MarioKart8Deluxe.nsp
```

Use:

- release builds;
- Vulkan for both emulators;
- an unlocked, active desktop session with the emulator window visible;
- the same host, GPU, Vulkan driver, resolution, game version, update, DLC,
  audio backend, and controller configuration;
- one emulator process at a time;
- separate but equivalent cache/config directories;
- both cold-cache and warm-cache runs, clearly labelled.

Do not use OpenGL results to diagnose this goal. Do not compare ruzu on one
machine with yuzu on another as the primary benchmark. Apple Silicon results
may be used afterward to determine whether a confirmed bottleneck is
x64-specific.

## Restore deterministic menu input

The environment-gated frontend automation is currently commented out.
Re-enable it before collecting race profiles:

1. In `ruzu_cmd/src/emu_window/emu_window_sdl2.rs`, restore
   `use std::time::Duration`.
2. Uncomment `schedule_auto_lr_if_requested` and
   `schedule_auto_a_if_requested`.
3. In `ruzu_cmd/src/main.rs`, uncomment both calls immediately before the main
   event loop.

Keep this automation environment-gated. It must have no active threads or
runtime cost when its variables are absent.

Use one `L+R` press followed by exactly eleven `A` presses. These values are a
starting calibration and may be adjusted for machine speed:

```bash
export RUZU_AUTO_LR_DELAY_MS=12000
export RUZU_AUTO_LR_REPEAT_COUNT=1
export RUZU_AUTO_A_DELAY_MS=16000
export RUZU_AUTO_A_REPEAT_COUNT=11
export RUZU_AUTO_A_REPEAT_MS=2000
export RUZU_AUTO_A_MARKER=/tmp/ruzu-mk8d-menu-complete
export RUZU_AUTO_A_MARKER_ATTEMPT=11
export RUZU_AUTO_A_MARKER_DELAY_MS=500
```

Verify visually that this reaches the same race and game state on every run.
The marker only proves that the input sequence completed; it does not prove
that the race loaded. Start the measured interval only after the race is
visibly running and transient shader compilation has settled.

Apply an equivalent deterministic input sequence to yuzu. Host input
automation is acceptable, or an instrumented yuzu worktree may inject the same
NPad states. Record the actual input timestamps for both emulators. Do not
modify the reference tree at `$YUZU_ROOT/src` directly.

## Baseline protocol

Build ruzu:

```bash
cd "$RUZU_ROOT"
cargo build --release --bin ruzu-cmd
```

Launch ruzu with normal audio:

```bash
cd "$RUZU_ROOT"
rm -f /tmp/ruzu-mk8d-menu-complete
RUST_LOG=error \
gnome-session-inhibit \
  --inhibit idle \
  --reason "MK8D performance benchmark" \
  target/release/ruzu-cmd --renderer vulkan -g "$MK8D_ROM"
```

Before accepting a run, verify that the desktop session is not locked:

```bash
loginctl show-session "$XDG_SESSION_ID" -p Active -p IdleHint -p LockedHint
```

`Active=yes` and `LockedHint=no` are required. A locked or hidden session can
throttle both ruzu and yuzu to roughly one presented frame per second, making
boot timing, input calibration, and profiles invalid. `gnome-session-inhibit`
prevents a currently unlocked GNOME session from becoming idle during the run;
it does not unlock an already locked session.

Before benchmarking, inspect the environment and disable every unrelated
`RUZU_TRACE_*`, `RUZU_PROFILE_*`, dump, validation, and debug variable. Keep
only the auto-input variables required above. Do the same for yuzu logging and
Vulkan validation. Diagnostic logging must not contaminate timing runs.

For both emulators, collect at least three warm-cache race samples of the same
duration and the same scene. Keep raw artifacts under a per-run directory such
as:

```text
/tmp/mk8d-race-perf/
  ruzu-before/run-01/
  yuzu/run-01/
  ruzu-after/run-01/
```

Record:

- guest FPS or emulation speed;
- host frame-time median, p95, and p99;
- CPU time, cycles, instructions, IPC, context switches, migrations, and page
  faults;
- per-thread CPU utilization and thread names;
- GPU utilization and GPU frame time when available;
- present cadence and time spent waiting for the GPU;
- shader and pipeline compilation events during the measured interval;
- whether audio, visuals, and game progression remain correct.

Use `perf stat`, `perf record`, and `pidstat` on Linux. Attach profiling only
for the stable in-race interval so boot, menus, and initial pipeline
compilation do not dominate the profile.

Example commands, where `$PID` is the running emulator:

```bash
perf stat -p "$PID" \
  -e task-clock,cycles,instructions,context-switches,cpu-migrations,page-faults \
  -- sleep 20

perf record -F 199 -g --call-graph fp -p "$PID" -- sleep 20
perf report --stdio --percent-limit 0.5

pidstat -t -p "$PID" 1 20
```

If frame-time telemetry is missing, add a low-overhead counter at the existing
renderer/present ownership point. Do not use per-draw synchronous logging.
Prefer the existing asynchronous trace system for diagnostics.

## Investigation order

1. Prove the slowdown with matched, repeatable in-race samples and report the
   ratio between ruzu and yuzu.
2. Determine whether the limiting resource is guest CPU/JIT, kernel/HLE
   scheduling, GPU command processing, Vulkan submission/synchronization,
   shader/pipeline compilation, presentation, audio, or logging.
3. Attribute the dominant ruzu samples to concrete functions and threads.
4. Compare those ownership paths line-by-line with the corresponding yuzu C++
   implementation under `$YUZU_ROOT/src`.
5. Fix small confirmed upstream divergences immediately. For a large missing
   prerequisite, record the interrupted slice in a project-local state file,
   implement and verify the prerequisite first, then resume.
6. Add a focused regression test for the corrected invariant or hot-path
   behavior.
7. Re-run the same race workload with the same caches and measurement window.
8. Run focused tests, then the full test suite for every affected crate.

Do not optimize based only on source inspection. A suspected hot path must
appear in measurements, or a controlled experiment must demonstrate its
impact. Do not trade correctness for speed or bypass synchronization without
matching upstream behavior.

## Completion criteria

This goal is complete only when:

- `L+R` followed by eleven `A` presses reaches the race reliably in three
  consecutive runs;
- before/after ruzu and yuzu race measurements are preserved and comparable;
- the dominant performance divergence has a measured root cause;
- the correction is verified against the upstream owner and documented in
  `DIFF.md`;
- focused regression tests pass;
- full tests pass for each affected crate;
- a final race run confirms correct graphics, text, audio, input, progression,
  and stable frame pacing;
- any remaining performance gap is quantified and attributed rather than
  described only as future work.

## Required final report

Report:

- exact revisions, machine, CPU, GPU, Vulkan driver, game/update version, and
  cache state;
- exact launch and input automation settings;
- baseline ruzu versus yuzu measurements;
- profile evidence identifying the bottleneck;
- upstream/Rust files compared;
- correction and regression test;
- after-fix measurements and percentage improvement;
- remaining measured gap and its owner, if any.

# Capture harness

`capture-harness` launches one executable, starts a monotonic timer immediately after
`Command::spawn`, and captures X11 reference images at absolute timecodes. It can record the same
window (or the complete screen) with `ffmpeg`.

## Build and run

```bash
cargo build --release -p capture_harness
target/release/capture-harness tools/capture_harness/example.toml --dry-run
target/release/capture-harness tools/capture_harness/example.toml
```

Paths in the TOML file are resolved relative to that file. A command consisting of a single name
is resolved through `PATH`.

`process.rom_path` is mandatory and is always appended to the executable arguments. Set the
optional `process.rom_arg` when the executable expects a flag immediately before the path (for
example `rom_arg = "-g"`). Keep renderer and other ordinary switches in `process.args`.

## Clean start

The optional `[cleanup]` table removes explicitly listed shader-cache and save-data paths before
the target process is spawned:

```toml
[cleanup]
shader_cache_paths = ["/absolute/cache/path/for-one-title"]
save_data_paths = ["/absolute/save/path/for-one-title"]
```

Relative cleanup paths are resolved from the TOML file. No glob or environment-variable expansion
is performed. `--dry-run` validates and displays the configuration without removing anything. The
harness refuses broad or ambiguous targets, including the filesystem root, the home/current
directory, duplicate or nested cleanup paths, and any path containing the executable, ROM,
configuration, log, working directory, or capture output. Existing symlinks are removed without
following them. Cleanup results are recorded in `capture-manifest.json`.

The X11 dependencies are `xdotool`, ImageMagick's `import`, and (when video is enabled) `ffmpeg`.
XWayland windows also work when they expose an X11 window ID. Native Wayland capture is deliberately
not approximated because compositor permission dialogs would make the timing non-reproducible.

## Timed controller buttons

The optional `[input]` timeline sends logical Switch buttons to the emulation window. Before the
target is launched, the harness temporarily switches player 1 to Keyboard/Mouse in the frontend's
own configuration file:

- `ruzu-cmd` uses `sdl2-config.ini` and SDL scancodes (`A=A`, `L=F`, `R=H`);
- `reden` uses `qt-config.ini` and Qt key codes (`A=C`, `L=Q`, `R=E`).

The frontend is inferred from `process.executable`. `input.config_file` can override the path; this
is also useful with an isolated configuration. Unless `restore_config = false` is requested, the
original file is restored when the harness run ends, including error paths.

```toml
[input]
default_hold_ms = 100
restore_config = true

[[input.events]]
at = "00:00:12.000"
buttons = ["l", "r"]
label = "L+R"

[[input.events]]
at = "00:00:14.000"
buttons = ["a"]
```

Supported names are `a`, `b`, `x`, `y`, `l_stick`, `r_stick`, `l`, `r`, `zl`, `zr`, `plus`,
`minus`, `d_left`, `d_up`, `d_right`, and `d_down`. Buttons listed in one event are held
simultaneously. `hold_ms` can override the default for one event. Press and release are independent
absolute timeline events and are both recorded in `capture-manifest.json`. Each input event activates
the emulation window before using XTEST keyboard injection, so the harness intentionally takes
keyboard focus while an input timeline is active.

The repository includes two MK8D Reden scenarios. `mk8d_input_reden_20260816.toml` preserves the
requested 2-second cadence exactly; on Reden, menu transitions can consume some of those presses.
`mk8d_race_reden_20260816.toml` uses 4-second menu intervals (and the requested final 6-second gap)
and has been verified to reach the Mario Kart Stadium course introduction. The equivalent exact
`ruzu-cmd` scenario is `mk8d_input_20260816.toml`.

## Timing contract

- The timer origin is immediately after the target process has been accepted by the OS.
- Every event waits for an absolute offset from that origin, so one slow screenshot does not shift
  all later screenshots.
- Window discovery occurs while that timer is already running. Choose capture/video timecodes later
  than `window_wait_timeout` when startup time is uncertain.
- `capture-manifest.json` records scheduled time, actual time, and lateness for every event.
- At equal timecodes, the order is video start, input release, input press, screenshot, video stop.

`target = "window"` finds the largest visible window matching the launched PID and optional title
regular expression. Set `match_process_pid = false` when a launcher creates the actual window in a
different process, or provide a fixed `window_id`. `target = "screen"` captures the complete X11
display.

By default the launched process is terminated after the final timeline event. Set
`terminate_after_timeline = false` to leave it running until it exits naturally.

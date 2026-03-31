//! Port of zuyu/src/core/perf_stats.h and zuyu/src/core/perf_stats.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Performance statistics tracker (FPS, frame times, emulation speed).

use parking_lot::Mutex;
use std::sync::atomic::{AtomicU32, Ordering};
use std::time::{Duration, Instant};

/// Purposefully ignore the first five frames, as there's a significant amount of overhead in
/// booting that we shouldn't account for.
const IGNORE_FRAMES: usize = 5;

/// Number of frametime history entries (one hour at 60fps).
const PERF_HISTORY_SIZE: usize = 216_000;

/// Performance statistics results.
#[derive(Debug, Clone, Copy, Default)]
pub struct PerfStatsResults {
    /// System FPS (LCD VBlanks) in Hz
    pub system_fps: f64,
    /// Average game FPS (GPU frame renders) in Hz
    pub average_game_fps: f64,
    /// Walltime per system frame, in seconds, excluding any waits
    pub frametime: f64,
    /// Ratio of walltime / emulated time elapsed
    pub emulation_speed: f64,
}

/// Class to manage and query performance/timing statistics.
/// All public functions of this class are thread-safe.
pub struct PerfStats {
    inner: Mutex<PerfStatsInner>,
    /// Cumulative number of game frames (GPU frame submissions) since last reset.
    /// Atomic for cross-thread access without lock.
    game_frames: AtomicU32,
    /// Title ID for the game that is running. 0 if there is no game running yet.
    _title_id: u64,
}

struct PerfStatsInner {
    /// Current index for writing to the perf_history array
    current_index: usize,
    /// Stores historical frametime data (in milliseconds) useful for processing
    /// and tracking performance regressions with code changes.
    perf_history: Box<[f64; PERF_HISTORY_SIZE]>,

    /// Point when the cumulative counters were reset
    reset_point: Instant,
    /// System time when the cumulative counters were reset
    reset_point_system_us: Duration,

    /// Cumulative duration (excluding v-sync/frame-limiting) of frames since last reset
    accumulated_frametime: Duration,
    /// Cumulative number of system frames (LCD VBlanks) presented since last reset
    system_frames: u32,

    /// Point when the previous system frame ended
    previous_frame_end: Instant,
    /// Point when the current system frame began
    frame_begin: Instant,
    /// Total visible duration (including frame-limiting, etc.) of the previous system frame
    previous_frame_length: Duration,
    /// Previously computed fps
    previous_fps: f64,
}

impl PerfStats {
    pub fn new(title_id: u64) -> Self {
        let now = Instant::now();
        Self {
            inner: Mutex::new(PerfStatsInner {
                current_index: 0,
                perf_history: Box::new([0.0; PERF_HISTORY_SIZE]),
                reset_point: now,
                reset_point_system_us: Duration::ZERO,
                accumulated_frametime: Duration::ZERO,
                system_frames: 0,
                previous_frame_end: now,
                frame_begin: now,
                previous_frame_length: Duration::ZERO,
                previous_fps: 0.0,
            }),
            game_frames: AtomicU32::new(0),
            _title_id: title_id,
        }
    }

    /// Marks the beginning of a system frame.
    pub fn begin_system_frame(&self) {
        let mut inner = self.inner.lock();
        inner.frame_begin = Instant::now();
    }

    /// Marks the end of a system frame and records timing data.
    pub fn end_system_frame(&self) {
        let mut inner = self.inner.lock();
        let frame_end = Instant::now();
        let frame_time = frame_end - inner.frame_begin;

        let idx = inner.current_index;
        if idx < PERF_HISTORY_SIZE {
            inner.perf_history[idx] = frame_time.as_secs_f64() * 1000.0;
            inner.current_index += 1;
        }

        inner.accumulated_frametime += frame_time;
        inner.system_frames += 1;

        inner.previous_frame_length = frame_end - inner.previous_frame_end;
        inner.previous_frame_end = frame_end;
    }

    /// Marks the end of a game frame (GPU frame submission).
    pub fn end_game_frame(&self) {
        self.game_frames.fetch_add(1, Ordering::Relaxed);
    }

    /// Returns the arithmetic mean of all frametime values stored in the performance history.
    pub fn get_mean_frametime(&self) -> f64 {
        let inner = self.inner.lock();
        if inner.current_index <= IGNORE_FRAMES {
            return 0.0;
        }
        let sum: f64 = inner.perf_history[IGNORE_FRAMES..inner.current_index]
            .iter()
            .sum();
        sum / (inner.current_index - IGNORE_FRAMES) as f64
    }

    /// Gets and resets core performance statistics.
    pub fn get_and_reset_stats(&self, current_system_time_us: Duration) -> PerfStatsResults {
        let mut inner = self.inner.lock();
        let now = Instant::now();

        // Walltime elapsed since stats were reset
        let interval = (now - inner.reset_point).as_secs_f64();
        if interval == 0.0 {
            return PerfStatsResults::default();
        }

        let system_us_per_second =
            (current_system_time_us - inner.reset_point_system_us).as_micros() as f64 / interval;
        let current_frames = self.game_frames.load(Ordering::Relaxed) as f64;
        let current_fps = current_frames / interval;

        let system_fps = inner.system_frames as f64 / interval;
        let frametime = if inner.system_frames > 0 {
            inner.accumulated_frametime.as_secs_f64() / inner.system_frames as f64
        } else {
            0.0
        };

        let results = PerfStatsResults {
            system_fps,
            average_game_fps: (current_fps + inner.previous_fps) / 2.0,
            frametime,
            emulation_speed: system_us_per_second / 1_000_000.0,
        };

        // Reset counters
        inner.reset_point = now;
        inner.reset_point_system_us = current_system_time_us;
        inner.accumulated_frametime = Duration::ZERO;
        inner.system_frames = 0;
        self.game_frames.store(0, Ordering::Relaxed);
        inner.previous_fps = current_fps;

        results
    }

    /// Gets the ratio between walltime and the emulated time of the previous system frame.
    /// This is useful for scaling inputs or outputs moving between the two time domains.
    pub fn get_last_frame_time_scale(&self) -> f64 {
        let inner = self.inner.lock();
        const FRAME_LENGTH: f64 = 1.0 / 60.0;
        inner.previous_frame_length.as_secs_f64() / FRAME_LENGTH
    }
}

/// Speed limiter for single-core mode.
pub struct SpeedLimiter {
    /// Emulated system time (in microseconds) at the last limiter invocation
    previous_system_time_us: Duration,
    /// Walltime at the last limiter invocation
    previous_walltime: Instant,
    /// Accumulated difference between walltime and emulated time
    speed_limiting_delta_err: i64,
}

impl SpeedLimiter {
    pub fn new() -> Self {
        Self {
            previous_system_time_us: Duration::ZERO,
            previous_walltime: Instant::now(),
            speed_limiting_delta_err: 0,
        }
    }

    /// Performs speed limiting for single-core mode.
    /// Uses the emulated system time to determine how long to sleep.
    pub fn do_speed_limiting(
        &mut self,
        current_system_time_us: Duration,
        use_multi_core: bool,
        use_speed_limit: bool,
        speed_limit_percent: u16,
    ) {
        if use_multi_core || !use_speed_limit {
            return;
        }

        let mut now = Instant::now();
        let sleep_scale = speed_limit_percent as f64 / 100.0;

        // Max lag caused by slow frames
        let max_lag_time_us = (25_000.0 / sleep_scale) as i64; // 25ms in microseconds, scaled

        let elapsed_system_us = current_system_time_us
            .saturating_sub(self.previous_system_time_us)
            .as_micros() as f64;
        let scaled_elapsed = (elapsed_system_us / sleep_scale) as i64;
        let wall_elapsed = (now - self.previous_walltime).as_micros() as i64;

        self.speed_limiting_delta_err += scaled_elapsed;
        self.speed_limiting_delta_err -= wall_elapsed;
        self.speed_limiting_delta_err = self
            .speed_limiting_delta_err
            .clamp(-max_lag_time_us, max_lag_time_us);

        if self.speed_limiting_delta_err > 0 {
            let sleep_duration = Duration::from_micros(self.speed_limiting_delta_err as u64);
            std::thread::sleep(sleep_duration);
            let now_after_sleep = Instant::now();
            self.speed_limiting_delta_err -= (now_after_sleep - now).as_micros() as i64;
            now = now_after_sleep;
        }

        self.previous_system_time_us = current_system_time_us;
        self.previous_walltime = now;
    }
}

impl Default for SpeedLimiter {
    fn default() -> Self {
        Self::new()
    }
}

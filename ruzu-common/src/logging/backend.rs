//! Port of zuyu/src/common/logging/backend.h and zuyu/src/common/logging/backend.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! The C++ version uses a singleton Impl with a background thread draining an MPSC queue.
//! This Rust port uses a similar architecture with a background thread, atomic flags, and
//! crossbeam-style channel (std::sync::mpsc).

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::sync::Mutex;
use std::time::Instant;

use super::filter::Filter;
use super::log_entry::Entry;
use super::text_formatter::{format_log_message, print_colored_message};
use super::types::{Class, Level};

/// Trait for logging backends (matching the C++ Backend interface).
trait Backend: Send {
    fn write(&mut self, entry: &Entry);
    fn flush(&mut self);
    fn enable_for_stacktrace(&mut self);
}

/// Backend that writes to stderr with color.
struct ColorConsoleBackend {
    enabled: bool,
}

impl ColorConsoleBackend {
    fn new() -> Self {
        Self { enabled: false }
    }

    fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }
}

impl Backend for ColorConsoleBackend {
    fn write(&mut self, entry: &Entry) {
        if self.enabled {
            print_colored_message(entry);
        }
    }

    fn flush(&mut self) {
        // stderr shouldn't be buffered
    }

    fn enable_for_stacktrace(&mut self) {
        self.enabled = true;
    }
}

/// Backend that writes to a file.
struct FileBackend {
    file: Option<fs::File>,
    enabled: bool,
    bytes_written: usize,
}

impl FileBackend {
    fn new(filename: &std::path::Path) -> Self {
        // Rotate old log file
        let old_filename = filename.with_extension("old.txt");
        let _ = fs::remove_file(&old_filename);
        let _ = fs::rename(filename, &old_filename);

        let file = fs::File::create(filename).ok();

        Self {
            file,
            enabled: true,
            bytes_written: 0,
        }
    }
}

impl Backend for FileBackend {
    fn write(&mut self, entry: &Entry) {
        if !self.enabled {
            return;
        }

        if let Some(ref mut file) = self.file {
            let msg = format!("{}\n", format_log_message(entry));
            let bytes = msg.as_bytes();
            if file.write_all(bytes).is_ok() {
                self.bytes_written += bytes.len();
            }

            // Prevent logs from exceeding 100 MiB
            const WRITE_LIMIT: usize = 100 * 1024 * 1024;
            let write_limit_exceeded = self.bytes_written > WRITE_LIMIT;

            if entry.log_level >= Level::Error || write_limit_exceeded {
                if write_limit_exceeded {
                    self.enabled = false;
                }
                let _ = file.flush();
            }
        }
    }

    fn flush(&mut self) {
        if let Some(ref mut file) = self.file {
            let _ = file.flush();
        }
    }

    fn enable_for_stacktrace(&mut self) {
        self.enabled = true;
        self.bytes_written = 0;
    }
}

/// Static state as a singleton, matching the C++ Impl.
struct LoggerImpl {
    filter: Filter,
    color_console_backend: ColorConsoleBackend,
    file_backend: FileBackend,
    time_origin: Instant,
    sender: mpsc::Sender<Entry>,
}

static LOGGER_INITIALIZED: AtomicBool = AtomicBool::new(false);
static SUPPRESS_LOGGING: AtomicBool = AtomicBool::new(true);

// We use a Mutex for the singleton since we need mutable access.
static LOGGER: Mutex<Option<LoggerState>> = Mutex::new(None);

struct LoggerState {
    filter: Filter,
    sender: mpsc::Sender<Entry>,
    time_origin: Instant,
    color_console_enabled: AtomicBool,
    thread_handle: Option<std::thread::JoinHandle<()>>,
}

/// Initializes the logging system. This should be called early in main.
pub fn initialize(log_dir: Option<PathBuf>, filter_string: Option<&str>) {
    let mut guard = LOGGER.lock().unwrap();
    if guard.is_some() {
        log::warn!("Reinitializing logging backend");
        return;
    }

    let log_dir = log_dir.unwrap_or_else(|| {
        let mut dir = dirs_or_default();
        dir.push("log");
        dir
    });

    let _ = fs::create_dir_all(&log_dir);

    let mut filter = Filter::default();
    if let Some(filter_str) = filter_string {
        filter.parse_filter_string(filter_str);
    }

    let log_file = log_dir.join("ruzu_log.txt");
    let (sender, receiver) = mpsc::channel::<Entry>();

    let time_origin = Instant::now();

    // Create backends for the background thread
    let mut file_backend = FileBackend::new(&log_file);

    // We'll read the color_enabled flag from shared state
    let color_flag = std::sync::Arc::new(AtomicBool::new(false));
    let color_flag_thread = color_flag.clone();

    let thread_handle = std::thread::Builder::new()
        .name("Logger".to_string())
        .spawn(move || {
            while let Ok(entry) = receiver.recv() {
                // Write to file backend
                file_backend.write(&entry);

                // Write to color console if enabled
                if color_flag_thread.load(Ordering::Relaxed) {
                    print_colored_message(&entry);
                }
            }

            // Drain remaining messages
            // (channel is closed, recv will return Err after sender is dropped)
            file_backend.flush();
        })
        .expect("Failed to spawn logger thread");

    *guard = Some(LoggerState {
        filter,
        sender,
        time_origin,
        color_console_enabled: AtomicBool::new(false),
        thread_handle: Some(thread_handle),
    });

    // Store the color_flag Arc somewhere accessible... For simplicity, we use the
    // AtomicBool in LoggerState and the thread checks it via the Arc.
    // Actually, let's just store the Arc in a separate static.
    {
        let mut cflag = COLOR_FLAG.lock().unwrap();
        *cflag = Some(color_flag);
    }

    SUPPRESS_LOGGING.store(false, Ordering::SeqCst);
    LOGGER_INITIALIZED.store(true, Ordering::SeqCst);
}

static COLOR_FLAG: Mutex<Option<std::sync::Arc<AtomicBool>>> = Mutex::new(None);

/// Stops the logger thread and flushes buffers.
pub fn stop() {
    let mut guard = LOGGER.lock().unwrap();
    if let Some(mut state) = guard.take() {
        // Drop the sender to signal the thread to finish
        drop(state.sender);
        if let Some(handle) = state.thread_handle.take() {
            let _ = handle.join();
        }
    }
    LOGGER_INITIALIZED.store(false, Ordering::SeqCst);
    SUPPRESS_LOGGING.store(true, Ordering::SeqCst);
}

/// Disables logging (used in tests).
pub fn disable_logging_in_tests() {
    SUPPRESS_LOGGING.store(true, Ordering::SeqCst);
}

/// Sets the global filter.
pub fn set_global_filter(filter: &Filter) {
    let mut guard = LOGGER.lock().unwrap();
    if let Some(ref mut state) = *guard {
        state.filter = filter.clone();
    }
}

/// Enables or disables the color console backend.
pub fn set_color_console_backend_enabled(enabled: bool) {
    if let Ok(cflag) = COLOR_FLAG.lock() {
        if let Some(ref flag) = *cflag {
            flag.store(enabled, Ordering::Relaxed);
        }
    }
}

/// Pushes a log entry into the logging system. Called by the `log_message!` macro.
pub fn push_entry(
    log_class: Class,
    log_level: Level,
    filename: &str,
    line_num: u32,
    function: &str,
    message: String,
) {
    if SUPPRESS_LOGGING.load(Ordering::Relaxed) {
        return;
    }

    let guard = LOGGER.lock().unwrap();
    if let Some(ref state) = *guard {
        if !state.filter.check_message(log_class, log_level) {
            return;
        }

        let entry = Entry {
            timestamp: state.time_origin.elapsed(),
            log_class,
            log_level,
            filename: filename.to_string(),
            line_num,
            function: function.to_string(),
            message,
        };

        let _ = state.sender.send(entry);
    }
}

fn dirs_or_default() -> PathBuf {
    if let Some(home) = std::env::var_os("HOME") {
        let mut p = PathBuf::from(home);
        p.push(".local");
        p.push("share");
        p.push("ruzu");
        p
    } else {
        PathBuf::from(".")
    }
}

/// Convenience macro matching the C++ LOG_INFO, LOG_WARNING, etc. patterns.
///
/// Usage: `log_message!(Class::Kernel_SVC, Level::Info, "SVC called: {}", svc_name);`
#[macro_export]
macro_rules! log_message {
    ($class:expr, $level:expr, $($arg:tt)*) => {{
        $crate::logging::backend::push_entry(
            $class,
            $level,
            file!(),
            line!(),
            // Rust doesn't have __func__, we use module_path! as approximation
            module_path!(),
            format!($($arg)*),
        );
    }};
}

/// LOG_TRACE - only active in debug builds (matching the C++ behavior).
#[macro_export]
macro_rules! log_trace {
    ($class:expr, $($arg:tt)*) => {{
        #[cfg(debug_assertions)]
        $crate::log_message!($class, $crate::logging::types::Level::Trace, $($arg)*);
    }};
}

#[macro_export]
macro_rules! log_debug {
    ($class:expr, $($arg:tt)*) => {
        $crate::log_message!($class, $crate::logging::types::Level::Debug, $($arg)*)
    };
}

#[macro_export]
macro_rules! log_info {
    ($class:expr, $($arg:tt)*) => {
        $crate::log_message!($class, $crate::logging::types::Level::Info, $($arg)*)
    };
}

#[macro_export]
macro_rules! log_warning {
    ($class:expr, $($arg:tt)*) => {
        $crate::log_message!($class, $crate::logging::types::Level::Warning, $($arg)*)
    };
}

#[macro_export]
macro_rules! log_error {
    ($class:expr, $($arg:tt)*) => {
        $crate::log_message!($class, $crate::logging::types::Level::Error, $($arg)*)
    };
}

#[macro_export]
macro_rules! log_critical {
    ($class:expr, $($arg:tt)*) => {
        $crate::log_message!($class, $crate::logging::types::Level::Critical, $($arg)*)
    };
}

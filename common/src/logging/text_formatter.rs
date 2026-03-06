//! Port of zuyu/src/common/logging/text_formatter.h and zuyu/src/common/logging/text_formatter.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use super::log_entry::Entry;
use super::types::Level;

/// Formats a log entry into a string matching the C++ output format:
/// `[seconds.microseconds] ClassName <LevelName> filename:function:line: message`
pub fn format_log_message(entry: &Entry) -> String {
    let time_seconds = entry.timestamp.as_secs();
    let time_fractional = entry.timestamp.subsec_micros();

    let class_name = entry.log_class.name();
    let level_name = entry.log_level.name();

    format!(
        "[{:4}.{:06}] {} <{}> {}:{}:{}: {}",
        time_seconds,
        time_fractional,
        class_name,
        level_name,
        entry.filename,
        entry.function,
        entry.line_num,
        entry.message,
    )
}

/// Formats and prints a log entry to stderr.
pub fn print_message(entry: &Entry) {
    let msg = format_log_message(entry);
    eprintln!("{}", msg);
}

/// Prints the same message as `print_message`, but colored according to the severity level.
pub fn print_colored_message(entry: &Entry) {
    let color = match entry.log_level {
        Level::Trace => "\x1b[1;30m",    // Grey
        Level::Debug => "\x1b[0;36m",    // Cyan
        Level::Info => "\x1b[0;37m",     // Bright gray
        Level::Warning => "\x1b[1;33m",  // Bright yellow
        Level::Error => "\x1b[1;31m",    // Bright red
        Level::Critical => "\x1b[1;35m", // Bright magenta
    };

    let msg = format_log_message(entry);
    eprint!("{}{}\x1b[0m\n", color, msg);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logging::types::Class;
    use std::time::Duration;

    #[test]
    fn test_format_log_message() {
        let entry = Entry {
            timestamp: Duration::from_micros(1_234_567),
            log_class: Class::Kernel,
            log_level: Level::Info,
            filename: "test.rs".to_string(),
            line_num: 42,
            function: "do_thing".to_string(),
            message: "Hello world".to_string(),
        };
        let formatted = format_log_message(&entry);
        assert!(formatted.contains("[   1.234567]"));
        assert!(formatted.contains("Kernel"));
        assert!(formatted.contains("<Info>"));
        assert!(formatted.contains("Hello world"));
    }
}

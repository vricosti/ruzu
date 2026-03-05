//! Port of zuyu/src/common/logging/filter.h and zuyu/src/common/logging/filter.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use super::types::{Class, Level};

/// Implements a log message filter which allows different log classes to have different minimum
/// severity levels. The filter can be changed at runtime and can be parsed from a string to allow
/// editing via the interface or loading from a configuration file.
#[derive(Debug, Clone)]
pub struct Filter {
    class_levels: [Level; Class::COUNT],
}

impl Filter {
    /// Initializes the filter with all classes having `default_level` as the minimum level.
    pub fn new(default_level: Level) -> Self {
        Self {
            class_levels: [default_level; Class::COUNT],
        }
    }

    /// Resets the filter so that all classes have `level` as the minimum displayed level.
    pub fn reset_all(&mut self, level: Level) {
        self.class_levels.fill(level);
    }

    /// Sets the minimum level of `log_class` (and not of its subclasses) to `level`.
    pub fn set_class_level(&mut self, log_class: Class, level: Level) {
        self.class_levels[log_class as usize] = level;
    }

    /// Parses a filter string and applies it to this filter.
    ///
    /// A filter string consists of a space-separated list of filter rules, each of the format
    /// `<class>:<level>`. `<class>` is a log class name, with subclasses separated using periods.
    /// `*` is allowed as a class name and will reset all filters to the specified level. `<level>`
    /// is a severity level name which will be set as the minimum logging level of the matched classes.
    pub fn parse_filter_string(&mut self, filter_str: &str) {
        for clause in filter_str.split_whitespace() {
            if clause.is_empty() {
                continue;
            }
            self.parse_filter_rule(clause);
        }
    }

    /// Matches class/level combination against the filter, returning true if it passed.
    pub fn check_message(&self, log_class: Class, level: Level) -> bool {
        (level as u8) >= (self.class_levels[log_class as usize] as u8)
    }

    /// Returns true if any logging classes are set to debug.
    pub fn is_debug(&self) -> bool {
        self.class_levels
            .iter()
            .any(|&l| (l as u8) <= (Level::Debug as u8))
    }

    fn parse_filter_rule(&mut self, rule: &str) {
        let Some(colon_pos) = rule.find(':') else {
            log::error!("Invalid log filter. Must specify a log level after ':': {}", rule);
            return;
        };

        let class_str = &rule[..colon_pos];
        let level_str = &rule[colon_pos + 1..];

        let Some(level) = get_level_by_name(level_str) else {
            log::error!("Unknown log level in filter: {}", rule);
            return;
        };

        if class_str == "*" {
            self.reset_all(level);
            return;
        }

        let Some(log_class) = Class::from_name(class_str) else {
            log::error!("Unknown log class in filter: {}", rule);
            return;
        };

        self.set_class_level(log_class, level);
    }
}

impl Default for Filter {
    fn default() -> Self {
        Self::new(Level::Info)
    }
}

fn get_level_by_name(name: &str) -> Option<Level> {
    for i in 0..Level::COUNT {
        if let Some(level) = Level::from_u8(i as u8) {
            if level.name().eq_ignore_ascii_case(name) {
                return Some(level);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_filter() {
        let filter = Filter::default();
        assert!(filter.check_message(Class::Log, Level::Info));
        assert!(filter.check_message(Class::Log, Level::Error));
        assert!(!filter.check_message(Class::Log, Level::Debug));
    }

    #[test]
    fn test_parse_filter_string() {
        let mut filter = Filter::new(Level::Info);
        filter.parse_filter_string("*:Debug Service.FS:Trace");
        assert!(filter.check_message(Class::Log, Level::Debug));
        assert!(filter.check_message(Class::Service_FS, Level::Trace));
    }

    #[test]
    fn test_is_debug() {
        let mut filter = Filter::new(Level::Info);
        assert!(!filter.is_debug());
        filter.set_class_level(Class::Kernel, Level::Debug);
        assert!(filter.is_debug());
    }
}

//! Port of zuyu/src/common/logging/log_entry.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::time::Duration;

use super::types::{Class, Level};

/// A log entry. Log entries are stored in a structured format to permit more varied output
/// formatting on different frontends, as well as facilitating filtering and aggregation.
#[derive(Debug, Clone)]
pub struct Entry {
    pub timestamp: Duration,
    pub log_class: Class,
    pub log_level: Level,
    pub filename: String,
    pub line_num: u32,
    pub function: String,
    pub message: String,
}

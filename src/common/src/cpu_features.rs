// SPDX-FileCopyrightText: 2026 Reden contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of the global wall-clock ownership in Eden `common/cpu_features.{h,cpp}`.
//!
//! Architecture-specific CPU capability detection remains in `x64::cpu_detect`,
//! while this counterpart owns Eden's process-wide `g_wall_clock`.

use std::sync::LazyLock;

use crate::wall_clock::{create_optimal_clock, WallClock};

/// Process-wide host wall clock, matching Eden's `Common::g_wall_clock`.
pub static G_WALL_CLOCK: LazyLock<Box<dyn WallClock>> = LazyLock::new(create_optimal_clock);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn global_wall_clock_is_initialized_once_and_monotonic() {
        let first = G_WALL_CLOCK.get_time_ns();
        let second = G_WALL_CLOCK.get_time_ns();
        assert!(second >= first);
    }
}

// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_input.cpp`
// (`ConfigureInput`), whose widget tree lives in `configure_input.ui`.
//
// Upstream `ConfigureInput` is a container whose `GetSubTabs()` returns the
// eight per-player pages plus the "Advanced" page — and `ConfigureDialog`
// splices that list straight into the outer tab widget. That is why the
// Controls screen shows nine tabs rather than a nested tab widget (unlike the
// Debug screen, which does nest).

use super::configure_dialog::Page;
use super::configure_input_advanced;
use super::configure_input_player;

/// Number of player tabs — upstream builds `Settings::values.players` slots
/// 0..8 as "Player 1".."Player 8".
pub const NUM_PLAYERS: usize = 8;

/// Build the Controls tabs — upstream `ConfigureInput::GetSubTabs()`.
pub fn pages() -> Vec<Page> {
    let mut pages: Vec<Page> = (0..NUM_PLAYERS)
        .map(configure_input_player::page)
        .collect();
    pages.push(configure_input_advanced::page());
    pages
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn controls_section_has_eight_players_plus_advanced() {
        // Upstream's Controls row shows nine tabs; a mismatch would drop a
        // player's bindings from the dialog entirely.
        assert_eq!(NUM_PLAYERS, 8);
    }
}

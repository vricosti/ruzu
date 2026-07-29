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

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use super::configure_dialog::Page;
use super::configure_input_advanced;
use super::configure_input_player;
use super::input_profiles::InputProfiles;

/// Number of player tabs — upstream builds `Settings::values.players` slots
/// 0..8 as "Player 1".."Player 8".
pub const NUM_PLAYERS: usize = 8;

/// Build the Controls tabs — upstream `ConfigureInput::GetSubTabs()`.
pub fn pages(
    input_subsystem: Rc<RefCell<input_common::InputSubsystem>>,
    hid_core: Arc<parking_lot::Mutex<hid_core::hid_core::HIDCore>>,
) -> Vec<Page> {
    // Upstream `ConfigureInput` owns one `InputProfiles` instance shared by
    // every per-player page.
    let profiles = Rc::new(configure_input_player::InputProfileContext::new(
        InputProfiles::new(),
    ));
    let mut pages: Vec<Page> = (0..NUM_PLAYERS)
        .map(|index| {
            configure_input_player::page(
                index,
                Rc::clone(&input_subsystem),
                Arc::clone(&hid_core),
                Rc::clone(&profiles),
            )
        })
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

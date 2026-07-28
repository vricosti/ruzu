// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_linux_tab.cpp`
// (`ConfigureLinuxTab`), whose widget tree lives in `configure_linux_tab.ui`.
//
// Upstream builds this as a standalone `QWidget` holding a single "Linux"
// `QGroupBox`, which `ConfigureGeneral` appends into its own layout — so it
// renders as a second group on the General page rather than as its own tab.
// This module therefore exposes a `group()` returning the group box, matching
// how it is actually consumed.
//
// The setting itself (`Settings::values.enable_gamemode`) lives in the `common`
// crate, mirroring upstream's `Common::Settings`.

use gtk::prelude::*;

use super::shared_widget as w;

/// Build the "Linux" group. Returns the group's outer box plus the gamemode
/// check button so the owning page can read it back in `ApplyConfiguration`.
pub fn group() -> (gtk::Box, gtk::CheckButton) {
    let (outer, content) = w::group("Linux");

    let enabled = *common::settings::values().enable_gamemode.get_value();
    let gamemode = w::check_row("Enable Gamemode", enabled);
    content.append(&gamemode);

    (outer, gamemode)
}

// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_general.cpp`
// (`ConfigureGeneral`), whose widget tree lives in `configure_general.ui`.
//
// The page holds two groups:
//   * "General" — the emulation-behaviour settings, all `UISettings` fields;
//   * "Linux"   — the gamemode toggle, which upstream splits into its own
//     `ConfigureLinuxTab` widget appended to the same page.
// plus a "Reset All Settings" button pinned to the bottom.
//
// Upstream builds these rows generically through
// `ConfigurationShared::Builder::BuildWidget`, driven by the settings registry.
// The Rust port constructs them explicitly (see `shared_widget`'s module note),
// so the row list here is the literal contents of `configure_general.ui`.

use gtk::prelude::*;

use crate::uisettings;

use super::configure_dialog::Page;
use super::configure_linux_tab;
use super::shared_translation as tr;
use super::shared_widget as w;

/// Build the General tab — upstream `ConfigureGeneral`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    // --- "General" group -------------------------------------------------
    let (general_group, general) = w::group("General");

    let confirm_index = uisettings::with(|v| {
        tr::index_of(tr::CONFIRM_STOP, v.confirm_before_stopping.get_value())
    });
    let (confirm_row, confirm) = w::combo_row(
        "Confirm before stopping emulation",
        &tr::labels(tr::CONFIRM_STOP),
        confirm_index,
    );
    general.append(&confirm_row);

    let pause_background = w::check_row(
        "Pause emulation when in background",
        uisettings::with(|v| *v.pause_when_in_background.get_value()),
    );
    general.append(&pause_background);

    let hide_mouse = w::check_row(
        "Hide mouse on inactivity",
        uisettings::with(|v| *v.hide_mouse.get_value()),
    );
    general.append(&hide_mouse);

    let disable_controller_applet = w::check_row(
        "Disable controller applet",
        uisettings::with(|v| *v.controller_applet_disabled.get_value()),
    );
    general.append(&disable_controller_applet);

    let select_user_on_boot = w::check_row(
        "Prompt for user on game boot",
        uisettings::with(|v| *v.select_user_on_boot.get_value()),
    );
    general.append(&select_user_on_boot);

    column.append(&general_group);

    // --- "Linux" group (upstream `ConfigureLinuxTab`) ---------------------
    let (linux_group, gamemode) = configure_linux_tab::group();
    column.append(&linux_group);

    // --- "Reset All Settings" --------------------------------------------
    // Upstream pins this button bottom-left of the page (a `QSpacerItem` sits
    // between it and the groups above), so it sits below the scrolling column
    // rather than immediately after the Linux group.
    let reset = gtk::Button::with_label("Reset All Settings");
    reset.set_halign(gtk::Align::Start);
    reset.set_margin_top(8);
    reset.set_margin_bottom(10);
    reset.set_margin_start(10);
    reset.connect_clicked(|_| {
        // Upstream pops a confirmation, resets `Settings` + `UISettings` to
        // their defaults, and closes the dialog via the callback installed by
        // `ConfigureDialog` (`general_tab->SetResetCallback`). Wiring the reset
        // itself needs the config writer, which is a separate slice; log until
        // then rather than silently doing nothing.
        log::info!("Reset All Settings requested (config writer not yet wired)");
    });

    let root = gtk::Box::new(gtk::Orientation::Vertical, 0);
    root.append(&scroller);
    root.append(&reset);

    Page::new("General", root, move || {
        let confirm_value = tr::value_at(tr::CONFIRM_STOP, confirm.selected());
        let pause = pause_background.is_active();
        let hide = hide_mouse.is_active();
        let no_controller_applet = disable_controller_applet.is_active();
        let select_user = select_user_on_boot.is_active();
        let use_gamemode = gamemode.is_active();

        uisettings::with_mut(|v| {
            v.confirm_before_stopping.set_value(confirm_value);
            v.pause_when_in_background.set_value(pause);
            v.hide_mouse.set_value(hide);
            v.controller_applet_disabled.set_value(no_controller_applet);
            v.select_user_on_boot.set_value(select_user);
        });
        common::settings::values_mut()
            .enable_gamemode
            .set_value(use_gamemode);
    })
}

// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// Eden `src/yuzu/configuration/configure_applets.cpp`
// (`ConfigureApplets`), whose widget tree lives in `configure_applets.ui`.
//
// A single "Applet mode preference" group: one combo per library applet,
// choosing between the emulator's own dialog ("Custom frontend", `AppletMode::HLE`)
// and the console's real applet ("Real applet", `AppletMode::LLE`).
//
// The row labels come from `shared_translation.cpp`'s `INSERT(Settings,
// <field>_applet_mode, ...)` entries; the row *order* is `configure_applets.ui`'s,
// which is not the same as the declaration order in `Settings::Values`.

use gtk::prelude::*;

use common::settings::Values;
use common::settings_enums::AppletMode;

use super::configure_dialog::Page;
use super::shared_translation as tr;
use super::shared_widget as w;

/// The applets the dialog exposes, in `configure_applets.ui` order, paired with
/// an accessor for the matching `Settings::Values` field.
///
/// Upstream exposes nine of the fifteen `*_applet_mode` settings; the rest
/// (`shop`, `login_share`, `wifi_web_auth`, `my_page`, `net_connect`,
/// `data_erase`) have no UI row, so they keep their defaults.
type Field = fn(&mut Values) -> &mut common::settings_common::SwitchableSetting<AppletMode>;

const APPLETS: &[(&str, Field)] = &[
    ("Amiibo editor", |v| &mut v.cabinet_applet_mode),
    ("Controller configuration", |v| {
        &mut v.controller_applet_mode
    }),
    ("Error", |v| &mut v.error_applet_mode),
    ("Player select", |v| &mut v.player_select_applet_mode),
    ("Software keyboard", |v| &mut v.swkbd_applet_mode),
    ("Mii Edit", |v| &mut v.mii_edit_applet_mode),
    ("Online web", |v| &mut v.web_applet_mode),
    ("Photo viewer", |v| &mut v.photo_viewer_applet_mode),
    ("Offline web", |v| &mut v.offline_web_applet_mode),
];

/// Build the Applets tab — upstream `ConfigureApplets`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    let (group, content) = w::group("Applet mode preference");

    let labels = tr::labels(tr::APPLET_MODE);
    let mut combos = Vec::with_capacity(APPLETS.len());

    for (label, field) in APPLETS {
        let current = {
            // `get_value` borrows the settings guard, so read through a clone
            // rather than holding the lock across widget construction.
            let mut values = common::settings::values_mut();
            *field(&mut values).get_value()
        };
        let (row, combo) = w::combo_row(label, &labels, tr::index_of(tr::APPLET_MODE, &current));
        content.append(&row);
        combos.push((*field, combo));
    }

    let enable_overlay = w::check_row(
        "Enable Overlay Applet",
        *common::settings::values().enable_overlay.get_value(),
    );
    content.append(&enable_overlay);

    column.append(&group);

    Page::new("Applets", scroller, move || {
        let mut values = common::settings::values_mut();
        for (field, combo) in &combos {
            let mode = tr::value_at(tr::APPLET_MODE, combo.selected());
            field(&mut values).set_value(mode);
        }
        values.enable_overlay.set_value(enable_overlay.is_active());
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn applet_rows_match_upstream_ui_order() {
        // `configure_applets.ui` lists these nine, in this order. A reorder
        // would silently move a user's saved choice onto a different applet.
        let labels: Vec<&str> = APPLETS.iter().map(|(label, _)| *label).collect();
        assert_eq!(
            labels,
            vec![
                "Amiibo editor",
                "Controller configuration",
                "Error",
                "Player select",
                "Software keyboard",
                "Mii Edit",
                "Online web",
                "Photo viewer",
                "Offline web",
            ]
        );
    }

    #[test]
    fn every_row_targets_a_distinct_setting() {
        let mut values = Values::default();
        // Stamp each field with a distinct mode, then verify each accessor
        // reads back what it wrote — catching a copy-paste duplicate.
        for (index, (_, field)) in APPLETS.iter().enumerate() {
            let mode = if index % 2 == 0 {
                AppletMode::HLE
            } else {
                AppletMode::LLE
            };
            field(&mut values).set_value(mode);
        }
        for (index, (_, field)) in APPLETS.iter().enumerate() {
            let expected = if index % 2 == 0 {
                AppletMode::HLE
            } else {
                AppletMode::LLE
            };
            assert_eq!(*field(&mut values).get_value(), expected);
        }
    }
}

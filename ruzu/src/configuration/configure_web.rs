// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_web.cpp`
// (`ConfigureWeb`), whose widget tree lives in `configure_web.ui`.
//
// Two groups: the web-service credentials (username, token, Verify) and the
// telemetry opt-in with its regenerable telemetry ID.

use gtk::prelude::*;

use super::configure_dialog::Page;
use super::shared_widget as w;

/// Build the Web tab — upstream `ConfigureWeb`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    // --- "ruzu Web Service" ----------------------------------------------
    let (service_group, service) = w::group("ruzu Web Service");

    let consent = gtk::Label::new(Some(
        "By providing your username and token, you agree to allow ruzu to collect additional \
         usage data, which may include user identifying information.",
    ));
    consent.set_xalign(0.0);
    consent.set_wrap(true);
    service.append(&consent);

    let username = common::settings::values().yuzu_username.get_value().clone();
    let username_label = gtk::Label::new(Some(&format!(
        "Username: {}",
        if username.is_empty() {
            "Unspecified".to_string()
        } else {
            username
        }
    )));
    username_label.set_xalign(0.0);
    service.append(&username_label);

    let token_value = common::settings::values().yuzu_token.get_value().clone();
    let (token_row, token) = w::entry_row("Token:", &token_value);
    // Upstream sets `QLineEdit::Password` echo mode on the token field.
    token.set_visibility(false);
    service.append(&token_row);

    let links = gtk::Box::new(gtk::Orientation::Horizontal, 12);
    let sign_up = gtk::LinkButton::with_label("https://profile.yuzu-emu.org/", "Sign up");
    sign_up.set_has_frame(false);
    let what_is_token = gtk::LinkButton::with_label(
        "https://yuzu-emu.org/wiki/yuzu-web-service/",
        "What is my token?",
    );
    what_is_token.set_has_frame(false);
    let spacer = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    spacer.set_hexpand(true);
    let verify = gtk::Button::with_label("Verify");
    links.append(&sign_up);
    links.append(&what_is_token);
    links.append(&spacer);
    links.append(&verify);
    service.append(&links);

    column.append(&service_group);

    // --- "Telemetry" ------------------------------------------------------
    let (telemetry_group, telemetry) = w::group("Telemetry");

    let share = w::check_row(
        "Share anonymous usage data with the ruzu team",
        *common::settings::values().enable_telemetry.get_value(),
    );
    telemetry.append(&share);

    let learn_more =
        gtk::LinkButton::with_label("https://yuzu-emu.org/help/feature/telemetry/", "Learn more");
    learn_more.set_has_frame(false);
    learn_more.set_halign(gtk::Align::Start);
    telemetry.append(&learn_more);

    let id_row = gtk::Box::new(gtk::Orientation::Horizontal, 6);
    let telemetry_id = gtk::Label::new(Some(&format!(
        "Telemetry ID: 0x{:016X}",
        current_telemetry_id()
    )));
    telemetry_id.set_xalign(0.0);
    telemetry_id.set_hexpand(true);
    let regenerate = gtk::Button::with_label("Regenerate");
    id_row.append(&telemetry_id);
    id_row.append(&regenerate);
    telemetry.append(&id_row);

    column.append(&telemetry_group);

    // Upstream's Verify posts the token to the web service and reports the
    // result; Regenerate calls `Core::RegenerateTelemetryId()`. Neither the web
    // service client nor the telemetry store is wired into ruzu yet.
    verify.connect_clicked(|_| {
        log::info!("Web: Verify requested (web service client not yet wired)");
    });
    regenerate.connect_clicked(|_| {
        log::info!("Web: Regenerate telemetry ID requested (telemetry store not yet wired)");
    });

    Page::new("Web", scroller, move || {
        let token_text = token.text().to_string();
        let telemetry_enabled = share.is_active();
        let mut values = common::settings::values_mut();
        values.yuzu_token.set_value(token_text);
        values.enable_telemetry.set_value(telemetry_enabled);
    })
}

/// The telemetry ID upstream reads from `Core::GetTelemetryId()`. That store is
/// not ported, so report 0 rather than inventing an ID that would then differ
/// from whatever the real store eventually holds.
fn current_telemetry_id() -> u64 {
    0
}

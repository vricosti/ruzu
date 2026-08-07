// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_tas.cpp`.

use gtk::glib;
use gtk::prelude::*;

pub fn present(parent: &gtk::Window) {
    let window = gtk::Window::builder()
        .title(crate::i18n::tr("TAS Configuration"))
        .transient_for(parent)
        .modal(true)
        .resizable(false)
        .default_width(560)
        .build();

    let content = gtk::Box::new(gtk::Orientation::Vertical, 10);
    content.set_margin_top(12);
    content.set_margin_bottom(12);
    content.set_margin_start(12);
    content.set_margin_end(12);

    let path = common::fs::path_util::get_ruzu_path_string(common::fs::path_util::RuzuPath::TASDir);
    let (path_row, path_entry, browse) = super::shared_widget::path_row("Path", &path);
    content.append(&path_row);

    let values = common::settings::values();
    let enabled = gtk::CheckButton::with_label(&crate::i18n::tr("Enable TAS features"));
    enabled.set_active(*values.tas_enable.get_value());
    let loop_script = gtk::CheckButton::with_label(&crate::i18n::tr("Loop script"));
    loop_script.set_active(*values.tas_loop.get_value());
    let pause_on_load =
        gtk::CheckButton::with_label(&crate::i18n::tr("Pause execution during loads"));
    pause_on_load.set_active(*values.pause_tas_on_load.get_value());
    pause_on_load.set_sensitive(false);
    drop(values);
    content.append(&enabled);
    content.append(&loop_script);
    content.append(&pause_on_load);

    browse.connect_clicked(glib::clone!(
        #[weak]
        window,
        #[weak]
        path_entry,
        move |_| {
            crate::gtk_compat::select_folder(
                Some(&window),
                "Select TAS Load Directory...",
                move |folder| {
                    if let Some(path) = folder.and_then(|folder| folder.path()) {
                        path_entry.set_text(&path.to_string_lossy());
                    }
                },
            );
        }
    ));

    let buttons = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    buttons.set_halign(gtk::Align::End);
    let cancel = gtk::Button::with_label(&crate::i18n::tr("Cancel"));
    let ok = gtk::Button::with_label(&crate::i18n::tr("OK"));
    ok.add_css_class("suggested-action");
    buttons.append(&cancel);
    buttons.append(&ok);
    content.append(&buttons);
    window.set_child(Some(&content));

    cancel.connect_clicked(glib::clone!(
        #[weak]
        window,
        move |_| window.close()
    ));
    ok.connect_clicked(glib::clone!(
        #[weak]
        window,
        move |_| {
            common::fs::path_util::set_ruzu_path(
                common::fs::path_util::RuzuPath::TASDir,
                std::path::Path::new(path_entry.text().as_str()),
            );
            let mut values = common::settings::values_mut();
            values.tas_enable.set_value(enabled.is_active());
            values.tas_loop.set_value(loop_script.is_active());
            values
                .pause_tas_on_load
                .set_value(pause_on_load.is_active());
            drop(values);
            if let Err(error) = super::qt_config::save_tas_values() {
                log::error!("Failed to save TAS configuration: {error}");
            }
            window.close();
        }
    ));
    window.present();
}

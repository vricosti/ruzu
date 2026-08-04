// SPDX-License-Identifier: GPL-3.0-or-later
//
// GTK compatibility adapters for APIs that GTK 4.10 replaced with
// AlertDialog and FileDialog. Keep only toolkit mechanics here; the owning
// frontend modules retain their actions and response handling.

use std::cell::RefCell;

use gtk::prelude::*;
use gtk::{gio, ButtonsType, FileChooserAction, MessageType, ResponseType};

/// Show a modal informational message using the GTK 4.0 MessageDialog API.
pub fn show_message<P: IsA<gtk::Window>>(parent: Option<&P>, message: &str, detail: &str) {
    show_message_with_type(parent, message, detail, MessageType::Info);
}

/// Show a modal warning using the GTK 4.0 MessageDialog API.
pub fn show_warning<P: IsA<gtk::Window>>(parent: Option<&P>, message: &str, detail: &str) {
    show_message_with_type(parent, message, detail, MessageType::Warning);
}

fn show_message_with_type<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    message: &str,
    detail: &str,
    message_type: MessageType,
) {
    let dialog = gtk::MessageDialog::builder()
        .modal(true)
        .message_type(message_type)
        .buttons(ButtonsType::Ok)
        .text(message)
        .secondary_text(detail)
        .build();
    if let Some(parent) = parent {
        dialog.set_transient_for(Some(parent));
    }
    dialog.connect_response(|dialog, _| dialog.close());
    dialog.present();
}

/// Show a two-button modal question and report whether the accept button won.
pub fn ask_question<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    message: &str,
    detail: &str,
    cancel_label: &str,
    accept_label: &str,
    callback: impl FnOnce(bool) + 'static,
) {
    let dialog = gtk::MessageDialog::builder()
        .modal(true)
        .message_type(MessageType::Question)
        .buttons(ButtonsType::None)
        .text(message)
        .secondary_text(detail)
        .build();
    if let Some(parent) = parent {
        dialog.set_transient_for(Some(parent));
    }
    dialog.add_button(cancel_label, ResponseType::Cancel);
    dialog.add_button(accept_label, ResponseType::Accept);
    dialog.set_default_response(ResponseType::Accept);

    let callback = RefCell::new(Some(callback));
    dialog.connect_response(move |dialog, response| {
        if let Some(callback) = callback.borrow_mut().take() {
            callback(response == ResponseType::Accept);
        }
        dialog.close();
    });
    dialog.present();
}

/// Open a native file chooser and return the selected file, or `None` when
/// cancelled. This is the pre-4.10 counterpart of `FileDialog::open`.
pub fn open_file<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    title: &str,
    filters: &[gtk::FileFilter],
    default_filter: Option<&gtk::FileFilter>,
    callback: impl FnOnce(Option<gio::File>) + 'static,
) {
    let dialog = gtk::FileChooserNative::new(
        Some(title),
        parent,
        FileChooserAction::Open,
        Some("Open"),
        Some("Cancel"),
    );
    dialog.set_modal(true);
    for filter in filters {
        dialog.add_filter(filter);
    }
    if let Some(filter) = default_filter {
        dialog.set_filter(filter);
    }
    // Unlike a GtkWindow, NativeDialog is not retained as an application
    // toplevel. Keep a strong reference until the response signal fires.
    let keep_alive = dialog.clone();
    dialog.run_async(move |dialog, response| {
        let file = (response == ResponseType::Accept)
            .then(|| dialog.file())
            .flatten();
        dialog.destroy();
        drop(keep_alive);
        callback(file);
    });
}

/// Open a native directory chooser and return the selected folder, or `None`
/// when cancelled. This is the pre-4.10 counterpart of
/// `FileDialog::select_folder`.
pub fn select_folder<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    title: &str,
    callback: impl FnOnce(Option<gio::File>) + 'static,
) {
    let dialog = gtk::FileChooserNative::new(
        Some(title),
        parent,
        FileChooserAction::SelectFolder,
        Some("Select"),
        Some("Cancel"),
    );
    dialog.set_modal(true);
    // Unlike a GtkWindow, NativeDialog is not retained as an application
    // toplevel. Keep a strong reference until the response signal fires.
    let keep_alive = dialog.clone();
    dialog.run_async(move |dialog, response| {
        let folder = (response == ResponseType::Accept)
            .then(|| dialog.file())
            .flatten();
        dialog.destroy();
        drop(keep_alive);
        callback(folder);
    });
}

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
    show_message_with_type(parent, message, detail, MessageType::Info, false);
}

/// Show a modal informational message and run `callback` once it is closed.
pub fn show_message_then<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    message: &str,
    detail: &str,
    callback: impl FnOnce() + 'static,
) {
    let message = crate::i18n::tr(message);
    let detail = crate::i18n::tr(detail);
    show_pretranslated_message_with_type_then(
        parent,
        &message,
        &detail,
        MessageType::Info,
        false,
        callback,
    );
}

/// Show an informational message whose title and detail were already passed
/// through the translation layer. This preserves dynamic values such as
/// emulator names and filesystem paths from brand normalization.
pub fn show_pretranslated_message<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    message: &str,
    detail: &str,
) {
    show_pretranslated_message_with_type(parent, message, detail, MessageType::Info, false);
}

/// Show a modal warning using the GTK 4.0 MessageDialog API.
pub fn show_warning<P: IsA<gtk::Window>>(parent: Option<&P>, message: &str, detail: &str) {
    show_message_with_type(parent, message, detail, MessageType::Warning, false);
}

/// Show a modal error using the GTK 4.0 MessageDialog API.
pub fn show_error<P: IsA<gtk::Window>>(parent: Option<&P>, message: &str, detail: &str) {
    show_message_with_type(parent, message, detail, MessageType::Error, false);
}

/// Show an error whose title and detail were already translated.
pub fn show_pretranslated_error<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    message: &str,
    detail: &str,
) {
    show_pretranslated_message_with_type(parent, message, detail, MessageType::Error, false);
}

fn show_message_with_type<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    message: &str,
    detail: &str,
    message_type: MessageType,
    detail_uses_markup: bool,
) {
    let message = crate::i18n::tr(message);
    let detail = crate::i18n::tr(detail);
    show_pretranslated_message_with_type(
        parent,
        &message,
        &detail,
        message_type,
        detail_uses_markup,
    );
}

fn show_pretranslated_message_with_type<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    message: &str,
    detail: &str,
    message_type: MessageType,
    detail_uses_markup: bool,
) {
    show_pretranslated_message_with_type_then(
        parent,
        message,
        detail,
        message_type,
        detail_uses_markup,
        || {},
    );
}

fn show_pretranslated_message_with_type_then<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    message: &str,
    detail: &str,
    message_type: MessageType,
    detail_uses_markup: bool,
    callback: impl FnOnce() + 'static,
) {
    let detail = if detail_uses_markup {
        // Qt rich text uses HTML line breaks; GtkLabel consumes Pango markup.
        detail.replace("<br>", "\n").replace("<br/>", "\n")
    } else {
        detail.to_owned()
    };
    let dialog = gtk::MessageDialog::builder()
        .modal(true)
        .message_type(message_type)
        .buttons(ButtonsType::Ok)
        .text(message)
        .secondary_text(&detail)
        .secondary_use_markup(detail_uses_markup)
        .build();
    if let Some(parent) = parent {
        dialog.set_transient_for(Some(parent));
    }
    let callback = RefCell::new(Some(callback));
    dialog.connect_response(move |dialog, _| {
        dialog.close();
        if let Some(callback) = callback.borrow_mut().take() {
            callback();
        }
    });
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
    let message = crate::i18n::tr(message);
    let detail = crate::i18n::tr(detail);
    let cancel_label = crate::i18n::tr(cancel_label);
    let accept_label = crate::i18n::tr(accept_label);
    let dialog = gtk::MessageDialog::builder()
        .modal(true)
        .message_type(MessageType::Question)
        .buttons(ButtonsType::None)
        .text(&message)
        .secondary_text(&detail)
        .build();
    if let Some(parent) = parent {
        dialog.set_transient_for(Some(parent));
    }
    dialog.add_button(&cancel_label, ResponseType::Cancel);
    dialog.add_button(&accept_label, ResponseType::Accept);
    dialog.set_default_response(ResponseType::Accept);

    let callback = RefCell::new(Some(callback));
    dialog.connect_response(move |dialog, response| {
        dialog.close();
        if let Some(callback) = callback.borrow_mut().take() {
            callback(response == ResponseType::Accept);
        }
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
    let title = crate::i18n::tr(title);
    let dialog = gtk::FileChooserNative::new(
        Some(&title),
        parent,
        FileChooserAction::Open,
        Some(&crate::i18n::tr("Open")),
        Some(&crate::i18n::tr("Cancel")),
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

/// Open a native save-file chooser and return the selected file, or `None`
/// when cancelled. This is the pre-4.10 counterpart of `FileDialog::save`.
#[cfg(target_os = "windows")]
pub fn save_file<P: IsA<gtk::Window>>(
    parent: Option<&P>,
    title: &str,
    initial_file: &std::path::Path,
    filters: &[gtk::FileFilter],
    default_filter: Option<&gtk::FileFilter>,
    callback: impl FnOnce(Option<gio::File>) + 'static,
) {
    let title = crate::i18n::tr(title);
    let dialog = gtk::FileChooserNative::new(
        Some(&title),
        parent,
        FileChooserAction::Save,
        Some(&crate::i18n::tr("Save")),
        Some(&crate::i18n::tr("Cancel")),
    );
    dialog.set_modal(true);
    for filter in filters {
        dialog.add_filter(filter);
    }
    if let Some(filter) = default_filter {
        dialog.set_filter(filter);
    }
    if let Some(parent) = initial_file.parent() {
        let folder = gio::File::for_path(parent);
        if let Err(error) = dialog.set_current_folder(Some(&folder)) {
            log::warn!("Failed to select initial screenshot directory: {error}");
        }
    }
    if let Some(name) = initial_file.file_name().and_then(|name| name.to_str()) {
        dialog.set_current_name(name);
    }

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
    let title = crate::i18n::tr(title);
    let dialog = gtk::FileChooserNative::new(
        Some(&title),
        parent,
        FileChooserAction::SelectFolder,
        Some(&crate::i18n::tr("Select")),
        Some(&crate::i18n::tr("Cancel")),
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

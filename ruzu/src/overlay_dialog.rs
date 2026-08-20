// SPDX-License-Identifier: GPL-3.0-or-later
//
// GTK counterpart of Eden's `src/yuzu/util/overlay_dialog.{h,cpp}` for the
// non-interactive shutdown overlay used by `MainWindow::OnShutdownBeginDialog`.

use gtk::glib::Propagation;
use gtk::prelude::*;

const BASE_PARENT_WIDTH: i32 = 1280;
const BASE_PARENT_HEIGHT: i32 = 720;
const BASE_PANEL_WIDTH: i32 = 780;
const BASE_PANEL_HEIGHT: i32 = 300;

/// Borderless, window-modal status panel displayed while emulation shuts down.
pub struct OverlayDialog {
    window: gtk::Window,
    close_request_handler: gtk::glib::SignalHandlerId,
}

impl OverlayDialog {
    /// Eden `MainWindow::OnShutdownBeginDialog`:
    /// `OverlayDialog(..., tr("Closing software..."), ..., AlignCenter)`.
    pub fn closing_software(parent: &gtk::ApplicationWindow) -> Self {
        install_css();

        let (width, height) = panel_size(parent.width(), parent.height());
        let label = gtk::Label::new(Some(&crate::i18n::tr("Closing software...")));
        label.set_hexpand(true);
        label.set_vexpand(true);
        label.set_halign(gtk::Align::Center);
        label.set_valign(gtk::Align::Center);
        label.set_justify(gtk::Justification::Center);
        label.set_wrap(true);
        label.add_css_class("ruzu-overlay-dialog-text");

        let panel = gtk::Box::new(gtk::Orientation::Vertical, 0);
        panel.set_hexpand(true);
        panel.set_vexpand(true);
        panel.add_css_class("ruzu-overlay-dialog-panel");
        panel.append(&label);

        let window = gtk::Window::builder()
            .modal(true)
            .transient_for(parent)
            .decorated(false)
            .resizable(false)
            .default_width(width)
            .default_height(height)
            .child(&panel)
            .build();

        // Eden ignores Escape when the overlay has no buttons. Prevent the
        // compositor's close shortcut from dismissing this status-only panel.
        let close_request_handler = window.connect_close_request(|_| Propagation::Stop);
        window.present();

        Self {
            window,
            close_request_handler,
        }
    }

    pub fn close(self) {
        // `Window::close` emits `close-request` too. Eden ignores only the
        // user's Escape/WM request while the status dialog is active; its
        // `deleteLater` from `OnEmulationStopped` must still destroy it.
        self.window.disconnect(self.close_request_handler);
        self.window.close();
    }
}

fn panel_size(parent_width: i32, parent_height: i32) -> (i32, i32) {
    let parent_width = if parent_width > 0 {
        parent_width
    } else {
        BASE_PARENT_WIDTH
    };
    let parent_height = if parent_height > 0 {
        parent_height
    } else {
        BASE_PARENT_HEIGHT
    };

    (
        (parent_width * BASE_PANEL_WIDTH / BASE_PARENT_WIDTH).max(1),
        (parent_height * BASE_PANEL_HEIGHT / BASE_PARENT_HEIGHT).max(1),
    )
}

fn install_css() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let Some(display) = gtk::gdk::Display::default() else {
            return;
        };
        let provider = gtk::CssProvider::new();
        provider.load_from_data(
            ".ruzu-overlay-dialog-panel {\
                 background-color: rgb(240, 240, 240);\
                 border-radius: 6px;\
             }\
             .ruzu-overlay-dialog-text {\
                 color: rgb(44, 44, 44);\
                 font-family: sans-serif;\
                 font-size: 18pt;\
                 font-weight: normal;\
                 padding: 20px 65px;\
             }",
        );
        gtk::style_context_add_provider_for_display(
            &display,
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;
    use std::rc::Rc;

    #[test]
    fn shutdown_panel_uses_edens_regular_overlay_proportions() {
        assert_eq!(panel_size(1280, 720), (780, 300));
        assert_eq!(panel_size(2560, 1440), (1560, 600));
    }

    #[test]
    fn shutdown_panel_falls_back_to_edens_base_geometry_before_map() {
        assert_eq!(panel_size(0, 0), (780, 300));
    }

    #[test]
    fn programmatic_close_bypasses_the_user_close_guard() {
        if gtk::init().is_err() {
            return;
        }
        let window = gtk::Window::new();
        let close_was_blocked = Rc::new(Cell::new(false));
        let close_was_blocked_for_handler = Rc::clone(&close_was_blocked);
        let close_request_handler = window.connect_close_request(move |_| {
            close_was_blocked_for_handler.set(true);
            Propagation::Stop
        });
        OverlayDialog {
            window,
            close_request_handler,
        }
        .close();

        assert!(!close_was_blocked.get());
    }
}

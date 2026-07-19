// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of the upstream Qt frontend entry point in
// `/Users/vricosti/Dev/emulators/zuyu/src/yuzu/main.cpp` (`int main(...)`).
//
// Upstream `main()` constructs a `QApplication`, instantiates the
// `GMainWindow`, shows it, and enters the Qt event loop. Here we construct a
// `gtk::Application`, install the menu bar into the native macOS menu bar on
// `startup`, build the main window on `activate`, and enter the GTK event loop.

use std::cell::RefCell;
use std::rc::Rc;

use gtk::prelude::*;
use gtk::{gio, glib};

mod boot;
mod emu_window;
mod file_menu;
mod config_import;
mod game_list;
mod loading_screen;
mod main_window;
#[cfg(target_os = "macos")]
mod render_window;
mod status_bar;

use main_window::GMainWindow;

/// Application identifier — mirrors upstream's reverse-DNS app id conventions
/// (`org.yuzu_emu.yuzu`), adapted for ruzu.
const APPLICATION_ID: &str = "org.ruzu_emu.ruzu";

thread_local! {
    /// Keeps the current main window alive for the process lifetime.
    static MAIN_WINDOW: RefCell<Option<Rc<GMainWindow>>> = const { RefCell::new(None) };
}

/// Store the main window, dropping any previous one.
fn set_main_window(window: Rc<GMainWindow>) {
    MAIN_WINDOW.with(|slot| *slot.borrow_mut() = Some(window));
}

fn main() -> glib::ExitCode {
    env_logger::init();

    // Migrate an existing yuzu configuration on first run (guarded by a marker
    // so it happens exactly once, not on every startup).
    config_import::import_yuzu_config_once();

    // Upstream constructs `QApplication app(argc, argv)`. We register handling
    // of file arguments ourselves later (open a game passed on the command
    // line), so declare HANDLES_OPEN even though the handler is not wired yet.
    let app = gtk::Application::builder()
        .application_id(APPLICATION_ID)
        .flags(gio::ApplicationFlags::HANDLES_OPEN)
        .build();

    // `startup` fires exactly once, before the first `activate`/`open`. This is
    // where the application-scoped menu bar and actions are installed. On the
    // macOS (quartz) GDK backend, the menu model set here is bridged into the
    // native global menu bar at the top of the screen.
    app.connect_startup(|app| {
        // Use a dark theme, matching yuzu.
        if let Some(settings) = gtk::Settings::default() {
            settings.set_gtk_application_prefer_dark_theme(true);
        }
        main_window::init_app_menu(app);
    });

    // Upstream: `GMainWindow main_window{...}; main_window.show();`
    // The `Rc<GMainWindow>` must outlive the closure — GTK keeps the widget
    // tree, but our wrapper owns the session, loading screen, and the `Weak`
    // captured by the menu actions. Keep it in a thread-local.
    app.connect_activate(|app| {
        let window = GMainWindow::new(app);
        window.present();
        set_main_window(window);
    });

    // With HANDLES_OPEN set, GTK routes file arguments to `open` instead of
    // `activate`. Boot the first file directly (like `yuzu <game>`); the window
    // defers the boot until its render surface is realized.
    app.connect_open(|app, files, _hint| {
        let window = GMainWindow::new(app);
        window.present();
        if let Some(path) = files.first().and_then(|f| f.path()) {
            window.boot_game(path.to_string_lossy().into_owned());
        }
        set_main_window(window);
    });

    // Upstream: `return app.exec();`
    app.run()
}

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
mod config_import;
mod configuration;
mod emu_window;
mod file_menu;
mod game_list;
mod gtk_compat;
mod loading_screen;
mod main_window;
#[cfg(target_os = "macos")]
mod render_window;
#[cfg(target_os = "windows")]
mod render_window_windows;
#[cfg(target_os = "linux")]
mod render_window_x11;
mod status_bar;
mod uisettings;
mod util;

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

/// Return the process-wide main window, if it has already been created.
///
/// `gio::Application` forwards later launches to the existing process and
/// emits `activate` or `open` again. Reusing the window preserves upstream's
/// single `GMainWindow` lifetime and, critically, its single input subsystem
/// and emulation `System`.
fn main_window() -> Option<Rc<GMainWindow>> {
    MAIN_WINDOW.with(|slot| slot.borrow().as_ref().cloned())
}

#[cfg(target_os = "linux")]
fn linux_gdk_backend_override(
    current_backend: Option<&str>,
    x11_display_available: bool,
) -> Option<&'static str> {
    if x11_display_available && current_backend != Some("x11") {
        Some("x11")
    } else {
        None
    }
}

/// GTK4 cannot expose a native Wayland child surface for the embedded renderer.
/// Until that path is ported, use XWayland whenever an X11 display is
/// available so every launcher boot path can create the same child window.
#[cfg(target_os = "linux")]
fn configure_linux_gdk_backend() -> bool {
    let current_backend = std::env::var("GDK_BACKEND").ok();
    let x11_display_available = std::env::var_os("DISPLAY").is_some();
    let Some(backend) =
        linux_gdk_backend_override(current_backend.as_deref(), x11_display_available)
    else {
        return false;
    };
    std::env::set_var("GDK_BACKEND", backend);
    true
}

fn main() -> glib::ExitCode {
    #[cfg(target_os = "linux")]
    let forced_x11 = configure_linux_gdk_backend();

    env_logger::init();

    #[cfg(target_os = "linux")]
    if forced_x11 {
        log::info!("Using the X11 GDK backend for the embedded Linux render surface");
    }

    // A yuzu configuration is *offered* for import on first run, not copied
    // silently — the prompt is raised from `GMainWindow` once the UI is up (see
    // `main_window::maybe_offer_yuzu_import`).

    // Load the configured game directories out of ruzu's own config, the way
    // upstream's `Config::ReadUIValues` fills `UISettings::values.game_dirs`
    // before the game list is built.
    let game_dirs = configuration::qt_config::load_game_dirs();
    log::info!("Loaded {} configured game directory(ies)", game_dirs.len());
    uisettings::with_mut(|v| v.game_dirs = game_dirs);

    // Upstream's `Config` constructor reads every category, controls included,
    // before the window is built. Without this the Controls page would open on
    // an empty mapping even though one was saved last session.
    configuration::qt_config::load_control_values();
    configuration::qt_config::load_system_values();

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
        // Upstream calls `UpdateUITheme()` early in the `GMainWindow`
        // constructor. It follows the desktop's dark-mode preference for the
        // system themes rather than forcing one, which is why yuzu renders
        // light on a light Linux desktop and dark on a dark macOS one.
        main_window::update_ui_theme();
        main_window::init_app_menu(app);
    });

    // Upstream: `GMainWindow main_window{...}; main_window.show();`
    // The `Rc<GMainWindow>` must outlive the closure — GTK keeps the widget
    // tree, but our wrapper owns the session, loading screen, and the `Weak`
    // captured by the menu actions. Keep it in a thread-local.
    app.connect_activate(|app| {
        if let Some(window) = main_window() {
            window.present();
            return;
        }

        let window = GMainWindow::new(app);
        window.present();
        set_main_window(window);
    });

    // With HANDLES_OPEN set, GTK routes file arguments to `open` instead of
    // `activate`. Boot the first file directly (like `yuzu <game>`); the window
    // defers the boot until its render surface is realized.
    app.connect_open(|app, files, _hint| {
        let existing_window = main_window();
        let window = existing_window
            .clone()
            .unwrap_or_else(|| GMainWindow::new_for_direct_game(app));
        window.present();
        if let Some(path) = files.first().and_then(|f| f.path()) {
            window.boot_game(path.to_string_lossy().into_owned());
        }
        if existing_window.is_none() {
            set_main_window(window);
        }
    });

    app.run()
}

#[cfg(all(test, target_os = "linux"))]
mod tests {
    use super::*;

    #[test]
    fn linux_launcher_uses_x11_when_xwayland_is_available() {
        assert_eq!(
            linux_gdk_backend_override(Some("wayland"), true),
            Some("x11")
        );
        assert_eq!(linux_gdk_backend_override(None, true), Some("x11"));
        assert_eq!(linux_gdk_backend_override(Some("x11"), true), None);
    }

    #[test]
    fn linux_launcher_keeps_wayland_without_an_x11_display() {
        assert_eq!(linux_gdk_backend_override(Some("wayland"), false), None);
    }
}

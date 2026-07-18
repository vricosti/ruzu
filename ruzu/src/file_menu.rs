// SPDX-License-Identifier: GPL-3.0-or-later
//
// Application-scoped File-menu handlers that do not depend on the window's
// render surface. Counterparts of upstream `main.cpp`:
//   * `OnOpenYuzuFolder`   (action_Open_yuzu_Folder)
//   * `QMainWindow::close` (action_Exit)
//
// The window-dependent File actions (Load File / Load Folder → in-process boot)
// live on `GMainWindow` (see `main_window.rs`), since they need the render
// surface, loading screen, and stack. The remaining stubs (Install Files to
// NAND, Load/Remove Amiibo, Recent Files) keep their logging placeholders.

use gtk::prelude::*;
use gtk::{gio, glib, Application};

use common::fs::path_util::{get_ruzu_path, RuzuPath};

/// Register the application-scoped File actions (open ruzu folder, exit).
/// Called from `init_app_menu`.
pub fn register(app: &Application) {
    // action_Open_yuzu_Folder → OnOpenYuzuFolder
    let open_folder = gio::SimpleAction::new("open_ruzu_folder", None);
    open_folder.connect_activate(|_, _| on_open_ruzu_folder());
    app.add_action(&open_folder);

    // action_Exit → QMainWindow::close
    let exit = gio::SimpleAction::new("exit", None);
    exit.connect_activate(glib::clone!(
        #[weak]
        app,
        move |_, _| {
            if let Some(window) = app.active_window() {
                window.close();
            }
        }
    ));
    app.add_action(&exit);
}

/// Upstream `OnOpenYuzuFolder`: reveal the ruzu data directory in the file
/// manager (Finder on macOS).
fn on_open_ruzu_folder() {
    let dir = get_ruzu_path(RuzuPath::RuzuDir);
    let file = gio::File::for_path(&dir);
    let launcher = gtk::FileLauncher::new(Some(&file));
    launcher.launch(gtk::Window::NONE, gio::Cancellable::NONE, move |result| {
        if let Err(err) = result {
            log::error!("Failed to open ruzu folder: {err}");
        }
    });
}

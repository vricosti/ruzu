// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_debug_tab.cpp`
// (`ConfigureDebugTab`), whose widget tree lives in `configure_debug_tab.ui`.
//
// Upstream `ConfigureDebugTab` is itself a `QTabWidget` nested inside the
// dialog's outer tab widget, holding two pages: "Debug" (`ConfigureDebug`) and
// "CPU" (`ConfigureCpuDebug`). That nesting is why the Debug screen shows two
// rows of tabs.
//
// `ConfigureDialog` resets this inner widget to page 0 whenever the outer tab
// changes (`debug_tab_tab->SetCurrentIndex(0)`).

use gtk::prelude::*;

use super::configure_cpu_debug;
use super::configure_debug;
use super::configure_dialog::Page;

/// Build the Debug tab — upstream `ConfigureDebugTab`.
pub fn page() -> Page {
    let notebook = gtk::Notebook::new();
    notebook.set_hexpand(true);
    notebook.set_vexpand(true);

    let debug = configure_debug::page();
    let cpu_debug = configure_cpu_debug::page();

    notebook.append_page(&debug.widget, Some(&gtk::Label::new(Some(&debug.title))));
    notebook.append_page(
        &cpu_debug.widget,
        Some(&gtk::Label::new(Some(&cpu_debug.title))),
    );

    Page::new("Debug", notebook, move || {
        // Upstream forwards `ApplyConfiguration` to both inner pages.
        (debug.apply)();
        (cpu_debug.apply)();
    })
}

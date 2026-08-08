// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_dialog.cpp`
// (`ConfigureDialog`), whose widget tree lives in `configure.ui`.
//
// Upstream layout:
//   * a `QListWidget` (`selectorList`) on the left with six rows;
//   * a `QTabWidget` (`tabWidget`) on the right whose tabs are *rebuilt* every
//     time the selection changes (`UpdateVisibleTabs` clears it and re-adds only
//     the tabs belonging to the selected row);
//   * a status label ("Some settings are only available when a game is not
//     running.") and a `QDialogButtonBox` with Cancel / OK along the bottom.
//
// The row → tabs mapping is upstream `PopulateSelectionList`:
//   General  → General, Hotkeys, UI, Web, Debug
//   System   → System, Profiles, Network, Filesystem, Applets
//   CPU      → CPU
//   Graphics → Graphics, Advanced
//   Audio    → Audio
//   Controls → Player 1..8, Advanced
//
// Note that upstream's tab *titles* come from each page's `accessibleName()`,
// which is why the "UI" page shows as "UI" and the graphics advanced page shows
// as "Advanced" rather than their class names.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use gtk::prelude::*;
use gtk::{glib, Window};

use super::{
    configure_applets, configure_audio, configure_cpu, configure_debug_tab, configure_filesystem,
    configure_general, configure_graphics, configure_graphics_advanced, configure_hotkeys,
    configure_input, configure_network, configure_profile_manager, configure_system, configure_ui,
    configure_web,
};

/// Default dialog geometry. Upstream calls `adjustSize()` and lets Qt derive
/// the size from `configure.ui`'s base plus the largest page — the Controls
/// page, whose binding grid is the widest and tallest thing in the dialog.
/// These figures are that resolved size, measured from the Qt dialog.
const DEFAULT_WIDTH: i32 = 1290;
const DEFAULT_HEIGHT: i32 = 850;

/// Fixed width of the left selector column, matching `configure.ui`'s
/// `selectorList` `maximumSize` of 120px.
const SELECTOR_WIDTH: i32 = 120;

/// A configuration page: the tab title plus its content widget.
///
/// Upstream stores the pages as `QWidget*` and reads the title back from
/// `accessibleName()`; carrying the title alongside the widget is the same
/// information without the Qt property round-trip.
pub struct Page {
    pub title: String,
    pub widget: gtk::Widget,
    /// Applies this page's widget state back into the settings — upstream
    /// `ApplyConfiguration()` on each tab.
    pub apply: Box<dyn Fn()>,
}

impl Page {
    pub fn new(title: &str, widget: impl IsA<gtk::Widget>, apply: impl Fn() + 'static) -> Self {
        Self {
            title: title.to_string(),
            widget: widget.upcast(),
            apply: Box::new(apply),
        }
    }
}

/// One row of the left selector list, with the pages it reveals.
struct Section {
    name: &'static str,
    pages: Vec<Page>,
}

/// The configuration dialog — upstream `ConfigureDialog`.
pub struct ConfigureDialog {
    window: Window,
    notebook: gtk::Notebook,
    sections: Rc<Vec<Section>>,
    /// Index of the section currently shown in the notebook, so a re-selection
    /// of the same row doesn't rebuild the tabs (which would reset the tab
    /// position, unlike upstream's `QSignalBlocker`-guarded rebuild).
    shown: RefCell<Option<usize>>,
}

impl ConfigureDialog {
    /// Build the dialog. Mirrors the upstream constructor: create every tab,
    /// populate the selector list, then select row 0.
    pub fn new(
        parent: Option<&impl IsA<Window>>,
        input_subsystem: Rc<RefCell<input_common::InputSubsystem>>,
        hid_core: Arc<parking_lot::Mutex<hid_core::hid_core::HIDCore>>,
    ) -> Rc<Self> {
        let window = Window::builder()
            .title("ruzu Configuration")
            .modal(true)
            .default_width(DEFAULT_WIDTH)
            .default_height(DEFAULT_HEIGHT)
            .build();
        // Divergence from upstream, forced by the platform: upstream passes the
        // main window as the `QDialog` parent. Setting `transient_for` here makes
        // GTK advertise the surface as `_NET_WM_WINDOW_TYPE_DIALOG`, and window
        // managers drop `_NET_WM_ACTION_MAXIMIZE_*` for dialogs — the maximize
        // button in the titlebar is drawn but does nothing. The window stays
        // modal, which is the behaviour `QDialog::exec` gives upstream; only the
        // transient hint is dropped, so the dialog can be maximized like the
        // main window.
        let _ = &parent;

        // Upstream constructs Advanced Graphics first and gives Graphics a
        // callback to `ExposeComputeOption` when a Vulkan device requires it.
        let advanced_graphics = configure_graphics_advanced::page();
        let graphics = configure_graphics::page(advanced_graphics.expose_compute_option);

        // Upstream `PopulateSelectionList`'s six rows, in order.
        let sections = vec![
            Section {
                name: "General",
                pages: vec![
                    configure_general::page(),
                    configure_hotkeys::page(),
                    configure_ui::page(),
                    configure_web::page(),
                    configure_debug_tab::page(),
                ],
            },
            Section {
                name: "System",
                pages: vec![
                    configure_system::page(),
                    configure_profile_manager::page(),
                    configure_network::page(),
                    configure_filesystem::page(),
                    configure_applets::page(),
                ],
            },
            Section {
                name: "CPU",
                pages: vec![configure_cpu::page()],
            },
            Section {
                name: "Graphics",
                pages: vec![graphics, advanced_graphics.page],
            },
            Section {
                name: "Audio",
                pages: vec![configure_audio::page()],
            },
            Section {
                name: "Controls",
                pages: configure_input::pages(input_subsystem, hid_core),
            },
        ];

        // --- Left selector list (upstream `selectorList`) --------------------
        let selector = gtk::ListBox::new();
        selector.set_selection_mode(gtk::SelectionMode::Single);
        selector.set_width_request(SELECTOR_WIDTH);
        for section in &sections {
            let label = gtk::Label::new(Some(section.name));
            label.set_xalign(0.0);
            label.set_margin_top(2);
            label.set_margin_bottom(2);
            label.set_margin_start(4);
            selector.append(&label);
        }

        let selector_scroll = gtk::ScrolledWindow::builder()
            .hscrollbar_policy(gtk::PolicyType::Never)
            .width_request(SELECTOR_WIDTH)
            .child(&selector)
            .build();

        // --- Right tab widget (upstream `tabWidget`) -------------------------
        let notebook = gtk::Notebook::new();
        notebook.set_hexpand(true);
        notebook.set_vexpand(true);
        notebook.set_scrollable(true);

        let split = gtk::Box::new(gtk::Orientation::Horizontal, 6);
        split.set_margin_top(10);
        split.set_margin_start(10);
        split.set_margin_end(10);
        split.append(&selector_scroll);
        split.append(&notebook);

        // --- Bottom bar (upstream status label + `buttonBox`) ----------------
        let status = gtk::Label::new(Some(
            "Some settings are only available when a game is not running.",
        ));
        status.set_xalign(0.0);
        status.set_hexpand(true);

        let cancel = gtk::Button::with_label("Cancel");
        let ok = gtk::Button::with_label("OK");

        let buttons = gtk::Box::new(gtk::Orientation::Horizontal, 6);
        buttons.set_margin_top(10);
        buttons.set_margin_bottom(10);
        buttons.set_margin_start(10);
        buttons.set_margin_end(10);
        buttons.append(&status);
        buttons.append(&cancel);
        buttons.append(&ok);

        let root = gtk::Box::new(gtk::Orientation::Vertical, 0);
        root.append(&split);
        root.append(&buttons);
        window.set_child(Some(&root));

        let this = Rc::new(Self {
            window,
            notebook,
            sections: Rc::new(sections),
            shown: RefCell::new(None),
        });

        // Upstream connects `itemSelectionChanged` to `UpdateVisibleTabs`.
        selector.connect_row_selected(glib::clone!(
            #[weak(rename_to = dialog)]
            this,
            move |_, row| {
                if let Some(row) = row {
                    dialog.update_visible_tabs(row.index() as usize);
                }
            }
        ));

        // Cancel discards; OK applies then closes — upstream wires the
        // `QDialogButtonBox`'s `rejected` / `accepted` the same way.
        cancel.connect_clicked(glib::clone!(
            #[weak(rename_to = dialog)]
            this,
            move |_| dialog.window.close()
        ));
        ok.connect_clicked(glib::clone!(
            #[weak(rename_to = dialog)]
            this,
            move |_| {
                dialog.apply_configuration();
                dialog.window.close();
            }
        ));

        // Upstream: `ui->selectorList->setCurrentRow(0);`
        if let Some(first) = selector.row_at_index(0) {
            selector.select_row(Some(&first));
        }

        this
    }

    /// Rebuild the notebook so it holds exactly the selected section's pages —
    /// upstream `UpdateVisibleTabs`.
    fn update_visible_tabs(&self, section_index: usize) {
        if *self.shown.borrow() == Some(section_index) {
            return;
        }
        let Some(section) = self.sections.get(section_index) else {
            return;
        };

        while self.notebook.n_pages() > 0 {
            self.notebook.remove_page(Some(0));
        }
        for page in &section.pages {
            log::debug!("configure: showing tab {}", page.title);
            self.notebook
                .append_page(&page.widget, Some(&gtk::Label::new(Some(&page.title))));
        }
        *self.shown.borrow_mut() = Some(section_index);
    }

    /// Push every page's widget state back into the settings — upstream
    /// `ConfigureDialog::ApplyConfiguration`, which calls `ApplyConfiguration()`
    /// on each tab regardless of which one is currently visible.
    fn apply_configuration(&self) {
        for section in self.sections.iter() {
            for page in &section.pages {
                (page.apply)();
            }
        }
        // Upstream `GMainWindow::OnConfigure` calls `config->Save()` once the
        // dialog is accepted; without it the new bindings would live only in
        // this process and be gone next launch.
        if let Err(error) = super::qt_config::save_control_values() {
            log::error!("Failed to save control settings: {error}");
        }
        if let Err(error) = super::qt_config::save_ui_language() {
            log::error!("Failed to save interface language: {error}");
        }
        common::settings::log_settings(&common::settings::values());
    }

    /// Show the dialog — upstream `ConfigureDialog::exec()`.
    pub fn present(&self) {
        crate::i18n::translate_widget_tree(&self.window);
        self.window.present();
    }

    /// Notify the owner once the GTK window closes so its `Rc` can be dropped,
    /// matching upstream's stack-owned dialog lifetime.
    pub fn connect_closed(&self, callback: impl Fn() + 'static) {
        self.window.connect_close_request(move |_| {
            callback();
            glib::Propagation::Proceed
        });
    }
}

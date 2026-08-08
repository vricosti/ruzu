// SPDX-License-Identifier: GPL-3.0-or-later
//
// Game list view — counterpart of upstream `GameList` / `GameListWorker`
// (`/home/vricosti/Dev/emulators/zuyu/src/yuzu/game_list*.cpp`). It reads the
// configured game directories from ruzu's own config, scans them for Switch
// executables, and shows them grouped under one expandable row per directory.
// Activating a game row (double-click / Enter) boots it.
//
// Divergence from upstream, deliberate: yuzu exposes "add a game directory" as
// a fake row appended *inside* the tree, which reads as an item belonging to
// the scanned folder. Here that action lives in a toolbar above the list, so
// the tree contains only real directories and real games. Directory and game
// actions otherwise live in per-row context menus, matching
// `GameList::PopupContextMenu`.

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::{Rc, Weak};
use std::sync::{Arc, Mutex};

use gtk::prelude::*;
use gtk::subclass::prelude::*;
use gtk::{gdk, gio, glib};

use ruzu_core::file_sys::control_metadata::NACP;
use ruzu_core::file_sys::fs_filesystem::OpenMode;
use ruzu_core::file_sys::registered_cache::ContentProviderUnion;
use ruzu_core::file_sys::vfs::vfs_real::RealVfsFilesystem;
use ruzu_core::hle::service::filesystem::filesystem::FileSystemController;
use ruzu_core::loader::loader::{get_loader, FileType, ResultStatus, System as LoaderSystem};

use crate::configuration::qt_config;
use crate::main_window::StartGameType;
use crate::uisettings::{self, GameDir};
use crate::util::controller_navigation::{ControllerNavigation, NavigationKey};

/// Pixel size of the game icon shown in the list.
const ICON_SIZE: i32 = 64;

/// Pixel size of the folder icon on a directory row.
const FOLDER_ICON_SIZE: i32 = 48;

/// Ruzu-specific default requested for newly added filesystem directories.
const NEW_DIRECTORY_DEEP_SCAN: bool = true;

/// Switch executable extensions listed in the game view. Mirrors
/// `GameList::supported_file_extensions`.
const SUPPORTED_EXTENSIONS: &[&str] = &["nsp", "xci", "nca", "nro", "nso", "kip"];

// ---------------------------------------------------------------------------
// GameEntry — a GObject row model for the ColumnView.
// ---------------------------------------------------------------------------
mod imp {
    use std::cell::{Cell, RefCell};

    use gtk::glib;
    use gtk::subclass::prelude::*;

    #[derive(Default)]
    pub struct GameEntry {
        pub name: RefCell<String>,
        pub developer: RefCell<String>,
        pub version: RefCell<String>,
        pub kind: RefCell<String>,
        pub size: RefCell<String>,
        pub path: RefCell<String>,
        pub icon: RefCell<Option<gtk::gdk::Texture>>,
        /// Application program id. Zero for homebrew without a title id.
        pub program_id: Cell<u64>,
        /// Directory rows group the games found beneath them.
        pub is_folder: Cell<bool>,
        /// Whether this directory is scanned recursively (directory rows only).
        pub deep_scan: Cell<bool>,
        /// Child rows, for directory rows.
        pub children: RefCell<Option<gtk::gio::ListStore>>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for GameEntry {
        const NAME: &'static str = "RuzuGameEntry";
        type Type = super::GameEntry;
    }

    impl ObjectImpl for GameEntry {}
}

glib::wrapper! {
    pub struct GameEntry(ObjectSubclass<imp::GameEntry>);
}

impl GameEntry {
    /// A game row.
    fn new_game(
        name: &str,
        developer: &str,
        version: &str,
        kind: &str,
        size: &str,
        path: &str,
        icon: Option<gdk::Texture>,
        program_id: u64,
    ) -> Self {
        let obj: Self = glib::Object::new();
        let imp = obj.imp();
        *imp.name.borrow_mut() = name.to_owned();
        *imp.developer.borrow_mut() = developer.to_owned();
        *imp.version.borrow_mut() = version.to_owned();
        *imp.kind.borrow_mut() = kind.to_owned();
        *imp.size.borrow_mut() = size.to_owned();
        *imp.path.borrow_mut() = path.to_owned();
        *imp.icon.borrow_mut() = icon;
        imp.program_id.set(program_id);
        imp.is_folder.set(false);
        obj
    }

    /// A directory row, holding the games found under it.
    fn new_folder(path: &str, deep_scan: bool, children: gio::ListStore) -> Self {
        let obj: Self = glib::Object::new();
        let imp = obj.imp();
        *imp.name.borrow_mut() = path.to_owned();
        *imp.path.borrow_mut() = path.to_owned();
        imp.is_folder.set(true);
        imp.deep_scan.set(deep_scan);
        *imp.children.borrow_mut() = Some(children);
        obj
    }

    fn name(&self) -> String {
        self.imp().name.borrow().clone()
    }
    fn developer(&self) -> String {
        self.imp().developer.borrow().clone()
    }
    fn version(&self) -> String {
        self.imp().version.borrow().clone()
    }
    fn kind(&self) -> String {
        self.imp().kind.borrow().clone()
    }
    fn size(&self) -> String {
        self.imp().size.borrow().clone()
    }
    fn path(&self) -> String {
        self.imp().path.borrow().clone()
    }
    fn icon(&self) -> Option<gdk::Texture> {
        self.imp().icon.borrow().clone()
    }
    fn program_id(&self) -> u64 {
        self.imp().program_id.get()
    }
    fn is_folder(&self) -> bool {
        self.imp().is_folder.get()
    }
    fn deep_scan(&self) -> bool {
        self.imp().deep_scan.get()
    }
    fn children(&self) -> Option<gio::ListStore> {
        self.imp().children.borrow().clone()
    }
}

/// The game list: a toolbar over either the tree or an empty-state placeholder.
///
/// Kept as a struct so the toolbar actions can rebuild the tree in place, the
/// way upstream re-runs `GameListWorker` after the directory list changes.
struct GameListView {
    root: gtk::Box,
    stack: gtk::Stack,
    filter_bar: gtk::Box,
    filter_entry: gtk::SearchEntry,
    filter_result: gtk::Label,
    column_view: gtk::ColumnView,
    store: gio::ListStore,
    /// Unfiltered children for every directory in `store`, in matching order.
    /// Upstream hides rows in its item model; GTK's tree list has no row-hidden
    /// API, so the visible child stores are rebuilt from this retained source.
    all_games: RefCell<Vec<Vec<GameEntry>>>,
    /// Kept so a rescan can restore the selected directory: rebuilding the
    /// store clears the selection, which would otherwise disable the
    /// per-directory toolbar actions after every single use of them.
    selection: gtk::SingleSelection,
    controller_navigation: ControllerNavigation,
    hid_core: Arc<parking_lot::Mutex<hid_core::hid_core::HIDCore>>,
    on_activate: Rc<dyn Fn(String, StartGameType)>,
    property_dialog:
        RefCell<Option<Rc<crate::configuration::configure_per_game::ConfigurePerGame>>>,
}

type ContextMenuHandler = Rc<dyn Fn(GameEntry, gtk::Widget, u32, f64, f64)>;

/// Stack page names.
const PAGE_LIST: &str = "list";
const PAGE_EMPTY: &str = "empty";

/// Handle to a built game list, letting the owner rescan it after the
/// configured directories change (e.g. once a yuzu config has been imported).
///
/// Holding it also keeps the view alive for the widget's lifetime.
#[derive(Clone)]
pub struct GameListHandle(Rc<GameListView>);

impl GameListHandle {
    /// Re-read the configured directories and rebuild the list.
    pub fn reload(&self) {
        self.0.reload();
    }

    /// Give keyboard navigation back to the list after returning from a game.
    pub fn focus(&self) {
        self.0.column_view.grab_focus();
    }

    /// Upstream `GameList::SetFilterVisible`, `SetFilterFocus`, and
    /// `ClearFilter` as driven by `GMainWindow::OnToggleFilterBar`.
    pub fn set_filter_visible(&self, visible: bool) {
        self.0.set_filter_visible(visible);
    }
}

/// Build the game list widget. `on_activate` is invoked with the game's path
/// when a game row is activated (double-click / Enter).
pub fn build<F: Fn(String, StartGameType) + 'static>(
    hid_core: &Arc<parking_lot::Mutex<hid_core::hid_core::HIDCore>>,
    on_activate: F,
) -> (gtk::Widget, GameListHandle) {
    install_list_css();

    let store = gio::ListStore::new::<GameEntry>();

    // --- Tree ------------------------------------------------------------
    // One expandable row per configured directory; its games are the children.
    let tree = gtk::TreeListModel::new(store.clone(), false, true, |item| {
        item.downcast_ref::<GameEntry>()
            .and_then(GameEntry::children)
            .map(Cast::upcast)
    });

    let selection = gtk::SingleSelection::new(Some(tree));
    // Upstream opens the game list with nothing selected; GTK's default is to
    // auto-select the first row, which would highlight a game the user never
    // picked.
    selection.set_autoselect(false);
    selection.set_can_unselect(true);
    selection.set_selected(gtk::INVALID_LIST_POSITION);

    let column_view = gtk::ColumnView::new(Some(selection.clone()));
    column_view.add_css_class("data-table");
    column_view.add_css_class("ruzu-game-list");
    // The banding comes from the CSS below, not from GTK's separators.
    column_view.set_show_row_separators(false);
    column_view.set_show_column_separators(false);
    column_view.connect_map(|view| {
        view.grab_focus();
    });

    let on_activate: Rc<dyn Fn(String, StartGameType)> = Rc::new(on_activate);
    let context_view: Rc<RefCell<Weak<GameListView>>> = Rc::new(RefCell::new(Weak::new()));
    let on_context_menu: ContextMenuHandler = {
        let context_view = Rc::clone(&context_view);
        let on_activate = Rc::clone(&on_activate);
        Rc::new(move |entry, anchor, position, x, y| {
            let Some(view) = context_view.borrow().upgrade() else {
                return;
            };
            view.selection.set_selected(position);
            view.popup_context_menu(&entry, &anchor, x, y, Rc::clone(&on_activate));
        })
    };

    column_view.append_column(&make_name_column(Rc::clone(&on_context_menu)));
    column_view.append_column(&make_text_column(
        "File type",
        GameEntry::kind,
        Rc::clone(&on_context_menu),
    ));
    column_view.append_column(&make_text_column("Size", GameEntry::size, on_context_menu));

    let scroller = gtk::ScrolledWindow::builder()
        .hexpand(true)
        .vexpand(true)
        .child(&column_view)
        .build();

    // --- Empty state ------------------------------------------------------
    let empty = build_empty_state();

    let stack = gtk::Stack::new();
    stack.add_named(&scroller, Some(PAGE_LIST));
    stack.add_named(&empty.root, Some(PAGE_EMPTY));

    // --- Toolbar ----------------------------------------------------------
    let toolbar = gtk::Box::new(gtk::Orientation::Horizontal, 6);
    // `ruzu-toolbar` draws the separating rule; without it the strip blends
    // into the list and reintroduces exactly the ambiguity this layout is
    // meant to remove.
    toolbar.add_css_class("ruzu-toolbar");
    toolbar.set_margin_top(4);
    toolbar.set_margin_bottom(4);
    toolbar.set_margin_start(6);
    toolbar.set_margin_end(6);

    // `Button::builder().label(..).icon_name(..)` is not additive — setting
    // `icon_name` replaces the label child — so build the icon+label row
    // explicitly.
    let add_button = icon_label_button("list-add-symbolic", "Add Game Directory");
    let refresh_button = gtk::Button::builder()
        .icon_name("view-refresh-symbolic")
        .tooltip_text("Rescan game directories")
        .build();
    let spacer = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    spacer.set_hexpand(true);

    toolbar.append(&add_button);
    toolbar.append(&refresh_button);
    toolbar.append(&spacer);

    let filter_bar = gtk::Box::new(gtk::Orientation::Horizontal, 10);
    filter_bar.set_margin_top(8);
    filter_bar.set_margin_bottom(8);
    filter_bar.set_margin_start(8);
    filter_bar.set_margin_end(8);
    let filter_label = gtk::Label::new(Some(&crate::i18n::tr("Filter:")));
    let filter_entry = gtk::SearchEntry::new();
    filter_entry.set_placeholder_text(Some(&crate::i18n::tr("Enter pattern to filter")));
    filter_entry.set_hexpand(true);
    let filter_result = gtk::Label::new(None);
    let filter_close = gtk::Button::builder()
        .icon_name("window-close-symbolic")
        .tooltip_text(crate::i18n::tr("Close"))
        .build();
    filter_bar.append(&filter_label);
    filter_bar.append(&filter_entry);
    filter_bar.append(&filter_result);
    filter_bar.append(&filter_close);

    let root = gtk::Box::new(gtk::Orientation::Vertical, 0);
    root.append(&toolbar);
    root.append(&stack);
    root.append(&filter_bar);

    let view = Rc::new(GameListView {
        root: root.clone(),
        stack,
        filter_bar,
        filter_entry: filter_entry.clone(),
        filter_result,
        column_view: column_view.clone(),
        store,
        all_games: RefCell::new(Vec::new()),
        selection: selection.clone(),
        controller_navigation: ControllerNavigation::new(hid_core),
        hid_core: Arc::clone(hid_core),
        on_activate,
        property_dialog: RefCell::new(None),
    });
    *context_view.borrow_mut() = Rc::downgrade(&view);

    // Activate (double-click / Enter) → boot a game; on a directory row, toggle
    // it open instead, which is what a tree row activation should do.
    column_view.connect_activate({
        let view = Rc::downgrade(&view);
        move |_, position| {
            if let Some(view) = view.upgrade() {
                view.activate_position(position);
            }
        }
    });

    let keys = gtk::EventControllerKey::new();
    keys.set_propagation_phase(gtk::PropagationPhase::Capture);
    keys.connect_key_pressed({
        let view = Rc::downgrade(&view);
        move |_, keyval, _, _| {
            let Some(key) = navigation_key_for_gdk(keyval) else {
                return glib::Propagation::Proceed;
            };
            if view
                .upgrade()
                .is_some_and(|view| view.handle_navigation(key))
            {
                glib::Propagation::Stop
            } else {
                glib::Propagation::Proceed
            }
        }
    });
    column_view.add_controller(keys);

    // HID callbacks can run outside GTK's main context. Drain their actions on
    // the UI thread and discard presses while the game list is not active,
    // matching upstream's `IsPoweredOn` / `isActiveWindow` guards.
    glib::timeout_add_local(std::time::Duration::from_millis(1), {
        let view = Rc::downgrade(&view);
        move || {
            let Some(view) = view.upgrade() else {
                return glib::ControlFlow::Break;
            };
            let list_is_active = view.root.is_mapped()
                && view
                    .parent_window()
                    .is_some_and(|window| window.is_active());
            if list_is_active {
                for key in view.controller_navigation.take_pending_keys() {
                    view.handle_navigation(key);
                }
            } else {
                view.controller_navigation.discard_pending_keys();
            }
            glib::ControlFlow::Continue
        }
    });

    // Toolbar + empty-state actions.
    for button in [&add_button, &empty.add_button] {
        let view = Rc::clone(&view);
        button.connect_clicked(move |_| view.prompt_add_directory());
    }
    {
        let view = Rc::clone(&view);
        refresh_button.connect_clicked(move |_| view.reload());
    }
    filter_entry.connect_search_changed({
        let view = Rc::downgrade(&view);
        move |entry| {
            if let Some(view) = view.upgrade() {
                view.apply_filter(&entry.text());
            }
        }
    });
    filter_close.connect_clicked({
        let view = Rc::downgrade(&view);
        move |_| {
            let Some(view) = view.upgrade() else { return };
            view.set_filter_visible(false);
            uisettings::with_mut(|values| values.show_filter_bar.set_value(false));
            if let Some(action) = gio::Application::default()
                .and_downcast::<gtk::Application>()
                .and_then(|app| app.lookup_action("show_filter_bar"))
                .and_downcast::<gio::SimpleAction>()
            {
                action.set_state(&false.to_variant());
            }
            if let Err(error) = qt_config::save_view_values() {
                log::error!("Failed to save View menu settings: {error}");
            }
        }
    });

    // Populate after all row actions are connected.
    view.reload();
    view.set_filter_visible(uisettings::with(|values| {
        *values.show_filter_bar.get_value()
    }));

    (root.upcast(), GameListHandle(view))
}

/// A button showing an icon beside a text label.
fn icon_label_button(icon_name: &str, label: &str) -> gtk::Button {
    let content = gtk::Box::new(gtk::Orientation::Horizontal, 6);
    content.append(&gtk::Image::from_icon_name(icon_name));
    content.append(&gtk::Label::new(Some(label)));

    let button = gtk::Button::new();
    button.set_child(Some(&content));
    button
}

/// The centred call-to-action shown when no game directory is configured.
struct EmptyState {
    root: gtk::Box,
    add_button: gtk::Button,
}

fn build_empty_state() -> EmptyState {
    let root = gtk::Box::new(gtk::Orientation::Vertical, 12);
    root.set_valign(gtk::Align::Center);
    root.set_halign(gtk::Align::Center);
    root.set_hexpand(true);
    root.set_vexpand(true);

    let icon = gtk::Image::from_icon_name("folder-symbolic");
    icon.set_pixel_size(64);
    icon.add_css_class("dim-label");
    root.append(&icon);

    let title = gtk::Label::new(Some("No games found"));
    title.add_css_class("title-2");
    root.append(&title);

    let subtitle = gtk::Label::new(Some(
        "Add the folder that holds your Switch titles to get started.",
    ));
    subtitle.add_css_class("dim-label");
    root.append(&subtitle);

    let add_button = gtk::Button::with_label("Add Game Directory");
    add_button.add_css_class("suggested-action");
    add_button.set_halign(gtk::Align::Center);
    root.append(&add_button);

    EmptyState { root, add_button }
}

impl GameListView {
    fn set_filter_visible(&self, visible: bool) {
        self.filter_bar.set_visible(visible);
        if visible {
            self.filter_entry.grab_focus();
        } else {
            self.filter_entry.set_text("");
        }
    }

    /// Upstream `GameList::OnTextChanged`.
    fn apply_filter(&self, text: &str) {
        let query = text.to_lowercase();
        let all_games = self.all_games.borrow();
        let total = all_games.iter().map(Vec::len).sum::<usize>();
        let mut visible = 0usize;

        for (directory_index, games) in all_games.iter().enumerate() {
            let Some(children) = self
                .store
                .item(directory_index as u32)
                .and_downcast::<GameEntry>()
                .and_then(|entry| entry.children())
            else {
                continue;
            };
            children.remove_all();
            for game in games {
                if game_matches_filter(game, &query) {
                    children.append(game);
                    visible += 1;
                }
            }
        }

        let result = crate::i18n::tr_args("%1 of %n result(s)", &[visible.to_string()])
            .replace("%n", &total.to_string());
        self.filter_result.set_text(&result);
    }

    fn activate_position(&self, position: u32) {
        let Some(row) = self
            .selection
            .model()
            .and_then(|model| model.item(position))
            .and_downcast::<gtk::TreeListRow>()
        else {
            return;
        };
        let Some(entry) = row.item().and_downcast::<GameEntry>() else {
            return;
        };
        if entry.is_folder() {
            row.set_expanded(!row.is_expanded());
        } else {
            (self.on_activate)(entry.path(), StartGameType::Normal);
        }
    }

    fn handle_navigation(&self, key: NavigationKey) -> bool {
        let Some(model) = self.selection.model() else {
            return false;
        };
        let count = model.n_items();
        if count == 0 {
            return false;
        }

        let selected = self.selection.selected();
        match key {
            NavigationKey::Down => {
                let next = if selected == gtk::INVALID_LIST_POSITION {
                    0
                } else {
                    (selected + 1).min(count - 1)
                };
                self.select_position(next);
            }
            NavigationKey::Up => {
                let next = if selected == gtk::INVALID_LIST_POSITION {
                    0
                } else {
                    selected.saturating_sub(1)
                };
                self.select_position(next);
            }
            NavigationKey::Left | NavigationKey::Right => {
                if selected == gtk::INVALID_LIST_POSITION {
                    self.select_position(0);
                    return true;
                }
                let Some(row) = model.item(selected).and_downcast::<gtk::TreeListRow>() else {
                    return false;
                };
                if key == NavigationKey::Right {
                    if row.is_expandable() && !row.is_expanded() {
                        row.set_expanded(true);
                    } else if let Some(child) = row.child_row(0) {
                        self.select_position(child.position());
                    }
                } else if row.is_expanded() {
                    row.set_expanded(false);
                } else if let Some(parent) = row.parent() {
                    self.select_position(parent.position());
                }
            }
            NavigationKey::Enter => {
                if selected == gtk::INVALID_LIST_POSITION {
                    self.select_position(0);
                } else {
                    self.activate_position(selected);
                }
            }
            NavigationKey::Escape => return false,
        }
        true
    }

    fn select_position(&self, position: u32) {
        self.selection.set_selected(position);
        self.column_view.grab_focus();
    }

    /// `GameList::PopupContextMenu`: show the menu owned by the clicked row.
    fn popup_context_menu(
        self: &Rc<Self>,
        entry: &GameEntry,
        anchor: &gtk::Widget,
        x: f64,
        y: f64,
        on_activate: Rc<dyn Fn(String, StartGameType)>,
    ) {
        if entry.is_folder() {
            self.popup_directory_context_menu(entry, anchor, x, y);
        } else {
            self.popup_game_context_menu(entry, anchor, x, y, on_activate);
        }
    }

    /// `GameList::AddPermDirPopup` followed by `AddCustomDirPopup`.
    fn popup_directory_context_menu(
        self: &Rc<Self>,
        entry: &GameEntry,
        anchor: &gtk::Widget,
        x: f64,
        y: f64,
    ) {
        let path = entry.path();
        let (position, count) = filesystem_directory_position(&path);

        let menu = gio::Menu::new();
        menu.append(
            Some(&crate::i18n::tr("▲ Move Up")),
            Some("game-list.move-up"),
        );
        menu.append(
            Some(&crate::i18n::tr("▼ Move Down")),
            Some("game-list.move-down"),
        );
        menu.append(
            Some(&crate::i18n::tr("Open Directory Location")),
            Some("game-list.open-directory"),
        );
        menu.append(
            Some(&crate::i18n::tr("Scan Subfolders")),
            Some("game-list.scan-subfolders"),
        );
        menu.append(
            Some(&crate::i18n::tr("Remove Game Directory")),
            Some("game-list.remove-directory"),
        );

        let actions = gio::SimpleActionGroup::new();

        let move_up = gio::SimpleAction::new("move-up", None);
        move_up.set_enabled(position.is_some_and(|index| index > 0));
        {
            let view = Rc::downgrade(self);
            let path = path.clone();
            move_up.connect_activate(move |_, _| {
                if let Some(view) = view.upgrade() {
                    view.move_directory(&path, -1);
                }
            });
        }
        actions.add_action(&move_up);

        let move_down = gio::SimpleAction::new("move-down", None);
        move_down.set_enabled(position.is_some_and(|index| index + 1 < count));
        {
            let view = Rc::downgrade(self);
            let path = path.clone();
            move_down.connect_activate(move |_, _| {
                if let Some(view) = view.upgrade() {
                    view.move_directory(&path, 1);
                }
            });
        }
        actions.add_action(&move_down);

        let open_directory = gio::SimpleAction::new("open-directory", None);
        {
            let path = path.clone();
            open_directory.connect_activate(move |_, _| open_directory_location(Path::new(&path)));
        }
        actions.add_action(&open_directory);

        let deep_scan = gio::SimpleAction::new_stateful(
            "scan-subfolders",
            None,
            &entry.deep_scan().to_variant(),
        );
        {
            let view = Rc::downgrade(self);
            let path = path.clone();
            deep_scan.connect_activate(move |action, _| {
                let enabled = !action
                    .state()
                    .and_then(|state| state.get::<bool>())
                    .unwrap_or(false);
                action.set_state(&enabled.to_variant());
                if let Some(view) = view.upgrade() {
                    view.set_deep_scan(&path, enabled);
                }
            });
        }
        actions.add_action(&deep_scan);

        let remove_directory = gio::SimpleAction::new("remove-directory", None);
        {
            let view = Rc::downgrade(self);
            remove_directory.connect_activate(move |_, _| {
                if let Some(view) = view.upgrade() {
                    view.remove_directory(&path);
                }
            });
        }
        actions.add_action(&remove_directory);

        show_context_menu(anchor, &menu, &actions, x, y);
    }

    /// Upstream `GameList::AddGamePopup`.
    fn popup_game_context_menu(
        self: &Rc<Self>,
        entry: &GameEntry,
        anchor: &gtk::Widget,
        x: f64,
        y: f64,
        on_activate: Rc<dyn Fn(String, StartGameType)>,
    ) {
        let path = entry.path();
        let program_id = entry.program_id();

        // `program_id == 0` hides the same title-id-dependent actions as
        // upstream's `setVisible(program_id != 0)` calls.
        let menu = gio::Menu::new();

        if program_id != 0 {
            let favorite_section = gio::Menu::new();
            favorite_section.append(
                Some(&crate::i18n::tr("Favorite")),
                Some("game-list.toggle-favorite"),
            );
            menu.append_section(None, &favorite_section);
        }

        let start_section = gio::Menu::new();
        start_section.append(
            Some(&crate::i18n::tr("Start Game")),
            Some("game-list.start-game"),
        );
        start_section.append(
            Some(&crate::i18n::tr("Start Game without Custom Configuration")),
            Some("game-list.start-game-global"),
        );
        menu.append_section(None, &start_section);

        let locations = gio::Menu::new();
        if program_id != 0 {
            locations.append(
                Some(&crate::i18n::tr("Open Save Data Location")),
                Some("game-list.open-save-data"),
            );
            locations.append(
                Some(&crate::i18n::tr("Open Mod Data Location")),
                Some("game-list.open-mod-data"),
            );
            locations.append(
                Some(&crate::i18n::tr("Open Transferable Pipeline Cache")),
                Some("game-list.open-pipeline-cache"),
            );
        }
        menu.append_section(None, &locations);

        let commands = gio::Menu::new();
        let remove = gio::Menu::new();
        let remove_individual = gio::Menu::new();
        if program_id != 0 {
            remove_individual.append(
                Some(&crate::i18n::tr("Remove Installed Update")),
                Some("game-list.remove-update"),
            );
            remove_individual.append(
                Some(&crate::i18n::tr("Remove All Installed DLC")),
                Some("game-list.remove-dlc"),
            );
        }
        remove_individual.append(
            Some(&crate::i18n::tr("Remove Custom Configuration")),
            Some("game-list.remove-custom-config"),
        );
        remove_individual.append(
            Some(&crate::i18n::tr("Remove Play Time Data")),
            Some("game-list.remove-play-time"),
        );
        remove_individual.append(
            Some(&crate::i18n::tr("Remove Cache Storage")),
            Some("game-list.remove-cache-storage"),
        );
        if program_id != 0 {
            remove_individual.append(
                Some(&crate::i18n::tr("Remove OpenGL Pipeline Cache")),
                Some("game-list.remove-gl-cache"),
            );
            remove_individual.append(
                Some(&crate::i18n::tr("Remove Vulkan Pipeline Cache")),
                Some("game-list.remove-vk-cache"),
            );
        }
        remove.append_section(None, &remove_individual);
        if program_id != 0 {
            let remove_all = gio::Menu::new();
            remove_all.append(
                Some(&crate::i18n::tr("Remove All Pipeline Caches")),
                Some("game-list.remove-all-caches"),
            );
            remove_all.append(
                Some(&crate::i18n::tr("Remove All Installed Contents")),
                Some("game-list.remove-all-content"),
            );
            remove.append_section(None, &remove_all);
        }
        commands.append_submenu(Some(&crate::i18n::tr("Remove")), &remove);

        let dump_romfs = gio::Menu::new();
        dump_romfs.append(
            Some(&crate::i18n::tr("Dump RomFS")),
            Some("game-list.dump-romfs"),
        );
        dump_romfs.append(
            Some(&crate::i18n::tr("Dump RomFS to SDMC")),
            Some("game-list.dump-romfs-sdmc"),
        );
        commands.append_submenu(Some(&crate::i18n::tr("Dump RomFS")), &dump_romfs);
        commands.append(
            Some(&crate::i18n::tr("Verify Integrity")),
            Some("game-list.verify-integrity"),
        );
        if program_id != 0 {
            commands.append(
                Some(&crate::i18n::tr("Copy Title ID to Clipboard")),
                Some("game-list.copy-title-id"),
            );
        }
        #[cfg(not(target_os = "macos"))]
        {
            let shortcuts = gio::Menu::new();
            shortcuts.append(
                Some(&crate::i18n::tr("Add to Desktop")),
                Some("game-list.shortcut-desktop"),
            );
            shortcuts.append(
                Some(&crate::i18n::tr("Add to Applications Menu")),
                Some("game-list.shortcut-applications"),
            );
            commands.append_submenu(Some(&crate::i18n::tr("Create Shortcut")), &shortcuts);
        }
        menu.append_section(None, &commands);

        let properties_section = gio::Menu::new();
        properties_section.append(
            Some(&crate::i18n::tr("Properties")),
            Some("game-list.properties"),
        );
        menu.append_section(None, &properties_section);

        let actions = gio::SimpleActionGroup::new();
        let start_game = gio::SimpleAction::new("start-game", None);
        {
            let path = path.clone();
            let on_activate = Rc::clone(&on_activate);
            start_game
                .connect_activate(move |_, _| on_activate(path.clone(), StartGameType::Normal));
        }
        actions.add_action(&start_game);

        let start_game_global = gio::SimpleAction::new("start-game-global", None);
        {
            let path = path.clone();
            start_game_global
                .connect_activate(move |_, _| on_activate(path.clone(), StartGameType::Global));
        }
        actions.add_action(&start_game_global);

        if program_id != 0 {
            let favorite = gio::SimpleAction::new_stateful(
                "toggle-favorite",
                None,
                &crate::configuration::qt_config::load_favorited_ids()
                    .contains(&program_id)
                    .to_variant(),
            );
            favorite.connect_activate(move |action, _| {
                let enabled = !action
                    .state()
                    .and_then(|value| value.get::<bool>())
                    .unwrap_or(false);
                action.set_state(&enabled.to_variant());
                let mut ids = crate::configuration::qt_config::load_favorited_ids();
                ids.retain(|id| *id != program_id);
                if enabled {
                    ids.push(program_id);
                }
                if let Err(error) = crate::configuration::qt_config::save_favorited_ids(&ids) {
                    log::error!("Failed to save favorite title: {error}");
                }
            });
            actions.add_action(&favorite);

            let open_save_data = gio::SimpleAction::new("open-save-data", None);
            {
                let view = Rc::downgrade(self);
                open_save_data.connect_activate(move |_, _| {
                    if let Some(view) = view.upgrade() {
                        view.open_save_data_location(program_id);
                    }
                });
            }
            actions.add_action(&open_save_data);

            let open_mod_data = gio::SimpleAction::new("open-mod-data", None);
            {
                let view = Rc::downgrade(self);
                open_mod_data.connect_activate(move |_, _| {
                    if let Some(view) = view.upgrade() {
                        view.open_mod_data_location(program_id);
                    }
                });
            }
            actions.add_action(&open_mod_data);

            let open_pipeline_cache = gio::SimpleAction::new("open-pipeline-cache", None);
            {
                let view = Rc::downgrade(self);
                open_pipeline_cache.connect_activate(move |_, _| {
                    if let Some(view) = view.upgrade() {
                        view.open_pipeline_cache_location(program_id);
                    }
                });
            }
            actions.add_action(&open_pipeline_cache);

            let copy_title_id = gio::SimpleAction::new("copy-title-id", None);
            copy_title_id.connect_activate(move |_, _| {
                if let Some(display) = gdk::Display::default() {
                    display.clipboard().set_text(&format!("{program_id:016X}"));
                }
            });
            actions.add_action(&copy_title_id);
        }

        for (name, detail) in [
            (
                "remove-update",
                "Removing installed updates is not available yet.",
            ),
            ("remove-dlc", "Removing installed DLC is not available yet."),
            (
                "remove-custom-config",
                "Removing custom configurations is not available yet.",
            ),
            (
                "remove-play-time",
                "Removing play-time data is not available yet.",
            ),
            (
                "remove-cache-storage",
                "Removing cache storage is not available yet.",
            ),
            (
                "remove-gl-cache",
                "Removing OpenGL pipeline caches is not available yet.",
            ),
            (
                "remove-vk-cache",
                "Removing Vulkan pipeline caches is not available yet.",
            ),
            (
                "remove-all-caches",
                "Removing all pipeline caches is not available yet.",
            ),
            (
                "remove-all-content",
                "Removing installed contents is not available yet.",
            ),
            ("dump-romfs", "Dumping RomFS is not available yet."),
            (
                "dump-romfs-sdmc",
                "Dumping RomFS to SDMC is not available yet.",
            ),
            (
                "verify-integrity",
                "Integrity verification is not available yet.",
            ),
            (
                "shortcut-desktop",
                "Desktop shortcut creation is not available yet.",
            ),
            (
                "shortcut-applications",
                "Applications-menu shortcut creation is not available yet.",
            ),
        ] {
            add_unavailable_action(&actions, name, self.parent_window(), detail);
        }

        let properties = gio::SimpleAction::new("properties", None);
        {
            let view = Rc::downgrade(self);
            let entry = entry.clone();
            properties.connect_activate(move |_, _| {
                if let Some(view) = view.upgrade() {
                    view.open_properties(&entry);
                }
            });
        }
        actions.add_action(&properties);

        show_context_menu(anchor, &menu, &actions, x, y);
    }

    fn open_properties(self: &Rc<Self>, entry: &GameEntry) {
        if let Some(dialog) = self.property_dialog.borrow().as_ref() {
            dialog.present();
            return;
        }

        let path = PathBuf::from(entry.path());
        let properties = crate::configuration::configure_per_game::GameProperties {
            name: entry.name(),
            developer: entry.developer(),
            version: entry.version(),
            title_id: entry.program_id(),
            format: entry.kind(),
            size: entry.size(),
            filename: path
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or_default()
                .to_string(),
            path,
            icon: entry.icon(),
        };
        let dialog = crate::configuration::configure_per_game::ConfigurePerGame::new(
            self.parent_window().as_ref(),
            properties,
            Arc::clone(&self.hid_core),
        );
        dialog.connect_closed({
            let view = Rc::downgrade(self);
            move || {
                if let Some(view) = view.upgrade() {
                    view.property_dialog.borrow_mut().take();
                }
            }
        });
        dialog.present();
        *self.property_dialog.borrow_mut() = Some(dialog);
    }

    fn open_save_data_location(&self, program_id: u64) {
        let root = common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::NANDDir)
            .join("user/save");
        let title = format!("{program_id:016X}");
        let found = find_directory_named(&root, &title, 4);
        let path = found.unwrap_or_else(|| {
            root.join("0000000000000000")
                .join("00000000000000000000000000000000")
                .join(title)
        });
        if let Err(error) = std::fs::create_dir_all(&path) {
            log::error!(
                "Failed to create save data directory {}: {error}",
                path.display()
            );
            crate::gtk_compat::show_warning(
                self.parent_window().as_ref(),
                "Error Opening Save Data Folder",
                "The save data directory could not be created.",
            );
            return;
        }
        open_directory_location(&path);
    }

    /// `GMainWindow::OnGameListOpenFolder`, `GameListOpenTarget::ModData`.
    fn open_mod_data_location(&self, program_id: u64) {
        let path = common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::LoadDir)
            .join(format!("{program_id:016X}"));
        if !path.is_dir() {
            crate::gtk_compat::show_warning(
                self.parent_window().as_ref(),
                "Error Opening Mod Data Folder",
                "Folder does not exist!",
            );
            return;
        }
        open_directory_location(&path);
    }

    /// `GMainWindow::OnTransferableShaderCacheOpenFile`.
    fn open_pipeline_cache_location(&self, program_id: u64) {
        let path = common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::ShaderDir)
            .join(format!("{program_id:016x}"));
        if let Err(error) = std::fs::create_dir_all(&path) {
            log::error!(
                "Failed to create pipeline cache directory {}: {error}",
                path.display()
            );
            crate::gtk_compat::show_warning(
                self.parent_window().as_ref(),
                "Error Opening Transferable Pipeline Cache",
                "Failed to create the pipeline cache directory for this title.",
            );
            return;
        }
        open_directory_location(&path);
    }

    fn parent_window(&self) -> Option<gtk::Window> {
        self.root.root().and_downcast::<gtk::Window>()
    }

    /// Rescan every configured directory and rebuild the tree — upstream
    /// re-runs `GameListWorker` after the directory list changes.
    fn reload(&self) {
        // Rebuilding the store drops the selection; remember which directory
        // was picked so it can be restored afterwards.
        let previously_selected = selected_directory_path(&self.selection);

        let dirs = uisettings::with(|v| v.game_dirs.clone());
        let scannable: Vec<GameDir> = dirs
            .into_iter()
            .filter(GameDir::is_filesystem_path)
            .collect();
        let directory_to_select =
            preferred_directory_path(previously_selected.as_deref(), &scannable);

        self.store.remove_all();
        self.all_games.borrow_mut().clear();

        let mut total = 0;
        for dir in &scannable {
            let games = scan_dir_games(Path::new(&dir.path), dir.deep_scan);
            total += games.len();

            let children = gio::ListStore::new::<GameEntry>();
            let mut all_games = Vec::with_capacity(games.len());
            for game in games {
                // Decode the control-data icon (JPEG) into a texture on the
                // main thread.
                let icon = game.icon.as_ref().and_then(|bytes| {
                    gdk::Texture::from_bytes(&glib::Bytes::from(bytes.as_slice())).ok()
                });
                let entry = GameEntry::new_game(
                    &game.name,
                    &game.developer,
                    &game.version,
                    &game.kind,
                    &human_size(game.size),
                    &game.path.to_string_lossy(),
                    icon,
                    game.program_id,
                );
                children.append(&entry);
                all_games.push(entry);
            }
            self.all_games.borrow_mut().push(all_games);
            self.store
                .append(&GameEntry::new_folder(&dir.path, dir.deep_scan, children));
        }

        log::info!(
            "Game list: found {total} game(s) across {} directory(ies)",
            scannable.len()
        );

        self.stack.set_visible_child_name(if scannable.is_empty() {
            PAGE_EMPTY
        } else {
            PAGE_LIST
        });

        if let Some(path) = directory_to_select {
            self.select_directory(&path);
        }
        self.apply_filter(&self.filter_entry.text());
    }

    /// Re-select the directory row for `path` after a rescan.
    ///
    /// The tree model is flat while every directory is collapsed, so the row
    /// index equals the directory index — but expanded directories contribute
    /// their games, so search the model rather than assuming.
    fn select_directory(&self, path: &str) {
        let model = self.selection.model();
        let Some(model) = model else { return };
        for position in 0..model.n_items() {
            let matches = model
                .item(position)
                .and_downcast::<gtk::TreeListRow>()
                .and_then(|row| row.item())
                .and_downcast::<GameEntry>()
                .is_some_and(|entry| entry.is_folder() && entry.path() == path);
            if matches {
                self.selection.set_selected(position);
                return;
            }
        }
    }

    /// Ask for a directory and add it — upstream `GMainWindow::OnGameListAddDirectory`.
    fn prompt_add_directory(self: &Rc<Self>) {
        let parent = self.root.root().and_downcast::<gtk::Window>();
        let view = Rc::clone(self);
        crate::gtk_compat::select_folder(parent.as_ref(), "Select Game Directory", move |result| {
            let Some(folder) = result else { return };
            let Some(path) = folder.path() else { return };
            view.add_directory(&path.to_string_lossy());
        });
    }

    /// Add `path` to the configured directories, unless it is already there.
    fn add_directory(&self, path: &str) {
        let already_present = uisettings::with(|v| v.game_dirs.iter().any(|d| d.path == path));
        if already_present {
            log::info!("Game list: {path} is already a game directory");
            return;
        }
        uisettings::with_mut(|v| {
            v.game_dirs.push(GameDir {
                path: path.to_string(),
                // User-facing ruzu default: discover titles in nested folders
                // immediately. The context-menu action can still disable it
                // per directory.
                deep_scan: NEW_DIRECTORY_DEEP_SCAN,
                expanded: true,
            })
        });
        self.persist();
        self.reload();
        self.select_directory(path);
    }

    /// Remove the directory at `path` — upstream's "Remove Game Directory".
    fn remove_directory(&self, path: &str) {
        uisettings::with_mut(|v| v.game_dirs.retain(|d| d.path != path));
        self.persist();
        self.reload();
    }

    /// Move a custom directory by one visible row, matching
    /// `GameList::AddPermDirPopup`.
    fn move_directory(&self, path: &str, direction: isize) {
        let moved = uisettings::with_mut(|values| {
            move_filesystem_directory(&mut values.game_dirs, path, direction)
        });
        if !moved {
            return;
        }
        self.persist();
        self.reload();
        self.select_directory(path);
    }

    /// Toggle recursive scanning for `path` — upstream's "Scan Subfolders".
    fn set_deep_scan(&self, path: &str, deep_scan: bool) {
        uisettings::with_mut(|v| {
            if let Some(dir) = v.game_dirs.iter_mut().find(|d| d.path == path) {
                dir.deep_scan = deep_scan;
            }
        });
        self.persist();
        self.reload();
    }

    /// Write the directory list back to ruzu's own config.
    fn persist(&self) {
        let dirs = uisettings::with(|v| v.game_dirs.clone());
        if let Err(e) = qt_config::save_game_dirs(&dirs) {
            log::error!("Failed to save game directories: {e}");
        }
    }
}

pub(crate) fn navigation_key_for_gdk(keyval: gdk::Key) -> Option<NavigationKey> {
    match keyval {
        gdk::Key::Return | gdk::Key::KP_Enter => Some(NavigationKey::Enter),
        gdk::Key::Escape => Some(NavigationKey::Escape),
        gdk::Key::Down => Some(NavigationKey::Down),
        gdk::Key::Left => Some(NavigationKey::Left),
        gdk::Key::Right => Some(NavigationKey::Right),
        gdk::Key::Up => Some(NavigationKey::Up),
        _ => None,
    }
}

/// Path of the directory row currently selected, if any.
fn selected_directory_path(selection: &gtk::SingleSelection) -> Option<String> {
    selection
        .selected_item()
        .and_downcast::<gtk::TreeListRow>()
        .and_then(|row| row.item())
        .and_downcast::<GameEntry>()
        .filter(|entry| entry.is_folder())
        .map(|entry| entry.path())
}

/// Preserve the selected directory across a reload. If there was no usable
/// selection and only one filesystem directory exists, select that directory
/// so the toolbar actions target it immediately.
fn preferred_directory_path(
    previously_selected: Option<&str>,
    directories: &[GameDir],
) -> Option<String> {
    if let Some(path) = previously_selected {
        if directories.iter().any(|directory| directory.path == path) {
            return Some(path.to_owned());
        }
    }

    if directories.len() == 1 {
        return Some(directories[0].path.clone());
    }

    None
}

/// Install the game-list CSS once.
///
/// Two effects upstream gets from Qt for free:
///  * `QTreeView::alternatingRowColors`, set on the game list in `main.ui`,
///    which produces the grey/white banding;
///  * `QPalette::Highlight` for the selected row.
///
/// GTK4's `ColumnView` has no alternating-row property, so the banding is done
/// with `:nth-child(even)` over the row widgets, derived from
/// `@theme_base_color` so it stays legible in both light and dark themes (see
/// `main_window::update_ui_theme`).
fn install_list_css() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let Some(display) = gdk::Display::default() else {
            return;
        };
        let provider = gtk::CssProvider::new();
        provider.load_from_data(&format!(
            ".ruzu-game-list > listview > row:nth-child(even) {{\
                 background-color: shade(@theme_base_color, {ALTERNATE_ROW_SHADE});\
             }}\
             .ruzu-game-list > listview > row:nth-child(odd) {{\
                 background-color: @theme_base_color;\
             }}\
             .ruzu-game-list > listview > row:selected {{\
                 background-color: {SELECTION_BG};\
                 color: #ffffff;\
             }}\
             .ruzu-game-list > listview > row:selected:focus {{\
                 outline: none;\
             }}\
             .ruzu-toolbar {{\
                 background-color: shade(@theme_bg_color, 1.02);\
                 border-bottom: 1px solid @borders;\
             }}\
             popover.ruzu-context-menu > contents,\
             popover.ruzu-context-menu contents {{\
                 border-radius: 0;\
             }}\
             popover.ruzu-context-menu > contents {{\
                 padding-top: 3px;\
                 padding-bottom: 3px;\
             }}\
             popover.ruzu-context-menu modelbutton {{\
                 min-height: 20px;\
                 padding-top: 2px;\
                 padding-bottom: 2px;\
             }}"
        ));
        gtk::style_context_add_provider_for_display(
            &display,
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );
    });
}

/// Selected-row background — Qt's `QPalette::Highlight` as the Fusion style
/// defines it, sampled from yuzu's own game list.
///
/// This is deliberately *not* GTK's `@theme_selected_bg_color`. yuzu runs its
/// default ("colorful") theme without a stylesheet, so its highlight comes from
/// the Qt style palette, which is a fixed blue rather than the desktop accent
/// colour. Inheriting the GTK accent instead makes the row orange on Ubuntu's
/// Yaru theme, purple on some others — a different colour per desktop, where
/// yuzu is blue everywhere.
const SELECTION_BG: &str = "#308CC6";

/// Alternating-row shade, likewise sampled from yuzu (`#F7F7F7` over white).
/// Expressed as a shade factor so it also works on a dark theme.
const ALTERNATE_ROW_SHADE: f32 = 0.97;

/// The "Name" column: expander, icon, and label, so a directory row can be
/// collapsed and its games are indented under it. Upstream likewise puts the
/// icon inside the Name column rather than in a column of its own.
fn make_name_column(on_context_menu: ContextMenuHandler) -> gtk::ColumnViewColumn {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(move |_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let row = gtk::Box::new(gtk::Orientation::Horizontal, 8);
        let picture = gtk::Picture::new();
        // GTK 4.8 renamed this pair to ContentFit::Contain.
        picture.set_keep_aspect_ratio(true);
        picture.set_can_shrink(true);
        let label = gtk::Label::builder().xalign(0.0).build();
        row.append(&picture);
        row.append(&label);

        let expander = gtk::TreeExpander::new();
        expander.set_child(Some(&row));
        install_context_menu_gesture(&expander, item, Rc::clone(&on_context_menu));

        item.set_child(Some(&expander));
    });
    factory.connect_bind(|_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let Some(expander) = item.child().and_downcast::<gtk::TreeExpander>() else {
            return;
        };
        let Some(tree_row) = item.item().and_downcast::<gtk::TreeListRow>() else {
            return;
        };
        expander.set_list_row(Some(&tree_row));

        let Some(entry) = tree_row.item().and_downcast::<GameEntry>() else {
            return;
        };
        let Some(row) = expander.child().and_downcast::<gtk::Box>() else {
            return;
        };
        let Some(picture) = row.first_child().and_downcast::<gtk::Picture>() else {
            return;
        };
        let Some(label) = row.last_child().and_downcast::<gtk::Label>() else {
            return;
        };

        if entry.is_folder() {
            picture.set_size_request(FOLDER_ICON_SIZE, FOLDER_ICON_SIZE);
            picture.set_paintable(gtk::gdk::Paintable::NONE);
            // A themed folder icon rather than control-data artwork.
            picture.set_paintable(folder_paintable().as_ref());
        } else {
            picture.set_size_request(ICON_SIZE, ICON_SIZE);
            picture.set_paintable(entry.icon().as_ref());
        }
        label.set_label(&entry.name());
    });

    let column = gtk::ColumnViewColumn::new(Some("Name"), Some(factory));
    column.set_expand(true);
    column.set_resizable(true);
    column
}

/// The themed folder icon used on directory rows.
fn folder_paintable() -> Option<gdk::Paintable> {
    let display = gdk::Display::default()?;
    let theme = gtk::IconTheme::for_display(&display);
    Some(
        theme
            .lookup_icon(
                "folder",
                &[],
                FOLDER_ICON_SIZE,
                1,
                gtk::TextDirection::None,
                gtk::IconLookupFlags::empty(),
            )
            .upcast(),
    )
}

/// Build one plain-text column bound to a `GameEntry` string getter.
fn make_text_column(
    title: &str,
    getter: fn(&GameEntry) -> String,
    on_context_menu: ContextMenuHandler,
) -> gtk::ColumnViewColumn {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(move |_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let label = gtk::Label::builder().xalign(0.0).build();
        install_context_menu_gesture(&label, item, Rc::clone(&on_context_menu));
        item.set_child(Some(&label));
    });
    factory.connect_bind(move |_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let Some(label) = item.child().and_downcast::<gtk::Label>() else {
            return;
        };
        let text = item
            .item()
            .and_downcast::<gtk::TreeListRow>()
            .and_then(|row| row.item())
            .and_downcast::<GameEntry>()
            .map(|entry| getter(&entry))
            .unwrap_or_default();
        label.set_label(&text);
    });

    let column = gtk::ColumnViewColumn::new(Some(title), Some(factory));
    column.set_resizable(true);
    column
}

/// Attach upstream's custom-context-menu behavior directly to one recycled
/// `ColumnView` cell. The `TreeListRow` held by the `ListItem` is the reliable
/// GTK4 equivalent of Qt's `QTreeView::indexAt(menu_location)`.
fn install_context_menu_gesture(
    anchor: &impl IsA<gtk::Widget>,
    item: &gtk::ListItem,
    on_context_menu: ContextMenuHandler,
) {
    let gesture = gtk::GestureClick::new();
    gesture.set_button(gdk::BUTTON_SECONDARY);
    let item = item.downgrade();
    let anchor = anchor.clone().upcast::<gtk::Widget>();
    let menu_anchor = anchor.clone();
    gesture.connect_pressed(move |gesture, _, x, y| {
        let Some(item) = item.upgrade() else { return };
        let Some(tree_row) = item.item().and_downcast::<gtk::TreeListRow>() else {
            return;
        };
        let Some(entry) = tree_row.item().and_downcast::<GameEntry>() else {
            return;
        };
        on_context_menu(entry, menu_anchor.clone(), tree_row.position(), x, y);
        gesture.set_state(gtk::EventSequenceState::Claimed);
    });
    anchor.add_controller(gesture);
}

/// Present a GTK menu at the click point. The action group is installed on the
/// clicked cell so `game-list.*` resolves exactly for this popup.
fn show_context_menu(
    anchor: &gtk::Widget,
    menu: &gio::Menu,
    actions: &gio::SimpleActionGroup,
    x: f64,
    y: f64,
) {
    let popover = gtk::PopoverMenu::from_model(Some(menu));
    // Upstream `QMenu` uses straight edges with the default Fusion style.
    // Override GTK themes that round popovers so the title menu matches it.
    popover.add_css_class("ruzu-context-menu");
    popover.set_has_arrow(false);
    popover.insert_action_group("game-list", Some(actions));
    popover.set_parent(anchor);
    popover.set_pointing_to(Some(&gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
    popover.connect_closed(|popover| {
        let popover = popover.clone();
        glib::idle_add_local_once(move || popover.unparent());
    });
    popover.popup();
}

fn add_unavailable_action(
    actions: &gio::SimpleActionGroup,
    name: &str,
    parent: Option<gtk::Window>,
    detail: &'static str,
) {
    let action = gio::SimpleAction::new(name, None);
    action.connect_activate(move |_, _| {
        crate::gtk_compat::show_warning(parent.as_ref(), "Game List", detail);
    });
    actions.add_action(&action);
}

fn find_directory_named(root: &Path, name: &str, remaining_depth: usize) -> Option<PathBuf> {
    if remaining_depth == 0 {
        return None;
    }
    for entry in std::fs::read_dir(root).ok()?.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        if path.file_name().and_then(|part| part.to_str()) == Some(name) {
            return Some(path);
        }
        if let Some(found) = find_directory_named(&path, name, remaining_depth - 1) {
            return Some(found);
        }
    }
    None
}

/// `GMainWindow::OnGameListOpenDirectory` using GTK/GIO's desktop launcher.
fn open_directory_location(path: &Path) {
    let directory = gio::File::for_path(path);
    if let Err(error) =
        gio::AppInfo::launch_default_for_uri(&directory.uri(), gio::AppLaunchContext::NONE)
    {
        log::error!("Failed to open directory {}: {error}", path.display());
    }
}

/// Visible index and visible directory count for `path`.
fn filesystem_directory_position(path: &str) -> (Option<usize>, usize) {
    uisettings::with(|values| {
        let paths: Vec<&str> = values
            .game_dirs
            .iter()
            .filter(|directory| directory.is_filesystem_path())
            .map(|directory| directory.path.as_str())
            .collect();
        (
            paths.iter().position(|candidate| *candidate == path),
            paths.len(),
        )
    })
}

/// Swap one custom directory with the adjacent visible custom directory.
fn move_filesystem_directory(directories: &mut [GameDir], path: &str, direction: isize) -> bool {
    let visible: Vec<usize> = directories
        .iter()
        .enumerate()
        .filter(|(_, directory)| directory.is_filesystem_path())
        .map(|(index, _)| index)
        .collect();
    let Some(visible_index) = visible
        .iter()
        .position(|index| directories[*index].path == path)
    else {
        return false;
    };
    let target = visible_index as isize + direction;
    if !(0..visible.len() as isize).contains(&target) {
        return false;
    }
    directories.swap(visible[visible_index], visible[target as usize]);
    true
}

// ---------------------------------------------------------------------------
// Scanning
// ---------------------------------------------------------------------------

/// A discovered game file, enriched with metadata read from the container.
struct GameFile {
    /// Display name: the real title from the control data if available, else the
    /// filename.
    name: String,
    developer: String,
    version: String,
    kind: String,
    size: u64,
    path: PathBuf,
    program_id: u64,
    /// Icon JPEG bytes from the control data, if any.
    icon: Option<Vec<u8>>,
}

/// Scan one directory and return the games it holds, sorted by title.
///
/// Mirrors upstream `GameListWorker::ScanFileSystem`: a candidate file is only
/// listed once a `Loader` accepts it *and* reports a real file type. That check
/// is what keeps update/DLC packages out of the list — an update-only NSP's
/// program NCA carries `ErrorMissingBKTRBaseRomFS` (it is a patch with no base
/// RomFS of its own), so `NSP::GetStatus()` is not `Success`, the loader
/// identifies the file as `FileType::Error`, and upstream skips it:
///
/// ```cpp
/// const auto file_type = loader->GetFileType();
/// if (file_type == Loader::FileType::Unknown || file_type == Loader::FileType::Error) {
///     return true;   // skip
/// }
/// ```
fn scan_dir_games(dir: &Path, deep_scan: bool) -> Vec<GameFile> {
    let mut candidates = Vec::new();
    collect_candidates(dir, deep_scan, &mut candidates);

    // Load each candidate once, keeping only what the loader accepts, and take
    // its title + icon from the same loader (upstream reuses the one loader for
    // `GetFileType` / `ReadTitle` / `ReadIcon` too).
    let mut reader = MetadataReader::new();
    let mut games = Vec::with_capacity(candidates.len());
    for mut game in candidates {
        let Some(metadata) = reader.read(&game.path.to_string_lossy()) else {
            log::debug!(
                "Game list: skipping {} (no loader accepted it)",
                game.path.display()
            );
            continue;
        };
        if let Some(title) = metadata.title {
            game.name = title;
        }
        game.developer = metadata.developer;
        game.version = metadata.version;
        game.icon = metadata.icon;
        game.program_id = metadata.program_id;
        games.push(game);
    }

    games.sort_by(|a, b| a.name.to_lowercase().cmp(&b.name.to_lowercase()));
    games
}

/// Collect candidate game files under `dir`, recursively when `deep_scan` is set.
fn collect_candidates(dir: &Path, deep_scan: bool, games: &mut Vec<GameFile>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        // `directory_entry::status()` in upstream follows symbolic links.
        let Ok(metadata) = entry.metadata() else {
            continue;
        };
        if metadata.is_dir() {
            if deep_scan {
                collect_candidates(&path, true, games);
            }
            continue;
        }
        let Some(ext) = path.extension().and_then(|e| e.to_str()) else {
            continue;
        };
        let ext_lower = ext.to_lowercase();
        if !SUPPORTED_EXTENSIONS.contains(&ext_lower.as_str()) {
            continue;
        }
        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("")
            .to_owned();
        games.push(GameFile {
            name,
            developer: String::new(),
            version: "1.0.0".to_string(),
            kind: ext_lower.to_uppercase(),
            size: metadata.len(),
            path,
            program_id: 0,
            icon: None,
        });
    }
}

/// Reads a game's control-data metadata (title, icon) without booting it.
///
/// Mirrors upstream `GameListWorker`'s use of `Loader::GetLoader` +
/// `ReadTitle`/`ReadIcon`. The loader only needs a lightweight
/// `loader::System` (content provider + filesystem controller), not the full
/// emulation `Core::System`. Keys come from the global `KeyManager` singleton.
struct MetadataReader {
    vfs: Arc<RealVfsFilesystem>,
    loader_system: LoaderSystem,
}

impl MetadataReader {
    fn new() -> Self {
        let vfs = RealVfsFilesystem::new();
        let content_provider = Arc::new(Mutex::new(ContentProviderUnion::new()));
        let mut controller = FileSystemController::new();
        controller.set_content_provider(content_provider.clone());
        controller.create_factories(vfs.clone(), false);
        let loader_system = LoaderSystem {
            content_provider: Some(content_provider),
            filesystem_controller: Some(Arc::new(Mutex::new(controller))),
        };
        Self { vfs, loader_system }
    }

    /// Metadata for a game the loader accepted; `None` when the file is not a
    /// bootable title (no loader, or `FileType::Unknown` / `FileType::Error`).
    fn read(&mut self, path: &str) -> Option<GameMetadata> {
        let file = self.vfs.arc_open_file(path, OpenMode::READ)?;
        let loader = get_loader(&mut self.loader_system, file, 0, 0)?;

        // Upstream's skip condition, verbatim: an update-only NSP lands here as
        // `Error` because its program NCA has no base RomFS to patch.
        let file_type = loader.get_file_type();
        if matches!(file_type, FileType::Unknown | FileType::Error) {
            return None;
        }

        let mut title = String::new();
        let title = if loader.read_title(&mut title) == ResultStatus::Success && !title.is_empty() {
            Some(title)
        } else {
            None
        };

        let mut icon = Vec::new();
        let icon = if loader.read_icon(&mut icon) == ResultStatus::Success && !icon.is_empty() {
            Some(icon)
        } else {
            None
        };

        let mut program_id = 0;
        if loader.read_program_id(&mut program_id) != ResultStatus::Success {
            program_id = 0;
        }

        let mut control = NACP::new();
        let (developer, version) =
            if loader.read_control_data(&mut control) == ResultStatus::Success {
                (control.get_developer_name(), control.get_version_string())
            } else {
                (String::new(), "1.0.0".to_string())
            };

        Some(GameMetadata {
            title,
            icon,
            program_id,
            developer,
            version,
        })
    }
}

/// What [`MetadataReader::read`] recovers from a container.
struct GameMetadata {
    title: Option<String>,
    icon: Option<Vec<u8>>,
    program_id: u64,
    developer: String,
    version: String,
}

/// Upstream `ContainsAllWords` plus the title-id branch in
/// `GameList::OnTextChanged`.
fn filter_fields_match(name: &str, path: &str, program_id: u64, query: &str) -> bool {
    if query.is_empty() {
        return true;
    }

    let filename = Path::new(path)
        .file_name()
        .and_then(|value| value.to_str())
        .unwrap_or_default();
    let haystack = format!("{filename} {name}").to_lowercase();
    let contains_all_words = query.split_whitespace().all(|word| haystack.contains(word));
    let title_id = format!("{program_id:016x}");
    contains_all_words || title_id.contains(query)
}

fn game_matches_filter(game: &GameEntry, query: &str) -> bool {
    filter_fields_match(&game.name(), &game.path(), game.program_id(), query)
}

/// Human-readable byte size (KiB / MiB / GiB), matching yuzu's display style.
fn human_size(bytes: u64) -> String {
    const UNITS: &[&str] = &["B", "KiB", "MiB", "GiB", "TiB"];
    let mut value = bytes as f64;
    let mut unit = 0;
    while value >= 1024.0 && unit < UNITS.len() - 1 {
        value /= 1024.0;
        unit += 1;
    }
    if unit == 0 {
        format!("{bytes} {}", UNITS[unit])
    } else {
        format!("{value:.1} {}", UNITS[unit])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU64, Ordering};

    static TEMP_DIR_COUNTER: AtomicU64 = AtomicU64::new(0);

    fn make_temp_dir() -> PathBuf {
        let counter = TEMP_DIR_COUNTER.fetch_add(1, Ordering::Relaxed);
        let path =
            std::env::temp_dir().join(format!("ruzu-game-list-{}-{counter}", std::process::id()));
        std::fs::create_dir_all(&path).unwrap();
        path
    }

    #[test]
    fn human_size_matches_yuzu_formatting() {
        assert_eq!(human_size(0), "0 B");
        assert_eq!(human_size(1023), "1023 B");
        assert_eq!(human_size(21_599_437), "20.6 MiB");
        assert_eq!(human_size(7_301_444_403), "6.8 GiB");
    }

    #[test]
    fn filter_matches_all_words_or_title_id_like_upstream() {
        let path = "/games/Sample Adventure [0100123456789ABC].nsp";
        assert!(filter_fields_match(
            "Sample Adventure",
            path,
            0x0100_1234_5678_9ABC,
            "adventure sample"
        ));
        assert!(filter_fields_match(
            "Sample Adventure",
            path,
            0x0100_1234_5678_9ABC,
            "56789abc"
        ));
        assert!(!filter_fields_match(
            "Sample Adventure",
            path,
            0x0100_1234_5678_9ABC,
            "missing sample"
        ));
    }

    #[test]
    fn supported_extensions_cover_every_switch_container() {
        // Mirrors `GameList::supported_file_extensions`; dropping one silently
        // hides a whole class of dumps.
        for ext in ["nsp", "xci", "nca", "nro", "nso", "kip"] {
            assert!(SUPPORTED_EXTENSIONS.contains(&ext), "{ext} missing");
        }
    }

    #[test]
    fn newly_added_directories_scan_subfolders_by_default() {
        assert!(NEW_DIRECTORY_DEEP_SCAN);
    }

    #[test]
    fn keyboard_navigation_matches_controller_actions() {
        assert_eq!(
            navigation_key_for_gdk(gdk::Key::Return),
            Some(NavigationKey::Enter)
        );
        assert_eq!(
            navigation_key_for_gdk(gdk::Key::KP_Enter),
            Some(NavigationKey::Enter)
        );
        assert_eq!(
            navigation_key_for_gdk(gdk::Key::Down),
            Some(NavigationKey::Down)
        );
        assert_eq!(
            navigation_key_for_gdk(gdk::Key::Left),
            Some(NavigationKey::Left)
        );
        assert_eq!(
            navigation_key_for_gdk(gdk::Key::Right),
            Some(NavigationKey::Right)
        );
        assert_eq!(
            navigation_key_for_gdk(gdk::Key::Up),
            Some(NavigationKey::Up)
        );
        assert_eq!(navigation_key_for_gdk(gdk::Key::F1), None);
    }

    #[test]
    fn deep_scan_matches_upstream_unbounded_recursion() {
        let root = make_temp_dir();
        let nested = root.join("one/two/three/four/five/six");
        std::fs::create_dir_all(&nested).unwrap();
        std::fs::write(root.join("direct.nsp"), []).unwrap();
        std::fs::write(nested.join("nested.nro"), []).unwrap();

        let mut shallow = Vec::new();
        collect_candidates(&root, false, &mut shallow);
        assert_eq!(shallow.len(), 1);
        assert_eq!(shallow[0].path, root.join("direct.nsp"));

        let mut recursive = Vec::new();
        collect_candidates(&root, true, &mut recursive);
        assert_eq!(recursive.len(), 2);
        assert!(recursive
            .iter()
            .any(|game| game.path == root.join("direct.nsp")));
        assert!(recursive
            .iter()
            .any(|game| game.path == nested.join("nested.nro")));

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn sole_directory_is_selected_after_reload() {
        let directory = GameDir {
            path: String::from(r"D:\Games\Switch"),
            deep_scan: false,
            expanded: true,
        };

        assert_eq!(
            preferred_directory_path(None, std::slice::from_ref(&directory)),
            Some(directory.path.clone())
        );
        assert_eq!(
            preferred_directory_path(Some(&directory.path), std::slice::from_ref(&directory)),
            Some(directory.path)
        );
        assert_eq!(preferred_directory_path(Some("removed"), &[]), None);
    }

    #[test]
    fn directory_context_move_preserves_non_filesystem_entries() {
        let mut directories = vec![
            GameDir {
                path: "SDMC".to_string(),
                deep_scan: false,
                expanded: true,
            },
            GameDir {
                path: "/games/one".to_string(),
                deep_scan: false,
                expanded: true,
            },
            GameDir {
                path: "UserNAND".to_string(),
                deep_scan: false,
                expanded: true,
            },
            GameDir {
                path: "/games/two".to_string(),
                deep_scan: true,
                expanded: false,
            },
        ];

        assert!(move_filesystem_directory(
            &mut directories,
            "/games/two",
            -1
        ));
        assert_eq!(directories[0].path, "SDMC");
        assert_eq!(directories[1].path, "/games/two");
        assert_eq!(directories[2].path, "UserNAND");
        assert_eq!(directories[3].path, "/games/one");
        assert!(!move_filesystem_directory(
            &mut directories,
            "/games/two",
            -1
        ));
        assert!(!move_filesystem_directory(&mut directories, "/missing", 1));
    }
}

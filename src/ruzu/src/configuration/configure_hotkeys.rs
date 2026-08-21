// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_hotkeys.cpp`
// (`ConfigureHotkeys`), whose widget tree lives in `configure_hotkeys.ui`.
//
// Upstream shows a `QTreeView` over a `QStandardItemModel` with three columns
// (Action / Hotkey / Controller Hotkey), grouped by context ("Main Window"),
// plus a hint label and the Clear All / Restore Defaults buttons. Double-clicking
// a binding opens `SequenceDialog` to record a new one.
//
// The default bindings come from `UISettings::default_hotkeys` in
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/uisettings.h`; [`DEFAULT_HOTKEYS`]
// below is that array, in the same order (upstream sorts by action name, which
// the array is already in).

use gtk::prelude::*;

use super::configure_dialog::Page;

/// `(action, keyboard hotkey, controller hotkey)`, mirroring
/// `UISettings::default_hotkeys`. All entries share the "Main Window" context,
/// which upstream renders as the single top-level group in the tree.
pub const DEFAULT_HOTKEYS: &[(&str, &str, &str)] = &[
    ("Audio Mute/Unmute", "Ctrl+M", "Home+Dpad_Right"),
    ("Audio Volume Down", "-", "Home+Dpad_Down"),
    ("Audio Volume Up", "=", "Home+Dpad_Up"),
    ("Capture Screenshot", "Ctrl+P", "Screenshot"),
    ("Change Adapting Filter", "F8", "Home+L"),
    ("Change Docked Mode", "F10", "Home+X"),
    ("Change GPU Accuracy", "F9", "Home+R"),
    ("Continue/Pause Emulation", "F4", "Home+Plus"),
    ("Exit Fullscreen", "Esc", ""),
    ("Exit ruzu", "Ctrl+Q", "Home+Minus"),
    ("Fullscreen", "F11", "Home+B"),
    ("Load File", "Ctrl+O", ""),
    ("Load/Remove Amiibo", "F2", "Home+A"),
    ("Multiplayer Browse Public Game Lobby", "Ctrl+B", ""),
    ("Multiplayer Create Room", "Ctrl+N", ""),
    ("Multiplayer Direct Connect to Room", "Ctrl+C", ""),
    ("Multiplayer Leave Room", "Ctrl+L", ""),
    ("Multiplayer Show Current Room", "Ctrl+R", ""),
    ("Restart Emulation", "F6", "R+Plus+Minus"),
    ("Stop Emulation", "F5", "L+Plus+Minus"),
    ("TAS Record", "Ctrl+F7", ""),
    ("TAS Reset", "Ctrl+F6", ""),
    ("TAS Start/Stop", "Ctrl+F5", ""),
    ("Toggle Filter Bar", "Ctrl+F", ""),
    ("Toggle Framerate Limit", "Ctrl+U", "Home+Y"),
    ("Toggle Mouse Panning", "Ctrl+F9", ""),
    ("Toggle Renderdoc Capture", "", ""),
    ("Toggle Status Bar", "Ctrl+S", ""),
];

/// The context every default hotkey belongs to — upstream's group row.
const CONTEXT: &str = "Main Window";

/// Column widths, roughly matching the Qt tree's resize-to-contents result.
const ACTION_COLUMN_WIDTH: i32 = 420;
const HOTKEY_COLUMN_WIDTH: i32 = 150;

/// Build the Hotkeys tab — upstream `ConfigureHotkeys`.
pub fn page() -> Page {
    let column = gtk::Box::new(gtk::Orientation::Vertical, 6);
    column.set_margin_top(10);
    column.set_margin_bottom(10);
    column.set_margin_start(10);
    column.set_margin_end(10);

    // Hint label + Clear All / Restore Defaults, on one row like `configure_hotkeys.ui`.
    let header = gtk::Box::new(gtk::Orientation::Horizontal, 6);
    let hint = gtk::Label::new(Some("Double-click on a binding to change it."));
    hint.set_xalign(0.0);
    hint.set_hexpand(true);
    let clear_all = gtk::Button::with_label("Clear All");
    let restore_defaults = gtk::Button::with_label("Restore Defaults");
    header.append(&hint);
    header.append(&clear_all);
    header.append(&restore_defaults);
    column.append(&header);

    // --- The binding tree -------------------------------------------------
    // GTK4's `ColumnView` is the closest analogue of `QTreeView` + model; a
    // `TreeListModel` supplies the one expandable "Main Window" group row that
    // upstream's `QStandardItemModel` produces.
    let store = gtk::gio::ListStore::new::<HotkeyRow>();
    store.append(&HotkeyRow::group(CONTEXT));

    let tree = gtk::TreeListModel::new(store.clone(), false, true, |item| {
        let row = item.downcast_ref::<HotkeyRow>()?;
        if !row.is_group() {
            return None;
        }
        let children = gtk::gio::ListStore::new::<HotkeyRow>();
        for (action, hotkey, controller) in DEFAULT_HOTKEYS {
            children.append(&HotkeyRow::binding(action, hotkey, controller));
        }
        Some(children.upcast())
    });

    let selection = gtk::SingleSelection::new(Some(tree));
    let view = gtk::ColumnView::new(Some(selection));
    view.set_vexpand(true);

    view.append_column(&expander_column("Action", ACTION_COLUMN_WIDTH, |row| {
        row.action()
    }));
    view.append_column(&text_column("Hotkey", HOTKEY_COLUMN_WIDTH, |row| {
        row.hotkey()
    }));
    view.append_column(&text_column(
        "Controller Hotkey",
        HOTKEY_COLUMN_WIDTH,
        |row| row.controller_hotkey(),
    ));

    let scroller = gtk::ScrolledWindow::builder()
        .hexpand(true)
        .vexpand(true)
        .child(&view)
        .build();
    column.append(&scroller);

    // Upstream's Clear All / Restore Defaults rewrite the model in place. Both
    // need the hotkey registry (`HotkeyRegistry`), which is not ported yet, so
    // they log rather than silently doing nothing.
    clear_all.connect_clicked(|_| {
        log::info!("Hotkeys: Clear All requested (hotkey registry not yet wired)");
    });
    restore_defaults.connect_clicked(|_| {
        log::info!("Hotkeys: Restore Defaults requested (hotkey registry not yet wired)");
    });

    Page::new("Hotkeys", column, || {
        // Upstream `ConfigureHotkeys::ApplyConfiguration(HotkeyRegistry&)` walks
        // the model back into the registry. Nothing to write back until the
        // registry exists and bindings are editable.
    })
}

/// Column whose cells carry the tree expander — the first column, as in Qt.
fn expander_column(
    title: &str,
    width: i32,
    get: fn(&HotkeyRow) -> String,
) -> gtk::ColumnViewColumn {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(|_, item| {
        let label = gtk::Label::new(None);
        label.set_xalign(0.0);
        let expander = gtk::TreeExpander::new();
        expander.set_child(Some(&label));
        item.downcast_ref::<gtk::ListItem>()
            .unwrap()
            .set_child(Some(&expander));
    });
    factory.connect_bind(move |_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let Some(expander) = item.child().and_downcast::<gtk::TreeExpander>() else {
            return;
        };
        let Some(tree_row) = item.item().and_downcast::<gtk::TreeListRow>() else {
            return;
        };
        expander.set_list_row(Some(&tree_row));
        if let (Some(label), Some(row)) = (
            expander.child().and_downcast::<gtk::Label>(),
            tree_row.item().and_downcast::<HotkeyRow>(),
        ) {
            label.set_text(&get(&row));
        }
    });

    let column = gtk::ColumnViewColumn::new(Some(title), Some(factory));
    column.set_fixed_width(width);
    column
}

/// Plain text column.
fn text_column(title: &str, width: i32, get: fn(&HotkeyRow) -> String) -> gtk::ColumnViewColumn {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(|_, item| {
        let label = gtk::Label::new(None);
        label.set_xalign(0.0);
        item.downcast_ref::<gtk::ListItem>()
            .unwrap()
            .set_child(Some(&label));
    });
    factory.connect_bind(move |_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let Some(label) = item.child().and_downcast::<gtk::Label>() else {
            return;
        };
        let text = item
            .item()
            .and_downcast::<gtk::TreeListRow>()
            .and_then(|r| r.item())
            .and_downcast::<HotkeyRow>()
            .map(|row| get(&row))
            .unwrap_or_default();
        label.set_text(&text);
    });

    let column = gtk::ColumnViewColumn::new(Some(title), Some(factory));
    column.set_fixed_width(width);
    column
}

// A `GObject` row so the list model can hold it. Upstream uses
// `QStandardItem`s carrying the same three strings.
mod imp {
    use std::cell::RefCell;

    use gtk::glib;
    use gtk::subclass::prelude::*;

    #[derive(Default)]
    pub struct HotkeyRow {
        pub action: RefCell<String>,
        pub hotkey: RefCell<String>,
        pub controller_hotkey: RefCell<String>,
        pub is_group: RefCell<bool>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for HotkeyRow {
        const NAME: &'static str = "RuzuHotkeyRow";
        type Type = super::HotkeyRow;
    }

    impl ObjectImpl for HotkeyRow {}
}

gtk::glib::wrapper! {
    /// One row of the hotkey tree: either the context group or a binding.
    pub struct HotkeyRow(ObjectSubclass<imp::HotkeyRow>);
}

impl HotkeyRow {
    /// The expandable context row ("Main Window").
    fn group(context: &str) -> Self {
        let this: Self = gtk::glib::Object::new();
        let imp = gtk::subclass::prelude::ObjectSubclassIsExt::imp(&this);
        *imp.action.borrow_mut() = context.to_string();
        *imp.is_group.borrow_mut() = true;
        this
    }

    /// A binding row.
    fn binding(action: &str, hotkey: &str, controller_hotkey: &str) -> Self {
        let this: Self = gtk::glib::Object::new();
        let imp = gtk::subclass::prelude::ObjectSubclassIsExt::imp(&this);
        *imp.action.borrow_mut() = action.to_string();
        *imp.hotkey.borrow_mut() = hotkey.to_string();
        *imp.controller_hotkey.borrow_mut() = controller_hotkey.to_string();
        this
    }

    fn is_group(&self) -> bool {
        *gtk::subclass::prelude::ObjectSubclassIsExt::imp(self)
            .is_group
            .borrow()
    }

    fn action(&self) -> String {
        gtk::subclass::prelude::ObjectSubclassIsExt::imp(self)
            .action
            .borrow()
            .clone()
    }

    fn hotkey(&self) -> String {
        gtk::subclass::prelude::ObjectSubclassIsExt::imp(self)
            .hotkey
            .borrow()
            .clone()
    }

    fn controller_hotkey(&self) -> String {
        gtk::subclass::prelude::ObjectSubclassIsExt::imp(self)
            .controller_hotkey
            .borrow()
            .clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_hotkeys_match_upstream_count() {
        // Upstream declares `std::array<Shortcut, 28> default_hotkeys`.
        assert_eq!(DEFAULT_HOTKEYS.len(), 28);
    }

    #[test]
    fn default_hotkeys_are_sorted_by_action() {
        // Upstream's array is already in the order the tree displays; a
        // re-ordering would silently change the UI.
        let mut sorted: Vec<&str> = DEFAULT_HOTKEYS.iter().map(|(a, _, _)| *a).collect();
        let original = sorted.clone();
        sorted.sort_unstable();
        assert_eq!(sorted, original);
    }

    #[test]
    fn renderdoc_capture_has_no_default_binding() {
        // The only entry upstream ships with both hotkey strings empty.
        let entry = DEFAULT_HOTKEYS
            .iter()
            .find(|(action, _, _)| *action == "Toggle Renderdoc Capture")
            .expect("entry present");
        assert_eq!(entry.1, "");
        assert_eq!(entry.2, "");
    }
}

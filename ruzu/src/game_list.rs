// SPDX-License-Identifier: GPL-3.0-or-later
//
// Game list view — counterpart of upstream `GameList` / `GameListWorker`
// (`/Users/vricosti/Dev/emulators/zuyu/src/yuzu/game_list*.cpp`). It reads the
// configured game directories from the (yuzu-schema) config, scans them for
// Switch executables, and shows them in a column view. Activating a row
// (double-click / Enter) boots the game.
//
// This first version keys off the file itself (name from the filename, type
// from the extension, size from the filesystem). Extracting the real title,
// title id, and icon from the container (as upstream does via `Loader`) is a
// follow-up.

use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use gtk::prelude::*;
use gtk::subclass::prelude::*;
use gtk::{gdk, gio, glib};

use common::fs::path_util::{get_ruzu_path, RuzuPath};
use ruzu_core::file_sys::fs_filesystem::OpenMode;
use ruzu_core::file_sys::registered_cache::ContentProviderUnion;
use ruzu_core::file_sys::vfs::vfs_real::RealVfsFilesystem;
use ruzu_core::hle::service::filesystem::filesystem::FileSystemController;
use ruzu_core::loader::loader::{get_loader, ResultStatus, System as LoaderSystem};

/// Pixel size of the game icon shown in the list.
const ICON_SIZE: i32 = 48;

/// Switch executable extensions listed in the game view. Mirrors
/// `GameList::supported_file_extensions`.
const SUPPORTED_EXTENSIONS: &[&str] = &["nsp", "xci", "nca", "nro", "nso", "kip"];

/// Max recursion depth when a game directory has deep-scan enabled.
const MAX_DEEP_SCAN_DEPTH: usize = 4;

// ---------------------------------------------------------------------------
// GameEntry — a GObject row model for the ColumnView.
// ---------------------------------------------------------------------------
mod imp {
    use std::cell::RefCell;

    use gtk::glib;
    use gtk::subclass::prelude::*;

    #[derive(Default)]
    pub struct GameEntry {
        pub name: RefCell<String>,
        pub kind: RefCell<String>,
        pub size: RefCell<String>,
        pub path: RefCell<String>,
        pub icon: RefCell<Option<gtk::gdk::Texture>>,
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
    fn new(name: &str, kind: &str, size: &str, path: &str, icon: Option<gdk::Texture>) -> Self {
        let obj: Self = glib::Object::new();
        let imp = obj.imp();
        *imp.name.borrow_mut() = name.to_owned();
        *imp.kind.borrow_mut() = kind.to_owned();
        *imp.size.borrow_mut() = size.to_owned();
        *imp.path.borrow_mut() = path.to_owned();
        *imp.icon.borrow_mut() = icon;
        obj
    }

    fn name(&self) -> String {
        self.imp().name.borrow().clone()
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
}

/// Build the game list widget. `on_activate` is invoked with the game's path
/// when a row is activated (double-click / Enter).
pub fn build<F: Fn(String) + 'static>(on_activate: F) -> gtk::Widget {
    let store = gio::ListStore::new::<GameEntry>();
    let games = scan_all_games();
    log::info!("Game list: found {} game(s)", games.len());
    for game in games {
        // Decode the control-data icon (JPEG) into a texture on the main thread.
        let icon = game.icon.as_ref().and_then(|bytes| {
            gdk::Texture::from_bytes(&glib::Bytes::from(bytes.as_slice())).ok()
        });
        store.append(&GameEntry::new(
            &game.name,
            &game.kind,
            &human_size(game.size),
            &game.path.to_string_lossy(),
            icon,
        ));
    }

    let selection = gtk::SingleSelection::new(Some(store));
    let column_view = gtk::ColumnView::new(Some(selection));
    column_view.add_css_class("data-table");

    column_view.append_column(&make_icon_column());
    let name_col = make_column("Name", GameEntry::name, true);
    column_view.append_column(&name_col);
    column_view.append_column(&make_column("File type", GameEntry::kind, false));
    column_view.append_column(&make_column("Size", GameEntry::size, false));

    // Activate (double-click / Enter) → boot.
    let on_activate = std::rc::Rc::new(on_activate);
    column_view.connect_activate(move |view, position| {
        if let Some(item) = view.model().and_then(|model| model.item(position)) {
            if let Ok(entry) = item.downcast::<GameEntry>() {
                on_activate(entry.path());
            }
        }
    });

    gtk::ScrolledWindow::builder()
        .hexpand(true)
        .vexpand(true)
        .child(&column_view)
        .build()
        .upcast()
}

/// Build one column bound to a `GameEntry` string getter.
fn make_column(
    title: &str,
    getter: fn(&GameEntry) -> String,
    expand: bool,
) -> gtk::ColumnViewColumn {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(|_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let label = gtk::Label::builder().xalign(0.0).build();
        item.set_child(Some(&label));
    });
    factory.connect_bind(move |_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let Some(entry) = item.item().and_downcast::<GameEntry>() else {
            return;
        };
        let Some(label) = item.child().and_downcast::<gtk::Label>() else {
            return;
        };
        label.set_label(&getter(&entry));
    });

    let column = gtk::ColumnViewColumn::new(Some(title), Some(factory));
    column.set_expand(expand);
    column.set_resizable(true);
    column
}

/// Build the leading icon column showing each game's control-data icon.
fn make_icon_column() -> gtk::ColumnViewColumn {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(|_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let picture = gtk::Picture::new();
        picture.set_size_request(ICON_SIZE, ICON_SIZE);
        picture.set_content_fit(gtk::ContentFit::Contain);
        item.set_child(Some(&picture));
    });
    factory.connect_bind(|_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let Some(entry) = item.item().and_downcast::<GameEntry>() else {
            return;
        };
        let Some(picture) = item.child().and_downcast::<gtk::Picture>() else {
            return;
        };
        picture.set_paintable(entry.icon().as_ref());
    });

    let column = gtk::ColumnViewColumn::new(Some(""), Some(factory));
    column.set_resizable(false);
    column
}

// ---------------------------------------------------------------------------
// Scanning
// ---------------------------------------------------------------------------

/// A discovered game file, enriched with metadata read from the container.
struct GameFile {
    /// Display name: the real title from the control data if available, else the
    /// filename.
    name: String,
    kind: String,
    size: u64,
    path: PathBuf,
    /// Icon JPEG bytes from the control data, if any.
    icon: Option<Vec<u8>>,
}

/// Scan every configured game directory for Switch executables and read each
/// one's title and icon from its control data.
fn scan_all_games() -> Vec<GameFile> {
    let mut games = Vec::new();
    for (dir, deep_scan) in read_game_dirs() {
        let max_depth = if deep_scan { MAX_DEEP_SCAN_DEPTH } else { 0 };
        scan_dir(&dir, max_depth, &mut games);
    }

    // Enrich with title + icon from each container's control NCA.
    let mut reader = MetadataReader::new();
    for game in &mut games {
        let (title, icon) = reader.read(&game.path.to_string_lossy());
        if let Some(title) = title {
            game.name = title;
        }
        game.icon = icon;
    }

    games.sort_by(|a, b| a.name.to_lowercase().cmp(&b.name.to_lowercase()));
    games
}

/// Recursively (up to `max_depth`) collect game files under `dir`.
fn scan_dir(dir: &Path, max_depth: usize, games: &mut Vec<GameFile>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let Ok(file_type) = entry.file_type() else {
            continue;
        };
        if file_type.is_dir() {
            if max_depth > 0 {
                scan_dir(&path, max_depth - 1, games);
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
        let size = entry.metadata().map(|m| m.len()).unwrap_or(0);
        games.push(GameFile {
            name,
            kind: ext_lower.to_uppercase(),
            size,
            path,
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
        controller.create_factories(vfs.clone(), false);
        let loader_system = LoaderSystem {
            content_provider: Some(content_provider),
            filesystem_controller: Some(Arc::new(Mutex::new(controller))),
        };
        Self {
            vfs,
            loader_system,
        }
    }

    /// Return `(title, icon_jpeg)` for the game at `path`; either may be `None`.
    fn read(&mut self, path: &str) -> (Option<String>, Option<Vec<u8>>) {
        let Some(file) = self.vfs.arc_open_file(path, OpenMode::READ) else {
            return (None, None);
        };
        let Some(loader) = get_loader(&mut self.loader_system, file, 0, 0) else {
            return (None, None);
        };

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

        (title, icon)
    }
}

/// Read the configured game directories (absolute paths only) and their
/// deep-scan flags from the ruzu config (yuzu `Paths\gamedirs\N\...` schema).
fn read_game_dirs() -> Vec<(PathBuf, bool)> {
    let config = get_ruzu_path(RuzuPath::ConfigDir).join("qt-config.ini");
    let Ok(contents) = std::fs::read_to_string(&config) else {
        return Vec::new();
    };

    use std::collections::BTreeMap;
    let mut paths: BTreeMap<u32, String> = BTreeMap::new();
    let mut deep: BTreeMap<u32, bool> = BTreeMap::new();

    for line in contents.lines() {
        let line = line.trim();
        let Some((key, value)) = line.split_once('=') else {
            continue;
        };
        // Keys look like `Paths\gamedirs\4\path` or `Paths\gamedirs\4\deep_scan`.
        let Some(rest) = key.strip_prefix("Paths\\gamedirs\\") else {
            continue;
        };
        let Some((index_str, field)) = rest.split_once('\\') else {
            continue;
        };
        let Ok(index) = index_str.parse::<u32>() else {
            continue;
        };
        match field {
            "path" => {
                paths.insert(index, value.to_owned());
            }
            "deep_scan" => {
                deep.insert(index, matches!(value, "true" | "1"));
            }
            _ => {}
        }
    }

    paths
        .into_iter()
        .filter_map(|(index, path)| {
            // Skip the special tokens (SDMC / UserNAND / SysNAND) — only real
            // filesystem directories are scanned for now.
            if !path.starts_with('/') {
                return None;
            }
            Some((PathBuf::from(path), deep.get(&index).copied().unwrap_or(false)))
        })
        .collect()
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

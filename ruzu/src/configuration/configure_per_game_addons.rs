// SPDX-FileCopyrightText: Copyright 2016 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! GTK counterpart of `yuzu/configuration/configure_per_game_addons.{h,cpp,ui}`.

use std::cell::Cell;
use std::cmp::Ordering;
use std::path::Path;
use std::sync::{Arc, Mutex};

use gtk::prelude::*;
use gtk::{gio, glib};
use ruzu_core::file_sys::patch_manager::PatchManager;
use ruzu_core::file_sys::registered_cache::ContentProviderUnion;
use ruzu_core::file_sys::vfs::vfs_real::RealVfsFilesystem;
use ruzu_core::hle::service::filesystem::filesystem::FileSystemController;
use ruzu_core::loader::loader::{get_loader, System as LoaderSystem};

use super::configure_dialog::Page;

/// One patch row supplied by the selected title's patch manager.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AddOn {
    pub name: String,
    pub version: String,
}

struct AddOnRow {
    name: String,
    version: String,
    enabled: Cell<bool>,
}

/// `ConfigurePerGameAddons::LoadFromFile`.
///
/// The GTK frontend does not keep a powered-on `Core::System` while the game
/// list is visible, so construct the same lightweight filesystem/controller
/// pair used by the list scanner and pass those upstream-owned dependencies to
/// `PatchManager`.
pub fn load_from_file(title_id: u64, path: &Path) -> Vec<AddOn> {
    use ruzu_core::file_sys::fs_filesystem::OpenMode;

    let vfs = RealVfsFilesystem::new();
    let content_provider = Arc::new(Mutex::new(ContentProviderUnion::new()));
    let mut controller = FileSystemController::new();
    controller.set_content_provider(content_provider.clone());
    controller.create_factories(vfs.clone(), false);
    let controller = Arc::new(Mutex::new(controller));
    let mut loader_system = LoaderSystem {
        content_provider: Some(content_provider.clone()),
        filesystem_controller: Some(controller.clone()),
    };

    let Some(file) = vfs.arc_open_file(&path.to_string_lossy(), OpenMode::READ) else {
        return Vec::new();
    };
    let Some(loader) = get_loader(&mut loader_system, file, 0, 0) else {
        return Vec::new();
    };
    let mut update_raw = None;
    loader.read_update_raw(&mut update_raw);

    let controller = controller.lock().unwrap_or_else(|error| error.into_inner());
    let content_provider = content_provider
        .lock()
        .unwrap_or_else(|error| error.into_inner());
    PatchManager::new(title_id, &controller, &*content_provider)
        .get_patches(update_raw)
        .into_iter()
        .map(|patch| AddOn {
            name: patch.name,
            version: patch.version,
        })
        .collect()
}

/// Build the sortable two-column patch list.
pub fn page(title_id: u64, patches: &[AddOn]) -> Page {
    let root = gtk::Box::new(gtk::Orientation::Vertical, 0);
    root.set_margin_top(10);
    root.set_margin_bottom(10);
    root.set_margin_start(10);
    root.set_margin_end(10);

    let disabled = common::settings::values()
        .disabled_addons
        .get(&title_id)
        .cloned()
        .unwrap_or_default();
    let store = gio::ListStore::new::<glib::BoxedAnyObject>();
    for patch in patches {
        store.append(&glib::BoxedAnyObject::new(AddOnRow {
            name: patch.name.clone(),
            version: patch.version.clone(),
            enabled: Cell::new(!disabled.contains(&patch.name)),
        }));
    }

    let view = gtk::ColumnView::new(None::<gtk::SingleSelection>);
    view.set_hexpand(true);
    view.set_vexpand(true);
    view.set_show_column_separators(true);
    view.set_show_row_separators(false);

    let name_column = addon_name_column();
    let version_column = addon_version_column();
    view.append_column(&name_column);
    view.append_column(&version_column);

    let sort_model = gtk::SortListModel::new(Some(store.clone()), view.sorter());
    let selection = gtk::SingleSelection::new(Some(sort_model));
    view.set_model(Some(&selection));

    let frame = gtk::Frame::new(None);
    frame.set_hexpand(true);
    frame.set_vexpand(true);
    frame.set_child(Some(&view));
    root.append(&frame);

    Page::new("Add-Ons", root, move || {
        let mut disabled: Vec<String> = (0..store.n_items())
            .filter_map(|index| store.item(index))
            .filter_map(|item| item.downcast::<glib::BoxedAnyObject>().ok())
            .filter_map(|item| {
                let row = item.borrow::<AddOnRow>();
                (!row.enabled.get()).then(|| row.name.clone())
            })
            .collect();
        disabled.sort();

        let mut settings = common::settings::values_mut();
        let mut current = settings
            .disabled_addons
            .get(&title_id)
            .cloned()
            .unwrap_or_default();
        current.sort();
        if current != disabled {
            let cache =
                common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::CacheDir)
                    .join("game_list")
                    .join(format!("{title_id:016X}.pv.txt"));
            let _ = std::fs::remove_file(cache);
        }
        settings.disabled_addons.insert(title_id, disabled);
    })
}

fn addon_name_column() -> gtk::ColumnViewColumn {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(|_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let check = gtk::CheckButton::new();
        check.set_hexpand(true);
        let weak_item = item.downgrade();
        check.connect_toggled(move |check| {
            let Some(item) = weak_item.upgrade() else {
                return;
            };
            let Some(row) = item.item().and_downcast::<glib::BoxedAnyObject>() else {
                return;
            };
            row.borrow::<AddOnRow>().enabled.set(check.is_active());
        });
        item.set_child(Some(&check));
    });
    factory.connect_bind(|_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let Some(check) = item.child().and_downcast::<gtk::CheckButton>() else {
            return;
        };
        let Some(row) = item.item().and_downcast::<glib::BoxedAnyObject>() else {
            return;
        };
        let row = row.borrow::<AddOnRow>();
        check.set_label(Some(&row.name));
        check.set_active(row.enabled.get());
    });

    let sorter =
        gtk::CustomSorter::new(|a, b| compare_addon_rows(a, b, |row| row.name.to_lowercase()));
    let column = gtk::ColumnViewColumn::new(Some("Patch Name"), Some(factory));
    column.set_expand(true);
    column.set_resizable(true);
    column.set_sorter(Some(&sorter));
    column
}

fn addon_version_column() -> gtk::ColumnViewColumn {
    let factory = gtk::SignalListItemFactory::new();
    factory.connect_setup(|_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        item.set_child(Some(&gtk::Label::builder().xalign(0.0).build()));
    });
    factory.connect_bind(|_, item| {
        let item = item.downcast_ref::<gtk::ListItem>().unwrap();
        let Some(label) = item.child().and_downcast::<gtk::Label>() else {
            return;
        };
        let Some(row) = item.item().and_downcast::<glib::BoxedAnyObject>() else {
            return;
        };
        label.set_label(&row.borrow::<AddOnRow>().version);
    });

    let sorter =
        gtk::CustomSorter::new(|a, b| compare_addon_rows(a, b, |row| row.version.to_lowercase()));
    let column = gtk::ColumnViewColumn::new(Some("Version"), Some(factory));
    column.set_fixed_width(130);
    column.set_resizable(true);
    column.set_sorter(Some(&sorter));
    column
}

fn compare_addon_rows(
    a: &glib::Object,
    b: &glib::Object,
    value: impl Fn(&AddOnRow) -> String,
) -> gtk::Ordering {
    let Some(a) = a.downcast_ref::<glib::BoxedAnyObject>() else {
        return gtk::Ordering::Equal;
    };
    let Some(b) = b.downcast_ref::<glib::BoxedAnyObject>() else {
        return gtk::Ordering::Equal;
    };
    match value(&a.borrow::<AddOnRow>()).cmp(&value(&b.borrow::<AddOnRow>())) {
        Ordering::Less => gtk::Ordering::Smaller,
        Ordering::Equal => gtk::Ordering::Equal,
        Ordering::Greater => gtk::Ordering::Larger,
    }
}

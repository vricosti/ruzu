// SPDX-FileCopyrightText: Copyright 2016 Citra Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! GTK counterpart of `yuzu/configuration/configure_per_game_addons.{h,cpp,ui}`.

use std::path::Path;
use std::sync::{Arc, Mutex};

use gtk::prelude::*;
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

    let table = gtk::Box::new(gtk::Orientation::Vertical, 0);
    let header = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    header.add_css_class("ruzu-properties-table-header");
    let name_header = gtk::Label::new(Some("Patch Name"));
    name_header.set_xalign(0.0);
    name_header.set_hexpand(true);
    let version_header = gtk::Label::new(Some("Version"));
    version_header.set_xalign(0.0);
    version_header.set_width_chars(16);
    header.append(&name_header);
    header.append(&version_header);
    table.append(&header);

    let rows = gtk::Box::new(gtk::Orientation::Vertical, 0);
    let disabled = common::settings::values()
        .disabled_addons
        .get(&title_id)
        .cloned()
        .unwrap_or_default();
    let mut checks = Vec::with_capacity(patches.len());
    for patch in patches {
        let row = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        let check = gtk::CheckButton::with_label(&patch.name);
        check.set_active(!disabled.contains(&patch.name));
        check.set_hexpand(true);
        let version = gtk::Label::new(Some(&patch.version));
        version.set_xalign(0.0);
        version.set_width_chars(16);
        row.append(&check);
        row.append(&version);
        rows.append(&row);
        checks.push((patch.name.clone(), check));
    }

    let scroller = gtk::ScrolledWindow::builder()
        .hexpand(true)
        .vexpand(true)
        .child(&rows)
        .build();
    table.append(&scroller);

    let frame = gtk::Frame::new(None);
    frame.set_hexpand(true);
    frame.set_vexpand(true);
    frame.set_child(Some(&table));
    root.append(&frame);

    Page::new("Add-Ons", root, move || {
        let mut disabled: Vec<String> = checks
            .iter()
            .filter(|(_, check)| !check.is_active())
            .map(|(name, _)| name.clone())
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

// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_profile_manager.cpp`
// (`ConfigureProfileManager`), whose widget tree lives in
// `configure_profile_manager.ui`.
//
// A "Profile Manager" group with the current user (avatar + name), a list of
// users (avatar, name, UUID), the Set Image / Add / Rename / Remove buttons, and
// the "Profile management is available only when game is not running." note.
//
// The users come from `Service::Account::ProfileManager`, which ruzu ports as
// `ruzu_core::hle::service::acc::profile_manager`.

use gtk::prelude::*;

use super::configure_dialog::Page;
use super::shared_widget as w;

/// Avatar size in the list and next to the current user, matching upstream.
const AVATAR_SIZE: i32 = 64;

/// Build the Profiles tab — upstream `ConfigureProfileManager`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    let (group, content) = w::group("Profile Manager");

    let profiles = load_profiles();
    let current_index = *common::settings::values().current_user.get_value();
    let current = usize::try_from(current_index)
        .ok()
        .and_then(|i| profiles.get(i));

    // --- "Current User" row ----------------------------------------------
    let current_row = gtk::Box::new(gtk::Orientation::Horizontal, 6);
    let current_label = gtk::Label::new(Some("Current User"));
    current_label.set_xalign(0.0);
    current_label.set_hexpand(true);
    current_row.append(&current_label);
    current_row.append(&avatar(
        current.and_then(|profile| profile.icon.as_ref()),
        AVATAR_SIZE,
    ));
    let current_name = gtk::Label::new(Some(
        current.map(|p| p.username.as_str()).unwrap_or_default(),
    ));
    current_name.set_xalign(0.0);
    current_name.set_width_chars(20);
    current_row.append(&current_name);
    content.append(&current_row);

    // --- "Users" list -----------------------------------------------------
    let users_frame = gtk::Frame::new(Some("Users"));
    users_frame.set_vexpand(true);

    let list = gtk::ListBox::new();
    list.set_selection_mode(gtk::SelectionMode::Single);
    for profile in &profiles {
        let row = gtk::Box::new(gtk::Orientation::Horizontal, 8);
        row.set_margin_top(4);
        row.set_margin_bottom(4);
        row.set_margin_start(6);
        row.append(&avatar(profile.icon.as_ref(), AVATAR_SIZE));

        let text = gtk::Box::new(gtk::Orientation::Vertical, 0);
        let name = gtk::Label::new(Some(&profile.username));
        name.set_xalign(0.0);
        let uuid = gtk::Label::new(Some(&profile.user_uuid));
        uuid.set_xalign(0.0);
        text.append(&name);
        text.append(&uuid);
        row.append(&text);

        list.append(&row);
    }

    let list_scroll = gtk::ScrolledWindow::builder()
        .hscrollbar_policy(gtk::PolicyType::Never)
        .vexpand(true)
        .min_content_height(320)
        .child(&list)
        .build();
    users_frame.set_child(Some(&list_scroll));
    content.append(&users_frame);

    // --- Buttons ----------------------------------------------------------
    let buttons = gtk::Box::new(gtk::Orientation::Horizontal, 6);
    let set_image = gtk::Button::with_label("Set Image");
    // Upstream keeps Set Image / Rename / Remove disabled until a row is picked.
    set_image.set_sensitive(false);
    let spacer = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    spacer.set_hexpand(true);
    let add = gtk::Button::with_label("Add");
    let rename = gtk::Button::with_label("Rename");
    rename.set_sensitive(false);
    let remove = gtk::Button::with_label("Remove");
    remove.set_sensitive(false);
    buttons.append(&set_image);
    buttons.append(&spacer);
    buttons.append(&add);
    buttons.append(&rename);
    buttons.append(&remove);
    content.append(&buttons);

    // Match upstream's `SetSelectedUser`, which enables the per-user buttons
    // once a row is selected.
    {
        let set_image = set_image.clone();
        let rename = rename.clone();
        let remove = remove.clone();
        list.connect_row_selected(move |_, row| {
            let selected = row.is_some();
            set_image.set_sensitive(selected);
            rename.set_sensitive(selected);
            remove.set_sensitive(selected);
        });
    }

    let note = gtk::Label::new(Some(
        "Profile management is available only when game is not running.",
    ));
    note.set_xalign(0.0);
    note.set_margin_top(4);
    content.append(&note);

    column.append(&group);

    // Add / Rename / Remove / Set Image all mutate the on-disk profile store
    // through `ProfileManager::{CreateNewUser, SetProfileBase, RemoveUser}`.
    // Writing to the real NAND profile database is a separate slice; log rather
    // than partially mutating it.
    for (button, action) in [
        (&add, "Add"),
        (&rename, "Rename"),
        (&remove, "Remove"),
        (&set_image, "Set Image"),
    ] {
        let action = action.to_string();
        button.connect_clicked(move |_| {
            log::info!("Profiles: {action} requested (profile store writes not yet wired)");
        });
    }

    Page::new("Profiles", scroller, || {
        // Upstream's `ApplyConfiguration` only writes `current_user`, which is
        // changed by picking a row rather than by OK; nothing to flush here.
    })
}

/// Render the profile JPEG loaded by [`load_profile_icon`].
fn avatar(icon: Option<&gtk::gdk::Texture>, size: i32) -> gtk::Picture {
    let picture = gtk::Picture::new();
    picture.set_size_request(size, size);
    picture.set_hexpand(false);
    picture.set_vexpand(false);
    picture.set_can_shrink(true);
    picture.set_keep_aspect_ratio(false);
    picture.set_paintable(icon);
    picture
}

/// One row of the users list.
struct Profile {
    username: String,
    user_uuid: String,
    icon: Option<gtk::gdk::Texture>,
}

/// Read the console's user profiles. Upstream constructs a
/// `Service::Account::ProfileManager` and walks `GetAllUsers()`.
///
fn load_profiles() -> Vec<Profile> {
    let manager = ruzu_core::hle::service::acc::profile_manager::ProfileManager::new();

    (0..manager.get_user_count())
        .filter_map(|index| {
            let base = manager.get_profile_base(Some(index))?;
            let uuid = common::uuid::UUID::from_bytes(base.user_uuid);
            let username_end = base
                .username
                .iter()
                .position(|byte| *byte == 0)
                .unwrap_or(base.username.len());

            Some(Profile {
                username: String::from_utf8_lossy(&base.username[..username_end]).into_owned(),
                user_uuid: uuid.formatted_string(),
                icon: load_profile_icon(uuid),
            })
        })
        .collect()
}

/// Upstream `GetImagePath` / `GetIcon`: load the per-user JPEG from the account
/// save and use `ACCOUNT_BACKUP_JPEG` when it is absent or invalid.
fn load_profile_icon(uuid: common::uuid::UUID) -> Option<gtk::gdk::Texture> {
    let path = common::fs::path_util::get_ruzu_path(common::fs::path_util::RuzuPath::NANDDir).join(
        format!(
            "system/save/8000000000000010/su/avators/{}.jpg",
            uuid.formatted_string()
        ),
    );

    std::fs::read(path)
        .ok()
        .and_then(|bytes| texture_from_bytes(&bytes, AVATAR_SIZE))
        .or_else(|| texture_from_bytes(&ruzu_core::constants::ACCOUNT_BACKUP_JPEG, AVATAR_SIZE))
}

fn texture_from_bytes(bytes: &[u8], size: i32) -> Option<gtk::gdk::Texture> {
    use gtk::gdk_pixbuf::prelude::PixbufLoaderExt;

    let loader = gtk::gdk_pixbuf::PixbufLoader::new();
    loader.write(bytes).ok()?;
    loader.close().ok()?;
    let pixbuf = loader.pixbuf()?;
    let scaled = pixbuf.scale_simple(size, size, gtk::gdk_pixbuf::InterpType::Bilinear)?;
    Some(gtk::gdk::Texture::for_pixbuf(&scaled))
}

#[cfg(test)]
mod tests {
    use super::{texture_from_bytes, AVATAR_SIZE};
    use gtk::prelude::TextureExt;

    #[test]
    fn account_backup_jpeg_matches_upstream_avatar_size() {
        let texture = texture_from_bytes(&ruzu_core::constants::ACCOUNT_BACKUP_JPEG, AVATAR_SIZE)
            .expect("the fallback avatar must decode");

        assert_eq!(texture.width(), AVATAR_SIZE);
        assert_eq!(texture.height(), AVATAR_SIZE);
    }
}

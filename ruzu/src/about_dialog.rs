// SPDX-License-Identifier: GPL-3.0-or-later
//
// GTK counterpart of yuzu/about_dialog.h and yuzu/about_dialog.cpp.

use gtk::prelude::*;

const PROJECT_URL: &str = "https://github.com/vricosti/ruzu";
const RUSTY_LEMON_ICON: &[u8] = include_bytes!("../assets/ruzu-rusty-lemon.png");
const ABOUT_DESCRIPTION: &str =
    "ruzu is an experimental open-source emulator for the Nintendo Switch licensed under GPLv3.0+.";
const LEGAL_NOTICE: &str =
    "This software should not be used to play games you have not legally obtained.";
const TRADEMARK_NOTICE: &str =
    "Nintendo Switch is a trademark of Nintendo. ruzu is not affiliated with Nintendo in any way.";

fn build_version() -> String {
    match (option_env!("GIT_BRANCH"), option_env!("GIT_DESC")) {
        (Some(branch), Some(description)) => {
            format!("Development Build | {branch}-{description}")
        }
        _ => env!("CARGO_PKG_VERSION").to_string(),
    }
}

pub struct AboutDialog {
    dialog: gtk::AboutDialog,
}

impl AboutDialog {
    /// Mirrors upstream `AboutDialog::AboutDialog` using GTK's native about
    /// dialogue in place of the Qt `.ui` form.
    pub fn new(parent: &impl IsA<gtk::Window>) -> Self {
        let comments = format!(
            "{}\n\n{}",
            crate::i18n::tr(ABOUT_DESCRIPTION),
            crate::i18n::tr(LEGAL_NOTICE)
        );
        let dialog = gtk::AboutDialog::builder()
            .transient_for(parent)
            .modal(true)
            .program_name("ruzu")
            .version(build_version())
            .comments(comments)
            .copyright(crate::i18n::tr(TRADEMARK_NOTICE))
            .website(PROJECT_URL)
            .website_label(crate::i18n::tr("Source Code"))
            .authors(["ruzu contributors"])
            .license_type(gtk::License::Gpl30)
            .build();
        let icon_bytes = gtk::glib::Bytes::from_static(RUSTY_LEMON_ICON);
        if let Ok(logo) = gtk::gdk::Texture::from_bytes(&icon_bytes) {
            dialog.set_logo(Some(&logo));
        }
        dialog.set_title(Some(&crate::i18n::tr("About ruzu")));

        Self { dialog }
    }

    pub fn present(&self) {
        self.dialog.present();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn project_metadata_is_ruzu_owned() {
        assert_eq!(PROJECT_URL, "https://github.com/vricosti/ruzu");
        assert!(ABOUT_DESCRIPTION.starts_with("ruzu is an experimental"));
        assert!(!ABOUT_DESCRIPTION.contains("yuzu"));
        assert!(!build_version().is_empty());
        assert!(RUSTY_LEMON_ICON.starts_with(b"\x89PNG\r\n\x1a\n"));
    }
}

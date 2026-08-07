// SPDX-License-Identifier: GPL-3.0-or-later
//
// GTK counterpart of yuzu's QTranslator ownership in GMainWindow. The Qt
// frontend is excluded from structural porting, so ruzu keeps the equivalent
// locale selection and widget translation in its GTK frontend.

use std::cell::Cell;
use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};

use gtk::prelude::*;

struct Catalogs {
    translations: HashMap<String, HashMap<String, String>>,
    sources: HashMap<String, String>,
}

fn catalogs() -> &'static Catalogs {
    static CATALOGS: OnceLock<Catalogs> = OnceLock::new();
    CATALOGS.get_or_init(|| {
        let translations: HashMap<String, HashMap<String, String>> =
            serde_json::from_str(include_str!("../i18n/catalogs.json"))
                .expect("embedded interface translation catalogs are valid JSON");
        let mut sources = HashMap::new();
        for messages in translations.values() {
            for (source, translated) in messages {
                sources
                    .entry(translated.clone())
                    .or_insert_with(|| source.clone());
            }
        }
        Catalogs {
            translations,
            sources,
        }
    })
}

fn catalog_translation(locale: &str, source: &str) -> Option<&'static str> {
    catalogs()
        .translations
        .get(locale)?
        .get(source)
        .map(String::as_str)
}

fn catalog_source(translated: &str) -> Option<&'static str> {
    catalogs().sources.get(translated).map(String::as_str)
}

/// Locale code and native display name, in upstream's `<System>`, English,
/// translated-catalog order.
pub const AVAILABLE_LANGUAGES: &[(&str, &str)] = &[
    ("", "<System>"),
    ("en", "English"),
    ("ar", "العربية"),
    ("ca", "Català"),
    ("cs", "Čeština"),
    ("da", "Dansk"),
    ("de", "Deutsch"),
    ("el", "Ελληνικά"),
    ("es", "Español"),
    ("fi", "Suomi"),
    ("fr", "Français (France)"),
    ("hu", "Magyar"),
    ("id", "Bahasa Indonesia"),
    ("it", "Italiano"),
    ("ja_JP", "日本語"),
    ("ko_KR", "한국어"),
    ("nb", "Norsk bokmål"),
    ("nl", "Nederlands"),
    ("pl", "Polski"),
    ("pt_BR", "Português (Brasil)"),
    ("pt_PT", "Português (Portugal)"),
    ("ru_RU", "Русский"),
    ("sv", "Svenska"),
    ("tr_TR", "Türkçe"),
    ("uk", "Українська"),
    ("vi", "Tiếng Việt"),
    ("vi_VN", "Tiếng Việt (Việt Nam)"),
    ("zh_CN", "简体中文"),
    ("zh_TW", "繁體中文"),
];

static CONFIGURED_LANGUAGE: OnceLock<RwLock<String>> = OnceLock::new();

thread_local! {
    static RETRANSLATING: Cell<bool> = const { Cell::new(false) };
}

struct RetranslationGuard;

impl Drop for RetranslationGuard {
    fn drop(&mut self) {
        RETRANSLATING.set(false);
    }
}

fn configured_language() -> &'static RwLock<String> {
    // Tests and non-GTK helpers are deterministic until the frontend applies
    // its stored locale. `main` explicitly sets `""` when System is selected.
    CONFIGURED_LANGUAGE.get_or_init(|| RwLock::new("en".to_string()))
}

pub fn set_language(locale: &str) {
    *configured_language().write().unwrap() = locale.to_string();
}

pub fn language() -> String {
    configured_language().read().unwrap().clone()
}

pub fn is_retranslating() -> bool {
    RETRANSLATING.get()
}

fn effective_language() -> String {
    let configured = language();
    if !configured.is_empty() {
        return resolve_catalog_locale(&configured);
    }

    ["LANGUAGE", "LC_ALL", "LC_MESSAGES", "LANG"]
        .into_iter()
        .find_map(|name| std::env::var(name).ok().filter(|value| !value.is_empty()))
        .map(|value| resolve_catalog_locale(&value))
        .unwrap_or_else(|| "en".to_string())
}

fn resolve_catalog_locale(locale: &str) -> String {
    let normalized = locale
        .split([':', '.', '@'])
        .next()
        .unwrap_or(locale)
        .replace('-', "_");
    if let Some((code, _)) = AVAILABLE_LANGUAGES
        .iter()
        .find(|(code, _)| !code.is_empty() && code.eq_ignore_ascii_case(&normalized))
    {
        return (*code).to_string();
    }
    let language = normalized.split('_').next().unwrap_or("en");
    AVAILABLE_LANGUAGES
        .iter()
        .find(|(code, _)| *code == language)
        .or_else(|| {
            AVAILABLE_LANGUAGES
                .iter()
                .find(|(code, _)| code.starts_with(&format!("{language}_")))
        })
        .map(|(code, _)| (*code).to_string())
        .unwrap_or_else(|| "en".to_string())
}

fn normalize_for_catalog(text: &str) -> (String, bool) {
    let branded = text.replace("ruzu", "yuzu").replace("Ruzu", "Yuzu");
    if catalog_source(&branded).is_some()
        || AVAILABLE_LANGUAGES
            .iter()
            .any(|(locale, _)| catalog_translation(locale, &branded).is_some())
    {
        return (branded, false);
    }

    let mnemonic = branded.replace('_', "&");
    let converted = mnemonic != branded
        && (catalog_source(&mnemonic).is_some()
            || AVAILABLE_LANGUAGES
                .iter()
                .any(|(locale, _)| catalog_translation(locale, &mnemonic).is_some()));
    if converted {
        (mnemonic, true)
    } else {
        (branded, false)
    }
}

/// Translate an English frontend string using the selected UI locale. Input
/// may already be translated, which lets an open window switch languages in
/// either direction without rebuilding every widget.
pub fn tr(text: &str) -> String {
    let (normalized, mnemonic) = normalize_for_catalog(text);
    let source = catalog_source(&normalized).unwrap_or(&normalized);
    let locale = effective_language();
    let translated = catalog_translation(&locale, source).unwrap_or(source);
    let translated = if mnemonic {
        translated.replace('&', "_")
    } else {
        translated.to_string()
    };
    translated.replace("yuzu", "ruzu").replace("Yuzu", "Ruzu")
}

/// Translate a Qt-style `%1`, `%2`, ... template and substitute its values.
pub fn tr_args(source: &str, arguments: &[String]) -> String {
    let mut translated = tr(source);
    for (index, value) in arguments.iter().enumerate() {
        translated = translated.replace(&format!("%{}", index + 1), value);
    }
    translated
}

/// Translate every textual GTK property below `root`. GTK stores button and
/// check-button captions as label children, so the recursive label pass also
/// covers those controls.
pub fn translate_widget_tree(root: &impl IsA<gtk::Widget>) {
    if RETRANSLATING.replace(true) {
        return;
    }
    let _guard = RetranslationGuard;
    translate_widget(root.as_ref());
}

fn translate_widget(widget: &gtk::Widget) {
    if let Some(label) = widget.downcast_ref::<gtk::Label>() {
        label.set_label(&tr(label.label().as_str()));
    }
    if let Some(window) = widget.downcast_ref::<gtk::Window>() {
        if let Some(title) = window.title() {
            window.set_title(Some(&tr(title.as_str())));
        }
    }
    if let Some(entry) = widget.downcast_ref::<gtk::Entry>() {
        if let Some(placeholder) = entry.placeholder_text() {
            entry.set_placeholder_text(Some(&tr(placeholder.as_str())));
        }
    }
    if let Some(dropdown) = widget.downcast_ref::<gtk::DropDown>() {
        if let Some(strings) = dropdown.model().and_downcast::<gtk::StringList>() {
            let selected = dropdown.selected();
            let translated: Vec<String> = (0..strings.n_items())
                .filter_map(|index| strings.string(index))
                .map(|value| tr(value.as_str()))
                .collect();
            let translated_refs: Vec<&str> = translated.iter().map(String::as_str).collect();
            strings.splice(0, strings.n_items(), &translated_refs);
            dropdown.set_selected(selected);
        }
    }
    if let Some(tooltip) = widget.tooltip_text() {
        widget.set_tooltip_text(Some(&tr(tooltip.as_str())));
    }

    let mut child = widget.first_child();
    while let Some(current) = child {
        child = current.next_sibling();
        translate_widget(&current);
    }
}

/// Translate `<attribute translatable="yes">` text before GtkBuilder parses
/// the menu model. This is the GTK equivalent of QTranslator translating the
/// actions declared by upstream's `main.ui`.
pub fn translate_builder_xml(xml: &str) -> String {
    const PREFIX: &str = "translatable=\"yes\">";
    const SUFFIX: &str = "</attribute>";

    let mut output = String::with_capacity(xml.len());
    let mut remaining = xml;
    while let Some(prefix_pos) = remaining.find(PREFIX) {
        let value_start = prefix_pos + PREFIX.len();
        let Some(relative_end) = remaining[value_start..].find(SUFFIX) else {
            break;
        };
        let value_end = value_start + relative_end;
        output.push_str(&remaining[..value_start]);
        output.push_str(&escape_xml_text(&tr(&remaining[value_start..value_end])));
        remaining = &remaining[value_end..];
    }
    output.push_str(remaining);
    output
}

fn escape_xml_text(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_lock() -> std::sync::MutexGuard<'static, ()> {
        static LOCK: OnceLock<std::sync::Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| std::sync::Mutex::new(()))
            .lock()
            .unwrap()
    }

    #[test]
    fn translations_handle_plain_mnemonic_brand_and_locale_switches() {
        let _guard = test_lock();
        set_language("fr");
        assert_eq!(tr("Cancel"), "Annuler");
        assert_eq!(tr("Add Game Directory"), "Ajouter un répertoire de jeux");
        assert_eq!(tr("_File"), "_Fichier");
        assert_eq!(tr("About ruzu"), "À propos de ruzu");
        set_language("de");
        assert_eq!(tr("Annuler"), "Abbrechen");
        set_language("en");
        assert_eq!(tr("Annuler"), "Cancel");
    }

    #[test]
    fn system_locale_uses_language_environment_prefix() {
        let _guard = test_lock();
        let old_language = std::env::var_os("LANGUAGE");
        set_language("");
        std::env::set_var("LANGUAGE", "fr_FR:en");
        assert_eq!(effective_language(), "fr");
        match old_language {
            Some(value) => std::env::set_var("LANGUAGE", value),
            None => std::env::remove_var("LANGUAGE"),
        }
    }

    #[test]
    fn builder_xml_translates_only_translatable_attributes() {
        let _guard = test_lock();
        set_language("fr");
        let translated = translate_builder_xml(
            r#"<attribute name="label" translatable="yes">_File</attribute><attribute name="action">app.file</attribute>"#,
        );
        assert!(translated.contains(">_Fichier</attribute>"));
        assert!(translated.contains(">app.file</attribute>"));
        set_language("en");
    }
}

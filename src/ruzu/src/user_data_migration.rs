// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK counterpart of Eden's `src/yuzu/user_data_migration.{h,cpp}`.
// The GTK widget hierarchy differs from Qt, but detection, frontend ownership,
// and the asynchronous worker boundary remain in this module.

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::mpsc;
use std::time::Duration;

use common::fs::path_util::{get_ruzu_path, RuzuPath};
use gtk::prelude::*;
use gtk::{glib, ResponseType};

use crate::migration_worker::{Emulator, LEGACY_EMULATORS};
use crate::migration_worker::{
    MigrationModeConversions, MigrationPlan, MigrationReport, MigrationSelection, MigrationStrategy,
};

/// Persistent first-start state. Unlike Eden's config-directory existence
/// check, this records that the migration question was actually answered.
const MIGRATION_PROMPT_MARKER: &str = ".migration_prompt_seen";
const EMULATOR_RESPONSE_BASE: u16 = 100;
const DIALOG_ACTION_MARGIN: i32 = 8;

#[derive(Debug, Clone)]
pub struct MigrationCompletion {
    pub emulator: Emulator,
    pub selection: MigrationSelection,
    pub report: MigrationReport,
}

type CompletionCallback = Box<dyn FnOnce(Option<MigrationCompletion>)>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum PromptMode {
    Startup,
    Manual,
}

#[derive(Clone)]
struct SystemWidgets {
    firmware: gtk::CheckButton,
    keys: gtk::CheckButton,
}

#[derive(Clone)]
struct StrategyWidgets {
    copy: gtk::CheckButton,
    share: gtk::CheckButton,
}

/// Discover legacy installations and show the one-time migration dialog.
/// Returns whether an asynchronous dialog was presented.
pub fn show<P: IsA<gtk::Window>>(
    parent: &P,
    callback: impl FnOnce(Option<MigrationCompletion>) + 'static,
) -> bool {
    show_dialog(parent, PromptMode::Startup, callback)
}

/// Open the migration dialog explicitly from the Tools menu. Unlike the
/// startup path, this ignores the one-time prompt marker. Cancelling a manual
/// invocation does not change that marker.
pub fn show_manual<P: IsA<gtk::Window>>(
    parent: &P,
    callback: impl FnOnce(Option<MigrationCompletion>) + 'static,
) -> bool {
    show_dialog(parent, PromptMode::Manual, callback)
}

fn show_dialog<P: IsA<gtk::Window>>(
    parent: &P,
    mode: PromptMode,
    callback: impl FnOnce(Option<MigrationCompletion>) + 'static,
) -> bool {
    if mode == PromptMode::Startup && migration_prompt_seen() {
        return false;
    }

    let emulators = discover_emulators();
    if emulators.is_empty() {
        // No prompt was shown, so `migration_prompt_seen` remains false. If a
        // legacy installation appears later, Ruzu can still offer it.
        return false;
    }

    let destinations = destinations();
    let dialog = gtk::Dialog::builder()
        .title(&crate::i18n::tr("User Data Migration"))
        .transient_for(parent)
        .modal(true)
        .default_width(760)
        .default_height(420)
        .build();
    let start_fresh_button =
        dialog.add_button(&crate::i18n::tr("Start Fresh"), ResponseType::Cancel);
    let review_button = dialog.add_button(
        &crate::i18n::tr("Review Migration"),
        ResponseType::Other(EMULATOR_RESPONSE_BASE),
    );
    for button in [&start_fresh_button, &review_button] {
        button.set_margin_bottom(DIALOG_ACTION_MARGIN);
    }
    review_button.set_margin_end(DIALOG_ACTION_MARGIN);

    let content = dialog.content_area();
    content.set_spacing(12);
    content.set_margin_top(16);
    content.set_margin_bottom(12);
    content.set_margin_start(16);
    content.set_margin_end(16);

    let introduction = gtk::Label::new(Some(&crate::i18n::tr(
        "Ruzu found data from another emulator. Choose the source, the data, and how Ruzu should use it.",
    )));
    introduction.set_xalign(0.0);
    introduction.set_wrap(true);
    content.append(&introduction);

    let warning = gtk::Label::new(Some(&crate::i18n::tr(
        "Source data is never moved or deleted. Copy is the recommended default; shared links make both emulators use the same directories. Shader caches are never migrated.",
    )));
    warning.set_xalign(0.0);
    warning.set_wrap(true);
    warning.add_css_class("dim-label");
    content.append(&warning);

    let source_row = gtk::Box::new(gtk::Orientation::Horizontal, 10);
    let source_label = gtk::Label::new(Some(&crate::i18n::tr("Source emulator")));
    source_label.set_xalign(0.0);
    let source_combo = gtk::ComboBoxText::new();
    for emulator in &emulators {
        source_combo.append_text(emulator.name);
    }
    source_combo.set_active(Some(0));
    source_combo.set_hexpand(true);
    source_row.append(&source_label);
    source_row.append(&source_combo);
    content.append(&source_row);

    let method_box = gtk::Box::new(gtk::Orientation::Vertical, 6);
    let method_label = gtk::Label::new(Some(&crate::i18n::tr("Migration method")));
    method_label.set_xalign(0.0);
    method_label.add_css_class("heading");
    let copy = migration_check("Copy from (recommended)", true);
    let share = migration_check("Share with (symbolic link / junction point)", false);
    share.set_group(Some(&copy));
    copy.set_tooltip_text(Some(&crate::i18n::tr(
        "Create an independent verified copy for Ruzu.",
    )));
    share.set_tooltip_text(Some(&crate::i18n::tr(
        "Use symbolic links on Linux and macOS, or directory junctions on Windows.",
    )));
    method_box.append(&method_label);
    method_box.append(&copy);
    method_box.append(&share);
    content.append(&method_box);

    let expert_notebook = gtk::Notebook::new();
    expert_notebook.set_vexpand(true);

    let system_box = gtk::Box::new(gtk::Orientation::Vertical, 8);
    system_box.set_margin_top(8);
    system_box.set_margin_bottom(8);
    system_box.set_margin_start(8);
    system_box.set_margin_end(8);
    let firmware = migration_check("Firmware", true);
    let keys = migration_check("Keys", true);
    for check in [&firmware, &keys] {
        system_box.append(check);
    }
    let system_note = gtk::Label::new(Some(&crate::i18n::tr(
        "Only firmware and keys are offered for now. Save data, settings, updates, DLC, SD card data, mods, and shader caches remain unchanged.",
    )));
    system_note.set_xalign(0.0);
    system_note.set_wrap(true);
    system_note.add_css_class("dim-label");
    system_box.append(&system_note);
    expert_notebook.append_page(
        &system_box,
        Some(&gtk::Label::new(Some(&crate::i18n::tr("System")))),
    );
    // The per-game migration tab is intentionally hidden for now. The worker
    // keeps its selective save/mod support so the UI can be restored later
    // without changing the verified copy format.
    content.append(&expert_notebook);

    let system_widgets = SystemWidgets { firmware, keys };
    let strategy_widgets = StrategyWidgets { copy, share };

    let callback: Rc<RefCell<Option<CompletionCallback>>> =
        Rc::new(RefCell::new(Some(Box::new(callback))));
    let emulators = Rc::new(emulators);
    dialog.connect_response({
        let callback = Rc::clone(&callback);
        let emulators = Rc::clone(&emulators);
        let destinations = destinations.clone();
        let system_widgets = system_widgets.clone();
        let strategy_widgets = strategy_widgets.clone();
        let source_combo = source_combo.clone();
        move |dialog, response| {
            let ResponseType::Other(EMULATOR_RESPONSE_BASE) = response else {
                if mode == PromptMode::Startup {
                    mark_migration_prompt_seen();
                }
                dialog.close();
                if let Some(callback) = callback.borrow_mut().take() {
                    callback(None);
                }
                return;
            };
            let Some(index) = source_combo
                .active()
                .map(|index| index as usize)
                .filter(|index| *index < emulators.len())
            else {
                return;
            };

            let selection = system_selection(&system_widgets);
            if !selection.any() {
                crate::gtk_compat::show_warning(
                    Some(dialog),
                    "Migration",
                    "Select at least one category or game to migrate.",
                );
                return;
            }

            let emulator = emulators[index].clone();
            let strategy = selected_strategy(&strategy_widgets);
            let mut plan = migration_plan(&emulator, &destinations, strategy, selection.clone());
            let conversions = match crate::migration_worker::inspect_mode_conversions(&plan) {
                Ok(conversions) => conversions,
                Err(error) => {
                    let detail = crate::i18n::tr_args(
                        "Ruzu could not inspect the existing migration destination:\n%1",
                        &[error.to_string()],
                    );
                    crate::gtk_compat::show_pretranslated_error(
                        Some(dialog),
                        &crate::i18n::tr("Migration Failed"),
                        &detail,
                    );
                    return;
                }
            };
            conversions.authorize(&mut plan);
            let estimated_size = (strategy == MigrationStrategy::Copy).then(|| {
                crate::migration_worker::estimate_selection_bytes(&plan)
                    .map(format_bytes)
                    .unwrap_or_else(|_| crate::i18n::tr("unknown"))
            });
            let worker_plan = plan.clone();
            let worker_emulator = emulator.clone();
            show_migration_confirmation(
                dialog,
                &emulator,
                &plan,
                conversions,
                estimated_size.as_deref(),
                {
                    let dialog = dialog.clone();
                    let callback = Rc::clone(&callback);
                    move |accepted| {
                        if !accepted {
                            return;
                        }
                        dialog.hide();
                        start_worker(
                            &dialog,
                            worker_plan,
                            worker_emulator,
                            selection,
                            Rc::clone(&callback),
                        );
                    }
                },
            );
        }
    });
    dialog.present();
    true
}

fn system_selection(system: &SystemWidgets) -> MigrationSelection {
    MigrationSelection {
        firmware: system.firmware.is_active(),
        keys: system.keys.is_active(),
        ..MigrationSelection::default()
    }
}

fn selected_strategy(strategy: &StrategyWidgets) -> MigrationStrategy {
    if strategy.share.is_active() {
        MigrationStrategy::Link
    } else {
        debug_assert!(strategy.copy.is_active());
        MigrationStrategy::Copy
    }
}

fn migration_check(label: &str, active: bool) -> gtk::CheckButton {
    let check = gtk::CheckButton::with_label(&crate::i18n::tr(label));
    check.set_active(active);
    check.set_margin_start(8);
    check.set_margin_end(8);
    check
}

fn start_worker(
    migration_dialog: &gtk::Dialog,
    plan: MigrationPlan,
    emulator: Emulator,
    selection: MigrationSelection,
    callback: Rc<RefCell<Option<CompletionCallback>>>,
) {
    let strategy = plan.strategy;
    let parent = migration_dialog.transient_for();
    let progress = gtk::Dialog::builder()
        .title(&crate::i18n::tr("Migrating"))
        .modal(true)
        .deletable(false)
        .default_width(420)
        .build();
    if let Some(parent) = parent.as_ref() {
        progress.set_transient_for(Some(parent));
    }
    let progress_box = progress.content_area();
    progress_box.set_spacing(12);
    progress_box.set_margin_top(20);
    progress_box.set_margin_bottom(20);
    progress_box.set_margin_start(20);
    progress_box.set_margin_end(20);
    let spinner = gtk::Spinner::new();
    spinner.start();
    let progress_text = match strategy {
        MigrationStrategy::Copy => "Copying and verifying data. This may take a while...",
        MigrationStrategy::Link => "Creating and verifying shared links...",
    };
    let label = gtk::Label::new(Some(&crate::i18n::tr(progress_text)));
    label.set_wrap(true);
    progress_box.append(&spinner);
    progress_box.append(&label);
    progress.present();

    let (sender, receiver) = mpsc::channel();
    std::thread::spawn(move || {
        let result = crate::migration_worker::process(&plan);
        let _ = sender.send(result);
    });

    let migration_dialog = migration_dialog.clone();
    let message_parent = parent;
    glib::timeout_add_local(Duration::from_millis(100), move || {
        let result = match receiver.try_recv() {
            Ok(result) => result,
            Err(mpsc::TryRecvError::Empty) => return glib::ControlFlow::Continue,
            Err(mpsc::TryRecvError::Disconnected) => Err(std::io::Error::other(
                "the migration worker stopped unexpectedly",
            )),
        };

        progress.close();
        match result {
            Ok(report) => {
                mark_migration_prompt_seen();
                let success = migration_success_text(strategy, report, emulator.name);
                crate::gtk_compat::show_pretranslated_message(
                    message_parent.as_ref(),
                    &crate::i18n::tr("Migration"),
                    &success,
                );
                if let Some(callback) = callback.borrow_mut().take() {
                    callback(Some(MigrationCompletion {
                        emulator: emulator.clone(),
                        selection: selection.clone(),
                        report,
                    }));
                }
            }
            Err(error) => {
                log::error!("Migration from {} failed: {error}", emulator.name);
                let detail = migration_error_text(strategy, &error.to_string());
                crate::gtk_compat::show_pretranslated_error(
                    message_parent.as_ref(),
                    &crate::i18n::tr("Migration Failed"),
                    &detail,
                );
                // The migration dialog was hidden while the worker ran. Show
                // it again so the user can retry or explicitly start fresh.
                // Closing it here can emit a Cancel response and incorrectly
                // persist the one-time prompt marker after a failed copy.
                migration_dialog.present();
            }
        }
        glib::ControlFlow::Break
    });
}

fn migration_success_text(
    strategy: MigrationStrategy,
    report: MigrationReport,
    source_name: &str,
) -> String {
    match strategy {
        MigrationStrategy::Copy => crate::i18n::tr_args(
            "Data was copied and verified successfully (%1 files, %2 bytes). The original %3 data was left unchanged.",
            &[
                report.files.to_string(),
                report.bytes.to_string(),
                source_name.to_owned(),
            ],
        ),
        MigrationStrategy::Link => crate::i18n::tr_args(
            "Shared links were created successfully (%1 directories). Ruzu and %2 now use the same selected data.",
            &[report.trees.to_string(), source_name.to_owned()],
        ),
    }
}

fn migration_error_text(strategy: MigrationStrategy, error: &str) -> String {
    let operation = match strategy {
        MigrationStrategy::Copy => crate::i18n::tr("verified copy"),
        MigrationStrategy::Link => crate::i18n::tr("shared-link setup"),
    };
    crate::i18n::tr_args(
        "No source data was removed. Ruzu could not complete the %1:\n%2",
        &[operation, error.to_owned()],
    )
}

#[derive(Debug, Clone)]
struct Destinations {
    config: PathBuf,
    nand: PathBuf,
    sdmc: PathBuf,
    load: PathBuf,
    keys: PathBuf,
}

fn destinations() -> Destinations {
    Destinations {
        config: get_ruzu_path(RuzuPath::ConfigDir),
        nand: get_ruzu_path(RuzuPath::NANDDir),
        sdmc: get_ruzu_path(RuzuPath::SDMCDir),
        load: get_ruzu_path(RuzuPath::LoadDir),
        keys: get_ruzu_path(RuzuPath::KeysDir),
    }
}

fn migration_plan(
    emulator: &Emulator,
    destinations: &Destinations,
    strategy: MigrationStrategy,
    selection: MigrationSelection,
) -> MigrationPlan {
    MigrationPlan {
        source_name: emulator.name.to_owned(),
        source_user_dir: emulator.get_user_dir().to_path_buf(),
        source_config_dir: emulator.get_config_dir().to_path_buf(),
        destination_config_dir: destinations.config.clone(),
        destination_nand_dir: destinations.nand.clone(),
        destination_sdmc_dir: destinations.sdmc.clone(),
        destination_load_dir: destinations.load.clone(),
        destination_keys_dir: destinations.keys.clone(),
        strategy,
        selection,
        confirmed_mode_conversion_destinations: Vec::new(),
    }
}

fn selected_data_text(plan: &MigrationPlan) -> String {
    let mut selected = Vec::new();
    if plan.selection.firmware {
        selected.push(crate::i18n::tr("firmware"));
    }
    if plan.selection.keys {
        selected.push(crate::i18n::tr("keys"));
    }
    if plan.selection.configuration {
        selected.push(crate::i18n::tr("configuration and game folders"));
    }
    if plan.selection.nand {
        selected.push(crate::i18n::tr("remaining NAND (updates and DLC)"));
    }
    if plan.selection.sdmc {
        selected.push(crate::i18n::tr("SD card"));
    }
    if !plan.selection.save_games.is_empty() {
        selected.push(crate::i18n::tr_args(
            "save data for %1 game(s)",
            &[plan.selection.save_games.len().to_string()],
        ));
    }
    if !plan.selection.mod_games.is_empty() {
        selected.push(crate::i18n::tr_args(
            "mods for %1 game(s)",
            &[plan.selection.mod_games.len().to_string()],
        ));
    }
    selected.join(", ")
}

fn show_migration_confirmation(
    parent: &gtk::Dialog,
    emulator: &Emulator,
    plan: &MigrationPlan,
    conversions: MigrationModeConversions,
    estimated_size: Option<&str>,
    callback: impl FnOnce(bool) + 'static,
) {
    let confirmation = gtk::Dialog::builder()
        .title(&crate::i18n::tr("Confirm Migration"))
        .transient_for(parent)
        .modal(true)
        .default_width(620)
        .build();
    let back_button = confirmation.add_button(&crate::i18n::tr("Back"), ResponseType::Cancel);
    let accept_label = match plan.strategy {
        MigrationStrategy::Copy => "Copy and Verify",
        MigrationStrategy::Link => "Create Shared Links",
    };
    let accept_button =
        confirmation.add_button(&crate::i18n::tr(accept_label), ResponseType::Accept);
    for button in [&back_button, &accept_button] {
        button.set_margin_bottom(DIALOG_ACTION_MARGIN);
    }
    accept_button.set_margin_end(DIALOG_ACTION_MARGIN);
    confirmation.set_default_response(ResponseType::Accept);

    let content = confirmation.content_area();
    content.set_spacing(12);
    content.set_margin_top(16);
    content.set_margin_bottom(16);
    content.set_margin_start(16);
    content.set_margin_end(16);
    let title = gtk::Label::new(Some(&crate::i18n::tr("Review the resulting state")));
    title.set_xalign(0.0);
    title.add_css_class("title-3");
    content.append(&title);

    let grid = gtk::Grid::builder()
        .row_spacing(10)
        .column_spacing(24)
        .build();
    let mut fields = vec![(crate::i18n::tr("Source emulator"), emulator.name.to_owned())];
    match plan.strategy {
        MigrationStrategy::Copy => {
            fields.push((crate::i18n::tr("Data to copy"), selected_data_text(plan)));
            fields.push((
                crate::i18n::tr("Estimated copy size"),
                estimated_size
                    .map(str::to_owned)
                    .unwrap_or_else(|| crate::i18n::tr("unknown")),
            ));
        }
        MigrationStrategy::Link => {
            fields.push((crate::i18n::tr("Data to share"), selected_data_text(plan)));
        }
    }
    for (row, (name, value)) in fields.into_iter().enumerate() {
        let name = gtk::Label::new(Some(&name));
        name.set_xalign(0.0);
        name.add_css_class("heading");
        grid.attach(&name, 0, row as i32, 1, 1);
        let value = gtk::Label::new(Some(&value));
        value.set_xalign(0.0);
        value.set_hexpand(true);
        value.set_wrap(true);
        grid.attach(&value, 1, row as i32, 1, 1);
    }
    content.append(&grid);

    if let Some(text) = conversion_warning_text(plan.strategy, conversions) {
        let warning = gtk::Label::new(Some(&text));
        warning.set_xalign(0.0);
        warning.set_wrap(true);
        warning.add_css_class("warning");
        content.append(&warning);
    }

    if plan.strategy == MigrationStrategy::Link {
        let warning = gtk::Label::new(Some(&crate::i18n::tr(
            "Ruzu and the source emulator will use the same selected directories. Changes made by either emulator affect both, and moving or deleting the source directories will break the links.",
        )));
        warning.set_xalign(0.0);
        warning.set_wrap(true);
        warning.add_css_class("warning");
        content.append(&warning);
    }

    let callback = RefCell::new(Some(callback));
    confirmation.connect_response(move |dialog, response| {
        if let Some(callback) = callback.borrow_mut().take() {
            callback(response == ResponseType::Accept);
        }
        dialog.close();
    });
    confirmation.present();
}

fn conversion_warning_text(
    strategy: MigrationStrategy,
    conversions: MigrationModeConversions,
) -> Option<String> {
    match strategy {
        MigrationStrategy::Link if conversions.copies_to_links != 0 => Some(crate::i18n::tr(
            "The existing Ruzu copy of the selected data will be deleted and replaced with a symbolic link (or a junction point on Windows).",
        )),
        MigrationStrategy::Copy if conversions.links_to_copies != 0 => Some(crate::i18n::tr(
            "The existing symbolic link (or junction point on Windows) will be deleted and the selected source data will be copied into Ruzu.",
        )),
        _ => None,
    }
}

fn format_bytes(bytes: u64) -> String {
    const UNITS: [&str; 5] = ["B", "KiB", "MiB", "GiB", "TiB"];
    if bytes < 1024 {
        return format!("{bytes} {}", UNITS[0]);
    }
    let mut value = bytes as f64;
    let mut unit = 0;
    while value >= 1024.0 && unit + 1 < UNITS.len() {
        value /= 1024.0;
        unit += 1;
    }
    format!("{value:.1} {}", UNITS[unit])
}

fn migration_prompt_seen() -> bool {
    let config = get_ruzu_path(RuzuPath::ConfigDir);
    migration_prompt_seen_in(&config)
}

fn mark_migration_prompt_seen() {
    let config = get_ruzu_path(RuzuPath::ConfigDir);
    if let Err(error) = mark_migration_prompt_seen_in(&config) {
        log::warn!("Could not persist migration_prompt_seen: {error}");
    }
}

fn migration_prompt_seen_in(config: &Path) -> bool {
    config.join(MIGRATION_PROMPT_MARKER).is_file()
}

fn mark_migration_prompt_seen_in(config: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(config)?;
    std::fs::write(config.join(MIGRATION_PROMPT_MARKER), b"true\n")
}

fn discover_emulators() -> Vec<Emulator> {
    #[cfg(windows)]
    {
        let base = common::fs::path_util::get_app_data_roaming_directory();
        discover_emulators_in(&base, &base, &base, true)
    }
    #[cfg(unix)]
    {
        let data = common::fs::path_util::get_data_directory("XDG_DATA_HOME");
        let config = common::fs::path_util::get_data_directory("XDG_CONFIG_HOME");
        let cache = common::fs::path_util::get_data_directory("XDG_CACHE_HOME");
        discover_emulators_in(&data, &config, &cache, false)
    }
}

fn discover_emulators_in(
    data: &Path,
    config: &Path,
    cache: &Path,
    auxiliary_dirs_inside_user: bool,
) -> Vec<Emulator> {
    LEGACY_EMULATORS
        .into_iter()
        .filter_map(|(name, directory_name)| {
            let user_dir = data.join(directory_name);
            if !user_dir.is_dir() {
                return None;
            }
            let config_dir = if auxiliary_dirs_inside_user {
                user_dir.join("config")
            } else {
                config.join(directory_name)
            };
            let cache_dir = if auxiliary_dirs_inside_user {
                user_dir.join("cache")
            } else {
                cache.join(directory_name)
            };
            Some(Emulator {
                name,
                directory_name,
                user_dir,
                config_dir,
                cache_dir,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    // Synthetic homebrew fixture id reserved for tests.
    const SYNTHETIC_HOMEBREW_TITLE_ID: u64 = 0x05AA_0000_0000_1000;

    #[test]
    fn discovery_uses_edens_emulator_order_and_xdg_layout() {
        let root = tempfile::tempdir().unwrap();
        let data = root.path().join("data");
        let config = root.path().join("config");
        let cache = root.path().join("cache");
        std::fs::create_dir_all(data.join("sudachi")).unwrap();
        std::fs::create_dir_all(data.join("yuzu")).unwrap();

        let found = discover_emulators_in(&data, &config, &cache, false);

        assert_eq!(
            found.iter().map(|emu| emu.name).collect::<Vec<_>>(),
            ["Sudachi", "Yuzu"]
        );
        assert_eq!(found[1].user_dir, data.join("yuzu"));
        assert_eq!(found[1].config_dir, config.join("yuzu"));
        assert_eq!(found[1].cache_dir, cache.join("yuzu"));
    }

    #[test]
    fn windows_style_layout_keeps_config_inside_user_directory() {
        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(root.path().join("yuzu")).unwrap();

        let found = discover_emulators_in(root.path(), root.path(), root.path(), true);

        assert_eq!(found[0].config_dir, root.path().join("yuzu/config"));
        assert_eq!(found[0].cache_dir, root.path().join("yuzu/cache"));
    }

    #[test]
    fn plans_never_contain_a_cache_or_shader_destination() {
        let root = tempfile::tempdir().unwrap();
        let emulator = Emulator {
            name: "Yuzu",
            directory_name: "yuzu",
            user_dir: root.path().join("yuzu"),
            config_dir: root.path().join("yuzu-config"),
            cache_dir: root.path().join("yuzu-cache"),
        };
        let destinations = Destinations {
            config: root.path().join("ruzu-config"),
            nand: root.path().join("ruzu/nand"),
            sdmc: root.path().join("ruzu/sdmc"),
            load: root.path().join("ruzu/load"),
            keys: root.path().join("ruzu/keys"),
        };
        let plan = migration_plan(
            &emulator,
            &destinations,
            MigrationStrategy::Link,
            MigrationSelection {
                firmware: true,
                configuration: true,
                nand: true,
                sdmc: true,
                keys: true,
                save_games: vec![SYNTHETIC_HOMEBREW_TITLE_ID],
                mod_games: vec![SYNTHETIC_HOMEBREW_TITLE_ID],
            },
        );
        assert_eq!(plan.strategy, MigrationStrategy::Link);
        let rendered = format!("{plan:?}");
        assert!(!rendered.contains("shader"));
        assert!(!rendered.contains("cache"));
    }

    #[test]
    fn migration_prompt_seen_is_an_explicit_dedicated_marker() {
        let root = tempfile::tempdir().unwrap();
        assert!(!migration_prompt_seen_in(root.path()));

        mark_migration_prompt_seen_in(root.path()).unwrap();
        assert!(migration_prompt_seen_in(root.path()));
        assert_eq!(
            std::fs::read(root.path().join(MIGRATION_PROMPT_MARKER)).unwrap(),
            b"true\n"
        );
    }

    #[test]
    fn translated_migration_messages_preserve_the_dynamic_source_name() {
        let copy = migration_success_text(
            MigrationStrategy::Copy,
            MigrationReport {
                trees: 2,
                files: 3,
                bytes: 4,
            },
            "Yuzu",
        );
        let link = migration_success_text(
            MigrationStrategy::Link,
            MigrationReport {
                trees: 2,
                files: 0,
                bytes: 0,
            },
            "Yuzu",
        );

        assert!(copy.contains("Yuzu"));
        assert!(!copy.contains("original Ruzu"));
        assert!(link.contains("Yuzu"));
        assert!(link.contains("Ruzu"));
    }

    #[test]
    fn confirmation_discloses_both_mode_conversions() {
        let mut conversions = MigrationModeConversions::default();
        conversions.copies_to_links = 1;
        let copy_to_link = conversion_warning_text(MigrationStrategy::Link, conversions).unwrap();
        assert!(copy_to_link.contains("copy"));
        assert!(copy_to_link.contains("deleted"));
        assert!(copy_to_link.contains("symbolic link"));
        assert!(copy_to_link.contains("junction point"));

        let mut conversions = MigrationModeConversions::default();
        conversions.links_to_copies = 1;
        let link_to_copy = conversion_warning_text(MigrationStrategy::Copy, conversions).unwrap();
        assert!(link_to_copy.contains("symbolic link"));
        assert!(link_to_copy.contains("deleted"));
        assert!(link_to_copy.contains("copied into Ruzu"));
    }
}

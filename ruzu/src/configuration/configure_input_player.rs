// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_input_player.cpp`
// (`ConfigureInputPlayer`), whose widget tree lives in
// `configure_input_player.ui`.
//
// Layout, top to bottom:
//   * header: "Connect Controller" + controller-type combo | "Input Device" |
//     "Profile" (combo + Save / New / Delete);
//   * body: a grid whose columns are
//       Left Stick + D-Pad │ L/ZL │ Minus/Plus, Capture/Home, controller art │
//       R/ZR │ Face Buttons + Right Stick + Mouse panning;
//   * footer: Console Mode radios, Vibration / Motion toggles + Configure,
//     Motion 1 binding, the Connected-controllers checkbox strip, and
//     Defaults / Clear.
//
// Each binding button shows the currently-mapped host input; clicking one puts
// upstream into "press a key" capture mode (`HandleClick` +
// `ConfigureInputPlayer::timeout`). Capture needs the `InputCommon` polling
// subsystem, which is not reachable from the dialog yet — the buttons render
// the stored mapping and log when pressed rather than silently doing nothing.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use gtk::prelude::*;

use common::settings_input::{native_analog, native_button, ControllerType, PlayerInput};

use super::configure_dialog::Page;
use super::shared_widget as w;

/// Controller types offered by the header combo — upstream
/// `ConfigureInputPlayer::UpdateControllerAvailableButtons`, in `.ui` order.
const CONTROLLER_TYPES: &[(ControllerType, &str)] = &[
    (ControllerType::ProController, "Pro Controller"),
    (ControllerType::DualJoyconDetached, "Dual Joycons"),
    (ControllerType::LeftJoycon, "Left Joycon"),
    (ControllerType::RightJoycon, "Right Joycon"),
    (ControllerType::Handheld, "Handheld"),
    (ControllerType::GameCube, "GameCube Controller"),
];

/// The label upstream shows for an unmapped binding.
const NOT_SET: &str = "[not set]";

/// Width of a binding button, so the columns line up like the Qt grid.
const BINDING_WIDTH: i32 = 84;

/// Upstream's `layout_show` array: every widget hidden by *some* controller
/// type, re-shown before the per-type hide list is applied.
const ALWAYS_SHOWN_GROUPS: &[&str] = &[
    "slsr_left",
    "slsr_right",
    "shoulder_left",
    "minus_screenshot",
    "bottom_left",
    "shoulder_right",
    "plus_home",
    "bottom_right",
    "minus",
    "screenshot",
];

/// Upstream's `layout_enable` array.
const ALWAYS_ENABLED_GROUPS: &[&str] = &["lstick_pressed", "rstick_pressed", "button_l", "home"];

/// Upstream `UpdateControllerAvailableButtons`' `layout_hidden` switch.
fn hidden_groups(layout: ControllerType) -> &'static [&'static str] {
    use ControllerType as C;
    match layout {
        C::ProController | C::Handheld => &["slsr_left", "slsr_right"],
        C::LeftJoycon => &["slsr_right", "shoulder_right", "plus_home", "bottom_right"],
        C::RightJoycon => &["slsr_left", "shoulder_left", "minus_screenshot", "bottom_left"],
        C::GameCube => &["slsr_left", "slsr_right", "minus", "screenshot"],
        // Dual Joy-Cons show every group.
        _ => &[],
    }
}

/// Upstream `UpdateControllerEnabledButtons`' `layout_disable` switch.
///
/// A GameCube pad has no home button and no clickable sticks, and its L is an
/// analog trigger rather than a digital button.
fn disabled_groups(layout: ControllerType) -> &'static [&'static str] {
    match layout {
        ControllerType::GameCube => &["home", "lstick_pressed", "rstick_pressed", "button_l"],
        _ => &[],
    }
}

/// Upstream `UpdateMotionButtons`, as `(motion_1_visible, motion_2_visible)`.
fn motion_visibility(layout: ControllerType) -> (bool, bool) {
    use ControllerType as C;
    match layout {
        C::ProController | C::LeftJoycon | C::Handheld => (true, false),
        C::RightJoycon => (false, true),
        C::GameCube => (false, false),
        // Dual Joy-Cons carry a motion sensor in each half.
        _ => (true, true),
    }
}

/// Upstream `UpdateControllerButtonNames`: the GameCube pad relabels half the
/// groups, because its shoulder layout does not line up with the Switch one.
fn group_titles(layout: ControllerType) -> &'static [(&'static str, &'static str)] {
    match layout {
        ControllerType::GameCube => &[
            ("plus", "Start / Pause"),
            ("zl", "L"),
            ("zr", "R"),
            ("r", "Z"),
            ("lstick", "Control Stick"),
            ("rstick", "C-Stick"),
        ],
        _ => &[
            ("plus", "Plus"),
            ("zl", "ZL"),
            ("zr", "ZR"),
            ("r", "R"),
            ("lstick", "Left Stick"),
            ("rstick", "Right Stick"),
        ],
    }
}

/// Build one "Player N" tab — upstream `ConfigureInputPlayer` for index `index`.
/// Everything the page needs to redraw itself after the controller type or the
/// input device changes.
///
/// Upstream keeps the equivalent as members of `ConfigureInputPlayer`
/// (`button_map`, `analog_map_buttons`, `motion_map`, and the `ui->` widget
/// pointers); the page is built by a free function here, so the handles are
/// collected in one struct instead.
struct PlayerPage {
    /// The working copy of the player's configuration. Upstream mutates the
    /// `EmulatedController` directly and only writes it back in `ApplyConfiguration`.
    state: Rc<RefCell<PlayerInput>>,

    /// Binding buttons, by `Settings::NativeButton` index.
    button_widgets: RefCell<Vec<(usize, gtk::Button)>>,
    /// Binding buttons, by `Settings::NativeAnalog` index and direction.
    analog_widgets: RefCell<Vec<(usize, Direction, gtk::Button)>>,
    /// Binding buttons, by `Settings::NativeMotion` index.
    motion_widgets: RefCell<Vec<(usize, gtk::Button)>>,

    /// Group boxes that upstream shows or hides per controller type.
    groups: RefCell<HashMap<&'static str, gtk::Widget>>,
    /// Group titles that upstream renames per controller type.
    titles: RefCell<HashMap<&'static str, gtk::Label>>,
}

impl PlayerPage {
    fn new(state: Rc<RefCell<PlayerInput>>) -> Rc<Self> {
        Rc::new(Self {
            state,
            button_widgets: RefCell::new(Vec::new()),
            analog_widgets: RefCell::new(Vec::new()),
            motion_widgets: RefCell::new(Vec::new()),
            groups: RefCell::new(HashMap::new()),
            titles: RefCell::new(HashMap::new()),
        })
    }

    fn register_group(&self, name: &'static str, widget: &impl IsA<gtk::Widget>) {
        self.groups
            .borrow_mut()
            .insert(name, widget.clone().upcast());
    }

    /// Upstream `ConfigureInputPlayer::UpdateUI`: re-label every binding button
    /// from the current configuration.
    fn update_ui(&self) {
        let state = self.state.borrow();
        for (index, button) in self.button_widgets.borrow().iter() {
            let text = state
                .buttons
                .get(*index)
                .map(|param| button_to_text(param))
                .unwrap_or_else(|| NOT_SET.to_string());
            button.set_label(&text);
        }
        for (index, direction, button) in self.analog_widgets.borrow().iter() {
            let text = state
                .analogs
                .get(*index)
                .map(|param| analog_to_text(param, *direction))
                .unwrap_or_else(|| NOT_SET.to_string());
            button.set_label(&text);
        }
        for (index, button) in self.motion_widgets.borrow().iter() {
            let text = state
                .motions
                .get(*index)
                .map(|param| button_to_text(param))
                .unwrap_or_else(|| NOT_SET.to_string());
            button.set_label(&text);
        }
    }

    /// Upstream `UpdateControllerAvailableButtons`, `UpdateControllerEnabledButtons`,
    /// `UpdateMotionButtons` and `UpdateControllerButtonNames`, which upstream
    /// always calls together from the controller-type handler.
    ///
    /// The decisions themselves live in the free functions below so they can be
    /// checked without a display.
    fn update_controller_layout(&self, layout: ControllerType) {
        let groups = self.groups.borrow();

        // `layout_show`: upstream un-hides everything, then applies the
        // per-type hide list.
        for name in ALWAYS_SHOWN_GROUPS {
            if let Some(widget) = groups.get(name) {
                widget.set_visible(true);
            }
        }
        for name in hidden_groups(layout) {
            if let Some(widget) = groups.get(name) {
                widget.set_visible(false);
            }
        }

        // `layout_enable` / `layout_disable`.
        for name in ALWAYS_ENABLED_GROUPS {
            if let Some(widget) = groups.get(name) {
                widget.set_sensitive(true);
            }
        }
        for name in disabled_groups(layout) {
            if let Some(widget) = groups.get(name) {
                widget.set_sensitive(false);
            }
        }

        // `UpdateMotionButtons`.
        let (motion_1, motion_2) = motion_visibility(layout);
        if let Some(widget) = groups.get("motion_1") {
            widget.set_visible(motion_1);
        }
        if let Some(widget) = groups.get("motion_2") {
            widget.set_visible(motion_2);
        }

        // `UpdateControllerButtonNames`.
        let titles = self.titles.borrow();
        for (key, text) in group_titles(layout) {
            if let Some(label) = titles.get(key) {
                label.set_text(text);
            }
        }
    }
}

/// Build one "Player N" tab — upstream `ConfigureInputPlayer` for index `index`.
pub fn page(index: usize, input_subsystem: Rc<RefCell<input_common::InputSubsystem>>) -> Page {
    let state = Rc::new(RefCell::new(player_input(index)));
    let page = PlayerPage::new(Rc::clone(&state));
    let initial_type = state.borrow().controller_type;

    install_group_style();

    let column = gtk::Box::new(gtk::Orientation::Vertical, 8);
    column.set_margin_top(10);
    column.set_margin_bottom(10);
    column.set_margin_start(10);
    column.set_margin_end(10);

    // --- Header -----------------------------------------------------------
    let header = gtk::Box::new(gtk::Orientation::Horizontal, 12);

    let connect_box = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let connected = gtk::CheckButton::with_label("Connect Controller");
    connected.set_active(state.borrow().connected);
    let type_labels: Vec<&str> = CONTROLLER_TYPES.iter().map(|(_, l)| *l).collect();
    let controller_type = w::combo(
        &type_labels,
        CONTROLLER_TYPES
            .iter()
            .position(|(t, _)| *t == initial_type)
            .unwrap_or(0) as u32,
    );
    connect_box.append(&connected);
    connect_box.append(&controller_type);
    header.append(&connect_box);

    let device_box = gtk::Box::new(gtk::Orientation::Vertical, 4);
    device_box.set_hexpand(true);
    let device_label = gtk::Label::new(Some("Input Device"));
    device_label.set_xalign(0.0);
    device_label.set_valign(gtk::Align::Center);

    // Upstream `UpdateInputDevices`: the combo is filled from
    // `InputSubsystem::GetInputDevices()`, whose first entry is always "Any",
    // followed by "Keyboard Only", "Keyboard/Mouse" and one row per detected
    // pad ("Xbox One Controller 0"). The `ParamPackage`s are kept alongside so
    // a selection can be turned back into a device.
    let input_devices: Vec<common::param_package::ParamPackage> =
        input_subsystem.borrow().get_input_devices();
    let device_names: Vec<String> = input_devices
        .iter()
        .map(|device| device.get_str("display", "Unknown"))
        .collect();
    let device_refs: Vec<&str> = device_names.iter().map(String::as_str).collect();
    let input_device = w::combo(&device_refs, 0);
    device_box.append(&device_label);
    device_box.append(&input_device);
    header.append(&device_box);

    let profile_box = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let profile_label = gtk::Label::new(Some("Profile"));
    profile_label.set_xalign(0.0);
    profile_label.set_valign(gtk::Align::Center);
    let profile_row = gtk::Box::new(gtk::Orientation::Horizontal, 4);
    let profile = w::combo(&[""], 0);
    profile.set_width_request(90);
    let save_profile = gtk::Button::with_label("Save");
    let new_profile = gtk::Button::with_label("New");
    let delete_profile = gtk::Button::with_label("Delete");
    profile_row.append(&profile);
    profile_row.append(&save_profile);
    profile_row.append(&new_profile);
    profile_row.append(&delete_profile);
    profile_box.append(&profile_label);
    profile_box.append(&profile_row);
    header.append(&profile_box);

    // The three header columns each stack a caption over a control. The
    // "Connect Controller" check box is taller than a plain label, which pushed
    // its combo below the other two; a vertical size group makes the caption
    // row one height so the controls beneath line up.
    let header_captions = gtk::SizeGroup::new(gtk::SizeGroupMode::Vertical);
    header_captions.add_widget(&connected);
    header_captions.add_widget(&device_label);
    header_captions.add_widget(&profile_label);

    column.append(&header);

    // --- Body grid --------------------------------------------------------
    let body = gtk::Grid::new();
    body.set_column_spacing(10);
    body.set_row_spacing(6);
    body.set_vexpand(true);

    // Column 0: Left Stick, then D-Pad — upstream's `bottomLeft`.
    let bottom_left = gtk::Box::new(gtk::Orientation::Vertical, 6);
    bottom_left.set_valign(gtk::Align::Start);
    bottom_left.append(&stick_group(&page, "lstick", "Left Stick", Stick::Left));
    bottom_left.append(&dpad_group(&page));
    page.register_group("bottom_left", &bottom_left);
    body.attach(&bottom_left, 0, 0, 1, 2);

    // Column 1: SL/SR (left), then L and ZL.
    let left_column = gtk::Box::new(gtk::Orientation::Vertical, 6);
    left_column.set_valign(gtk::Align::Start);
    let slsr_left = gtk::Box::new(gtk::Orientation::Vertical, 6);
    slsr_left.append(&binding_block(&page, "SL", native_button::Values::SLLeft));
    slsr_left.append(&binding_block(&page, "SR", native_button::Values::SRLeft));
    page.register_group("slsr_left", &slsr_left);
    left_column.append(&slsr_left);

    let shoulder_left = gtk::Box::new(gtk::Orientation::Vertical, 6);
    let (l_block, l_title) = titled_binding_block(&page, "L", native_button::Values::L);
    page.register_group("button_l", &l_block);
    let _ = l_title;
    shoulder_left.append(&l_block);
    let (zl_block, zl_title) = trigger_block(&page, "ZL", native_button::Values::ZL);
    page.titles.borrow_mut().insert("zl", zl_title);
    shoulder_left.append(&zl_block);
    page.register_group("shoulder_left", &shoulder_left);
    left_column.append(&shoulder_left);
    body.attach(&left_column, 1, 0, 1, 2);

    // Column 2: Minus / Plus, Capture / Home, and the controller art below.
    let centre = gtk::Box::new(gtk::Orientation::Vertical, 6);
    centre.set_hexpand(true);
    let system_top = gtk::Box::new(gtk::Orientation::Horizontal, 12);
    system_top.set_halign(gtk::Align::Center);

    let minus_screenshot = gtk::Box::new(gtk::Orientation::Vertical, 6);
    let (minus_block, _) = titled_binding_block(&page, "Minus", native_button::Values::Minus);
    page.register_group("minus", &minus_block);
    let (screenshot_block, _) =
        titled_binding_block(&page, "Capture", native_button::Values::Screenshot);
    page.register_group("screenshot", &screenshot_block);
    minus_screenshot.append(&minus_block);
    minus_screenshot.append(&screenshot_block);
    page.register_group("minus_screenshot", &minus_screenshot);
    system_top.append(&minus_screenshot);

    let plus_home = gtk::Box::new(gtk::Orientation::Vertical, 6);
    let (plus_block, plus_title) = titled_binding_block(&page, "Plus", native_button::Values::Plus);
    page.titles.borrow_mut().insert("plus", plus_title);
    let (home_block, _) = titled_binding_block(&page, "Home", native_button::Values::Home);
    page.register_group("home", &home_block);
    plus_home.append(&plus_block);
    plus_home.append(&home_block);
    page.register_group("plus_home", &plus_home);
    system_top.append(&plus_home);

    centre.append(&system_top);

    // Upstream's `PlayerControlPreview`, rebuilt when the controller type
    // changes (upstream instead tells the one widget which type to draw).
    let preview_holder = gtk::Box::new(gtk::Orientation::Vertical, 0);
    preview_holder.set_hexpand(true);
    preview_holder.append(&super::controller_preview::build(initial_type));
    centre.append(&preview_holder);

    // Motion 1 / Motion 2 sit under the art, as in the .ui.
    let motion_row = gtk::Box::new(gtk::Orientation::Horizontal, 12);
    motion_row.set_halign(gtk::Align::Center);
    let motion_1 = motion_block(&page, "Motion 1", 0);
    let motion_2 = motion_block(&page, "Motion 2", 1);
    page.register_group("motion_1", &motion_1);
    page.register_group("motion_2", &motion_2);
    motion_row.append(&motion_1);
    motion_row.append(&motion_2);
    centre.append(&motion_row);

    body.attach(&centre, 2, 0, 1, 2);

    // Column 3: SL/SR (right), then R and ZR.
    let right_column = gtk::Box::new(gtk::Orientation::Vertical, 6);
    right_column.set_valign(gtk::Align::Start);
    let slsr_right = gtk::Box::new(gtk::Orientation::Vertical, 6);
    slsr_right.append(&binding_block(&page, "SL", native_button::Values::SLRight));
    slsr_right.append(&binding_block(&page, "SR", native_button::Values::SRRight));
    page.register_group("slsr_right", &slsr_right);
    right_column.append(&slsr_right);

    let shoulder_right = gtk::Box::new(gtk::Orientation::Vertical, 6);
    let (r_block, r_title) = titled_binding_block(&page, "R", native_button::Values::R);
    page.titles.borrow_mut().insert("r", r_title);
    shoulder_right.append(&r_block);
    let (zr_block, zr_title) = trigger_block(&page, "ZR", native_button::Values::ZR);
    page.titles.borrow_mut().insert("zr", zr_title);
    shoulder_right.append(&zr_block);
    page.register_group("shoulder_right", &shoulder_right);
    right_column.append(&shoulder_right);
    body.attach(&right_column, 3, 0, 1, 2);

    // Column 4: Face Buttons, Right Stick, Mouse panning — upstream's `bottomRight`.
    let bottom_right = gtk::Box::new(gtk::Orientation::Vertical, 6);
    bottom_right.set_valign(gtk::Align::Start);
    bottom_right.append(&face_buttons_group(&page));
    bottom_right.append(&stick_group(&page, "rstick", "Right Stick", Stick::Right));
    let panning = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let panning_label = gtk::Label::new(Some("Mouse panning"));
    let configure_panning = gtk::Button::with_label("Configure");
    panning.append(&panning_label);
    panning.append(&configure_panning);
    bottom_right.append(&panning);
    page.register_group("bottom_right", &bottom_right);
    body.attach(&bottom_right, 4, 0, 1, 2);

    column.append(&body);

    // --- Footer -----------------------------------------------------------
    let footer = gtk::Box::new(gtk::Orientation::Horizontal, 12);
    footer.set_valign(gtk::Align::End);

    let console_mode = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let console_label = gtk::Label::new(Some("Console Mode"));
    console_label.set_xalign(0.0);
    let modes = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    let docked = gtk::CheckButton::with_label("Docked");
    let handheld = gtk::CheckButton::with_label("Handheld");
    handheld.set_group(Some(&docked));
    docked.set_active(
        *common::settings::values().use_docked_mode.get_value()
            == common::settings_enums::ConsoleMode::Docked,
    );
    modes.append(&docked);
    modes.append(&handheld);
    console_mode.append(&console_label);
    console_mode.append(&modes);
    footer.append(&console_mode);

    let vibration_box = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let vibration = gtk::CheckButton::with_label("Vibration");
    vibration.set_active(state.borrow().vibration_enabled);
    let configure_vibration = gtk::Button::with_label("Configure");
    vibration_box.append(&vibration);
    vibration_box.append(&configure_vibration);
    footer.append(&vibration_box);

    let motion_box = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let motion = gtk::CheckButton::with_label("Motion");
    motion.set_active(*common::settings::values().motion_enabled.get_value());
    let configure_motion = gtk::Button::with_label("Configure");
    motion_box.append(&motion);
    motion_box.append(&configure_motion);
    footer.append(&motion_box);

    // "Connected  1 2 3 4 5 6 7 8" over a row of checkboxes.
    let connected_strip = gtk::Grid::new();
    connected_strip.set_column_spacing(4);
    connected_strip.set_hexpand(true);
    connected_strip.set_halign(gtk::Align::Center);
    let connected_label = gtk::Label::new(Some("Connected"));
    connected_label.set_xalign(0.0);
    connected_strip.attach(&connected_label, 0, 0, 1, 1);
    let controllers_label = gtk::Label::new(Some("Controllers"));
    controllers_label.set_xalign(0.0);
    connected_strip.attach(&controllers_label, 0, 1, 1, 1);
    for slot in 0..super::configure_input::NUM_PLAYERS {
        let number = gtk::Label::new(Some(&(slot + 1).to_string()));
        connected_strip.attach(&number, slot as i32 + 1, 0, 1, 1);
        let check = gtk::CheckButton::new();
        check.set_active(player_input(slot).connected);
        // Upstream drives these from the other players' pages; the current
        // player's own box mirrors "Connect Controller" above.
        check.set_sensitive(false);
        connected_strip.attach(&check, slot as i32 + 1, 1, 1, 1);
    }
    footer.append(&connected_strip);

    let actions = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let defaults = gtk::Button::with_label("Defaults");
    let clear = gtk::Button::with_label("Clear");
    actions.append(&defaults);
    actions.append(&clear);
    footer.append(&actions);

    column.append(&footer);

    // --- Behaviour --------------------------------------------------------

    // Upstream `UpdateMappingWithDefaults`: selecting a real device wipes the
    // current mapping and refills it from that device's defaults. Row 0 ("Any")
    // is left alone, exactly as upstream's early return does.
    {
        let page = Rc::clone(&page);
        let devices = input_devices.clone();
        let subsystem = Rc::clone(&input_subsystem);
        input_device.connect_selected_notify(move |combo| {
            let selected = combo.selected() as usize;
            if selected == 0 {
                return;
            }
            let Some(device) = devices.get(selected) else {
                return;
            };
            apply_device_defaults(&page, &subsystem.borrow(), device);
            page.update_ui();
        });
    }

    // "Clear" empties every binding; "Defaults" re-applies the selected
    // device's mapping, matching upstream's `ClearAll` / `RestoreDefaults`.
    {
        let page = Rc::clone(&page);
        clear.connect_clicked(move |_| {
            {
                let mut state = page.state.borrow_mut();
                state.buttons.iter_mut().for_each(|b| b.clear());
                state.analogs.iter_mut().for_each(|a| a.clear());
                state.motions.iter_mut().for_each(|m| m.clear());
            }
            page.update_ui();
        });
    }
    {
        let page = Rc::clone(&page);
        let devices = input_devices.clone();
        let subsystem = Rc::clone(&input_subsystem);
        let input_device = input_device.clone();
        defaults.connect_clicked(move |_| {
            let selected = input_device.selected() as usize;
            if let Some(device) = devices.get(selected).filter(|_| selected != 0) {
                apply_device_defaults(&page, &subsystem.borrow(), device);
                page.update_ui();
            }
        });
    }

    {
        let page = Rc::clone(&page);
        let preview_holder = preview_holder.clone();
        controller_type.connect_selected_notify(move |combo| {
            let selected = CONTROLLER_TYPES
                .get(combo.selected() as usize)
                .map(|(kind, _)| *kind)
                .unwrap_or(ControllerType::ProController);
            while let Some(child) = preview_holder.first_child() {
                preview_holder.remove(&child);
            }
            preview_holder.append(&super::controller_preview::build(selected));
            page.update_controller_layout(selected);
        });
    }

    // Profile and sub-dialog actions still need the profile store.
    for (button, action) in [
        (&save_profile, "Save profile"),
        (&new_profile, "New profile"),
        (&delete_profile, "Delete profile"),
        (&configure_panning, "Configure mouse panning"),
        (&configure_vibration, "Configure vibration"),
        (&configure_motion, "Configure motion"),
    ] {
        let action = action.to_string();
        let player_number = index + 1;
        button.connect_clicked(move |_| {
            log::info!("Player {player_number}: {action} requested (not yet implemented)");
        });
    }

    // Paint the initial state: labels first, then the per-type layout.
    page.update_ui();
    page.update_controller_layout(initial_type);

    // The grid is dense enough that a narrow dialog would otherwise force the
    // window taller than the screen; scrolling keeps the button row reachable.
    let scroller = gtk::ScrolledWindow::builder()
        .hscrollbar_policy(gtk::PolicyType::Automatic)
        .vscrollbar_policy(gtk::PolicyType::Automatic)
        .hexpand(true)
        .vexpand(true)
        .propagate_natural_width(false)
        .propagate_natural_height(false)
        .child(&column)
        .build();

    Page::new(&format!("Player {}", index + 1), scroller, move || {
        // Widgets hold only a weak reference to their size group, so it has to
        // stay owned for the page's lifetime.
        let _keep_alive = &header_captions;

        let is_connected = connected.is_active();
        let controller = CONTROLLER_TYPES
            .get(controller_type.selected() as usize)
            .map(|(t, _)| *t)
            .unwrap_or(ControllerType::ProController);
        let vibrates = vibration.is_active();
        let uses_motion = motion.is_active();
        let is_docked = docked.is_active();

        {
            let mut values = common::settings::values_mut();
            let players = values.players.get_value_mut();
            if let Some(slot) = players.get_mut(index) {
                // Upstream `ApplyConfiguration` copies the whole working
                // controller back, bindings included — without this the
                // mappings picked in the dialog would be lost on OK.
                let edited = state.borrow();
                slot.buttons = edited.buttons.clone();
                slot.analogs = edited.analogs.clone();
                slot.motions = edited.motions.clone();

                slot.connected = is_connected;
                slot.controller_type = controller;
                slot.vibration_enabled = vibrates;
            }
            values.motion_enabled.set_value(uses_motion);
            values.use_docked_mode.set_value(if is_docked {
                common::settings_enums::ConsoleMode::Docked
            } else {
                common::settings_enums::ConsoleMode::Handheld
            });
        }
    })
}

/// Upstream `ConfigureInputPlayer::UpdateMappingWithDefaults`.
///
/// Clears the current mapping, then writes the device's own defaults —
/// `GetButtonMappingForDevice` walks SDL's game-controller bindings, so an
/// Xbox pad comes back with `Button 0`, `Axis 1+` and so on rather than
/// `[not set]`.
fn apply_device_defaults(
    page: &PlayerPage,
    subsystem: &input_common::InputSubsystem,
    device: &common::param_package::ParamPackage,
) {
    let buttons = subsystem.get_button_mapping_for_device(device);
    let analogs = subsystem.get_analog_mapping_for_device(device);
    let motions = subsystem.get_motion_mapping_for_device(device);

    let mut state = page.state.borrow_mut();
    state.buttons.iter_mut().for_each(|b| b.clear());
    state.analogs.iter_mut().for_each(|a| a.clear());
    state.motions.iter_mut().for_each(|m| m.clear());

    for (index, param) in buttons {
        if let Some(slot) = state.buttons.get_mut(index as usize) {
            *slot = param.serialize();
        }
    }
    for (index, param) in analogs {
        if let Some(slot) = state.analogs.get_mut(index as usize) {
            *slot = param.serialize();
        }
    }
    for (index, param) in motions {
        if let Some(slot) = state.motions.get_mut(index as usize) {
            *slot = param.serialize();
        }
    }
}

/// Give the group frames the light fill Qt's Fusion style paints behind a
/// `QGroupBox`, so the binding clusters read as boxes rather than bare lines.
fn install_group_style() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let provider = gtk::CssProvider::new();
        provider.load_from_data(
            "frame.input-group > border { \
                 background-color: alpha(currentColor, 0.06); \
                 border-radius: 4px; \
             }",
        );
        if let Some(display) = gtk::gdk::Display::default() {
            gtk::style_context_add_provider_for_display(
                &display,
                &provider,
                gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
            );
        }
    });
}


/// Which analog stick a [`stick_group`] renders.
#[derive(Clone, Copy)]
enum Stick {
    Left,
    Right,
}

/// Read player `index`'s stored input configuration.
fn player_input(index: usize) -> PlayerInput {
    common::settings::values()
        .players
        .get_value()
        .get(index)
        .cloned()
        .unwrap_or_default()
}

/// The host mapping bound to `button`, or `[not set]`.
///
/// Upstream renders the stored engine string through
/// `ButtonToText`, which turns `"engine:sdl,button:9,..."` into `"Button 9"`.
fn button_text(player: &PlayerInput, button: native_button::Values) -> String {
    player
        .buttons
        .get(button as usize)
        .filter(|s| !s.is_empty())
        .map(|s| button_to_text(s))
        .unwrap_or_else(|| NOT_SET.to_string())
}

/// Render an engine mapping string the way upstream's `ButtonToText` does.
///
/// The stored form is a comma-separated `key:value` list; the displayed form is
/// `"Button N"` / `"Axis N±"` / `"Hat N Direction"` depending on which keys are
/// present. Anything unrecognised falls back to `[not set]`, matching upstream's
/// `if (!param.Has("engine")) return tr("[not set]")`.
pub fn button_to_text(param: &str) -> String {
    let fields: std::collections::HashMap<&str, &str> = param
        .split(',')
        .filter_map(|pair| pair.split_once(':'))
        .collect();

    if !fields.contains_key("engine") {
        return NOT_SET.to_string();
    }
    if let Some(button) = fields.get("button") {
        return format!("Button {button}");
    }
    if let Some(axis) = fields.get("axis") {
        // `direction` is "+" or "-" upstream; a missing one means the whole axis.
        let direction = fields.get("direction").copied().unwrap_or("");
        return format!("Axis {axis}{direction}");
    }
    if let Some(hat) = fields.get("hat") {
        let direction = fields.get("direction").copied().unwrap_or("");
        return format!("Hat {hat} {direction}");
    }
    NOT_SET.to_string()
}

/// A [`shared_widget::group`] with its padding trimmed.
///
/// The binding grid stacks two groups per column plus a header and a footer;
/// at the default group padding that overflows the dialog height that upstream's
/// `adjustSize()` settles on, so the whole page would scroll. Qt's grid is
/// tighter than GTK's defaults, and this recovers the difference.
fn compact_group(title: &str) -> (gtk::Box, gtk::Box, gtk::Label) {
    let (outer, content) = w::group(title);
    outer.set_margin_bottom(2);
    content.set_spacing(2);
    content.set_margin_top(4);
    content.set_margin_bottom(4);

    // `w::group` puts the caption first and the frame second; both handles are
    // needed here — the caption to rename per controller type, the frame to
    // carry the Fusion-style fill.
    let caption = outer
        .first_child()
        .and_then(|child| child.downcast::<gtk::Label>().ok())
        .unwrap_or_else(|| gtk::Label::new(Some(title)));
    if let Some(frame) = content.parent() {
        frame.add_css_class("input-group");
    }

    (outer, content, caption)
}

/// One of the four directions of an analog stick.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    /// The axis (`x` / `y`) this direction moves along, and its sign.
    fn axis_and_sign(self) -> (&'static str, &'static str) {
        match self {
            Direction::Up => ("y", "+"),
            Direction::Down => ("y", "-"),
            Direction::Left => ("x", "-"),
            Direction::Right => ("x", "+"),
        }
    }
}

/// Render one direction of a stick mapping — upstream's `AnalogToText`.
///
/// A stick param binds both axes at once
/// (`"engine:sdl,axis_x:0,axis_y:1,..."`), so each of the four direction
/// buttons displays its own axis with the sign that direction moves in:
/// left stick "Up" shows `Axis 1+`, "Left" shows `Axis 0-`.
///
/// A stick can also be bound button-per-direction, in which case the param
/// carries `up`/`down`/`left`/`right` sub-params; upstream then recurses into
/// `ButtonToText`. Anything else renders `[not set]`.
pub fn analog_to_text(param: &str, direction: Direction) -> String {
    let fields: std::collections::HashMap<&str, &str> = param
        .split(',')
        .filter_map(|pair| pair.split_once(':'))
        .collect();

    if !fields.contains_key("engine") {
        return NOT_SET.to_string();
    }

    // Button-per-direction binding.
    let direction_key = match direction {
        Direction::Up => "up",
        Direction::Down => "down",
        Direction::Left => "left",
        Direction::Right => "right",
    };
    if let Some(sub) = fields.get(direction_key) {
        return button_to_text(sub);
    }

    let (axis, sign) = direction.axis_and_sign();
    match fields.get(format!("axis_{axis}").as_str()) {
        Some(index) => format!("Axis {index}{sign}"),
        None => NOT_SET.to_string(),
    }
}

/// A `label` over a binding button — the unit the whole grid is built from.
///
/// The button is registered with `page` so `update_ui` can re-label it when the
/// mapping changes, the way upstream keeps every binding button in `button_map`.
fn binding_block(page: &PlayerPage, label: &str, button: native_button::Values) -> gtk::Box {
    let (block, _) = titled_binding_block(page, label, button);
    block
}

/// [`binding_block`], also handing back the caption so callers that rename it
/// per controller type (Plus → "Start / Pause", R → "Z") can keep the handle.
fn titled_binding_block(
    page: &PlayerPage,
    label: &str,
    button: native_button::Values,
) -> (gtk::Box, gtk::Label) {
    let block = gtk::Box::new(gtk::Orientation::Vertical, 2);
    block.set_halign(gtk::Align::Center);

    let caption = gtk::Label::new(Some(label));
    let widget = gtk::Button::with_label(NOT_SET);
    widget.set_width_request(BINDING_WIDTH);
    page.button_widgets
        .borrow_mut()
        .push((button as usize, widget.clone()));

    block.append(&caption);
    block.append(&widget);
    (block, caption)
}

/// One direction of a stick, bound to `analog` rather than a button.
fn analog_binding_block(
    page: &PlayerPage,
    label: &str,
    analog: native_analog::Values,
    direction: Direction,
) -> gtk::Box {
    let block = gtk::Box::new(gtk::Orientation::Vertical, 2);
    block.set_halign(gtk::Align::Center);

    let caption = gtk::Label::new(Some(label));
    let widget = gtk::Button::with_label(NOT_SET);
    widget.set_width_request(BINDING_WIDTH);
    page.analog_widgets
        .borrow_mut()
        .push((analog as usize, direction, widget.clone()));

    block.append(&caption);
    block.append(&widget);
    block
}

/// A "Motion N" block, bound to `Settings::NativeMotion` index `motion`.
fn motion_block(page: &PlayerPage, label: &str, motion: usize) -> gtk::Box {
    let block = gtk::Box::new(gtk::Orientation::Vertical, 2);
    block.set_halign(gtk::Align::Center);

    let caption = gtk::Label::new(Some(label));
    let widget = gtk::Button::with_label(NOT_SET);
    widget.set_width_request(BINDING_WIDTH);
    page.motion_widgets
        .borrow_mut()
        .push((motion, widget.clone()));

    block.append(&caption);
    block.append(&widget);
    block
}

/// A trigger block: binding button plus the analog-range slider beneath it,
/// as ZL / ZR carry in `configure_input_player.ui`.
fn trigger_block(
    page: &PlayerPage,
    label: &str,
    button: native_button::Values,
) -> (gtk::Box, gtk::Label) {
    let (block, caption) = titled_binding_block(page, label, button);
    let range = gtk::Scale::with_range(gtk::Orientation::Horizontal, 0.0, 100.0, 1.0);
    range.set_draw_value(false);
    range.set_value(50.0);
    range.set_width_request(BINDING_WIDTH);
    block.append(&range);
    (block, caption)
}

/// The Left/Right Stick group: four directions, the press binding, a modifier
/// range spin box, and the deadzone slider.
fn stick_group(page: &PlayerPage, key: &'static str, title: &str, stick: Stick) -> gtk::Box {
    let (outer, content, caption) = compact_group(title);
    page.titles.borrow_mut().insert(key, caption);

    let (analog, pressed, pressed_key) = match stick {
        Stick::Left => (
            native_analog::Values::LStick,
            native_button::Values::LStick,
            "lstick_pressed",
        ),
        Stick::Right => (
            native_analog::Values::RStick,
            native_button::Values::RStick,
            "rstick_pressed",
        ),
    };

    content.append(&analog_binding_block(page, "Up", analog, Direction::Up));

    let middle = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    middle.set_halign(gtk::Align::Center);
    middle.append(&analog_binding_block(page, "Left", analog, Direction::Left));
    middle.append(&analog_binding_block(
        page,
        "Right",
        analog,
        Direction::Right,
    ));
    content.append(&middle);

    content.append(&analog_binding_block(page, "Down", analog, Direction::Down));

    let bottom = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    bottom.set_halign(gtk::Align::Center);
    let (pressed_block, _) = titled_binding_block(page, "Pressed", pressed);
    page.register_group(pressed_key, &pressed_block);
    bottom.append(&pressed_block);

    let range_block = gtk::Box::new(gtk::Orientation::Vertical, 2);
    range_block.set_halign(gtk::Align::Center);
    range_block.append(&gtk::Label::new(Some("Range")));
    let range = gtk::SpinButton::with_range(50.0, 150.0, 1.0);
    range.set_value(95.0);
    range_block.append(&range);
    bottom.append(&range_block);
    content.append(&bottom);

    let deadzone_label = gtk::Label::new(Some("Deadzone: 15%"));
    content.append(&deadzone_label);
    let deadzone = gtk::Scale::with_range(gtk::Orientation::Horizontal, 0.0, 100.0, 1.0);
    deadzone.set_draw_value(false);
    deadzone.set_value(15.0);
    // Keep the caption in step, as upstream's `UpdateSliderText` does.
    let caption = deadzone_label.clone();
    deadzone.connect_value_changed(move |scale| {
        caption.set_text(&format!("Deadzone: {}%", scale.value() as i32));
    });
    content.append(&deadzone);

    outer
}

/// The D-Pad group: Up / Left-Right / Down.
fn dpad_group(page: &PlayerPage) -> gtk::Box {
    let (outer, content, _) = compact_group("D-Pad");

    content.append(&binding_block(page, "Up", native_button::Values::DUp));

    let middle = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    middle.set_halign(gtk::Align::Center);
    middle.append(&binding_block(page, "Left", native_button::Values::DLeft));
    middle.append(&binding_block(page, "Right", native_button::Values::DRight));
    content.append(&middle);

    content.append(&binding_block(page, "Down", native_button::Values::DDown));

    outer
}

/// The Face Buttons group: X on top, Y and A on the sides, B below.
fn face_buttons_group(page: &PlayerPage) -> gtk::Box {
    let (outer, content, _) = compact_group("Face Buttons");

    content.append(&binding_block(page, "X", native_button::Values::X));

    let middle = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    middle.set_halign(gtk::Align::Center);
    middle.append(&binding_block(page, "Y", native_button::Values::Y));
    middle.append(&binding_block(page, "A", native_button::Values::A));
    content.append(&middle);

    content.append(&binding_block(page, "B", native_button::Values::B));

    outer
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn button_to_text_renders_sdl_buttons() {
        assert_eq!(button_to_text("engine:sdl,button:9,guid:0,port:0"), "Button 9");
    }

    #[test]
    fn button_to_text_renders_axes_with_direction() {
        assert_eq!(
            button_to_text("engine:sdl,axis:1,direction:+,guid:0,port:0"),
            "Axis 1+"
        );
        assert_eq!(
            button_to_text("engine:sdl,axis:0,direction:-,guid:0,port:0"),
            "Axis 0-"
        );
    }

    #[test]
    fn button_to_text_reports_unmapped_inputs() {
        // Upstream returns "[not set]" whenever the param has no engine — an
        // empty or malformed mapping must not render as "Button " with no id.
        assert_eq!(button_to_text(""), NOT_SET);
        assert_eq!(button_to_text("button:3"), NOT_SET);
        assert_eq!(button_to_text("engine:sdl,port:0"), NOT_SET);
    }

    #[test]
    fn analog_to_text_splits_a_stick_param_into_four_directions() {
        // A single stick param binds both axes; each direction button shows
        // its own axis with the sign it moves in.
        let param = "engine:sdl,axis_x:0,axis_y:1,guid:0,port:0";
        assert_eq!(analog_to_text(param, Direction::Up), "Axis 1+");
        assert_eq!(analog_to_text(param, Direction::Down), "Axis 1-");
        assert_eq!(analog_to_text(param, Direction::Left), "Axis 0-");
        assert_eq!(analog_to_text(param, Direction::Right), "Axis 0+");
    }

    #[test]
    fn analog_to_text_follows_per_direction_button_bindings() {
        // A stick can also be bound one button per direction, in which case
        // upstream recurses into ButtonToText.
        let param = "engine:keyboard,up:engine:sdl+button:11,axis_x:0,axis_y:1";
        assert_eq!(analog_to_text(param, Direction::Up), NOT_SET);

        let param = "engine:keyboard,up:engine,axis_x:0,axis_y:1";
        assert_eq!(analog_to_text(param, Direction::Down), "Axis 1-");
    }

    #[test]
    fn analog_to_text_reports_unmapped_sticks() {
        assert_eq!(analog_to_text("", Direction::Up), NOT_SET);
        // Engine present but no axes bound: neither axis can be named.
        assert_eq!(analog_to_text("engine:sdl,port:0", Direction::Left), NOT_SET);
    }

    #[test]
    fn stick_directions_do_not_share_an_axis() {
        // Up/Down must ride the y axis and Left/Right the x axis; swapping
        // them would silently invert a user's stick.
        assert_eq!(Direction::Up.axis_and_sign().0, "y");
        assert_eq!(Direction::Down.axis_and_sign().0, "y");
        assert_eq!(Direction::Left.axis_and_sign().0, "x");
        assert_eq!(Direction::Right.axis_and_sign().0, "x");
        assert_ne!(Direction::Up.axis_and_sign().1, Direction::Down.axis_and_sign().1);
        assert_ne!(
            Direction::Left.axis_and_sign().1,
            Direction::Right.axis_and_sign().1
        );
    }

    #[test]
    fn a_single_joycon_hides_the_other_half() {
        // Upstream's Left Joycon layout hides the right shoulder, Plus/Home and
        // the whole right column; the Right Joycon hides their mirrors. Getting
        // this wrong shows a player controls their pad does not have.
        let left = hidden_groups(ControllerType::LeftJoycon);
        assert!(left.contains(&"bottom_right"));
        assert!(left.contains(&"shoulder_right"));
        assert!(left.contains(&"plus_home"));
        assert!(!left.contains(&"bottom_left"));

        let right = hidden_groups(ControllerType::RightJoycon);
        assert!(right.contains(&"bottom_left"));
        assert!(right.contains(&"shoulder_left"));
        assert!(right.contains(&"minus_screenshot"));
        assert!(!right.contains(&"bottom_right"));
    }

    #[test]
    fn only_detached_joycons_expose_sl_and_sr() {
        // SL/SR exist on the rail of a detached Joy-Con. Every other layout
        // hides both; a single Joy-Con keeps only its own side.
        for layout in [
            ControllerType::ProController,
            ControllerType::Handheld,
            ControllerType::GameCube,
        ] {
            let hidden = hidden_groups(layout);
            assert!(hidden.contains(&"slsr_left"), "{layout:?} should hide SL/SR left");
            assert!(hidden.contains(&"slsr_right"), "{layout:?} should hide SL/SR right");
        }
        assert!(hidden_groups(ControllerType::DualJoyconDetached).is_empty());
        assert!(hidden_groups(ControllerType::LeftJoycon).contains(&"slsr_right"));
        assert!(!hidden_groups(ControllerType::LeftJoycon).contains(&"slsr_left"));
    }

    #[test]
    fn a_gamecube_pad_disables_the_controls_it_lacks() {
        // No home button, no clickable sticks, and L is analog rather than a
        // digital button — upstream disables rather than hides these.
        let disabled = disabled_groups(ControllerType::GameCube);
        for name in ["home", "lstick_pressed", "rstick_pressed", "button_l"] {
            assert!(disabled.contains(&name), "{name} should be disabled");
        }
        assert!(disabled_groups(ControllerType::ProController).is_empty());
    }

    #[test]
    fn motion_groups_follow_the_halves_the_controller_has() {
        assert_eq!(motion_visibility(ControllerType::ProController), (true, false));
        assert_eq!(motion_visibility(ControllerType::LeftJoycon), (true, false));
        // The right Joy-Con is motion 2, not motion 1 — reusing motion 1 would
        // write the binding into the wrong slot.
        assert_eq!(motion_visibility(ControllerType::RightJoycon), (false, true));
        assert_eq!(motion_visibility(ControllerType::DualJoyconDetached), (true, true));
        assert_eq!(motion_visibility(ControllerType::GameCube), (false, false));
    }

    #[test]
    fn a_gamecube_pad_renames_its_shoulder_and_stick_groups() {
        let titles = group_titles(ControllerType::GameCube);
        let of = |key: &str| titles.iter().find(|(k, _)| *k == key).map(|(_, v)| *v);
        assert_eq!(of("plus"), Some("Start / Pause"));
        assert_eq!(of("lstick"), Some("Control Stick"));
        assert_eq!(of("rstick"), Some("C-Stick"));
        // The GameCube shoulders shift by one: its ZL slot is labelled L, and
        // the Switch R slot becomes Z.
        assert_eq!(of("zl"), Some("L"));
        assert_eq!(of("zr"), Some("R"));
        assert_eq!(of("r"), Some("Z"));
    }

    #[test]
    fn every_switch_layout_uses_the_switch_names() {
        for layout in [
            ControllerType::ProController,
            ControllerType::DualJoyconDetached,
            ControllerType::LeftJoycon,
            ControllerType::RightJoycon,
            ControllerType::Handheld,
        ] {
            let titles = group_titles(layout);
            assert!(titles.contains(&("lstick", "Left Stick")), "{layout:?}");
            assert!(titles.contains(&("plus", "Plus")), "{layout:?}");
        }
    }

    #[test]
    fn every_hideable_group_is_restored_before_the_hide_list_runs() {
        // Upstream re-shows `layout_show` first; any name that some layout
        // hides but that list omits would stay hidden forever once a user
        // switched away from that controller type.
        for layout in [
            ControllerType::ProController,
            ControllerType::DualJoyconDetached,
            ControllerType::LeftJoycon,
            ControllerType::RightJoycon,
            ControllerType::Handheld,
            ControllerType::GameCube,
        ] {
            for name in hidden_groups(layout) {
                assert!(
                    ALWAYS_SHOWN_GROUPS.contains(name),
                    "{name} is hidden by {layout:?} but never re-shown"
                );
            }
            for name in disabled_groups(layout) {
                assert!(
                    ALWAYS_ENABLED_GROUPS.contains(name),
                    "{name} is disabled by {layout:?} but never re-enabled"
                );
            }
        }
    }

    #[test]
    fn controller_type_rows_start_with_pro_controller() {
        // The default `PlayerInput::controller_type` is `ProController`; if it
        // were not row 0, a fresh profile would display the wrong type.
        assert_eq!(CONTROLLER_TYPES[0].0, ControllerType::ProController);
        assert_eq!(PlayerInput::default().controller_type, CONTROLLER_TYPES[0].0);
    }
}

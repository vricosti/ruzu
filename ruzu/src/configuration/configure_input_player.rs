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

/// Build one "Player N" tab — upstream `ConfigureInputPlayer` for index `index`.
pub fn page(index: usize) -> Page {
    let player = player_input(index);

    let column = gtk::Box::new(gtk::Orientation::Vertical, 8);
    column.set_margin_top(10);
    column.set_margin_bottom(10);
    column.set_margin_start(10);
    column.set_margin_end(10);

    // --- Header -----------------------------------------------------------
    let header = gtk::Box::new(gtk::Orientation::Horizontal, 12);

    let connect_box = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let connected = gtk::CheckButton::with_label("Connect Controller");
    connected.set_active(player.connected);
    let type_labels: Vec<&str> = CONTROLLER_TYPES.iter().map(|(_, l)| *l).collect();
    let controller_type = w::combo(
        &type_labels,
        CONTROLLER_TYPES
            .iter()
            .position(|(t, _)| *t == player.controller_type)
            .unwrap_or(0) as u32,
    );
    connect_box.append(&connected);
    connect_box.append(&controller_type);
    header.append(&connect_box);

    let device_box = gtk::Box::new(gtk::Orientation::Vertical, 4);
    device_box.set_hexpand(true);
    let device_label = gtk::Label::new(Some("Input Device"));
    device_label.set_xalign(0.0);
    // Upstream fills this from `InputCommon::InputSubsystem::GetInputDevices()`,
    // always with "Any" first.
    let input_device = w::combo(&["Any"], 0);
    device_box.append(&device_label);
    device_box.append(&input_device);
    header.append(&device_box);

    let profile_box = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let profile_label = gtk::Label::new(Some("Profile"));
    profile_label.set_xalign(0.0);
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

    column.append(&header);

    // --- Body grid --------------------------------------------------------
    let body = gtk::Grid::new();
    body.set_column_spacing(10);
    body.set_row_spacing(6);
    body.set_vexpand(true);

    // Column 0: Left Stick, then D-Pad.
    body.attach(&stick_group("Left Stick", &player, Stick::Left), 0, 0, 1, 1);
    body.attach(&dpad_group(&player), 0, 1, 1, 1);

    // Column 1: L and ZL (ZL carries a range slider, as in the .ui).
    let shoulder_left = gtk::Box::new(gtk::Orientation::Vertical, 6);
    shoulder_left.set_valign(gtk::Align::Start);
    shoulder_left.append(&binding_block("L", &button_text(&player, native_button::Values::L)));
    shoulder_left.append(&trigger_block(
        "ZL",
        &button_text(&player, native_button::Values::ZL),
    ));
    body.attach(&shoulder_left, 1, 0, 1, 1);

    // Column 2: Minus / Plus, Capture / Home, and the controller art below.
    let centre = gtk::Box::new(gtk::Orientation::Vertical, 6);
    centre.set_hexpand(true);
    let system_top = gtk::Box::new(gtk::Orientation::Horizontal, 12);
    system_top.set_halign(gtk::Align::Center);
    system_top.append(&binding_block(
        "Minus",
        &button_text(&player, native_button::Values::Minus),
    ));
    system_top.append(&binding_block(
        "Plus",
        &button_text(&player, native_button::Values::Plus),
    ));
    centre.append(&system_top);
    let system_bottom = gtk::Box::new(gtk::Orientation::Horizontal, 12);
    system_bottom.set_halign(gtk::Align::Center);
    system_bottom.append(&binding_block(
        "Capture",
        &button_text(&player, native_button::Values::Screenshot),
    ));
    system_bottom.append(&binding_block(
        "Home",
        &button_text(&player, native_button::Values::Home),
    ));
    centre.append(&system_bottom);
    centre.append(&controller_art());
    body.attach(&centre, 2, 0, 1, 2);

    // Column 3: R and ZR.
    let shoulder_right = gtk::Box::new(gtk::Orientation::Vertical, 6);
    shoulder_right.set_valign(gtk::Align::Start);
    shoulder_right.append(&binding_block(
        "R",
        &button_text(&player, native_button::Values::R),
    ));
    shoulder_right.append(&trigger_block(
        "ZR",
        &button_text(&player, native_button::Values::ZR),
    ));
    body.attach(&shoulder_right, 3, 0, 1, 1);

    // Column 4: Face Buttons, Right Stick, Mouse panning.
    body.attach(&face_buttons_group(&player), 4, 0, 1, 1);
    let right_column = gtk::Box::new(gtk::Orientation::Vertical, 6);
    right_column.append(&stick_group("Right Stick", &player, Stick::Right));
    let panning = gtk::Box::new(gtk::Orientation::Vertical, 4);
    let panning_label = gtk::Label::new(Some("Mouse panning"));
    let configure_panning = gtk::Button::with_label("Configure");
    panning.append(&panning_label);
    panning.append(&configure_panning);
    right_column.append(&panning);
    body.attach(&right_column, 4, 1, 1, 1);

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
    vibration.set_active(player.vibration_enabled);
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

    let motion_binding = gtk::Box::new(gtk::Orientation::Vertical, 4);
    motion_binding.set_halign(gtk::Align::Center);
    motion_binding.set_hexpand(true);
    let motion_label = gtk::Label::new(Some("Motion 1"));
    let motion_button = gtk::Button::with_label(
        player
            .motions
            .first()
            .filter(|s| !s.is_empty())
            .map(String::as_str)
            .unwrap_or(NOT_SET),
    );
    motion_binding.append(&motion_label);
    motion_binding.append(&motion_button);
    footer.append(&motion_binding);

    // "Connected  1 2 3 4 5 6 7 8" over a row of checkboxes.
    let connected_strip = gtk::Grid::new();
    connected_strip.set_column_spacing(4);
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

    // Profile / capture / sub-dialog actions all need `InputCommon` or the
    // profile store; log rather than half-applying them.
    for (button, action) in [
        (&save_profile, "Save profile"),
        (&new_profile, "New profile"),
        (&delete_profile, "Delete profile"),
        (&configure_panning, "Configure mouse panning"),
        (&configure_vibration, "Configure vibration"),
        (&configure_motion, "Configure motion"),
        (&defaults, "Restore defaults"),
        (&clear, "Clear bindings"),
    ] {
        let action = action.to_string();
        let player_number = index + 1;
        button.connect_clicked(move |_| {
            log::info!(
                "Player {player_number}: {action} requested (input subsystem not yet wired)"
            );
        });
    }

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
fn compact_group(title: &str) -> (gtk::Box, gtk::Box) {
    let (outer, content) = w::group(title);
    outer.set_margin_bottom(2);
    content.set_spacing(2);
    content.set_margin_top(4);
    content.set_margin_bottom(4);
    (outer, content)
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
fn binding_block(label: &str, binding: &str) -> gtk::Box {
    let block = gtk::Box::new(gtk::Orientation::Vertical, 2);
    block.set_halign(gtk::Align::Center);

    let caption = gtk::Label::new(Some(label));
    let button = gtk::Button::with_label(binding);
    button.set_width_request(BINDING_WIDTH);

    block.append(&caption);
    block.append(&button);
    block
}

/// A trigger block: binding button plus the analog-range slider beneath it,
/// as ZL / ZR carry in `configure_input_player.ui`.
fn trigger_block(label: &str, binding: &str) -> gtk::Box {
    let block = binding_block(label, binding);
    let range = gtk::Scale::with_range(gtk::Orientation::Horizontal, 0.0, 100.0, 1.0);
    range.set_draw_value(false);
    range.set_value(50.0);
    range.set_width_request(BINDING_WIDTH);
    block.append(&range);
    block
}

/// The Left/Right Stick group: four directions, the press binding, a modifier
/// range spin box, and the deadzone slider.
fn stick_group(title: &str, player: &PlayerInput, stick: Stick) -> gtk::Box {
    let (outer, content) = compact_group(title);

    let (analog, pressed) = match stick {
        Stick::Left => (native_analog::Values::LStick, native_button::Values::LStick),
        Stick::Right => (native_analog::Values::RStick, native_button::Values::RStick),
    };
    let mapping = player
        .analogs
        .get(analog as usize)
        .cloned()
        .unwrap_or_default();

    content.append(&binding_block(
        "Up",
        &analog_to_text(&mapping, Direction::Up),
    ));

    let middle = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    middle.set_halign(gtk::Align::Center);
    middle.append(&binding_block(
        "Left",
        &analog_to_text(&mapping, Direction::Left),
    ));
    middle.append(&binding_block(
        "Right",
        &analog_to_text(&mapping, Direction::Right),
    ));
    content.append(&middle);

    content.append(&binding_block(
        "Down",
        &analog_to_text(&mapping, Direction::Down),
    ));

    let bottom = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    bottom.set_halign(gtk::Align::Center);
    bottom.append(&binding_block("Pressed", &button_text(player, pressed)));

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
fn dpad_group(player: &PlayerInput) -> gtk::Box {
    let (outer, content) = compact_group("D-Pad");

    content.append(&binding_block(
        "Up",
        &button_text(player, native_button::Values::DUp),
    ));

    let middle = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    middle.set_halign(gtk::Align::Center);
    middle.append(&binding_block(
        "Left",
        &button_text(player, native_button::Values::DLeft),
    ));
    middle.append(&binding_block(
        "Right",
        &button_text(player, native_button::Values::DRight),
    ));
    content.append(&middle);

    content.append(&binding_block(
        "Down",
        &button_text(player, native_button::Values::DDown),
    ));

    outer
}

/// The Face Buttons group: X on top, Y and A on the sides, B below.
fn face_buttons_group(player: &PlayerInput) -> gtk::Box {
    let (outer, content) = compact_group("Face Buttons");

    content.append(&binding_block(
        "X",
        &button_text(player, native_button::Values::X),
    ));

    let middle = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    middle.set_halign(gtk::Align::Center);
    middle.append(&binding_block(
        "Y",
        &button_text(player, native_button::Values::Y),
    ));
    middle.append(&binding_block(
        "A",
        &button_text(player, native_button::Values::A),
    ));
    content.append(&middle);

    content.append(&binding_block(
        "B",
        &button_text(player, native_button::Values::B),
    ));

    outer
}

/// The controller illustration in the middle of the page — upstream's
/// `PlayerControlPreview` custom widget, which draws the controller with each
/// pressed input highlighted live.
///
/// Live input highlighting needs the polling subsystem; this draws the static
/// outline so the page's proportions match the Qt dialog.
fn controller_art() -> gtk::DrawingArea {
    let area = gtk::DrawingArea::new();
    area.set_content_width(320);
    area.set_content_height(190);
    area.set_hexpand(true);
    // Deliberately not `vexpand`: the page must fit the dialog without forcing
    // it taller, which would push the Cancel / OK row off-screen.
    area.set_valign(gtk::Align::Center);
    area.set_draw_func(|_, cr, width, height| {
        let (w, h) = (width as f64, height as f64);
        cr.set_line_width(2.0);
        cr.set_source_rgb(0.45, 0.45, 0.45);

        // Body: a rounded slab across the middle.
        let body_x = w * 0.14;
        let body_y = h * 0.22;
        let body_w = w * 0.72;
        let body_h = h * 0.42;
        rounded_rect(cr, body_x, body_y, body_w, body_h, h * 0.10);
        let _ = cr.stroke();

        // Grips: one arc down from each end of the body.
        for side in [0.0, 1.0] {
            let cx = body_x + body_w * (0.14 + 0.72 * side);
            cr.arc(cx, body_y + body_h, w * 0.11, 0.0, std::f64::consts::PI);
            let _ = cr.stroke();
        }

        // Sticks: left high, right low, as on a Pro Controller.
        cr.arc(w * 0.28, h * 0.36, h * 0.07, 0.0, std::f64::consts::TAU);
        let _ = cr.stroke();
        cr.arc(w * 0.66, h * 0.50, h * 0.07, 0.0, std::f64::consts::TAU);
        let _ = cr.stroke();

        // Face buttons: diamond on the right.
        for (dx, dy) in [(0.0, -0.06), (0.0, 0.06), (-0.045, 0.0), (0.045, 0.0)] {
            cr.arc(
                w * (0.78 + dx),
                h * (0.36 + dy),
                h * 0.028,
                0.0,
                std::f64::consts::TAU,
            );
            let _ = cr.stroke();
        }

        // D-Pad: cross on the left.
        let arm = h * 0.035;
        cr.rectangle(w * 0.36 - arm * 0.5, h * 0.50 - arm * 1.5, arm, arm * 3.0);
        cr.rectangle(w * 0.36 - arm * 1.5, h * 0.50 - arm * 0.5, arm * 3.0, arm);
        let _ = cr.stroke();
    });
    area
}

/// Stroke-friendly rounded rectangle path.
fn rounded_rect(cr: &gtk::cairo::Context, x: f64, y: f64, w: f64, h: f64, r: f64) {
    use std::f64::consts::PI;
    cr.new_sub_path();
    cr.arc(x + w - r, y + r, r, -PI / 2.0, 0.0);
    cr.arc(x + w - r, y + h - r, r, 0.0, PI / 2.0);
    cr.arc(x + r, y + h - r, r, PI / 2.0, PI);
    cr.arc(x + r, y + r, r, PI, 1.5 * PI);
    cr.close_path();
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
    fn controller_type_rows_start_with_pro_controller() {
        // The default `PlayerInput::controller_type` is `ProController`; if it
        // were not row 0, a fresh profile would display the wrong type.
        assert_eq!(CONTROLLER_TYPES[0].0, ControllerType::ProController);
        assert_eq!(PlayerInput::default().controller_type, CONTROLLER_TYPES[0].0);
    }
}

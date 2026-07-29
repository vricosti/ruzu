// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_mouse_panning.cpp`.

use std::cell::RefCell;
use std::rc::Rc;

use gtk::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MousePanningConfiguration {
    enabled: bool,
    x_sensitivity: u8,
    y_sensitivity: u8,
    deadzone_counterweight: u8,
    decay_strength: u8,
    min_decay: u8,
}

impl MousePanningConfiguration {
    fn current() -> Self {
        let values = common::settings::values();
        Self {
            enabled: *values.mouse_panning.get_value(),
            x_sensitivity: *values.mouse_panning_x_sensitivity.get_value(),
            y_sensitivity: *values.mouse_panning_y_sensitivity.get_value(),
            deadzone_counterweight: *values.mouse_panning_deadzone_counterweight.get_value(),
            decay_strength: *values.mouse_panning_decay_strength.get_value(),
            min_decay: *values.mouse_panning_min_decay.get_value(),
        }
    }

    fn defaults() -> Self {
        let values = common::settings::values();
        Self {
            // Upstream's SetDefaultConfiguration deliberately leaves Enable
            // unchanged.
            enabled: *values.mouse_panning.get_value(),
            x_sensitivity: *values.mouse_panning_x_sensitivity.get_default(),
            y_sensitivity: *values.mouse_panning_y_sensitivity.get_default(),
            deadzone_counterweight: *values.mouse_panning_deadzone_counterweight.get_default(),
            decay_strength: *values.mouse_panning_decay_strength.get_default(),
            min_decay: *values.mouse_panning_min_decay.get_default(),
        }
    }
}

/// Present upstream's `ConfigureMousePanning` dialog.
pub fn present(
    source: &impl IsA<gtk::Widget>,
    _input_subsystem: Rc<RefCell<input_common::InputSubsystem>>,
    right_stick_deadzone: f32,
    right_stick_range: f32,
) {
    let window = gtk::Window::builder()
        .title("Configure mouse panning")
        .modal(true)
        .resizable(false)
        .default_width(650)
        .build();
    if let Some(parent) = source.root().and_downcast::<gtk::Window>() {
        window.set_transient_for(Some(&parent));
    }

    let content = gtk::Box::new(gtk::Orientation::Vertical, 10);
    content.set_margin_top(12);
    content.set_margin_bottom(12);
    content.set_margin_start(12);
    content.set_margin_end(12);

    let configuration = MousePanningConfiguration::current();
    let enable = gtk::CheckButton::with_label("Enable mouse panning");
    enable.set_tooltip_text(Some(
        "Can be toggled via a hotkey. Default hotkey is Ctrl + F9",
    ));
    enable.set_active(configuration.enabled);
    content.append(&enable);

    let groups = gtk::Box::new(gtk::Orientation::Horizontal, 10);
    let (sensitivity, sensitivity_grid) = group_grid("Sensitivity");
    let x_sensitivity = percent_spin(configuration.x_sensitivity, 1, 100);
    let y_sensitivity = percent_spin(configuration.y_sensitivity, 1, 100);
    attach_spin_row(&sensitivity_grid, 0, "Horizontal", &x_sensitivity);
    attach_spin_row(&sensitivity_grid, 1, "Vertical", &y_sensitivity);
    groups.append(&sensitivity);

    let (counterweight, counterweight_grid) = group_grid("Deadzone counterweight");
    counterweight.set_tooltip_text(Some("Counteracts a game's built-in deadzone"));
    let deadzone_counterweight = percent_spin(configuration.deadzone_counterweight, 0, 100);
    attach_spin_row(&counterweight_grid, 0, "Deadzone", &deadzone_counterweight);
    groups.append(&counterweight);

    let (decay, decay_grid) = group_grid("Stick decay");
    let decay_strength = percent_spin(configuration.decay_strength, 0, 100);
    let min_decay = percent_spin(configuration.min_decay, 0, 100);
    attach_spin_row(&decay_grid, 0, "Strength", &decay_strength);
    attach_spin_row(&decay_grid, 1, "Minimum", &min_decay);
    groups.append(&decay);
    content.append(&groups);

    let mouse_enabled = *common::settings::values().mouse_enabled.get_value();
    let warning = gtk::Label::new(Some(&warning_text(
        right_stick_deadzone,
        right_stick_range,
        mouse_enabled,
    )));
    warning.set_xalign(0.0);
    warning.set_wrap(true);
    content.append(&warning);

    let actions = gtk::Box::new(gtk::Orientation::Horizontal, 8);
    let defaults = gtk::Button::with_label("Default");
    let spacer = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    spacer.set_hexpand(true);
    let cancel = gtk::Button::with_label("Cancel");
    let ok = gtk::Button::with_label("OK");
    ok.add_css_class("suggested-action");
    actions.append(&defaults);
    actions.append(&spacer);
    actions.append(&cancel);
    actions.append(&ok);
    content.append(&actions);
    window.set_child(Some(&content));

    {
        let x_sensitivity = x_sensitivity.clone();
        let y_sensitivity = y_sensitivity.clone();
        let deadzone_counterweight = deadzone_counterweight.clone();
        let decay_strength = decay_strength.clone();
        let min_decay = min_decay.clone();
        defaults.connect_clicked(move |_| {
            let defaults = MousePanningConfiguration::defaults();
            x_sensitivity.set_value(defaults.x_sensitivity.into());
            y_sensitivity.set_value(defaults.y_sensitivity.into());
            deadzone_counterweight.set_value(defaults.deadzone_counterweight.into());
            decay_strength.set_value(defaults.decay_strength.into());
            min_decay.set_value(defaults.min_decay.into());
        });
    }
    {
        let window = window.downgrade();
        cancel.connect_clicked(move |_| {
            if let Some(window) = window.upgrade() {
                window.close();
            }
        });
    }
    {
        let window = window.downgrade();
        ok.connect_clicked(move |button| {
            let configuration = MousePanningConfiguration {
                enabled: enable.is_active(),
                x_sensitivity: x_sensitivity.value_as_int() as u8,
                y_sensitivity: y_sensitivity.value_as_int() as u8,
                deadzone_counterweight: deadzone_counterweight.value_as_int() as u8,
                decay_strength: decay_strength.value_as_int() as u8,
                min_decay: min_decay.value_as_int() as u8,
            };

            if apply_configuration(configuration).is_err() {
                crate::gtk_compat::show_message(
                    window.upgrade().as_ref(),
                    "Emulated mouse is enabled",
                    "Real mouse input and mouse panning are incompatible. Please disable the \
                     emulated mouse in input advanced settings to allow mouse panning.",
                );
                return;
            }

            if let Some(window) = window.upgrade() {
                window.close();
            } else {
                button.set_sensitive(false);
            }
        });
    }

    window.present();
}

fn apply_configuration(configuration: MousePanningConfiguration) -> Result<(), ()> {
    let mut values = common::settings::values_mut();
    values.mouse_panning.set_value(configuration.enabled);
    values
        .mouse_panning_x_sensitivity
        .set_value(configuration.x_sensitivity);
    values
        .mouse_panning_y_sensitivity
        .set_value(configuration.y_sensitivity);
    values
        .mouse_panning_deadzone_counterweight
        .set_value(configuration.deadzone_counterweight);
    values
        .mouse_panning_decay_strength
        .set_value(configuration.decay_strength);
    values
        .mouse_panning_min_decay
        .set_value(configuration.min_decay);

    if *values.mouse_enabled.get_value() && *values.mouse_panning.get_value() {
        values.mouse_panning.set_value(false);
        return Err(());
    }
    Ok(())
}

fn warning_text(deadzone: f32, range: f32, mouse_enabled: bool) -> String {
    if mouse_enabled {
        return "Emulated mouse is enabled. This is incompatible with mouse panning.".to_string();
    }
    if deadzone > 0.0 || range != 1.0 {
        return format!(
            "Mouse panning works better with a deadzone of 0% and a range of 100%.\n\
             Current values are {}% and {}% respectively.",
            (deadzone * 100.0) as i32,
            (range * 100.0) as i32
        );
    }
    String::new()
}

fn group_grid(title: &str) -> (gtk::Frame, gtk::Grid) {
    let frame = gtk::Frame::new(Some(title));
    let grid = gtk::Grid::new();
    grid.set_row_spacing(8);
    grid.set_column_spacing(8);
    grid.set_margin_top(8);
    grid.set_margin_bottom(8);
    grid.set_margin_start(8);
    grid.set_margin_end(8);
    frame.set_child(Some(&grid));
    (frame, grid)
}

fn percent_spin(value: u8, minimum: u8, maximum: u8) -> gtk::SpinButton {
    let spin = gtk::SpinButton::with_range(minimum.into(), maximum.into(), 1.0);
    spin.set_value(value.into());
    spin.set_width_chars(5);
    spin
}

fn attach_spin_row(grid: &gtk::Grid, row: i32, text: &str, spin: &gtk::SpinButton) {
    let label = gtk::Label::new(Some(text));
    label.set_xalign(0.0);
    grid.attach(&label, 0, row, 1, 1);

    let holder = gtk::Box::new(gtk::Orientation::Horizontal, 2);
    holder.append(spin);
    holder.append(&gtk::Label::new(Some("%")));
    grid.attach(&holder, 1, row, 1, 1);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn warning_matches_upstream_priority_and_percent_conversion() {
        assert_eq!(warning_text(0.0, 1.0, false), "");
        assert_eq!(
            warning_text(0.15, 0.8, false),
            "Mouse panning works better with a deadzone of 0% and a range of 100%.\n\
             Current values are 15% and 80% respectively."
        );
        assert_eq!(
            warning_text(0.15, 0.8, true),
            "Emulated mouse is enabled. This is incompatible with mouse panning."
        );
    }
}

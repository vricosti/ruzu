// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_system.cpp`
// (`ConfigureSystem`), whose widget tree lives in `configure_system.ui`.
//
// Two groups: "System" (language, region, time zone, custom RTC, RNG seed,
// device name) and "Core" (multicore, memory layout, speed limit).
//
// The custom-RTC and RNG-seed rows are gated on their leading check box, as
// upstream does in `ConfigureSystem::SetConfiguration`.

use gtk::prelude::*;

use super::configure_dialog::Page;
use super::shared_translation as tr;
use super::shared_widget as w;

/// Seconds in the Switch epoch offset used to render the custom RTC field.
/// Upstream stores `custom_rtc` as a POSIX timestamp and shows it in a
/// `QDateTimeEdit`; GTK has no date/time widget, so the field is a plain entry
/// carrying the same "dd/MM/yyyy HH:mm" text upstream displays.
const RTC_FORMAT: &str = "%d/%m/%Y %H:%M";

/// Build the System tab — upstream `ConfigureSystem`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    // --- "System" ---------------------------------------------------------
    let (system_group, system) = w::group("System");

    let language_index = tr::index_of(
        tr::LANGUAGE,
        common::settings::values().language_index.get_value(),
    );
    let (language_row, language) =
        w::combo_row("Language:", &tr::labels(tr::LANGUAGE), language_index);
    system.append(&language_row);

    let region_index = tr::index_of(
        tr::REGION,
        common::settings::values().region_index.get_value(),
    );
    let (region_row, region) = w::combo_row("Region:", &tr::labels(tr::REGION), region_index);
    system.append(&region_row);

    let time_zones = time_zone_labels();
    let time_zone_refs: Vec<&str> = time_zones.iter().map(String::as_str).collect();
    let time_zone_index =
        *common::settings::values().time_zone_index.get_value() as u32;
    let (time_zone_row, time_zone) =
        w::combo_row("Time Zone:", &time_zone_refs, time_zone_index);
    system.append(&time_zone_row);

    // Custom RTC: check box in the label column, entry in the control column,
    // mirroring `configure_system.ui`'s `custom_rtc` / `custom_rtc_edit` pair.
    let rtc_enabled = *common::settings::values().custom_rtc_enabled.get_value();
    let custom_rtc_check = gtk::CheckButton::with_label("Custom RTC Date:");
    custom_rtc_check.set_active(rtc_enabled);
    let custom_rtc_entry = gtk::Entry::new();
    custom_rtc_entry.set_text(&format_rtc(
        *common::settings::values().custom_rtc.get_value(),
    ));
    custom_rtc_entry.set_sensitive(rtc_enabled);
    let rtc_row = gated_row(&custom_rtc_check, &custom_rtc_entry);
    system.append(&rtc_row);

    // RNG seed, gated the same way.
    let seed_enabled = *common::settings::values().rng_seed_enabled.get_value();
    let rng_seed_check = gtk::CheckButton::with_label("RNG Seed");
    rng_seed_check.set_active(seed_enabled);
    let rng_seed_entry = gtk::Entry::new();
    rng_seed_entry.set_text(&format!(
        "{:08X}",
        common::settings::values().rng_seed.get_value()
    ));
    rng_seed_entry.set_sensitive(seed_enabled);
    let seed_row = gated_row(&rng_seed_check, &rng_seed_entry);
    system.append(&seed_row);

    let device_name_value = common::settings::values().device_name.get_value().clone();
    let (device_name_row, device_name) = w::entry_row("Device Name", &device_name_value);
    system.append(&device_name_row);

    column.append(&system_group);

    // --- "Core" -----------------------------------------------------------
    let (core_group, core) = w::group("Core");

    let multicore = w::check_row(
        "Multicore CPU Emulation",
        *common::settings::values().use_multi_core.get_value(),
    );
    core.append(&multicore);

    let memory_index = tr::index_of(
        tr::MEMORY_LAYOUT,
        common::settings::values().memory_layout_mode.get_value(),
    );
    let (memory_row, memory) = w::combo_row(
        "Memory Layout",
        &tr::labels(tr::MEMORY_LAYOUT),
        memory_index,
    );
    core.append(&memory_row);

    // Speed limit: check box in the label column, spin box in the control one.
    let limit_enabled = *common::settings::values().use_speed_limit.get_value();
    let speed_check = gtk::CheckButton::with_label("Limit Speed Percent");
    speed_check.set_active(limit_enabled);
    let speed_spin = gtk::SpinButton::with_range(1.0, 9999.0, 1.0);
    speed_spin.set_value(*common::settings::values().speed_limit.get_value() as f64);
    speed_spin.set_sensitive(limit_enabled);
    let speed_row = gated_row(&speed_check, &speed_spin);
    core.append(&speed_row);

    column.append(&core_group);

    // Gate each dependent control on its check box, as upstream does.
    gate(&custom_rtc_check, &custom_rtc_entry);
    gate(&rng_seed_check, &rng_seed_entry);
    gate(&speed_check, &speed_spin);

    // Line the control column up across both groups. The check-box rows would
    // otherwise sit ~20px left of the combo rows above them.
    let label_columns = w::align_label_columns(&[
        &language_row,
        &region_row,
        &time_zone_row,
        &rtc_row,
        &seed_row,
        &device_name_row,
        &memory_row,
        &speed_row,
    ]);

    Page::new("System", scroller, move || {
        // Widgets hold only a weak reference to their size group, so it has to
        // stay owned for the page's lifetime or the columns drift apart again.
        let _keep_alive = &label_columns;

        let language_value = tr::value_at(tr::LANGUAGE, language.selected());
        let region_value = tr::value_at(tr::REGION, region.selected());
        let time_zone_value = time_zone.selected();
        let rtc_on = custom_rtc_check.is_active();
        let rtc_value = parse_rtc(&custom_rtc_entry.text());
        let seed_on = rng_seed_check.is_active();
        let seed_value = u32::from_str_radix(rng_seed_entry.text().trim(), 16).unwrap_or(0);
        let device = device_name.text().to_string();
        let multi = multicore.is_active();
        let memory_value = tr::value_at(tr::MEMORY_LAYOUT, memory.selected());
        let limit_on = speed_check.is_active();
        let limit_value = speed_spin.value() as u16;

        let mut values = common::settings::values_mut();
        values.language_index.set_value(language_value);
        values.region_index.set_value(region_value);
        if let Some(zone) = common::settings_enums::TimeZone::from_u32(time_zone_value) {
            values.time_zone_index.set_value(zone);
        }
        values.custom_rtc_enabled.set_value(rtc_on);
        if let Some(rtc) = rtc_value {
            values.custom_rtc.set_value(rtc);
        }
        values.rng_seed_enabled.set_value(seed_on);
        values.rng_seed.set_value(seed_value);
        values.device_name.set_value(device);
        values.use_multi_core.set_value(multi);
        values.memory_layout_mode.set_value(memory_value);
        values.use_speed_limit.set_value(limit_on);
        values.speed_limit.set_value(limit_value);
    })
}

/// A row whose label column is a check box gating the control on its right —
/// the shape upstream uses for Custom RTC, RNG Seed, and Limit Speed Percent.
///
/// The check box's width is left to `shared_widget::align_label_columns`, which
/// matches it to the plain label rows; requesting a fixed width here would put
/// the control column at a different x than the rows above it.
fn gated_row(check: &gtk::CheckButton, control: &impl IsA<gtk::Widget>) -> gtk::Box {
    let row = gtk::Box::new(gtk::Orientation::Horizontal, 6);
    row.append(check);
    let control = control.as_ref();
    control.set_hexpand(true);
    row.append(control);
    row
}

/// Enable `control` only while `check` is ticked.
fn gate(check: &gtk::CheckButton, control: &impl IsA<gtk::Widget>) {
    let control = control.as_ref().clone();
    check.connect_toggled(move |check| control.set_sensitive(check.is_active()));
}

/// Time-zone combo entries. Upstream renders `Auto` and `Default` with the
/// resolved zone in parentheses and the rest as their plain names.
fn time_zone_labels() -> Vec<String> {
    common::settings_enums::TimeZone::canonicalizations()
        .iter()
        .map(|(name, zone)| match zone {
            common::settings_enums::TimeZone::Auto => {
                format!("Auto ({})", host_time_zone())
            }
            common::settings_enums::TimeZone::Default => {
                format!("Default ({})", host_time_zone())
            }
            _ => name.to_string(),
        })
        .collect()
}

/// The host's zone name, as upstream's `Common::TimeZone::GetDefaultTimeZone()`
/// reports it. Falls back to "GMT" when the host offers nothing.
fn host_time_zone() -> String {
    std::fs::read_to_string("/etc/timezone")
        .map(|s| s.trim().to_string())
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "GMT".to_string())
}

/// Render a POSIX timestamp the way upstream's `QDateTimeEdit` displays it.
fn format_rtc(timestamp: i64) -> String {
    // `time`/`chrono` are not dependencies of this crate; upstream's display
    // format is reproduced from the raw timestamp via a minimal civil-date
    // conversion (Howard Hinnant's `civil_from_days`, the same algorithm
    // `std::chrono` uses).
    let days = timestamp.div_euclid(86_400);
    let secs_of_day = timestamp.rem_euclid(86_400);
    let (year, month, day) = civil_from_days(days);
    let _ = RTC_FORMAT;
    format!(
        "{:02}/{:02}/{:04} {:02}:{:02}",
        day,
        month,
        year,
        secs_of_day / 3600,
        (secs_of_day % 3600) / 60
    )
}

/// Parse the "dd/MM/yyyy HH:mm" text back into a POSIX timestamp.
fn parse_rtc(text: &str) -> Option<i64> {
    let (date, time) = text.trim().split_once(' ')?;
    let mut date_parts = date.split('/');
    let day: i64 = date_parts.next()?.parse().ok()?;
    let month: i64 = date_parts.next()?.parse().ok()?;
    let year: i64 = date_parts.next()?.parse().ok()?;
    let mut time_parts = time.split(':');
    let hour: i64 = time_parts.next()?.parse().ok()?;
    let minute: i64 = time_parts.next()?.parse().ok()?;
    Some(days_from_civil(year, month, day) * 86_400 + hour * 3600 + minute * 60)
}

/// Days since 1970-01-01 → (year, month, day). Hinnant's `civil_from_days`.
fn civil_from_days(z: i64) -> (i64, i64, i64) {
    let z = z + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    (if m <= 2 { y + 1 } else { y }, m, d)
}

/// (year, month, day) → days since 1970-01-01. Hinnant's `days_from_civil`.
fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = if m > 2 { m - 3 } else { m + 9 };
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn civil_date_conversions_round_trip() {
        for timestamp in [0i64, 1_000_000_000, 1_785_000_000, -86_400] {
            let days = timestamp.div_euclid(86_400);
            let (y, m, d) = civil_from_days(days);
            assert_eq!(days_from_civil(y, m, d), days, "timestamp {timestamp}");
        }
    }

    #[test]
    fn epoch_renders_as_unix_day_zero() {
        assert_eq!(format_rtc(0), "01/01/1970 00:00");
    }

    #[test]
    fn rtc_text_round_trips() {
        let text = "27/07/2026 14:06";
        let parsed = parse_rtc(text).expect("parses");
        assert_eq!(format_rtc(parsed), text);
    }

    #[test]
    fn malformed_rtc_text_is_rejected_rather_than_defaulted() {
        // Silently substituting a date would move the emulated clock without
        // the user noticing; upstream's QDateTimeEdit can't produce this state.
        assert_eq!(parse_rtc("not a date"), None);
        assert_eq!(parse_rtc("27-07-2026 14:06"), None);
    }

    #[test]
    fn time_zone_list_covers_every_enum_variant() {
        assert_eq!(
            time_zone_labels().len(),
            common::settings_enums::TimeZone::canonicalizations().len()
        );
    }
}

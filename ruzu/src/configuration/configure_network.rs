// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_network.cpp`
// (`ConfigureNetwork`), whose widget tree lives in `configure_network.ui`.
//
// A single "General" group with the network-interface picker. Upstream fills
// the combo from `Network::GetAvailableNetworkInterfaces()` with a leading
// "None" entry, and stores the *interface name* (not the index).

use gtk::prelude::*;

use super::configure_dialog::Page;
use super::shared_widget as w;

/// The first combo entry, meaning "no interface bound" — upstream inserts
/// `tr("None")` before the enumerated interfaces.
const NONE_ENTRY: &str = "None";

/// Build the Network tab — upstream `ConfigureNetwork`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    let (general_group, general) = w::group("General");

    let mut entries = vec![NONE_ENTRY.to_string()];
    entries.extend(available_network_interfaces());
    let entry_refs: Vec<&str> = entries.iter().map(String::as_str).collect();

    let current = common::settings::values()
        .network_interface
        .get_value()
        .clone();
    let selected = entries
        .iter()
        .position(|name| *name == current)
        .unwrap_or(0) as u32;

    let (interface_row, interface) = w::combo_row("Network Interface", &entry_refs, selected);
    general.append(&interface_row);

    column.append(&general_group);

    Page::new("Network", scroller, move || {
        let index = interface.selected() as usize;
        // Row 0 is "None", which upstream stores as an empty interface name.
        let name = if index == 0 {
            String::new()
        } else {
            entries.get(index).cloned().unwrap_or_default()
        };
        common::settings::values_mut()
            .network_interface
            .set_value(name);
    })
}

/// Host network interface names — upstream
/// `Network::GetAvailableNetworkInterfaces()`.
///
/// Reads them from `/sys/class/net`, which is the same set `getifaddrs` reports
/// on Linux. Returns an empty list on other platforms or if the directory can't
/// be read, leaving just the "None" entry — the same result upstream produces
/// when enumeration fails.
fn available_network_interfaces() -> Vec<String> {
    let Ok(entries) = std::fs::read_dir("/sys/class/net") else {
        return Vec::new();
    };
    let mut names: Vec<String> = entries
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.file_name().to_string_lossy().into_owned())
        // Upstream skips the loopback interface; it can't reach a LAN peer.
        .filter(|name| name != "lo")
        .collect();
    names.sort_unstable();
    names
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loopback_is_not_offered() {
        // Binding the emulated console to `lo` can never reach another host,
        // so upstream leaves it out of the picker.
        assert!(!available_network_interfaces().iter().any(|n| n == "lo"));
    }

    #[test]
    fn interfaces_are_sorted() {
        let names = available_network_interfaces();
        let mut sorted = names.clone();
        sorted.sort_unstable();
        assert_eq!(names, sorted);
    }
}

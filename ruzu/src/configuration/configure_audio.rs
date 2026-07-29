// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rust/GTK4 counterpart of
// `/home/vricosti/Dev/emulators/zuyu/src/yuzu/configuration/configure_audio.cpp`
// (`ConfigureAudio`), whose widget tree lives in `configure_audio.ui`.
//
// A single "Audio" group: output engine, output device, input device, sound
// output mode, volume slider, and the two mute toggles.
//
// The device lists are refreshed whenever the engine changes
// (`ConfigureAudio::UpdateAudioDevices`), because each sink enumerates its own
// devices.

use gtk::prelude::*;

use common::settings_enums::AudioEngine;

use super::configure_dialog::Page;
use super::shared_translation as tr;
use super::shared_widget as w;

/// The device entry meaning "let the sink pick" — upstream inserts `"auto"`
/// as the first row of both device combos.
const AUTO_DEVICE: &str = "auto";

/// Build the Audio tab — upstream `ConfigureAudio`.
pub fn page() -> Page {
    let (scroller, column) = w::page();

    let (group, content) = w::group("Audio");

    // Output engine. Upstream lists every compiled-in sink, `auto` first.
    let engines = audio_engine_labels();
    let engine_refs: Vec<&str> = engines.iter().map(String::as_str).collect();
    let engine_index = engines
        .iter()
        .position(|name| {
            name == common::settings::values()
                .sink_id
                .get_value()
                .canonicalize()
        })
        .unwrap_or(0) as u32;
    let (engine_row, engine) = w::combo_row("Output Engine:", &engine_refs, engine_index);
    content.append(&engine_row);

    let output_device_value = common::settings::values()
        .audio_output_device_id
        .get_value()
        .clone();
    let (output_row, output_device) = w::combo_row(
        "Output Device:",
        &[AUTO_DEVICE],
        if output_device_value == AUTO_DEVICE {
            0
        } else {
            0
        },
    );
    content.append(&output_row);

    let (input_row, input_device) = w::combo_row("Input Device:", &[AUTO_DEVICE], 0);
    content.append(&input_row);

    let mode_value = *common::settings::values().sound_index.get_value();
    let (mode_row, mode) = w::combo_row(
        "Sound Output Mode:",
        &tr::labels(tr::AUDIO_MODE),
        tr::index_of(tr::AUDIO_MODE, &mode_value),
    );
    content.append(&mode_row);

    let volume_value = *common::settings::values().volume.get_value();
    let (volume_row, volume, _) = w::percent_slider_row("Volume:", volume_value as f64, 0.0, 200.0);
    content.append(&volume_row);

    let mute = w::check_row(
        "Mute audio",
        *common::settings::values().audio_muted.get_value(),
    );
    content.append(&mute);

    let mute_background = w::check_row(
        "Mute audio when in background",
        crate::uisettings::with(|v| *v.mute_when_in_background.get_value()),
    );
    content.append(&mute_background);

    column.append(&group);

    // Upstream re-enumerates both device combos when the engine changes. The
    // sink registry (`AudioCore::Sink::GetDeviceListForSink`) is not reachable
    // from the dialog yet, so the lists stay at "auto"; log the intent so the
    // gap is visible rather than looking like the sink has one device.
    engine.connect_selected_notify(|_| {
        log::info!("Audio: engine changed (device enumeration not yet wired)");
    });

    Page::new("Audio", scroller, move || {
        let engine_name = engines
            .get(engine.selected() as usize)
            .cloned()
            .unwrap_or_else(|| AUTO_DEVICE.to_string());
        let output_name = combo_text(&output_device).unwrap_or_else(|| AUTO_DEVICE.to_string());
        let input_name = combo_text(&input_device).unwrap_or_else(|| AUTO_DEVICE.to_string());
        let mode_value = tr::value_at(tr::AUDIO_MODE, mode.selected());
        let volume_value = volume.value() as u8;
        let muted = mute.is_active();
        let muted_background = mute_background.is_active();

        {
            let mut values = common::settings::values_mut();
            if let Some(sink) = AudioEngine::from_string(&engine_name) {
                values.sink_id.set_value(sink);
            }
            values.audio_output_device_id.set_value(output_name);
            values.audio_input_device_id.set_value(input_name);
            values.sound_index.set_value(mode_value);
            values.volume.set_value(volume_value);
            values.audio_muted.set_value(muted);
        }
        crate::uisettings::with_mut(|v| v.mute_when_in_background.set_value(muted_background));
    })
}

/// Selected text of a `DropDown` backed by a `StringList`.
fn combo_text(dropdown: &gtk::DropDown) -> Option<String> {
    dropdown
        .model()
        .and_downcast::<gtk::StringList>()
        .and_then(|list| list.string(dropdown.selected()))
        .map(|s| s.to_string())
}

/// The audio sinks the build offers — upstream `AudioCore::Sink::GetSinkIDs()`,
/// which always starts with `auto`.
///
/// `oboe` is Android-only upstream and has no ruzu backend, so it is left out
/// rather than offered as a sink that cannot be selected successfully.
const AUDIO_ENGINES: &[AudioEngine] = &[
    AudioEngine::Auto,
    AudioEngine::Cubeb,
    AudioEngine::Sdl2,
    AudioEngine::Null,
];

fn audio_engine_labels() -> Vec<String> {
    AUDIO_ENGINES
        .iter()
        .map(|engine| engine.canonicalize().to_string())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn auto_is_the_first_engine() {
        // Upstream's sink list always leads with "auto"; the combo's default
        // selection depends on it.
        assert_eq!(audio_engine_labels()[0], "auto");
    }

    #[test]
    fn engine_labels_round_trip_through_from_string() {
        for label in audio_engine_labels() {
            assert!(
                AudioEngine::from_string(&label).is_some(),
                "engine label {label} is not parseable"
            );
        }
    }
}

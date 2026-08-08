use crate::sink::cubeb_sink::{is_cubeb_suitable, list_cubeb_sink_devices, CubebSink};
use crate::sink::null_sink::NullSink;
use crate::sink::sdl2_sink::{is_sdl_suitable, list_sdl_sink_devices, SDLSink};
use crate::sink::sink::{Sink, SinkBox};
use common::settings_enums::AudioEngine;
use log::{error, info};

fn get_output_sink_id(sink_id: AudioEngine) -> AudioEngine {
    if sink_id == AudioEngine::Auto {
        let selected = if is_cubeb_suitable() {
            AudioEngine::Cubeb
        } else if is_sdl_suitable() {
            AudioEngine::Sdl2
        } else {
            AudioEngine::Null
        };
        info!("audio_core: auto-selecting the {selected:?} backend");
        return selected;
    }

    let suitable = match sink_id {
        AudioEngine::Cubeb => is_cubeb_suitable(),
        AudioEngine::Sdl2 => is_sdl_suitable(),
        AudioEngine::Null => true,
        AudioEngine::Auto | AudioEngine::Oboe => false,
    };
    if suitable {
        sink_id
    } else {
        error!("audio_core: selected backend {sink_id:?} is not suitable, falling back to null");
        AudioEngine::Null
    }
}

pub fn get_sink_ids() -> Vec<AudioEngine> {
    // Keep the compiled sink registry visible regardless of runtime
    // suitability. `GetOutputSinkDetails` performs the fallback upstream.
    vec![AudioEngine::Cubeb, AudioEngine::Sdl2, AudioEngine::Null]
}

pub fn get_device_list_for_sink(sink_id: AudioEngine, capture: bool) -> Vec<String> {
    match get_output_sink_id(sink_id) {
        AudioEngine::Cubeb => list_cubeb_sink_devices(capture),
        AudioEngine::Sdl2 => list_sdl_sink_devices(capture),
        AudioEngine::Null => vec!["null".to_string()],
        AudioEngine::Auto | AudioEngine::Oboe => unreachable!(),
    }
}

pub fn create_sink_from_id(sink_id: AudioEngine, device_id: &str) -> SinkBox {
    match get_output_sink_id(sink_id) {
        AudioEngine::Cubeb => Box::new(CubebSink::new(device_id)) as Box<dyn Sink>,
        AudioEngine::Sdl2 => Box::new(SDLSink::new(device_id)) as Box<dyn Sink>,
        AudioEngine::Null => Box::new(NullSink::new(device_id)) as Box<dyn Sink>,
        AudioEngine::Auto | AudioEngine::Oboe => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sink_ids_follow_the_compiled_registry_order() {
        assert_eq!(
            get_sink_ids(),
            vec![AudioEngine::Cubeb, AudioEngine::Sdl2, AudioEngine::Null]
        );
    }
}

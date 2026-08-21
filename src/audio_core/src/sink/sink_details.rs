use crate::common::common::TARGET_SAMPLE_COUNT;
use crate::sink::cubeb_sink::{get_cubeb_latency, list_cubeb_sink_devices, CubebSink};
use crate::sink::null_sink::NullSink;
use crate::sink::sdl3_sink::{list_sdl_sink_devices, SDLSink};
use crate::sink::sink::{Sink, SinkBox};
use common::settings_enums::AudioEngine;
use log::{error, info};

fn get_output_sink_id(sink_id: AudioEngine) -> AudioEngine {
    if sink_id == AudioEngine::Auto {
        let selected = if get_cubeb_latency() > TARGET_SAMPLE_COUNT * 3 {
            AudioEngine::Sdl3
        } else {
            AudioEngine::Cubeb
        };
        info!("audio_core: auto-selecting the {selected:?} backend");
        return selected;
    }

    match sink_id {
        AudioEngine::Cubeb | AudioEngine::Sdl3 | AudioEngine::Null => sink_id,
        AudioEngine::Auto => unreachable!(),
        AudioEngine::Oboe => {
            error!("audio_core: invalid sink_id {sink_id:?}, falling back to null");
            AudioEngine::Null
        }
    }
}

pub fn get_sink_ids() -> Vec<AudioEngine> {
    vec![AudioEngine::Cubeb, AudioEngine::Sdl3, AudioEngine::Null]
}

pub fn get_device_list_for_sink(sink_id: AudioEngine, capture: bool) -> Vec<String> {
    match get_output_sink_id(sink_id) {
        AudioEngine::Cubeb => list_cubeb_sink_devices(capture),
        AudioEngine::Sdl3 => list_sdl_sink_devices(capture),
        AudioEngine::Null => vec!["null".to_string()],
        AudioEngine::Auto | AudioEngine::Oboe => unreachable!(),
    }
}

pub fn create_sink_from_id(sink_id: AudioEngine, device_id: &str) -> SinkBox {
    match get_output_sink_id(sink_id) {
        AudioEngine::Cubeb => Box::new(CubebSink::new(device_id)) as Box<dyn Sink>,
        AudioEngine::Sdl3 => Box::new(SDLSink::new(device_id)) as Box<dyn Sink>,
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
            vec![AudioEngine::Cubeb, AudioEngine::Sdl3, AudioEngine::Null]
        );
    }
}

use crate::sink::cubeb_sink::{is_cubeb_suitable, list_cubeb_sink_devices, CubebSink};
use crate::sink::null_sink::NullSink;
use crate::sink::oboe_sink::OboeSink;
use crate::sink::sdl2_sink::{is_sdl_suitable, list_sdl_sink_devices, SDLSink};
use crate::sink::sink::{Sink, SinkBox};
use common::settings_enums::AudioEngine;
use log::{error, info, warn};

pub fn get_sink_ids() -> Vec<AudioEngine> {
    let mut ids = Vec::new();
    if is_cubeb_suitable() {
        ids.push(AudioEngine::Cubeb);
    }
    if is_sdl_suitable() {
        ids.push(AudioEngine::Sdl2);
    }
    ids.push(AudioEngine::Null);
    ids
}

pub fn get_device_list_for_sink(sink_id: AudioEngine, capture: bool) -> Vec<String> {
    match sink_id {
        AudioEngine::Cubeb => list_cubeb_sink_devices(capture),
        AudioEngine::Sdl2 => list_sdl_sink_devices(capture),
        AudioEngine::Null => vec!["null".to_string()],
        AudioEngine::Auto => {
            if is_cubeb_suitable() {
                list_cubeb_sink_devices(capture)
            } else if is_sdl_suitable() {
                list_sdl_sink_devices(capture)
            } else {
                vec!["null".to_string()]
            }
        }
        AudioEngine::Oboe => {
            warn!("audio_core: Oboe backend is only available on Android");
            Vec::new()
        }
    }
}

pub fn create_sink_from_id(sink_id: AudioEngine, device_id: &str) -> SinkBox {
    match sink_id {
        AudioEngine::Auto => {
            if is_cubeb_suitable() {
                info!("audio_core: auto-selecting the cubeb backend");
                Box::new(CubebSink::new(device_id)) as Box<dyn Sink>
            } else if is_sdl_suitable() {
                info!("audio_core: auto-selecting the SDL2 backend");
                Box::new(SDLSink::new(device_id)) as Box<dyn Sink>
            } else {
                info!("audio_core: auto-selecting the null backend");
                Box::new(NullSink::new(device_id)) as Box<dyn Sink>
            }
        }
        AudioEngine::Cubeb => {
            if is_cubeb_suitable() {
                Box::new(CubebSink::new(device_id)) as Box<dyn Sink>
            } else {
                warn!("audio_core: cubeb not suitable, falling back to null");
                Box::new(NullSink::new(device_id)) as Box<dyn Sink>
            }
        }
        AudioEngine::Sdl2 => {
            if is_sdl_suitable() {
                Box::new(SDLSink::new(device_id)) as Box<dyn Sink>
            } else {
                warn!("audio_core: SDL2 not suitable, falling back to null");
                Box::new(NullSink::new(device_id)) as Box<dyn Sink>
            }
        }
        AudioEngine::Oboe => {
            warn!("audio_core: Oboe is only available on Android, falling back to null");
            Box::new(OboeSink::new()) as Box<dyn Sink>
        }
        AudioEngine::Null => Box::new(NullSink::new(device_id)) as Box<dyn Sink>,
    }
}

use crate::common::common::{TARGET_SAMPLE_COUNT, TARGET_SAMPLE_RATE};
use crate::sink::sink::{new_stream_handle, Sink, AUTO_DEVICE_NAME};
use crate::sink::sink_stream::{SinkStream, SinkStreamHandle, StreamType};
use crate::SharedSystem;
use log::{error, info, warn};
use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};
use sdl2::AudioSubsystem;
use std::sync::Arc;

struct SDLCallback {
    stream_handle: SinkStreamHandle,
    stream_type: StreamType,
    device_channels: u32,
}

impl AudioCallback for SDLCallback {
    type Channel = i16;

    fn callback(&mut self, output: &mut [i16]) {
        let num_channels = self.device_channels as usize;
        let frame_size = num_channels.max(1);
        let num_frames = if frame_size > 0 {
            output.len() / frame_size
        } else {
            output.len()
        };

        let mut stream = self.stream_handle.lock();
        if self.stream_type == StreamType::In {
            stream.process_audio_in(output, num_frames);
        } else {
            stream.process_audio_out_and_render(output, num_frames);
        }
    }
}

enum SDLDevice {
    Playback(AudioDevice<SDLCallback>),
    Capture(AudioDevice<SDLCallback>),
}

struct SDLStream {
    name: String,
    stream_type: StreamType,
    handle: SinkStreamHandle,
    _device: SDLDevice,
}

pub struct SDLSink {
    audio_subsystem: Option<AudioSubsystem>,
    output_device: String,
    input_device: String,
    device_channels: u32,
    system_channels: u32,
    streams: Vec<SDLStream>,
}

// Safety: SDL2 audio subsystem and devices contain raw pointers internally,
// but are designed to be used across threads. The C++ upstream also shares
// the SDL sink across threads freely.
unsafe impl Send for SDLSink {}
unsafe impl Sync for SDLSink {}

impl SDLSink {
    pub fn new(target_device_name: &str) -> Self {
        let sdl_context = sdl2::init();
        let audio_subsystem = match sdl_context {
            Ok(ctx) => match ctx.audio() {
                Ok(audio) => Some(audio),
                Err(e) => {
                    error!("SDL_InitSubSystem audio failed: {}", e);
                    None
                }
            },
            Err(e) => {
                error!("SDL_Init failed: {}", e);
                None
            }
        };

        let output_device = if target_device_name != AUTO_DEVICE_NAME
            && !target_device_name.is_empty()
        {
            target_device_name.to_string()
        } else {
            String::new()
        };

        Self {
            audio_subsystem,
            output_device,
            input_device: String::new(),
            device_channels: 2,
            system_channels: 2,
            streams: Vec::new(),
        }
    }
}

impl Sink for SDLSink {
    fn acquire_sink_stream(
        &mut self,
        system: SharedSystem,
        system_channels: u32,
        name: &str,
        stream_type: StreamType,
    ) -> SinkStreamHandle {
        self.system_channels = system_channels;

        let Some(ref audio) = self.audio_subsystem else {
            let mut stream = SinkStream::new(system, stream_type);
            stream.system_channels = system_channels;
            stream.device_channels = self.device_channels;
            stream.name = name.to_string();
            return new_stream_handle(stream);
        };

        let mut sink_stream = SinkStream::new(system, stream_type);
        sink_stream.system_channels = system_channels;
        sink_stream.device_channels = self.device_channels;
        sink_stream.name = name.to_string();
        let handle = new_stream_handle(sink_stream);

        let desired_spec = AudioSpecDesired {
            freq: Some(TARGET_SAMPLE_RATE as i32),
            channels: Some(self.device_channels as u8),
            samples: Some((TARGET_SAMPLE_COUNT * 2) as u16),
        };

        let device_name = if stream_type == StreamType::In {
            &self.input_device
        } else {
            &self.output_device
        };

        let device_name_opt: Option<&str> = if device_name.is_empty() {
            None
        } else {
            Some(device_name.as_str())
        };

        let stream_handle = handle.clone();
        let device_channels = self.device_channels;

        let sdl_device = if stream_type == StreamType::In {
            match AudioDevice::open_capture(audio, device_name_opt, &desired_spec, |obtained| {
                info!(
                    "Opening SDL stream {} type {:?} with: rate {} channels {} (system channels {}) samples {}",
                    name, stream_type, obtained.freq, obtained.channels, system_channels, obtained.samples
                );
                SDLCallback {
                    stream_handle,
                    stream_type,
                    device_channels,
                }
            }) {
                Ok(dev) => Some(SDLDevice::Capture(dev)),
                Err(e) => {
                    error!("Error opening SDL audio capture device: {}", e);
                    None
                }
            }
        } else {
            match AudioDevice::open_playback(audio, device_name_opt, &desired_spec, |obtained| {
                info!(
                    "Opening SDL stream {} type {:?} with: rate {} channels {} (system channels {}) samples {}",
                    name, stream_type, obtained.freq, obtained.channels, system_channels, obtained.samples
                );
                SDLCallback {
                    stream_handle,
                    stream_type,
                    device_channels,
                }
            }) {
                Ok(dev) => Some(SDLDevice::Playback(dev)),
                Err(e) => {
                    error!("Error opening SDL audio playback device: {}", e);
                    None
                }
            }
        };

        if let Some(device) = sdl_device {
            self.streams.push(SDLStream {
                name: name.to_string(),
                stream_type,
                handle: handle.clone(),
                _device: device,
            });
        }

        handle
    }

    fn close_stream(&mut self, stream: &SinkStreamHandle) {
        self.streams
            .retain(|entry| !Arc::ptr_eq(&entry.handle, stream));
    }

    fn close_streams(&mut self) {
        self.streams.clear();
    }

    fn get_device_volume(&self) -> f32 {
        if let Some(entry) = self.streams.first() {
            entry.handle.lock().get_device_volume()
        } else {
            1.0
        }
    }

    fn set_device_volume(&mut self, volume: f32) {
        for entry in &self.streams {
            entry.handle.lock().set_device_volume(volume);
        }
    }

    fn set_system_volume(&mut self, volume: f32) {
        for entry in &self.streams {
            entry.handle.lock().set_system_volume(volume);
        }
    }

    fn get_device_channels(&self) -> u32 {
        self.device_channels
    }

    fn get_system_channels(&self) -> u32 {
        self.system_channels
    }
}

/// Get a list of connected audio devices from SDL.
pub fn list_sdl_sink_devices(capture: bool) -> Vec<String> {
    let sdl_context = match sdl2::init() {
        Ok(ctx) => ctx,
        Err(e) => {
            error!("SDL_Init failed: {}", e);
            return Vec::new();
        }
    };

    let audio = match sdl_context.audio() {
        Ok(audio) => audio,
        Err(e) => {
            error!("SDL_InitSubSystem audio failed: {}", e);
            return Vec::new();
        }
    };

    let count = if capture {
        audio.num_audio_capture_devices().unwrap_or(0)
    } else {
        audio.num_audio_playback_devices().unwrap_or(0)
    };

    let mut device_list = Vec::new();
    for i in 0..count {
        let name = if capture {
            audio.audio_capture_device_name(i)
        } else {
            audio.audio_playback_device_name(i)
        };
        if let Ok(name) = name {
            device_list.push(name);
        }
    }
    device_list
}

/// Check if the SDL audio backend is suitable for use.
pub fn is_sdl_suitable() -> bool {
    let sdl_context = match sdl2::init() {
        Ok(ctx) => ctx,
        Err(e) => {
            error!("SDL failed to init, it is not suitable. Error: {}", e);
            return false;
        }
    };

    let audio = match sdl_context.audio() {
        Ok(audio) => audio,
        Err(e) => {
            error!("SDL audio failed to init, it is not suitable. Error: {}", e);
            return false;
        }
    };

    let desired_spec = AudioSpecDesired {
        freq: Some(TARGET_SAMPLE_RATE as i32),
        channels: Some(2),
        samples: Some((TARGET_SAMPLE_COUNT * 2) as u16),
    };

    struct NullCallback;
    impl AudioCallback for NullCallback {
        type Channel = i16;
        fn callback(&mut self, _: &mut [i16]) {}
    }

    match AudioDevice::open_playback(&audio, None::<&str>, &desired_spec, |_| NullCallback) {
        Ok(_device) => true,
        Err(e) => {
            error!(
                "SDL failed to open a device, it is not suitable. Error: {}",
                e
            );
            false
        }
    }
}

use crate::common::common::{TARGET_SAMPLE_COUNT, TARGET_SAMPLE_RATE};
use crate::sink::sink::{new_stream_handle, Sink, AUTO_DEVICE_NAME};
use crate::sink::sink_stream::{SinkStream, SinkStreamHandle, StreamType};
use crate::SharedSystem;
use cubeb::{Context, DeviceState, DeviceType, SampleFormat, StreamParamsBuilder};
use log::{error, info, warn};
use std::sync::Arc;

struct CubebStream {
    name: String,
    stream_type: StreamType,
    handle: SinkStreamHandle,
    _backend: cubeb::Stream<i16>,
}

pub struct CubebSink {
    ctx: Option<Context>,
    output_device: cubeb::DeviceId,
    input_device: cubeb::DeviceId,
    device_channels: u32,
    system_channels: u32,
    streams: Vec<CubebStream>,
}

// Safety: cubeb::Context and cubeb::DeviceId contain raw pointers internally,
// but the cubeb library is designed to be used from multiple threads.
// The C++ upstream also shares these across threads freely.
unsafe impl Send for CubebSink {}
unsafe impl Sync for CubebSink {}

impl CubebSink {
    pub fn new(target_device_name: &str) -> Self {
        let ctx = match Context::init(Some(c"ruzu"), None) {
            Ok(ctx) => Some(ctx),
            Err(e) => {
                error!("cubeb_init failed: {:?}", e);
                None
            }
        };

        let mut output_device = cubeb::DeviceId::default();
        let input_device = cubeb::DeviceId::default();
        let mut device_channels = 2u32;

        if let Some(ref ctx) = ctx {
            // Query max channel count
            if let Ok(max_channels) = ctx.max_channel_count() {
                device_channels = if max_channels >= 6 { 6 } else { 2 };
            }

            // Find specific output device if requested
            if target_device_name != AUTO_DEVICE_NAME && !target_device_name.is_empty() {
                if let Ok(devices) = ctx.enumerate_devices(DeviceType::OUTPUT) {
                    for device in devices.iter() {
                        if let Some(friendly_name) = device.friendly_name() {
                            if friendly_name == target_device_name {
                                output_device = device.devid();
                                break;
                            }
                        }
                    }
                } else {
                    warn!("Audio output device enumeration not supported");
                }
            }
        }

        Self {
            ctx,
            output_device,
            input_device,
            device_channels,
            system_channels: 2,
            streams: Vec::new(),
        }
    }
}

impl Sink for CubebSink {
    fn acquire_sink_stream(
        &mut self,
        system: SharedSystem,
        system_channels: u32,
        name: &str,
        stream_type: StreamType,
    ) -> SinkStreamHandle {
        self.system_channels = system_channels;

        let Some(ref ctx) = self.ctx else {
            // Fallback: return a stream without cubeb backend
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

        // Build cubeb stream params
        let layout = match self.device_channels {
            1 => cubeb::ChannelLayout::MONO,
            6 => cubeb::ChannelLayout::_3F2_LFE,
            _ => cubeb::ChannelLayout::STEREO,
        };

        let params = StreamParamsBuilder::new()
            .rate(TARGET_SAMPLE_RATE)
            .channels(self.device_channels)
            .format(SampleFormat::S16LE)
            .layout(layout)
            .take();

        let minimum_latency = match ctx.min_latency(&params) {
            Ok(latency) => latency.max(TARGET_SAMPLE_COUNT * 2),
            Err(e) => {
                error!("Error getting minimum latency: {:?}", e);
                TARGET_SAMPLE_COUNT * 2
            }
        };

        info!(
            "Opening cubeb stream {} type {:?} with: rate {} channels {} (system channels {}) latency {}",
            name, stream_type, TARGET_SAMPLE_RATE, self.device_channels, system_channels, minimum_latency
        );

        let stream_handle = handle.clone();
        let device_channels = self.device_channels;
        let st = stream_type;

        let data_callback = move |_: &[i16], output: &mut [i16]| {
            let num_channels = device_channels as usize;
            let frame_size = num_channels.max(1);
            let num_frames = if frame_size > 0 {
                output.len() / frame_size
            } else {
                output.len()
            };

            let mut stream = stream_handle.lock();
            if st == StreamType::In {
                // For input streams, the `output` buffer here actually contains
                // captured input data from cubeb. Process it as audio in.
                stream.process_audio_in(output, num_frames);
            } else {
                stream.process_audio_out_and_render(output, num_frames);
            }
            output.len() as isize
        };

        let state_callback = |_: cubeb::State| {};

        let mut builder = cubeb::StreamBuilder::<i16>::new();
        builder.name(name.to_string()).latency(minimum_latency);

        if stream_type == StreamType::In {
            builder.input(self.input_device, &params);
        } else {
            builder.output(self.output_device, &params);
        }

        builder
            .data_callback(data_callback)
            .state_callback(state_callback);

        match builder.init(ctx) {
            Ok(backend) => {
                // Extract the raw cubeb_stream pointer for start/stop control.
                // The backend is stored in CubebStream and outlives the callback.
                let raw_ptr = backend.as_ptr() as usize;
                handle.lock().set_backend_ctl(Box::new(move |start| unsafe {
                    let ptr = raw_ptr as *mut cubeb::ffi::cubeb_stream;
                    if start {
                        if cubeb::ffi::cubeb_stream_start(ptr) != 0 {
                            log::error!("Error starting cubeb stream");
                        }
                    } else {
                        if cubeb::ffi::cubeb_stream_stop(ptr) != 0 {
                            log::error!("Error stopping cubeb stream");
                        }
                    }
                }));
                self.streams.push(CubebStream {
                    name: name.to_string(),
                    stream_type,
                    handle: handle.clone(),
                    _backend: backend,
                });
            }
            Err(e) => {
                error!("Error initializing cubeb stream: {:?}", e);
            }
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

/// Get a list of connected devices from cubeb.
pub fn list_cubeb_sink_devices(capture: bool) -> Vec<String> {
    let ctx = match Context::init(Some(c"ruzu Device Enumerator"), None) {
        Ok(ctx) => ctx,
        Err(e) => {
            error!("cubeb_init failed: {:?}", e);
            return Vec::new();
        }
    };

    let device_type = if capture {
        DeviceType::INPUT
    } else {
        DeviceType::OUTPUT
    };

    let devices = match ctx.enumerate_devices(device_type) {
        Ok(devices) => devices,
        Err(_) => {
            warn!("Audio output device enumeration not supported");
            return Vec::new();
        }
    };

    let mut device_list = Vec::new();
    for device in devices.iter() {
        if let Some(friendly_name) = device.friendly_name() {
            if !friendly_name.is_empty() && device.state() == DeviceState::Enabled {
                device_list.push(friendly_name.to_string());
            }
        }
    }
    device_list
}

/// Check if the cubeb backend is suitable for use.
pub fn is_cubeb_suitable() -> bool {
    let ctx = match Context::init(Some(c"ruzu Latency Getter"), None) {
        Ok(ctx) => ctx,
        Err(_) => {
            error!("Cubeb failed to init, it is not suitable.");
            return false;
        }
    };

    let params = StreamParamsBuilder::new()
        .rate(TARGET_SAMPLE_RATE)
        .channels(2)
        .format(SampleFormat::S16LE)
        .layout(cubeb::ChannelLayout::STEREO)
        .take();

    let latency = match ctx.min_latency(&params) {
        Ok(l) => l.max(TARGET_SAMPLE_COUNT * 2),
        Err(_) => {
            error!("Cubeb could not get min latency, it is not suitable.");
            return false;
        }
    };

    let data_cb = |_: &[i16], _: &mut [i16]| TARGET_SAMPLE_COUNT as isize;
    let state_cb = |_: cubeb::State| {};

    let mut builder = cubeb::StreamBuilder::<i16>::new();
    builder
        .name("Ruzu test")
        .default_output(&params)
        .output(cubeb::DeviceId::default(), &params)
        .latency(latency)
        .data_callback(data_cb)
        .state_callback(state_cb);

    match builder.init(&ctx) {
        Ok(stream) => {
            drop(stream);
            true
        }
        Err(_) => {
            error!("Cubeb could not open a device, it is not suitable.");
            false
        }
    }
}

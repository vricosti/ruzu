use crate::common::common::{TARGET_SAMPLE_COUNT, TARGET_SAMPLE_RATE};
use crate::sink::sink::{new_stream_handle, Sink, AUTO_DEVICE_NAME};
use crate::sink::sink_stream::{SinkStream, SinkStreamHandle, StreamType};
use crate::SharedSystem;
use cubeb::{Context, DeviceState, DeviceType, SampleFormat, StreamParamsBuilder};
use log::{error, info, warn};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

static CUBEB_CALLBACK_TRACE_COUNT: AtomicU32 = AtomicU32::new(0);

fn should_trace_cubeb_callback() -> bool {
    std::env::var_os("RUZU_TRACE_CUBEB_CALLBACK").is_some()
}

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

fn should_trace_cubeb_state() -> bool {
    std::env::var_os("RUZU_TRACE_CUBEB_STATE").is_some()
}

fn process_stream_callback(
    stream_handle: &SinkStreamHandle,
    stream_type: StreamType,
    device_channels: u32,
    input: &[i16],
    output: &mut [i16],
) -> isize {
    let num_channels = device_channels as usize;
    let frame_size = num_channels.max(1);
    let num_frames = if frame_size > 0 {
        output.len() / frame_size
    } else {
        output.len()
    };

    let mut stream = stream_handle.lock();
    let queue_before = stream.get_queue_size();
    if stream_type == StreamType::In {
        stream.process_audio_in(input, num_frames);
    } else {
        stream.process_audio_out_and_render(output, num_frames);
    }
    let queue_after = stream.get_queue_size();

    if std::env::var_os("RUZU_PROFILE_CUBEB_CB").is_some() {
        use std::sync::atomic::{AtomicU64, Ordering};
        use std::sync::OnceLock;
        use std::time::Instant;
        static CALLS: AtomicU64 = AtomicU64::new(0);
        static FRAMES: AtomicU64 = AtomicU64::new(0);
        static START: OnceLock<Instant> = OnceLock::new();
        let start = START.get_or_init(Instant::now);
        let n = CALLS.fetch_add(1, Ordering::Relaxed) + 1;
        let f = FRAMES.fetch_add(num_frames as u64, Ordering::Relaxed) + num_frames as u64;
        if n % 20 == 0 {
            let elapsed = start.elapsed().as_secs_f64();
            eprintln!(
                "[CUBEB_CB] type={:?} calls={} total_frames={} t={:.2}s callback_Hz={:.1} frames_Hz={:.0} queue_before={} queue_after={}",
                stream_type, n, f, elapsed,
                n as f64 / elapsed, f as f64 / elapsed,
                queue_before, queue_after
            );
        }
    }

    if should_trace_cubeb_callback() {
        let count = CUBEB_CALLBACK_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
        if count < 32 {
            log::info!(
                "CubebSink callback type={:?} frames={} samples={} queue_before={} queue_after={} paused={}",
                stream_type,
                num_frames,
                output.len(),
                queue_before,
                queue_after,
                stream.is_paused()
            );
        }
    }

    num_frames as isize
}

impl CubebSink {
    pub fn new(target_device_name: &str) -> Self {
        // RUZU_CUBEB_BACKEND env override — useful to bypass pulse-rust on
        // pipewire-pulse systems where it oscillates Drained↔Started (see
        // project_mk8d_audio_pipewire_drain_2026_05_17).
        let backend_cstring = std::env::var("RUZU_CUBEB_BACKEND")
            .ok()
            .and_then(|s| std::ffi::CString::new(s).ok());
        let backend_name = backend_cstring.as_deref();
        let ctx = match Context::init(Some(c"ruzu"), backend_name) {
            Ok(ctx) => Some(ctx),
            Err(e) => {
                error!("cubeb_init failed (backend={:?}): {:?}", backend_name, e);
                None
            }
        };

        let mut output_device = cubeb::DeviceId::default();
        let input_device = cubeb::DeviceId::default();
        let mut device_channels = 2u32;

        if let Some(ref ctx) = ctx {
            info!("cubeb backend_id: {}", ctx.backend_id());

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

        // RUZU_AUDIO_LATENCY_FRAMES=N — override the cubeb-reported minimum
        // latency at runtime. Useful for diagnosing the audio-event-rate
        // collapse on PipeWire-pulse hosts where cubeb's C pulse backend
        // reports 1200 frames (25 ms ≈ 40 Hz callback) which throttles the
        // audio renderer to ~65 Hz, ~3x below the ~200 Hz target. Default
        // behavior matches upstream (no clamp below what cubeb says).
        let env_latency = std::env::var("RUZU_AUDIO_LATENCY_FRAMES")
            .ok()
            .and_then(|s| s.parse::<u32>().ok());
        let minimum_latency = if let Some(v) = env_latency {
            v
        } else {
            match ctx.min_latency(&params) {
                Ok(latency) => latency.max(TARGET_SAMPLE_COUNT * 2),
                Err(e) => {
                    error!("Error getting minimum latency: {:?}", e);
                    TARGET_SAMPLE_COUNT * 2
                }
            }
        };

        info!(
            "Opening cubeb stream {} type {:?} with: rate {} channels {} (system channels {}) latency {}",
            name, stream_type, TARGET_SAMPLE_RATE, self.device_channels, system_channels, minimum_latency
        );

        let stream_handle = handle.clone();
        let device_channels = self.device_channels;
        let st = stream_type;

        // cubeb-rs's `StreamBuilder<F>` treats `F` as a FRAME type, not a
        // sample type. Its internal `data_cb_c` slices the raw output buffer
        // as `nframes` elements of size `sizeof(F)`. With `F = i16` for a
        // stereo stream we only see/fill half the bytes cubeb actually
        // allocated, return `nframes/channels` to cubeb, and produce audio
        // at `1/channels` of real-time — observed as ~65 Hz audio-event rate
        // instead of zuyu's ~200 Hz on this host. (See
        // project_mk8d_real_cause_cubeb_backend_2026_05_17.)
        //
        // Fix: extend the slice from `nframes` elements to the real
        // `nframes * device_channels` elements via raw pointer reslice. This
        // requires no F-type change because the underlying pointer is the
        // same; we're just correcting the slice length cubeb-rs got wrong
        // for any non-mono stream.
        let data_callback = move |input: &[i16], output: &mut [i16]| -> isize {
            let chans = device_channels as usize;
            // cubeb-rs gave us `nframes`-long slices; cubeb's real
            // allocation is `nframes * chans` samples. Re-slice to the
            // correct length.
            let in_full: &[i16] = if input.is_empty() {
                &[]
            } else {
                unsafe { std::slice::from_raw_parts(input.as_ptr(), input.len() * chans) }
            };
            let out_full: &mut [i16] = if output.is_empty() {
                &mut []
            } else {
                unsafe { std::slice::from_raw_parts_mut(output.as_mut_ptr(), output.len() * chans) }
            };
            process_stream_callback(&stream_handle, st, device_channels, in_full, out_full)
        };

        let callback_name = name.to_string();
        let callback_type = stream_type;
        let state_stream_handle = handle.clone();
        let state_callback = move |state: cubeb::State| {
            if should_trace_cubeb_state() {
                log::info!(
                    "CubebSink state name={} type={:?} state={:?}",
                    callback_name,
                    callback_type,
                    state
                );
            }
            if state == cubeb::State::Drained {
                state_stream_handle.lock().signal_pause();
            }
        };

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
                let ctl_name = name.to_string();
                let ctl_type = stream_type;
                handle.lock().set_backend_ctl(Box::new(move |start| unsafe {
                    let ptr = raw_ptr as *mut cubeb::ffi::cubeb_stream;
                    if should_trace_cubeb_state() {
                        log::info!(
                            "CubebSink backend_ctl name={} type={:?} start={}",
                            ctl_name,
                            ctl_type,
                            start
                        );
                    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sink::sink_stream::{SinkBuffer, StreamType};
    use parking_lot::Mutex;

    fn make_system() -> SharedSystem {
        Arc::new(Mutex::new(ruzu_core::core::System::new()))
    }

    #[test]
    fn cubeb_callback_returns_frame_count_not_sample_count() {
        let system = make_system();
        let mut stream = SinkStream::new(system, StreamType::Out);
        stream.device_channels = 2;
        stream.system_channels = 2;
        stream.append_buffer(
            SinkBuffer {
                frames: 2,
                frames_played: 0,
                tag: 1,
                consumed: false,
            },
            &[1, 2, 3, 4],
        );
        let handle = Arc::new(parking_lot::Mutex::new(stream));
        let mut output = [0i16; 4];

        let written = process_stream_callback(&handle, StreamType::Out, 2, &[], &mut output);

        assert_eq!(written, 2);
        assert_eq!(output, [1, 2, 3, 4]);
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

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Audio services: `audren:u`, `audren:IAudioRenderer`, `audren:IAudioDevice`,
//! `audout:u`, and `audout:IAudioOut`.
//!
//! The `audren:*` services are now backed by the Rust `audio_core` port for
//! renderer creation, work-buffer sizing, start/stop metadata, and
//! `RequestUpdate` when the caller provides the expected A/B buffers.

use std::collections::VecDeque;
use std::sync::Arc;

use audio_core::common::{
    AudioRendererParameterInternal, ExecutionMode, CURRENT_REVISION, MAX_WAVE_BUFFERS,
    TARGET_SAMPLE_COUNT, TARGET_SAMPLE_RATE,
};
use audio_core::errors::RESULT_OUT_OF_SESSIONS;
use audio_core::renderer::{Renderer as AudioRendererInstance, System as AudioRenderSystem};
use audio_core::{AudioCore, AudioRenderManager};
use common::ResultCode;
use parking_lot::Mutex;

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

// ── audren shared state ─────────────────────────────────────────────────────

pub type SharedAudioContext = Arc<Mutex<AudioServiceContext>>;

/// Memory pool output state: Attached.
const MEMPOOL_STATE_ATTACHED: u32 = 5;

fn default_renderer_params() -> AudioRendererParameterInternal {
    AudioRendererParameterInternal {
        sample_rate: TARGET_SAMPLE_RATE,
        sample_count: TARGET_SAMPLE_COUNT,
        mixes: 1,
        sub_mixes: 1,
        voices: 24,
        sinks: 2,
        effects: 0,
        perf_frames: 0,
        voice_drop_enabled: 0,
        unk_21: 0,
        rendering_device: 0,
        execution_mode: ExecutionMode::Auto,
        splitter_infos: 0,
        splitter_destinations: 0,
        external_context_size: 0,
        revision: CURRENT_REVISION,
    }
}

fn parse_u64_words(words: &[u32], start: usize) -> Option<u64> {
    let lo = *words.get(start)? as u64;
    let hi = *words.get(start + 1)? as u64;
    Some(lo | (hi << 32))
}

fn parse_renderer_params(raw_data: &[u32]) -> AudioRendererParameterInternal {
    if raw_data.len() < 13 {
        return default_renderer_params();
    }

    let packed = raw_data[8];
    AudioRendererParameterInternal {
        sample_rate: raw_data[0],
        sample_count: raw_data[1],
        mixes: raw_data[2],
        sub_mixes: raw_data[3],
        voices: raw_data[4],
        sinks: raw_data[5],
        effects: raw_data[6],
        perf_frames: raw_data[7],
        voice_drop_enabled: (packed & 0xFF) as u8,
        unk_21: ((packed >> 8) & 0xFF) as u8,
        rendering_device: ((packed >> 16) & 0xFF) as u8,
        execution_mode: match ((packed >> 24) & 0xFF) as u8 {
            1 => ExecutionMode::Manual,
            _ => ExecutionMode::Auto,
        },
        splitter_infos: raw_data[9],
        splitter_destinations: raw_data[10] as i32,
        external_context_size: raw_data[11],
        revision: raw_data[12],
    }
}

#[cfg(test)]
fn encode_renderer_params(params: &AudioRendererParameterInternal) -> Vec<u32> {
    vec![
        params.sample_rate,
        params.sample_count,
        params.mixes,
        params.sub_mixes,
        params.voices,
        params.sinks,
        params.effects,
        params.perf_frames,
        (params.voice_drop_enabled as u32)
            | ((params.unk_21 as u32) << 8)
            | ((params.rendering_device as u32) << 16)
            | ((params.execution_mode as u32) << 24),
        params.splitter_infos,
        params.splitter_destinations as u32,
        params.external_context_size,
        params.revision,
    ]
}

pub struct AudioServiceContext {
    _runtime: AudioCore,
    render_manager: Arc<Mutex<AudioRenderManager>>,
    audio_renderer_handle: audio_core::adsp::adsp::AudioRendererHandle,
    renderer: Option<AudioRendererInstance>,
    params: AudioRendererParameterInternal,
}

impl AudioServiceContext {
    fn new() -> Self {
        let system = Arc::new(Mutex::new(ruzu_core::core::System::new()));
        let settings = Arc::new(common::settings::Values::default());
        let runtime = AudioCore::new(system.clone(), settings);
        let audio_renderer_handle = runtime.adsp().audio_renderer();
        let render_manager = Arc::new(Mutex::new(AudioRenderManager::new(
            system,
            audio_renderer_handle.clone(),
        )));

        Self {
            _runtime: runtime,
            render_manager,
            audio_renderer_handle,
            renderer: None,
            params: default_renderer_params(),
        }
    }

    fn get_work_buffer_size(
        &self,
        params: &AudioRendererParameterInternal,
    ) -> Result<u64, ResultCode> {
        let mut size = 0;
        let result = self
            .render_manager
            .lock()
            .get_work_buffer_size(params, &mut size);
        if result.is_error() {
            Err(result)
        } else {
            Ok(size)
        }
    }

    fn open_renderer(
        &mut self,
        params: AudioRendererParameterInternal,
        transfer_memory_size: u64,
        applet_resource_user_id: u64,
    ) -> ResultCode {
        self.renderer.take();

        let session_id = self.render_manager.lock().get_session_id();
        if session_id < 0 {
            return RESULT_OUT_OF_SESSIONS;
        }

        let shared_system = self.render_manager.lock().system();
        let system = Arc::new(Mutex::new(AudioRenderSystem::new(
            shared_system,
            self.audio_renderer_handle.clone(),
        )));
        let mut renderer = AudioRendererInstance::new(self.render_manager.clone(), system);
        let result = renderer.initialize(
            &params,
            transfer_memory_size,
            true,
            applet_resource_user_id,
            session_id,
        );
        if result.is_success() {
            self.params = params;
            self.renderer = Some(renderer);
        }
        result
    }

    fn get_sample_rate(&self) -> u32 {
        self.renderer
            .as_ref()
            .map(|renderer| renderer.get_system().lock().get_sample_rate())
            .unwrap_or(self.params.sample_rate)
    }

    fn get_sample_count(&self) -> u32 {
        self.renderer
            .as_ref()
            .map(|renderer| renderer.get_system().lock().get_sample_count())
            .unwrap_or(self.params.sample_count)
    }

    fn start(&mut self) {
        if let Some(renderer) = self.renderer.as_ref() {
            renderer.start();
        }
    }

    fn stop(&mut self) {
        if let Some(renderer) = self.renderer.as_ref() {
            renderer.stop();
        }
    }

    fn build_request_update_output(&self) -> Vec<u8> {
        let voice_out_size = self.params.voices as usize * 0x10;
        let effect_out_size = self.params.effects as usize * 0x10;
        let sink_out_size = self.params.sinks as usize * 0x20;
        let mempool_count = self.params.effects + self.params.voices * MAX_WAVE_BUFFERS;
        let mempool_out_size = mempool_count as usize * 0x10;
        let perf_out_size = 0x10;
        let behavior_out_size = 0x10;
        let render_info_size = 0x10;

        let total = 0x40
            + voice_out_size
            + effect_out_size
            + sink_out_size
            + mempool_out_size
            + perf_out_size
            + behavior_out_size
            + render_info_size;

        let mut buf = vec![0u8; total];
        buf[0x3C..0x40].copy_from_slice(&(total as u32).to_le_bytes());

        let mempool_offset = 0x40 + voice_out_size + effect_out_size + sink_out_size;
        for i in 0..mempool_count as usize {
            let entry = mempool_offset + i * 0x10;
            if entry + 4 <= buf.len() {
                buf[entry..entry + 4].copy_from_slice(&MEMPOOL_STATE_ATTACHED.to_le_bytes());
            }
        }

        buf
    }

    fn request_update(
        &self,
        input: &[u8],
        performance_size: usize,
        output_size: usize,
    ) -> Result<(Vec<u8>, Vec<u8>), ResultCode> {
        let Some(renderer) = self.renderer.as_ref() else {
            let mut output = self.build_request_update_output();
            let requested_output_size = output_size.max(0x40);
            output.resize(requested_output_size, 0);
            return Ok((output, vec![0u8; performance_size]));
        };

        let mut performance = vec![0u8; performance_size];
        let mut output = vec![0u8; output_size.max(0x40)];
        let result = renderer.request_update(input, &mut performance, &mut output);
        if result.is_error() {
            Err(result)
        } else {
            Ok((output, performance))
        }
    }
}

pub fn new_audio_context() -> SharedAudioContext {
    Arc::new(Mutex::new(AudioServiceContext::new()))
}

// ── audren:u ─────────────────────────────────────────────────────────────────

pub struct AudRendererService {
    context: SharedAudioContext,
}

impl AudRendererService {
    pub fn new() -> Self {
        Self::new_with_context(new_audio_context())
    }

    pub fn new_with_context(context: SharedAudioContext) -> Self {
        Self { context }
    }
}

impl ServiceHandler for AudRendererService {
    fn service_name(&self) -> &str {
        "audren:u"
    }

    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse {
        log::debug!("audren:u: cmd_id={}", cmd_id);
        match cmd_id {
            // OpenAudioRenderer
            0 => {
                let params = parse_renderer_params(&command.raw_data);
                let mut context = self.context.lock();
                let transfer_memory_size = parse_u64_words(&command.raw_data, 13)
                    .filter(|size| *size != 0)
                    .or_else(|| context.get_work_buffer_size(&params).ok())
                    .unwrap_or(0x40000);
                let applet_resource_user_id = parse_u64_words(&command.raw_data, 15).unwrap_or(0);
                let result =
                    context.open_renderer(params, transfer_memory_size, applet_resource_user_id);
                if result.is_success() {
                    log::info!(
                        "audren:u: OpenAudioRenderer (rate={}, samples={}, voices={}, sinks={})",
                        params.sample_rate,
                        params.sample_count,
                        params.voices,
                        params.sinks
                    );
                    IpcResponse::success().with_move_handle(0)
                } else {
                    log::warn!("audren:u: OpenAudioRenderer failed: 0x{:08X}", result.raw());
                    IpcResponse::error(result)
                }
            }
            // GetWorkBufferSize
            1 => {
                let params = parse_renderer_params(&command.raw_data);
                let context = self.context.lock();
                match context.get_work_buffer_size(&params) {
                    Ok(size) => {
                        log::info!("audren:u: GetWorkBufferSize -> 0x{:X}", size);
                        IpcResponse::success_with_data(vec![size as u32, (size >> 32) as u32])
                    }
                    Err(result) => {
                        log::warn!("audren:u: GetWorkBufferSize failed: 0x{:08X}", result.raw());
                        IpcResponse::error(result)
                    }
                }
            }
            // GetAudioDeviceService
            2 => {
                log::info!("audren:u: GetAudioDeviceService");
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("audren:u: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── audren:IAudioRenderer ────────────────────────────────────────────────────

pub struct AudioRendererService {
    context: SharedAudioContext,
    event_handle: u32,
}

impl AudioRendererService {
    pub fn new() -> Self {
        Self::new_with_context_and_event(new_audio_context(), 0)
    }

    pub fn new_with_context(context: SharedAudioContext) -> Self {
        Self::new_with_context_and_event(context, 0)
    }

    pub fn new_with_context_and_event(context: SharedAudioContext, event_handle: u32) -> Self {
        Self {
            context,
            event_handle,
        }
    }
}

impl ServiceHandler for AudioRendererService {
    fn service_name(&self) -> &str {
        "audren:IAudioRenderer"
    }

    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse {
        log::debug!("audren:IAudioRenderer: cmd_id={}", cmd_id);
        match cmd_id {
            // GetSampleRate
            0 => {
                let sample_rate = self.context.lock().get_sample_rate();
                log::info!("audren:IAudioRenderer: GetSampleRate ({})", sample_rate);
                IpcResponse::success_with_data(vec![sample_rate])
            }
            // GetSampleCount
            1 => {
                let sample_count = self.context.lock().get_sample_count();
                log::info!("audren:IAudioRenderer: GetSampleCount ({})", sample_count);
                IpcResponse::success_with_data(vec![sample_count])
            }
            // RequestUpdate
            4 => {
                let input = command.a_buf_data.first().map(Vec::as_slice).unwrap_or(&[]);
                let output_size = command.b_buf_sizes.first().copied().unwrap_or(0x40) as usize;
                let performance_size = command.b_buf_sizes.get(1).copied().unwrap_or(0) as usize;
                match self
                    .context
                    .lock()
                    .request_update(input, performance_size, output_size)
                {
                    Ok((output, performance)) => {
                        log::debug!(
                            "audren:IAudioRenderer: RequestUpdate input=0x{:X} output=0x{:X} perf=0x{:X}",
                            input.len(),
                            output.len(),
                            performance.len()
                        );
                        let mut response = IpcResponse::success().with_out_buf(output);
                        if performance_size != 0 {
                            response = response.with_out_buf(performance);
                        }
                        response
                    }
                    Err(result) => {
                        log::warn!(
                            "audren:IAudioRenderer: RequestUpdate failed: 0x{:08X}",
                            result.raw()
                        );
                        IpcResponse::error(result)
                    }
                }
            }
            // Start
            5 => {
                log::info!("audren:IAudioRenderer: Start");
                self.context.lock().start();
                IpcResponse::success()
            }
            // Stop
            6 => {
                log::info!("audren:IAudioRenderer: Stop");
                self.context.lock().stop();
                IpcResponse::success()
            }
            // QuerySystemEvent
            7 => {
                log::info!(
                    "audren:IAudioRenderer: QuerySystemEvent (handle={})",
                    self.event_handle
                );
                IpcResponse::success().with_copy_handle(self.event_handle)
            }
            _ => {
                log::warn!("audren:IAudioRenderer: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── audren:IAudioDevice ──────────────────────────────────────────────────────

pub struct AudioDeviceService {
    event_handle: u32,
}

impl AudioDeviceService {
    pub fn new() -> Self {
        Self::new_with_event(0)
    }

    pub fn new_with_event(event_handle: u32) -> Self {
        Self { event_handle }
    }
}

/// Encode a short string as u32 words (NUL-padded to 4-byte boundary).
fn encode_string_as_words(s: &str) -> Vec<u32> {
    let bytes = s.as_bytes();
    // Pad to multiple of 4 with NULs, plus one extra NUL terminator.
    let padded_len = ((bytes.len() + 1) + 3) & !3;
    let mut padded = vec![0u8; padded_len];
    padded[..bytes.len()].copy_from_slice(bytes);

    padded
        .chunks(4)
        .map(|chunk| {
            u32::from_le_bytes([
                chunk[0],
                chunk.get(1).copied().unwrap_or(0),
                chunk.get(2).copied().unwrap_or(0),
                chunk.get(3).copied().unwrap_or(0),
            ])
        })
        .collect()
}

impl ServiceHandler for AudioDeviceService {
    fn service_name(&self) -> &str {
        "audren:IAudioDevice"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("audren:IAudioDevice: cmd_id={}", cmd_id);
        match cmd_id {
            // ListAudioDeviceName
            0 => {
                log::info!("audren:IAudioDevice: ListAudioDeviceName");
                IpcResponse::success_with_data(encode_string_as_words("AudioTvOutput"))
            }
            // SetAudioDeviceOutputVolume
            1 => {
                log::info!("audren:IAudioDevice: SetAudioDeviceOutputVolume");
                IpcResponse::success()
            }
            // GetAudioDeviceOutputVolume — 1.0f32
            2 => {
                log::info!("audren:IAudioDevice: GetAudioDeviceOutputVolume (1.0)");
                IpcResponse::success_with_data(vec![1.0f32.to_bits()])
            }
            // GetActiveAudioDeviceName
            4 => {
                log::info!("audren:IAudioDevice: GetActiveAudioDeviceName");
                IpcResponse::success_with_data(encode_string_as_words("AudioTvOutput"))
            }
            // QueryAudioDeviceSystemEvent
            5 => {
                log::info!(
                    "audren:IAudioDevice: QueryAudioDeviceSystemEvent (handle={})",
                    self.event_handle
                );
                IpcResponse::success().with_copy_handle(self.event_handle)
            }
            // GetActiveChannelCount — stereo
            6 => {
                log::info!("audren:IAudioDevice: GetActiveChannelCount (2)");
                IpcResponse::success_with_data(vec![2])
            }
            _ => {
                log::warn!("audren:IAudioDevice: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── audout:u ─────────────────────────────────────────────────────────────────

pub struct AudOutService;

impl AudOutService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for AudOutService {
    fn service_name(&self) -> &str {
        "audout:u"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("audout:u: cmd_id={}", cmd_id);
        match cmd_id {
            // ListAudioOuts
            0 => {
                log::info!("audout:u: ListAudioOuts");
                IpcResponse::success_with_data(encode_string_as_words("DeviceOut"))
            }
            // OpenAudioOut
            1 => {
                log::info!("audout:u: OpenAudioOut");
                IpcResponse::success().with_move_handle(0)
            }
            _ => {
                log::warn!("audout:u: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── audout:IAudioOut ─────────────────────────────────────────────────────────

pub struct AudioOutService {
    /// Kernel event handle returned for RegisterBufferEvent.
    /// When signaled (by the main loop), the audio thread wakes and drains pending_keys.
    event_handle: u32,
    /// Buffer keys queued by AppendAudioOutBuffer, drained by GetReleasedAudioOutBuffers.
    pending_keys: VecDeque<u64>,
}

impl AudioOutService {
    /// Create with a dummy handle (for tests / headless mode).
    pub fn new() -> Self {
        Self::new_with_event(0)
    }

    /// Create with a real kernel event handle allocated by the caller.
    pub fn new_with_event(event_handle: u32) -> Self {
        Self {
            event_handle,
            pending_keys: VecDeque::new(),
        }
    }
}

impl ServiceHandler for AudioOutService {
    fn service_name(&self) -> &str {
        "audout:IAudioOut"
    }

    fn handle_request(&mut self, cmd_id: u32, command: &IpcCommand) -> IpcResponse {
        log::debug!("audout:IAudioOut: cmd_id={}", cmd_id);
        match cmd_id {
            // GetAudioOutState — stopped
            0 => {
                log::info!("audout:IAudioOut: GetAudioOutState (stopped)");
                IpcResponse::success_with_data(vec![0])
            }
            // Start
            1 => {
                log::info!("audout:IAudioOut: Start");
                IpcResponse::success()
            }
            // Stop
            2 => {
                log::info!("audout:IAudioOut: Stop");
                IpcResponse::success()
            }
            // AppendAudioOutBuffer(buffer_ptr: u64, buffer_key: u64)
            // raw_data[0..1] = buffer_ptr, raw_data[1..3] = buffer_key
            3 => {
                let buffer_key = if command.raw_data.len() >= 4 {
                    (command.raw_data[2] as u64) | ((command.raw_data[3] as u64) << 32)
                } else if command.raw_data.len() >= 3 {
                    (command.raw_data[1] as u64) | ((command.raw_data[2] as u64) << 32)
                } else if command.raw_data.len() >= 2 {
                    (command.raw_data[0] as u64) | ((command.raw_data[1] as u64) << 32)
                } else {
                    self.pending_keys.len() as u64 + 1
                };
                log::debug!(
                    "audout:IAudioOut: AppendAudioOutBuffer (key=0x{:X})",
                    buffer_key
                );
                self.pending_keys.push_back(buffer_key);
                IpcResponse::success()
            }
            // RegisterBufferEvent — return the real kernel event handle
            4 => {
                log::info!(
                    "audout:IAudioOut: RegisterBufferEvent (handle={})",
                    self.event_handle
                );
                IpcResponse::success().with_copy_handle(self.event_handle)
            }
            // GetReleasedAudioOutBuffers — drain pending keys (up to 8)
            5 => {
                let count = self.pending_keys.len().min(8) as u32;
                let mut data = vec![count];
                for _ in 0..count as usize {
                    if let Some(key) = self.pending_keys.pop_front() {
                        data.push(key as u32);
                        data.push((key >> 32) as u32);
                    }
                }
                log::debug!(
                    "audout:IAudioOut: GetReleasedAudioOutBuffers (count={})",
                    count
                );
                IpcResponse::success_with_data(data)
            }
            // ContainsAudioOutBuffer — false
            6 => {
                log::debug!("audout:IAudioOut: ContainsAudioOutBuffer (false)");
                IpcResponse::success_with_data(vec![0])
            }
            // GetAudioOutBufferCount
            7 => {
                let count = self.pending_keys.len() as u32;
                log::debug!("audout:IAudioOut: GetAudioOutBufferCount ({})", count);
                IpcResponse::success_with_data(vec![count])
            }
            _ => {
                log::warn!("audout:IAudioOut: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ipc::CommandType;

    fn make_command(cmd_id: u32) -> IpcCommand {
        IpcCommand {
            command_type: CommandType::Request,
            data_size: 0,
            num_x_bufs: 0,
            num_a_bufs: 0,
            num_b_bufs: 0,
            has_handle_descriptor: false,
            handles_to_copy: Vec::new(),
            handles_to_move: Vec::new(),
            send_pid: false,
            cmif_magic: 0x49434653,
            command_id: cmd_id,
            raw_data: Vec::new(),
            b_buf_addrs: Vec::new(),
            x_bufs: Vec::new(),
            a_bufs: Vec::new(),
            a_buf_data: Vec::new(),
            b_buf_sizes: Vec::new(),
        }
    }

    fn make_command_with_raw(cmd_id: u32, raw_data: Vec<u32>) -> IpcCommand {
        IpcCommand {
            raw_data,
            ..make_command(cmd_id)
        }
    }

    #[test]
    fn test_audren_open_renderer() {
        let mut svc = AudRendererService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move, vec![0]);
    }

    #[test]
    fn test_audren_work_buffer_size() {
        let mut svc = AudRendererService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data.len(), 2);
        assert_ne!(resp.data, vec![0x40000, 0]);
    }

    #[test]
    fn test_audio_renderer_sample_rate() {
        let mut svc = AudioRendererService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![48000]);
    }

    #[test]
    fn test_audio_renderer_request_update_uses_buffer_sizes() {
        let mut svc = AudioRendererService::new();
        let mut cmd = make_command(4);
        cmd.a_buf_data = vec![vec![0; 0x40]];
        cmd.b_buf_sizes = vec![0x80, 0x20];

        let resp = svc.handle_request(4, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.out_bufs.len(), 2);
        assert_eq!(resp.out_bufs[0].len(), 0x80);
        assert_eq!(resp.out_bufs[1].len(), 0x20);
    }

    #[test]
    fn test_shared_audio_renderer_context_updates_metadata() {
        let context = new_audio_context();
        let mut root = AudRendererService::new_with_context(context.clone());
        let mut renderer = AudioRendererService::new_with_context(context);

        let mut params = default_renderer_params();
        params.sample_rate = 32000;
        params.sample_count = 160;
        params.voices = 16;
        params.sinks = 1;
        let cmd = make_command_with_raw(0, encode_renderer_params(&params));
        let resp = root.handle_request(0, &cmd);
        assert!(resp.result.is_success());

        let rate = renderer.handle_request(0, &make_command(0));
        let count = renderer.handle_request(1, &make_command(1));
        assert_eq!(rate.data, vec![32000]);
        assert_eq!(count.data, vec![160]);
    }

    #[test]
    fn test_audio_device_channel_count() {
        let mut svc = AudioDeviceService::new();
        let cmd = make_command(6);
        let resp = svc.handle_request(6, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![2]);
    }

    #[test]
    fn test_audio_renderer_query_system_event_returns_assigned_handle() {
        let context = new_audio_context();
        let mut svc = AudioRendererService::new_with_context_and_event(context, 42);
        let resp = svc.handle_request(7, &make_command(7));
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_copy, vec![42]);
    }

    #[test]
    fn test_audio_device_query_system_event_returns_assigned_handle() {
        let mut svc = AudioDeviceService::new_with_event(24);
        let resp = svc.handle_request(5, &make_command(5));
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_copy, vec![24]);
    }

    #[test]
    fn test_audout_open() {
        let mut svc = AudOutService::new();
        let cmd = make_command(1);
        let resp = svc.handle_request(1, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.handles_to_move, vec![0]);
    }

    #[test]
    fn test_audio_out_state() {
        let mut svc = AudioOutService::new();
        let cmd = make_command(0);
        let resp = svc.handle_request(0, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![0]);
    }

    #[test]
    fn test_encode_string() {
        let words = encode_string_as_words("ABC");
        assert_eq!(words.len(), 1);
        assert_eq!(words[0], u32::from_le_bytes([b'A', b'B', b'C', 0]));
    }
}

// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Audio service stubs: `audren:u` and `audout:u`.
//!
//! These are "black hole" stubs — they accept all audio data but produce no
//! output. This prevents games from crashing during audio initialization.

use std::collections::VecDeque;

use crate::framework::ServiceHandler;
use crate::ipc::{IpcCommand, IpcResponse};

// ── audren:u ─────────────────────────────────────────────────────────────────

pub struct AudRendererService;

impl AudRendererService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for AudRendererService {
    fn service_name(&self) -> &str {
        "audren:u"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("audren:u: cmd_id={}", cmd_id);
        match cmd_id {
            // OpenAudioRenderer
            0 => {
                log::info!("audren:u: OpenAudioRenderer");
                IpcResponse::success().with_move_handle(0)
            }
            // GetWorkBufferSize — 256KB
            1 => {
                log::info!("audren:u: GetWorkBufferSize (256KB)");
                IpcResponse::success_with_data(vec![0x40000, 0])
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

pub struct AudioRendererService;

impl AudioRendererService {
    pub fn new() -> Self {
        Self
    }
}

impl ServiceHandler for AudioRendererService {
    fn service_name(&self) -> &str {
        "audren:IAudioRenderer"
    }

    fn handle_request(&mut self, cmd_id: u32, _command: &IpcCommand) -> IpcResponse {
        log::debug!("audren:IAudioRenderer: cmd_id={}", cmd_id);
        match cmd_id {
            // GetSampleRate — 48000 Hz
            0 => {
                log::info!("audren:IAudioRenderer: GetSampleRate (48000)");
                IpcResponse::success_with_data(vec![48000])
            }
            // GetSampleCount — 240 samples per frame
            1 => {
                log::info!("audren:IAudioRenderer: GetSampleCount (240)");
                IpcResponse::success_with_data(vec![240])
            }
            // RequestUpdate
            4 => {
                log::debug!("audren:IAudioRenderer: RequestUpdate");
                IpcResponse::success()
            }
            // Start
            5 => {
                log::info!("audren:IAudioRenderer: Start");
                IpcResponse::success()
            }
            // Stop
            6 => {
                log::info!("audren:IAudioRenderer: Stop");
                IpcResponse::success()
            }
            // QuerySystemEvent — render event
            7 => {
                log::info!("audren:IAudioRenderer: QuerySystemEvent");
                IpcResponse::success().with_copy_handle(0)
            }
            _ => {
                log::warn!("audren:IAudioRenderer: unhandled cmd_id={}", cmd_id);
                IpcResponse::success()
            }
        }
    }
}

// ── audren:IAudioDevice ──────────────────────────────────────────────────────

pub struct AudioDeviceService;

impl AudioDeviceService {
    pub fn new() -> Self {
        Self
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
                log::info!("audren:IAudioDevice: QueryAudioDeviceSystemEvent");
                IpcResponse::success().with_copy_handle(0)
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
                log::debug!("audout:IAudioOut: AppendAudioOutBuffer (key=0x{:X})", buffer_key);
                self.pending_keys.push_back(buffer_key);
                IpcResponse::success()
            }
            // RegisterBufferEvent — return the real kernel event handle
            4 => {
                log::info!("audout:IAudioOut: RegisterBufferEvent (handle={})", self.event_handle);
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
                log::debug!("audout:IAudioOut: GetReleasedAudioOutBuffers (count={})", count);
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
        assert_eq!(resp.data, vec![0x40000, 0]);
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
    fn test_audio_device_channel_count() {
        let mut svc = AudioDeviceService::new();
        let cmd = make_command(6);
        let resp = svc.handle_request(6, &cmd);
        assert!(resp.result.is_success());
        assert_eq!(resp.data, vec![2]);
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
        assert_eq!(resp.data, vec![0]); // stopped
    }

    #[test]
    fn test_encode_string() {
        let words = encode_string_as_words("ABC");
        assert_eq!(words.len(), 1);
        assert_eq!(words[0], u32::from_le_bytes([b'A', b'B', b'C', 0]));
    }
}

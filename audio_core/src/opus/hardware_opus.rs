use crate::adsp::apps::opus::shared_memory::{SharedMemory, SharedMemoryHandle};
use crate::adsp::apps::opus::{Direction, Message, OpusDecoder as AdspOpusDecoder};
use crate::errors::{
    RESULT_BUFFER_TOO_SMALL, RESULT_INVALID_OPUS_CHANNEL_COUNT, RESULT_INVALID_OPUS_SAMPLE_RATE,
    RESULT_LIB_OPUS_INVALID_STATE,
};
use crate::opus::parameters::{MAX_CHANNELS, OPUS_STREAM_COUNT_MAX};
use crate::Result;
use common::alignment::align_up;
use common::ResultCode;
use parking_lot::Mutex;
use std::mem::size_of;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DecodeMode {
    None,
    Single,
    MultiStream,
}

#[derive(Debug, Clone, Copy)]
struct State {
    running: bool,
    mapped: bool,
    mode: DecodeMode,
    sample_rate: u32,
    channel_count: u32,
    total_stream_count: u32,
    stereo_stream_count: u32,
    buffer_size: u64,
}

impl Default for State {
    fn default() -> Self {
        Self {
            running: true,
            mapped: false,
            mode: DecodeMode::None,
            sample_rate: 0,
            channel_count: 0,
            total_stream_count: 0,
            stereo_stream_count: 0,
            buffer_size: 0,
        }
    }
}

struct AdspBackend {
    decoder: Arc<Mutex<AdspOpusDecoder>>,
    shared_memory: SharedMemoryHandle,
    state: Mutex<State>,
    buffer_id: u64,
}

enum Backend {
    Local(Mutex<State>),
    Adsp(AdspBackend),
}

pub struct HardwareOpus {
    backend: Backend,
}

impl HardwareOpus {
    pub fn new() -> Self {
        Self {
            backend: Backend::Local(Mutex::new(State::default())),
        }
    }

    pub fn new_from_adsp(decoder: Arc<Mutex<AdspOpusDecoder>>) -> Self {
        Self {
            backend: Backend::Adsp(AdspBackend {
                decoder,
                shared_memory: Arc::new(Mutex::new(SharedMemory::new(0))),
                state: Mutex::new(State::default()),
                buffer_id: 0x1000,
            }),
        }
    }

    pub fn get_work_buffer_size(&self, channel: u32) -> u32 {
        match &self.backend {
            Backend::Local(state) => {
                let state = state.lock();
                if !state.running || channel == 0 {
                    return 0;
                }
                0x2000 * channel
            }
            Backend::Adsp(backend) => {
                let _guard = backend.state.lock();
                let mut decoder = backend.decoder.lock();
                decoder.set_shared_memory(backend.shared_memory.clone());
                {
                    let mut shared = backend.shared_memory.lock();
                    shared.host_send_data[0] = channel as u64;
                }
                decoder.send(Direction::Dsp, Message::GetWorkBufferSize);
                if decoder.receive(Direction::Host) != Message::GetWorkBufferSizeOK {
                    return 0;
                }
                backend.shared_memory.lock().dsp_return_data[0] as u32
            }
        }
    }

    pub fn get_work_buffer_size_for_multi_stream(
        &self,
        total_stream_count: u32,
        stereo_stream_count: u32,
    ) -> u32 {
        match &self.backend {
            Backend::Local(state) => {
                let state = state.lock();
                if !state.running || total_stream_count == 0 {
                    return 0;
                }
                0x2000 * total_stream_count.max(stereo_stream_count)
            }
            Backend::Adsp(backend) => {
                let _guard = backend.state.lock();
                let mut decoder = backend.decoder.lock();
                decoder.set_shared_memory(backend.shared_memory.clone());
                {
                    let mut shared = backend.shared_memory.lock();
                    shared.host_send_data[0] = total_stream_count as u64;
                    shared.host_send_data[1] = stereo_stream_count as u64;
                }
                decoder.send(Direction::Dsp, Message::GetWorkBufferSizeForMultiStream);
                if decoder.receive(Direction::Host) != Message::GetWorkBufferSizeForMultiStreamOK {
                    return 0;
                }
                backend.shared_memory.lock().dsp_return_data[0] as u32
            }
        }
    }

    pub fn initialize_decode_object(
        &self,
        sample_rate: u32,
        channel_count: u32,
        buffer_size: u64,
    ) -> Result {
        match &self.backend {
            Backend::Local(state) => initialize_decode_object_local(
                &mut state.lock(),
                sample_rate,
                channel_count,
                buffer_size,
            ),
            Backend::Adsp(backend) => {
                let mut state = backend.state.lock();
                let mut decoder = backend.decoder.lock();
                decoder.set_shared_memory(backend.shared_memory.clone());
                {
                    let mut shared = backend.shared_memory.lock();
                    shared.resize_transfer_memory(required_workbuffer_region_size(
                        backend.buffer_id,
                        buffer_size,
                    ));
                    shared.host_send_data[0] = backend.buffer_id;
                    shared.host_send_data[1] = buffer_size;
                    shared.host_send_data[2] = sample_rate as u64;
                    shared.host_send_data[3] = channel_count as u64;
                }
                decoder.send(Direction::Dsp, Message::InitializeDecodeObject);
                if decoder.receive(Direction::Host) != Message::InitializeDecodeObjectOK {
                    return RESULT_LIB_OPUS_INVALID_STATE;
                }
                let result = ResultCode(backend.shared_memory.lock().dsp_return_data[0] as u32);
                if result.is_success() {
                    state.mode = DecodeMode::Single;
                    state.mapped = true;
                    state.sample_rate = sample_rate;
                    state.channel_count = channel_count;
                    state.total_stream_count = 0;
                    state.stereo_stream_count = 0;
                    state.buffer_size = buffer_size;
                }
                result
            }
        }
    }

    pub fn initialize_multi_stream_decode_object(
        &self,
        sample_rate: u32,
        channel_count: u32,
        total_stream_count: u32,
        stereo_stream_count: u32,
        mappings: &[u8],
        buffer_size: u64,
    ) -> Result {
        match &self.backend {
            Backend::Local(state) => initialize_multi_stream_decode_object_local(
                &mut state.lock(),
                sample_rate,
                channel_count,
                total_stream_count,
                stereo_stream_count,
                buffer_size,
            ),
            Backend::Adsp(backend) => {
                let mut state = backend.state.lock();
                let mut decoder = backend.decoder.lock();
                decoder.set_shared_memory(backend.shared_memory.clone());
                {
                    let mut shared = backend.shared_memory.lock();
                    shared.resize_transfer_memory(required_workbuffer_region_size(
                        backend.buffer_id,
                        buffer_size,
                    ));
                    shared.host_send_data[0] = backend.buffer_id;
                    shared.host_send_data[1] = buffer_size;
                    shared.host_send_data[2] = sample_rate as u64;
                    shared.host_send_data[3] = channel_count as u64;
                    shared.host_send_data[4] = total_stream_count as u64;
                    shared.host_send_data[5] = stereo_stream_count as u64;
                    shared.channel_mapping.fill(0);
                    let mapping_count = mappings.len().min(shared.channel_mapping.len());
                    shared.channel_mapping[..mapping_count]
                        .copy_from_slice(&mappings[..mapping_count]);
                }
                decoder.send(Direction::Dsp, Message::InitializeMultiStreamDecodeObject);
                if decoder.receive(Direction::Host) != Message::InitializeMultiStreamDecodeObjectOK
                {
                    return RESULT_LIB_OPUS_INVALID_STATE;
                }
                let result = ResultCode(backend.shared_memory.lock().dsp_return_data[0] as u32);
                if result.is_success() {
                    state.mode = DecodeMode::MultiStream;
                    state.mapped = true;
                    state.sample_rate = sample_rate;
                    state.channel_count = channel_count;
                    state.total_stream_count = total_stream_count;
                    state.stereo_stream_count = stereo_stream_count;
                    state.buffer_size = buffer_size;
                }
                result
            }
        }
    }

    pub fn shutdown_decode_object(&self, buffer_size: u64) -> Result {
        match &self.backend {
            Backend::Local(state) => shutdown_decode_object_local(&mut state.lock(), buffer_size),
            Backend::Adsp(backend) => {
                let mut state = backend.state.lock();
                let mut decoder = backend.decoder.lock();
                decoder.set_shared_memory(backend.shared_memory.clone());
                {
                    let mut shared = backend.shared_memory.lock();
                    shared.host_send_data[0] = backend.buffer_id;
                    shared.host_send_data[1] = buffer_size;
                }
                decoder.send(Direction::Dsp, Message::ShutdownDecodeObject);
                if decoder.receive(Direction::Host) != Message::ShutdownDecodeObjectOK {
                    return RESULT_LIB_OPUS_INVALID_STATE;
                }
                let result = ResultCode(backend.shared_memory.lock().dsp_return_data[0] as u32);
                if result.is_success() {
                    *state = State::default();
                }
                result
            }
        }
    }

    pub fn shutdown_multi_stream_decode_object(&self, buffer_size: u64) -> Result {
        match &self.backend {
            Backend::Local(state) => {
                shutdown_multi_stream_decode_object_local(&mut state.lock(), buffer_size)
            }
            Backend::Adsp(backend) => {
                let mut state = backend.state.lock();
                let mut decoder = backend.decoder.lock();
                decoder.set_shared_memory(backend.shared_memory.clone());
                {
                    let mut shared = backend.shared_memory.lock();
                    shared.host_send_data[0] = backend.buffer_id;
                    shared.host_send_data[1] = buffer_size;
                }
                decoder.send(Direction::Dsp, Message::ShutdownMultiStreamDecodeObject);
                if decoder.receive(Direction::Host) != Message::ShutdownMultiStreamDecodeObjectOK {
                    return RESULT_LIB_OPUS_INVALID_STATE;
                }
                let result = ResultCode(backend.shared_memory.lock().dsp_return_data[0] as u32);
                if result.is_success() {
                    *state = State::default();
                }
                result
            }
        }
    }

    pub fn reset_decode_object(&self) -> Result {
        match &self.backend {
            Backend::Local(state) => reset_decode_object_local(&state.lock()),
            Backend::Adsp(backend) => reset_decode_object_adsp(backend, false),
        }
    }

    pub fn reset_multi_stream_decode_object(&self) -> Result {
        match &self.backend {
            Backend::Local(state) => reset_multi_stream_decode_object_local(&state.lock()),
            Backend::Adsp(backend) => reset_decode_object_adsp(backend, true),
        }
    }

    pub fn decode_interleaved(
        &self,
        out_sample_count: &mut u32,
        output_data: &mut [u8],
        channel_count: u32,
        input_data: &[u8],
        out_time_taken: &mut u64,
        reset: bool,
    ) -> Result {
        match &self.backend {
            Backend::Local(state) => decode_interleaved_local(
                &*state.lock(),
                out_sample_count,
                output_data,
                channel_count,
                input_data,
                out_time_taken,
            ),
            Backend::Adsp(backend) => decode_interleaved_adsp(
                backend,
                out_sample_count,
                output_data,
                channel_count,
                input_data,
                out_time_taken,
                reset,
                false,
            ),
        }
    }

    pub fn decode_interleaved_for_multi_stream(
        &self,
        out_sample_count: &mut u32,
        output_data: &mut [u8],
        channel_count: u32,
        input_data: &[u8],
        out_time_taken: &mut u64,
        reset: bool,
    ) -> Result {
        match &self.backend {
            Backend::Local(state) => decode_interleaved_for_multi_stream_local(
                &*state.lock(),
                out_sample_count,
                output_data,
                channel_count,
                input_data,
                out_time_taken,
            ),
            Backend::Adsp(backend) => decode_interleaved_adsp(
                backend,
                out_sample_count,
                output_data,
                channel_count,
                input_data,
                out_time_taken,
                reset,
                true,
            ),
        }
    }

    pub fn map_memory(&self, buffer_size: u64) -> Result {
        match &self.backend {
            Backend::Local(state) => map_memory_local(&mut state.lock(), buffer_size),
            Backend::Adsp(backend) => {
                let mut state = backend.state.lock();
                let mut decoder = backend.decoder.lock();
                decoder.set_shared_memory(backend.shared_memory.clone());
                {
                    let mut shared = backend.shared_memory.lock();
                    shared.host_send_data[0] = backend.buffer_id;
                    shared.host_send_data[1] = buffer_size;
                }
                decoder.send(Direction::Dsp, Message::MapMemory);
                if decoder.receive(Direction::Host) != Message::MapMemoryOK {
                    return RESULT_LIB_OPUS_INVALID_STATE;
                }
                let result = ResultCode(backend.shared_memory.lock().dsp_return_data[0] as u32);
                if result.is_success() {
                    state.mapped = true;
                }
                result
            }
        }
    }

    pub fn unmap_memory(&self, buffer_size: u64) -> Result {
        match &self.backend {
            Backend::Local(state) => unmap_memory_local(&mut state.lock(), buffer_size),
            Backend::Adsp(backend) => {
                let mut state = backend.state.lock();
                let mut decoder = backend.decoder.lock();
                decoder.set_shared_memory(backend.shared_memory.clone());
                {
                    let mut shared = backend.shared_memory.lock();
                    shared.host_send_data[0] = backend.buffer_id;
                    shared.host_send_data[1] = buffer_size;
                }
                decoder.send(Direction::Dsp, Message::UnmapMemory);
                if decoder.receive(Direction::Host) != Message::UnmapMemoryOK {
                    return RESULT_LIB_OPUS_INVALID_STATE;
                }
                let result = ResultCode(backend.shared_memory.lock().dsp_return_data[0] as u32);
                if result.is_success() {
                    state.mapped = false;
                }
                result
            }
        }
    }
}

impl Default for HardwareOpus {
    fn default() -> Self {
        Self::new()
    }
}

fn initialize_decode_object_local(
    state: &mut State,
    sample_rate: u32,
    channel_count: u32,
    buffer_size: u64,
) -> Result {
    let result = validate_sample_rate(sample_rate);
    if result.is_error() {
        return result;
    }
    let result = validate_single_stream_channel_count(channel_count);
    if result.is_error() {
        return result;
    }
    if buffer_size == 0 {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }

    state.mode = DecodeMode::Single;
    state.mapped = true;
    state.sample_rate = sample_rate;
    state.channel_count = channel_count;
    state.total_stream_count = 0;
    state.stereo_stream_count = 0;
    state.buffer_size = buffer_size;
    ResultCode::SUCCESS
}

fn initialize_multi_stream_decode_object_local(
    state: &mut State,
    sample_rate: u32,
    channel_count: u32,
    total_stream_count: u32,
    stereo_stream_count: u32,
    buffer_size: u64,
) -> Result {
    let result = validate_sample_rate(sample_rate);
    if result.is_error() {
        return result;
    }
    if channel_count == 0
        || channel_count > OPUS_STREAM_COUNT_MAX as u32
        || total_stream_count == 0
        || buffer_size == 0
    {
        return RESULT_INVALID_OPUS_CHANNEL_COUNT;
    }
    if stereo_stream_count > total_stream_count
        || total_stream_count.saturating_add(stereo_stream_count) > channel_count
    {
        return RESULT_INVALID_OPUS_CHANNEL_COUNT;
    }

    state.mode = DecodeMode::MultiStream;
    state.mapped = true;
    state.sample_rate = sample_rate;
    state.channel_count = channel_count;
    state.total_stream_count = total_stream_count;
    state.stereo_stream_count = stereo_stream_count;
    state.buffer_size = buffer_size;
    ResultCode::SUCCESS
}

fn shutdown_decode_object_local(state: &mut State, buffer_size: u64) -> Result {
    if state.mode != DecodeMode::Single || state.buffer_size != buffer_size {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    *state = State::default();
    ResultCode::SUCCESS
}

fn shutdown_multi_stream_decode_object_local(state: &mut State, buffer_size: u64) -> Result {
    if state.mode != DecodeMode::MultiStream || state.buffer_size != buffer_size {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    *state = State::default();
    ResultCode::SUCCESS
}

fn reset_decode_object_local(state: &State) -> Result {
    if state.mode != DecodeMode::Single || !state.mapped || state.buffer_size == 0 {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    ResultCode::SUCCESS
}

fn reset_multi_stream_decode_object_local(state: &State) -> Result {
    if state.mode != DecodeMode::MultiStream || !state.mapped || state.buffer_size == 0 {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    ResultCode::SUCCESS
}

fn decode_interleaved_local(
    state: &State,
    out_sample_count: &mut u32,
    output_data: &mut [u8],
    channel_count: u32,
    input_data: &[u8],
    out_time_taken: &mut u64,
) -> Result {
    if state.mode != DecodeMode::Single || !state.mapped || state.channel_count != channel_count {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    do_decode(
        out_sample_count,
        output_data,
        channel_count,
        input_data,
        out_time_taken,
    )
}

fn decode_interleaved_for_multi_stream_local(
    state: &State,
    out_sample_count: &mut u32,
    output_data: &mut [u8],
    channel_count: u32,
    input_data: &[u8],
    out_time_taken: &mut u64,
) -> Result {
    if state.mode != DecodeMode::MultiStream
        || !state.mapped
        || state.channel_count != channel_count
    {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    do_decode(
        out_sample_count,
        output_data,
        channel_count,
        input_data,
        out_time_taken,
    )
}

fn map_memory_local(state: &mut State, buffer_size: u64) -> Result {
    if buffer_size == 0 {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    if state.buffer_size != 0 && state.buffer_size != buffer_size {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    state.mapped = true;
    ResultCode::SUCCESS
}

fn unmap_memory_local(state: &mut State, buffer_size: u64) -> Result {
    if buffer_size == 0 || (state.buffer_size != 0 && state.buffer_size != buffer_size) {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    state.mapped = false;
    ResultCode::SUCCESS
}

fn decode_interleaved_adsp(
    backend: &AdspBackend,
    out_sample_count: &mut u32,
    output_data: &mut [u8],
    channel_count: u32,
    input_data: &[u8],
    out_time_taken: &mut u64,
    reset: bool,
    multi_stream: bool,
) -> Result {
    let state = backend.state.lock();
    if !state.mapped || state.channel_count != channel_count {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    if (!multi_stream && state.mode != DecodeMode::Single)
        || (multi_stream && state.mode != DecodeMode::MultiStream)
    {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }

    let packet_size = size_of::<u32>() * 2 + input_data.len();
    let input_offset = 0x40usize;
    let output_offset = align_up((input_offset + packet_size) as u64, 0x40) as usize;
    let required_transfer_size =
        required_workbuffer_region_size(backend.buffer_id, state.buffer_size)
            .max(output_offset.saturating_add(output_data.len()));

    let mut decoder = backend.decoder.lock();
    decoder.set_shared_memory(backend.shared_memory.clone());
    {
        let mut shared = backend.shared_memory.lock();
        shared.resize_transfer_memory(required_transfer_size);
        let mut packet = Vec::with_capacity(packet_size);
        packet.extend_from_slice(&(input_data.len() as u32).to_be_bytes());
        packet.extend_from_slice(&0u32.to_be_bytes());
        packet.extend_from_slice(input_data);
        let _ = shared.write_transfer(input_offset, &packet);
        shared.host_send_data[0] = backend.buffer_id;
        shared.host_send_data[1] = input_offset as u64;
        shared.host_send_data[2] = packet.len() as u64;
        shared.host_send_data[3] = output_offset as u64;
        shared.host_send_data[4] = output_data.len() as u64;
        shared.host_send_data[5] = 0;
        shared.host_send_data[6] = reset as u64;
    }
    decoder.send(
        Direction::Dsp,
        if multi_stream {
            Message::DecodeInterleavedForMultiStream
        } else {
            Message::DecodeInterleaved
        },
    );
    let expected = if multi_stream {
        Message::DecodeInterleavedForMultiStreamOK
    } else {
        Message::DecodeInterleavedOK
    };
    if decoder.receive(Direction::Host) != expected {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }

    let shared = backend.shared_memory.lock();
    let result = ResultCode(shared.dsp_return_data[0] as u32);
    if result.is_error() {
        return result;
    }
    *out_sample_count = shared.dsp_return_data[1] as u32;
    *out_time_taken = shared.dsp_return_data[2] * 1000;
    let output_size = (*out_sample_count as usize)
        .saturating_mul(state.channel_count.max(1) as usize)
        .saturating_mul(size_of::<i16>());
    let Some(output) = shared.read_transfer(output_offset, output_size.min(output_data.len()))
    else {
        return RESULT_BUFFER_TOO_SMALL;
    };
    output_data[..output.len()].copy_from_slice(output);
    ResultCode::SUCCESS
}

fn reset_decode_object_adsp(backend: &AdspBackend, multi_stream: bool) -> Result {
    let state = backend.state.lock();
    if !state.mapped || state.buffer_size == 0 {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    if (!multi_stream && state.mode != DecodeMode::Single)
        || (multi_stream && state.mode != DecodeMode::MultiStream)
    {
        return RESULT_LIB_OPUS_INVALID_STATE;
    }
    ResultCode::SUCCESS
}

fn required_workbuffer_region_size(buffer_id: u64, buffer_size: u64) -> usize {
    buffer_id.saturating_add(buffer_size).min(usize::MAX as u64) as usize
}

fn validate_sample_rate(sample_rate: u32) -> Result {
    if matches!(sample_rate, 8_000 | 12_000 | 16_000 | 24_000 | 48_000) {
        ResultCode::SUCCESS
    } else {
        RESULT_INVALID_OPUS_SAMPLE_RATE
    }
}

fn validate_single_stream_channel_count(channel_count: u32) -> Result {
    if (1..=MAX_CHANNELS as u32).contains(&channel_count) {
        ResultCode::SUCCESS
    } else {
        RESULT_INVALID_OPUS_CHANNEL_COUNT
    }
}

fn do_decode(
    out_sample_count: &mut u32,
    output_data: &mut [u8],
    channel_count: u32,
    input_data: &[u8],
    out_time_taken: &mut u64,
) -> Result {
    if channel_count == 0 {
        return RESULT_INVALID_OPUS_CHANNEL_COUNT;
    }
    if input_data.is_empty() {
        return RESULT_BUFFER_TOO_SMALL;
    }
    let bytes_per_frame = channel_count as usize * 2;
    if output_data.len() < bytes_per_frame {
        return RESULT_BUFFER_TOO_SMALL;
    }

    *out_sample_count = (output_data.len() / bytes_per_frame) as u32;
    output_data.fill(0);
    *out_time_taken = input_data.len() as u64 * 1000;
    ResultCode::SUCCESS
}

impl Clone for HardwareOpus {
    fn clone(&self) -> Self {
        match &self.backend {
            Backend::Local(state) => Self {
                backend: Backend::Local(Mutex::new(*state.lock())),
            },
            Backend::Adsp(backend) => Self {
                backend: Backend::Adsp(AdspBackend {
                    decoder: backend.decoder.clone(),
                    shared_memory: Arc::new(Mutex::new(backend.shared_memory.lock().clone())),
                    state: Mutex::new(*backend.state.lock()),
                    buffer_id: backend.buffer_id,
                }),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn packet(size: usize) -> Vec<u8> {
        (0..size).map(|i| i as u8).collect()
    }

    #[test]
    fn decode_requires_initialized_single_stream_state() {
        let opus = HardwareOpus::new();
        let mut out_samples = 0;
        let mut out_time = 0;
        let mut output = vec![0; 256];

        assert_eq!(
            opus.decode_interleaved(
                &mut out_samples,
                &mut output,
                2,
                &[1, 2, 3],
                &mut out_time,
                false
            ),
            RESULT_LIB_OPUS_INVALID_STATE
        );
        assert_eq!(
            opus.initialize_decode_object(48_000, 2, 0x4000),
            ResultCode::SUCCESS
        );
        assert_eq!(
            opus.decode_interleaved(
                &mut out_samples,
                &mut output,
                2,
                &[1, 2, 3],
                &mut out_time,
                false
            ),
            ResultCode::SUCCESS
        );
        assert_eq!(out_samples, 64);
        assert_eq!(out_time, 3000);
    }

    #[test]
    fn shutdown_validates_mode_and_buffer_size() {
        let opus = HardwareOpus::new();
        assert_eq!(
            opus.initialize_multi_stream_decode_object(48_000, 4, 2, 1, &[0, 1, 2, 3], 0x5000),
            ResultCode::SUCCESS
        );
        assert_eq!(
            opus.shutdown_decode_object(0x5000),
            RESULT_LIB_OPUS_INVALID_STATE
        );
        assert_eq!(
            opus.shutdown_multi_stream_decode_object(0x4000),
            RESULT_LIB_OPUS_INVALID_STATE
        );
        assert_eq!(
            opus.shutdown_multi_stream_decode_object(0x5000),
            ResultCode::SUCCESS
        );
    }

    #[test]
    fn map_and_unmap_validate_buffer_size() {
        let opus = HardwareOpus::new();
        assert_eq!(
            opus.initialize_decode_object(48_000, 2, 0x4000),
            ResultCode::SUCCESS
        );
        assert_eq!(opus.map_memory(0), RESULT_LIB_OPUS_INVALID_STATE);
        assert_eq!(opus.map_memory(0x5000), RESULT_LIB_OPUS_INVALID_STATE);
        assert_eq!(opus.map_memory(0x4000), ResultCode::SUCCESS);
        assert_eq!(opus.unmap_memory(0x5000), RESULT_LIB_OPUS_INVALID_STATE);
        assert_eq!(opus.unmap_memory(0x4000), ResultCode::SUCCESS);
    }

    #[test]
    fn multistream_initialize_rejects_channel_count_above_protocol_max() {
        let opus = HardwareOpus::new();
        assert_eq!(
            opus.initialize_multi_stream_decode_object(
                48_000,
                OPUS_STREAM_COUNT_MAX as u32 + 1,
                1,
                0,
                &[],
                0x4000,
            ),
            RESULT_INVALID_OPUS_CHANNEL_COUNT
        );
    }

    #[test]
    fn reset_decode_object_validates_initialized_single_stream_state() {
        let opus = HardwareOpus::new();

        assert_eq!(opus.reset_decode_object(), RESULT_LIB_OPUS_INVALID_STATE);
        assert_eq!(
            opus.initialize_decode_object(48_000, 2, 0x4000),
            ResultCode::SUCCESS
        );
        assert_eq!(opus.reset_decode_object(), ResultCode::SUCCESS);
    }

    #[test]
    fn reset_multi_stream_decode_object_validates_initialized_multi_stream_state() {
        let opus = HardwareOpus::new();

        assert_eq!(
            opus.reset_multi_stream_decode_object(),
            RESULT_LIB_OPUS_INVALID_STATE
        );
        assert_eq!(
            opus.initialize_multi_stream_decode_object(48_000, 4, 2, 1, &[0, 1, 2, 3], 0x5000),
            ResultCode::SUCCESS
        );
        assert_eq!(opus.reset_multi_stream_decode_object(), ResultCode::SUCCESS);
    }

    #[test]
    fn adsp_backend_round_trips_basic_decode() {
        let decoder = Arc::new(Mutex::new(AdspOpusDecoder::new(Arc::new(Mutex::new(
            ruzu_core::core::System::new(),
        )))));
        {
            let decoder = decoder.lock();
            decoder.send(Direction::Dsp, Message::Start);
            assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        }

        let opus = HardwareOpus::new_from_adsp(decoder);
        let mut out_samples = 0;
        let mut out_time = 0;
        let mut output = vec![0; 0x2000];

        assert_eq!(opus.get_work_buffer_size(2), 0x4000);
        assert_eq!(
            opus.initialize_decode_object(48_000, 2, 0x6000),
            ResultCode::SUCCESS
        );
        assert_eq!(
            opus.decode_interleaved(
                &mut out_samples,
                &mut output,
                2,
                &packet(64),
                &mut out_time,
                false,
            ),
            ResultCode::SUCCESS
        );
        assert!(out_samples > 0);
        assert!(out_time > 0);
    }
}

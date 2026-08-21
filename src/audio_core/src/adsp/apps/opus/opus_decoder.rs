use crate::adsp::apps::opus::opus_decode_object::{
    OpusDecodeObject, OPUS_INVALID_PACKET, OPUS_INVALID_STATE, OPUS_OK,
};
use crate::adsp::apps::opus::opus_multistream_decode_object::OpusMultiStreamDecodeObject;
use crate::adsp::apps::opus::shared_memory::SharedMemoryHandle;
use crate::adsp::mailbox::{Direction as MailboxDirection, Mailbox};
use crate::opus::parameters::OPUS_STREAM_COUNT_MAX;
use crate::SharedSystem;
use common::thread::set_current_thread_name;
use parking_lot::Mutex;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::{self, JoinHandle};

const OPUS_BUFFER_TOO_SMALL: i32 = -2;

fn is_valid_channel_count(channel_count: i32) -> bool {
    channel_count == 1 || channel_count == 2
}

fn is_valid_multi_stream_channel_count(channel_count: i32) -> bool {
    channel_count <= OPUS_STREAM_COUNT_MAX as i32
}

fn is_valid_multi_stream_stream_counts(total_stream_count: i32, stereo_stream_count: i32) -> bool {
    is_valid_multi_stream_channel_count(total_stream_count)
        && total_stream_count > 0
        && stereo_stream_count >= 0
        && stereo_stream_count <= total_stream_count
}

fn soft_assert(condition: bool, expression: &str) {
    if !condition {
        // Eden's ASSERT logs and continues unless use_debug_asserts is enabled.
        log::error!("audio_core/adsp/apps/opus/opus_decoder.cpp: assert {expression}");
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Host,
    Dsp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(u32)]
pub enum Message {
    Invalid = 0,
    Start = 1,
    Shutdown = 2,
    StartOK = 11,
    ShutdownOK = 12,
    GetWorkBufferSize = 21,
    InitializeDecodeObject = 22,
    ShutdownDecodeObject = 23,
    DecodeInterleaved = 24,
    MapMemory = 25,
    UnmapMemory = 26,
    GetWorkBufferSizeForMultiStream = 27,
    InitializeMultiStreamDecodeObject = 28,
    ShutdownMultiStreamDecodeObject = 29,
    DecodeInterleavedForMultiStream = 30,
    GetWorkBufferSizeOK = 41,
    InitializeDecodeObjectOK = 42,
    ShutdownDecodeObjectOK = 43,
    DecodeInterleavedOK = 44,
    MapMemoryOK = 45,
    UnmapMemoryOK = 46,
    GetWorkBufferSizeForMultiStreamOK = 47,
    InitializeMultiStreamDecodeObjectOK = 48,
    ShutdownMultiStreamDecodeObjectOK = 49,
    DecodeInterleavedForMultiStreamOK = 50,
}

pub struct OpusDecoder {
    system: SharedSystem,
    mailbox: Mailbox,
    init_thread: Option<JoinHandle<()>>,
    main_thread: Arc<Mutex<Option<JoinHandle<()>>>>,
    running: Arc<AtomicBool>,
    stop_requested: Arc<AtomicBool>,
    shared_memory: Arc<Mutex<SharedMemoryHandle>>,
}

struct DecoderState {
    decoders: HashMap<DecoderKey, ActiveDecoder>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct DecoderKey {
    shared_memory: usize,
    buffer: u64,
}

enum ActiveDecoder {
    Single {
        object: OpusDecodeObject,
        channel_count: u32,
    },
    MultiStream {
        object: OpusMultiStreamDecodeObject,
        channel_count: u32,
    },
}

impl DecoderState {
    fn new() -> Self {
        Self {
            decoders: HashMap::new(),
        }
    }
}

fn decoder_key(shared_memory: &SharedMemoryHandle, buffer: u64) -> DecoderKey {
    DecoderKey {
        shared_memory: Arc::as_ptr(shared_memory) as usize,
        buffer,
    }
}

impl OpusDecoder {
    pub fn new(system: SharedSystem) -> Self {
        let state = Arc::new(Mutex::new(DecoderState::new()));
        let mut decoder = Self {
            system,
            mailbox: Mailbox::default(),
            init_thread: None,
            main_thread: Arc::new(Mutex::new(None)),
            running: Arc::new(AtomicBool::new(false)),
            stop_requested: Arc::new(AtomicBool::new(false)),
            shared_memory: Arc::new(Mutex::new(Arc::new(Mutex::new(
                crate::adsp::apps::opus::SharedMemory::new(0),
            )))),
        };

        let init_thread = {
            let mailbox = decoder.mailbox.clone();
            let system = decoder.system.clone();
            let running = decoder.running.clone();
            let stop_requested = decoder.stop_requested.clone();
            let main_thread = decoder.main_thread.clone();
            let shared_memory = decoder.shared_memory.clone();
            let state = state.clone();
            thread::Builder::new()
                .name("DSP_OpusDecoder_Init".to_string())
                .spawn(move || {
                    Self::init(
                        system,
                        mailbox,
                        running,
                        stop_requested,
                        main_thread,
                        shared_memory,
                        state,
                    )
                })
                .expect("failed to spawn DSP opus init thread")
        };
        decoder.init_thread = Some(init_thread);
        decoder
    }

    pub fn send(&self, direction: Direction, message: Message) {
        self.mailbox.send(direction.into(), message as u32);
    }

    pub fn receive(&self, direction: Direction) -> Message {
        Self::decode_message(self.mailbox.receive(direction.into()))
    }

    pub fn receive_with_stop(
        &self,
        direction: Direction,
        stop_requested: &AtomicBool,
    ) -> Option<Message> {
        self.mailbox
            .receive_with_stop(direction.into(), stop_requested)
            .map(Self::decode_message)
    }

    pub fn is_running(&self) -> bool {
        self.running.load(Ordering::SeqCst)
    }

    pub fn set_shared_memory(&mut self, shared_memory: SharedMemoryHandle) {
        *self.shared_memory.lock() = shared_memory;
    }

    pub fn shared_memory(&self) -> SharedMemoryHandle {
        self.shared_memory.lock().clone()
    }

    pub fn shutdown(&mut self) {
        if self.running.load(Ordering::SeqCst) {
            self.mailbox
                .send(MailboxDirection::Dsp, Message::Shutdown as u32);
            let msg = self.mailbox.receive(MailboxDirection::Host);
            debug_assert_eq!(
                msg,
                Message::ShutdownOK as u32,
                "Expected Opus shutdown code {:?}, got {}",
                Message::ShutdownOK,
                msg
            );
            self.running.store(false, Ordering::SeqCst);
        }

        self.stop_requested.store(true, Ordering::SeqCst);
        self.mailbox.reset();

        if let Some(thread) = self.main_thread.lock().take() {
            let _ = thread.join();
        }
        if let Some(thread) = self.init_thread.take() {
            let _ = thread.join();
        }
    }

    fn init(
        system: SharedSystem,
        mailbox: Mailbox,
        running: Arc<AtomicBool>,
        stop_requested: Arc<AtomicBool>,
        main_thread: Arc<Mutex<Option<JoinHandle<()>>>>,
        shared_memory: Arc<Mutex<SharedMemoryHandle>>,
        state: Arc<Mutex<DecoderState>>,
    ) {
        set_current_thread_name("DSP_OpusDecoder_Init");

        let Some(message) = mailbox.receive_with_stop(MailboxDirection::Dsp, &stop_requested)
        else {
            return;
        };
        if message != Message::Start as u32 {
            log::error!(
                "DSP OpusDecoder failed to receive Start message. Opus initialization failed."
            );
            return;
        }

        let main_mailbox = mailbox.clone();
        let main_running = running.clone();
        let thread = thread::Builder::new()
            .name("DSP_OpusDecoder_Main".to_string())
            .spawn(move || {
                Self::main(
                    system,
                    main_mailbox,
                    main_running,
                    stop_requested,
                    shared_memory,
                    state,
                )
            })
            .expect("failed to spawn DSP opus main thread");
        *main_thread.lock() = Some(thread);
        running.store(true, Ordering::SeqCst);
        mailbox.send(MailboxDirection::Host, Message::StartOK as u32);
    }

    fn main(
        system: SharedSystem,
        mailbox: Mailbox,
        running: Arc<AtomicBool>,
        stop_requested: Arc<AtomicBool>,
        shared_memory: Arc<Mutex<SharedMemoryHandle>>,
        state: Arc<Mutex<DecoderState>>,
    ) {
        set_current_thread_name("DSP_OpusDecoder_Main");

        while !stop_requested.load(Ordering::SeqCst) {
            let Some(message) = mailbox.receive_with_stop(MailboxDirection::Dsp, &stop_requested)
            else {
                break;
            };

            if message == Message::Shutdown as u32 {
                mailbox.send(MailboxDirection::Host, Message::ShutdownOK as u32);
                running.store(false, Ordering::SeqCst);
                return;
            }

            let message = Self::decode_message(message);
            let response = match message {
                Message::GetWorkBufferSize => {
                    Self::process_get_work_buffer_size(&shared_memory);
                    Message::GetWorkBufferSizeOK
                }
                Message::InitializeDecodeObject => {
                    Self::process_initialize_decode_object(&shared_memory, &state);
                    Message::InitializeDecodeObjectOK
                }
                Message::ShutdownDecodeObject => {
                    Self::process_shutdown_decode_object(&shared_memory, &state);
                    Message::ShutdownDecodeObjectOK
                }
                Message::DecodeInterleaved => {
                    Self::process_decode_interleaved(&system, &shared_memory, &state, false);
                    Message::DecodeInterleavedOK
                }
                Message::MapMemory => {
                    Self::process_map_memory(&shared_memory, &state);
                    Message::MapMemoryOK
                }
                Message::UnmapMemory => {
                    Self::process_unmap_memory(&shared_memory, &state);
                    Message::UnmapMemoryOK
                }
                Message::GetWorkBufferSizeForMultiStream => {
                    Self::process_get_work_buffer_size_for_multi_stream(&shared_memory);
                    Message::GetWorkBufferSizeForMultiStreamOK
                }
                Message::InitializeMultiStreamDecodeObject => {
                    Self::process_initialize_multi_stream_decode_object(&shared_memory, &state);
                    Message::InitializeMultiStreamDecodeObjectOK
                }
                Message::ShutdownMultiStreamDecodeObject => {
                    Self::process_shutdown_multi_stream_decode_object(&shared_memory, &state);
                    Message::ShutdownMultiStreamDecodeObjectOK
                }
                Message::DecodeInterleavedForMultiStream => {
                    Self::process_decode_interleaved(&system, &shared_memory, &state, true);
                    Message::DecodeInterleavedForMultiStreamOK
                }
                Message::Invalid
                | Message::Start
                | Message::StartOK
                | Message::ShutdownOK
                | Message::GetWorkBufferSizeOK
                | Message::InitializeDecodeObjectOK
                | Message::ShutdownDecodeObjectOK
                | Message::DecodeInterleavedOK
                | Message::MapMemoryOK
                | Message::UnmapMemoryOK
                | Message::GetWorkBufferSizeForMultiStreamOK
                | Message::InitializeMultiStreamDecodeObjectOK
                | Message::ShutdownMultiStreamDecodeObjectOK
                | Message::DecodeInterleavedForMultiStreamOK => {
                    log::error!("Invalid OpusDecoder command {:?}", message);
                    continue;
                }
                Message::Shutdown => unreachable!(),
            };

            mailbox.send(MailboxDirection::Host, response as u32);
        }
        running.store(false, Ordering::SeqCst);
    }

    fn process_get_work_buffer_size(shared_memory: &Arc<Mutex<SharedMemoryHandle>>) {
        let binding = shared_memory.lock().clone();
        let channel_count = binding.lock().host_send_data[0] as i32;
        soft_assert(
            is_valid_channel_count(channel_count),
            "IsValidChannelCount(channel_count)",
        );
        let size = OpusDecodeObject::get_work_buffer_size(channel_count as u32);
        binding.lock().dsp_return_data[0] = size as u64;
    }

    fn process_get_work_buffer_size_for_multi_stream(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
    ) {
        let binding = shared_memory.lock().clone();
        let shared = binding.lock();
        let total_stream_count = shared.host_send_data[0] as i32;
        let stereo_stream_count = shared.host_send_data[1] as i32;
        drop(shared);
        soft_assert(
            is_valid_multi_stream_stream_counts(total_stream_count, stereo_stream_count),
            "IsValidMultiStreamStreamCounts(total_stream_count, stereo_stream_count)",
        );
        let size = OpusMultiStreamDecodeObject::get_work_buffer_size(
            total_stream_count as u32,
            stereo_stream_count as u32,
        );
        binding.lock().dsp_return_data[0] = size as u64;
    }

    fn process_initialize_decode_object(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
    ) {
        let binding = shared_memory.lock().clone();
        let shared = binding.lock();
        let buffer = shared.host_send_data[0];
        let buffer_size = shared.host_send_data[1];
        let sample_rate = shared.host_send_data[2] as i32;
        let channel_count = shared.host_send_data[3] as i32;
        drop(shared);
        let key = decoder_key(&binding, buffer);

        soft_assert(sample_rate >= 0, "sample_rate >= 0");
        soft_assert(
            is_valid_channel_count(channel_count),
            "IsValidChannelCount(channel_count)",
        );
        soft_assert(
            buffer_size >= OpusDecodeObject::get_work_buffer_size(channel_count as u32) as u64,
            "buffer_size >= OpusDecodeObject::GetWorkBufferSize(channel_count)",
        );

        let result = {
            let mut state = state.lock();
            if state.decoders.contains_key(&key) {
                OPUS_OK
            } else {
                let mut decode_object = OpusDecodeObject::initialize(buffer, buffer, None);
                let result =
                    decode_object.initialize_decoder(sample_rate as u32, channel_count as u32);
                if result == OPUS_OK {
                    state.decoders.insert(
                        key,
                        ActiveDecoder::Single {
                            object: decode_object,
                            channel_count: channel_count as u32,
                        },
                    );
                }
                result
            }
        };
        binding.lock().dsp_return_data[0] = result as i64 as u64;
    }

    fn process_initialize_multi_stream_decode_object(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
    ) {
        let binding = shared_memory.lock().clone();
        let shared = binding.lock();
        let buffer = shared.host_send_data[0];
        let buffer_size = shared.host_send_data[1];
        let sample_rate = shared.host_send_data[2] as i32;
        let channel_count = shared.host_send_data[3] as i32;
        let total_stream_count = shared.host_send_data[4] as i32;
        let stereo_stream_count = shared.host_send_data[5] as i32;
        let mut mappings = [0u8; OPUS_STREAM_COUNT_MAX + 1];
        mappings.copy_from_slice(&shared.channel_mapping);
        drop(shared);
        let key = decoder_key(&binding, buffer);

        soft_assert(
            is_valid_multi_stream_stream_counts(total_stream_count, stereo_stream_count),
            "IsValidMultiStreamStreamCounts(total_stream_count, stereo_stream_count)",
        );
        soft_assert(sample_rate >= 0, "sample_rate >= 0");
        soft_assert(
            buffer_size
                >= OpusMultiStreamDecodeObject::get_work_buffer_size(
                    total_stream_count as u32,
                    stereo_stream_count as u32,
                ) as u64,
            "buffer_size >= OpusMultiStreamDecodeObject::GetWorkBufferSize(total_stream_count, stereo_stream_count)",
        );

        let result = {
            let mut state = state.lock();
            if state.decoders.contains_key(&key) {
                OPUS_OK
            } else {
                let mut decode_object =
                    OpusMultiStreamDecodeObject::initialize(buffer, buffer, None);
                let result = decode_object.initialize_decoder(
                    sample_rate as u32,
                    total_stream_count as u32,
                    channel_count as u32,
                    stereo_stream_count as u32,
                    &mappings[..channel_count as usize],
                );
                if result == OPUS_OK {
                    state.decoders.insert(
                        key,
                        ActiveDecoder::MultiStream {
                            object: decode_object,
                            channel_count: channel_count as u32,
                        },
                    );
                }
                result
            }
        };
        binding.lock().dsp_return_data[0] = result as i64 as u64;
    }

    fn process_shutdown_decode_object(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
    ) {
        let binding = shared_memory.lock().clone();
        let buffer = {
            let shared = binding.lock();
            shared.host_send_data[0]
        };
        let key = decoder_key(&binding, buffer);
        let result = {
            let mut state = state.lock();
            if let Some(active_decoder) = state.decoders.get_mut(&key) {
                let result = match active_decoder {
                    ActiveDecoder::Single { object, .. } => object.shutdown(),
                    ActiveDecoder::MultiStream { object, .. } => object.shutdown(),
                };
                if result == OPUS_OK {
                    let _ = state.decoders.remove(&key);
                }
                result
            } else {
                OPUS_OK
            }
        };
        binding.lock().dsp_return_data[0] = result as i64 as u64;
    }

    fn process_shutdown_multi_stream_decode_object(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
    ) {
        let binding = shared_memory.lock().clone();
        let buffer = {
            let shared = binding.lock();
            shared.host_send_data[0]
        };
        let key = decoder_key(&binding, buffer);
        let result = {
            let mut state = state.lock();
            if let Some(active_decoder) = state.decoders.get_mut(&key) {
                let result = match active_decoder {
                    ActiveDecoder::MultiStream { object, .. } => object.shutdown(),
                    ActiveDecoder::Single { object, .. } => object.shutdown(),
                };
                if result == OPUS_OK {
                    let _ = state.decoders.remove(&key);
                }
                result
            } else {
                OPUS_OK
            }
        };
        binding.lock().dsp_return_data[0] = result as i64 as u64;
    }

    fn process_decode_interleaved(
        system: &SharedSystem,
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
        multi_stream: bool,
    ) {
        let (
            buffer,
            input_data,
            input_data_size,
            output_data,
            output_data_size,
            final_range,
            reset_requested,
        ) = {
            let binding = shared_memory.lock().clone();
            let shared = binding.lock();
            (
                shared.host_send_data[0],
                shared.host_send_data[1] as usize,
                shared.host_send_data[2] as usize,
                shared.host_send_data[3] as usize,
                shared.host_send_data[4] as usize,
                shared.host_send_data[5] as u32,
                shared.host_send_data[6] != 0,
            )
        };

        let binding = shared_memory.lock().clone();
        let key = decoder_key(&binding, buffer);
        let input = binding
            .lock()
            .read_transfer(input_data, input_data_size)
            .map(|data| data.to_vec())
            .unwrap_or_default();
        let mut output = vec![0; output_data_size];
        let decode_start_time = system.get().core_timing().get_global_time_us().as_micros() as u64;

        let mut decoded_samples = 0;
        let (result, channel_count) = {
            let mut state = state.lock();
            match state.decoders.get_mut(&key) {
                Some(ActiveDecoder::Single {
                    object,
                    channel_count,
                }) if !multi_stream => {
                    let mut result = OPUS_OK;
                    if reset_requested {
                        result = object.reset_decoder();
                    }
                    if result == OPUS_OK {
                        result = object.decode(&mut decoded_samples, &mut output, &input);
                    }
                    if result == OPUS_OK
                        && final_range != 0
                        && object.get_final_range() != final_range
                    {
                        result = OPUS_INVALID_PACKET;
                    }
                    (result, *channel_count)
                }
                Some(ActiveDecoder::MultiStream {
                    object,
                    channel_count,
                }) if multi_stream => {
                    let mut result = OPUS_OK;
                    if reset_requested {
                        result = object.reset_decoder();
                    }
                    if result == OPUS_OK {
                        result = object.decode(&mut decoded_samples, &mut output, &input);
                    }
                    if result == OPUS_OK
                        && final_range != 0
                        && object.get_final_range() != final_range
                    {
                        result = OPUS_INVALID_PACKET;
                    }
                    (result, *channel_count)
                }
                _ => (OPUS_INVALID_STATE, 0),
            }
        };

        let mut shared = binding.lock();
        let decode_end_time = system.get().core_timing().get_global_time_us().as_micros() as u64;
        let time_taken = decode_end_time.wrapping_sub(decode_start_time);
        if result == OPUS_OK {
            let output_bytes = (decoded_samples as usize)
                .wrapping_mul(std::mem::size_of::<i16>())
                .wrapping_mul(channel_count as usize);
            if output_bytes > output.len()
                || output_bytes > output_data_size
                || !shared.write_transfer(output_data, &output[..output_bytes])
            {
                shared.dsp_return_data[0] = OPUS_BUFFER_TOO_SMALL as i64 as u64;
                shared.dsp_return_data[1] = 0;
                shared.dsp_return_data[2] = 0;
                return;
            }
        }
        Self::write_decode_result(&mut shared, result, decoded_samples, time_taken);
    }

    fn process_map_memory(
        _shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        _state: &Arc<Mutex<DecoderState>>,
    ) {
    }

    fn process_unmap_memory(
        _shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        _state: &Arc<Mutex<DecoderState>>,
    ) {
    }

    fn decode_message(raw: u32) -> Message {
        match raw {
            1 => Message::Start,
            2 => Message::Shutdown,
            11 => Message::StartOK,
            12 => Message::ShutdownOK,
            21 => Message::GetWorkBufferSize,
            22 => Message::InitializeDecodeObject,
            23 => Message::ShutdownDecodeObject,
            24 => Message::DecodeInterleaved,
            25 => Message::MapMemory,
            26 => Message::UnmapMemory,
            27 => Message::GetWorkBufferSizeForMultiStream,
            28 => Message::InitializeMultiStreamDecodeObject,
            29 => Message::ShutdownMultiStreamDecodeObject,
            30 => Message::DecodeInterleavedForMultiStream,
            41 => Message::GetWorkBufferSizeOK,
            42 => Message::InitializeDecodeObjectOK,
            43 => Message::ShutdownDecodeObjectOK,
            44 => Message::DecodeInterleavedOK,
            45 => Message::MapMemoryOK,
            46 => Message::UnmapMemoryOK,
            47 => Message::GetWorkBufferSizeForMultiStreamOK,
            48 => Message::InitializeMultiStreamDecodeObjectOK,
            49 => Message::ShutdownMultiStreamDecodeObjectOK,
            50 => Message::DecodeInterleavedForMultiStreamOK,
            _ => Message::Invalid,
        }
    }

    fn write_decode_result(
        shared: &mut crate::adsp::apps::opus::SharedMemory,
        result: i32,
        decoded_samples: u32,
        time_taken: u64,
    ) {
        shared.dsp_return_data[0] = result as i64 as u64;
        shared.dsp_return_data[1] = decoded_samples as u64;
        shared.dsp_return_data[2] = time_taken;
    }
}

impl Drop for OpusDecoder {
    fn drop(&mut self) {
        self.shutdown();
    }
}

impl From<Direction> for MailboxDirection {
    fn from(value: Direction) -> Self {
        match value {
            Direction::Host => Self::Host,
            Direction::Dsp => Self::Dsp,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adsp::apps::opus::SharedMemory;
    use std::sync::atomic::AtomicBool;

    fn make_system() -> SharedSystem {
        crate::make_test_system()
    }

    fn opus_silence_packet() -> Vec<u8> {
        vec![0xF8, 0xFF, 0xFE]
    }

    #[test]
    fn start_message_starts_decoder_main_thread() {
        let decoder = OpusDecoder::new(make_system());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        assert!(decoder.is_running());
    }

    #[test]
    fn receive_with_stop_returns_none_when_stopped() {
        let decoder = OpusDecoder::new(make_system());
        let stop_requested = AtomicBool::new(true);

        assert_eq!(
            decoder.receive_with_stop(Direction::Host, &stop_requested),
            None
        );
    }

    #[test]
    fn shutdown_message_returns_shutdown_ok() {
        let decoder = OpusDecoder::new(make_system());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        decoder.send(Direction::Dsp, Message::Shutdown);
        assert_eq!(decoder.receive(Direction::Host), Message::ShutdownOK);
    }

    #[test]
    fn get_work_buffer_size_command_writes_return_slot() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x2000)));
        shared_memory.lock().host_send_data[0] = 2;
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        decoder.send(Direction::Dsp, Message::GetWorkBufferSize);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::GetWorkBufferSizeOK
        );

        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            OpusDecodeObject::get_work_buffer_size(2) as u64
        );
    }

    #[test]
    fn initialize_and_decode_interleaved_write_output_and_return_fields() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x10000;
            shared.host_send_data[2] = 48_000;
            shared.host_send_data[3] = 2;
        }
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        decoder.send(Direction::Dsp, Message::InitializeDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeDecodeObjectOK
        );
        assert_eq!(shared_memory.lock().dsp_return_data[0], OPUS_OK as u64);

        let packet = opus_silence_packet();
        {
            let mut shared = shared_memory.lock();
            assert!(shared.write_transfer(0x40, &packet));
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x40;
            shared.host_send_data[2] = packet.len() as u64;
            shared.host_send_data[3] = 0x400;
            shared.host_send_data[4] = 0x1000;
            shared.host_send_data[5] = 0;
            shared.host_send_data[6] = 0;
        }

        decoder.send(Direction::Dsp, Message::DecodeInterleaved);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::DecodeInterleavedOK
        );

        let shared = shared_memory.lock();
        assert_eq!(shared.dsp_return_data[0], OPUS_OK as u64);
        assert!(shared.dsp_return_data[1] > 0);
        assert!(shared.read_transfer(0x400, 8).is_some());
    }

    #[test]
    fn drop_before_start_does_not_hang() {
        let _decoder = OpusDecoder::new(make_system());
    }

    #[test]
    fn shutdown_decode_object_without_live_decoder_entry_returns_success() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x2000)));
        shared_memory.lock().host_send_data[0] = 0x1234;
        shared_memory.lock().host_send_data[1] = 0x2000;
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        decoder.send(Direction::Dsp, Message::ShutdownDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::ShutdownDecodeObjectOK
        );
        assert_eq!(shared_memory.lock().dsp_return_data[0], OPUS_OK as u64);
    }

    #[test]
    fn map_and_unmap_memory_acknowledge_without_decoder_state() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x2000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x2000;
        }
        decoder.set_shared_memory(shared_memory);

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);

        decoder.send(Direction::Dsp, Message::MapMemory);
        assert_eq!(decoder.receive(Direction::Host), Message::MapMemoryOK);

        decoder.send(Direction::Dsp, Message::UnmapMemory);
        assert_eq!(decoder.receive(Direction::Host), Message::UnmapMemoryOK);
    }

    #[test]
    fn get_work_buffer_size_for_multi_stream_writes_return_slot() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x2000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 2;
            shared.host_send_data[1] = 1;
        }
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        decoder.send(Direction::Dsp, Message::GetWorkBufferSizeForMultiStream);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::GetWorkBufferSizeForMultiStreamOK
        );

        assert!(shared_memory.lock().dsp_return_data[0] > 0);
    }

    #[test]
    fn initialize_decode_interleaved_and_shutdown_multi_stream_succeed() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x2000;
            shared.host_send_data[1] = 0x8000;
            shared.host_send_data[2] = 48_000;
            shared.host_send_data[3] = 2;
            shared.host_send_data[4] = 1;
            shared.host_send_data[5] = 1;
            shared.channel_mapping[0] = 0;
            shared.channel_mapping[1] = 1;
        }
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);

        decoder.send(Direction::Dsp, Message::InitializeMultiStreamDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeMultiStreamDecodeObjectOK
        );
        assert_eq!(shared_memory.lock().dsp_return_data[0], OPUS_OK as u64);

        let packet = opus_silence_packet();
        {
            let mut shared = shared_memory.lock();
            assert!(shared.write_transfer(0x40, &packet));
            shared.host_send_data[0] = 0x2000;
            shared.host_send_data[1] = 0x40;
            shared.host_send_data[2] = packet.len() as u64;
            shared.host_send_data[3] = 0x400;
            shared.host_send_data[4] = 0x1000;
            shared.host_send_data[5] = 0;
            shared.host_send_data[6] = 0;
        }

        decoder.send(Direction::Dsp, Message::DecodeInterleavedForMultiStream);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::DecodeInterleavedForMultiStreamOK
        );

        {
            let shared = shared_memory.lock();
            assert_eq!(shared.dsp_return_data[0], OPUS_OK as u64);
            assert!(shared.dsp_return_data[1] > 0);
            assert!(shared.read_transfer(0x400, 8).is_some());
        }

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x2000;
            shared.host_send_data[1] = 0x8000;
        }
        decoder.send(Direction::Dsp, Message::ShutdownMultiStreamDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::ShutdownMultiStreamDecodeObjectOK
        );
        assert_eq!(shared_memory.lock().dsp_return_data[0], OPUS_OK as u64);
    }

    #[test]
    fn initialize_decode_object_continues_after_soft_buffer_assert() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x2000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x1000;
            shared.host_send_data[2] = 48_000;
            shared.host_send_data[3] = 2;
        }
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        decoder.send(Direction::Dsp, Message::InitializeDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeDecodeObjectOK
        );
        assert_eq!(shared_memory.lock().dsp_return_data[0], OPUS_OK as u64);
    }

    #[test]
    fn shutdown_decode_object_accepts_multistream_entry_like_upstream_destructor() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x2000;
            shared.host_send_data[1] = 0x8000;
            shared.host_send_data[2] = 48_000;
            shared.host_send_data[3] = 2;
            shared.host_send_data[4] = 1;
            shared.host_send_data[5] = 1;
            shared.channel_mapping[0] = 0;
            shared.channel_mapping[1] = 1;
        }
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);
        decoder.send(Direction::Dsp, Message::InitializeMultiStreamDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeMultiStreamDecodeObjectOK
        );
        assert_eq!(shared_memory.lock().dsp_return_data[0], OPUS_OK as u64);

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x2000;
            shared.host_send_data[1] = 0x8000;
        }
        decoder.send(Direction::Dsp, Message::ShutdownDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::ShutdownDecodeObjectOK
        );
        assert_eq!(shared_memory.lock().dsp_return_data[0], OPUS_OK as u64);
    }
}

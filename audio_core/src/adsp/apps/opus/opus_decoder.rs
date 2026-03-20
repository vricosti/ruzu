use crate::adsp::apps::opus::opus_decode_object::{DecodeObjectHeader, OpusDecodeObject};
use crate::adsp::apps::opus::opus_multistream_decode_object::OpusMultiStreamDecodeObject;
use crate::adsp::apps::opus::shared_memory::SharedMemoryHandle;
use crate::adsp::mailbox::{Direction as MailboxDirection, Mailbox};
use crate::errors::{RESULT_BUFFER_TOO_SMALL, RESULT_LIB_OPUS_INVALID_STATE};
use crate::opus::parameters::{OpusPacketHeader, OPUS_STREAM_COUNT_MAX};
use crate::SharedSystem;
use common::thread::set_current_thread_name;
use common::ResultCode;
use parking_lot::Mutex;
use std::collections::HashMap;
use std::mem::size_of;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::{self, JoinHandle};

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
    decoders: HashMap<u64, ActiveDecoder>,
}

enum ActiveDecoder {
    Single(OpusDecodeObject),
    MultiStream(OpusMultiStreamDecodeObject),
}

impl DecoderState {
    fn new() -> Self {
        Self {
            decoders: HashMap::new(),
        }
    }
}

impl Default for OpusDecoder {
    fn default() -> Self {
        Self::new(Arc::new(Mutex::new(ruzu_core::core::System::new())))
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

        while !stop_requested.load(Ordering::SeqCst) {
            let Some(message) = mailbox.receive_with_stop(MailboxDirection::Dsp, &stop_requested)
            else {
                return;
            };
            if message != Message::Start as u32 {
                continue;
            }

            let thread = {
                let system = system.clone();
                let mailbox = mailbox.clone();
                let running = running.clone();
                let stop_requested = stop_requested.clone();
                let shared_memory = shared_memory.clone();
                let state = state.clone();
                thread::Builder::new()
                    .name("DSP_OpusDecoder_Main".to_string())
                    .spawn(move || {
                        Self::main(
                            system,
                            mailbox,
                            running,
                            stop_requested,
                            shared_memory,
                            state,
                        )
                    })
                    .expect("failed to spawn DSP opus main thread")
            };
            *main_thread.lock() = Some(thread);
            running.store(true, Ordering::SeqCst);
            mailbox.send(MailboxDirection::Host, Message::StartOK as u32);
            return;
        }
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
        let channel_count = binding.lock().host_send_data[0] as u32;
        let size = OpusDecodeObject::get_work_buffer_size(channel_count);
        binding.lock().dsp_return_data[0] = size as u64;
    }

    fn process_get_work_buffer_size_for_multi_stream(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
    ) {
        let binding = shared_memory.lock().clone();
        let shared = binding.lock();
        let total_stream_count = shared.host_send_data[0] as u32;
        let stereo_stream_count = shared.host_send_data[1] as u32;
        drop(shared);
        let size = OpusMultiStreamDecodeObject::get_work_buffer_size(
            total_stream_count,
            stereo_stream_count,
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
        let sample_rate = shared.host_send_data[2] as u32;
        let channel_count = shared.host_send_data[3] as u32;
        drop(shared);

        let result = {
            let mut state = state.lock();
            let required_size = OpusDecodeObject::get_work_buffer_size(channel_count);
            if required_size != 0 && buffer_size < required_size as u64 {
                RESULT_BUFFER_TOO_SMALL
            } else if state.decoders.contains_key(&buffer) {
                if OpusDecodeObject::matches_config(
                    &binding.lock(),
                    buffer,
                    sample_rate,
                    channel_count,
                    buffer_size,
                ) {
                    ResultCode::SUCCESS
                } else {
                    let mut decode_object = OpusDecodeObject::initialize(
                        buffer,
                        buffer,
                        state
                            .decoders
                            .remove(&buffer)
                            .and_then(|decoder| match decoder {
                                ActiveDecoder::Single(object) => Some(object),
                                ActiveDecoder::MultiStream(_) => None,
                            }),
                    );
                    let result =
                        decode_object.initialize_decoder(sample_rate, channel_count, buffer_size);
                    if result.is_success() {
                        state
                            .decoders
                            .insert(buffer, ActiveDecoder::Single(decode_object));
                    }
                    result
                }
            } else {
                let mut decode_object = OpusDecodeObject::initialize(buffer, buffer, None);
                let result =
                    decode_object.initialize_decoder(sample_rate, channel_count, buffer_size);
                if result.is_success() {
                    state
                        .decoders
                        .insert(buffer, ActiveDecoder::Single(decode_object));
                }
                result
            }
        };
        let mut shared = binding.lock();
        if result.is_success() {
            let _ = OpusDecodeObject::write_successful_header(
                &mut shared,
                buffer,
                sample_rate,
                channel_count,
                buffer_size,
            );
        }
        shared.dsp_return_data[0] = result.raw() as u64;
    }

    fn process_initialize_multi_stream_decode_object(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
    ) {
        let binding = shared_memory.lock().clone();
        let shared = binding.lock();
        let buffer = shared.host_send_data[0];
        let buffer_size = shared.host_send_data[1];
        let sample_rate = shared.host_send_data[2] as u32;
        let channel_count = shared.host_send_data[3] as u32;
        let total_stream_count = shared.host_send_data[4] as u32;
        let stereo_stream_count = shared.host_send_data[5] as u32;
        let mut mappings = [0u8; OPUS_STREAM_COUNT_MAX + 1];
        mappings.copy_from_slice(&shared.channel_mapping);
        drop(shared);

        let result = {
            let mut state = state.lock();
            let required_size = OpusMultiStreamDecodeObject::get_work_buffer_size(
                total_stream_count,
                stereo_stream_count,
            );
            if required_size != 0 && buffer_size < required_size as u64 {
                RESULT_BUFFER_TOO_SMALL
            } else if state.decoders.contains_key(&buffer) {
                if OpusMultiStreamDecodeObject::matches_config(
                    &binding.lock(),
                    buffer,
                    sample_rate,
                    channel_count,
                    total_stream_count,
                    stereo_stream_count,
                    buffer_size,
                ) {
                    ResultCode::SUCCESS
                } else {
                    let mut decode_object = OpusMultiStreamDecodeObject::initialize(
                        buffer,
                        buffer,
                        state
                            .decoders
                            .remove(&buffer)
                            .and_then(|decoder| match decoder {
                                ActiveDecoder::MultiStream(object) => Some(object),
                                ActiveDecoder::Single(_) => None,
                            }),
                    );
                    let result = decode_object.initialize_decoder(
                        sample_rate,
                        total_stream_count,
                        channel_count,
                        stereo_stream_count,
                        &mappings[..channel_count as usize],
                        buffer_size,
                    );
                    if result.is_success() {
                        state
                            .decoders
                            .insert(buffer, ActiveDecoder::MultiStream(decode_object));
                    }
                    result
                }
            } else {
                let mut decode_object =
                    OpusMultiStreamDecodeObject::initialize(buffer, buffer, None);
                let result = decode_object.initialize_decoder(
                    sample_rate,
                    total_stream_count,
                    channel_count,
                    stereo_stream_count,
                    &mappings[..channel_count as usize],
                    buffer_size,
                );
                if result.is_success() {
                    state
                        .decoders
                        .insert(buffer, ActiveDecoder::MultiStream(decode_object));
                }
                result
            }
        };
        let mut shared = binding.lock();
        if result.is_success() {
            let _ = OpusMultiStreamDecodeObject::write_successful_header(
                &mut shared,
                buffer,
                sample_rate,
                channel_count,
                total_stream_count,
                stereo_stream_count,
                buffer_size,
            );
        }
        shared.dsp_return_data[0] = result.raw() as u64;
    }

    fn process_shutdown_decode_object(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
    ) {
        let binding = shared_memory.lock().clone();
        let (buffer, buffer_size) = {
            let shared = binding.lock();
            (shared.host_send_data[0], shared.host_send_data[1])
        };
        let result = {
            let mut state = state.lock();
            if let Some(active_decoder) = state.decoders.get_mut(&buffer) {
                let result = match active_decoder {
                    ActiveDecoder::Single(object) => {
                        object.shutdown_with_header(&mut binding.lock(), buffer, buffer_size)
                    }
                    ActiveDecoder::MultiStream(_) => RESULT_LIB_OPUS_INVALID_STATE,
                };
                if result.is_success() {
                    let _ = state.decoders.remove(&buffer);
                }
                result
            } else {
                ResultCode::SUCCESS
            }
        };
        binding.lock().dsp_return_data[0] = result.raw() as u64;
    }

    fn process_shutdown_multi_stream_decode_object(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
    ) {
        let binding = shared_memory.lock().clone();
        let (buffer, buffer_size) = {
            let shared = binding.lock();
            (shared.host_send_data[0], shared.host_send_data[1])
        };
        let result = {
            let mut state = state.lock();
            if let Some(active_decoder) = state.decoders.get_mut(&buffer) {
                let result = match active_decoder {
                    ActiveDecoder::MultiStream(object) => {
                        object.shutdown_with_header(&mut binding.lock(), buffer, buffer_size)
                    }
                    ActiveDecoder::Single(_) => RESULT_LIB_OPUS_INVALID_STATE,
                };
                if result.is_success() {
                    let _ = state.decoders.remove(&buffer);
                }
                result
            } else {
                ResultCode::SUCCESS
            }
        };
        binding.lock().dsp_return_data[0] = result.raw() as u64;
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
        let input = binding
            .lock()
            .read_transfer(input_data, input_data_size)
            .map(|data| data.to_vec())
            .unwrap_or_default();
        let mut output = vec![0; output_data_size];
        let decode_start_time = system.lock().core_timing().lock().unwrap().get_global_time_us().as_micros() as u64;

        let mut decoded_samples = 0;
        let mut time_taken = 0;
        let mut decoded_final_range = None;
        let result = if input.len() <= size_of::<OpusPacketHeader>() {
            RESULT_BUFFER_TOO_SMALL
        } else {
            let mut state = state.lock();
            if let Some(active_decoder) = state.decoders.get_mut(&buffer) {
                let header = read_header(&input);
                let payload_end = size_of::<OpusPacketHeader>() + header.size as usize;
                if payload_end > input.len() {
                    RESULT_BUFFER_TOO_SMALL
                } else {
                    let payload = &input[size_of::<OpusPacketHeader>()..payload_end];
                    match active_decoder {
                        ActiveDecoder::Single(object) if !multi_stream => object
                            .decode_interleaved_message(
                                &binding.lock(),
                                buffer,
                                payload,
                                &mut output,
                                &mut decoded_samples,
                                &mut time_taken,
                                header.final_range,
                                final_range,
                                reset_requested,
                                &mut decoded_final_range,
                            ),
                        ActiveDecoder::MultiStream(object) if multi_stream => object
                            .decode_interleaved_message(
                                &binding.lock(),
                                buffer,
                                payload,
                                &mut output,
                                &mut decoded_samples,
                                &mut time_taken,
                                header.final_range,
                                final_range,
                                reset_requested,
                                &mut decoded_final_range,
                            ),
                        _ => RESULT_LIB_OPUS_INVALID_STATE,
                    }
                }
            } else {
                RESULT_LIB_OPUS_INVALID_STATE
            }
        };

        let mut shared = binding.lock();
        let decode_end_time = system.lock().core_timing().lock().unwrap().get_global_time_us().as_micros() as u64;
        let core_timing_time_taken = decode_end_time.saturating_sub(decode_start_time);
        if core_timing_time_taken != 0 {
            time_taken = core_timing_time_taken;
        }
        if result.is_success() {
            let channel_count = DecodeObjectHeader::read(&shared, buffer)
                .map(|header| header.channel_count.max(1) as usize)
                .unwrap_or(1);
            let output_bytes = decoded_samples as usize * size_of::<i16>() * channel_count;
            let write_size = output_bytes.min(output.len()).min(output_data_size);
            if !shared.write_transfer(output_data, &output[..write_size]) {
                shared.dsp_return_data[0] = RESULT_BUFFER_TOO_SMALL.raw() as u64;
                shared.dsp_return_data[1] = 0;
                shared.dsp_return_data[2] = 0;
                return;
            }
            let final_range_to_write =
                decoded_final_range.unwrap_or_else(|| read_header(&input).final_range);
            let _ =
                DecodeObjectHeader::write_final_range(&mut shared, buffer, final_range_to_write);
        }
        Self::write_decode_result(&mut shared, result, decoded_samples, time_taken);
    }

    fn process_map_memory(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
    ) {
        let binding = shared_memory.lock().clone();
        let (buffer, buffer_size) = {
            let shared = binding.lock();
            (shared.host_send_data[0], shared.host_send_data[1])
        };
        let result = {
            let state = state.lock();
            if state.decoders.contains_key(&buffer) {
                let mut shared = binding.lock();
                if let Some(ActiveDecoder::Single(_)) = state.decoders.get(&buffer) {
                    OpusDecodeObject::map_memory(&mut shared, buffer, buffer_size, true)
                } else if let Some(ActiveDecoder::MultiStream(_)) = state.decoders.get(&buffer) {
                    OpusMultiStreamDecodeObject::map_memory(&mut shared, buffer, buffer_size, true)
                } else {
                    RESULT_LIB_OPUS_INVALID_STATE
                }
            } else {
                RESULT_LIB_OPUS_INVALID_STATE
            }
        };
        binding.lock().dsp_return_data[0] = result.raw() as u64;
    }

    fn process_unmap_memory(
        shared_memory: &Arc<Mutex<SharedMemoryHandle>>,
        state: &Arc<Mutex<DecoderState>>,
    ) {
        let binding = shared_memory.lock().clone();
        let (buffer, buffer_size) = {
            let shared = binding.lock();
            (shared.host_send_data[0], shared.host_send_data[1])
        };
        let result = {
            let state = state.lock();
            if state.decoders.contains_key(&buffer) {
                let mut shared = binding.lock();
                if let Some(ActiveDecoder::Single(_)) = state.decoders.get(&buffer) {
                    OpusDecodeObject::map_memory(&mut shared, buffer, buffer_size, false)
                } else if let Some(ActiveDecoder::MultiStream(_)) = state.decoders.get(&buffer) {
                    OpusMultiStreamDecodeObject::map_memory(&mut shared, buffer, buffer_size, false)
                } else {
                    RESULT_LIB_OPUS_INVALID_STATE
                }
            } else {
                RESULT_LIB_OPUS_INVALID_STATE
            }
        };
        binding.lock().dsp_return_data[0] = result.raw() as u64;
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
        result: ResultCode,
        decoded_samples: u32,
        time_taken: u64,
    ) {
        shared.dsp_return_data[0] = result.raw() as u64;
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

fn read_header(input_data: &[u8]) -> OpusPacketHeader {
    let mut header = OpusPacketHeader::default();
    if input_data.len() >= size_of::<OpusPacketHeader>() {
        header.size =
            u32::from_ne_bytes(input_data[0..4].try_into().unwrap_or([0; 4])).swap_bytes();
        header.final_range =
            u32::from_ne_bytes(input_data[4..8].try_into().unwrap_or([0; 4])).swap_bytes();
    }
    header
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adsp::apps::opus::opus_decode_object::{
        DecodeObjectHeader, DECODE_OBJECT_MAGIC, FLAG_INITIALIZED, FLAG_MAPPED, FLAG_MULTI_STREAM,
    };
    use crate::adsp::apps::opus::SharedMemory;
    use std::sync::atomic::AtomicBool;

    fn make_system() -> SharedSystem {
        Arc::new(Mutex::new(ruzu_core::core::System::new()))
    }

    fn packet_with_payload(size: usize) -> Vec<u8> {
        let mut packet = Vec::with_capacity(size_of::<OpusPacketHeader>() + size);
        packet.extend_from_slice(&(size as u32).to_be_bytes());
        packet.extend_from_slice(&0u32.to_be_bytes());
        packet.extend((0..size).map(|i| i as u8));
        packet
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

        assert_eq!(shared_memory.lock().dsp_return_data[0], 0x4000);
    }

    #[test]
    fn initialize_and_decode_interleaved_write_output_and_return_fields() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
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
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );

        let packet = packet_with_payload(64);
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
        assert_eq!(shared.dsp_return_data[0], ResultCode::SUCCESS.raw() as u64);
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
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );
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
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );

        let packet = packet_with_payload(64);
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
            assert_eq!(shared.dsp_return_data[0], ResultCode::SUCCESS.raw() as u64);
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
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );
    }

    #[test]
    fn initialize_decode_object_rejects_too_small_buffer() {
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
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            RESULT_BUFFER_TOO_SMALL.raw() as u64
        );
    }

    #[test]
    fn shutdown_decode_object_rejects_multistream_entry() {
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
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );

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
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            RESULT_LIB_OPUS_INVALID_STATE.raw() as u64
        );
    }

    #[test]
    fn initialize_decode_object_writes_header_into_workbuffer() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
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

        let header = {
            let shared = shared_memory.lock();
            DecodeObjectHeader::read(&shared, 0x1000).unwrap()
        };
        assert_eq!(header.magic, DECODE_OBJECT_MAGIC);
        assert_eq!(header.sample_rate, 48_000);
        assert_eq!(header.channel_count, 2);
        assert_eq!(header.total_stream_count, 0);
        assert_eq!(header.stereo_stream_count, 0);
        assert_eq!(header.buffer_size, 0x6000);
        assert_eq!(header.flags, FLAG_INITIALIZED | FLAG_MAPPED);
    }

    #[test]
    fn reinitialize_decode_object_with_different_sample_rate_rebuilds_header() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
            shared.host_send_data[2] = 48_000;
            shared.host_send_data[3] = 2;
        }
        decoder.send(Direction::Dsp, Message::InitializeDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeDecodeObjectOK
        );
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[2] = 24_000;
        }
        decoder.send(Direction::Dsp, Message::InitializeDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeDecodeObjectOK
        );
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );

        let header = {
            let shared = shared_memory.lock();
            DecodeObjectHeader::read(&shared, 0x1000).unwrap()
        };
        assert_eq!(header.sample_rate, 24_000);
        assert_eq!(header.channel_count, 2);
        assert_eq!(header.flags, FLAG_INITIALIZED | FLAG_MAPPED);
    }

    #[test]
    fn reinitialize_multi_stream_with_different_layout_rebuilds_header() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);

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
        decoder.send(Direction::Dsp, Message::InitializeMultiStreamDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeMultiStreamDecodeObjectOK
        );
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[3] = 3;
            shared.host_send_data[4] = 2;
            shared.host_send_data[5] = 1;
            shared.channel_mapping[0] = 0;
            shared.channel_mapping[1] = 1;
            shared.channel_mapping[2] = 2;
        }
        decoder.send(Direction::Dsp, Message::InitializeMultiStreamDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeMultiStreamDecodeObjectOK
        );
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );

        let header = {
            let shared = shared_memory.lock();
            DecodeObjectHeader::read(&shared, 0x2000).unwrap()
        };
        assert_eq!(header.sample_rate, 48_000);
        assert_eq!(header.channel_count, 3);
        assert_eq!(header.total_stream_count, 2);
        assert_eq!(header.stereo_stream_count, 1);
        assert_eq!(
            header.flags,
            FLAG_INITIALIZED | FLAG_MULTI_STREAM | FLAG_MAPPED
        );
    }

    #[test]
    fn unmap_memory_clears_mapped_flag_and_map_memory_restores_it() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
            shared.host_send_data[2] = 48_000;
            shared.host_send_data[3] = 2;
        }
        decoder.send(Direction::Dsp, Message::InitializeDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeDecodeObjectOK
        );

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
        }
        decoder.send(Direction::Dsp, Message::UnmapMemory);
        assert_eq!(decoder.receive(Direction::Host), Message::UnmapMemoryOK);
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );
        {
            let shared = shared_memory.lock();
            let header = DecodeObjectHeader::read(&shared, 0x1000).unwrap();
            assert!(!header.is_mapped());
        }

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
        }
        decoder.send(Direction::Dsp, Message::MapMemory);
        assert_eq!(decoder.receive(Direction::Host), Message::MapMemoryOK);
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            ResultCode::SUCCESS.raw() as u64
        );
        {
            let shared = shared_memory.lock();
            let header = DecodeObjectHeader::read(&shared, 0x1000).unwrap();
            assert!(header.is_mapped());
        }
    }

    #[test]
    fn decode_rejects_unmapped_workbuffer_header() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        decoder.set_shared_memory(shared_memory.clone());

        decoder.send(Direction::Dsp, Message::Start);
        assert_eq!(decoder.receive(Direction::Host), Message::StartOK);

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
            shared.host_send_data[2] = 48_000;
            shared.host_send_data[3] = 2;
        }
        decoder.send(Direction::Dsp, Message::InitializeDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::InitializeDecodeObjectOK
        );

        {
            let mut shared = shared_memory.lock();
            let mut header = DecodeObjectHeader::read(&shared, 0x1000).unwrap();
            header.flags &= !FLAG_MAPPED;
            let _ = header.write(&mut shared, 0x1000);
            let packet = packet_with_payload(64);
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
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            RESULT_LIB_OPUS_INVALID_STATE.raw() as u64
        );
    }

    #[test]
    fn successful_shutdown_clears_workbuffer_header() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
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

        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
        }
        decoder.send(Direction::Dsp, Message::ShutdownDecodeObject);
        assert_eq!(
            decoder.receive(Direction::Host),
            Message::ShutdownDecodeObjectOK
        );

        let header = {
            let shared = shared_memory.lock();
            DecodeObjectHeader::read(&shared, 0x1000).unwrap()
        };
        assert_eq!(header.magic, 0);
        assert_eq!(header.flags, 0);
    }

    #[test]
    fn decode_rejects_invalid_workbuffer_header_even_with_live_decoder_entry() {
        let mut decoder = OpusDecoder::new(make_system());
        let shared_memory = Arc::new(Mutex::new(SharedMemory::new(0x20000)));
        {
            let mut shared = shared_memory.lock();
            shared.host_send_data[0] = 0x1000;
            shared.host_send_data[1] = 0x6000;
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

        {
            let mut shared = shared_memory.lock();
            let _ = DecodeObjectHeader::default().write(&mut shared, 0x1000);
            let packet = packet_with_payload(64);
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
        assert_eq!(
            shared_memory.lock().dsp_return_data[0],
            RESULT_LIB_OPUS_INVALID_STATE.raw() as u64
        );
    }
}

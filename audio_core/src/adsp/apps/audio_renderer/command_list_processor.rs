use crate::adsp::apps::audio_renderer::command_buffer::ProcessHandle;
use crate::common::common::CpuAddr;
use crate::renderer::command::commands::{dump_command, process_command, verify_command};
use crate::renderer::command::icommand::{CommandHeader, COMMAND_MAGIC};
use crate::renderer::command::{CommandListHeader, COMMAND_LIST_HEADER_SIZE};
#[cfg(test)]
use crate::renderer::effect::light_limiter::StatisticsInternal;
use crate::sink::sink_stream::SinkStreamHandle;
use crate::SharedSystem;
use log::error;
use ruzu_kernel::memory_manager::MemoryManager;
use ruzu_kernel::KProcess;
use std::fmt::Write;
use std::mem::size_of;

#[derive(Clone, Copy, Default)]
struct MemoryHandle(usize);

impl MemoryHandle {
    fn from_ptr(ptr: *mut MemoryManager) -> Self {
        Self(ptr as usize)
    }

    fn as_ptr(self) -> *mut MemoryManager {
        self.0 as *mut MemoryManager
    }
}

#[derive(Default)]
pub struct CommandListProcessor {
    system: Option<SharedSystem>,
    process: ProcessHandle,
    memory: MemoryHandle,
    stream: Option<SinkStreamHandle>,
    header: CommandListHeader,
    commands: CpuAddr,
    commands_buffer_size: usize,
    command_count: u32,
    sample_count: u32,
    target_sample_rate: u32,
    buffer_count: i16,
    mix_buffers_addr: CpuAddr,
    mix_buffers_len: usize,
    processed_command_count: u32,
    max_process_time: u64,
    start_time: u64,
    current_processing_time: u64,
    end_time: u64,
    last_dump: String,
    dump_audio_commands: bool,
}

impl CommandListProcessor {
    pub fn initialize(
        &mut self,
        system: SharedSystem,
        process: *mut KProcess,
        buffer: CpuAddr,
        size: u64,
        stream: SinkStreamHandle,
    ) -> bool {
        if size < COMMAND_LIST_HEADER_SIZE as u64 || buffer == 0 {
            return false;
        }

        let Some(header) = read_command_list_header(buffer) else {
            return false;
        };
        let Some(buffer_count) = u32::try_from(header.buffer_count).ok() else {
            return false;
        };

        self.system = Some(system);
        self.process = ProcessHandle::from_ptr(process);
        self.memory = if process.is_null() {
            MemoryHandle::default()
        } else {
            MemoryHandle::from_ptr(unsafe { &mut (*process).memory as *mut MemoryManager })
        };
        self.stream = Some(stream);
        self.header = header;
        self.commands = buffer.saturating_add(COMMAND_LIST_HEADER_SIZE);
        self.commands_buffer_size = size as usize;
        self.command_count = header.command_count;
        self.sample_count = header.sample_count;
        self.target_sample_rate = header.sample_rate;
        self.buffer_count = header.buffer_count;
        self.mix_buffers_addr = header.samples_buffer;
        self.mix_buffers_len = header.sample_count.saturating_mul(buffer_count) as usize;
        self.processed_command_count = 0;
        true
    }

    pub fn set_process_time_max(&mut self, time: u64) {
        self.max_process_time = time;
    }

    pub fn get_remaining_command_count(&self) -> u32 {
        self.command_count
            .saturating_sub(self.processed_command_count)
    }

    pub fn get_output_sink_stream(&self) -> Option<SinkStreamHandle> {
        self.stream.clone()
    }

    pub fn get_last_dump(&self) -> &str {
        &self.last_dump
    }

    pub fn set_dump_audio_commands(&mut self, enabled: bool) {
        self.dump_audio_commands = enabled;
    }

    pub fn get_process(&self) -> *mut KProcess {
        self.process.as_ptr()
    }

    pub fn get_memory(&self) -> *mut MemoryManager {
        self.memory.as_ptr()
    }

    pub fn get_mix_buffer_count(&self) -> usize {
        usize::try_from(self.buffer_count).unwrap_or(0)
    }

    pub fn get_mix_buffer_len(&self) -> usize {
        self.mix_buffers_len
    }

    pub(crate) fn get_target_sample_rate(&self) -> u32 {
        self.target_sample_rate
    }

    pub(crate) fn get_buffer_count_raw(&self) -> i16 {
        self.buffer_count
    }

    pub(crate) fn append_command_dump(dump: &mut String, header: &CommandHeader) {
        let _ = writeln!(dump, "{:?} node={}", header.type_, header.node_id);
    }

    fn dump_command(&self, header: &CommandHeader, payload_addr: CpuAddr, dump: &mut String) {
        dump_command(header, payload_addr, self.target_sample_rate, dump);
    }

    pub fn process(&mut self, session_id: u32) -> u64 {
        let Some(system) = self.system.as_ref().cloned() else {
            return 0;
        };

        let start_time = system.lock().core_timing().get_global_time_us().as_micros() as u64;
        let command_base = self.commands;

        if self.processed_command_count > 0 {
            self.current_processing_time = self
                .current_processing_time
                .saturating_add(start_time.saturating_sub(self.end_time));
        } else {
            self.start_time = start_time;
            self.current_processing_time = 0;
        }

        let mut dump = if self.dump_audio_commands {
            Some(format!("\nSession {session_id}\n"))
        } else {
            None
        };

        while self.processed_command_count < self.command_count {
            let Some(header) = read_pod::<CommandHeader>(self.commands) else {
                break;
            };
            if header.magic != COMMAND_MAGIC {
                error!(
                    "Command has invalid magic! Expected 0x{COMMAND_MAGIC:08X}, got {:08X}",
                    header.magic
                );
                return system.lock().core_timing().get_global_time_us().as_micros() as u64
                    - start_time;
            }
            let Some(command_size) = usize::try_from(header.size).ok() else {
                error!("Command has negative size {}", header.size);
                return system.lock().core_timing().get_global_time_us().as_micros() as u64
                    - start_time;
            };
            if command_size < size_of::<CommandHeader>() {
                break;
            }
            let current_offset = self.commands.saturating_sub(command_base) as usize;
            if current_offset.saturating_add(command_size) > self.commands_buffer_size {
                error!(
                    "Command exceeded command buffer, buffer size {:08X}, command ends at {:08X}",
                    self.commands_buffer_size,
                    self.commands.saturating_add(command_size) - COMMAND_LIST_HEADER_SIZE
                );
                return system.lock().core_timing().get_global_time_us().as_micros() as u64
                    - start_time;
            }

            if let Some(dump) = dump.as_mut() {
                self.dump_command(
                    &header,
                    self.commands.saturating_add(size_of::<CommandHeader>()),
                    dump,
                );
            }
            if !self.verify_command(
                &header,
                self.commands.saturating_add(size_of::<CommandHeader>()),
            ) {
                break;
            }

            if header.enabled != 0 {
                self.process_command(
                    &header,
                    self.commands.saturating_add(size_of::<CommandHeader>()),
                );
            } else {
                if let Some(dump) = dump.as_mut() {
                    dump.push_str("\tDisabled!\n");
                }
            }

            self.processed_command_count = self.processed_command_count.saturating_add(1);
            self.commands = self.commands.saturating_add(command_size);
        }

        self.end_time = system.lock().core_timing().get_global_time_us().as_micros() as u64;
        if let Some(dump) = dump {
            if dump != self.last_dump {
                self.last_dump = dump;
            }
        }
        self.end_time.saturating_sub(start_time)
    }

    fn verify_command(&self, header: &CommandHeader, payload_addr: CpuAddr) -> bool {
        verify_command(header, payload_addr)
    }

    fn process_command(&mut self, header: &CommandHeader, payload_addr: CpuAddr) {
        process_command(self, header, payload_addr);
    }

    pub(crate) fn current_process_time_offset(&self) -> u64 {
        self.system
            .as_ref()
            .map(|system| system.lock().core_timing().get_global_time_us().as_micros() as u64)
            .unwrap_or(self.end_time)
            .saturating_sub(self.start_time.saturating_add(self.current_processing_time))
    }

    pub(crate) fn with_mix_buffers<R>(&self, f: impl FnOnce(&[i32], usize, i16) -> R) -> Option<R> {
        let sample_count = self.sample_count as usize;
        let buffer_count = self.buffer_count;
        let mix_buffers = self.mix_buffers()?;
        Some(f(mix_buffers, sample_count, buffer_count))
    }

    pub(crate) fn with_mix_buffers_mut<R>(
        &mut self,
        f: impl FnOnce(&mut [i32], usize, i16) -> R,
    ) -> Option<R> {
        let sample_count = self.sample_count as usize;
        let buffer_count = self.buffer_count;
        let mix_buffers = self.mix_buffers_mut()?;
        Some(f(mix_buffers, sample_count, buffer_count))
    }

    fn mix_buffers(&self) -> Option<&[i32]> {
        if self.mix_buffers_addr == 0 {
            return None;
        }
        Some(unsafe {
            std::slice::from_raw_parts(self.mix_buffers_addr as *const i32, self.mix_buffers_len)
        })
    }

    fn mix_buffers_mut(&mut self) -> Option<&mut [i32]> {
        if self.mix_buffers_addr == 0 {
            return None;
        }
        Some(unsafe {
            std::slice::from_raw_parts_mut(self.mix_buffers_addr as *mut i32, self.mix_buffers_len)
        })
    }
}

fn read_command_list_header(addr: CpuAddr) -> Option<CommandListHeader> {
    if addr == 0 {
        return None;
    }
    let bytes = unsafe { std::slice::from_raw_parts(addr as *const u8, COMMAND_LIST_HEADER_SIZE) };
    Some(CommandListHeader {
        buffer_size: u64::from_le_bytes(bytes[0..8].try_into().ok()?),
        command_count: u32::from_le_bytes(bytes[8..12].try_into().ok()?),
        samples_buffer: u64::from_le_bytes(bytes[12..20].try_into().ok()?) as CpuAddr,
        buffer_count: i16::from_le_bytes(bytes[20..22].try_into().ok()?),
        sample_count: u32::from_le_bytes(bytes[24..28].try_into().ok()?),
        sample_rate: u32::from_le_bytes(bytes[28..32].try_into().ok()?),
    })
}

fn read_pod<T: Copy>(addr: CpuAddr) -> Option<T> {
    if addr == 0 {
        return None;
    }
    Some(unsafe { (addr as *const T).read_unaligned() })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::renderer::behavior::BehaviorInfo;
    use crate::renderer::command::command_buffer::CommandBuffer as RenderCommandBuffer;
    use crate::renderer::command::command_processing_time_estimator::CommandProcessingTimeEstimator;
    use crate::renderer::command::commands::{
        AuxCommand, BiquadFilterCommand, CaptureCommand, CircularBufferSinkCommand,
        ClearMixBufferCommand, Command, CompressorCommand, CopyMixBufferCommand, DataSourceCommand,
        DelayCommand, DepopForMixBuffersCommand, DepopPrepareCommand, DeviceSinkCommand,
        DownMix6chTo2chCommand, I3dl2ReverbCommand, LightLimiterVersion1Command,
        LightLimiterVersion2Command, MixCommand, MixRampCommand, MultiTapBiquadFilterCommand,
        PerformanceCommand, ReverbCommand, UpsampleCommand, VolumeCommand, VolumeRampCommand,
    };
    use crate::renderer::command::effect::compressor::CompressorState;
    use crate::renderer::command::effect::delay::DelayState;
    use crate::renderer::command::effect::i3dl2_reverb::I3dl2ReverbState;
    use crate::renderer::command::effect::light_limiter::LightLimiterState;
    use crate::renderer::command::effect::reverb::ReverbState;
    use crate::renderer::effect::aux_::AuxInfoDsp;
    use crate::renderer::effect::compressor;
    use crate::renderer::effect::delay;
    use crate::renderer::effect::effect_info_base::ParameterState;
    use crate::renderer::effect::i3dl2;
    use crate::renderer::effect::light_limiter::{self, ProcessingMode};
    use crate::renderer::effect::reverb;
    use crate::renderer::performance::{PerformanceEntryAddresses, PerformanceState};
    use crate::renderer::upsampler::UpsamplerInfo;
    use crate::renderer::voice::voice_info::BiquadFilterParameter;
    use crate::renderer::voice::voice_state::BiquadFilterState;
    use crate::renderer::voice::VoiceState;
    use crate::sink::null_sink::NullSink;
    use crate::sink::sink::new_sink_handle;
    use crate::sink::StreamType;
    use common::fixed_point::FixedPoint;
    use parking_lot::Mutex;
    use std::sync::Arc;

    fn make_system() -> SharedSystem {
        Arc::new(Mutex::new(ruzu_core::core::System::new()))
    }

    fn serialize_commands(
        system: SharedSystem,
        commands: &[Command],
        node_id: u32,
        sample_buffer: CpuAddr,
        buffer_count: i16,
        sample_count: u32,
    ) -> (Vec<u8>, SinkStreamHandle) {
        let behavior = BehaviorInfo::new();
        let estimator = CommandProcessingTimeEstimator::new(&behavior, sample_count, 2);
        let mut command_buffer = RenderCommandBuffer::new(0x1000, estimator);
        for command in commands {
            assert!(command_buffer.push(*command, node_id));
        }
        let header = CommandListHeader {
            buffer_size: 0x1000,
            command_count: command_buffer.count(),
            samples_buffer: sample_buffer,
            buffer_count,
            sample_count,
            sample_rate: 48_000,
        };
        let mut bytes = vec![0u8; 0x1000];
        let written = command_buffer.serialize_into(&header, &mut bytes);
        bytes.truncate(written);

        let sink = new_sink_handle(Box::new(NullSink::new("test")));
        let stream = sink
            .lock()
            .acquire_sink_stream(system, 2, "processor", StreamType::Render);
        (bytes, stream)
    }

    #[test]
    fn process_does_not_truncate_command_list_to_max_process_time() {
        let system = make_system();
        let mut samples = vec![123i32; 8];
        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::ClearMixBuffer(ClearMixBufferCommand { buffer_count: 2 }),
                Command::DeviceSink(DeviceSinkCommand {
                    name: [0; 0x100],
                    session_id: 0,
                    sample_buffer: samples.as_ptr() as CpuAddr,
                    sample_count: 4,
                    input_count: 2,
                    inputs: [0, 1, 0, 0, 0, 0],
                }),
            ],
            1,
            samples.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(1);
        let _ = processor.process(0);

        assert_eq!(processor.get_remaining_command_count(), 0);
        assert_eq!(stream.lock().release_buffer(4), vec![0, 0, 0, 0]);
        assert!(samples.iter().all(|&sample| sample == 0));
    }

    #[test]
    fn initialize_caches_process_and_mix_buffer_span() {
        let system = make_system();
        let mut samples = vec![0i32; 8];
        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::ClearMixBuffer(ClearMixBufferCommand {
                buffer_count: 2,
            })],
            1,
            samples.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let process = Box::into_raw(Box::new(ruzu_kernel::KProcess::new(1, "test".to_string())));
        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            process,
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));

        assert_eq!(processor.get_process(), process);
        let expected_memory =
            unsafe { &mut (*process).memory as *mut ruzu_kernel::memory_manager::MemoryManager };
        assert_eq!(processor.get_memory(), expected_memory);
        assert_eq!(processor.get_mix_buffer_count(), 2);
        assert_eq!(processor.get_mix_buffer_len(), 8);

        unsafe {
            drop(Box::from_raw(process));
        }
    }

    #[test]
    fn process_populates_last_dump() {
        let system = make_system();
        let mut samples = vec![0i32; 8];
        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::ClearMixBuffer(ClearMixBufferCommand {
                buffer_count: 2,
            })],
            7,
            samples.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_dump_audio_commands(true);

        let _ = processor.process(0);

        assert!(processor.get_last_dump().contains("Session 0"));
        assert!(processor.get_last_dump().contains("ClearMixBufferCommand"));
    }

    #[test]
    fn process_marks_disabled_commands_in_dump() {
        let system = make_system();
        let mut samples = vec![1i32; 8];
        let (mut bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::ClearMixBuffer(ClearMixBufferCommand {
                buffer_count: 2,
            })],
            9,
            samples.as_mut_ptr() as CpuAddr,
            2,
            4,
        );
        let enabled_offset = COMMAND_LIST_HEADER_SIZE + 4;
        bytes[enabled_offset] = 0;
        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_dump_audio_commands(true);

        let _ = processor.process(1);

        assert!(processor.get_last_dump().contains("Session 1"));
        assert!(processor.get_last_dump().contains("ClearMixBuffer"));
        assert!(processor.get_last_dump().contains("Disabled!"));
    }

    #[test]
    fn process_does_not_store_dump_when_dump_audio_commands_is_disabled() {
        let system = make_system();
        let mut samples = vec![0i32; 8];
        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::ClearMixBuffer(ClearMixBufferCommand {
                buffer_count: 2,
            })],
            11,
            samples.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));

        let _ = processor.process(0);

        assert!(processor.get_last_dump().is_empty());
    }

    #[test]
    fn initialize_rejects_negative_buffer_count() {
        let system = make_system();
        let mut samples = vec![0i32; 8];
        let (mut bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::ClearMixBuffer(ClearMixBufferCommand {
                buffer_count: 2,
            })],
            1,
            samples.as_mut_ptr() as CpuAddr,
            2,
            4,
        );
        bytes[20..22].copy_from_slice(&(-1i16).to_le_bytes());

        let mut processor = CommandListProcessor::default();
        assert!(!processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
    }

    #[test]
    fn process_stops_on_negative_command_size() {
        let system = make_system();
        let mut samples = vec![0i32; 8];
        let (mut bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::ClearMixBuffer(ClearMixBufferCommand {
                buffer_count: 2,
            })],
            1,
            samples.as_mut_ptr() as CpuAddr,
            2,
            4,
        );
        let size_offset = COMMAND_LIST_HEADER_SIZE + 6;
        bytes[size_offset..size_offset + 2].copy_from_slice(&(-1i16).to_le_bytes());

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));

        let _ = processor.process(0);

        assert_eq!(processor.get_remaining_command_count(), 1);
    }

    #[test]
    fn process_downmix_and_device_sink_use_planar_inputs() {
        let system = make_system();
        let mut mix_buffers = vec![
            100, 200, 300, 400, // front left
            10, 20, 30, 40, // front right
            50, 50, 50, 50, // center
            0, 0, 0, 0, // lfe
            25, 25, 25, 25, // back left
            5, 5, 5, 5, // back right
        ];
        let coeff = FixedPoint::<48, 16>::from_f32(1.0).to_raw();
        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::DownMix6chTo2ch(DownMix6chTo2chCommand {
                    inputs: [0, 1, 2, 3, 4, 5],
                    outputs: [0, 1, 2, 3, 4, 5],
                    down_mix_coeff: [coeff; 4],
                }),
                Command::DeviceSink(DeviceSinkCommand {
                    name: [0; 0x100],
                    session_id: 0,
                    sample_buffer: mix_buffers.as_ptr() as CpuAddr,
                    sample_count: 8,
                    input_count: 2,
                    inputs: [0, 1, 0, 0, 0, 0],
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            6,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(
            stream.lock().release_buffer(8),
            vec![175, 65, 275, 75, 375, 85, 475, 95]
        );
    }

    #[test]
    fn process_upsample_and_device_sink_output_constant_frames() {
        let system = make_system();
        let mut mix_buffers = vec![300i32; 160];
        let mut upsampled = vec![0i32; 240];
        let mut upsampler_info = UpsamplerInfo::default();
        upsampler_info.enabled = true;
        upsampler_info.sample_count = 240;
        upsampler_info.input_count = 1;
        upsampler_info.inputs[0] = 0;

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::Upsample(UpsampleCommand {
                    samples_buffer: upsampled.as_mut_ptr() as CpuAddr,
                    inputs: upsampler_info.inputs.as_ptr() as CpuAddr,
                    buffer_count: 1,
                    unk_20: 0,
                    source_sample_count: 160,
                    source_sample_rate: 32_000,
                    upsampler_info: (&mut upsampler_info as *mut UpsamplerInfo) as CpuAddr,
                }),
                Command::DeviceSink(DeviceSinkCommand {
                    name: [0; 0x100],
                    session_id: 0,
                    sample_buffer: upsampled.as_ptr() as CpuAddr,
                    sample_count: 240,
                    input_count: 1,
                    inputs: [0, 0, 0, 0, 0, 0],
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            1,
            160,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        let released = stream.lock().release_buffer(240);
        assert_eq!(released.len(), 240);
        assert!(released.iter().any(|&sample| sample != 0));
        assert_eq!(*released.last().unwrap(), 300);
    }

    #[test]
    fn process_volume_and_mix_commands_modify_mix_buffers_before_device_sink() {
        let system = make_system();
        let mut mix_buffers = vec![
            100, 200, 300, 400, // input
            0, 0, 0, 0, // volume output
            10, 20, 30, 40, // mix output
        ];
        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::Volume(VolumeCommand {
                    precision: 15,
                    input_index: 0,
                    output_index: 1,
                    volume: 0.5,
                }),
                Command::Mix(MixCommand {
                    precision: 15,
                    input_index: 1,
                    output_index: 2,
                    volume: 1.0,
                }),
                Command::DeviceSink(DeviceSinkCommand {
                    name: [0; 0x100],
                    session_id: 0,
                    sample_buffer: mix_buffers.as_ptr() as CpuAddr,
                    sample_count: 12,
                    input_count: 2,
                    inputs: [1, 2, 0, 0, 0, 0],
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            3,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(
            stream.lock().release_buffer(8),
            vec![50, 60, 100, 120, 150, 180, 200, 240]
        );
    }

    #[test]
    fn process_circular_sink_writes_planar_samples_wraps_ring_position_and_persists_it() {
        let system = make_system();
        let mut mix_buffers = vec![
            10, 20, 30, 40, // input 0
            50, 60, 70, 80, // input 1
        ];
        let mut ring = vec![0x55u8; 12];

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::CircularBufferSink(CircularBufferSinkCommand {
                input_count: 1,
                inputs: [1, 0, 0, 0, 0, 0],
                address: ring.as_mut_ptr() as CpuAddr,
                size: ring.len() as u32,
                pos: 8,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system.clone(),
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(
            ring,
            vec![70, 0, 80, 0, 0x55, 0x55, 0x55, 0x55, 50, 0, 60, 0,]
        );

        ring.fill(0x11);
        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(
            ring,
            vec![50, 0, 60, 0, 70, 0, 80, 0, 0x11, 0x11, 0x11, 0x11,]
        );
    }

    #[test]
    fn process_biquad_and_multitap_commands_filter_mix_buffers() {
        let system = make_system();
        let mut mix_buffers = vec![
            10, 20, 30, 40, // input
            0, 0, 0, 0, // biquad output
            0, 0, 0, 0, // multitap output
        ];
        let mut biquad_state = BiquadFilterState::default();
        let mut multitap_states = [BiquadFilterState::default(); 2];
        let identity = BiquadFilterParameter {
            enabled: true,
            _padding: 0,
            b: [1 << 14, 0, 0],
            a: [0, 0],
        };

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::BiquadFilter(BiquadFilterCommand {
                    input: 0,
                    output: 1,
                    biquad: identity,
                    state: (&mut biquad_state as *mut BiquadFilterState) as CpuAddr,
                    needs_init: true,
                    use_float_processing: false,
                }),
                Command::MultiTapBiquadFilter(MultiTapBiquadFilterCommand {
                    input: 0,
                    output: 2,
                    biquads: [identity, identity],
                    states: [
                        (&mut multitap_states[0] as *mut BiquadFilterState) as CpuAddr,
                        (&mut multitap_states[1] as *mut BiquadFilterState) as CpuAddr,
                    ],
                    needs_init: [true, true],
                    filter_tap_count: 2,
                }),
                Command::DeviceSink(DeviceSinkCommand {
                    name: [0; 0x100],
                    session_id: 0,
                    sample_buffer: mix_buffers.as_ptr() as CpuAddr,
                    sample_count: 12,
                    input_count: 2,
                    inputs: [1, 2, 0, 0, 0, 0],
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            3,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(
            stream.lock().release_buffer(8),
            vec![10, 10, 20, 20, 30, 30, 40, 40]
        );
    }

    #[test]
    fn process_pcm_int16_data_source_outputs_samples_and_advances_voice_state() {
        let system = make_system();
        let mut mix_buffers = vec![0i32; 4];
        let pcm = vec![10i16, 20, 30, 40];
        let mut voice_state = VoiceState::default();
        voice_state.wave_buffer_valid[0] = true;

        let mut wave_buffers = [crate::common::wave_buffer::WaveBufferVersion2::default(); 4];
        wave_buffers[0].buffer = pcm.as_ptr() as CpuAddr;
        wave_buffers[0].buffer_size = (pcm.len() * std::mem::size_of::<i16>()) as u64;
        wave_buffers[0].start_offset = 0;
        wave_buffers[0].end_offset = 4;

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::DataSourcePcmInt16Version2(DataSourceCommand {
                    src_quality: crate::common::common::SrcQuality::Medium,
                    output_index: 0,
                    flags: 2,
                    sample_rate: 48_000,
                    pitch: 1.0,
                    channel_index: 0,
                    channel_count: 1,
                    wave_buffers,
                    voice_state: (&mut voice_state as *mut VoiceState) as CpuAddr,
                    data_address: 0,
                    data_size: 0,
                }),
                Command::DeviceSink(DeviceSinkCommand {
                    name: [0; 0x100],
                    session_id: 0,
                    sample_buffer: mix_buffers.as_ptr() as CpuAddr,
                    sample_count: 4,
                    input_count: 1,
                    inputs: [0, 0, 0, 0, 0, 0],
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            1,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(stream.lock().release_buffer(4), vec![10, 20, 30, 40]);
        assert_eq!(voice_state.played_sample_count, 4);
        assert_eq!(voice_state.wave_buffers_consumed, 1);
        assert_eq!(voice_state.offset, 0);
    }

    #[test]
    fn process_pcm_float_data_source_converts_to_s16_mix_samples() {
        let system = make_system();
        let mut mix_buffers = vec![0i32; 4];
        let pcm = vec![0.0f32, 0.5, -0.5, 1.0];
        let mut voice_state = VoiceState::default();
        voice_state.wave_buffer_valid[0] = true;

        let mut wave_buffers = [crate::common::wave_buffer::WaveBufferVersion2::default(); 4];
        wave_buffers[0].buffer = pcm.as_ptr() as CpuAddr;
        wave_buffers[0].buffer_size = (pcm.len() * std::mem::size_of::<f32>()) as u64;
        wave_buffers[0].start_offset = 0;
        wave_buffers[0].end_offset = 4;

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::DataSourcePcmFloatVersion2(DataSourceCommand {
                    src_quality: crate::common::common::SrcQuality::Medium,
                    output_index: 0,
                    flags: 2,
                    sample_rate: 48_000,
                    pitch: 1.0,
                    channel_index: 0,
                    channel_count: 1,
                    wave_buffers,
                    voice_state: (&mut voice_state as *mut VoiceState) as CpuAddr,
                    data_address: 0,
                    data_size: 0,
                }),
                Command::DeviceSink(DeviceSinkCommand {
                    name: [0; 0x100],
                    session_id: 0,
                    sample_buffer: mix_buffers.as_ptr() as CpuAddr,
                    sample_count: 4,
                    input_count: 1,
                    inputs: [0, 0, 0, 0, 0, 0],
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            1,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(
            stream.lock().release_buffer(4),
            vec![0, 16383, -16383, 32767]
        );
        assert_eq!(voice_state.played_sample_count, 4);
        assert_eq!(voice_state.wave_buffers_consumed, 1);
        assert_eq!(voice_state.offset, 0);
    }

    #[test]
    fn process_adpcm_data_source_decodes_basic_frame() {
        let system = make_system();
        let mut mix_buffers = vec![0i32; 4];
        let adpcm = vec![0x00u8, 0x12, 0x34, 0x00, 0x00, 0x00, 0x00, 0x00];
        let coeffs = vec![0i16; 16];
        let mut voice_state = VoiceState::default();
        voice_state.wave_buffer_valid[0] = true;

        let mut wave_buffers = [crate::common::wave_buffer::WaveBufferVersion2::default(); 4];
        wave_buffers[0].buffer = adpcm.as_ptr() as CpuAddr;
        wave_buffers[0].buffer_size = adpcm.len() as u64;
        wave_buffers[0].start_offset = 0;
        wave_buffers[0].end_offset = 4;

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::DataSourceAdpcmVersion2(DataSourceCommand {
                    src_quality: crate::common::common::SrcQuality::Medium,
                    output_index: 0,
                    flags: 2,
                    sample_rate: 48_000,
                    pitch: 1.0,
                    channel_index: 0,
                    channel_count: 1,
                    wave_buffers,
                    voice_state: (&mut voice_state as *mut VoiceState) as CpuAddr,
                    data_address: coeffs.as_ptr() as CpuAddr,
                    data_size: (coeffs.len() * std::mem::size_of::<i16>()) as u64,
                }),
                Command::DeviceSink(DeviceSinkCommand {
                    name: [0; 0x100],
                    session_id: 0,
                    sample_buffer: mix_buffers.as_ptr() as CpuAddr,
                    sample_count: 4,
                    input_count: 1,
                    inputs: [0, 0, 0, 0, 0, 0],
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            1,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(stream.lock().release_buffer(4), vec![1, 2, 3, 4]);
        assert_eq!(voice_state.played_sample_count, 4);
        assert_eq!(voice_state.wave_buffers_consumed, 1);
        assert_eq!(voice_state.offset, 0);
    }

    #[test]
    fn process_ramp_copy_and_depop_commands_update_buffers() {
        let system = make_system();
        let mut mix_buffers = vec![
            100, 100, 100, 100, // input
            0, 0, 0, 0, // ramp output
            0, 0, 0, 0, // mix/depop output
        ];
        let mut previous_sample = 0i32;
        let mut previous_samples = vec![5, -3];
        let mut depop_buffer = vec![0i32; 3];
        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::VolumeRamp(VolumeRampCommand {
                    precision: 15,
                    input_index: 0,
                    output_index: 1,
                    prev_volume: 0.0,
                    volume: 1.0,
                }),
                Command::MixRamp(MixRampCommand {
                    precision: 15,
                    input_index: 1,
                    output_index: 2,
                    prev_volume: 0.0,
                    volume: 1.0,
                    previous_sample: (&mut previous_sample as *mut i32) as CpuAddr,
                }),
                Command::CopyMixBuffer(CopyMixBufferCommand {
                    input_index: 2,
                    output_index: 0,
                }),
                Command::DepopPrepare(DepopPrepareCommand {
                    inputs: [
                        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    ],
                    previous_samples: previous_samples.as_mut_ptr() as CpuAddr,
                    buffer_count: 2,
                    depop_buffer: depop_buffer.as_mut_ptr() as CpuAddr,
                }),
                Command::DepopForMixBuffers(DepopForMixBuffersCommand {
                    input: 0,
                    count: 2,
                    decay: FixedPoint::<49, 15>::from_f32(1.0).to_raw(),
                    depop_buffer: depop_buffer.as_mut_ptr() as CpuAddr,
                }),
                Command::DeviceSink(DeviceSinkCommand {
                    name: [0; 0x100],
                    session_id: 0,
                    sample_buffer: mix_buffers.as_ptr() as CpuAddr,
                    sample_count: 8,
                    input_count: 2,
                    inputs: [0, 1, 0, 0, 0, 0],
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            3,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream.clone(),
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(previous_sample, 56);
        assert_eq!(previous_samples, vec![0, 0]);
        assert_eq!(depop_buffer[0], 5);
        assert_eq!(depop_buffer[1], -3);
        assert_eq!(
            stream.lock().release_buffer(8),
            vec![5, -3, 11, 22, 30, 47, 61, 72]
        );
    }

    #[test]
    fn process_aux_enabled_moves_send_and_return_buffers() {
        let system = make_system();
        let mut mix_buffers = vec![
            10, 20, 30, 40, // input
            0, 0, 0, 0, // output
        ];
        let mut send_info = AuxInfoDsp::default();
        let mut return_info = AuxInfoDsp::default();
        let mut send_buffer = vec![0i32; 8];
        let return_buffer = vec![101i32, 102, 103, 104, 0, 0, 0, 0];

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::Aux(AuxCommand {
                input: 0,
                output: 1,
                send_buffer_info: (&mut send_info as *mut AuxInfoDsp) as CpuAddr,
                return_buffer_info: (&mut return_info as *mut AuxInfoDsp) as CpuAddr,
                send_buffer: send_buffer.as_mut_ptr() as CpuAddr,
                return_buffer: return_buffer.as_ptr() as CpuAddr,
                count_max: 8,
                write_offset: 0,
                update_count: 4,
                effect_enabled: true,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(&send_buffer[..4], &[10, 20, 30, 40]);
        assert_eq!(&mix_buffers[4..8], &[101, 102, 103, 104]);
        assert_eq!(send_info.write_offset, 4);
        assert_eq!(send_info.total_sample_count, 4);
        assert_eq!(return_info.read_offset, 4);
    }

    #[test]
    fn process_aux_disabled_resets_info_and_copies_input_to_output() {
        let system = make_system();
        let mut mix_buffers = vec![
            11, 22, 33, 44, // input
            0, 0, 0, 0, // output
        ];
        let mut send_info = AuxInfoDsp {
            read_offset: 2,
            write_offset: 3,
            lost_sample_count: 7,
            total_sample_count: 9,
            ..Default::default()
        };
        let mut return_info = send_info;
        let mut send_buffer = vec![0i32; 8];
        let return_buffer = vec![0i32; 8];

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::Aux(AuxCommand {
                input: 0,
                output: 1,
                send_buffer_info: (&mut send_info as *mut AuxInfoDsp) as CpuAddr,
                return_buffer_info: (&mut return_info as *mut AuxInfoDsp) as CpuAddr,
                send_buffer: send_buffer.as_mut_ptr() as CpuAddr,
                return_buffer: return_buffer.as_ptr() as CpuAddr,
                count_max: 8,
                write_offset: 0,
                update_count: 4,
                effect_enabled: false,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(&mix_buffers[4..8], &[11, 22, 33, 44]);
        assert_eq!(send_info.read_offset, 0);
        assert_eq!(send_info.write_offset, 0);
        assert_eq!(send_info.total_sample_count, 0);
        assert_eq!(return_info.read_offset, 0);
        assert_eq!(return_info.write_offset, 0);
        assert_eq!(return_info.total_sample_count, 0);
    }

    #[test]
    fn process_capture_enabled_and_disabled_update_send_buffer_state() {
        let system = make_system();
        let mut mix_buffers = vec![
            9, 8, 7, 6, // input
            0, 0, 0, 0, // unused
        ];
        let mut send_info = AuxInfoDsp::default();
        let mut send_buffer = vec![0i32; 8];

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::Capture(CaptureCommand {
                    input: 0,
                    output: 1,
                    send_buffer_info: (&mut send_info as *mut AuxInfoDsp) as CpuAddr,
                    send_buffer: send_buffer.as_mut_ptr() as CpuAddr,
                    count_max: 8,
                    write_offset: 0,
                    update_count: 4,
                    effect_enabled: true,
                }),
                Command::Capture(CaptureCommand {
                    input: 0,
                    output: 1,
                    send_buffer_info: (&mut send_info as *mut AuxInfoDsp) as CpuAddr,
                    send_buffer: send_buffer.as_mut_ptr() as CpuAddr,
                    count_max: 8,
                    write_offset: 0,
                    update_count: 4,
                    effect_enabled: false,
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(&send_buffer[..4], &[9, 8, 7, 6]);
        assert_eq!(send_info.read_offset, 0);
        assert_eq!(send_info.write_offset, 0);
        assert_eq!(send_info.total_sample_count, 0);
    }

    #[test]
    fn process_performance_start_and_stop_write_entry_timestamps() {
        let system = make_system();
        let mut mix_buffers = vec![0i32; 4];
        let mut performance_frame = vec![0u8; 64];
        system.lock().core_timing_mut().add_ticks(10_000);

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::Performance(PerformanceCommand {
                    state: PerformanceState::Start,
                    entry_addresses: PerformanceEntryAddresses {
                        translated_address: performance_frame.as_mut_ptr() as usize,
                        entry_start_time_offset: 4,
                        header_entry_count_offset: 0,
                        entry_processed_time_offset: 8,
                    },
                }),
                Command::Performance(PerformanceCommand {
                    state: PerformanceState::Stop,
                    entry_addresses: PerformanceEntryAddresses {
                        translated_address: performance_frame.as_mut_ptr() as usize,
                        entry_start_time_offset: 4,
                        header_entry_count_offset: 0,
                        entry_processed_time_offset: 8,
                    },
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            1,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(
            u32::from_le_bytes(performance_frame[0..4].try_into().unwrap()),
            1
        );
        let start = u32::from_le_bytes(performance_frame[4..8].try_into().unwrap());
        let processed = u32::from_le_bytes(performance_frame[8..12].try_into().unwrap());
        assert_eq!(start, 0);
        assert_eq!(processed, 0);
    }

    #[test]
    fn process_compressor_initializes_state_and_writes_scaled_output() {
        let system = make_system();
        let mut mix_buffers = vec![
            1000, -1000, 2000, -2000, // input
            0, 0, 0, 0, // output
        ];
        let mut state = CompressorState::default();
        let parameter = compressor::ParameterVersion2 {
            inputs: [0, 0, 0, 0, 0, 0],
            outputs: [1, 1, 1, 1, 1, 1],
            channel_count_max: 1,
            channel_count: 1,
            sample_rate: 48_000,
            threshold: -3.0,
            compressor_ratio: 2.0,
            attack_time: 0,
            release_time: 0,
            unk_24: 0.0,
            unk_28: 0.1,
            unk_2c: 0.2,
            out_gain: 6.0,
            state: ParameterState::Initialized,
            makeup_gain_enabled: false,
        };

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::Compressor(CompressorCommand {
                inputs: [0, 0, 0, 0, 0, 0],
                outputs: [1, 1, 1, 1, 1, 1],
                parameter,
                state: (&mut state as *mut CompressorState) as CpuAddr,
                workbuffer: 0,
                enabled: true,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_ne!(mix_buffers[4], mix_buffers[0]);
        assert_ne!(mix_buffers[6], mix_buffers[2]);
        assert!(mix_buffers[5] < 0);
        assert!(mix_buffers[7] < 0);
        assert!(i32::abs(mix_buffers[6]) < mix_buffers[2] * 2);
        assert!(state.unk_04.is_finite());
        assert!(state.unk_08.is_finite());
        assert!(state.unk_20 > 1.0);
    }

    #[test]
    fn process_light_limiter_v2_updates_statistics_and_writes_output() {
        let system = make_system();
        let mut mix_buffers = vec![
            1000, -2000, 3000, -4000, // input
            0, 0, 0, 0, // output
        ];
        let mut state = LightLimiterState::default();
        let mut statistics = StatisticsInternal::default();
        let mut workbuffer = vec![0.0f32; 4];
        let parameter = light_limiter::ParameterVersion2 {
            inputs: [0, 0, 0, 0, 0, 0],
            outputs: [1, 1, 1, 1, 1, 1],
            channel_count_max: 1,
            channel_count: 1,
            sample_rate: 48_000,
            look_ahead_time_max: 0,
            attack_time: 0,
            release_time: 0,
            look_ahead_time: 0,
            attack_coeff: 1.0,
            release_coeff: 1.0,
            threshold: 0.25,
            input_gain: 1.0,
            output_gain: 1.0,
            look_ahead_samples_min: 1,
            look_ahead_samples_max: 1,
            state: ParameterState::Initialized,
            statistics_enabled: true,
            statistics_reset_required: true,
            processing_mode: ProcessingMode::Mode0,
        };

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::LightLimiterVersion2(LightLimiterVersion2Command {
                inputs: [0, 0, 0, 0, 0, 0],
                outputs: [1, 1, 1, 1, 1, 1],
                parameter,
                state: (&mut state as *mut LightLimiterState) as CpuAddr,
                workbuffer: workbuffer.as_mut_ptr() as CpuAddr,
                result_state: (&mut statistics as *mut StatisticsInternal) as CpuAddr,
                effect_enabled: true,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(mix_buffers[4], 0);
        assert_ne!(&mix_buffers[4..8], &[0, 0, 0, 0]);
        assert!(statistics.channel_max_sample[0] >= 4000.0);
        assert!(statistics.channel_compression_gain_min[0].is_finite());
        assert!(statistics.channel_compression_gain_min[0] <= 1.0);
        assert!(state.samples_average[0] > 0.0);
        assert!(state.compression_gain[0].is_finite());
    }

    #[test]
    fn process_light_limiter_v1_disabled_copies_input_to_output() {
        let system = make_system();
        let mut mix_buffers = vec![
            100, -200, 300, -400, // input
            0, 0, 0, 0, // output
        ];
        let mut state = LightLimiterState::default();
        let mut workbuffer = vec![0.0f32; 4];
        let parameter = light_limiter::ParameterVersion1 {
            inputs: [0, 0, 0, 0, 0, 0],
            outputs: [1, 1, 1, 1, 1, 1],
            channel_count_max: 1,
            channel_count: 1,
            sample_rate: 48_000,
            look_ahead_time_max: 0,
            attack_time: 0,
            release_time: 0,
            look_ahead_time: 0,
            attack_coeff: 1.0,
            release_coeff: 1.0,
            threshold: 0.25,
            input_gain: 1.0,
            output_gain: 1.0,
            look_ahead_samples_min: 1,
            look_ahead_samples_max: 1,
            state: ParameterState::Initialized,
            statistics_enabled: false,
            statistics_reset_required: false,
            processing_mode: ProcessingMode::Mode0,
        };

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::LightLimiterVersion1(LightLimiterVersion1Command {
                inputs: [0, 0, 0, 0, 0, 0],
                outputs: [1, 1, 1, 1, 1, 1],
                parameter,
                state: (&mut state as *mut LightLimiterState) as CpuAddr,
                workbuffer: workbuffer.as_mut_ptr() as CpuAddr,
                effect_enabled: false,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(&mix_buffers[4..8], &mix_buffers[0..4]);
    }

    #[test]
    fn process_delay_enabled_outputs_one_sample_delayed_signal() {
        let system = make_system();
        let mut mix_buffers = vec![
            100, 200, 300, 400, // input
            0, 0, 0, 0, // output
        ];
        let mut state = DelayState::default();
        let mut workbuffer = vec![0.0f32; 4];
        let parameter = delay::ParameterVersion2 {
            inputs: [0, 0, 0, 0, 0, 0],
            outputs: [1, 1, 1, 1, 1, 1],
            channel_count_max: 1,
            channel_count: 1,
            delay_time_max: 1,
            delay_time: 0,
            sample_rate: 48_000 << 14,
            in_gain: 1 << 14,
            feedback_gain: 0,
            wet_gain: 1 << 14,
            dry_gain: 0,
            channel_spread: 0,
            lowpass_amount: 0,
            state: ParameterState::Initialized,
        };

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::Delay(DelayCommand {
                inputs: [0, 0, 0, 0, 0, 0],
                outputs: [1, 1, 1, 1, 1, 1],
                parameter,
                state: (&mut state as *mut DelayState) as CpuAddr,
                workbuffer: workbuffer.as_mut_ptr() as CpuAddr,
                effect_enabled: true,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(&mix_buffers[4..8], &[0, 100, 200, 300]);
        assert_eq!(state.sample_count[0], 1);
        assert_eq!(state.buffer_pos[0], 0);
    }

    #[test]
    fn process_delay_disabled_copies_input_to_output() {
        let system = make_system();
        let mut mix_buffers = vec![
            10, -20, 30, -40, // input
            0, 0, 0, 0, // output
        ];
        let mut state = DelayState::default();
        let mut workbuffer = vec![0.0f32; 4];
        let parameter = delay::ParameterVersion2 {
            inputs: [0, 0, 0, 0, 0, 0],
            outputs: [1, 1, 1, 1, 1, 1],
            channel_count_max: 1,
            channel_count: 1,
            delay_time_max: 1,
            delay_time: 0,
            sample_rate: 48_000 << 14,
            in_gain: 1 << 14,
            feedback_gain: 0,
            wet_gain: 1 << 14,
            dry_gain: 0,
            channel_spread: 0,
            lowpass_amount: 0,
            state: ParameterState::Initialized,
        };

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::Delay(DelayCommand {
                inputs: [0, 0, 0, 0, 0, 0],
                outputs: [1, 1, 1, 1, 1, 1],
                parameter,
                state: (&mut state as *mut DelayState) as CpuAddr,
                workbuffer: workbuffer.as_mut_ptr() as CpuAddr,
                effect_enabled: false,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(&mix_buffers[4..8], &mix_buffers[0..4]);
    }

    #[test]
    fn process_reverb_disabled_copies_input_to_output() {
        let system = make_system();
        let mut mix_buffers = vec![
            10, -20, 30, -40, // input
            0, 0, 0, 0, // output
        ];
        let mut state = ReverbState::default();
        let mut workbuffer = vec![0.0f32; 40_000];
        let parameter = reverb::ParameterVersion2 {
            inputs: [0, 0, 0, 0, 0, 0],
            outputs: [1, 1, 1, 1, 1, 1],
            channel_count_max: 1,
            channel_count: 1,
            sample_rate: 48_000,
            early_mode: 0,
            early_gain: 1 << 14,
            pre_delay: 0,
            late_mode: 0,
            late_gain: 1 << 14,
            decay_time: 1 << 14,
            high_freq_decay_ratio: 1 << 14,
            colouration: 0,
            base_gain: 1 << 14,
            wet_gain: 1 << 14,
            dry_gain: 1 << 14,
            state: ParameterState::Initialized,
        };

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::Reverb(ReverbCommand {
                inputs: [0, 0, 0, 0, 0, 0],
                outputs: [1, 1, 1, 1, 1, 1],
                parameter,
                state: (&mut state as *mut ReverbState) as CpuAddr,
                workbuffer: workbuffer.as_mut_ptr() as CpuAddr,
                effect_enabled: false,
                long_size_pre_delay_supported: false,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(&mix_buffers[4..8], &mix_buffers[0..4]);
    }

    #[test]
    fn process_reverb_enabled_initializes_state_and_writes_wet_output() {
        let system = make_system();
        let mut mix_buffers = vec![
            1000, 1000, 1000, 1000, // input L
            0, 0, 0, 0, // output L
            0, 0, 0, 0, // input R
            0, 0, 0, 0, // output R
        ];
        let mut state = ReverbState::default();
        let mut workbuffer = vec![0.0f32; 40_000];
        let parameter = reverb::ParameterVersion2 {
            inputs: [0, 2, 0, 0, 0, 0],
            outputs: [1, 3, 0, 0, 0, 0],
            channel_count_max: 2,
            channel_count: 2,
            sample_rate: 48_000,
            early_mode: 0,
            early_gain: 1 << 14,
            pre_delay: 0,
            late_mode: 2,
            late_gain: 1 << 14,
            decay_time: 1 << 14,
            high_freq_decay_ratio: (0.5 * 16384.0) as i32,
            colouration: 0,
            base_gain: 1 << 14,
            wet_gain: 1 << 14,
            dry_gain: 0,
            state: ParameterState::Initialized,
        };

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::Reverb(ReverbCommand {
                inputs: [0, 2, 0, 0, 0, 0],
                outputs: [1, 3, 0, 0, 0, 0],
                parameter,
                state: (&mut state as *mut ReverbState) as CpuAddr,
                workbuffer: workbuffer.as_mut_ptr() as CpuAddr,
                effect_enabled: true,
                long_size_pre_delay_supported: false,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            4,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert!(state.pre_delay_len > 0);
        assert!(state.fdn_delay_samples.iter().all(|&v| v > 0));
        assert_ne!(&mix_buffers[4..8], &[0, 0, 0, 0]);
    }

    #[test]
    fn process_i3dl2_reverb_disabled_copies_input_to_output() {
        let system = make_system();
        let mut mix_buffers = vec![
            12, -24, 36, -48, // input
            0, 0, 0, 0, // output
        ];
        let mut state = I3dl2ReverbState::default();
        let mut workbuffer = vec![0.0f32; 70_000];
        let parameter = i3dl2::ParameterVersion2 {
            inputs: [0, 0, 0, 0, 0, 0],
            outputs: [1, 1, 1, 1, 1, 1],
            channel_count_max: 1,
            channel_count: 1,
            unk10: [0; 4],
            sample_rate: 48_000,
            room_hf_gain: 0.0,
            reference_hf: 5_000.0,
            late_reverb_decay_time: 1.0,
            late_reverb_hf_decay_ratio: 0.5,
            room_gain: 0.0,
            reflection_gain: 0.0,
            reverb_gain: 0.0,
            late_reverb_diffusion: 0.5,
            reflection_delay: 0.01,
            late_reverb_delay_time: 0.02,
            late_reverb_density: 50.0,
            dry_gain: 1.0,
            state: ParameterState::Initialized,
            unk49: [0; 3],
        };

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[Command::I3dl2Reverb(I3dl2ReverbCommand {
                inputs: [0, 0, 0, 0, 0, 0],
                outputs: [1, 1, 1, 1, 1, 1],
                parameter,
                state: (&mut state as *mut I3dl2ReverbState) as CpuAddr,
                workbuffer: workbuffer.as_mut_ptr() as CpuAddr,
                effect_enabled: false,
            })],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            2,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert_eq!(&mix_buffers[4..8], &mix_buffers[0..4]);
    }

    #[test]
    fn process_i3dl2_reverb_enabled_initializes_state_and_writes_output() {
        let system = make_system();
        let mut mix_buffers = vec![
            800, 800, 800, 800, // input L
            0, 0, 0, 0, // output L
            0, 0, 0, 0, // input R
            0, 0, 0, 0, // output R
        ];
        let mut state = I3dl2ReverbState::default();
        let mut workbuffer = vec![0.0f32; 70_000];
        let parameter = i3dl2::ParameterVersion2 {
            inputs: [0, 2, 0, 0, 0, 0],
            outputs: [1, 3, 0, 0, 0, 0],
            channel_count_max: 2,
            channel_count: 2,
            unk10: [0; 4],
            sample_rate: 48_000,
            room_hf_gain: -1000.0,
            reference_hf: 5_000.0,
            late_reverb_decay_time: 1.5,
            late_reverb_hf_decay_ratio: 0.7,
            room_gain: 0.0,
            reflection_gain: 0.0,
            reverb_gain: 0.0,
            late_reverb_diffusion: 0.5,
            reflection_delay: 0.01,
            late_reverb_delay_time: 0.02,
            late_reverb_density: 50.0,
            dry_gain: 0.0,
            state: ParameterState::Initialized,
            unk49: [0; 3],
        };
        let mut parameter_updated = parameter;
        parameter_updated.state = ParameterState::Updated;

        let (bytes, stream) = serialize_commands(
            system.clone(),
            &[
                Command::I3dl2Reverb(I3dl2ReverbCommand {
                    inputs: [0, 2, 0, 0, 0, 0],
                    outputs: [1, 3, 0, 0, 0, 0],
                    parameter,
                    state: (&mut state as *mut I3dl2ReverbState) as CpuAddr,
                    workbuffer: workbuffer.as_mut_ptr() as CpuAddr,
                    effect_enabled: true,
                }),
                Command::I3dl2Reverb(I3dl2ReverbCommand {
                    inputs: [0, 2, 0, 0, 0, 0],
                    outputs: [1, 3, 0, 0, 0, 0],
                    parameter: parameter_updated,
                    state: (&mut state as *mut I3dl2ReverbState) as CpuAddr,
                    workbuffer: workbuffer.as_mut_ptr() as CpuAddr,
                    effect_enabled: true,
                }),
            ],
            1,
            mix_buffers.as_mut_ptr() as CpuAddr,
            4,
            4,
        );

        let mut processor = CommandListProcessor::default();
        assert!(processor.initialize(
            system,
            std::ptr::null_mut(),
            bytes.as_ptr() as CpuAddr,
            bytes.len() as u64,
            stream,
        ));
        processor.set_process_time_max(u64::MAX);
        let _ = processor.process(0);

        assert!(state.early_delay_len > 0);
        assert!(state.fdn_delay_samples.iter().all(|&v| v > 0));
        assert!(state.early_delay_pos > 0);
        assert!(state.fdn_positions.iter().any(|&v| v > 0));
    }
}

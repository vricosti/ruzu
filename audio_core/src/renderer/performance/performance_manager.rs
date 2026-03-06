use super::performance_detail::PerformanceDetailType;
use super::performance_entry::PerformanceEntryType;
use super::performance_entry_addresses::PerformanceEntryAddresses;
use crate::common::audio_renderer_parameter::AudioRendererParameterInternal;
use crate::common::common::{make_magic, CpuAddr};
use crate::renderer::behavior::BehaviorInfo;
use crate::renderer::memory::{MemoryPoolInfo, PoolLocation, PoolMapper};
use crate::Result;
use common::ResultCode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PerformanceVersion {
    Version1,
    Version2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PerformanceSysDetailType {
    PcmInt16 = 15,
    PcmFloat = 16,
    Adpcm = 17,
    LightLimiter = 37,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PerformanceState {
    Invalid,
    Start,
    Stop,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct InParameter {
    pub target_node_id: i32,
    pub unk04: [u8; 0xC],
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct OutStatus {
    pub history_size: i32,
    pub unk04: [u8; 0xC],
}

pub struct PerformanceManager {
    initialized: bool,
    frame_size: u64,
    version: PerformanceVersion,
    target_node_id: u32,
    current_frame: Vec<u8>,
    current_frame_pool: MemoryPoolInfo,
    history: Vec<Vec<u8>>,
    output_frame_index: u32,
    last_output_frame_index: u32,
    history_frame_index: u32,
    entries_per_frame: u32,
    entry_count: u32,
    detail_count: u32,
}

impl PerformanceManager {
    pub const MAX_DETAIL_ENTRIES: usize = 100;

    pub fn new() -> Self {
        Self {
            initialized: false,
            frame_size: 0,
            version: PerformanceVersion::Version1,
            target_node_id: 0,
            current_frame: Vec::new(),
            current_frame_pool: MemoryPoolInfo::new(PoolLocation::Dsp),
            history: Vec::new(),
            output_frame_index: 0,
            last_output_frame_index: 0,
            history_frame_index: 0,
            entries_per_frame: 0,
            entry_count: 0,
            detail_count: 0,
        }
    }

    pub fn get_required_buffer_size_for_performance_metrics_per_frame(
        behavior: &BehaviorInfo,
        params: &AudioRendererParameterInternal,
    ) -> u64 {
        let entry_count =
            (params.voices + params.effects + params.sub_mixes + params.sinks + 1) as u64;
        match behavior.get_performance_metrics_data_format() {
            2 => 0x30 + entry_count * 0x18 + Self::MAX_DETAIL_ENTRIES as u64 * 0x18,
            _ => 0x18 + entry_count * 0x10 + Self::MAX_DETAIL_ENTRIES as u64 * 0x10,
        }
    }

    pub fn initialize(
        &mut self,
        _workbuffer: &mut [u8],
        workbuffer_size: u64,
        params: &AudioRendererParameterInternal,
        behavior: &BehaviorInfo,
    ) {
        self.frame_size =
            Self::get_required_buffer_size_for_performance_metrics_per_frame(behavior, params);
        self.version = if behavior.get_performance_metrics_data_format() == 2 {
            PerformanceVersion::Version2
        } else {
            PerformanceVersion::Version1
        };
        self.entries_per_frame =
            params.voices + params.effects + params.sinks + params.sub_mixes + 1;

        let frame_count = if self.frame_size == 0 {
            0
        } else {
            workbuffer_size / self.frame_size
        };
        let history_frames = frame_count.saturating_sub(1) as usize;
        self.current_frame = vec![0; self.frame_size as usize];
        self.current_frame_pool = MemoryPoolInfo::new(PoolLocation::Dsp);
        let pool_mapper = PoolMapper::new(None, false);
        let _ = pool_mapper.initialize_system_pool(
            &mut self.current_frame_pool,
            &self.current_frame,
            self.current_frame.len() as u64,
        );
        self.history = vec![vec![0; self.frame_size as usize]; history_frames];
        self.output_frame_index = 0;
        self.last_output_frame_index = 0;
        self.history_frame_index = 0;
        self.entry_count = 0;
        self.detail_count = 0;
        self.initialized = true;
        self.reset_current_frame();
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    pub fn copy_histories(&mut self, out_buffer: &mut [u8]) -> u32 {
        if !self.initialized || self.last_output_frame_index == self.output_frame_index {
            return 0;
        }

        let mut written = 0usize;
        let mut wrote_any = false;
        while self.last_output_frame_index != self.output_frame_index {
            let Some(frame) = self.history.get(self.last_output_frame_index as usize) else {
                break;
            };

            let history_entry_count = self.read_u32_from(frame, 0x04) as usize;
            let history_detail_count = self.read_u32_from(frame, 0x08) as usize;
            let min_required = self.header_size()
                + history_entry_count * self.entry_size()
                + history_detail_count * self.detail_size()
                + self.header_size();
            if written + min_required > out_buffer.len() {
                break;
            }

            let header_size = self.header_size();
            let entry_size = self.entry_size();
            let detail_size = self.detail_size();
            let detail_base = header_size + self.entries_per_frame as usize * entry_size;

            let mut out_offset = written + header_size;
            let mut out_entry_count = 0u32;
            let mut out_detail_count = 0u32;
            let mut total_processing_time = 0u32;

            for entry_index in 0..history_entry_count {
                let src_offset = header_size + entry_index * entry_size;
                let start_time = self.read_u32_from(frame, src_offset + 0x04);
                let processed_time = self.read_u32_from(frame, src_offset + 0x08);
                if start_time == 0 && processed_time == 0 {
                    continue;
                }

                let src = &frame[src_offset..src_offset + entry_size];
                out_buffer[out_offset..out_offset + entry_size].copy_from_slice(src);
                out_offset += entry_size;
                out_entry_count = out_entry_count.saturating_add(1);
                total_processing_time = total_processing_time.saturating_add(processed_time);
            }

            for detail_index in 0..history_detail_count {
                let src_offset = detail_base + detail_index * detail_size;
                let start_time = self.read_u32_from(frame, src_offset + 0x04);
                let processed_time = self.read_u32_from(frame, src_offset + 0x08);
                if start_time == 0 && processed_time == 0 {
                    continue;
                }

                let src = &frame[src_offset..src_offset + detail_size];
                out_buffer[out_offset..out_offset + detail_size].copy_from_slice(src);
                out_offset += detail_size;
                out_detail_count = out_detail_count.saturating_add(1);
            }

            out_buffer[written..written + header_size].fill(0);
            self.write_u32_to(out_buffer, written + 0x00, make_magic('P', 'E', 'R', 'F'));
            self.write_u32_to(out_buffer, written + 0x04, out_entry_count);
            self.write_u32_to(out_buffer, written + 0x08, out_detail_count);
            self.write_u32_to(out_buffer, written + 0x0C, (out_offset - written) as u32);
            self.write_u32_to(out_buffer, written + 0x10, total_processing_time);
            if matches!(self.version, PerformanceVersion::Version2) {
                self.write_u32_to(out_buffer, written + 0x14, self.read_u32_from(frame, 0x14));
                self.write_u64_to(out_buffer, written + 0x18, self.read_u64_from(frame, 0x18));
                self.write_u32_to(out_buffer, written + 0x20, self.read_u32_from(frame, 0x20));
                if out_buffer.len() > written + 0x24 && frame.len() > 0x24 {
                    out_buffer[written + 0x24] = frame[0x24];
                }
            } else {
                self.write_u32_to(out_buffer, written + 0x14, self.read_u32_from(frame, 0x14));
            }

            written = out_offset;
            wrote_any = true;
            self.last_output_frame_index =
                (self.last_output_frame_index + 1) % self.history.len().max(1) as u32;
            if self.history.is_empty() {
                break;
            }
        }

        if wrote_any && written + self.header_size() <= out_buffer.len() {
            out_buffer[written..written + self.header_size()].fill(0);
        }

        written as u32
    }

    pub fn get_next_entry(
        &mut self,
        addresses: &mut PerformanceEntryAddresses,
        entry_type: PerformanceEntryType,
        node_id: i32,
    ) -> bool {
        if !self.initialized || self.entry_count >= self.entries_per_frame {
            return false;
        }

        let entry_size = self.entry_size();
        let entry_offset = self.header_size() + self.entry_count as usize * entry_size;
        let start_offset = entry_offset + 0x04;
        let processed_offset = entry_offset + 0x08;
        self.entry_count = self.entry_count.saturating_add(1);

        self.current_frame[entry_offset..entry_offset + entry_size].fill(0);
        self.write_u32(entry_offset, node_id as u32);
        self.current_frame[entry_offset + 0x0C] = entry_type as u8;
        self.write_header_count(false, self.entry_count);

        *addresses = PerformanceEntryAddresses {
            translated_address: self.current_frame_pool.translate(
                self.current_frame.as_ptr() as CpuAddr,
                self.current_frame.len() as u64,
            ),
            entry_start_time_offset: start_offset,
            header_entry_count_offset: self.header_entry_count_offset(false),
            entry_processed_time_offset: processed_offset,
        };
        true
    }

    pub fn get_next_detail(
        &mut self,
        addresses: &mut PerformanceEntryAddresses,
        detail_type: PerformanceDetailType,
        entry_type: PerformanceEntryType,
        node_id: i32,
    ) -> bool {
        if !self.initialized || self.detail_count >= Self::MAX_DETAIL_ENTRIES as u32 {
            return false;
        }

        let detail_size = self.detail_size();
        let detail_offset = self.header_size()
            + self.entries_per_frame as usize * self.entry_size()
            + self.detail_count as usize * detail_size;
        let start_offset = detail_offset + 0x04;
        let processed_offset = detail_offset + 0x08;
        self.detail_count = self.detail_count.saturating_add(1);

        self.current_frame[detail_offset..detail_offset + detail_size].fill(0);
        self.write_u32(detail_offset, node_id as u32);
        self.current_frame[detail_offset + 0x0C] = detail_type as u8;
        if matches!(
            self.version,
            PerformanceVersion::Version1 | PerformanceVersion::Version2
        ) {
            self.current_frame[detail_offset + 0x0D] = entry_type as u8;
        }
        self.write_header_count(true, self.detail_count);

        *addresses = PerformanceEntryAddresses {
            translated_address: self.current_frame_pool.translate(
                self.current_frame.as_ptr() as CpuAddr,
                self.current_frame.len() as u64,
            ),
            entry_start_time_offset: start_offset,
            header_entry_count_offset: self.header_entry_count_offset(true),
            entry_processed_time_offset: processed_offset,
        };
        true
    }

    pub fn tap_frame(&mut self, dsp_behind: bool, voices_dropped: u32, rendering_start_tick: u64) {
        if !self.initialized {
            return;
        }

        let total_processing_time = self
            .entry_count
            .saturating_add(self.detail_count)
            .saturating_mul(100);
        self.write_u32(0x10, total_processing_time);

        if matches!(self.version, PerformanceVersion::Version2) {
            self.write_u32(0x14, voices_dropped);
            self.write_u64(0x18, rendering_start_tick);
            self.write_u32(0x20, self.history_frame_index);
            if self.current_frame.len() > 0x24 {
                self.current_frame[0x24] = u8::from(dsp_behind);
            }
        } else {
            self.write_u32(0x14, self.history_frame_index);
        }

        if !self.history.is_empty() {
            self.history[self.output_frame_index as usize].copy_from_slice(&self.current_frame);
            self.output_frame_index = (self.output_frame_index + 1) % self.history.len() as u32;
        }

        self.history_frame_index = self.history_frame_index.saturating_add(1);
        self.entry_count = 0;
        self.detail_count = 0;
        self.reset_current_frame();
    }

    pub fn is_detail_target(&self, target_node_id: u32) -> bool {
        self.target_node_id == target_node_id
    }

    pub fn set_detail_target(&mut self, target_node_id: u32) {
        self.target_node_id = target_node_id;
    }

    pub fn version(&self) -> PerformanceVersion {
        self.version
    }

    pub fn noop_result(&self) -> Result {
        ResultCode::SUCCESS
    }

    fn header_size(&self) -> usize {
        match self.version {
            PerformanceVersion::Version1 => 0x18,
            PerformanceVersion::Version2 => 0x30,
        }
    }

    fn entry_size(&self) -> usize {
        match self.version {
            PerformanceVersion::Version1 => 0x10,
            PerformanceVersion::Version2 => 0x18,
        }
    }

    fn detail_size(&self) -> usize {
        match self.version {
            PerformanceVersion::Version1 => 0x10,
            PerformanceVersion::Version2 => 0x18,
        }
    }

    fn header_entry_count_offset(&self, detail: bool) -> usize {
        if detail {
            0x08
        } else {
            0x04
        }
    }

    fn write_header_count(&mut self, detail: bool, value: u32) {
        self.write_u32(self.header_entry_count_offset(detail), value);
    }

    fn reset_current_frame(&mut self) {
        if self.current_frame.is_empty() {
            return;
        }

        self.current_frame.fill(0);
        self.write_u32(0x00, make_magic('P', 'E', 'R', 'F'));
        self.write_u32(0x04, 0);
        self.write_u32(0x08, 0);
        self.write_u32(0x0C, self.frame_size as u32);
        self.write_u32(0x10, 0);
        if matches!(self.version, PerformanceVersion::Version2) {
            self.write_u32(0x14, 0);
            self.write_u64(0x18, 0);
            self.write_u32(0x20, 0);
            if self.current_frame.len() > 0x24 {
                self.current_frame[0x24] = 0;
            }
        } else {
            self.write_u32(0x14, 0);
        }
    }

    fn write_u32(&mut self, offset: usize, value: u32) {
        let Some(target) = self.current_frame.get_mut(offset..offset + 4) else {
            return;
        };
        target.copy_from_slice(&value.to_le_bytes());
    }

    fn write_u64(&mut self, offset: usize, value: u64) {
        let Some(target) = self.current_frame.get_mut(offset..offset + 8) else {
            return;
        };
        target.copy_from_slice(&value.to_le_bytes());
    }

    fn write_u32_to(&self, buffer: &mut [u8], offset: usize, value: u32) {
        let Some(target) = buffer.get_mut(offset..offset + 4) else {
            return;
        };
        target.copy_from_slice(&value.to_le_bytes());
    }

    fn write_u64_to(&self, buffer: &mut [u8], offset: usize, value: u64) {
        let Some(target) = buffer.get_mut(offset..offset + 8) else {
            return;
        };
        target.copy_from_slice(&value.to_le_bytes());
    }

    fn read_u32_from(&self, buffer: &[u8], offset: usize) -> u32 {
        let Some(source) = buffer.get(offset..offset + 4) else {
            return 0;
        };
        let mut bytes = [0u8; 4];
        bytes.copy_from_slice(source);
        u32::from_le_bytes(bytes)
    }

    fn read_u64_from(&self, buffer: &[u8], offset: usize) -> u64 {
        let Some(source) = buffer.get(offset..offset + 8) else {
            return 0;
        };
        let mut bytes = [0u8; 8];
        bytes.copy_from_slice(source);
        u64::from_le_bytes(bytes)
    }
}

impl Default for PerformanceManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::audio_renderer_parameter::{AudioRendererParameterInternal, ExecutionMode};
    use crate::common::feature_support::CURRENT_REVISION;

    fn make_params() -> AudioRendererParameterInternal {
        AudioRendererParameterInternal {
            sample_rate: 48_000,
            sample_count: 160,
            mixes: 1,
            sub_mixes: 0,
            voices: 1,
            sinks: 0,
            effects: 0,
            perf_frames: 1,
            voice_drop_enabled: 0,
            unk_21: 0,
            rendering_device: 0,
            execution_mode: ExecutionMode::Manual,
            splitter_infos: 0,
            splitter_destinations: 0,
            external_context_size: 0,
            revision: CURRENT_REVISION,
        }
    }

    #[test]
    fn get_next_entry_preserves_negative_node_id_bits() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);
        let params = make_params();
        let frame_size =
            PerformanceManager::get_required_buffer_size_for_performance_metrics_per_frame(
                &behavior, &params,
            );
        let mut workbuffer = vec![0u8; frame_size as usize * 2 + 0xC0];
        let workbuffer_len = workbuffer.len() as u64;
        let mut manager = PerformanceManager::new();
        let mut addresses = PerformanceEntryAddresses::default();

        manager.initialize(&mut workbuffer, workbuffer_len, &params, &behavior);

        assert!(manager.get_next_entry(&mut addresses, PerformanceEntryType::Voice, -1));
        assert_eq!(
            manager.read_u32_from(&manager.current_frame, manager.header_size()),
            u32::MAX
        );
    }

    #[test]
    fn get_next_detail_preserves_negative_node_id_bits() {
        let mut behavior = BehaviorInfo::new();
        behavior.set_user_lib_revision(CURRENT_REVISION);
        let params = make_params();
        let frame_size =
            PerformanceManager::get_required_buffer_size_for_performance_metrics_per_frame(
                &behavior, &params,
            );
        let mut workbuffer = vec![0u8; frame_size as usize * 2 + 0xC0];
        let workbuffer_len = workbuffer.len() as u64;
        let mut manager = PerformanceManager::new();
        let mut addresses = PerformanceEntryAddresses::default();

        manager.initialize(&mut workbuffer, workbuffer_len, &params, &behavior);

        assert!(manager.get_next_detail(
            &mut addresses,
            PerformanceDetailType::Unk1,
            PerformanceEntryType::Voice,
            -1,
        ));
        let detail_offset =
            manager.header_size() + manager.entries_per_frame as usize * manager.entry_size();
        assert_eq!(
            manager.read_u32_from(&manager.current_frame, detail_offset),
            u32::MAX
        );
    }
}

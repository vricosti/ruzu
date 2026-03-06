use crate::common::common::{CpuAddr, MAX_CHANNELS, TARGET_SAMPLE_COUNT};
use crate::renderer::memory::{MemoryPoolInfo, PoolLocation, PoolMapper};

use super::UpsamplerInfo;

#[derive(Debug, Clone, Default)]
pub struct UpsamplerManager {
    count: u32,
    upsampler_infos: Vec<UpsamplerInfo>,
    workbuffer: Vec<i32>,
    infos_pool: MemoryPoolInfo,
    workbuffer_pool: MemoryPoolInfo,
}

impl UpsamplerManager {
    pub fn new(count: u32) -> Self {
        let mut manager = Self {
            count,
            upsampler_infos: vec![UpsamplerInfo::default(); count as usize],
            workbuffer: vec![0; count as usize * TARGET_SAMPLE_COUNT as usize * MAX_CHANNELS],
            infos_pool: MemoryPoolInfo::new(PoolLocation::Dsp),
            workbuffer_pool: MemoryPoolInfo::new(PoolLocation::Dsp),
        };
        let pool_mapper = PoolMapper::new(None, false);
        let infos_bytes = typed_slice_as_bytes(&manager.upsampler_infos);
        let _ = pool_mapper.initialize_system_pool(
            &mut manager.infos_pool,
            infos_bytes,
            infos_bytes.len() as u64,
        );
        let workbuffer_bytes = typed_slice_as_bytes(&manager.workbuffer);
        let _ = pool_mapper.initialize_system_pool(
            &mut manager.workbuffer_pool,
            workbuffer_bytes,
            workbuffer_bytes.len() as u64,
        );
        manager
    }

    pub fn allocate(&mut self) -> Option<usize> {
        if self.count == 0 {
            return None;
        }

        let manager_ptr = self.manager_address();
        let free_index = self
            .upsampler_infos
            .iter()
            .position(|upsampler| !upsampler.enabled)?;
        let offset = free_index * TARGET_SAMPLE_COUNT as usize * MAX_CHANNELS;
        let upsampler = &mut self.upsampler_infos[free_index];
        upsampler.manager_ptr = manager_ptr;
        upsampler.sample_count = TARGET_SAMPLE_COUNT;
        upsampler.samples_pos = self.workbuffer[offset..].as_ptr() as CpuAddr;
        upsampler.input_count = 0;
        upsampler.inputs.fill(0);
        upsampler.enabled = true;
        Some(free_index)
    }

    pub fn free(&mut self, index: usize) {
        if let Some(info) = self.upsampler_infos.get_mut(index) {
            info.enabled = false;
            info.input_count = 0;
            info.inputs.fill(0);
        }
    }

    pub fn get(&self, index: usize) -> Option<&UpsamplerInfo> {
        self.upsampler_infos.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut UpsamplerInfo> {
        self.upsampler_infos.get_mut(index)
    }

    pub fn count(&self) -> u32 {
        self.count
    }

    pub fn translated_info_address(&self, index: usize) -> CpuAddr {
        let Some(info) = self.upsampler_infos.get(index) else {
            return 0;
        };
        self.infos_pool.translate(
            info as *const _ as CpuAddr,
            std::mem::size_of::<UpsamplerInfo>() as u64,
        )
    }

    pub fn translated_inputs_address(&self, index: usize) -> CpuAddr {
        let Some(info) = self.upsampler_infos.get(index) else {
            return 0;
        };
        self.infos_pool.translate(
            info.inputs.as_ptr() as CpuAddr,
            std::mem::size_of_val(&info.inputs) as u64,
        )
    }

    pub fn translated_command_samples_address(&self, index: usize) -> CpuAddr {
        let Some(info) = self.upsampler_infos.get(index) else {
            return 0;
        };
        self.workbuffer_pool.translate(
            info.samples_pos,
            info.sample_count as u64 * std::mem::size_of::<i32>() as u64,
        )
    }

    pub fn translated_device_samples_address(&self, index: usize, input_count: u32) -> CpuAddr {
        let Some(info) = self.upsampler_infos.get(index) else {
            return 0;
        };
        self.workbuffer_pool.translate(
            info.samples_pos,
            info.sample_count as u64 * input_count as u64 * std::mem::size_of::<i32>() as u64,
        )
    }

    fn manager_address(&self) -> CpuAddr {
        self as *const Self as CpuAddr
    }
}

fn typed_slice_as_bytes<T>(slice: &[T]) -> &[u8] {
    unsafe { std::slice::from_raw_parts(slice.as_ptr() as *const u8, std::mem::size_of_val(slice)) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocate_populates_manager_ptr_and_sample_storage() {
        let mut manager = UpsamplerManager::new(2);
        let manager_ptr = &manager as *const UpsamplerManager as CpuAddr;

        let index = manager
            .allocate()
            .expect("upsampler allocation should succeed");
        let info = manager
            .get(index)
            .expect("allocated upsampler should exist");

        assert_eq!(info.manager_ptr, manager_ptr);
        assert_eq!(info.sample_count, TARGET_SAMPLE_COUNT);
        assert_ne!(info.samples_pos, 0);
        assert!(info.enabled);
    }

    #[test]
    fn free_allows_reuse_of_same_slot() {
        let mut manager = UpsamplerManager::new(1);

        let first = manager.allocate().expect("first allocation should succeed");
        manager.free(first);
        let second = manager
            .allocate()
            .expect("freed upsampler slot should be reusable");

        assert_eq!(first, second);
    }

    #[test]
    fn translated_sample_addresses_follow_command_and_device_sizes() {
        let mut manager = UpsamplerManager::new(1);
        let index = manager.allocate().expect("allocation should succeed");

        let command_addr = manager.translated_command_samples_address(index);
        let device_addr = manager.translated_device_samples_address(index, 2);

        assert_ne!(command_addr, 0);
        assert_eq!(command_addr, device_addr);
    }

    #[test]
    fn oversized_device_sample_span_is_rejected() {
        let mut manager = UpsamplerManager::new(1);
        let index = manager.allocate().expect("allocation should succeed");

        let oversized = manager.translated_device_samples_address(index, (MAX_CHANNELS + 1) as u32);

        assert_eq!(oversized, 0);
    }
}

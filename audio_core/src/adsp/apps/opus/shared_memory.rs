use parking_lot::Mutex;
use std::sync::Arc;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct SharedMemory {
    pub channel_mapping: [u8; 0x100],
    pub host_send_data: [u64; 16],
    pub dsp_return_data: [u64; 16],
    transfer_memory: Vec<u8>,
}

impl SharedMemory {
    pub fn new(transfer_memory_size: usize) -> Self {
        Self {
            channel_mapping: [0; 0x100],
            host_send_data: [0; 16],
            dsp_return_data: [0; 16],
            transfer_memory: vec![0; transfer_memory_size],
        }
    }

    pub fn transfer_memory(&self) -> &[u8] {
        &self.transfer_memory
    }

    pub fn transfer_memory_mut(&mut self) -> &mut [u8] {
        &mut self.transfer_memory
    }

    pub fn resize_transfer_memory(&mut self, size: usize) {
        if self.transfer_memory.len() < size {
            self.transfer_memory.resize(size, 0);
        }
    }

    pub fn read_transfer(&self, address: usize, size: usize) -> Option<&[u8]> {
        self.transfer_memory
            .get(address..address.saturating_add(size))
    }

    pub fn write_transfer(&mut self, address: usize, data: &[u8]) -> bool {
        let Some(buffer) = self
            .transfer_memory
            .get_mut(address..address.saturating_add(data.len()))
        else {
            return false;
        };
        buffer.copy_from_slice(data);
        true
    }
}

pub type SharedMemoryHandle = Arc<Mutex<SharedMemory>>;

#[cfg(test)]
mod tests {
    use super::SharedMemory;

    #[test]
    fn shared_memory_matches_upstream_control_array_sizes() {
        let shared = SharedMemory::new(0);
        assert_eq!(shared.channel_mapping.len(), 0x100);
        assert_eq!(shared.host_send_data.len(), 16);
        assert_eq!(shared.dsp_return_data.len(), 16);
    }
}

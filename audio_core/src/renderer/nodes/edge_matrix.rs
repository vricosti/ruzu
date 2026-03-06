use crate::renderer::nodes::BitArray;
use common::alignment::align_up;

#[derive(Debug, Clone, Default)]
pub struct EdgeMatrix {
    edges: BitArray,
    count: u32,
}

impl EdgeMatrix {
    pub fn get_work_buffer_size(count: u32) -> u64 {
        align_up((count * count) as u64, 0x40) / std::mem::size_of::<u64>() as u64
    }

    pub fn initialize(&mut self, _buffer: &[u8], _node_buffer_size: u64, count: u32) {
        self.count = count;
        self.edges.buffer.resize((count * count) as usize, false);
        self.edges.size = count * count;
        self.edges.reset();
    }

    pub fn connected(&self, id: u32, destination_id: u32) -> bool {
        self.edges.buffer[(self.count * id + destination_id) as usize]
    }

    pub fn connect(&mut self, id: u32, destination_id: u32) {
        self.edges.buffer[(self.count * id + destination_id) as usize] = true;
    }

    pub fn disconnect(&mut self, id: u32, destination_id: u32) {
        self.edges.buffer[(self.count * id + destination_id) as usize] = false;
    }

    pub fn remove_edges(&mut self, id: u32) {
        for dest in 0..self.count {
            self.disconnect(id, dest);
        }
    }

    pub fn get_node_count(&self) -> u32 {
        self.count
    }
}

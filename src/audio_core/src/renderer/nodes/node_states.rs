use crate::renderer::nodes::{BitArray, EdgeMatrix};
use common::alignment::align_up;
use log::error;

#[derive(Debug, Clone, Default)]
pub struct NodeStates {
    node_count: u32,
    result_pos: u32,
    nodes_found: BitArray,
    nodes_complete: BitArray,
    results: Vec<u32>,
    stack: Stack,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SearchState {
    Unknown,
    Found,
    Complete,
}

#[derive(Debug, Clone, Default)]
struct Stack {
    stack: Vec<u32>,
    size: u32,
    pos: u32,
    unk_10: u32,
}

impl Stack {
    fn calc_buffer_size(count: u32) -> u32 {
        count * std::mem::size_of::<u32>() as u32
    }

    fn reset(&mut self, size: u32) {
        self.stack.clear();
        self.stack.reserve(size as usize);
        self.size = size;
        self.pos = 0;
        self.unk_10 = size;
    }

    fn count(&self) -> u32 {
        self.pos
    }

    fn push(&mut self, data: u32) {
        self.stack.push(data);
        self.pos += 1;
    }

    fn pop(&mut self) -> u32 {
        self.pos -= 1;
        self.stack.pop().unwrap_or_default()
    }

    fn top(&self) -> u32 {
        *self.stack.last().unwrap_or(&0)
    }
}

impl NodeStates {
    pub fn get_work_buffer_size(count: u32) -> u64 {
        (align_up(count as u64, 0x40) / std::mem::size_of::<u64>() as u64) * 2
            + count as u64 * std::mem::size_of::<BitArray>() as u64
            + count as u64 * Stack::calc_buffer_size(count) as u64
    }

    pub fn initialize(&mut self, _buffer: &[u8], _node_buffer_size: u64, count: u32) {
        self.node_count = count;
        self.nodes_found.buffer.resize(count as usize, false);
        self.nodes_found.size = count;
        self.nodes_found.reset();
        self.nodes_complete.buffer.resize(count as usize, false);
        self.nodes_complete.size = count;
        self.nodes_complete.reset();
        self.results = vec![u32::MAX; count as usize];
        self.stack.reset(count * count);
    }

    pub fn tsort(&mut self, edge_matrix: &EdgeMatrix) -> bool {
        let mut stack = std::mem::take(&mut self.stack);
        let result = self.depth_first_search(edge_matrix, &mut stack);
        self.stack = stack;
        result
    }

    fn depth_first_search(&mut self, edge_matrix: &EdgeMatrix, stack: &mut Stack) -> bool {
        self.reset_state();

        for node_id in 0..self.node_count {
            if self.get_state(node_id) == SearchState::Unknown {
                stack.push(node_id);
            }

            while stack.count() > 0 {
                let current_node = stack.top();
                match self.get_state(current_node) {
                    SearchState::Unknown => self.set_state(current_node, SearchState::Found),
                    SearchState::Found => {
                        self.set_state(current_node, SearchState::Complete);
                        self.push_tsort_result(current_node);
                        stack.pop();
                        continue;
                    }
                    SearchState::Complete => {
                        stack.pop();
                        continue;
                    }
                }

                let edge_count = edge_matrix.get_node_count();
                for edge_id in 0..edge_count {
                    if !edge_matrix.connected(current_node, edge_id) {
                        continue;
                    }
                    match self.get_state(edge_id) {
                        SearchState::Unknown => stack.push(edge_id),
                        SearchState::Found => {
                            error!("audio_core: cycle detected in node graph");
                            self.reset_state();
                            return false;
                        }
                        SearchState::Complete => {}
                    }
                }
            }
        }

        true
    }

    fn get_state(&self, id: u32) -> SearchState {
        if self.nodes_found.buffer[id as usize] {
            SearchState::Found
        } else if self.nodes_complete.buffer[id as usize] {
            SearchState::Complete
        } else {
            SearchState::Unknown
        }
    }

    fn push_tsort_result(&mut self, id: u32) {
        self.results[self.result_pos as usize] = id;
        self.result_pos += 1;
    }

    fn set_state(&mut self, id: u32, state: SearchState) {
        match state {
            SearchState::Complete => {
                self.nodes_found.buffer[id as usize] = false;
                self.nodes_complete.buffer[id as usize] = true;
            }
            SearchState::Found => {
                self.nodes_found.buffer[id as usize] = true;
                self.nodes_complete.buffer[id as usize] = false;
            }
            SearchState::Unknown => {
                self.nodes_found.buffer[id as usize] = false;
                self.nodes_complete.buffer[id as usize] = false;
            }
        }
    }

    fn reset_state(&mut self) {
        self.nodes_found.reset();
        self.nodes_complete.reset();
        self.results.fill(u32::MAX);
        self.result_pos = 0;
    }

    pub fn get_node_count(&self) -> u32 {
        self.node_count
    }

    pub fn get_sorted_results(&self) -> Vec<u32> {
        let mut out: Vec<u32> = self.results[..self.result_pos as usize].to_vec();
        out.reverse();
        out
    }
}

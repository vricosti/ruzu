use crate::common::common::get_splitter_info_magic;

use super::SplitterDestinationData;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct InParameter {
    pub magic: u32,
    pub id: i32,
    pub sample_rate: u32,
    pub destination_count: u32,
}

#[derive(Debug, Clone)]
pub struct SplitterInfo {
    id: i32,
    sample_rate: u32,
    destination_count: u32,
    has_new_connection: bool,
    destinations_head: Option<usize>,
    channel_count: u32,
}

impl SplitterInfo {
    pub fn new(id: i32) -> Self {
        Self {
            id,
            sample_rate: 0,
            destination_count: 0,
            has_new_connection: true,
            destinations_head: None,
            channel_count: 0,
        }
    }

    pub fn update(&mut self, params: &InParameter) -> u32 {
        if params.id != self.id || params.magic != get_splitter_info_magic() {
            return 0;
        }
        self.sample_rate = params.sample_rate;
        self.has_new_connection = true;
        self.channel_count = params.destination_count;
        ((std::mem::size_of::<InParameter>() + 3 * std::mem::size_of::<i32>()) as u32)
            .saturating_add(params.destination_count * std::mem::size_of::<i32>() as u32)
    }

    pub fn get_data<'a>(
        &self,
        destination_id: u32,
        destinations: &'a [SplitterDestinationData],
    ) -> Option<&'a SplitterDestinationData> {
        let mut out_destination = self.destinations_head;
        let mut i = 0;
        while i < destination_id {
            out_destination = out_destination.and_then(|index| destinations.get(index)?.next());
            if out_destination.is_none() {
                break;
            }
            i += 1;
        }
        out_destination.and_then(|index| destinations.get(index))
    }

    pub fn get_destination_count(&self) -> u32 {
        self.destination_count
    }

    pub fn set_destination_count(&mut self, count: u32) {
        self.destination_count = count;
    }

    pub fn has_new_connection(&self) -> bool {
        self.has_new_connection
    }

    pub fn clear_new_connection_flag(&mut self) {
        self.has_new_connection = false;
    }

    pub fn set_new_connection_flag(&mut self) {
        self.has_new_connection = true;
    }

    pub fn update_internal_state(&self, destinations: &mut [SplitterDestinationData]) {
        let mut destination = self.destinations_head;
        while let Some(index) = destination {
            let next = destinations[index].next();
            destinations[index].update_internal_state();
            destination = next;
        }
    }

    pub fn set_destinations(&mut self, head: Option<usize>) {
        self.destinations_head = head;
    }

    pub fn destinations_head(&self) -> Option<usize> {
        self.destinations_head
    }
}

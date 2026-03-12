// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/irs_ring_lifo.h

/// IRS-specific LIFO buffer (different layout from HID Lifo)
#[repr(C)]
pub struct IrsLifo<State: Copy + Default, const MAX_BUFFER_SIZE: usize> {
    pub sampling_number: i64,
    pub buffer_count: i64,
    pub entries: [State; MAX_BUFFER_SIZE],
}

impl<State: Copy + Default, const MAX_BUFFER_SIZE: usize> Default
    for IrsLifo<State, MAX_BUFFER_SIZE>
{
    fn default() -> Self {
        Self {
            sampling_number: 0,
            buffer_count: 0,
            entries: [State::default(); MAX_BUFFER_SIZE],
        }
    }
}

impl<State: Copy + Default, const MAX_BUFFER_SIZE: usize> IrsLifo<State, MAX_BUFFER_SIZE> {
    pub fn get_buffer_tail(&self) -> usize {
        (self.sampling_number as usize) % MAX_BUFFER_SIZE
    }

    pub fn read_current_entry(&self) -> &State {
        &self.entries[self.get_buffer_tail()]
    }

    pub fn get_previous_entry_index(&self) -> usize {
        (self.get_buffer_tail() + MAX_BUFFER_SIZE - 1) % MAX_BUFFER_SIZE
    }

    pub fn read_previous_entry(&self) -> &State {
        &self.entries[self.get_previous_entry_index()]
    }

    pub fn get_next_entry_index(&self) -> usize {
        (self.get_buffer_tail() + 1) % MAX_BUFFER_SIZE
    }

    pub fn write_next_entry(&mut self, new_state: State) {
        if self.buffer_count < MAX_BUFFER_SIZE as i64 {
            self.buffer_count += 1;
        }
        self.sampling_number += 1;
        let tail = self.get_buffer_tail();
        self.entries[tail] = new_state;
    }
}

// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of hid_core/resources/ring_lifo.h

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct AtomicStorage<State: Copy + Default> {
    pub sampling_number: i64,
    pub state: State,
}

/// Rust counterpart of the `State::sampling_number` member required by the
/// upstream `Lifo::WriteNextEntry` template.
pub trait LifoState: Copy + Default {
    fn sampling_number(&self) -> i64;
}

#[repr(C)]
pub struct Lifo<State: Copy + Default, const MAX_BUFFER_SIZE: usize> {
    pub timestamp: i64,
    pub total_buffer_count: i64,
    pub buffer_tail: i64,
    pub buffer_count: i64,
    pub entries: [AtomicStorage<State>; MAX_BUFFER_SIZE],
}

impl<State: Copy + Default, const MAX_BUFFER_SIZE: usize> Default for Lifo<State, MAX_BUFFER_SIZE> {
    fn default() -> Self {
        Self {
            timestamp: 0,
            total_buffer_count: MAX_BUFFER_SIZE as i64,
            buffer_tail: 0,
            buffer_count: 0,
            entries: [AtomicStorage::default(); MAX_BUFFER_SIZE],
        }
    }
}

impl<State: Copy + Default + std::fmt::Debug, const MAX_BUFFER_SIZE: usize> std::fmt::Debug
    for Lifo<State, MAX_BUFFER_SIZE>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Lifo")
            .field("timestamp", &self.timestamp)
            .field("total_buffer_count", &self.total_buffer_count)
            .field("buffer_tail", &self.buffer_tail)
            .field("buffer_count", &self.buffer_count)
            .finish()
    }
}

impl<State: Copy + Default, const MAX_BUFFER_SIZE: usize> Clone for Lifo<State, MAX_BUFFER_SIZE> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<State: Copy + Default, const MAX_BUFFER_SIZE: usize> Copy for Lifo<State, MAX_BUFFER_SIZE> {}

impl<State: Copy + Default, const MAX_BUFFER_SIZE: usize> Lifo<State, MAX_BUFFER_SIZE> {
    /// Read the entry at `buffer_tail`. Upstream uses `entries[buffer_tail]`
    /// directly; if `buffer_tail` is corrupt this is UB in C++ but silently
    /// reads adjacent memory. Rust would otherwise panic, so we mask by
    /// MAX_BUFFER_SIZE to keep the game running with the closest legal entry.
    /// Set `RUZU_TRACE_LIFO_CORRUPTION=1` to log occurrences (one-shot per
    /// process) for diagnosis.
    pub fn read_current_entry(&self) -> &AtomicStorage<State> {
        let raw_tail = self.buffer_tail as usize;
        let tail = raw_tail % MAX_BUFFER_SIZE;
        if raw_tail >= MAX_BUFFER_SIZE && std::env::var_os("RUZU_TRACE_LIFO_CORRUPTION").is_some() {
            use std::sync::atomic::{AtomicBool, Ordering};
            static REPORTED: AtomicBool = AtomicBool::new(false);
            if !REPORTED.swap(true, Ordering::Relaxed) {
                log::error!(
                    "[LIFO_CORRUPTION] read_current_entry: buffer_tail={} (raw i64={:#x}) max={} self_addr={:p} masked_tail={}",
                    raw_tail,
                    self.buffer_tail,
                    MAX_BUFFER_SIZE,
                    self,
                    tail,
                );
            }
        }
        &self.entries[tail]
    }

    pub fn read_previous_entry(&self) -> &AtomicStorage<State> {
        &self.entries[self.get_previous_entry_index()]
    }

    pub fn get_previous_entry_index(&self) -> usize {
        let tail = (self.buffer_tail as usize) % MAX_BUFFER_SIZE;
        (tail + MAX_BUFFER_SIZE - 1) % MAX_BUFFER_SIZE
    }

    pub fn get_next_entry_index(&self) -> usize {
        let tail = (self.buffer_tail as usize) % MAX_BUFFER_SIZE;
        (tail + 1) % MAX_BUFFER_SIZE
    }

    pub fn write_next_entry(&mut self, new_state: State)
    where
        State: LifoState,
    {
        if self.buffer_count < (MAX_BUFFER_SIZE as i64) - 1 {
            self.buffer_count += 1;
        }
        self.buffer_tail = self.get_next_entry_index() as i64;
        let tail = self.buffer_tail as usize;
        self.entries[tail].sampling_number = new_state.sampling_number() << 1;
        self.entries[tail].state = new_state;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
    #[repr(C)]
    struct TestState {
        sampling_number: i64,
        value: u64,
    }

    impl LifoState for TestState {
        fn sampling_number(&self) -> i64 {
            self.sampling_number
        }
    }

    #[test]
    fn write_next_entry_publishes_an_even_atomic_sampling_number() {
        let mut lifo = Lifo::<TestState, 17>::default();
        let state = TestState {
            sampling_number: 7,
            value: 0x1234,
        };

        lifo.write_next_entry(state);

        let entry = lifo.read_current_entry();
        assert_eq!(entry.sampling_number, 14);
        assert_eq!(entry.sampling_number & 1, 0);
        assert_eq!(entry.state, state);
    }

    #[test]
    fn write_next_entry_uses_state_sampling_instead_of_previous_marker() {
        let mut lifo = Lifo::<TestState, 17>::default();
        lifo.entries[0].sampling_number = 0x1235;

        lifo.write_next_entry(TestState {
            sampling_number: 3,
            value: 1,
        });

        assert_eq!(lifo.read_current_entry().sampling_number, 6);
    }
}

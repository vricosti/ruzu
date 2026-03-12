// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/delayed_destruction_ring.h
//!
//! Container to push objects to be destroyed a few ticks in the future.

/// A ring buffer that defers destruction of objects by `TICKS_TO_DESTROY` ticks.
///
/// Each call to [`tick`] advances to the next slot, clearing any objects in that slot.
/// Objects pushed via [`push`] are placed in the current slot and will be dropped
/// when that slot is cleared after a full cycle.
pub struct DelayedDestructionRing<T, const TICKS_TO_DESTROY: usize> {
    index: usize,
    elements: Vec<Vec<T>>,
}

impl<T, const TICKS_TO_DESTROY: usize> DelayedDestructionRing<T, TICKS_TO_DESTROY> {
    /// Creates a new ring with `TICKS_TO_DESTROY` slots.
    pub fn new() -> Self {
        let mut elements = Vec::with_capacity(TICKS_TO_DESTROY);
        for _ in 0..TICKS_TO_DESTROY {
            elements.push(Vec::new());
        }
        Self { index: 0, elements }
    }

    /// Advances to the next tick, dropping all objects in the new slot.
    pub fn tick(&mut self) {
        self.index = (self.index + 1) % TICKS_TO_DESTROY;
        self.elements[self.index].clear();
    }

    /// Pushes an object into the current slot for deferred destruction.
    pub fn push(&mut self, object: T) {
        self.elements[self.index].push(object);
    }
}

impl<T, const TICKS_TO_DESTROY: usize> Default for DelayedDestructionRing<T, TICKS_TO_DESTROY> {
    fn default() -> Self {
        Self::new()
    }
}

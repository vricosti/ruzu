// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Simplified buffer queue for framebuffer presentation.
//!
//! Implements a 4-slot queue matching the Android BufferQueue protocol used by
//! the Switch's vi service. Games dequeue a slot, write pixel data, then queue
//! it for presentation. The main loop acquires the latest queued buffer.

/// Number of buffer slots.
pub const NUM_SLOTS: usize = 4;

/// State of a buffer slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotState {
    Free,
    Dequeued,
    Queued,
    Acquired,
}

/// Description of a graphic buffer stored in a slot.
#[derive(Debug, Clone)]
pub struct GraphicBuffer {
    pub width: u32,
    pub height: u32,
    /// Pixel format: 1 = RGBA8888.
    pub format: u32,
    /// NvMap handle that owns the backing memory.
    pub nvmap_handle: u32,
    /// Guest memory offset/address of the pixel data.
    pub offset: u64,
    /// Stride in pixels.
    pub stride: u32,
}

/// Simplified buffer queue (4 slots, no fences).
pub struct BufferQueue {
    slots: [SlotState; NUM_SLOTS],
    buffers: [Option<GraphicBuffer>; NUM_SLOTS],
    /// Index of the last queued slot (for presentation).
    queued_slot: Option<usize>,
}

impl BufferQueue {
    pub fn new() -> Self {
        Self {
            slots: [SlotState::Free; NUM_SLOTS],
            buffers: [None, None, None, None],
            queued_slot: None,
        }
    }

    /// Dequeue a free buffer slot for the game to write into.
    /// Returns `Some(slot_index)` or `None` if all slots are busy.
    pub fn dequeue(&mut self) -> Option<usize> {
        for (i, state) in self.slots.iter_mut().enumerate() {
            if *state == SlotState::Free {
                *state = SlotState::Dequeued;
                log::debug!("buffer_queue: dequeue slot={}", i);
                return Some(i);
            }
        }
        log::warn!("buffer_queue: no free slots");
        None
    }

    /// Queue a slot for presentation after the game has written pixel data.
    pub fn queue(&mut self, slot: usize) {
        if slot < NUM_SLOTS && self.slots[slot] == SlotState::Dequeued {
            self.slots[slot] = SlotState::Queued;
            self.queued_slot = Some(slot);
            log::debug!("buffer_queue: queue slot={}", slot);
        } else {
            log::warn!("buffer_queue: cannot queue slot={} (state={:?})", slot,
                       if slot < NUM_SLOTS { self.slots[slot] } else { SlotState::Free });
        }
    }

    /// Set the graphic buffer description for a slot (SetPreallocatedBuffer).
    pub fn set_buffer(&mut self, slot: usize, buffer: GraphicBuffer) {
        if slot < NUM_SLOTS {
            log::debug!(
                "buffer_queue: set_buffer slot={}, {}x{}, handle={}, offset=0x{:X}",
                slot,
                buffer.width,
                buffer.height,
                buffer.nvmap_handle,
                buffer.offset
            );
            self.buffers[slot] = Some(buffer);
            self.slots[slot] = SlotState::Free;
        }
    }

    /// Acquire the most recently queued buffer for presentation.
    /// Returns `Some((slot, buffer))` or `None` if nothing is queued.
    pub fn acquire(&mut self) -> Option<(usize, &GraphicBuffer)> {
        if let Some(slot) = self.queued_slot {
            if self.slots[slot] == SlotState::Queued {
                self.slots[slot] = SlotState::Acquired;
                self.queued_slot = None;
                if let Some(ref buf) = self.buffers[slot] {
                    return Some((slot, buf));
                }
            }
        }
        None
    }

    /// Release a previously acquired slot back to the free pool.
    pub fn release(&mut self, slot: usize) {
        if slot < NUM_SLOTS && self.slots[slot] == SlotState::Acquired {
            self.slots[slot] = SlotState::Free;
            log::debug!("buffer_queue: release slot={}", slot);
        }
    }

    /// Get the buffer description for a slot (for RequestBuffer).
    pub fn get_buffer(&self, slot: usize) -> Option<&GraphicBuffer> {
        if slot < NUM_SLOTS {
            self.buffers[slot].as_ref()
        } else {
            None
        }
    }
}

impl Default for BufferQueue {
    fn default() -> Self {
        Self::new()
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_buffer(w: u32, h: u32) -> GraphicBuffer {
        GraphicBuffer {
            width: w,
            height: h,
            format: 1,
            nvmap_handle: 1,
            offset: 0x1000,
            stride: w,
        }
    }

    #[test]
    fn test_dequeue_returns_slot() {
        let mut bq = BufferQueue::new();
        bq.set_buffer(0, make_buffer(1280, 720));
        let slot = bq.dequeue();
        assert_eq!(slot, Some(0));
        assert_eq!(bq.slots[0], SlotState::Dequeued);
    }

    #[test]
    fn test_queue_and_acquire() {
        let mut bq = BufferQueue::new();
        bq.set_buffer(0, make_buffer(1280, 720));

        let slot = bq.dequeue().unwrap();
        bq.queue(slot);

        let acquired = bq.acquire();
        assert!(acquired.is_some());
        let (acq_slot, buf) = acquired.unwrap();
        assert_eq!(acq_slot, 0);
        assert_eq!(buf.width, 1280);
        assert_eq!(bq.slots[0], SlotState::Acquired);
    }

    #[test]
    fn test_release() {
        let mut bq = BufferQueue::new();
        bq.set_buffer(0, make_buffer(1280, 720));

        let slot = bq.dequeue().unwrap();
        bq.queue(slot);
        let (acq_slot, _) = bq.acquire().unwrap();
        bq.release(acq_slot);

        assert_eq!(bq.slots[0], SlotState::Free);
    }

    #[test]
    fn test_no_free_slots() {
        let mut bq = BufferQueue::new();
        // Dequeue all 4 slots.
        for i in 0..NUM_SLOTS {
            bq.set_buffer(i, make_buffer(100, 100));
        }
        for _ in 0..NUM_SLOTS {
            assert!(bq.dequeue().is_some());
        }
        // Fifth dequeue should fail.
        assert!(bq.dequeue().is_none());
    }

    #[test]
    fn test_acquire_without_queue() {
        let mut bq = BufferQueue::new();
        assert!(bq.acquire().is_none());
    }

    #[test]
    fn test_multiple_queue_overwrites() {
        let mut bq = BufferQueue::new();
        bq.set_buffer(0, make_buffer(1280, 720));
        bq.set_buffer(1, make_buffer(640, 480));

        let slot0 = bq.dequeue().unwrap();
        bq.queue(slot0);

        let slot1 = bq.dequeue().unwrap();
        bq.queue(slot1);

        // Should acquire the latest queued (slot 1).
        let (acq_slot, buf) = bq.acquire().unwrap();
        assert_eq!(acq_slot, 1);
        assert_eq!(buf.width, 640);
    }
}

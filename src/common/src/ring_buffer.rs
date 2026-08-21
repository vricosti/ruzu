//! Port of zuyu/src/common/ring_buffer.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::sync::atomic::{AtomicUsize, Ordering};

/// SPSC (Single Producer, Single Consumer) lock-free ring buffer.
///
/// Mirrors the C++ `Common::RingBuffer<T, capacity>`. The capacity must be a
/// power of two. `T` must be `Copy` (equivalent to C++ trivially copyable).
///
/// Uses cache-line padding between the read and write indices to avoid false
/// sharing, matching the C++ implementation.
pub struct RingBuffer<T: Copy + Default, const CAPACITY: usize> {
    // Pad to separate cache lines (128 bytes as in C++)
    read_index: CacheAligned<AtomicUsize>,
    write_index: CacheAligned<AtomicUsize>,
    data: Box<[T; CAPACITY]>,
}

/// Wrapper to force 128-byte alignment, matching the C++ `alignas(128)`.
#[repr(align(128))]
struct CacheAligned<T>(T);

// SAFETY: RingBuffer is designed for SPSC use. The atomic indices ensure
// correct synchronization between the single producer and single consumer
// threads.
unsafe impl<T: Copy + Default + Send, const CAPACITY: usize> Send for RingBuffer<T, CAPACITY> {}
unsafe impl<T: Copy + Default + Send, const CAPACITY: usize> Sync for RingBuffer<T, CAPACITY> {}

impl<T: Copy + Default, const CAPACITY: usize> RingBuffer<T, CAPACITY> {
    const _ASSERT_POWER_OF_TWO: () = assert!(
        CAPACITY > 0 && (CAPACITY & (CAPACITY - 1)) == 0,
        "capacity must be a power of two"
    );

    /// Create a new ring buffer.
    pub fn new() -> Self {
        // Force compile-time check
        let _ = Self::_ASSERT_POWER_OF_TWO;

        Self {
            read_index: CacheAligned(AtomicUsize::new(0)),
            write_index: CacheAligned(AtomicUsize::new(0)),
            data: Box::new([T::default(); CAPACITY]),
        }
    }

    /// Push slots into the ring buffer from a slice.
    ///
    /// Returns the number of slots actually pushed (may be less than
    /// `slots.len()` if the buffer is full).
    pub fn push(&self, slots: &[T]) -> usize {
        let write_index = self.write_index.0.load(Ordering::Relaxed);
        let slots_free = CAPACITY + self.read_index.0.load(Ordering::Acquire) - write_index;
        let push_count = slots.len().min(slots_free);

        let pos = write_index % CAPACITY;
        let first_copy = (CAPACITY - pos).min(push_count);
        let second_copy = push_count - first_copy;

        // SAFETY: We have exclusive write access (single producer) and the
        // indices guarantee we don't overlap with the reader's active region.
        unsafe {
            let data_ptr = self.data.as_ptr() as *mut T;
            std::ptr::copy_nonoverlapping(slots.as_ptr(), data_ptr.add(pos), first_copy);
            if second_copy > 0 {
                std::ptr::copy_nonoverlapping(
                    slots.as_ptr().add(first_copy),
                    data_ptr,
                    second_copy,
                );
            }
        }

        self.write_index
            .0
            .store(write_index + push_count, Ordering::Release);

        push_count
    }

    /// Pop slots from the ring buffer into `output`.
    ///
    /// Returns the number of slots actually popped.
    pub fn pop_into(&self, output: &mut [T]) -> usize {
        let read_index = self.read_index.0.load(Ordering::Relaxed);
        let slots_filled = self.write_index.0.load(Ordering::Acquire) - read_index;
        let pop_count = slots_filled.min(output.len());

        let pos = read_index % CAPACITY;
        let first_copy = (CAPACITY - pos).min(pop_count);
        let second_copy = pop_count - first_copy;

        unsafe {
            let data_ptr = self.data.as_ptr();
            std::ptr::copy_nonoverlapping(data_ptr.add(pos), output.as_mut_ptr(), first_copy);
            if second_copy > 0 {
                std::ptr::copy_nonoverlapping(
                    data_ptr,
                    output.as_mut_ptr().add(first_copy),
                    second_copy,
                );
            }
        }

        self.read_index
            .0
            .store(read_index + pop_count, Ordering::Release);

        pop_count
    }

    /// Pop up to `max_slots` from the ring buffer, returning them as a `Vec`.
    pub fn pop(&self, max_slots: usize) -> Vec<T> {
        let count = max_slots.min(CAPACITY);
        let mut out = vec![T::default(); count];
        let actual = self.pop_into(&mut out);
        out.truncate(actual);
        out
    }

    /// Returns the number of slots currently used.
    pub fn size(&self) -> usize {
        self.write_index.0.load(Ordering::Acquire) - self.read_index.0.load(Ordering::Acquire)
    }

    /// Returns the capacity of the ring buffer.
    pub const fn capacity(&self) -> usize {
        CAPACITY
    }
}

impl<T: Copy + Default, const CAPACITY: usize> Default for RingBuffer<T, CAPACITY> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop() {
        let rb = RingBuffer::<i32, 8>::new();
        let data = [1, 2, 3, 4];
        assert_eq!(rb.push(&data), 4);
        assert_eq!(rb.size(), 4);

        let out = rb.pop(10);
        assert_eq!(out, vec![1, 2, 3, 4]);
        assert_eq!(rb.size(), 0);
    }

    #[test]
    fn test_wraparound() {
        let rb = RingBuffer::<u8, 4>::new();
        assert_eq!(rb.push(&[1, 2, 3]), 3);
        let _ = rb.pop(2); // pop 1, 2
        assert_eq!(rb.push(&[4, 5, 6]), 3); // wraps around
        let out = rb.pop(10);
        assert_eq!(out, vec![3, 4, 5, 6]);
    }

    #[test]
    fn test_full_buffer() {
        let rb = RingBuffer::<i32, 4>::new();
        assert_eq!(rb.push(&[1, 2, 3, 4, 5]), 4); // only 4 fit
        assert_eq!(rb.size(), 4);
    }
}

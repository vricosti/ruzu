//! Port of zuyu/src/core/hle/kernel/k_page_buffer.h
//! Status: Stubbed
//! Derniere synchro: 2026-03-11
//!
//! KPageBuffer and KPageBufferSlabHeap.

use super::k_memory_block::PAGE_SIZE;

/// Port of Kernel::KPageBufferSlabHeap.
pub struct KPageBufferSlabHeap;

impl KPageBufferSlabHeap {
    pub const BUFFER_SIZE: usize = PAGE_SIZE;

    pub fn new() -> Self {
        Self
    }

    /// Stubbed: depends on Core::System for slab heap initialization.
    pub fn initialize(&mut self) {
        // TODO: implement when slab heap infrastructure is ported.
    }
}

impl Default for KPageBufferSlabHeap {
    fn default() -> Self {
        Self::new()
    }
}

/// Port of Kernel::KPageBuffer.
///
/// A page-aligned buffer of exactly one page.
#[repr(C, align(4096))]
pub struct KPageBuffer {
    m_buffer: [u8; PAGE_SIZE],
}

impl KPageBuffer {
    pub fn new() -> Self {
        Self {
            m_buffer: [0u8; PAGE_SIZE],
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.m_buffer
    }

    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        &mut self.m_buffer
    }
}

impl Default for KPageBuffer {
    fn default() -> Self {
        Self::new()
    }
}

const _: () = assert!(std::mem::size_of::<KPageBuffer>() == PAGE_SIZE);

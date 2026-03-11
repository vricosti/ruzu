//! Port of zuyu/src/core/hle/kernel/code_set.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! CodeSet: represents executable data (text, rodata, data segments) that
//! may be loaded into a kernel process.

/// A single segment within a code set.
#[derive(Debug, Clone, Default)]
pub struct Segment {
    /// The byte offset of this segment within the backing memory.
    pub offset: usize,
    /// The virtual address to map this segment to.
    pub addr: u64,
    /// The size of this segment in bytes.
    pub size: u32,
}

/// Represents executable data that may be loaded into a kernel process.
///
/// A code set consists of three basic segments:
/// - A code (text) segment containing executable instructions
/// - A read-only data segment (rodata) containing constants
/// - A data segment containing mutable variables
pub struct CodeSet {
    /// The overall data that backs this code set (PhysicalMemory).
    pub memory: Vec<u8>,
    /// The segments: [text, rodata, data].
    pub segments: [Segment; 3],
    /// The entry point address for this code set.
    pub entrypoint: u64,
}

impl CodeSet {
    pub fn new() -> Self {
        Self {
            memory: Vec::new(),
            segments: Default::default(),
            entrypoint: 0,
        }
    }

    pub fn code_segment(&self) -> &Segment {
        &self.segments[0]
    }

    pub fn code_segment_mut(&mut self) -> &mut Segment {
        &mut self.segments[0]
    }

    pub fn rodata_segment(&self) -> &Segment {
        &self.segments[1]
    }

    pub fn rodata_segment_mut(&mut self) -> &mut Segment {
        &mut self.segments[1]
    }

    pub fn data_segment(&self) -> &Segment {
        &self.segments[2]
    }

    pub fn data_segment_mut(&mut self) -> &mut Segment {
        &mut self.segments[2]
    }
}

impl Default for CodeSet {
    fn default() -> Self {
        Self::new()
    }
}

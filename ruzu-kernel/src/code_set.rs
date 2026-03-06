//! Port of zuyu/src/core/hle/kernel/code_set.h and code_set.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Represents executable data that may be loaded into a kernel process.
//! A code set consists of three basic segments:
//!   - A code (text) segment containing executable instructions
//!   - A read-only data segment (rodata) containing constants
//!   - A data segment containing mutable variables (+ BSS)

use common::VAddr;

/// A single segment within a code set.
#[derive(Debug, Clone, Default)]
pub struct Segment {
    /// The byte offset of this segment within the backing memory.
    pub offset: usize,
    /// The virtual address to map this segment to.
    pub addr: VAddr,
    /// The size of this segment in bytes.
    pub size: u32,
}

/// Represents executable data that may be loaded into a kernel process.
///
/// Contains the backing memory for all segments plus metadata describing
/// how text, rodata, and data segments are laid out.
#[derive(Debug, Default)]
pub struct CodeSet {
    /// The overall data that backs this code set.
    /// All segment offsets are relative to this buffer.
    pub memory: Vec<u8>,

    /// The segments that comprise this code set: [text, rodata, data].
    pub segments: [Segment; 3],

    /// The entry point address for this code set.
    pub entrypoint: VAddr,
}

impl CodeSet {
    /// Create a new empty code set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a reference to the code (text) segment.
    pub fn code_segment(&self) -> &Segment {
        &self.segments[0]
    }

    /// Get a mutable reference to the code (text) segment.
    pub fn code_segment_mut(&mut self) -> &mut Segment {
        &mut self.segments[0]
    }

    /// Get a reference to the read-only data segment.
    pub fn rodata_segment(&self) -> &Segment {
        &self.segments[1]
    }

    /// Get a mutable reference to the read-only data segment.
    pub fn rodata_segment_mut(&mut self) -> &mut Segment {
        &mut self.segments[1]
    }

    /// Get a reference to the data segment.
    pub fn data_segment(&self) -> &Segment {
        &self.segments[2]
    }

    /// Get a mutable reference to the data segment.
    pub fn data_segment_mut(&mut self) -> &mut Segment {
        &mut self.segments[2]
    }

    /// Get the bytes for a given segment from the backing memory.
    pub fn segment_data(&self, seg: &Segment) -> &[u8] {
        let end = seg.offset + seg.size as usize;
        if end <= self.memory.len() {
            &self.memory[seg.offset..end]
        } else {
            &[]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_set_default() {
        let cs = CodeSet::new();
        assert_eq!(cs.entrypoint, 0);
        assert!(cs.memory.is_empty());
        for seg in &cs.segments {
            assert_eq!(seg.offset, 0);
            assert_eq!(seg.addr, 0);
            assert_eq!(seg.size, 0);
        }
    }

    #[test]
    fn test_segment_accessors() {
        let mut cs = CodeSet::new();
        cs.code_segment_mut().size = 0x1000;
        cs.rodata_segment_mut().size = 0x800;
        cs.data_segment_mut().size = 0x2000;

        assert_eq!(cs.code_segment().size, 0x1000);
        assert_eq!(cs.rodata_segment().size, 0x800);
        assert_eq!(cs.data_segment().size, 0x2000);
    }

    #[test]
    fn test_segment_data() {
        let mut cs = CodeSet::new();
        cs.memory = vec![0xAA; 0x100];
        cs.code_segment_mut().offset = 0;
        cs.code_segment_mut().size = 0x80;

        let data = cs.segment_data(cs.code_segment());
        assert_eq!(data.len(), 0x80);
        assert!(data.iter().all(|&b| b == 0xAA));
    }
}

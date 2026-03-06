use common::alignment::align_up;
use log::error;

pub struct WorkbufferAllocator<'a> {
    buffer: &'a mut [u8],
    size: u64,
    offset: u64,
}

impl<'a> WorkbufferAllocator<'a> {
    pub fn new(buffer: &'a mut [u8], size: u64) -> Self {
        Self {
            buffer,
            size,
            offset: 0,
        }
    }

    pub fn allocate_bytes(&mut self, count: u64, alignment: u64) -> Option<&mut [u8]> {
        let byte_size = count as usize;
        if byte_size == 0 {
            return Some(&mut self.buffer[0..0]);
        }

        let current = self.offset;
        let aligned = align_up(current, alignment);
        let end = aligned.saturating_add(count);
        if end > self.size {
            error!(
                "Allocated buffer was too small to hold new alloc. allocator_size=0x{size:08X}, offset=0x{offset:08X}, request=0x{request:08X}, alignment=0x{alignment:02X}",
                size = self.size,
                offset = self.offset,
                request = count,
                alignment = alignment,
            );
            return None;
        }

        self.offset = end;
        let start = aligned as usize;
        let end = end as usize;
        Some(&mut self.buffer[start..end])
    }

    pub fn align(&mut self, alignment: u64) {
        self.offset = align_up(self.offset, alignment);
    }

    pub const fn current_offset(&self) -> u64 {
        self.offset
    }

    pub const fn size(&self) -> u64 {
        self.size
    }

    pub const fn remaining_size(&self) -> u64 {
        self.size.saturating_sub(self.offset)
    }
}

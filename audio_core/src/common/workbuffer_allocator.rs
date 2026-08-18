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

        let base = self.buffer.as_ptr() as usize as u64;
        let current = base + self.offset;
        let aligned = align_up(current, alignment) - base;
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
        let base = self.buffer.as_ptr() as usize as u64;
        self.offset = align_up(base + self.offset, alignment) - base;
    }

    pub const fn get_current_offset(&self) -> u64 {
        self.offset
    }

    pub const fn get_size(&self) -> u64 {
        self.size
    }

    pub const fn get_remaining_size(&self) -> u64 {
        self.size.saturating_sub(self.offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocation_alignment_uses_the_absolute_buffer_address() {
        let mut storage = [0u8; 96];
        let base = storage.as_ptr() as usize;
        let start = (0..16)
            .find(|offset| (base + offset) % 16 != 0)
            .expect("one of the first 16 offsets must be misaligned");
        let buffer = &mut storage[start..];
        let buffer_address = buffer.as_ptr() as usize;
        let mut allocator = WorkbufferAllocator::new(buffer, (96 - start) as u64);

        let (allocation_address, allocation_len) = {
            let allocation = allocator.allocate_bytes(4, 16).unwrap();
            (allocation.as_ptr() as usize, allocation.len())
        };

        assert_eq!(allocation_address % 16, 0);
        assert_eq!(
            allocator.get_current_offset(),
            (allocation_address - buffer_address + allocation_len) as u64
        );
    }

    #[test]
    fn align_uses_the_absolute_buffer_address() {
        let mut storage = [0u8; 96];
        let base = storage.as_ptr() as usize;
        let start = (0..16)
            .find(|offset| (base + offset) % 16 != 0)
            .expect("one of the first 16 offsets must be misaligned");
        let buffer = &mut storage[start..];
        let buffer_address = buffer.as_ptr() as usize;
        let mut allocator = WorkbufferAllocator::new(buffer, (96 - start) as u64);

        allocator.align(16);

        assert_eq!(
            (buffer_address + allocator.get_current_offset() as usize) % 16,
            0
        );
    }
}

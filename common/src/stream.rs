// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SeekOrigin {
    SetOrigin,
    FromCurrentPos,
    FromEnd,
}

/// A simple bitstream wrapper that provides common functionality on a byte buffer.
#[derive(Debug, Default)]
pub struct Stream {
    buffer: Vec<u8>,
    position: usize,
}

impl Stream {
    pub fn new() -> Self {
        Self {
            buffer: Vec::new(),
            position: 0,
        }
    }

    /// Reposition bitstream "cursor" to the specified offset from origin.
    pub fn seek(&mut self, offset: i32, origin: SeekOrigin) {
        match origin {
            SeekOrigin::SetOrigin => {
                if offset < 0 {
                    self.position = 0;
                } else if self.position >= self.buffer.len() {
                    self.position = self.buffer.len();
                } else {
                    self.position = offset as usize;
                }
            }
            SeekOrigin::FromCurrentPos => {
                let new_offset = self.position as i32 + offset;
                self.seek(new_offset, SeekOrigin::SetOrigin);
            }
            SeekOrigin::FromEnd => {
                let new_offset = self.buffer.len() as i32 - offset;
                self.seek(new_offset, SeekOrigin::SetOrigin);
            }
        }
    }

    /// Reads next byte in the stream buffer and increments position.
    /// Panics if position is out of range (matching upstream throw behavior).
    pub fn read_byte(&mut self) -> u8 {
        if self.position < self.buffer.len() {
            let byte = self.buffer[self.position];
            self.position += 1;
            byte
        } else {
            panic!("Attempting to read a byte not within the buffer range");
        }
    }

    /// Writes byte at current position.
    /// If at end, appends. Otherwise, inserts at the current position.
    pub fn write_byte(&mut self, byte: u8) {
        if self.position == self.buffer.len() {
            self.buffer.push(byte);
            self.position += 1;
        } else {
            self.buffer.insert(self.position, byte);
        }
    }

    pub fn get_position(&self) -> usize {
        self.position
    }

    pub fn get_buffer(&self) -> &Vec<u8> {
        &self.buffer
    }

    pub fn get_buffer_mut(&mut self) -> &mut Vec<u8> {
        &mut self.buffer
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_appends() {
        let mut s = Stream::new();
        s.write_byte(0xAA);
        s.write_byte(0xBB);
        s.write_byte(0xCC);

        assert_eq!(s.get_position(), 3);
        assert_eq!(s.get_buffer().len(), 3);
        assert_eq!(s.get_buffer()[0], 0xAA);
        assert_eq!(s.get_buffer()[1], 0xBB);
        assert_eq!(s.get_buffer()[2], 0xCC);
    }

    #[test]
    fn test_read_sequential() {
        // Build buffer, then read by re-creating stream at position 0
        let mut s = Stream::new();
        s.write_byte(0x01);
        s.write_byte(0x02);
        s.write_byte(0x03);

        // Manually reset position for reading (tests use inner access)
        s.position = 0;
        assert_eq!(s.read_byte(), 0x01);
        assert_eq!(s.read_byte(), 0x02);
        assert_eq!(s.read_byte(), 0x03);
    }

    #[test]
    fn test_seek_set_origin_quirk() {
        // Upstream quirk: SetOrigin checks `position >= buffer.size()`, not `offset`.
        // When position == len, seek always clamps to len.
        let mut s = Stream::new();
        s.write_byte(0x01);
        s.write_byte(0x02);
        // position=2, len=2
        s.seek(0, SeekOrigin::SetOrigin);
        assert_eq!(s.get_position(), 2); // clamped to len, not set to 0
    }

    #[test]
    fn test_seek_set_origin_when_within() {
        let mut s = Stream::new();
        s.write_byte(0x01);
        s.write_byte(0x02);
        s.write_byte(0x03);

        // After read_byte, position advances but stays < len until last read
        s.position = 0;
        assert_eq!(s.read_byte(), 0x01); // position = 1, which is < 3

        // Now position=1 < len=3, so SetOrigin works
        s.seek(2, SeekOrigin::SetOrigin);
        assert_eq!(s.get_position(), 2);
        assert_eq!(s.read_byte(), 0x03);
    }

    #[test]
    fn test_seek_from_end() {
        let mut s = Stream::new();
        s.write_byte(0x01);
        s.write_byte(0x02);
        s.write_byte(0x03);

        // Set position within buffer first
        s.position = 0;
        // FromEnd(1) => SetOrigin(3-1=2), position(0) < len(3), so it works
        s.seek(1, SeekOrigin::FromEnd);
        assert_eq!(s.get_position(), 2);
        assert_eq!(s.read_byte(), 0x03);
    }

    #[test]
    fn test_seek_from_current() {
        let mut s = Stream::new();
        s.write_byte(0x01);
        s.write_byte(0x02);
        s.write_byte(0x03);

        s.position = 0;
        s.seek(2, SeekOrigin::FromCurrentPos);
        assert_eq!(s.get_position(), 2);
        assert_eq!(s.read_byte(), 0x03);
    }

    #[test]
    fn test_seek_negative_clamps_to_zero() {
        let mut s = Stream::new();
        s.write_byte(0x01);
        s.position = 0;
        s.seek(-100, SeekOrigin::SetOrigin);
        assert_eq!(s.get_position(), 0);
    }

    #[test]
    fn test_write_insert() {
        let mut s = Stream::new();
        s.write_byte(0x01);
        s.write_byte(0x03);

        s.position = 1;
        s.write_byte(0x02);
        // insert at position 1: buffer=[0x01, 0x02, 0x03]
        assert_eq!(s.get_buffer()[0], 0x01);
        assert_eq!(s.get_buffer()[1], 0x02);
        assert_eq!(s.get_buffer()[2], 0x03);
    }

    #[test]
    #[should_panic(expected = "Attempting to read a byte not within the buffer range")]
    fn test_read_past_end() {
        let mut s = Stream::new();
        s.read_byte();
    }
}

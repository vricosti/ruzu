#[derive(Debug, Clone, Default)]
pub struct BitArray {
    pub buffer: Vec<bool>,
    pub size: u32,
}

impl BitArray {
    pub fn reset(&mut self) {
        self.buffer.fill(false);
    }
}

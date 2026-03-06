#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct EffectResultState {
    pub buffer: [u8; 0x80],
}

impl Default for EffectResultState {
    fn default() -> Self {
        Self { buffer: [0; 0x80] }
    }
}

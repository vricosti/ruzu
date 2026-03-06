use crate::common::common::MAX_MIX_BUFFERS;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct InParameter {
    pub id: u32,
    pub mix_volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub in_use: bool,
    pub _unk65: [u8; 0xB],
}

#[derive(Debug, Clone)]
pub struct VoiceChannelResource {
    pub mix_volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub prev_mix_volumes: [f32; MAX_MIX_BUFFERS as usize],
    pub id: u32,
    pub in_use: bool,
}

impl VoiceChannelResource {
    pub fn new(id: u32) -> Self {
        Self {
            mix_volumes: [0.0; MAX_MIX_BUFFERS as usize],
            prev_mix_volumes: [0.0; MAX_MIX_BUFFERS as usize],
            id,
            in_use: false,
        }
    }

    pub fn update(&mut self, params: &InParameter) {
        if params.id != self.id {
            return;
        }
        self.prev_mix_volumes = self.mix_volumes;
        self.mix_volumes = params.mix_volumes;
        self.in_use = params.in_use;
    }
}

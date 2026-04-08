use log::error;

use super::{VoiceChannelResource, VoiceInfo, VoiceState};

#[derive(Debug, Default, Clone)]
pub struct VoiceContext {
    sorted_voice_indices: Vec<usize>,
    voices: Vec<VoiceInfo>,
    channel_resources: Vec<VoiceChannelResource>,
    cpu_states: Vec<VoiceState>,
    dsp_states: Vec<VoiceState>,
    voice_count: u32,
    active_count: u32,
}

impl VoiceContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn initialize(&mut self, voice_count: u32, channel_resource_count: u32, state_count: u32) {
        self.sorted_voice_indices = (0..voice_count as usize).collect();
        self.voices = vec![VoiceInfo::default(); voice_count as usize];
        self.channel_resources = (0..channel_resource_count)
            .map(VoiceChannelResource::new)
            .collect();
        self.cpu_states = vec![VoiceState::default(); state_count as usize];
        self.dsp_states = vec![VoiceState::default(); state_count as usize];
        self.voice_count = voice_count;
        self.active_count = 0;
    }

    pub fn get_dsp_shared_state(&mut self, index: u32) -> Option<&mut VoiceState> {
        if index as usize >= self.dsp_states.len() {
            error!("audio_core: invalid voice dsp state index {:04X}", index);
            return None;
        }
        self.dsp_states.get_mut(index as usize)
    }

    pub fn get_dsp_shared_state_direct(&mut self, index: u32) -> &mut VoiceState {
        if index as usize >= self.dsp_states.len() {
            error!("audio_core: invalid voice dsp state index {:04X}", index);
        }
        &mut self.dsp_states[index as usize]
    }

    pub fn get_dsp_shared_state_ref(&self, index: u32) -> Option<&VoiceState> {
        if index as usize >= self.dsp_states.len() {
            error!("audio_core: invalid voice dsp state index {:04X}", index);
            return None;
        }
        self.dsp_states.get(index as usize)
    }

    pub fn dsp_shared_states(&self) -> &[VoiceState] {
        &self.dsp_states
    }

    pub fn get_channel_resource(&mut self, index: u32) -> Option<&mut VoiceChannelResource> {
        if index as usize >= self.channel_resources.len() {
            error!(
                "audio_core: invalid voice channel resource index {:04X}",
                index
            );
            return None;
        }
        self.channel_resources.get_mut(index as usize)
    }

    pub fn get_channel_resource_direct(&mut self, index: u32) -> &mut VoiceChannelResource {
        if index as usize >= self.channel_resources.len() {
            error!(
                "audio_core: invalid voice channel resource index {:04X}",
                index
            );
        }
        &mut self.channel_resources[index as usize]
    }

    pub fn get_channel_resource_ref(&self, index: u32) -> Option<&VoiceChannelResource> {
        if index as usize >= self.channel_resources.len() {
            error!(
                "audio_core: invalid voice channel resource index {:04X}",
                index
            );
            return None;
        }
        self.channel_resources.get(index as usize)
    }

    pub fn get_sorted_info(&self, index: u32) -> Option<&VoiceInfo> {
        let Some(voice_index) = self.sorted_voice_indices.get(index as usize).copied() else {
            error!("audio_core: invalid voice sorted info index {:04X}", index);
            return None;
        };
        self.voices.get(voice_index)
    }

    pub fn get_sorted_info_mut(&mut self, index: u32) -> Option<&mut VoiceInfo> {
        let Some(voice_index) = self.sorted_voice_indices.get(index as usize).copied() else {
            error!("audio_core: invalid voice sorted info index {:04X}", index);
            return None;
        };
        self.voices.get_mut(voice_index)
    }

    pub fn get_info(&self, index: u32) -> Option<&VoiceInfo> {
        if index as usize >= self.voices.len() {
            error!("audio_core: invalid voice info index {:04X}", index);
            return None;
        }
        self.voices.get(index as usize)
    }

    pub fn get_info_direct(&self, index: u32) -> &VoiceInfo {
        if index as usize >= self.voices.len() {
            error!("audio_core: invalid voice info index {:04X}", index);
        }
        &self.voices[index as usize]
    }

    pub fn get_info_mut(&mut self, index: u32) -> Option<&mut VoiceInfo> {
        if index as usize >= self.voices.len() {
            error!("audio_core: invalid voice info index {:04X}", index);
            return None;
        }
        self.voices.get_mut(index as usize)
    }

    pub fn get_info_mut_direct(&mut self, index: u32) -> &mut VoiceInfo {
        if index as usize >= self.voices.len() {
            error!("audio_core: invalid voice info index {:04X}", index);
        }
        &mut self.voices[index as usize]
    }

    pub fn get_state(&self, index: u32) -> Option<&VoiceState> {
        if index as usize >= self.cpu_states.len() {
            error!("audio_core: invalid voice cpu state index {:04X}", index);
            return None;
        }
        self.cpu_states.get(index as usize)
    }

    pub fn get_state_direct(&self, index: u32) -> &VoiceState {
        if index as usize >= self.cpu_states.len() {
            error!("audio_core: invalid voice cpu state index {:04X}", index);
        }
        &self.cpu_states[index as usize]
    }

    pub fn get_state_mut(&mut self, index: u32) -> Option<&mut VoiceState> {
        if index as usize >= self.cpu_states.len() {
            error!("audio_core: invalid voice cpu state index {:04X}", index);
            return None;
        }
        self.cpu_states.get_mut(index as usize)
    }

    pub fn get_state_mut_direct(&mut self, index: u32) -> &mut VoiceState {
        if index as usize >= self.cpu_states.len() {
            error!("audio_core: invalid voice cpu state index {:04X}", index);
        }
        &mut self.cpu_states[index as usize]
    }

    pub fn sorted_voice_indices(&self) -> &[usize] {
        &self.sorted_voice_indices
    }

    pub fn infos_mut(&mut self) -> &mut [VoiceInfo] {
        &mut self.voices
    }

    pub fn update_info_for_command_generation(&mut self, index: usize) -> Option<VoiceInfo> {
        let mut voice = self.voices.get(index)?.clone();
        let should_generate = voice.update_for_command_generation(self);
        self.voices[index] = voice.clone();
        should_generate.then_some(voice)
    }

    pub fn get_count(&self) -> u32 {
        self.voice_count
    }

    pub fn get_active_count(&self) -> u32 {
        self.active_count
    }

    pub fn set_active_count(&mut self, active_count: u32) {
        self.active_count = active_count;
    }

    pub fn sort_info(&mut self) {
        self.sorted_voice_indices = (0..self.voice_count as usize).collect();
        self.sorted_voice_indices.sort_by(|lhs, rhs| {
            let lhs = &self.voices[*lhs];
            let rhs = &self.voices[*rhs];
            rhs.priority
                .cmp(&lhs.priority)
                .then_with(|| rhs.sort_order.cmp(&lhs.sort_order))
        });
    }

    pub fn update_state_by_dsp_shared(&mut self) {
        let count = self.voice_count as usize;
        if self.cpu_states.len() < count || self.dsp_states.len() < count {
            return;
        }
        self.cpu_states[..count].copy_from_slice(&self.dsp_states[..count]);
    }
}

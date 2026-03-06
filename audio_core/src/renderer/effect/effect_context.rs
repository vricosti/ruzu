use super::{EffectInfoBase, EffectResultState};

#[derive(Debug, Default, Clone)]
pub struct EffectContext {
    effect_infos: Vec<EffectInfoBase>,
    effect_count: u32,
    result_states_cpu: Vec<EffectResultState>,
    result_states_dsp: Vec<EffectResultState>,
    dsp_state_count: usize,
}

impl EffectContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn initialize(&mut self, effect_count: u32, dsp_state_count: usize) {
        self.effect_infos = vec![EffectInfoBase::default(); effect_count as usize];
        self.effect_count = effect_count;
        self.result_states_cpu = vec![EffectResultState::default(); dsp_state_count];
        self.result_states_dsp = vec![EffectResultState::default(); dsp_state_count];
        self.dsp_state_count = dsp_state_count;
    }

    pub fn get_info(&self, index: u32) -> Option<&EffectInfoBase> {
        self.effect_infos.get(index as usize)
    }

    pub fn infos(&self) -> &[EffectInfoBase] {
        &self.effect_infos
    }

    pub fn get_info_mut(&mut self, index: u32) -> Option<&mut EffectInfoBase> {
        self.effect_infos.get_mut(index as usize)
    }

    pub fn update_info_for_command_generation(&mut self, index: usize) -> Option<EffectInfoBase> {
        let source = self.effect_infos.get_mut(index)?;
        source.refresh_runtime_addresses();
        let mut effect = source.clone();
        effect.update_for_command_generation();
        self.effect_infos[index] = effect.clone();
        Some(effect)
    }

    pub fn get_result_state(&self, index: u32) -> Option<&EffectResultState> {
        self.result_states_cpu.get(index as usize)
    }

    pub fn get_result_state_mut(&mut self, index: u32) -> Option<&mut EffectResultState> {
        self.result_states_cpu.get_mut(index as usize)
    }

    pub fn get_dsp_shared_result_state(&self, index: u32) -> Option<&EffectResultState> {
        self.result_states_dsp.get(index as usize)
    }

    pub fn dsp_shared_result_states(&self) -> &[EffectResultState] {
        &self.result_states_dsp
    }

    pub fn get_dsp_shared_result_state_mut(
        &mut self,
        index: u32,
    ) -> Option<&mut EffectResultState> {
        self.result_states_dsp.get_mut(index as usize)
    }

    pub fn get_count(&self) -> u32 {
        self.effect_count
    }

    pub fn update_state_by_dsp_shared(&mut self) {
        let count = self
            .dsp_state_count
            .min(self.result_states_cpu.len())
            .min(self.result_states_dsp.len())
            .min(self.effect_infos.len());
        for index in 0..count {
            let effect = self.effect_infos[index].clone();
            let dsp_state = self.result_states_dsp[index];
            effect.update_result_state(&mut self.result_states_cpu[index], &dsp_state);
        }
    }
}

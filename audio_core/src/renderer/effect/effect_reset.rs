use super::effect_info_base::{EffectInfoBase, EffectType};

pub fn reset_effect(effect: &mut EffectInfoBase, type_: EffectType) {
    effect.cleanup();

    match type_ {
        EffectType::Invalid
        | EffectType::Mix
        | EffectType::Aux
        | EffectType::Delay
        | EffectType::Reverb
        | EffectType::I3dl2Reverb
        | EffectType::BiquadFilter
        | EffectType::LightLimiter
        | EffectType::Capture
        | EffectType::Compressor => effect.set_type(type_),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::common::{INVALID_PROCESS_ORDER, UNUSED_MIX_ID};
    use crate::renderer::effect::effect_info_base::{OutStatus, ParameterState, UsageState};

    #[test]
    fn reset_effect_clears_runtime_state_and_sets_new_type() {
        let mut effect = EffectInfoBase::default();
        effect.set_type(EffectType::Delay);
        effect.enabled = true;
        effect.mix_id = 3;
        effect.process_order = 7;
        effect.buffer_unmapped = true;
        effect.usage_state = UsageState::Enabled;
        effect.out_status = OutStatus::Used;
        effect.parameter_state = ParameterState::Updated;
        effect.parameter[0] = 1;
        effect.send_buffer = 2;
        effect.return_buffer = 3;

        reset_effect(&mut effect, EffectType::Reverb);

        assert_eq!(effect.get_type(), EffectType::Reverb);
        assert!(!effect.enabled);
        assert_eq!(effect.mix_id, UNUSED_MIX_ID);
        assert_eq!(effect.process_order, INVALID_PROCESS_ORDER);
        assert!(!effect.buffer_unmapped);
        assert_eq!(effect.usage_state, UsageState::Invalid);
        assert_eq!(effect.out_status, OutStatus::Invalid);
        assert_eq!(effect.parameter_state, ParameterState::Initialized);
        assert_eq!(effect.parameter, [0; 0xA0]);
        assert_eq!(effect.send_buffer, 0);
        assert_eq!(effect.return_buffer, 0);
    }
}

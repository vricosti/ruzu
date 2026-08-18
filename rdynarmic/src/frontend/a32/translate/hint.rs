use crate::ir::a32_emitter::A32IREmitter;

/// ARM WFI / WFE / YIELD hint instructions.
/// Upstream treats them as NOPs when hook_hint_instructions is false.
pub fn arm_wfi(_ir: &mut A32IREmitter) -> bool {
    true
}

pub fn arm_wfe(_ir: &mut A32IREmitter) -> bool {
    true
}

pub fn arm_yield(_ir: &mut A32IREmitter) -> bool {
    true
}

use crate::common::common::{FINAL_MIX_ID, INVALID_DISTANCE_FROM_FINAL_MIX};
use crate::renderer::behavior::BehaviorInfo;
use crate::renderer::nodes::{EdgeMatrix, NodeStates};
use crate::renderer::splitter::SplitterContext;

use super::MixInfo;

#[derive(Debug, Default, Clone)]
pub struct MixContext {
    sorted_mix_indices: Vec<usize>,
    mix_infos: Vec<MixInfo>,
    count: i32,
    effect_process_order_buffer: Vec<i32>,
    effect_count: u64,
    node_states: NodeStates,
    edge_matrix: EdgeMatrix,
}

impl MixContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn initialize(&mut self, count: u32, effect_count: u32, behavior: &BehaviorInfo) {
        self.count = count as i32;
        self.effect_count = effect_count as u64;
        self.sorted_mix_indices = (0..count as usize).collect();
        self.mix_infos = (0..count)
            .map(|_| MixInfo::new(effect_count as i32, behavior))
            .collect();
        self.effect_process_order_buffer = vec![-1; (count * effect_count.max(1)) as usize];

        let node_buffer_size = NodeStates::get_work_buffer_size(count);
        let edge_buffer_size = EdgeMatrix::get_work_buffer_size(count);
        let node_buffer = vec![0u8; node_buffer_size as usize];
        let edge_buffer = vec![0u8; edge_buffer_size as usize];
        self.node_states
            .initialize(&node_buffer, node_buffer_size, count);
        self.edge_matrix
            .initialize(&edge_buffer, edge_buffer_size, count);
    }

    pub fn get_sorted_info(&self, index: i32) -> Option<&MixInfo> {
        let index = usize::try_from(index).ok()?;
        let mix_index = *self.sorted_mix_indices.get(index)?;
        self.mix_infos.get(mix_index)
    }

    pub fn get_sorted_info_mut(&mut self, index: i32) -> Option<&mut MixInfo> {
        let index = usize::try_from(index).ok()?;
        let mix_index = *self.sorted_mix_indices.get(index)?;
        self.mix_infos.get_mut(mix_index)
    }

    pub fn set_sorted_info(&mut self, index: i32, mix_index: usize) {
        let Some(index) = usize::try_from(index).ok() else {
            return;
        };
        if let Some(slot) = self.sorted_mix_indices.get_mut(index) {
            *slot = mix_index;
        }
    }

    pub fn get_info(&self, index: i32) -> Option<&MixInfo> {
        let index = usize::try_from(index).ok()?;
        self.mix_infos.get(index)
    }

    pub fn get_info_mut(&mut self, index: i32) -> Option<&mut MixInfo> {
        let index = usize::try_from(index).ok()?;
        self.mix_infos.get_mut(index)
    }

    pub fn update_mix(
        &mut self,
        mix_id: i32,
        params: &super::mix_info::InParameter,
        effect_context: &crate::renderer::effect::EffectContext,
        splitter_context: &mut SplitterContext,
        behavior: &BehaviorInfo,
    ) -> bool {
        let Some(index) = usize::try_from(mix_id).ok() else {
            return false;
        };
        let Some(mix_info) = self.mix_infos.get_mut(index) else {
            return false;
        };
        mix_info.update(
            &mut self.edge_matrix,
            params,
            effect_context,
            splitter_context,
            behavior,
        )
    }

    pub fn get_final_mix_info(&self) -> Option<&MixInfo> {
        self.mix_infos.get(FINAL_MIX_ID as usize)
    }

    pub fn get_count(&self) -> i32 {
        self.count
    }

    pub fn update_distances_from_final_mix(&mut self) {
        for mix in &mut self.mix_infos {
            mix.distance_from_final_mix = INVALID_DISTANCE_FROM_FINAL_MIX;
        }

        for index in 0..self.count.max(0) as usize {
            let mix_id = self.mix_infos[index].mix_id;
            self.sorted_mix_indices[index] = index;

            if !self.mix_infos[index].in_use {
                continue;
            }

            let mut current_mix_id = mix_id;
            let mut distance_to_final_mix = FINAL_MIX_ID;

            while distance_to_final_mix < self.count {
                if current_mix_id == FINAL_MIX_ID {
                    break;
                }
                if current_mix_id == crate::common::common::UNUSED_MIX_ID {
                    distance_to_final_mix = INVALID_DISTANCE_FROM_FINAL_MIX;
                    break;
                }
                let Some(next_mix_index) = usize::try_from(current_mix_id).ok() else {
                    distance_to_final_mix = INVALID_DISTANCE_FROM_FINAL_MIX;
                    break;
                };
                let Some(next_mix) = self.mix_infos.get(next_mix_index) else {
                    distance_to_final_mix = INVALID_DISTANCE_FROM_FINAL_MIX;
                    break;
                };
                if next_mix.distance_from_final_mix != INVALID_DISTANCE_FROM_FINAL_MIX {
                    distance_to_final_mix = next_mix.distance_from_final_mix + 1;
                    break;
                }
                distance_to_final_mix += 1;
                current_mix_id = next_mix.dst_mix_id;
            }

            if distance_to_final_mix >= self.count {
                distance_to_final_mix = INVALID_DISTANCE_FROM_FINAL_MIX;
            }
            self.mix_infos[index].distance_from_final_mix = distance_to_final_mix;
        }
    }

    pub fn sort_info(&mut self) {
        self.update_distances_from_final_mix();
        self.sorted_mix_indices.sort_by(|lhs, rhs| {
            self.mix_infos[*rhs]
                .distance_from_final_mix
                .cmp(&self.mix_infos[*lhs].distance_from_final_mix)
        });
        self.calc_mix_buffer_offset();
    }

    pub fn calc_mix_buffer_offset(&mut self) {
        let mut offset = 0i16;
        for mix_index in self.sorted_mix_indices.iter().copied() {
            let mix_info = &mut self.mix_infos[mix_index];
            if mix_info.in_use {
                mix_info.buffer_offset = offset;
                offset = offset.saturating_add(mix_info.buffer_count);
            }
        }
    }

    pub fn tsort_info(&mut self, splitter_context: &SplitterContext) -> bool {
        if !splitter_context.using_splitter() {
            self.calc_mix_buffer_offset();
            return true;
        }

        if !self.node_states.tsort(&self.edge_matrix) {
            return false;
        }

        let sorted_results = self.node_states.get_sorted_results();
        let result_size = (self.count.max(0) as usize).min(sorted_results.len());
        for (slot, result) in sorted_results.into_iter().take(result_size).enumerate() {
            self.sorted_mix_indices[slot] = result as usize;
        }

        self.calc_mix_buffer_offset();
        true
    }

    pub fn get_edge_matrix(&self) -> &EdgeMatrix {
        &self.edge_matrix
    }

    pub fn get_edge_matrix_mut(&mut self) -> &mut EdgeMatrix {
        &mut self.edge_matrix
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::renderer::effect::EffectContext;

    #[test]
    fn negative_mix_indices_do_not_alias_slot_zero() {
        let behavior = BehaviorInfo::new();
        let mut mix_context = MixContext::new();
        mix_context.initialize(2, 0, &behavior);

        assert!(mix_context.get_info(-1).is_none());
        assert!(mix_context.get_info_mut(-1).is_none());
        assert!(mix_context.get_sorted_info(-1).is_none());
        assert!(mix_context.get_sorted_info_mut(-1).is_none());
    }

    #[test]
    fn update_mix_rejects_negative_mix_id() {
        let behavior = BehaviorInfo::new();
        let mut mix_context = MixContext::new();
        let effect_context = EffectContext::new();
        let mut splitter_context = SplitterContext::new();
        mix_context.initialize(2, 0, &behavior);

        assert!(!mix_context.update_mix(
            -1,
            &crate::renderer::mix::mix_info::InParameter::default(),
            &effect_context,
            &mut splitter_context,
            &behavior,
        ));
    }

    #[test]
    fn negative_next_mix_id_stays_invalid_during_distance_update() {
        let behavior = BehaviorInfo::new();
        let mut mix_context = MixContext::new();
        mix_context.initialize(2, 0, &behavior);
        if let Some(mix) = mix_context.get_info_mut(1) {
            mix.in_use = true;
            mix.mix_id = 1;
            mix.dst_mix_id = -2;
        }

        mix_context.update_distances_from_final_mix();

        assert_eq!(
            mix_context.get_info(1).unwrap().distance_from_final_mix,
            INVALID_DISTANCE_FROM_FINAL_MIX
        );
    }
}

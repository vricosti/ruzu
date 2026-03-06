use crate::common::common::{
    FINAL_MIX_ID, INVALID_DISTANCE_FROM_FINAL_MIX, MAX_MIX_BUFFERS, UNUSED_MIX_ID,
    UNUSED_SPLITTER_ID,
};
use crate::renderer::behavior::BehaviorInfo;
use crate::renderer::effect::EffectContext;
use crate::renderer::nodes::EdgeMatrix;
use crate::renderer::splitter::SplitterContext;

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct InParameter {
    pub volume: f32,
    pub sample_rate: u32,
    pub buffer_count: u32,
    pub in_use: bool,
    pub is_dirty: bool,
    pub _padding: [u8; 2],
    pub mix_id: i32,
    pub effect_count: u32,
    pub node_id: i32,
    pub _unk01c: [u8; 0x8],
    pub mix_volumes: [[f32; MAX_MIX_BUFFERS as usize]; MAX_MIX_BUFFERS as usize],
    pub dest_mix_id: i32,
    pub dest_splitter_id: i32,
    pub _unk92c: [u8; 0x4],
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct InDirtyParameter {
    pub magic: u32,
    pub count: i32,
    pub _unk08: [u8; 0x18],
}

#[derive(Debug, Clone)]
pub struct MixInfo {
    pub volume: f32,
    pub sample_rate: u32,
    pub buffer_count: i16,
    pub in_use: bool,
    pub enabled: bool,
    pub mix_id: i32,
    pub node_id: i32,
    pub buffer_offset: i16,
    pub distance_from_final_mix: i32,
    pub effect_order_buffer: Vec<i32>,
    pub effect_count: i32,
    pub dst_mix_id: i32,
    pub mix_volumes: [[f32; MAX_MIX_BUFFERS as usize]; MAX_MIX_BUFFERS as usize],
    pub dst_splitter_id: i32,
    pub long_size_pre_delay_supported: bool,
}

impl MixInfo {
    pub fn new(effect_count: i32, behavior: &BehaviorInfo) -> Self {
        let mut out = Self {
            volume: 0.0,
            sample_rate: 0,
            buffer_count: 0,
            in_use: false,
            enabled: false,
            mix_id: UNUSED_MIX_ID,
            node_id: 0,
            buffer_offset: 0,
            distance_from_final_mix: INVALID_DISTANCE_FROM_FINAL_MIX,
            effect_order_buffer: vec![-1; effect_count.max(0) as usize],
            effect_count,
            dst_mix_id: UNUSED_MIX_ID,
            mix_volumes: [[0.0; MAX_MIX_BUFFERS as usize]; MAX_MIX_BUFFERS as usize],
            dst_splitter_id: UNUSED_SPLITTER_ID,
            long_size_pre_delay_supported: behavior.is_long_size_pre_delay_supported(),
        };
        out.clear_effect_processing_order();
        out
    }

    pub fn cleanup(&mut self) {
        self.mix_id = UNUSED_MIX_ID;
        self.dst_mix_id = UNUSED_MIX_ID;
        self.dst_splitter_id = UNUSED_SPLITTER_ID;
    }

    pub fn clear_effect_processing_order(&mut self) {
        self.effect_order_buffer.fill(-1);
    }

    pub fn update(
        &mut self,
        edge_matrix: &mut EdgeMatrix,
        in_params: &InParameter,
        effect_context: &EffectContext,
        splitter_context: &mut SplitterContext,
        behavior: &BehaviorInfo,
    ) -> bool {
        self.volume = in_params.volume;
        self.sample_rate = in_params.sample_rate;
        self.buffer_count = in_params.buffer_count as i16;
        self.in_use = in_params.in_use;
        self.enabled = in_params.in_use;
        self.mix_id = in_params.mix_id;
        self.node_id = in_params.node_id;
        self.mix_volumes = in_params.mix_volumes;

        let sort_required = if behavior.is_splitter_supported() {
            self.update_connection(edge_matrix, in_params, splitter_context)
        } else {
            let sort_required = self.dst_mix_id != in_params.dest_mix_id;
            self.dst_mix_id = in_params.dest_mix_id;
            self.dst_splitter_id = UNUSED_SPLITTER_ID;
            sort_required
        };

        self.clear_effect_processing_order();
        for effect_index in 0..effect_context.get_count() {
            let Some(info) = effect_context.get_info(effect_index) else {
                continue;
            };
            if self.mix_id == info.get_mix_id() {
                let processing_order = info.get_processing_order();
                if processing_order < 0
                    || processing_order as usize >= self.effect_order_buffer.len()
                {
                    continue;
                }
                self.effect_order_buffer[processing_order as usize] = effect_index as i32;
            }
        }

        sort_required
    }

    pub fn update_connection(
        &mut self,
        edge_matrix: &mut EdgeMatrix,
        in_params: &InParameter,
        splitter_context: &mut SplitterContext,
    ) -> bool {
        let mut has_new_connection = false;
        if self.dst_splitter_id != UNUSED_SPLITTER_ID {
            if let Some(splitter_info) = splitter_context.get_info(self.dst_splitter_id) {
                has_new_connection = splitter_info.has_new_connection();
            }
        }

        if self.dst_mix_id == in_params.dest_mix_id
            && self.dst_splitter_id == in_params.dest_splitter_id
            && !has_new_connection
        {
            return false;
        }

        if self.mix_id >= 0 {
            edge_matrix.remove_edges(self.mix_id as u32);
        }

        if in_params.dest_mix_id == UNUSED_MIX_ID {
            if in_params.dest_splitter_id != UNUSED_SPLITTER_ID {
                let destination_count = splitter_context
                    .get_info(in_params.dest_splitter_id)
                    .map(|info| info.get_destination_count())
                    .unwrap_or(0);
                for destination_index in 0..destination_count {
                    if let Some(destination) = splitter_context
                        .get_destination_data(in_params.dest_splitter_id, destination_index as i32)
                    {
                        let destination_id = destination.get_mix_id();
                        if destination_id != UNUSED_MIX_ID && self.mix_id >= 0 {
                            edge_matrix.connect(self.mix_id as u32, destination_id as u32);
                        }
                    }
                }
            }
        } else if self.mix_id >= 0 {
            edge_matrix.connect(self.mix_id as u32, in_params.dest_mix_id as u32);
        }

        self.dst_mix_id = in_params.dest_mix_id;
        self.dst_splitter_id = in_params.dest_splitter_id;
        true
    }

    pub fn has_any_connection(&self) -> bool {
        self.dst_mix_id != UNUSED_MIX_ID || self.dst_splitter_id != UNUSED_SPLITTER_ID
    }

    pub fn is_final_mix(&self) -> bool {
        self.mix_id == FINAL_MIX_ID
    }
}

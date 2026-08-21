// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_pad_holder.h and abstract_pad_holder.cpp

use std::sync::Arc;

use common::ResultCode;
use parking_lot::Mutex;

use crate::hid_result;
use crate::hid_types::*;
use crate::resources::npad::npad_types::*;

pub type AbstractPadRef = Arc<Mutex<IAbstractedPad>>;

#[derive(Clone)]
struct AbstractAssignmentHolder {
    abstracted_pad: Option<AbstractPadRef>,
    device_type: NpadStyleIndex,
    controller_id: u64,
}

impl Default for AbstractAssignmentHolder {
    fn default() -> Self {
        Self {
            abstracted_pad: None,
            device_type: NpadStyleIndex::None,
            controller_id: 0,
        }
    }
}

/// This is nn::hid::server::NpadAbstractedPadHolder
pub struct NpadAbstractedPadHolder {
    assignment_list: [AbstractAssignmentHolder; 5],
    list_size: u32,
    assignment_mode: NpadJoyAssignmentMode,
}

impl Default for NpadAbstractedPadHolder {
    fn default() -> Self {
        Self {
            assignment_list: Default::default(),
            list_size: 0,
            assignment_mode: NpadJoyAssignmentMode::Dual,
        }
    }
}

impl NpadAbstractedPadHolder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register_abstract_pad(&mut self, pad: AbstractPadRef) -> ResultCode {
        if self.list_size as usize >= self.assignment_list.len() {
            return hid_result::RESULT_NPAD_IS_NOT_PRO_CONTROLLER;
        }

        let pad_state = pad.lock();
        for i in 0..self.list_size as usize {
            if self.assignment_list[i].device_type == pad_state.device_type {
                return hid_result::RESULT_NPAD_IS_NOT_PRO_CONTROLLER;
            }
        }

        let idx = self.list_size as usize;
        self.assignment_list[idx] = AbstractAssignmentHolder {
            abstracted_pad: Some(Arc::clone(&pad)),
            device_type: pad_state.device_type,
            controller_id: pad_state.controller_id,
        };
        drop(pad_state);

        self.list_size += 1;
        ResultCode::SUCCESS
    }

    pub fn remove_abstract_pad_by_controller_id(&mut self, controller_id: u64) {
        if self.list_size == 0 || controller_id == 0 {
            return;
        }
        for i in 0..self.list_size as usize {
            if self.assignment_list[i].controller_id != controller_id {
                continue;
            }
            // Shift elements left
            let size = self.list_size as usize;
            for e in (i + 1)..size {
                self.assignment_list.swap(e - 1, e);
            }
            self.list_size -= 1;
            return;
        }
    }

    pub fn detach_abstracted_pad(&mut self) {
        while self.list_size > 0 {
            let size = self.list_size as usize;
            for i in 1..size {
                self.assignment_list.swap(i - 1, i);
            }
            self.list_size -= 1;
        }
    }

    pub fn remove_abstract_pad_by_assignment_style(
        &mut self,
        assignment_style: AssignmentStyle,
    ) -> u64 {
        for i in 0..self.list_size as usize {
            let matches = self.assignment_list[i]
                .abstracted_pad
                .as_ref()
                .is_some_and(|pad| assignment_style.raw & pad.lock().assignment_style.raw != 0);
            if !matches {
                continue;
            }
            for e in (i + 1)..self.list_size as usize {
                self.assignment_list[e - 1] = self.assignment_list[e].clone();
            }
            self.list_size -= 1;
            return self.list_size as u64;
        }
        self.list_size as u64
    }

    pub fn get_abstracted_pads(&self, list: &mut [Option<AbstractPadRef>]) -> u32 {
        let count = usize::min(list.len(), self.list_size as usize);
        for (destination, assignment) in list.iter_mut().zip(&self.assignment_list).take(count) {
            *destination = assignment.abstracted_pad.clone();
        }
        count as u32
    }

    pub fn get_abstracted_pad_count(&self) -> u32 {
        self.list_size
    }

    pub fn set_assignment_mode(&mut self, mode: NpadJoyAssignmentMode) {
        self.assignment_mode = mode;
    }

    pub fn get_assignment_mode(&self) -> NpadJoyAssignmentMode {
        self.assignment_mode
    }

    pub fn get_style_index_list(&self, list: &mut [NpadStyleIndex]) -> usize {
        let count = std::cmp::min(self.list_size as usize, list.len());
        for i in 0..count {
            list[i] = self.assignment_list[i].device_type;
        }
        self.list_size as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pad(
        controller_id: u64,
        device_type: NpadStyleIndex,
        assignment_style: u32,
    ) -> AbstractPadRef {
        Arc::new(Mutex::new(IAbstractedPad {
            controller_id,
            device_type,
            interface_type: NpadInterfaceType::Bluetooth,
            assignment_style: AssignmentStyle {
                raw: assignment_style,
            },
            ..IAbstractedPad::default()
        }))
    }

    #[test]
    fn holder_preserves_live_pad_identity_and_upstream_order() {
        let first = pad(11, NpadStyleIndex::Fullkey, 1 << 0);
        let second = pad(22, NpadStyleIndex::JoyconLeft, 1 << 1);
        let mut holder = NpadAbstractedPadHolder::new();
        assert!(holder
            .register_abstract_pad(Arc::clone(&first))
            .is_success());
        assert!(holder
            .register_abstract_pad(Arc::clone(&second))
            .is_success());

        first.lock().internal_flags.set_is_connected(true);
        let mut pads: [Option<AbstractPadRef>; 5] = std::array::from_fn(|_| None);
        assert_eq!(holder.get_abstracted_pads(&mut pads), 2);
        assert!(Arc::ptr_eq(pads[0].as_ref().unwrap(), &first));
        assert!(pads[0]
            .as_ref()
            .unwrap()
            .lock()
            .internal_flags
            .is_connected());
        assert!(Arc::ptr_eq(pads[1].as_ref().unwrap(), &second));

        assert_eq!(
            holder.remove_abstract_pad_by_assignment_style(AssignmentStyle { raw: 1 << 0 }),
            1
        );
        let mut remaining: [Option<AbstractPadRef>; 5] = std::array::from_fn(|_| None);
        assert_eq!(holder.get_abstracted_pads(&mut remaining), 1);
        assert!(Arc::ptr_eq(remaining[0].as_ref().unwrap(), &second));
    }

    #[test]
    fn duplicate_style_and_controller_removal_match_upstream() {
        let first = pad(11, NpadStyleIndex::Fullkey, 1);
        let duplicate = pad(22, NpadStyleIndex::Fullkey, 2);
        let second = pad(33, NpadStyleIndex::JoyconRight, 4);
        let mut holder = NpadAbstractedPadHolder::new();
        assert!(holder.register_abstract_pad(first).is_success());
        assert_eq!(
            holder.register_abstract_pad(duplicate),
            hid_result::RESULT_NPAD_IS_NOT_PRO_CONTROLLER
        );
        assert!(holder.register_abstract_pad(second).is_success());

        holder.remove_abstract_pad_by_controller_id(11);
        let mut pads: [Option<AbstractPadRef>; 5] = std::array::from_fn(|_| None);
        assert_eq!(holder.get_abstracted_pads(&mut pads), 1);
        assert_eq!(pads[0].as_ref().unwrap().lock().controller_id, 33);
    }
}

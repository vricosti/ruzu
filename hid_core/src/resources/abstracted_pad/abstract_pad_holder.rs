// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/resources/abstracted_pad/abstract_pad_holder.h and abstract_pad_holder.cpp

use common::ResultCode;

use crate::hid_result;
use crate::hid_types::*;
use crate::resources::npad::npad_types::*;

struct AbstractAssignmentHolder {
    device_type: NpadStyleIndex,
    interface_type: NpadInterfaceType,
    controller_id: u64,
}

impl Default for AbstractAssignmentHolder {
    fn default() -> Self {
        Self {
            device_type: NpadStyleIndex::None,
            interface_type: NpadInterfaceType::None,
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

    pub fn register_abstract_pad(&mut self, pad: &IAbstractedPad) -> ResultCode {
        if self.list_size as usize >= self.assignment_list.len() {
            return hid_result::RESULT_NPAD_IS_NOT_PRO_CONTROLLER;
        }

        for i in 0..self.list_size as usize {
            if self.assignment_list[i].device_type == pad.device_type {
                return hid_result::RESULT_NPAD_IS_NOT_PRO_CONTROLLER;
            }
        }

        let idx = self.list_size as usize;
        self.assignment_list[idx] = AbstractAssignmentHolder {
            device_type: pad.device_type,
            interface_type: pad.interface_type,
            controller_id: pad.controller_id,
        };

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

// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of hid_core/irsensor/processor_base.h and processor_base.cpp

use super::irs_types::ImageTransferProcessorFormat;

/// Base trait for IR sensor processors
pub trait Processor {
    fn start_processor(&mut self);
    fn suspend_processor(&mut self);
    fn stop_processor(&mut self);
    fn is_processor_active(&self) -> bool;
}

pub struct ProcessorBase {
    pub is_active: bool,
}

impl ProcessorBase {
    pub fn new() -> Self {
        Self { is_active: false }
    }

    pub fn is_processor_active(&self) -> bool {
        self.is_active
    }

    /// Returns the number of bytes the image uses
    pub fn get_data_size(format: ImageTransferProcessorFormat) -> usize {
        match format {
            ImageTransferProcessorFormat::Size320x240 => 320 * 240,
            ImageTransferProcessorFormat::Size160x120 => 160 * 120,
            ImageTransferProcessorFormat::Size80x60 => 80 * 60,
            ImageTransferProcessorFormat::Size40x30 => 40 * 30,
            ImageTransferProcessorFormat::Size20x15 => 20 * 15,
        }
    }

    pub fn get_data_width(format: ImageTransferProcessorFormat) -> usize {
        match format {
            ImageTransferProcessorFormat::Size320x240 => 320,
            ImageTransferProcessorFormat::Size160x120 => 160,
            ImageTransferProcessorFormat::Size80x60 => 80,
            ImageTransferProcessorFormat::Size40x30 => 40,
            ImageTransferProcessorFormat::Size20x15 => 20,
        }
    }

    pub fn get_data_height(format: ImageTransferProcessorFormat) -> usize {
        match format {
            ImageTransferProcessorFormat::Size320x240 => 240,
            ImageTransferProcessorFormat::Size160x120 => 120,
            ImageTransferProcessorFormat::Size80x60 => 60,
            ImageTransferProcessorFormat::Size40x30 => 30,
            ImageTransferProcessorFormat::Size20x15 => 15,
        }
    }
}

impl Default for ProcessorBase {
    fn default() -> Self {
        Self::new()
    }
}

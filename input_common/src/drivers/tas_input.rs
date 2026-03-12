// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/tas_input.h` and `input_common/drivers/tas_input.cpp`.
//!
//! TAS (Tool-Assisted Speedrun) input driver for recording and playing back controller inputs.

use crate::input_engine::{InputEngine, PadIdentifier};

/// Port of PLAYER_NUMBER constant from tas_input.h
pub const PLAYER_NUMBER: usize = 10;

/// Port of `TasButton` enum from tas_input.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u64)]
pub enum TasButton {
    ButtonA = 1 << 0,
    ButtonB = 1 << 1,
    ButtonX = 1 << 2,
    ButtonY = 1 << 3,
    StickL = 1 << 4,
    StickR = 1 << 5,
    TriggerL = 1 << 6,
    TriggerR = 1 << 7,
    TriggerZL = 1 << 8,
    TriggerZR = 1 << 9,
    ButtonPlus = 1 << 10,
    ButtonMinus = 1 << 11,
    ButtonLeft = 1 << 12,
    ButtonUp = 1 << 13,
    ButtonRight = 1 << 14,
    ButtonDown = 1 << 15,
    ButtonSL = 1 << 16,
    ButtonSR = 1 << 17,
    ButtonHome = 1 << 18,
    ButtonCapture = 1 << 19,
}

/// Port of `TasAnalog` struct from tas_input.h
#[derive(Debug, Clone, Default)]
pub struct TasAnalog {
    pub x: f32,
    pub y: f32,
}

/// Port of `TasState` enum from tas_input.h
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TasState {
    Running,
    Recording,
    Stopped,
}

/// Port of Tas::TasAxis enum from tas_input.h
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum TasAxis {
    // Axis indices defined per upstream
    _Placeholder = 0,
}

/// Port of Tas::TASCommand struct from tas_input.h
#[derive(Debug, Clone, Default)]
struct TasCommand {
    buttons: u64,
    l_axis: TasAnalog,
    r_axis: TasAnalog,
}

/// Port of `Tas` class from tas_input.h / tas_input.cpp
pub struct Tas {
    engine: InputEngine,
    script_length: usize,
    is_recording: bool,
    is_running: bool,
    needs_reset: bool,
    commands: [Vec<TasCommand>; PLAYER_NUMBER],
    record_commands: Vec<TasCommand>,
    current_command: usize,
    last_input: TasCommand,
}

impl Tas {
    /// Port of Tas::Tas
    pub fn new(input_engine: String) -> Self {
        Self {
            engine: InputEngine::new(input_engine),
            script_length: 0,
            is_recording: false,
            is_running: false,
            needs_reset: false,
            commands: Default::default(),
            record_commands: Vec::new(),
            current_command: 0,
            last_input: TasCommand::default(),
        }
    }

    /// Returns a reference to the underlying input engine.
    pub fn engine(&self) -> &InputEngine {
        &self.engine
    }

    /// Returns a mutable reference to the underlying input engine.
    pub fn engine_mut(&mut self) -> &mut InputEngine {
        &mut self.engine
    }

    /// Changes the input status that will be stored in each frame.
    /// Port of Tas::RecordInput
    pub fn record_input(&mut self, _buttons: u64, _left_axis: TasAnalog, _right_axis: TasAnalog) {
        todo!()
    }

    /// Main loop that records or executes input.
    /// Port of Tas::UpdateThread
    pub fn update_thread(&mut self) {
        todo!()
    }

    /// Sets the flag to start or stop the TAS command execution.
    /// Port of Tas::StartStop
    pub fn start_stop(&mut self) {
        todo!()
    }

    /// Stop the TAS and reverts any controller profile.
    /// Port of Tas::Stop
    pub fn stop(&mut self) {
        todo!()
    }

    /// Sets the flag to reload the file and start from the beginning.
    /// Port of Tas::Reset
    pub fn reset(&mut self) {
        todo!()
    }

    /// Sets the flag to enable or disable recording of inputs.
    /// Returns true if the current recording status is enabled.
    /// Port of Tas::Record
    pub fn record(&mut self) -> bool {
        todo!()
    }

    /// Saves contents of record_commands to a file.
    /// Port of Tas::SaveRecording
    pub fn save_recording(&self, _overwrite_file: bool) {
        todo!()
    }

    /// Returns the current status values of TAS playback/recording.
    /// Port of Tas::GetStatus
    pub fn get_status(&self) -> (TasState, usize, [usize; PLAYER_NUMBER]) {
        todo!()
    }

    // ---- Private methods ----

    fn load_tas_files(&mut self) {
        todo!()
    }

    fn load_tas_file(&mut self, _player_index: usize, _file_index: usize) {
        todo!()
    }

    fn write_tas_file(&self, _file_name: &str) {
        todo!()
    }

    fn read_command_axis(&self, _line: &str) -> TasAnalog {
        todo!()
    }

    fn read_command_buttons(&self, _line: &str) -> u64 {
        todo!()
    }

    fn clear_input(&mut self) {
        todo!()
    }

    fn write_command_buttons(&self, _buttons: u64) -> String {
        todo!()
    }

    fn write_command_axis(&self, _analog: &TasAnalog) -> String {
        todo!()
    }

    fn set_tas_axis(&mut self, _identifier: &PadIdentifier, _axis: u8, _value: f32) {
        todo!()
    }
}

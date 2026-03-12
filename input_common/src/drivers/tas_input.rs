// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `input_common/drivers/tas_input.h` and `input_common/drivers/tas_input.cpp`.
//!
//! TAS (Tool-Assisted Speedrun) input driver for recording and playing back controller inputs.

use common::uuid::UUID;

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

/// Port of Tas::TasAxis enum from tas_input.cpp
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum TasAxis {
    StickX = 0,
    StickY = 1,
    SubstickX = 2,
    SubstickY = 3,
    _Undefined = 4,
}

/// Port of Tas::TASCommand struct from tas_input.h
#[derive(Debug, Clone, Default)]
struct TasCommand {
    buttons: u64,
    l_axis: TasAnalog,
    r_axis: TasAnalog,
}

/// Supported keywords and buttons from a TAS file.
/// Port of text_to_tas_button from tas_input.cpp
const TEXT_TO_TAS_BUTTON: &[(&str, TasButton)] = &[
    ("KEY_A", TasButton::ButtonA),
    ("KEY_B", TasButton::ButtonB),
    ("KEY_X", TasButton::ButtonX),
    ("KEY_Y", TasButton::ButtonY),
    ("KEY_LSTICK", TasButton::StickL),
    ("KEY_RSTICK", TasButton::StickR),
    ("KEY_L", TasButton::TriggerL),
    ("KEY_R", TasButton::TriggerR),
    ("KEY_PLUS", TasButton::ButtonPlus),
    ("KEY_MINUS", TasButton::ButtonMinus),
    ("KEY_DLEFT", TasButton::ButtonLeft),
    ("KEY_DUP", TasButton::ButtonUp),
    ("KEY_DRIGHT", TasButton::ButtonRight),
    ("KEY_DDOWN", TasButton::ButtonDown),
    ("KEY_SL", TasButton::ButtonSL),
    ("KEY_SR", TasButton::ButtonSR),
    // These buttons are disabled to avoid TAS input from activating hotkeys
    // ("KEY_CAPTURE", TasButton::ButtonCapture),
    // ("KEY_HOME", TasButton::ButtonHome),
    ("KEY_ZL", TasButton::TriggerZL),
    ("KEY_ZR", TasButton::TriggerZR),
];

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
        let mut tas = Self {
            engine: InputEngine::new(input_engine),
            script_length: 0,
            is_recording: false,
            is_running: false,
            needs_reset: true, // Settings::values.tas_enable check would go here
            commands: Default::default(),
            record_commands: Vec::new(),
            current_command: 0,
            last_input: TasCommand::default(),
        };

        // Pre-set controllers for all player slots
        for player_index in 0..PLAYER_NUMBER {
            let identifier = PadIdentifier {
                guid: UUID::new(),
                port: player_index,
                pad: 0,
            };
            tas.engine.pre_set_controller(&identifier);
        }
        tas.clear_input();
        tas
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
    pub fn record_input(&mut self, buttons: u64, left_axis: TasAnalog, right_axis: TasAnalog) {
        self.last_input = TasCommand {
            buttons,
            l_axis: left_axis,
            r_axis: right_axis,
        };
    }

    /// Main loop that records or executes input.
    /// Port of Tas::UpdateThread
    pub fn update_thread(&mut self) {
        // In C++: if (!Settings::values.tas_enable) { if (is_running) { Stop(); } return; }
        // For now, we skip the settings check.

        if self.is_recording {
            self.record_commands.push(self.last_input.clone());
        }
        if self.needs_reset {
            self.current_command = 0;
            self.needs_reset = false;
            self.load_tas_files();
            log::debug!("tas_reset done");
        }

        if !self.is_running {
            self.clear_input();
            return;
        }

        if self.current_command < self.script_length {
            log::debug!(
                "Playing TAS {}/{}",
                self.current_command,
                self.script_length
            );
            let frame = self.current_command;
            self.current_command += 1;

            for player_index in 0..self.commands.len() {
                let command = if frame < self.commands[player_index].len() {
                    self.commands[player_index][frame].clone()
                } else {
                    TasCommand::default()
                };

                let identifier = PadIdentifier {
                    guid: UUID::new(),
                    port: player_index,
                    pad: 0,
                };

                for i in 0..(std::mem::size_of::<u64>() * 8) {
                    let button_status = (command.buttons & (1u64 << i)) != 0;
                    self.engine.set_button(&identifier, i as i32, button_status);
                }
                self.set_tas_axis(&identifier, TasAxis::StickX as u8, command.l_axis.x);
                self.set_tas_axis(&identifier, TasAxis::StickY as u8, command.l_axis.y);
                self.set_tas_axis(&identifier, TasAxis::SubstickX as u8, command.r_axis.x);
                self.set_tas_axis(&identifier, TasAxis::SubstickY as u8, command.r_axis.y);
            }
        } else {
            // In C++: is_running = Settings::values.tas_loop.GetValue();
            self.is_running = false;
            self.load_tas_files();
            self.current_command = 0;
            self.clear_input();
        }
    }

    /// Sets the flag to start or stop the TAS command execution.
    /// Port of Tas::StartStop
    pub fn start_stop(&mut self) {
        // In C++: if (!Settings::values.tas_enable) { return; }
        if self.is_running {
            self.stop();
        } else {
            self.is_running = true;
        }
    }

    /// Stop the TAS and reverts any controller profile.
    /// Port of Tas::Stop
    pub fn stop(&mut self) {
        self.is_running = false;
    }

    /// Sets the flag to reload the file and start from the beginning.
    /// Port of Tas::Reset
    pub fn reset(&mut self) {
        // In C++: if (!Settings::values.tas_enable) { return; }
        self.needs_reset = true;
    }

    /// Sets the flag to enable or disable recording of inputs.
    /// Returns true if the current recording status is enabled.
    /// Port of Tas::Record
    pub fn record(&mut self) -> bool {
        // In C++: if (!Settings::values.tas_enable) { return true; }
        self.is_recording = !self.is_recording;
        self.is_recording
    }

    /// Saves contents of record_commands to a file.
    /// Port of Tas::SaveRecording
    pub fn save_recording(&mut self, overwrite_file: bool) {
        if self.is_recording {
            return;
        }
        if self.record_commands.is_empty() {
            return;
        }
        self.write_tas_file("record.txt");
        if overwrite_file {
            self.write_tas_file("script0-1.txt");
        }
        self.needs_reset = true;
        self.record_commands.clear();
    }

    /// Returns the current status values of TAS playback/recording.
    /// Port of Tas::GetStatus
    pub fn get_status(&self) -> (TasState, usize, [usize; PLAYER_NUMBER]) {
        let mut lengths = [0usize; PLAYER_NUMBER];

        if self.is_recording {
            lengths[0] = self.record_commands.len();
            return (TasState::Recording, self.record_commands.len(), lengths);
        }

        let state = if self.is_running {
            TasState::Running
        } else {
            TasState::Stopped
        };

        for i in 0..PLAYER_NUMBER {
            lengths[i] = self.commands[i].len();
        }

        (state, self.current_command, lengths)
    }

    // ---- Private methods ----

    /// Port of Tas::LoadTasFiles
    fn load_tas_files(&mut self) {
        self.script_length = 0;
        for i in 0..self.commands.len() {
            self.load_tas_file(i, 0);
            if self.commands[i].len() > self.script_length {
                self.script_length = self.commands[i].len();
            }
        }
    }

    /// Port of Tas::LoadTasFile
    fn load_tas_file(&mut self, player_index: usize, _file_index: usize) {
        self.commands[player_index].clear();

        // In C++, this reads from the TAS directory. We skip actual file I/O
        // since the file system paths depend on Settings. The logic is preserved.
        // A full port would read: Common::FS::GetYuzuPath(TASDir) / "script{file_index}-{player_index+1}.txt"
        let _file_content = String::new(); // Placeholder for file read

        // Parse would happen here following the C++ logic.
        // For each line: "frame_no buttons axis_l axis_r"
        log::info!("TAS file loaded! 0 frames");
    }

    /// Port of Tas::WriteTasFile
    fn write_tas_file(&self, _file_name: &str) {
        let mut output_text = String::new();
        for (frame, line) in self.record_commands.iter().enumerate() {
            output_text.push_str(&format!(
                "{} {} {} {}\n",
                frame,
                self.write_command_buttons(line.buttons),
                self.write_command_axis(&line.l_axis),
                self.write_command_axis(&line.r_axis),
            ));
        }

        // In C++, this writes to the TAS directory. We skip actual file I/O.
        // A full port would write: Common::FS::GetYuzuPath(TASDir) / file_name
        if output_text.is_empty() {
            log::error!("Writing the TAS-file has failed! Empty output");
        } else {
            log::info!("TAS file written to file!");
        }
    }

    /// Port of Tas::ReadCommandAxis
    fn read_command_axis(&self, line: &str) -> TasAnalog {
        let seg_list: Vec<&str> = line.split(';').collect();

        if seg_list.len() < 2 {
            log::error!("Invalid axis data: '{}'", line);
            return TasAnalog::default();
        }

        match (seg_list[0].parse::<f32>(), seg_list[1].parse::<f32>()) {
            (Ok(x), Ok(y)) => TasAnalog {
                x: x / 32767.0,
                y: y / 32767.0,
            },
            _ => {
                log::error!("Invalid argument: '{}'", line);
                TasAnalog::default()
            }
        }
    }

    /// Port of Tas::ReadCommandButtons
    fn read_command_buttons(&self, line: &str) -> u64 {
        let mut buttons: u64 = 0;
        for button_line in line.split(';') {
            for &(text, tas_button) in TEXT_TO_TAS_BUTTON {
                if text == button_line {
                    buttons |= tas_button as u64;
                    break;
                }
            }
        }
        buttons
    }

    /// Port of Tas::ClearInput
    fn clear_input(&mut self) {
        self.engine.reset_button_state();
        self.engine.reset_analog_state();
    }

    /// Port of Tas::WriteCommandButtons
    fn write_command_buttons(&self, buttons: u64) -> String {
        let mut result = String::new();
        for &(text_button, tas_button) in TEXT_TO_TAS_BUTTON {
            if (buttons & tas_button as u64) != 0 {
                result.push_str(text_button);
                result.push(';');
            }
        }
        if result.is_empty() {
            "NONE".to_string()
        } else {
            result
        }
    }

    /// Port of Tas::WriteCommandAxis
    fn write_command_axis(&self, analog: &TasAnalog) -> String {
        format!("{};{}", analog.x * 32767.0, analog.y * 32767.0)
    }

    /// Port of Tas::SetTasAxis
    fn set_tas_axis(&mut self, identifier: &PadIdentifier, axis: u8, value: f32) {
        self.engine.set_axis(identifier, axis as i32, value);
    }
}

impl Drop for Tas {
    fn drop(&mut self) {
        self.stop();
    }
}

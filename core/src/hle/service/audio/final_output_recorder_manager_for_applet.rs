//! Port of zuyu/src/core/hle/service/audio/final_output_recorder_manager_for_applet.h and .cpp
//!
//! IFinalOutputRecorderManagerForApplet service ("audrec:a").

/// IPC command table for IFinalOutputRecorderManagerForApplet ("audrec:a"):
///
/// | Cmd | Name           |
/// |-----|----------------|
/// | 0   | RequestSuspend |
/// | 1   | RequestResume  |
pub struct IFinalOutputRecorderManagerForApplet;

impl IFinalOutputRecorderManagerForApplet {
    pub fn new() -> Self {
        Self
    }
}

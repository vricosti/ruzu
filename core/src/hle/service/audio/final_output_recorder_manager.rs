//! Port of zuyu/src/core/hle/service/audio/final_output_recorder_manager.h and .cpp
//!
//! IFinalOutputRecorderManager service ("audrec:u").

/// IPC command table for IFinalOutputRecorderManager ("audrec:u"):
///
/// | Cmd | Name                        |
/// |-----|-----------------------------|
/// | 0   | OpenFinalOutputRecorder     |
///
/// IFinalOutputRecorder (returned by OpenFinalOutputRecorder):
///
/// | Cmd | Name                                           |
/// |-----|------------------------------------------------|
/// | 0   | GetFinalOutputRecorderState                    |
/// | 1   | Start                                          |
/// | 2   | Stop                                           |
/// | 3   | AppendFinalOutputRecorderBuffer                |
/// | 4   | RegisterBufferEvent                            |
/// | 5   | GetReleasedFinalOutputRecorderBuffers          |
/// | 6   | ContainsFinalOutputRecorderBuffer              |
/// | 7   | GetFinalOutputRecorderBufferEndTime            |
/// | 8   | AppendFinalOutputRecorderBufferAuto            |
/// | 9   | GetReleasedFinalOutputRecorderBufferAuto       |
/// | 10  | FlushFinalOutputRecorderBuffers                |
/// | 11  | AttachWorkBuffer                               |
pub struct IFinalOutputRecorderManager;

impl IFinalOutputRecorderManager {
    pub fn new() -> Self {
        Self
    }
}

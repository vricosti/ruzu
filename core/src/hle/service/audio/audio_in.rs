//! Port of zuyu/src/core/hle/service/audio/audio_in.h and audio_in.cpp
//!
//! IAudioIn service.

/// IPC command table for IAudioIn:
///
/// | Cmd | Name                          |
/// |-----|-------------------------------|
/// | 0   | GetAudioInState               |
/// | 1   | Start                         |
/// | 2   | Stop                          |
/// | 3   | AppendAudioInBuffer           |
/// | 4   | RegisterBufferEvent           |
/// | 5   | GetReleasedAudioInBuffers     |
/// | 6   | ContainsAudioInBuffer         |
/// | 7   | AppendUacInBuffer             |
/// | 8   | AppendAudioInBufferAuto       |
/// | 9   | GetReleasedAudioInBuffersAuto |
/// | 10  | AppendUacInBufferAuto         |
/// | 11  | GetAudioInBufferCount         |
/// | 12  | SetDeviceGain                 |
/// | 13  | GetDeviceGain                 |
/// | 14  | FlushAudioInBuffers           |
pub struct IAudioIn {
    // TODO: Wire up when audio_core and kernel are available.
}

impl IAudioIn {
    pub fn new() -> Self {
        Self {}
    }
}

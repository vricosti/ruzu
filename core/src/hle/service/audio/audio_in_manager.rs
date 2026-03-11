//! Port of zuyu/src/core/hle/service/audio/audio_in_manager.h and audio_in_manager.cpp
//!
//! IAudioInManager service ("audin:u").

/// IPC command table for IAudioInManager ("audin:u"):
///
/// | Cmd | Name                          |
/// |-----|-------------------------------|
/// | 0   | ListAudioIns                  |
/// | 1   | OpenAudioIn                   |
/// | 2   | ListAudioInsAuto              |
/// | 3   | OpenAudioInAuto               |
/// | 4   | ListAudioInsAutoFiltered      |
/// | 5   | OpenAudioInProtocolSpecified   |
pub struct IAudioInManager {
    // impl: AudioCore::AudioIn::Manager
}

impl IAudioInManager {
    pub fn new() -> Self {
        Self {}
    }
}

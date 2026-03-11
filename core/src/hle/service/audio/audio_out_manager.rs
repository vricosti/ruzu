//! Port of zuyu/src/core/hle/service/audio/audio_out_manager.h and audio_out_manager.cpp
//!
//! IAudioOutManager service ("audout:u").

/// IPC command table for IAudioOutManager ("audout:u"):
///
/// | Cmd | Name              |
/// |-----|-------------------|
/// | 0   | ListAudioOuts     |
/// | 1   | OpenAudioOut      |
/// | 2   | ListAudioOutsAuto |
/// | 3   | OpenAudioOutAuto  |
pub struct IAudioOutManager {
    // impl: AudioCore::AudioOut::Manager
}

impl IAudioOutManager {
    pub fn new() -> Self {
        Self {}
    }
}

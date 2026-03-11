//! Port of zuyu/src/core/hle/service/audio/audio_renderer.h and audio_renderer.cpp
//!
//! IAudioRenderer service.

/// IPC command table for IAudioRenderer:
///
/// | Cmd | Name                              |
/// |-----|-----------------------------------|
/// | 0   | GetSampleRate                     |
/// | 1   | GetSampleCount                    |
/// | 2   | GetMixBufferCount                 |
/// | 3   | GetState                          |
/// | 4   | RequestUpdate                     |
/// | 5   | Start                             |
/// | 6   | Stop                              |
/// | 7   | QuerySystemEvent                  |
/// | 8   | SetRenderingTimeLimit             |
/// | 9   | GetRenderingTimeLimit             |
/// | 10  | RequestUpdateAuto                 |
/// | 11  | ExecuteAudioRendererRendering     |
/// | 12  | SetVoiceDropParameter             |
/// | 13  | GetVoiceDropParameter             |
pub struct IAudioRenderer {
    // TODO: Wire up when audio_core and kernel are available.
}

impl IAudioRenderer {
    pub fn new() -> Self {
        Self {}
    }
}

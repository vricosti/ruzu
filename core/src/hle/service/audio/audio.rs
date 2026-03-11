//! Port of zuyu/src/core/hle/service/audio/audio.h and audio.cpp
//!
//! Entry point for the Audio service module. Registers all audio service endpoints.

/// Named services registered by the audio module:
/// - "audctl"    -> IAudioController
/// - "audin:u"   -> IAudioInManager
/// - "audout:u"  -> IAudioOutManager
/// - "audrec:a"  -> IFinalOutputRecorderManagerForApplet
/// - "audrec:u"  -> IFinalOutputRecorderManager
/// - "audren:u"  -> IAudioRendererManager
/// - "hwopus"    -> IHardwareOpusDecoderManager
pub fn loop_process() {
    // TODO: Wire up to ServerManager when service framework is ported.
    // Upstream registers:
    //   server_manager->RegisterNamedService("audctl", IAudioController)
    //   server_manager->RegisterNamedService("audin:u", IAudioInManager)
    //   server_manager->RegisterNamedService("audout:u", IAudioOutManager)
    //   server_manager->RegisterNamedService("audrec:a", IFinalOutputRecorderManagerForApplet)
    //   server_manager->RegisterNamedService("audrec:u", IFinalOutputRecorderManager)
    //   server_manager->RegisterNamedService("audren:u", IAudioRendererManager)
    //   server_manager->RegisterNamedService("hwopus", IHardwareOpusDecoderManager)
    todo!("Audio::LoopProcess");
}

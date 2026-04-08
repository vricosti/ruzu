//! Port of zuyu/src/core/hle/service/audio/audio.h and audio.cpp
//!
//! Entry point for the Audio service module. Registers all audio service endpoints.

use crate::hle::service::hle_ipc::SessionRequestHandlerPtr;
use crate::hle::service::server_manager::ServerManager;

/// Registers all audio services and runs the server.
///
/// Matches upstream `Audio::LoopProcess(Core::System& system)` in audio.cpp:
/// ```cpp
/// server_manager->RegisterNamedService("audctl", std::make_shared<IAudioController>(system));
/// server_manager->RegisterNamedService("audin:u", std::make_shared<IAudioInManager>(system));
/// server_manager->RegisterNamedService("audout:u", std::make_shared<IAudioOutManager>(system));
/// server_manager->RegisterNamedService("audrec:a", std::make_shared<IFinalOutputRecorderManagerForApplet>(system));
/// server_manager->RegisterNamedService("audrec:u", std::make_shared<IFinalOutputRecorderManager>(system));
/// server_manager->RegisterNamedService("audren:u", std::make_shared<IAudioRendererManager>(system));
/// server_manager->RegisterNamedService("hwopus", std::make_shared<IHardwareOpusDecoderManager>(system));
/// ```
pub fn loop_process(system: crate::core::SystemRef) {
    let mut server_manager = ServerManager::new(system);

    // audctl — IAudioController does not yet implement SessionRequestHandler, use stub
    crate::hle::service::services::register_stub_services(&mut server_manager, &["audctl"]);

    server_manager.register_named_service(
        "audin:u",
        Box::new(|| -> SessionRequestHandlerPtr {
            std::sync::Arc::new(super::audio_in_manager::IAudioInManager::new())
        }),
        16,
    );

    server_manager.register_named_service(
        "audout:u",
        Box::new(|| -> SessionRequestHandlerPtr {
            std::sync::Arc::new(super::audio_out_manager::IAudioOutManager::new())
        }),
        16,
    );

    server_manager.register_named_service(
        "audrec:a",
        Box::new(|| -> SessionRequestHandlerPtr {
            std::sync::Arc::new(super::final_output_recorder_manager_for_applet::IFinalOutputRecorderManagerForApplet::new())
        }),
        16,
    );

    server_manager.register_named_service(
        "audrec:u",
        Box::new(|| -> SessionRequestHandlerPtr {
            std::sync::Arc::new(
                super::final_output_recorder_manager::IFinalOutputRecorderManager::new(),
            )
        }),
        16,
    );

    server_manager.register_named_service(
        "audren:u",
        Box::new(move || -> SessionRequestHandlerPtr {
            std::sync::Arc::new(super::audio_renderer_manager::IAudioRendererManager::new(system))
        }),
        16,
    );

    server_manager.register_named_service(
        "hwopus",
        Box::new(|| -> SessionRequestHandlerPtr {
            std::sync::Arc::new(
                super::hardware_opus_decoder_manager::IHardwareOpusDecoderManager::new(),
            )
        }),
        16,
    );

    ServerManager::run_server(server_manager);
}

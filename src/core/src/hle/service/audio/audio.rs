//! Port of zuyu/src/core/hle/service/audio/audio.h and audio.cpp
//!
//! Entry point for the Audio service module. Registers all audio service endpoints.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::server_manager::ServerManager;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

macro_rules! define_stub_service {
    ($type:ident, $service_name:literal, [$(($id:expr, $name:literal)),* $(,)?]) => {
        pub struct $type {
            handlers: BTreeMap<u32, FunctionInfo>,
            handlers_tipc: BTreeMap<u32, FunctionInfo>,
        }

        impl $type {
            pub fn new() -> Self {
                Self {
                    handlers: build_handler_map(&[$(($id, None, $name)),*]),
                    handlers_tipc: BTreeMap::new(),
                }
            }
        }

        impl SessionRequestHandler for $type {
            fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
                ServiceFramework::handle_sync_request_impl(self, ctx)
            }

            fn service_name(&self) -> &str {
                $service_name
            }
        }

        impl ServiceFramework for $type {
            fn get_service_name(&self) -> &str {
                $service_name
            }

            fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
                &self.handlers
            }

            fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
                &self.handlers_tipc
            }
        }
    };
}

define_stub_service!(
    IAudioOutManagerForApplet,
    "audout:a",
    [
        (0, "RequestSuspend"),
        (1, "RequestResume"),
        (2, "GetProcessMasterVolume"),
        (3, "SetProcessMasterVolume"),
        (4, "GetProcessRecordVolume"),
        (5, "SetProcessRecordVolume")
    ]
);
define_stub_service!(
    IAudioSnoopManager,
    "auddev",
    [
        (0, "GetDspStatistics"),
        (1, "GetAppletStateSummaries"),
        (2, "SetDspStatisticsParameter"),
        (3, "GetDspStatisticsParameter"),
        (6, "GetDspUsage")
    ]
);
define_stub_service!(
    IAudioInManagerForApplet,
    "audin:a",
    [
        (0, "RequestSuspend"),
        (1, "RequestResume"),
        (2, "GetProcessMasterVolume"),
        (3, "SetProcessMasterVolume")
    ]
);
define_stub_service!(
    IAudioRendererManagerForApplet,
    "audren:a",
    [
        (0, "RequestSuspend"),
        (1, "RequestResume"),
        (2, "GetProcessMasterVolume"),
        (3, "SetProcessMasterVolume"),
        (4, "RegisterAppletResourceUserId"),
        (5, "UnregisterAppletResourceUserId"),
        (6, "GetProcessRecordVolume"),
        (7, "SetProcessRecordVolume")
    ]
);
define_stub_service!(
    IAudioOutManagerForDebugger,
    "audout:d",
    [(0, "RequestSuspend"), (1, "RequestResume")]
);
define_stub_service!(
    IAudioInManagerForDebugger,
    "audin:d",
    [(0, "RequestSuspend"), (1, "RequestResume")]
);
define_stub_service!(
    IFinalOutputRecorderManagerForDebugger,
    "audrec:d",
    [(0, "RequestSuspend"), (1, "RequestResume")]
);
define_stub_service!(
    IAudioRendererManagerForDebugger,
    "audren:d",
    [(0, "RequestSuspend"), (1, "RequestResume")]
);
define_stub_service!(
    IAudioSystemManagerForApplet,
    "aud:a",
    [
        (0, "RegisterAppletResourceUserId"),
        (1, "UnregisterAppletResourceUserId"),
        (2, "RequestSuspendAudio"),
        (3, "RequestResumeAudio"),
        (4, "GetAudioOutputProcessMasterVolume"),
        (5, "SetAudioOutputProcessMasterVolume"),
        (6, "GetAudioInputProcessMasterVolume"),
        (7, "SetAudioInputProcessMasterVolume"),
        (8, "GetAudioOutputProcessRecordVolume"),
        (9, "SetAudioOutputProcessRecordVolume"),
        (10, "GetAppletStateSummaries")
    ]
);
define_stub_service!(
    IAudioSystemManagerForDebugger,
    "aud:d",
    [
        (0, "RequestSuspendAudioForDebug"),
        (1, "RequestResumeAudioForDebug")
    ]
);

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
    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();

        server_manager.register_named_service(
            "aud:a",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IAudioSystemManagerForApplet::new())
            }),
            16,
        );
        server_manager.register_named_service(
            "aud:d",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IAudioSystemManagerForDebugger::new())
            }),
            16,
        );
        server_manager.register_named_service(
            "audout:d",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IAudioOutManagerForDebugger::new())
            }),
            16,
        );
        server_manager.register_named_service(
            "audin:d",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IAudioInManagerForDebugger::new())
            }),
            16,
        );
        server_manager.register_named_service(
            "audrec:d",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IFinalOutputRecorderManagerForDebugger::new())
            }),
            16,
        );
        // This surprising factory is literal upstream behavior: `audren:d`
        // is registered with IAudioInManager, not IAudioRendererManagerForDebugger.
        server_manager.register_named_service(
            "audren:d",
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(super::audio_in_manager::IAudioInManager::new(system))
            }),
            16,
        );

        server_manager.register_named_service(
            "audin:u",
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(super::audio_in_manager::IAudioInManager::new(system))
            }),
            16,
        );

        server_manager.register_named_service(
            "audin:a",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IAudioInManagerForApplet::new())
            }),
            16,
        );

        server_manager.register_named_service(
            "audout:u",
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(super::audio_out_manager::IAudioOutManager::new(system))
            }),
            16,
        );

        server_manager.register_named_service(
            "audout:a",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IAudioOutManagerForApplet::new())
            }),
            16,
        );
        server_manager.register_named_service(
            "auddev",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IAudioSnoopManager::new())
            }),
            16,
        );
        // Depends on audout:u and audin:u in the upstream constructor.
        server_manager.register_named_service(
            "audctl",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(super::audio_controller::IAudioController::new())
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
                std::sync::Arc::new(super::audio_renderer_manager::IAudioRendererManager::new(
                    system,
                ))
            }),
            16,
        );

        server_manager.register_named_service(
            "audren:a",
            Box::new(|| -> SessionRequestHandlerPtr {
                std::sync::Arc::new(IAudioRendererManagerForApplet::new())
            }),
            16,
        );

        server_manager.register_named_service(
            "hwopus",
            Box::new(move || -> SessionRequestHandlerPtr {
                std::sync::Arc::new(
                    super::hardware_opus_decoder_manager::IHardwareOpusDecoderManager::new(system),
                )
            }),
            16,
        );
    }

    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn added_audio_service_tables_match_upstream() {
        assert_eq!(IAudioSystemManagerForApplet::new().handlers().len(), 11);
        assert_eq!(IAudioRendererManagerForApplet::new().handlers().len(), 8);
        assert_eq!(IAudioOutManagerForApplet::new().handlers().len(), 6);
        assert_eq!(IAudioSnoopManager::new().handlers().len(), 5);
    }
}

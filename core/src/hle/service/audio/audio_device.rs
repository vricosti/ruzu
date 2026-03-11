//! Port of zuyu/src/core/hle/service/audio/audio_device.h and audio_device.cpp
//!
//! IAudioDevice service.

/// IPC command table for IAudioDevice:
///
/// | Cmd | Name                            |
/// |-----|---------------------------------|
/// | 0   | ListAudioDeviceName             |
/// | 1   | SetAudioDeviceOutputVolume      |
/// | 2   | GetAudioDeviceOutputVolume      |
/// | 3   | GetActiveAudioDeviceName        |
/// | 4   | QueryAudioDeviceSystemEvent     |
/// | 5   | GetActiveChannelCount           |
/// | 6   | ListAudioDeviceNameAuto         |
/// | 7   | SetAudioDeviceOutputVolumeAuto  |
/// | 8   | GetAudioDeviceOutputVolumeAuto  |
/// | 10  | GetActiveAudioDeviceNameAuto    |
/// | 11  | QueryAudioDeviceInputEvent      |
/// | 12  | QueryAudioDeviceOutputEvent     |
/// | 13  | GetActiveAudioOutputDeviceName  |
/// | 14  | ListAudioOutputDeviceName       |
pub struct IAudioDevice {
    _applet_resource_user_id: u64,
    _revision: u32,
    _device_num: u32,
}

impl IAudioDevice {
    pub fn new(applet_resource_user_id: u64, revision: u32, device_num: u32) -> Self {
        Self {
            _applet_resource_user_id: applet_resource_user_id,
            _revision: revision,
            _device_num: device_num,
        }
    }
}

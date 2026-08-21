use crate::common::feature_support::{check_feature_supported, SupportTags};
use crate::sink::SinkHandle;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AudioDeviceName {
    pub name: [u8; 0x100],
}

impl AudioDeviceName {
    pub const fn new() -> Self {
        Self { name: [0; 0x100] }
    }

    pub fn from_str(name: &str) -> Self {
        let mut out = Self::new();
        let bytes = name.as_bytes();
        let len = if bytes.len() >= out.name.len() {
            out.name.len() - 1
        } else {
            bytes.len()
        };
        let mut i = 0;
        while i < len {
            out.name[i] = bytes[i];
            i += 1;
        }
        out
    }
}

pub struct AudioDevice {
    output_sink: SinkHandle,
    applet_resource_user_id: u64,
    user_revision: u32,
}

impl AudioDevice {
    pub fn new(output_sink: SinkHandle, applet_resource_user_id: u64, revision: u32) -> Self {
        Self {
            output_sink,
            applet_resource_user_id,
            user_revision: revision,
        }
    }

    pub fn list_audio_device_name(&self, out_buffer: &mut [AudioDeviceName]) -> u32 {
        let names =
            if check_feature_supported(SupportTags::AudioUsbDeviceOutput, self.user_revision) {
                vec![
                    AudioDeviceName::from_str("AudioStereoJackOutput"),
                    AudioDeviceName::from_str("AudioBuiltInSpeakerOutput"),
                    AudioDeviceName::from_str("AudioTvOutput"),
                    AudioDeviceName::from_str("AudioUsbDeviceOutput"),
                ]
            } else {
                vec![
                    AudioDeviceName::from_str("AudioStereoJackOutput"),
                    AudioDeviceName::from_str("AudioBuiltInSpeakerOutput"),
                    AudioDeviceName::from_str("AudioTvOutput"),
                ]
            };

        let out_count = out_buffer.len().min(names.len());
        out_buffer[..out_count].copy_from_slice(&names[..out_count]);
        out_count as u32
    }

    pub fn list_audio_output_device_name(&self, out_buffer: &mut [AudioDeviceName]) -> u32 {
        let names = [
            AudioDeviceName::from_str("AudioBuiltInSpeakerOutput"),
            AudioDeviceName::from_str("AudioTvOutput"),
            AudioDeviceName::from_str("AudioExternalOutput"),
        ];
        let out_count = out_buffer.len().min(names.len());
        out_buffer[..out_count].copy_from_slice(&names[..out_count]);
        out_count as u32
    }

    pub fn set_device_volumes(&self, volume: f32) {
        self.output_sink.lock().set_device_volume(volume);
    }

    pub fn get_device_volume(&self, _name: &str) -> f32 {
        self.output_sink.lock().get_device_volume()
    }

    pub fn applet_resource_user_id(&self) -> u64 {
        self.applet_resource_user_id
    }
}

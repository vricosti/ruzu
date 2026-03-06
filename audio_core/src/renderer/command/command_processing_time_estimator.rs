use crate::common::common::SrcQuality;
use crate::renderer::behavior::BehaviorInfo;
use crate::renderer::effect::light_limiter::ProcessingMode;

use super::commands::Command;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommandProcessingTimeEstimatorVersion {
    Version1,
    Version2,
    Version3,
    Version4,
    Version5,
}

#[derive(Debug, Clone, Copy)]
pub struct CommandProcessingTimeEstimator {
    version: CommandProcessingTimeEstimatorVersion,
    sample_count: u32,
    buffer_count: u32,
}

impl CommandProcessingTimeEstimator {
    pub fn new(behavior: &BehaviorInfo, sample_count: u32, buffer_count: u32) -> Self {
        let version = if behavior.is_command_processing_time_estimator_version5_supported() {
            CommandProcessingTimeEstimatorVersion::Version5
        } else if behavior.is_command_processing_time_estimator_version4_supported() {
            CommandProcessingTimeEstimatorVersion::Version4
        } else if behavior.is_command_processing_time_estimator_version3_supported() {
            CommandProcessingTimeEstimatorVersion::Version3
        } else if behavior.is_command_processing_time_estimator_version2_supported() {
            CommandProcessingTimeEstimatorVersion::Version2
        } else {
            CommandProcessingTimeEstimatorVersion::Version1
        };
        Self {
            version,
            sample_count,
            buffer_count,
        }
    }

    pub fn version(&self) -> CommandProcessingTimeEstimatorVersion {
        self.version
    }

    pub fn estimate(&self, command: &Command) -> u32 {
        match self.version {
            CommandProcessingTimeEstimatorVersion::Version1 => self.estimate_v1(command),
            CommandProcessingTimeEstimatorVersion::Version2 => self.estimate_v2(command),
            CommandProcessingTimeEstimatorVersion::Version3 => self.estimate_v3(command),
            CommandProcessingTimeEstimatorVersion::Version4 => self.estimate_v4(command),
            CommandProcessingTimeEstimatorVersion::Version5 => self.estimate_v5(command),
        }
    }

    fn estimate_v1(&self, command: &Command) -> u32 {
        match command {
            Command::DataSourcePcmInt16Version1(data)
            | Command::DataSourcePcmInt16Version2(data) => (data.pitch * 0.25 * 1.2) as u32,
            Command::DataSourcePcmFloatVersion1(_) | Command::DataSourcePcmFloatVersion2(_) => 0,
            Command::DataSourceAdpcmVersion1(data) | Command::DataSourceAdpcmVersion2(data) => {
                (data.pitch * 0.46 * 1.2) as u32
            }
            Command::Volume(_) => ((self.sample_count as f32 * 8.8) * 1.2) as u32,
            Command::VolumeRamp(_) => ((self.sample_count as f32 * 9.8) * 1.2) as u32,
            Command::BiquadFilter(_) => ((self.sample_count as f32 * 58.0) * 1.2) as u32,
            Command::Mix(_) => ((self.sample_count as f32 * 10.0) * 1.2) as u32,
            Command::MixRamp(_) => ((self.sample_count as f32 * 14.4) * 1.2) as u32,
            Command::MixRampGrouped(cmd) => {
                (((self.sample_count as f32 * 14.4) * 1.2) * cmd.buffer_count as f32) as u32
            }
            Command::DepopPrepare(_) => 1080,
            Command::DepopForMixBuffers(cmd) => {
                ((self.sample_count as f32 * 8.9) * cmd.count as f32) as u32
            }
            Command::Delay(cmd) => {
                ((self.sample_count as f32 * cmd.parameter.channel_count as f32) * 202.5) as u32
            }
            Command::Upsample(_) => 357_915,
            Command::DownMix6chTo2ch(_) => 16_108,
            Command::Aux(cmd) => {
                if cmd.effect_enabled {
                    15_956
                } else {
                    3_765
                }
            }
            Command::DeviceSink(_) => 10_042,
            Command::CircularBufferSink(_) => 55,
            Command::Reverb(cmd) => {
                if cmd.effect_enabled {
                    ((cmd.parameter.channel_count as f32 * self.sample_count as f32 * 750.0) * 1.2)
                        as u32
                } else {
                    0
                }
            }
            Command::I3dl2Reverb(cmd) => {
                if cmd.effect_enabled {
                    ((cmd.parameter.channel_count as f32 * self.sample_count as f32 * 530.0) * 1.2)
                        as u32
                } else {
                    0
                }
            }
            Command::Performance(_) => 1454,
            Command::ClearMixBuffer(_) => {
                (((self.sample_count as f32 * 0.83) * self.buffer_count as f32) * 1.2) as u32
            }
            Command::CopyMixBuffer(_) => 0,
            Command::LightLimiterVersion1(_)
            | Command::LightLimiterVersion2(_)
            | Command::MultiTapBiquadFilter(_)
            | Command::Capture(_)
            | Command::Compressor(_) => 0,
        }
    }

    fn estimate_v2(&self, command: &Command) -> u32 {
        match command {
            Command::DataSourcePcmInt16Version1(data)
            | Command::DataSourcePcmInt16Version2(data)
            | Command::DataSourcePcmFloatVersion1(data)
            | Command::DataSourcePcmFloatVersion2(data) => self.estimate_src_v2(
                data.sample_rate,
                data.pitch,
                749.269,
                6138.94,
                1195.456,
                7797.047,
            ),
            Command::DataSourceAdpcmVersion1(data) | Command::DataSourceAdpcmVersion2(data) => self
                .estimate_src_v2(
                    data.sample_rate,
                    data.pitch,
                    2125.588,
                    9039.47,
                    3564.088,
                    6225.471,
                ),
            Command::Volume(_) => self.by_sample_count(1280.3, 1737.8),
            Command::VolumeRamp(_) => self.by_sample_count(1403.9, 1884.3),
            Command::BiquadFilter(_) => self.by_sample_count(4813.2, 6915.4),
            Command::Mix(_) => self.by_sample_count(1342.2, 1833.2),
            Command::MixRamp(_) => self.by_sample_count(1859.0, 2286.1),
            Command::MixRampGrouped(cmd) => {
                let count = Self::active_mix_ramp_group_count(cmd);
                match self.sample_count {
                    160 | 240 => ((self.sample_count as f32 * 7.245) * count as f32) as u32,
                    _ => 0,
                }
            }
            Command::DepopPrepare(_) => self.by_sample_count(306.62, 293.22),
            Command::DepopForMixBuffers(_) => self.by_sample_count(762.96, 726.96),
            Command::Delay(cmd) => {
                self.delay_v2(cmd.effect_enabled, cmd.parameter.channel_count as u16)
            }
            Command::Upsample(_) => self.by_sample_count(292000.0, 0.0),
            Command::DownMix6chTo2ch(_) => self.by_sample_count(10009.0, 14577.0),
            Command::Aux(cmd) => match self.sample_count {
                160 => {
                    if cmd.effect_enabled {
                        489
                    } else {
                        7177
                    }
                }
                240 => {
                    if cmd.effect_enabled {
                        485
                    } else {
                        9499
                    }
                }
                _ => 0,
            },
            Command::DeviceSink(cmd) => self.device_v2(cmd.input_count as u16),
            Command::CircularBufferSink(cmd) => match self.sample_count {
                160 => (cmd.input_count as f32 * 853.629 + 1284.517) as u32,
                240 => (cmd.input_count as f32 * 1726.021 + 1369.683) as u32,
                _ => 0,
            },
            Command::Reverb(cmd) => self.reverb_v2(cmd.effect_enabled, cmd.parameter.channel_count),
            Command::I3dl2Reverb(cmd) => {
                self.i3dl2_reverb_v2(cmd.effect_enabled, cmd.parameter.channel_count)
            }
            Command::Performance(_) => self.by_sample_count(489.35, 491.18),
            Command::ClearMixBuffer(_) => match self.sample_count {
                160 => (self.buffer_count as f32 * 260.4 + 139.65) as u32,
                240 => (self.buffer_count as f32 * 668.85 + 193.2) as u32,
                _ => 0,
            },
            Command::CopyMixBuffer(_) => self.by_sample_count(836.32, 1000.9),
            Command::LightLimiterVersion1(_)
            | Command::LightLimiterVersion2(_)
            | Command::MultiTapBiquadFilter(_)
            | Command::Capture(_)
            | Command::Compressor(_) => 0,
        }
    }

    fn estimate_v3(&self, command: &Command) -> u32 {
        match command {
            Command::DataSourcePcmInt16Version1(data) => self.estimate_src_v345_v1(
                data.sample_rate,
                data.pitch,
                427.52,
                6329.442,
                710.143,
                7853.286,
            ),
            Command::DataSourcePcmInt16Version2(data) => self.estimate_src_v345_v2(
                data.sample_rate,
                data.pitch,
                data.src_quality,
                (427.52, 6329.442),
                (371.876, 8049.415),
                (423.43, 5062.659),
                (710.143, 7853.286),
                (610.487, 10138.842),
                (676.722, 5810.962),
            ),
            Command::DataSourcePcmFloatVersion1(data) => self.estimate_src_v345_v1(
                data.sample_rate,
                data.pitch,
                1672.026,
                7681.211,
                2550.414,
                9663.969,
            ),
            Command::DataSourcePcmFloatVersion2(data) => self.estimate_src_v345_v2(
                data.sample_rate,
                data.pitch,
                data.src_quality,
                (1672.026, 7681.211),
                (1672.982, 9038.011),
                (1673.216, 6027.577),
                (2550.414, 9663.969),
                (2522.303, 11758.571),
                (2537.061, 7369.309),
            ),
            Command::DataSourceAdpcmVersion1(data) => self.estimate_src_v345_v1(
                data.sample_rate,
                data.pitch,
                1827.665,
                7913.808,
                2756.372,
                9736.702,
            ),
            Command::DataSourceAdpcmVersion2(data) => self.estimate_src_v345_v2(
                data.sample_rate,
                data.pitch,
                data.src_quality,
                (1827.665, 7913.808),
                (1829.285, 9607.814),
                (1824.609, 6517.476),
                (2756.372, 9736.702),
                (2731.308, 12154.379),
                (2732.152, 7929.442),
            ),
            Command::Volume(_) => self.by_sample_count(1311.1, 1713.6),
            Command::VolumeRamp(_) => self.by_sample_count(1425.3, 1700.0),
            Command::BiquadFilter(_) => self.by_sample_count(4173.2, 5585.1),
            Command::Mix(_) => self.by_sample_count(1402.8, 1853.2),
            Command::MixRamp(_) => self.by_sample_count(1968.7, 2459.4),
            Command::MixRampGrouped(cmd) => self.mix_ramp_grouped_v345(cmd),
            Command::DepopPrepare(_) => 0,
            Command::DepopForMixBuffers(_) => self.by_sample_count(739.64, 910.97),
            Command::Delay(cmd) => {
                self.delay_v345(cmd.effect_enabled, cmd.parameter.channel_count as u16)
            }
            Command::Upsample(_) => self.by_sample_count(312990.0, 0.0),
            Command::DownMix6chTo2ch(_) => self.by_sample_count(9949.7, 14679.0),
            Command::Aux(cmd) => self.aux_v345(cmd.effect_enabled),
            Command::DeviceSink(cmd) => self.device_v345(cmd.input_count as u16),
            Command::CircularBufferSink(cmd) => self.circular_sink_v345(cmd.input_count as u16),
            Command::Reverb(cmd) => {
                self.reverb_v345(cmd.effect_enabled, cmd.parameter.channel_count)
            }
            Command::I3dl2Reverb(cmd) => {
                self.i3dl2_reverb_v345(cmd.effect_enabled, cmd.parameter.channel_count)
            }
            Command::Performance(_) => self.by_sample_count(498.17, 489.42),
            Command::ClearMixBuffer(_) => self.clear_mix_v345(),
            Command::CopyMixBuffer(_) => self.by_sample_count(842.59, 986.72),
            Command::LightLimiterVersion1(cmd) => {
                self.light_limiter_v34(cmd.effect_enabled, cmd.parameter.channel_count)
            }
            Command::LightLimiterVersion2(cmd) => self.light_limiter_v34_with_stats(
                cmd.effect_enabled,
                cmd.parameter.channel_count,
                cmd.parameter.statistics_enabled,
            ),
            Command::MultiTapBiquadFilter(_) => 0,
            Command::Capture(_) | Command::Compressor(_) => 0,
        }
    }

    fn estimate_v4(&self, command: &Command) -> u32 {
        match command {
            Command::MultiTapBiquadFilter(_) => self.by_sample_count(7424.5, 9730.4),
            Command::Capture(cmd) => self.capture_v45(cmd.effect_enabled),
            Command::Compressor(_) => 0,
            _ => self.estimate_v3(command),
        }
    }

    fn estimate_v5(&self, command: &Command) -> u32 {
        match command {
            Command::LightLimiterVersion1(cmd) => {
                self.light_limiter_v5_v1(cmd.effect_enabled, cmd.parameter.channel_count)
            }
            Command::LightLimiterVersion2(cmd) => self.light_limiter_v5_v2(
                cmd.effect_enabled,
                cmd.parameter.channel_count,
                cmd.parameter.processing_mode,
                cmd.parameter.statistics_enabled,
            ),
            Command::MultiTapBiquadFilter(_) => self.by_sample_count(7424.5, 9730.4),
            Command::Capture(cmd) => self.capture_v45(cmd.effect_enabled),
            Command::Compressor(cmd) => {
                self.compressor_v5(cmd.enabled, cmd.parameter.channel_count as u16)
            }
            _ => self.estimate_v3(command),
        }
    }

    fn by_sample_count(&self, count_160: f32, count_240: f32) -> u32 {
        match self.sample_count {
            160 => count_160 as u32,
            240 => count_240 as u32,
            _ => 0,
        }
    }

    fn by_channel_count(channel_count: u16, one: f32, two: f32, four: f32, six: f32) -> u32 {
        match channel_count {
            1 => one as u32,
            2 => two as u32,
            4 => four as u32,
            6 => six as u32,
            _ => 0,
        }
    }

    fn estimate_src_v2(
        &self,
        sample_rate: u32,
        pitch: f32,
        mult_160: f32,
        base_160: f32,
        mult_240: f32,
        base_240: f32,
    ) -> u32 {
        let scaled = (sample_rate as f32 / 200.0 / self.sample_count as f32) * (pitch * 2.0);
        match self.sample_count {
            160 => (scaled * mult_160 + base_160) as u32,
            240 => (scaled * mult_240 + base_240) as u32,
            _ => 0,
        }
    }

    fn estimate_src_v345_v1(
        &self,
        sample_rate: u32,
        pitch: f32,
        mult_160: f32,
        base_160: f32,
        mult_240: f32,
        base_240: f32,
    ) -> u32 {
        let scaled =
            (sample_rate as f32 / 200.0 / self.sample_count as f32) * (pitch * 0.000030518);
        match self.sample_count {
            160 => (scaled * mult_160 + base_160) as u32,
            240 => (scaled * mult_240 + base_240) as u32,
            _ => 0,
        }
    }

    fn estimate_src_v345_v2(
        &self,
        sample_rate: u32,
        pitch: f32,
        quality: SrcQuality,
        medium_160: (f32, f32),
        high_160: (f32, f32),
        low_160: (f32, f32),
        medium_240: (f32, f32),
        high_240: (f32, f32),
        low_240: (f32, f32),
    ) -> u32 {
        let scaled =
            ((sample_rate as f32 / 200.0 / self.sample_count as f32) * (pitch * 0.000030518)) - 1.0;
        match self.sample_count {
            160 => {
                let (mult, base) = match quality {
                    SrcQuality::Medium => medium_160,
                    SrcQuality::High => high_160,
                    SrcQuality::Low => low_160,
                };
                (scaled * mult + base) as u32
            }
            240 => {
                let (mult, base) = match quality {
                    SrcQuality::Medium => medium_240,
                    SrcQuality::High => high_240,
                    SrcQuality::Low => low_240,
                };
                (scaled * mult + base) as u32
            }
            _ => 0,
        }
    }

    fn active_mix_ramp_group_count(cmd: &super::commands::MixRampGroupedCommand) -> u32 {
        cmd.volumes
            .iter()
            .zip(cmd.prev_volumes.iter())
            .take(cmd.buffer_count as usize)
            .filter(|(volume, prev_volume)| **volume != 0.0 || **prev_volume != 0.0)
            .count() as u32
    }

    fn mix_ramp_grouped_v345(&self, cmd: &super::commands::MixRampGroupedCommand) -> u32 {
        let count = Self::active_mix_ramp_group_count(cmd);
        match self.sample_count {
            160 => ((self.sample_count as f32 * 6.708) * count as f32) as u32,
            240 => ((self.sample_count as f32 * 6.443) * count as f32) as u32,
            _ => 0,
        }
    }

    fn delay_v2(&self, enabled: bool, channel_count: u16) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        41635.555,
                        97861.211,
                        192515.516,
                        301755.969,
                    )
                } else {
                    Self::by_channel_count(channel_count, 578.529, 663.064, 703.983, 760.032)
                }
            }
            240 => {
                if enabled {
                    Self::by_channel_count(channel_count, 8770.345, 25741.18, 47551.168, 81629.219)
                } else {
                    Self::by_channel_count(channel_count, 521.283, 585.396, 629.884, 713.57)
                }
            }
            _ => 0,
        }
    }

    fn device_v2(&self, input_count: u16) -> u32 {
        match input_count {
            2 => self.by_sample_count(9261.545, 9336.054),
            6 => self.by_sample_count(9336.054, 9566.728),
            _ => 0,
        }
    }

    fn reverb_v2(&self, enabled: bool, channel_count: u16) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        97192.227,
                        103278.555,
                        109579.039,
                        115065.438,
                    )
                } else {
                    Self::by_channel_count(channel_count, 492.009, 554.463, 595.864, 656.617)
                }
            }
            240 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        136463.641,
                        145749.047,
                        154796.938,
                        161968.406,
                    )
                } else {
                    Self::by_channel_count(channel_count, 495.789, 527.163, 598.752, 666.025)
                }
            }
            _ => 0,
        }
    }

    fn i3dl2_reverb_v2(&self, enabled: bool, channel_count: u16) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        138836.484,
                        135428.172,
                        199181.844,
                        247345.906,
                    )
                } else {
                    Self::by_channel_count(channel_count, 718.704, 751.296, 797.464, 867.426)
                }
            }
            240 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        199952.734,
                        195199.5,
                        290575.875,
                        363494.531,
                    )
                } else {
                    Self::by_channel_count(channel_count, 534.24, 570.874, 660.933, 694.596)
                }
            }
            _ => 0,
        }
    }

    fn delay_v345(&self, enabled: bool, channel_count: u16) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    Self::by_channel_count(channel_count, 8929.042, 25500.75, 47759.617, 82203.07)
                } else {
                    Self::by_channel_count(channel_count, 1295.206, 1213.6, 942.028, 1001.553)
                }
            }
            240 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        11941.051,
                        37197.371,
                        69749.836,
                        120042.398,
                    )
                } else {
                    Self::by_channel_count(channel_count, 997.668, 977.634, 792.309, 875.427)
                }
            }
            _ => 0,
        }
    }

    fn aux_v345(&self, enabled: bool) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    7182
                } else {
                    472
                }
            }
            240 => {
                if enabled {
                    9435
                } else {
                    462
                }
            }
            _ => 0,
        }
    }

    fn device_v345(&self, input_count: u16) -> u32 {
        match input_count {
            2 => self.by_sample_count(8979.956, 9221.907),
            6 => self.by_sample_count(9177.903, 9725.897),
            _ => 0,
        }
    }

    fn circular_sink_v345(&self, input_count: u16) -> u32 {
        match self.sample_count {
            160 => (input_count as f32 * 531.069) as u32,
            240 => (input_count as f32 * 770.257) as u32,
            _ => 0,
        }
    }

    fn reverb_v345(&self, enabled: bool, channel_count: u16) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    Self::by_channel_count(channel_count, 81475.055, 84975.0, 91625.148, 95332.266)
                } else {
                    Self::by_channel_count(channel_count, 536.298, 588.798, 643.702, 705.999)
                }
            }
            240 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        120174.469,
                        125262.219,
                        135751.234,
                        141129.234,
                    )
                } else {
                    Self::by_channel_count(channel_count, 617.641, 659.536, 711.438, 778.071)
                }
            }
            _ => 0,
        }
    }

    fn i3dl2_reverb_v345(&self, enabled: bool, channel_count: u16) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        116754.984,
                        125912.055,
                        146336.031,
                        165812.656,
                    )
                } else {
                    Self::by_channel_count(channel_count, 735.0, 766.615, 834.067, 875.437)
                }
            }
            240 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        170292.344,
                        183875.625,
                        214696.188,
                        243846.766,
                    )
                } else {
                    Self::by_channel_count(channel_count, 508.473, 582.445, 626.419, 682.468)
                }
            }
            _ => 0,
        }
    }

    fn clear_mix_v345(&self) -> u32 {
        match self.sample_count {
            160 => (self.buffer_count.saturating_sub(1) as f32 * 266.645) as u32,
            240 => (self.buffer_count.saturating_sub(1) as f32 * 440.681) as u32,
            _ => 0,
        }
    }

    fn light_limiter_v34(&self, enabled: bool, channel_count: u16) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        21392.383,
                        26829.389,
                        32405.152,
                        52218.586,
                    )
                } else {
                    Self::by_channel_count(channel_count, 897.004, 931.549, 975.387, 1016.778)
                }
            }
            240 => {
                if enabled {
                    Self::by_channel_count(channel_count, 30555.504, 39010.785, 48270.18, 76711.875)
                } else {
                    Self::by_channel_count(channel_count, 874.429, 921.553, 945.262, 992.26)
                }
            }
            _ => 0,
        }
    }

    fn light_limiter_v34_with_stats(
        &self,
        enabled: bool,
        channel_count: u16,
        statistics_enabled: bool,
    ) -> u32 {
        if !enabled {
            return self.light_limiter_v34(false, channel_count);
        }
        if !statistics_enabled {
            return self.light_limiter_v34(true, channel_count);
        }
        match self.sample_count {
            160 => {
                Self::by_channel_count(channel_count, 23308.928, 29954.062, 35807.477, 58339.773)
            }
            240 => {
                Self::by_channel_count(channel_count, 33526.121, 43549.355, 52190.281, 85526.516)
            }
            _ => 0,
        }
    }

    fn capture_v45(&self, enabled: bool) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    426
                } else {
                    4261
                }
            }
            240 => {
                if enabled {
                    435
                } else {
                    5858
                }
            }
            _ => 0,
        }
    }

    fn light_limiter_v5_v1(&self, enabled: bool, channel_count: u16) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    Self::by_channel_count(channel_count, 21508.01, 23120.453, 26270.053, 40471.902)
                } else {
                    Self::by_channel_count(channel_count, 897.004, 931.549, 975.387, 1016.778)
                }
            }
            240 => {
                if enabled {
                    Self::by_channel_count(channel_count, 30565.961, 32812.91, 37354.852, 58486.699)
                } else {
                    Self::by_channel_count(channel_count, 874.429, 921.553, 945.262, 992.26)
                }
            }
            _ => 0,
        }
    }

    fn light_limiter_v5_v2(
        &self,
        enabled: bool,
        channel_count: u16,
        processing_mode: ProcessingMode,
        statistics_enabled: bool,
    ) -> u32 {
        if !enabled {
            return match self.sample_count {
                160 => Self::by_channel_count(channel_count, 897.004, 931.549, 975.387, 1016.778),
                240 => Self::by_channel_count(channel_count, 874.429, 921.553, 945.262, 992.26),
                _ => 0,
            };
        }

        match self.sample_count {
            160 => match processing_mode {
                ProcessingMode::Mode0 => {
                    if statistics_enabled {
                        Self::by_channel_count(
                            channel_count,
                            23639.584,
                            24666.725,
                            28876.459,
                            47096.078,
                        )
                    } else {
                        Self::by_channel_count(channel_count, 0.0, 0.0, 0.0, 0.0)
                    }
                }
                ProcessingMode::Mode1 => {
                    if statistics_enabled {
                        Self::by_channel_count(
                            channel_count,
                            23639.584,
                            29954.062,
                            35807.477,
                            58339.773,
                        )
                    } else {
                        Self::by_channel_count(channel_count, 0.0, 0.0, 0.0, 0.0)
                    }
                }
            },
            240 => match processing_mode {
                ProcessingMode::Mode0 => {
                    if statistics_enabled {
                        Self::by_channel_count(
                            channel_count,
                            33875.023,
                            35199.938,
                            41371.230,
                            68370.914,
                        )
                    } else {
                        Self::by_channel_count(
                            channel_count,
                            30565.961,
                            32812.91,
                            37354.852,
                            58486.699,
                        )
                    }
                }
                ProcessingMode::Mode1 => {
                    if statistics_enabled {
                        Self::by_channel_count(
                            channel_count,
                            33942.980,
                            28698.893,
                            34774.277,
                            61897.773,
                        )
                    } else {
                        Self::by_channel_count(
                            channel_count,
                            30610.248,
                            26322.408,
                            30369.000,
                            51892.090,
                        )
                    }
                }
            },
            _ => 0,
        }
    }

    fn compressor_v5(&self, enabled: bool, channel_count: u16) -> u32 {
        match self.sample_count {
            160 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        34430.570,
                        44253.320,
                        63827.457,
                        83361.484,
                    )
                } else {
                    Self::by_channel_count(channel_count, 630.115, 638.274, 705.862, 782.019)
                }
            }
            240 => {
                if enabled {
                    Self::by_channel_count(
                        channel_count,
                        51095.348,
                        65693.094,
                        95382.852,
                        124509.906,
                    )
                } else {
                    Self::by_channel_count(channel_count, 840.136, 826.098, 901.876, 965.286)
                }
            }
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::common::MAX_MIX_BUFFERS;
    use crate::renderer::command::commands::{
        CaptureCommand, CompressorCommand, DataSourceCommand, DeviceSinkCommand,
        LightLimiterVersion2Command, MixRampGroupedCommand, VolumeCommand,
    };
    use crate::renderer::effect::{compressor, light_limiter};

    fn estimator(
        version: CommandProcessingTimeEstimatorVersion,
        sample_count: u32,
    ) -> CommandProcessingTimeEstimator {
        CommandProcessingTimeEstimator {
            version,
            sample_count,
            buffer_count: 24,
        }
    }

    #[test]
    fn version2_adpcm_240_matches_zuyu_formula() {
        let estimator = estimator(CommandProcessingTimeEstimatorVersion::Version2, 240);
        let command = Command::DataSourceAdpcmVersion1(DataSourceCommand {
            src_quality: SrcQuality::Medium,
            output_index: 0,
            flags: 0,
            sample_rate: 48_000,
            pitch: 1.0,
            channel_index: 0,
            channel_count: 1,
            wave_buffers: [Default::default(); 4],
            voice_state: 0,
            data_address: 0,
            data_size: 0,
        });

        assert_eq!(estimator.estimate(&command), 13353);
    }

    #[test]
    fn version3_volume_matches_exact_table() {
        let estimator = estimator(CommandProcessingTimeEstimatorVersion::Version3, 160);
        assert_eq!(
            estimator.estimate(&Command::Volume(VolumeCommand {
                precision: 0,
                input_index: 0,
                output_index: 1,
                volume: 1.0,
            })),
            1311
        );
    }

    #[test]
    fn version3_mix_ramp_grouped_counts_only_active_buffers() {
        let estimator = estimator(CommandProcessingTimeEstimatorVersion::Version3, 240);
        let mut volumes = [0.0; MAX_MIX_BUFFERS as usize];
        let mut prev_volumes = [0.0; MAX_MIX_BUFFERS as usize];
        volumes[0] = 0.5;
        prev_volumes[3] = 0.25;
        let command = Command::MixRampGrouped(MixRampGroupedCommand {
            buffer_count: 6,
            precision: 0,
            inputs: [0; MAX_MIX_BUFFERS as usize],
            outputs: [0; MAX_MIX_BUFFERS as usize],
            prev_volumes,
            volumes,
            previous_samples: 0,
        });

        assert_eq!(estimator.estimate(&command), 3092);
    }

    #[test]
    fn version4_capture_differs_from_version3() {
        let v3 = estimator(CommandProcessingTimeEstimatorVersion::Version3, 160);
        let v4 = estimator(CommandProcessingTimeEstimatorVersion::Version4, 160);
        let command = Command::Capture(CaptureCommand {
            input: 0,
            output: 1,
            send_buffer_info: 0,
            send_buffer: 0,
            count_max: 0,
            write_offset: 0,
            update_count: 0,
            effect_enabled: false,
        });

        assert_eq!(v3.estimate(&command), 0);
        assert_eq!(v4.estimate(&command), 4261);
    }

    #[test]
    fn version4_multitap_biquad_is_no_longer_zero() {
        let v3 = estimator(CommandProcessingTimeEstimatorVersion::Version3, 160);
        let v4 = estimator(CommandProcessingTimeEstimatorVersion::Version4, 160);
        let command =
            Command::MultiTapBiquadFilter(super::super::commands::MultiTapBiquadFilterCommand {
                input: 0,
                output: 1,
                biquads: [Default::default(); 2],
                states: [0; 2],
                needs_init: [false; 2],
                filter_tap_count: 2,
            });

        assert_eq!(v3.estimate(&command), 0);
        assert_eq!(v4.estimate(&command), 7424);
    }

    #[test]
    fn version5_compressor_matches_table() {
        let estimator = estimator(CommandProcessingTimeEstimatorVersion::Version5, 240);
        let command = Command::Compressor(CompressorCommand {
            inputs: [0; 6],
            outputs: [0; 6],
            parameter: compressor::ParameterVersion2 {
                channel_count: 2,
                ..Default::default()
            },
            state: 0,
            workbuffer: 0,
            enabled: true,
        });

        assert_eq!(estimator.estimate(&command), 65693);
    }

    #[test]
    fn version5_light_limiter_mode1_without_statistics_uses_mode_specific_values() {
        let estimator = estimator(CommandProcessingTimeEstimatorVersion::Version5, 240);
        let command = Command::LightLimiterVersion2(LightLimiterVersion2Command {
            inputs: [0; 6],
            outputs: [0; 6],
            parameter: light_limiter::ParameterVersion2 {
                channel_count: 2,
                statistics_enabled: false,
                processing_mode: ProcessingMode::Mode1,
                ..Default::default()
            },
            state: 0,
            workbuffer: 0,
            result_state: 0,
            effect_enabled: true,
        });

        assert_eq!(estimator.estimate(&command), 26322);
    }

    #[test]
    fn version3_device_uses_exact_input_count_table() {
        let estimator = estimator(CommandProcessingTimeEstimatorVersion::Version3, 240);
        let command = Command::DeviceSink(DeviceSinkCommand {
            name: [0; 0x100],
            session_id: 0,
            sample_buffer: 0,
            sample_count: 0,
            input_count: 6,
            inputs: [0; 6],
        });

        assert_eq!(estimator.estimate(&command), 9725);
    }
}

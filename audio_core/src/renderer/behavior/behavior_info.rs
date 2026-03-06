use crate::common::common::{make_magic, CpuAddr};
use crate::common::feature_support::{
    check_feature_supported, get_revision_num, SupportTags, CURRENT_REVISION,
};
use crate::Result;
use log::error;

pub const MAX_ERRORS: usize = 10;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ErrorInfo {
    pub error_code: Result,
    pub unk_04: u32,
    pub address: CpuAddr,
}

impl Default for ErrorInfo {
    fn default() -> Self {
        Self {
            error_code: common::ResultCode::SUCCESS,
            unk_04: 0,
            address: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct Flags {
    pub is_memory_force_mapping_enabled: bool,
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct InParameter {
    pub revision: u32,
    pub _padding: u32,
    pub flags: Flags,
    pub _padding2: [u8; 7],
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct OutStatus {
    pub errors: [ErrorInfo; MAX_ERRORS],
    pub error_count: u32,
    pub unk_a4: [u8; 0xC],
}

impl Default for OutStatus {
    fn default() -> Self {
        Self {
            errors: [ErrorInfo::default(); MAX_ERRORS],
            error_count: 0,
            unk_a4: [0; 0xC],
        }
    }
}

pub struct BehaviorInfo {
    process_revision: u32,
    user_revision: u32,
    error_count: u32,
    errors: [ErrorInfo; MAX_ERRORS],
    flags: Flags,
}

impl BehaviorInfo {
    pub fn new() -> Self {
        Self {
            process_revision: CURRENT_REVISION,
            user_revision: 0,
            error_count: 0,
            errors: [ErrorInfo::default(); MAX_ERRORS],
            flags: Flags::default(),
        }
    }

    pub fn get_process_revision_num(&self) -> u32 {
        self.process_revision
    }

    pub fn get_process_revision(&self) -> u32 {
        make_magic(
            'R',
            'E',
            'V',
            char::from_u32('0' as u32 + self.process_revision).unwrap_or(';'),
        )
    }

    pub fn get_user_revision_num(&self) -> u32 {
        self.user_revision
    }

    pub fn get_user_revision(&self) -> u32 {
        make_magic(
            'R',
            'E',
            'V',
            char::from_u32('0' as u32 + self.user_revision).unwrap_or(';'),
        )
    }

    pub fn set_user_lib_revision(&mut self, user_revision: u32) {
        self.user_revision = get_revision_num(user_revision);
    }

    pub fn clear_error(&mut self) {
        self.error_count = 0;
        self.errors.fill(ErrorInfo::default());
    }

    pub fn append_error(&mut self, error_info: ErrorInfo) {
        error!(
            "audio_core: RequestUpdate error code=0x{:08X} address=0x{:X}",
            error_info.error_code.raw(),
            error_info.address
        );
        if (self.error_count as usize) < MAX_ERRORS {
            self.errors[self.error_count as usize] = error_info;
            self.error_count += 1;
        }
    }

    pub fn copy_error_info(&self, out_errors: &mut [ErrorInfo], out_count: &mut u32) {
        *out_count = self.error_count.min(MAX_ERRORS as u32);
        for (index, out_error) in out_errors.iter_mut().enumerate() {
            *out_error = if index < *out_count as usize {
                self.errors[index]
            } else {
                ErrorInfo::default()
            };
        }
    }

    pub fn update_flags(&mut self, flags: Flags) {
        self.flags = flags;
    }

    pub fn is_memory_force_mapping_enabled(&self) -> bool {
        self.flags.is_memory_force_mapping_enabled
    }

    pub fn is_adpcm_loop_context_bug_fixed(&self) -> bool {
        self.supported(SupportTags::AdpcmLoopContextBugFix)
    }

    pub fn is_splitter_supported(&self) -> bool {
        self.supported(SupportTags::Splitter)
    }

    pub fn is_splitter_bug_fixed(&self) -> bool {
        self.supported(SupportTags::SplitterBugFix)
    }

    pub fn is_effect_info_version2_supported(&self) -> bool {
        self.supported(SupportTags::EffectInfoVer2)
    }

    pub fn is_variadic_command_buffer_size_supported(&self) -> bool {
        self.supported(SupportTags::AudioRendererVariadicCommandBufferSize)
    }

    pub fn is_wave_buffer_ver2_supported(&self) -> bool {
        self.supported(SupportTags::WaveBufferVer2)
    }

    pub fn is_long_size_pre_delay_supported(&self) -> bool {
        self.supported(SupportTags::LongSizePreDelay)
    }

    pub fn is_command_processing_time_estimator_version2_supported(&self) -> bool {
        self.supported(SupportTags::CommandProcessingTimeEstimatorVersion2)
    }

    pub fn is_command_processing_time_estimator_version3_supported(&self) -> bool {
        self.supported(SupportTags::CommandProcessingTimeEstimatorVersion3)
    }

    pub fn is_command_processing_time_estimator_version4_supported(&self) -> bool {
        self.supported(SupportTags::CommandProcessingTimeEstimatorVersion4)
    }

    pub fn is_command_processing_time_estimator_version5_supported(&self) -> bool {
        self.supported(SupportTags::CommandProcessingTimeEstimatorVersion4)
    }

    pub fn is_audio_renderer_processing_time_limit70_percent_supported(&self) -> bool {
        self.supported(SupportTags::AudioRendererProcessingTimeLimit70Percent)
    }

    pub fn is_audio_renderer_processing_time_limit75_percent_supported(&self) -> bool {
        self.supported(SupportTags::AudioRendererProcessingTimeLimit75Percent)
    }

    pub fn is_audio_renderer_processing_time_limit80_percent_supported(&self) -> bool {
        self.supported(SupportTags::AudioRendererProcessingTimeLimit80Percent)
    }

    pub fn is_flush_voice_wave_buffers_supported(&self) -> bool {
        self.supported(SupportTags::FlushVoiceWaveBuffers)
    }

    pub fn is_elapsed_frame_count_supported(&self) -> bool {
        self.supported(SupportTags::ElapsedFrameCount)
    }

    pub fn is_performance_metrics_data_format_version2_supported(&self) -> bool {
        self.supported(SupportTags::PerformanceMetricsDataFormatVersion2)
    }

    pub fn get_performance_metrics_data_format(&self) -> usize {
        if self.is_performance_metrics_data_format_version2_supported() {
            2
        } else {
            1
        }
    }

    pub fn is_voice_pitch_and_src_skipped_supported(&self) -> bool {
        self.supported(SupportTags::VoicePitchAndSrcSkipped)
    }

    pub fn is_voice_played_sample_count_reset_at_loop_point_supported(&self) -> bool {
        self.supported(SupportTags::VoicePlayedSampleCountResetAtLoopPoint)
    }

    pub fn is_biquad_filter_effect_state_clear_bug_fixed(&self) -> bool {
        self.supported(SupportTags::BiquadFilterEffectStateClearBugFix)
    }

    pub fn is_volume_mix_parameter_precision_q23_supported(&self) -> bool {
        self.supported(SupportTags::VolumeMixParameterPrecisionQ23)
    }

    pub fn use_biquad_filter_float_processing(&self) -> bool {
        self.supported(SupportTags::BiquadFilterFloatProcessing)
    }

    pub fn is_mix_in_parameter_dirty_only_update_supported(&self) -> bool {
        self.supported(SupportTags::MixInParameterDirtyOnlyUpdate)
    }

    pub fn use_multi_tap_biquad_filter_processing(&self) -> bool {
        self.supported(SupportTags::MultiTapBiquadFilterProcessing)
    }

    pub fn is_device_api_version2_supported(&self) -> bool {
        self.supported(SupportTags::DeviceApiVersion2)
    }

    pub fn is_delay_channel_mapping_changed(&self) -> bool {
        self.supported(SupportTags::DelayChannelMappingChange)
    }

    pub fn is_reverb_channel_mapping_changed(&self) -> bool {
        self.supported(SupportTags::ReverbChannelMappingChange)
    }

    pub fn is_i3dl2_reverb_channel_mapping_changed(&self) -> bool {
        self.supported(SupportTags::I3dl2ReverbChannelMappingChange)
    }

    fn supported(&self, tag: SupportTags) -> bool {
        check_feature_supported(tag, self.user_revision)
    }
}

impl Default for BehaviorInfo {
    fn default() -> Self {
        Self::new()
    }
}

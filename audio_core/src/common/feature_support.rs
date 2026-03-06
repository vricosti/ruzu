use crate::common::common::make_magic;

pub const CURRENT_REVISION: u32 = 11;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SupportTags {
    CommandProcessingTimeEstimatorVersion4,
    CommandProcessingTimeEstimatorVersion3,
    CommandProcessingTimeEstimatorVersion2,
    MultiTapBiquadFilterProcessing,
    EffectInfoVer2,
    WaveBufferVer2,
    BiquadFilterFloatProcessing,
    VolumeMixParameterPrecisionQ23,
    MixInParameterDirtyOnlyUpdate,
    BiquadFilterEffectStateClearBugFix,
    VoicePlayedSampleCountResetAtLoopPoint,
    VoicePitchAndSrcSkipped,
    SplitterBugFix,
    FlushVoiceWaveBuffers,
    ElapsedFrameCount,
    AudioRendererVariadicCommandBufferSize,
    PerformanceMetricsDataFormatVersion2,
    AudioRendererProcessingTimeLimit80Percent,
    AudioRendererProcessingTimeLimit75Percent,
    AudioRendererProcessingTimeLimit70Percent,
    AdpcmLoopContextBugFix,
    Splitter,
    LongSizePreDelay,
    AudioUsbDeviceOutput,
    DeviceApiVersion2,
    DelayChannelMappingChange,
    ReverbChannelMappingChange,
    I3dl2ReverbChannelMappingChange,
}

const FEATURES: &[(SupportTags, u32)] = &[
    (SupportTags::AudioRendererProcessingTimeLimit70Percent, 1),
    (SupportTags::Splitter, 2),
    (SupportTags::AdpcmLoopContextBugFix, 2),
    (SupportTags::LongSizePreDelay, 3),
    (SupportTags::AudioUsbDeviceOutput, 4),
    (SupportTags::AudioRendererProcessingTimeLimit75Percent, 4),
    (SupportTags::VoicePlayedSampleCountResetAtLoopPoint, 5),
    (SupportTags::VoicePitchAndSrcSkipped, 5),
    (SupportTags::SplitterBugFix, 5),
    (SupportTags::FlushVoiceWaveBuffers, 5),
    (SupportTags::ElapsedFrameCount, 5),
    (SupportTags::AudioRendererProcessingTimeLimit80Percent, 5),
    (SupportTags::AudioRendererVariadicCommandBufferSize, 5),
    (SupportTags::PerformanceMetricsDataFormatVersion2, 5),
    (SupportTags::CommandProcessingTimeEstimatorVersion2, 5),
    (SupportTags::BiquadFilterEffectStateClearBugFix, 6),
    (SupportTags::BiquadFilterFloatProcessing, 7),
    (SupportTags::VolumeMixParameterPrecisionQ23, 7),
    (SupportTags::MixInParameterDirtyOnlyUpdate, 7),
    (SupportTags::WaveBufferVer2, 8),
    (SupportTags::CommandProcessingTimeEstimatorVersion3, 8),
    (SupportTags::EffectInfoVer2, 9),
    (SupportTags::CommandProcessingTimeEstimatorVersion4, 10),
    (SupportTags::MultiTapBiquadFilterProcessing, 10),
    (SupportTags::DelayChannelMappingChange, 11),
    (SupportTags::ReverbChannelMappingChange, 11),
    (SupportTags::I3dl2ReverbChannelMappingChange, 11),
];

pub const fn get_revision_num(mut user_revision: u32) -> u32 {
    if user_revision >= 0x100 {
        user_revision -= make_magic('R', 'E', 'V', '0');
        user_revision >>= 24;
    }
    user_revision
}

pub fn check_feature_supported(tag: SupportTags, user_revision: u32) -> bool {
    FEATURES
        .iter()
        .find(|(feature, _)| *feature == tag)
        .map(|(_, revision)| *revision <= get_revision_num(user_revision))
        .unwrap_or(false)
}

pub const fn check_valid_revision(user_revision: u32) -> bool {
    get_revision_num(user_revision) <= CURRENT_REVISION
}

use crate::common::common::CpuAddr;

#[derive(Debug, Clone, Copy, Default)]
pub struct PerformanceEntryAddresses {
    pub translated_address: CpuAddr,
    pub entry_start_time_offset: CpuAddr,
    pub header_entry_count_offset: CpuAddr,
    pub entry_processed_time_offset: CpuAddr,
}

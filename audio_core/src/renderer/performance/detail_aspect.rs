use super::performance_detail::PerformanceDetailType;
use super::performance_entry::PerformanceEntryType;
use super::performance_entry_addresses::PerformanceEntryAddresses;
use super::performance_manager::PerformanceManager;

#[derive(Debug, Clone, Copy)]
pub struct DetailAspect {
    pub performance_entry_address: PerformanceEntryAddresses,
    pub initialized: bool,
    pub node_id: i32,
    pub entry_type: PerformanceEntryType,
    pub detail_type: PerformanceDetailType,
}

impl Default for DetailAspect {
    fn default() -> Self {
        Self {
            performance_entry_address: PerformanceEntryAddresses::default(),
            initialized: false,
            node_id: 0,
            entry_type: PerformanceEntryType::Invalid,
            detail_type: PerformanceDetailType::Invalid,
        }
    }
}

impl DetailAspect {
    pub fn new(
        manager: &mut PerformanceManager,
        detail_type: PerformanceDetailType,
        entry_type: PerformanceEntryType,
        node_id: i32,
    ) -> Self {
        let mut performance_entry_address = PerformanceEntryAddresses::default();
        let initialized = manager.get_next_detail(
            &mut performance_entry_address,
            detail_type,
            entry_type,
            node_id,
        );
        Self {
            performance_entry_address,
            initialized,
            node_id,
            entry_type,
            detail_type,
        }
    }
}

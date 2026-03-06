use super::performance_entry::PerformanceEntryType;
use super::performance_entry_addresses::PerformanceEntryAddresses;
use super::performance_manager::PerformanceManager;

#[derive(Debug, Clone, Copy)]
pub struct EntryAspect {
    pub performance_entry_address: PerformanceEntryAddresses,
    pub initialized: bool,
    pub node_id: i32,
    pub entry_type: PerformanceEntryType,
}

impl Default for EntryAspect {
    fn default() -> Self {
        Self {
            performance_entry_address: PerformanceEntryAddresses::default(),
            initialized: false,
            node_id: 0,
            entry_type: PerformanceEntryType::Invalid,
        }
    }
}

impl EntryAspect {
    pub fn new(
        manager: &mut PerformanceManager,
        entry_type: PerformanceEntryType,
        node_id: i32,
    ) -> Self {
        let mut performance_entry_address = PerformanceEntryAddresses::default();
        let initialized =
            manager.get_next_entry(&mut performance_entry_address, entry_type, node_id);
        Self {
            performance_entry_address,
            initialized,
            node_id,
            entry_type,
        }
    }
}

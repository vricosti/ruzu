//! Port of zuyu/src/core/hle/kernel/k_page_table.h
//! Status: Stubbed (thin wrapper over KPageTableBase)
//! Derniere synchro: 2026-03-11
//!
//! KPageTable is the concrete page table type. Upstream it inherits from KPageTableBase;
//! here it re-exports / wraps KPageTableBase.

use super::k_page_table_base::KPageTableBase;

/// Port of Kernel::KPageTable.
///
/// In upstream, `KPageTable final : public KPageTableBase` with no additional members.
pub struct KPageTable {
    pub base: KPageTableBase,
}

impl KPageTable {
    pub fn new() -> Self {
        Self {
            base: KPageTableBase::new(),
        }
    }
}

impl Default for KPageTable {
    fn default() -> Self {
        Self::new()
    }
}

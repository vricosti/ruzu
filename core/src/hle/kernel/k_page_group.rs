//! Port of zuyu/src/core/hle/kernel/k_page_group.h and k_page_group.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-21

use super::k_memory_block::PAGE_SIZE;

// ---------------------------------------------------------------------------
// KBlockInfo
// ---------------------------------------------------------------------------

/// Port of Kernel::KBlockInfo.
#[derive(Debug, Clone)]
pub struct KBlockInfo {
    m_page_index: u32,
    m_num_pages: u32,
}

impl KBlockInfo {
    pub fn new() -> Self {
        Self {
            m_page_index: 0,
            m_num_pages: 0,
        }
    }

    pub fn initialize(&mut self, addr: u64, np: usize) {
        debug_assert!(addr % PAGE_SIZE as u64 == 0);
        debug_assert!(np as u32 as usize == np);
        self.m_page_index = (addr / PAGE_SIZE as u64) as u32;
        self.m_num_pages = np as u32;
    }

    pub fn get_address(&self) -> u64 {
        self.m_page_index as u64 * PAGE_SIZE as u64
    }
    pub fn get_num_pages(&self) -> usize {
        self.m_num_pages as usize
    }
    pub fn get_size(&self) -> usize {
        self.get_num_pages() * PAGE_SIZE
    }
    pub fn get_end_address(&self) -> u64 {
        (self.m_page_index as u64 + self.m_num_pages as u64) * PAGE_SIZE as u64
    }
    pub fn get_last_address(&self) -> u64 {
        self.get_end_address() - 1
    }

    pub fn is_equivalent_to(&self, rhs: &KBlockInfo) -> bool {
        self.m_page_index == rhs.m_page_index && self.m_num_pages == rhs.m_num_pages
    }

    pub fn is_strictly_before(&self, addr: u64) -> bool {
        let end = self.get_end_address();
        if self.m_page_index != 0 && end == 0 {
            return false;
        }
        end < addr
    }

    pub fn try_concatenate(&mut self, addr: u64, np: usize) -> bool {
        if addr != 0 && addr == self.get_end_address() {
            self.m_num_pages += np as u32;
            true
        } else {
            false
        }
    }
}

impl Default for KBlockInfo {
    fn default() -> Self {
        Self::new()
    }
}

const _: () = assert!(std::mem::size_of::<KBlockInfo>() <= 0x10);

// ---------------------------------------------------------------------------
// KPageGroup
// ---------------------------------------------------------------------------

/// Port of Kernel::KPageGroup.
///
/// Uses a Vec<KBlockInfo> instead of an intrusive linked list.
pub struct KPageGroup {
    blocks: Vec<KBlockInfo>,
}

impl KPageGroup {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub fn finalize(&mut self) {
        self.blocks.clear();
    }

    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &KBlockInfo> {
        self.blocks.iter()
    }

    pub fn add_block(&mut self, addr: u64, num_pages: usize) -> Result<(), ()> {
        if num_pages == 0 {
            return Ok(());
        }
        debug_assert!(addr < addr + num_pages as u64 * PAGE_SIZE as u64);

        // Try to concatenate with the last block.
        if let Some(last) = self.blocks.last_mut() {
            if last.try_concatenate(addr, num_pages) {
                return Ok(());
            }
        }

        // Allocate a new block.
        let mut new_block = KBlockInfo::new();
        new_block.initialize(addr, num_pages);
        self.blocks.push(new_block);
        Ok(())
    }

    pub fn get_num_pages(&self) -> usize {
        let mut total = 0;
        for block in &self.blocks {
            total += block.get_num_pages();
        }
        total
    }

    /// Increment reference count for all blocks.
    /// Port of upstream `KPageGroup::Open`.
    /// Upstream delegates to `KMemoryManager::Open(addr, num_pages)` for each block.
    /// In the host-emulated model, physical page reference counting is a no-op.
    pub fn open(&self) {
        // Host-emulated: no physical page reference counting needed.
    }

    /// Increment reference count for all blocks (first reference).
    /// Port of upstream `KPageGroup::OpenFirst`.
    pub fn open_first(&self) {
        // Host-emulated: no physical page reference counting needed.
    }

    /// Decrement reference count for all blocks.
    /// Port of upstream `KPageGroup::Close`.
    pub fn close(&self) {
        // Host-emulated: no physical page reference counting needed.
    }

    /// Decrement reference count and reset the page group.
    /// Port of upstream `KPageGroup::CloseAndReset`.
    pub fn close_and_reset(&mut self) {
        // Upstream: mm.Close(block.addr, block.num_pages) for each, then free blocks.
        self.blocks.clear();
    }

    pub fn is_equivalent_to(&self, rhs: &KPageGroup) -> bool {
        if self.blocks.len() != rhs.blocks.len() {
            return false;
        }
        for (a, b) in self.blocks.iter().zip(rhs.blocks.iter()) {
            if !a.is_equivalent_to(b) {
                return false;
            }
        }
        true
    }
}

impl Default for KPageGroup {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for KPageGroup {
    fn drop(&mut self) {
        self.finalize();
    }
}

// ---------------------------------------------------------------------------
// KScopedPageGroup
// ---------------------------------------------------------------------------

/// RAII guard that opens a page group on construction and closes it on drop.
/// Port of upstream `Kernel::KScopedPageGroup`.
pub struct KScopedPageGroup<'a> {
    pg: Option<&'a KPageGroup>,
}

impl<'a> KScopedPageGroup<'a> {
    /// Create a scoped page group. Opens the page group immediately.
    /// If `not_first` is true, calls `Open()`; otherwise calls `OpenFirst()`.
    pub fn new(pg: &'a KPageGroup, not_first: bool) -> Self {
        if not_first {
            pg.open();
        } else {
            pg.open_first();
        }
        Self { pg: Some(pg) }
    }

    /// Cancel the close — prevents `Close()` on drop.
    pub fn cancel_close(&mut self) {
        self.pg = None;
    }
}

impl Drop for KScopedPageGroup<'_> {
    fn drop(&mut self) {
        if let Some(pg) = self.pg {
            pg.close();
        }
    }
}

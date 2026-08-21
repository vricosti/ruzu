use crate::ir::location::LocationDescriptor;
use hashbrown::HashMap;

/// A compiled native code block.
pub struct CachedBlock {
    /// Absolute native code address (within the code buffer).
    pub entrypoint: *const u8,
    /// Offset from code buffer base.
    pub entrypoint_offset: usize,
    /// Size of the compiled native code in bytes.
    pub size: usize,
}

/// Cache of compiled blocks, keyed by LocationDescriptor (PC + FPCR hash).
///
/// Single-threaded: no internal locking (one JIT per CPU core).
pub struct BlockCache {
    blocks: HashMap<LocationDescriptor, CachedBlock>,
}

impl BlockCache {
    pub fn new() -> Self {
        Self {
            blocks: HashMap::new(),
        }
    }

    /// Look up an emitted host block in the cache.
    ///
    /// Port of upstream `EmitX64::GetBasicBlock`'s map lookup.
    pub fn get(&self, location: &LocationDescriptor) -> Option<&CachedBlock> {
        self.blocks.get(location)
    }

    /// Insert a compiled block into the cache.
    pub fn insert(&mut self, location: LocationDescriptor, block: CachedBlock) {
        self.blocks.insert(location, block);
    }

    pub fn contains(&self, location: &LocationDescriptor) -> bool {
        self.blocks.contains_key(location)
    }

    /// Remove one exact location descriptor.
    ///
    /// This is the operation upstream `InvalidateBasicBlocks` uses for a
    /// fault-triggered fastmem recompile; other FPCR/upper-state variants at
    /// the same PC remain cached.
    pub fn remove(&mut self, location: &LocationDescriptor) -> bool {
        self.blocks.remove(location).is_some()
    }

    /// Clear all cached blocks.
    pub fn clear(&mut self) {
        self.blocks.clear();
    }

    /// Invalidate blocks whose PC falls within [start, start+length).
    pub fn invalidate_range(&mut self, start: u64, length: u64) {
        let end = start.wrapping_add(length);
        self.blocks.retain(|loc, _| {
            let pc = loc.value() & 0x00FF_FFFF_FFFF_FFFF; // PC mask (56 bits)
            pc < start || pc >= end
        });
    }

    /// Number of cached blocks.
    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    /// Whether the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }

    /// Iterate over all cached location descriptors.
    pub fn keys(&self) -> impl Iterator<Item = &LocationDescriptor> {
        self.blocks.keys()
    }
}

impl Default for BlockCache {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_block_cache_insert_and_get() {
        let mut cache = BlockCache::new();
        let loc = LocationDescriptor::new(0x1000);
        cache.insert(
            loc,
            CachedBlock {
                entrypoint: std::ptr::null(),
                entrypoint_offset: 0x100,
                size: 64,
            },
        );
        assert_eq!(cache.len(), 1);
        let block = cache.get(&loc).unwrap();
        assert_eq!(block.entrypoint_offset, 0x100);
        assert_eq!(block.size, 64);
    }

    #[test]
    fn test_block_cache_invalidate_range() {
        let mut cache = BlockCache::new();
        cache.insert(
            LocationDescriptor::new(0x1000),
            CachedBlock {
                entrypoint: std::ptr::null(),
                entrypoint_offset: 0,
                size: 32,
            },
        );
        cache.insert(
            LocationDescriptor::new(0x2000),
            CachedBlock {
                entrypoint: std::ptr::null(),
                entrypoint_offset: 32,
                size: 32,
            },
        );
        cache.insert(
            LocationDescriptor::new(0x3000),
            CachedBlock {
                entrypoint: std::ptr::null(),
                entrypoint_offset: 64,
                size: 32,
            },
        );
        assert_eq!(cache.len(), 3);

        // Invalidate range [0x1000, 0x2800) — should remove 0x1000 and 0x2000
        cache.invalidate_range(0x1000, 0x1800);
        assert_eq!(cache.len(), 1);
        assert!(cache.get(&LocationDescriptor::new(0x3000)).is_some());
    }

    #[test]
    fn test_block_cache_clear() {
        let mut cache = BlockCache::new();
        cache.insert(
            LocationDescriptor::new(0x1000),
            CachedBlock {
                entrypoint: std::ptr::null(),
                entrypoint_offset: 0,
                size: 32,
            },
        );
        assert_eq!(cache.len(), 1);
        cache.clear();
        assert!(cache.is_empty());
    }

    #[test]
    fn remove_only_erases_exact_location_descriptor() {
        let mut cache = BlockCache::new();
        let first = LocationDescriptor::new(0x0000_0000_0000_1000);
        let variant = LocationDescriptor::new(0x0000_0001_0000_1000);
        for location in [first, variant] {
            cache.insert(
                location,
                CachedBlock {
                    entrypoint: std::ptr::null(),
                    entrypoint_offset: 0,
                    size: 32,
                },
            );
        }

        assert!(cache.remove(&first));
        assert!(!cache.contains(&first));
        assert!(cache.contains(&variant));
        assert!(!cache.remove(&first));
    }
}
